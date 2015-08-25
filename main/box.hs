{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           BuildInfo_box

import           Box hiding ((</>))

import qualified Control.Arrow as Arrow
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Data.List (sort)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Options.Applicative

import           P

import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath ((</>))
import           System.IO
import           System.Posix.Process
import           System.Posix.User

import           Text.PrettyPrint.Boxes ((<+>))
import qualified Text.PrettyPrint.Boxes as PB

import           X.Options.Applicative

------------------------------------------------------------------------
-- Types

data BoxCommand =
    BoxIP   Query HostType
  | BoxSSH  Query [SSHArg]
  | BoxList Query
  deriving (Eq, Show)

data HostType =
    ExternalHost
  | InternalHost
  deriving (Eq, Show)

type SSHArg = Text

data BoxCommandError =
    BoxError BoxError
  | BoxNoFilter
  | BoxNoMatches
  deriving (Eq, Show)


------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch boxP >>= \case
    VersionCommand ->
      getProgName >>= \prog -> (putStrLn $ prog <> ": " <> buildInfoVersion) >> exitSuccess
    RunCommand r cmd -> do
      case cmd of
        BoxIP   q host -> runCommand (boxIP  r q host)
        BoxSSH  q args -> runCommand (boxSSH r q args)
        BoxList q      -> runCommand (boxList q)

runCommand :: ([Box] -> EitherT BoxCommandError IO ()) -> IO ()
runCommand cmd = orDie boxCommandErrorRender $ do
  boxes <- fetchBoxes
  cmd boxes
  liftIO exitSuccess


------------------------------------------------------------------------
-- Commands

boxIP :: RunType -> Query -> HostType -> [Box] -> EitherT BoxCommandError IO ()
boxIP _ q hostType boxes = do
  b <- randomBoxOfQuery q boxes
  liftIO (putStrLn . T.unpack . unHost . selectHost hostType $ b)

boxSSH :: RunType -> Query -> [SSHArg] -> [Box] -> EitherT BoxCommandError IO ()
boxSSH runType qTarget args boxes = do
  let qGateway = Query ExactAll (Exact (Flavour "gateway")) InfixAll ExactAll

  t <- randomBoxOfQuery qTarget  boxes
  g <- randomBoxOfQuery qGateway boxes

  let targetHost  = unHost (selectHost InternalHost t)
  let targetName  = unName (boxName t) <> "." <> unInstanceId (boxInstance t)
  let gatewayHost = unHost (selectHost ExternalHost g)

  user     <- liftIO userEnv
  identity <- liftIO identityEnv

  let args' = [ -- ProxyCommand bounces us off the gateway. Note that we pipe
                -- netcat's stderr to /dev/null to avoid the annoying 'Killed
                -- by signal 1.' message when the session finishes.
                "-o", "ProxyCommand=ssh" <> " -i " <> identity
                                         <> " " <> user <> "@" <> gatewayHost
                                         <> " nc %h %p 2>/dev/null"

                -- Keep track of host keys using the target's somewhat unique
                -- name.
              , "-o", "HostKeyAlias=box." <> targetName <> ".jump"

                -- Connect to target box.
              , user <> "@" <> targetHost

                -- Pass on any additional arguments to 'ssh'.
              ] <> args

  case runType of
    DryRun  ->
      liftIO (print ("ssh" : args'))

    RealRun -> do
      -- Set the title of the terminal.
      liftIO (T.putStr ("\ESC]0;" <> targetName <> "\BEL"))
      liftIO (hFlush stdout)

      -- This call never returns, the current process is replaced by 'ssh'.
      liftIO (exec "ssh" args')

boxList :: Query -> [Box] -> EitherT BoxCommandError IO ()
boxList q boxes = liftIO $ do
    putStrLn ("total " <> show (length sorted))

    PB.printBox $ col PB.left  (unClient     . boxClient)
              <+> col PB.left  (unFlavour    . boxFlavour)
             <++> col PB.right (unName       . boxShortName)
             <++> col PB.left  (unHost       . boxHost)
             <++> col PB.left  (unHost       . boxPublicHost)
             <++> col PB.left  (unInstanceId . boxInstance)
  where
    sorted       = sort (query q boxes)
    col align f  = PB.vcat align (fmap (PB.text . T.unpack . f) sorted)

    (<++>) l r = l PB.<> PB.emptyBox 0 2 PB.<> r


------------------------------------------------------------------------
-- Utils

cachedBoxes :: EitherT BoxCommandError IO [Box]
cachedBoxes = do
  let timeout = 60 -- seconds
  path  <- liftIO cacheEnv
  cache <- liftIO (readCache path timeout)
  case cache of
    Just boxes -> return boxes
    Nothing    -> fetchBoxes

fetchBoxes :: EitherT BoxCommandError IO [Box]
fetchBoxes = do
  store <- liftIO storeEnv
  boxes <- EitherT (Arrow.left BoxError <$> runS3WithDefaults (readBoxes store))
  path  <- liftIO cacheEnv
  liftIO (writeCache path boxes)
  return boxes

randomBoxOfQuery :: MonadIO m => Query -> [Box] -> EitherT BoxCommandError m Box
randomBoxOfQuery q bs = do
  unless (queryHasMatch q) (left BoxNoFilter)
  maybe (left BoxNoMatches) return =<< liftIO (selectRandomBox (query q bs))

selectHost :: HostType -> Box -> Host
selectHost InternalHost = boxHost
selectHost ExternalHost = boxPublicHost

boxCommandErrorRender :: BoxCommandError -> Text
boxCommandErrorRender (BoxError e)   = boxErrorRender e
boxCommandErrorRender (BoxNoFilter)  = "No filter specified"
boxCommandErrorRender (BoxNoMatches) = "No matching boxes found"

exec :: Text -> [Text] -> IO a
exec cmd args = executeFile (T.unpack cmd) True (fmap T.unpack args) Nothing


------------------------------------------------------------------------
-- Environment Variables

storeEnv :: IO BoxStore
storeEnv =
    fmap (maybe (BoxStoreS3 boxStoreAddress) (\s -> maybe (BoxStoreLocal s) BoxStoreS3 . addressFromText $ T.pack s))
  $ lookupEnv "BOX_STORE"

userEnv :: IO Text
userEnv =
  T.pack <$> (maybe getEffectiveUserName return =<< lookupEnv "BOX_USER")

identityEnv :: IO Text
identityEnv = do
  home <- getHomeDirectory
  T.pack . fromMaybe (home </> ".ssh/ambiata_rsa") <$> lookupEnv "BOX_IDENTITY"

cacheEnv :: IO FilePath
cacheEnv = do
  home <- getHomeDirectory
  fromMaybe (home </> ".ambiata/box/cache") <$> lookupEnv "BOX_CACHE"


------------------------------------------------------------------------
-- Argument Parsing

boxP :: Parser (SafeCommand BoxCommand)
boxP = safeCommand commandP

commandP :: Parser BoxCommand
commandP = subparser $
     command' "ip"
              "Get the IP address of a box."
              (BoxIP <$> queryP <*> hostTypeP)
  <> command' "ssh"
              "SSH to a box."
              (BoxSSH <$> queryP <*> many sshArgP)
  <> command' "ls"
              "List available boxes."
              (BoxList <$> (queryP <|> pure matchAll))

hostTypeP :: Parser HostType
hostTypeP =
  flag InternalHost ExternalHost $
       long "external"
    <> help "Display the external ip address rather than the internal one (which is the default)."

queryP :: Parser Query
queryP =
  argument (pOption queryParser) $
       metavar "FILTER"
    <> completer filterCompleter
    <> help "Filter using the following syntax: CLIENT[:FLAVOUR[:NAME]]"

matchAll :: Query
matchAll = Query ExactAll ExactAll InfixAll ExactAll

sshArgP :: Parser SSHArg
sshArgP =
  argument textRead $
       metavar "SSH_ARGUMENTS"
    <> help "Extra arguments to pass to ssh."

filterCompleter :: Completer
filterCompleter = mkCompleter $ \arg -> do
    boxes <- tryFetchBoxes
    return . fmap T.unpack
           $ completions (T.pack arg) boxes
  where
    tryFetchBoxes :: IO [Box]
    tryFetchBoxes = either (const []) id <$> runEitherT cachedBoxes
