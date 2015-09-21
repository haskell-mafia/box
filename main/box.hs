{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Main (main) where

import           BuildInfo_ambiata_box

import           Box hiding ((</>))

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either

import qualified Data.Attoparsec.Text as A
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
import           System.Process hiding (runCommand, env)

import           Text.PrettyPrint.Boxes ((<+>))
import qualified Text.PrettyPrint.Boxes as PB

import           X.Options.Applicative

------------------------------------------------------------------------
-- Types

data BoxCommand =
    BoxIP   Query HostType
  | BoxSSH  Query [SSHArg]
  | BoxList Query
  | BoxPreview Query Source OpenWith
  deriving (Eq, Show)

data Source =
    LocalSource FilePath
  | S3Source Address
  deriving (Eq, Show)

data OpenWith =
    OpenApplication Text
  | DefaultEditor
  deriving (Eq, Show)

data HostType =
    ExternalHost
  | InternalHost
  deriving (Eq, Show)

type SSHArg = Text

data BoxCommandError =
    BoxError    BoxError
  | BoxAwsError Error
  | BoxNoFilter
  | BoxNoMatches


------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch boxP >>= \case

    ZshCommands cmds -> do
      forM_ cmds $ \(Command label desc _) ->
        T.putStrLn (label <> ":" <> desc)

    Safe VersionCommand -> do
      prog <- getProgName
      putStrLn (prog <> ": " <> buildInfoVersion)
      exitSuccess

    Safe (RunCommand r cmd) -> case cmd of
      BoxIP q host ->
        runCommand (boxIP  r q host)
      BoxSSH q args ->
        runCommand (boxSSH r q args)
      BoxList q ->
        runCommand (boxList q)
      BoxPreview q s a ->
        runCommand (boxPreview r q s a)

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
  args' <- generateArgs qTarget args boxes

  case runType of
    DryRun  ->
      liftIO (print ("ssh" : args'))

    RealRun -> do
      -- Set the title of the terminal.
--      liftIO (T.putStr ("\ESC]0;" <> boxNiceName target <> "\BEL"))
--      liftIO (hFlush stdout)

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

-- do it
-- box ssh :hub:thog.purple.542:i-dbd61c04 'cat file.pdf' | open -f -a /Applications/Preview.app
boxPreview :: RunType -> Query -> Source -> OpenWith -> [Box] -> EitherT BoxCommandError IO ()
boxPreview _ q s o boxes = do
  let sshargs = case s of
        LocalSource fp ->
          ["cat", T.pack fp]
        S3Source a ->
          ["s3", "cat", addressToText a]

  args <- generateArgs q sshargs boxes

  (_, Just hout, _, h) <- lift $ createProcess (proc "ssh" $ fmap T.unpack args) { std_out = CreatePipe }
  (_, _, _, h2) <- lift $ createProcess (proc "open" $ ["-f"] <> openArguments) { std_in = UseHandle hout }

  e <- lift $ waitForProcess h2
  lift $ terminateProcess h
  lift $ exitWith e
    where
      openArguments | OpenApplication a <- o
                    = ["-a", T.unpack a]
                    | DefaultEditor <- o
                    = ["-t"]
                    | otherwise
                    = []
------------------------------------------------------------------------
-- Utils

boxNiceName :: Box -> Text
boxNiceName b =
  unName (boxName b) <> "." <> unInstanceId (boxInstance b)

generateArgs :: Query -> [SSHArg] -> [Box] -> EitherT BoxCommandError IO [Text]
generateArgs qTarget args boxes = do
  let qGateway = Query ExactAll (Exact (Flavour "gateway")) InfixAll ExactAll

  target  <- randomBoxOfQuery qTarget  boxes
  gateway <- randomBoxOfQuery qGateway boxes

  let boxAlias    b = "box." <> boxNiceName b <> ".jump"

  let targetHost  = unHost (selectHost InternalHost target)
      gatewayHost = unHost (selectHost ExternalHost gateway)

  user     <- liftIO userEnv
  identity <- liftIO identityEnv

  pure $ [ -- ProxyCommand bounces us off the gateway. Note that we pipe
           -- netcat's stderr to /dev/null to avoid the annoying 'Killed
           -- by signal 1.' message when the session finishes.
           "-o", "ProxyCommand=ssh" <> " -i " <> identity
                                    <> " -o HostKeyAlias=" <> boxAlias gateway
                                    <> " " <> user <> "@" <> gatewayHost
                                    <> " nc %h %p 2>/dev/null"

           -- Keep track of host keys using the target's somewhat unique
           -- name.
           , "-o", "HostKeyAlias=" <> boxAlias target

           -- Connect to target box.
           , user <> "@" <> targetHost

           -- Pass on any additional arguments to 'ssh'.
           ] <> args



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
    -- We don't want people setting a default AWS region
    -- on their local machine, use Sydney instead.
    mregion <- getRegionFromEnv
    env     <- newEnv (fromMaybe Sydney mregion) Discover
    store   <- liftIO storeEnv
    boxes   <- squash (runAWST env (readBoxes store))
    path    <- liftIO cacheEnv
    liftIO (writeCache path boxes)
    return boxes
  where
    squash x = bimapEitherT BoxError    id . hoistEither
           =<< bimapEitherT BoxAwsError id x

randomBoxOfQuery :: MonadIO m => Query -> [Box] -> EitherT BoxCommandError m Box
randomBoxOfQuery q bs = do
  unless (queryHasMatch q) (left BoxNoFilter)
  maybe (left BoxNoMatches) return =<< liftIO (selectRandomBox (query q bs))

selectHost :: HostType -> Box -> Host
selectHost InternalHost = boxHost
selectHost ExternalHost = boxPublicHost

boxCommandErrorRender :: BoxCommandError -> Text
boxCommandErrorRender (BoxError e)    = boxErrorRender e
boxCommandErrorRender (BoxAwsError e) = errorRender e
boxCommandErrorRender (BoxNoFilter)   = "No filter specified"
boxCommandErrorRender (BoxNoMatches)  = "No matching boxes found"

exec :: Text -> [Text] -> IO a
exec cmd args =
  executeFile (T.unpack cmd) True (fmap T.unpack args) Nothing


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

boxP :: Parser (CompCommands BoxCommand)
boxP =
  commandsP boxCommands

boxCommands :: [Command BoxCommand]
boxCommands =
  [ Command "ip"  "Get the IP address of a box."
            (BoxIP   <$> queryP <*> hostTypeP)

  , Command "ssh" "SSH to a box."
            (BoxSSH  <$> queryP <*> many sshArgP)

  , Command "ls"  "List available boxes."
            (BoxList <$> (queryP <|> pure matchAll))

  , Command "preview"  "Preview a file from a box."
            (BoxPreview <$> (queryP <|> pure matchAll) <*> sourceP <*> (isTextFileP <|> applicationP))
  ]

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

sourceP :: Parser Source
sourceP =
  argument (pOption $ fmap S3Source s3Parser <|> fmap (LocalSource . T.unpack) A.takeText) $
     metavar "SOURCE"
  <> help "An s3 uri or absolute file path, i.e. s3://bucket/prefix/key"

isTextFileP :: Parser OpenWith
isTextFileP =
  flag' DefaultEditor $
     short 't'
  <> long  "text"
  <> help  "Open with default editor"
  <> hidden

applicationP :: Parser OpenWith
applicationP =
  fmap OpenApplication $
  option textRead $
     short 'a'
  <> long  "app"
  <> metavar "APP"
  <> help  "A path to an Application bundle."
  <> value "/Applications/Preview.app/"
  <> showDefault
  <> action "file"
  <> hidden

filterCompleter :: Completer
filterCompleter = mkCompleter $ \arg -> do
    boxes <- tryFetchBoxes
    return . fmap T.unpack
           $ completions (T.pack arg) boxes
  where
    tryFetchBoxes :: IO [Box]
    tryFetchBoxes = either (const []) id <$> runEitherT cachedBoxes


------------------------------------------------------------------------
-- Command Arguments
--   This can be moved to X.Options.Applicative once we get it right.

data Command a = Command {
    _cmdLabel       :: Text
  , _cmdDescription :: Text
  , _cmdParser      :: Parser a
  }

data CompCommands a =
    ZshCommands [Command a]
  | Safe (SafeCommand a)

commandsP :: [Command a] -> Parser (CompCommands a)
commandsP commands =
      ZshCommands commands <$ commandsFlag
  <|> Safe <$> safeCommand cmdP
  where
    cmdP = subparser
         . mconcat
         . fmap fromCommand
         $ commands

commandsFlag :: Parser ()
commandsFlag = flag' () (long "zsh-commands" <> hidden)

fromCommand :: Command a -> Mod CommandFields a
fromCommand (Command label description parser) =
  command' (T.unpack label) (T.unpack description) parser
