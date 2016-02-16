{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

import           BuildInfo_ambiata_box

import           Box

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
import           System.FilePath ((</>), (<.>), dropFileName)
import           System.IO
import           System.Posix.Process
import           System.Posix.User

import           Text.PrettyPrint.Boxes ((<+>))
import qualified Text.PrettyPrint.Boxes as PB

import           X.Control.Monad.Trans.Either
import           X.Options.Applicative

------------------------------------------------------------------------
-- Types

data BoxCommand =
    BoxIP    Query HostType
  | BoxSSH   GatewayType Query SSHCommandType [SSHArg]
  | BoxRSH   GatewayType Query [SSHArg]
  | BoxRSync GatewayType Query [SSHArg]
  | BoxList  Query
  deriving (Eq, Show)

data HostType =
    ExternalHost
  | InternalHost
  deriving (Eq, Show)

data SSHCommandType =
    RegularSSH
  | AutoSSH
  deriving (Eq, Show)

sshCommand :: SSHCommandType -> Text
sshCommand RegularSSH = "ssh"
sshCommand AutoSSH = "autossh"

type SSHArg = Text

data BoxCommandError =
    BoxError    BoxError
  | BoxAwsError Error
  | BoxNoFilter
  | BoxNoMatches
  | BoxNoGateway
  | BoxMissingRSHHost
  | BoxInvalidRSHHost Text

data ANSIEscapes =
    EnableANSIEscapes
  | DisableANSIEscapes

------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  dispatch boxP >>= \(env, ccs) -> case ccs of

    ZshCommands cmds -> do
      forM_ cmds $ \(Command label desc _) ->
        T.putStrLn (label <> ":" <> desc)

    Safe VersionCommand -> do
      prog <- getProgName
      putStrLn (prog <> ": " <> buildInfoVersion)
      exitSuccess

    Safe (RunCommand r cmd) -> do
      case cmd of
        BoxIP     q host   -> runCommand env (boxIP  r q host)
        BoxSSH    g q sc args -> runCommand env (boxSSH r g q sc args)
        BoxRSH    g q args -> runCommand env (boxRSH r g q args)
        BoxRSync  g q args -> runCommand env (const $ boxRSync r g q args)
        BoxList   q        -> runCommand env (boxList q)

runCommand :: Environment -> ([Box] -> EitherT BoxCommandError IO ()) -> IO ()
runCommand env cmd = orDie boxCommandErrorRender $ do
  boxes <- fetchBoxes env
  cmd boxes
  liftIO exitSuccess


------------------------------------------------------------------------
-- Commands

boxIP :: RunType -> Query -> HostType -> [Box] -> EitherT BoxCommandError IO ()
boxIP _ q hostType boxes = do
  b <- randomBoxOfQuery q boxes
  liftIO (putStrLn . T.unpack . unHost . selectHost hostType $ b)

boxSSH :: RunType -> GatewayType -> Query -> SSHCommandType -> [SSHArg] -> [Box] -> EitherT BoxCommandError IO ()
boxSSH runType gwType qTarget sshc args boxes = do
  target  <- randomBoxOfQuery qTarget  boxes
  user    <- liftIO userEnv
  ident   <- liftIO identityEnv
  if match qGateway target
    then goDirect target user ident -- Use proxy
    else goJump   target user ident -- ... unless target is a gateway
  where
    qGateway = Query ExactAll (Exact $ gatewayFlavour gwType) InfixAll ExactAll
    randomGw = firstEitherT (\case
                   BoxNoMatches -> BoxNoGateway
                   e            -> e) $ randomBoxOfQuery qGateway boxes
    boxNiceName b  = unName (boxName b) <> "." <> unInstanceId (boxInstance b)
    boxAlias    b  = "box." <> boxNiceName b <> ".jump"
    -- Keep track of host keys using the target's somewhat unique name
    hostKeyAlias t = ["-o", "HostKeyAlias=" <> boxAlias t]
    userHost u th  = [u <> "@" <> th]
    ---
    goJump target user ident = do
      gateway <- randomGw
      let targetHost = unHost (selectHost InternalHost target)
          gatewayHost = unHost (selectHost ExternalHost gateway)
      go runType (boxNiceName target) $
        [ -- ProxyCommand bounces us off the gateway. Note that we pipe
          -- netcat's stderr to /dev/null to avoid the annoying 'Killed
          -- by signal 1.' message when the session finishes.
          "-o", "ProxyCommand=ssh" <> " -i " <> ident
                                   <> " -o HostKeyAlias=" <> boxAlias gateway
                                   <> " " <> user <> "@" <> gatewayHost
                                   <> " nc %h %p 2>/dev/null"
        ] <> hostKeyAlias target      -- SSH host alias
          <> userHost user targetHost -- Connect to target
          <> args                     -- Pass on remaining args to SSH
    ---
    goDirect target user _ident = do
      let targetHost = unHost (selectHost ExternalHost target)
      go runType (boxNiceName target) $ hostKeyAlias target <> userHost user targetHost <> args
    ---
    go runtype boxname args' = case runtype of
      DryRun  ->
        liftIO (print (sshc' : args'))

      RealRun -> do
        -- Set the title of the terminal.
        ansi <- liftIO ansiEscapesEnv
        case ansi of
          DisableANSIEscapes -> return ()
          EnableANSIEscapes  -> liftIO $ do
            T.putStr ("\ESC]0;" <> boxname <> "\BEL")
            hFlush stdout

        -- This call never returns, the current process is replaced by 'ssh'.
        liftIO (exec sshc' args')

    sshc' = sshCommand sshc

boxRSH :: RunType -> GatewayType -> Query -> [SSHArg] -> [Box] -> EitherT BoxCommandError IO ()
boxRSH runType gwType qTarget args boxes = do
  case args of
    [] ->
      left BoxMissingRSHHost
    ("":xs) ->
      boxSSH runType gwType qTarget RegularSSH xs boxes
    (h:_) ->
      left $ BoxInvalidRSHHost h

boxRSync :: RunType -> GatewayType -> Query -> [SSHArg] -> EitherT BoxCommandError IO ()
boxRSync runType gwType qTarget args = do
  box <- liftIO getExecutablePath
  let secure = if gwType == GatewaySecure then "--secure" else ""
  let args' = [ "--rsh", T.intercalate " " [T.pack box, "rsh", secure, queryRender qTarget, "--"]] <> args
  case runType of
    DryRun  ->
      liftIO (print ("rsync" : args'))
    RealRun -> do
      liftIO (exec "rsync" args')

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
  env   <- liftIO getEnvFromArgs
  ifM (liftIO useCache) (fetchCache env) (fetchBoxes env) -- Skip cache when BOX_STORE set
  where
    timeout = 60 -- seconds
    fetchCache env = do
      path  <- liftIO (cacheEnv env)
      cache <- liftIO (readCache path timeout)
      fromMaybeM (fetchBoxes env) (cache >>= rightToMaybe . boxesFromText)
    getEnvFromArgs = do
      args <- getArgs
      return $ getEnvArgs (filter (/= "--bash-completion-word") args)
    getEnvArgs = \case
      -- Grab the -e / --environment arg from inside a Completer
      -- Awful hack, but Applicative gives us no choice
      ("-e":env:_)            -> SomeEnv (T.pack env)
      ("--environment":env:_) -> SomeEnv (T.pack env)
      ("--":_)                -> DefaultEnv
      (_:xs)                  -> getEnvArgs xs
      []                      -> DefaultEnv


fetchBoxes :: Environment -> EitherT BoxCommandError IO [Box]
fetchBoxes en = do
    -- We don't want people setting a default AWS region
    -- on their local machine, use Sydney instead.
    mregion <- getRegionFromEnv
    env     <- newEnv (fromMaybe Sydney mregion) Discover
    store   <- liftIO storeEnv
    boxes   <- squash (runAWST env (readBoxes store en))
    path    <- liftIO (cacheEnv en)
    liftIO $ whenM useCache (writeCache path (boxesToText boxes))
    return boxes
    where squash x = bimapEitherT BoxError    id . hoistEither
                 =<< bimapEitherT BoxAwsError id x

useCache :: IO Bool
useCache = (==) defaultBoxStore <$> storeEnv

randomBoxOfQuery :: MonadIO m => Query -> [Box] -> EitherT BoxCommandError m Box
randomBoxOfQuery q bs = do
  unless (queryHasMatch q) (left BoxNoFilter)
  maybe (left BoxNoMatches) return =<< liftIO (selectRandomBox (query q bs))

selectHost :: HostType -> Box -> Host
selectHost InternalHost = boxHost
selectHost ExternalHost = boxPublicHost

boxCommandErrorRender :: BoxCommandError -> Text
boxCommandErrorRender (BoxError e)          = boxErrorRender e
boxCommandErrorRender (BoxAwsError e)       = errorRender e
boxCommandErrorRender (BoxNoFilter)         = "No filter specified"
boxCommandErrorRender (BoxNoMatches)        = "No matching boxes found"
boxCommandErrorRender (BoxNoGateway)        = "No usable gateways found"
boxCommandErrorRender (BoxMissingRSHHost)   = "Expected rsync to pass an argument for the hostname"
boxCommandErrorRender (BoxInvalidRSHHost _) = "Expected an empty host to be specified, i.e. box rsync <query> -- -aH local-file :remote-file."

exec :: Text -> [Text] -> IO a
exec cmd args = executeFile (T.unpack cmd) True (fmap T.unpack args) Nothing


------------------------------------------------------------------------
-- Environment Variables

storeEnv :: IO BoxStore
storeEnv =
    -- Grab BOX_STORE from $ENV if defined, try to parse it as S3
    let envStore = lookupEnv "BOX_STORE"
        tryS3 s = maybe (BoxStoreLocal s)
                         BoxStoreS3 . addressFromText $ T.pack s
    in  fmap (maybe defaultBoxStore tryS3) envStore

userEnv :: IO Text
userEnv =
  T.pack <$> (maybe getEffectiveUserName return =<< lookupEnv "BOX_USER")

identityEnv :: IO Text
identityEnv = do
  home <- getHomeDirectory
  T.pack . fromMaybe (home </> ".ssh/ambiata_rsa") <$> lookupEnv "BOX_IDENTITY"

cacheEnv :: Environment -> IO FilePath
cacheEnv env = do
  -- Cache each environment separately
  home <- getHomeDirectory
  baseCache <- fromMaybe (home </> ".ambiata/box/cache") <$> lookupEnv "BOX_CACHE"
  return $ case env of
    DefaultEnv  -> baseCache
    (SomeEnv e) -> dropFileName baseCache </> T.unpack e <.> "cache"

ansiEscapesEnv :: IO ANSIEscapes
ansiEscapesEnv = do
  tty <- liftIO (hIsTerminalDevice stdout)
  ok  <- (/= "0") . fromMaybe "1" <$> lookupEnv "BOX_ANSI_ESCAPES"
  return (if tty && ok then EnableANSIEscapes
                       else DisableANSIEscapes)

envCacheEnvFactoryBean :: IO FilePath
envCacheEnvFactoryBean = do
  -- Grab environment cache filename from BOX_CACHE in env
  baseCache <- cacheEnv DefaultEnv
  pure (dropFileName baseCache </> "environments")

------------------------------------------------------------------------
-- Argument Parsing

boxP :: Parser (Environment, CompCommands BoxCommand)
boxP = (,) <$> envP <*> commandsP boxCommands

envP :: Parser Environment
envP = option readEnv $
     metavar "ENV"
  <> help "Use an alternate box store / environment"
  <> short 'e'
  <> long "environment"
  <> completer envCompleter
  <> value DefaultEnv
  where readEnv = SomeEnv <$> textRead

boxCommands :: [Command BoxCommand]
boxCommands =
  [ Command "ip"  "Get the IP address of a box."
            (BoxIP    <$> queryP <*> hostTypeP)

  , Command "ssh" "SSH to a box."
            (BoxSSH   <$> gatewayP <*> queryP <*> sshCommandP <*> many sshArgP)

  , Command "rsh" "SSH to a box with a shell interface compatible with rsync."
            (BoxRSH   <$> gatewayP <*> queryP <*> many sshArgP )

  , Command "rsync" "Invoke rsync via box."
            (BoxRSync <$> gatewayP <*> queryP <*> many sshArgP)

  , Command "ls"  "List available boxes."
            (BoxList  <$> (queryP <|> pure matchAll))

  ]

hostTypeP :: Parser HostType
hostTypeP =
  flag InternalHost ExternalHost $
       long "external"
    <> help "Display the external ip address rather than the internal one (which is the default)."

gatewayP :: Parser GatewayType
gatewayP =
  flag Gateway GatewaySecure $
       long "secure"
    <> short 's'
    <> help "Use a 2FA-enabled gateway."

queryP :: Parser Query
queryP =
  argument (pOption queryParser) $
       metavar "FILTER"
    <> completer filterCompleter
    <> help "Filter using the following syntax: CLIENT[:FLAVOUR[:NAME[:INSTANCE-ID]]]"

sshCommandP :: Parser SSHCommandType
sshCommandP =
  flag RegularSSH AutoSSH $
       long "autossh"
    <> short 'a'
    <> help "Use the autossh to restart sessions (useful for proxying)."
  
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

envCompleter :: Completer
envCompleter = mkCompleter $ \arg -> do
  envs <- cachedEnvs
  return . fmap T.unpack $ filter ((T.pack arg) `T.isPrefixOf`) envs
  where
    cachedEnvs :: IO [Text]
    cachedEnvs = do
      cacheFile <- envCacheEnvFactoryBean
      -- Only visit cache when permitted
      useCache' <- useCache
      cache     <- valueOrZeroM useCache' (readCache cacheFile 86400) -- 24h expiration
      case cache of
        Just envs -> return (T.lines envs)
        Nothing   -> do
          -- Head off to S3 or filesystem, write out the cache
          mregion <- getRegionFromEnv
          awsEnv  <- newEnv (fromMaybe Sydney mregion) Discover
          store   <- storeEnv
          envs    <- runAWS awsEnv (listEnvironments store)
          -- Only write out cache when permitted
          when useCache' (writeCache cacheFile (T.unlines envs))
          return envs

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
