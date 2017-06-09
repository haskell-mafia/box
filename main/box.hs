{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

import           BuildInfo_ambiata_box
import           DependencyInfo_ambiata_box

import           Box

import           Control.Exception (bracket)
import           Control.Monad.IO.Class (MonadIO(..))

import           Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mismi (Region(..))
import qualified Mismi as Mismi
import           Mismi.Amazonka (Credentials(..))
import qualified Mismi.Amazonka as Mismi (newEnv)
import qualified Mismi.S3 as Mismi

import           P

import           System.Directory (createDirectoryIfMissing)
import           System.Directory (getHomeDirectory, getTemporaryDirectory)
import           System.Environment (lookupEnv, getArgs, getProgName, getExecutablePath)
import           System.Exit (exitSuccess)
import           System.FilePath ((</>), (<.>), dropFileName)
import           System.IO (IO, FilePath, BufferMode(..))
import           System.IO (hClose, hFlush, hIsTerminalDevice, hSetBuffering)
import           System.IO (stdout, stderr, print, openTempFile)
import           System.IO (putStrLn, hPutStrLn)
import           System.Posix.Files (setFileMode, accessModes)
import           System.Posix.Process (executeFile)
import           System.Posix.User (getEffectiveUserName)

import           Text.PrettyPrint.Boxes ((<+>))
import qualified Text.PrettyPrint.Boxes as PB

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, left, hoistEither)
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative (Mod, Parser, Completer, CommandFields)
import           X.Options.Applicative (SafeCommand(..), RunType(..))
import           X.Options.Applicative (dispatch, subparser, safeCommand, command')
import           X.Options.Applicative (hidden, short, long, flag, flag', help, value)
import           X.Options.Applicative (metavar, argument, option, pOption, textRead)
import           X.Options.Applicative (mkCompleter, completer)

------------------------------------------------------------------------
-- Types

data BoxCommand =
    BoxIP    Query HostType
  | BoxSSH   GatewayType ProcessType Query SSHType [SSHArg]
  | BoxRSH   GatewayType Query [SSHArg]
  | BoxRSync GatewayType Query [SSHArg]
  | BoxList  Query
  deriving (Eq, Show)

data HostType =
    ExternalHost
  | InternalHost
  deriving (Eq, Show)

data SSHType =
    AutoSSH
  | PlainSSH
  deriving (Eq, Show)

data ProcessType =
    ForeGround
  | BackGround
  deriving (Eq, Show)

type SSHArg = Text

data BoxCommandError =
    BoxError    BoxError
  | BoxAwsError Mismi.Error
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

    Safe DependencyCommand -> do
      mapM_ putStrLn dependencyInfo
      exitSuccess

    Safe (RunCommand r cmd) -> do
      case cmd of
        BoxIP     q host   -> runCommand env (boxIP  r q host)
        BoxSSH    g p q a args -> runCommand env (boxSSH r g p q a args)
        BoxRSH    g q args -> runCommand env (boxRSH r g q args)
        BoxRSync  g q args -> runCommand env (const $ boxRSync env r g q args)
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

boxSSH :: RunType -> GatewayType -> ProcessType -> Query -> SSHType -> [SSHArg] -> [Box] -> EitherT BoxCommandError IO ()
boxSSH runType gwType pType qTarget autoSSH args' boxes = do
  target  <- randomBoxOfQuery qTarget  boxes
  user    <- liftIO userEnv
  ident   <- liftIO identityEnv
  if match qGateway target
    then goDirect target user ident -- Use proxy
    else goJump   target user ident -- ... unless target is a gateway
  where
    qGateway = Query ExactAll (Exact $ gatewayFlavour gwType) InfixAll ExactAll
    randomGw = firstT (\case
                   BoxNoMatches -> BoxNoGateway
                   e            -> e) $ randomBoxOfQuery qGateway boxes
    boxNiceName b  = unName (boxName b) <> "." <> unInstanceId (boxInstance b)
    (autoSSHArgs, args) = setupAutoSSHArgs autoSSH pType args'
    userHost u th  = [u <> "@" <> th]
    ---
    goJump target user ident = do
      gateway <- randomGw
      let targetHost = unHost (selectHost InternalHost target)
          gatewayHost = unHost (selectHost ExternalHost gateway)
      tmpKnownHosts <- liftIO $ knownHostsFile gateway (Just target)
      go runType (boxNiceName target) $
        [ -- ProxyCommand bounces us off the gateway. Note that we pipe
          -- netcat's stderr to /dev/null to avoid the annoying 'Killed
          -- by signal 1.' message when the session finishes.
          "-o", "ProxyCommand=ssh" <> " -i " <> ident
                                   <> " -o UserKnownHostsFile=" <> tmpKnownHosts
                                   <> " " <> user <> "@" <> gatewayHost
                                   <> " nc %h %p 2>/dev/null"
        ] <> [ "-o", "UserKnownHostsFile=" <> tmpKnownHosts ]
          <> userHost user targetHost -- Connect to target
          <> args                     -- Pass on remaining args to SSH
    ---
    goDirect target user ident = do
      let targetHost = unHost (selectHost ExternalHost target)
      tmpKnownHosts <- liftIO $ knownHostsFile target Nothing
      go runType (boxNiceName target) $
        [ "-i", ident, "-o", "UserKnownHostsFile=" <> tmpKnownHosts ]
        <> userHost user targetHost <> args
    ---
    go runtype boxname args'' = case runtype of
      DryRun  ->
        liftIO $ do
          case autoSSH of
            PlainSSH ->
              (print ("ssh" : args''))
            AutoSSH ->
              (print ("autossh" : (autoSSHArgs <> args'')))

      RealRun -> do
        -- Set the title of the terminal.
        ansi <- liftIO ansiEscapesEnv
        case ansi of
          DisableANSIEscapes -> return ()
          EnableANSIEscapes  -> liftIO $ do
            T.putStr ("\ESC]0;" <> boxname <> "\BEL")
            hFlush stdout

        -- This call never returns, the current process is replaced by 'ssh'.
        case autoSSH of
          PlainSSH ->
            liftIO (exec "ssh" args'')
          AutoSSH ->
            liftIO (exec "autossh" (autoSSHArgs <> args''))

boxRSH :: RunType -> GatewayType -> Query -> [SSHArg] -> [Box] -> EitherT BoxCommandError IO ()
boxRSH runType gwType qTarget args boxes = do
  case args of
    [] ->
      left BoxMissingRSHHost
    ("":xs) ->
      boxSSH runType gwType ForeGround qTarget PlainSSH xs boxes
    (h:_) ->
      left $ BoxInvalidRSHHost h

boxRSync :: Environment -> RunType -> GatewayType -> Query -> [SSHArg] -> EitherT BoxCommandError IO ()
boxRSync env runType gwType qTarget args = do
  box <- liftIO getExecutablePath
  let
    secure = if gwType == GatewaySecure then "--secure" else ""
    envarg = case env of
               SomeEnv x -> "-e " <> x
               DefaultEnv -> ""
    args' = [ "--rsh", T.intercalate " " [T.pack box, envarg, "rsh", secure, queryRender qTarget, "--"]] <> args
  case runType of
    DryRun  ->
      liftIO (print ("rsync" : args'))
    RealRun -> do
      liftIO (exec "rsync" args')

boxList :: Query -> [Box] -> EitherT BoxCommandError IO ()
boxList q boxes = liftIO $ do
    hPutStrLn stderr ("total " <> show (length sorted))

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
    mregion <- fmap (either (const Sydney) id) $ runEitherT Mismi.getRegionFromEnv
    env     <- Mismi.newEnv mregion Discover
    store   <- liftIO storeEnv
    boxes   <- squash (Mismi.runAWS env (readBoxes store en))
    path    <- liftIO (cacheEnv en)
    liftIO $ whenM useCache (writeCache path (boxesToText boxes))
    return boxes
    where squash x = firstT BoxError    . hoistEither
                 =<< firstT BoxAwsError x

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
boxCommandErrorRender (BoxAwsError e)       = Mismi.renderError e
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
                         BoxStoreS3 . Mismi.addressFromText $ T.pack s
    in  fmap (maybe defaultBoxStore tryS3) envStore

userEnv :: IO Text
userEnv =
  T.pack <$> (maybe getEffectiveUserName return =<< lookupEnv "BOX_USER")

identityEnv :: IO Text
identityEnv = do
  home <- getHomeDirectory
  T.pack . fromMaybe (home </> ".ssh/ambiata_rsa") <$> lookupEnv "BOX_IDENTITY"

knownHostsFile :: Box -> Maybe Box -> IO Text
knownHostsFile gateway mtarget =
  maybe (tmpKnownHostsFile gateway mtarget) (return . T.pack) =<< lookupEnv "BOX_KNOWN_HOSTS"

tmpKnownHostsFile :: Box -> Maybe Box -> IO Text
tmpKnownHostsFile gateway mtarget = do
  -- Generate and populate a temporary SSH known_hosts file.
  -- The file will be created in /tmp/box/ (just to avoid poluting /tmp too
  -- much). The directory is created world RWX, but the temp file's access will
  -- be set by the users umask setting.
  -- Returns the `FilePath` of the temp file as `Text`.
  tmpdir <- (</> "box") <$> getTemporaryDirectory
  user <- getEffectiveUserName
  mkWorldAccessDir tmpdir
  fpath <- bracket (openTempFile tmpdir user) (hClose . snd) $ \(fp, hdl) -> do
                     writeHostKey hdl ExternalHost gateway
                     maybe (return ()) (writeHostKey hdl InternalHost) mtarget
                     return fp
  pure $ T.pack fpath
  where
    writeHostKey hdl htype box =
      T.hPutStrLn hdl $ unHost (selectHost htype box) <> " " <> unHostKey (boxHostKey box)

    mkWorldAccessDir :: FilePath -> IO ()
    mkWorldAccessDir fp =
        createDirectoryIfMissing True fp >> setFileMode fp accessModes


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
  ok  <- (/= "0") . fromMaybe "0" <$> lookupEnv "BOX_ANSI_ESCAPES"
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
            (BoxSSH   <$> gatewayP <*> backGroundP <*> queryP <*> autoP <*> many sshArgP)

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

autoP :: Parser SSHType
autoP =
  flag PlainSSH AutoSSH $
       long "auto"
    <> short 'a'
    <> help "Use autossh. (Requires autossh to be installed locally)"

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

matchAll :: Query
matchAll = Query ExactAll ExactAll InfixAll ExactAll

sshArgP :: Parser SSHArg
sshArgP =
  argument textRead $
       metavar "SSH_ARGUMENTS"
    <> help "Extra arguments to pass to ssh."

backGroundP :: Parser ProcessType
backGroundP =
  flag ForeGround BackGround $
     short 'b'
  <> help "Run in the background."


setupAutoSSHArgs :: SSHType -> ProcessType -> [SSHArg] -> ([SSHArg], [SSHArg])
setupAutoSSHArgs autoSSH pType sshArgs =
  let
    autoSSHArgs = [ -- We are disabling autossh's monitoring port
                    -- and choosing to use ServerAliveInterval
                    -- and ServerAliveCountMax instead
                    -- more at: https://www.everythingcli.org/ssh-tunnelling-for-fun-and-profit-autossh/
                    "-M", "0"
                  , "-o", "ServerAliveInterval 30"
                  , "-o", "ServerAliveCountMax 3"]
  in
    case (autoSSH, pType) of
      (PlainSSH, ForeGround) ->
        ([], sshArgs)
      (PlainSSH, BackGround) ->
        ([], sshArgs <> ["-f"])
      (AutoSSH, ForeGround) ->
        (autoSSHArgs, sshArgs)
      (AutoSSH, BackGround) ->
        (["-f"] <> autoSSHArgs, sshArgs)

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
          envs <- fmap (either (const []) id) $ do
            mregion <- runEitherT Mismi.getRegionFromEnv
            awsEnv  <- Mismi.newEnv (either (const Sydney) id mregion) Discover
            store   <- storeEnv
            runEitherT $ Mismi.runAWS awsEnv (listEnvironments store)
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
