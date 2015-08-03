{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
import           BuildInfo_box

import           Box

import           Control.Monad.Trans.Either

import           Data.Text as T

import           Options.Applicative

import           P

import           System.IO
import           System.Environment
import           System.Exit

import           X.Options.Applicative


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch boxP >>= \sc ->
    case sc of
      VersionCommand ->
        getProgName >>= \prog -> (putStrLn $ prog <> ": " <> buildInfoVersion) >> exitSuccess
      RunCommand _ query' -> do
        unless (queryHasMatch query')
          $ hPutStrLn stderr "No filter specified" >> exitFailure
        bs <- storeEnv
        b <- orDie boxErrorRender . EitherT . runS3WithDefaults $ queryBoxes query' bs
        selectRandomHost b >>= \case
          Nothing ->
            exitFailure
          Just (Host h) ->
            putStrLn (T.unpack h) >> exitSuccess

boxP :: Parser (SafeCommand Query)
boxP =
  safeCommand queryP

queryP :: Parser Query
queryP = Query
  <$> matchParser instanceIdP
  <*> matchParser nameP
  <*> matchParser clientP
  <*> matchParser flavourP

nameP :: Parser Name
nameP =
  fmap Name . option textRead $
       long "name"
    <> short 'n'
    <> metavar "NAME_FILTER"
    <> help "Filter by the instance name"

instanceIdP :: Parser InstanceId
instanceIdP =
  fmap InstanceId . option textRead $
       long "instance"
    <> short 'i'
    <> metavar "INSTANCE_FILTER"
    <> help "Filter by the instance ID"

clientP :: Parser Client
clientP =
  fmap Client . option textRead $
       long "client"
    <> short 'c'
    <> metavar "CLIENT_FILTER"
    <> help "Filter by the instance client"

flavourP :: Parser Flavour
flavourP =
  fmap Flavour . option textRead $
       long "flavour"
    <> short 'f'
    <> metavar "FLAVOUR_FILTER"
    <> help "Filter by the instance flavour"

storeEnv :: IO BoxStore
storeEnv =
    fmap (maybe (BoxStoreS3 boxStoreAddress) (\s -> maybe (BoxStoreLocal s) BoxStoreS3 . addressFromText $ T.pack s))
  $ lookupEnv "BOX_STORE"

matchParser :: Parser a -> Parser (Match a)
matchParser =
  fmap (maybe MatchAll Match) . optional
