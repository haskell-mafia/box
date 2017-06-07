{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Box.Store (
    readBoxes
  , writeBoxes
  , listEnvironments
  , defaultBoxStore
  ) where

import           Box.Data

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mismi.Amazonka (AWS)
import           Mismi.S3 (Address(..), Key(..), Bucket(..), WriteMode(..))
import qualified Mismi.S3 as Mismi

import           P

import           System.Directory (getDirectoryContents, doesFileExist)
import           System.FilePath ((</>), (<.>))
import           System.FilePath (takeExtension, dropExtension)
import           System.FilePath (takeFileName, dropFileName)
import           System.FilePath (takeDirectory)

import           X.Control.Monad.Trans.Either (pattern EitherT, runEitherT, hoistEither)


readBoxes :: BoxStore -> Environment -> AWS (Either BoxError [Box])
readBoxes bs env =
  with (mapM readBoxFile . unBoxFiles $ boxStoreWithEnv env bs) $ \xs ->
    fmap join $ sequence xs

readBoxFile :: BoxFile -> AWS (Either BoxError [Box])
readBoxFile bf =
  case bf of
    BoxFileLocal fp ->
      liftIO . runEitherT $ do
        t <- EitherT $ ifM (doesFileExist fp) (fmap Right $ T.readFile fp) (pure . Left $ BoxFileNotFound bf)
        hoistEither . first BoxParseError $ boxesFromText t
    BoxFileS3 a ->
      (=<<) (first BoxParseError . boxesFromText) . maybeToRight (BoxFileNotFound bf) <$> Mismi.read a

writeBoxes :: [Box] -> BoxFile -> Environment -> AWS ()
writeBoxes bs bf env = case boxFileWithEnv env bf of
  BoxFileLocal lf -> liftIO $
    ifM (doesFileExist lf) (fail $ "File already exists " <> lf) (T.writeFile lf . boxesToText $ bs)
  BoxFileS3 a ->
    void . Mismi.writeWithMode Fail a . boxesToText $ bs

listEnvironments :: BoxStore -> AWS [Text]
listEnvironments (BoxStore bfs) =
  fmap join $ mapM envNames bfs
  where
    envNames (BoxFileLocal fp) = do
      paths <- liftIO $ getDirectoryContents (takeDirectory fp)
      return [ noSuffix p | p <- paths, validEnv p ]
    envNames (BoxFileS3 (Address b k)) = do
      paths <- Mismi.list (Address b (Mismi.dirname k))
      return [ noSuffix x | p <- paths, let x = unpackKey p, validEnv x]
    noSuffix = T.pack . dropExtension . takeFileName
    validEnv = (== ".v3") . takeExtension
    unpackKey (Address _ (Key k)) = T.unpack k

defaultBoxStore :: BoxStore
defaultBoxStore =
  BoxStore [
      BoxFileS3 $ Address (Bucket "ambiata-dispensary") (Key "box/prod.v3")
    ]

-- Filepath munging for alt environments
boxStoreWithEnv :: Environment -> BoxStore -> BoxStore
boxStoreWithEnv e (BoxStore bfs) =
  BoxStore $ fmap (boxFileWithEnv e) bfs

boxFileWithEnv :: Environment -> BoxFile -> BoxFile
boxFileWithEnv env bf =
    case (env, bf) of
      (DefaultEnv, _) ->
        bf
      (SomeEnv e, BoxFileS3 (Address b (Key k))) ->
        BoxFileS3 $ Address b (Key . T.pack $ dropFileName (T.unpack k) </> T.unpack e <.> ".v3")
      (SomeEnv e, BoxFileLocal lf) ->
        BoxFileLocal $ dropFileName lf </> T.unpack e <.> ".v3"
