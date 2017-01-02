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
readBoxes bxs env = case boxStoreWithEnv bxs env of
  bs@(BoxStoreLocal fp) -> liftIO . runEitherT $ do
    t <- EitherT $ ifM (doesFileExist fp) (fmap Right $ T.readFile fp) (pure . Left $ BoxNotFound bs)
    hoistEither . first BoxParseError $ boxesFromText t
  bs@(BoxStoreS3 a) ->
    (=<<) (first BoxParseError . boxesFromText) . maybeToRight (BoxNotFound bs) <$> Mismi.read a

writeBoxes :: [Box] -> BoxStore -> Environment -> AWS ()
writeBoxes bs bxs env = case boxStoreWithEnv bxs env of
  BoxStoreLocal lf -> liftIO $
    ifM (doesFileExist lf) (fail $ "File already exists " <> lf) (T.writeFile lf . boxesToText $ bs)
  BoxStoreS3 a ->
    void . Mismi.writeWithMode Fail a . boxesToText $ bs

listEnvironments :: BoxStore -> AWS [Text]
listEnvironments bs = envNames bs
  where
    envNames (BoxStoreLocal fp) = do
      paths <- liftIO $ getDirectoryContents (takeDirectory fp)
      return [ noSuffix p | p <- paths, validEnv p ]
    envNames (BoxStoreS3 (Address b k)) = do
      paths <- Mismi.list (Address b (Mismi.dirname k))
      return [ noSuffix x | p <- paths, let x = unpackKey p, validEnv x]
    noSuffix = T.pack . dropExtension . takeFileName
    validEnv = (== ".v2") . takeExtension
    unpackKey (Address _ (Key k)) = T.unpack k

defaultBoxStore :: BoxStore
defaultBoxStore =
  BoxStoreS3 (Address (Bucket "ambiata-dispensary") (Key "box/prod.v3"))

-- Filepath munging for alt environments
boxStoreWithEnv :: BoxStore -> Environment -> BoxStore
boxStoreWithEnv bs DefaultEnv = bs
boxStoreWithEnv (BoxStoreS3 (Address b (Key k))) (SomeEnv e) =
  BoxStoreS3 (Address b (Key . T.pack $ dropFileName (T.unpack k) </> T.unpack e <.> ".v3"))
boxStoreWithEnv (BoxStoreLocal lf) (SomeEnv e) =
  BoxStoreLocal (dropFileName lf </> T.unpack e <.> ".v3")
