{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Box.Store (
    module Mismi
  , readBoxes
  , writeBoxes
  , listEnvironments
  , defaultBoxStore
  ) where

import           Box.Data

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Data.Text as T
import           Data.Text.IO as T

import           Mismi as Mismi hiding (InstanceId, matchAll, timeout, parser)
import           Mismi.S3 as Mismi hiding (InstanceId, key, matchAll, timeout, parser, (</>))

import           P

import           System.Directory
import           System.FilePath as FP

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
    envNames (BoxStoreS3 (Address b key)) = do
      paths <- Mismi.list (Address b (dirname key))
      return [ noSuffix x | p <- paths, let x = unpackKey p, validEnv x]
    noSuffix = T.pack . dropExtension . takeFileName
    validEnv = (== ".v2") . takeExtension
    unpackKey (Address _ (Key k)) = T.unpack k

defaultBoxStore :: BoxStore
defaultBoxStore =
  BoxStoreS3 (Address (Bucket "ambiata-dispensary") (Key "box/v2"))

-- Filepath munging for alt environments
boxStoreWithEnv :: BoxStore -> Environment -> BoxStore
boxStoreWithEnv bs DefaultEnv = bs
boxStoreWithEnv (BoxStoreS3 (Address b (Key k))) (SomeEnv e) =
  BoxStoreS3 (Address b (Key . T.pack $ dropFileName (T.unpack k) </> T.unpack e <.> ".v2"))
boxStoreWithEnv (BoxStoreLocal lf) (SomeEnv e) =
  BoxStoreLocal (dropFileName lf </> T.unpack e <.> ".v2")
