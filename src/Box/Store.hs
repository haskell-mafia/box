{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Box.Store (
    module Mismi
  , readBoxes
  , writeBoxes
  , boxStoreAddress
  ) where

import           Box.Data

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Data.Text.IO as T

import           Mismi as Mismi hiding (InstanceId, matchAll, timeout, parser)
import           Mismi.S3 as Mismi hiding (InstanceId, matchAll, timeout, parser)

import           P

import           System.Directory


readBoxes :: BoxStore -> AWS (Either BoxError [Box])
readBoxes bs@(BoxStoreLocal fp) = liftIO . runEitherT $ do
   t <- EitherT $ ifM (doesFileExist fp) (fmap Right $ T.readFile fp) (pure . Left $ BoxNotFound bs)
   hoistEither . first BoxParseError $ boxesFromText t
readBoxes bs@(BoxStoreS3 a) =
  (=<<) (first BoxParseError . boxesFromText) . maybeToRight (BoxNotFound bs) <$> Mismi.read a

writeBoxes :: [Box] -> BoxStore -> AWS ()
writeBoxes bs (BoxStoreLocal lf) = liftIO $
  ifM (doesFileExist lf) (fail $ "File already exists " <> lf) (T.writeFile lf . boxesToText $ bs)
writeBoxes bs (BoxStoreS3 a) =
  void . Mismi.writeWithMode Fail a . boxesToText $ bs

boxStoreAddress :: Address
boxStoreAddress =
  Address (Bucket "ambiata-dispensary") (Key "box/v2")
