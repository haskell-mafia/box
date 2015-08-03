{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Box.Store where

import           Box.Data
import           Box.Store

import           Disorder.Core.IO

import           P

import           System.IO.Temp

import           Test.Box.Arbitrary ()
import           Test.Mismi.S3
import           Test.QuickCheck


prop_readwrite_s3 t bs = testIO . runS3WithDefaults $
  withToken t (writeReadBoxes bs . BoxStoreS3)

prop_readwrite_local bs = testIO . withSystemTempDirectory "box" $
  runS3WithDefaults . writeReadBoxes bs . BoxStoreLocal . (<> "/file")


writeReadBoxes :: [Box] -> BoxStore -> S3Action Property
writeReadBoxes bs f = do
  writeBoxes bs f
  bs' <- readBoxes f
  pure $ bs' === Right bs


return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
