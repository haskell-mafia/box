{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Box.Store where

import           Box.Data
import           Box.Store

import           Mismi (AWS)

import           P

import           Test.Box.Arbitrary ()
import           Test.Mismi.S3 (testAWS, newFilePath, newAddress)
import           Test.QuickCheck


prop_readwrite_s3 bs = testAWS $ do
  path <- newAddress
  writeReadBoxes bs . BoxStoreS3 $ path

prop_readwrite_local bs = testAWS $ do
  path <- newFilePath
  writeReadBoxes bs . BoxStoreLocal . (<> "/file") $ path


writeReadBoxes :: [Box] -> BoxStore -> AWS Property
writeReadBoxes bs f = do
  writeBoxes bs f DefaultEnv
  bs' <- readBoxes f DefaultEnv
  pure $ bs' === Right bs


return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
