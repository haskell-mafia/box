{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Box.Store where

import           Box.Data
import           Box.Store

import           P

import           Test.Box.Arbitrary ()
import           Test.Mismi.S3 (testAWS, newFilePath, newAddress)
import           Test.QuickCheck


prop_readwrite_s3 bs bs' = testAWS $ do
  path <- newAddress
  path' <- newAddress
  writeBoxes bs (BoxFileS3 path) DefaultEnv
  writeBoxes bs' (BoxFileS3 path') DefaultEnv
  bxs <- readBoxes (BoxStore [BoxFileS3 path, BoxFileS3 path']) DefaultEnv
  pure $ bxs === Right (bs <> bs')

prop_readwrite_local bs bs' = testAWS $ do
  path <- newFilePath
  path' <- newFilePath
  let
    lf = BoxFileLocal . (<> "/file") $ path
    lf' = BoxFileLocal . (<> "/file") $ path'
  writeBoxes bs lf DefaultEnv
  writeBoxes bs' lf' DefaultEnv
  bxs <- readBoxes (BoxStore [lf, lf']) DefaultEnv
  pure $ bxs === Right (bs <> bs')

return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
