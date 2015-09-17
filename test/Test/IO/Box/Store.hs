{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Box.Store where

import           Box.Data
import           Box.Store

import           P

import           Test.Box.Arbitrary ()
import           Test.Mismi.Amazonka
import           Test.QuickCheck


prop_readwrite_s3 bs = withAWS (writeReadBoxes bs . BoxStoreS3)

prop_readwrite_local bs = withLocalAWS $ \path _ -> do
  writeReadBoxes bs . BoxStoreLocal . (<> "/file") $ path


writeReadBoxes :: [Box] -> BoxStore -> AWS Property
writeReadBoxes bs f = do
  writeBoxes bs f
  bs' <- readBoxes f
  pure $ bs' === Right bs


return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
