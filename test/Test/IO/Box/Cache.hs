{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.IO.Box.Cache where

import           Box.Data
import           Box.Cache

import           Disorder.Core.IO

import           P

import           System.IO
import           System.IO.Temp

import           Test.Box.Arbitrary ()
import           Test.QuickCheck

------------------------------------------------------------------------

prop_roundtrip bs = testIO . withSystemTempDirectory "box" $ \dir ->
    roundtripCache (dir <> "/cache") bs

roundtripCache :: FilePath -> [Box] -> IO Property
roundtripCache path bs = do
  writeCache path bs
  bs' <- readCache path 60
  return (bs' === Just bs)

------------------------------------------------------------------------

return []
tests = $quickCheckAll
