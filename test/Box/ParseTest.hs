{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Box.ParseTest where

import           Test.QuickCheck
import           Box.Arbitrary
import           Box.Data
import           Box.Query
import           Box.Parse

prop_symmetricQuery :: Bool
prop_symmetricQuery =
  True

run = $quickCheckAll
