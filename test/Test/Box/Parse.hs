{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Box.Parse where

import           Test.QuickCheck
import           Test.Box.Arbitrary ()

prop_symmetricQuery :: Bool
prop_symmetricQuery = True

return []
tests = $quickCheckAll
