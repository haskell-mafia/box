{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Box.Query where

import           Box.Query

import           P

import           Test.Box.Arbitrary
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_query (BoxResult b q) bs =
  elem b $ query q (b : bs)

prop_match_none b q bs =
  not (match q b) ==>
  not . elem b $ query q (b : bs)

prop_match (BoxResult b q) =
  match q b


return []
tests = $quickCheckAll
