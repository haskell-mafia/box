{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Box.Completion where

import           Box.Completion
import           Box.Data

import           Data.List (nub, null)
import           Data.Text (Text)
import qualified Data.Text as T

import           P

import           Test.Box.Arbitrary
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

------------------------------------------------------------------------

prop_none (NonNegative n) q = with arg [] $ \results ->
    null results
  where
    arg = T.take n (queryRender q)

------------------------------------------------------------------------

prop_one (NonNegative n) box = with arg [box] $ \results ->
    length results == 1
  where
    arg     = T.take n  (client <> ":" <> flavour <> ":" <> name)
    client  = unClient  (boxClient    box)
    flavour = unFlavour (boxFlavour   box)
    name    = unName    (boxShortName box)

------------------------------------------------------------------------

prop_at_least_one_match (NonNegative n) (NonEmpty boxes@(box:_)) =
    with arg boxes (\rs -> length rs >= 1)
  where
    arg = T.take n . queryRender . queryOfBox $ box

------------------------------------------------------------------------

prop_no_duplicates (NonNegative n) (NonEmpty boxes@(box:_)) =
    with arg boxes (\rs -> nub rs == rs)
  where
    arg = T.take n . queryRender . queryOfBox $ box

------------------------------------------------------------------------

with :: Testable p => Text -> [Box] -> ([Text] -> p) -> Property
with arg boxes prop =
    counterexample ("Argument    = " <> show arg) $
    counterexample ("Completions = " <> show results) $
    prop results
  where
    results = completions arg boxes

------------------------------------------------------------------------

return []
tests = $quickCheckAll
