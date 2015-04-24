{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Box.Query where

import           Test.QuickCheck
import           Box.Data
import           Box.Query

lab = Group "ambiata" "lab"
worker = Group "ambiata" "worker"

statler = Box lab "statler" [] []
waldorf = Box lab "statler" [] []
gonzo = Box worker  "gonzo" [] []

boxes :: [Box]
boxes = [
    statler
  , waldorf
  , gonzo
  ]

prop_naive :: Bool
prop_naive =
  query (Query lab Nothing []) boxes == [statler, waldorf]

return []
run = $quickCheckAll
