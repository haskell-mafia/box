{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Box.QuerySpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Box.Data
import Box.Query

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "query" $ do
    it "returns boxes" $ property $
      \(s :: Int) -> prop_naive

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
