{-# LANGUAGE OverloadedStrings #-}
module Box.Arbitrary where

import           Box.Data
import           Box.Query
import           Control.Applicative
import           Data.Text (Text)
import           Data.Version
import           Test.QuickCheck

colours :: [Text]
colours = [
    "red"
  , "blue"
  , "green"
  , "orange"
  , "purple"
  , "pink"
  , "yellow"
  , "black"
  , "white"
  ]

muppets :: [Text]
muppets = [
    "kermit"
  , "miss piggy"
  , "statler"
  , "waldorf"
  , "chef"
  , "animal"
  , "fozzy"
  ]

brands :: [Text]
brands = [
    "dinoco"
  , "pizza-planet"
  , "als-toy-barn"
  , "buy-in-large"
  , "lightyear-tyres"
  , "re-volting-batteries"
  , "sea-meister-cameras"
  , "allinol"
  ]

roles :: [Text]
roles = [
    "lab"
  , "science"
  , "worker"
  , "experiment"
  , "sandbox"
  , "factory"
  , "bakery"
  ]

commands :: [Text]
commands = [
    "ls"
  , "cat"
  , "wc"
  , "sort"
  , "uniq"
  , "xargs"
  , "find"
  , "grep"
  , "cut"
  , "head"
  , "tail"
  ]

--instance Arbitrary Query where
--  arbitrary =

instance Arbitrary Version where
  arbitrary =
    Version <$> (listOf1 . elements) [1..20] <*> pure []

instance Arbitrary Group where
  arbitrary =
    Group <$> elements brands <*> elements roles

instance Arbitrary Software where
  arbitrary =
    Software <$> elements commands <*> arbitrary

instance Arbitrary Box where
  arbitrary =
    Box <$> arbitrary <*> elements muppets <*> (listOf . elements) colours  <*> arbitrary
