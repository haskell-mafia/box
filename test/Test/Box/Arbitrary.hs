{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Box.Arbitrary where

import           Box.Data
import           Box.Query
import           Control.Applicative
import           Data.Text (Text)
import           Data.Version
import           Test.QuickCheck

tag' :: [Text]
tag' = [
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

name' :: [Text]
name' = [
    "kermit"
  , "miss piggy"
  , "statler"
  , "waldorf"
  , "chef"
  , "animal"
  , "fozzy"
  ]

customer' :: [Text]
customer' = [
    "dinoco"
  , "pizza-planet"
  , "als-toy-barn"
  , "buy-in-large"
  , "lightyear-tyres"
  , "re-volting-batteries"
  , "sea-meister-cameras"
  , "allinol"
  ]

role' :: [Text]
role' = [
    "lab"
  , "science"
  , "worker"
  , "experiment"
  , "sandbox"
  , "factory"
  , "bakery"
  ]

software' :: [Text]
software' = [
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
    Group <$> elements customer' <*> elements role'

instance Arbitrary Software where
  arbitrary =
    Software <$> elements software' <*> arbitrary

instance Arbitrary Box where
  arbitrary =
    Box <$> arbitrary <*> elements name' <*> (listOf . elements) tag'  <*> arbitrary

instance Arbitrary Qualifier where
  arbitrary = frequency [
      (4, Tag <$> elements tag')
    , (1, Installed <$> elements software' <*> arbitrary)
    ]

instance Arbitrary Constraint where
  arbitrary = oneof [
      Eq <$> arbitrary
    , Gt <$> arbitrary
    , Lt <$> arbitrary
    , arbitrary >>= \v -> pure $ Or (Gt v) (Eq v)
    , arbitrary >>= \v -> pure $ Or (Lt v) (Eq v)
    ]

instance Arbitrary Query where
  arbitrary =
    Query <$> arbitrary <*> frequency [(1, Just <$> elements name'), (4, pure Nothing)] <*> arbitrary
