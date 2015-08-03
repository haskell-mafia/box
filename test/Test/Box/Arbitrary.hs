{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Box.Arbitrary where

import           Box.Data

import           Data.Text as T

import           Disorder.Corpus

import           P

import           Test.QuickCheck


data BoxResult =
  BoxResult Box Query
  deriving (Eq, Show)

instance Arbitrary BoxResult where
  arbitrary = do
    b@(Box i _ _ n c f) <- arbitrary
    q <- Query <$> genMatch (pure i) <*> genMatch (pure n) <*> genMatch (pure c) <*> genMatch (pure f)
    pure $ BoxResult b q

instance Arbitrary Box where
  arbitrary = Box
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Query where
  arbitrary = Query
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary a => Arbitrary (Match a) where
  arbitrary = genMatch arbitrary

genMatch :: Gen a -> Gen (Match a)
genMatch g = frequency [
    (3, Match <$> g)
  , (1, pure MatchAll)
  ]

instance Arbitrary InstanceId where
  arbitrary = InstanceId <$> elements simpsons

instance Arbitrary Host where
  arbitrary =
    (\a b c d -> (Host . T.intercalate "." . fmap (T.pack . show)) [a, b, c, d])
    <$> choose (0 :: Int, 254) <*> choose (0, 254) <*> choose (0, 254) <*> choose (0, 254)

instance Arbitrary Name where
  arbitrary = Name <$> elements muppets

instance Arbitrary Client where
  arbitrary = Client <$> elements southpark

instance Arbitrary Flavour where
  arbitrary = Flavour <$> elements cooking
