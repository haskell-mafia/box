{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Box.Arbitrary where

import           Box.Data

import           Data.Text as T

import           Disorder.Corpus

import           P

import           Test.QuickCheck

import           Data.Bits (shiftL)

data BoxResult =
  BoxResult Box Query
  deriving (Eq, Show)

instance Arbitrary BoxResult where
  arbitrary = do
    b@(Box c f (Name n) i _ _ _) <- arbitrary

    ndrop <- choose (0, T.length n)
    ntake <- choose (0, T.length n - ndrop)
    let n' = Name (T.take ntake (T.drop ndrop n))

    q <- Query <$> genExact (pure c)
               <*> genExact (pure f)
               <*> genInfix (pure n')
               <*> genExact (pure i)

    pure $ BoxResult b q

instance Arbitrary Box where
  arbitrary = Box
    <$> arbitrary
    <*> arbitrary
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

instance Arbitrary a => Arbitrary (Exact a) where
  arbitrary = genExact arbitrary

genExact :: Gen a -> Gen (Exact a)
genExact g = frequency [
    (3, Exact <$> g)
  , (1, pure ExactAll)
  ]

instance Arbitrary a => Arbitrary (Infix a) where
  arbitrary = genInfix arbitrary

genInfix :: Gen a -> Gen (Infix a)
genInfix g = frequency [
    (3, Infix <$> g)
  , (1, pure InfixAll)
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

instance Arbitrary HostKey where
  -- real data has a trailing space character
  arbitrary = do
     hk <- show <$> choose (2 `shiftL` 200, 3 `shiftL` 300 :: Integer)
     pure $ HostKey ((pack hk) <> " ")

