{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Box.Query (
    query
  , match
  , matchExact
  , matchInfix
  ) where

import           Box.Data

import           Data.List as L (filter)
import           Data.Text as T

import           P

------------------------------------------------------------------------

query :: Query -> [Box] -> [Box]
query q =
  L.filter (match q)

match :: Query -> Box -> Bool
match (Query qc qf qn qi) (Box bc bf bn bi _ _) =
     matchExact unClient     bc qc
  && matchExact unFlavour    bf qf
  && matchInfix unName       bn qn
  && matchExact unInstanceId bi qi

matchExact :: (a -> Text) -> a -> Exact a -> Bool
matchExact takeText b = \case
  Exact q  -> takeText q == takeText b
  ExactAll -> True

matchInfix :: (a -> Text) -> a -> Infix a -> Bool
matchInfix takeText b = \case
  Infix q  -> takeText q `T.isInfixOf` takeText b
  InfixAll -> True
