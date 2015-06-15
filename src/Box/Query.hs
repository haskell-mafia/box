{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Box.Query (
    Qualifier (..)
  , Constraint (..)
  , Query (..)
  , queryGroup
  , queryName
  , queryQualifiers
  , qualifies
  , satisfies
  , query
  ) where

import           Box.Prelude
import           Box.Data

import           Control.Lens
import           Data.List (filter)
import           Data.Text (Text)
import           Data.Version

-- FIX add "LATEST" and other date related qualifiers
data Qualifier =
    Tag Text
  | Installed Text Constraint
  deriving (Eq, Show)

data Constraint =
    Eq Version
  | Gt Version
  | Lt Version
  | And Constraint Constraint
  | Or Constraint Constraint
  deriving (Eq, Show)

data Query = Query {
    _queryGroup :: Group
  , _queryName :: Maybe Text
  , _queryQualifiers :: [Qualifier]
  } deriving (Eq, Show)

makeLenses ''Query

qualifies :: Box -> Qualifier -> Bool
qualifies b (Tag t) = any (== t) $ b ^.tags
qualifies b (Installed n _) = any (== n) $ (b ^. software) <&> (^. softwareName) -- FIX check version constraints

satisfies :: Query -> Box -> Bool
satisfies q b =
  q ^. queryGroup == b ^. group &&
  all (== b ^. name) (q ^. queryName) &&
  all (qualifies b) (q ^. queryQualifiers)

query :: Query -> [Box] -> [Box]
query q = filter (satisfies q)
