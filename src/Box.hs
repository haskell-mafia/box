{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Box where

import           Box.Prelude

import           Control.Lens
import           Data.List (filter)
import           Data.Text (Text)
import           Data.Version
import           Prelude (undefined)
import           System.IO

data Software = Software {
    _softwareName :: Text
  , _softwareVersion :: Version
  } deriving (Eq, Show)

makeLenses ''Software

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

data Group = Group {
    _customer :: Text
  , _role :: Text
  } deriving (Eq, Show)

makeLenses ''Group

data Box = Box {
    _group :: Group
  , _name :: Text
  , _tags :: [Text]
  , _software :: [Software]
  } deriving (Eq, Show)

makeLenses ''Box

data Query = Query {
    _queryGroup :: Group
  , _queryName :: Maybe Text
  , _queryQualifiers :: [Qualifier]
  } deriving (Eq, Show)

makeLenses ''Query

demo :: [Box]
demo = [
    Box (Group "ambiata" "lab") "statler" [] []
  , Box (Group "ambiata" "lab") "waldorf" [] []
  , Box (Group "ambiata" "worker") "gonzo" [] []
  ]

qualifies :: Box -> Qualifier -> Bool
qualifies b (Tag t) = any (== t) $ b ^.tags
qualifies b (Installed n c) = any (== n) $ (b ^. software) <&> (^. softwareName) -- FIX check version constraints

satisfies :: Query -> Box -> Bool
satisfies q b =
  q ^. queryGroup == b ^. group &&
  all (== b ^. name) (q ^. queryName) &&
  all (qualifies b) (q ^. queryQualifiers)

query :: Query -> [Box] -> [Box]
query q = filter (satisfies q)

main :: IO ()
main = putStrLn "box"
