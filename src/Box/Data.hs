{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Box.Data (
    Software (..)
  , softwareName
  , softwareVersion
  , Group (..)
  , customer
  , role
  , Box (..)
  , group
  , name
  , tags
  , software
  ) where

import           Box.Prelude

import           Control.Lens
import           Data.Text (Text)
import           Data.Version

data Software = Software {
    _softwareName :: Text
  , _softwareVersion :: Version
  } deriving (Eq, Show)

data Group = Group {
    _customer :: Text
  , _role :: Text
  } deriving (Eq, Show)

data Box = Box {
    _group :: Group
  , _name :: Text
  , _tags :: [Text]
  , _software :: [Software]
  } deriving (Eq, Show)

makeLenses ''Software
makeLenses ''Group
makeLenses ''Box
