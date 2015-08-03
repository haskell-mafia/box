{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Box.Query (
    query
  , match
  , matchText
  ) where

import           Box.Data

import           Data.List as L (filter)
import           Data.Text as T

import           P

import qualified System.FilePath.Glob as G


query :: Query -> [Box] -> [Box]
query q =
  L.filter (match q)

match :: Query -> Box -> Bool
match (Query qi qn qc qf) (Box bi _ bn bc bf) =
     matchText unName bn qn
  && matchText unInstanceId bi qi
  && matchText unClient bc qc
  && matchText unFlavour bf qf

matchText :: (a -> Text) -> a -> Match a -> Bool
matchText _ _ MatchAll =
  True
matchText un a (Match mq) =
  flip G.match (T.unpack $ un a) . G.compile . T.unpack . un $ mq
