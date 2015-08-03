{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Box.Data (
    Box (..)
  , BoxStore (..)
  , Query (..)
  , Match (..)
  , InstanceId (..)
  , Host (..)
  , Name (..)
  , Client (..)
  , Flavour (..)
  , BoxError (..)
  , isMatch
  , queryHasMatch
  , boxesFromText
  , boxFromText
  , boxesToText
  , boxToText
  , boxParser
  , boxErrorRender
  , selectRandomHost
  ) where

import           Data.Attoparsec.Text as AP
import           Data.Text as T

import           Mismi.S3.Data

import           P

import           System.IO
import           System.Random.Shuffle


data Box =
  Box {
      boxInstance :: InstanceId
    , boxHost :: Host
    , boxName :: Name
    , boxClient :: Client
    , boxFlavour :: Flavour
    } deriving (Eq, Show)

data BoxStore =
    BoxStoreLocal FilePath
  | BoxStoreS3 Address
  deriving (Eq, Show)

data Query = Query {
    queryInstance :: Match InstanceId
  , queryName :: Match Name
  , queryClient :: Match Client
  , queryFlavour :: Match Flavour
  } deriving (Eq, Show)

data Match a =
    Match a
  | MatchAll
  deriving (Eq, Functor, Show)

newtype InstanceId =
  InstanceId {
    unInstanceId :: Text
  } deriving (Eq, Show)

newtype Host =
  Host {
    unHost :: Text
  } deriving (Eq, Show)

newtype Name =
  Name {
    unName :: Text
  } deriving (Eq, Show)

newtype Client =
  Client {
    unClient :: Text
  } deriving (Eq, Show)

newtype Flavour =
  Flavour {
    unFlavour :: Text
  } deriving (Eq, Show)

data BoxError =
    BoxNotFound BoxStore
  | BoxParseError Text
  deriving (Eq, Show)


isMatch :: Match a -> Bool
isMatch (Match _) = True
isMatch MatchAll = False

queryHasMatch :: Query -> Bool
queryHasMatch (Query i n c f) =
  isMatch i || isMatch n || isMatch c || isMatch f

boxesFromText :: Text -> Either Text [Box]
boxesFromText =
  traverse boxFromText . T.lines

boxFromText :: Text -> Either Text Box
boxFromText =
  first T.pack . parseOnly boxParser

boxesToText :: [Box] -> Text
boxesToText [] =
  ""
boxesToText bs =
  (<> "\n") . T.intercalate "\n" . fmap boxToText $ bs

boxToText :: Box -> Text
boxToText (Box (InstanceId i) (Host h) (Name n) (Client c) (Flavour f)) =
  T.intercalate " " [i, h, n, c, f]

boxParser :: Parser Box
boxParser = do
  let t = AP.takeWhile (/= ' ')
      ts = t <* string " "
  i <- InstanceId <$> ts
  h <- Host <$> ts
  n <- Name <$> ts
  c <- Client <$> ts
  f <- Flavour <$> t
  endOfInput
  pure $ Box i h n c f

boxStoreRender :: BoxStore -> Text
boxStoreRender (BoxStoreLocal f) =
  T.pack f
boxStoreRender (BoxStoreS3 a) =
  addressToText a

boxErrorRender :: BoxError -> Text
boxErrorRender (BoxNotFound bs) =
  "Could not find the box file located at: " <> boxStoreRender bs
boxErrorRender (BoxParseError e) =
  "Error parsing box file with the following error: " <> e

-- | Select a host at random so that the first box isn't the list isn't always picked again and again
selectRandomHost :: [Box] -> IO (Maybe Host)
selectRandomHost =
  fmap (fmap boxHost . listToMaybe) . shuffleM
