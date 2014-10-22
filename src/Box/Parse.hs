{-# LANGUAGE NoImplicitPrelude #-}
module Box.Parse (
    parseQuery
  ) where

import           Box.Data
import           Box.Prelude
import           Box.Query

import           Data.Char
import           Data.Text hiding (takeWhile)
import           Data.Attoparsec.Text

parseQuery :: Text -> Either String Query
parseQuery = parseOnly queryParser

queryParser :: Parser Query
queryParser = Query <$> groupParser <*> return Nothing <*> return []

groupParser :: Parser Group
groupParser = do
  c <- takeWhile (isAlphaNum)
  _ <- char ':'
  r <- takeWhile (isAlphaNum)
  return $ Group c r

