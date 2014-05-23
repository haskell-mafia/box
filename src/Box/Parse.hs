module Box.Parse (
    parseQuery
  ) where

import           Box.Prelude
import           Box.Query

import           Data.Text

parseQuery :: Text -> Maybe Query
parseQuery = (?)
