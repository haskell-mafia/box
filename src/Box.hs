{-# LANGUAGE NoImplicitPrelude #-}
module Box (
    module X
  , queryBoxes
  ) where

import           Box.Data as X
import           Box.Query as X
import           Box.Store as X

import           P


queryBoxes :: Query -> BoxStore -> S3Action (Either BoxError [Box])
queryBoxes q =
  fmap (second (query q)) . readBoxes
