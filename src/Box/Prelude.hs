{-# LANGUAGE NoImplicitPrelude #-}
module Box.Prelude (
    module X
  , (?)
  ) where

import qualified Prelude as P
import           Control.Applicative as X
import           Control.Monad as X hiding (mapM, sequence, forM, mapM_, sequence_, forM_, msum)
import           Data.Eq as X
import           Data.Bool as X
import           Data.Function as X
import           Data.Maybe as X hiding (fromJust)
import           Data.Either as X
import           Data.Traversable as X
import           Data.Foldable as X
import           Text.Show as X

(?) :: a
(?) = P.undefined
