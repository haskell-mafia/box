{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Box.Completion (
    completions
  ) where

import           Box.Data

import           Data.List (nub)
import           Data.Text (Text)
import qualified Data.Text as T

import           P

------------------------------------------------------------------------

completions :: Text -> [Box] -> [Text]
completions arg boxes =
  case results of
    [arg'] | arg /= arg' -> completions arg' boxes
    _                    -> results
  where
    results = nub (concatMap (completionsOfBox arg) boxes)

completionsOfBox :: Text -> Box -> [Text]
completionsOfBox arg box =
  filter (arg `T.isPrefixOf`) $
  case colons arg of

    0 -> do c <- [ client ]
            return (c)

    1 -> do c <- [ client, ":" ]
            f <- [ flavour ]
            return (c <> f)

    2 -> do c <- [ client,  ":" ]
            f <- [ flavour, ":" ]
            n <- [ name ]
            return (c <> f <> n)

    _ -> []
  where
    client  = unClient  (boxClient    box) <> ":"
    flavour = unFlavour (boxFlavour   box) <> ":"
    name    = unName    (boxShortName box)

------------------------------------------------------------------------

colons :: Text -> Int
colons = count (== ':') . T.unpack
