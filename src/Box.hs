{-# LANGUAGE NoImplicitPrelude #-}
module Box where

import           Box.Data
import           Box.Parse
import           Box.Prelude
import           Box.Query

import           Data.List (head)
import           Data.Text (Text, pack)

import           Options.Applicative

import           System.IO


main :: IO ()
main = do
  args <- execParser opts
  case (parseQuery (queryText args)) of
    Right qry -> do
      results <- eval qry
      -- TODO Fail if singleMatch is true and list is empty
      let boxes = if singleMatch args then [head results] else results
      mapM_ print boxes

    Left m    -> putStrLn m

eval :: Query -> IO [Box]
eval _qry = (?)

opts :: ParserInfo Args
opts = info
  (Args <$>
    (switch (short 'x' <> long "single" <> help "Whether to select the first box (ie. for ssh)")) <*>
    (pack <$> argument str (metavar "QUERY" <> help "The query to run"))
    )
  (fullDesc <> progDesc "Find the ip address of a known box" <> header "box")

data Args = Args {
    singleMatch :: Bool
  , queryText   :: Text
  }
