{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Box.Cache (
      readCache
    , writeCache
    ) where

import           Box.Data

import           Control.Exception (IOException, handle)

import qualified Data.Text.IO as T
import           Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)

import           P

import           System.Directory (getModificationTime, createDirectoryIfMissing)
import           System.FilePath (takeDirectory)
import           System.IO (IO, FilePath)

------------------------------------------------------------------------

readCache :: FilePath -> NominalDiffTime -> IO (Maybe [Box])
readCache path timeout = handle onError $ do
    stale <- isStale path timeout
    case stale of
      True  -> return Nothing
      False -> do
       cache <- T.readFile path
       return (either (const Nothing) Just (boxesFromText cache))
  where
    onError (_ :: IOException) = return Nothing

writeCache :: FilePath -> [Box] -> IO ()
writeCache path boxes = handle onError $ do
    createDirectoryIfMissing True (takeDirectory path)
    T.writeFile path (boxesToText boxes)
  where
    onError (_ :: IOException) = return ()

------------------------------------------------------------------------

isStale :: FilePath -> NominalDiffTime -> IO Bool
isStale path timeout = do
    now      <- getCurrentTime
    modified <- getModificationTime path

    let age = now `diffUTCTime` modified

    return (age > timeout)
