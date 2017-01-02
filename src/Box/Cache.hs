{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Box.Cache (
      readCache
    , writeCache
    ) where

import           Control.Exception (IOException, handle)
import qualified Data.Text.IO      as T
import           Data.Time         (NominalDiffTime, diffUTCTime, getCurrentTime)

import           P

import           System.Directory  (createDirectoryIfMissing, getModificationTime)
import           System.FilePath   (takeDirectory)
import           System.IO         (FilePath, IO)

------------------------------------------------------------------------

readCache :: FilePath -> NominalDiffTime -> IO (Maybe Text)
readCache path timeout = handle onError $ do
    stale <- isStale path timeout
    if stale then return Nothing else Just <$> T.readFile path
  where
    onError (_ :: IOException) = return Nothing

writeCache :: FilePath -> Text -> IO ()
writeCache path content = handle onError $ do
    createDirectoryIfMissing True (takeDirectory path)
    T.writeFile path content
  where
    onError (_ :: IOException) = return ()

------------------------------------------------------------------------

isStale :: FilePath -> NominalDiffTime -> IO Bool
isStale path timeout = do
    now      <- getCurrentTime
    modified <- getModificationTime path

    let age = now `diffUTCTime` modified

    return (age > timeout)
