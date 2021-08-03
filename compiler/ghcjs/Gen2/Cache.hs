{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables, TupleSections #-}
{- |
  Simple persistent cache. Cache file is deleted if the modification time
  of any of the dependencies has changed.
 -}

module Gen2.Cache (getCached, putCached) where

import qualified Control.Exception      as E

import qualified Crypto.Hash.SHA1       as SHA1

import qualified Data.Binary            as DB
import qualified Data.Binary.Get        as DB
import qualified Data.Binary.Put        as DB
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BL
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Time.Clock.POSIX

import           System.Directory
import           System.FilePath
import           System.IO.Error

import           DynFlags
import Prelude

import qualified Compiler.Info          as Info

getCacheMeta :: [FilePath] -> IO (Maybe BL.ByteString)
getCacheMeta files = do
  m <- mapM (\file -> (file,) <$> getModified file) files
  return $ if any ((==0).snd) m then Nothing else Just (DB.runPut $ DB.put m)

checkCacheMeta :: BL.ByteString -> IO Bool
checkCacheMeta meta =
  (and :: [Bool] -> Bool) <$> mapM (\(file,mod) -> (\m -> m==mod && m/=0) <$>
    getModified file) (DB.runGet DB.get $ meta)

getModified :: FilePath -> IO Integer
getModified file =
  (round . (*1000) . utcTimeToPOSIXSeconds <$> getModificationTime file)
    `catchIOError` \_ -> return 0

cacheFileName :: DynFlags
              -> Text
              -> Text
              -> IO (Maybe FilePath)
cacheFileName _dflags prefix key = do
  let b = prefix <> "-" <> (T.decodeUtf8 . B16.encode . SHA1.hash . T.encodeUtf8 $ key)
  Info.getUserCacheDir >>= \case
    Nothing   -> return Nothing
    Just cdir -> (createDirectoryIfMissing True cdir >> return (Just $ cdir </> T.unpack b <.> "cache"))
                   `catchIOError` \_ -> return Nothing

removeCacheFile :: FilePath -> IO ()
removeCacheFile file = removeFile file `catchIOError` \_ -> return ()

getCached :: DynFlags
          -> Text
          -> Text
          -> IO (Maybe ByteString)
getCached dflags prefix name =
  let getCacheEntry =
       cacheFileName dflags prefix name >>= \case
          Nothing   -> return Nothing
          Just file -> getCacheFile file `E.onException` removeCacheFile file
      getCacheFile file = do
        dat <- BL.readFile file
        let (meta, content) = DB.runGet DB.get dat
        valid <- checkCacheMeta meta
        if valid then content `seq` return (Just content)
                 else removeCacheFile file >> return Nothing
  in  getCacheEntry `E.catch` \(_::E.SomeException) -> return Nothing

{-
  put a file in the cache, returns False if the cache file could not be created
-}
putCached :: DynFlags
          -> Text        -- prefix name, ends up in file name
          -> Text        -- unique name (may be long)
          -> [FilePath]  -- files, invalidate cache item if these are modified
          -> ByteString  -- contents
          -> IO Bool
putCached dflags prefix key deps content =
  cacheFileName dflags prefix key >>= \case
    Nothing   -> return False
    Just file ->
      getCacheMeta deps >>= \case
        Nothing -> return False
        Just meta ->
          (BL.writeFile file (DB.runPut $ DB.put (meta, content)) >> return True) `catchIOError`
            \_ -> return False
