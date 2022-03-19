{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TupleSections      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Archive
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Sylvain Henry  <sylvain.henry@iohk.io>
--                Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
--  FIXME: Jeff(2022,04): Remove this module completely, its only consumer is
--  GHC.StgToJS.Linker.Dynamic and is likely no longer necessary with the new
--  GHC Api. I simply decided adapting this module was faster/easier than
--  removing it and figuring out GHC.StgToJS.Linker.Dynamic with the new API
-----------------------------------------------------------------------------
module GHC.StgToJS.Linker.Archive
  ( Entry(..), Index, IndexEntry(..), Meta(..)
  , buildArchive
  , readMeta, readIndex
  , readSource, readAllSources
  , readObject, withObject, withAllObjects
  ) where

import           Control.Monad

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Data
import           Data.Int
import           GHC.Data.ShortText (ShortText)
import qualified GHC.Data.ShortText as T

import           GHC.Generics hiding (Meta)

import           System.IO
import Prelude

import           GHC.Unit.Module

import           GHC.StgToJS.Object ( versionTag, versionTagLength )


-- entry, offset in data section, length
type Index = [IndexEntry]

data IndexEntry = IndexEntry { ieEntry  :: Entry
                             , ieOffset :: Int64
                             , ieLength :: Int64
                             } deriving (Show, Typeable, Generic)

instance Binary IndexEntry

data Entry = Object    ShortText -- module name
           | JsSource  FilePath
           deriving (Show, Typeable, Generic)

instance Binary Entry

data Meta = Meta { metaCppOptions :: [String]
                 } deriving (Show, Typeable, Generic)

instance Binary Meta

-- sizes of the sections in bytes
data Sections = Sections { sectionIndex :: !Word64
                         , sectionMeta  :: !Word64
                         , sectionData  :: !Word64
                         } deriving (Eq, Ord, Generic)

instance Binary Sections where
  put (Sections i m d) = putWord64le i >> putWord64le m >> putWord64le d
  get = Sections <$> getWord64le <*> getWord64le <*> getWord64le

sectionsLength :: Int
sectionsLength = 24

buildArchive :: Meta -> [(Entry, ByteString)] -> ByteString
buildArchive meta entries =
  versionTag <> sections <> index <> meta' <> entries'
  where
    bl       = fromIntegral . B.length
    sections = runPut . put $ Sections (bl index) (bl meta') (bl entries')
    meta'    = runPut (put meta)
    index    = runPut . put $ scanl1 (\(IndexEntry _ o l) (IndexEntry e _ l') -> IndexEntry e (o+l) l') $
                              map (\(e,b) -> IndexEntry e 0 (B.length b)) entries
    entries' = mconcat (map snd entries)

readMeta :: FilePath -> IO Meta
readMeta file = withBinaryFile file ReadMode $ \h -> do
  sections <- hReadHeader ("readMeta " ++ file) h
  hSeek h RelativeSeek (toInteger $ sectionIndex sections)
  m <- B.hGet h (fromIntegral $ sectionMeta sections)
  return $! runGet get m

readIndex :: FilePath -> IO Index
readIndex file =
  withArchive "readIndex" file $ \_sections index _h -> return index

readSource :: FilePath -> FilePath -> IO ByteString
readSource source file = withArchive "readSource" file $
  withEntry ("readSource " ++ file)
            ("source file " ++ source)
            selectSrc
            (\h l -> B.hGet h $ fromIntegral l)
  where
    selectSrc (JsSource src) = src == source
    selectSrc _              = False

readAllSources :: FilePath -> IO [(FilePath, ByteString)]
readAllSources file = withArchive "readAllSources" file $ \sections index h ->
  forM [ (o, l, src) | IndexEntry (JsSource src) o l <- index ] $ \(o, l, src) -> do
    hSeek h AbsoluteSeek (fromIntegral $ dataSectionStart sections + fromIntegral o)
    (src,) <$> B.hGet h (fromIntegral l)

readObject :: ModuleName -> FilePath -> IO ByteString
readObject m file = withArchive "readObject" file $
  withModuleObject ("readObject " ++ file) m (\h l -> B.hGet h $ fromIntegral l)

-- | seeks to the starting position of the object in the file
withObject :: ModuleName -> FilePath -> (Handle -> Int64 -> IO a) -> IO a
withObject m file f = withArchive "withObject" file $
  withModuleObject ("withObject " ++ file) m f


withAllObjects :: FilePath -> (ModuleName -> Handle -> Int64 -> IO a) -> IO [a]
withAllObjects file f = withArchive "withAllObjects" file $ \sections index h ->
  forM [ (o, l, mn) | IndexEntry (Object mn) o l <- index ] $ \(o, l, mn) -> do
    hSeek h AbsoluteSeek (fromIntegral $ dataSectionStart sections + fromIntegral o)
    f (mkModuleName (T.unpack mn)) h l

---------------------------------------------------------------------------------

withArchive :: String -> FilePath -> (Sections -> Index -> Handle -> IO a) -> IO a
withArchive name file f = withBinaryFile file ReadMode $ \h -> do
  let name' = name ++ " " ++ file
  putStrLn ("reading archive: " ++ name ++ " -> " ++ file)
  sections <- hReadHeader name' h
  index <- hReadIndex name' sections h
  f sections index h

-- | seeks to start of entry data in file, then runs the action
--   exactly one matching entry is expected
withEntry :: String -> String
          -> (Entry -> Bool) -> (Handle -> Int64 -> IO a)
          -> Sections -> Index -> Handle
          -> IO a
withEntry name entryName p f sections index h =
  case filter (p . ieEntry) index of
    [] -> error (name ++ ": cannot find " ++ entryName)
    [IndexEntry _ o l] -> do
      hSeek h AbsoluteSeek (dataSectionStart sections + toInteger o)
      f h (fromIntegral l)
    _ -> error (name ++ ": multiple matches for " ++ entryName)

withModuleObject :: String -> ModuleName -> (Handle -> Int64 -> IO a)
                 -> Sections -> Index -> Handle
                 -> IO a
withModuleObject name m f =
  withEntry name ("object for module " ++ ms) selectEntry f
  where
    ms = moduleNameString m
    mt = T.pack ms
    selectEntry (Object m') = mt == m'
    selectEntry _           = False

-- | expects Handle to be positioned at the start of the header
--   Handle is positioned at start of index after return
hReadHeader :: String -> Handle -> IO Sections
hReadHeader name h = do
  ts <- B.hGet h (versionTagLength + sectionsLength)
  when (B.take (fromIntegral versionTagLength) ts /= versionTag)
       (error $ name ++ ": version tag mismatch")
  return $! runGet get (B.drop (fromIntegral versionTagLength) ts)

-- | expects Handle to be positioned at the start of the index
--   Handle is positioned at start of metadata section after return
hReadIndex :: String -> Sections -> Handle -> IO Index
hReadIndex _name s h = do
  i <- B.hGet h (fromIntegral $ sectionIndex s)
  return $! runGet get i

-- start of data section in file
dataSectionStart :: Sections -> Integer
dataSectionStart s = toInteger (versionTagLength + sectionsLength)
                   + toInteger (sectionIndex s + sectionMeta s)
