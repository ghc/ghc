{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE LambdaCase         #-}

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
-- Various utilies used in the JS Linker which wrap around ar.
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Archive
  ( Entry(..), Index, IndexEntry(..), Meta(..)
  , writeArchive
  , readMeta, readIndex
  , getArchiveEntries
  , getArchiveEntry
  ) where

import Prelude
import Data.ByteString (ByteString)
import Data.Word
import Control.Monad

import GHC.Unit.Module
import GHC.Utils.Binary
import GHC.Utils.Panic
import GHC.Utils.Monad
import GHC.Settings.Constants (hiVersion)


type Index = [IndexEntry]

-- | An @IndexEntry@ is a payload and an offset into the archive
data IndexEntry = IndexEntry
  { ieEntry  :: !Entry              -- ^ Entry identifier
  , ieOffset :: !(Bin ByteString)   -- ^ Offset in the archive
  } deriving (Show)

instance Binary IndexEntry where
  put_ bh (IndexEntry a b) = do
    put_ bh a
    put_ bh b
  get bh = IndexEntry <$> get bh <*> get bh

-- | An @Entry@ is either a module or a JavaScript source file.
data Entry
  = Object    !ModuleName           -- ^ A Haskell Module
  | JsSource  !FilePath             -- ^ A JS Source file
  deriving (Show)

instance Binary Entry where
  put_ bh = \case
    Object m   -> putByte bh 0 >> put_ bh m
    JsSource p -> putByte bh 1 >> put_ bh p
  get bh = getByte bh >>= \case
    0 -> Object   <$> get bh
    _ -> JsSource <$> get bh


data Meta = Meta
  { metaCppOptions :: [String]
  }

instance Binary Meta where
  put_ bh (Meta a) = put_ bh a
  get bh = Meta <$> get bh

-- | A header indicating we are in JS land.
magic :: FixedLengthEncoding Word64
magic = FixedLengthEncoding 0x435241534a434847 -- "GHCJSARC"

-- | Write payload, entries, to @FilePath@, path, using @Meta@, meta, to
-- construct the payload header.
writeArchive :: FilePath -> Meta -> [(Entry, ByteString)] -> IO ()
writeArchive path meta entries = do
  bh <- openBinMem (4*1024*1000)
  put_ bh magic
  put_ bh (show hiVersion)

  put_ bh meta

  -- forward put the index
  forwardPut_ bh (put_ bh) $ do
    idx <- forM entries $ \(e,bs) -> do
      p <- tellBin bh
      put_ bh bs
      pure $ IndexEntry
        { ieEntry  = e
        , ieOffset = p
        }
    pure idx

  writeBinMem bh path

data Header = Header
  { hdrMeta   :: !Meta
  , hdrIndex  :: !Index
  , hdrHandle :: !BinHandle
  }

-- | Given a binary handle, retrieve the header from the archive. Note this
-- function is unsafe and may panic.
getArchiveHeader :: BinHandle -> IO Header
getArchiveHeader bh = do
  is_magic <- (== magic) <$> get bh
  unless is_magic $ panic "getArchiveHeader: invalid magic header"

  is_correct_version <- ((== hiVersion) . read) <$> get bh
  unless is_correct_version $ panic "getArchiveHeader: invalid header version"

  meta <- get bh
  idx  <- forwardGet bh (get bh)
  pure $ Header
    { hdrMeta   = meta
    , hdrIndex  = idx
    , hdrHandle = bh
    }

-- | Read the meta data from an archive pointed to by 'file'.
readMeta :: FilePath -> IO Meta
readMeta file = do
  bh <- readBinMem file
  hdr <- getArchiveHeader bh
  pure $! hdrMeta hdr

-- | Read the index from an archive pointed to by 'file'.
readIndex :: FilePath -> IO Index
readIndex file = do
  bh <- readBinMem file
  hdr <- getArchiveHeader bh
  pure $! hdrIndex hdr

-- | Read the all payloads that satisfy the input predicate, 'pred', in the
-- archive pointed to by 'hdr'.
getArchiveEntries :: Header -> (Entry -> Bool) -> IO [ByteString]
getArchiveEntries hdr pred = mapMaybeM read_entry (hdrIndex hdr)
  where
    bh = hdrHandle hdr
    read_entry (IndexEntry e offset)
      | pred e  = do
          seekBin bh offset
          Just <$> get bh
      | otherwise = pure Nothing

-- | Get a single payload by searching @IndexEntry@s in 'hdr', returns the first
-- entry that satisfies the input predicate, 'pred', in the archive pointed to
-- by 'hdr'. Returns Nothing if 'pred' fails for all entries.
getArchiveEntry :: Header -> (Entry -> Bool) -> IO (Maybe ByteString)
getArchiveEntry hdr pred = go (hdrIndex hdr)
  where
    bh = hdrHandle hdr
    go = \case
      (IndexEntry e offset:es)
        | pred e    -> seekBin bh offset >> (Just <$> get bh)
        | otherwise -> go es
      []            -> pure Nothing
