{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Tar
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-- Reading, writing and manipulating \"@.tar@\" archive files.
--
-----------------------------------------------------------------------------
module Distribution.Client.Tar (
  -- * @tar.gz@ operations
  createTarGzFile,
  extractTarGzFile,

  -- * Other local utils
  buildTreeRefTypeCode,
  buildTreeSnapshotTypeCode,
  isBuildTreeRefTypeCode,
  filterEntries,
  filterEntriesM,
  entriesToList,
  ) where

import qualified Data.ByteString.Lazy    as BS
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Check as Tar
import qualified Codec.Compression.GZip  as GZip
import qualified Distribution.Client.GZipUtils as GZipUtils

import Control.Exception (Exception(..), throw)

--
-- * High level operations
--

createTarGzFile :: FilePath  -- ^ Full Tarball path
                -> FilePath  -- ^ Base directory
                -> FilePath  -- ^ Directory to archive, relative to base dir
                -> IO ()
createTarGzFile tar base dir =
  BS.writeFile tar . GZip.compress . Tar.write =<< Tar.pack base [dir]

extractTarGzFile :: FilePath -- ^ Destination directory
                 -> FilePath -- ^ Expected subdir (to check for tarbombs)
                 -> FilePath -- ^ Tarball
                -> IO ()
extractTarGzFile dir expected tar =
  Tar.unpack dir . Tar.checkTarbomb expected . Tar.read
  . GZipUtils.maybeDecompress =<< BS.readFile tar

instance (Exception a, Exception b) => Exception (Either a b) where
  toException (Left  e) = toException e
  toException (Right e) = toException e

  fromException e =
    case fromException e of
      Just e' -> Just (Left e')
      Nothing -> case fromException e of
                   Just e' -> Just (Right e')
                   Nothing -> Nothing


-- | Type code for the local build tree reference entry type. We don't use the
-- symbolic link entry type because it allows only 100 ASCII characters for the
-- path.
buildTreeRefTypeCode :: Tar.TypeCode
buildTreeRefTypeCode = 'C'

-- | Type code for the local build tree snapshot entry type.
buildTreeSnapshotTypeCode :: Tar.TypeCode
buildTreeSnapshotTypeCode = 'S'

-- | Is this a type code for a build tree reference?
isBuildTreeRefTypeCode :: Tar.TypeCode -> Bool
isBuildTreeRefTypeCode typeCode
  | (typeCode == buildTreeRefTypeCode
     || typeCode == buildTreeSnapshotTypeCode) = True
  | otherwise                                  = False

filterEntries :: (Tar.Entry -> Bool) -> Tar.Entries e -> Tar.Entries e
filterEntries p =
  Tar.foldEntries
    (\e es -> if p e then Tar.Next e es else es)
    Tar.Done
    Tar.Fail

filterEntriesM :: Monad m => (Tar.Entry -> m Bool)
               -> Tar.Entries e -> m (Tar.Entries e)
filterEntriesM p =
  Tar.foldEntries
    (\entry rest -> do
         keep <- p entry
         xs   <- rest
         if keep
           then return (Tar.Next entry xs)
           else return xs)
    (return Tar.Done)
    (return . Tar.Fail)

entriesToList :: Exception e => Tar.Entries e -> [Tar.Entry]
entriesToList = Tar.foldEntries (:) [] throw

