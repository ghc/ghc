-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Utils
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- Various utilies used in the JS Linker
--
----------------------------- FIXMEs -------------------------------------------
--  - resolve macOS comment in @writeBinaryFile@
--  - remove redundant function @jsExeFileName@
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Utils where

import           System.FilePath
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           System.IO (withBinaryFile, IOMode(WriteMode))

import          GHC.Driver.Session
import          GHC.Settings.Config (cProjectVersion)

import          GHC.Data.ShortText
import          GHC.Unit.State
import          GHC.Unit.Types
import          GHC.Utils.Panic

import           Prelude
import GHC.Platform
import Data.List (isPrefixOf)


{-
      macOS has trouble writing more than 2GiB at once to a file
  (tested with 10.14.6), and the base library doesn't work around this
  problem yet (tested with GHC 8.6), so we work around it here.

  in this workaround we write a binary file in chunks of 1 GiB
  FIXME: Jeff (2022,03): Is this still true?
 -}
writeBinaryFile :: FilePath -> ByteString -> IO ()
writeBinaryFile file bs =
  withBinaryFile file WriteMode $ \h -> mapM_ (B.hPut h) (chunks bs)
  where
    -- split the ByteString into a nonempty list of chunks of at most 1GiB
    chunks :: ByteString -> [ByteString]
    chunks b =
      let (b1, b2) = B.splitAt 1073741824 b
      in  b1 : if B.null b1 then [] else chunks b2

getInstalledPackageLibDirs :: UnitState -> UnitId -> [FilePath]
getInstalledPackageLibDirs us = fmap unpack . maybe mempty unitLibraryDirs . lookupUnitId us

getInstalledPackageHsLibs :: UnitState -> UnitId -> [String]
getInstalledPackageHsLibs us = fmap unpack . maybe mempty unitLibraries . lookupUnitId us

tryReadShimFile :: DynFlags -> FilePath -> IO B.ByteString
tryReadShimFile = panic "tryReadShimFile: Shims not yet implemented!"

readShimsArchive :: DynFlags -> FilePath -> IO B.ByteString
readShimsArchive = panic "readShimsArchive: Shims not yet implemented!"

getCompilerVersion :: String
getCompilerVersion = cProjectVersion

jsexeExtension :: String
jsexeExtension = "jsexe"

-- FIXME: Jeff (2022,04): remove this function since it is a duplicate of
-- GHC.Linker.Static.Utils.exeFileName
jsExeFileName :: DynFlags -> FilePath
jsExeFileName dflags
  | Just s <- outputFile_ dflags =
      -- unmunge the extension
      let s' = dropPrefix "js_" (drop 1 $ takeExtension s)
                    -- FIXME: add this check when support for Windows check is added
      in if Prelude.null s' -- || (Platform.isWindows && map toLower s' == "exe")
           then dropExtension s <.> jsexeExtension
           else dropExtension s <.> s'
  | otherwise =
      if platformOS (targetPlatform dflags) == OSMinGW32
           then "main.jsexe"
           else "a.jsexe"
  where
    dropPrefix prefix xs
      | prefix `isPrefixOf` xs = drop (length prefix) xs
      | otherwise              = xs
