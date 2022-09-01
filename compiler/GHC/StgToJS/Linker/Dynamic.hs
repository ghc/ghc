{-# LANGUAGE CPP                      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Dynamic
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- Various utilities for building and loading dynamic libraries, to make
-- Template Haskell work in GHCJS
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Dynamic
  ( jsLinkBinary
  , jsLinkLib
  )
where

import GHC.Driver.Session

import GHC.StgToJS.Types
import GHC.StgToJS.Linker.Archive
import GHC.StgToJS.Linker.Types
import GHC.StgToJS.Linker.Utils
import qualified GHC.StgToJS.Linker.Linker as JSLink

import GHC.Linker.Types

import GHC.Unit.Module
import GHC.Unit.Module.ModIface

import GHC.Types.Unique.DFM
import GHC.Types.Basic

import GHC.Unit.Home.ModInfo
import GHC.Unit.Env

import Prelude

import           Control.Monad

import qualified Data.ByteString as BS
import           Data.List ( nub )

import           System.FilePath
import GHC.Platform.Ways
import GHC.Utils.Logger
import GHC.Utils.TmpFs (TmpFs)

---------------------------------------------------------------------------------
-- Link libraries

jsLinkLib      :: JSLinkConfig
               -> [FilePath] -- ^ extra JS files
               -> DynFlags
               -> Logger
               -> HomePackageTable
               -> IO SuccessFlag
jsLinkLib settings jsFiles dflags _logger hpt
  | Just jsLib <- lcLinkJsLib settings = do
      let profSuff | WayProf `elem` ways dflags = "_p"
                   | otherwise                  = ""
          libFileName    = ("lib" ++ jsLib ++ profSuff) <.> "js_a"
          inOutputDir file =
            maybe file
                  (</>file)
                  (lcJsLibOutputDir settings `mplus` objectDir dflags)
          outputFile     = inOutputDir libFileName
          jsFiles' = nub (lcJsLibSrcs settings ++ jsFiles)
          meta    = Meta (opt_P dflags)
      jsEntries <- forM jsFiles' $ \file ->
        (JsSource file,) <$> BS.readFile file
      objEntries <- forM (eltsUDFM hpt) $ \hmi -> do
        let mod_name = moduleName . mi_module . hm_iface $ hmi
            files = maybe [] (\l -> [ o | DotO o <- linkableUnlinked l]) (hm_linkable hmi)
        -- fixme archive does not handle multiple files for a module yet
        forM files (fmap ((Object mod_name,)) . BS.readFile)
      writeArchive outputFile meta (concat objEntries ++ jsEntries)
      -- we don't use shared js_so libraries ourselves, but Cabal expects that we
      -- generate one when building with --dynamic-too. Just write an empty file
      when (gopt Opt_BuildDynamicToo dflags || WayDyn `elem` ways dflags) $ do
        let sharedLibFileName =
              "lib" ++ jsLib ++ "-ghcjs" ++ getCompilerVersion ++ profSuff <.> "js_so"
            sharedOutputFile = inOutputDir sharedLibFileName
        -- keep strip happy
        BS.writeFile sharedOutputFile =<< BS.readFile (topDir dflags </> "empty.o")
      return Succeeded
  | otherwise =
      return Succeeded

jsLinkBinary      :: GhcjsEnv
                  -> JSLinkConfig
                  -> StgToJSConfig
                  -> [FilePath]
                  -> Logger
                  -> TmpFs
                  -> DynFlags
                  -> UnitEnv
                  -> [FilePath]
                  -> [UnitId]
                  -> IO ()
jsLinkBinary env lc_cfg cfg jsFiles logger tmpfs dflags u_env objs dep_pkgs =
  void $ JSLink.link env lc_cfg cfg logger tmpfs dflags u_env exe mempty dep_pkgs objs' jsFiles isRoot mempty
    where
      objs'    = map ObjFile objs
      isRoot _ = True
      exe      = jsExeFileName dflags
