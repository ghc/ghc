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
--
----------------------------- FIXMEs -------------------------------------------
-- FIXME: Jeff (2022,04): This module may be completely redundant and consist of
-- duplicate code. Before we can remove it we must understand how it alters the
-- link code in the GHC.Linker directory. Thus for the time being we live with
-- it. In particular cases where we have duplicated functions in
-- GHC.Driver.Pipeline and GHC.Linker.Static, I've prefixed these with "js"
-- except for @link@ and @link'@, for example GHC.Linker.Static.linkStaticLib
-- becomes GHC.StgToJS.Linker.Dynamic.jsLinkStaticLib.
--
-- FIXME: Jeff (2022,04): In jsLinkBinary I've commented out a line that
-- dispatches to different systools based on a boolean flag. This line seems to
-- be a relic of the old ghc api but I left it in since it will require
-- attention to be verified correct. I suspect that entire function is made
-- redundant by the corresponding GHC.Linker.Static.linkBinary anyhow. Please
-- see the fixme comment in jsLinkBinary
--
-- FIXME: Jeff (2022,04): You'll notice that the APIs for the linking functions,
-- @link@, @link'@ etc are quite hairy with lots of inputs, and over half of
-- those inputs are environments of some sort including DynFlags. Of course this
-- is insanity. The API is forced due a let expression in
-- @GHC.StgToJS.Linker.Dynamic.link'@ which requires all linking functions to
-- have the same interface as GHC.Linker.Static.linkBinary. To Fix this we
-- should begin removing these environments by refining JSLinkConfig. For
-- example:
-- 1. Move any required flags from StgToJSConfig to JSLinkConfig
-- 2. Remove DynFlags by removing any opts needed for linking and add them to
--    JSLinkConfig
-- 3. Similar for HscEnv, we might need to decouple GHCs Linker from DynFlags in
--    order to have a proper api
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Dynamic
  ( jsLinkBinary
  , jsLinkLib
  ) where

import GHC.StgToJS.Linker.Archive
import GHC.StgToJS.Linker.Types
import GHC.StgToJS.Linker.Utils
import qualified GHC.StgToJS.Linker.Linker as JSLink

import GHC.Linker.Types

import GHC.Unit.Module
import GHC.Unit.Module.ModIface
import GHC.Driver.Session

import GHC.Types.Unique.DFM
import GHC.Types.Basic

import GHC.Unit.Home.ModInfo
import GHC.Unit.Env

import Prelude

import           Control.Monad

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import           Data.List ( nub )
import qualified GHC.Data.ShortText as T

import           System.FilePath
import GHC.Platform.Ways
import GHC.Utils.Logger
import GHC.StgToJS.Types
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
        (JsSource file,) . B.fromStrict <$> BS.readFile file
      objEntries <- forM (eltsUDFM hpt) $ \hmi -> do
        let mt    = T.pack . moduleNameString . moduleName . mi_module . hm_iface $ hmi
            files = maybe [] (\l -> [ o | DotO o <- linkableUnlinked l]) (hm_linkable hmi)
        -- fixme archive does not handle multiple files for a module yet
        forM files (fmap ((Object mt,) . B.fromStrict) . BS.readFile)
      B.writeFile outputFile (buildArchive meta (concat objEntries ++ jsEntries))
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
jsLinkBinary env lc_cfg cfg jsFiles logger _tmpfs dflags u_env objs dep_pkgs =
  void $ JSLink.link env lc_cfg cfg logger dflags u_env exe mempty dep_pkgs objs' jsFiles isRoot mempty
    where
      objs'    = map ObjFile objs
      isRoot _ = True
      exe      = jsExeFileName dflags
