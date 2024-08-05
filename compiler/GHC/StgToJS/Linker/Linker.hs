{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE BlockArguments    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Linker
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- GHCJS linker, collects dependencies from the object files
-- which contain linkable units with dependency information
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Linker
  ( jsLinkBinary
  , jsLink
  , embedJsFile
  , staticInitStat
  , staticDeclStat
  , mkExportedFuns
  , mkExportedModFuns
  , computeLinkDependencies
  , LinkSpec (..)
  , LinkPlan (..)
  , emptyLinkPlan
  , incrementLinkPlan
  , ArchiveCache
  , newArchiveCache
  )
where

import GHC.Prelude

import GHC.Platform.Host (hostPlatformArchOS)

import GHC.JS.Make
import GHC.JS.Optimizer
import GHC.JS.Ident
import GHC.JS.JStg.Syntax
import GHC.JS.JStg.Monad
import qualified GHC.JS.Syntax as JS
import GHC.JS.Transform

import GHC.Driver.DynFlags (DynFlags(..))
import Language.Haskell.Syntax.Module.Name
import GHC.SysTools.Cpp
import GHC.SysTools

import GHC.Linker.Static.Utils (exeFileName)
import GHC.Linker.Types (Unlinked(..), linkableUnlinked)
import GHC.Linker.External

import GHC.StgToJS.Linker.Types
import GHC.StgToJS.Linker.Utils
import GHC.StgToJS.Linker.Opt
import GHC.StgToJS.Rts.Rts
import GHC.StgToJS.Object
import GHC.StgToJS.Types hiding (LinkableUnit)
import GHC.StgToJS.Symbols
import GHC.StgToJS.Arg
import GHC.StgToJS.Closure

import GHC.Unit.State
import GHC.Unit.Env
import GHC.Unit.Home.ModInfo
import GHC.Unit.Types
import GHC.Unit.Module (moduleStableString)

import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.BufHandle
import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Logger (Logger, logVerbAtLeast)
import GHC.Utils.Binary
import qualified GHC.Utils.Ppr as Ppr
import GHC.Utils.TmpFs

import GHC.Types.Unique.Set

import qualified GHC.SysTools.Ar          as Ar

import qualified GHC.Data.ShortText as ST
import GHC.Data.FastString

import Control.Monad

import Data.Array
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString          as BS
import Data.Function            (on)
import qualified Data.IntSet              as IS
import Data.IORef
import Data.List  ( nub, intercalate, groupBy, intersperse, sortBy)
import Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import Data.Maybe
import Data.Set                 (Set)
import qualified Data.Set                 as S
import Data.Word
import Data.Monoid

import System.IO
import System.FilePath ((<.>), (</>), dropExtension, takeDirectory)
import System.Directory ( createDirectoryIfMissing
                        , doesFileExist
                        , getCurrentDirectory
                        , Permissions(..)
                        , setPermissions
                        , getPermissions
                        )

import GHC.Unit.Finder.Types
import GHC.Unit.Finder (findObjectLinkableMaybe, findHomeModule)
import GHC.Driver.Config.Finder (initFinderOpts)

data LinkerStats = LinkerStats
  { bytesPerModule     :: !(Map Module Word64) -- ^ number of bytes linked per module
  , packedMetaDataSize :: !Word64              -- ^ number of bytes for metadata
  }

newtype ArchiveCache = ArchiveCache { loadedArchives :: IORef (Map FilePath Ar.Archive) }

newArchiveCache :: IO ArchiveCache
newArchiveCache = ArchiveCache <$> newIORef M.empty

defaultJsContext :: SDocContext
defaultJsContext = defaultSDocContext{sdocStyle = PprCode}

jsLinkBinary
  :: FinderCache
  -> JSLinkConfig
  -> StgToJSConfig
  -> Logger
  -> TmpFs
  -> DynFlags
  -> UnitEnv
  -> [FilePath]
  -> [UnitId]
  -> IO ()
jsLinkBinary finder_cache lc_cfg cfg logger tmpfs dflags unit_env hs_objs dep_units
  | lcNoJSExecutables lc_cfg = return ()
  | otherwise = do

    -- additional objects to link are passed as FileOption ldInputs...
    let cmdline_objs = [ f | FileOption _ f <- ldInputs dflags ]

    -- cmdline objects: discriminate between the 3 kinds of objects we have
    let disc hss jss ccs = \case
          []     -> pure (hss, jss, ccs)
          (o:os) -> getObjectKind o >>= \case
            Just ObjHs -> disc (o:hss) jss ccs os
            Just ObjJs -> disc hss (o:jss) ccs os
            Just ObjCc -> disc hss jss (o:ccs) os
            Nothing    -> do
              logInfo logger (vcat [text "Ignoring unexpected command-line object: ", text o])
              disc hss jss ccs os
    (cmdline_hs_objs, cmdline_js_objs, cmdline_cc_objs) <- disc [] [] [] cmdline_objs

    let
        exe         = jsExeFileName dflags
        all_hs_objs = hs_objs ++ cmdline_hs_objs
        all_js_objs = cmdline_js_objs
        all_cc_objs = cmdline_cc_objs
        is_root _   = True
                      -- FIXME: we shouldn't consider every function as a root,
                      -- but only the program entry point (main), either the
                      -- generated one or coming from an object

    -- compute dependencies
    let link_spec = LinkSpec
          { lks_unit_ids        = dep_units
          , lks_obj_root_filter = is_root
          , lks_extra_roots     = mempty
          , lks_objs_hs         = all_hs_objs
          , lks_objs_js         = all_js_objs
          , lks_objs_cc         = all_cc_objs
          }

    let finder_opts = initFinderOpts dflags
    ar_cache <- newArchiveCache

    link_plan <- computeLinkDependencies cfg unit_env link_spec finder_opts finder_cache ar_cache

    void $ jsLink lc_cfg cfg logger tmpfs ar_cache exe link_plan

-- | link and write result to disk (jsexe directory)
jsLink
     :: JSLinkConfig
     -> StgToJSConfig
     -> Logger
     -> TmpFs
     -> ArchiveCache
     -> FilePath               -- ^ output file/directory
     -> LinkPlan
     -> IO ()
jsLink lc_cfg cfg logger tmpfs ar_cache out link_plan = do

      -- create output directory
      createDirectoryIfMissing False out

      when (logVerbAtLeast logger 2) $
        logInfo logger $ hang (text "jsLink:") 2 (ppr link_plan)

      -------------------------------------------------------------
      -- link all Haskell code (program + dependencies) into out.js

      -- retrieve code for Haskell dependencies
      mods <- collectModuleCodes ar_cache link_plan

      -- LTO + rendering of JS code
      link_stats <- withBinaryFile (out </> "out.js") WriteMode $ \h ->
        renderModules h (csPrettyRender cfg) mods

      -------------------------------------------------------------

      -- dump foreign references file (.frefs)
      when (lcForeignRefs lc_cfg) $ do
        let frefsFile  = "out.frefs"
            -- frefs      = concatMap mc_frefs mods
            jsonFrefs  = mempty -- FIXME: toJson frefs

        BL.writeFile (out </> frefsFile <.> "json") jsonFrefs
        BL.writeFile (out </> frefsFile <.> "js")
                     ("h$checkForeignRefs(" <> jsonFrefs <> ");")

      -- dump stats
      unless (lcNoStats lc_cfg) $ do
        let statsFile = "out.stats"
        writeFile (out </> statsFile) (renderLinkerStats link_stats)

      -- link generated RTS parts into rts.js
      unless (lcNoRts lc_cfg) $ do
        jsm <- initJSM
        withFile (out </> "rts.js") WriteMode $ \h -> do
          let opt = jsOptimize (runJSM jsm $ jStgStatToJS <$> rts cfg)
          void $
            hPutJS (csPrettyRender cfg) h opt


      -- link user-provided JS files into lib.js
      (emcc_opts,lib_cc_objs) <- withBinaryFile (out </> "lib.js") WriteMode $ \h -> do

        let
            tmp_dir = linkerTempDir (csLinkerConfig cfg)

            -- JS objects from dependencies' archives (.a)
            go_archives emcc_opts cc_objs = \case
              []     -> pure (emcc_opts, cc_objs)
              (a:as) -> do
                Ar.Archive entries <- loadArchive ar_cache a
                (emcc_opts', cc_objs') <- go_entries emcc_opts cc_objs entries
                go_archives emcc_opts' cc_objs' as

            -- archive's entries
            go_entries emcc_opts cc_objs = \case
              []     -> pure (emcc_opts, cc_objs)
              (e:es) -> case getObjectKindBS (Ar.filedata e) of
                Just ObjHs -> do
                  -- Nothing to do. HS objects are collected in
                  -- collectModuleCodes
                  go_entries emcc_opts cc_objs es
                Just ObjCc -> do
                  -- extract the object file from the archive in a temporary
                  -- file and return its path
                  cc_obj_fn <- newTempName logger tmpfs tmp_dir TFL_CurrentModule "o"
                  B.writeFile cc_obj_fn (Ar.filedata e)
                  let cc_objs' = cc_obj_fn:cc_objs
                  go_entries emcc_opts cc_objs' es
                Just ObjJs -> do
                  -- extract the JS code and append it to the `lib.js` file
                  (opts,bs) <- parseJSObjectBS (Ar.filedata e)
                  B.hPut   h bs
                  hPutChar h '\n'
                  let emcc_opts' = emcc_opts <> opts
                  go_entries emcc_opts' cc_objs es
                Nothing -> case Ar.filename e of
                  -- JavaScript code linker does not support symbol table processing.
                  -- Currently the linker does nothing when the symbol table is met.
                  -- Ar/Ranlib usually do not create a record for the symbol table
                  -- in the object archive when the table has no entries.
                  -- For JavaScript code it should not be created by default.

                  "__.SYMDEF" ->
                    -- GNU Ar added the symbol table.

                    -- Emscripten Ar (at least 3.1.24 version)
                    -- adds it even when the symbol table is empty.
                    go_entries emcc_opts cc_objs es
                  "__.SYMDEF SORTED" ->
                    -- BSD-like Ar added the symbol table.

                    -- By default, Clang Ar does not add it when the
                    -- symbol table is empty (and it should be empty) but we left
                    -- it here to handle the case with symbol table completely
                    -- for GNU and BSD tools.
                    go_entries emcc_opts cc_objs es
                  unknown_name -> do
                    logInfo logger (vcat [text "Ignoring unexpected archive entry: ", text unknown_name])
                    go_entries emcc_opts cc_objs es

            -- additional JS objects (e.g. from the command-line)
            go_extra emcc_opts = \case
              []     -> pure emcc_opts
              (e:es) -> do
                (opts,bs) <- readJSObject e
                B.hPut h bs
                hPutChar h '\n'
                let emcc_opts' = emcc_opts <> opts
                go_extra emcc_opts' es

        -- archives
        (emcc_opts0, cc_objs) <- go_archives defaultJSOptions [] (S.toList (lkp_archives link_plan))
        -- extra object files
        emcc_opts1            <- go_extra emcc_opts0 (S.toList (lkp_objs_js link_plan))
        pure (emcc_opts1,cc_objs)


      -- Link Cc objects using emcc's linker
      --
      -- Cc objects have been extracted from archives (see above) and are listed
      -- in lib_cc_objs.
      --
      -- We don't link C sources if there are none (obviously) or if asked
      -- explicitly by the user with -ddisable-js-c-sources (mostly used for
      -- debugging purpose).
      let emcc_objs     = lib_cc_objs ++ S.toList (lkp_objs_cc link_plan)
      let has_emcc_objs = not (null emcc_objs)
      let link_c_sources = lcLinkCsources lc_cfg && has_emcc_objs

      when link_c_sources $ do

        runLink logger tmpfs (csLinkerConfig cfg) $
          [ Option "-o"
          , FileOption "" (out </> "clibs.js")
          -- Embed wasm files into a single .js file
          , Option "-sSINGLE_FILE=1"
          -- Enable support for addFunction (callbacks)
          , Option "-sALLOW_TABLE_GROWTH"
          -- keep some RTS methods and functions (otherwise removed as dead
          -- code)
          , Option ("-sEXPORTED_RUNTIME_METHODS=" ++ concat (intersperse "," (emccExportedRuntimeMethods emcc_opts)))
          , Option ("-sEXPORTED_FUNCTIONS=" ++ concat (intersperse "," (emccExportedFunctions emcc_opts)))
          ]
          -- pass extra options from JS files' pragmas
          ++ map Option (emccExtraOptions emcc_opts)
          -- link objects
          ++ map (FileOption "") emcc_objs

      -- Don't enable the Emcc rts when not needed (i.e. no Wasm module to link
      -- with) and not forced by the caller (e.g. in the future iserv may require
      -- incremental linking of Wasm modules, hence the emcc rts even building
      -- iserv itself doesn't require the emcc rts)
      let use_emcc_rts = UseEmccRts $ link_c_sources || lcForceEmccRts lc_cfg


      -- link everything together into a runnable all.js
      -- only if we link a complete application,
      --   no incremental linking and no skipped parts
      when (lcCombineAll lc_cfg && not (lcNoRts lc_cfg)) $ do
        writeRunMain out use_emcc_rts
        _ <- combineFiles lc_cfg link_c_sources out
        writeHtml    out
        writeRunner lc_cfg out
        writeExterns out

data LinkSpec = LinkSpec
  { lks_unit_ids        :: [UnitId]
  , lks_obj_root_filter :: ExportedFun -> Bool -- ^ Predicate for exported functions in objects to declare as root
  , lks_extra_roots     :: Set ExportedFun -- ^ Extra root functions from loaded units
  , lks_objs_hs         :: [FilePath]      -- ^ HS objects to link
  , lks_objs_js         :: [FilePath]      -- ^ JS objects to link
  , lks_objs_cc         :: [FilePath]      -- ^ Cc objects to link
  }

instance Outputable LinkSpec where
  ppr s = hang (text "LinkSpec") 2 $ vcat
            [ hcat [text "Unit ids: ", ppr (lks_unit_ids s)]
            , hcat [text "HS objects:", vcat (fmap text (lks_objs_hs s))]
            , hang (text "JS objects::") 2 (vcat (fmap text (lks_objs_js s)))
            , hang (text "Cc objects::") 2 (vcat (fmap text (lks_objs_cc s)))
            , text "Object root filter: <function>"
            , hcat [text "Extra roots: ", ppr (lks_extra_roots s)]
            ]

emptyLinkPlan :: LinkPlan
emptyLinkPlan = LinkPlan
  { lkp_block_info = mempty
  , lkp_dep_blocks = mempty
  , lkp_archives   = mempty
  , lkp_objs_js    = mempty
  , lkp_objs_cc    = mempty
  }

-- | Given a `base` link plan (assumed to be already linked) and a `new` link
-- plan, compute `(diff, total)` link plans.
--
-- - `diff` is the incremental link plan to get from `base` to `total`
-- - `total` is the total link plan as if `base` and `new` were linked at once
incrementLinkPlan :: LinkPlan -> LinkPlan -> (LinkPlan, LinkPlan)
incrementLinkPlan base new = (diff,total)
  where
    total = LinkPlan
      { lkp_block_info = M.union (lkp_block_info base) (lkp_block_info new)
      , lkp_dep_blocks = S.union (lkp_dep_blocks base) (lkp_dep_blocks new)
      , lkp_archives   = S.union (lkp_archives base) (lkp_archives new)
      , lkp_objs_js    = S.union (lkp_objs_js base) (lkp_objs_js new)
      , lkp_objs_cc    = S.union (lkp_objs_cc base) (lkp_objs_cc new)
      }
    diff = LinkPlan
      { lkp_block_info = lkp_block_info new -- block info from "new" contains all we need to load new blocks
      , lkp_dep_blocks = S.difference (lkp_dep_blocks new) (lkp_dep_blocks base)
      , lkp_archives   = S.difference (lkp_archives new)   (lkp_archives base)
      , lkp_objs_js    = S.difference (lkp_objs_js new)    (lkp_objs_js base)
      , lkp_objs_cc    = S.difference (lkp_objs_cc new)    (lkp_objs_cc base)
      }


computeLinkDependencies
  :: StgToJSConfig
  -> UnitEnv
  -> LinkSpec
  -> FinderOpts
  -> FinderCache
  -> ArchiveCache
  -> IO LinkPlan
computeLinkDependencies cfg unit_env link_spec finder_opts finder_cache ar_cache = do

  let units       = lks_unit_ids        link_spec
  let hs_objs     = lks_objs_hs         link_spec
  let js_objs     = lks_objs_js         link_spec
  let cc_objs     = lks_objs_cc         link_spec
  let extra_roots = lks_extra_roots     link_spec
  let obj_is_root = lks_obj_root_filter link_spec

  -- Process:
  -- 1) Find new required linkables (object files, libraries, etc.) for all
  -- transitive dependencies
  -- 2) Load ObjBlockInfo from them and cache them
  -- 3) Compute ObjBlock dependencies and return the link plan

  -- TODO (#23013): currently we directly compute the ObjBlock dependencies and
  -- find/load linkable on-demand when a module is missing.


  (objs_block_info, objs_required_blocks) <- loadObjBlockInfo hs_objs

  let obj_roots = S.fromList . filter obj_is_root $ concatMap (M.keys . bi_exports . lbi_info) (M.elems objs_block_info)
      obj_units = map moduleUnitId $ nub (M.keys objs_block_info)

  let (rts_wired_units, rts_wired_functions) = rtsDeps

  -- all the units we want to link together, without their dependencies
  let root_units = filter (/= ue_currentUnit unit_env)
                   $ filter (/= interactiveUnitId)
                   $ nub
                   $ rts_wired_units ++ reverse obj_units ++ reverse units

  -- all the units we want to link together, including their dependencies,
  -- preload units, and backpack instantiations
  all_units_infos <- mayThrowUnitErr (preloadUnitsInfo' unit_env root_units)

  let all_units = fmap unitId all_units_infos

  dep_archives <- getPackageArchives cfg unit_env all_units
  (archives_block_info, archives_required_blocks) <- loadArchiveBlockInfo ar_cache dep_archives

  -- compute dependencies
  let block_info      = objs_block_info `M.union` archives_block_info
      dep_fun_roots   = obj_roots `S.union` rts_wired_functions `S.union` extra_roots

  -- read transitive dependencies
  new_required_blocks_var <- newIORef []
  let load_info mod = do
        -- Adapted from the tangled code in GHC.Linker.Loader.getLinkDeps.
        linkable <- case lookupHugByModule mod (ue_home_unit_graph unit_env) of
          Nothing ->
                -- It's not in the HPT because we are in one shot mode,
                -- so use the Finder to get a ModLocation...
              case ue_homeUnit unit_env of
                Nothing -> pprPanic "getDeps: No home-unit: " (pprModule mod)
                Just home_unit -> do
                    mb_stuff <- findHomeModule finder_cache finder_opts home_unit (moduleName mod)
                    case mb_stuff of
                      Found loc mod -> found loc mod
                      _ -> pprPanic "getDeps: Couldn't find home-module: " (pprModule mod)
                where
                    found loc mod = do {
                      mb_lnk <- findObjectLinkableMaybe mod loc ;
                      case mb_lnk of {
                          Nothing  -> pprPanic "getDeps: Couldn't find linkable for module: " (pprModule mod) ;
                          Just lnk -> pure lnk
                      }}

          Just mod_info -> case homeModInfoObject mod_info of
            Nothing  -> pprPanic "getDeps: Couldn't find object file for home-module: " (pprModule mod)
            Just lnk -> pure lnk

        case linkableUnlinked linkable of
              [DotO p] -> do
                  (bis, req_b) <- loadObjBlockInfo [p]
                  -- Store new required blocks in IORef
                  modifyIORef new_required_blocks_var ((++) req_b)
                  case M.lookup mod bis of
                    Nothing -> pprPanic "getDeps: Didn't load any block info for home-module: " (pprModule mod)
                    Just bi -> pure bi
              ul -> pprPanic "getDeps: Unrecognized linkable for home-module: "
                      (vcat [ pprModule mod
                            , ppr ul])

  -- required blocks have no dependencies, so don't have to use them as roots in
  -- the traversal
  (updated_block_info, transitive_deps) <- getDeps block_info load_info dep_fun_roots mempty

  new_required_blocks <- readIORef new_required_blocks_var
  let required_blocks = S.fromList $ mconcat
        [ archives_required_blocks
        , objs_required_blocks
        , new_required_blocks
        ]

  let all_deps = S.union transitive_deps required_blocks

  let plan = LinkPlan
        { lkp_block_info = updated_block_info
        , lkp_dep_blocks = all_deps
        , lkp_archives   = S.fromList dep_archives
        , lkp_objs_js    = S.fromList js_objs
        , lkp_objs_cc    = S.fromList cc_objs
        }

  return plan


-- | Compiled module
data ModuleCode = ModuleCode
  { mc_module   :: !Module
  , mc_js_code  :: !JS.JStat
  , mc_exports  :: !B.ByteString        -- ^ rendered exports
  , mc_closures :: ![ClosureInfo]
  , mc_statics  :: ![StaticInfo]
  , mc_frefs    :: ![ForeignJSRef]
  }

-- | ModuleCode after link with other modules.
--
-- It contains less information than ModuleCode because they have been commoned
-- up into global "metadata" for the whole link.
data CompactedModuleCode = CompactedModuleCode
  { cmc_module  :: !Module
  , cmc_js_code :: !JS.JStat
  , cmc_exports :: !B.ByteString        -- ^ rendered exports
  }

-- | Output JS statements and return the output size in bytes.
hPutJS :: Bool -> Handle -> JS.JStat -> IO Integer
hPutJS render_pretty h = \case
  JS.BlockStat [] -> pure 0
  x                -> do
    before <- hTell h
    if render_pretty
      then do
        printSDoc defaultJsContext (Ppr.PageMode True) h (pretty render_pretty x)
      else do
        bh <- newBufHandle h
        bPutHDoc bh defaultJsContext (line $ pretty render_pretty x)
        bFlush bh
    -- Append an empty line to correctly end the file in a newline
    hPutChar h '\n'
    after <- hTell h
    pure $! (after - before)

-- | Link modules and pretty-print them into the given Handle
renderModules
  :: Handle
  -> Bool         -- ^ should we render readable JS for debugging?
  -> [ModuleCode] -- ^ linked code per module
  -> IO LinkerStats
renderModules h render_pretty mods = do

  -- link modules
  let (compacted_mods, meta) = linkModules mods

  let
    putJS   = hPutJS render_pretty h

  ---------------------------------------------------------
  -- Pretty-print JavaScript code for all the dependencies.
  --
  -- We have to pretty-print at link time because we want to be able to perform
  -- global link-time optimisations (e.g. renamings) on the whole generated JS
  -- file.

  -- modules themselves
  mod_sizes <- forM compacted_mods $ \m -> do

    !mod_size <- fromIntegral <$> (putJS $ cmc_js_code m)
    let !mod_mod  = cmc_module m
    pure (mod_mod, mod_size)

  -- commoned up metadata
  let meta_opt = jsOptimize meta
  !meta_length <- fromIntegral <$> putJS meta_opt

  -- module exports
  mapM_ (B.hPut h . cmc_exports) compacted_mods

  -- stats
  let !link_stats = LinkerStats
        { bytesPerModule     = M.fromList mod_sizes
        , packedMetaDataSize = meta_length
        }

  pure link_stats

-- | Render linker stats
renderLinkerStats :: LinkerStats -> String
renderLinkerStats s =
  intercalate "\n\n" [meta_stats, package_stats, module_stats] <> "\n\n"
  where
    meta = packedMetaDataSize s
    meta_stats = "number of modules: " <> show (length bytes_per_mod)
                 <> "\npacked metadata:   " <> show meta

    bytes_per_mod = M.toList $ bytesPerModule s

    show_unit (UnitId fs) = unpackFS fs

    ps :: Map UnitId Word64
    ps = M.fromListWith (+) . map (\(m,s) -> (moduleUnitId m,s)) $ bytes_per_mod

    pad :: Int -> String -> String
    pad n t = let l = length t
              in  if l < n then t <> replicate (n-l) ' ' else t

    pkgMods :: [[(Module,Word64)]]
    pkgMods = groupBy ((==) `on` (moduleUnitId . fst)) bytes_per_mod

    showMod :: (Module, Word64) -> String
    showMod (m,s) = pad 40 ("    " <> moduleStableString m <> ":") <> show s <> "\n"

    package_stats :: String
    package_stats = "code size summary per package (in bytes):\n\n"
                     <> concatMap (\(p,s) -> pad 25 (show_unit p <> ":") <> show s <> "\n") (M.toList ps)

    module_stats :: String
    module_stats = "code size per module (in bytes):\n\n" <> unlines (map (concatMap showMod) pkgMods)


getPackageArchives :: StgToJSConfig -> UnitEnv -> [UnitId] -> IO [FilePath]
getPackageArchives cfg unit_env units =
  filterM doesFileExist [ ST.unpack p </> "lib" ++ ST.unpack l ++ profSuff <.> "a"
                        | u <- units
                        , p <- getInstalledPackageLibDirs ue_state u
                        , l <- getInstalledPackageHsLibs  ue_state u
                        ]
  where
    ue_state = ue_units unit_env

    -- XXX the profiling library name is probably wrong now
    profSuff | csProf cfg = "_p"
             | otherwise  = ""


-- | Combine rts.js, lib.js, out.js to all.js that can be run
-- directly with node.js or SpiderMonkey jsshell
combineFiles :: JSLinkConfig
             -> Bool -- has clibs.js
             -> FilePath
             -> IO ()
combineFiles cfg has_clibs fp = do
  let files = map (fp </>) $ catMaybes
        [ Just "rts.js"
        , Just "lib.js"
        , Just "out.js"
        , if has_clibs      then Just "clibs.js" else Nothing
        , if lcNoHsMain cfg then Nothing else Just "runmain.js"
        ]
  withBinaryFile (fp </> "all.js") WriteMode $ \h ->
    forM_ files $ \i ->
      B.readFile i >>= B.hPut h

-- | write the index.html file that loads the program if it does not exit
writeHtml
  :: FilePath -- ^ output directory
  -> IO ()
writeHtml out = do
  let htmlFile = out </> "index.html"
  e <- doesFileExist htmlFile
  unless e $
    B.writeFile htmlFile templateHtml


templateHtml :: B.ByteString
templateHtml =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \  <head>\n\
  \  </head>\n\
  \  <body>\n\
  \  </body>\n\
  \  <script language=\"javascript\" src=\"all.js\" defer></script>\n\
  \</html>"

-- | write the runmain.js file that will be run with defer so that it runs after
-- index.html is loaded
writeRunMain
  :: FilePath -- ^ output directory
  -> UseEmccRts
  -> IO ()
writeRunMain out use_emcc_rts = do
  let runMainFile = out </> "runmain.js"
  B.writeFile runMainFile (runMainJS use_emcc_rts)

newtype UseEmccRts = UseEmccRts Bool

runMainJS :: UseEmccRts -> B.ByteString
runMainJS (UseEmccRts use_emcc_rts) = if use_emcc_rts
  then "Module['onRuntimeInitialized'] = function() {\n\
       \h$initEmscriptenHeap();\n\
       \h$main(h$mainZCZCMainzimain);\n\
       \}\n"
  else "h$main(h$mainZCZCMainzimain);\n"

writeRunner :: JSLinkConfig -- ^ Settings
            -> FilePath     -- ^ Output directory
            -> IO ()
writeRunner _settings out = do
  cd    <- getCurrentDirectory
  let arch_os = hostPlatformArchOS
  let runner  = cd </> exeFileName arch_os False (Just (dropExtension out))
      srcFile = out </> "all" <.> "js"
      nodePgm :: B.ByteString
      nodePgm = "node"
  src <- B.readFile (cd </> srcFile)
  B.writeFile runner ("#!/usr/bin/env " <> nodePgm <> "\n" <> src)
  perms <- getPermissions runner
  setPermissions runner (perms {executable = True})

rtsExterns :: FastString
rtsExterns =
  -- Google Closure Compiler --externs option is deprecated.
  -- Need pass them as a js file with @externs module-level jsdoc.
  "/** @externs @suppress {duplicate} */\n" <>
  "// GHCJS RTS externs for closure compiler ADVANCED_OPTIMIZATIONS\n\n" <>
  mconcat
    -- See GHC.StgToJS
    -- We connect all payload fields "dXX" on JavaScript Object.
    -- That's most simple way to make Google Closure Compiler prevent
    -- property names mangling.
    (map (\x -> "/** @type {*} */\nObject.d" <> mkFastString (show x) <> ";\n")
    [(1::Int)..16384]) <>
  mconcat
    (map (\x -> "/** @type {*} */\nObject." <> x <> ";\n")
    -- We do same for special STG properties as well.
    ["m", "f", "cc", "t", "size", "i", "n", "a", "r", "s"]) <>
  mconcat
    [ -- Used at h$mkForeignCallback
      "/** @type {*} */\nObject.mv;\n"
    ] <>
  mconcat
    (map (\x -> x <> ";\n")
    [ -- Externs needed by node environment
      "/** @type {string} */ var __dirname"
      -- Copied minimally from https://github.com/externs/nodejs/blob/6c6882c73efcdceecf42e7ba11f1e3e5c9c041f0/v8/nodejs.js#L8
    , "/** @const */ var NodeJS = {}"
      -- NodeJS Stream interface
    , "/** @interface */ NodeJS.Stream = function () {}"
    , "/** @template THIS @this {THIS} @return {THIS} */ NodeJS.Stream.prototype.on = function() {}"
    , "/** @return {boolean} */ NodeJS.Stream.prototype.write = function() {}"
      -- NodeJS versions property contains actual versions of the environment
    , "/** @interface */ NodeJS.ProcessVersions = function() {}"
    , "/** @type {string} */ NodeJS.ProcessVersions.prototype.node"
      -- NodeJS Process interface
    , "/** @interface */ NodeJS.Process = function() {}"
    , "/** @type {!NodeJS.Stream} */ NodeJS.Process.prototype.stderr"
    , "/** @type {!NodeJS.Stream} */ NodeJS.Process.prototype.stdin"
    , "/** @type {!NodeJS.Stream} */ NodeJS.Process.prototype.stdout"
    , "/** @type {!NodeJS.ProcessVersions} */ NodeJS.Process.prototype.versions"
    , "/** @return {?} */ NodeJS.Process.prototype.exit = function() {}"
    , "/** @type {!Array<string>} */ NodeJS.Process.prototype.argv"
      -- NodeJS Process definition
    , "/** @type {!NodeJS.Process} */ var process"
      -- NodeJS Buffer class
    , "/** @extends {Uint8Array} @constructor */ function Buffer(arg1, encoding) {}"
    , "/** @return {!Buffer} */ Buffer.alloc = function() {}"
      -- Emscripten Module
      -- Emscripten RTS's definitions we use in mem.js to support C sources.
      -- When we link with emcc the actual definitions are linked, but when we
      -- don't use C sources we don't use emcc and these variables are detected
      -- as undefined.
    , "/** @type {*} */ var Module"
    , "/** @type {!Int8Array} */ Module.HEAP8"
    , "/** @type {!Uint8Array} */ Module.HEAPU8"
    , "/** @return {number} */ Module.getEmptyTableSlot = function() {}"
    , "/** @return {*} */ Module._free = function() {}"
    , "/** @return {*} */ Module._malloc = function() {}"
      -- Mozilla's Narcissus (JS in JS interpreter implemented on top of SpiderMonkey) environment
    , "/** @type {*} */ var putstr"
    , "/** @type {*} */ var printErr"
      -- Apples's JavaScriptCore environment
    , "/** @type {*} */ var debug"
    ])

writeExterns :: FilePath -> IO ()
writeExterns out = writeFile (out </> "all.externs.js")
  $ unpackFS rtsExterns

-- | Get all block dependencies for a given set of roots
--
-- Returns the updated block info map and the blocks.
getDeps :: Map Module LocatedBlockInfo     -- ^ Block info per module
        -> (Module -> IO LocatedBlockInfo) -- ^ Used to load block info if missing
        -> Set ExportedFun                 -- ^ start here
        -> Set BlockRef                    -- ^ and also link these
        -> IO (Map Module LocatedBlockInfo, Set BlockRef)
getDeps init_infos load_info root_funs root_blocks = traverse_funs init_infos S.empty root_blocks (S.toList root_funs)
  where
    -- A block may depend on:
    --  1. other blocks from the same module
    --  2. exported functions from another module
    --
    -- Process:
    --  1. We use the BlockInfos to find the block corresponding to every
    --  exported root functions.
    --
    --  2. We add these blocks to the set of root_blocks if they aren't already
    --  added to the result.
    --
    --  3. Then we traverse the root_blocks to find their dependencies and we
    --  add them to root_blocks (if they aren't already added to the result) and
    --  to root_funs.
    --
    --  4. back to 1

    lookup_info infos mod = case M.lookup mod infos of
      Just info -> pure (infos, lbi_info info)
      Nothing   -> do
        -- load info and update cache with it
        info <- load_info mod
        pure (M.insert mod info infos, lbi_info info)

    traverse_blocks
      :: Map Module LocatedBlockInfo
      -> Set BlockRef
      -> Set BlockRef
      -> IO (Map Module LocatedBlockInfo, Set BlockRef)
    traverse_blocks infos result open = case S.minView open of
      Nothing -> return (infos, result)
      Just (ref, open') -> do
          let mod = block_ref_mod ref
          !(infos',info) <- lookup_info infos mod
          let block =  bi_block_deps info ! block_ref_idx ref
              result' = S.insert ref result
              to_block_ref i = BlockRef
                                { block_ref_mod = mod
                                , block_ref_idx = i
                                }
          traverse_funs infos' result'
             (addOpen result' open' $
               map to_block_ref (blockBlockDeps block)) (blockFunDeps block)

    traverse_funs
      :: Map Module LocatedBlockInfo
      -> Set BlockRef
      -> Set BlockRef
      -> [ExportedFun]
      -> IO (Map Module LocatedBlockInfo, Set BlockRef)
    traverse_funs infos result open = \case
      []     -> traverse_blocks infos result open
      (f:fs) -> do
        let mod = funModule f
        -- lookup module block info for the module that exports the function
        !(infos',info) <- lookup_info infos mod
        -- lookup block index associated to the function in the block info
        case M.lookup f (bi_exports info) of
          Nothing  -> pprPanic "exported function not found: " $ ppr f
          Just idx -> do
            let fun_block_ref = BlockRef
                   { block_ref_mod = mod
                   , block_ref_idx = idx
                   }
            -- always add the module "global block" when we link a module
            let global_block_ref = BlockRef
                   { block_ref_mod = mod
                   , block_ref_idx = 0
                   }
            traverse_funs infos' result (addOpen result open [fun_block_ref,global_block_ref]) fs

    -- extend the open block set with new blocks that are not already in the
    -- result block set nor in the open block set.
    addOpen
      :: Set BlockRef
      -> Set BlockRef
      -> [BlockRef]
      -> Set BlockRef
    addOpen result open new_blocks =
      let alreadyLinked s = S.member s result || S.member s open
      in  open `S.union` S.fromList (filter (not . alreadyLinked) new_blocks)

-- | collect dependencies for a set of roots
collectModuleCodes :: ArchiveCache -> LinkPlan -> IO [ModuleCode]
collectModuleCodes ar_cache link_plan = do

  let block_info = lkp_block_info link_plan
  let blocks     = lkp_dep_blocks link_plan

  -- we're going to load all the blocks. Instead of doing this randomly, we
  -- group them by module first.
  let module_blocks :: Map Module BlockIds
      module_blocks = M.fromListWith IS.union $
                      map (\ref -> (block_ref_mod ref, IS.singleton (block_ref_idx ref))) (S.toList blocks)

  -- GHCJS had this comment: "read ghc-prim first, since we depend on that for
  -- static initialization". Not sure if it's still true as we haven't ported
  -- the compactor yet. Still we sort to read ghc-prim blocks first just in
  -- case.
  let pred x = moduleUnitId (fst x) == primUnitId
      cmp x y = case (pred x, pred y) of
        (True,False)  -> LT
        (False,True)  -> GT
        (True,True)   -> EQ
        (False,False) -> EQ

      sorted_module_blocks :: [(Module,BlockIds)]
      sorted_module_blocks = sortBy cmp (M.toList module_blocks)

  -- load blocks
  forM sorted_module_blocks $ \(mod,bids) -> do
    case M.lookup mod block_info of
      Nothing  -> pprPanic "collectModuleCodes: couldn't find block info for module" (ppr mod)
      Just lbi -> extractBlocks ar_cache lbi bids

extractBlocks :: ArchiveCache -> LocatedBlockInfo -> BlockIds -> IO ModuleCode
extractBlocks ar_state lbi blocks = do
  case lbi_loc lbi of
    ObjectFile fp -> do
      us <- readObjectBlocks fp blocks
      pure (collectCode us)
    ArchiveFile a -> do
      obj <- readArObject ar_state mod a
      us <- getObjectBlocks obj blocks
      pure (collectCode us)
    InMemory _n obj -> do
      us <- getObjectBlocks obj blocks
      pure (collectCode us)
  where
    mod           = bi_module (lbi_info lbi)
    newline       = BC.pack "\n"
    mk_exports    = mconcat . intersperse newline . filter (not . BS.null) . map oiRaw
    mk_js_code    = mconcat . map oiStat
    collectCode l = ModuleCode
                      { mc_module   = mod
                      , mc_js_code  = mk_js_code l
                      , mc_exports  = mk_exports l
                      , mc_closures = concatMap oiClInfo l
                      , mc_statics  = concatMap oiStatic l
                      , mc_frefs    = concatMap oiFImports l
                      }

-- | Load an archive in memory and store it in the cache for future loads.
loadArchive :: ArchiveCache -> FilePath -> IO Ar.Archive
loadArchive ar_cache ar_file = do
  loaded_ars <- readIORef (loadedArchives ar_cache)
  case M.lookup ar_file loaded_ars of
    Just a -> pure a
    Nothing -> do
      a <- Ar.loadAr ar_file
      modifyIORef (loadedArchives ar_cache) (M.insert ar_file a)
      pure a


readArObject :: ArchiveCache -> Module -> FilePath -> IO Object
readArObject ar_cache mod ar_file = do
  Ar.Archive entries <- loadArchive ar_cache ar_file

  -- look for the right object in archive
  let go_entries = \case
        -- XXX this shouldn't be an exception probably
        [] -> panic $ "could not find object for module "
                      ++ moduleNameString (moduleName mod)
                      ++ " in "
                      ++ ar_file

        (e:es) -> do
          let bs = Ar.filedata e
          bh <- unsafeUnpackBinBuffer bs
          getObjectHeader bh >>= \case
            Left _         -> go_entries es -- not a valid object entry
            Right mod_name
              | mod_name /= moduleName mod
              -> go_entries es -- not the module we're looking for
              | otherwise
              -> getObjectBody bh mod_name -- found it

  go_entries entries

-- | dependencies for the RTS, these need to be always linked
rtsDeps :: ([UnitId], Set ExportedFun)
rtsDeps =
  ( [ghcInternalUnitId, primUnitId]
  , S.fromList $ concat
      [ mkInternalFuns "GHC.Internal.Conc.Sync"
          ["reportError"]
      , mkInternalFuns "GHC.Internal.Control.Exception.Base"
          ["nonTermination"]
      , mkInternalFuns "GHC.Internal.Exception.Type"
          [ "SomeException"
          , "underflowException"
          , "overflowException"
          , "divZeroException"
          ]
      , mkInternalFuns "GHC.Internal.TopHandler"
          [ "runMainIO"
          , "topHandler"
          ]
      , mkInternalFuns "GHC.Internal.Base"
          ["$fMonadIO"]
      , mkInternalFuns "GHC.Internal.Maybe"
          [ "Nothing"
          , "Just"
          ]
      , mkInternalFuns "GHC.Internal.Ptr"
          ["Ptr"]
      , mkInternalFuns "GHC.Internal.JS.Prim"
          [ "JSVal"
          , "JSException"
          , "$fShowJSException"
          , "$fExceptionJSException"
          , "resolve"
          , "resolveIO"
          , "toIO"
          ]
      , mkInternalFuns "GHC.Internal.JS.Prim.Internal"
          [ "wouldBlock"
          , "blockedIndefinitelyOnMVar"
          , "blockedIndefinitelyOnSTM"
          , "ignoreException"
          , "setCurrentThreadResultException"
          , "setCurrentThreadResultValue"
          ]
      , mkPrimFuns "GHC.Types"
          [ ":"
          , "[]"
          ]
      , mkPrimFuns "GHC.Tuple"
          [ "(,)"
          , "(,,)"
          , "(,,,)"
          , "(,,,,)"
          , "(,,,,,)"
          , "(,,,,,,)"
          , "(,,,,,,,)"
          , "(,,,,,,,,)"
          , "(,,,,,,,,,)"
          ]
      ]
  )

-- | Export the functions in @ghc-internal@
mkInternalFuns :: FastString -> [FastString] -> [ExportedFun]
mkInternalFuns = mkExportedFuns ghcInternalUnitId

-- | Export the Prim functions
mkPrimFuns :: FastString -> [FastString] -> [ExportedFun]
mkPrimFuns = mkExportedFuns primUnitId

-- | Given a @UnitId@, a module name, and a set of symbols in the module,
-- package these into an @ExportedFun@.
mkExportedFuns :: UnitId -> FastString -> [FastString] -> [ExportedFun]
mkExportedFuns uid mod_name symbols = mkExportedModFuns mod names
  where
    mod        = mkModule (RealUnit (Definite uid)) (mkModuleNameFS mod_name)
    names      = map (mkJsSymbol True mod) symbols

-- | Given a @Module@ and a set of symbols in the module, package these into an
-- @ExportedFun@.
mkExportedModFuns :: Module -> [FastString] -> [ExportedFun]
mkExportedModFuns mod symbols = map mk_fun symbols
  where
    mk_fun sym = ExportedFun mod (LexicalFastString sym)

-- | read all dependency data from the to-be-linked files
loadObjBlockInfo
  :: [FilePath] -- ^ object files to link
  -> IO (Map Module LocatedBlockInfo, [BlockRef])
loadObjBlockInfo objs = (prepareLoadedDeps . catMaybes) <$> mapM readBlockInfoFromObj objs

-- | Load dependencies for the Linker from Ar
loadArchiveBlockInfo :: ArchiveCache -> [FilePath] -> IO (Map Module LocatedBlockInfo, [BlockRef])
loadArchiveBlockInfo ar_cache archives = do
  archDeps <- forM archives $ \file -> do
    (Ar.Archive entries) <- loadArchive ar_cache file
    catMaybes <$> mapM (readEntry file) entries
  return (prepareLoadedDeps $ concat archDeps)
    where
      readEntry :: FilePath -> Ar.ArchiveEntry -> IO (Maybe LocatedBlockInfo)
      readEntry ar_file ar_entry = do
          let bs = Ar.filedata ar_entry
          bh <- unsafeUnpackBinBuffer bs
          getObjectHeader bh >>= \case
            Left _         -> pure Nothing -- not a valid object entry
            Right mod_name -> do
              obj <- getObjectBody bh mod_name
              let !info = objBlockInfo obj
              pure $ Just (LocatedBlockInfo (ArchiveFile ar_file) info)

prepareLoadedDeps :: [LocatedBlockInfo]
                  -> (Map Module LocatedBlockInfo, [BlockRef])
prepareLoadedDeps lbis = (module_blocks, must_link)
  where
    must_link     = concatMap (requiredBlocks . lbi_info) lbis
    module_blocks = M.fromList $ map (\d -> (bi_module (lbi_info d), d)) lbis

requiredBlocks :: BlockInfo -> [BlockRef]
requiredBlocks d = map mk_block_ref (IS.toList $ bi_must_link d)
  where
    mk_block_ref i = BlockRef
                      { block_ref_mod = bi_module d
                      , block_ref_idx = i
                      }

-- | read block info from an object that might have already been into memory
-- pulls in all Deps from an archive
readBlockInfoFromObj :: FilePath -> IO (Maybe LocatedBlockInfo)
readBlockInfoFromObj file = do
  readObjectBlockInfo file >>= \case
    Nothing   -> pure Nothing
    Just info -> pure $ Just (LocatedBlockInfo (ObjectFile file) info)


-- | Embed a JS file into a JS object .o file
--
-- JS files may contain option pragmas of the form: //#OPTIONS:
-- One of those is //#OPTIONS:CPP. When it is set, we append some common CPP
-- definitions to the file and call cpp on it.
--
-- Other options (e.g. EMCC additional flags for link time) are stored in the
-- JS object header. See JSOptions.
embedJsFile :: Logger -> DynFlags -> TmpFs -> UnitEnv -> FilePath -> FilePath -> IO ()
embedJsFile logger dflags tmpfs unit_env input_fn output_fn = do
  let profiling  = False -- FIXME: add support for profiling way

  createDirectoryIfMissing True (takeDirectory output_fn)

  -- the header lets the linker recognize processed JavaScript files
  -- But don't add JavaScript header to object files!

  -- read pragmas from JS file
  -- we need to store them explicitly as they can be removed by CPP.
  opts <- getOptionsFromJsFile input_fn

  -- run CPP if needed
  cpp_fn <- case enableCPP opts of
    False -> pure input_fn
    True  -> do
      -- append common CPP definitions to the .js file.
      -- They define macros that avoid directly wiring zencoded names
      -- in RTS JS files
      pp_fn <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "js"
      payload <- B.readFile input_fn
      B.writeFile pp_fn (commonCppDefs profiling <> payload)

      -- run CPP on the input JS file
      js_fn <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "js"
      let
        cpp_opts = CppOpts
          { sourceCodePreprocessor  = SCPJsCpp
          -- JS code requires keeping JSDoc comments for third party minification tooling
          , cppLinePragmas          = False -- LINE pragmas aren't JS compatible
          }
      doCpp logger
              tmpfs
              dflags
              unit_env
              cpp_opts
              pp_fn
              js_fn
      pure js_fn

  -- write JS object
  cpp_bs <- B.readFile cpp_fn
  writeJSObject opts cpp_bs output_fn

-- | Link module codes.
--
-- Performs link time optimizations and produces one JStat per module plus some
-- commoned up initialization code.
linkModules :: [ModuleCode] -> ([CompactedModuleCode], JS.JStat)
linkModules mods = (compact_mods, meta)
  where
    compact_mods = map compact mods

    -- here GHCJS used to:
    --  - deduplicate declarations
    --  - rename local variables into shorter ones
    --  - compress initialization data
    -- but we haven't ported it (yet).
    compact m = CompactedModuleCode
      { cmc_js_code = mc_js_code m
      , cmc_module  = mc_module m
      , cmc_exports = mc_exports m
      }

    -- common up statics: different bindings may reference the same statics, we
    -- filter them here to initialize them once
    statics = nubStaticInfo (concatMap mc_statics mods)

    infos   = concatMap mc_closures mods
    debug   = False -- TODO: this could be enabled in a debug build.
                    -- It adds debug info to heap objects
    meta = mconcat
            -- render metadata as individual statements
            [ mconcat (map staticDeclStat statics)
            , mconcat (map staticInitStat statics)
            , jStgStatToJS $ mconcat (map (closureInfoStat debug) infos)
            ]

-- | Only keep a single StaticInfo with a given name
nubStaticInfo :: [StaticInfo] -> [StaticInfo]
nubStaticInfo = go emptyUniqSet
  where
    go us = \case
      []     -> []
      (x:xs) ->
        -- only match on siVar. There is no reason for the initializing value to
        -- be different for the same global name.
        let name = siVar x
        in if elementOfUniqSet name us
          then go us xs
          else x : go (addOneToUniqSet us name) xs

-- | Initialize a global object.
--
-- All global objects have to be declared (staticInfoDecl) first.
staticInitStat :: StaticInfo -> JS.JStat
staticInitStat (StaticInfo i sv mcc) =
  jStgStatToJS $
  case sv of
    StaticData con args         -> appS hdStiStr $ add_cc_arg
                                    [ global i
                                    , global con
                                    , jsStaticArgs args
                                    ]
    StaticFun  f   args         -> appS hdStiStr $ add_cc_arg
                                    [ global i
                                    , global f
                                    , jsStaticArgs args
                                    ]
    StaticList args mt          -> appS hdStlStr $ add_cc_arg
                                    [ global i
                                    , jsStaticArgs args
                                    , toJExpr $ maybe null_ (toJExpr . TxtI) mt
                                    ]
    StaticThunk (Just (f,args)) -> appS hdStcStr $ add_cc_arg
                                    [ global i
                                    , global f
                                    , jsStaticArgs args
                                    ]
    _                           -> mempty
  where
    -- add optional cost-center argument
    add_cc_arg as = case mcc of
      Nothing -> as
      Just cc -> as ++ [toJExpr cc]

-- | declare and do first-pass init of a global object (create JS object for heap objects)
staticDeclStat :: StaticInfo -> JS.JStat
staticDeclStat (StaticInfo global_name static_value _) = jStgStatToJS decl
  where
    global_ident = name global_name
    decl_init v  = global_ident ||= v
    decl_no_init = appS hdDiStr [toJExpr global_ident]

    decl = case static_value of
      StaticUnboxed u     -> decl_init (unboxed_expr u)
      StaticThunk Nothing -> decl_no_init -- CAF initialized in an alternative way
      _                   -> decl_init (app hdDStr [])

    unboxed_expr = \case
      StaticUnboxedBool b          -> app hdPStr [toJExpr b]
      StaticUnboxedInt i           -> app hdPStr [toJExpr i]
      StaticUnboxedDouble d        -> app hdPStr [toJExpr (unSaneDouble d)]
      -- GHCJS used a function wrapper for this:
      -- StaticUnboxedString str      -> ApplExpr (initStr str) []
      -- But we are defining it statically for now.
      StaticUnboxedString str      -> initStr str
      StaticUnboxedStringOffset {} -> 0

    to_byte_list = JList . map (Int . fromIntegral) . BS.unpack

    initStr :: BS.ByteString -> JStgExpr
    initStr str =
      case decodeModifiedUTF8 str of
        Just t  -> app hdEncodeModifiedUtf8Str [ValExpr (JStr t)]
        Nothing -> app hdRawStringDataStr      [ValExpr $ to_byte_list str]
