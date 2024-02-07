{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}

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
  )
where

import Prelude

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
import GHC.Utils.Monad
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

newtype ArchiveState = ArchiveState { loadedArchives :: IORef (Map FilePath Ar.Archive) }

emptyArchiveState :: IO ArchiveState
emptyArchiveState = ArchiveState <$> newIORef M.empty

defaultJsContext :: SDocContext
defaultJsContext = defaultSDocContext{sdocStyle = PprCode}

jsLinkBinary
  :: FinderCache
  -> JSLinkConfig
  -> StgToJSConfig
  -> [FilePath]
  -> Logger
  -> DynFlags
  -> UnitEnv
  -> [FilePath]
  -> [UnitId]
  -> IO ()
jsLinkBinary finder_cache lc_cfg cfg js_srcs logger dflags unit_env objs dep_units
  | lcNoJSExecutables lc_cfg = return ()
  | otherwise = do
    -- additional objects to link are passed as FileOption ldInputs...
    let cmdline_objs = [ f | FileOption _ f <- ldInputs dflags ]
    -- discriminate JavaScript sources from real object files.
    (cmdline_js_srcs, cmdline_js_objs) <- partitionM isJsFile cmdline_objs
    let
        objs'    = map ObjFile (objs ++ cmdline_js_objs)
        js_srcs' = js_srcs ++ cmdline_js_srcs
        is_root _ = True -- FIXME: we shouldn't consider every function as a root,
                         -- but only the program entry point (main), either the
                         -- generated one or coming from an object
        exe      = jsExeFileName dflags

    -- compute dependencies
    let link_spec = LinkSpec
          { lks_unit_ids        = dep_units
          , lks_obj_files       = objs'
          , lks_obj_root_filter = is_root
          , lks_extra_roots     = mempty
          , lks_extra_js        = js_srcs'
          }

    let finder_opts = initFinderOpts dflags

    link_plan <- computeLinkDependencies cfg unit_env link_spec finder_opts finder_cache

    void $ jsLink lc_cfg cfg logger exe link_plan

-- | link and write result to disk (jsexe directory)
jsLink
     :: JSLinkConfig
     -> StgToJSConfig
     -> Logger
     -> FilePath               -- ^ output file/directory
     -> LinkPlan
     -> IO ()
jsLink lc_cfg cfg logger out link_plan = do

      -- create output directory
      createDirectoryIfMissing False out

      when (logVerbAtLeast logger 2) $
        logInfo logger $ hang (text "jsLink:") 2 (ppr link_plan)

      -------------------------------------------------------------
      -- link all Haskell code (program + dependencies) into out.js

      -- retrieve code for Haskell dependencies
      mods <- collectModuleCodes link_plan

      -- LTO + rendering of JS code
      link_stats <- withBinaryFile (out </> "out.js") WriteMode $ \h ->
        renderLinker h (csPrettyRender cfg) mods (lkp_extra_js link_plan)

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
          void $
            hPutJS (csPrettyRender cfg) h (jsOptimize $ runJSM jsm $ jStgStatToJS <$> rts cfg)

      -- link dependencies' JS files into lib.js
      withBinaryFile (out </> "lib.js") WriteMode $ \h -> do
        forM_ (lkp_archives link_plan) $ \archive_file -> do
          Ar.Archive entries <- Ar.loadAr archive_file
          forM_ entries $ \entry -> do
            case getJsArchiveEntry entry of
              Nothing -> return ()
              Just bs -> do
                B.hPut   h bs
                hPutChar h '\n'

      -- link everything together into a runnable all.js
      -- only if we link a complete application,
      --   no incremental linking and no skipped parts
      when (lcCombineAll lc_cfg && not (lcNoRts lc_cfg)) $ do
        _ <- combineFiles lc_cfg out
        writeHtml    out
        writeRunMain out
        writeRunner lc_cfg out
        writeExterns out

data LinkSpec = LinkSpec
  { lks_unit_ids        :: [UnitId]

  , lks_obj_files       :: [LinkedObj]

  , lks_obj_root_filter :: ExportedFun -> Bool
      -- ^ Predicate for exported functions in objects to declare as root

  , lks_extra_roots     :: Set ExportedFun
      -- ^ Extra root functions from loaded units

  , lks_extra_js        :: [FilePath]
      -- ^ Extra JS files to link
  }

instance Outputable LinkSpec where
  ppr s = hang (text "LinkSpec") 2 $ vcat
            [ hcat [text "Unit ids: ", ppr (lks_unit_ids s)]
            , hcat [text "Object files:", ppr (lks_obj_files s)]
            , text "Object root filter: <function>"
            , hcat [text "Extra roots: ", ppr (lks_extra_roots s)]
            , hang (text "Extra JS:") 2 (vcat (fmap text (lks_extra_js s)))
            ]

emptyLinkPlan :: LinkPlan
emptyLinkPlan = LinkPlan
  { lkp_block_info = mempty
  , lkp_dep_blocks = mempty
  , lkp_archives   = mempty
  , lkp_extra_js   = mempty
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
      , lkp_extra_js   = S.union (lkp_extra_js base) (lkp_extra_js new)
      }
    diff = LinkPlan
      { lkp_block_info = lkp_block_info new -- block info from "new" contains all we need to load new blocks
      , lkp_dep_blocks = S.difference (lkp_dep_blocks new) (lkp_dep_blocks base)
      , lkp_archives   = S.difference (lkp_archives new)   (lkp_archives base)
      , lkp_extra_js   = S.difference (lkp_extra_js new)   (lkp_extra_js base)
      }


computeLinkDependencies
  :: StgToJSConfig
  -> UnitEnv
  -> LinkSpec
  -> FinderOpts
  -> FinderCache
  -> IO LinkPlan
computeLinkDependencies cfg unit_env link_spec finder_opts finder_cache = do

  let units       = lks_unit_ids        link_spec
  let obj_files   = lks_obj_files       link_spec
  let extra_roots = lks_extra_roots     link_spec
  let obj_is_root = lks_obj_root_filter link_spec

  -- Process:
  -- 1) Find new required linkables (object files, libraries, etc.) for all
  -- transitive dependencies
  -- 2) Load ObjBlockInfo from them and cache them
  -- 3) Compute ObjBlock dependencies and return the link plan

  -- TODO (#23013): currently we directly compute the ObjBlock dependencies and
  -- find/load linkable on-demand when a module is missing.


  (objs_block_info, objs_required_blocks) <- loadObjBlockInfo obj_files

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
  (archives_block_info, archives_required_blocks) <- loadArchiveBlockInfo dep_archives

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
                  (bis, req_b) <- loadObjBlockInfo [ObjFile p]
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
        , lkp_extra_js   = S.fromList (lks_extra_js link_spec)
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
renderLinker
  :: Handle
  -> Bool         -- ^ should we render readable JS for debugging?
  -> [ModuleCode] -- ^ linked code per module
  -> Set FilePath -- ^ additional JS files
  -> IO LinkerStats
renderLinker h render_pretty mods js_files = do

  -- link modules
  let (compacted_mods, meta) = linkModules mods

  let
    putBS   = B.hPut h
    putJS   = hPutJS render_pretty h

  ---------------------------------------------------------
  -- Pretty-print JavaScript code for all the dependencies.
  --
  -- We have to pretty-print at link time because we want to be able to perform
  -- global link-time optimisations (e.g. renamings) on the whole generated JS
  -- file.

  -- modules themselves
  mod_sizes <- forM compacted_mods $ \m -> do
    !mod_size <- fromIntegral <$> putJS (cmc_js_code m)
    let !mod_mod  = cmc_module m
    pure (mod_mod, mod_size)

  -- commoned up metadata
  !meta_length <- fromIntegral <$> putJS (jsOptimize meta)

  -- module exports
  mapM_ (putBS . cmc_exports) compacted_mods

  -- explicit additional JS files
  mapM_ (\i -> B.readFile i >>= putBS) (S.toList js_files)

  -- stats
  let link_stats = LinkerStats
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
             -> FilePath
             -> IO ()
combineFiles cfg fp = do
  let files = map (fp </>) ["rts.js", "lib.js", "out.js"]
  withBinaryFile (fp </> "all.js") WriteMode $ \h -> do
    let cpy i = B.readFile i >>= B.hPut h
    mapM_ cpy files
    unless (lcNoHsMain cfg) $ do
      B.hPut h runMainJS

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
  -> IO ()
writeRunMain out = do
  let runMainFile = out </> "runmain.js"
  e <- doesFileExist runMainFile
  unless e $
    B.writeFile runMainFile runMainJS

runMainJS :: B.ByteString
runMainJS = "h$main(h$mainZCZCMainzimain);\n"

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
  "// GHCJS RTS externs for closure compiler ADVANCED_OPTIMIZATIONS\n\n" <>
  mconcat (map (\x -> "/** @type {*} */\nObject.d" <> mkFastString (show x) <> ";\n")
               [(7::Int)..16384])

writeExterns :: FilePath -> IO ()
writeExterns out = writeFile (out </> "all.js.externs")
  $ unpackFS rtsExterns

-- | Get all block dependencies for a given set of roots
--
-- Returns the update block info map and the blocks.
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
    --  2. We had these blocks to the set of root_blocks if they aren't already
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
collectModuleCodes :: LinkPlan -> IO [ModuleCode]
collectModuleCodes link_plan = do

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
  ar_state <- emptyArchiveState
  forM sorted_module_blocks $ \(mod,bids) -> do
    case M.lookup mod block_info of
      Nothing  -> pprPanic "collectModuleCodes: couldn't find block info for module" (ppr mod)
      Just lbi -> extractBlocks ar_state lbi bids

extractBlocks :: ArchiveState -> LocatedBlockInfo -> BlockIds -> IO ModuleCode
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

readArObject :: ArchiveState -> Module -> FilePath -> IO Object
readArObject ar_state mod ar_file = do
  loaded_ars <- readIORef (loadedArchives ar_state)
  (Ar.Archive entries) <- case M.lookup ar_file loaded_ars of
    Just a -> pure a
    Nothing -> do
      a <- Ar.loadAr ar_file
      modifyIORef (loadedArchives ar_state) (M.insert ar_file a)
      pure a

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


-- | A helper function to read system dependencies that are hardcoded
diffDeps
  :: [UnitId]                    -- ^ Packages that are already Linked
  -> ([UnitId], Set ExportedFun) -- ^ New units and functions to link
  -> ([UnitId], Set ExportedFun) -- ^ Diff
diffDeps pkgs (deps_pkgs,deps_funs) =
  ( filter linked_pkg deps_pkgs
  , S.filter linked_fun deps_funs
  )
  where
    linked_fun f = moduleUnitId (funModule f) `S.member` linked_pkgs
    linked_pkg p = S.member p linked_pkgs
    linked_pkgs  = S.fromList pkgs

-- | dependencies for the RTS, these need to be always linked
rtsDeps :: ([UnitId], Set ExportedFun)
rtsDeps =
  ( [ghcInternalUnitId, primUnitId]
  , S.fromList $ concat
      [ mkInternalFuns "GHC.Conc.Sync"
          ["reportError"]
      , mkInternalFuns "Control.Exception.Base"
          ["nonTermination"]
      , mkInternalFuns "GHC.Exception.Type"
          [ "SomeException"
          , "underflowException"
          , "overflowException"
          , "divZeroException"
          ]
      , mkInternalFuns "GHC.TopHandler"
          [ "runMainIO"
          , "topHandler"
          ]
      , mkInternalFuns "GHC.Base"
          ["$fMonadIO"]
      , mkInternalFuns "GHC.Maybe"
          [ "Nothing"
          , "Just"
          ]
      , mkInternalFuns "GHC.Ptr"
          ["Ptr"]
      , mkInternalFuns "GHC.JS.Prim"
          [ "JSVal"
          , "JSException"
          , "$fShowJSException"
          , "$fExceptionJSException"
          , "resolve"
          , "resolveIO"
          , "toIO"
          ]
      , mkInternalFuns "GHC.JS.Prim.Internal"
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
      , mkPrimFuns "GHC.Tuple.Prim"
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

-- | Export the functions in base
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
loadObjBlockInfo :: [LinkedObj] -- ^ object files to link
            -> IO (Map Module LocatedBlockInfo, [BlockRef])
loadObjBlockInfo objs = (prepareLoadedDeps . catMaybes) <$> mapM readBlockInfoFromObj objs

-- | Load dependencies for the Linker from Ar
loadArchiveBlockInfo :: [FilePath] -> IO (Map Module LocatedBlockInfo, [BlockRef])
loadArchiveBlockInfo archives = do
  archDeps <- forM archives $ \file -> do
    (Ar.Archive entries) <- Ar.loadAr file
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

-- | Predicate to check that an entry in Ar is a JS source
-- and to return it without its header
getJsArchiveEntry :: Ar.ArchiveEntry -> Maybe B.ByteString
getJsArchiveEntry entry = getJsBS (Ar.filedata entry)

-- | Predicate to check that a file is a JS source
isJsFile :: FilePath -> IO Bool
isJsFile fp = withBinaryFile fp ReadMode $ \h -> do
  bs <- B.hGet h jsHeaderLength
  pure (isJsBS bs)

isJsBS :: B.ByteString -> Bool
isJsBS bs = isJust (getJsBS bs)

-- | Get JS source with its header (if it's one)
getJsBS :: B.ByteString -> Maybe B.ByteString
getJsBS bs = B.stripPrefix jsHeader bs

-- Header added to JS sources to discriminate them from other object files.
-- They all have .o extension but JS sources have this header.
jsHeader :: B.ByteString
jsHeader = "//JavaScript"

jsHeaderLength :: Int
jsHeaderLength = B.length jsHeader



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
readBlockInfoFromObj :: LinkedObj -> IO (Maybe LocatedBlockInfo)
readBlockInfoFromObj = \case
  ObjLoaded name obj -> do
    let !info = objBlockInfo obj
    pure $ Just (LocatedBlockInfo (InMemory name obj) info)
  ObjFile file -> do
    readObjectBlockInfo file >>= \case
      Nothing   -> pure Nothing
      Just info -> pure $ Just (LocatedBlockInfo (ObjectFile file) info)


-- | Embed a JS file into a .o file
--
-- The JS file is merely copied into a .o file with an additional header
-- ("//Javascript") in order to be recognized later on.
--
-- JS files may contain option pragmas of the form: //#OPTIONS:
-- For now, only the CPP option is supported. If the CPP option is set, we
-- append some common CPP definitions to the file and call cpp on it.
embedJsFile :: Logger -> DynFlags -> TmpFs -> UnitEnv -> FilePath -> FilePath -> IO ()
embedJsFile logger dflags tmpfs unit_env input_fn output_fn = do
  let profiling  = False -- FIXME: add support for profiling way

  createDirectoryIfMissing True (takeDirectory output_fn)

  -- the header lets the linker recognize processed JavaScript files
  -- But don't add JavaScript header to object files!

  -- header appended to JS files stored as .o to recognize them.
  let header = "//JavaScript\n"
  jsFileNeedsCpp input_fn >>= \case
    False -> copyWithHeader header input_fn output_fn
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
          { useHsCpp       = False
          , cppLinePragmas = False -- LINE pragmas aren't JS compatible
          }
      doCpp logger
              tmpfs
              dflags
              unit_env
              cpp_opts
              pp_fn
              js_fn
      -- add header to recognize the object as a JS file
      copyWithHeader header js_fn output_fn

jsFileNeedsCpp :: FilePath -> IO Bool
jsFileNeedsCpp fn = do
  opts <- getOptionsFromJsFile fn
  pure (CPP `elem` opts)

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
    StaticData con args         -> appS "h$sti" $ add_cc_arg
                                    [ var i
                                    , var con
                                    , jsStaticArgs args
                                    ]
    StaticFun  f   args         -> appS "h$sti" $ add_cc_arg
                                    [ var i
                                    , var f
                                    , jsStaticArgs args
                                    ]
    StaticList args mt          -> appS "h$stl" $ add_cc_arg
                                    [ var i
                                    , jsStaticArgs args
                                    , toJExpr $ maybe null_ (toJExpr . TxtI) mt
                                    ]
    StaticThunk (Just (f,args)) -> appS "h$stc" $ add_cc_arg
                                    [ var i
                                    , var f
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
    global_ident = global global_name
    decl_init v  = global_ident ||= v
    decl_no_init = appS "h$di" [toJExpr global_ident]

    decl = case static_value of
      StaticUnboxed u     -> decl_init (unboxed_expr u)
      StaticThunk Nothing -> decl_no_init -- CAF initialized in an alternative way
      _                   -> decl_init (app "h$d" [])

    unboxed_expr = \case
      StaticUnboxedBool b          -> app "h$p" [toJExpr b]
      StaticUnboxedInt i           -> app "h$p" [toJExpr i]
      StaticUnboxedDouble d        -> app "h$p" [toJExpr (unSaneDouble d)]
      StaticUnboxedString str      -> app "h$rawStringData" [ValExpr (to_byte_list str)]
      StaticUnboxedStringOffset {} -> 0

    to_byte_list = JList . map (Int . fromIntegral) . BS.unpack
