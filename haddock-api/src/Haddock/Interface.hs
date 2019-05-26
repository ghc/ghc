{-# LANGUAGE CPP, OverloadedStrings, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2010,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module typechecks Haskell modules using the GHC API and processes
-- the result to create 'Interface's. The typechecking and the 'Interface'
-- creation is interleaved, so that when a module is processed, the
-- 'Interface's of all previously processed modules are available. The
-- creation of an 'Interface' from a typechecked module is delegated to
-- "Haddock.Interface.Create".
--
-- When all modules have been typechecked and processed, information about
-- instances are attached to each 'Interface'. This task is delegated to
-- "Haddock.Interface.AttachInstances". Note that this is done as a separate
-- step because GHC can't know about all instances until all modules have been
-- typechecked.
--
-- As a last step a link environment is built which maps names to the \"best\"
-- places to link to in the documentation, and all 'Interface's are \"renamed\"
-- using this environment.
-----------------------------------------------------------------------------
module Haddock.Interface (
  processModules
) where


import Haddock.GhcUtils
import Haddock.InterfaceFile
import Haddock.Interface.Create
import Haddock.Interface.AttachInstances
import Haddock.Interface.Rename
import Haddock.Options hiding (verbosity)
import Haddock.Types
import Haddock.Utils

import Control.Monad
import Control.Exception (evaluate)
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Printf

import Module (mkModuleSet, emptyModuleSet, unionModuleSet, ModuleSet)
import Digraph
import DynFlags hiding (verbosity)
import GHC hiding (verbosity)
import HscTypes
import FastString (unpackFS)
import TcRnTypes (tcg_rdr_env)
import Name (nameIsFromExternalPackage, nameOccName)
import OccName (isTcOcc)
import RdrName (unQualOK, gre_name, globalRdrEnvElts)
import ErrUtils (withTiming)
import DynamicLoading (initializePlugins)

#if defined(mingw32_HOST_OS)
import System.IO
import GHC.IO.Encoding.CodePage (mkLocaleEncoding)
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))
#endif

-- | Create 'Interface's and a link environment by typechecking the list of
-- modules using the GHC API and processing the resulting syntax trees.
processModules
  :: Verbosity                  -- ^ Verbosity of logging to 'stdout'
  -> [String]                   -- ^ A list of file or module names sorted by
                                -- module topology
  -> [Flag]                     -- ^ Command-line flags
  -> [InterfaceFile]            -- ^ Interface files of package dependencies
  -> Ghc ([Interface], LinkEnv) -- ^ Resulting list of interfaces and renaming
                                -- environment
processModules verbosity modules flags extIfaces = do
#if defined(mingw32_HOST_OS)
  -- Avoid internal error: <stderr>: hPutChar: invalid argument (invalid character)' non UTF-8 Windows
  liftIO $ hSetEncoding stdout $ mkLocaleEncoding TransliterateCodingFailure
  liftIO $ hSetEncoding stderr $ mkLocaleEncoding TransliterateCodingFailure
#endif

  out verbosity verbose "Creating interfaces..."
  let instIfaceMap =  Map.fromList [ (instMod iface, iface) | ext <- extIfaces
                                   , iface <- ifInstalledIfaces ext ]
  (interfaces, ms) <- createIfaces verbosity modules flags instIfaceMap

  let exportedNames =
        Set.unions $ map (Set.fromList . ifaceExports) $
        filter (\i -> not $ OptHide `elem` ifaceOptions i) interfaces
      mods = Set.fromList $ map ifaceMod interfaces
  out verbosity verbose "Attaching instances..."
  interfaces' <- {-# SCC attachInstances #-}
                 withTiming getDynFlags "attachInstances" (const ()) $ do
                   attachInstances (exportedNames, mods) interfaces instIfaceMap ms

  out verbosity verbose "Building cross-linking environment..."
  -- Combine the link envs of the external packages into one
  let extLinks  = Map.unions (map ifLinkEnv extIfaces)
      homeLinks = buildHomeLinks interfaces' -- Build the environment for the home
                                             -- package
      links     = homeLinks `Map.union` extLinks

  out verbosity verbose "Renaming interfaces..."
  let warnings = Flag_NoWarnings `notElem` flags
  dflags <- getDynFlags
  let (interfaces'', msgs) =
         runWriter $ mapM (renameInterface dflags links warnings) interfaces'
  liftIO $ mapM_ putStrLn msgs

  return (interfaces'', homeLinks)


--------------------------------------------------------------------------------
-- * Module typechecking and Interface creation
--------------------------------------------------------------------------------


createIfaces :: Verbosity -> [String] -> [Flag] -> InstIfaceMap -> Ghc ([Interface], ModuleSet)
createIfaces verbosity modules flags instIfaceMap = do
  -- Ask GHC to tell us what the module graph is
  targets <- mapM (\filePath -> guessTarget filePath Nothing) modules
  setTargets targets
  modGraph <- depanal [] False

  -- Visit modules in that order
  let sortedMods = flattenSCCs $ topSortModuleGraph False modGraph Nothing
  out verbosity normal "Haddock coverage:"
  (ifaces, _, !ms) <- foldM f ([], Map.empty, emptyModuleSet) sortedMods
  return (reverse ifaces, ms)
  where
    f (ifaces, ifaceMap, !ms) modSummary = do
      x <- {-# SCC processModule #-}
           withTiming getDynFlags "processModule" (const ()) $ do
             processModule verbosity modSummary flags ifaceMap instIfaceMap
      return $ case x of
        Just (iface, ms') -> ( iface:ifaces
                             , Map.insert (ifaceMod iface) iface ifaceMap
                             , unionModuleSet ms ms' )
        Nothing           -> ( ifaces
                             , ifaceMap
                             , ms ) -- Boot modules don't generate ifaces.


processModule :: Verbosity -> ModSummary -> [Flag] -> IfaceMap -> InstIfaceMap -> Ghc (Maybe (Interface, ModuleSet))
processModule verbosity modsum flags modMap instIfaceMap = do
  out verbosity verbose $ "Checking module " ++ moduleString (ms_mod modsum) ++ "..."

  -- Since GHC 8.6, plugins are initialized on a per module basis
  hsc_env' <- getSession
  dynflags' <- liftIO (initializePlugins hsc_env' (GHC.ms_hspp_opts modsum))
  let modsum' = modsum { ms_hspp_opts = dynflags' }

  tm <- {-# SCC "parse/typecheck/load" #-} loadModule =<< typecheckModule =<< parseModule modsum'

  if not $ isBootSummary modsum then do
    out verbosity verbose "Creating interface..."
    (interface, msgs) <- {-# SCC createIterface #-}
                        withTiming getDynFlags "createInterface" (const ()) $ do
                          runWriterGhc $ createInterface tm flags modMap instIfaceMap

    -- We need to keep track of which modules were somehow in scope so that when
    -- Haddock later looks for instances, it also looks in these modules too.
    --
    -- See https://github.com/haskell/haddock/issues/469.
    hsc_env <- getSession
    let new_rdr_env = tcg_rdr_env . fst . GHC.tm_internals_ $ tm
        this_pkg = thisPackage (hsc_dflags hsc_env)
        !mods = mkModuleSet [ nameModule name
                            | gre <- globalRdrEnvElts new_rdr_env
                            , let name = gre_name gre
                            , nameIsFromExternalPackage this_pkg name
                            , isTcOcc (nameOccName name)   -- Types and classes only
                            , unQualOK gre ]               -- In scope unqualified

    liftIO $ mapM_ putStrLn (nub msgs)
    dflags <- getDynFlags
    let (haddockable, haddocked) = ifaceHaddockCoverage interface
        percentage = round (fromIntegral haddocked * 100 / fromIntegral haddockable :: Double) :: Int
        modString = moduleString (ifaceMod interface)
        coverageMsg = printf " %3d%% (%3d /%3d) in '%s'" percentage haddocked haddockable modString
        header = case ifaceDoc interface of
          Documentation Nothing _ -> False
          _ -> True
        undocumentedExports = [ formatName s n | ExportDecl { expItemDecl = L s n
                                                            , expItemMbDoc = (Documentation Nothing _, _)
                                                            } <- ifaceExportItems interface ]
          where
            formatName :: SrcSpan -> HsDecl GhcRn -> String
            formatName loc n = p (getMainDeclBinder n) ++ case loc of
              RealSrcSpan rss -> " (" ++ unpackFS (srcSpanFile rss) ++ ":" ++ show (srcSpanStartLine rss) ++ ")"
              _ -> ""

            p [] = ""
            p (x:_) = let n = pretty dflags x
                          ms = modString ++ "."
                      in if ms `isPrefixOf` n
                         then drop (length ms) n
                         else n

    when (OptHide `notElem` ifaceOptions interface) $ do
      out verbosity normal coverageMsg
      when (Flag_NoPrintMissingDocs `notElem` flags
            && not (null undocumentedExports && header)) $ do
        out verbosity normal "  Missing documentation for:"
        unless header $ out verbosity normal "    Module header"
        mapM_ (out verbosity normal . ("    " ++)) undocumentedExports
    interface' <- liftIO $ evaluate interface
    return (Just (interface', mods))
  else
    return Nothing


--------------------------------------------------------------------------------
-- * Building of cross-linking environment
--------------------------------------------------------------------------------


-- | Build a mapping which for each original name, points to the "best"
-- place to link to in the documentation.  For the definition of
-- "best", we use "the module nearest the bottom of the dependency
-- graph which exports this name", not including hidden modules.  When
-- there are multiple choices, we pick a random one.
--
-- The interfaces are passed in in topologically sorted order, but we start
-- by reversing the list so we can do a foldl.
buildHomeLinks :: [Interface] -> LinkEnv
buildHomeLinks ifaces = foldl upd Map.empty (reverse ifaces)
  where
    upd old_env iface
      | OptHide    `elem` ifaceOptions iface = old_env
      | OptNotHome `elem` ifaceOptions iface =
        foldl' keep_old old_env exported_names
      | otherwise = foldl' keep_new old_env exported_names
      where
        exported_names = ifaceVisibleExports iface ++ map getName (ifaceInstances iface)
        mdl            = ifaceMod iface
        keep_old env n = Map.insertWith (\_ old -> old) n mdl env
        keep_new env n = Map.insert n mdl env

