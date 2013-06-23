%
% (c) The University of Glasgow 2005-2012
%
\begin{code}
-- | The dynamic linker for GHCi.
--
-- This module deals with the top-level issues of dynamic linking,
-- calling the object-code linker and the byte-code linker where
-- necessary.

{-# OPTIONS -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

module Linker ( HValue, getHValue, showLinkerState,
                linkExpr, linkDecls, unload, withExtendedLinkEnv,
                extendLinkEnv, deleteFromLinkEnv,
                extendLoadedPkgs,
                linkPackages,initDynLinker,linkModule,

                -- Saving/restoring globals
                PersistentLinkerState, saveLinkerGlobals, restoreLinkerGlobals
        ) where

#include "HsVersions.h"

import LoadIface
import ObjLink
import ByteCodeLink
import ByteCodeItbls
import ByteCodeAsm
import TcRnMonad
import Packages
import DriverPhases
import Finder
import HscTypes
import Name
import NameEnv
import NameSet
import UniqFM
import Module
import ListSetOps
import DynFlags
import BasicTypes
import Outputable
import Panic
import Util
import ErrUtils
import SrcLoc
import qualified Maybes
import UniqSet
import FastString
import Config
import Platform
import SysTools
import PrelNames

-- Standard libraries
import Control.Monad

import Data.IORef
import Data.List
import qualified Data.Map as Map
import Control.Concurrent.MVar

import System.FilePath
import System.IO
#if __GLASGOW_HASKELL__ > 704
import System.Directory hiding (findFile)
#else
import System.Directory
#endif

import Distribution.Package hiding (depends, PackageId)

import Exception
\end{code}


%************************************************************************
%*                                                                      *
                        The Linker's state
%*                                                                      *
%************************************************************************

The persistent linker state *must* match the actual state of the
C dynamic linker at all times, so we keep it in a private global variable.

The global IORef used for PersistentLinkerState actually contains another MVar.
The reason for this is that we want to allow another loaded copy of the GHC
library to side-effect the PLS and for those changes to be reflected here.

The PersistentLinkerState maps Names to actual closures (for
interpreted code only), for use during linking.

\begin{code}
GLOBAL_VAR_M(v_PersistentLinkerState, newMVar (panic "Dynamic linker not initialised"), MVar PersistentLinkerState)
GLOBAL_VAR(v_InitLinkerDone, False, Bool) -- Set True when dynamic linker is initialised

modifyPLS_ :: (PersistentLinkerState -> IO PersistentLinkerState) -> IO ()
modifyPLS_ f = readIORef v_PersistentLinkerState >>= flip modifyMVar_ f

modifyPLS :: (PersistentLinkerState -> IO (PersistentLinkerState, a)) -> IO a
modifyPLS f = readIORef v_PersistentLinkerState >>= flip modifyMVar f

data PersistentLinkerState
   = PersistentLinkerState {

        -- Current global mapping from Names to their true values
        closure_env :: ClosureEnv,

        -- The current global mapping from RdrNames of DataCons to
        -- info table addresses.
        -- When a new Unlinked is linked into the running image, or an existing
        -- module in the image is replaced, the itbl_env must be updated
        -- appropriately.
        itbl_env    :: !ItblEnv,

        -- The currently loaded interpreted modules (home package)
        bcos_loaded :: ![Linkable],

        -- And the currently-loaded compiled modules (home package)
        objs_loaded :: ![Linkable],

        -- The currently-loaded packages; always object code
        -- Held, as usual, in dependency order; though I am not sure if
        -- that is really important
        pkgs_loaded :: ![PackageId]
     }

emptyPLS :: DynFlags -> PersistentLinkerState
emptyPLS _ = PersistentLinkerState {
                        closure_env = emptyNameEnv,
                        itbl_env    = emptyNameEnv,
                        pkgs_loaded = init_pkgs,
                        bcos_loaded = [],
                        objs_loaded = [] }

  -- Packages that don't need loading, because the compiler
  -- shares them with the interpreted program.
  --
  -- The linker's symbol table is populated with RTS symbols using an
  -- explicit list.  See rts/Linker.c for details.
  where init_pkgs = [rtsPackageId]


extendLoadedPkgs :: [PackageId] -> IO ()
extendLoadedPkgs pkgs =
  modifyPLS_ $ \s ->
      return s{ pkgs_loaded = pkgs ++ pkgs_loaded s }

extendLinkEnv :: [(Name,HValue)] -> IO ()
-- Automatically discards shadowed bindings
extendLinkEnv new_bindings =
  modifyPLS_ $ \pls ->
    let new_closure_env = extendClosureEnv (closure_env pls) new_bindings
    in return pls{ closure_env = new_closure_env }

deleteFromLinkEnv :: [Name] -> IO ()
deleteFromLinkEnv to_remove =
  modifyPLS_ $ \pls ->
    let new_closure_env = delListFromNameEnv (closure_env pls) to_remove
    in return pls{ closure_env = new_closure_env }

-- | Get the 'HValue' associated with the given name.
--
-- May cause loading the module that contains the name.
--
-- Throws a 'ProgramError' if loading fails or the name cannot be found.
getHValue :: HscEnv -> Name -> IO HValue
getHValue hsc_env name = do
  initDynLinker (hsc_dflags hsc_env)
  pls <- modifyPLS $ \pls -> do
           if (isExternalName name) then do
             (pls', ok) <- linkDependencies hsc_env pls noSrcSpan [nameModule name]
             if (failed ok) then throwGhcExceptionIO (ProgramError "")
                            else return (pls', pls')
            else
             return (pls, pls)
  lookupName (closure_env pls) name

linkDependencies :: HscEnv -> PersistentLinkerState
                 -> SrcSpan -> [Module]
                 -> IO (PersistentLinkerState, SuccessFlag)
linkDependencies hsc_env pls span needed_mods = do
--   initDynLinker (hsc_dflags hsc_env)
   let hpt = hsc_HPT hsc_env
       dflags = hsc_dflags hsc_env
   -- The interpreter and dynamic linker can only handle object code built
   -- the "normal" way, i.e. no non-std ways like profiling or ticky-ticky.
   -- So here we check the build tag: if we're building a non-standard way
   -- then we need to find & link object files built the "normal" way.
   maybe_normal_osuf <- checkNonStdWay dflags span

   -- Find what packages and linkables are required
   (lnks, pkgs) <- getLinkDeps hsc_env hpt pls
                               maybe_normal_osuf span needed_mods

   -- Link the packages and modules required
   pls1 <- linkPackages' dflags pkgs pls
   linkModules dflags pls1 lnks


-- | Temporarily extend the linker state.

withExtendedLinkEnv :: (MonadIO m, ExceptionMonad m) =>
                       [(Name,HValue)] -> m a -> m a
withExtendedLinkEnv new_env action
    = gbracket (liftIO $ extendLinkEnv new_env)
               (\_ -> reset_old_env)
               (\_ -> action)
    where
        -- Remember that the linker state might be side-effected
        -- during the execution of the IO action, and we don't want to
        -- lose those changes (we might have linked a new module or
        -- package), so the reset action only removes the names we
        -- added earlier.
          reset_old_env = liftIO $ do
            modifyPLS_ $ \pls ->
                let cur = closure_env pls
                    new = delListFromNameEnv cur (map fst new_env)
                in return pls{ closure_env = new }

-- filterNameMap removes from the environment all entries except
-- those for a given set of modules;
-- Note that this removes all *local* (i.e. non-isExternal) names too
-- (these are the temporary bindings from the command line).
-- Used to filter both the ClosureEnv and ItblEnv

filterNameMap :: [Module] -> NameEnv (Name, a) -> NameEnv (Name, a)
filterNameMap mods env
   = filterNameEnv keep_elt env
   where
     keep_elt (n,_) = isExternalName n
                      && (nameModule n `elem` mods)


-- | Display the persistent linker state.
showLinkerState :: DynFlags -> IO ()
showLinkerState dflags
  = do pls <- readIORef v_PersistentLinkerState >>= readMVar
       log_action dflags dflags SevDump noSrcSpan defaultDumpStyle
                 (vcat [text "----- Linker state -----",
                        text "Pkgs:" <+> ppr (pkgs_loaded pls),
                        text "Objs:" <+> ppr (objs_loaded pls),
                        text "BCOs:" <+> ppr (bcos_loaded pls)])
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Initialisation}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Initialise the dynamic linker.  This entails
--
--  a) Calling the C initialisation procedure,
--
--  b) Loading any packages specified on the command line,
--
--  c) Loading any packages specified on the command line, now held in the
--     @-l@ options in @v_Opt_l@,
--
--  d) Loading any @.o\/.dll@ files specified on the command line, now held
--     in @ldInputs@,
--
--  e) Loading any MacOS frameworks.
--
-- NOTE: This function is idempotent; if called more than once, it does
-- nothing.  This is useful in Template Haskell, where we call it before
-- trying to link.
--
initDynLinker :: DynFlags -> IO ()
initDynLinker dflags =
  modifyPLS_ $ \pls0 -> do
    done <- readIORef v_InitLinkerDone
    if done then return pls0
            else do writeIORef v_InitLinkerDone True
                    reallyInitDynLinker dflags

reallyInitDynLinker :: DynFlags -> IO PersistentLinkerState
reallyInitDynLinker dflags =
    do  {  -- Initialise the linker state
          let pls0 = emptyPLS dflags

          -- (a) initialise the C dynamic linker
        ; initObjLinker

          -- (b) Load packages from the command-line (Note [preload packages])
        ; pls <- linkPackages' dflags (preloadPackages (pkgState dflags)) pls0

          -- (c) Link libraries from the command-line
        ; let cmdline_ld_inputs = ldInputs dflags
        ; let minus_ls = [ lib | Option ('-':'l':lib) <- cmdline_ld_inputs ]
        ; let lib_paths = libraryPaths dflags
        ; libspecs <- mapM (locateLib dflags False lib_paths) minus_ls

          -- (d) Link .o files from the command-line
        ; classified_ld_inputs <- mapM (classifyLdInput dflags)
                                    [ f | FileOption _ f <- cmdline_ld_inputs ]

          -- (e) Link any MacOS frameworks
        ; let platform = targetPlatform dflags
        ; let framework_paths = if platformUsesFrameworks platform
                                then frameworkPaths dflags
                                else []
        ; let frameworks = if platformUsesFrameworks platform
                           then cmdlineFrameworks dflags
                           else []
          -- Finally do (c),(d),(e)
        ; let cmdline_lib_specs = [ l | Just l <- classified_ld_inputs ]
                               ++ libspecs
                               ++ map Framework frameworks
        ; if null cmdline_lib_specs then return pls
                                    else do

        { mapM_ (preloadLib dflags lib_paths framework_paths) cmdline_lib_specs
        ; maybePutStr dflags "final link ... "
        ; ok <- resolveObjs

        ; if succeeded ok then maybePutStrLn dflags "done"
          else throwGhcExceptionIO (ProgramError "linking extra libraries/objects failed")

        ; return pls
        }}


{- Note [preload packages]

Why do we need to preload packages from the command line?  This is an
explanation copied from #2437:

I tried to implement the suggestion from #3560, thinking it would be
easy, but there are two reasons we link in packages eagerly when they
are mentioned on the command line:

  * So that you can link in extra object files or libraries that
    depend on the packages. e.g. ghc -package foo -lbar where bar is a
    C library that depends on something in foo. So we could link in
    foo eagerly if and only if there are extra C libs or objects to
    link in, but....

  * Haskell code can depend on a C function exported by a package, and
    the normal dependency tracking that TH uses can't know about these
    dependencies. The test ghcilink004 relies on this, for example.

I conclude that we need two -package flags: one that says "this is a
package I want to make available", and one that says "this is a
package I want to link in eagerly". Would that be too complicated for
users?
-}

classifyLdInput :: DynFlags -> FilePath -> IO (Maybe LibrarySpec)
classifyLdInput dflags f
  | isObjectFilename platform f = return (Just (Object f))
  | isDynLibFilename platform f = return (Just (DLLPath f))
  | otherwise          = do
        log_action dflags dflags SevInfo noSrcSpan defaultUserStyle
            (text ("Warning: ignoring unrecognised input `" ++ f ++ "'"))
        return Nothing
    where platform = targetPlatform dflags

preloadLib :: DynFlags -> [String] -> [String] -> LibrarySpec -> IO ()
preloadLib dflags lib_paths framework_paths lib_spec
  = do maybePutStr dflags ("Loading object " ++ showLS lib_spec ++ " ... ")
       case lib_spec of
          Object static_ish
             -> do b <- preload_static lib_paths static_ish
                   maybePutStrLn dflags (if b  then "done"
                                                else "not found")

          Archive static_ish
             -> do b <- preload_static_archive lib_paths static_ish
                   maybePutStrLn dflags (if b  then "done"
                                                else "not found")

          DLL dll_unadorned
             -> do maybe_errstr <- loadDLL (mkSOName platform dll_unadorned)
                   case maybe_errstr of
                      Nothing -> maybePutStrLn dflags "done"
                      Just mm -> preloadFailed mm lib_paths lib_spec

          DLLPath dll_path
             -> do maybe_errstr <- loadDLL dll_path
                   case maybe_errstr of
                      Nothing -> maybePutStrLn dflags "done"
                      Just mm -> preloadFailed mm lib_paths lib_spec

          Framework framework ->
              if platformUsesFrameworks (targetPlatform dflags)
              then do maybe_errstr <- loadFramework framework_paths framework
                      case maybe_errstr of
                         Nothing -> maybePutStrLn dflags "done"
                         Just mm -> preloadFailed mm framework_paths lib_spec
              else panic "preloadLib Framework"

  where
    platform = targetPlatform dflags

    preloadFailed :: String -> [String] -> LibrarySpec -> IO ()
    preloadFailed sys_errmsg paths spec
       = do maybePutStr dflags "failed.\n"
            throwGhcExceptionIO $
              CmdLineError (
                    "user specified .o/.so/.DLL could not be loaded ("
                    ++ sys_errmsg ++ ")\nWhilst trying to load:  "
                    ++ showLS spec ++ "\nAdditional directories searched:"
                    ++ (if null paths then " (none)" else
                        (concat (intersperse "\n" (map ("   "++) paths)))))

    -- Not interested in the paths in the static case.
    preload_static _paths name
       = do b <- doesFileExist name
            if not b then return False
                     else do if cDYNAMIC_GHC_PROGRAMS
                                 then dynLoadObjs dflags [name]
                                 else loadObj name
                             return True
    preload_static_archive _paths name
       = do b <- doesFileExist name
            if not b then return False
                     else do if cDYNAMIC_GHC_PROGRAMS
                                 then panic "Loading archives not supported"
                                 else loadArchive name
                             return True
\end{code}


%************************************************************************
%*                                                                      *
                Link a byte-code expression
%*                                                                      *
%************************************************************************

\begin{code}
-- | Link a single expression, /including/ first linking packages and
-- modules that this expression depends on.
--
-- Raises an IO exception ('ProgramError') if it can't find a compiled
-- version of the dependents to link.
--
linkExpr :: HscEnv -> SrcSpan -> UnlinkedBCO -> IO HValue
linkExpr hsc_env span root_ul_bco
  = do {
     -- Initialise the linker (if it's not been done already)
     let dflags = hsc_dflags hsc_env
   ; initDynLinker dflags

     -- Take lock for the actual work.
   ; modifyPLS $ \pls0 -> do {

     -- Link the packages and modules required
   ; (pls, ok) <- linkDependencies hsc_env pls0 span needed_mods
   ; if failed ok then
        throwGhcExceptionIO (ProgramError "")
     else do {

     -- Link the expression itself
     let ie = itbl_env pls
         ce = closure_env pls

     -- Link the necessary packages and linkables
   ; (_, (root_hval:_)) <- linkSomeBCOs dflags False ie ce [root_ul_bco]
   ; return (pls, root_hval)
   }}}
   where
     free_names = nameSetToList (bcoFreeNames root_ul_bco)

     needed_mods :: [Module]
     needed_mods = [ nameModule n | n <- free_names,
                     isExternalName n,      -- Names from other modules
                     not (isWiredInName n)  -- Exclude wired-in names
                   ]                        -- (see note below)
        -- Exclude wired-in names because we may not have read
        -- their interface files, so getLinkDeps will fail
        -- All wired-in names are in the base package, which we link
        -- by default, so we can safely ignore them here.

dieWith :: DynFlags -> SrcSpan -> MsgDoc -> IO a
dieWith dflags span msg = throwGhcExceptionIO (ProgramError (showSDoc dflags (mkLocMessage SevFatal span msg)))


checkNonStdWay :: DynFlags -> SrcSpan -> IO (Maybe FilePath)
checkNonStdWay dflags srcspan =
  if interpWays == haskellWays
      then return Nothing
    -- see #3604: object files compiled for way "dyn" need to link to the
    -- dynamic packages, so we can't load them into a statically-linked GHCi.
    -- we have to treat "dyn" in the same way as "prof".
    --
    -- In the future when GHCi is dynamically linked we should be able to relax
    -- this, but they we may have to make it possible to load either ordinary
    -- .o files or -dynamic .o files into GHCi (currently that's not possible
    -- because the dynamic objects contain refs to e.g. __stginit_base_Prelude_dyn
    -- whereas we have __stginit_base_Prelude_.
      else if objectSuf dflags == normalObjectSuffix && not (null haskellWays)
      then failNonStd dflags srcspan
      else return $ Just $ if cDYNAMIC_GHC_PROGRAMS
                           then "dyn_o"
                           else "o"
    where haskellWays = filter (not . wayRTSOnly) (ways dflags)

normalObjectSuffix :: String
normalObjectSuffix = phaseInputExt StopLn

failNonStd :: DynFlags -> SrcSpan -> IO (Maybe FilePath)
failNonStd dflags srcspan = dieWith dflags srcspan $
  ptext (sLit "Dynamic linking required, but this is a non-standard build (eg. prof).") $$
  ptext (sLit "You need to build the program twice: once the") <+> ghciWay <+> ptext (sLit "way, and then") $$
  ptext (sLit "in the desired way using -osuf to set the object file suffix.")
    where ghciWay = if cDYNAMIC_GHC_PROGRAMS
                    then ptext (sLit "dynamic")
                    else ptext (sLit "normal")

getLinkDeps :: HscEnv -> HomePackageTable
            -> PersistentLinkerState
            -> Maybe FilePath                   -- replace object suffices?
            -> SrcSpan                          -- for error messages
            -> [Module]                         -- If you need these
            -> IO ([Linkable], [PackageId])     -- ... then link these first
-- Fails with an IO exception if it can't find enough files

getLinkDeps hsc_env hpt pls replace_osuf span mods
-- Find all the packages and linkables that a set of modules depends on
 = do {
        -- 1.  Find the dependent home-pkg-modules/packages from each iface
        -- (omitting iINTERACTIVE, which is already linked)
        (mods_s, pkgs_s) <- follow_deps (filter ((/=) iNTERACTIVE) mods)
                                        emptyUniqSet emptyUniqSet;

        let {
        -- 2.  Exclude ones already linked
        --      Main reason: avoid findModule calls in get_linkable
            mods_needed = mods_s `minusList` linked_mods     ;
            pkgs_needed = pkgs_s `minusList` pkgs_loaded pls ;

            linked_mods = map (moduleName.linkableModule)
                                (objs_loaded pls ++ bcos_loaded pls)
        } ;

        -- 3.  For each dependent module, find its linkable
        --     This will either be in the HPT or (in the case of one-shot
        --     compilation) we may need to use maybe_getFileLinkable
        let { osuf = objectSuf dflags } ;
        lnks_needed <- mapM (get_linkable osuf) mods_needed ;

        return (lnks_needed, pkgs_needed) }
  where
    dflags = hsc_dflags hsc_env
    this_pkg = thisPackage dflags

        -- The ModIface contains the transitive closure of the module dependencies
        -- within the current package, *except* for boot modules: if we encounter
        -- a boot module, we have to find its real interface and discover the
        -- dependencies of that.  Hence we need to traverse the dependency
        -- tree recursively.  See bug #936, testcase ghci/prog007.
    follow_deps :: [Module]             -- modules to follow
                -> UniqSet ModuleName         -- accum. module dependencies
                -> UniqSet PackageId          -- accum. package dependencies
                -> IO ([ModuleName], [PackageId]) -- result
    follow_deps []     acc_mods acc_pkgs
        = return (uniqSetToList acc_mods, uniqSetToList acc_pkgs)
    follow_deps (mod:mods) acc_mods acc_pkgs
        = do
          mb_iface <- initIfaceCheck hsc_env $
                        loadInterface msg mod (ImportByUser False)
          iface <- case mb_iface of
                    Maybes.Failed err      -> throwGhcExceptionIO (ProgramError (showSDoc dflags err))
                    Maybes.Succeeded iface -> return iface

          when (mi_boot iface) $ link_boot_mod_error mod

          let
            pkg = modulePackageId mod
            deps  = mi_deps iface

            pkg_deps = dep_pkgs deps
            (boot_deps, mod_deps) = partitionWith is_boot (dep_mods deps)
                    where is_boot (m,True)  = Left m
                          is_boot (m,False) = Right m

            boot_deps' = filter (not . (`elementOfUniqSet` acc_mods)) boot_deps
            acc_mods'  = addListToUniqSet acc_mods (moduleName mod : mod_deps)
            acc_pkgs'  = addListToUniqSet acc_pkgs $ map fst pkg_deps
          --
          if pkg /= this_pkg
             then follow_deps mods acc_mods (addOneToUniqSet acc_pkgs' pkg)
             else follow_deps (map (mkModule this_pkg) boot_deps' ++ mods)
                              acc_mods' acc_pkgs'
        where
            msg = text "need to link module" <+> ppr mod <+>
                  text "due to use of Template Haskell"


    link_boot_mod_error mod =
        throwGhcExceptionIO (ProgramError (showSDoc dflags (
            text "module" <+> ppr mod <+>
            text "cannot be linked; it is only available as a boot module")))

    no_obj :: Outputable a => a -> IO b
    no_obj mod = dieWith dflags span $
                     ptext (sLit "cannot find object file for module ") <>
                        quotes (ppr mod) $$
                     while_linking_expr

    while_linking_expr = ptext (sLit "while linking an interpreted expression")

        -- This one is a build-system bug

    get_linkable osuf mod_name      -- A home-package module
        | Just mod_info <- lookupUFM hpt mod_name
        = adjust_linkable (Maybes.expectJust "getLinkDeps" (hm_linkable mod_info))
        | otherwise
        = do    -- It's not in the HPT because we are in one shot mode,
                -- so use the Finder to get a ModLocation...
             mb_stuff <- findHomeModule hsc_env mod_name
             case mb_stuff of
                  Found loc mod -> found loc mod
                  _ -> no_obj mod_name
        where
            found loc mod = do {
                -- ...and then find the linkable for it
               mb_lnk <- findObjectLinkableMaybe mod loc ;
               case mb_lnk of {
                  Nothing  -> no_obj mod ;
                  Just lnk -> adjust_linkable lnk
              }}

            adjust_linkable lnk
                | Just new_osuf <- replace_osuf = do
                        new_uls <- mapM (adjust_ul new_osuf)
                                        (linkableUnlinked lnk)
                        return lnk{ linkableUnlinked=new_uls }
                | otherwise =
                        return lnk

            adjust_ul new_osuf (DotO file) = do
                MASSERT(osuf `isSuffixOf` file)
                let file_base = reverse (drop (length osuf + 1) (reverse file))
                    new_file = file_base <.> new_osuf
                ok <- doesFileExist new_file
                if (not ok)
                   then dieWith dflags span $
                          ptext (sLit "cannot find normal object file ")
                                <> quotes (text new_file) $$ while_linking_expr
                   else return (DotO new_file)
            adjust_ul _ (DotA fp) = panic ("adjust_ul DotA " ++ show fp)
            adjust_ul _ (DotDLL fp) = panic ("adjust_ul DotDLL " ++ show fp)
            adjust_ul _ l@(BCOs {}) = return l
\end{code}


%************************************************************************
%*                                                                      *
              Loading a Decls statement
%*                                                                      *
%************************************************************************
\begin{code}
linkDecls :: HscEnv -> SrcSpan -> CompiledByteCode -> IO () --[HValue]
linkDecls hsc_env span (ByteCode unlinkedBCOs itblEnv) = do
    -- Initialise the linker (if it's not been done already)
    let dflags = hsc_dflags hsc_env
    initDynLinker dflags

    -- Take lock for the actual work.
    modifyPLS $ \pls0 -> do

    -- Link the packages and modules required
    (pls, ok) <- linkDependencies hsc_env pls0 span needed_mods
    if failed ok
      then throwGhcExceptionIO (ProgramError "")
      else do

    -- Link the expression itself
    let ie = plusNameEnv (itbl_env pls) itblEnv
        ce = closure_env pls

    -- Link the necessary packages and linkables
    (final_gce, _) <- linkSomeBCOs dflags False ie ce unlinkedBCOs
    let pls2 = pls { closure_env = final_gce,
                     itbl_env    = ie }
    return (pls2, ()) --hvals)
  where
    free_names =  concatMap (nameSetToList . bcoFreeNames) unlinkedBCOs

    needed_mods :: [Module]
    needed_mods = [ nameModule n | n <- free_names,
                    isExternalName n,       -- Names from other modules
                    not (isWiredInName n)   -- Exclude wired-in names
                  ]                         -- (see note below)
    -- Exclude wired-in names because we may not have read
    -- their interface files, so getLinkDeps will fail
    -- All wired-in names are in the base package, which we link
    -- by default, so we can safely ignore them here.
\end{code}



%************************************************************************
%*                                                                      *
              Loading a single module
%*                                                                      *
%************************************************************************

\begin{code}
linkModule :: HscEnv -> Module -> IO ()
linkModule hsc_env mod = do
  initDynLinker (hsc_dflags hsc_env)
  modifyPLS_ $ \pls -> do
    (pls', ok) <- linkDependencies hsc_env pls noSrcSpan [mod]
    if (failed ok) then throwGhcExceptionIO (ProgramError "could not link module")
      else return pls'
\end{code}

%************************************************************************
%*                                                                      *
                Link some linkables
        The linkables may consist of a mixture of
        byte-code modules and object modules
%*                                                                      *
%************************************************************************

\begin{code}
linkModules :: DynFlags -> PersistentLinkerState -> [Linkable]
            -> IO (PersistentLinkerState, SuccessFlag)
linkModules dflags pls linkables
  = mask_ $ do  -- don't want to be interrupted by ^C in here

        let (objs, bcos) = partition isObjectLinkable
                              (concatMap partitionLinkable linkables)

                -- Load objects first; they can't depend on BCOs
        (pls1, ok_flag) <- dynLinkObjs dflags pls objs

        if failed ok_flag then
                return (pls1, Failed)
          else do
                pls2 <- dynLinkBCOs dflags pls1 bcos
                return (pls2, Succeeded)


-- HACK to support f-x-dynamic in the interpreter; no other purpose
partitionLinkable :: Linkable -> [Linkable]
partitionLinkable li
   = let li_uls = linkableUnlinked li
         li_uls_obj = filter isObject li_uls
         li_uls_bco = filter isInterpretable li_uls
     in
         case (li_uls_obj, li_uls_bco) of
            (_:_, _:_) -> [li {linkableUnlinked=li_uls_obj},
                           li {linkableUnlinked=li_uls_bco}]
            _ -> [li]

findModuleLinkable_maybe :: [Linkable] -> Module -> Maybe Linkable
findModuleLinkable_maybe lis mod
   = case [LM time nm us | LM time nm us <- lis, nm == mod] of
        []   -> Nothing
        [li] -> Just li
        _    -> pprPanic "findModuleLinkable" (ppr mod)

linkableInSet :: Linkable -> [Linkable] -> Bool
linkableInSet l objs_loaded =
  case findModuleLinkable_maybe objs_loaded (linkableModule l) of
        Nothing -> False
        Just m  -> linkableTime l == linkableTime m
\end{code}


%************************************************************************
%*                                                                      *
\subsection{The object-code linker}
%*                                                                      *
%************************************************************************

\begin{code}
dynLinkObjs :: DynFlags -> PersistentLinkerState -> [Linkable]
            -> IO (PersistentLinkerState, SuccessFlag)
dynLinkObjs dflags pls objs = do
        -- Load the object files and link them
        let (objs_loaded', new_objs) = rmDupLinkables (objs_loaded pls) objs
            pls1                     = pls { objs_loaded = objs_loaded' }
            unlinkeds                = concatMap linkableUnlinked new_objs
            wanted_objs              = map nameOfObject unlinkeds

        if cDYNAMIC_GHC_PROGRAMS
            then do dynLoadObjs dflags wanted_objs
                    return (pls, Succeeded)
            else do mapM_ loadObj wanted_objs

                    -- Link them all together
                    ok <- resolveObjs

                    -- If resolving failed, unload all our
                    -- object modules and carry on
                    if succeeded ok then do
                            return (pls1, Succeeded)
                      else do
                            pls2 <- unload_wkr dflags [] pls1
                            return (pls2, Failed)

dynLoadObjs :: DynFlags -> [FilePath] -> IO ()
dynLoadObjs dflags objs = do
    let platform = targetPlatform dflags
    soFile <- newTempName dflags (soExt platform)
    let -- When running TH for a non-dynamic way, we still need to make
        -- -l flags to link against the dynamic libraries, so we turn
        -- Opt_Static off
        dflags1 = gopt_unset dflags Opt_Static
        dflags2 = dflags1 {
                      -- We don't want to link the ldInputs in; we'll
                      -- be calling dynLoadObjs with any objects that
                      -- need to be linked.
                      ldInputs = [],
                      -- Even if we're e.g. profiling, we still want
                      -- the vanilla dynamic libraries, so we set the
                      -- ways / build tag to be just WayDyn.
                      ways = [WayDyn],
                      buildTag = mkBuildTag [WayDyn],
                      outputFile = Just soFile
                  }
    linkDynLib dflags2 objs []
    consIORef (filesToNotIntermediateClean dflags) soFile
    m <- loadDLL soFile
    case m of
        Nothing -> return ()
        Just err -> panic ("Loading temp shared object failed: " ++ err)

rmDupLinkables :: [Linkable]    -- Already loaded
               -> [Linkable]    -- New linkables
               -> ([Linkable],  -- New loaded set (including new ones)
                   [Linkable])  -- New linkables (excluding dups)
rmDupLinkables already ls
  = go already [] ls
  where
    go already extras [] = (already, extras)
    go already extras (l:ls)
        | linkableInSet l already = go already     extras     ls
        | otherwise               = go (l:already) (l:extras) ls
\end{code}

%************************************************************************
%*                                                                      *
\subsection{The byte-code linker}
%*                                                                      *
%************************************************************************

\begin{code}
dynLinkBCOs :: DynFlags -> PersistentLinkerState -> [Linkable]
            -> IO PersistentLinkerState
dynLinkBCOs dflags pls bcos = do

        let (bcos_loaded', new_bcos) = rmDupLinkables (bcos_loaded pls) bcos
            pls1                     = pls { bcos_loaded = bcos_loaded' }
            unlinkeds :: [Unlinked]
            unlinkeds                = concatMap linkableUnlinked new_bcos

            cbcs :: [CompiledByteCode]
            cbcs      = map byteCodeOfObject unlinkeds


            ul_bcos    = [b | ByteCode bs _  <- cbcs, b <- bs]
            ies        = [ie | ByteCode _ ie <- cbcs]
            gce       = closure_env pls
            final_ie  = foldr plusNameEnv (itbl_env pls) ies

        (final_gce, _linked_bcos) <- linkSomeBCOs dflags True final_ie gce ul_bcos
                -- XXX What happens to these linked_bcos?

        let pls2 = pls1 { closure_env = final_gce,
                          itbl_env    = final_ie }

        return pls2

-- Link a bunch of BCOs and return them + updated closure env.
linkSomeBCOs :: DynFlags
             -> Bool    -- False <=> add _all_ BCOs to returned closure env
                        -- True  <=> add only toplevel BCOs to closure env
             -> ItblEnv
             -> ClosureEnv
             -> [UnlinkedBCO]
             -> IO (ClosureEnv, [HValue])
                        -- The returned HValues are associated 1-1 with
                        -- the incoming unlinked BCOs.  Each gives the
                        -- value of the corresponding unlinked BCO

linkSomeBCOs dflags toplevs_only ie ce_in ul_bcos
   = do let nms = map unlinkedBCOName ul_bcos
        hvals <- fixIO
                    ( \ hvs -> let ce_out = extendClosureEnv ce_in (zipLazy nms hvs)
                               in  mapM (linkBCO dflags ie ce_out) ul_bcos )
        let ce_all_additions = zip nms hvals
            ce_top_additions = filter (isExternalName.fst) ce_all_additions
            ce_additions     = if toplevs_only then ce_top_additions
                                               else ce_all_additions
            ce_out = -- make sure we're not inserting duplicate names into the
                     -- closure environment, which leads to trouble.
                     ASSERT(all (not . (`elemNameEnv` ce_in)) (map fst ce_additions))
                     extendClosureEnv ce_in ce_additions
        return (ce_out, hvals)

\end{code}


%************************************************************************
%*                                                                      *
                Unload some object modules
%*                                                                      *
%************************************************************************

\begin{code}
-- ---------------------------------------------------------------------------
-- | Unloading old objects ready for a new compilation sweep.
--
-- The compilation manager provides us with a list of linkables that it
-- considers \"stable\", i.e. won't be recompiled this time around.  For
-- each of the modules current linked in memory,
--
--   * if the linkable is stable (and it's the same one -- the user may have
--     recompiled the module on the side), we keep it,
--
--   * otherwise, we unload it.
--
--   * we also implicitly unload all temporary bindings at this point.
--
unload :: DynFlags
       -> [Linkable] -- ^ The linkables to *keep*.
       -> IO ()
unload dflags linkables
  = mask_ $ do -- mask, so we're safe from Ctrl-C in here

        -- Initialise the linker (if it's not been done already)
        initDynLinker dflags

        new_pls
            <- modifyPLS $ \pls -> do
                 pls1 <- unload_wkr dflags linkables pls
                 return (pls1, pls1)

        debugTraceMsg dflags 3 (text "unload: retaining objs" <+> ppr (objs_loaded new_pls))
        debugTraceMsg dflags 3 (text "unload: retaining bcos" <+> ppr (bcos_loaded new_pls))
        return ()

unload_wkr :: DynFlags
           -> [Linkable]                -- stable linkables
           -> PersistentLinkerState
           -> IO PersistentLinkerState
-- Does the core unload business
-- (the wrapper blocks exceptions and deals with the PLS get and put)

unload_wkr _ linkables pls
  = do  let (objs_to_keep, bcos_to_keep) = partition isObjectLinkable linkables

        objs_loaded' <- filterM (maybeUnload objs_to_keep) (objs_loaded pls)
        bcos_loaded' <- filterM (maybeUnload bcos_to_keep) (bcos_loaded pls)

        let bcos_retained = map linkableModule bcos_loaded'
            itbl_env'     = filterNameMap bcos_retained (itbl_env pls)
            closure_env'  = filterNameMap bcos_retained (closure_env pls)
            new_pls = pls { itbl_env = itbl_env',
                            closure_env = closure_env',
                            bcos_loaded = bcos_loaded',
                            objs_loaded = objs_loaded' }

        return new_pls
  where
    maybeUnload :: [Linkable] -> Linkable -> IO Bool
    maybeUnload keep_linkables lnk
      | linkableInSet lnk keep_linkables = return True
      | otherwise
      = do mapM_ unloadObj [f | DotO f <- linkableUnlinked lnk]
                -- The components of a BCO linkable may contain
                -- dot-o files.  Which is very confusing.
                --
                -- But the BCO parts can be unlinked just by
                -- letting go of them (plus of course depopulating
                -- the symbol table which is done in the main body)
           return False
\end{code}


%************************************************************************
%*                                                                      *
                Loading packages
%*                                                                      *
%************************************************************************


\begin{code}
data LibrarySpec
   = Object FilePath    -- Full path name of a .o file, including trailing .o
                        -- For dynamic objects only, try to find the object
                        -- file in all the directories specified in
                        -- v_Library_paths before giving up.

   | Archive FilePath   -- Full path name of a .a file, including trailing .a

   | DLL String         -- "Unadorned" name of a .DLL/.so
                        --  e.g.    On unix     "qt"  denotes "libqt.so"
                        --          On WinDoze  "burble"  denotes "burble.DLL"
                        --  loadDLL is platform-specific and adds the lib/.so/.DLL
                        --  suffixes platform-dependently

   | DLLPath FilePath   -- Absolute or relative pathname to a dynamic library
                        -- (ends with .dll or .so).

   | Framework String   -- Only used for darwin, but does no harm

-- If this package is already part of the GHCi binary, we'll already
-- have the right DLLs for this package loaded, so don't try to
-- load them again.
--
-- But on Win32 we must load them 'again'; doing so is a harmless no-op
-- as far as the loader is concerned, but it does initialise the list
-- of DLL handles that rts/Linker.c maintains, and that in turn is
-- used by lookupSymbol.  So we must call addDLL for each library
-- just to get the DLL handle into the list.
partOfGHCi :: [PackageName]
partOfGHCi
 | isWindowsHost || isDarwinHost = []
 | otherwise = map PackageName
                   ["base", "template-haskell", "editline"]

showLS :: LibrarySpec -> String
showLS (Object nm)    = "(static) " ++ nm
showLS (Archive nm)   = "(static archive) " ++ nm
showLS (DLL nm)       = "(dynamic) " ++ nm
showLS (DLLPath nm)   = "(dynamic) " ++ nm
showLS (Framework nm) = "(framework) " ++ nm

-- | Link exactly the specified packages, and their dependents (unless of
-- course they are already linked).  The dependents are linked
-- automatically, and it doesn't matter what order you specify the input
-- packages.
--
linkPackages :: DynFlags -> [PackageId] -> IO ()
-- NOTE: in fact, since each module tracks all the packages it depends on,
--       we don't really need to use the package-config dependencies.
--
-- However we do need the package-config stuff (to find aux libs etc),
-- and following them lets us load libraries in the right order, which
-- perhaps makes the error message a bit more localised if we get a link
-- failure.  So the dependency walking code is still here.

linkPackages dflags new_pkgs = do
  -- It's probably not safe to try to load packages concurrently, so we take
  -- a lock.
  initDynLinker dflags
  modifyPLS_ $ \pls -> do
    linkPackages' dflags new_pkgs pls

linkPackages' :: DynFlags -> [PackageId] -> PersistentLinkerState
             -> IO PersistentLinkerState
linkPackages' dflags new_pks pls = do
    pkgs' <- link (pkgs_loaded pls) new_pks
    return $! pls { pkgs_loaded = pkgs' }
  where
     pkg_map = pkgIdMap (pkgState dflags)
     ipid_map = installedPackageIdMap (pkgState dflags)

     link :: [PackageId] -> [PackageId] -> IO [PackageId]
     link pkgs new_pkgs =
         foldM link_one pkgs new_pkgs

     link_one pkgs new_pkg
        | new_pkg `elem` pkgs   -- Already linked
        = return pkgs

        | Just pkg_cfg <- lookupPackage pkg_map new_pkg
        = do {  -- Link dependents first
               pkgs' <- link pkgs [ Maybes.expectJust "link_one" $
                                    Map.lookup ipid ipid_map
                                  | ipid <- depends pkg_cfg ]
                -- Now link the package itself
             ; linkPackage dflags pkg_cfg
             ; return (new_pkg : pkgs') }

        | otherwise
        = throwGhcExceptionIO (CmdLineError ("unknown package: " ++ packageIdString new_pkg))


linkPackage :: DynFlags -> PackageConfig -> IO ()
linkPackage dflags pkg
   = do
        let platform  = targetPlatform dflags
            dirs      =  Packages.libraryDirs pkg

        let hs_libs   =  Packages.hsLibraries pkg
            -- The FFI GHCi import lib isn't needed as
            -- compiler/ghci/Linker.lhs + rts/Linker.c link the
            -- interpreted references to FFI to the compiled FFI.
            -- We therefore filter it out so that we don't get
            -- duplicate symbol errors.
            hs_libs'  =  filter ("HSffi" /=) hs_libs

        -- Because of slight differences between the GHC dynamic linker and
        -- the native system linker some packages have to link with a
        -- different list of libraries when using GHCi. Examples include: libs
        -- that are actually gnu ld scripts, and the possability that the .a
        -- libs do not exactly match the .so/.dll equivalents. So if the
        -- package file provides an "extra-ghci-libraries" field then we use
        -- that instead of the "extra-libraries" field.
            extra_libs =
                      (if null (Packages.extraGHCiLibraries pkg)
                            then Packages.extraLibraries pkg
                            else Packages.extraGHCiLibraries pkg)
                      ++ [ lib | '-':'l':lib <- Packages.ldOptions pkg ]

        hs_classifieds    <- mapM (locateLib dflags True  dirs) hs_libs'
        extra_classifieds <- mapM (locateLib dflags False dirs) extra_libs
        let classifieds = hs_classifieds ++ extra_classifieds

        -- Complication: all the .so's must be loaded before any of the .o's.
        let known_dlls = [ dll  | DLLPath dll    <- classifieds ]
            dlls       = [ dll  | DLL dll        <- classifieds ]
            objs       = [ obj  | Object obj     <- classifieds ]
            archs      = [ arch | Archive arch   <- classifieds ]

        maybePutStr dflags ("Loading package " ++ display (sourcePackageId pkg) ++ " ... ")

        -- See comments with partOfGHCi
        when (packageName pkg `notElem` partOfGHCi) $ do
            loadFrameworks platform pkg
            mapM_ load_dyn (known_dlls ++ map (mkSOName platform) dlls)

        -- After loading all the DLLs, we can load the static objects.
        -- Ordering isn't important here, because we do one final link
        -- step to resolve everything.
        mapM_ loadObj objs
        mapM_ loadArchive archs

        maybePutStr dflags "linking ... "
        ok <- resolveObjs
        if succeeded ok then maybePutStrLn dflags "done."
              else throwGhcExceptionIO (InstallationError ("unable to load package `" ++ display (sourcePackageId pkg) ++ "'"))

-- we have already searched the filesystem; the strings passed to load_dyn
-- can be passed directly to loadDLL.  They are either fully-qualified
-- ("/usr/lib/libfoo.so"), or unqualified ("libfoo.so").  In the latter case,
-- loadDLL is going to search the system paths to find the library.
--
load_dyn :: FilePath -> IO ()
load_dyn dll = do r <- loadDLL dll
                  case r of
                    Nothing  -> return ()
                    Just err -> throwGhcExceptionIO (CmdLineError ("can't load .so/.DLL for: "
                                                              ++ dll ++ " (" ++ err ++ ")" ))

loadFrameworks :: Platform -> InstalledPackageInfo_ ModuleName -> IO ()
loadFrameworks platform pkg
    = if platformUsesFrameworks platform
      then mapM_ load frameworks
      else return ()
  where
    fw_dirs    = Packages.frameworkDirs pkg
    frameworks = Packages.frameworks pkg

    load fw = do  r <- loadFramework fw_dirs fw
                  case r of
                    Nothing  -> return ()
                    Just err -> throwGhcExceptionIO (CmdLineError ("can't load framework: "
                                                        ++ fw ++ " (" ++ err ++ ")" ))

-- Try to find an object file for a given library in the given paths.
-- If it isn't present, we assume that addDLL in the RTS can find it,
-- which generally means that it should be a dynamic library in the
-- standard system search path.

locateLib :: DynFlags -> Bool -> [FilePath] -> String -> IO LibrarySpec
locateLib dflags is_hs dirs lib
  | not is_hs
    -- For non-Haskell libraries (e.g. gmp, iconv):
    --   first look in library-dirs for a dynamic library (libfoo.so)
    --   then  look in library-dirs for a static library (libfoo.a)
    --   then  try "gcc --print-file-name" to search gcc's search path
    --       for a dynamic library (#5289)
    --   otherwise, assume loadDLL can find it
    --
  = findDll `orElse` findArchive `orElse` tryGcc `orElse` assumeDll

  | not cDYNAMIC_GHC_PROGRAMS
    -- When the GHC package was not compiled as dynamic library
    -- (=DYNAMIC not set), we search for .o libraries or, if they
    -- don't exist, .a libraries.
  = findObject `orElse` findArchive `orElse` assumeDll

  | otherwise
    -- When the GHC package was compiled as dynamic library (=DYNAMIC set),
    -- we search for .so libraries first.
  = findHSDll `orElse` findDynObject `orElse` assumeDll
   where
     mk_obj_path      dir = dir </> (lib <.> "o")
     mk_dyn_obj_path  dir = dir </> (lib <.> "dyn_o")
     mk_arch_path     dir = dir </> ("lib" ++ lib <.> "a")

     hs_dyn_lib_name = lib ++ "-ghc" ++ cProjectVersion
     mk_hs_dyn_lib_path dir = dir </> mkHsSOName platform hs_dyn_lib_name

     so_name = mkSOName platform lib
     mk_dyn_lib_path dir = dir </> so_name

     findObject     = liftM (fmap Object)  $ findFile mk_obj_path        dirs
     findDynObject  = liftM (fmap Object)  $ findFile mk_dyn_obj_path    dirs
     findArchive    = liftM (fmap Archive) $ findFile mk_arch_path       dirs
     findHSDll      = liftM (fmap DLLPath) $ findFile mk_hs_dyn_lib_path dirs
     findDll        = liftM (fmap DLLPath) $ findFile mk_dyn_lib_path    dirs
     tryGcc         = liftM (fmap DLLPath) $ searchForLibUsingGcc dflags so_name dirs

     assumeDll   = return (DLL lib)
     infixr `orElse`
     f `orElse` g = do m <- f
                       case m of
                           Just x -> return x
                           Nothing -> g

     platform = targetPlatform dflags

searchForLibUsingGcc :: DynFlags -> String -> [FilePath] -> IO (Maybe FilePath)
searchForLibUsingGcc dflags so dirs = do
   str <- askCc dflags (map (FileOption "-L") dirs
                          ++ [Option "--print-file-name", Option so])
   let file = case lines str of
                []  -> ""
                l:_ -> l
   if (file == so)
      then return Nothing
      else return (Just file)

-- ----------------------------------------------------------------------------
-- Loading a dynamic library (dlopen()-ish on Unix, LoadLibrary-ish on Win32)

-- Darwin / MacOS X only: load a framework
-- a framework is a dynamic library packaged inside a directory of the same
-- name. They are searched for in different paths than normal libraries.
loadFramework :: [FilePath] -> FilePath -> IO (Maybe String)
loadFramework extraPaths rootname
   = do { either_dir <- tryIO getHomeDirectory
        ; let homeFrameworkPath = case either_dir of
                                  Left _ -> []
                                  Right dir -> [dir ++ "/Library/Frameworks"]
              ps = extraPaths ++ homeFrameworkPath ++ defaultFrameworkPaths
        ; mb_fwk <- findFile mk_fwk ps
        ; case mb_fwk of
            Just fwk_path -> loadDLL fwk_path
            Nothing       -> return (Just "not found") }
                -- Tried all our known library paths, but dlopen()
                -- has no built-in paths for frameworks: give up
   where
     mk_fwk dir = dir </> (rootname ++ ".framework/" ++ rootname)
        -- sorry for the hardcoded paths, I hope they won't change anytime soon:
     defaultFrameworkPaths = ["/Library/Frameworks", "/System/Library/Frameworks"]
\end{code}

%************************************************************************
%*                                                                      *
                Helper functions
%*                                                                      *
%************************************************************************

\begin{code}
findFile :: (FilePath -> FilePath)      -- Maps a directory path to a file path
         -> [FilePath]                  -- Directories to look in
         -> IO (Maybe FilePath)         -- The first file path to match
findFile _            [] = return Nothing
findFile mk_file_path (dir : dirs)
  = do let file_path = mk_file_path dir
       b <- doesFileExist file_path
       if b then return (Just file_path)
            else findFile mk_file_path dirs
\end{code}

\begin{code}
maybePutStr :: DynFlags -> String -> IO ()
maybePutStr dflags s
    = when (verbosity dflags > 0) $
          do let act = log_action dflags
             act dflags SevInteractive noSrcSpan defaultUserStyle (text s)

maybePutStrLn :: DynFlags -> String -> IO ()
maybePutStrLn dflags s = maybePutStr dflags (s ++ "\n")
\end{code}

%************************************************************************
%*                                                                      *
        Tunneling global variables into new instance of GHC library
%*                                                                      *
%************************************************************************

\begin{code}
saveLinkerGlobals :: IO (MVar PersistentLinkerState, Bool)
saveLinkerGlobals = liftM2 (,) (readIORef v_PersistentLinkerState) (readIORef v_InitLinkerDone)

restoreLinkerGlobals :: (MVar PersistentLinkerState, Bool) -> IO ()
restoreLinkerGlobals (pls, ild) = do
    writeIORef v_PersistentLinkerState pls
    writeIORef v_InitLinkerDone ild
\end{code}
