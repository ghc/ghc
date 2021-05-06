{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

-- | Functions for finding transitive dependencies of modules and packages
module GHC.Driver.Dependencies (findHomeModules, getLinkDeps
                               , LibrarySpec(..)
                               , loadPackagesX
                               , getGCCPaths
                               , locateLib
                               , addEnvPaths
                               , showLS
                               , computePackagesDeps
                               , computePackageDeps)
                               where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Ppr

import GHC.Iface.Load

import GHC.Tc.Utils.Monad

import GHC.Data.Maybe
import GHC.Data.FastString

import GHC.Utils.Error
import GHC.Utils.Panic
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Misc as Utils hiding ( eqListBy )
import GHC.Utils.Logger

import GHC.Unit.Finder
import GHC.Unit.State
import GHC.Unit.Home
import GHC.Unit.Module
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Deps
import GHC.Unit.Home.ModInfo

import Control.Monad
import Data.Function

import GHC.Types.Unique.FM

import GHC.Types.Unique.DSet
import qualified GHC.Data.Maybe as Maybes
import GHC.Linker.Types
import GHC.Data.List.SetOps
import Data.List ( isSuffixOf, nub, isPrefixOf, intercalate, sortBy )
import GHC.Runtime.Interpreter
import qualified GHC.Data.ShortText as ST
import qualified GHC.Unit as Packages
import GHC.Settings
import GHC.SysTools
import GHC.IO.Unsafe
import Data.Char
import System.Environment
import GHC.Platform.ArchOS
import GHC.Types.SrcLoc (SrcSpan)
import System.FilePath
import System.Directory
import GHC.Platform
import Data.IORef
import GHC.Platform.Ways
import GHC.Driver.Phases
import Data.Ord


-- | Used in OneShot mode to work out what the HPT looks like
findHomeModules :: HscEnv -> HomeUnit -> [ModuleNameWithIsBoot] -> IO (ModuleNameEnv (ModIface, ModuleNameWithIsBoot))
findHomeModules hsc_env home_unit mns = go mns mempty
  where
    go [] seen = return seen
    go (mn:mods) seen
      | Just (_, mn') <- lookupUFM seen (gwib_mod mn)
        -- Already seen the module before
      , gwib_isBoot mn' == gwib_isBoot mn = go mods seen
      | otherwise = do
         let mod = mkHomeModule home_unit (gwib_mod mn)
         -- NB: Compute interface is used here which avoids the caches but makes sure we actually get
         -- the right interface file..
         mb_iface <- computeInterface hsc_env (text "need mi_direct_deps for") (gwib_isBoot mn) mod
         -- loadInterface should work but if you load A.hi-boot before trying to load A.hi, then the A.hi-boot
         -- interface will be returned, which is not what you want.
--         mb_iface <- loadInterface  (text "need mi_direct_deps for") mod (ImportByUser (gwib_isBoot mn))
         case mb_iface of
           -- This case should **never** happen because there has to already be
           -- interface files for all the dependencies already in OneShot mode.
           -- However, we don't panic so we might get a more civilised error later.
           Failed _err -> ASSERT2(True, ppr mn) (pprPanic "failed" _err)
           Succeeded (imported_iface, _) ->
            let new_names = dep_direct_mods $ mi_deps imported_iface
                comb m@(_, (GWIB { gwib_isBoot = NotBoot })) _ = m
                comb (_, (GWIB { gwib_isBoot = IsBoot })) x  = x
            in go (new_names ++ mods)
                  (addToUFM_C comb seen (gwib_mod mn) (imported_iface, mn))

-- | Find all depedencies that we need to link, used for GHCi and
-- interface file dependency calculation
getLinkDeps :: SDoc
            -> HscEnv
            -> Interp
            -> HomePackageTable
            -- Already loaded things.
            -> ([UnitId] -- Packages
               , ([Linkable]) -- Objects
               , ([Linkable]))  -- BCOs
            -> SrcSpan                          -- for error messages
            -> [Module]                         -- If you need these modules
            -> [UnitId]                         -- and these packages
            -> IO ([Linkable], [UnitId])     -- ... then link these first
-- Fails with an IO exception if it can't find enough files

getLinkDeps herald hsc_env interp hpt (loaded_pkgs, objs, bcos) span mods pkgs
-- Find all the packages and linkables that a set of modules depends on
 = do {
        replace_osuf <- checkNonStdWay (hsc_dflags hsc_env) interp span
        -- 1.  Find the dependent home-pkg-modules/packages from each iface
      ; (mods_s, pkgs_s) <- follow_deps mods emptyUniqDSet emptyUniqDSet;

      ; let {
        -- 2.  Exclude ones already linked
        --      Main reason: avoid findModule calls in get_linkable
            mods_needed = mods_s `minusList` linked_mods     ;
            pkgs_needed = (pkgs ++ pkgs_s) `minusList` loaded_pkgs ;

            linked_mods = map (moduleName.linkableModule)
                                (objs ++ bcos )  }

        -- 3.  For each dependent module, find its linkable
        --     This will either be in the HPT or (in the case of one-shot
        --     compilation) we may need to use maybe_getFileLinkable
      ; let { osuf = objectSuf dflags }
      ; lnks_needed <- mapM (get_linkable replace_osuf osuf) mods_needed

      ; return (lnks_needed, pkgs_needed) }
  where
    dflags = hsc_dflags hsc_env

        -- The ModIface only contains the direct module dependencies
        -- within the current package so we have to recurse to find all
        -- the transitive dependencies.
        -- See bug #936, testcase ghci/prog007.
    follow_deps :: [Module]             -- modules to follow
                -> UniqDSet ModuleName         -- accum. module dependencies
                -> UniqDSet UnitId          -- accum. package dependencies
                -> IO ([ModuleName], [UnitId]) -- result
    follow_deps []     acc_mods acc_pkgs
        = return (uniqDSetToList acc_mods, uniqDSetToList acc_pkgs)
    follow_deps (mod:mods) acc_mods acc_pkgs
        = do
          mb_iface <- initIfaceCheck (text "getLinkDeps") hsc_env $
                        loadInterface msg mod (ImportByUser NotBoot)
          iface <- case mb_iface of
                    Maybes.Failed err      -> throwGhcExceptionIO (ProgramError (showSDoc dflags err))
                    Maybes.Succeeded iface -> return iface

          when (mi_boot iface == IsBoot) $ link_boot_mod_error mod

          let
            pkg = moduleUnit mod
            deps  = mi_deps iface
            home_unit = hsc_home_unit hsc_env

            pkg_deps = dep_direct_pkgs deps
            (boot_deps, mod_deps) = flip partitionWith (dep_direct_mods deps) $
              \ (GWIB { gwib_mod = m, gwib_isBoot = is_boot }) ->
                m & case is_boot of
                  IsBoot -> Left
                  NotBoot -> Right

            --
            mod_deps' = filter (not . (`elementOfUniqDSet` acc_mods)) (boot_deps ++ mod_deps)
            acc_mods'  = addListToUniqDSet acc_mods (moduleName mod : mod_deps)
            acc_pkgs'  = addListToUniqDSet acc_pkgs $ (map fst pkg_deps)
          --
          if not (isHomeUnit home_unit pkg)
             then follow_deps mods acc_mods (addOneToUniqDSet acc_pkgs' (toUnitId pkg))
             else follow_deps (map (mkHomeModule home_unit) mod_deps' ++ mods)
                              acc_mods' acc_pkgs'
        where
            msg = text "need to find module dependencies" <+> ppr mod <+>
                  text "due to" <+> herald


    link_boot_mod_error mod =
        throwGhcExceptionIO (ProgramError (showSDoc dflags (
            text "module" <+> ppr mod <+>
            text "cannot be linked; it is only available as a boot module")))

    no_obj :: Outputable a => a -> IO b
    no_obj mod = dieWith dflags span $
                     text "cannot find object file for module " <>
                        quotes (ppr mod) $$
                     while_linking_expr


    while_linking_expr = text "while finding linking dependencies for an expression"

        -- This one is a build-system bug

    get_linkable replace_osuf osuf mod_name      -- A home-package module
        | Just mod_info <- lookupHpt hpt mod_name
        = adjust_linkable (Maybes.expectJust "getLinkDeps" (hm_linkable mod_info))
        | otherwise
        = do    -- It's not in the HPT because we are in one shot mode,
                -- so use the Finder to get a ModLocation...
             let fc = hsc_FC hsc_env
             let home_unit = hsc_home_unit hsc_env
             let dflags = hsc_dflags hsc_env
             mb_stuff <- findHomeModule fc home_unit dflags mod_name
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
                let file_base = fromJust (stripExtension osuf file)
                    new_file = file_base <.> new_osuf
                ok <- doesFileExist new_file
                if (not ok)
                   then dieWith dflags span $
                          text "cannot find object file "
                                <> quotes (text new_file) $$ while_linking_expr
                   else return (DotO new_file)
            adjust_ul _ (DotA fp) = panic ("adjust_ul DotA " ++ show fp)
            adjust_ul _ (DotDLL fp) = panic ("adjust_ul DotDLL " ++ show fp)
            adjust_ul _ l@(BCOs {}) = return l


dieWith :: DynFlags -> SrcSpan -> SDoc -> IO a
dieWith dflags span msg = throwGhcExceptionIO (ProgramError (showSDoc dflags (mkLocMessage MCFatal span msg)))

   -- The interpreter and dynamic linker can only handle object code built
   -- the "normal" way, i.e. no non-std ways like profiling or ticky-ticky.
   -- So here we check the build tag: if we're building a non-standard way
   -- then we need to find & link object files built the "normal" way.

checkNonStdWay :: DynFlags -> Interp -> SrcSpan -> IO (Maybe FilePath)
checkNonStdWay dflags interp srcspan
  | ExternalInterp {} <- interpInstance interp = return Nothing
    -- with -fexternal-interpreter we load the .o files, whatever way
    -- they were built.  If they were built for a non-std way, then
    -- we will use the appropriate variant of the iserv binary to load them.

  | hostFullWays == targetFullWays = return Nothing
    -- Only if we are compiling with the same ways as GHC is built
    -- with, can we dynamically load those object files. (see #3604)

  | objectSuf dflags == normalObjectSuffix && not (null targetFullWays)
  = failNonStd dflags srcspan

  | otherwise = return (Just (hostWayTag ++ "o"))
  where
    targetFullWays = fullWays (ways dflags)
    hostWayTag = case waysTag hostFullWays of
                  "" -> ""
                  tag -> tag ++ "_"
normalObjectSuffix :: String
normalObjectSuffix = phaseInputExt StopLn

failNonStd :: DynFlags -> SrcSpan -> IO (Maybe FilePath)
failNonStd dflags srcspan = dieWith dflags srcspan $
  text "Cannot load" <+> compWay <+>
     text "objects when GHC is built" <+> ghciWay $$
  text "To fix this, either:" $$
  text "  (1) Use -fexternal-interpreter, or" $$
  text "  (2) Build the program twice: once" <+>
                       ghciWay <> text ", and then" $$
  text "      with" <+> compWay <+>
     text "using -osuf to set a different object file suffix."
    where compWay
            | WayDyn `elem` ways dflags = text "-dynamic"
            | WayProf `elem` ways dflags = text "-prof"
            | otherwise = text "normal"
          ghciWay
            | hostIsDynamic = text "with -dynamic"
            | hostIsProfiled = text "with -prof"
            | otherwise = text "the normal way"

loadPackagesX :: forall a . Monoid a => (UnitInfo -> IO a) -> HscEnv -> [UnitId] -> [UnitId] -> IO (a, [UnitId])
loadPackagesX k hsc_env new_pks loaded_pkgs = link (mempty, loaded_pkgs) new_pks
  where
     link :: (a, [UnitId]) -> [UnitId] -> IO (a, [UnitId])
     link pkgs new_pkgs =
         foldM link_one pkgs new_pkgs

     link_one :: ((a, [UnitId]) -> UnitId -> IO (a, [UnitId]))
     link_one (acc, pkgs) new_pkg
        | new_pkg `elem` pkgs   -- Already linked
        = return (acc, pkgs)

        | Just pkg_cfg <- lookupUnitId (hsc_units hsc_env) new_pkg
        = do {  -- Link dependents first
               (acc'', pkgs') <- link (acc, pkgs) (unitDepends pkg_cfg)
                -- Now link the package itself
             ; acc' <- k pkg_cfg
             ; return (acc' `mappend` acc'', new_pkg : pkgs') }

        | otherwise
        = throwGhcExceptionIO (CmdLineError ("unknown package: " ++ unpackFS (unitIdFS new_pkg)))

computePackagesDeps :: Interp -> HscEnv -> [UnitId] -> IO ([LibrarySpec], [UnitId])
computePackagesDeps interp hsc_env pkgs = loadPackagesX (computePackageDeps interp hsc_env) hsc_env pkgs []

computePackageDeps :: Interp -> HscEnv -> UnitInfo -> IO [LibrarySpec]
computePackageDeps interp hsc_env pkg
   = do
        let dflags    = hsc_dflags hsc_env
        let logger    = hsc_logger hsc_env
            platform  = targetPlatform dflags
            is_dyn    = interpreterDynamic interp
            dirs | is_dyn    = map ST.unpack $ Packages.unitLibraryDynDirs pkg
                 | otherwise = map ST.unpack $ Packages.unitLibraryDirs pkg

        let hs_libs   = map ST.unpack $ Packages.unitLibraries pkg
            -- The FFI GHCi import lib isn't needed as
            -- GHC.Linker.Loader + rts/Linker.c link the
            -- interpreted references to FFI to the compiled FFI.
            -- We therefore filter it out so that we don't get
            -- duplicate symbol errors.
            hs_libs'  =  filter ("HSffi" /=) hs_libs

        -- Because of slight differences between the GHC dynamic linker and
        -- the native system linker some packages have to link with a
        -- different list of libraries when using GHCi. Examples include: libs
        -- that are actually gnu ld scripts, and the possibility that the .a
        -- libs do not exactly match the .so/.dll equivalents. So if the
        -- package file provides an "extra-ghci-libraries" field then we use
        -- that instead of the "extra-libraries" field.
            extdeplibs = map ST.unpack (if null (Packages.unitExtDepLibsGhc pkg)
                                      then Packages.unitExtDepLibsSys pkg
                                      else Packages.unitExtDepLibsGhc pkg)
            linkerlibs = [ lib | '-':'l':lib <- (map ST.unpack $ Packages.unitLinkerOptions pkg) ]
            extra_libs = extdeplibs ++ linkerlibs

        -- See Note [Fork/Exec Windows]
        gcc_paths <- getGCCPaths logger dflags (platformOS platform)
        dirs_env <- addEnvPaths "LIBRARY_PATH" dirs

        hs_classifieds
           <- mapM (locateLib interp hsc_env True  dirs_env gcc_paths) hs_libs'
        extra_classifieds
           <- mapM (locateLib interp hsc_env False dirs_env gcc_paths) extra_libs
        let classifieds = hs_classifieds ++ extra_classifieds
        return classifieds

-- Try to find an object file for a given library in the given paths.
-- If it isn't present, we assume that addDLL in the RTS can find it,
-- which generally means that it should be a dynamic library in the
-- standard system search path.
-- For GHCi we tend to prefer dynamic libraries over static ones as
-- they are easier to load and manage, have less overhead.
locateLib
  :: Interp
  -> HscEnv
  -> Bool
  -> [FilePath]
  -> [FilePath]
  -> String
  -> IO LibrarySpec
locateLib interp hsc_env is_hs lib_dirs gcc_dirs lib
  | not is_hs
    -- For non-Haskell libraries (e.g. gmp, iconv):
    --   first look in library-dirs for a dynamic library (on User paths only)
    --   (libfoo.so)
    --   then  try looking for import libraries on Windows (on User paths only)
    --   (.dll.a, .lib)
    --   first look in library-dirs for a dynamic library (on GCC paths only)
    --   (libfoo.so)
    --   then  check for system dynamic libraries (e.g. kernel32.dll on windows)
    --   then  try looking for import libraries on Windows (on GCC paths only)
    --   (.dll.a, .lib)
    --   then  look in library-dirs for a static library (libfoo.a)
    --   then look in library-dirs and inplace GCC for a dynamic library (libfoo.so)
    --   then  try looking for import libraries on Windows (.dll.a, .lib)
    --   then  look in library-dirs and inplace GCC for a static library (libfoo.a)
    --   then  try "gcc --print-file-name" to search gcc's search path
    --       for a dynamic library (#5289)
    --   otherwise, assume loadDLL can find it
    --
    --   The logic is a bit complicated, but the rationale behind it is that
    --   loading a shared library for us is O(1) while loading an archive is
    --   O(n). Loading an import library is also O(n) so in general we prefer
    --   shared libraries because they are simpler and faster.
    --
  =
#if defined(CAN_LOAD_DLL)
    findDll   user `orElse`
#endif
    tryImpLib user `orElse`
#if defined(CAN_LOAD_DLL)
    findDll   gcc  `orElse`
    findSysDll     `orElse`
#endif
    tryImpLib gcc  `orElse`
    findArchive    `orElse`
    tryGcc         `orElse`
    assumeDll

  | loading_dynamic_hs_libs -- search for .so libraries first.
  = findHSDll     `orElse`
    findDynObject `orElse`
    assumeDll

  | otherwise
    -- use HSfoo.{o,p_o} if it exists, otherwise fallback to libHSfoo{,_p}.a
  = findObject  `orElse`
    findArchive `orElse`
    assumeDll

   where
     dflags = hsc_dflags hsc_env
     logger = hsc_logger hsc_env
     dirs   = lib_dirs ++ gcc_dirs
     gcc    = False
     user   = True

     obj_file
       | is_hs && loading_profiled_hs_libs = lib <.> "p_o"
       | otherwise = lib <.> "o"
     dyn_obj_file = lib <.> "dyn_o"
     arch_files = [ "lib" ++ lib ++ lib_tag <.> "a"
                  , lib <.> "a" -- native code has no lib_tag
                  , "lib" ++ lib, lib
                  ]
     lib_tag = if is_hs && loading_profiled_hs_libs then "_p" else ""

     loading_profiled_hs_libs = interpreterProfiled interp
     loading_dynamic_hs_libs  = interpreterDynamic  interp

     import_libs  = [ lib <.> "lib"           , "lib" ++ lib <.> "lib"
                    , "lib" ++ lib <.> "dll.a", lib <.> "dll.a"
                    ]

     hs_dyn_lib_name = lib ++ dynLibSuffix (ghcNameVersion dflags)
     hs_dyn_lib_file = platformHsSOName platform hs_dyn_lib_name

     so_name     = platformSOName platform lib
     lib_so_name = "lib" ++ so_name
     dyn_lib_file = case (arch, os) of
                             (ArchX86_64, OSSolaris2) -> "64" </> so_name
                             _ -> so_name

     findObject    = liftM (fmap $ Objects . (:[]))  $ findFile dirs obj_file
     findDynObject = liftM (fmap $ Objects . (:[]))  $ findFile dirs dyn_obj_file
     findArchive   = let local name = liftM (fmap Archive) $ findFile dirs name
                     in  apply (map local arch_files)
     findHSDll     = liftM (fmap DLLPath) $ findFile dirs hs_dyn_lib_file
     findDll    re = let dirs' = if re == user then lib_dirs else gcc_dirs
                     in liftM (fmap DLLPath) $ findFile dirs' dyn_lib_file
     findSysDll    = fmap (fmap $ DLL . dropExtension . takeFileName) $
                        findSystemLibrary interp so_name
     tryGcc        = let search   = searchForLibUsingGcc logger dflags
                         dllpath  = liftM (fmap DLLPath)
                         short    = dllpath $ search so_name lib_dirs
                         full     = dllpath $ search lib_so_name lib_dirs
                         gcc name = liftM (fmap Archive) $ search name lib_dirs
                         files    = import_libs ++ arch_files
                         dlls     = [short, full]
                         archives = map gcc files
                     in apply $
#if defined(CAN_LOAD_DLL)
                          dlls ++
#endif
                          archives
     tryImpLib re = case os of
                       OSMinGW32 ->
                        let dirs' = if re == user then lib_dirs else gcc_dirs
                            implib name = liftM (fmap Archive) $
                                            findFile dirs' name
                        in apply (map implib import_libs)
                       _         -> return Nothing

     -- TH Makes use of the interpreter so this failure is not obvious.
     -- So we are nice and warn/inform users why we fail before we do.
     -- But only for haskell libraries, as C libraries don't have a
     -- profiling/non-profiling distinction to begin with.
     assumeDll
      | is_hs
      , not loading_dynamic_hs_libs
      , interpreterProfiled interp
      = do
          warningMsg logger dflags
            (text "Interpreter failed to load profiled static library" <+> text lib <> char '.' $$
              text " \tTrying dynamic library instead. If this fails try to rebuild" <+>
              text "libraries with profiling support.")
          return (DLL lib)
      | otherwise = return (DLL lib)
     infixr `orElse`
     f `orElse` g = f >>= maybe g return

     apply :: [IO (Maybe a)] -> IO (Maybe a)
     apply []     = return Nothing
     apply (x:xs) = do x' <- x
                       if isJust x'
                          then return x'
                          else apply xs

     platform = targetPlatform dflags
     arch = platformArch platform
     os = platformOS platform

searchForLibUsingGcc :: Logger -> DynFlags -> String -> [FilePath] -> IO (Maybe FilePath)
searchForLibUsingGcc logger dflags so dirs = do
   -- GCC does not seem to extend the library search path (using -L) when using
   -- --print-file-name. So instead pass it a new base location.
   str <- askLd logger dflags (map (FileOption "-B") dirs
                          ++ [Option "--print-file-name", Option so])
   let file = case lines str of
                []  -> ""
                l:_ -> l
   if (file == so)
      then return Nothing
      else do b <- doesFileExist file -- file could be a folder (see #16063)
              return (if b then Just file else Nothing)

-- | Retrieve the list of search directory GCC and the System use to find
--   libraries and components. See Note [Fork/Exec Windows].
getGCCPaths :: Logger -> DynFlags -> OS -> IO [FilePath]
getGCCPaths logger dflags os
  = case os of
      OSMinGW32 ->
        do gcc_dirs <- getGccSearchDirectory logger dflags "libraries"
           sys_dirs <- getSystemDirectories
           return $ nub $ gcc_dirs ++ sys_dirs
      _         -> return []

-- | Cache for the GCC search directories as this can't easily change
--   during an invocation of GHC. (Maybe with some env. variable but we'll)
--   deal with that highly unlikely scenario then.
{-# NOINLINE gccSearchDirCache #-}
gccSearchDirCache :: IORef [(String, [String])]
gccSearchDirCache = unsafePerformIO $ newIORef []

-- Note [Fork/Exec Windows]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- fork/exec is expensive on Windows, for each time we ask GCC for a library we
-- have to eat the cost of af least 3 of these: gcc -> real_gcc -> cc1.
-- So instead get a list of location that GCC would search and use findDirs
-- which hopefully is written in an optimized mannor to take advantage of
-- caching. At the very least we remove the overhead of the fork/exec and waits
-- which dominate a large percentage of startup time on Windows.
getGccSearchDirectory :: Logger -> DynFlags -> String -> IO [FilePath]
getGccSearchDirectory logger dflags key = do
    cache <- readIORef gccSearchDirCache
    case lookup key cache of
      Just x  -> return x
      Nothing -> do
        str <- askLd logger dflags [Option "--print-search-dirs"]
        let line = dropWhile isSpace str
            name = key ++ ": ="
        if null line
          then return []
          else do let val = split $ find name line
                  dirs <- filterM doesDirectoryExist val
                  modifyIORef' gccSearchDirCache ((key, dirs):)
                  return val
      where split :: FilePath -> [FilePath]
            split r = case break (==';') r of
                        (s, []    ) -> [s]
                        (s, (_:xs)) -> s : split xs

            find :: String -> String -> String
            find r x = let lst = lines x
                           val = filter (r `isPrefixOf`) lst
                       in if null val
                             then []
                             else case break (=='=') (head val) of
                                     (_ , [])    -> []
                                     (_, (_:xs)) -> xs

-- | Get a list of system search directories, this to alleviate pressure on
-- the findSysDll function.
getSystemDirectories :: IO [FilePath]
#if defined(mingw32_HOST_OS)
getSystemDirectories = fmap (:[]) getSystemDirectory
#else
getSystemDirectories = return []
#endif

-- | Merge the given list of paths with those in the environment variable
--   given. If the variable does not exist then just return the identity.
addEnvPaths :: String -> [String] -> IO [String]
addEnvPaths name list
  = do -- According to POSIX (chapter 8.3) a zero-length prefix means current
       -- working directory. Replace empty strings in the env variable with
       -- `working_dir` (see also #14695).
       working_dir <- getCurrentDirectory
       values <- lookupEnv name
       case values of
         Nothing  -> return list
         Just arr -> return $ list ++ splitEnv working_dir arr
    where
      splitEnv :: FilePath -> String -> [String]
      splitEnv working_dir value =
        case break (== envListSep) value of
          (x, []    ) ->
            [if null x then working_dir else x]
          (x, (_:xs)) ->
            (if null x then working_dir else x) : splitEnv working_dir xs
#if defined(mingw32_HOST_OS)
      envListSep = ';'
#else
      envListSep = ':'
#endif

{- **********************************************************************

                Loading packages

  ********************************************************************* -}

data LibrarySpec
   = Objects [FilePath] -- Full path names of set of .o files, including trailing .o
                        -- We allow batched loading to ensure that cyclic symbol
                        -- references can be resolved (see #13786).
                        -- For dynamic objects only, try to find the object
                        -- file in all the directories specified in
                        -- v_Library_paths before giving up.

   | Archive FilePath   -- Full path name of a .a file, including trailing .a

   | DLL String         -- "Unadorned" name of a .DLL/.so
                        --  e.g.    On unix     "qt"  denotes "libqt.so"
                        --          On Windows  "burble"  denotes "burble.DLL" or "libburble.dll"
                        --  loadDLL is platform-specific and adds the lib/.so/.DLL
                        --  suffixes platform-dependently

   | DLLPath FilePath   -- Absolute or relative pathname to a dynamic library
                        -- (ends with .dll or .so).

   | Framework String   -- Only used for darwin, but does no harm

instance Outputable LibrarySpec where
  ppr (Objects objs) = text "Objects" <+> ppr objs
  ppr (Archive a) = text "Archive" <+> text a
  ppr (DLL s) = text "DLL" <+> text s
  ppr (DLLPath f) = text "DLLPath" <+> text f
  ppr (Framework s) = text "Framework" <+> text s


showLS :: LibrarySpec -> String
showLS (Objects nms)  = "(static) [" ++ intercalate ", " nms ++ "]"
showLS (Archive nm)   = "(static archive) " ++ nm
showLS (DLL nm)       = "(dynamic) " ++ nm
showLS (DLLPath nm)   = "(dynamic) " ++ nm
showLS (Framework nm) = "(framework) " ++ nm