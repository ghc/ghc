{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

#include <ghcplatform.h>

module GHC.SysTools.Cpp
  ( doCpp
  , CppOpts (..)
  , getGhcVersionPathName
  , applyCDefs
  , offsetIncludePaths
  )
where

import GHC.Prelude
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.CmmToLlvm.Config
import GHC.Platform
import GHC.Platform.ArchOS

import GHC.SysTools

import GHC.Unit.Env
import GHC.Unit.Info
import GHC.Unit.State
import GHC.Unit.Types

import GHC.Utils.Logger
import GHC.Utils.TmpFs
import GHC.Utils.Panic

import Data.Version
import Data.List (intercalate)
import Data.Maybe

import Control.Monad

import System.Directory
import System.FilePath

data CppOpts = CppOpts
  { cppUseCc       :: !Bool -- ^ Use "cc -E" as preprocessor, otherwise use "cpp"
  , cppLinePragmas :: !Bool -- ^ Enable generation of LINE pragmas
  }

-- | Run CPP
--
-- UnitEnv is needed to compute MIN_VERSION macros
doCpp :: Logger -> TmpFs -> DynFlags -> UnitEnv -> CppOpts -> FilePath -> FilePath -> IO ()
doCpp logger tmpfs dflags unit_env opts input_fn output_fn = do
    let hscpp_opts = picPOpts dflags
    let cmdline_include_paths = offsetIncludePaths dflags (includePaths dflags)
    let unit_state = ue_units unit_env
    pkg_include_dirs <- mayThrowUnitErr
                        (collectIncludeDirs <$> preloadUnitsInfo unit_env)
    -- MP: This is not quite right, the headers which are supposed to be installed in
    -- the package might not be the same as the provided include paths, but it's a close
    -- enough approximation for things to work. A proper solution would be to have to declare which paths should
    -- be propagated to dependent packages.
    let home_pkg_deps =
         [homeUnitEnv_dflags . ue_findHomeUnitEnv uid $ unit_env | uid <- ue_transitiveHomeDeps (ue_currentUnit unit_env) unit_env]
        dep_pkg_extra_inputs = [offsetIncludePaths fs (includePaths fs) | fs <- home_pkg_deps]

    let include_paths_global = foldr (\ x xs -> ("-I" ++ x) : xs) []
          (includePathsGlobal cmdline_include_paths ++ pkg_include_dirs
                                                    ++ concatMap includePathsGlobal dep_pkg_extra_inputs)
    let include_paths_quote = foldr (\ x xs -> ("-iquote" ++ x) : xs) []
          (includePathsQuote cmdline_include_paths ++
           includePathsQuoteImplicit cmdline_include_paths)
    let include_paths = include_paths_quote ++ include_paths_global

    let verbFlags = getVerbFlags dflags

    let cpp_prog args
          | cppUseCc opts = GHC.SysTools.runCc Nothing logger tmpfs dflags
                                               (GHC.SysTools.Option "-E" : args)
          | otherwise     = GHC.SysTools.runCpp logger dflags args

    let platform   = targetPlatform dflags
        targetArch = stringEncodeArch $ platformArch platform
        targetOS = stringEncodeOS $ platformOS platform
        isWindows = platformOS platform == OSMinGW32
    let target_defs =
          [ "-D" ++ HOST_OS     ++ "_BUILD_OS",
            "-D" ++ HOST_ARCH   ++ "_BUILD_ARCH",
            "-D" ++ targetOS    ++ "_HOST_OS",
            "-D" ++ targetArch  ++ "_HOST_ARCH" ]
        -- remember, in code we *compile*, the HOST is the same our TARGET,
        -- and BUILD is the same as our HOST.

    let io_manager_defs =
          [ "-D__IO_MANAGER_WINIO__=1" | isWindows ] ++
          [ "-D__IO_MANAGER_MIO__=1"               ]

    let sse_defs =
          [ "-D__SSE__"      | isSseEnabled      platform ] ++
          [ "-D__SSE2__"     | isSse2Enabled     platform ] ++
          [ "-D__SSE4_2__"   | isSse4_2Enabled   dflags ]

    let avx_defs =
          [ "-D__AVX__"      | isAvxEnabled      dflags ] ++
          [ "-D__AVX2__"     | isAvx2Enabled     dflags ] ++
          [ "-D__AVX512CD__" | isAvx512cdEnabled dflags ] ++
          [ "-D__AVX512ER__" | isAvx512erEnabled dflags ] ++
          [ "-D__AVX512F__"  | isAvx512fEnabled  dflags ] ++
          [ "-D__AVX512PF__" | isAvx512pfEnabled dflags ]

    backend_defs <- applyCDefs (backendCDefs $ backend dflags) logger dflags

    let th_defs = [ "-D__GLASGOW_HASKELL_TH__" ]
    -- Default CPP defines in Haskell source
    ghcVersionH <- getGhcVersionPathName dflags unit_env
    let hsSourceCppOpts = [ "-include", ghcVersionH ]

    -- MIN_VERSION macros
    let uids = explicitUnits unit_state
        pkgs = mapMaybe (lookupUnit unit_state . fst) uids
    mb_macro_include <-
        if not (null pkgs) && gopt Opt_VersionMacros dflags
            then do macro_stub <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "h"
                    writeFile macro_stub (generatePackageVersionMacros pkgs)
                    -- Include version macros for every *exposed* package.
                    -- Without -hide-all-packages and with a package database
                    -- size of 1000 packages, it takes cpp an estimated 2
                    -- milliseconds to process this file. See #10970
                    -- comment 8.
                    return [GHC.SysTools.FileOption "-include" macro_stub]
            else return []

    let line_pragmas
          | cppLinePragmas opts = [] -- on by default
          | otherwise           = [GHC.SysTools.Option "-P"] -- disable LINE markers

    cpp_prog       (   map GHC.SysTools.Option verbFlags
                    ++ map GHC.SysTools.Option include_paths
                    ++ map GHC.SysTools.Option hsSourceCppOpts
                    ++ map GHC.SysTools.Option target_defs
                    ++ map GHC.SysTools.Option backend_defs
                    ++ map GHC.SysTools.Option th_defs
                    ++ map GHC.SysTools.Option hscpp_opts
                    ++ map GHC.SysTools.Option sse_defs
                    ++ map GHC.SysTools.Option avx_defs
                    ++ map GHC.SysTools.Option io_manager_defs
                    ++ mb_macro_include
                    ++ line_pragmas
        -- Set the language mode to assembler-with-cpp when preprocessing. This
        -- alleviates some of the C99 macro rules relating to whitespace and the hash
        -- operator, which we tend to abuse. Clang in particular is not very happy
        -- about this.
                    ++ [ GHC.SysTools.Option     "-x"
                       , GHC.SysTools.Option     "assembler-with-cpp"
                       , GHC.SysTools.Option     input_fn
        -- We hackily use Option instead of FileOption here, so that the file
        -- name is not back-slashed on Windows.  cpp is capable of
        -- dealing with / in filenames, so it works fine.  Furthermore
        -- if we put in backslashes, cpp outputs #line directives
        -- with *double* backslashes.   And that in turn means that
        -- our error messages get double backslashes in them.
        -- In due course we should arrange that the lexer deals
        -- with these \\ escapes properly.
                       , GHC.SysTools.Option     "-o"
                       , GHC.SysTools.FileOption "" output_fn
                       ])

-- ---------------------------------------------------------------------------
-- Macros (cribbed from Cabal)

generatePackageVersionMacros :: [UnitInfo] -> String
generatePackageVersionMacros pkgs = concat
  -- Do not add any C-style comments. See #3389.
  [ generateMacros "" pkgname version
  | pkg <- pkgs
  , let version = unitPackageVersion pkg
        pkgname = map fixchar (unitPackageNameString pkg)
  ]

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c   = c

generateMacros :: String -> String -> Version -> String
generateMacros prefix name version =
  concat
  ["#define ", prefix, "VERSION_",name," ",show (showVersion version),"\n"
  ,"#define MIN_", prefix, "VERSION_",name,"(major1,major2,minor) (\\\n"
  ,"  (major1) <  ",major1," || \\\n"
  ,"  (major1) == ",major1," && (major2) <  ",major2," || \\\n"
  ,"  (major1) == ",major1," && (major2) == ",major2," && (minor) <= ",minor,")"
  ,"\n\n"
  ]
  where
    take3 = \case
      (a:b:c:_) -> (a,b,c)
      _         -> error "take3"
    (major1,major2,minor) = take3 $ map show (versionBranch version) ++ repeat "0"


-- | Find out path to @ghcversion.h@ file
getGhcVersionPathName :: DynFlags -> UnitEnv -> IO FilePath
getGhcVersionPathName dflags unit_env = do
  let candidates = case ghcVersionFile dflags of
        -- the user has provided an explicit `ghcversion.h` file to use.
        Just path -> [path]
        -- otherwise, try to find it in the rts' include-dirs.
        -- Note: only in the RTS include-dirs! not all preload units less we may
        -- use a wrong file. See #25106 where a globally installed
        -- /usr/include/ghcversion.h file was used instead of the one provided
        -- by the rts.
        Nothing -> case lookupUnitId (ue_units unit_env) rtsUnitId of
          Nothing   -> []
          Just info -> (</> "ghcversion.h") <$> collectIncludeDirs [info]

  found <- filterM doesFileExist candidates
  case found of
      []    -> throwGhcExceptionIO (InstallationError
                                    ("ghcversion.h missing; tried: "
                                      ++ intercalate ", " candidates))
      (x:_) -> return x

applyCDefs :: DefunctionalizedCDefs -> Logger -> DynFlags -> IO [String]
applyCDefs NoCDefs _ _ = return []
applyCDefs LlvmCDefs logger dflags = do
    llvmVer <- figureLlvmVersion logger dflags
    return $ case fmap llvmVersionList llvmVer of
               Just [m] -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,0) ]
               Just (m:n:_) -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,n) ]
               _ -> []
  where
    format (major, minor)
      | minor >= 100 = error "backendCDefs: Unsupported minor version"
      | otherwise = show (100 * major + minor :: Int) -- Contract is Int


-- Note [Filepaths and Multiple Home Units]
offsetIncludePaths :: DynFlags -> IncludeSpecs -> IncludeSpecs
offsetIncludePaths dflags (IncludeSpecs incs quotes impl) =
     let go = map (augmentByWorkingDirectory dflags)
     in IncludeSpecs (go incs) (go quotes) (go impl)
