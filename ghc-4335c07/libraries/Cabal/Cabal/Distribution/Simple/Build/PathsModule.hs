-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build.Macros
-- Copyright   :  Isaac Jones 2003-2005,
--                Ross Paterson 2006,
--                Duncan Coutts 2007-2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Generating the Paths_pkgname module.
--
-- This is a module that Cabal generates for the benefit of packages. It
-- enables them to find their version number and find any installed data files
-- at runtime. This code should probably be split off into another module.
--
module Distribution.Simple.Build.PathsModule (
    generate, pkgPathEnvVar
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.System
import Distribution.Simple.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.Text
import Distribution.Version

import System.FilePath ( pathSeparator )

-- ------------------------------------------------------------
-- * Building Paths_<pkg>.hs
-- ------------------------------------------------------------

generate :: PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> String
generate pkg_descr lbi clbi =
   let pragmas = cpp_pragma ++ ffi_pragmas ++ warning_pragmas

       cpp_pragma | supports_cpp = "{-# LANGUAGE CPP #-}\n"
                  | otherwise    = ""

       ffi_pragmas
        | absolute = ""
        | supports_language_pragma =
          "{-# LANGUAGE ForeignFunctionInterface #-}\n"
        | otherwise =
          "{-# OPTIONS_GHC -fffi #-}\n"++
          "{-# OPTIONS_JHC -fffi #-}\n"

       warning_pragmas =
        "{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}\n"++
        "{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}\n"

       foreign_imports
        | absolute = ""
        | otherwise =
          "import Foreign\n"++
          "import Foreign.C\n"

       reloc_imports
        | reloc =
          "import System.Environment (getExecutablePath)\n"
        | otherwise = ""

       header =
        pragmas++
        "module " ++ display paths_modulename ++ " (\n"++
        "    version,\n"++
        "    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,\n"++
        "    getDataFileName, getSysconfDir\n"++
        "  ) where\n"++
        "\n"++
        foreign_imports++
        "import qualified Control.Exception as Exception\n"++
        "import Data.Version (Version(..))\n"++
        "import System.Environment (getEnv)\n"++
        reloc_imports ++
        "import Prelude\n"++
        "\n"++
        (if supports_cpp
         then
           ("#if defined(VERSION_base)\n"++
            "\n"++
            "#if MIN_VERSION_base(4,0,0)\n"++
            "catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a\n"++
            "#else\n"++
            "catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a\n"++
            "#endif\n"++
            "\n"++
            "#else\n"++
            "catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a\n"++
            "#endif\n")
         else
           "catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a\n")++
        "catchIO = Exception.catch\n" ++
        "\n"++
        "version :: Version"++
        "\nversion = Version " ++ show branch ++ " []"
          where branch = versionNumbers $ packageVersion pkg_descr

       body
        | reloc =
          "\n\nbindirrel :: FilePath\n" ++
          "bindirrel = " ++ show flat_bindirreloc ++
          "\n"++
          "\ngetBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath\n"++
          "getBinDir = "++mkGetEnvOrReloc "bindir" flat_bindirreloc++"\n"++
          "getLibDir = "++mkGetEnvOrReloc "libdir" flat_libdirreloc++"\n"++
          "getDynLibDir = "++mkGetEnvOrReloc "libdir" flat_dynlibdirreloc++"\n"++
          "getDataDir = "++mkGetEnvOrReloc "datadir" flat_datadirreloc++"\n"++
          "getLibexecDir = "++mkGetEnvOrReloc "libexecdir" flat_libexecdirreloc++"\n"++
          "getSysconfDir = "++mkGetEnvOrReloc "sysconfdir" flat_sysconfdirreloc++"\n"++
          "\n"++
          "getDataFileName :: FilePath -> IO FilePath\n"++
          "getDataFileName name = do\n"++
          "  dir <- getDataDir\n"++
          "  return (dir `joinFileName` name)\n"++
          "\n"++
          get_prefix_reloc_stuff++
          "\n"++
          filename_stuff
        | absolute =
          "\nbindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath\n"++
          "\nbindir     = " ++ show flat_bindir ++
          "\nlibdir     = " ++ show flat_libdir ++
          "\ndynlibdir  = " ++ show flat_dynlibdir ++
          "\ndatadir    = " ++ show flat_datadir ++
          "\nlibexecdir = " ++ show flat_libexecdir ++
          "\nsysconfdir = " ++ show flat_sysconfdir ++
          "\n"++
          "\ngetBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath\n"++
          "getBinDir = "++mkGetEnvOr "bindir" "return bindir"++"\n"++
          "getLibDir = "++mkGetEnvOr "libdir" "return libdir"++"\n"++
          "getDynLibDir = "++mkGetEnvOr "dynlibdir" "return dynlibdir"++"\n"++
          "getDataDir = "++mkGetEnvOr "datadir" "return datadir"++"\n"++
          "getLibexecDir = "++mkGetEnvOr "libexecdir" "return libexecdir"++"\n"++
          "getSysconfDir = "++mkGetEnvOr "sysconfdir" "return sysconfdir"++"\n"++
          "\n"++
          "getDataFileName :: FilePath -> IO FilePath\n"++
          "getDataFileName name = do\n"++
          "  dir <- getDataDir\n"++
          "  return (dir ++ "++path_sep++" ++ name)\n"
        | otherwise =
          "\nprefix, bindirrel :: FilePath" ++
          "\nprefix        = " ++ show flat_prefix ++
          "\nbindirrel     = " ++ show (fromMaybe (error "PathsModule.generate") flat_bindirrel) ++
          "\n\n"++
          "getBinDir :: IO FilePath\n"++
          "getBinDir = getPrefixDirRel bindirrel\n\n"++
          "getLibDir :: IO FilePath\n"++
          "getLibDir = "++mkGetDir flat_libdir flat_libdirrel++"\n\n"++
          "getDynLibDir :: IO FilePath\n"++
          "getDynLibDir = "++mkGetDir flat_dynlibdir flat_dynlibdirrel++"\n\n"++
          "getDataDir :: IO FilePath\n"++
          "getDataDir =  "++ mkGetEnvOr "datadir"
                              (mkGetDir flat_datadir flat_datadirrel)++"\n\n"++
          "getLibexecDir :: IO FilePath\n"++
          "getLibexecDir = "++mkGetDir flat_libexecdir flat_libexecdirrel++"\n\n"++
          "getSysconfDir :: IO FilePath\n"++
          "getSysconfDir = "++mkGetDir flat_sysconfdir flat_sysconfdirrel++"\n\n"++
          "getDataFileName :: FilePath -> IO FilePath\n"++
          "getDataFileName name = do\n"++
          "  dir <- getDataDir\n"++
          "  return (dir `joinFileName` name)\n"++
          "\n"++
          get_prefix_stuff++
          "\n"++
          filename_stuff
   in header++body

 where
        cid = componentUnitId clbi

        InstallDirs {
          prefix     = flat_prefix,
          bindir     = flat_bindir,
          libdir     = flat_libdir,
          dynlibdir  = flat_dynlibdir,
          datadir    = flat_datadir,
          libexecdir = flat_libexecdir,
          sysconfdir = flat_sysconfdir
        } = absoluteComponentInstallDirs pkg_descr lbi cid NoCopyDest
        InstallDirs {
          bindir     = flat_bindirrel,
          libdir     = flat_libdirrel,
          dynlibdir  = flat_dynlibdirrel,
          datadir    = flat_datadirrel,
          libexecdir = flat_libexecdirrel,
          sysconfdir = flat_sysconfdirrel
        } = prefixRelativeComponentInstallDirs (packageId pkg_descr) lbi cid

        flat_bindirreloc = shortRelativePath flat_prefix flat_bindir
        flat_libdirreloc = shortRelativePath flat_prefix flat_libdir
        flat_dynlibdirreloc = shortRelativePath flat_prefix flat_dynlibdir
        flat_datadirreloc = shortRelativePath flat_prefix flat_datadir
        flat_libexecdirreloc = shortRelativePath flat_prefix flat_libexecdir
        flat_sysconfdirreloc = shortRelativePath flat_prefix flat_sysconfdir

        mkGetDir _   (Just dirrel) = "getPrefixDirRel " ++ show dirrel
        mkGetDir dir Nothing       = "return " ++ show dir

        mkGetEnvOrReloc var dirrel = "catchIO (getEnv \""++var'++"\")" ++
                                     " (\\_ -> getPrefixDirReloc \"" ++ dirrel ++
                                     "\")"
          where var' = pkgPathEnvVar pkg_descr var

        mkGetEnvOr var expr = "catchIO (getEnv \""++var'++"\")"++
                              " (\\_ -> "++expr++")"
          where var' = pkgPathEnvVar pkg_descr var

        -- In several cases we cannot make relocatable installations
        absolute =
             hasLibs pkg_descr        -- we can only make progs relocatable
          || isNothing flat_bindirrel -- if the bin dir is an absolute path
          || not (supportsRelocatableProgs (compilerFlavor (compiler lbi)))

        reloc = relocatable lbi

        supportsRelocatableProgs GHC  = case buildOS of
                           Windows   -> True
                           _         -> False
        supportsRelocatableProgs GHCJS = case buildOS of
                           Windows   -> True
                           _         -> False
        supportsRelocatableProgs _    = False

        paths_modulename = autogenPathsModuleName pkg_descr

        get_prefix_stuff = get_prefix_win32 buildArch

        path_sep = show [pathSeparator]

        supports_cpp = compilerFlavor (compiler lbi) == GHC

        supports_language_pragma =
          (compilerFlavor (compiler lbi) == GHC &&
            (compilerVersion (compiler lbi)
              `withinRange` orLaterVersion (mkVersion [6,6,1]))) ||
           compilerFlavor (compiler lbi) == GHCJS

-- | Generates the name of the environment variable controlling the path
-- component of interest.
--
-- Note: The format of these strings is part of Cabal's public API;
-- changing this function constitutes a *backwards-compatibility* break.
pkgPathEnvVar :: PackageDescription
              -> String     -- ^ path component; one of \"bindir\", \"libdir\",
                            -- \"datadir\", \"libexecdir\", or \"sysconfdir\"
              -> String     -- ^ environment variable name
pkgPathEnvVar pkg_descr var =
    showPkgName (packageName pkg_descr) ++ "_" ++ var
    where
        showPkgName = map fixchar . display
        fixchar '-' = '_'
        fixchar c   = c

get_prefix_reloc_stuff :: String
get_prefix_reloc_stuff =
  "getPrefixDirReloc :: FilePath -> IO FilePath\n"++
  "getPrefixDirReloc dirRel = do\n"++
  "  exePath <- getExecutablePath\n"++
  "  let (bindir,_) = splitFileName exePath\n"++
  "  return ((bindir `minusFileName` bindirrel) `joinFileName` dirRel)\n"

get_prefix_win32 :: Arch -> String
get_prefix_win32 arch =
  "getPrefixDirRel :: FilePath -> IO FilePath\n"++
  "getPrefixDirRel dirRel = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.\n"++
  "  where\n"++
  "    try_size size = allocaArray (fromIntegral size) $ \\buf -> do\n"++
  "        ret <- c_GetModuleFileName nullPtr buf size\n"++
  "        case ret of\n"++
  "          0 -> return (prefix `joinFileName` dirRel)\n"++
  "          _ | ret < size -> do\n"++
  "              exePath <- peekCWString buf\n"++
  "              let (bindir,_) = splitFileName exePath\n"++
  "              return ((bindir `minusFileName` bindirrel) `joinFileName` dirRel)\n"++
  "            | otherwise  -> try_size (size * 2)\n"++
  "\n"++
  "foreign import " ++ cconv ++ " unsafe \"windows.h GetModuleFileNameW\"\n"++
  "  c_GetModuleFileName :: Ptr () -> CWString -> Int32 -> IO Int32\n"
    where cconv = case arch of
                  I386 -> "stdcall"
                  X86_64 -> "ccall"
                  _ -> error "win32 supported only with I386, X86_64"

filename_stuff :: String
filename_stuff =
  "minusFileName :: FilePath -> String -> FilePath\n"++
  "minusFileName dir \"\"     = dir\n"++
  "minusFileName dir \".\"    = dir\n"++
  "minusFileName dir suffix =\n"++
  "  minusFileName (fst (splitFileName dir)) (fst (splitFileName suffix))\n"++
  "\n"++
  "joinFileName :: String -> String -> FilePath\n"++
  "joinFileName \"\"  fname = fname\n"++
  "joinFileName \".\" fname = fname\n"++
  "joinFileName dir \"\"    = dir\n"++
  "joinFileName dir fname\n"++
  "  | isPathSeparator (last dir) = dir++fname\n"++
  "  | otherwise                  = dir++pathSeparator:fname\n"++
  "\n"++
  "splitFileName :: FilePath -> (String, String)\n"++
  "splitFileName p = (reverse (path2++drive), reverse fname)\n"++
  "  where\n"++
  "    (path,drive) = case p of\n"++
  "       (c:':':p') -> (reverse p',[':',c])\n"++
  "       _          -> (reverse p ,\"\")\n"++
  "    (fname,path1) = break isPathSeparator path\n"++
  "    path2 = case path1 of\n"++
  "      []                           -> \".\"\n"++
  "      [_]                          -> path1   -- don't remove the trailing slash if \n"++
  "                                              -- there is only one character\n"++
  "      (c:path') | isPathSeparator c -> path'\n"++
  "      _                             -> path1\n"++
  "\n"++
  "pathSeparator :: Char\n"++
  (case buildOS of
       Windows   -> "pathSeparator = '\\\\'\n"
       _         -> "pathSeparator = '/'\n") ++
  "\n"++
  "isPathSeparator :: Char -> Bool\n"++
  (case buildOS of
       Windows   -> "isPathSeparator c = c == '/' || c == '\\\\'\n"
       _         -> "isPathSeparator c = c == '/'\n")
