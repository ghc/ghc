{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.JHC
-- Copyright   :  Isaac Jones 2003-2006
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the JHC-specific code for configuring, building
-- and installing packages.

module Distribution.Simple.JHC (
        configure, getInstalledPackages,
        buildLib, buildExe,
        installLib, installExe
 ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.InstalledPackageInfo
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Language.Haskell.Extension
import Distribution.Simple.Program
import Distribution.Types.MungedPackageId (mungedName)
import Distribution.Types.PackageId
import Distribution.Types.UnitId
import Distribution.Version
import Distribution.Package
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Text

import System.FilePath          ( (</>) )
import Distribution.Compat.ReadP
    ( readP_to_S, string, skipSpaces )
import Distribution.System ( Platform )

import qualified Data.Map as Map  ( empty )

import qualified Data.ByteString.Lazy.Char8 as BS.Char8


-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramDb -> IO (Compiler, Maybe Platform, ProgramDb)
configure verbosity hcPath _hcPkgPath progdb = do

  (jhcProg, _, progdb') <- requireProgramVersion verbosity
                           jhcProgram (orLaterVersion (mkVersion [0,7,2]))
                           (userMaybeSpecifyPath "jhc" hcPath progdb)

  let Just version = programVersion jhcProg
      comp = Compiler {
        compilerId             = CompilerId JHC version,
        compilerAbiTag         = NoAbiTag,
        compilerCompat         = [],
        compilerLanguages      = jhcLanguages,
        compilerExtensions     = jhcLanguageExtensions,
        compilerProperties     = Map.empty
      }
      compPlatform = Nothing
  return (comp, compPlatform, progdb')

jhcLanguages :: [(Language, Flag)]
jhcLanguages = [(Haskell98, "")]

-- | The flags for the supported extensions
jhcLanguageExtensions :: [(Extension, Flag)]
jhcLanguageExtensions =
    [(EnableExtension  TypeSynonymInstances       , "")
    ,(DisableExtension TypeSynonymInstances       , "")
    ,(EnableExtension  ForeignFunctionInterface   , "")
    ,(DisableExtension ForeignFunctionInterface   , "")
    ,(EnableExtension  ImplicitPrelude            , "") -- Wrong
    ,(DisableExtension ImplicitPrelude            , "--noprelude")
    ,(EnableExtension  CPP                        , "-fcpp")
    ,(DisableExtension CPP                        , "-fno-cpp")
    ]

getInstalledPackages :: Verbosity -> PackageDBStack -> ProgramDb
                    -> IO InstalledPackageIndex
getInstalledPackages verbosity _packageDBs progdb = do
   -- jhc --list-libraries lists all available libraries.
   -- How shall I find out, whether they are global or local
   -- without checking all files and locations?
   str <- getDbProgramOutput verbosity jhcProgram progdb ["--list-libraries"]
   let pCheck :: [(a, String)] -> [a]
       pCheck rs = [ r | (r,s) <- rs, all isSpace s ]
   let parseLine ln =
          pCheck (readP_to_S
             (skipSpaces >> string "Name:" >> skipSpaces >> parse) ln)
   return $
      PackageIndex.fromList $
      map (\p -> emptyInstalledPackageInfo {
                    InstalledPackageInfo.installedUnitId = mkLegacyUnitId p,
                    InstalledPackageInfo.sourcePackageId = p
                 }) $
      concatMap parseLine $
      lines str

-- -----------------------------------------------------------------------------
-- Building

-- | Building a package for JHC.
-- Currently C source files are not supported.
buildLib :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity pkg_descr lbi lib clbi = do
  let Just jhcProg = lookupProgram jhcProgram (withPrograms lbi)
  let libBi = libBuildInfo lib
  let args  = constructJHCCmdLine lbi libBi clbi (buildDir lbi) verbosity
  let pkgid = display (packageId pkg_descr)
      pfile = buildDir lbi </> "jhc-pkg.conf"
      hlfile= buildDir lbi </> (pkgid ++ ".hl")
  writeFileAtomic pfile . BS.Char8.pack $ jhcPkgConf pkg_descr
  runProgram verbosity jhcProg $
     ["--build-hl="++pfile, "-o", hlfile] ++
     args ++ map display (allLibModules lib clbi)

-- | Building an executable for JHC.
-- Currently C source files are not supported.
buildExe :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe verbosity _pkg_descr lbi exe clbi = do
  let Just jhcProg = lookupProgram jhcProgram (withPrograms lbi)
  let exeBi = buildInfo exe
  let out   = buildDir lbi </> display (exeName exe)
  let args  = constructJHCCmdLine lbi exeBi clbi (buildDir lbi) verbosity
  runProgram verbosity jhcProg (["-o",out] ++ args ++ [modulePath exe])

constructJHCCmdLine :: LocalBuildInfo -> BuildInfo -> ComponentLocalBuildInfo
                    -> FilePath -> Verbosity -> [String]
constructJHCCmdLine lbi bi clbi _odir verbosity =
        (if verbosity >= deafening then ["-v"] else [])
     ++ hcOptions JHC bi
     ++ languageToFlags (compiler lbi) (defaultLanguage bi)
     ++ extensionsToFlags (compiler lbi) (usedExtensions bi)
     ++ ["--noauto","-i-"]
     ++ concat [["-i", l] | l <- nub (hsSourceDirs bi)]
     ++ ["-i", autogenComponentModulesDir lbi clbi]
     ++ ["-i", autogenPackageModulesDir lbi]
     ++ ["-optc" ++ opt | opt <- PD.ccOptions bi]
     -- It would be better if JHC would accept package names with versions,
     -- but JHC-0.7.2 doesn't accept this.
     -- Thus, we have to strip the version with 'pkgName'.
     ++ (concat [ ["-p", display (mungedName pkgid)]
                | (_, pkgid) <- componentPackageDeps clbi ])

jhcPkgConf :: PackageDescription -> String
jhcPkgConf pd =
  let sline name sel = name ++ ": "++sel pd
      lib pd' = case library pd' of
                Just lib' -> lib'
                Nothing -> error "no library available"
      comma = intercalate "," . map display
  in unlines [sline "name" (display . pkgName . packageId)
             ,sline "version" (display . pkgVersion . packageId)
             ,sline "exposed-modules" (comma . PD.exposedModules . lib)
             ,sline "hidden-modules" (comma . otherModules . libBuildInfo . lib)
             ]

installLib :: Verbosity
           -> LocalBuildInfo
           -> FilePath
           -> FilePath
           -> FilePath
           -> PackageDescription
           -> Library
           -> ComponentLocalBuildInfo
           -> IO ()
installLib verb _lbi dest _dyn_dest build_dir pkg_descr _lib _clbi = do
    let p = display (packageId pkg_descr)++".hl"
    createDirectoryIfMissingVerbose verb True dest
    installOrdinaryFile verb (build_dir </> p) (dest </> p)

installExe :: Verbosity -> FilePath -> FilePath -> (FilePath,FilePath) -> PackageDescription -> Executable -> IO ()
installExe verb dest build_dir (progprefix,progsuffix) _ exe = do
    let exe_name = display $ exeName exe
        src = exe_name </> exeExtension
        out   = (progprefix ++ exe_name ++ progsuffix) </> exeExtension
    createDirectoryIfMissingVerbose verb True dest
    installExecutableFile verb (build_dir </> src) (dest </> out)
