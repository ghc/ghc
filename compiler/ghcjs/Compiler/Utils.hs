{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings #-}

#include <ghcplatform.h>

-- | Compiler utility functions, mainly dealing with IO and files
module Compiler.Utils
    (
      -- * File utils
      touchFile
    , copyNoOverwrite
    , writeBinaryFile
    , readBinaryFile
    , readBinaryFileL
    , findFile
    , jsexeExtension
    , addExeExtension
    , exeFileName
    , mkGhcjsSuf
    , mkGhcjsOutput
    , substPatterns
      -- * Source code and JS related utilities
    , ghcjsSrcSpan
    , isJsFile
      -- * User interaction
    , compilationProgressMsg
      -- * Environment
    , getEnvMay
    , getEnvOpt
      -- * Preprocessing for JS
    , doCpp
      -- * Error messages
    , simpleSrcErr
    ) where

import           DynFlags
import           GHC
import           GHC.Platform
import           HscTypes
import           Bag
-- import           FileCleanup
import           Module
import           Outputable        hiding ((<>))
import           ErrUtils          (mkPlainErrMsg)
import           Packages          (getPackageIncludePath, Version
                                   ,versionBranch, packageNameString
                                   ,packageVersion, lookupPackage
                                   ,explicitPackages, PackageConfig)
-- import           Config            (cProjectVersionInt)
import           Panic             (throwGhcExceptionIO)
import qualified SysTools
import           FileCleanup ( newTempName, TempFileLifetime(..) )
import           LlvmCodeGen ( llvmVersionList )
-- import           Panic

import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.IO.Class
import Prelude

import           Data.Char
import           Data.List         (isPrefixOf, isInfixOf, foldl', intercalate)
import           Data.Maybe        (mapMaybe)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Version      (showVersion)

import           System.Directory  (doesFileExist, copyFile)
import           System.Environment
import           System.FilePath

import           Gen2.Utils

import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL

import           System.IO (withBinaryFile, Handle, IOMode(ReadMode, WriteMode))

import qualified Compiler.Platform as Platform

touchFile :: DynFlags -> FilePath -> IO ()
touchFile df file = do
  e <- doesFileExist file
  when e (SysTools.touch df "keep build system happy" file)

copyNoOverwrite :: FilePath -> FilePath -> IO ()
copyNoOverwrite from to = do
  ef <- doesFileExist from
  et <- doesFileExist to
  when (ef && not et) (copyFile from to)

findFile :: (FilePath -> FilePath)      -- Maps a directory path to a file path
         -> [FilePath]                  -- Directories to look in
         -> IO (Maybe FilePath)         -- The first file path to match
findFile _            [] = return Nothing
findFile mk_file_path (dir : dirs)
  = do let file_path = mk_file_path dir
       b <- doesFileExist file_path
       if b then return (Just file_path)
            else findFile mk_file_path dirs

{-
      macOS has trouble writing more than 2GiB at once to a file
  (tested with 10.14.6), and the base library doesn't work around this
  problem yet (tested with GHC 8.6), so we work around it here.

  in this workaround we write a binary file in chunks of 1 GiB
 -}
writeBinaryFile :: FilePath -> ByteString -> IO ()
writeBinaryFile file bs =
  withBinaryFile file WriteMode $ \h -> mapM_ (B.hPut h) (chunks bs)
  where
    -- split the ByteString into a nonempty list of chunks of at most 1GiB
    chunks :: ByteString -> [ByteString]
    chunks b =
      let (b1, b2) = B.splitAt 1073741824 b
      in  b1 : if B.null b1 then [] else chunks b2

readBinaryFile :: FilePath -> IO ByteString
readBinaryFile file = mconcat <$> readBinaryFileChunks file

readBinaryFileL :: FilePath -> IO BL.ByteString
readBinaryFileL file = BL.fromChunks <$> readBinaryFileChunks file

readBinaryFileChunks :: FilePath -> IO [ByteString]
readBinaryFileChunks file =
  withBinaryFile file ReadMode getChunks
  where
    getChunks :: Handle -> IO [ByteString]
    getChunks h = do
      chunk <- B.hGet h 1073741824
      if B.null chunk then pure [chunk]
                      else (chunk:) <$> getChunks h

jsexeExtension :: String
jsexeExtension = "jsexe"

addExeExtension :: FilePath -> FilePath
addExeExtension
  | Platform.isWindows = (<.> "exe")
  | otherwise          = id

exeFileName :: DynFlags -> FilePath
exeFileName dflags
  | Just s <- outputFile dflags =
      -- unmunge the extension
      let s' = dropPrefix "js_" (drop 1 $ takeExtension s)
      in if null s' || (Platform.isWindows && map toLower s' == "exe")
           then dropExtension s <.> jsexeExtension
           else dropExtension s <.> s'
  | otherwise =
      if platformOS (targetPlatform dflags) == OSMinGW32
           then "main.jsexe"
           else "a.jsexe"
  where
    dropPrefix prefix xs
      | prefix `isPrefixOf` xs = drop (length prefix) xs
      | otherwise              = xs

isJsFile :: FilePath -> Bool
isJsFile = (`elem` exts) . takeExtensions
  where
    exts = [".js", ".mjs", ".js.pp", ".mjs.pp" ]


mkGhcjsOutput :: String -> String
mkGhcjsOutput xs = xs -- "" = ""
{-
mkGhcjsOutput "" = ""
mkGhcjsOutput file
  | null ext         = file
  | ext == ".js"     = file
  | ext == ".hi"     = replaceExtension file ".js_hi"
  | ext == ".o"      = replaceExtension file ".js_o"
  | ext == ".dyn_hi" = replaceExtension file ".js_dyn_hi"
  | ext == ".dyn_o"  = replaceExtension file ".js_dyn_o"
  |  ".js_" `isPrefixOf` ext || "_js_" `isInfixOf` ext = file
  | otherwise        = replaceExtension file (".js_" ++ drop 1 ext)
  where
    ext = takeExtension file
-}

mkGhcjsSuf :: String -> String
mkGhcjsSuf xs = xs
{-
mkGhcjsSuf "o"      = "js_o"
mkGhcjsSuf "hi"     = "js_hi"
mkGhcjsSuf "dyn_o"  = "js_dyn_o"
mkGhcjsSuf "dyn_hi" = "js_dyn_hi"
mkGhcjsSuf xs | "js_" `isPrefixOf` xs || "_js_" `isInfixOf` xs = xs
mkGhcjsSuf xs       = "js_" ++ xs -- is this correct?
-}

getEnvMay :: String -> IO (Maybe String)
getEnvMay xs = fmap Just (getEnv xs)
               `Ex.catch` \(_::Ex.SomeException) -> return Nothing

getEnvOpt :: MonadIO m => String -> m Bool
getEnvOpt xs = liftIO (maybe False ((`notElem` ["0","no"]).map toLower) <$> getEnvMay xs)

doCpp :: DynFlags -> Bool -> FilePath -> FilePath -> IO ()
doCpp dflags raw input_fn output_fn = do
    let hscpp_opts = picPOpts dflags
    let cmdline_include_paths = includePaths dflags

    pkg_include_dirs <- getPackageIncludePath dflags []
    let include_paths_global = foldr (\ x xs -> ("-I" ++ x) : xs) []
          (includePathsGlobal cmdline_include_paths ++ pkg_include_dirs)
    let include_paths_quote = foldr (\ x xs -> ("-iquote" ++ x) : xs) []
          (includePathsQuote cmdline_include_paths)
    let include_paths = include_paths_quote ++ include_paths_global

    let verbFlags = getVerbFlags dflags

    let cpp_prog args | raw       = SysTools.runCpp dflags args
                      | otherwise = SysTools.runCc Nothing dflags (SysTools.Option "-E" : args)


    let target_defs =
          [ "-D" ++ HOST_OS     ++ "_BUILD_OS=1",
            "-D" ++ HOST_ARCH   ++ "_BUILD_ARCH=1",
            "-Dghcjs_HOST_OS=1",
            "-Dghcjs_HOST_ARCH=1" ]
        -- remember, in code we *compile*, the HOST is the same our TARGET,
        -- and BUILD is the same as our HOST.

    -- profiling related definitions
    let prof_defs =
          if buildingProf dflags then ["-DGHCJS_PROF"] else []

    let sse_defs =
          [ "-D__SSE__"      | isSseEnabled      dflags ] ++
          [ "-D__SSE2__"     | isSse2Enabled     dflags ] ++
          [ "-D__SSE4_2__"   | isSse4_2Enabled   dflags ]

    let avx_defs =
          [ "-D__AVX__"      | isAvxEnabled      dflags ] ++
          [ "-D__AVX2__"     | isAvx2Enabled     dflags ] ++
          [ "-D__AVX512CD__" | isAvx512cdEnabled dflags ] ++
          [ "-D__AVX512ER__" | isAvx512erEnabled dflags ] ++
          [ "-D__AVX512F__"  | isAvx512fEnabled  dflags ] ++
          [ "-D__AVX512PF__" | isAvx512pfEnabled dflags ]

    backend_defs <- getBackendDefs dflags

    let th_defs = [ "-D__GLASGOW_HASKELL_TH__" ]
    -- Default CPP defines in Haskell source
    ghcVersionH <- getGhcVersionPathName dflags
    let hsSourceCppOpts = [ "-include", ghcVersionH ]

    -- MIN_VERSION macros
    let uids = explicitPackages (pkgState dflags)
        pkgs = mapMaybe (lookupPackage dflags) uids
    mb_macro_include <-
        if not (null pkgs) && gopt Opt_VersionMacros dflags
            then do macro_stub <- newTempName dflags TFL_CurrentModule "h"
                    writeFile macro_stub (generatePackageVersionMacros pkgs)
                    -- Include version macros for every *exposed* package.
                    -- Without -hide-all-packages and with a package database
                    -- size of 1000 packages, it takes cpp an estimated 2
                    -- milliseconds to process this file. See Trac #10970
                    -- comment 8.
                    return [SysTools.FileOption "-include" macro_stub]
            else return []

    cpp_prog       (   map SysTools.Option verbFlags
                    ++ map SysTools.Option include_paths
                    ++ map SysTools.Option hsSourceCppOpts
                    ++ map SysTools.Option target_defs
                    ++ map SysTools.Option prof_defs
                    ++ map SysTools.Option backend_defs
                    ++ map SysTools.Option th_defs
                    ++ map SysTools.Option hscpp_opts
                    ++ map SysTools.Option sse_defs
                    ++ map SysTools.Option avx_defs
                    ++ mb_macro_include
        -- Set the language mode to assembler-with-cpp when preprocessing. This
        -- alleviates some of the C99 macro rules relating to whitespace and the hash
        -- operator, which we tend to abuse. Clang in particular is not very happy
        -- about this.
                    ++ [ SysTools.Option     "-x"
                       , SysTools.Option     "assembler-with-cpp"
                       , SysTools.Option     input_fn
        -- We hackily use Option instead of FileOption here, so that the file
        -- name is not back-slashed on Windows.  cpp is capable of
        -- dealing with / in filenames, so it works fine.  Furthermore
        -- if we put in backslashes, cpp outputs #line directives
        -- with *double* backslashes.   And that in turn means that
        -- our error messages get double backslashes in them.
        -- In due course we should arrange that the lexer deals
        -- with these \\ escapes properly.
                       , SysTools.Option     "-o"
                       , SysTools.FileOption "" output_fn
                       ])

getBackendDefs :: DynFlags -> IO [String]
getBackendDefs dflags | hscTarget dflags == HscLlvm = do
    llvmVer <- SysTools.figureLlvmVersion dflags
    return $ case fmap llvmVersionList llvmVer of
               Just [m] -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,0) ]
               Just (m:n:_) -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,n) ]
               _ -> []
  where
    format (major, minor)
      | minor >= 100 = error "getBackendDefs: Unsupported minor version"
      | otherwise = show $ (100 * major + minor :: Int) -- Contract is Int

getBackendDefs _ =
    return []

-- | Find out path to @ghcversion.h@ file
getGhcVersionPathName :: DynFlags -> IO FilePath
getGhcVersionPathName dflags = do
  candidates <- case ghcVersionFile dflags of
    Just path -> return [path]
    Nothing -> (map (</> "ghcversion.h")) <$>
               (getPackageIncludePath dflags [toInstalledUnitId rtsUnitId])

  found <- filterM doesFileExist candidates
  case found of
      []    -> throwGhcExceptionIO (InstallationError
                                    ("ghcversion.h missing; tried: "
                                      ++ intercalate ", " candidates))
      (x:_) -> return x

simpleSrcErr :: DynFlags -> SrcSpan -> String -> SourceError
simpleSrcErr df span msg = mkSrcErr (unitBag errMsg)
  where
    errMsg = mkPlainErrMsg df span (text msg)

-- | replace {abc} and {{abc}} patterns (case sensitive), {abc} replaced first
--   unknown {{abc}} patterns are replaced by the empty string
substPatterns :: [(Text,Text)] -- ^ replace {abc}   -> def
              -> [(Text,Text)] -- ^ replace {{abc}} -> def
              -> Text          -- ^ input
              -> Text          -- ^ result
substPatterns single double = unmatched
                            . f substDouble double
                            . f substSingle single
  where
    f g xs z = foldl' (flip g) z xs
    substSingle (var, val) = T.replace ("{"<>var<>"}") val
    substDouble (var, val) = T.replace ("{{"<>var<>"}}") val
    unmatched l | T.null b || T.null d = l
                | otherwise            = a <> unmatched d
          where (a,b)  = T.breakOn "{{" l
                (_c,d) = T.breakOn "}}" b

-- ---------------------------------------------------------------------------
-- Macros (cribbed from Cabal)

generatePackageVersionMacros :: [PackageConfig] -> String
generatePackageVersionMacros pkgs = concat
  -- Do not add any C-style comments. See Trac #3389.
  [ generateMacros "" pkgname version
  | pkg <- pkgs
  , let version = packageVersion pkg
        pkgname = map fixchar (packageNameString pkg)
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
    (major1:major2:minor:_) = map show (versionBranch version ++ repeat 0)
