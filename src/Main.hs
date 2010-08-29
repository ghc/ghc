{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Haddock - A Haskell Documentation Tool
--
-- Program entry point and top-level code.
-----------------------------------------------------------------------------
module Main (main, readPackagesAndProcessModules) where


import Haddock.Backends.Xhtml
import Haddock.Backends.Xhtml.Themes (getThemes)
import Haddock.Backends.LaTeX
import Haddock.Backends.Hoogle
import Haddock.Interface
import Haddock.Lex
import Haddock.Parse
import Haddock.Types
import Haddock.Version
import Haddock.InterfaceFile
import Haddock.Options
import Haddock.Utils
import Haddock.GhcUtils

import Control.Monad
import Control.Exception
import Data.Maybe
import Data.IORef
import qualified Data.Map as Map
import System.IO
import System.Exit
import System.Environment

#if defined(mingw32_HOST_OS)
import Foreign
import Foreign.C
import Data.Int
#endif

#ifdef IN_GHC_TREE
import System.FilePath
#else
import GHC.Paths
import Paths_haddock
#endif

import GHC hiding (flags, verbosity)
import Config
import DynFlags hiding (flags, verbosity)
import Panic (handleGhcException)
import Module


--------------------------------------------------------------------------------
-- * Exception handling
--------------------------------------------------------------------------------


handleTopExceptions :: IO a -> IO a
handleTopExceptions =
  handleNormalExceptions . handleHaddockExceptions . handleGhcExceptions


-- | Either returns normally or throws an ExitCode exception;
-- all other exceptions are turned into exit exceptions.
handleNormalExceptions :: IO a -> IO a
handleNormalExceptions inner =
  (inner `onException` hFlush stdout)
  `catches`
  [  Handler (\(code :: ExitCode) -> exitWith code)
  ,  Handler (\(StackOverflow) -> do
        putStrLn "stack overflow: use -g +RTS -K<size> to increase it"
        exitFailure)
  ,  Handler (\(ex :: SomeException) -> do
        putStrLn ("haddock: internal Haddock or GHC error: " ++ show ex)
        exitFailure)
  ]


handleHaddockExceptions :: IO a -> IO a
handleHaddockExceptions inner =
  catches inner [Handler handler]
  where
    handler (e::HaddockException) = do
      putStrLn $ "haddock: " ++ show e
      exitFailure


handleGhcExceptions :: IO a -> IO a
handleGhcExceptions =
  -- error messages propagated as exceptions
  handleGhcException $ \e -> do
    hFlush stdout
    case e of
      PhaseFailed _ code -> exitWith code
#if ! MIN_VERSION_ghc(6,13,0)
      Interrupted -> exitFailure
#endif
      _ -> do
        print (e :: GhcException)
        exitFailure


-------------------------------------------------------------------------------
-- * Top level
-------------------------------------------------------------------------------


main :: IO ()
main = handleTopExceptions $ do

  -- Parse command-line flags and handle some of them initially.
  args <- getArgs
  (flags, files) <- parseHaddockOpts args
  shortcutFlags flags

  if not (null files)
    then do
      (packages, ifaces, homeLinks) <- readPackagesAndProcessModules flags files

      -- Dump an "interface file" (.haddock file), if requested.
      case optDumpInterfaceFile flags of
        Just f -> dumpInterfaceFile f (map toInstalledIface ifaces) homeLinks
        Nothing -> return ()

      -- Render the interfaces.
      renderStep flags packages ifaces

    else do
      when (any (`elem` [Flag_Html, Flag_Hoogle, Flag_LaTeX]) flags) $
        throwE "No input file(s)."

      -- Get packages supplied with --read-interface.
      packages <- readInterfaceFiles freshNameCache (readIfaceArgs flags)

      -- Render even though there are no input files (usually contents/index).
      renderStep flags packages []


readPackagesAndProcessModules :: [Flag] -> [String]
                              -> IO ([(DocPaths, InterfaceFile)], [Interface], LinkEnv)
readPackagesAndProcessModules flags files = do
  libDir <- getGhcLibDir flags

  -- Catches all GHC source errors, then prints and re-throws them.
  let handleSrcErrors action' = flip handleSourceError action' $ \err -> do
        printExceptionAndWarnings err
        liftIO exitFailure

  -- Initialize GHC.
  withGhc libDir (ghcFlags flags) $ \_ -> handleSrcErrors $ do

    -- Get packages supplied with --read-interface.
    packages <- readInterfaceFiles nameCacheFromGhc (readIfaceArgs flags)

    -- Create the interfaces -- this is the core part of Haddock.
    let ifaceFiles = map snd packages
    (ifaces, homeLinks) <- processModules (verbosity flags) files flags ifaceFiles

    return (packages, ifaces, homeLinks)


renderStep :: [Flag] -> [(DocPaths, InterfaceFile)] -> [Interface] -> IO ()
renderStep flags pkgs interfaces = do
  updateHTMLXRefs pkgs
  let
    ifaceFiles = map snd pkgs
    installedIfaces = concatMap ifInstalledIfaces ifaceFiles
    srcMap = Map.fromList [ (ifPackageId if_, x) | ((_, Just x), if_) <- pkgs ]
  render flags interfaces installedIfaces srcMap


-- | Render the interfaces with whatever backend is specified in the flags.
render :: [Flag] -> [Interface] -> [InstalledInterface] -> SrcMap -> IO ()
render flags ifaces installedIfaces srcMap = do

  let
    title                = fromMaybe "" (optTitle flags)
    unicode              = Flag_UseUnicode `elem` flags
    opt_wiki_urls        = wikiUrls          flags
    opt_contents_url     = optContentsUrl    flags
    opt_index_url        = optIndexUrl       flags
    odir                 = outputDir         flags
    opt_latex_style      = optLaTeXStyle     flags

    visibleIfaces    = [ i | i <- ifaces, OptHide `notElem` ifaceOptions i ]

    -- /All/ visible interfaces including external package modules.
    allIfaces        = map toInstalledIface ifaces ++ installedIfaces
    allVisibleIfaces = [ i | i <- allIfaces, OptHide `notElem` instOptions i ]

    pkgMod           = ifaceMod (head ifaces)
    pkgId            = modulePackageId pkgMod
    pkgStr           = Just (packageIdString pkgId)
    (pkgName,pkgVer) = modulePackageInfo pkgMod

    (srcBase, srcModule, srcEntity) = sourceUrls flags
    srcMap' = maybe srcMap (\path -> Map.insert pkgId path srcMap) srcEntity
    sourceUrls' = (srcBase, srcModule, srcMap')

  libDir   <- getHaddockLibDir flags
  prologue <- getPrologue flags
  themes <- getThemes libDir flags >>= either bye return

  when (Flag_GenIndex `elem` flags) $ do
    ppHtmlIndex odir title pkgStr
                themes opt_contents_url sourceUrls' opt_wiki_urls
                allVisibleIfaces
    copyHtmlBits odir libDir themes

  when (Flag_GenContents `elem` flags) $ do
    ppHtmlContents odir title pkgStr
                   themes opt_index_url sourceUrls' opt_wiki_urls
                   allVisibleIfaces True prologue
    copyHtmlBits odir libDir themes

  when (Flag_Html `elem` flags) $ do
    ppHtml title pkgStr visibleIfaces odir
                prologue
                themes sourceUrls' opt_wiki_urls
                opt_contents_url opt_index_url unicode
    copyHtmlBits odir libDir themes

  when (Flag_Hoogle `elem` flags) $ do
    let pkgName2 = if pkgName == "main" && title /= [] then title else pkgName
    ppHoogle pkgName2 pkgVer title prologue visibleIfaces odir

  when (Flag_LaTeX `elem` flags) $ do
    ppLaTeX title pkgStr visibleIfaces odir prologue opt_latex_style
                  libDir

-------------------------------------------------------------------------------
-- * Reading and dumping interface files
-------------------------------------------------------------------------------


readInterfaceFiles :: MonadIO m =>
                      NameCacheAccessor m
                   -> [(DocPaths, FilePath)] ->
                      m [(DocPaths, InterfaceFile)]
readInterfaceFiles name_cache_accessor pairs = do
  mbPackages <- mapM tryReadIface pairs
  return (catMaybes mbPackages)
  where
    -- try to read an interface, warn if we can't
    tryReadIface (paths, file) = do
      eIface <- readInterfaceFile name_cache_accessor file
      case eIface of
        Left err -> liftIO $ do
          putStrLn ("Warning: Cannot read " ++ file ++ ":")
          putStrLn ("   " ++ err)
          putStrLn "Skipping this interface."
          return Nothing
        Right f -> return $ Just (paths, f)


dumpInterfaceFile :: FilePath -> [InstalledInterface] -> LinkEnv -> IO ()
dumpInterfaceFile path ifaces homeLinks = writeInterfaceFile path ifaceFile
  where
    ifaceFile = InterfaceFile {
        ifInstalledIfaces = ifaces,
        ifLinkEnv         = homeLinks
      }


-------------------------------------------------------------------------------
-- * Creating a GHC session
-------------------------------------------------------------------------------


-- | Start a GHC session with the -haddock flag set. Also turn off
-- compilation and linking. Then run the given 'Ghc' action.
withGhc :: String -> [String] -> (DynFlags -> Ghc a) -> IO a
withGhc libDir flags ghcActs = do
  -- TODO: handle warnings?
  (restFlags, _) <- parseStaticFlags (map noLoc flags)
  runGhc (Just libDir) $ do
    dynflags  <- getSessionDynFlags
    let dynflags' = dopt_set dynflags Opt_Haddock
    let dynflags'' = dynflags' {
        hscTarget = HscNothing,
        ghcMode   = CompManager,
        ghcLink   = NoLink
      }
    dynflags''' <- parseGhcFlags dynflags'' restFlags flags
    defaultCleanupHandler dynflags''' $ do
        -- ignore the following return-value, which is a list of packages
        -- that may need to be re-linked: Haddock doesn't do any
        -- dynamic or static linking at all!
        _ <- setSessionDynFlags dynflags'''
        ghcActs dynflags'''
  where
    parseGhcFlags :: Monad m => DynFlags -> [Located String]
                  -> [String] -> m DynFlags
    parseGhcFlags dynflags flags_ origFlags = do
      -- TODO: handle warnings?
      (dynflags', rest, _) <- parseDynamicFlags dynflags flags_
      if not (null rest)
        then throwE ("Couldn't parse GHC options: " ++ unwords origFlags)
        else return dynflags'


-------------------------------------------------------------------------------
-- * Misc
-------------------------------------------------------------------------------


getHaddockLibDir :: [Flag] -> IO String
getHaddockLibDir flags =
  case [str | Flag_Lib str <- flags] of
    [] ->
#ifdef IN_GHC_TREE
      getInTreeLibDir
#else
      getDataDir -- provided by Cabal
#endif
    fs -> return (last fs)


getGhcLibDir :: [Flag] -> IO String
getGhcLibDir flags =
  case [ dir | Flag_GhcLibDir dir <- flags ] of
    [] ->
#ifdef IN_GHC_TREE
      getInTreeLibDir
#else
      return libdir -- from GHC.Paths
#endif
    xs -> return $ last xs


shortcutFlags :: [Flag] -> IO ()
shortcutFlags flags = do
  usage <- getUsage

  when (Flag_Help           `elem` flags) (bye usage)
  when (Flag_Version        `elem` flags) byeVersion
  when (Flag_GhcVersion     `elem` flags) byeGhcVersion

  when (Flag_PrintGhcLibDir `elem` flags) $ do
    dir <- getGhcLibDir flags
    bye $ dir ++ "\n"

  when (Flag_UseUnicode `elem` flags && Flag_Html `notElem` flags) $
    throwE "Unicode can only be enabled for HTML output."

  when ((Flag_GenIndex `elem` flags || Flag_GenContents `elem` flags)
        && Flag_Html `elem` flags) $
    throwE "-h cannot be used with --gen-index or --gen-contents"

  when ((Flag_GenIndex `elem` flags || Flag_GenContents `elem` flags)
        && Flag_Hoogle `elem` flags) $
    throwE "--hoogle cannot be used with --gen-index or --gen-contents"

  when ((Flag_GenIndex `elem` flags || Flag_GenContents `elem` flags)
        && Flag_LaTeX `elem` flags) $
    throwE "--latex cannot be used with --gen-index or --gen-contents"
  where
    byeVersion = bye $
      "Haddock version " ++ projectVersion ++ ", (c) Simon Marlow 2006\n"
      ++ "Ported to use the GHC API by David Waern 2006-2008\n"

    byeGhcVersion = bye (cProjectVersion ++ "\n")


updateHTMLXRefs :: [(DocPaths, InterfaceFile)] -> IO ()
updateHTMLXRefs packages = writeIORef html_xrefs_ref (Map.fromList mapping)
  where
    mapping = [ (instMod iface, html) | ((html, _), ifaces) <- packages
              , iface <- ifInstalledIfaces ifaces ]


getPrologue :: [Flag] -> IO (Maybe (Doc RdrName))
getPrologue flags =
  case [filename | Flag_Prologue filename <- flags ] of
    [] -> return Nothing
    [filename] -> do
      str <- readFile filename
#if ! MIN_VERSION_ghc(6,13,0)
      let f = id
#else
      let f = flattenExtensionFlags
#endif
      case parseParas (tokenise (f defaultDynFlags) str
                      (1,0) {- TODO: real position -}) of
        Nothing -> throwE $ "failed to parse haddock prologue from file: " ++ filename
        Just doc -> return (Just doc)
    _otherwise -> throwE "multiple -p/--prologue options"


#ifdef IN_GHC_TREE

getInTreeLibDir :: IO String
getInTreeLibDir =
      do m <- getExecDir
         case m of
             Nothing -> error "No GhcLibDir found"
#ifdef NEW_GHC_LAYOUT
             Just d -> return (d </> ".." </> "lib")
#else
             Just d -> return (d </> "..")
#endif


getExecDir :: IO (Maybe String)
#if defined(mingw32_HOST_OS)
getExecDir = allocaArray len $ \buf -> do
    ret <- getModuleFileName nullPtr buf len
    if ret == 0
        then return Nothing
        else do s <- peekCString buf
                return (Just (dropFileName s))
  where len = 2048 -- Plenty, PATH_MAX is 512 under Win32.


foreign import stdcall unsafe "GetModuleFileNameA"
  getModuleFileName :: Ptr () -> CString -> Int -> IO Int32
#else
getExecDir = return Nothing
#endif

#endif

