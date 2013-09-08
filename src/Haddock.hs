{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE CPP, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock
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
module Haddock (haddock, readPackagesAndProcessModules, withGhc') where


import Haddock.Backends.Xhtml
import Haddock.Backends.Xhtml.Themes (getThemes)
import Haddock.Backends.LaTeX
import Haddock.Backends.Hoogle
import Haddock.Interface
import Haddock.Parser
import Haddock.Types
import Haddock.Version
import Haddock.InterfaceFile
import Haddock.Options
import Haddock.Utils
import Haddock.GhcUtils hiding (pretty)

import Control.Monad hiding (forM_)
import Data.Foldable (forM_)
import Data.List (isPrefixOf)
import Control.Exception
import Data.Maybe
import Data.IORef
import qualified Data.Map as Map
import System.IO
import System.Exit
import System.Directory

#if defined(mingw32_HOST_OS)
import Foreign
import Foreign.C
import Data.Int
#endif

#ifdef IN_GHC_TREE
import System.FilePath
#else
import qualified GHC.Paths as GhcPaths
import Paths_haddock
#endif

import GHC hiding (verbosity)
import Config
import DynFlags hiding (verbosity)
import StaticFlags (discardStaticFlags)
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

  ,  Handler (\(ex :: AsyncException) ->
       case ex of
         StackOverflow -> do
           putStrLn "stack overflow: use -g +RTS -K<size> to increase it"
           exitFailure
         _ -> do
           putStrLn ("haddock: " ++ show ex)
           exitFailure)

  ,  Handler (\(ex :: SomeException) -> do
        putStrLn ("haddock: internal error: " ++ show ex)
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
      _ -> do
        print (e :: GhcException)
        exitFailure


-------------------------------------------------------------------------------
-- * Top level
-------------------------------------------------------------------------------


-- | Run Haddock with given list of arguments.
--
-- Haddock's own main function is defined in terms of this:
--
-- > main = getArgs >>= haddock
haddock :: [String] -> IO ()
haddock args = handleTopExceptions $ do

  -- Parse command-line flags and handle some of them initially.
  -- TODO: unify all of this (and some of what's in the 'render' function),
  -- into one function that returns a record with a field for each option,
  -- or which exits with an error or help message.
  (flags, files) <- parseHaddockOpts args
  shortcutFlags flags
  qual <- case qualification flags of {Left msg -> throwE msg; Right q -> return q}

  -- inject dynamic-too into flags before we proceed
  flags' <- withGhc' flags $ do
        df <- getDynFlags
        case lookup "GHC Dynamic" (compilerInfo df) of
          Just "YES" -> return $ Flag_OptGhc "-dynamic-too" : flags
          _ -> return flags

  unless (Flag_NoWarnings `elem` flags) $ do
    forM_ (warnings args) $ \warning -> do
      hPutStrLn stderr warning

  withGhc' flags' $ do

    dflags <- getDynFlags

    if not (null files) then do
      (packages, ifaces, homeLinks) <- readPackagesAndProcessModules flags files

      -- Dump an "interface file" (.haddock file), if requested.
      forM_ (optDumpInterfaceFile flags) $ \path -> liftIO $ do
        writeInterfaceFile path InterfaceFile {
            ifInstalledIfaces = map toInstalledIface ifaces
          , ifLinkEnv         = homeLinks
          }

      -- Render the interfaces.
      liftIO $ renderStep dflags flags qual packages ifaces

    else do
      when (any (`elem` [Flag_Html, Flag_Hoogle, Flag_LaTeX]) flags) $
        throwE "No input file(s)."

      -- Get packages supplied with --read-interface.
      packages <- liftIO $ readInterfaceFiles freshNameCache (readIfaceArgs flags)

      -- Render even though there are no input files (usually contents/index).
      liftIO $ renderStep dflags flags qual packages []

-- | Create warnings about potential misuse of -optghc
warnings :: [String] -> [String]
warnings = map format . filter (isPrefixOf "-optghc")
  where
    format arg = concat ["Warning: `", arg, "' means `-o ", drop 2 arg, "', did you mean `-", arg, "'?"]


withGhc' :: [Flag] -> Ghc a -> IO a
withGhc' flags action = do
  libDir <- fmap snd (getGhcDirs flags)

  -- Catches all GHC source errors, then prints and re-throws them.
  let handleSrcErrors action' = flip handleSourceError action' $ \err -> do
        printException err
        liftIO exitFailure

  withGhc libDir (ghcFlags flags) (\_ -> handleSrcErrors action)


readPackagesAndProcessModules :: [Flag] -> [String]
                              -> Ghc ([(DocPaths, InterfaceFile)], [Interface], LinkEnv)
readPackagesAndProcessModules flags files = do
    -- Get packages supplied with --read-interface.
    packages <- readInterfaceFiles nameCacheFromGhc (readIfaceArgs flags)

    -- Create the interfaces -- this is the core part of Haddock.
    let ifaceFiles = map snd packages
    (ifaces, homeLinks) <- processModules (verbosity flags) files flags ifaceFiles

    return (packages, ifaces, homeLinks)


renderStep :: DynFlags -> [Flag] -> QualOption -> [(DocPaths, InterfaceFile)] -> [Interface] -> IO ()
renderStep dflags flags qual pkgs interfaces = do
  updateHTMLXRefs pkgs
  let
    ifaceFiles = map snd pkgs
    installedIfaces = concatMap ifInstalledIfaces ifaceFiles
    srcMap = Map.fromList [ (ifPackageId if_, x) | ((_, Just x), if_) <- pkgs ]
  render dflags flags qual interfaces installedIfaces srcMap


-- | Render the interfaces with whatever backend is specified in the flags.
render :: DynFlags -> [Flag] -> QualOption -> [Interface] -> [InstalledInterface] -> SrcMap -> IO ()
render dflags flags qual ifaces installedIfaces srcMap = do

  let
    title                = fromMaybe "" (optTitle flags)
    unicode              = Flag_UseUnicode `elem` flags
    pretty               = Flag_PrettyHtml `elem` flags
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
  prologue <- getPrologue dflags flags
  themes   <- getThemes libDir flags >>= either bye return

  when (Flag_GenIndex `elem` flags) $ do
    ppHtmlIndex odir title pkgStr
                themes opt_contents_url sourceUrls' opt_wiki_urls
                allVisibleIfaces pretty
    copyHtmlBits odir libDir themes

  when (Flag_GenContents `elem` flags) $ do
    ppHtmlContents odir title pkgStr
                   themes opt_index_url sourceUrls' opt_wiki_urls
                   allVisibleIfaces True prologue pretty
                   (makeContentsQual qual)
    copyHtmlBits odir libDir themes

  when (Flag_Html `elem` flags) $ do
    ppHtml title pkgStr visibleIfaces odir
                prologue
                themes sourceUrls' opt_wiki_urls
                opt_contents_url opt_index_url unicode qual
                pretty
    copyHtmlBits odir libDir themes

  when (Flag_Hoogle `elem` flags) $ do
    let pkgName2 = if pkgName == "main" && title /= [] then title else pkgName
    ppHoogle dflags pkgName2 pkgVer title prologue visibleIfaces odir

  when (Flag_LaTeX `elem` flags) $ do
    ppLaTeX title pkgStr visibleIfaces odir prologue opt_latex_style
                  libDir


-------------------------------------------------------------------------------
-- * Reading and dumping interface files
-------------------------------------------------------------------------------


readInterfaceFiles :: MonadIO m
                   => NameCacheAccessor m
                   -> [(DocPaths, FilePath)]
                   -> m [(DocPaths, InterfaceFile)]
readInterfaceFiles name_cache_accessor pairs = do
  catMaybes `liftM` mapM tryReadIface pairs
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


-------------------------------------------------------------------------------
-- * Creating a GHC session
-------------------------------------------------------------------------------


-- | Start a GHC session with the -haddock flag set. Also turn off
-- compilation and linking. Then run the given 'Ghc' action.
withGhc :: String -> [String] -> (DynFlags -> Ghc a) -> IO a
withGhc libDir flags ghcActs = do
  runGhc (Just libDir) $ do
    dynflags  <- getSessionDynFlags
    let dynflags' = gopt_set dynflags Opt_Haddock
    let dynflags'' = dynflags' {
        hscTarget = HscNothing,
        ghcMode   = CompManager,
        ghcLink   = NoLink
      }
    dynflags''' <- parseGhcFlags dynflags''
    defaultCleanupHandler dynflags''' $ do
        -- ignore the following return-value, which is a list of packages
        -- that may need to be re-linked: Haddock doesn't do any
        -- dynamic or static linking at all!
        _ <- setSessionDynFlags dynflags'''
        ghcActs dynflags'''
  where
    parseGhcFlags :: MonadIO m => DynFlags -> m DynFlags
    parseGhcFlags dynflags = do
      -- TODO: handle warnings?

      -- NOTA BENE: We _MUST_ discard any static flags here, because we cannot
      -- rely on Haddock to parse them, as it only parses the DynFlags. Yet if
      -- we pass any, Haddock will fail. Since StaticFlags are global to the
      -- GHC invocation, there's also no way to reparse/save them to set them
      -- again properly.
      --
      -- This is a bit of a hack until we get rid of the rest of the remaining
      -- StaticFlags. See GHC issue #8276.
      let flags' = discardStaticFlags flags
      (dynflags', rest, _) <- parseDynamicFlags dynflags (map noLoc flags')
      if not (null rest)
        then throwE ("Couldn't parse GHC options: " ++ unwords flags')
        else return dynflags'

-------------------------------------------------------------------------------
-- * Misc
-------------------------------------------------------------------------------


getHaddockLibDir :: [Flag] -> IO String
getHaddockLibDir flags =
  case [str | Flag_Lib str <- flags] of
    [] -> do
#ifdef IN_GHC_TREE
      getInTreeDir
#else
      d <- getDataDir -- provided by Cabal
      doesDirectoryExist d >>= \exists -> case exists of
        True -> return d
        False -> do
          -- If directory does not exist then we are probably invoking from
          -- ./dist/build/haddock/haddock so we use ./resources as a fallback.
          doesDirectoryExist "resources" >>= \exists_ -> case exists_ of
            True -> return "resources"
            False -> die ("Haddock's resource directory (" ++ d ++ ") does not exist!\n")
#endif
    fs -> return (last fs)


getGhcDirs :: [Flag] -> IO (String, String)
getGhcDirs flags = do
  case [ dir | Flag_GhcLibDir dir <- flags ] of
    [] -> do
#ifdef IN_GHC_TREE
      libDir <- getInTreeDir
      return (ghcPath, libDir)
#else
      return (ghcPath, GhcPaths.libdir)
#endif
    xs -> return (ghcPath, last xs)
  where
#ifdef IN_GHC_TREE
    ghcPath = "not available"
#else
    ghcPath = GhcPaths.ghc
#endif


shortcutFlags :: [Flag] -> IO ()
shortcutFlags flags = do
  usage <- getUsage

  when (Flag_Help             `elem` flags) (bye usage)
  when (Flag_Version          `elem` flags) byeVersion
  when (Flag_InterfaceVersion `elem` flags) (bye (show binaryInterfaceVersion ++ "\n"))
  when (Flag_CompatibleInterfaceVersions `elem` flags)
    (bye (unwords (map show binaryInterfaceVersionCompatibility) ++ "\n"))
  when (Flag_GhcVersion       `elem` flags) (bye (cProjectVersion ++ "\n"))

  when (Flag_PrintGhcPath `elem` flags) $ do
    dir <- fmap fst (getGhcDirs flags)
    bye $ dir ++ "\n"

  when (Flag_PrintGhcLibDir `elem` flags) $ do
    dir <- fmap snd (getGhcDirs flags)
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


updateHTMLXRefs :: [(DocPaths, InterfaceFile)] -> IO ()
updateHTMLXRefs packages = do
  writeIORef html_xrefs_ref (Map.fromList mapping)
  writeIORef html_xrefs_ref' (Map.fromList mapping')
  where
    mapping = [ (instMod iface, html) | ((html, _), ifaces) <- packages
              , iface <- ifInstalledIfaces ifaces ]
    mapping' = [ (moduleName m, html) | (m, html) <- mapping ]


getPrologue :: DynFlags -> [Flag] -> IO (Maybe (Doc RdrName))
getPrologue dflags flags =
  case [filename | Flag_Prologue filename <- flags ] of
    [] -> return Nothing
    [filename] -> do
      str <- readFile filename
      case parseParasMaybe dflags str of
        Nothing -> throwE $ "failed to parse haddock prologue from file: " ++ filename
        Just doc -> return (Just doc)
    _otherwise -> throwE "multiple -p/--prologue options"


#ifdef IN_GHC_TREE

getInTreeDir :: IO String
getInTreeDir = do
  m <- getExecDir
  case m of
    Nothing -> error "No GhcDir found"
    Just d -> return (d </> ".." </> "lib")


getExecDir :: IO (Maybe String)
#if defined(mingw32_HOST_OS)
getExecDir = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
  where
    try_size size = allocaArray (fromIntegral size) $ \buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
          0 -> return Nothing
          _ | ret < size -> fmap (Just . dropFileName) $ peekCWString buf
            | otherwise  -> try_size (size * 2)

foreign import stdcall unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32
#else
getExecDir = return Nothing
#endif

#endif
