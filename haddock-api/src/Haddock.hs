{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings, Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2010,
--                    Mateusz Kowalczyk 2014
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
module Haddock (
  haddock,
  haddockWithGhc,
  getGhcDirs,
  readPackagesAndProcessModules,
  withGhc
) where

import Haddock.Backends.Xhtml
import Haddock.Backends.Xhtml.Meta
import Haddock.Backends.Xhtml.Themes (getThemes)
import Haddock.Backends.LaTeX
import Haddock.Backends.Hoogle
import Haddock.Backends.Hyperlinker
import Haddock.Interface
import Haddock.Interface.Json
import Haddock.Parser
import Haddock.Types
import Haddock.Version
import Haddock.InterfaceFile
import Haddock.Options
import Haddock.Utils

import Control.Monad hiding (forM_)
import Data.Foldable (forM_, foldl')
import Data.Traversable (for)
import Data.List (isPrefixOf)
import Control.Exception
import Data.Maybe
import Data.IORef
import Data.Map (Map)
import Data.Version (makeVersion)
import qualified Data.Map as Map
import System.IO
import System.Exit

#if defined(mingw32_HOST_OS)
import Foreign
import Foreign.C
import Data.Int
#endif

#ifdef IN_GHC_TREE
import System.FilePath
#else
import qualified GHC.Paths as GhcPaths
import Paths_haddock_api (getDataDir)
import System.Directory (doesDirectoryExist)
#endif

import Text.ParserCombinators.ReadP (readP_to_S)
import GHC hiding (verbosity)
import Config
import DynFlags hiding (projectVersion, verbosity)
import ErrUtils
import Packages
import Panic (handleGhcException)
import Module
import FastString
import qualified DynamicLoading

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
haddock args = haddockWithGhc withGhc args

haddockWithGhc :: (forall a. [Flag] -> Ghc a -> IO a) -> [String] -> IO ()
haddockWithGhc ghc args = handleTopExceptions $ do

  -- Parse command-line flags and handle some of them initially.
  -- TODO: unify all of this (and some of what's in the 'render' function),
  -- into one function that returns a record with a field for each option,
  -- or which exits with an error or help message.
  (flags, files) <- parseHaddockOpts args
  shortcutFlags flags
  qual <- rightOrThrowE (qualification flags)
  sinceQual <- rightOrThrowE (sinceQualification flags)

  -- inject dynamic-too into flags before we proceed
  flags' <- ghc flags $ do
        df <- getDynFlags
        case lookup "GHC Dynamic" (compilerInfo df) of
          Just "YES" -> return $ Flag_OptGhc "-dynamic-too" : flags
          _ -> return flags

  unless (Flag_NoWarnings `elem` flags) $ do
    hypSrcWarnings flags
    forM_ (warnings args) $ \warning -> do
      hPutStrLn stderr warning

  ghc flags' $ do
    dflags <- getDynFlags

    forM_ (optShowInterfaceFile flags) $ \path -> liftIO $ do
      mIfaceFile <- readInterfaceFiles freshNameCache [(("", Nothing), path)]
      forM_ mIfaceFile $ \(_, ifaceFile) -> do
        putMsg dflags (renderJson (jsonInterfaceFile ifaceFile))

    if not (null files) then do
      (packages, ifaces, homeLinks) <- readPackagesAndProcessModules flags files

      -- Dump an "interface file" (.haddock file), if requested.
      forM_ (optDumpInterfaceFile flags) $ \path -> liftIO $ do
        writeInterfaceFile path InterfaceFile {
            ifInstalledIfaces = map toInstalledIface ifaces
          , ifLinkEnv         = homeLinks
          }

      -- Render the interfaces.
      liftIO $ renderStep dflags flags sinceQual qual packages ifaces

    else do
      when (any (`elem` [Flag_Html, Flag_Hoogle, Flag_LaTeX]) flags) $
        throwE "No input file(s)."

      -- Get packages supplied with --read-interface.
      packages <- liftIO $ readInterfaceFiles freshNameCache (readIfaceArgs flags)

      -- Render even though there are no input files (usually contents/index).
      liftIO $ renderStep dflags flags sinceQual qual packages []

-- | Create warnings about potential misuse of -optghc
warnings :: [String] -> [String]
warnings = map format . filter (isPrefixOf "-optghc")
  where
    format arg = concat ["Warning: `", arg, "' means `-o ", drop 2 arg, "', did you mean `-", arg, "'?"]


withGhc :: [Flag] -> Ghc a -> IO a
withGhc flags action = do
  libDir <- fmap snd (getGhcDirs flags)

  -- Catches all GHC source errors, then prints and re-throws them.
  let handleSrcErrors action' = flip handleSourceError action' $ \err -> do
        printException err
        liftIO exitFailure

  withGhc' libDir (ghcFlags flags) (\_ -> handleSrcErrors action)


readPackagesAndProcessModules :: [Flag] -> [String]
                              -> Ghc ([(DocPaths, InterfaceFile)], [Interface], LinkEnv)
readPackagesAndProcessModules flags files = do
    -- Get packages supplied with --read-interface.
    packages <- readInterfaceFiles nameCacheFromGhc (readIfaceArgs flags)

    -- Create the interfaces -- this is the core part of Haddock.
    let ifaceFiles = map snd packages
    (ifaces, homeLinks) <- processModules (verbosity flags) files flags ifaceFiles

    return (packages, ifaces, homeLinks)


renderStep :: DynFlags -> [Flag] -> SinceQual -> QualOption
           -> [(DocPaths, InterfaceFile)] -> [Interface] -> IO ()
renderStep dflags flags sinceQual nameQual pkgs interfaces = do
  updateHTMLXRefs pkgs
  let
    ifaceFiles = map snd pkgs
    installedIfaces = concatMap ifInstalledIfaces ifaceFiles
    extSrcMap = Map.fromList $ do
      ((_, Just path), ifile) <- pkgs
      iface <- ifInstalledIfaces ifile
      return (instMod iface, path)
  render dflags flags sinceQual nameQual interfaces installedIfaces extSrcMap

-- | Render the interfaces with whatever backend is specified in the flags.
render :: DynFlags -> [Flag] -> SinceQual -> QualOption -> [Interface]
       -> [InstalledInterface] -> Map Module FilePath -> IO ()
render dflags flags sinceQual qual ifaces installedIfaces extSrcMap = do

  let
    title                = fromMaybe "" (optTitle flags)
    unicode              = Flag_UseUnicode `elem` flags
    pretty               = Flag_PrettyHtml `elem` flags
    opt_wiki_urls        = wikiUrls          flags
    opt_contents_url     = optContentsUrl    flags
    opt_index_url        = optIndexUrl       flags
    odir                 = outputDir         flags
    opt_latex_style      = optLaTeXStyle     flags
    opt_source_css       = optSourceCssFile  flags
    opt_mathjax          = optMathjax        flags
    dflags'
      | unicode          = gopt_set dflags Opt_PrintUnicodeSyntax
      | otherwise        = dflags

    visibleIfaces    = [ i | i <- ifaces, OptHide `notElem` ifaceOptions i ]

    -- /All/ visible interfaces including external package modules.
    allIfaces        = map toInstalledIface ifaces ++ installedIfaces
    allVisibleIfaces = [ i | i <- allIfaces, OptHide `notElem` instOptions i ]

    pkgMod           = ifaceMod (head ifaces)
    pkgKey           = moduleUnitId pkgMod
    pkgStr           = Just (unitIdString pkgKey)
    pkgNameVer       = modulePackageInfo dflags flags pkgMod
    pkgName          = fmap (unpackFS . (\(PackageName n) -> n)) (fst pkgNameVer)
    sincePkg         = case sinceQual of
                         External -> pkgName
                         Always -> Nothing

    (srcBase, srcModule, srcEntity, srcLEntity) = sourceUrls flags

    srcModule'
      | Flag_HyperlinkedSource `elem` flags = Just hypSrcModuleUrlFormat
      | otherwise = srcModule

    srcMap = Map.union
      (Map.map SrcExternal extSrcMap)
      (Map.fromList [ (ifaceMod iface, SrcLocal) | iface <- ifaces ])

    pkgSrcMap = Map.mapKeys moduleUnitId extSrcMap
    pkgSrcMap'
      | Flag_HyperlinkedSource `elem` flags =
          Map.insert pkgKey hypSrcModuleNameUrlFormat pkgSrcMap
      | Just srcNameUrl <- srcEntity = Map.insert pkgKey srcNameUrl pkgSrcMap
      | otherwise = pkgSrcMap

    -- TODO: Get these from the interface files as with srcMap
    pkgSrcLMap'
      | Flag_HyperlinkedSource `elem` flags =
          Map.singleton pkgKey hypSrcModuleLineUrlFormat
      | Just path <- srcLEntity = Map.singleton pkgKey path
      | otherwise = Map.empty

    sourceUrls' = (srcBase, srcModule', pkgSrcMap', pkgSrcLMap')

    installedMap :: Map Module InstalledInterface
    installedMap = Map.fromList [ (unwire (instMod iface), iface) | iface <- installedIfaces ]

    -- The user gives use base-4.9.0.0, but the InstalledInterface
    -- records the *wired in* identity base.  So untranslate it
    -- so that we can service the request.
    unwire :: Module -> Module
    unwire m = m { moduleUnitId = unwireUnitId dflags (moduleUnitId m) }

  reexportedIfaces <- concat `fmap` (for (reexportFlags flags) $ \mod_str -> do
    let warn = hPutStrLn stderr . ("Warning: " ++)
    case readP_to_S parseModuleId mod_str of
      [(m, "")]
        | Just iface <- Map.lookup m installedMap
        -> return [iface]
        | otherwise
        -> warn ("Cannot find reexported module '" ++ mod_str ++ "'") >> return []
      _ -> warn ("Cannot parse reexported module flag '" ++ mod_str ++ "'") >> return [])

  libDir   <- getHaddockLibDir flags
  prologue <- getPrologue dflags' flags
  themes   <- getThemes libDir flags >>= either bye return

  let withQuickjump = Flag_QuickJumpIndex `elem` flags

  when (Flag_GenIndex `elem` flags) $ do
    withTiming (pure dflags') "ppHtmlIndex" (const ()) $ do
      _ <- {-# SCC ppHtmlIndex #-}
           ppHtmlIndex odir title pkgStr
                  themes opt_mathjax opt_contents_url sourceUrls' opt_wiki_urls
                  allVisibleIfaces pretty
      return ()

    copyHtmlBits odir libDir themes withQuickjump

  when (Flag_GenContents `elem` flags) $ do
    withTiming (pure dflags') "ppHtmlContents" (const ()) $ do
      _ <- {-# SCC ppHtmlContents #-}
           ppHtmlContents dflags' odir title pkgStr
                     themes opt_mathjax opt_index_url sourceUrls' opt_wiki_urls
                     allVisibleIfaces True prologue pretty
                     sincePkg (makeContentsQual qual)
      return ()
    copyHtmlBits odir libDir themes withQuickjump

  when (Flag_Html `elem` flags) $ do
    withTiming (pure dflags') "ppHtml" (const ()) $ do
      _ <- {-# SCC ppHtml #-}
           ppHtml dflags' title pkgStr visibleIfaces reexportedIfaces odir
                  prologue
                  themes opt_mathjax sourceUrls' opt_wiki_urls
                  opt_contents_url opt_index_url unicode sincePkg qual
                  pretty withQuickjump
      return ()
    copyHtmlBits odir libDir themes withQuickjump
    writeHaddockMeta odir withQuickjump

  -- TODO: we throw away Meta for both Hoogle and LaTeX right now,
  -- might want to fix that if/when these two get some work on them
  when (Flag_Hoogle `elem` flags) $ do
    case pkgNameVer of
      (Just (PackageName pkgNameFS), mpkgVer) ->
          let
            pkgNameStr | unpackFS pkgNameFS == "main" && title /= [] = title
                       | otherwise = unpackFS pkgNameFS

            pkgVer =
              fromMaybe (makeVersion []) mpkgVer
          in ppHoogle dflags' pkgNameStr pkgVer title (fmap _doc prologue)
               visibleIfaces odir
      _ -> putStrLn . unlines $
          [ "haddock: Unable to find a package providing module "
            ++ moduleNameString (moduleName pkgMod) ++ ", skipping Hoogle."
          , ""
          , "         Perhaps try specifying the desired package explicitly"
            ++ " using the --package-name"
          , "         and --package-version arguments."
          ]

  when (Flag_LaTeX `elem` flags) $ do
    withTiming (pure dflags') "ppLatex" (const ()) $ do
      _ <- {-# SCC ppLatex #-}
           ppLaTeX title pkgStr visibleIfaces odir (fmap _doc prologue) opt_latex_style
                   libDir
      return ()

  when (Flag_HyperlinkedSource `elem` flags && not (null ifaces)) $ do
    withTiming (pure dflags') "ppHyperlinkedSource" (const ()) $ do
      _ <- {-# SCC ppHyperlinkedSource #-}
           ppHyperlinkedSource odir libDir opt_source_css pretty srcMap ifaces
      return ()


-------------------------------------------------------------------------------
-- * Reading and dumping interface files
-------------------------------------------------------------------------------


readInterfaceFiles :: MonadIO m
                   => NameCacheAccessor m
                   -> [(DocPaths, FilePath)]
                   -> m [(DocPaths, InterfaceFile)]
readInterfaceFiles name_cache_accessor pairs = do
  catMaybes `liftM` mapM ({-# SCC readInterfaceFile #-} tryReadIface) pairs
  where
    -- try to read an interface, warn if we can't
    tryReadIface (paths, file) =
      readInterfaceFile name_cache_accessor file >>= \case
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
withGhc' :: String -> [String] -> (DynFlags -> Ghc a) -> IO a
withGhc' libDir flags ghcActs = runGhc (Just libDir) $ do
  dynflags  <- getSessionDynFlags
  dynflags' <- parseGhcFlags (gopt_set dynflags Opt_Haddock) {
    hscTarget = HscNothing,
    ghcMode   = CompManager,
    ghcLink   = NoLink
    }
  -- We disable pattern match warnings because than can be very
  -- expensive to check
  let dynflags'' = unsetPatternMatchWarnings $
        updOptLevel 0 $
        gopt_unset dynflags' Opt_SplitObjs
  -- ignore the following return-value, which is a list of packages
  -- that may need to be re-linked: Haddock doesn't do any
  -- dynamic or static linking at all!
  _ <- setSessionDynFlags dynflags''
  hscenv <- GHC.getSession
  dynflags''' <- liftIO (DynamicLoading.initializePlugins hscenv dynflags'')
  _ <- setSessionDynFlags dynflags'''
  ghcActs dynflags'''
  where

    -- ignore sublists of flags that start with "+RTS" and end in "-RTS"
    --
    -- See https://github.com/haskell/haddock/issues/666
    filterRtsFlags :: [String] -> [String]
    filterRtsFlags flgs = foldr go (const []) flgs True
      where go "-RTS" func _ = func True
            go "+RTS" func _ = func False
            go _      func False = func False
            go arg    func True = arg : func True


    parseGhcFlags :: MonadIO m => DynFlags -> m DynFlags
    parseGhcFlags dynflags = do
      -- TODO: handle warnings?

      let flags' = filterRtsFlags flags
      (dynflags', rest, _) <- parseDynamicFlags dynflags (map noLoc flags')
      if not (null rest)
        then throwE ("Couldn't parse GHC options: " ++ unwords flags')
        else return dynflags'

unsetPatternMatchWarnings :: DynFlags -> DynFlags
unsetPatternMatchWarnings dflags =
  foldl' wopt_unset dflags pattern_match_warnings
  where
    pattern_match_warnings =
      [ Opt_WarnIncompletePatterns
      , Opt_WarnIncompleteUniPatterns
      , Opt_WarnIncompletePatternsRecUpd
      , Opt_WarnOverlappingPatterns
      ]

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
      -- if data directory does not exist we are probably
      -- invoking from either ./haddock-api or ./
      let res_dirs = [ getDataDir -- provided by Cabal
                     , pure "resources"
                     , pure "haddock-api/resources"
                     ]

          check get_path = do
            p <- get_path
            exists <- doesDirectoryExist p
            pure $ if exists then Just p else Nothing

      dirs <- mapM check res_dirs
      case [p | Just p <- dirs] of
        (p : _) -> return p
        _       -> die "Haddock's resource directory does not exist!\n"
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
    throwE "-h/--html cannot be used with --gen-index or --gen-contents"

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


-- | Generate some warnings about potential misuse of @--hyperlinked-source@.
hypSrcWarnings :: [Flag] -> IO ()
hypSrcWarnings flags = do

    when (hypSrc && any isSourceUrlFlag flags) $
        hPutStrLn stderr $ concat
            [ "Warning: "
            , "--source-* options are ignored when "
            , "--hyperlinked-source is enabled."
            ]

    when (not hypSrc && any isSourceCssFlag flags) $
        hPutStrLn stderr $ concat
            [ "Warning: "
            , "source CSS file is specified but "
            , "--hyperlinked-source is disabled."
            ]

  where
    hypSrc = Flag_HyperlinkedSource `elem` flags
    isSourceUrlFlag (Flag_SourceBaseURL _) = True
    isSourceUrlFlag (Flag_SourceModuleURL _) = True
    isSourceUrlFlag (Flag_SourceEntityURL _) = True
    isSourceUrlFlag (Flag_SourceLEntityURL _) = True
    isSourceUrlFlag _ = False
    isSourceCssFlag (Flag_SourceCss _) = True
    isSourceCssFlag _ = False


updateHTMLXRefs :: [(DocPaths, InterfaceFile)] -> IO ()
updateHTMLXRefs packages = do
  writeIORef html_xrefs_ref (Map.fromList mapping)
  writeIORef html_xrefs_ref' (Map.fromList mapping')
  where
    mapping = [ (instMod iface, html) | ((html, _), ifaces) <- packages
              , iface <- ifInstalledIfaces ifaces ]
    mapping' = [ (moduleName m, html) | (m, html) <- mapping ]


getPrologue :: DynFlags -> [Flag] -> IO (Maybe (MDoc RdrName))
getPrologue dflags flags =
  case [filename | Flag_Prologue filename <- flags ] of
    [] -> return Nothing
    [filename] -> do
      h <- openFile filename ReadMode
      hSetEncoding h utf8
      str <- hGetContents h -- semi-closes the handle
      return . Just $! parseParas dflags Nothing str
    _ -> throwE "multiple -p/--prologue options"


rightOrThrowE :: Either String b -> IO b
rightOrThrowE (Left msg) = throwE msg
rightOrThrowE (Right x) = pure x


#ifdef IN_GHC_TREE

getInTreeDir :: IO String
getInTreeDir = getExecDir >>= \case
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

# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32
#else
getExecDir = return Nothing
#endif

#endif
