{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.DeepSeq (force)
import Control.Monad hiding (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (second)
import Data.Foldable (forM_)
import Data.Traversable (for)
import qualified Data.List as List
import Control.Exception
import Data.Maybe
import Data.IORef
import Data.Map.Strict (Map)
import Data.Version (makeVersion)
import GHC.Parser.Lexer (ParserOpts)
import qualified GHC.Driver.Config.Parser as Parser
import qualified Data.Map.Strict as Map
import System.IO
import System.Exit
import System.FilePath
#ifdef IN_GHC_TREE
import System.Environment (getExecutablePath)
#else
import qualified GHC.Paths as GhcPaths
import Paths_haddock_api (getDataDir)
#endif
import System.Directory (doesDirectoryExist, getTemporaryDirectory)
import Text.ParserCombinators.ReadP (readP_to_S)
import GHC hiding (verbosity)
import GHC.Settings.Config
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Env
import GHC.Driver.Session hiding (projectVersion, verbosity)
import qualified GHC.Driver.Session as DynFlags (DynFlags(..))
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Types.Name.Cache
import GHC.Unit
import GHC.Utils.Panic (handleGhcException)
import GHC.Data.FastString

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
import Haddock.GhcUtils (modifySessionDynFlags, setOutputDir, getSupportedLanguagesAndExtensions)

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

  -- If argument tracing is enabled, print the arguments we were given
  when (Flag_TraceArgs `elem` flags) $ do
    putStrLn $ "haddock received arguments:"
    mapM_ (putStrLn . ("  " ++)) args

  qual <- rightOrThrowE (qualification flags)
  sinceQual <- rightOrThrowE (sinceQualification flags)

  let isOneShotMode = isJust (optOneShot flags)

  -- Inject dynamic-too into ghc options if the ghc we are using was built with
  -- dynamic linking (except when in one-shot mode)
  flags'' <- ghc flags $ do
        df <- getDynFlags
        case lookup "GHC Dynamic" (compilerInfo df) of
          Just "YES" | not isOneShotMode -> return $ Flag_OptGhc "-dynamic-too" : flags
          _ -> return flags

  -- Inject `-j` into ghc options, if given to Haddock
  flags' <- pure $ case optParCount flags'' of
    Nothing       -> flags''
    Just Nothing  -> Flag_OptGhc "-j" : flags''
    Just (Just n) -> Flag_OptGhc ("-j" ++ show n) : flags''

  -- Whether or not to bypass the interface version check
  let noChecks = Flag_BypassInterfaceVersonCheck `elem` flags

  -- Create a temporary directory and redirect GHC output there (unless user
  -- requested otherwise).
  --
  -- Output dir needs to be set before calling 'depanal' since 'depanal' uses it
  -- to compute output file names that are stored in the 'DynFlags' of the
  -- resulting 'ModSummary's.
  let withDir | Flag_NoTmpCompDir `elem` flags = id
              | isOneShotMode = id
              | otherwise = withTempOutputDir

  -- Output warnings about potential misuse of some flags
  unless (Flag_NoWarnings `elem` flags) $ do
    hypSrcWarnings flags
    mapM_ (hPutStrLn stderr) (optGhcWarnings args)
    when noChecks $
      hPutStrLn stderr noCheckWarning

  ghc flags' $ withDir $ do
    dflags' <- getDynFlags
    let unicode = Flag_UseUnicode `elem` flags
    let dflags
          | unicode = gopt_set dflags' Opt_PrintUnicodeSyntax
          | otherwise = dflags'
    logger' <- getLogger
    let logger = setLogFlags logger' (initLogFlags dflags)
    let parserOpts = Parser.initParserOpts dflags
    !unit_state <- hsc_units <$> getSession

    -- If any --show-interface was used, show the given interfaces
    forM_ (optShowInterfaceFile flags) $ \path -> liftIO $ do
      name_cache <- freshNameCache
      mIfaceFile <- readInterfaceFiles name_cache [(DocPaths "" Nothing, Visible, path)] noChecks
      forM_ mIfaceFile $ \(_,_,_, ifaceFile) -> do
        putMsg logger $ renderJson (jsonInterfaceFile ifaceFile)

    -- If we were given source files to generate documentation from, do it
    if not (null files) || isJust (optOneShot flags) then do
      (packages, ifaces, homeLinks) <- readPackagesAndProcessModules flags files
      let packageInfo = PackageInfo { piPackageName =
                                        fromMaybe (PackageName mempty) (optPackageName flags)
                                    , piPackageVersion =
                                        fromMaybe (makeVersion []) (optPackageVersion flags)
                                    }
          languagesAndExtensions = getSupportedLanguagesAndExtensions ifaces

      -- Dump an "interface file" (.haddock file), if requested.
      forM_ (optDumpInterfaceFile flags) $ \path -> liftIO $ do
        writeInterfaceFile path InterfaceFile {
            ifInstalledIfaces = map toInstalledIface ifaces
          , ifPackageInfo     = packageInfo
          , ifLinkEnv         = homeLinks
          }

      -- Render the interfaces.
      liftIO $ renderStep dflags languagesAndExtensions parserOpts logger unit_state flags sinceQual qual packages ifaces

    -- If we were not given any input files, error if documentation was
    -- requested
    else do
      when (any (`elem` [Flag_Html, Flag_Hoogle, Flag_LaTeX]) flags) $
        throwE "No input file(s)."

      -- Get packages supplied with --read-interface.
      name_cache <- liftIO $ freshNameCache
      packages <- liftIO $ readInterfaceFiles name_cache (readIfaceArgs flags) noChecks

      -- Render even though there are no input files (usually contents/index).
      liftIO $ renderStep dflags [] parserOpts logger unit_state flags sinceQual qual packages []

-- | Run the GHC action using a temporary output directory
withTempOutputDir :: Ghc a -> Ghc a
withTempOutputDir action = do
  tmp <- liftIO getTemporaryDirectory
  x   <- liftIO getProcessID
  let dir = tmp </> ".haddock-" ++ show x
  modifySessionDynFlags (setOutputDir dir)
  withTempDir dir action

-- | Create warnings about potential misuse of -optghc
optGhcWarnings :: [String] -> [String]
optGhcWarnings = map format . filter (List.isPrefixOf "-optghc")
  where
    format arg = concat ["Warning: `", arg, "' means `-o ", drop 2 arg, "', did you mean `-", arg, "'?"]

-- | Create a warning about bypassing the interface version check
noCheckWarning :: String
noCheckWarning = "Warning: `--bypass-interface-version-check' can cause " ++
                 "Haddock to crash when reading Haddock interface files."

withGhc :: [Flag] -> Ghc a -> IO a
withGhc flags action = do
  libDir <- fmap (fromMaybe (error "No GhcDir found") . snd) (getGhcDirs flags)

  -- Catches all GHC source errors, then prints and re-throws them.
  let handleSrcErrors action' = flip handleSourceError action' $ \err -> do
        printException err
        liftIO exitFailure
      needHieFiles = Flag_HyperlinkedSource `elem` flags

  withGhc' libDir needHieFiles (ghcFlags flags) (\_ -> handleSrcErrors action)


readPackagesAndProcessModules :: [Flag] -> [String]
                              -> Ghc ([(DocPaths, Visibility, FilePath, InterfaceFile)], [Interface], LinkEnv)
readPackagesAndProcessModules flags files = do
    -- Whether or not we bypass the interface file version check
    let noChecks = Flag_BypassInterfaceVersonCheck `elem` flags

    -- Read package dependency interface files supplied with --read-interface
    name_cache <- hsc_NC <$> getSession
    packages <- liftIO $ readInterfaceFiles name_cache (readIfaceArgs flags) noChecks

    -- Create the interfaces for the given modules -- this is the core part of Haddock
    let ifaceFiles = map (\(_, _, _, ifaceFile) -> ifaceFile) packages
    (ifaces, homeLinks) <- processModules (verbosity flags) files flags ifaceFiles

    return (packages, ifaces, homeLinks)


renderStep
  :: DynFlags
  -> [String]
  -- ^ Supported languages and extensions based on architecture and OS
  -> ParserOpts
  -> Logger
  -> UnitState
  -> [Flag]
  -> SinceQual
  -> QualOption
  -> [(DocPaths, Visibility, FilePath, InterfaceFile)]
  -> [Interface]
  -> IO ()
renderStep dflags languagesAndExtensions parserOpts logger unit_state flags sinceQual nameQual pkgs interfaces = do
  updateHTMLXRefs (map (\(docPath, _ifaceFilePath, _showModules, ifaceFile) ->
                          ( case baseUrl flags of
                              Nothing  -> docPathsHtml docPath
                              Just url -> url </> packageName (ifUnitId ifaceFile)
                          , ifaceFile)) pkgs)
  let
    installedIfaces =
      map
        (\(_, showModules, ifaceFilePath, ifaceFile)
          -> (ifaceFilePath, mkPackageInterfaces showModules ifaceFile))
        pkgs
    extSrcMap = Map.fromList $ do
      (DocPaths {docPathsSources=Just path}, _, _, ifile) <- pkgs
      iface <- ifInstalledIfaces ifile
      return (instMod iface, path)
  render dflags languagesAndExtensions parserOpts logger unit_state flags sinceQual nameQual interfaces installedIfaces extSrcMap
  where
    -- get package name from unit-id
    packageName :: Unit -> String
    packageName unit =
      case lookupUnit unit_state unit of
        Nothing  -> show unit
        Just pkg -> unitPackageNameString pkg

-- | Render the interfaces with whatever backend is specified in the flags.
render
  :: DynFlags
  -> [String]
  -- ^ Supported languages and extensions based on architecture and OS
  -> ParserOpts
  -> Logger
  -> UnitState
  -> [Flag]
  -> SinceQual
  -> QualOption
  -> [Interface]
  -> [(FilePath, PackageInterfaces)]
  -> Map Module FilePath
  -> IO ()
render dflags languagesAndExtensions parserOpts logger unit_state flags sinceQual qual ifaces packages extSrcMap = do
  let
    packageInfo = PackageInfo { piPackageName    = fromMaybe (PackageName mempty)
                                                 $ optPackageName flags
                              , piPackageVersion = fromMaybe (makeVersion [])
                                                 $ optPackageVersion flags
                              }

    title                = fromMaybe "" (optTitle flags)
    unicode              = Flag_UseUnicode `elem` flags
    pretty               = Flag_PrettyHtml `elem` flags
    opt_wiki_urls        = wikiUrls          flags
    opt_base_url         = baseUrl           flags
    opt_contents_url     = optContentsUrl    flags
    opt_index_url        = optIndexUrl       flags
    odir                 = outputDir         flags
    opt_latex_style      = optLaTeXStyle     flags
    opt_source_css       = optSourceCssFile  flags
    opt_mathjax          = optMathjax        flags

    visibleIfaces    = [ i | i <- ifaces, OptHide `notElem` ifaceOptions i ]

    -- /All/ interfaces including external package modules, grouped by
    -- interface file (package).
    allPackages      :: [PackageInterfaces]
    allPackages      = [PackageInterfaces
                         { piPackageInfo = packageInfo
                         , piVisibility  = Visible
                         , piInstalledInterfaces  = map toInstalledIface ifaces
                         }]
                    ++ map snd packages

    -- /All/ visible interfaces including external package modules, grouped by
    -- interface file (package).
    allVisiblePackages :: [PackageInterfaces]
    allVisiblePackages = [ pinfo { piInstalledInterfaces =
                                     filter (\i -> OptHide `notElem` instOptions i)
                                            piInstalledInterfaces
                                 }
                         | pinfo@PackageInterfaces
                             { piVisibility = Visible
                             , piInstalledInterfaces
                             } <- allPackages
                         ]

    -- /All/ installed interfaces.
    allInstalledIfaces :: [InstalledInterface]
    allInstalledIfaces = concatMap (piInstalledInterfaces . snd) packages

    pkgMod           = fmap ifaceMod (listToMaybe ifaces)
    pkgKey           = fmap moduleUnit pkgMod
    pkgStr           = fmap unitString pkgKey
    pkgNameVer       = modulePackageInfo unit_state flags pkgMod
    pkgName          = fmap (unpackFS . (\(PackageName n) -> n)) (fst pkgNameVer)
    sincePkg         = case sinceQual of
                         External -> pkgName
                         Always -> Nothing

    (srcBase, srcModule, srcEntity, srcLEntity) = sourceUrls flags

    srcModule'
      | Flag_HyperlinkedSource `elem` flags = Just hypSrcModuleUrlFormat
      | otherwise = srcModule

    -- These urls have a template for the module %M
    srcMap = Map.union
      (Map.map (SrcExternal . hypSrcPkgUrlToModuleFormat) extSrcMap)
      (Map.fromList [ (ifaceMod iface, SrcLocal) | iface <- ifaces ])

    -- These urls have a template for the module %M and the name %N
    pkgSrcMap = Map.map (hypSrcModuleUrlToNameFormat . hypSrcPkgUrlToModuleFormat)
              $ Map.mapKeys moduleUnit extSrcMap
    pkgSrcMap'
      | Flag_HyperlinkedSource `elem` flags
      , Just k <- pkgKey
      = Map.insert k hypSrcModuleNameUrlFormat pkgSrcMap
      | Just srcNameUrl <- srcEntity
      , Just k <- pkgKey
      = Map.insert k srcNameUrl pkgSrcMap
      | otherwise = pkgSrcMap

    pkgSrcLMap = Map.map (hypSrcModuleUrlToLineFormat . hypSrcPkgUrlToModuleFormat)
               $ Map.mapKeys moduleUnit extSrcMap
    -- These urls have a template for the module %M and the line %L
    pkgSrcLMap'
      | Flag_HyperlinkedSource `elem` flags
      , Just k <- pkgKey
      = Map.insert k hypSrcModuleLineUrlFormat pkgSrcLMap
      | Just path <- srcLEntity
      , Just k <- pkgKey
      = Map.insert k path pkgSrcLMap
      | otherwise = pkgSrcLMap

    sourceUrls' = (srcBase, srcModule', pkgSrcMap', pkgSrcLMap')

    installedMap :: Map Module InstalledInterface
    installedMap = Map.fromList [ (unwire (instMod iface), iface) | iface <- allInstalledIfaces ]

    -- The user gives use base-4.9.0.0, but the InstalledInterface
    -- records the *wired in* identity base.  So untranslate it
    -- so that we can service the request.
    unwire :: Module -> Module
    unwire m = m { moduleUnit = unwireUnit unit_state (moduleUnit m) }

  reexportedIfaces <- concat `fmap` (for (reexportFlags flags) $ \mod_str -> do
    let warn' = hPutStrLn stderr . ("Warning: " ++)
    case readP_to_S parseHoleyModule mod_str of
      [(m, "")]
        | Just iface <- Map.lookup m installedMap
        -> return [iface]
        | otherwise
        -> warn' ("Cannot find reexported module '" ++ mod_str ++ "'") >> return []
      _ -> warn' ("Cannot parse reexported module flag '" ++ mod_str ++ "'") >> return [])

  libDir   <- getHaddockLibDir flags
  !prologue <- force <$> getPrologue parserOpts flags
  themes   <- getThemes libDir flags >>= either bye return

  let withQuickjump = Flag_QuickJumpIndex `elem` flags
      withBaseURL = isJust
                  . List.find (\flag -> case flag of
                           Flag_BaseURL base_url ->
                             base_url /= "." && base_url /= "./"
                           _ -> False
                         )
                  $ flags

  when (Flag_GenIndex `elem` flags) $ do
    withTiming logger "ppHtmlIndex" (const ()) $ do
      _ <- {-# SCC ppHtmlIndex #-}
           ppHtmlIndex odir title pkgStr
                  themes opt_mathjax opt_contents_url sourceUrls' opt_wiki_urls
                  (concatMap piInstalledInterfaces allVisiblePackages) pretty
      return ()

    unless withBaseURL $
      copyHtmlBits odir libDir themes withQuickjump

  when (Flag_GenContents `elem` flags) $ do
    withTiming logger "ppHtmlContents" (const ()) $ do
      _ <- {-# SCC ppHtmlContents #-}
           ppHtmlContents unit_state odir title pkgStr
                     themes opt_mathjax opt_index_url sourceUrls' opt_wiki_urls
                     allVisiblePackages True prologue pretty
                     sincePkg (makeContentsQual qual)
      return ()
    copyHtmlBits odir libDir themes withQuickjump

  when withQuickjump $ void $
            ppJsonIndex odir sourceUrls' opt_wiki_urls
                        unicode Nothing qual
                        ifaces
                        ( List.nub
                        . map fst
                        . filter ((== Visible) . piVisibility . snd)
                        $ packages)

  when (Flag_Html `elem` flags) $ do
    withTiming logger "ppHtml" (const ()) $ do
      _ <- {-# SCC ppHtml #-}
           ppHtml unit_state title pkgStr visibleIfaces reexportedIfaces odir
                  prologue
                  themes opt_mathjax sourceUrls' opt_wiki_urls opt_base_url
                  opt_contents_url opt_index_url unicode sincePkg packageInfo
                  qual pretty withQuickjump
      return ()
    unless (withBaseURL || isJust (optOneShot flags)) $ do
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
          in ppHoogle dflags unit_state pkgNameStr pkgVer title (fmap _doc prologue)
               visibleIfaces odir
      _ -> putStrLn . unlines $
          [ "haddock: Unable to find a package providing module "
            ++ maybe "<no-mod>" (moduleNameString . moduleName) pkgMod
            ++ ", skipping Hoogle."
          , ""
          , "         Perhaps try specifying the desired package explicitly"
            ++ " using the --package-name"
          , "         and --package-version arguments."
          ]

  when (Flag_LaTeX `elem` flags) $ do
    withTiming logger "ppLatex" (const ()) $ do
      _ <- {-# SCC ppLatex #-}
           ppLaTeX title pkgStr visibleIfaces odir (fmap _doc prologue) opt_latex_style
                   libDir
      return ()

  when (Flag_HyperlinkedSource `elem` flags && not (null ifaces)) $ do
    withTiming logger "ppHyperlinkedSource" (const ()) $ do
      _ <- {-# SCC ppHyperlinkedSource #-}
           ppHyperlinkedSource (verbosity flags) (isJust (optOneShot flags)) languagesAndExtensions  odir libDir opt_source_css pretty srcMap ifaces
      return ()


-------------------------------------------------------------------------------
-- * Reading and dumping interface files
-------------------------------------------------------------------------------


readInterfaceFiles :: NameCache
                   -> [(DocPaths, Visibility, FilePath)]
                   -> Bool
                   -> IO [(DocPaths, Visibility, FilePath, InterfaceFile)]
readInterfaceFiles name_cache_accessor pairs bypass_version_check = do
  catMaybes `liftM` mapM ({-# SCC readInterfaceFile #-} tryReadIface) pairs
  where
    -- try to read an interface, warn if we can't
    tryReadIface (paths, vis, file) =
      readInterfaceFile name_cache_accessor file bypass_version_check >>= \case
        Left err -> do
          putStrLn ("Warning: Cannot read " ++ file ++ ":")
          putStrLn ("   " ++ err)
          putStrLn "Skipping this interface."
          return Nothing
        Right f -> return (Just (paths, vis, file, f))


-------------------------------------------------------------------------------
-- * Creating a GHC session
-------------------------------------------------------------------------------


-- | Start a GHC session with the -haddock flag set. Also turn off
-- compilation and linking. Then run the given 'Ghc' action.
withGhc' :: String -> Bool -> [String] -> (DynFlags -> Ghc a) -> IO a
withGhc' libDir needHieFiles flags ghcActs = runGhc (Just libDir) $ do
    logger <- getLogger

    -- Set default GHC verbosity to 1. This is better for hi-haddock since -v0
    -- creates an awkward silence during the load operation
    default_dflags <- getSessionDynFlags >>= \dflags ->
      pure dflags { DynFlags.verbosity = 1 }

    dynflags' <- parseGhcFlags logger default_dflags

    -- Disable pattern match warnings because they can be very expensive to
    -- check, set optimization level to 0 for fastest compilation.
    let dynflags'' = unsetPatternMatchWarnings $ updOptLevel 0 dynflags'

    -- ignore the following return-value, which is a list of packages
    -- that may need to be re-linked: Haddock doesn't do any
    -- dynamic or static linking at all!
    _ <- setSessionDynFlags dynflags''
    ghcActs dynflags''
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

    parseGhcFlags :: MonadIO m => Logger -> DynFlags -> m DynFlags
    parseGhcFlags logger dynflags = do
      -- TODO: handle warnings?

      let extra_opts =
            [ -- Include docstrings in .hi files.
              Opt_Haddock

              -- Do not recompile because of changes to optimization flags
            , Opt_IgnoreOptimChanges
            ]
              -- Write .hie files if we need them for hyperlinked src
              ++ if needHieFiles
                    then [Opt_WriteHie] -- Generate .hie-files
                    else []
          dynflags' = (List.foldl' gopt_set dynflags extra_opts)
                        { backend = noBackend
                        , ghcMode = CompManager
                        , ghcLink = NoLink
                        }
          flags' = filterRtsFlags flags

      (dynflags'', rest, _) <- parseDynamicFlags logger dynflags' (map noLoc flags')
      if not (null rest)
        then throwE ("Couldn't parse GHC options: " ++ unwords flags')
        else return dynflags''

unsetPatternMatchWarnings :: DynFlags -> DynFlags
unsetPatternMatchWarnings dflags =
  List.foldl' wopt_unset dflags pattern_match_warnings
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


getHaddockLibDir :: [Flag] -> IO FilePath
getHaddockLibDir flags =
  case [str | Flag_Lib str <- flags] of
    [] -> do
#ifdef IN_GHC_TREE

      -- When in the GHC tree, we should be able to locate the "lib" folder
      -- based on the location of the current executable.
      base_dir <- getBaseDir      -- Provided by GHC
      let res_dirs = [ d | Just d <- [base_dir] ] ++

#else

      -- When Haddock was installed by @cabal@, the resources (which are listed
      -- under @data-files@ in the Cabal file) will have been copied to a
      -- special directory.
      data_dir <- getDataDir      -- Provided by Cabal
      let res_dirs = [ data_dir ] ++

#endif

      -- When Haddock is built locally (eg. regular @cabal new-build@), the data
      -- directory does not exist and we are probably invoking from either
      -- @./haddock-api@ or @./@
                     [ "resources"
                     , "haddock-api/resources"
                     ]

      res_dir <- check res_dirs
      case res_dir of
        Just p -> return p
        _      -> die "Haddock's resource directory does not exist!\n"

    fs -> return (last fs)
  where
    -- Pick the first path that corresponds to a directory that exists
    check :: [FilePath] -> IO (Maybe FilePath)
    check [] = pure Nothing
    check (path : other_paths) = do
      exists <- doesDirectoryExist path
      if exists then pure (Just path) else check other_paths

-- | Find the @lib@ directory for GHC and the path to @ghc@
getGhcDirs :: [Flag] -> IO (Maybe FilePath, Maybe FilePath)
getGhcDirs flags = do

#ifdef IN_GHC_TREE
  base_dir <- getBaseDir
  let ghc_path = Nothing
#else
  let base_dir = Just GhcPaths.libdir
      ghc_path = Just GhcPaths.ghc
#endif

  -- If the user explicitly specifies a lib dir, use that
  let ghc_dir = case [ dir | Flag_GhcLibDir dir <- flags ] of
                  [] -> base_dir
                  xs -> Just (last xs)

  pure (ghc_path, ghc_dir)


#ifdef IN_GHC_TREE

-- | See 'getBaseDir' in "SysTools.BaseDir"
getBaseDir :: IO (Maybe FilePath)
getBaseDir = do

  -- Getting executable path can fail. Turn that into 'Nothing'
  exec_path_opt <- catch (Just <$> getExecutablePath)
                         (\(_ :: SomeException) -> pure Nothing)

  -- Check that the path we are about to return actually exists
  case exec_path_opt of
    Nothing -> pure Nothing
    Just exec_path -> do
      let base_dir = takeDirectory (takeDirectory exec_path) </> "lib"
      exists <- doesDirectoryExist base_dir
      pure (if exists then Just base_dir else Nothing)

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
    path <- fmap fst (getGhcDirs flags)
    bye $ fromMaybe "not available" path ++ "\n"

  when (Flag_PrintGhcLibDir `elem` flags) $ do
    dir <- fmap snd (getGhcDirs flags)
    bye $ fromMaybe "not available" dir ++ "\n"

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
    hypSrc :: Bool
    hypSrc = Flag_HyperlinkedSource `elem` flags

    isSourceUrlFlag :: Flag -> Bool
    isSourceUrlFlag (Flag_SourceBaseURL _) = True
    isSourceUrlFlag (Flag_SourceModuleURL _) = True
    isSourceUrlFlag (Flag_SourceEntityURL _) = True
    isSourceUrlFlag (Flag_SourceLEntityURL _) = True
    isSourceUrlFlag _ = False

    isSourceCssFlag :: Flag -> Bool
    isSourceCssFlag (Flag_SourceCss _) = True
    isSourceCssFlag _ = False


updateHTMLXRefs :: [(FilePath, InterfaceFile)] -> IO ()
updateHTMLXRefs packages = do
  let !modMap     = force $ Map.fromList mapping
      !modNameMap = force $ Map.fromList mapping'
  writeIORef html_xrefs_ref  modMap
  writeIORef html_xrefs_ref' modNameMap
  where
    mapping = [ (instMod iface, html) | (html, ifaces) <- packages
              , iface <- ifInstalledIfaces ifaces ]
    mapping' = [ (moduleName m, html) | (m, html) <- mapping ]


getPrologue :: ParserOpts -> [Flag] -> IO (Maybe (MDoc RdrName))
getPrologue parserOpts flags =
  case [filename | Flag_Prologue filename <- flags ] of
    [] -> return Nothing
    [filename] -> do
      h <- openFile filename ReadMode
      hSetEncoding h utf8
      str <- hGetContents h -- semi-closes the handle
      return . Just $! second (fmap rdrName) $ parseParas parserOpts Nothing str
    _ -> throwE "multiple -p/--prologue options"


rightOrThrowE :: Either String b -> IO b
rightOrThrowE (Left msg) = throwE msg
rightOrThrowE (Right x) = pure x

