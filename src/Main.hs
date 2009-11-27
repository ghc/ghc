{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2009
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

module Main (main) where


import Haddock.Backends.Html
import Haddock.Backends.Hoogle
import Haddock.Interface
import Haddock.Interface.Lex
import Haddock.Interface.Parse
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
import Distribution.Verbosity

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


--------------------------------------------------------------------------------
-- Exception handling
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
      Interrupted -> exitFailure
      _ -> do
        print (e :: GhcException)
        exitFailure


-------------------------------------------------------------------------------
-- Top level
-------------------------------------------------------------------------------


main :: IO ()
main = handleTopExceptions $ do

  -- parse command-line flags and handle some of them initially
  args <- getArgs
  (flags, fileArgs) <- parseHaddockOpts args
  handleEasyFlags flags
  verbosity <- getVerbosity flags

  let renderStep packages interfaces = do
        updateHTMLXRefs packages
        let ifaceFiles = map fst packages
            installedIfaces = concatMap ifInstalledIfaces ifaceFiles
        render flags interfaces installedIfaces

  if not (null fileArgs)
    then do

      libDir <- getGhcLibDir flags

      -- We have one global error handler for all GHC source errors.  Other kinds
      -- of exceptions will be propagated to the top-level error handler.
      let handleSrcErrors action = flip handleSourceError action $ \err -> do
            printExceptionAndWarnings err
            liftIO exitFailure

      -- initialize GHC
      startGhc libDir (ghcFlags flags) $ \_ -> handleSrcErrors $ do

        -- get packages supplied with --read-interface
        packages <- readInterfaceFiles nameCacheFromGhc (ifacePairs flags)


        -- create the interfaces -- this is the core part of Haddock
        (interfaces, homeLinks) <- createInterfaces verbosity fileArgs flags
                                                    (map fst packages)

        liftIO $ do
          -- render the interfaces
          renderStep packages interfaces

          -- last but not least, dump the interface file
          dumpInterfaceFile (map toInstalledIface interfaces) homeLinks flags

    else do
      -- get packages supplied with --read-interface
      packages <- readInterfaceFiles freshNameCache (ifacePairs flags)

      -- render even though there are no input files (usually contents/index)
      renderStep packages []


-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------


-- | Render the interfaces with whatever backend is specified in the flags
render :: [Flag] -> [Interface] -> [InstalledInterface] -> IO ()
render flags ifaces installedIfaces = do
  let
    title = case [str | Flag_Heading str <- flags] of
              [] -> ""
              (t:_) -> t

    maybe_source_urls = (listToMaybe [str | Flag_SourceBaseURL   str <- flags]
                        ,listToMaybe [str | Flag_SourceModuleURL str <- flags]
                        ,listToMaybe [str | Flag_SourceEntityURL str <- flags])

    maybe_wiki_urls = (listToMaybe [str | Flag_WikiBaseURL   str <- flags]
                      ,listToMaybe [str | Flag_WikiModuleURL str <- flags]
                      ,listToMaybe [str | Flag_WikiEntityURL str <- flags])

  libDir <- getHaddockLibDir flags
  let unicode = Flag_UseUnicode `elem` flags
  let css_file = case [str | Flag_CSS str <- flags] of
                   [] -> Nothing
                   fs -> Just (last fs)

  odir <- case [str | Flag_OutputDir str <- flags] of
            [] -> return "."
            fs -> return (last fs)

  let
    maybe_contents_url =
      case [url | Flag_UseContents url <- flags] of
        [] -> Nothing
        us -> Just (last us)

    maybe_index_url =
      case [url | Flag_UseIndex url <- flags] of
        [] -> Nothing
        us -> Just (last us)

    maybe_html_help_format =
      case [hhformat | Flag_HtmlHelp hhformat <- flags] of
        []      -> Nothing
        formats -> Just (last formats)

  prologue <- getPrologue flags

  let
    visibleIfaces    = [ i | i <- ifaces, OptHide `notElem` ifaceOptions i ]

    -- *all* visible interfaces including external package modules
    allIfaces        = map toInstalledIface ifaces ++ installedIfaces
    allVisibleIfaces = [ i | i <- allIfaces, OptHide `notElem` instOptions i ]

    packageMod       = ifaceMod (head ifaces)
    packageStr       = Just (modulePackageString packageMod)
    (pkgName,pkgVer) = modulePackageInfo packageMod


  when (Flag_GenIndex `elem` flags) $ do
    ppHtmlIndex odir title packageStr maybe_html_help_format
                maybe_contents_url maybe_source_urls maybe_wiki_urls
                allVisibleIfaces
    copyHtmlBits odir libDir css_file

  when (Flag_GenContents `elem` flags && Flag_GenIndex `elem` flags) $
    ppHtmlHelpFiles title packageStr visibleIfaces odir maybe_html_help_format []

  when (Flag_GenContents `elem` flags) $ do
    ppHtmlContents odir title packageStr maybe_html_help_format
                   maybe_index_url maybe_source_urls maybe_wiki_urls
                   allVisibleIfaces True prologue
    copyHtmlBits odir libDir css_file

  when (Flag_Html `elem` flags) $ do
    ppHtml title packageStr visibleIfaces odir
                prologue maybe_html_help_format
                maybe_source_urls maybe_wiki_urls
                maybe_contents_url maybe_index_url unicode
    copyHtmlBits odir libDir css_file

  when (Flag_Hoogle `elem` flags) $ do
    let pkgName2 = if pkgName == "main" && title /= [] then title else pkgName
    ppHoogle pkgName2 pkgVer title prologue visibleIfaces odir


-------------------------------------------------------------------------------
-- Reading and dumping interface files
-------------------------------------------------------------------------------


readInterfaceFiles :: MonadIO m =>
                      NameCacheAccessor m
                   -> [(FilePath, FilePath)] ->
                      m [(InterfaceFile, FilePath)]
readInterfaceFiles name_cache_accessor pairs = do
  mbPackages <- mapM tryReadIface pairs
  return (catMaybes mbPackages)
  where
    -- try to read an interface, warn if we can't
    tryReadIface (html, iface) = do
      eIface <- readInterfaceFile name_cache_accessor iface
      case eIface of
        Left err -> liftIO $ do
          putStrLn ("Warning: Cannot read " ++ iface ++ ":")
          putStrLn ("   " ++ show err)
          putStrLn "Skipping this interface."
          return Nothing
        Right f -> return $ Just (f, html)


dumpInterfaceFile :: [InstalledInterface] -> LinkEnv -> [Flag] -> IO ()
dumpInterfaceFile ifaces homeLinks flags =
  case [str | Flag_DumpInterface str <- flags] of
    [] -> return ()
    fs -> let filename = last fs in writeInterfaceFile filename ifaceFile
  where
    ifaceFile = InterfaceFile {
        ifInstalledIfaces = ifaces,
        ifLinkEnv         = homeLinks
      }


-------------------------------------------------------------------------------
-- Creating a GHC session
-------------------------------------------------------------------------------

-- | Start a GHC session with the -haddock flag set. Also turn off
-- compilation and linking.
startGhc :: String -> [String] -> (DynFlags -> Ghc a) -> IO a
startGhc libDir flags ghcActs = do
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
-- Misc
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


getVerbosity :: Monad m => [Flag] -> m Verbosity
getVerbosity flags =
  case [ str | Flag_Verbosity str <- flags ] of
    [] -> return normal
    x:_ -> case parseVerbosity x of
      Left e -> throwE e
      Right v -> return v


handleEasyFlags :: [Flag] -> IO ()
handleEasyFlags flags = do
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
  where
    byeVersion = bye $
      "Haddock version " ++ projectVersion ++ ", (c) Simon Marlow 2006\n"
      ++ "Ported to use the GHC API by David Waern 2006-2008\n"

    byeGhcVersion = bye (cProjectVersion ++ "\n")


updateHTMLXRefs :: [(InterfaceFile, FilePath)] -> IO ()
updateHTMLXRefs packages = writeIORef html_xrefs_ref (Map.fromList mapping)
  where
    mapping = [ (instMod iface, html) | (ifaces, html) <- packages
              , iface <- ifInstalledIfaces ifaces ]


getPrologue :: [Flag] -> IO (Maybe (HsDoc RdrName))
getPrologue flags =
  case [filename | Flag_Prologue filename <- flags ] of
    [] -> return Nothing
    [filename] -> do
      str <- readFile filename
      case parseHaddockParagraphs (tokenise str) of
        Nothing -> throwE "parsing haddock prologue failed"
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

