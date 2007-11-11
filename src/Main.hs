--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
-- 
-- Ported to use the GHC API by David Waern during "Summer of Code" 2006
--


module Main (main) where


import Haddock.Backends.Html
import Haddock.Backends.Hoogle
import Haddock.Interface
import Haddock.Types hiding (NoLink)
import Haddock.Version
import Haddock.InterfaceFile
import Haddock.Exception
import Haddock.Options
import Haddock.GHC
import Haddock.Utils
import Paths_haddock

import Control.Monad
import Control.Exception
import Control.Exception
import Data.Dynamic
import Data.Maybe
import Data.IORef
import qualified Data.Map as Map
import System.IO
import System.Exit
import System.Environment

import GHC
import DynFlags
import Bag
import Util (handleDyn)
import ErrUtils


--------------------------------------------------------------------------------
-- Exception handling
--------------------------------------------------------------------------------


handleTopExceptions = 
  handleNormalExceptions . handleHaddockExceptions . handleGhcExceptions


handleNormalExceptions inner =
  handle (\exception -> do
    hFlush stdout    
    case exception of
      AsyncException StackOverflow -> do
        putStrLn "stack overflow: use -g +RTS -K<size> to increase it"
        exitFailure
      ExitException code -> exitWith code
      _other -> do
        putStrLn ("haddock: internal Haddock or GHC error: " ++ show exception)
        exitFailure
  ) inner


handleHaddockExceptions inner = 
  handleDyn (\(e::HaddockException) -> do
    putStrLn $ "haddock: " ++ (show e)
    exitFailure
  ) inner


handleGhcExceptions inner = 
  -- compilation errors: messages with locations attached
  handleDyn (\dyn -> do
    putStrLn "haddock: Compilation error(s):"
    printBagOfErrors defaultDynFlags (unitBag dyn)
    exitFailure
  ) $

  -- error messages propagated as exceptions
  handleDyn (\dyn -> do
    hFlush stdout
    case dyn of
      PhaseFailed _ code -> exitWith code
      Interrupted -> exitFailure
      _ -> do 
        print (dyn :: GhcException)
        exitFailure
  ) inner


-------------------------------------------------------------------------------
-- Top level
-------------------------------------------------------------------------------


main :: IO ()
main = handleTopExceptions $ do

  -- parse command-line flags and handle some of them initially
  args <- getArgs
  (flags, fileArgs) <- parseHaddockOpts args
  libDir <- handleEasyFlags flags fileArgs
  
  -- initialize GHC
  (session, dynflags) <- startGhc libDir (ghcFlags flags)

  -- get packages via --read-interface
  packages <- readInterfaceFiles session (ifacePairs flags)

  -- typecheck argument modules using GHC
  modules <- typecheckFiles session fileArgs

  -- combine the link envs of the external packages into one
  let extLinks = Map.unions (map (ifLinkEnv . fst) packages)

  -- create the interfaces -- this is the core part of Haddock
  let (interfaces, homeLinks, messages) = createInterfaces modules extLinks flags
  mapM_ putStrLn messages

  -- render the interfaces
  updateHTMLXRefs packages
  let ifaceFiles = map fst packages
  let installedIfaces = concatMap ifInstalledIfaces ifaceFiles
  render flags interfaces installedIfaces

  -- last but not least, dump the interface file!
  dumpInterfaceFile (map toInstalledIface interfaces) homeLinks flags


-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------


-- | Render the interfaces with whatever backend is specified in the flags 
render :: [Flag] -> [Interface] -> [InstalledInterface] -> IO ()
render flags interfaces installedIfaces = do
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

    verbose = Flag_Verbose `elem` flags

  libdir <- case [str | Flag_Lib str <- flags] of
		[] -> getDataDir -- provided by Cabal
		fs -> return (last fs)

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
    -- visible home-module interfaces
    visibleIfaces = [ m | m <- interfaces, OptHide `notElem` (ifaceOptions m) ]

    -- *all* visible interfaces including external package modules
    allVisibleIfaces = map toInstalledIface visibleIfaces
                       ++ installedIfaces
    
    packageName     = (Just . modulePkgStr . ifaceMod . head) visibleIfaces
 
  when (Flag_GenIndex `elem` flags) $ do
    ppHtmlIndex odir title packageName maybe_html_help_format
                maybe_contents_url maybe_source_urls maybe_wiki_urls
                allVisibleIfaces
    copyHtmlBits odir libdir css_file
        
  when (Flag_GenContents `elem` flags && Flag_GenIndex `elem` flags) $ do
    ppHtmlHelpFiles title packageName visibleIfaces odir maybe_html_help_format []

  when (Flag_GenContents `elem` flags) $ do
    ppHtmlContents odir title packageName maybe_html_help_format
	                 maybe_index_url maybe_source_urls maybe_wiki_urls
	                 allVisibleIfaces True prologue
    copyHtmlBits odir libdir css_file

  when (Flag_Html `elem` flags) $ do
    ppHtml title packageName visibleIfaces odir
                prologue maybe_html_help_format
                maybe_source_urls maybe_wiki_urls
                maybe_contents_url maybe_index_url
    copyHtmlBits odir libdir css_file


-------------------------------------------------------------------------------
-- Reading and dumping interface files
-------------------------------------------------------------------------------


readInterfaceFiles :: Session -> [(FilePath, FilePath)] -> IO [(InterfaceFile, FilePath)]
readInterfaceFiles session pairs = do
  mbPackages <- mapM tryReadIface pairs
  return (catMaybes mbPackages)
  where
    -- try to read an interface, warn if we can't
    tryReadIface (html, iface) = do
      eIface <- readInterfaceFile session iface
      case eIface of
        Left err -> do
          putStrLn ("Warning: Cannot read " ++ iface ++ ":")
          putStrLn ("   " ++ show err)
          putStrLn "Skipping this interface."
          return Nothing
        Right iface -> return $ Just (iface, html)


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
-- Misc
-------------------------------------------------------------------------------


handleEasyFlags flags fileArgs = do
  usage <- getUsage

  when (Flag_Help       `elem` flags) (bye usage)
  when (Flag_Version    `elem` flags) byeVersion
  when (Flag_GhcVersion `elem` flags) byeGhcVersion

  let ghcLibDir = case [ dir | Flag_GhcLibDir dir <- flags ] of
                    [] -> throwE "no GHC lib dir specified"
                    xs -> last xs

  when ((Flag_GenIndex `elem` flags || Flag_GenContents `elem` flags)
        && Flag_Html `elem` flags) $
    throwE ("-h cannot be used with --gen-index or --gen-contents")

  return ghcLibDir
  where
    byeVersion = bye $
      "Haddock version " ++ projectVersion ++ 
      ", (c) Simon Marlow 2003; ported to the GHC-API by David Waern 2006\n"

    byeGhcVersion = bye $ 
      (fromJust $ lookup "Project version" $ compilerInfo) ++ "\n"


updateHTMLXRefs :: [(InterfaceFile, FilePath)] -> IO ()
updateHTMLXRefs packages = do
  writeIORef html_xrefs_ref (Map.fromList mapping)
  where
    mapping = [ (instMod iface, html) | (ifaces, html) <- packages,
                iface <- ifInstalledIfaces ifaces ]


getPrologue :: [Flag] -> IO (Maybe (HsDoc RdrName))
getPrologue flags
  = case [filename | Flag_Prologue filename <- flags ] of
	[] -> return Nothing 
	[filename] -> do
	   str <- readFile filename
	   case parseHaddockComment str of
		Left err -> throwE err
		Right doc -> return (Just doc)
	_otherwise -> throwE "multiple -p/--prologue options"
