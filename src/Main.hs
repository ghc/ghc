--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

module Main (main) where

import HaddockHtml
import HaddockHoogle
import HaddockRename
import HaddockTypes
import HaddockUtil
import HaddockVersion
import GHCUtils
import Paths_haddock_ghc     ( getDataDir, compilerPath )

import Prelude hiding ( catch )
import Control.Exception     
import Control.Monad         ( when, liftM, foldM )
import Control.Monad.Writer  ( Writer, runWriter, tell )
import Data.Char             ( isSpace )
import Data.IORef            ( writeIORef )
import Data.Ord
import Data.List             ( nub, nubBy, (\\), foldl', sortBy, foldl1, init, 
                               mapAccumL, find, isPrefixOf )
import Data.Maybe            ( Maybe(..), isJust, isNothing, maybeToList, 
                               listToMaybe, fromJust, catMaybes )
import Data.Word
import Data.Typeable
import Data.Graph hiding ( flattenSCC )
import System.Console.GetOpt ( getOpt, usageInfo, ArgOrder(..), OptDescr(..), 
                               ArgDescr(..) )
import System.Environment    ( getArgs )
import System.Directory      ( doesDirectoryExist )
import System.FilePath
import System.Cmd            ( system )
import System.Exit           ( ExitCode(..) )
import System.IO

import qualified Data.Map as Map
import Data.Map              (Map)

import Distribution.InstalledPackageInfo ( InstalledPackageInfo(..) ) 
import Distribution.Simple.Utils         ( withTempFile )

import GHC
import Outputable
import SrcLoc
import Digraph               ( flattenSCC )
import Name
import Module                ( mkModule ) 
import InstEnv
import Class
import TypeRep
import Var hiding ( varName )
import TyCon
import PrelNames
import Bag
import Binary
import HscTypes

import FastString
#define FSLIT(x) (mkFastString# (x#))

import DynFlags hiding ( Option )
import Packages hiding ( package ) 
import StaticFlags           ( parseStaticFlags )

--------------------------------------------------------------------------------
-- Top-level stuff
--------------------------------------------------------------------------------

-- | Get the GHC lib dir by asking the GHC binary that this program was built
-- with (or should've been built with if it hasn't moved).
getGHCLibDir = do
  str <- systemCaptureStdout 0 (compilerPath ++ " --print-libdir")
  case lines str of 
    (path:_) -> return path 
    _ -> die ("Error: " ++ compilerPath ++ " did not respond well to " ++
             "--print-libdir")  

-- temporarily taken from Cabal. TODO: use a library
systemCaptureStdout :: Int -> String -> IO String
systemCaptureStdout verbose cmd = do
   withTempFile "." "" $ \tmp -> do
      let cmd_line  = cmd ++ " >" ++ tmp
      when (verbose > 0) $ putStrLn cmd_line
      res <- system cmd_line
      case res of
        ExitFailure _ -> die ("executing external program failed: "++cmd_line)
        ExitSuccess   -> do str <- readFile tmp
                            let ev [] = ' '; ev xs = last xs
                            ev str `seq` return str

main :: IO ()
main = do
  args <- getArgs
  libDir <- getGHCLibDir

  -- find out which flag mode we are in
  let (isGHCMode, rest) = parseModeFlag args

  -- initialize GHC 
  (session, dynflags) <- startGHC libDir

  -- parse GHC flags given to the program 
  (dynflags', rest') <- if isGHCMode 
    then parseGHCFlags_GHCMode     dynflags rest  
    else parseGHCFlags_HaddockMode dynflags rest 
  setSessionDynFlags session dynflags'

  -- parse Haddock specific flags
  (flags, fileArgs) <- parseHaddockOpts rest'

  -- try to sort and check the input files using the GHC API 
  modules <- sortAndCheckModules session dynflags' fileArgs

  -- create a PackageData for each external package in the session
  -- using the GHC API. The PackageData contains an html path,
  -- a doc env and a list of module names.
  packages <- getPackages session dynflags' flags

  -- update the html references (module -> html file mapping)
  updateHTMLXRefs packages

  -- combine the doc envs of the external packages into one
  let env = packagesDocEnv packages

	-- TODO: continue to break up the run function into parts
  run flags modules env

parseModeFlag :: [String] -> (Bool, [String])
parseModeFlag ("--ghc-flags":rest) = (True, rest)
parseModeFlag rest = (False, rest)

parseGHCFlags_GHCMode :: DynFlags -> [String] -> IO (DynFlags, [String])
parseGHCFlags_GHCMode dynflags args = do
  (dynflags', rest) <- parseDynamicFlags dynflags args
  rest' <- parseStaticFlags rest
  return (dynflags', rest')

parseGHCFlags_HaddockMode = parseGHCFlags

parseGHCFlags :: DynFlags -> [String] -> IO (DynFlags, [String])
parseGHCFlags dynflags args = case args of
  [] -> return (dynflags, args)
  ("-g":rest) -> worker rest 
  (('-':'-':'g':'h':'c':'-':'f':'l':'a':'g':'=':str):rest) -> worker (str:rest)
  (x:xs) -> do 
    (flags, rest) <- parseGHCFlags dynflags xs
    return (flags, x:rest)
  where 
{-    worker strs = do
      let (inside, _:outside) = break (=='"') (unwords strs) 
      (dynflags', rest) <- parseDynamicFlags dynflags (words inside)    
      when (rest == words inside) $ parseStaticFlags (words inside) >> return ()
      parseGHCFlags dynflags' (words outside)  
-}
    worker rest = do
      (mbFlags, rest') <- parseGHCFlag dynflags rest
      case mbFlags of 
        Just flags -> parseGHCFlags flags rest'
        Nothing -> parseGHCFlags dynflags rest'

parseGHCFlag :: DynFlags -> [String] -> IO (Maybe DynFlags, [String])
parseGHCFlag _ [] = die "No GHC flag supplied\n"
parseGHCFlag dynflags args = do
  mbDyn <- findDynamic
  case mbDyn of 
    Just (dynflags', rest) -> return (Just dynflags', rest)   
    Nothing -> do
      mbStat <- findStatic
      case mbStat of
        Just (_, rest) -> return (Nothing, rest)
        Nothing -> die ("Not a GHC flag: " ++ (head args) ++ "\n")
  where
    findDynamic = findFlag (\xs -> (do 
          (fs, xs') <- parseDynamicFlags dynflags xs
          if xs' /= xs then return (Just fs) else return Nothing
        ) `catch` (\_ -> return Nothing) 
      ) 

    findStatic = findFlag (\xs -> do 
        xs' <- parseStaticFlags xs
        if xs /= xs' then return (Just ()) else return Nothing
      )

    findFlag p = do 
      xs <- (sequence . snd) (mapAccumL (f p) [] args)
      case [ (x, index) | Just x <- xs | index <- [1..] ] of
        ((x, index):_) -> return (Just (x, drop index args))
        _ -> return Nothing

    f :: ([String] -> IO a) -> [String] -> String -> ([String], IO a)
    f parser previousArgs arg = 
      let args' = previousArgs ++ [arg]
      in (args', parser args')

parseHaddockOpts :: [String] -> IO ([Flag], [String])
parseHaddockOpts words =
  case getOpt Permute (options True) words of
    (flags, args, []) -> return (flags, args)
    (_, _, errors)    -> do 
      prog <- getProgramName
      die (concat errors ++ usageInfo (usageHeader prog) (options False))

usageHeader :: String -> String
usageHeader prog = "Usage: " ++ prog ++ " [OPTION...] file...\n"

extractGHCFlags :: [Flag] -> [String]
extractGHCFlags flags = [ flag | Flag_GHCFlag flag <- flags ]

startGHC :: String -> IO (Session, DynFlags)
startGHC libDir = do
  let ghcMode = BatchCompile
  session <- newSession ghcMode (Just libDir)
  flags   <- getSessionDynFlags session
  flags'  <- liftM fst (initPackages flags)
  let flags'' = dopt_set flags' Opt_Haddock 
  return (session, flags'')

sortAndCheckModules :: Session -> DynFlags -> [FilePath] -> IO [CheckedMod]
sortAndCheckModules session flags files = defaultErrorHandler flags $ do 
  targets <- mapM (\s -> guessTarget s Nothing) files
  setTargets session targets 
  mbModGraph <- depanal session [] True
  moduleGraph <- case mbModGraph of 
    Just mg -> return mg 
    Nothing -> die "Failed to load all modules\n" 
  let 
    modSumFile    = fromJust . ml_hs_file . ms_location
    sortedGraph   = topSortModuleGraph False moduleGraph Nothing
    sortedModules = concatMap flattenSCC sortedGraph 
    modsAndFiles  = [ (ms_mod modsum, modSumFile modsum) | 
                      modsum <- sortedModules, 
                      modSumFile modsum `elem` files ] 
  checkedMods <- mapM (\(mod, file) -> do
    mbMod <- checkModule session (moduleName mod)
    checkedMod <- case mbMod of 
      Just m  -> return m
      Nothing -> die ("Failed to load module: " ++ moduleString mod ++ "\n")
    return (mod, file, checkedMod)) modsAndFiles 
  ensureFullyChecked checkedMods
  where
    ensureFullyChecked modules 
      | length modules' == length modules = return modules'
      | otherwise = die "Failed to check all modules properly\n" 
      where modules' = [ (mod, f, (a,b,c,d)) | 
                         (mod, f, CheckedModule a (Just b) (Just c) (Just d)) 
                         <- modules ] 

data Flag
  = Flag_CSS String
  | Flag_Debug
--  | Flag_DocBook
  | Flag_Heading String
  | Flag_Package String
  | Flag_Html
  | Flag_Hoogle
  | Flag_HtmlHelp String
  | Flag_Lib String
  | Flag_NoImplicitPrelude
  | Flag_OutputDir FilePath
  | Flag_Prologue FilePath
  | Flag_SourceBaseURL   String
  | Flag_SourceModuleURL String
  | Flag_SourceEntityURL String
  | Flag_WikiBaseURL   String
  | Flag_WikiModuleURL String
  | Flag_WikiEntityURL String
  | Flag_Help
  | Flag_Verbose
  | Flag_Version
  | Flag_UseContents String
  | Flag_GenContents
  | Flag_UseIndex String
  | Flag_GenIndex
  | Flag_IgnoreAllExports
  | Flag_HideModule String
  | Flag_UsePackage String
  | Flag_GHCFlag String
  deriving (Eq)

options :: Bool -> [OptDescr Flag]
options backwardsCompat =
  [
    Option ['o']  ["odir"]     (ReqArg Flag_OutputDir "DIR")
	"directory in which to put the output files",
   Option ['l']  ["lib"]         (ReqArg Flag_Lib "DIR") 
	"location of Haddock's auxiliary files",
--    Option ['S']  ["docbook"]  (NoArg Flag_DocBook)
--	"output in DocBook XML",
    Option ['h']  ["html"]     (NoArg Flag_Html)
	"output in HTML",
    Option []  ["hoogle"]     (NoArg Flag_Hoogle)
    "output for Hoogle",
    Option []  ["html-help"]    (ReqArg Flag_HtmlHelp "format")
	"produce index and table of contents in\nmshelp, mshelp2 or devhelp format (with -h)",
    Option []  ["source-base"]   (ReqArg Flag_SourceBaseURL "URL") 
	"URL for a source code link on the contents\nand index pages",
    Option ['s'] (if backwardsCompat then ["source", "source-module"] else ["source-module"])
        (ReqArg Flag_SourceModuleURL "URL")
	"URL for a source code link for each module\n(using the %{FILE} or %{MODULE} vars)",
    Option []  ["source-entity"]  (ReqArg Flag_SourceEntityURL "URL") 
	"URL for a source code link for each entity\n(using the %{FILE}, %{MODULE} or %{NAME} vars)",
    Option []  ["comments-base"]   (ReqArg Flag_WikiBaseURL "URL")
	"URL for a comments link on the contents\nand index pages",
    Option []  ["comments-module"]  (ReqArg Flag_WikiModuleURL "URL") 
	"URL for a comments link for each module\n(using the %{MODULE} var)",
    Option []  ["comments-entity"]  (ReqArg Flag_WikiEntityURL "URL") 
	"URL for a comments link for each entity\n(using the %{FILE}, %{MODULE} or %{NAME} vars)",
    Option ['c']  ["css"]         (ReqArg Flag_CSS "FILE") 
	"the CSS file to use for HTML output",
    Option ['p']  ["prologue"] (ReqArg Flag_Prologue "FILE")
	"file containing prologue text",
    Option ['t']  ["title"]    (ReqArg Flag_Heading "TITLE")
	"page heading",
    Option ['k']  ["package"]  (ReqArg Flag_Package "NAME")
	"package name (optional)",
    Option ['n']  ["no-implicit-prelude"] (NoArg Flag_NoImplicitPrelude)
 	"do not assume Prelude is imported",
    Option ['d']  ["debug"]  (NoArg Flag_Debug)
	"extra debugging output",
    Option ['?']  ["help"]  (NoArg Flag_Help)
	"display this help and exit",
    Option ['V']  ["version"]  (NoArg Flag_Version)
	"output version information and exit",
    Option ['v']  ["verbose"]  (NoArg Flag_Verbose)
        "increase verbosity",
    Option [] ["use-contents"] (ReqArg Flag_UseContents "URL")
	"use a separately-generated HTML contents page",
    Option [] ["gen-contents"] (NoArg Flag_GenContents)
	"generate an HTML contents from specified\ninterfaces",
    Option [] ["use-index"] (ReqArg Flag_UseIndex "URL")
	"use a separately-generated HTML index",
    Option [] ["gen-index"] (NoArg Flag_GenIndex)
	"generate an HTML index from specified\ninterfaces",
    Option [] ["ignore-all-exports"] (NoArg Flag_IgnoreAllExports)
	"behave as if all modules have the\nignore-exports atribute",
    Option [] ["hide"] (ReqArg Flag_HideModule "MODULE")
	"behave as if MODULE has the hide attribute",
    Option [] ["use-package"] (ReqArg Flag_UsePackage "PACKAGE")
	"the modules being processed depend on PACKAGE",
    Option ['g'] ["ghc-flag"] (ReqArg Flag_GHCFlag "FLAG")
 	"send a flag to the Glasgow Haskell Compiler"       
  ]

handleEagerFlags flags = do
  whenFlag Flag_Help $ do
    prog <- getProgramName
    bye (usageInfo (usageHeader prog) (options False))

  whenFlag Flag_Version $
    bye ("Haddock version " ++ projectVersion ++ 
         ", (c) Simon Marlow 2003; port to GHC-api by David Waern 2006\n")

  when ((Flag_GenIndex `elem` flags || Flag_GenContents `elem` flags)
        && Flag_Html `elem` flags) $
    die ("-h cannot be used with --gen-index or --gen-contents")

  return (listToMaybe [str | Flag_Package str <- flags])
  where 
    whenFlag flag action = when (flag `elem` flags) action 

run :: [Flag] -> [CheckedMod] -> Map Name Name -> IO ()
run flags modules extEnv = do
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
    -- collect the data from GHC that we need for each home module
    ghcModuleData = map moduleDataGHC modules
    -- run pass 1 on this data
    (modMap, messages) = runWriter (pass1 ghcModuleData flags) 

    haddockMods = catMaybes [ Map.lookup mod modMap | (mod,_,_) <- modules ]
    homeEnv = buildGlobalDocEnv haddockMods
    env = homeEnv `Map.union` extEnv
    haddockMods' = attachInstances haddockMods
    (haddockMods'', messages') = runWriter $ mapM (renameModule env) haddockMods'
  
  mapM_ putStrLn messages
  mapM_ putStrLn messages'

  let 
    visibleMods = [ m | m <- haddockMods'', DocOptHide `notElem` (hmod_options m) ]
    packageName = (Just . packageIdString . modulePackageId . 
                   hmod_mod . head) visibleMods
 
  when (Flag_GenIndex `elem` flags) $ do
	ppHtmlIndex odir title packageName maybe_html_help_format
                maybe_contents_url maybe_source_urls maybe_wiki_urls
                visibleMods
	copyHtmlBits odir libdir css_file
        
  when (Flag_GenContents `elem` flags && Flag_GenIndex `elem` flags) $ do
    ppHtmlHelpFiles title packageName visibleMods odir maybe_html_help_format []

  when (Flag_GenContents `elem` flags) $ do
	ppHtmlContents odir title packageName maybe_html_help_format
	               maybe_index_url maybe_source_urls maybe_wiki_urls
	               visibleMods True prologue
	copyHtmlBits odir libdir css_file

  when (Flag_Html `elem` flags) $ do
    ppHtml title packageName visibleMods odir
                prologue maybe_html_help_format
                maybe_source_urls maybe_wiki_urls
                maybe_contents_url maybe_index_url
    copyHtmlBits odir libdir css_file

  case [str | Flag_DumpInterface str <- flags] of
        [] -> return ()
        fs -> let filename = (last fs) in 
              writePackageFile filename homeEnv
{- 
instance Outputable (DocEntity Name) where
  ppr (DocEntity d) = ppr d
  ppr (DeclEntity name) = ppr name

instance Show Name where
  show name = show (ppr name defaultUserStyle)

instance Show a => Show (DocDecl a) where
  show (DocCommentNext doc) = "next" ++ show doc
  show (DocCommentPrev doc) = "prev" ++ show doc
  show _ = "other" 
-}

type CheckedMod = (Module, FilePath, FullyCheckedMod)

type FullyCheckedMod = (ParsedSource, 
                        RenamedSource, 
                        TypecheckedSource, 
                        ModuleInfo)

--printEntity (DocEntity doc) = show doc
--printEntity (DeclEntity name) = show $ ppr name defaultUserStyle

-- | This data structure collects all the information we need about a home 
-- package module
data ModuleDataGHC = ModuleDataGHC {
   ghcModule         :: Module,
   ghcFilename       :: FilePath,
   ghcDocOpts        :: [DocOption],
   ghcHaddockModInfo :: HaddockModInfo Name,
   ghcMbDoc          :: Maybe (HsDoc Name),
   ghcGroup          :: HsGroup Name,
   ghcMbExports      :: Maybe [LIE Name],
   ghcExportedNames  :: [Name],
   ghcNamesInScope   :: [Name],
   ghcInstances      :: [Instance]
}

-- | Dig out what we want from the GHC API without altering anything
moduleDataGHC :: CheckedMod -> ModuleDataGHC 
moduleDataGHC (mod, file, checkedMod) = ModuleDataGHC {
  ghcModule         = mod,
  ghcFilename       = file,
  ghcDocOpts        = opts,
  ghcHaddockModInfo = info,
  ghcMbDoc          = mbDoc,
  ghcGroup          = group,
  ghcMbExports      = mbExports,
  ghcExportedNames  = modInfoExports modInfo,
  ghcNamesInScope   = fromJust $ modInfoTopLevelScope modInfo, 
  ghcInstances      = modInfoInstances modInfo
}
  where
    HsModule _ _ _ _ _ opts _ _      = unLoc parsed
    (group, _, mbExports, (mbDoc, info, nodocs)) = renamed
    (parsed, renamed, _, modInfo)      = checkedMod 

-- | Massage the data in ModuleDataGHC to produce something closer to what
-- we want to render. To do this, we need access to modules before this one
-- in the topological sort, to which we have already done this conversion. 
-- That's what's in the ModuleMap.
pass1data :: ModuleDataGHC -> [Flag] -> ModuleMap -> ErrMsgM HaddockModule
pass1data modData flags modMap = do

  let mod          = ghcModule modData
      opts         = docOpts (ghcDocOpts modData) mod
      group        = ghcGroup modData
      entities     = (nubBy sameName . collectEntities) group
      exports      = fmap (reverse . map unLoc) (ghcMbExports modData)
      entityNames_ = entityNames entities
      subNames     = allSubNames group
      localNames   = entityNames_ ++ subNames
      subMap       = mkSubMap group
      expDeclMap   = mkDeclMap (ghcExportedNames modData) group
      localDeclMap = mkDeclMap entityNames_ group
      docMap       = mkDocMap group 
      ignoreExps   = Flag_IgnoreAllExports `elem` flags

  visibleNames <- mkVisibleNames mod modMap localNames (ghcNamesInScope modData) 
                                 subMap exports opts localDeclMap 

  exportItems <- mkExportItems modMap mod (ghcExportedNames modData)
                               expDeclMap localDeclMap subMap entities 
                               opts exports ignoreExps docMap 

  -- prune the export list to just those declarations that have
  -- documentation, if the 'prune' option is on.
  let 
    prunedExportItems
      | DocOptPrune `elem` opts = pruneExportItems exportItems
      | otherwise = exportItems
 
  return HM {
    hmod_mod                = mod,
    hmod_orig_filename      = ghcFilename modData,
    hmod_info               = ghcHaddockModInfo modData,
    hmod_doc                = ghcMbDoc modData,
    hmod_rn_doc             = Nothing,
    hmod_options            = opts,
    hmod_locals             = localNames,
    hmod_doc_map            = docMap,
    hmod_rn_doc_map         = Map.empty,
    hmod_sub_map            = subMap,
    hmod_export_items       = prunedExportItems,
    hmod_rn_export_items    = [], 
    hmod_exports            = ghcExportedNames modData,
    hmod_visible_exports    = visibleNames, 
    hmod_exported_decl_map  = expDeclMap,
    hmod_instances          = ghcInstances modData
  }
  where
    docOpts opts mod =  
      if Flag_HideModule (moduleString mod) `elem` flags 
        then DocOptHide : opts
        else opts      

-- | Produce a map of HaddockModules with information that is close to 
-- renderable.  What is lacking after this pass are the renamed export items.
pass1 :: [ModuleDataGHC] -> [Flag] -> ErrMsgM ModuleMap
pass1 modules flags = foldM produceAndInsert Map.empty modules
  where
    produceAndInsert modMap modData = do
      resultMod <- pass1data modData flags modMap
      let key = ghcModule modData
      return (Map.insert key resultMod modMap)

sameName (DocEntity _) _ = False
sameName (DeclEntity _) (DocEntity _) = False
sameName (DeclEntity a) (DeclEntity b) = a == b

-- This map includes everything that can be exported separately,
-- that means: top declarations, class methods and record selectors
-- TODO: merge this with mkDeclMap and the extractXXX functions 
mkDocMap :: HsGroup Name -> Map Name (HsDoc Name)
mkDocMap group = Map.fromList (topDeclDocs ++ classMethDocs ++ recordFieldDocs)
  where
    tyclds    = map unLoc (hs_tyclds group)
    classes   = filter isClassDecl tyclds 
    datadecls = filter isDataDecl tyclds
    constrs   = [ con | d <- datadecls, L _ con <- tcdCons d ]
    fields    = concat [ fields | RecCon fields <- map con_details constrs]

    topDeclDocs   = collectDocs (collectEntities group)
    classMethDocs = concatMap (collectDocs . collectClassEntities) classes

    recordFieldDocs = [ (unLoc lname, doc) | 
                        HsRecField lname _ (Just (L _ doc)) <- fields ]

--------------------------------------------------------------------------------
-- Source code entities
--------------------------------------------------------------------------------

data Entity = DocEntity (DocDecl Name) | DeclEntity Name
data LEntity = Located Entity

sortByLoc = map unLoc . sortBy (comparing getLoc)

-- | Collect all the entities in a class that can be documented. 
-- The entities are sorted by their SrcLoc.
collectClassEntities tcd = sortByLoc (docs ++ meths ++ sigs)
  where
    docs = [ L l (DocEntity d) | L l d <- tcdDocs tcd ]
    meths = 
      let bindings = bagToList (tcdMeths tcd)
          bindingName = unLoc . fun_id
      in [ L l (DeclEntity (bindingName b)) | L l b <- bindings ] 
    sigs = 
      let sigName = fromJust . sigNameNoLoc 
      in [ L l (DeclEntity (sigName sig)) | L l sig <- tcdSigs tcd ]  

-- | Collect all the entities in the source file that can be documented. 
-- The entities are sorted by their SrcLoc.
collectEntities :: HsGroup Name -> [Entity]
collectEntities group = sortByLoc (docs ++ declarations)
  where
    docs = [ L l (DocEntity d) | L l d <- hs_docs group ]

    declarations = [ L l (DeclEntity n) | (l, n) <- valds ++ tyclds ++ fords ]
      where
        valds = let ValBindsOut _ sigs = hs_valds group 
             -- we just use the sigs here for now.
             -- TODO: collect from the bindings as well 
             -- (needed for docs to work for inferred entities)
                in [ (l, fromJust (sigNameNoLoc s)) | L l s <- sigs ] 
        tyclds = [ (l, tcdName t) | L l t <- hs_tyclds group ]
        fords  = [ (l, forName f) | L l f <- hs_fords group ]  
          where
            forName (ForeignImport name _ _) = unLoc name
            forName (ForeignExport name _ _) = unLoc name

-- | Collect the docs and attach them to the right name
collectDocs :: [Entity] -> [(Name, HsDoc Name)]
collectDocs entities = collect Nothing DocEmpty entities

collect :: Maybe Entity -> HsDoc Name -> [Entity] -> [(Name, HsDoc Name)]
collect d doc_so_far [] =
   case d of
        Nothing -> []
        Just d0  -> finishedDoc d0 doc_so_far []

collect d doc_so_far (e:es) =
  case e of
    DocEntity (DocCommentNext str) ->
      case d of
        Nothing -> collect d (docAppend doc_so_far str) es
        Just d0 -> finishedDoc d0 doc_so_far (collect Nothing str es)

    DocEntity (DocCommentPrev str) -> collect d (docAppend doc_so_far str) es

    _ -> case d of
      Nothing -> collect (Just e) doc_so_far es
      Just d0
        | sameName d0 e -> collect d doc_so_far es  
        | otherwise -> finishedDoc d0 doc_so_far (collect (Just e) DocEmpty es)

finishedDoc :: Entity -> HsDoc Name -> [(Name, HsDoc Name)] -> 
               [(Name, HsDoc Name)]
finishedDoc d DocEmpty rest = rest
finishedDoc (DeclEntity name) doc rest = (name, doc) : rest
finishedDoc _ _ rest = rest

--------------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------------
       
allSubNames :: HsGroup Name -> [Name]
allSubNames group = 
  concat [ tail (map unLoc (tyClDeclNames tycld)) | L _ tycld <- hs_tyclds group ]

mkSubMap :: HsGroup Name -> Map Name [Name]
mkSubMap group = Map.fromList [ (name, subs) | L _ tycld <- hs_tyclds group,
 let name:subs = map unLoc (tyClDeclNames tycld) ]

mkDeclMap :: [Name] -> HsGroup Name -> Map Name (LHsDecl Name) 
mkDeclMap names group = Map.fromList [ (n,d)  | (n,Just d) <- maybeDecls ]
  where 
  maybeDecls = [ (name, getDeclFromGroup group name) | name <- names ]

entityNames :: [Entity] -> [Name]
entityNames entities = [ name | DeclEntity name <- entities ] 
{-
getValSig :: Name -> HsValBinds Name -> TypeEnv -> Maybe (LSig Name)
getValSig name (ValBindsOut recsAndBinds _) typEnv = case matchingBinds of
  [bind] -> -- OK we have found a binding that matches. Now look up the
            -- type, even though it may be present in the ValBindsOut
            let tything = lookupTypeEnv typeEnv name       
  _ -> Nothing
  where 
    binds = snd $ unzip recsAndBinds 
    matchingBinds = Bag.filter matchesName binds
    matchesName (L _ bind) = fun_id bind == name
getValSig _ _ _ = error "getValSig"
-}
getDeclFromGroup :: HsGroup Name -> Name -> Maybe (LHsDecl Name)
getDeclFromGroup group name = 
  case catMaybes [ getDeclFromVals  (hs_valds  group), 
                   getDeclFromTyCls (hs_tyclds group),
                   getDeclFromFors  (hs_fords  group) ] of
    [decl] -> Just decl
    _ -> Nothing
  where 
    getDeclFromVals (ValBindsOut _ lsigs) = case matching of 
      [lsig] -> Just (L (getLoc lsig) (SigD (unLoc lsig)))
      _      -> Nothing
     where 
        matching = [ lsig | lsig <- lsigs, let Just n = sigName lsig, n == name, 
                     isNormal (unLoc lsig) ]
        isNormal (TypeSig _ _) = True
        isNormal _ = False

    getDeclFromVals _ = error "getDeclFromVals: illegal input"

{-    getDeclFromVals (ValBindsOut recsAndbinds _) = 
      let binds = snd $ unzip recsAndBinds 
          matchingBinds = Bag.filter matchesName binds
          matchesName (L _ bind) = fun_id bind == name
      in case matchingBinds of 
        [bind] -> -- OK we have found a binding that matches. Now look up the
                  -- type, even though it may be present in the ValBindsOut
                  
        _ -> Nothing
     where 
        matching = [ lsig | lsig <- lsigs, let Just n = sigName lsig, n == name ]
    getDeclFromVals _ = error "getDeclFromVals: illegal input"
  -}    
    getDeclFromTyCls ltycls = case matching of 
      [ltycl] -> Just (L (getLoc ltycl) (TyClD (unLoc ltycl)))
      _       -> Nothing
      where
        matching = [ ltycl | ltycl <- ltycls, 
                     name `elem` map unLoc (tyClDeclNames (unLoc ltycl))]
 
    getDeclFromFors lfors = case matching of 
      [for] -> Just (L (getLoc for) (ForD (unLoc for)))
      _      -> Nothing
      where
        matching = [ for | for <- lfors, forName (unLoc for) == name ]
        forName (ForeignExport n _ _) = unLoc n
        forName (ForeignImport n _ _) = unLoc n
 
parseIfaceOption :: String -> (FilePath,FilePath)
parseIfaceOption s = 
  case break (==',') s of
	(fpath,',':file) -> (fpath,file)
	(file, _)        -> ("", file)
	
updateHTMLXRefs :: [PackageData] -> IO ()
updateHTMLXRefs packages = writeIORef html_xrefs_ref (Map.fromList mapping)
  where
    mapping = [ (mod, html) | 
                (PackageData mods _ html) <- packages, mod <- mods ] 

getPrologue :: [Flag] -> IO (Maybe (HsDoc RdrName))
getPrologue flags
  = case [filename | Flag_Prologue filename <- flags ] of
	[] -> return Nothing 
	[filename] -> do
	   str <- readFile filename
	   case parseHaddockComment str of
		Left err -> die err
		Right doc -> return (Just doc)
	_otherwise -> die "multiple -p/--prologue options"

-- -----------------------------------------------------------------------------
-- Phase 2

renameModule :: Map Name Name -> HaddockModule -> ErrMsgM HaddockModule
renameModule renamingEnv mod =

  -- first create the local env, where every name exported by this module
  -- is mapped to itself, and everything else comes from the global renaming
  -- env
  let localEnv = foldl fn renamingEnv (hmod_visible_exports mod)
        where fn env name = Map.insert name (nameSetMod name (hmod_mod mod)) env
      
      docs = Map.toList (hmod_doc_map mod)
      renameMapElem (k,d) = do d' <- renameDoc d; return (k, d') 

      -- rename names in the exported declarations to point to things that
      -- are closer to, or maybe even exported by, the current module.
      (renamedExportItems, missingNames1)
        = runRnFM localEnv (renameExportItems (hmod_export_items mod))

      (rnDocMap, missingNames2) 
        = runRnFM localEnv (liftM Map.fromList (mapM renameMapElem docs))

      (finalModuleDoc, missingNames3)
        = runRnFM localEnv (renameMaybeDoc (hmod_doc mod))

      -- combine the missing names and filter out the built-ins, which would
      -- otherwise allways be missing. 
      missingNames = nub $ filter isExternalName
                    (missingNames1 ++ missingNames2 ++ missingNames3)

      -- filter out certain built in type constructors using their string 
      -- representation. TODO: use the Name constants from the GHC API.
      strings = filter (`notElem` ["()", "[]", "(->)"]) 
                (map (showSDoc . ppr) missingNames) 
     
  in do
    -- report things that we couldn't link to. Only do this for non-hidden
    -- modules.
    when (DocOptHide `notElem` hmod_options mod && not (null strings)) $
	  tell ["Warning: " ++ show (ppr (hmod_mod mod) defaultUserStyle) ++ 
		": could not find link destinations for:\n"++
		"   " ++ concat (map (' ':) strings) ]

    return $ mod { hmod_rn_doc = finalModuleDoc,
                   hmod_rn_doc_map = rnDocMap,
                   hmod_rn_export_items = renamedExportItems }

-- -----------------------------------------------------------------------------
-- Build the list of items that will become the documentation, from the
-- export list.  At this point, the list of ExportItems is in terms of
-- original names.

mkExportItems
  :: ModuleMap
  -> Module			-- this module
  -> [Name]			-- exported names (orig)
  -> Map Name (LHsDecl Name) -- maps exported names to declarations
  -> Map Name (LHsDecl Name) -- maps local names to declarations
  -> Map Name [Name]	-- sub-map for this module
  -> [Entity]	-- entities in the current module
  -> [DocOption]
  -> Maybe [IE Name]
  -> Bool				-- --ignore-all-exports flag
  -> Map Name (HsDoc Name)
  -> ErrMsgM [ExportItem Name]

mkExportItems mod_map this_mod exported_names exportedDeclMap localDeclMap sub_map entities
              opts maybe_exps ignore_all_exports docMap
  | isNothing maybe_exps || ignore_all_exports || DocOptIgnoreExports `elem` opts
    = everything_local_exported
  | Just specs <- maybe_exps = do 
      exps <- mapM lookupExport specs
      return (concat exps)
  where
    everything_local_exported =  -- everything exported
      return (fullContentsOfThisModule this_mod entities localDeclMap docMap)
   
    packageId = modulePackageId this_mod

    lookupExport (IEVar x)             = declWith x
    lookupExport (IEThingAbs t)        = declWith t
    lookupExport (IEThingAll t)        = declWith t
    lookupExport (IEThingWith t cs)    = declWith t
    lookupExport (IEModuleContents m)  = fullContentsOf (mkModule packageId m)
    lookupExport (IEGroup lev doc)     = return [ ExportGroup lev "" doc ]
    lookupExport (IEDoc doc)           = return [ ExportDoc doc ] 
    lookupExport (IEDocNamed str)
	= do r <- findNamedDoc str entities
	     case r of
		Nothing -> return []
		Just found -> return [ ExportDoc found ]
 
    declWith :: Name -> ErrMsgM [ ExportItem Name ]
    declWith t
	| (Just decl, maybeDoc) <- findDecl t
        = return [ ExportDecl t (restrictTo subs (extractDecl t mdl decl)) maybeDoc [] ]
	| otherwise
	= return []
	where 
              mdl = nameModule t
	      subs = filter (`elem` exported_names) all_subs
              all_subs | mdl == this_mod = Map.findWithDefault [] t sub_map
		       | otherwise       = allSubsOfName mod_map t

    fullContentsOf m  
	| m == this_mod = return (fullContentsOfThisModule this_mod entities localDeclMap docMap)
	| otherwise = 
	   case Map.lookup m mod_map of
	     Just hmod
		| DocOptHide `elem` hmod_options hmod
			-> return (hmod_export_items hmod)
		| otherwise -> return [ ExportModule m ]
	     Nothing -> return [] -- already emitted a warning in visibleNames

    findDecl :: Name -> (Maybe (LHsDecl Name), Maybe (HsDoc Name))
    findDecl n | not (isExternalName n) = error "This shouldn't happen"
    findDecl n 
	| m == this_mod = (Map.lookup n exportedDeclMap, Map.lookup n docMap)
	| otherwise = 
	   case Map.lookup m mod_map of
		Just hmod -> (Map.lookup n (hmod_exported_decl_map hmod), 
                              Map.lookup n (hmod_doc_map hmod))
		Nothing -> (Nothing, Nothing)
      where
        m = nameModule n

fullContentsOfThisModule :: Module -> [Entity] -> Map Name (LHsDecl Name) ->
                            Map Name (HsDoc Name) -> [ExportItem Name]
fullContentsOfThisModule module_ entities declMap docMap 
  = catMaybes (map mkExportItem entities)
  where 
    mkExportItem (DocEntity (DocGroup lev doc)) = Just (ExportGroup lev "" doc)
    mkExportItem (DeclEntity name) = fmap mkExport (Map.lookup name declMap) 
      where mkExport decl = ExportDecl name decl (Map.lookup name docMap) []

-- Sometimes the declaration we want to export is not the "main" declaration:
-- it might be an individual record selector or a class method.  In these
-- cases we have to extract the required declaration (and somehow cobble 
-- together a type signature for it...)
 
extractDecl :: Name -> Module -> LHsDecl Name -> LHsDecl Name
extractDecl name mdl decl
  | Just n <- getMainDeclBinder (unLoc decl), n == name = decl
  | otherwise  =  
    case unLoc decl of
      TyClD d | isClassDecl d -> 
        let matches = [ sig | sig <- tcdSigs d, sigName sig == Just name ] 
        in case matches of 
          [s0] -> let (n, tyvar_names) = name_and_tyvars d
                      L pos sig = extractClassDecl n mdl tyvar_names s0
                  in L pos (SigD sig)
          _ -> error "internal: extractDecl" 
      TyClD d | isDataDecl d -> 
        let (n, tyvar_names) = name_and_tyvars d
            L pos sig = extractRecSel name mdl n tyvar_names (tcdCons d)
        in L pos (SigD sig)
      _ -> error "internal: extractDecl"
  where
    name_and_tyvars d = (unLoc (tcdLName d), hsLTyVarLocNames (tcdTyVars d))

toTypeNoLoc :: Located Name -> LHsType Name
toTypeNoLoc lname = noLoc (HsTyVar (unLoc lname))

rmLoc :: Located a -> Located a
rmLoc a = noLoc (unLoc a)

extractClassDecl :: Name -> Module -> [Located Name] -> LSig Name -> LSig Name
extractClassDecl c mdl tvs0 (L pos (TypeSig lname ltype)) = case ltype of
  L _ (HsForAllTy exp tvs (L _ preds) ty) -> 
    L pos (TypeSig lname (noLoc (HsForAllTy exp tvs (lctxt preds) ty)))
  _ -> L pos (TypeSig lname (noLoc (mkImplicitHsForAllTy (lctxt []) ltype)))
  where
    lctxt preds = noLoc (ctxt preds)
    ctxt preds = [noLoc (HsClassP c (map toTypeNoLoc tvs0))] ++ preds  

extractClassDecl _ _ _ d = error $ "extractClassDecl: unexpected decl"

extractRecSel :: Name -> Module -> Name -> [Located Name] -> [LConDecl Name]
              -> LSig Name
extractRecSel _ _ _ _ [] = error "extractRecSel: selector not found"

extractRecSel nm mdl t tvs (L _ con : rest) =
  case con_details con of
    RecCon fields | (HsRecField n ty _ : _) <- matching_fields fields -> 
      L (getLoc n) (TypeSig (noLoc nm) (noLoc (HsFunTy data_ty (getBangType ty))))
    _ -> extractRecSel nm mdl t tvs rest
 where 
  matching_fields flds = [ f | f@(HsRecField n _ _) <- flds, (unLoc n) == nm ]   
  data_ty = foldl (\x y -> noLoc (HsAppTy x y)) (noLoc (HsTyVar t)) (map toTypeNoLoc tvs)

-- -----------------------------------------------------------------------------
-- Pruning

pruneExportItems :: [ExportItem Name] -> [ExportItem Name]
pruneExportItems items = filter hasDoc items
  where hasDoc (ExportDecl _ _ d _) = isJust d
	hasDoc _ = True

-- -----------------------------------------------------------------------------
-- Gather a list of original names exported from this module

mkVisibleNames :: Module 
             -> ModuleMap  
             -> [Name] 
             -> [Name]
             -> Map Name [Name]
             -> Maybe [IE Name]
             -> [DocOption]
             -> Map Name (LHsDecl Name)
             -> ErrMsgM [Name]

mkVisibleNames mdl modMap localNames scope subMap maybeExps opts declMap 
  -- if no export list, just return all local names 
  | Nothing <- maybeExps         = return (filter hasDecl localNames)
  | DocOptIgnoreExports `elem` opts = return localNames
  | Just expspecs <- maybeExps = do
      visibleNames <- mapM extract expspecs
      return $ filter isNotPackageName (concat visibleNames)
 where
  hasDecl name = isJust (Map.lookup name declMap)
  isNotPackageName name = nameMod == mdl || isJust (Map.lookup nameMod modMap)
    where nameMod = nameModule name

  extract e = 
   case e of
    IEVar x -> return [x]
    IEThingAbs t -> return [t]
    IEThingAll t -> return (t : all_subs)
	 where
	      all_subs | nameModule t == mdl = Map.findWithDefault [] t subMap
		       | otherwise = allSubsOfName modMap t

    IEThingWith t cs -> return (t : cs)
	
    IEModuleContents m
	| mkModule (modulePackageId mdl) m == mdl -> return localNames 
	| otherwise -> let m' = mkModule (modulePackageId mdl) m in
	  case Map.lookup m' modMap of
	    Just mod
		| DocOptHide `elem` hmod_options mod ->
		    return (filter (`elem` scope) (hmod_exports mod))
		| otherwise -> return []
	    Nothing
		-> tell (exportModuleMissingErr mdl m') >> return []
  
    _ -> return []

exportModuleMissingErr this mdl 
  = ["Warning: in export list of " ++ show (moduleString this)
	 ++ ": module not found: " ++ show (moduleString mdl)]

-- for a given entity, find all the names it "owns" (ie. all the
-- constructors and field names of a tycon, or all the methods of a
-- class).
allSubsOfName :: ModuleMap -> Name -> [Name]
allSubsOfName mod_map name 
  | isExternalName name =
    case Map.lookup (nameModule name) mod_map of
      Just hmod -> Map.findWithDefault [] name (hmod_sub_map hmod)
      Nothing   -> []
  | otherwise =  error $ "Main.allSubsOfName: unexpected unqual'd name"

-- | Build a mapping which for each original name, points to the "best"
-- place to link to in the documentation.  For the definition of
-- "best", we use "the module nearest the bottom of the dependency
-- graph which exports this name", not including hidden modules.  When
-- there are multiple choices, we pick a random one.
-- 
-- The interfaces are passed in in topologically sorted order, but we start
-- by reversing the list so we can do a foldl.
buildGlobalDocEnv :: [HaddockModule] -> Map Name Name
buildGlobalDocEnv modules
 = foldl upd Map.empty (reverse modules)
 where
  upd old_env mod
     | DocOptHide `elem` hmod_options mod
     = old_env
     | DocOptNotHome `elem` hmod_options mod
     = foldl' keep_old old_env exported_names
     | otherwise
     = foldl' keep_new old_env exported_names
     where
	exported_names = hmod_visible_exports mod
        modName = hmod_mod mod

	keep_old env n = Map.insertWith (\new old -> old) 
			 n (nameSetMod n modName) env
	keep_new env n = Map.insert n (nameSetMod n modName) env 

nameSetMod n newMod = mkExternalName (nameUnique n) newMod (nameOccName n)
                      (nameSrcLoc n)

-- -----------------------------------------------------------------------------
-- Named documentation

findNamedDoc :: String -> [Entity] -> ErrMsgM (Maybe (HsDoc Name))
findNamedDoc name entities = search entities 
	where search [] = do
		tell ["Cannot find documentation for: $" ++ name]
		return Nothing
	      search ((DocEntity (DocCommentNamed name' doc)):rest) 
			| name == name' = return (Just doc)
		   	| otherwise = search rest
	      search (_other_decl : rest) = search rest

-- simplified type for sorting types, ignoring qualification (not visible
-- in Haddock output) and unifying special tycons with normal ones.
data SimpleType = SimpleType Name [SimpleType] deriving (Eq,Ord)

attachInstances :: [HaddockModule] -> [HaddockModule]
attachInstances modules = map attach modules
  where
    instMap = fmap (map toHsInstHead . sortImage instHead) $ collectInstances modules
    attach mod = mod { hmod_export_items = newItems }
      where
        newItems = map attachExport (hmod_export_items mod)

        attachExport (ExportDecl n decl doc _) =
          ExportDecl n decl doc (case Map.lookup n instMap of
                                   Nothing -> []
                                   Just instheads -> instheads)
        attachExport otherExport = otherExport

collectInstances
   :: [HaddockModule]
   -> Map Name [([TyVar], [PredType], Class, [Type])]  -- maps class/type names to instances

collectInstances modules
  = Map.fromListWith (flip (++)) tyInstPairs `Map.union`
    Map.fromListWith (flip (++)) classInstPairs
  where
    allInstances = concat (map hmod_instances modules)
    classInstPairs = [ (is_cls inst, [instanceHead inst]) | 
                       inst <- allInstances ]
    tyInstPairs = [ (tycon, [instanceHead inst]) | inst <- allInstances, 
                    Just tycon <- nub (is_tcs inst) ]

instHead :: ([TyVar], [PredType], Class, [Type]) -> ([Int], Name, [SimpleType])
instHead (_, _, cls, args)
  = (map argCount args, className cls, map simplify args)
  where
    argCount (AppTy t _) = argCount t + 1
    argCount (TyConApp _ ts) = length ts
    argCount (FunTy _ _ ) = 2
    argCount (ForAllTy _ t) = argCount t
    argCount (NoteTy _ t) = argCount t
    argCount _ = 0

    simplify (ForAllTy _ t) = simplify t
    simplify (FunTy t1 t2) = 
      SimpleType funTyConName [simplify t1, simplify t2]
    simplify (AppTy t1 t2) = SimpleType s (args ++ [simplify t2])
      where (SimpleType s args) = simplify t1
    simplify (TyVarTy v) = SimpleType (tyVarName v) []
    simplify (TyConApp tc ts) = SimpleType (tyConName tc) (map simplify ts)
    simplify (NoteTy _ t) = simplify t
    simplify _ = error "simplify"

-- sortImage f = sortBy (\x y -> compare (f x) (f y))
sortImage :: Ord b => (a -> b) -> [a] -> [a]
sortImage f xs = map snd $ sortBy cmp_fst [(f x, x) | x <- xs]
 where cmp_fst (x,_) (y,_) = compare x y

funTyConName = mkWiredInName gHC_PRIM
                        (mkOccNameFS tcName FSLIT("(->)"))
                        funTyConKey
                        (ATyCon funTyCon)       -- Relevant TyCon
                        BuiltInSyntax


toHsInstHead :: ([TyVar], [PredType], Class, [Type]) -> InstHead Name
toHsInstHead (_, preds, cls, ts) = (map toHsPred preds, className cls, map toHsType ts) 

--------------------------------------------------------------------------------
-- Type -> HsType conversion
--------------------------------------------------------------------------------

toHsPred :: PredType -> HsPred Name 
toHsPred (ClassP cls ts) = HsClassP (className cls) (map toLHsType ts)
toHsPred (IParam n t) = HsIParam n (toLHsType t)

toLHsType = noLoc . toHsType
 
toHsType :: Type -> HsType Name
toHsType t = case t of 
  TyVarTy v -> HsTyVar (tyVarName v) 
  AppTy a b -> HsAppTy (toLHsType a) (toLHsType b)
  TyConApp tc ts -> case ts of 
    [] -> HsTyVar (tyConName tc)
    _  -> app (tycon tc) ts
  FunTy a b -> HsFunTy (toLHsType a) (toLHsType b) 
  ForAllTy v t -> cvForAll [v] t 
  PredTy p -> HsPredTy (toHsPred p) 
  NoteTy _ t -> toHsType t
  where
    tycon tc = HsTyVar (tyConName tc)
    app tc ts = foldl (\a b -> HsAppTy (noLoc a) (noLoc b)) tc (map toHsType ts)
    cvForAll vs (ForAllTy v t) = cvForAll (v:vs) t
    cvForAll vs t = mkExplicitHsForAllTy (tyvarbinders vs) (noLoc []) (toLHsType t)
    tyvarbinders vs = map (noLoc . UserTyVar . tyVarName) vs

-- -----------------------------------------------------------------------------
-- A monad which collects error messages

type ErrMsg = String
type ErrMsgM a = Writer [ErrMsg] a

--------------------------------------------------------------------------------
-- Packages 
--------------------------------------------------------------------------------

type DocEnv = Map Name Name

data PackageData = PackageData {
  pdModules  :: [Module],
  pdDocEnv   :: DocEnv,
  pdHtmlPath :: FilePath
}

data HaddockException = HaddockException String deriving Typeable
throwE str = throwDyn (HaddockException str)  

-- | Recreate exposed modules from an InstalledPackageInfo
packageModules :: InstalledPackageInfo -> [Module]
packageModules pkgInfo = map (mkModule (pkgId pkgInfo)) moduleNames
  where moduleNames = map mkModuleName (exposedModules pkgInfo)

pkgId :: InstalledPackageInfo -> PackageId
pkgId = mkPackageId . package 

{-
-- | Topologically sort a list of modules that belong to an external package,
-- using the dependency information available in the ModIface structure for 
-- each module. 
sortPackageModules :: [ModuleInfo] -> [ModuleInfo]
sortPackageModules modinfos = flattenSCCs $ stronglyConnComp nodes
  where 
    nodes = map mkNode modinfos
      where
       mkNode modinfo = let iface = minf_iface modinfo
                            modNames = (map fst . dep_mods . mi_deps) iface
                            modName  = moduleName (mi_module iface)
                        in (modinfo, modName, modNames)
-}

-- | For each module in the list, try to retrieve a ModuleInfo structure  
moduleInfo :: Session -> [Module] -> IO (Maybe [ModuleInfo])
moduleInfo session modules = do
  mbModInfo <- mapM (getModuleInfo session) modules
  return (sequence mbModInfo)

-- | Get the Haddock HTML directory path for a package
getHtml :: InstalledPackageInfo -> IO FilePath
getHtml pkgInfo = case haddockHTMLs pkgInfo of 
  (path:_) | not (null path) -> do
    dirExists <- doesDirectoryExist path
    if dirExists then return path else throwE $
       "HTML directory " ++ path ++ " does not exist."
  _ -> throwE "No Haddock documentation installed."

-- | Get the Haddock interface path for a package
getIface :: InstalledPackageInfo -> IO FilePath
getIface pkgInfo = case haddockInterfaces pkgInfo of
  (path:_) | not (null path) -> do
    dirExists <- doesDirectoryExist path
    if dirExists then return path else throwE $
       "Interface directory " ++ path ++ " does not exist."
  _ -> throwE "No Haddock interface installed."

-- | Try to create a PackageData structure for a package
getPackage :: Session -> InstalledPackageInfo -> IO PackageData 
getPackage session pkgInfo = do
  html <- getHtml pkgInfo
  iface <- getIface pkgInfo
  docEnv <- readPackageFile iface

  let modules = packageModules pkgInfo

  -- try to get a ModuleInfo struct for each module
  mbModInfos <- moduleInfo session modules
  modInfos <- case mbModInfos of 
    Just x -> return x
    Nothing -> throwE "Could not get ModuleInfo for all exposed modules." 

  --let modInfos' = sortPackageModules modInfos

  return $ PackageData {
    pdModules  = modules,
    pdDocEnv   = docEnv,
    pdHtmlPath = html
  } 

-- | Build a package doc env out of a topologically sorted list of modules
{-packageDocEnv :: [ModuleInfo] -> Map Name Name
packageDocEnv modInfos = foldl addModuleEnv Map.empty (reverse modInfos)
  where
    addModuleEnv oldEnv thisMod 
      | "GHC" `isPrefixOf` modStr = oldEnv 
      | DocOptHide `elem` options = oldEnv
      | DocOptNotHome `elem` options = foldl' keepOld oldEnv visibleNames
      | otherwise = foldl' keepNew oldEnv visibleNames
      where 
        modStr = moduleNameString (modInfoName thisMod)
        options = mi_docopts $ minf_iface thisMod
        visibleNames = modInfoExports thisMod
        modName = modInfoMod thisMod
        keepOld env n = Map.insertWith (\new old -> old) n 
                        (nameSetMod n modName) env
        keepNew env n = Map.insert n (nameSetMod n modName) env
-}
        
-- | Try to create a PackageData for each package in the session except for 
-- rts. Print a warning on stdout if a PackageData could not be created.
getPackages :: Session -> DynFlags -> [Flag] -> IO [PackageData]
getPackages session dynflags flags = do

  -- get InstalledPackageInfos for every package in the session
  pkgInfos <- getPreloadPackagesAnd dynflags []

  -- return a list of those packages that we could create PackageDatas for 
  let pkgInfos' = filter notRTS pkgInfos
  liftM catMaybes $ mapM tryGetPackage pkgInfos'

  where
    -- no better way to do this?
    notRTS p = pkgName (package p) /= packageIdString rtsPackageId  

    -- try to get a PackageData, warn if we can't
    tryGetPackage pkgInfo = 
        (getPackage session pkgInfo >>= return . Just)
      `catchDyn`
        (\(HaddockException e) -> do 
          let pkgName = showPackageId (package pkgInfo)
          putStrLn ("Warning: Cannot use package " ++ pkgName ++ ":")
          putStrLn ("   " ++ e)
          return Nothing
        )

-- | Build one big doc env out of a list of packages. If multiple packages 
-- export the same (original) name, we just pick one of the packages as the 
-- documentation site.
packagesDocEnv :: [PackageData] -> DocEnv
packagesDocEnv packages = Map.unions (map pdDocEnv packages)

--------------------------------------------------------------------------------
-- Package/Interface files 
--------------------------------------------------------------------------------

packageFileMagic = 0xDA303001 :: Word32
packageFileVersion = 0 :: Word16

writePackageFile :: FilePath -> DocEnv -> IO ()
writePackageFile filename pkgEnv = do
  h <- openBinaryFile filename WriteMode   
  bh <- openBinIO h 

  ud <- newWriteState
  bh <- return $ setUserData bh ud

  put_ bh packageFileMagic
  put_ bh packageFileVersion
  put_ bh (Map.toList pkgEnv)
  hClose h

readPackageFile :: FilePath -> IO DocEnv
readPackageFile filename = do
  h <- openBinaryFile filename ReadMode
  bh <- openBinIO h

  ud <- newReadState undefined
  bh <- return (setUserData bh ud)

  magic <- get bh
  when (magic /= packageFileMagic) $ throwE $
    "Magic number mismatch: couldn't load interface file: " ++ filename
 
  (version :: Word16) <- get bh
  envList <- get bh
  return (Map.fromList envList)
