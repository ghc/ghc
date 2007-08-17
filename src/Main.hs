--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
-- 
-- Ported to use the GHC API by David Waern during "Summer of Code" 2006
--


module Main (main) where


import Haddock.Html
import Haddock.Hoogle
import Haddock.Rename
import Haddock.Types hiding (NoLink)
import Haddock.Utils
import Haddock.Version
import Haddock.InterfaceFile
import Haddock.Exception
import Haddock.Options
import Haddock.Typecheck
import Haddock.Utils.GHC
import Paths_haddock


import Prelude hiding (catch)
import Control.Exception     
import Control.Monad
import Control.Monad.Writer
import Control.Arrow
import Data.Char
import Data.IORef
import Data.Ord
import Data.List
import Data.Maybe
import Data.Typeable
import Data.Graph hiding (flattenSCC)
import Data.Dynamic
import Data.Foldable (foldlM)
import System.Console.GetOpt 
import System.Environment
import System.Directory
import System.FilePath
import System.Cmd
import System.Exit           
import System.IO

import qualified Data.Map as Map
import Data.Map (Map)

import Distribution.InstalledPackageInfo
import Distribution.Simple.Utils


import GHC
import Outputable
import SrcLoc
import Name
import Module
import InstEnv
import Class
import TypeRep
import Var hiding (varName)
import TyCon
import PrelNames
import Bag
import HscTypes
import Util (handleDyn)
import ErrUtils (printBagOfErrors)
import UniqFM

import FastString
#define FSLIT(x) (mkFastString# (x#))

import DynFlags hiding (Option)
import Packages hiding (package) 
import StaticFlags


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
-- Top-level
-------------------------------------------------------------------------------


main :: IO ()
main = handleTopExceptions $ do

  -- parse command-line flags and handle some of them initially
  args <- getArgs
  (flags, fileArgs) <- parseHaddockOpts args
  libDir <- handleFlags flags fileArgs
  
  -- initialize GHC 
  restGhcFlags <- tryParseStaticFlags flags
  (session, _) <- startGHC libDir

  -- get the -use-package packages, and expose them to ghc
  usePackages <- getUsePackages flags session

  -- parse and set the ghc flags
  dynflags <- parseGhcFlags session restGhcFlags
  setSessionDynFlags session dynflags

  -- init and get the package dependencies 
  (_, depPackages) <- initPackages dynflags
  let depPkgs = map (fromJust . unpackPackageId) depPackages

  -- compute the exposed packages
  let exposedPackages = [ mkPackageId pkg | pkg <- depPkgs, 
                          pkgName pkg `elem` usePackages ]

  -- get the HaddockPackages
  packages <- getPackages session exposedPackages

  -- typechecking
  modules  <- typecheckFiles session fileArgs

  -- update the html references for rendering phase (global variable)
  updateHTMLXRefs packages

  -- combine the doc envs of the exposed packages into one
  let env = packagesDocEnv packages

  -- TODO: continue to break up the run function into parts
  run flags modules env


startGHC :: String -> IO (Session, DynFlags)
startGHC libDir = do
  session <- newSession (Just libDir)
  flags   <- getSessionDynFlags session
  let flags' = dopt_set flags Opt_Haddock
  let flags'' = flags' {
      hscTarget = HscNothing,
      ghcMode   = CompManager,
      ghcLink   = NoLink
    }
  setSessionDynFlags session flags''
  return (session, flags'')

 
run :: [Flag] -> [GhcModule] -> Map Name Name -> IO ()
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
    -- run pass 1 on this data
    (modMap, messages) = runWriter (pass1 modules flags) 

    haddockMods = catMaybes [ Map.lookup (ghcModule m) modMap | m <- modules ]
    homeEnv = buildGlobalDocEnv haddockMods
    env = homeEnv `Map.union` extEnv
    haddockMods' = attachInstances haddockMods
    (haddockMods'', messages') = runWriter $ mapM (renameModule env) haddockMods'
  
  mapM_ putStrLn messages
  mapM_ putStrLn messages'

  let 
    visibleMods = [ m | m <- haddockMods'', OptHide `notElem` (hmod_options m) ]
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

  let iface = InterfaceFile {
        ifDocEnv  = homeEnv
--        ifModules = map hmod2interface visibleMods
      }

  case [str | Flag_DumpInterface str <- flags] of
        [] -> return ()
        fs -> let filename = (last fs) in 
              writeInterfaceFile filename iface


-------------------------------------------------------------------------------
-- Flags 
-------------------------------------------------------------------------------


handleFlags flags fileArgs = do
  usage <- getUsage

  when (Flag_Help    `elem` flags) (bye usage)
  when (Flag_Version `elem` flags) byeVersion
  when (null fileArgs) (bye usage)

  let ghcLibDir = case [ dir | Flag_GhcLibDir dir <- flags ] of
                    [] -> throwE "no GHC lib dir specified"
                    xs -> last xs

  when ((Flag_GenIndex `elem` flags || Flag_GenContents `elem` flags)
        && Flag_Html `elem` flags) $
    throwE ("-h cannot be used with --gen-index or --gen-contents")

  return ghcLibDir


-- | Handle the -use-package flags
-- 
-- Returns the names of the packages (without version number), if parsing
-- succeeded.
--
-- It would be better to try to get the "exposed" packages from GHC instead.
-- This would make the -use-package flag unnecessary. But currently it 
-- seems all you can get from the GHC api is all packages that are linked in 
-- (i.e the closure of the exposed packages).
getUsePackages :: [Flag] -> Session -> IO [String]
getUsePackages flags session = do

  -- get the packages from the commandline flags
  let packages = [ pkg | Flag_UsePackage pkg <- flags ]

  -- expose these packages 
  -- (makes "-use-package pkg" equal to "-g '-package pkg'")

  dfs <- getSessionDynFlags session
  let dfs' = dfs { packageFlags = packageFlags dfs ++ map ExposePackage packages }
  setSessionDynFlags session dfs'

  -- try to parse these packages into PackageIndentifiers

  mapM (handleParse . unpackPackageId . stringToPackageId) packages
  where
    handleParse (Just pkg) = return (pkgName pkg)
    handleParse Nothing = throwE "Could not parse package identifier"


-- | Filter out the GHC specific flags and try to parse and set them as static 
-- flags. Return a list of flags that couldn't be parsed. 
tryParseStaticFlags flags = do
  let ghcFlags = [ str | Flag_GhcFlag str <- flags ]
  parseStaticFlags ghcFlags


-- | Try to parse dynamic GHC flags
parseGhcFlags session ghcFlags = do
  dflags <- getSessionDynFlags session
  foldlM parseFlag dflags (map words ghcFlags)
  where 
    -- try to parse a flag as either a dynamic or static GHC flag
    parseFlag dynflags ghcFlag = do
      (dynflags', rest) <- parseDynamicFlags dynflags ghcFlag
      when (rest == ghcFlag) $
          throwE ("Couldn't parse GHC flag: " ++ (unwords ghcFlag))           
      return dynflags'

 
byeVersion = 
  bye ("Haddock version " ++ projectVersion ++ 
       ", (c) Simon Marlow 2003; ported to the GHC-API by David Waern 2006\n")


-------------------------------------------------------------------------------
-- Phase 1
-------------------------------------------------------------------------------


-- | Produce a map of HaddockModules with information that is close to 
-- renderable.  What is lacking after this pass are the renamed export items.
pass1 :: [GhcModule] -> [Flag] -> ErrMsgM ModuleMap
pass1 modules flags = foldM produceAndInsert Map.empty modules
  where
    produceAndInsert modMap modData = do
      resultMod <- pass1data modData flags modMap
      let key = ghcModule modData
      return (Map.insert key resultMod modMap)


-- | Massage the data in GhcModule to produce something closer to what
-- we want to render. To do this, we need access to modules before this one
-- in the topological sort, to which we have already done this conversion. 
-- That's what's in the ModuleMap.
pass1data :: GhcModule -> [Flag] -> ModuleMap -> ErrMsgM HaddockModule
pass1data modData flags modMap = do

  let mod = ghcModule modData

  opts <- mkDocOpts (ghcMbDocOpts modData) mod

  let group        = ghcGroup modData
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
      | OptPrune `elem` opts = pruneExportItems exportItems
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
    mkDocOpts mbOpts mod = do
      opts <- case mbOpts of 
        Just opts -> processOptions opts
        Nothing -> return []
      let opts' = if Flag_HideModule (moduleString mod) `elem` flags 
            then OptHide : opts
            else opts      
      return opts'


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
                        ConDeclField lname _ (Just (L _ doc)) <- fields ]


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


--------------------------------------------------------------------------------
-- Collect docs
--------------------------------------------------------------------------------


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


-------------------------------------------------------------------------------
-- 
-------------------------------------------------------------------------------

       
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

	
updateHTMLXRefs :: [HaddockPackage] -> IO ()
updateHTMLXRefs packages = do
  writeIORef html_xrefs_ref (Map.fromList mapping)
  where
    mapping = [ (mod, html) | 
                (HaddockPackage mods _ html) <- packages, mod <- mods ] 


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


-------------------------------------------------------------------------------
-- Phase 2
-------------------------------------------------------------------------------


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
    when (OptHide `notElem` hmod_options mod && not (null strings)) $
	  tell ["Warning: " ++ show (ppr (hmod_mod mod) defaultUserStyle) ++ 
		": could not find link destinations for:\n"++
		"   " ++ concat (map (' ':) strings) ]

    return $ mod { hmod_rn_doc = finalModuleDoc,
                   hmod_rn_doc_map = rnDocMap,
                   hmod_rn_export_items = renamedExportItems }


-- | Build the list of items that will become the documentation, from the
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
  | isNothing maybe_exps || ignore_all_exports || OptIgnoreExports `elem` opts
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
		| OptHide `elem` hmod_options hmod
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
    mkExportItem _ = Nothing


-- | Sometimes the declaration we want to export is not the "main" declaration:
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
    RecCon fields | (ConDeclField n ty _ : _) <- matching_fields fields -> 
      L (getLoc n) (TypeSig (noLoc nm) (noLoc (HsFunTy data_ty (getBangType ty))))
    _ -> extractRecSel nm mdl t tvs rest
 where 
  matching_fields flds = [ f | f@(ConDeclField n _ _) <- flds, (unLoc n) == nm ]   
  data_ty = foldl (\x y -> noLoc (HsAppTy x y)) (noLoc (HsTyVar t)) (map toTypeNoLoc tvs)


-- Pruning
pruneExportItems :: [ExportItem Name] -> [ExportItem Name]
pruneExportItems items = filter hasDoc items
  where hasDoc (ExportDecl _ _ d _) = isJust d
	hasDoc _ = True


-- | Gather a list of original names exported from this module
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
  | OptIgnoreExports `elem` opts = return localNames
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
		| OptHide `elem` hmod_options mod ->
		    return (filter (`elem` scope) (hmod_exports mod))
		| otherwise -> return []
	    Nothing
		-> tell (exportModuleMissingErr mdl m') >> return []
  
    _ -> return []


exportModuleMissingErr this mdl 
  = ["Warning: in export list of " ++ show (moduleString this)
	 ++ ": module not found: " ++ show (moduleString mdl)]


-- | For a given entity, find all the names it "owns" (ie. all the
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
     | OptHide `elem` hmod_options mod
     = old_env
     | OptNotHome `elem` hmod_options mod
     = foldl' keep_old old_env exported_names
     | otherwise
     = foldl' keep_new old_env exported_names
     where
	exported_names = hmod_visible_exports mod
        modName = hmod_mod mod

	keep_old env n = Map.insertWith (\new old -> old) 
			 n (nameSetMod n modName) env
	keep_new env n = Map.insert n (nameSetMod n modName) env 


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


-- Haddock options embedded in the source file

processOptions_ str = let (opts, msg) = runWriter (processOptions str) 
                      in print msg >> return opts 

processOptions :: String -> ErrMsgM [DocOption]
processOptions str = do
  case break (== ',') str of
    (this, ',':rest) -> do
	opt <- parseOption this
	opts <- processOptions rest
	return (maybeToList opt ++ opts)
    (this, _)
	| all isSpace this -> return []
	| otherwise -> do opt <- parseOption this; return (maybeToList opt)


parseOption :: String -> ErrMsgM (Maybe DocOption)
parseOption "hide" = return (Just OptHide)
parseOption "prune" = return (Just OptPrune)
parseOption "ignore-exports" = return (Just OptIgnoreExports)
parseOption "not-home" = return (Just OptNotHome)
parseOption other = do tell ["Unrecognised option: " ++ other]; return Nothing


-- | Simplified type for sorting types, ignoring qualification (not visible
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


-- A monad which collects error messages

type ErrMsg = String
type ErrMsgM a = Writer [ErrMsg] a


--------------------------------------------------------------------------------
-- Packages 
--------------------------------------------------------------------------------


-- | Represents the installed Haddock information of a package
data HaddockPackage = HaddockPackage {
  pdModules  :: [Module],
  pdDocEnv   :: DocEnv,
  pdHtmlPath :: FilePath
}


-- | Recreate exposed modules from an InstalledPackageInfo
packageModules :: InstalledPackageInfo -> [Module]
packageModules pkgInfo = map (mkModule (pkgId pkgInfo)) moduleNames
  where 
    moduleNames = map mkModuleName (exposedModules pkgInfo)
    pkgId = mkPackageId . package 


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
  (file:_) | not (null file) -> do
    fileExists <- doesFileExist file
    if fileExists then return file else throwE $
       "Interface file " ++ file ++ " does not exist."
  _ -> throwE "No Haddock interface installed."


-- | Try to create a HaddockPackage structure for a package
getPackage :: Session -> InstalledPackageInfo -> IO HaddockPackage
getPackage session pkgInfo = do

  html <- getHtml pkgInfo
  ifacePath <- getIface pkgInfo
  iface <- readInterfaceFile ifacePath
  
  let docEnv  = ifDocEnv iface
      modules = packageModules pkgInfo

  return $ HaddockPackage {
    pdModules  = modules,
    pdDocEnv   = docEnv,
    pdHtmlPath = html
  } 

       
-- | Try to create a HaddockPackage for each package.
-- Print a warning on stdout if a HaddockPackage could not be created.
getPackages :: Session -> [PackageId] -> IO [HaddockPackage]
getPackages session packages = do

  -- get InstalledPackageInfos for each package
  dynflags <- getSessionDynFlags session
  let pkgInfos = map (getPackageDetails (pkgState dynflags)) packages

  -- try to read the installed haddock information (.haddock interface file and
  -- html path) for the packages
  liftM catMaybes $ mapM tryGetPackage pkgInfos
  where
    -- try to get a HaddockPackage, warn if we can't
    tryGetPackage pkgInfo = 
        (getPackage session pkgInfo >>= return . Just)
      `catchDyn`
        (\(e::HaddockException) -> do 
          let pkgName = showPackageId (package pkgInfo)
          putStrLn ("Warning: Cannot use package " ++ pkgName ++ ":")
          putStrLn ("   " ++ show e)
          return Nothing
        )


-- | Build one big doc env out of a list of packages. If multiple packages 
-- export the same (original) name, we just pick one of the packages as the 
-- documentation site.
packagesDocEnv :: [HaddockPackage] -> DocEnv
packagesDocEnv packages = Map.unions (map pdDocEnv packages)
