--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

module Main (main) where

import HaddockVersion
import HaddockRename
import HaddockParse
import HaddockLex
--import HaddockDB   -- not compiling
import HaddockHtml
import HaddockTypes
import HaddockUtil
import Digraph
import Binary

import HsParser
import HsParseMonad
import HsSyn
import System

--import Pretty

import Maybe	( isJust, maybeToList )
import List	( nub )
import Monad	( when )
import Char	( isSpace )
import IO

#if __GLASGOW_HASKELL__ < 503
import MonadWriter
import FiniteMap
import GetOpt
import IOExts
#else
import Control.Monad.Writer
import Data.FiniteMap
import System.Console.GetOpt
import Data.IORef
import Debug.Trace
import System.IO.Unsafe	 ( unsafePerformIO )
#endif

#if __GLASGOW_HASKELL__ < 500
import Regex
import PackedString
#endif

-----------------------------------------------------------------------------
-- Top-level stuff
main :: IO ()
main = do
  cmdline <- getArgs
  case getOpt Permute options cmdline of
    (flags, args, []    ) -> run flags args
    (_,     _,    errors) -> do prog <- getProgramName
                                die (concat errors ++
                                     usageInfo (usageHeader prog) options)

usageHeader :: String -> String
usageHeader prog = "Usage: " ++ prog ++ " [OPTION...] file...\n"

data Flag
  = Flag_CSS String
  | Flag_Debug
--  | Flag_DocBook
  | Flag_DumpInterface FilePath
  | Flag_Heading String
  | Flag_Html
  | Flag_Lib String
  | Flag_MSHtmlHelp
  | Flag_NoImplicitPrelude
  | Flag_OutputDir FilePath
  | Flag_Prologue FilePath
  | Flag_ReadInterface FilePath
  | Flag_SourceURL String
  | Flag_Help
  | Flag_Version
  deriving (Eq)

options :: [OptDescr Flag]
options =
  [ 
    Option ['o']  ["odir"]     (ReqArg Flag_OutputDir "DIR")
	"directory in which to put the output files",
    Option ['i'] ["read-interface"] (ReqArg Flag_ReadInterface "FILE")
	"read an interface from FILE",
    Option ['D']  ["dump-interface"]   (ReqArg Flag_DumpInterface "FILE")
        "dump an interface for these modules in FILE",
    Option ['l']  ["lib"]         (ReqArg Flag_Lib "DIR") 
	"location of Haddock's auxiliary files",
--    Option ['S']  ["docbook"]  (NoArg Flag_DocBook)
--	"output in docbook (SGML)",
    Option ['h']  ["html"]     (NoArg Flag_Html)
	"output in HTML",
    Option ['m']  ["ms-help"]    (NoArg Flag_MSHtmlHelp)
	"produce Microsoft HTML Help files (with -h)",
    Option ['s']  ["source"]   (ReqArg Flag_SourceURL "URL") 
	"base URL for links to source code",
    Option ['c']  ["css"]         (ReqArg Flag_CSS "FILE") 
	"the CSS file to use for HTML output",
    Option ['p']  ["prologue"] (ReqArg Flag_Prologue "FILE")
	"file containing prologue text",
    Option ['t']  ["title"]    (ReqArg Flag_Heading "TITLE")
	"page heading",
    Option ['n']  ["no-implicit-prelude"] (NoArg Flag_NoImplicitPrelude)
 	"do not assume Prelude is imported",
    Option ['d']  ["debug"]  (NoArg Flag_Debug)
	"extra debugging output",
    Option ['?']  ["help"]  (NoArg Flag_Help)
	"display this help and exit",
    Option ['V']  ["version"]  (NoArg Flag_Version)
	"output version information and exit"
  ]

saved_flags :: IORef [Flag]
saved_flags = unsafePerformIO (newIORef (error "no flags yet"))

run :: [Flag] -> [FilePath] -> IO ()
run flags files = do
  when (Flag_Help `elem` flags) $ do
     prog <- getProgramName
     bye (usageInfo (usageHeader prog) options)

  when (Flag_Version `elem` flags) $
     bye ("Haddock version " ++ projectVersion ++ ", (c) Simon Marlow 2002\n")

  let title = case [str | Flag_Heading str <- flags] of
		[] -> ""
		(t:_) -> t

      source_url = case [str | Flag_SourceURL str <- flags] of
			[] -> Nothing
			(t:_) -> Just t

  libdir <- case [str | Flag_Lib str <- flags] of
		[] -> dieMsg "no --lib option"
		fs -> return (last fs)

  let css_file = case [str | Flag_CSS str <- flags] of
			[] -> Nothing
			fs -> Just (last fs)

  odir <- case [str | Flag_OutputDir str <- flags] of
		[] -> return "."
		fs -> return (last fs)

  let dump_iface = case [str | Flag_DumpInterface str <- flags] of
		  	[] -> Nothing
		  	fs -> Just (last fs)

      ifaces_to_read = [ parseIfaceOption str 
		       | Flag_ReadInterface str <- flags ]

      no_implicit_prelude = Flag_NoImplicitPrelude `elem` flags

  prologue <- getPrologue flags

  read_ifaces_s <- mapM readIface (map snd ifaces_to_read)

  updateHTMLXRefs (map fst ifaces_to_read) read_ifaces_s

  writeIORef saved_flags flags
  parsed_mods <- mapM parse_file files

  let read_ifaces = concat read_ifaces_s
      external_mods = map fst read_ifaces

  sorted_mod_files <- sortModules (zip parsed_mods files)
	-- emits an error message if there are recursive modules

  -- process the modules in sorted order, building up a mapping from
  -- modules to interfaces.
  let 
	loop ifaces [] = return ifaces
	loop ifaces ((hsmod,file):mdls)  = do 
	   let ((mdl,iface),msgs) = runWriter $
		   mkInterface no_implicit_prelude ifaces file hsmod
	       new_ifaces = addToFM ifaces mdl iface
	   mapM (hPutStrLn stderr) msgs
	   loop new_ifaces mdls

  module_map <- loop (listToFM read_ifaces) sorted_mod_files
  let mod_ifaces = fmToList module_map

      these_mod_ifaces0 = [ (mdl, iface) 
			  | (mdl, iface) <- mod_ifaces,
			    mdl `notElem` external_mods ]

--  when (Flag_DocBook `elem` flags) $
--    putStr (ppDocBook odir mod_ifaces)

  let these_mod_ifaces = attachInstances these_mod_ifaces0

  when (Flag_Debug `elem` flags) $ do
    mapM_ putStrLn (map show [ (mdl, fmToList (iface_env i), 
				     fmToList (iface_sub i))
			     | (mdl, i) <-  these_mod_ifaces ])

  when (Flag_Html `elem` flags) $
    ppHtml title source_url these_mod_ifaces odir css_file 
	libdir prologue (Flag_MSHtmlHelp `elem` flags)

  -- dump an interface if requested
  case dump_iface of
     Nothing -> return ()
     Just fn -> do
	bh <- openBinMem 100000
	put_ bh prepared_ifaces
	writeBinMem bh fn
      where
	prepared_ifaces = 
	  [ (mdl, fmToList (iface_env iface), fmToList (iface_sub iface))
	  | (mdl, iface) <- these_mod_ifaces ]

parseIfaceOption :: String -> (FilePath,FilePath)
parseIfaceOption s = 
  case break (==',') s of
	(fpath,',':file) -> (fpath,file)
	(file, _)        -> ("", file)

readIface :: FilePath -> IO [(Module,Interface)]
readIface filename = do
  bh <- readBinMem filename
  stuff <- get bh
  return (map to_interface stuff)
 where 
   to_interface (mdl, env, sub) = 
	  (mdl, Interface { 
		   iface_filename     = "",
		   iface_env          = listToFM env,
		   iface_import_env   = emptyFM,
		   iface_sub	      = listToFM sub,
		   iface_reexported   = emptyFM,
		   iface_exports      = [],
		   iface_orig_exports = [],
		   iface_insts	      = [],
		   iface_decls        = emptyFM,
		   iface_info	      = Nothing,
		   iface_doc          = Nothing,
		   iface_options      = []
		}
      	  )
		

updateHTMLXRefs :: [FilePath] -> [[(Module,Interface)]] -> IO ()
updateHTMLXRefs paths ifaces_s =
  writeIORef html_xrefs_ref (listToFM mapping)
 where
  mapping = [ (mdl,fpath) 
	    | (fpath, ifaces) <- zip paths ifaces_s,
	      (mdl, _iface) <- ifaces
	    ]

parse_file :: FilePath -> IO HsModule
parse_file file = do
  bracket 
    (openFile file ReadMode)
    (\h -> hClose h)
    (\h -> do stuff <- hGetContents h 
	      case parse stuff (SrcLoc 1 1) 1 0 [] of
	        Ok _ e -> return e
	        Failed err -> die (file ++ ':':err ++ "\n")
    )

getPrologue :: [Flag] -> IO (Maybe Doc)
getPrologue flags
  = case [filename | Flag_Prologue filename <- flags ] of
	[] -> return Nothing 
	[filename] -> do
	   str <- readFile filename
	   case parseParas (tokenise str) of
		Left err -> dieMsg err
		Right doc -> return (Just doc)
	_otherwise -> dieMsg "multiple -p/--prologue options"

-----------------------------------------------------------------------------
-- Figuring out the definitions that are exported from a module

mkInterface
   :: Bool				-- no implicit prelude
   -> ModuleMap -> FilePath -> HsModule
   -> ErrMsgM (
	       Module, 		-- the module name
	       Interface	-- its "interface"
	      )

mkInterface no_implicit_prelude mod_map filename 
	(HsModule mdl exps imps decls maybe_opts maybe_info maybe_doc) = do  

  -- Process the options, if available
  opts <- case maybe_opts of
		Just opt_str -> processOptions opt_str
		Nothing      -> return []

  let
     -- expand type signatures with multiple variables into multiple
     -- type signatures
     expanded_decls = concat (map expandDecl decls)

     sub_map = mkSubNames expanded_decls

     -- first, attach documentation to declarations
     annotated_decls = collectDoc expanded_decls

     -- now find the defined names
     locally_defined_names = collectNames annotated_decls

     qual_local_names   = map (Qual mdl) locally_defined_names
     unqual_local_names = map UnQual     locally_defined_names

     local_orig_env = listToFM (zip unqual_local_names qual_local_names ++
			        zip qual_local_names   qual_local_names)
	 -- both qualified and unqualifed names are in scope for local things

     implicit_imps
	| no_implicit_prelude || any is_prel_import imps = imps
	| otherwise = HsImportDecl loc prelude_mod False Nothing Nothing : imps
	where 
		loc = SrcLoc 0 0
	 	is_prel_import (HsImportDecl _ mdl0 _ _ _ ) = mdl0 == prelude_mod

     -- build the orig_env, which maps names to *original* names (so we can
     -- find the original declarations & docs for things).
     orig_env = buildOrigEnv mod_map implicit_imps `plusFM` local_orig_env

     -- convert names in source code to original, fully qualified, names
     (orig_exports, missing_names1) 
	= runRnFM orig_env (mapMaybeM renameExportList exps)

     (orig_decls, missing_names2)
	= runRnFM orig_env (mapM renameDecl annotated_decls)

     -- gather up a list of entities that are exported (original names)
     (exported_names, exported_visible_names)
	 = exportedNames mdl mod_map
			locally_defined_names orig_env sub_map
			orig_exports opts

     -- build the import env, which maps original names to import names
     local_import_env = listToFM (zip qual_local_names qual_local_names)
     import_env = local_import_env `plusFM`
		   buildImportEnv mod_map mdl exported_visible_names 
			implicit_imps

--   trace (show (fmToList orig_env)) $ do
--  trace (show (fmToList import_env)) $ do
  let
     final_decls = orig_decls

     decl_map :: FiniteMap HsName HsDecl
     decl_map = listToFM [ (n,d) | d <- final_decls, n <- declBinders d ]

     instances = [ d | d@HsInstDecl{} <- final_decls ] ++
		 [ d | decl <- orig_decls, d <- derivedInstances mdl decl]

  -- make the "export items", which will be converted into docs later
  orig_export_list <- mkExportItems mod_map mdl orig_env decl_map sub_map
			 		final_decls opts orig_exports

  let
     -- prune the export list to just those declarations that have
     -- documentation, if the 'prune' option is on.
     pruned_export_list
	| OptPrune `elem` opts = pruneExportItems orig_export_list
	| otherwise = orig_export_list

     -- rename names in the exported declarations to point to things that
     -- are closer, or maybe even exported by, the current module.
     (renamed_export_list, _missing_names3)
        = runRnFM import_env (renameExportItems pruned_export_list)

     name_env = listToFM [ (nameOfQName n, n) | n <- exported_names ]

     -- find the names exported by this module that other modules should *not*
     -- link to (and point them to where they should).
     reexports = getReExports mdl mod_map orig_exports

  let
     (orig_module_doc, missing_names4)
        = runRnFM orig_env (renameMaybeDoc maybe_doc)

     (final_module_doc, _missing_names5)
        = runRnFM import_env (renameMaybeDoc orig_module_doc)

  -- report any names we couldn't find/resolve

  let missing_names = missing_names1 ++ missing_names2 ++ missing_names4
			 --ignore missing_names3 & missing_names5 for now
      name_strings = nub (map show missing_names)

  when (not (null name_strings)) $
	  tell ["Warning: " ++ show mdl ++ 
		": the following names could not be resolved:\n\ 
		\   " ++ concat (map (' ':) name_strings)
		]

  return (mdl, Interface { 
		   iface_filename     = filename,
		   iface_env          = name_env,
		   iface_import_env   = import_env,
		   iface_reexported   = reexports,
		   iface_exports      = renamed_export_list,
		   iface_sub	      = sub_map,
		   iface_orig_exports = pruned_export_list,
		   iface_insts	      = instances,
		   iface_decls        = decl_map,
		   iface_info	      = maybe_info,
		   iface_doc          = final_module_doc,
		   iface_options      = opts
		}
      	  )

-- Try to generate instance declarations for derived instances.
-- We can't do this properly without instance inference, but if a type
-- variable occurs as a constructor argument, then we can just
-- propagate the derived class to the variable.  But we know nothing of
-- the constraints on any type variables that occur elsewhere.
-- Note that a type variable may be in both categories: then we know a
-- constraint, but there may be more, or a stronger constraint.
derivedInstances :: Module -> HsDecl -> [HsDecl]
derivedInstances mdl decl = case decl of
   HsDataDecl srcloc ctxt n tvs cons drv@(_:_) _ ->
      derived srcloc ctxt n tvs cons drv
   HsNewTypeDecl srcloc ctxt n tvs con drv@(_:_) _ ->
      derived srcloc ctxt n tvs [con] drv
   _ -> []
 where
  derived srcloc ctxt n tvs cons drv =
     [HsInstDecl srcloc
		 (ctxt ++ [(cls,[v]) | v <- simple_tvars] ++ extra_constraint)
		 (cls,[t]) [] |
	cls <- drv]
   where
     tvar_map = fmToList $ unionMaps (map tvarsConstr cons)
     simple_tvars = [HsTyVar v | (v,(in_constr,_)) <- tvar_map, in_constr]
     complex_tvars = [HsTyVar v | (v,(_,in_tycons)) <- tvar_map, in_tycons]
     extra_constraint
	| null complex_tvars = []
	| otherwise = [(unknownConstraint,complex_tvars)]
     t  | n == tuple_tycon_name (length tvs - 1) =
	   HsTyTuple True (map HsTyVar tvs)
        | otherwise = foldl HsTyApp (HsTyCon (Qual mdl n)) (map HsTyVar tvs)

  -- collect the type variables occurring free in a constr
  tvarsConstr :: HsConDecl -> FiniteMap HsName (Bool,Bool)
  -- first Bool: tvar occurs as a data constructor argument
  -- second Bool: tvar occurs as a type constructor argument
  tvarsConstr (HsConDecl _ _ vs _ bts _) =
     unionMaps (map tvarsBangType bts) `delListFromFM` vs
  tvarsConstr (HsRecDecl _ _ vs _ fs _) =
     unionMaps (map tvarsField fs) `delListFromFM` vs

  tvarsField (HsFieldDecl _ bt _) = tvarsBangType bt

  tvarsBangType (HsBangedTy t) = tvarsType t
  tvarsBangType (HsUnBangedTy t) = tvarsType t

  tvarsType (HsTyTuple _ ts) = unionMaps (map tvarsType ts)
  tvarsType (HsTyVar tv) = unitFM tv (True,False)
  tvarsType (HsTyDoc t _) = tvarsType t
  tvarsType t = tvarsType2 t

  tvarsType2 (HsForAllType (Just tvs) _ t) = tvarsType2 t `delListFromFM` tvs
  tvarsType2 (HsForAllType Nothing _ t) = tvarsType2 t
  tvarsType2 (HsTyFun t1 t2) = tvarsType2 t1 `unionMap` tvarsType2 t2
  tvarsType2 (HsTyTuple _ ts) = unionMaps (map tvarsType2 ts)
  tvarsType2 (HsTyApp t1 t2) = tvarsType2 t1 `unionMap` tvarsType2 t2
  tvarsType2 (HsTyVar tv) = unitFM tv (False,True)
  tvarsType2 (HsTyCon _) = emptyFM
  tvarsType2 (HsTyDoc t _) = tvarsType2 t

  unionMaps :: [FiniteMap HsName (Bool,Bool)] -> FiniteMap HsName (Bool,Bool)
  unionMaps = foldr unionMap emptyFM

  unionMap :: FiniteMap HsName (Bool,Bool) -> FiniteMap HsName (Bool,Bool) ->
	FiniteMap HsName (Bool,Bool)
  unionMap = plusFM_C or2

  or2 :: (Bool,Bool) -> (Bool,Bool) -> (Bool,Bool)
  or2 (a1,b1) (a2,b2) = (a1 || a2, b1 || b2)

unknownConstraint :: HsQName
unknownConstraint = UnQual (HsTyClsName (HsIdent "???"))

-- -----------------------------------------------------------------------------
-- Build the list of items that will become the documentation, from the
-- export list.  At this point, the list of ExportItems is in terms of
-- original names.

mkExportItems
	:: ModuleMap
	-> Module			-- this module
	-> FiniteMap HsQName HsQName	-- the orig env
	-> FiniteMap HsName HsDecl	-- maps local names to declarations
	-> FiniteMap HsName [HsName]	-- sub-map for this module
	-> [HsDecl]			-- decls in the current module
	-> [DocOption]
	-> Maybe [HsExportSpec]
	-> ErrMsgM [ExportItem]

mkExportItems mod_map this_mod orig_env decl_map sub_map decls
	 opts maybe_exps
  | Nothing <- maybe_exps	 = everything_local_exported
  | OptIgnoreExports `elem` opts = everything_local_exported
  | Just specs <- maybe_exps     = do 
	exps <- mapM lookupExport specs
        return (concat exps)
  where
    everything_local_exported =  -- everything exported
	return (fullContentsOfThisModule this_mod decls)

    lookupExport (HsEVar x)            = declWith x (Just [])
    lookupExport (HsEAbs t)            = declWith t (Just [])
    lookupExport (HsEThingAll t)       = declWith t Nothing
    lookupExport (HsEThingWith t cs)   = declWith t (Just cs)
    lookupExport (HsEModuleContents m) = fullContentsOf m
    lookupExport (HsEGroup lev doc)    = return [ ExportGroup lev "" doc ]
    lookupExport (HsEDoc doc)          = return [ ExportDoc doc ]
    lookupExport (HsEDocNamed str)
	= do r <- findNamedDoc str decls
	     case r of
		Nothing -> return []
		Just found -> return [ ExportDoc found ]
	
    in_scope = eltsFM orig_env

    declWith :: HsQName -> Maybe [HsQName] -> ErrMsgM [ ExportItem ]
    declWith (UnQual _)     _ = return []
    declWith t@(Qual mdl x) mb_subs
	| Just decl <- findDecl t
	= return [ ExportDecl t (restrictTo subs (extractDecl x mdl decl)) [] ]
	| otherwise
	= return []
	where 
	      subs = 
		case mb_subs of
		  Nothing -> in_scope_subs
	      	  Just xs -> filter (`elem` map nameOfQName xs) in_scope_subs

	      in_scope_subs = map nameOfQName in_scope_subs_qnames
	      in_scope_subs_qnames = filter (`elem` in_scope) all_subs_qnames

	      all_subs_qnames = map (Qual mdl) all_subs

	      all_subs | mdl == this_mod = lookupWithDefaultFM sub_map [] x
		       | otherwise       = all_subs_of_qname mod_map t

    fullContentsOf m
	| m == this_mod  = return (fullContentsOfThisModule this_mod decls)
	| otherwise = 
	   case lookupFM mod_map m of
	     Just iface
		| OptHide `elem` iface_options iface
			-> return (iface_orig_exports iface)
		| otherwise -> return [ ExportModule m ]
	     Nothing -> do tell ["Warning: module not found: " ++ show m]
			   return []

    findDecl :: HsQName -> Maybe HsDecl
    findDecl (UnQual _)
	= Nothing	-- must be a name we couldn't resolve
    findDecl (Qual m n)
	| m == this_mod  = lookupFM decl_map n
	| otherwise = 
	   case lookupFM mod_map m of
		Just iface -> lookupFM (iface_decls iface) n
		Nothing -> Nothing

fullContentsOfThisModule :: Module -> [HsDecl] -> [ExportItem]
fullContentsOfThisModule mdl decls = 
  map mkExportItem (filter keepDecl decls)
  where mkExportItem (HsDocGroup _ lev doc) = ExportGroup lev "" doc
	mkExportItem decl = ExportDecl (Qual mdl x) decl []
	     where Just x = declMainBinder decl

keepDecl :: HsDecl -> Bool
keepDecl HsTypeSig{}       = True
keepDecl HsTypeDecl{}      = True
keepDecl HsNewTypeDecl{}   = True
keepDecl HsDataDecl{}      = True
keepDecl HsClassDecl{}     = True
keepDecl HsDocGroup{}	   = True
keepDecl HsForeignImport{} = True
keepDecl _ = False

-- Sometimes the declaration we want to export is not the "main" declaration:
-- it might be an individual record selector or a class method.  In these
-- cases we have to extract the required declaration (and somehow cobble 
-- together a type signature for it...)

extractDecl :: HsName -> Module -> HsDecl -> HsDecl
extractDecl name mdl decl
  | Just n <- declMainBinder decl, n == name  =  decl
  | otherwise  =  
	case decl of
	    HsClassDecl _ _ n tvs _ decls _ ->
		case [ d | d@HsTypeSig{} <- decls, 
			   declMainBinder d == Just name ] of
		  [d0] -> extractClassDecl n mdl tvs d0
		  _ -> error "internal: extractDecl"

	    HsDataDecl _ _ t tvs cons _ _ ->
		extractRecSel name mdl t tvs cons

	    HsNewTypeDecl _ _ t tvs con _ _ ->
		extractRecSel name mdl t tvs [con]

	    _ -> error ("extractDecl: "  ++ show decl)

extractClassDecl :: HsName -> Module -> [HsName] -> HsDecl -> HsDecl
extractClassDecl c mdl tvs0 (HsTypeSig loc [n] ty doc)
 = case ty of
 	HsForAllType tvs ctxt' ty' -> 
	  HsTypeSig loc [n] (HsForAllType tvs (ctxt ++ ctxt') ty') doc
	_ -> 
	  HsTypeSig loc [n] (HsForAllType Nothing ctxt ty) doc
 where
  ctxt = [(Qual mdl c, map HsTyVar tvs0)]
extractClassDecl _ _ _ d =
     error $ "Main.extractClassDecl: unexpected decl: " ++ show d

extractRecSel :: HsName -> Module -> HsName -> [HsName] -> [HsConDecl]
              -> HsDecl
extractRecSel _ _ _ _ [] = error "extractRecSel: selector not found"
extractRecSel _ _ _ _ (d@(HsConDecl{}):_) =
    error $ "Main.extractRecSel: unexpected (con)decl" ++ show d
extractRecSel nm mdl t tvs (HsRecDecl loc _ _tvs _ fields _mb_doc : rest)
  | (HsFieldDecl ns ty mb_doc : _) <- matching_fields
	= HsTypeSig loc [nm] (HsTyFun data_ty (unbang ty)) mb_doc
  | otherwise = extractRecSel nm mdl t tvs rest
  where
	matching_fields = [ f | f@(HsFieldDecl ns ty mb_doc) <- fields,
			        nm `elem` ns ]

	data_ty = foldl HsTyApp (HsTyCon (Qual mdl t)) (map HsTyVar tvs)

-- -----------------------------------------------------------------------------
-- Pruning

pruneExportItems :: [ExportItem] -> [ExportItem]
pruneExportItems items = filter has_doc items
  where has_doc (ExportDecl _ d _) = isJust (declDoc d)
	has_doc _ = True

-- -----------------------------------------------------------------------------
-- Make a sub-name map for this module

mkSubNames :: [HsDecl] -> FiniteMap HsName [HsName]
mkSubNames decls = 
  listToFM [ (n, subs) | d <- decls, 
		         Just n <- [declMainBinder d],
			 subs@(_:_) <- [declSubBinders d] ]

-- -----------------------------------------------------------------------------
-- Gather a list of original names exported from this module

exportedNames :: Module -> ModuleMap -> [HsName]
	-> FiniteMap HsQName HsQName
	-> FiniteMap HsName [HsName]
	-> Maybe [HsExportSpec]
	-> [DocOption]
	-> ([HsQName], [HsQName])

exportedNames mdl mod_map local_names orig_env sub_map maybe_exps opts
  | Nothing <- maybe_exps 	    = all_local_names_pr
  | OptIgnoreExports `elem` opts    = all_local_names_pr
  | Just expspecs <- maybe_exps     = 
	(concat (map extract expspecs), 
	 concat (map extract_vis expspecs))
 where
  all_local_names = map (Qual mdl) local_names
  all_local_names_pr = (all_local_names,all_local_names)

  in_scope = eltsFM orig_env

  extract e = 
   case e of
    HsEVar x -> [x]
    HsEAbs t -> [t]
    HsEThingAll t@(Qual m x) ->
	 t : filter (`elem` in_scope) (map (Qual m) all_subs)
	 where
	      all_subs | m == mdl  = lookupWithDefaultFM sub_map [] x
		       | otherwise = all_subs_of_qname mod_map t

    HsEThingWith t cs -> t : cs
    HsEModuleContents m
	| m == mdl  -> map (Qual mdl) local_names
	| otherwise ->
	  case lookupFM mod_map m of
	    Just iface -> filter (`elem` in_scope) (eltsFM (iface_env iface))
	    Nothing    -> trace ("Warning: module not found: " ++ show m) $ []
    _ -> []

  -- Just the names that will be visible in the documentation
  -- (ie. omit names exported via a 'module M' export, if we are just
  -- going to cross-reference the module).
  extract_vis e = 
   case e of
    HsEModuleContents m
	| m == mdl  -> map (Qual mdl) local_names
	| otherwise ->
	  case lookupFM mod_map m of
	    Just iface
		| OptHide `elem` iface_options iface ->
			filter (`elem` in_scope) (eltsFM (iface_env iface))
		| otherwise -> []
	    Nothing
		-> trace ("Warning: module not found: " ++ show m) $ []
    _ -> extract e

-- for a given entity, find all the names it "owns" (ie. all the
-- constructors and field names of a tycon, or all the methods of a
-- class).
all_subs_of_qname :: ModuleMap -> HsQName -> [HsName]
all_subs_of_qname mod_map (Qual mdl nm) =
  case lookupFM mod_map mdl of
	Just iface -> lookupWithDefaultFM (iface_sub iface) [] nm
	Nothing    -> []
all_subs_of_qname _ n@(UnQual _) =
    error $ "Main.all_subs_of_qname: unexpected unqual'd name:" ++ show n

-- ----------------------------------------------------------------------------
-- Get a list of names exported by this module that are not actually
-- documented here, and build a mapping to point to where the
-- documentation for those names can be found.  This is used for
-- constructing the iface_reexports field of the Interface.

getReExports :: Module -> ModuleMap -> Maybe [HsExportSpec] -> FiniteMap HsName HsQName
getReExports mdl mod_map Nothing = emptyFM
getReExports mdl mod_map (Just exps)
  = foldr plusFM emptyFM (map extract exps)
  where
    extract (HsEModuleContents m) | m /= mdl =
	  case lookupFM mod_map m of
	    Nothing -> emptyFM
	    Just iface
		| OptHide `elem` iface_options iface -> emptyFM
		| otherwise -> listToFM (map get_name (keysFM (iface_env iface)))
		where
		   get_name n = case lookupFM (iface_reexported iface) n of
				   Just somewhere_else -> (n, somewhere_else)
				   Nothing -> (n, Qual m n)
    extract _ = emptyFM

-- ----------------------------------------------------------------------------
-- Building name environments

-- The orig env maps names in the current source file to
-- fully-qualified "original" names.

buildOrigEnv :: ModuleMap -> [HsImportDecl] -> FiniteMap HsQName HsQName
buildOrigEnv mod_map imp_decls
  = foldr plusFM emptyFM (map build imp_decls)
  where
  build imp_decl@(HsImportDecl _ mdl qual maybe_as _)
    = case lookupFM mod_map mdl of
       Nothing -> 
	  trace ("Warning: module not found: " ++ show mdl) $ emptyFM
       Just iface -> 
	  listToFM (concat (map orig_map (processImportDecl mod_map imp_decl)))
        where

	-- bring both qualified and unqualified names into scope, unless
	-- the import was 'qualified'.
	orig_map (nm,qnm)
	  | qual      = [ (Qual qual_module nm, qnm) ]
	  | otherwise = [ (Qual qual_module nm, qnm), (UnQual nm, qnm) ]

        qual_module
	  | Just m <- maybe_as = m
	  | otherwise          = mdl


-- The import env maps each "original" name referred to in the current
-- module to the qualified name that we want to link to in the
-- documentation.

buildImportEnv :: ModuleMap -> Module -> [HsQName] -> [HsImportDecl]
	-> FiniteMap HsQName HsQName
buildImportEnv mod_map this_mod exported_names imp_decls
  = foldr plusFM emptyFM (map build imp_decls)
  where
	build imp_decl@(HsImportDecl _ mdl _ _ _) = 
	  case lookupFM mod_map mdl of
       	    Nothing    -> emptyFM
            Just iface -> listToFM (map import_map imported_names)
	     where
	      imported_names = processImportDecl mod_map imp_decl
	      reexport_env = iface_reexported iface

	      import_map (nm,qnm) = (qnm, maps_to)
 	       where 
		maps_to
		 -- we re-export it: just link to this module
		 | qnm `elem` exported_names = Qual this_mod nm
		 -- re-exported from the other module, but not documented there:
		 -- find the right place using the iface_reexported environment.
		 | Just new_qnm <- lookupFM reexport_env nm = new_qnm
		 -- otherwise, it's documented in the other module
		 | otherwise = Qual mdl nm


processImportDecl :: ModuleMap -> HsImportDecl -> [(HsName,HsQName)]
processImportDecl mod_map (HsImportDecl _ mdl is_qualified maybe_as imp_specs)
    = case lookupFM mod_map mdl of
       Nothing    -> []
       Just iface -> imported_names
        where
	 env = iface_env iface
	 sub = iface_sub iface

 	 all_names = fmToList env

	 imported_names :: [(HsName,HsQName)]
	 imported_names
	   = case imp_specs of
		Nothing          -> all_names
	        Just (False,specs) -> [ (n,qnm) | (n,qnm) <- all_names,
						n `elem` names specs False ]
	        Just (True, specs) -> [ (n,qnm) | (n,qnm) <- all_names,
						n `notElem` names specs True ]
	      where
		names specs is_hiding 
		  = concat (map (spec_names is_hiding) specs)

	-- when hiding, a conid refers to both the constructor and
	-- the type/class constructor.
	 spec_names _hid (HsIVar v)		= [v]
	 spec_names True  (HsIAbs (HsTyClsName i))
		 = [HsTyClsName i, HsVarName i]
	 spec_names False (HsIAbs v)		= [v]
	 spec_names _hid (HsIThingAll v)	= v : sub_names v
	 spec_names _hid (HsIThingWith v xs) 	= v : xs

	 sub_names :: HsName -> [HsName]
	 sub_names nm =
	  case lookupFM env nm of
	    Just qnm -> filter (`elemFM` env) (all_subs_of_qname mod_map qnm)
	    _ -> []

-- -----------------------------------------------------------------------------
-- Expand multiple type signatures

expandDecl :: HsDecl -> [HsDecl]
expandDecl (HsTypeSig loc fs qt doc) = [ HsTypeSig loc [f] qt doc | f <- fs ]
expandDecl (HsClassDecl loc ctxt n tvs fds decls doc)
  = [ HsClassDecl loc ctxt n tvs fds (concat (map expandDecl decls)) doc ]
expandDecl d = [ d ]

-----------------------------------------------------------------------------
-- Collecting documentation and attach it to the right declarations

collectDoc :: [HsDecl] -> [HsDecl]
collectDoc decls = collect Nothing DocEmpty decls

collect :: Maybe HsDecl -> GenDoc [HsQName] -> [HsDecl] -> [HsDecl]
collect d doc_so_far [] = 
   case d of
	Nothing -> []
	Just d0  -> finishedDoc d0 doc_so_far []

collect d doc_so_far (decl:ds) = 
   case decl of
      HsDocCommentNext _ str -> 
	case d of
	   Nothing -> collect d (docAppend doc_so_far str) ds
	   Just d0 -> finishedDoc d0 doc_so_far (collect Nothing str ds)

      HsDocCommentPrev _ str -> collect d (docAppend doc_so_far str) ds

      _other -> 
	let decl' = collectInDecl decl in
	case d of
	    Nothing -> collect (Just decl') doc_so_far ds
	    Just d0 -> finishedDoc d0 doc_so_far
                           (collect (Just decl') DocEmpty ds)

finishedDoc :: HsDecl -> GenDoc [HsQName] -> [HsDecl] -> [HsDecl]
finishedDoc d DocEmpty rest = d : rest
finishedDoc d doc rest = d' : rest
 where d' = 
	 case d of
	  HsTypeDecl loc n ns ty _ -> 
		HsTypeDecl loc n ns ty (Just doc)
	  HsDataDecl loc ctxt n ns cons drv _ -> 
		HsDataDecl loc ctxt n ns cons drv (Just doc)
	  HsNewTypeDecl loc ctxt n ns con drv _ -> 
		HsNewTypeDecl loc ctxt n ns con drv (Just doc)
	  HsClassDecl loc ctxt n tvs fds meths _ -> 
		HsClassDecl loc ctxt n tvs fds meths (Just doc)
	  HsTypeSig loc ns ty _ -> 
		HsTypeSig loc ns ty (Just doc)
	  HsForeignImport loc cc sf str n ty _ ->
		HsForeignImport loc cc sf str n ty (Just doc)
	  _other -> d

collectInDecl :: HsDecl -> HsDecl
collectInDecl (HsClassDecl loc ctxt n tvs fds meths doc)
  = HsClassDecl loc ctxt n tvs fds (collect Nothing DocEmpty meths) doc
collectInDecl decl
  = decl

-- -----------------------------------------------------------------------------
-- Named documentation

findNamedDoc :: String -> [HsDecl] -> ErrMsgM (Maybe Doc)
findNamedDoc name decls = search decls
	where search [] = do
		tell ["Cannot find documentation for: $" ++ name]
		return Nothing
	      search (HsDocCommentNamed _ name' doc : rest) 
			| name == name' = return (Just doc)
		   	| otherwise = search rest
	      search (_other_decl : rest) = search rest

-- -----------------------------------------------------------------------------
-- Haddock options embedded in the source file

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
parseOption other = do tell ["Unrecognised option: " ++ other]; return Nothing

-- -----------------------------------------------------------------------------
-- Topologically sort the modules

sortModules :: [(HsModule,FilePath)] -> IO [(HsModule,FilePath)]
sortModules mdls = mapM for_each_scc sccs
  where
	sccs = stronglyConnComp edges

	edges :: [((HsModule,FilePath), Module, [Module])]
	edges = [ ((hsmod,file), mdl, get_imps impdecls)
		| (hsmod@(HsModule mdl _ impdecls _ _ _ _), file) <- mdls
		]

        get_imps impdecls  = [ imp | HsImportDecl _ imp _ _ _ <- impdecls  ]

	get_mods hsmodules = [ mdl | HsModule mdl _ _ _ _ _ _ <- hsmodules ]

	for_each_scc (AcyclicSCC hsmodule) = return hsmodule
	for_each_scc (CyclicSCC  hsmodules) = 
	   dieMsg ("modules are recursive: " ++
		   unwords (map show (get_mods (map fst hsmodules))))

-- -----------------------------------------------------------------------------
-- Collect instances and attach them to declarations

attachInstances :: [(Module,Interface)] -> [(Module,Interface)]
attachInstances mod_ifaces
  = map attach mod_ifaces
  where
  inst_map = collectInstances mod_ifaces

  attach (mod,iface) = (mod, iface{ iface_exports = new_exports })
   where
	new_exports = map attach_export (iface_exports iface)

	rename_insts :: [InstHead] -> [InstHead]
	rename_insts insts = fst (runRnFM (iface_import_env iface)
				    (mapM renameInstHead insts))

	attach_export (ExportDecl nm decl _) =
	    ExportDecl nm decl (case lookupFM inst_map nm of
				  Nothing -> []
				  Just instheads -> rename_insts instheads)
	attach_export other_export =
	    other_export

collectInstances
   :: [(Module,Interface)]
   -> FiniteMap HsQName [InstHead]  -- maps class/type names to instances

collectInstances mod_ifaces
  = addListToFM_C (++) emptyFM class_inst_pairs `plusFM` 
    addListToFM_C (++) emptyFM ty_inst_pairs
  where
    all_instances = concat (map (iface_insts.snd) mod_ifaces)

    class_inst_pairs = [ (cls, [(ctxt,(cls,args))])
		       | HsInstDecl _ ctxt (cls,args) _ <- all_instances ]
			
    ty_inst_pairs = [ (nm, [(ctxt,(cls,args))])
		    | HsInstDecl _ ctxt (cls,args) _ <- all_instances,
		      arg <- args,
		      nm <- freeTyCons arg
		    ]
 
-- -----------------------------------------------------------------------------
-- A monad which collects error messages

type ErrMsg = String
type ErrMsgM a = Writer [ErrMsg] a

