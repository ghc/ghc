--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

module Main (main) where

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
import GetOpt
import System
import FiniteMap

--import Pretty

import Maybe	( isJust, maybeToList )
import List	( nub )
import Monad	( when )
import Char	( isSpace )
import IO

#ifdef __GLASGOW_HASKELL__
import IOExts
#endif

import MonadWriter

#if __GLASGOW_HASKELL__ < 500
import Regex
import PackedString
#endif

-----------------------------------------------------------------------------
-- Top-level stuff

main = do
  args <- getArgs
  case getOpt Permute options args of
    (flags, args, []    ) -> run flags args
    (_,     _,    errors) -> do sequence_ (map putStr errors)
				putStr usage

usage = usageInfo "usage: haddock [OPTION] file...\n" options

data Flag
  = Flag_Verbose
  | Flag_DocBook
  | Flag_Html
  | Flag_Heading String
  | Flag_Prologue FilePath
  | Flag_SourceURL String
  | Flag_CSS String
  | Flag_Lib String
  | Flag_OutputDir FilePath
  | Flag_ReadInterface FilePath
  | Flag_DumpInterface FilePath
  | Flag_NoImplicitPrelude
  deriving (Eq)

options =
  [ 
    Option ['d']  ["docbook"]  (NoArg Flag_DocBook)
	"output in docbook (SGML)",
    Option ['h']  ["html"]     (NoArg Flag_Html)
	"output in HTML",
    Option ['o']  ["odir"]     (ReqArg Flag_OutputDir "DIR")
	"directory in which to put the output files",
    Option ['p']  ["prologue"] (ReqArg Flag_Prologue "FILE")
	"file containing prologue text",
    Option ['s']  ["source"]   (ReqArg Flag_SourceURL "URL") 
	"base URL for links to source code",
    Option ['t']  ["title"]    (ReqArg Flag_Heading "TITLE")
	"page heading",
    Option ['v']  ["verbose"]  (NoArg Flag_Verbose)
	"be verbose",
    Option ['i'] ["read-interface"] (ReqArg Flag_ReadInterface "FILE")
	"read an interface from FILE",
    Option []  ["dump-interface"]   (ReqArg Flag_DumpInterface "FILE")
        "dump an interface for these modules in FILE",
    Option []  ["css"]         (ReqArg Flag_CSS "FILE") 
	"The CSS file to use for HTML output",
    Option []  ["lib"]         (ReqArg Flag_Lib "DIR") 
	"Directory containing Haddock's auxiliary files",
    Option []  ["no-implicit-prelude"] (NoArg Flag_NoImplicitPrelude)
 	"Do not assume Prelude is imported"
  ]

saved_flags :: IORef [Flag]
saved_flags = unsafePerformIO (newIORef (error "no flags yet"))

run flags files = do
  let title = case [str | Flag_Heading str <- flags] of
		[] -> ""
		(t:ts) -> t

      source_url = case [str | Flag_SourceURL str <- flags] of
			[] -> Nothing
			(t:ts) -> Just t

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
	loop ifaces ((hsmod,file):mods)  = do 
	   let ((mod,iface),msgs) = runWriter $
		   mkInterface no_implicit_prelude ifaces file hsmod
	       new_ifaces = addToFM ifaces mod iface
	   mapM (hPutStrLn stderr) msgs
	   loop new_ifaces mods

  module_map <- loop (listToFM read_ifaces) sorted_mod_files
  let mod_ifaces = fmToList module_map

      these_mod_ifaces = [ (mod, iface) 
			 | (mod, iface) <- mod_ifaces,
			   mod `notElem` external_mods ]

--  when (Flag_DocBook `elem` flags) $
--    putStr (ppDocBook odir mod_ifaces)

  let inst_maps = collectInstances these_mod_ifaces

  when (Flag_Html `elem` flags) $
    ppHtml title source_url these_mod_ifaces odir css_file 
	libdir inst_maps prologue

  -- dump an interface if requested
  case dump_iface of
     Nothing -> return ()
     Just fn -> do
	bh <- openBinMem 100000
	put_ bh prepared_ifaces
	writeBinMem bh fn
      where
	prepared_ifaces = [ (mod, fmToList (iface_env iface))
		          | (mod, iface) <- these_mod_ifaces ]

parseIfaceOption :: String -> (FilePath,FilePath)
parseIfaceOption s = 
  case break (==',') s of
	(path,',':file) -> (path,file)
	(_, file)       -> ("", file)

readIface :: FilePath -> IO [(Module,Interface)]
readIface filename = do
  bh <- readBinMem filename
  stuff <- get bh
  return (map to_interface stuff)
 where 
   to_interface (mod, env) = 
	  (mod, Interface { 
		   iface_filename     = "",
		   iface_env          = listToFM env,
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
  mapping = [ (mod,path) 
	    | (path, ifaces) <- zip paths ifaces_s,
	      (mod, _iface) <- ifaces
	    ]


parse_file file = do
  bracket 
    (openFile file ReadMode)
    (\h -> hClose h)
    (\h -> do stuff <- hGetContents h 
	      case parse stuff (SrcLoc 1 1) 1 0 [] of
	        Ok state e -> return e
	        Failed err -> do hPutStrLn stderr (file ++ ':':err)
				 exitWith (ExitFailure 1)
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
	(HsModule mod exps imps decls maybe_opts maybe_info maybe_doc) = do  

  -- Process the options, if available
  options <- case maybe_opts of
		Just opt_str -> processOptions opt_str
		Nothing      -> return []

  let
     -- first, attach documentation to declarations
     annotated_decls = collectDoc decls

     -- now find the defined names
     locally_defined_names = collectNames annotated_decls

     qual_local_names   = map (Qual mod) locally_defined_names
     unqual_local_names = map UnQual     locally_defined_names

     local_orig_env = listToFM (zip unqual_local_names qual_local_names ++
			        zip qual_local_names   qual_local_names)
	 -- both qualified and unqualifed names are in scope for local things

     implicit_imps
	| no_implicit_prelude || any is_prel_import imps = imps
	| otherwise = HsImportDecl loc prelude_mod False Nothing Nothing : imps
	where 
		loc = SrcLoc 0 0
	 	is_prel_import (HsImportDecl _ mod _ _ _ ) = mod == prelude_mod

     -- build the orig_env, which maps names to *original* names (so we can
     -- find the original declarations & docs for things).
     orig_env = buildOrigEnv mod_map implicit_imps `plusFM` local_orig_env

     -- convert names in source code to original, fully qualified, names
     (orig_exports, missing_names1) 
	= runRnFM orig_env (mapMaybeM renameExportList exps)

     (orig_decls, missing_names2)
	= runRnFM orig_env (mapM renameDecl annotated_decls)

     orig_decl_map :: FiniteMap HsName HsDecl
     orig_decl_map = listToFM [ (n,d) | d <- orig_decls, n <- declBinders d ]

     -- gather up a list of entities that are exported (original names)
     (exported_names, exported_visible_names)
	 = exportedNames mod mod_map orig_decls
			locally_defined_names orig_exports
			orig_decl_map options

     -- build the import env, which maps original names to import names
     local_import_env = listToFM (zip qual_local_names qual_local_names)
     import_env = local_import_env `plusFM`
		   buildImportEnv mod_map mod exported_visible_names 
			implicit_imps

  let
     final_decls = concat (map expandDecl orig_decls)

     decl_map :: FiniteMap HsName HsDecl
     decl_map = listToFM [ (n,d) | d <- final_decls, n <- declBinders d ]

     instances = [ d | d@HsInstDecl{} <- final_decls ]

  -- make the "export items", which will be converted into docs later
  orig_export_list <- mkExportItems mod_map mod decl_map
			 		final_decls options orig_exports

  let
     -- prune the export list to just those declarations that have
     -- documentation, if the 'prune' option is on.
     pruned_export_list
	| OptPrune `elem` options = pruneExportItems orig_export_list
	| otherwise = orig_export_list

     -- rename names in the exported declarations to point to things that
     -- are closer, or maybe even exported by, the current module.
     (renamed_export_list, _missing_names3)
        = runRnFM import_env (renameExportItems pruned_export_list)

     name_env = listToFM [ (nameOfQName n, n) | n <- exported_names ]

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
	  tell ["Warning: " ++ show mod ++ 
		": the following names could not be resolved:\n\ 
		\   " ++ concat (map (' ':) name_strings)
		]

  return (mod, Interface { 
		   iface_filename     = filename,
		   iface_env          = name_env,
		   iface_exports      = renamed_export_list,
		   iface_orig_exports = pruned_export_list,
		   iface_insts	      = instances,
		   iface_decls        = decl_map,
		   iface_info	      = maybe_info,
		   iface_doc          = final_module_doc,
		   iface_options      = options
		}
      	  )

-- -----------------------------------------------------------------------------
-- Build the list of items that will become the documentation, from the
-- export list.  At this point, the list of ExportItems is in terms of
-- original names.

mkExportItems
	:: ModuleMap
	-> Module			-- this module
	-> FiniteMap HsName HsDecl	-- maps local names to declarations
	-> [HsDecl]			-- decls in the current module
	-> [DocOption]
	-> Maybe [HsExportSpec]
	-> ErrMsgM [ExportItem]

mkExportItems mod_map mod decl_map decls options maybe_exps
  | Nothing <- maybe_exps	    = everything_local_exported
  | OptIgnoreExports `elem` options = everything_local_exported
  | Just specs <- maybe_exps = do 
	exps <- mapM lookupExport specs
        return (concat exps)
  where

    everything_local_exported =
	return (fullContentsOfThisModule mod decl_map) -- everything exported

    lookupExport (HsEVar x) 
	| Just decl <- findDecl x
	= return [ ExportDecl x (extractDecl (nameOfQName x) x_mod decl) ]
        where x_mod | Qual m _ <- x = m
	  -- ToDo: cope with record selectors here
    lookupExport (HsEAbs t)
	| Just decl <- findDecl t
	= return [ ExportDecl t (restrictTo [] decl) ]
    lookupExport (HsEThingAll t)
	| Just decl <- findDecl t
	= return [ ExportDecl t decl ]
    lookupExport (HsEThingWith t cs)
	| Just decl <- findDecl t
	= return [ ExportDecl t (restrictTo (map nameOfQName cs) decl) ]
    lookupExport (HsEModuleContents m) = fullContentsOf m
    lookupExport (HsEGroup lev doc)
	= return [ ExportGroup lev "" doc ]
    lookupExport (HsEDoc doc)
	= return [ ExportDoc doc ]
    lookupExport (HsEDocNamed str)
	= do r <- findNamedDoc str decls
	     case r of
		Nothing -> return []
		Just found -> return [ ExportDoc found ]
	
    lookupExport _ = return [] -- didn't find it?

    fullContentsOf m
	| m == mod  = return (fullContentsOfThisModule mod decl_map)
	| otherwise = 
	   case lookupFM mod_map m of
	     Just iface
		| OptHide `elem` iface_options iface
			-> return (iface_orig_exports iface)
		| otherwise -> return [ ExportModule m ]
	     Nothing -> do tell ["Warning: module not found: " ++ show m]
			   return []

    findDecl :: HsQName -> Maybe HsDecl
    findDecl (UnQual n)
	= Nothing	-- must be a name we couldn't resolve
    findDecl (Qual m n)
	| m == mod  = lookupFM decl_map n
	| otherwise = 
	   case lookupFM mod_map m of
		Just iface -> lookupFM (iface_decls iface) n
		Nothing -> Nothing

fullContentsOfThisModule mod decl_map = 
  map mkExportItem (filter (keepDecl.snd) (fmToList decl_map))
  where mkExportItem (x,HsDocGroup loc lev doc) = ExportGroup lev "" doc
	mkExportItem (x,decl) = ExportDecl (Qual mod x) decl

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
extractDecl name mod decl
  | Just n <- declMainBinder decl, n == name  =  decl
  | otherwise  =  
	case decl of
	    HsClassDecl loc ctxt n tvs fds decls mb_doc ->
		case [ d | d@HsTypeSig{} <- decls, 
			   declMainBinder d == Just name ] of
		  [decl] -> extractClassDecl n mod tvs decl
		  _ -> error "internal: extractDecl"

	    HsDataDecl loc ctxt t tvs cons drvs mb_doc ->
		extractRecSel name mod t tvs cons

	    HsNewTypeDecl loc ctxt t tvs con drvs mb_doc ->
		extractRecSel name mod t tvs [con]

	    _ -> error ("extractDecl: "  ++ show decl)


extractClassDecl c mod tvs (HsTypeSig loc [n] ty doc)
 = case ty of
 	HsForAllType tvs ctxt' ty' -> 
	  HsTypeSig loc [n] (HsForAllType tvs (ctxt ++ ctxt') ty') doc
	ty -> 
	  HsTypeSig loc [n] (HsForAllType Nothing ctxt ty) doc
 where
  ctxt = [(Qual mod c, map HsTyVar tvs)]

extractRecSel nm mod t tvs [] = error "extractRecSel: selector not found"
extractRecSel nm mod t tvs (HsRecDecl loc c _tvs ctxt fields _mb_doc : rest)
  | (HsFieldDecl ns ty mb_doc : _) <- matching_fields
	= HsTypeSig loc [nm] (HsTyFun data_ty (unbang ty)) mb_doc
  | otherwise = extractRecSel nm mod t tvs rest
  where
	matching_fields = [ f | f@(HsFieldDecl ns ty mb_doc) <- fields,
			        nm `elem` ns ]

	data_ty = foldl HsTyApp (HsTyCon (Qual mod t)) (map HsTyVar tvs)

-- -----------------------------------------------------------------------------
-- Pruning

pruneExportItems :: [ExportItem] -> [ExportItem]
pruneExportItems items = filter has_doc items
  where has_doc (ExportDecl x d) = isJust (declDoc d)
	has_doc _ = True

-- -----------------------------------------------------------------------------
-- Gather a list of original names exported from this module

exportedNames :: Module -> ModuleMap -> [HsDecl] -> [HsName]
	-> Maybe [HsExportSpec]
	-> FiniteMap HsName HsDecl
	-> [DocOption]
	-> ([HsQName], [HsQName])

exportedNames mod mod_map decls local_names maybe_exps decl_map options
  | Nothing <- maybe_exps 	    = all_local_names_pr
  | OptIgnoreExports `elem` options = all_local_names_pr
  | Just expspecs <- maybe_exps     = 
	(concat (map extract expspecs), concat (map extract_vis expspecs))
 where
  all_local_names = map (Qual mod) local_names
  all_local_names_pr = (all_local_names,all_local_names)

  extract e = 
   case e of
    HsEVar x -> [x]
    HsEAbs t -> [t]
    HsEThingAll t
	|  Just decl <- export_lookup t 
	-> t : map (Qual t_mod) (declBinders decl)
	where t_mod = case t of Qual m _ -> m; otherwise -> mod
    HsEThingWith t cs -> t : cs
    HsEModuleContents m
	| m == mod  -> map (Qual mod) local_names
	| otherwise ->
	  case lookupFM mod_map m of
	    Just iface -> eltsFM (iface_env iface)
	    Nothing    -> trace ("Warning: module not found: " ++ show m) $ []
    _ -> []

  -- Just the names that will be visible in the documentation
  -- (ie. omit names exported via a 'module M' export, if we are just
  -- going to cross-reference the module).
  extract_vis e = 
   case e of
    HsEModuleContents m
	| m == mod  -> map (Qual mod) local_names
	| otherwise ->
	  case lookupFM mod_map m of
	    Just iface
		| OptHide `elem` iface_options iface -> eltsFM (iface_env iface)
		| otherwise -> []
	    Nothing
		-> trace ("Warning: module not found: " ++ show m) $ []
    _ -> extract e

  export_lookup :: HsQName -> Maybe HsDecl
  export_lookup (UnQual n)
	= trace ("Warning(exportedNames): UnQual! " ++ show n) $ Nothing
  export_lookup (Qual m n)
	| m == mod  = lookupFM decl_map n
	| otherwise
	    = case lookupFM mod_map m of
		Just iface -> lookupFM (iface_decls iface) n
		Nothing    -> trace ("Warning: module not found: " ++ show m) 
				Nothing

-- -----------------------------------------------------------------------------
-- Building name environments

buildOrigEnv :: ModuleMap -> [HsImportDecl] -> FiniteMap HsQName HsQName
buildOrigEnv mod_map imp_decls
  = foldr plusFM emptyFM (map build imp_decls)
  where
  build (HsImportDecl _ mod qual maybe_as _)
    = case lookupFM mod_map mod of
       Nothing -> 
	  trace ("Warning: module not found: " ++ show mod) $ emptyFM
       Just iface -> 
	  listToFM (concat (map orig_map (fmToList (iface_env iface))))
    where
	-- bring both qualified and unqualified names into scope, unless
	-- the import was 'qualified'.
	orig_map (nm,qnm)
	  | qual      = [ (Qual qual_module nm, qnm) ]
	  | otherwise = [ (UnQual nm, qnm), (Qual qual_module nm, qnm) ]

        qual_module
	  | Just m <- maybe_as = m
	  | otherwise          = mod

buildImportEnv :: ModuleMap -> Module -> [HsQName] -> [HsImportDecl]
	-> FiniteMap HsQName HsQName
buildImportEnv mod_map this_mod exported_names imp_decls
  = foldr plusFM emptyFM (map build imp_decls)
  where
  build (HsImportDecl _ mod qual maybe_as _)
    = case lookupFM mod_map mod of
       Nothing    -> emptyFM
       Just iface -> listToFM (map import_map (fmToList (iface_env iface)))
    where
	import_map (nm,qnm) = (qnm, maps_to)
 	  where maps_to | qnm `elem` exported_names = Qual this_mod nm
		        | otherwise = Qual mod nm

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

collect d doc_so_far [] = 
   case d of
	Nothing -> []
	Just d  -> finishedDoc d doc_so_far []

collect d doc_so_far (decl:ds) = 
   case decl of
      HsDocCommentNext loc str -> 
	case d of
	   Nothing -> collect d (docAppend doc_so_far str) ds
	   Just d  -> finishedDoc d doc_so_far (collect Nothing str ds)

      HsDocCommentPrev loc str -> collect d (docAppend doc_so_far str) ds

      _other -> 
	let decl' = collectInDecl decl in
	case d of
	    Nothing -> collect (Just decl') doc_so_far ds
	    Just d  -> finishedDoc d doc_so_far (collect (Just decl') DocEmpty ds)

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
	      search (HsDocCommentNamed loc name' doc : rest) 
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
sortModules mods = mapM for_each_scc sccs
  where
	sccs = stronglyConnComp edges

	edges :: [((HsModule,FilePath), Module, [Module])]
	edges = [ ((hsmod,file), mod, get_imps impdecls)
		| (hsmod@(HsModule mod _ impdecls _ _ _ _), file) <- mods
		]

        get_imps impdecls  = [ imp | HsImportDecl _ imp _ _ _ <- impdecls  ]

	get_mods hsmodules = [ mod | HsModule mod _ _ _ _ _ _ <- hsmodules ]

	for_each_scc (AcyclicSCC hsmodule) = return hsmodule
	for_each_scc (CyclicSCC  hsmodules) = 
	   dieMsg ("modules are recursive: " ++
		   unwords (map show (get_mods (map fst hsmodules))))

-- -----------------------------------------------------------------------------
-- Collect instances

collectInstances
   :: [(Module,Interface)] 
   -> (FiniteMap HsQName [InstHead],	-- maps class names to instances
       FiniteMap HsQName [InstHead])	-- maps type names to instances

collectInstances mod_ifaces
  = (addListToFM_C (++) emptyFM class_inst_pairs, 
     addListToFM_C (++) emptyFM ty_inst_pairs)
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

