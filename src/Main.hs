--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--

module Main (main) where

import Binary
import Digraph
--import HaddockDB   -- not compiling
import HaddockHtml
import HaddockLex
import HaddockParse
import HaddockRename
import HaddockTypes
import HaddockUtil
import HaddockVersion
import HsParseMonad
import HsParser
import HsSyn
import Map ( Map )
import qualified Map hiding ( Map )
import Set

import Control.Exception ( bracket )
import Control.Monad ( when )
import Control.Monad.Writer ( Writer, runWriter, tell )
import Data.Char ( isSpace )
import Data.IORef ( writeIORef )
import Data.List ( nub )
import Data.Maybe ( isJust, maybeToList )
--import Debug.Trace
import System.Console.GetOpt ( getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..) )
import System.Environment ( getArgs )
import System.IO ( stderr, IOMode(..), openFile, hClose, hGetContents, hPutStrLn )
#if defined(mingw32_HOST_OS)
import Foreign
import Foreign.C
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
  | Flag_Package String
  | Flag_Html
  | Flag_HtmlHelp String
  | Flag_Lib String
  | Flag_NoImplicitPrelude
  | Flag_OutputDir FilePath
  | Flag_Prologue FilePath
  | Flag_ReadInterface FilePath
  | Flag_SourceURL String
  | Flag_Help
  | Flag_Verbose
  | Flag_Version
  | Flag_UseContents String
  | Flag_GenContents
  | Flag_UseIndex String
  | Flag_GenIndex
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
--	"output in DocBook XML",
    Option ['h']  ["html"]     (NoArg Flag_Html)
	"output in HTML",
    Option []  ["html-help"]    (ReqArg Flag_HtmlHelp "format")
	"produce index and table of contents in mshelp, mshelp2 or devhelp format (with -h)",
    Option ['s']  ["source"]   (ReqArg Flag_SourceURL "URL") 
	"base URL for links to source code",
    Option ['c']  ["css"]         (ReqArg Flag_CSS "FILE") 
	"the CSS file to use for HTML output",
    Option ['p']  ["prologue"] (ReqArg Flag_Prologue "FILE")
	"file containing prologue text",
    Option ['t']  ["title"]    (ReqArg Flag_Heading "TITLE")
	"page heading",
    Option ['k']  ["package"]  (ReqArg Flag_Package "PACKAGE")
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
	"generate an HTML contents from specified interfaces",
    Option [] ["use-index"] (ReqArg Flag_UseIndex "URL")
	"use a separately-generated HTML index",
    Option [] ["gen-index"] (NoArg Flag_GenIndex)
	"generate an HTML index from specified interfaces"
  ]

run :: [Flag] -> [FilePath] -> IO ()
run flags files = do
  when (Flag_Help `elem` flags) $ do
     prog <- getProgramName
     bye (usageInfo (usageHeader prog) options)

  when (Flag_Version `elem` flags) $
     bye ("Haddock version " ++ projectVersion ++ ", (c) Simon Marlow 2003\n")

  let title = case [str | Flag_Heading str <- flags] of
		[] -> ""
		(t:_) -> t

      package = case [str | Flag_Package str <- flags] of
		[] -> Nothing
		(t:_) -> Just t

      source_url = case [str | Flag_SourceURL str <- flags] of
			[] -> Nothing
			(t:_) -> Just t

  libdir <- case [str | Flag_Lib str <- flags] of
		[] -> do maybe_exec_dir <- getBaseDir
				-- Get directory of executable
			 case maybe_exec_dir of
                                       Nothing  -> return "."
                                       Just dir -> return dir
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
      verbose = Flag_Verbose `elem` flags

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

  read_ifaces_s <- mapM readIface (map snd ifaces_to_read)

  let read_ifaces = concat read_ifaces_s
      visible_read_ifaces = filter ((OptHide `notElem`) . iface_options . snd) 
				read_ifaces
      external_mods = map fst read_ifaces
      pkg_paths = map fst ifaces_to_read

  updateHTMLXRefs pkg_paths read_ifaces_s

  when ((Flag_GenIndex `elem` flags || Flag_GenContents `elem` flags)
	&& Flag_Html `elem` flags) $
	die ("-h cannot be used with --gen-index or --gen-contents")

  when (Flag_GenContents `elem` flags) $ do
	ppHtmlContents odir title package maybe_html_help_format maybe_index_url visible_read_ifaces prologue
        copyHtmlBits odir libdir css_file

  when (Flag_GenIndex `elem` flags) $ do
	ppHtmlIndex odir title package maybe_html_help_format maybe_contents_url visible_read_ifaces
        copyHtmlBits odir libdir css_file
        
  when (Flag_GenContents `elem` flags && Flag_GenIndex `elem` flags) $ do
    ppHtmlHelpFiles title package visible_read_ifaces odir maybe_html_help_format pkg_paths

  parsed_mods <- mapM parse_file files

  sorted_mod_files <- sortModules (zip parsed_mods files)
	-- emits an error message if there are recursive modules

  -- process the modules in sorted order, building up a mapping from
  -- modules to interfaces.
  let 
	loop ifaces [] = return ifaces
	loop ifaces ((hsmod,file):mdls)  = do 
	   let ((mdl,iface),msgs) = runWriter $
		   mkInterface no_implicit_prelude verbose ifaces 
			file package hsmod
	       new_ifaces = Map.insert mdl iface ifaces
	   mapM (hPutStrLn stderr) msgs
	   loop new_ifaces mdls

  module_map <- loop (Map.fromList read_ifaces) sorted_mod_files
  let mod_ifaces = Map.toAscList module_map

      these_mod_ifaces0 = [ (mdl, iface) 
			  | (mdl, iface) <- mod_ifaces,
			    mdl `notElem` external_mods ]

--  when (Flag_DocBook `elem` flags) $
--    putStr (ppDocBook odir mod_ifaces)

  let these_mod_ifaces = attachInstances these_mod_ifaces0

  when (Flag_Debug `elem` flags) $ do
    mapM_ putStrLn (map show [ (mdl, Map.toAscList (iface_env i), 
				     Map.toAscList (iface_sub i))
			     | (mdl, i) <-  these_mod_ifaces ])

  when (Flag_Html `elem` flags) $ do
    ppHtml title package source_url these_mod_ifaces odir
		prologue maybe_html_help_format
		maybe_contents_url maybe_index_url
    copyHtmlBits odir libdir css_file

  -- dump an interface if requested
  case dump_iface of
     Nothing -> return ()
     Just fn -> dumpInterfaces these_mod_ifaces fn

parseIfaceOption :: String -> (FilePath,FilePath)
parseIfaceOption s = 
  case break (==',') s of
	(fpath,',':file) -> (fpath,file)
	(file, _)        -> ("", file)
		

updateHTMLXRefs :: [FilePath] -> [[(Module,Interface)]] -> IO ()
updateHTMLXRefs paths ifaces_s =
  writeIORef html_xrefs_ref (Map.fromList mapping)
 where
  mapping = [ (mdl, fpath)
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
   -> Bool				-- verbose
   -> ModuleMap -> FilePath -> Maybe String -> HsModule
   -> ErrMsgM (
	       Module, 		-- the module name
	       Interface	-- its "interface"
	      )

mkInterface no_implicit_prelude verbose mod_map filename package
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

     local_orig_env = Map.fromList (zip unqual_local_names qual_local_names ++
			            zip qual_local_names   qual_local_names)
	 -- both qualified and unqualifed names are in scope for local things

     implicit_imps
	| no_implicit_prelude || any is_prel_import imps = imps
	| otherwise = HsImportDecl loc prelude_mod False Nothing Nothing : imps
	where 
		loc = SrcLoc 0 0
	 	is_prel_import (HsImportDecl _ mdl0 _ _ _ ) = mdl0 == prelude_mod
  -- in

     -- build the orig_env, which maps names to *original* names (so we can
     -- find the original declarations & docs for things).
  imported_orig_env <- buildOrigEnv mdl verbose mod_map implicit_imps
 
  let
     orig_env = local_orig_env `Map.union` imported_orig_env

     -- convert names in source code to original, fully qualified, names
     (orig_exports, missing_names1) 
	= runRnFM orig_env (mapMaybeM renameExportList exps)

     (orig_decls, missing_names2)
	= runRnFM orig_env (mapM renameDecl annotated_decls)

     -- gather up a list of entities that are exported (original names)
  (exported_names, exported_visible_names)
	 <- exportedNames mdl mod_map
			locally_defined_names orig_env sub_map
			orig_exports opts
  let

     -- build the import env, which maps original names to import names
     local_import_env = Map.fromList (zip qual_local_names qual_local_names)

     -- find the names exported by this module that other modules should *not*
     -- link to (and point them to where they should).
     reexports = getReExports mdl exported_names exported_visible_names
			import_env

     import_env = buildImportEnv mod_map mdl exported_visible_names implicit_imps
                  `Map.union` local_import_env

--   trace (show (Map.toAscList orig_env)) $ do
--  trace (show (Map.toAscList import_env)) $ do
  let
     final_decls = orig_decls

     decl_map :: Map HsName HsDecl
     decl_map = Map.fromList [ (n,d) | d <- final_decls, n <- declBinders d ]

     instances = [ d | d@HsInstDecl{} <- final_decls ] ++
		 [ d | decl <- orig_decls, d <- derivedInstances mdl decl]

  -- make the "export items", which will be converted into docs later
  orig_export_list <- mkExportItems mod_map mdl exported_names decl_map sub_map
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

     name_env = Map.fromList [ (nameOfQName n, n) | n <- exported_names ]

  let
     (orig_module_doc, missing_names4)
        = runRnFM orig_env (renameMaybeDoc maybe_doc)

     (final_module_doc, _missing_names5)
        = runRnFM import_env (renameMaybeDoc orig_module_doc)

  -- report any names we couldn't find/resolve

  let missing_names = missing_names1 ++ missing_names2 ++ missing_names4
			 --ignore missing_names3 & missing_names5 for now
      filtered_missing_names = filter (`notElem` ignore) missing_names

      -- ignore certain builtin names ((),[], etc.), because these
      -- cannot be exported anyway.
      ignore = [unit_tycon_qname, fun_tycon_qname, list_tycon_qname,
		unit_con_name, nil_con_name]	

      name_strings = nub (map show filtered_missing_names)

  when (not (null name_strings)) $
	  tell ["Warning: " ++ show mdl ++ 
		": the following names could not be resolved:\n"++
		"   " ++ concat (map (' ':) name_strings)
		]

  return (mdl, Interface { 
		   iface_filename     = filename,
		   iface_package      = package,
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
		 (cls,[lhs]) [] |
	cls <- drv]
   where
     targs = map stripDocs (targsConstrs cons)
     -- a type variable is simple if it occurs as a data constructor argument
     simple_tvars = map HsTyVar $ Set.elems $ Set.fromList $ [tv | HsTyVar tv <- targs]
     -- a type variable is complex if it occurs inside a data constructor
     -- argument, except where the argument is identical to the lhs.
     complex_tvars = map HsTyVar $ Set.elems $ Set.unions $ map tvarsType $
			filter (/= lhs) $ filter (not . isVar) targs
     isVar (HsTyVar _) = True
     isVar _ = False
     extra_constraint
 	| null complex_tvars = []
 	| otherwise = [(unknownConstraint,complex_tvars)]
     lhs
	| n == tuple_tycon_name (length tvs - 1) =
	   HsTyTuple True (map HsTyVar tvs)
        | otherwise = foldl HsTyApp (HsTyCon (Qual mdl n)) (map HsTyVar tvs)

  -- collect type arguments of constructors
  targsConstrs :: [HsConDecl] -> [HsType]
  targsConstrs = foldr targsConstr []

  targsConstr :: HsConDecl -> [HsType] -> [HsType]
  targsConstr (HsConDecl _ _ _ _ bts _) ts = foldr targsBangType ts bts
  targsConstr (HsRecDecl _ _ _ _ fs _) ts = foldr targsField ts fs

  targsField (HsFieldDecl _ bt _) = targsBangType bt

  targsBangType (HsBangedTy t) ts = t : ts
  targsBangType (HsUnBangedTy t) ts = t : ts

  -- remove documentation comments from a type
  stripDocs :: HsType -> HsType
  stripDocs (HsForAllType n ctxt t) = HsForAllType n ctxt (stripDocs t)
  stripDocs (HsTyFun t1 t2) = HsTyFun (stripDocs t1) (stripDocs t2)
  stripDocs (HsTyTuple boxed ts) = HsTyTuple boxed (map stripDocs ts)
  stripDocs (HsTyApp t1 t2) = HsTyApp (stripDocs t1) (stripDocs t2)
  stripDocs (HsTyDoc t _) = stripDocs t
  stripDocs (HsTyIP n t) = HsTyIP n (stripDocs t)
  stripDocs t = t

  -- collect the type variables occurring free in a type
  tvarsType (HsForAllType (Just tvs) _ t) = foldl (flip Set.delete) (tvarsType t) tvs
  tvarsType (HsForAllType Nothing _ t) = tvarsType t
  tvarsType (HsTyFun t1 t2) = tvarsType t1 `Set.union` tvarsType t2
  tvarsType (HsTyTuple _ ts) = Set.unions (map tvarsType ts)
  tvarsType (HsTyApp t1 t2) = tvarsType t1 `Set.union` tvarsType t2
  tvarsType (HsTyVar tv) = Set.singleton tv
  tvarsType (HsTyCon _) = Set.empty
  tvarsType (HsTyDoc t _) = tvarsType t
  tvarsType (HsTyIP _ t) = tvarsType t

unknownConstraint :: HsQName
unknownConstraint = UnQual (HsTyClsName (HsIdent "???"))

-- -----------------------------------------------------------------------------
-- Build the list of items that will become the documentation, from the
-- export list.  At this point, the list of ExportItems is in terms of
-- original names.

mkExportItems
	:: ModuleMap
	-> Module			-- this module
	-> [HsQName]			-- exported names (orig)
	-> Map HsName HsDecl		-- maps local names to declarations
	-> Map HsName [HsName]		-- sub-map for this module
	-> [HsDecl]			-- decls in the current module
	-> [DocOption]
	-> Maybe [HsExportSpec]
	-> ErrMsgM [ExportItem]

mkExportItems mod_map this_mod exported_names decl_map sub_map decls
	 opts maybe_exps
  | Nothing <- maybe_exps	 = everything_local_exported
  | OptIgnoreExports `elem` opts = everything_local_exported
  | Just specs <- maybe_exps     = do 
	exps <- mapM lookupExport specs
        return (concat exps)
  where
    everything_local_exported =  -- everything exported
	return (fullContentsOfThisModule this_mod decls)

    lookupExport (HsEVar x)            = declWith x
    lookupExport (HsEAbs t)            = declWith t
    lookupExport (HsEThingAll t)       = declWith t
    lookupExport (HsEThingWith t cs)   = declWith t
    lookupExport (HsEModuleContents m) = fullContentsOf m
    lookupExport (HsEGroup lev doc)    = return [ ExportGroup lev "" doc ]
    lookupExport (HsEDoc doc)          = return [ ExportDoc doc ]
    lookupExport (HsEDocNamed str)
	= do r <- findNamedDoc str decls
	     case r of
		Nothing -> return []
		Just found -> return [ ExportDoc found ]
	
    declWith :: HsQName -> ErrMsgM [ ExportItem ]
    declWith (UnQual _) = return []
    declWith t@(Qual mdl x)
	| Just decl <- findDecl t
	= return [ ExportDecl t (restrictTo subs (extractDecl x mdl decl)) [] ]
	| otherwise
	= return [ ExportNoDecl t t (map (Qual mdl) subs) ]
	-- can't find the decl (it might be from another package), but let's
	-- list the entity anyway.  Later on, the renamer will change the
	-- orig name into the import name, so we get a proper link to
	-- the doc for this entity.
	where 
	      subs = map nameOfQName subs_qnames
	      subs_qnames = filter (`elem` exported_names) all_subs_qnames

	      all_subs_qnames = map (Qual mdl) all_subs

	      all_subs | mdl == this_mod = Map.findWithDefault [] x sub_map
		       | otherwise       = all_subs_of_qname mod_map t

    fullContentsOf m
	| m == this_mod  = return (fullContentsOfThisModule this_mod decls)
	| otherwise = 
	   case Map.lookup m mod_map of
	     Just iface
		| OptHide `elem` iface_options iface
			-> return (iface_orig_exports iface)
		| otherwise -> return [ ExportModule m ]
	     Nothing -> return [] -- already emitted a warning in exportedNames

    findDecl :: HsQName -> Maybe HsDecl
    findDecl (UnQual _)
	= Nothing	-- must be a name we couldn't resolve
    findDecl (Qual m n)
	| m == this_mod  = Map.lookup n decl_map
	| otherwise = 
	   case Map.lookup m mod_map of
		Just iface -> Map.lookup n (iface_decls iface)
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
  ctxt = [HsAssump (Qual mdl c, map HsTyVar tvs0)]
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

mkSubNames :: [HsDecl] -> Map HsName [HsName]
mkSubNames decls = 
  Map.fromList [ (n, subs) | d <- decls, 
		             Just n <- [declMainBinder d],
			     subs@(_:_) <- [declSubBinders d] ]

-- -----------------------------------------------------------------------------
-- Gather a list of original names exported from this module

exportedNames :: Module -> ModuleMap -> [HsName]
	-> Map HsQName HsQName
	-> Map HsName [HsName]
	-> Maybe [HsExportSpec]
	-> [DocOption]
	-> ErrMsgM ([HsQName], [HsQName])

exportedNames mdl mod_map local_names orig_env sub_map maybe_exps opts
  | Nothing <- maybe_exps 	    
	= return all_local_names_pr
  | OptIgnoreExports `elem` opts
	= return all_local_names_pr
  | Just expspecs <- maybe_exps
	= do all_names <- mapM extract expspecs
	     all_vis_names <- mapM extract_vis expspecs
	     return (concat all_names, concat all_vis_names)
 where
  all_local_names = map (Qual mdl) local_names
  all_local_names_pr = (all_local_names,all_local_names)

  in_scope = Set.fromList (Map.elems orig_env)

  extract e = 
   case e of
    HsEVar x -> return [x]
    HsEAbs t -> return [t]
    HsEThingAll t@(Qual m x) ->
	 return (t : filter (`Set.member` in_scope) (map (Qual m) all_subs))
	 where
	      all_subs | m == mdl  = Map.findWithDefault [] x sub_map
		       | otherwise = all_subs_of_qname mod_map t

    HsEThingWith t cs -> return (t : cs)
    HsEModuleContents m
	| m == mdl  -> return (map (Qual mdl) local_names)
	| otherwise ->
	  case Map.lookup m mod_map of
	    Just iface -> 
		return (filter (`Set.member` in_scope) (Map.elems (iface_env iface)))
	    Nothing    -> 
		do tell (exportModuleMissingErr mdl m)
		   return []
    _ -> return []

  -- Just the names that will be visible in the documentation
  -- (ie. omit names exported via a 'module M' export, if we are just
  -- going to cross-reference the module).
  extract_vis e = 
   case e of
    HsEModuleContents m
	| m == mdl  -> return (map (Qual mdl) local_names)
	| otherwise ->
	  case Map.lookup m mod_map of
	    Just iface
		| OptHide `elem` iface_options iface ->
		    return (filter (`Set.member` in_scope) (Map.elems (iface_env iface)))
		| otherwise -> return []
	    Nothing
		-> return []  -- we already emitted a warning above

    -- remaining cases: we have to catch names which are reexported from
    -- here, but for which we have no documentation, perhaps because they
    -- are from another package.  We have to do this by looking for
    -- the declaration in the other module.
    _ -> do xs <- extract e
	    return (filter is_documented_here xs)

  is_documented_here (UnQual _) = False
  is_documented_here (Qual m n)
    | m == mdl  = True -- well, it's not documented anywhere else!
    | otherwise =
	case Map.lookup m mod_map of
	  Nothing -> False
	  Just iface -> isJust (Map.lookup n (iface_decls iface))

exportModuleMissingErr this mdl 
  = ["Warning: in export list of " ++ show this
	 ++ ": module not found: " ++ show mdl]

-- for a given entity, find all the names it "owns" (ie. all the
-- constructors and field names of a tycon, or all the methods of a
-- class).
all_subs_of_qname :: ModuleMap -> HsQName -> [HsName]
all_subs_of_qname mod_map (Qual mdl nm) =
  case Map.lookup mdl mod_map of
	Just iface -> Map.findWithDefault [] nm (iface_sub iface)
	Nothing    -> []
all_subs_of_qname _ n@(UnQual _) =
    error $ "Main.all_subs_of_qname: unexpected unqual'd name:" ++ show n

-- ----------------------------------------------------------------------------
-- Get a list of names exported by this module that are not actually
-- documented here, and build a mapping to point to where the
-- documentation for those names can be found.  This is used for
-- constructing the iface_reexports field of the Interface.

getReExports :: Module
   -> [HsQName]   -- all exported names
   -> [HsQName]   -- exported names which are documented here
   -> Map HsQName HsQName
   -> Map HsName HsQName
getReExports mdl exported exported_visible import_env
  = Map.fromList (concat invisible_names)  
  where
   invisible_names = [ get_name n | n <- exported, 
				       n `notElem` exported_visible ]

   get_name (UnQual _) = []
   get_name n@(Qual m un) = 
	case Map.lookup n import_env of
	    Nothing -> []
	    Just n' -> [(un,n')]
 
-- ----------------------------------------------------------------------------
-- Building name environments

-- The orig env maps names in the current source file to
-- fully-qualified "original" names.

buildOrigEnv :: Module -> Bool -> ModuleMap -> [HsImportDecl]
   -> ErrMsgM (Map HsQName HsQName)
buildOrigEnv this_mdl verbose mod_map imp_decls
  = do maps <- mapM build imp_decls
       return (Map.unions (reverse maps))
  where
  build imp_decl@(HsImportDecl _ mdl qual maybe_as _)
    = case Map.lookup mdl mod_map of
       Nothing -> do 
	  when verbose $
	     -- only emit missing module messages when -v is on.  Otherwise
  	     -- we get a ton of spurious messages about missing "Prelude".
	     tell ["Warning: " ++ show this_mdl
		   ++ ": imported module not found: " ++ show mdl]
	  return Map.empty
       Just iface -> 
	  return (Map.fromList (concat (map orig_map 
			                    (processImportDecl mod_map imp_decl))))
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

buildImportEnv :: ModuleMap -> Module
        -> [HsQName]	   -- a list of names exported from here *with docs*
	-> [HsImportDecl]  -- the import decls
	-> Map HsQName HsQName
buildImportEnv mod_map this_mod exported_names imp_decls
  = foldr (flip (Map.unionWith (flip best_name))) Map.empty (map build imp_decls)
  where
	-- choose qualified results over unqualified ones.  In the future
	-- we might make more intelligent decisions about which name to
	-- link to.
	best_name n@(Qual _ _) _ = n
	best_name _ n@(Qual _ _) = n
	best_name n _ = n

	build imp_decl@(HsImportDecl _ mdl _ _ _) = 
	  case Map.lookup mdl mod_map of
       	    Nothing    -> Map.empty
            Just iface -> Map.fromList (map import_map imported_names)
	     where
	      imported_names = processImportDecl mod_map imp_decl
	      reexport_env = iface_reexported iface

	      import_map (nm,qnm) = (qnm, maps_to)
 	       where 
		maps_to
		 -- we re-export it, with docs
		 | qnm `elem` exported_names = Qual this_mod nm
		 -- re-exported from the other module, but not documented there:
		 -- find the right place using the iface_reexported environment.
		 | Just new_qnm <- Map.lookup nm reexport_env = new_qnm
		 -- if the destination is hidden, we have nowhere to link to
		 | OptHide `elem` iface_options iface  = UnQual nm
		 -- otherwise, it's documented in the other module
		 | otherwise = Qual mdl nm


processImportDecl :: ModuleMap -> HsImportDecl -> [(HsName,HsQName)]
processImportDecl mod_map (HsImportDecl _ mdl is_qualified maybe_as imp_specs)
    = case Map.lookup mdl mod_map of
       Nothing    -> []
       Just iface -> imported_names
        where
	 env = iface_env iface
	 sub = iface_sub iface

 	 all_names = Map.toAscList env

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
	  case Map.lookup nm env of
	    Just qnm -> filter (`Map.member` env) (all_subs_of_qname mod_map qnm)
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
	    ExportDecl nm decl (case Map.lookup nm inst_map of
				  Nothing -> []
				  Just instheads -> rename_insts instheads)
	attach_export other_export =
	    other_export

collectInstances
   :: [(Module,Interface)]
   -> Map HsQName [InstHead]  -- maps class/type names to instances

collectInstances mod_ifaces
  = Map.fromListWith (flip (++)) ty_inst_pairs `Map.union`
    Map.fromListWith (flip (++)) class_inst_pairs
  where
    all_instances = concat (map (iface_insts.snd) mod_ifaces)

    class_inst_pairs = [ (cls, [(ctxt,(cls,args))])
		       | HsInstDecl _ ctxt (cls,args) _ <- all_instances ]
			
    ty_inst_pairs = [ (nm, [(ctxt,(cls,args))])
		    | HsInstDecl _ ctxt (cls,args) _ <- all_instances,
		      nm <- nub (concat (map freeTyCons args))
		    ]

-- -----------------------------------------------------------------------------
-- The interface file format.
-- This has to read interfaces up to Haddock 0.6 (without the short
-- document annotations), and interfaces afterwards, so we use the
-- FormatVersion hack to work out which one the interface file contains.

thisFormatVersion :: FormatVersion
thisFormatVersion = mkFormatVersion 1

-- | How we store interfaces.  Not everything is stored.
type StoredInterface =
   (Module,Maybe Doc,Maybe String,Bool,[(HsName,HsQName)],[(HsName,HsQName)],
      [(HsName,[HsName])])

-- | How we used to store interfaces.
type NullVersionStoredInterface = 
   (Module,Maybe String,Bool,[(HsName,HsQName)],[(HsName,HsQName)],
      [(HsName,[HsName])])

dumpInterfaces :: [(Module,Interface)] -> FilePath -> IO ()
dumpInterfaces interfaces fileName =
   do
      let
         preparedInterfaces :: [StoredInterface]
         preparedInterfaces = map from_interface interfaces

      bh <- openBinMem 100000
      put_ bh thisFormatVersion
      put_ bh preparedInterfaces
      writeBinMem bh fileName


readIface :: FilePath -> IO [(Module,Interface)]
readIface fileName = do
   bh <- readBinMem fileName
   formatVersion <- get bh
   if formatVersion == thisFormatVersion
      then
         do
            (stuff :: [StoredInterface]) <- get bh
            return (map to_interface stuff)
      else
         if formatVersion == nullFormatVersion
            then
               do
                  (stuff :: [NullVersionStoredInterface]) <- get bh
                  return (map nullVersion_to_interface stuff)
               else
                  do
                     noDieMsg (
                        "Warning: The interface file " ++ show fileName 
                           ++ " could not be read.\n"
                           ++ "Maybe it's from a later version of Haddock?\n")
                     return [] 

from_interface :: (Module,Interface) -> StoredInterface
from_interface (mdl,iface) =
   (mdl, toDescription iface,iface_package iface,
      OptHide `elem` iface_options iface,
      Map.toAscList (iface_env iface), 
      Map.toAscList (iface_reexported iface),
      Map.toAscList (iface_sub iface)
      )

to_interface :: StoredInterface -> (Module,Interface)
to_interface (mdl,descriptionOpt,package, hide, env, reexported, sub) = 
   (mdl, Interface { 
      iface_filename     = "",
      iface_package      = package,
      iface_env          = Map.fromList env,
      iface_import_env   = Map.empty,
      iface_sub          = Map.fromList sub,
      iface_reexported   = Map.fromList reexported,
      iface_exports      = [],
      iface_orig_exports = [],
      iface_insts        = [],
      iface_decls        = Map.empty,
      iface_info         = toModuleInfo descriptionOpt,
      iface_doc          = Nothing,
      iface_options      = if hide then [OptHide] else []
      })

nullVersion_to_interface :: NullVersionStoredInterface -> (Module,Interface)
nullVersion_to_interface (mdl, package, hide, env, reexported, sub) = 
   (mdl, Interface { 
      iface_filename     = "",
      iface_package      = package,
      iface_env          = Map.fromList env,
      iface_import_env   = Map.empty,
      iface_sub          = Map.fromList sub,
      iface_reexported   = Map.fromList reexported,
      iface_exports      = [],
      iface_orig_exports = [],
      iface_insts        = [],
      iface_decls        = Map.empty,
      iface_info         = emptyModuleInfo,
      iface_doc          = Nothing,
      iface_options      = if hide then [OptHide] else []
      })

toModuleInfo :: Maybe Doc -> ModuleInfo
toModuleInfo descriptionOpt = 
   emptyModuleInfo {description = descriptionOpt}


 
-- -----------------------------------------------------------------------------
-- A monad which collects error messages

type ErrMsg = String
type ErrMsgM a = Writer [ErrMsg] a

getBaseDir :: IO (Maybe String)
#if defined(mingw32_HOST_OS)
getBaseDir = do let len = (2048::Int) -- plenty, PATH_MAX is 512 under Win32.
		buf <- mallocArray len
                ret <- getModuleFileName nullPtr buf len
                if ret == 0 then free buf >> return Nothing
                            else do s <- peekCString buf
                                    free buf
                                    return (Just (rootDir s))
  where
    rootDir s = reverse (dropList "/haddock.exe" (reverse (normalisePath s)))

foreign import stdcall unsafe "GetModuleFileNameA" 
  getModuleFileName :: Ptr () -> CString -> Int -> IO Int32
#else
getBaseDir :: IO (Maybe String) = do return Nothing
#endif
normalisePath :: String -> String
-- Just changes '\' to '/'

#if defined(mingw32_HOST_OS)
normalisePath xs = subst '\\' '/' xs
subst a b ls = map (\ x -> if x == a then b else x) ls
#else
normalisePath xs   = xs
#endif
dropList :: [b] -> [a] -> [a]
dropList [] xs    = xs
dropList _  xs@[] = xs
dropList (_:xs) (_:ys) = dropList xs ys
