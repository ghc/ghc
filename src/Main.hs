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
import Data.List ( nub, (\\), foldl' )
import Data.Maybe ( isJust, isNothing, maybeToList )
--import Debug.Trace
import System.Console.GetOpt ( getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..) )
import System.Environment ( getArgs )
import System.IO ( stderr, IOMode(..), openFile, hClose, hGetContents, hPutStrLn )
#if defined(mingw32_HOST_OS)
import Foreign
import Foreign.C
#endif

#if __GLASGOW_HASKELL__ >= 603
import System.Process
import System.Exit
import Control.Exception	( Exception(..), throwIO, catch )
import Prelude hiding (catch)
import System.Directory		( doesDirectoryExist, doesFileExist )
import Control.Concurrent
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
  | Flag_IgnoreAllExports
  | Flag_HideModule String
  | Flag_UsePackage String
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
	"generate an HTML index from specified interfaces",
    Option [] ["ignore-all-exports"] (NoArg Flag_IgnoreAllExports)
	"behave as if all modules have the ignore-exports atribute",
    Option [] ["hide"] (ReqArg Flag_HideModule "M")
	"behave as if module M has the hide attribute",
    Option [] ["use-package"] (ReqArg Flag_UsePackage "P")
	"the modules being processed depend on package P"
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

      verbose = Flag_Verbose `elem` flags

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

      read_iface_flags = [ parseIfaceOption str 
		       | Flag_ReadInterface str <- flags ]

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

  -- grok the --use-package flags
  pkg_ifaces_to_read <- getPackageIfaces flags verbose

  let ifaces_to_read = read_iface_flags ++ pkg_ifaces_to_read

  read_iface_stuff <- mapM readIface (map snd ifaces_to_read)

  let 
      (read_ifacess, doc_envs) = unzip read_iface_stuff
      read_ifaces = concat read_ifacess

      ext_doc_env = Map.unions doc_envs
      
      visible_read_ifaces = filter ((OptHide `notElem`) . iface_options) 
				read_ifaces
      external_mods = map iface_module read_ifaces
      pkg_paths = map fst ifaces_to_read

  updateHTMLXRefs pkg_paths read_ifacess

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
	loop mod_env ifaces [] = return (reverse ifaces)
	loop mod_env ifaces ((hsmod,file):mdls)  = do 
	   let (iface,msgs) = runWriter $
		   mkInterfacePhase1 flags verbose mod_env file package hsmod
	       new_mod_env = Map.insert (iface_module iface) iface mod_env
	   mapM_ (hPutStrLn stderr) msgs
	   loop new_mod_env (iface:ifaces) mdls

  let 
	mod_map = Map.fromList [ (iface_module iface,iface) 
			       | iface <- read_ifaces ]
  
  ifaces <- loop mod_map read_ifaces sorted_mod_files
  let 
      these_ifaces0 = [ iface | iface <- ifaces,
		    	        iface_module iface `notElem` external_mods ]

  let these_ifaces1  = attachInstances these_ifaces0
      this_doc_env   = buildGlobalDocEnv these_ifaces1
      global_doc_env = this_doc_env `Map.union`
		       ext_doc_env `Map.union`
		       builtinDocEnv


--  Now do phase 2
  let
	loop2 ifaces [] = return (reverse ifaces)
	loop2 ifaces (iface:rest) = do
	   let (iface',msgs) = runWriter $
		    mkInterfacePhase2 verbose iface global_doc_env
	   mapM_ (hPutStrLn stderr) msgs
	   loop2 (iface':ifaces) rest

  these_ifaces <- loop2 [] these_ifaces1  

--  when (Flag_DocBook `elem` flags) $
--    putStr (ppDocBook odir mod_ifaces)


  when (Flag_Debug `elem` flags) $ do
    mapM_ putStrLn (map show [ (iface_module i, 
				     Map.toAscList (iface_env i), 
				     Map.toAscList (iface_sub i))
			     | i <-  these_ifaces ])

  when (Flag_Html `elem` flags) $ do
    ppHtml title package source_url these_ifaces odir
		prologue maybe_html_help_format
		maybe_contents_url maybe_index_url
    copyHtmlBits odir libdir css_file

  -- dump an interface if requested
  case dump_iface of
     Nothing -> return ()
     Just fn -> dumpInterfaces these_ifaces this_doc_env fn

parseIfaceOption :: String -> (FilePath,FilePath)
parseIfaceOption s = 
  case break (==',') s of
	(fpath,',':file) -> (fpath,file)
	(file, _)        -> ("", file)
		

updateHTMLXRefs :: [FilePath] -> [[Interface]] -> IO ()
updateHTMLXRefs paths ifaces_s =
  writeIORef html_xrefs_ref (Map.fromList mapping)
 where
  mapping = [ (iface_module iface, fpath)
	    | (fpath, ifaces) <- zip paths ifaces_s,
	      iface <- ifaces
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

-- ---------------------------------------------------------------------------
-- External packages

getPackageIfaces :: [Flag] -> Bool -> IO [(String,String)]
getPackageIfaces flags verbose =
  let
	pkgs = [pkg | Flag_UsePackage pkg <- flags]
  in
#if __GLASGOW_HASKELL__ < 603
  if (not (null pkgs))
	then die ("-use-package not supported; recompile Haddock with GHC 6.4 or later")
	else return ()
#else
  do
    mb_iface_details <- mapM getPkgIface pkgs
    return [ ok | Just ok <- mb_iface_details ]
 where
  hc_pkg = "ghc-pkg"  -- ToDo: flag

  getPkgIface pkg = do
	when verbose $
	   putStrLn ("querying ghc-pkg for " ++ pkg ++ "...")
        getPkgIface' pkg
	   `catch` (\e -> do
		  putStrLn ("Warning: cannot use package " ++ pkg ++ ":")
		  putStrLn ("   " ++ show e)
		  return Nothing)

  getPkgIface' pkg = do
	(hin,hout,herr,p) <- runInteractiveProcess hc_pkg 
				["field", "haddock-interfaces", pkg]
				Nothing Nothing
	hClose hin
	out <- hGetContents hout
	forkIO (hGetContents herr >> return ()) -- just sink the stderr
	r <- waitForProcess p
	when (r /= ExitSuccess) $
	   throwIO (ErrorCall ("ghc-pkg failed"))
	let iface = dropWhile isSpace (tail (dropWhile (/=':') out))

	(hin,hout,herr,p) <- runInteractiveProcess hc_pkg
	  			["field", "haddock-html", pkg]
				Nothing Nothing
	hClose hin
	forkIO (hGetContents herr >> return ()) -- just sink the stderr
	out <- hGetContents hout
	r <- waitForProcess p
	when (r /= ExitSuccess) $
	   throwIO (ErrorCall ("ghc-pkg failed"))
	let html = dropWhile isSpace (tail (dropWhile (/=':') out))

	when verbose $ 
	   putStrLn ("   interface: " ++ iface ++ "\n   html: " ++ html)

	iface_exists <- doesFileExist iface
	when (not iface_exists) $ do
	   throwIO (ErrorCall ("interface " ++ iface ++ " does not exist."))
	html_exists <- doesDirectoryExist html
	when (not html_exists) $ do
	   throwIO (ErrorCall ("HTML directory " ++ html ++ " does not exist."))
	return (Just (iface, html))
#endif

-----------------------------------------------------------------------------
-- Figuring out the definitions that are exported from a module

-- We're going to make interfaces in two passes:
--
--   1. Rename the code.  This basically involves resolving all
--      the names to "original names".
--
--   2. Convert all the entity references to "doc names".  These are
--      the names we want to link to in the documentation.

mkInterfacePhase1
   :: [Flag]
   -> Bool				-- verbose
   -> ModuleMap -> FilePath -> Maybe String -> HsModule
   -> ErrMsgM Interface			-- the "interface" of the module

mkInterfacePhase1 flags verbose mod_map filename package
	(HsModule mdl exps imps decls maybe_opts maybe_info maybe_doc) = do  

  let
      no_implicit_prelude = Flag_NoImplicitPrelude `elem` flags
      ignore_all_exports = Flag_IgnoreAllExports `elem` flags

  -- Process the options, if available
  opts0 <- case maybe_opts of
		Just opt_str -> processOptions opt_str
		Nothing      -> return []
  let
	-- check for a --hide option
	Module mod_str = mdl
	opts
	  | Flag_HideModule mod_str `elem` flags = OptHide : opts0
	  | otherwise			         = opts0

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

     (orig_module_doc, missing_names3)
        = runRnFM orig_env (renameMaybeDoc maybe_doc)

     decl_map :: Map HsName HsDecl
     decl_map = Map.fromList [ (n,d) | d <- orig_decls, n <- declBinders d ]

     instances = [ d | d@HsInstDecl{} <- orig_decls ] ++
		 [ d | decl <- orig_decls, d <- derivedInstances mdl decl]

  -- trace (show (Map.toAscList orig_env)) $ do

     -- gather up a list of entities that are exported (original names)
  (exported_names, exported_visible_names)
	 <- exportedNames mdl mod_map
			locally_defined_names orig_env sub_map
			orig_exports opts

  let
     -- maps exported HsNames to orig HsQNames
     name_env = Map.fromList [ (nameOfQName n, n) | n <- exported_names ]

     -- find the names exported by this module that other modules should *not*
     -- link to.
     reexports = [ nm | n@(Qual _ nm) <- exported_names, 
			n `notElem` exported_visible_names ]

  -- in

  -- make the "export items", which will be converted into docs later
  orig_export_items <- mkExportItems mod_map mdl exported_names decl_map sub_map
			 		orig_decls opts orig_exports 
					ignore_all_exports
  let
     -- prune the export list to just those declarations that have
     -- documentation, if the 'prune' option is on.
     pruned_export_list
	| OptPrune `elem` opts = pruneExportItems orig_export_items
	| otherwise = orig_export_items
  -- in

  -- report any names we couldn't find/resolve
  let
      missing_names = missing_names1 ++ missing_names2 ++ missing_names3
			 --ignore missing_names3 & missing_names5 for now
      filtered_missing_names = filter (`notElem` builtinNames) missing_names

      name_strings = nub (map show filtered_missing_names)
  -- in

  when (OptHide `notElem` opts &&
	not (null name_strings)) $
	  tell ["Warning: " ++ show mdl ++ 
		": the following names could not be resolved:\n"++
		"   " ++ concat (map (' ':) name_strings)
		]

  return (Interface { 
		   iface_filename     = filename,
		   iface_module	      = mdl,
		   iface_package      = package,
		   iface_env          = name_env,
		   iface_reexported   = reexports,
		   iface_sub	      = sub_map,
		   iface_orig_exports = pruned_export_list,
		   iface_decls        = decl_map,
		   iface_info	      = maybe_info,
		   iface_doc          = orig_module_doc,
		   iface_options      = opts,
		   iface_exports      = error "iface_exports",
		   iface_insts	      = instances
		}
      	  )

-- -----------------------------------------------------------------------------
-- Phase 2

mkInterfacePhase2
   :: Bool			-- verbose
   -> Interface
   -> Map HsQName HsQName	-- global doc-name mapping
   -> ErrMsgM Interface

mkInterfacePhase2 verbose iface gbl_doc_env =
  case iface of {
    Interface {
	iface_module = this_mdl,
	iface_env = env,
	iface_reexported = reexports,
	iface_orig_exports = orig_export_items,
	iface_doc = orig_module_doc } ->

   let
     exported_visible_names = 
	[orig | (nm,orig) <- Map.toAscList env, nm `notElem` reexports ]

     -- build the import_env.
     import_env = foldl fn gbl_doc_env exported_visible_names
	where fn env qnm@(Qual _ nm) = Map.insert qnm (Qual this_mdl nm) env
	      fn env (UnQual nm) = env

     -- rename names in the exported declarations to point to things that
     -- are closer, or maybe even exported by, the current module.
     (renamed_export_list, missing_names1)
        = runRnUnqualFM import_env (renameExportItems orig_export_items)

     (final_module_doc, missing_names2)
        = runRnUnqualFM import_env (renameMaybeDoc orig_module_doc)

	-- we're only interested in reporting missing *qualfied*
	-- names, the unqualified ones are the ones that couldn't
	-- be resolved in phase 1 and have already been reported.
     filtered_missing_names = 
	filter isQual (missing_names1 ++ missing_names2)
	where isQual (Qual _ _) = True
	      isQual _ = False

     missing_names = map show (nub filtered_missing_names)
   in do

	-- report things that we couldn't link to.  Only do this
	-- for non-hidden modules.
   when (OptHide `notElem` iface_options iface &&
	 not (null missing_names)) $
	  tell ["Warning: " ++ show this_mdl ++ 
		": could not find link destinations for:\n"++
		"   " ++ concat (map (' ':) missing_names)
		]

   --  trace (show (Map.toAscList import_env)) $ do

   return iface{ iface_exports = renamed_export_list,
	  	 iface_doc = final_module_doc }
 }

-- -----------------------------------------------------------------------------

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
	-> Bool				-- --ignore-all-exports flag
	-> ErrMsgM [ExportItem]

mkExportItems mod_map this_mod exported_names decl_map sub_map decls
	 opts maybe_exps ignore_all_exports
  | isNothing maybe_exps
    || ignore_all_exports
    || OptIgnoreExports `elem` opts
     = everything_local_exported
  | Just specs <- maybe_exps
     = do 
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

-- | Build a mapping which for each original name, points to the "best"
-- place to link to in the documentation.  For the definition of
-- "best", we use "the module nearest the bottom of the dependency
-- graph which exports this name", not including hidden modules.  When
-- there are multiple choices, we pick a random one.
-- 
-- The interfaces are passed in in topologically sorted order, but we start
-- by reversing the list so we can do a foldl.
-- 
buildGlobalDocEnv :: [Interface] -> Map HsQName HsQName
buildGlobalDocEnv ifaces
 = foldl upd Map.empty (reverse ifaces)
 where
  upd old_env iface
     | OptHide `elem` iface_options iface
     = old_env
     | OptNotDefinitive `elem` iface_options iface
     = foldl' keep_old old_env exported_names
     | otherwise
     = foldl' keep_new old_env exported_names
     where
	mdl = iface_module iface
	exported_names = filter not_reexported (Map.elems (iface_env iface))

	not_reexported (Qual _ n) = n `notElem` iface_reexported iface
	not_reexported (UnQual n) = n `notElem` iface_reexported iface
		-- UnQual probably shouldn't happen

	keep_old env qnm = Map.insertWith const qnm (Qual mdl nm) env
		where nm = nameOfQName qnm
	keep_new env qnm = Map.insert qnm (Qual mdl nm) env 
		where nm = nameOfQName qnm

builtinDocEnv = Map.fromList (map (\a -> (a,a)) builtinNames)

-- These names cannot be explicitly exported, so we need to treat
-- them specially.
builtinNames = 
     [unit_tycon_qname, fun_tycon_qname, list_tycon_qname,
      unit_con_name, nil_con_name]	

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

attachInstances :: [Interface] -> [Interface]
attachInstances mod_ifaces
  = map attach mod_ifaces
  where
  inst_map = collectInstances mod_ifaces

  attach iface = iface{ iface_orig_exports = new_exports }
   where
	new_exports = map attach_export (iface_orig_exports iface)

	attach_export (ExportDecl nm decl _) =
	    ExportDecl nm decl (case Map.lookup nm inst_map of
				  Nothing -> []
				  Just instheads -> instheads)
	attach_export other_export =
	    other_export

collectInstances 
   :: [Interface]
   -> Map HsQName [InstHead]  -- maps class/type names to instances

collectInstances ifaces
  = Map.fromListWith (flip (++)) ty_inst_pairs `Map.union`
    Map.fromListWith (flip (++)) class_inst_pairs
  where
    all_instances = concat (map iface_insts ifaces)

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
thisFormatVersion = mkFormatVersion 2

-- | How we store interfaces.  Not everything is stored.
type StoredInterface2 =
   (Module,Maybe Doc,Maybe String,Bool,[(HsName,Module)], [(HsName,[HsName])])

-- | How we store interfaces.  Not everything is stored.
type StoredInterface1 =
   (Module,Maybe Doc,Maybe String,Bool,[(HsName,HsQName)],[(HsName,HsQName)],
      [(HsName,[HsName])])

-- | How we used to store interfaces.
type NullVersionStoredInterface = 
   (Module,Maybe String,Bool,[(HsName,HsQName)],[(HsName,HsQName)],
      [(HsName,[HsName])])

dumpInterfaces :: [Interface] -> Map HsQName HsQName -> FilePath -> IO ()
dumpInterfaces interfaces global_doc_env fileName =
   do
      let
         preparedInterfaces :: [StoredInterface2]
         preparedInterfaces = map from_interface interfaces

      bh <- openBinMem 100000
      put_ bh thisFormatVersion
      put_ bh preparedInterfaces
      putDocEnv bh global_doc_env
      writeBinMem bh fileName


readIface :: FilePath -> IO ([Interface], Map HsQName HsQName)
readIface fileName = do
   bh <- readBinMem fileName
   formatVersion <- get bh
   case formatVersion of
     v | v == thisFormatVersion -> do
            (stuff :: [StoredInterface2]) <- get bh
	    doc_env <- getDocEnv bh
            return (map to_interface2 stuff, doc_env)
     v | v == mkFormatVersion 1 -> do
            (stuff :: [StoredInterface1]) <- get bh
            return (map to_interface1 stuff, Map.empty)
     v | v == nullFormatVersion -> do
            (stuff :: [NullVersionStoredInterface]) <- get bh
            return (map nullVersion_to_interface stuff, Map.empty)
     otherwise -> do
            noDieMsg (
               "Warning: The interface file " ++ show fileName 
                  ++ " could not be read.\n"
                  ++ "Maybe it's from a later version of Haddock?\n")
            return ([], Map.empty)

from_interface :: Interface -> StoredInterface2
from_interface iface =
   (  iface_module iface,
      toDescription iface,iface_package iface,
      OptHide `elem` iface_options iface,
      [(n,mdl) | (n,Qual mdl n') <- Map.toAscList (iface_env iface),
		 if n /= n' then error "help!" else True], 
      Map.toAscList (iface_sub iface)
      )

getDocEnv :: BinHandle -> IO (Map HsQName HsQName)
getDocEnv bh = do
   doc_env_list <- get bh
   return (Map.fromList [(Qual mdl1 nm,Qual mdl2 nm) | 
			 (mdl1,nm,mdl2) <- doc_env_list])

putDocEnv :: BinHandle -> Map HsQName HsQName -> IO ()
putDocEnv bh env = do
   let doc_env_list = 
	 [(mdl1,nm,mdl2) | (Qual mdl1 nm, Qual mdl2 _) <- Map.toAscList env]
   put_ bh doc_env_list
  

to_interface1 :: StoredInterface1 -> Interface
to_interface1 (mdl,descriptionOpt,package, hide, env, _, sub) = 
   Interface { 
      iface_module	 = mdl,
      iface_filename     = "",
      iface_package      = package,
      iface_env          = Map.fromList env,
      iface_sub          = Map.fromList sub,
      iface_reexported   = [],
      iface_exports      = [],
      iface_orig_exports = [],
      iface_insts        = [],
      iface_decls        = Map.empty,
      iface_info         = toModuleInfo descriptionOpt,
      iface_doc          = Nothing,
      iface_options      = if hide then [OptHide] else []
      }

to_interface2 :: StoredInterface2 -> Interface
to_interface2 (mdl,descriptionOpt,package, hide, env, sub) =
   Interface { 
      iface_module	 = mdl,
      iface_filename     = "",
      iface_package      = package,
      iface_env          = 
	Map.fromList [(n,Qual mdl n) | (n,mdl) <- env],
      iface_sub          = Map.fromList sub,
      iface_reexported   = [],
      iface_exports      = [],
      iface_orig_exports = [],
      iface_insts        = [],
      iface_decls        = Map.empty,
      iface_info         = toModuleInfo descriptionOpt,
      iface_doc          = Nothing,
      iface_options      = if hide then [OptHide] else []
      }

nullVersion_to_interface :: NullVersionStoredInterface -> Interface
nullVersion_to_interface (mdl, package, hide, env, reexported, sub) = 
   Interface { 
      iface_module	 = mdl,
      iface_filename     = "",
      iface_package      = package,
      iface_env          = Map.fromList env,
      iface_sub          = Map.fromList sub,
      iface_reexported   = [],
      iface_exports      = [],
      iface_orig_exports = [],
      iface_insts        = [],
      iface_decls        = Map.empty,
      iface_info         = emptyModuleInfo,
      iface_doc          = Nothing,
      iface_options      = if hide then [OptHide] else []
      }

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
