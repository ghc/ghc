--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

module Main (main) where

import HaddockRename
import HaddockParse
--import HaddockDB   -- not compiling
import HaddockHtml
import HaddockTypes
import HaddockUtil
import Digraph

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
import IOExts

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
  | Flag_SourceURL String
  | Flag_CSS String
  | Flag_Lib String
  | Flag_OutputDir FilePath
  deriving (Eq)

options =
  [ 
    Option ['d']  ["docbook"]  (NoArg Flag_DocBook)
	"output in docbook (SGML)",
    Option ['h']  ["html"]     (NoArg Flag_Html)
	"output in HTML",
    Option ['o']  ["odir"]     (ReqArg Flag_OutputDir "DIR")
	"directory in which to put the output files",
    Option ['s']  ["source"]   (ReqArg Flag_SourceURL "URL") 
	"base URL for links to source code",
    Option ['t']  ["title"]  (ReqArg Flag_Heading "TITLE")
	"page heading",
    Option ['v']  ["verbose"]  (NoArg Flag_Verbose)
	"be verbose",
    Option []  ["css"]         (ReqArg Flag_CSS "FILE") 
	"The CSS file to use for HTML output",
    Option []  ["lib"]         (ReqArg Flag_Lib "DIR") 
	"Directory containing Haddock's auxiliary files"
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

  writeIORef saved_flags flags
  parsed_mods <- sequence (map parse_file files)

  sorted_mods <- sortModules parsed_mods
	-- emits an error message if there are recursive modules

  -- process the modules in sorted order, building up a mapping from
  -- modules to interfaces.
  let 
	loop ifaces [] _ = return ifaces
	loop ifaces (hsmod:hsmods) (file:files) = do 
	   let ((mod,iface),msgs) = runWriter (mkInterface ifaces file hsmod)
	       new_ifaces = addToFM ifaces mod iface
	   mapM (hPutStrLn stderr) msgs
	   loop new_ifaces hsmods files    

  module_map <- loop emptyFM sorted_mods files
  let mod_ifaces = fmToList module_map

--  when (Flag_DocBook `elem` flags) $
--    putStr (ppDocBook odir mod_ifaces)

  when (Flag_Html `elem` flags) $
    ppHtml title source_url mod_ifaces odir css_file libdir


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

-----------------------------------------------------------------------------
-- Figuring out the definitions that are exported from a module

mkInterface
   :: ModuleMap -> FilePath -> HsModule
   -> ErrMsgM (
	       Module, 		-- the module name
	       Interface	-- its "interface"
	      )

mkInterface mod_map filename 
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

     local_env = listToFM (zip unqual_local_names qual_local_names ++
			   zip qual_local_names   qual_local_names)
	 -- both qualified and unqualifed names are in scope for local things

     -- build the orig_env, which maps names to *original* names (so we can
     -- find the original declarations & docs for things).
     (ext_orig_envs, ext_import_envs) 
	= unzip (map (buildEnv mod_map mod exported_names) imps)
     orig_env   = foldr plusFM local_env ext_orig_envs  
     import_env = foldr plusFM local_env ext_import_envs  

     -- convert names in source code to original, fully qualified, names
     (orig_exports, missing_names1) 
	= runRnFM orig_env (mapMaybeM renameExportList exps)

     (orig_decls, missing_names2)
	= runRnFM orig_env (mapM renameDecl annotated_decls)

     orig_decl_map :: FiniteMap HsName HsDecl
     orig_decl_map = listToFM [ (n,d) | d <- orig_decls, n <- declBinders d ]

     -- gather up a list of entities that are exported (original names)
     exported_names = exportedNames mod mod_map orig_decls
			locally_defined_names orig_exports
			orig_decl_map options

  let
     final_decls = concat (map expandDecl orig_decls)

     decl_map :: FiniteMap HsName HsDecl
     decl_map = listToFM [ (n,d) | d <- final_decls, n <- declBinders d ]

  -- make the "export items", which will be converted into docs later
  orig_export_list <- mkExportItems mod_map mod orig_env
   			 decl_map final_decls options orig_exports

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

  -- report any names we couldn't find/resolve

  let missing_names = missing_names1 ++ missing_names2
			 --ignore missing_names3 for now,
      name_strings = nub (map show missing_names)

  when (not (null name_strings)) $
	  tell ["Warning: in module " ++ show mod ++ 
		", the following names could not be resolved:\n\ 
		\   " ++ concat (map (' ':) name_strings)
		]

  return (mod, Interface { 
		   iface_filename     = filename,
		   iface_env          = name_env,
		   iface_exports      = renamed_export_list,
		   iface_orig_exports = pruned_export_list,
		   iface_decls        = decl_map,
		   iface_info	      = maybe_info,
		   iface_doc          = maybe_doc,
		   iface_options      = options
		}
      	  )

-- -----------------------------------------------------------------------------
-- Build the list of items that will become the documentation, from the
-- export list.  At the same time we rename *original* names in the declarations
-- to *imported* names.

mkExportItems :: ModuleMap -> Module
	-> FiniteMap HsQName HsQName	-- maps orig to imported names
	-> FiniteMap HsName HsDecl	-- maps local names to declarations
	-> [HsDecl]			-- decls in the current module
	-> [DocOption]
	-> Maybe [HsExportSpec]
	-> ErrMsgM [ExportItem]
mkExportItems mod_map mod env decl_map decls options maybe_exps
  | Nothing <- maybe_exps	    = everything_local_exported
  | OptIgnoreExports `elem` options = everything_local_exported
  | Just specs <- maybe_exps = do 
	exps <- mapM lookupExport specs
        return (concat exps)
  where

    everything_local_exported =
	fullContentsOfThisModule mod decls env -- everything exported

    lookupExport (HsEVar x) 
	| Just decl <- findDecl x
	= let decl' | HsTypeSig loc ns ty doc <- decl
			= HsTypeSig loc [nameOfQName x] ty doc
		    | otherwise
		  	= decl
	  in
	  return [ ExportDecl decl' ]
	  -- ToDo: cope with record selectors here
    lookupExport (HsEAbs t)
	| Just decl <- findDecl t
	= return [ ExportDecl (restrictTo [] decl) ]
    lookupExport (HsEThingAll t)
	| Just decl <- findDecl t
	= return [ ExportDecl decl ]
    lookupExport (HsEThingWith t cs)
	| Just decl <- findDecl t
	= return [ ExportDecl (restrictTo (map nameOfQName cs) decl) ]
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
	| m == mod  = fullContentsOfThisModule mod decls env
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

fullContentsOfThisModule mod decls env = 
  mapM mkExportItem (filter keepDecl decls)
  where mkExportItem (HsDocGroup loc lev doc) =
	   return (ExportGroup lev "" doc)
	mkExportItem decl = 
	   return (ExportDecl decl)

keepDecl HsTypeSig{}     = True
keepDecl HsTypeDecl{}    = True
keepDecl HsNewTypeDecl{} = True
keepDecl HsDataDecl{}    = True
keepDecl HsClassDecl{}   = True
keepDecl HsDocGroup{}	 = True
keepDecl _ = False

-- -----------------------------------------------------------------------------
-- Pruning

pruneExportItems :: [ExportItem] -> [ExportItem]
pruneExportItems items = filter has_doc items
  where has_doc (ExportDecl d) = isJust (declDoc d)
	has_doc _ = True

-- -----------------------------------------------------------------------------
-- Gather a list of original names exported from this module

exportedNames :: Module -> ModuleMap -> [HsDecl] -> [HsName]
	-> Maybe [HsExportSpec]
	-> FiniteMap HsName HsDecl
	-> [DocOption]
	-> [HsQName]

exportedNames mod mod_scope decls local_names maybe_exps decl_map options
  | Nothing <- maybe_exps 	    = all_local_names
  | OptIgnoreExports `elem` options = all_local_names
  | Just expspecs <- maybe_exps     = concat (map extract expspecs)
 where
  all_local_names = map (Qual mod) local_names

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
	  case lookupFM mod_scope m of
	    Just iface -> eltsFM (iface_env iface)
	    Nothing    -> trace ("Warning: module not found: " ++ show m) $ []
    _ -> []

  export_lookup :: HsQName -> Maybe HsDecl
  export_lookup (UnQual n)
	= trace ("Warning(exportedNames): UnQual! " ++ show n) $ Nothing
  export_lookup (Qual m n)
	| m == mod  = lookupFM decl_map n
	| otherwise
	    = case lookupFM mod_scope m of
		Just iface -> lookupFM (iface_decls iface) n
		Nothing    -> trace ("Warning: module not found: " ++ show m) 
				Nothing

-- -----------------------------------------------------------------------------
-- Building name environments

buildEnv :: ModuleMap -> Module -> [HsQName] -> HsImportDecl
   -> ( FiniteMap HsQName HsQName, 	-- source name ==> orig name
        FiniteMap HsQName HsQName	-- orig name ==> import name
      )
buildEnv mod_map this_mod exported_names (HsImportDecl _ mod qual maybe_as _)
   = case lookupFM mod_map mod of
       Nothing    -> trace ("Warning: module not found: " ++ show mod) 
			(emptyFM, emptyFM)
       Just iface -> 
	  let env = fmToList (iface_env iface) in
	  ( listToFM (concat (map orig_map env))
	  , listToFM (map import_map env)
  	  )
  where
	-- bring both qualified and unqualified names into scope, unless
	-- the import was 'qualified'.
     orig_map (nm,qnm)
	| qual      = [ (Qual qual_module nm, qnm) ]
	| otherwise = [ (UnQual nm, qnm), (Qual qual_module nm, qnm) ]

     qual_module
	| Just m <- maybe_as = m
	| otherwise          = mod

     import_map (nm,qnm) = (qnm, maps_to)
	where maps_to | qnm `elem` exported_names = Qual this_mod nm
		      | otherwise = Qual mod nm

-- -----------------------------------------------------------------------------
-- Expand multiple type signatures

expandDecl :: HsDecl -> [HsDecl]
expandDecl (HsTypeSig loc fs qt doc) = [ HsTypeSig loc [f] qt doc | f <- fs ]
expandDecl (HsClassDecl loc ty fds decls doc)
  = [ HsClassDecl loc ty fds (concat (map expandDecl decls)) doc ]
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
	  HsClassDecl loc ty fds meths _ -> 
		HsClassDecl loc ty fds meths (Just doc)
	  HsTypeSig loc ns ty _ -> 
		HsTypeSig loc ns ty (Just doc)
	  HsForeignImport loc cc sf str n ty _ ->
		HsForeignImport loc cc sf str n ty (Just doc)
	  _other -> d

collectInDecl (HsClassDecl loc ty fds meths doc)
  = HsClassDecl loc ty fds (collect Nothing DocEmpty meths) doc
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

sortModules :: [HsModule] -> IO [HsModule]
sortModules hsmodules = mapM for_each_scc sccs
  where
	sccs = stronglyConnComp edges

	edges :: [(HsModule, Module, [Module])]
	edges = [ (hsmod, mod, [ imp | HsImportDecl _ imp _ _ _ <- impdecls ]) 
		| hsmod@(HsModule mod _ impdecls _ _ _ _) <- hsmodules
		]

	for_each_scc (AcyclicSCC hsmodule) = return hsmodule
	for_each_scc (CyclicSCC  hsmodules) = 
	   dieMsg ("modules are recursive: " ++ 
		   unwords (map show [ mod | HsModule mod _ _ _ _ _ _
						<- hsmodules ]))

-- -----------------------------------------------------------------------------
-- A monad which collects error messages

type ErrMsg = String
type ErrMsgM a = Writer [ErrMsg] a

