--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

module Main (main) where

import HaddockRename
import HaddockParse
import HaddockLex
import HaddockDB
import HaddockHtml
import HaddockTypes
import HaddockUtil
import Digraph

import HsLexer hiding (Token)
import HsParser
import HsParseMonad
import HsSyn
import GetOpt
import System
import FiniteMap

--import Pretty

import RegexString
import Maybe	( maybeToList )
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

  when (Flag_DocBook `elem` flags) $
    putStr (ppDocBook odir mod_ifaces)

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
	(HsModule mod exps imps decls maybe_opts maybe_doc) = do  

  -- Process the options, if available
  options <- case maybe_opts of
		Just opt_str -> processOptions opt_str
		Nothing      -> return []

  let
     locally_defined_names = collectNames decls

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
	= runRnFM orig_env (mapM renameDecl decls)

     orig_decl_map :: FiniteMap HsName HsDecl
     orig_decl_map = listToFM [ (n,d) | d <- orig_decls, n <- declBinders d ]

     -- gather up a list of entities that are exported (original names)
     exported_names = exportedNames mod mod_map orig_decls
			locally_defined_names orig_exports
			orig_decl_map options

  -- Parse the module header
  (module_doc, maybe_info, missing_names_doc1) <-
       case maybe_doc of
	 Nothing  -> return (Nothing, Nothing, [])
	 Just doc -> do
	    let (doc1, maybe_info) = parseModuleHeader doc
	    (doc2,ns) <- formatDocString mod (lookupForDoc import_env) doc1
	    return (Just doc2, maybe_info, ns)

  let
     final_decls = concat (map expandDecl orig_decls)

     -- match documentation to names, and resolve identifiers in the 
     -- documentation
     local_docstrings :: [(HsName,DocString)]
     local_docstrings = collectDoc final_decls

     formatLocalDoc (n,doc) = do
	doc' <- formatDocString mod (lookupForDoc orig_env) doc
	return (n,doc')

  local_docs_formatted <- mapM formatLocalDoc local_docstrings

  let
     local_docs :: [(HsName,Doc)]		-- with *original* names
     local_docs = [ (n,doc) | (n,(doc,_)) <- local_docs_formatted ]

     -- collect the list of names which we couldn't resolve in the documentation
     missing_names_doc2 = concat [ ns | (n,(doc,ns)) <- local_docs_formatted ]

     -- get the documentation associated with entities exported from this module
     -- ToDo: we should really store the documentation in both orig and imported
     -- forms, like the export items.
     doc_map :: FiniteMap HsName Doc	-- with *imported* names
     doc_map = listToFM 
       [ (nameOfQName n, doc)
       | n <- exported_names,
         Just doc <- [lookupDoc mod_map mod local_docs import_env n] ]

     decl_map :: FiniteMap HsName HsDecl
     decl_map = listToFM [ (n,d) | d <- final_decls, n <- declBinders d ]

  -- make the "export items", which will be converted into docs later
  orig_export_list <- mkExportItems mod_map mod orig_env
   			 decl_map final_decls options orig_exports

  let
     -- prune the export list to just those declarations that have
     -- documentation, if the 'prune' option is on.
     pruned_export_list
	| OptPrune `elem` options = pruneExportItems doc_map orig_export_list
	| otherwise = orig_export_list

     -- rename names in the exported declarations to point to things that
     -- are closer, or maybe even exported by, the current module.
     (renamed_export_list, missing_names3)
        = runRnFM import_env (renameExportItems pruned_export_list)

     name_env = listToFM [ (nameOfQName n, n) | n <- exported_names ]

  -- report any names we couldn't find/resolve

  let missing_names_doc = missing_names_doc1 ++ missing_names_doc2
      missing_names = missing_names1 ++ missing_names2
			 --ignore missing_names3 for now,

      name_strings = nub (map show missing_names ++ missing_names_doc)

  when (not (null name_strings)) $
	  tell ["Warning: in module " ++ show mod ++ 
		", the following names could not be resolved:\n\ 
		\   " ++ concat (map (' ':) name_strings)
		]

  return (mod, Interface { 
		   iface_filename = filename,
		   iface_env = name_env,
		   iface_exports = renamed_export_list,
		   iface_orig_exports = pruned_export_list,
		   iface_decls =  decl_map,
		   iface_info = maybe_info,
		   iface_name_docs   = doc_map,
		   iface_doc         = module_doc,
		   iface_options     = options
		}
      	  )

-- -----------------------------------------------------------------------------
-- Find the documentation for a particular name, and rename the
-- original identifiers embedded in it to imported names.

lookupDoc :: ModuleMap -> Module -> [(HsName,Doc)]
	-> FiniteMap HsQName HsQName -> HsQName -> Maybe Doc
lookupDoc mod_map this_mod local_doc env name
  = case name of
	UnQual n -> Nothing
	Qual mod n
	  | mod == this_mod -> 
		fst (runRnFM env (mapMaybeM renameDoc (lookup n local_doc)))
		-- ToDo: report missing names
	  | otherwise       -> 
		case lookupFM mod_map mod of
		   Nothing -> Nothing
		   Just iface -> 
			fst (runRnFM env (mapMaybeM renameDoc
				     (lookupFM (iface_name_docs iface) n)))
		-- ToDo: report missing names

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
	= let decl' | HsTypeSig loc ns ty <- decl
			= HsTypeSig loc [nameOfQName x] ty
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
    lookupExport (HsEGroup lev str)
	= do (doc, _names) <- formatDocHeading mod (lookupForDoc env) str
	     return [ ExportGroup lev "" doc ]
	  -- ToDo: report the unresolved names
    lookupExport (HsEDoc str)
	= do (doc, _names) <- formatDocString mod (lookupForDoc env) str
	     return [ ExportDoc doc ]
	-- ToDo: report the unresolved names
    lookupExport (HsEDocNamed str)
	= do r <- findNamedDoc str decls
	     case r of
		Nothing -> return []
		Just found -> do
		  (doc, _nms) <- formatDocString mod (lookupForDoc env) found
	  	  return [ ExportDoc doc ]
	
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
  where mkExportItem (HsDocGroup lev str) = do
	   (doc, _names) <- formatDocHeading mod (lookupForDoc env) str
	   return (ExportGroup lev "" doc)
	   -- ToDo: report the unresolved names
	mkExportItem decl = return (ExportDecl decl)


keepDecl HsTypeSig{}     = True
keepDecl HsTypeDecl{}    = True
keepDecl HsNewTypeDecl{} = True
keepDecl HsDataDecl{}    = True
keepDecl HsClassDecl{}   = True
keepDecl HsDocGroup{}	 = True
keepDecl _ = False

-- -----------------------------------------------------------------------------
-- Pruning

pruneExportItems :: FiniteMap HsName Doc -> [ExportItem] -> [ExportItem]
pruneExportItems doc_map items = filter has_doc items
  where has_doc (ExportDecl d) | Just n <- declMainBinder d = n `elemFM` doc_map
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
expandDecl (HsTypeSig loc fs qt) = [ HsTypeSig loc [f] qt | f <- fs ]
expandDecl (HsClassDecl loc ty fds decls)
  = [ HsClassDecl loc ty fds (concat (map expandDecl decls)) ]
expandDecl d = [ d ]

-----------------------------------------------------------------------------
-- Collecting documentation and associating it with declarations

collectDoc :: [HsDecl] -> [(HsName, DocString)]
collectDoc decls = collect Nothing "" decls

collect name doc_so_far [] = 
   case name of
	Nothing -> []
	Just n  -> finishedDoc n doc_so_far []

collect name doc_so_far (decl:ds) = 
   case decl of
      HsDocCommentNext str -> 
	case name of
	   Nothing -> collect name (doc_so_far ++ str) ds
	   Just n  -> finishedDoc n doc_so_far (collect Nothing str ds)

      HsDocCommentPrev str -> collect name (doc_so_far ++ str) ds

      _other -> 
	docsFromDecl decl ++
	case name of
	    Nothing -> collect bndr doc_so_far ds
	    Just n  -> finishedDoc n doc_so_far (collect bndr "" ds)
        where 
	    bndr = declMainBinder decl

finishedDoc n s rest | all isSpace s = rest
 	             | otherwise     = (n,s) : rest

-- look inside a declaration and get docs for the bits
-- (constructors, record fields, class methods)
docsFromDecl :: HsDecl -> [(HsName, DocString)]
docsFromDecl (HsDataDecl loc ctxt nm tvs cons drvs)
  = concat (map docsFromConDecl cons)
docsFromDecl (HsNewTypeDecl loc ctxt nm tvs con drvs)
  = docsFromConDecl con
docsFromDecl (HsClassDecl loc ty fds decls)
  = collect Nothing "" decls
docsFromDecl _
  = []

docsFromConDecl :: HsConDecl -> [(HsName, DocString)]
docsFromConDecl (HsConDecl loc nm tvs ctxt tys (Just doc))
  = finishedDoc nm doc []
docsFromConDecl (HsRecDecl loc nm tvs ctxt fields (Just doc))
  = finishedDoc nm doc (foldr docsFromField [] fields)
docsFromConDecl (HsRecDecl loc nm tvs ctxt fields Nothing)
  = foldr docsFromField [] fields
docsFromConDecl _ 
  = []

docsFromField (HsFieldDecl nms ty (Just doc)) rest
  = foldr (\n -> finishedDoc n doc) rest nms
docsFromField (HsFieldDecl nms ty Nothing) rest
  = rest

-----------------------------------------------------------------------------
-- formatting is done in two stages.  Firstly we partially apply
-- formatDocString to the lookup function and the DocString to get a
-- markup-independent string.  Finally the back ends apply the markup
-- description to this function to get the marked-up text.

-- this one formats a heading
formatDocHeading :: Module -> (String -> Maybe HsQName) -> DocString
  -> ErrMsgM (Doc,[String])
formatDocHeading mod lookup string = format mod parseString lookup string

-- this one formats a sequence of paragraphs
formatDocString :: Module -> (String -> Maybe HsQName) -> DocString
  -> ErrMsgM (Doc,[String])
formatDocString mod lookup string = format mod parseParas lookup string

format 	:: Module			--  for error messages only
	-> ([Token] -> Either String ParsedDoc)
	-> (String -> Maybe HsQName)
	-> DocString
       	-> ErrMsgM (Doc, [String])
format mod parse lookup string
  = case parse (tokenise string) of
	Left error -> do 	
	  tell ["Warning: in " ++ show mod ++
		", parse error in doc string beginning:\n\ 
		\    " ++ take 40 string]
	  return (DocEmpty, [])
	Right doc -> 
	  return (runRn lookup (resolveDoc doc))

-- ---------------------------------------------------------------------------
-- Looking up names in documentation

lookupForDoc :: FiniteMap HsQName HsQName -> (String -> Maybe HsQName)
lookupForDoc fm str
  = case [ n | Just n <- map (lookupFM fm) (strToHsQNames str) ] of
	(n:_) -> Just n
	[] -> Nothing
 
strToHsQNames :: String -> [ HsQName ]
strToHsQNames str
 = case lexer (\t -> returnP t) str (SrcLoc 1 1) 1 1 [] of
	Ok _ (VarId str)
		-> [ UnQual (HsVarName (HsIdent str)) ]
        Ok _ (QVarId (mod,str))
		-> [ Qual (Module mod) (HsVarName (HsIdent str)) ]
	Ok _ (ConId str)
		-> [ UnQual (HsTyClsName (HsIdent str)),
		     UnQual (HsVarName (HsIdent str)) ]
        Ok _ (QConId (mod,str))
		-> [ Qual (Module mod) (HsTyClsName (HsIdent str)),
		     Qual (Module mod) (HsVarName (HsIdent str)) ]
        Ok _ (VarSym str)
		-> [ UnQual (HsVarName (HsSymbol str)) ]
        Ok _ (ConSym str)
		-> [ UnQual (HsTyClsName (HsSymbol str)),
		     UnQual (HsVarName (HsSymbol str)) ]
        Ok _ (QVarSym (mod,str))
		-> [ Qual (Module mod) (HsVarName (HsSymbol str)) ]
        Ok _ (QConSym (mod,str))
		-> [ Qual (Module mod) (HsTyClsName (HsSymbol str)),
		     Qual (Module mod) (HsVarName (HsSymbol str)) ]
	other -> []

-- -----------------------------------------------------------------------------
-- Parsing module headers

parseModuleHeader :: String -> (String, Maybe ModuleInfo)
parseModuleHeader str =
  case matchRegexAll moduleHeaderRE str of
	Just (before, match, after, _, (_:_:_:s1:s2:s3:_)) -> 
	   (after, Just (ModuleInfo { 
				 portability = s3,
				 stability   = s2,
				 maintainer  = s1 }))
	_other -> (str, Nothing)

moduleHeaderRE = mkRegexWithOpts
			 "^([ \t\n]*Module[ \t]*:.*\n)?\ 
			  \([ \t\n]*Copyright[ \t]*:.*\n)?\ 
			  \([ \t\n]*License[ \t]*:.*\n)?\ 
			  \[ \t\n]*Maintainer[ \t]*:(.*)\n\ 
			  \[ \t\n]*Stability[ \t]*:(.*)\n\ 
			  \[ \t\n]*Portability[ \t]*:([^\n]*)\n"
		True -- match "\n" with "."
		False -- not case sensitive
	-- All fields except the last (Portability) may be multi-line.
	-- This is so that the portability field doesn't swallow up the
	-- rest of the module documentation - we might want to revist
	-- this at some point (perhaps have a separator between the 
	-- portability field and the module documentation?).

#if __GLASGOW_HASKELL__ < 500
mkRegexWithOpts :: String -> Bool -> Bool -> Regex
mkRegexWithOpts s single_line case_sensitive
      = unsafePerformIO (re_compile_pattern (packString s) 
                              single_line case_sensitive)
#endif

-- -----------------------------------------------------------------------------
-- Named documentation

findNamedDoc :: String -> [HsDecl] -> ErrMsgM (Maybe String)
findNamedDoc str decls = 
  case matchRegex docNameRE str of
     Just (name:_) -> search decls
	where search [] = do
		tell ["Cannot find documentation for: $" ++ name]
		return Nothing
	      search (HsDocCommentNamed str : rest) = 
		case matchRegexAll docNameRE str of
		   Just (_, _, after, _, name':_)
			| name == name' -> return (Just after)
		   _otherwise -> search rest
	      search (_other_decl : rest) = search rest
     _other -> do
	tell ["Invalid documentation name: $" ++ str]
	return Nothing

docNameRE = mkRegex "[ \t]*([A-Za-z0-9_]*)"

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
		| hsmod@(HsModule mod _ impdecls _ _ _) <- hsmodules
		]

	for_each_scc (AcyclicSCC hsmodule) = return hsmodule
	for_each_scc (CyclicSCC  hsmodules) = 
	   dieMsg ("modules are recursive: " ++ 
		   unwords (map show [ mod | HsModule mod _ _ _ _ _ 
						<- hsmodules ]))

-- -----------------------------------------------------------------------------
-- A monad which collects error messages

type ErrMsg = String
type ErrMsgM a = Writer [ErrMsg] a

