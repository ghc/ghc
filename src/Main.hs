--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

module Main (main) where

import HaddockParse
import HaddockLex
import HaddockDB
import HaddockHtml
import HaddockTypes

import HsLexer hiding (Token)
import HsParser
import HsParseMonad
import HsSyn
import GetOpt
import System
import FiniteMap

--import Pretty

import Monad	( when )
import Char	( isSpace )
import IO
import IOExts

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
  deriving (Eq)

options =
  [ 
    Option ['t']  ["heading"]  (ReqArg Flag_Heading "HEADING")
	"page heading",
    Option ['v']  ["verbose"]  (NoArg Flag_Verbose)
	"be verbose",
    Option ['d']  ["docbook"]  (NoArg Flag_DocBook)
	"output in docbook (SGML)",
    Option ['h']  ["html"]     (NoArg Flag_Html)
	"output in HTML",
    Option ['s']  ["source"]   (ReqArg Flag_SourceURL "URL") 
	"base URL for links to source code"
  ]

saved_flags :: IORef [Flag]
saved_flags = unsafePerformIO (newIORef (error "no flags yet"))

run flags files = do
  seq stderr $ do
  writeIORef saved_flags flags
  parsed_mods <- sequence (map parse_file files)

  let ifaces = [ mkInterface module_map file parsed 
	       | (file,parsed) <- zip files parsed_mods ]

      module_map = listToFM ifaces

  let title = case [str | Flag_Heading str <- flags] of
		[] -> ""
		(t:ts) -> t

      source_url = case [str | Flag_SourceURL str <- flags] of
			[] -> Nothing
			(t:ts) -> Just t

  when (Flag_DocBook `elem` flags) $
    putStr (ppDocBook ifaces)

  when (Flag_Html `elem` flags) $
    ppHtml title source_url ifaces


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

-- we want to 
--
--    (a) build a list of definitions that are exported from this module
--
--    (b) resolve any references in these declarations to qualified names
--        (qualified by the module imported from, not the original module).

mkInterface :: ModuleMap -> FilePath -> HsModule -> (Module,Interface)
mkInterface mod_map filename (HsModule mod exps imps decls maybe_doc)
  = (mod, Interface { 
	   iface_filename = filename,
	   iface_env = name_env,
	   iface_exports = export_list,
	   iface_decls =  decl_map,
	   iface_portability = "portable",
	   iface_maintainer  = "libraries@haskell.org",
	   iface_stability   = "stable",
	   iface_name_docs   = doc_map,
	   iface_doc         = fmap (formatDocString (lookupForDoc import_env))
				maybe_doc
	} )
  where
  locally_defined_names = collectNames decls

  qual_local_names   = map (Qual mod) locally_defined_names
  unqual_local_names = map UnQual     locally_defined_names

  local_env = listToFM (zip unqual_local_names qual_local_names ++
			zip qual_local_names   qual_local_names)
	 -- both qualified and unqualifed names are in scope for local things

  -- build the orig_env, which maps names to *original* names (so we can
  -- find the original declarations & docs for things).
  external_env = foldr plusFM emptyFM (map (getOrigEnv mod_map) imps)
  orig_env     = external_env `plusFM` local_env

  -- resolve the names in the export list to original names
  renamed_exports = fmap (renameExportList orig_env) exps

  unrenamed_decl_map :: FiniteMap HsName HsDecl
  unrenamed_decl_map = listToFM [ (n,d) | d <- renamed_decls,
					  n <- declBinders d ]

  -- gather up a list of entities that are exported
  exported_names = exportedNames mod mod_map renamed_decls
			locally_defined_names renamed_exports
			unrenamed_decl_map

  -- Now build the environment we'll use for renaming the source: it maps
  -- names to *imported* names (not original names).  The imported name is
  -- a name qualified by the closest module which exports it (including
  -- the current module).
  import_env = local_env `plusFM`
		foldr plusFM emptyFM 
		  (map (getImportEnv mod mod_map exported_names) imps)

  -- convert names to original, fully qualified, names
  renamed_decls = map (renameDecl import_env) decls

  final_decls = concat (map expandDecl renamed_decls)

  -- match documentation to names, and resolve identifiers in the documentation
  local_docs :: [(HsName,Doc)]
  local_docs = [ (n, formatDocString (lookupForDoc import_env) doc) 
	       | (n, doc) <- collectDoc final_decls
	       ]

  doc_map :: FiniteMap HsName Doc
  doc_map = listToFM [ (nameOfQName n, doc)
		     | n <- exported_names,
		       Just doc <- [lookupDoc mod_map mod local_docs n] ]

  decl_map :: FiniteMap HsName HsDecl
  decl_map = listToFM [ (n,d) | d <- final_decls, n <- declBinders d ]

  -- make the "export items", which will be converted into docs later
  export_list = mkExportItems mod_map mod import_env
			decl_map final_decls renamed_exports 

  name_env = listToFM [ (nameOfQName n, n) | n <- exported_names ]


lookupDoc :: ModuleMap -> Module -> [(HsName,Doc)] -> HsQName -> Maybe Doc
lookupDoc mod_map this_mod local_doc name 
  = case name of
	UnQual n -> Nothing
	Qual mod n | mod == this_mod -> lookup n local_doc
		   | otherwise       -> 
			case lookupFM mod_map mod of
			   Nothing -> Nothing
			   Just iface -> lookupFM (iface_name_docs iface) n


mkExportItems :: ModuleMap -> Module
	-> FiniteMap HsQName HsQName
	-> FiniteMap HsName HsDecl
	-> [HsDecl]
	-> Maybe [HsExportSpec]
	-> [ExportItem]
mkExportItems mod_map mod env decl_map decls Nothing
  = fullContentsOfThisModule decls env -- everything exported
mkExportItems mod_map mod env decl_map decls (Just specs)
  = concat (map lookupExport specs)
  where
    lookupExport (HsEVar x) 
	| Just decl <- findDecl x
	= let decl' | HsTypeSig loc ns ty <- decl
			= HsTypeSig loc [nameOfQName x] ty
		    | otherwise
		  	= decl
	  in
	  [ ExportDecl decl' ]
	  -- ToDo: cope with record selectors here
    lookupExport (HsEAbs t)
	| Just decl <- findDecl t
	= [ ExportDecl (restrictTo [] decl) ]
    lookupExport (HsEThingAll t)
	| Just decl <- findDecl t
	= [ ExportDecl decl ]
    lookupExport (HsEThingWith t cs)
	| Just decl <- findDecl t
	= [ ExportDecl (restrictTo (map nameOfQName cs) decl) ]
    lookupExport (HsEModuleContents m) = fullContentsOf m
    lookupExport (HsEGroup lev str)
	= [ ExportGroup lev (formatDocHeading (lookupForDoc env) str) ]
    lookupExport _ = [] -- didn't find it?

    fullContentsOf m
	| m == mod  = fullContentsOfThisModule decls env
	| otherwise = 
	   case lookupFM mod_map m of
	     Just iface -> iface_exports iface
	     Nothing    -> trace ("Warning: module not found: " ++ show m) []

    findDecl :: HsQName -> Maybe HsDecl
    findDecl (UnQual n) = trace ("Warning(mkExportItems): UnQual! " ++ show n) $ Nothing
    findDecl (Qual m n)
	| m == mod  = lookupFM decl_map n
	| otherwise = case lookupFM mod_map m of
			Just iface -> lookupFM (iface_decls iface) n
			Nothing    -> trace ("Warning: module not found: " ++ show m) Nothing

fullContentsOfThisModule decls env = 
  [ mkExportItem decl | decl <- decls, keepDecl decl ]
  where mkExportItem (HsDocGroup lev str) =
	   ExportGroup lev (formatDocHeading (lookupForDoc env) str)
	mkExportItem decl = ExportDecl decl


keepDecl HsTypeSig{}     = True
keepDecl HsTypeDecl{}    = True
keepDecl HsNewTypeDecl{} = True
keepDecl HsDataDecl{}    = True
keepDecl HsClassDecl{}   = True
keepDecl HsDocGroup{}	 = True
keepDecl _ = False


exportedNames :: Module -> ModuleMap -> [HsDecl] -> [HsName]
	-> Maybe [HsExportSpec]
	-> FiniteMap HsName HsDecl
	-> [HsQName]
exportedNames mod mod_scope decls local_names Nothing decl_map
  = map (Qual mod) local_names
exportedNames mod mod_scope decls local_names (Just expspecs) decl_map
  = concat (map extract expspecs)
 where
  extract e = 
   case e of
    HsEVar x -> [x]
    HsEAbs t -> [t]
    HsEThingAll t
	|  Just decl <- export_lookup t 
	-> t : map (Qual mod) (declBinders decl)
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

-- (1) Build an environment mapping names to *original* names

getOrigEnv :: ModuleMap -> HsImportDecl -> FiniteMap HsQName HsQName
getOrigEnv mod_scopes (HsImportDecl _ mod qual _ _)
   = case lookupFM mod_scopes mod of
       Just iface -> listToFM (concat (map fn (fmToList (iface_env iface))))
       Nothing    -> trace ("Warning: module not found: " ++ show mod) emptyFM
  where
	-- bring both qualified and unqualified names into scope, unless
	-- the import was 'qualified'.
     fn (nm,qnm)
	| qual      = [ (Qual mod nm, qnm) ]
	| otherwise = [ (UnQual nm, qnm), (Qual mod nm, qnm) ]

-- (2) Build an environment mapping names to *imported* names

getImportEnv :: Module -> ModuleMap -> [HsQName] -> HsImportDecl
	-> FiniteMap HsQName HsQName
getImportEnv this_mod mod_scopes exported_names (HsImportDecl _ mod qual _ _)
   = case lookupFM mod_scopes mod of
       Just iface -> 
	 listToFM (concat (map (fn mod) (fmToList (iface_env iface))))
       Nothing ->
	 trace ("Warning: module not found: " ++ show mod) emptyFM
  where
	-- bring both qualified and unqualified names into scope, unless
	-- the import was 'qualified'.
     fn mod (nm,qnm)
	| qual      = [ (Qual mod nm, maps_to) ]
	| otherwise = [ (UnQual nm, maps_to), (Qual mod nm, maps_to) ]
	where maps_to | qnm `elem` exported_names = Qual this_mod nm
		      | otherwise = Qual mod nm
		-- if this name is also exported, then pretend that the
		-- local module defines it for the purposes of hyperlinking
		-- (since we're going to include its documentation in the
		-- documentation for this module).

-- -----------------------------------------------------------------------------
-- Expand multiple type signatures

expandDecl :: HsDecl -> [HsDecl]
expandDecl (HsTypeSig loc fs qt) = [ HsTypeSig loc [f] qt | f <- fs ]
expandDecl (HsClassDecl loc ty decls)
  = [ HsClassDecl loc ty (concat (map expandDecl decls)) ]
expandDecl d = [ d ]

-- -----------------------------------------------------------------------------
-- Renaming source code

renameExportList :: FiniteMap HsQName HsQName -> [HsExportSpec]
	-> [HsExportSpec]
renameExportList env spec = map renameExport spec
  where
    renameExport (HsEVar x) = HsEVar (rnLookupName env x)
    renameExport (HsEAbs x) = HsEAbs (rnLookupName env x)
    renameExport (HsEThingAll x) = HsEThingAll (rnLookupName env x)
    renameExport (HsEThingWith x cs)
	 = HsEThingWith (rnLookupName env x) (map (rnLookupName env) cs)
    renameExport (HsEModuleContents m) = HsEModuleContents m
    renameExport (HsEGroup lev str) = HsEGroup lev str

renameDecl
  :: FiniteMap HsQName HsQName
  -> HsDecl -> HsDecl
renameDecl scope decl
  = case decl of
	HsTypeDecl loc t args ty -> 
	    HsTypeDecl loc t args (renameType scope ty)
	HsDataDecl loc ctx t args cons drv -> 
	    HsDataDecl loc ctx t args (map (renameConDecl scope) cons) drv
        HsNewTypeDecl loc ctx t args con drv ->
	    HsNewTypeDecl loc ctx t args (renameConDecl scope con) drv
        HsClassDecl loc qt decls -> 
	    HsClassDecl loc (renameClassHead scope qt) 
		(map (renameDecl scope) decls)
	HsTypeSig loc fs qt ->
	    HsTypeSig loc fs (renameType scope qt)
	HsForeignImport loc cc safe ent n ty ->
	    HsForeignImport loc cc safe ent n (renameType scope ty)
	_ -> decl

renameClassHead s (HsForAllType tvs ctx ty)
  = HsForAllType tvs (map (renamePred s) ctx) ty
renameClassHead s ty
  = ty

renameConDecl s (HsConDecl loc nm tys maybe_doc)
  = HsConDecl loc nm (map (renameBangTy s) tys) maybe_doc
renameConDecl s (HsRecDecl loc nm fields maybe_doc)
  = HsRecDecl loc nm (map (renameField s) fields) maybe_doc

renameField s (HsFieldDecl ns ty doc) = HsFieldDecl ns (renameBangTy s ty) doc

renameBangTy s (HsBangedTy ty) = HsBangedTy (renameType s ty)
renameBangTy s (HsUnBangedTy ty) = HsUnBangedTy (renameType s ty)

renamePred s (c,tys) = (rnLookupName s c, map (renameType s) tys)

renameType s (HsForAllType tvs ctx ty)
  = HsForAllType tvs (map (renamePred s) ctx) (renameType s ty)
renameType s (HsTyFun arg res)
  = HsTyFun (renameType s arg) (renameType s res)
renameType s (HsTyTuple b tys)
  = HsTyTuple b (map (renameType s) tys)
renameType s (HsTyApp ty arg)
  = HsTyApp  (renameType s ty) (renameType s arg)
renameType s (HsTyVar nm)
  = HsTyVar nm
renameType s (HsTyCon nm)
  = HsTyCon (rnLookupName s nm)

rnLookupName :: FiniteMap HsQName HsQName -> HsQName -> HsQName
rnLookupName s nm
  = case lookupFM s nm of
	Just n -> n
	Nothing -> trace ("Warning: unknown name: " ++ show nm) nm

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

      HsDocCommentPrev str -> collect name (doc_so_far++str) ds

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
docsFromDecl (HsClassDecl loc ty decls)
  = collect Nothing "" decls
docsFromDecl _
  = []

docsFromConDecl :: HsConDecl -> [(HsName, DocString)]
docsFromConDecl (HsConDecl loc nm tys (Just doc))
  = finishedDoc nm doc []
docsFromConDecl (HsRecDecl loc nm fields (Just doc))
  = finishedDoc nm doc (foldr docsFromField [] fields)
docsFromConDecl (HsRecDecl loc nm fields Nothing)
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
formatDocHeading :: (String -> Maybe HsQName) -> DocString -> Doc
formatDocHeading lookup string = format parseString lookup string

-- this one formats a sequence of paragraphs
formatDocString :: (String -> Maybe HsQName) -> DocString -> Doc
formatDocString lookup string = format parseParas lookup string

format 	:: ([Token] -> ParsedDoc)
	-> (String -> Maybe HsQName)
	-> DocString
       	-> Doc
format parse lookup string = markup (mapIdent ident) parsed_doc
  where
	--parsed_doc :: DocMarkup String a -> a
	parsed_doc = parse (tokenise string)

	ident str = case lookup str of
			Just n  -> DocIdentifier n
			Nothing -> DocString str

-- ---------------------------------------------------------------------------
-- Looking up names in documentation

lookupForDoc :: FiniteMap HsQName HsQName -> (String -> Maybe HsQName)
lookupForDoc fm str
  = case [ n | Just n <- map (lookupFM fm) (strToHsQNames str) ] of
	(n:_) -> Just n
	[] -> trace ("Warning: unknown name: " ++ str) Nothing
 
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

-----------------------------------------------------------------------------
-- misc.

mapSnd f [] = []
mapSnd f ((x,y):xs) = (x,f y) : mapSnd f xs
