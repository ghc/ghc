--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

module HaddockHtml (ppHtml) where

import Prelude hiding (div)
import HaddockVersion
import HaddockTypes
import HsSyn

import Maybe	( fromJust, isNothing, isJust )
import FiniteMap
import List 	( sortBy )
import Char	( toUpper, toLower )
import Monad	( when )

import Html
import qualified Html

-- -----------------------------------------------------------------------------
-- Generating HTML documentation

ppHtml :: String -> Maybe String -> [(Module, Interface)] -> IO ()
ppHtml title source_url ifaces =  do
  ppHtmlContents title source_url (map fst ifaces)
  ppHtmlIndex title ifaces
  mapM_ (ppHtmlModule title source_url) ifaces

moduleHtmlFile :: String -> FilePath
moduleHtmlFile mod = mod ++ ".html" -- ToDo: Z-encode filename?

contentsHtmlFile = "index.html"
indexHtmlFile    = "doc-index.html"
styleSheetFile   = "haddock.css"
subIndexHtmlFile k a = "doc-index-" ++ k:a:".html"

footer = 
  tda [theclass "botbar"] << 
	( toHtml "Produced by" <+> 
	  (anchor ! [href projectUrl] << toHtml projectName) <+>
	  toHtml ("version " ++ projectVersion)
	)
   

src_button source_url mod file
  | Just u <- source_url = 
	let src_url = if (last u == '/') then u ++ file else u ++ '/':file
	in
	(tda [theclass "topbut", nowrap] <<
  	   anchor ! [href src_url] << toHtml "Source code")
  | otherwise =
	Html.emptyTable
  

parent_button mod = 
  case span (/= '.') (reverse mod) of
   (m, '.':rest) -> 
       (tda [theclass "topbut", nowrap] <<
  	 anchor ! [href (moduleHtmlFile (reverse rest))] << toHtml "Parent")
   _ -> 
	Html.emptyTable

contentsButton = tda [theclass "topbut", nowrap] <<
  	 	    anchor ! [href contentsHtmlFile] << toHtml "Contents"

indexButton = tda [theclass "topbut", nowrap] <<
  	 	    anchor ! [href indexHtmlFile] << toHtml "Index"

simpleHeader title = 
  (tda [theclass "topbar"] << 
     vanillaTable << (
       (td << 
  	image ! [src "haskell_icon.gif", width "16", height 16, alt " " ]
       ) <->
       (tda [theclass "title"] << toHtml title) <->
	contentsButton <-> indexButton
   ))

pageHeader mod iface title source_url =
  (tda [theclass "topbar"] << 
    vanillaTable << (
       (td << 
  	image ! [src "haskell_icon.gif", width "16", height 16, alt " "]
       ) <->
       (tda [theclass "title"] << toHtml title) <->
	src_button source_url mod (iface_filename iface) <->
	parent_button mod <->
	contentsButton <->
	indexButton
    )
   ) </>
   tda [theclass "modulebar"] <<
	(vanillaTable << (
	  (td << font ! [size "6"] << toHtml mod) <->
          (tda [align "right"] <<
             (table ! [width "300", border 0, cellspacing 0, cellpadding 0] << (
        	  (tda [width "50%"] << font ! [color "#ffffff"] <<
        		bold << toHtml "Portability") <->
        	  (tda [width "50%"] << font ! [color "#ffffff"] <<
        		toHtml (iface_portability iface)) </>
        	  (tda [width "50%"] << font ! [color "#ffffff"] <<
        		bold << toHtml "Stability") <->
        	  (tda [width "50%"] << font ! [color "#ffffff"] <<
        		toHtml (iface_stability iface)) </>
        	  (tda [width "50%"] << font ! [color "#ffffff"] <<
        		bold << toHtml "Maintainer") <->
        	  (tda [width "50%"] << font ! [color "#ffffff"] <<
        		toHtml (iface_maintainer iface))
              ))
	  ))
    )

-- ---------------------------------------------------------------------------
-- Generate the module contents

ppHtmlContents :: String -> Maybe String -> [Module] -> IO ()
ppHtmlContents title source_url mods = do
  let tree = mkModuleTree mods  
      html = 
	header (thetitle (toHtml title) +++
		thelink ! [href styleSheetFile, 
		  rel "stylesheet", thetype "text/css"]) +++
        body <<  
	  table ! [width "100%", cellpadding 0, cellspacing 1] << (
   	    simpleHeader title </>
	    ppModuleTree title tree </>
	    footer
	  )
  writeFile contentsHtmlFile (renderHtml html)

ppModuleTree :: String -> [ModuleTree] -> HtmlTable
ppModuleTree title ts = 
  tda [theclass "section1"] << toHtml "Modules" </>
  td <<  table ! [cellpadding 0, cellspacing 2] << aboves (map (mkNode []) ts)

mkNode :: [String] -> ModuleTree -> HtmlTable
mkNode ss (Node s leaf []) =
  td << mkLeaf s ss leaf
mkNode ss (Node s leaf ts) = 
  (td << mkLeaf s ss leaf)
  </>
  (tda [theclass "children"] << 
     vanillaTable (toHtml (aboves (map (mkNode (s:ss)) ts))))

mkLeaf s ss False = toHtml s
mkLeaf s ss True  = anchor ! [href (moduleHtmlFile mod)] << toHtml s
  where mod = foldr (++) "" (s' : map ('.':) ss')
	(s':ss') = reverse (s:ss)
	 -- reconstruct the module name

data ModuleTree = Node String Bool [ModuleTree]

mkModuleTree :: [Module] -> [ModuleTree]
mkModuleTree mods = foldr addToTrees [] (map splitModule mods)

addToTrees :: [String] -> [ModuleTree] -> [ModuleTree]
addToTrees [] ts = ts
addToTrees ss [] = mkSubTree ss
addToTrees (s1:ss) (t@(Node s2 leaf subs) : ts)
  | s1 >  s2  = t : addToTrees (s1:ss) ts
  | s1 == s2  = Node s2 (leaf || null ss) (addToTrees ss subs) : ts
  | otherwise = mkSubTree (s1:ss) ++ t : ts

mkSubTree [] = []
mkSubTree (s:ss) = [Node s (null ss) (mkSubTree ss)]

splitModule :: Module -> [String]
splitModule (Module mod) = split mod
  where split mod = case break (== '.') mod of
     			(s1, '.':s2) -> s1 : split s2
     			(s1, _) -> [s1]

-- ---------------------------------------------------------------------------
-- Generate the index

ppHtmlIndex :: String -> [(Module,Interface)] -> IO ()
ppHtmlIndex title ifaces = do
  let html = 
	header (thetitle (toHtml (title ++ " (Index)")) +++
		thelink ! [href styleSheetFile, 
		  rel "stylesheet", thetype "text/css"]) +++
        body <<  
	  table ! [width "100%", cellpadding 0, cellspacing 1] << (
	    simpleHeader title </>
	    tda [theclass "section1"] << toHtml "Type/Class Index" </>
	    index_html tycls_index 't' </>
	    tda [theclass "section1"] << toHtml "Function/Constructor Index" </>
	    index_html var_index 'v'
	   )

  when split_indices
    (do mapM_ (do_sub_index "Type/Class" tycls_index 't') ['A'..'Z'] 
        mapM_ (do_sub_index "Function/Constructor" var_index 'v') ['A'..'Z'] 
    )

  writeFile indexHtmlFile (renderHtml html)

 where
  split_indices = length tycls_index > 50 || length var_index > 50

  index_html this_ix kind
    | split_indices = 
	td << table ! [cellpadding 0, cellspacing 5] <<
	    besides [ td << anchor ! [href (subIndexHtmlFile kind c)] <<
			 toHtml [c]
		    | c <- ['A'..'Z'] ]
   | otherwise =
	td << table ! [cellpadding 0, cellspacing 5] <<
	  aboves (map indexElt this_ix) 
 	
  do_sub_index descr this_ix kind c
    = writeFile (subIndexHtmlFile kind c) (renderHtml html)
    where 
      html = header (thetitle (toHtml (title ++ " (" ++ descr ++ "Index)")) +++
		thelink ! [href styleSheetFile, 
		  rel "stylesheet", thetype "text/css"]) +++
             body <<  
	      table ! [width "100%", cellpadding 0, cellspacing 1] << (
	        simpleHeader title </>
	        tda [theclass "section1"] << 
	      	toHtml (descr ++ " Index (" ++ c:")") </>
	        td << table ! [cellpadding 0, cellspacing 5] <<
	      	  aboves (map indexElt index_part) 
	       )

      index_part = [(n,stuff) | (n,stuff) <- this_ix, n `nameBeginsWith` c]

  tycls_index = index isTyClsName
  var_index   = index (not.isTyClsName)

  isTyClsName (HsTyClsName _) = True
  isTyClsName _ = False

  index :: (HsName -> Bool) -> [(HsName, [(Module,Bool)])]
  index f = sortBy cmp (fmToList (full_index f))
    where cmp (n1,_) (n2,_) = n1 `compare` n2
    
  iface_indices f = map (getIfaceIndex f) ifaces
  full_index f = foldr1 (plusFM_C (++)) (iface_indices f)

  getIfaceIndex f (mod,iface) = listToFM
    [ (name, [(mod, mod == mod')]) 
    | (name, Qual mod' _) <- fmToList (iface_env iface),
      f name ]

  indexElt :: (HsName, [(Module,Bool)]) -> HtmlTable
  indexElt (nm, entries) = 
     td << ppHsName nm
     <-> td << (hsep [ if defining then
			 bold << anchor ! [href (linkId mod nm)] << toHtml mod
		       else
			 anchor ! [href (linkId mod nm)] << toHtml mod
	             | (Module mod, defining) <- entries ])

nameBeginsWith (HsTyClsName id) c = idBeginsWith id c
nameBeginsWith (HsVarName   id) c = idBeginsWith id c

idBeginsWith (HsIdent   s) c = head s `elem` [toLower c, toUpper c]
idBeginsWith (HsSymbol  s) c = head s `elem` [toLower c, toUpper c]
idBeginsWith (HsSpecial s) c = head s `elem` [toLower c, toUpper c]

-- ---------------------------------------------------------------------------
-- Generate the HTML page for a module

ppHtmlModule :: String -> Maybe String -> (Module,Interface) -> IO ()
ppHtmlModule title source_url (Module mod,iface) = do
  let html = 
	header (thetitle (toHtml mod) +++
		thelink ! [href styleSheetFile, 
		  rel "stylesheet", thetype "text/css"]) +++
        body <<  
	  table ! [width "100%", cellpadding 0, cellspacing 1] << (
	    pageHeader mod iface title source_url </>
	    ifaceToHtml mod iface </>
	    footer
         )
  writeFile (moduleHtmlFile mod) (renderHtml html)

ifaceToHtml :: String -> Interface -> HtmlTable
ifaceToHtml mod iface
  | null exports = Html.emptyTable
  | otherwise =
    td << table ! [width "100%", cellpadding 0, cellspacing 15] << 
	(contents </> description </> synopsis </> maybe_hr </> body)
  where 
	exports = numberSectionHeadings (iface_exports iface)
	doc_map = iface_name_docs iface

	has_doc (ExportDecl d)
	 | Just x <- declMainBinder d = isJust (lookupFM doc_map x)
	has_doc _ = True

	no_doc_at_all = not (any has_doc exports)

	contents = td << ppModuleContents exports

	description
         | Just doc <- iface_doc iface
         = (tda [theclass "section1"] << toHtml "Description") </>
	   docBox (markup htmlMarkup doc)
	 | otherwise
	 = Html.emptyTable

	-- omit the synopsis if there are no documentation annotations at all
	synopsis
	  | no_doc_at_all = Html.emptyTable
	  | otherwise
	  = (tda [theclass "section1"] << toHtml "Synopsis") </>
            (tda [width "100%", theclass "synopsis"] << 
  	      table ! [width "100%", cellpadding 0, cellspacing 8, border 0] << 
  	        aboves (map (processExport doc_map True)  exports))

	maybe_hr
	     | not (no_doc_at_all),  ExportGroup 1 _ _ <- head exports
		 = td << hr
	     | otherwise  = Html.emptyTable

	body = aboves (map (processExport doc_map False) exports)

ppModuleContents :: [ExportItem] -> HtmlTable
ppModuleContents exports
  | null sections = Html.emptyTable
  | otherwise     = tda [theclass "section4"] << bold << toHtml "Contents"
  		     </> td << dlist << concatHtml sections
 where
  (sections, _leftovers{-should be []-}) = process 0 exports

  process :: Int -> [ExportItem] -> ([Html],[ExportItem])
  process n [] = ([], [])
  process n (ExportDecl _ : rest) = process n rest
  process n items@(ExportGroup lev id doc : rest) 
    | lev <= n  = ( [], items )
    | otherwise = ( html:sections, rest2 )
    where
	html = (dterm << anchor ! [href ('#':id)] << markup htmlMarkup doc)
		 +++ mk_subsections subsections
	(subsections, rest1) = process lev rest
	(sections,    rest2) = process n   rest1

  mk_subsections [] = noHtml
  mk_subsections ss = ddef << dlist << concatHtml ss

-- we need to assign a unique id to each section heading so we can hyperlink
-- them from the contents:
numberSectionHeadings :: [ExportItem] -> [ExportItem]
numberSectionHeadings exports = go 1 exports
  where go n [] = []
	go n (ExportGroup lev _ doc : es) 
	  = ExportGroup lev (show n) doc : go (n+1) es
	go n (other:es)
	  = other : go n es

processExport :: FiniteMap HsName Doc -> Bool -> ExportItem -> HtmlTable
processExport doc_map summary (ExportGroup lev id doc)
  | summary   = Html.emptyTable
  | otherwise = ppDocGroup lev (anchor ! [name id] << markup htmlMarkup doc)
processExport doc_map summary (ExportDecl decl)
  = doDecl doc_map summary decl

ppDocGroup lev doc
  | lev == 1  = tda [ theclass "section1" ] << doc
  | lev == 2  = tda [ theclass "section2" ] << doc
  | lev == 3  = tda [ theclass "section3" ] << doc
  | otherwise = tda [ theclass "section4" ] << doc

-- -----------------------------------------------------------------------------
-- Converting declarations to HTML

declWithDoc :: Bool -> Maybe Doc -> Html -> HtmlTable
declWithDoc True  doc        html_decl = declBox html_decl
declWithDoc False Nothing    html_decl = declBox html_decl
declWithDoc False (Just doc) html_decl = 
	tda [width "100%"] << 
	    vanillaTable << 
		(declBox html_decl </> docBox (markup htmlMarkup doc))

doDecl :: FiniteMap HsName Doc -> Bool -> HsDecl -> HtmlTable
doDecl doc_map summary decl = do_decl decl
  where
     doc | Just n <- declMainBinder decl  =  lookupFM doc_map n
	 | otherwise = Nothing

     do_decl (HsTypeSig _ [nm] ty) = 
	declWithDoc summary doc (ppTypeSig summary nm ty)

     do_decl (HsTypeSig _ nms ty) 
	= declWithDoc summary doc (
	    vanillaTable << aboves (map do_one nms))
	where do_one nm = declBox (ppTypeSig summary nm ty)

     do_decl (HsForeignImport _ _ _ _ n ty)
	= declWithDoc summary doc (ppTypeSig summary n ty)

     do_decl (HsTypeDecl _ nm args ty)
	= declWithDoc summary doc (
	      hsep ([keyword "type", ppHsBinder summary nm]
		 ++ map ppHsName args) <+> equals <+> ppHsType ty)

     do_decl (HsNewTypeDecl loc ctx nm args con drv)
	= ppHsDataDecl doc_map summary True{-is newtype-}
		(HsDataDecl loc ctx nm args [con] drv)
	  -- print it as a single-constructor datatype

     do_decl decl@(HsDataDecl loc ctx nm args cons drv) 
	= ppHsDataDecl doc_map summary False{-not newtype-} decl

     do_decl decl@(HsClassDecl _ _ _)
	= ppHsClassDecl doc_map summary decl

     do_decl (HsDocGroup lev str) 
	= if summary then Html.emptyTable else ppDocGroup lev str

     do_decl _ = error (show decl)


ppTypeSig summary nm ty = ppHsBinder summary nm <+> toHtml "::" <+> ppHsType ty


keepDecl HsTypeSig{}     = True
keepDecl HsTypeDecl{}    = True
keepDecl HsNewTypeDecl{} = True
keepDecl HsDataDecl{}    = True
keepDecl HsClassDecl{}   = True
keepDecl _ = False

-- -----------------------------------------------------------------------------
-- Data & newtype declarations

ppShortDataDecl doc_map summary is_newty 
	(HsDataDecl loc ctx nm args [con] drv) =
   declBox (  -- single constructor special case
      ppHsDataHeader summary is_newty nm args      
      <+> equals <+> ppShortConstr summary con
   )
ppShortDataDecl doc_map summary is_newty
	(HsDataDecl loc ctx nm args cons drv) = 
   declBox << vanillaTable << (
     aboves (
	(declBox (ppHsDataHeader summary is_newty nm args) :
 	zipWith do_constr ('=':repeat '|') cons
     )
    )
  )
  where do_constr c con = tda [theclass "condecl"] << (
				toHtml [c] <+> ppShortConstr summary con)

-- First, the abstract case:

ppHsDataDecl doc_map summary is_newty (HsDataDecl loc ctx nm args [] drv) = 
   declWithDoc summary (lookupFM doc_map nm)
     (ppHsDataHeader summary is_newty nm args)

-- The rest of the cases:

ppHsDataDecl doc_map summary is_newty decl@(HsDataDecl loc ctx nm args cons drv)
  | summary || (isNothing doc && all constr_has_no_doc cons)
	= ppShortDataDecl doc_map summary is_newty decl

  | otherwise
        = td << vanillaTable << (header </> datadoc </> constrs)
  where
	header = declBox (ppHsDataHeader False is_newty nm args)
	datadoc = docBox (markup htmlMarkup (fromJust doc))

	constr_hdr = tda [ theclass "section4" ] << toHtml "Constructors"

	constrs
	  | null cons = Html.emptyTable
	  | otherwise = 
		tda [theclass "databody"] << (
	    	    table ! [width "100%", cellpadding 0, cellspacing 10] <<
			aboves (constr_hdr : map do_constr cons)
           	  )

	do_constr con = ppHsFullConstr doc_map con

	Just c = declMainBinder decl
	doc = lookupFM doc_map c

	constr_has_no_doc (HsConDecl _ nm _ _) 
	   = isNothing (lookupFM doc_map nm)
	constr_has_no_doc (HsRecDecl _ nm _ _) 
	   = isNothing (lookupFM doc_map nm)


ppShortConstr :: Bool -> HsConDecl -> Html
ppShortConstr summary (HsConDecl pos nm typeList _maybe_doc) = 
   hsep (ppHsBinder summary nm : map ppHsBangType typeList)
ppShortConstr summary (HsRecDecl pos nm fields maybe_doc) =
   ppHsBinder summary nm +++
   braces (vanillaTable << aboves (map (ppShortField summary) fields))

ppHsFullConstr doc_map (HsConDecl pos nm typeList _maybe_doc) = 
     declWithDoc False doc (
	hsep (ppHsBinder False nm : map ppHsBangType typeList)
      )
   where
     doc = lookupFM doc_map nm
ppHsFullConstr doc_map (HsRecDecl pos nm fields maybe_doc) =
   td << vanillaTable << (
     case doc of
       Nothing  -> aboves [hdr, fields_html]
       Just doc -> aboves [hdr, constr_doc, fields_html]
   )

  where hdr = declBox (ppHsBinder False nm)
	constr_doc = docBox (markup htmlMarkup (fromJust doc))
	fields_html = 
	   td << 
	      table ! [width "100%", cellpadding 0, cellspacing 8] << (
		   aboves (map (ppFullField doc_map)
				   (concat (map expandField fields)))
		)
        doc = lookupFM doc_map nm


ppShortField summary (HsFieldDecl ns ty _doc) 
  = tda [theclass "recfield"] << (
	  hsep (punctuate comma (map (ppHsBinder summary) ns))
	    <+> toHtml "::" <+> ppHsBangType ty
   )

ppFullField doc_map (HsFieldDecl [n] ty _doc) 
  = declWithDoc False (lookupFM doc_map n) (
	ppHsBinder False n <+> toHtml "::" <+> ppHsBangType ty
    )
ppFullField _ _ = error "ppFullField"

expandField (HsFieldDecl ns ty doc) = [ HsFieldDecl [n] ty doc | n <- ns ]

ppHsDataHeader summary is_newty nm args = 
  (if is_newty then keyword "newtype" else keyword "data") <+> 
	ppHsBinder summary nm <+> hsep (map ppHsName args)

ppHsBangType :: HsBangType -> Html
ppHsBangType (HsBangedTy ty) = char '!' +++ ppHsAType ty
ppHsBangType (HsUnBangedTy ty) = ppHsAType ty

-- -----------------------------------------------------------------------------
-- Class declarations

ppClassHdr ty = keyword "class" <+> ppHsType ty

ppShortClassDecl doc_map summary decl@(HsClassDecl loc ty decls) = 
  if null decls
    then declBox hdr
    else td << (
	  vanillaTable << (
           declBox (hdr <+> keyword "where")
	    </> 
           tda [theclass "cbody"] << (
	    vanillaTable << (
	       aboves (map (doDecl doc_map summary) (filter keepDecl decls))
           ))
         ))
   where
	Just c = declMainBinder decl
	hdr | not summary = linkTarget c +++ ppClassHdr ty
	    | otherwise   = ppClassHdr ty

ppHsClassDecl doc_map summary decl@(HsClassDecl loc ty decls)
  |  summary || (isNothing doc && all decl_has_no_doc kept_decls)
	= ppShortClassDecl doc_map summary decl

  | otherwise
        = td << vanillaTable << (header </> classdoc </> body)

   where 
	doc    = lookupFM doc_map c
	Just c = declMainBinder decl

	header
	   | null decls = declBox (linkTarget c +++ ppClassHdr ty)
	   | otherwise  = declBox (linkTarget c +++ ppClassHdr ty <+> 
					keyword "where")

	classdoc
	   | Just d <- doc = docBox (markup htmlMarkup d)
	   | otherwise     = Html.emptyTable

	meth_hdr = tda [ theclass "section4" ] << toHtml "Methods"

	body
	   | null decls = Html.emptyTable
	   | otherwise  = 
		td << table ! [width "100%", cellpadding 0, cellspacing 8] << (
			meth_hdr </>
	       		aboves (map (doDecl doc_map False) kept_decls)
           	      )

	kept_decls = filter keepDecl decls

        decl_has_no_doc decl
	 | Just b <- declMainBinder decl = isNothing (lookupFM doc_map b)
	 | otherwise = True

-- -----------------------------------------------------------------------------
-- Types and contexts

ppHsContext :: HsContext -> Html
ppHsContext []      = empty
ppHsContext context = parenList (map (\ (a,b) -> ppHsQName a <+> 
					 hsep (map ppHsAType b)) context)

ppHsType :: HsType -> Html
ppHsType (HsForAllType Nothing context htype) =
     hsep [ ppHsContext context, toHtml "=>", ppHsType htype]
ppHsType (HsForAllType (Just tvs) [] htype) =
     hsep (keyword "forall" : map ppHsName tvs ++ toHtml "." : [ppHsType htype])
ppHsType (HsForAllType (Just tvs) context htype) =
     hsep (keyword "forall" : map ppHsName tvs ++ toHtml "." : 
	   ppHsContext context : toHtml "=>" : [ppHsType htype])
ppHsType (HsTyFun a b) = hsep [ppHsBType a, toHtml "->", ppHsType b]
ppHsType t = ppHsBType t

ppHsBType (HsTyApp (HsTyCon (Qual (Module "Prelude") (HsTyClsName (HsSpecial "[]")))) b )
  = brackets $ ppHsType b
ppHsBType (HsTyApp a b) = ppHsBType a <+> ppHsAType b
ppHsBType t = ppHsAType t

-- -----------------------------------------------------------------------------
-- Names

linkTarget :: HsName -> Html
linkTarget nm = anchor ! [name (hsNameStr nm)] << toHtml ""

ppHsAType :: HsType -> Html
ppHsAType (HsTyTuple True l)  = parenList . map ppHsType $ l
ppHsAType (HsTyTuple False l) = ubxParenList . map ppHsType $ l
ppHsAType (HsTyVar name) = ppHsName name
ppHsAType (HsTyCon name) = ppHsQName name
ppHsAType t = parens $ ppHsType t

ppHsQName :: HsQName -> Html
ppHsQName (UnQual str)			= ppHsName str
ppHsQName n@(Qual (Module mod) str)
  | n == unit_con_name	= ppHsName str
  | isSpecial str	= ppHsName str
  | otherwise		= anchor ! [href (linkId mod str)] << ppHsName str

isSpecial (HsTyClsName id) | HsSpecial _ <- id = True
isSpecial (HsVarName id) | HsSpecial _ <- id = True
isSpecial _ = False

ppHsName :: HsName -> Html
ppHsName nm = toHtml (hsNameStr nm)

hsNameStr :: HsName -> String
hsNameStr (HsTyClsName id) = ppHsIdentifier id
hsNameStr (HsVarName id)   = ppHsIdentifier id

ppHsIdentifier :: HsIdentifier -> String
ppHsIdentifier (HsIdent str)   =  str
ppHsIdentifier (HsSymbol str)  =  str
ppHsIdentifier (HsSpecial str) =  str

ppHsBinder :: Bool -> HsName -> Html
ppHsBinder True nm = anchor ! [href ('#':hsNameStr nm)] << ppHsBinder' nm
ppHsBinder False nm = linkTarget nm +++ ppHsBinder' nm

ppHsBinder' (HsTyClsName id) = ppHsBindIdent id
ppHsBinder' (HsVarName id)   = ppHsBindIdent id

ppHsBindIdent :: HsIdentifier -> Html
ppHsBindIdent (HsIdent str)   =  toHtml str
ppHsBindIdent (HsSymbol str)  =  parens (toHtml str)
ppHsBindIdent (HsSpecial str) =  toHtml str

linkId :: String -> HsName -> String
linkId mod str = moduleHtmlFile mod ++ '#': hsNameStr str

ppHsModule :: String -> Html
ppHsModule mod = anchor ! [href (moduleHtmlFile mod)] << toHtml mod

-- -----------------------------------------------------------------------------
-- * Doc Markup

htmlMarkup = Markup {
  markupParagraph     = paragraph,
  markupEmpty	      = toHtml "",
  markupString        = toHtml,
  markupAppend        = (+++),
  markupIdentifier    = ppHsQName,
  markupModule        = ppHsModule,
  markupEmphasis      = emphasize . toHtml,
  markupMonospaced    = tt . toHtml,
  markupUnorderedList = ulist . concatHtml . map (li <<),
  markupOrderedList   = olist . concatHtml . map (li <<),
  markupCodeBlock     = pre,
  markupURL	      = \url -> anchor ! [href url] << toHtml url
  }

-- -----------------------------------------------------------------------------
-- * Misc

hsep :: [Html] -> Html
hsep [] = noHtml
hsep htmls = foldr1 (\a b -> a+++" "+++b) htmls

infixr 8 <+>
a <+> b = Html (getHtmlElements (toHtml a) ++ HtmlString " ": getHtmlElements (toHtml b))

keyword s = bold << toHtml s

equals = char '='
comma  = char ','

char c = toHtml [c]
empty  = toHtml ""

parens p        = char '(' +++ p +++ char ')'
brackets p      = char '[' +++ p +++ char ']'
braces p        = char '{' +++ p +++ char '}'

punctuate :: Html -> [Html] -> [Html]
punctuate p []     = []
punctuate p (d:ds) = go d ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d +++ p) : go e es

parenList :: [Html] -> Html
parenList = parens . hsep . punctuate comma

ubxParenList :: [Html] -> Html
ubxParenList = ubxparens . hsep . punctuate comma

ubxparens p = toHtml "(#" +++ p +++ toHtml "#)"

text   = strAttr "TEXT"

declBox :: Html -> HtmlTable
declBox html = tda [theclass "decl"] << html

docBox :: Html -> HtmlTable
docBox html = tda [theclass "doc"] << html

vanillaTable = table ! [width "100%", cellpadding 0, cellspacing 0, border 0]
