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

import Maybe	( fromJust, isJust )
import FiniteMap
import Html	hiding (text)

-- -----------------------------------------------------------------------------
-- Generating HTML documentation

ppHtml :: String -> Maybe String -> [(Module, Interface)] -> IO ()
ppHtml title source_url ifaces =  do
  ppHtmlIndex title source_url (map fst ifaces)
  mapM_ (ppHtmlModule title source_url) ifaces

moduleHtmlFile :: String -> FilePath
moduleHtmlFile mod = mod ++ ".html" -- ToDo: Z-encode filename?

indexHtmlFile = "index.html"
styleSheetFile = "haddock.css"

footer = 
  td ! [theclass "botbar"] << 
	( toHtml "Produced by" <+> 
	  (anchor ! [href projectUrl] << toHtml projectName) <+>
	  toHtml ("version " ++ projectVersion)
	)
   

simpleHeader title = 
  (td ! [theclass "topbar"] << 
     vanillaTable << (
       (td << 
  	image ! [src "haskell_icon.gif", width "16", height 16, 
  		 align "absmiddle"]
       ) <->
       (td ! [theclass "title", width "100%"] << toHtml title)
   ))

buttons1 source_url mod file
  | Just u <- source_url = 
	let src_url = if (last u == '/') then u ++ file else u ++ '/':file
	in
	(td ! [theclass "topbut", nowrap] <<
  	   anchor ! [href src_url] << toHtml "Source code") <-> buttons2 mod
  | otherwise =
	buttons2 mod
  

buttons2 mod = 
  case span (/= '.') (reverse mod) of
   (m, '.':rest) -> 
       (td ! [theclass "topbut", nowrap] <<
  	 anchor ! [href (moduleHtmlFile (reverse rest))] << toHtml "Parent") <->
	contentsButton
   _ -> cell contentsButton

contentsButton = (td ! [theclass "topbut", nowrap] <<
  	 	    anchor ! [href indexHtmlFile] << toHtml "Contents")

pageHeader mod iface title source_url =
  (td ! [theclass "topbar"] << 
    vanillaTable << (
       (td << 
  	image ! [src "haskell_icon.gif", width "16", height 16, 
  		 align "absmiddle"]
       ) <->
       (td ! [theclass "title", width "100%"] << toHtml title) <->
	buttons1 source_url mod (iface_filename iface)
    )
   ) </>
   td ! [theclass "modulebar"] <<
	(vanillaTable << (
	  (td << font ! [size "6"] << toHtml mod) <->
          (td ! [align "right"] <<
             (table ! [width "300", border 0, cellspacing 0, cellpadding 0] << (
        	  (td ! [width "50%"] << font ! [color "#ffffff"] <<
        		bold << toHtml "Portability") <->
        	  (td ! [width "50%"] << font ! [color "#ffffff"] <<
        		toHtml (iface_portability iface)) </>
        	  (td ! [width "50%"] << font ! [color "#ffffff"] <<
        		bold << toHtml "Stability") <->
        	  (td ! [width "50%"] << font ! [color "#ffffff"] <<
        		toHtml (iface_stability iface)) </>
        	  (td ! [width "50%"] << font ! [color "#ffffff"] <<
        		bold << toHtml "Maintainer") <->
        	  (td ! [width "50%"] << font ! [color "#ffffff"] <<
        		toHtml (iface_maintainer iface))
              ))
	  ))
    )

-- ---------------------------------------------------------------------------
-- Generate the module index

ppHtmlIndex :: String -> Maybe String -> [Module] -> IO ()
ppHtmlIndex title source_url mods = do
  let tree = mkModuleTree mods  
      html = 
	header (thetitle (toHtml title)) +++
	mylink ! [href styleSheetFile, 
		  rel "stylesheet", thetype "text/css"] +++
        body <<  
	  table ! [width "100%", cellpadding 0, cellspacing 1] << (
   	    simpleHeader title </>
	    td << (ppModuleTree title tree) </>
	    footer
	  )
  writeFile indexHtmlFile (Html.renderHtml html)

ppModuleTree :: String -> [ModuleTree] -> Html
ppModuleTree title ts = 
  h1 << toHtml "Modules" +++
  table ! [cellpadding 0, cellspacing 2] << aboves (map (mkNode []) ts)

mkNode ss (Node s leaf []) =
  td << mkLeaf s ss leaf
mkNode ss (Node s leaf ts) = 
  td << table ! [cellpadding 0, cellspacing 2] <<
	  ((td << mkLeaf s ss leaf)
	     </> indent <-> aboves (map (mkNode (s:ss)) ts))

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
  | s1 == s2  = Node s2 (leaf || null ss) (addToTrees ss subs) : ts
  | otherwise = t : addToTrees (s1:ss) ts

mkSubTree [] = []
mkSubTree (s:ss) = [Node s (null ss) (mkSubTree ss)]

splitModule :: Module -> [String]
splitModule (Module mod) = split mod
  where split mod = case break (== '.') mod of
     			(s1, '.':s2) -> s1 : split s2
     			(s1, _) -> [s1]

-- ---------------------------------------------------------------------------
-- Generate the HTML page for a module

ppHtmlModule :: String -> Maybe String -> (Module,Interface) -> IO ()
ppHtmlModule title source_url (Module mod,iface) = do
  let html = 
	header (thetitle (toHtml mod)) +++
	mylink ! [href styleSheetFile, 
		  rel "stylesheet", thetype "text/css"] +++
        body <<  
	  table ! [width "100%", cellpadding 0, cellspacing 1] << (
	    pageHeader mod iface title source_url </>
	    ifaceToHtml mod iface </>
	    footer
         )
  writeFile (moduleHtmlFile mod) (Html.renderHtml html)

ifaceToHtml :: String -> Interface -> Html
ifaceToHtml mod iface
  | null exports = noHtml
  | otherwise =
    td << table ! [width "100%", cellpadding 0, cellspacing 15] << body1
 where exports = iface_exports iface
       doc_map = iface_name_docs iface

       body1
         | Just doc <- iface_doc iface
         = td ! [theclass "section1"] << toHtml "Description" </>
	   docBox (markup htmlMarkup doc) </>
	   body2
	 | otherwise
	 = body2

       body2 =
         (td ! [theclass "section1"] << toHtml "Synopsis") </>
         (td ! [width "100%", theclass "synopsis"] << 
  	   table ! [width "100%", cellpadding 0, cellspacing 8, border 0] << 
  	     aboves (map (processExport doc_map True)  exports)) </>
         td << hr </>
           aboves (map (processExport doc_map False) exports)

processExport :: FiniteMap HsName Doc -> Bool -> ExportItem -> Html
processExport doc_map summary (ExportGroup lev doc)
  | summary   = noHtml
  | otherwise = ppDocGroup lev (markup htmlMarkup doc)
processExport doc_map summary (ExportDecl decl)
  = doDecl doc_map summary decl

ppDocGroup lev doc
  | lev == 1  = td ! [ theclass "section1" ] << doc
  | lev == 2  = td ! [ theclass "section2" ] << doc
  | lev == 3  = td ! [ theclass "section3" ] << doc
  | otherwise = td ! [ theclass "section4" ] << doc

-- -----------------------------------------------------------------------------
-- Converting declarations to HTML

declWithDoc :: Bool -> Maybe Doc -> Html -> Html
declWithDoc True  doc        html_decl = declBox html_decl
declWithDoc False Nothing    html_decl = declBox html_decl
declWithDoc False (Just doc) html_decl = 
	td ! [width "100%"] << 
	    vanillaTable << 
		(declBox html_decl </> docBox (markup htmlMarkup doc))

doDecl :: FiniteMap HsName Doc -> Bool -> HsDecl -> Html
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
	= ppHsDataDecl doc_map summary (HsDataDecl loc ctx nm args [con] drv)
	  -- print it as a single-constructor datatype

     do_decl decl@(HsDataDecl loc ctx nm args cons drv) 
	= ppHsDataDecl doc_map summary decl

     do_decl decl@(HsClassDecl _ _ _)
	= ppHsClassDecl doc_map summary decl

     do_decl (HsDocGroup lev str) 
	= if summary then noHtml else ppDocGroup lev str

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

-- First, the abstract case:

ppHsDataDecl doc_map summary (HsDataDecl loc ctx nm args [] drv) = 
   declWithDoc summary (lookupFM doc_map nm)
     (ppHsDataHeader summary nm args)

-- Second, the summary cases:

ppHsDataDecl doc_map True (HsDataDecl loc ctx nm args [con] drv) = 
   declBox (  -- single constructor special case
      ppHsDataHeader True nm args      
      <+> equals <+> ppHsSummaryConstr con
   )
ppHsDataDecl doc_map True (HsDataDecl loc ctx nm args cons drv) = 
   td << (
    vanillaTable << (
     aboves (
	(declBox (ppHsDataHeader True nm args) :
 	zipWith do_constr ('=':repeat '|') cons
     )
    )
  ))
  where do_constr c con = td ! [theclass "condecl"] << (
				toHtml [c] <+> ppHsSummaryConstr con)

-- Now, the full expanded documented version:

ppHsDataDecl doc_map False decl@(HsDataDecl loc ctx nm args cons drv) =
  td << (
    vanillaTable << (
	if isJust doc
	  then aboves [header, datadoc, constrs]
	  else aboves [header, constrs]
     )
    )
  where
	header = declBox (ppHsDataHeader False nm args)
	datadoc = docBox (markup htmlMarkup (fromJust doc))
	constr_hdr = td ! [ theclass "section4" ] << toHtml "Constructors"

	constrs = td ! [theclass "databody"] << (
	    	    table ! [width "100%", cellpadding 0, cellspacing 10] <<
			aboves (constr_hdr : map do_constr cons)
           	  )

	do_constr con = ppHsFullConstr doc_map con

	Just c = declMainBinder decl
	doc = lookupFM doc_map c


ppHsSummaryConstr :: HsConDecl -> Html
ppHsSummaryConstr (HsConDecl pos nm typeList _maybe_doc) = 
   hsep (ppHsBinder True nm : map ppHsBangType typeList)
ppHsSummaryConstr (HsRecDecl pos nm fields maybe_doc) =
   ppHsBinder True nm +++
   braces (vanillaTable << aboves (map (td . ppSummaryField) fields))

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


ppSummaryField (HsFieldDecl ns ty _doc) 
  = td ! [theclass "recfield"] << (
	  hsep (punctuate comma (map (ppHsBinder True) ns))
	    <+> toHtml "::" <+> ppHsBangType ty
   )

ppFullField doc_map (HsFieldDecl [n] ty _doc) 
  = declWithDoc False (lookupFM doc_map n) (
	ppHsBinder False n <+> toHtml "::" <+> ppHsBangType ty
    )
ppFullField _ _ = error "ppFullField"

expandField (HsFieldDecl ns ty doc) = [ HsFieldDecl [n] ty doc | n <- ns ]

ppHsDataHeader summary nm args = 
  keyword "data" <+> ppHsBinder summary nm <+> hsep (map ppHsName args)

ppHsBangType :: HsBangType -> Html
ppHsBangType (HsBangedTy ty) = char '!' +++ ppHsAType ty
ppHsBangType (HsUnBangedTy ty) = ppHsAType ty

-- -----------------------------------------------------------------------------
-- Class declarations

ppClassHdr ty = keyword "class" <+> ppHsType ty

ppHsClassDecl doc_map True (HsClassDecl loc ty decls) =
  if null decls 
    then declBox (ppClassHdr ty)
    else td << (
	  vanillaTable << (
           declBox (ppClassHdr ty <+> keyword "where")
	    </> 
           td ! [theclass "cbody"] << (
	    vanillaTable << (
	       aboves (map (doDecl doc_map True) (filter keepDecl decls))
           ))
         ))

ppHsClassDecl doc_map False decl@(HsClassDecl loc ty decls) =
  linkTarget c +++ 
  if null decls
    then declBox (ppClassHdr ty)
    else td << (
	   vanillaTable << (
	     if isJust doc
		then aboves [header, classdoc, body]
		else aboves [header, body]
        ))
   where header = declBox (ppClassHdr ty <+> keyword "where")
	 classdoc = docBox (markup htmlMarkup (fromJust doc))
	 meth_hdr = td ! [ theclass "section4" ] << toHtml "Methods"
	 body   = td << (
	    	    table ! [width "100%", cellpadding 0, cellspacing 8] << (
			meth_hdr </>
	       		aboves (map (doDecl doc_map False) 
					(filter keepDecl decls))
           	  ))

 	 Just c = declMainBinder decl
         doc = lookupFM doc_map c

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
  markupCodeBlock     = pre
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

quotes p        = char '`' +++ p +++ char '\''
doubleQuotes p  = char '"' +++ p +++ char '"'
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

indent = td ! [width "10"] << ""

text   = strAttr "TEXT"
div    = tag "DIV"
mylink = itag "LINK"

declBox :: Html -> Html
declBox html = td ! [theclass "decl"] << html

docBox :: Html -> Html
docBox html = td ! [theclass "doc"] << html

vanillaTable = table ! [width "100%", cellpadding 0, cellspacing 0, border 0]

