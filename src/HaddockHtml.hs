--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

module HaddockHtml (ppHtml) where

import Prelude hiding (div)
import HaddockVersion
import HaddockTypes
import HaddockUtil
import HaddockModuleTree
import HaddockHH
import HsSyn

import IO
import Maybe	( fromJust, isJust )
import FiniteMap
import List 	( sortBy )
import Char	( toUpper, toLower )
import Monad	( when )

#ifdef __GLASGOW_HASKELL__
import IOExts
#endif

import Html
import qualified Html

-- -----------------------------------------------------------------------------
-- Files we need to copy from our $libdir

cssFile  = "haddock.css"
iconFile = "haskell_icon.gif"

-- -----------------------------------------------------------------------------
-- Generating HTML documentation

type InstMaps = 
	(FiniteMap HsQName [InstHead], -- maps class names to instances
	 FiniteMap HsQName [InstHead]) -- maps type names to instances

ppHtml	:: String
	-> Maybe String
	-> [(Module, Interface)]
	-> FilePath			-- destination directory
	-> Maybe String			-- CSS file
	-> String			-- $libdir
	-> InstMaps
	-> Maybe Doc			-- prologue text, maybe
	-> Bool				-- do MS Help stuff
	-> IO ()

ppHtml title source_url ifaces odir maybe_css libdir inst_maps prologue
 do_ms_help =  do
  let 
	css_file = case maybe_css of
			Nothing -> libdir ++ pathSeparator:cssFile
			Just f  -> f
	css_destination = odir ++ pathSeparator:cssFile

	icon_file        = libdir ++ pathSeparator:iconFile
	icon_destination = odir   ++ pathSeparator:iconFile

	visible_ifaces = filter visible ifaces
	visible (m,i) = OptHide `notElem` iface_options i

  css_contents <- readFile css_file
  writeFile css_destination css_contents
  icon_contents <- readFile icon_file
  writeFile icon_destination icon_contents

  ppHtmlContents odir title source_url (map fst visible_ifaces) prologue
  ppHtmlIndex odir title visible_ifaces

  -- Generate index and contents page for MS help if requested
  when do_ms_help $ do
    ppHHContents odir (map fst visible_ifaces)
    ppHHIndex odir visible_ifaces

  mapM_ (ppHtmlModule odir title source_url inst_maps) visible_ifaces


contentsHtmlFile = "index.html"
indexHtmlFile    = "doc-index.html"
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
	topButBox (anchor ! [href src_url] << toHtml "Source code")
  | otherwise =
	Html.emptyTable
  

parent_button mod = 
  case span (/= '.') (reverse mod) of
   (m, '.':rest) -> 
       topButBox (
  	 anchor ! [href (moduleHtmlFile "" (reverse rest))] << toHtml "Parent")
   _ -> 
	Html.emptyTable

contentsButton = topButBox (anchor ! [href contentsHtmlFile] << 
				toHtml "Contents")

indexButton = topButBox (anchor ! [href indexHtmlFile] << toHtml "Index")

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
	  moduleInfo iface
	)
    )

moduleInfo iface = 
  case iface_info iface of
    Nothing   -> Html.emptyTable
    Just info ->
          tda [align "right"] << narrowTable << (
        	  (tda [theclass "infohead"] << toHtml "Portability") <->
        	  (tda [theclass "infoval"] << toHtml (portability info)) </>
        	  (tda [theclass "infohead"] << toHtml "Stability") <->
        	  (tda [theclass "infoval"] << toHtml (stability info)) </>
        	  (tda [theclass "infohead"] << toHtml "Maintainer") <->
        	  (tda [theclass "infoval"] << toHtml (maintainer info))
              )

-- ---------------------------------------------------------------------------
-- Generate the module contents

ppHtmlContents :: FilePath -> String -> Maybe String -> [Module] -> Maybe Doc
   -> IO ()
ppHtmlContents odir title source_url mods prologue = do
  let tree = mkModuleTree mods  
      html = 
	header (thetitle (toHtml title) +++
		thelink ! [href cssFile, 
		  rel "stylesheet", thetype "text/css"]) +++
        body << vanillaTable << (
   	    simpleHeader title </>
	    ppPrologue prologue </>
	    ppModuleTree title tree </>
	    s15 </>
	    footer
	  )
  writeFile (odir ++ pathSeparator:contentsHtmlFile) (renderHtml html)

ppPrologue :: Maybe Doc -> HtmlTable
ppPrologue Nothing = Html.emptyTable
ppPrologue (Just doc) = 
  (tda [theclass "section1"] << toHtml "Description") </>
  docBox (docToHtml doc)

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
mkLeaf s ss True  = anchor ! [href (moduleHtmlFile "" mod)] << toHtml s
  where mod = foldr (++) "" (s' : map ('.':) ss')
	(s':ss') = reverse (s:ss)
	 -- reconstruct the module name

-- ---------------------------------------------------------------------------
-- Generate the index

ppHtmlIndex :: FilePath -> String -> [(Module,Interface)] -> IO ()
ppHtmlIndex odir title ifaces = do
  let html = 
	header (thetitle (toHtml (title ++ " (Index)")) +++
		thelink ! [href cssFile, 
		  rel "stylesheet", thetype "text/css"]) +++
        body << vanillaTable << (
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

  writeFile (odir ++ pathSeparator:indexHtmlFile) (renderHtml html)

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
    = writeFile (odir ++ pathSeparator:subIndexHtmlFile kind c)
	(renderHtml html)
    where 
      html = header (thetitle (toHtml (title ++ " (" ++ descr ++ "Index)")) +++
		thelink ! [href cssFile, 
		  rel "stylesheet", thetype "text/css"]) +++
             body << vanillaTable << (
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
  full_index f = foldr (plusFM_C (++)) emptyFM (iface_indices f)

  getIfaceIndex f (mod,iface) = listToFM
    [ (name, [(mod, mod == mod')]) 
    | (name, Qual mod' _) <- fmToList (iface_env iface),
      f name ]

  indexElt :: (HsName, [(Module,Bool)]) -> HtmlTable
  indexElt (nm, entries) = 
     td << ppHsName nm
     <-> td << (hsep [ if defining then
			 bold << anchor ! [href (linkId (Module mod) nm)]
			   << toHtml mod
		       else
			 anchor ! [href (linkId (Module mod) nm)] << toHtml mod
	             | (Module mod, defining) <- entries ])

nameBeginsWith (HsTyClsName id) c = idBeginsWith id c
nameBeginsWith (HsVarName   id) c = idBeginsWith id c

idBeginsWith (HsIdent   s) c = head s `elem` [toLower c, toUpper c]
idBeginsWith (HsSymbol  s) c = head s `elem` [toLower c, toUpper c]
idBeginsWith (HsSpecial s) c = head s `elem` [toLower c, toUpper c]

-- ---------------------------------------------------------------------------
-- Generate the HTML page for a module

ppHtmlModule :: FilePath -> String -> Maybe String -> InstMaps
	-> (Module,Interface) -> IO ()
ppHtmlModule odir title source_url inst_maps (Module mod,iface) = do
  let html = 
	header (thetitle (toHtml mod) +++
		thelink ! [href cssFile,
		  rel "stylesheet", thetype "text/css"]) +++
        body << vanillaTable << (
	    pageHeader mod iface title source_url </> s15 </>
	    ifaceToHtml mod iface inst_maps </> s15 </>
	    footer
         )
  writeFile (moduleHtmlFile odir mod) (renderHtml html)

ifaceToHtml :: String -> Interface -> InstMaps -> HtmlTable
ifaceToHtml mod iface inst_maps
  = abovesSep s15 (contents: description: synopsis: maybe_doc_hdr: body)
  where 
	exports = numberSectionHeadings (iface_exports iface)

	has_doc (ExportDecl _ d) = isJust (declDoc d)
	has_doc (ExportModule _) = False
	has_doc _ = True

	no_doc_at_all = not (any has_doc exports)

	contents = td << ppModuleContents exports

	description
         | Just doc <- iface_doc iface
         = (tda [theclass "section1"] << toHtml "Description") </>
	   docBox (docToHtml doc)
	 | otherwise
	 = Html.emptyTable

	-- omit the synopsis if there are no documentation annotations at all
	synopsis
	  | no_doc_at_all = Html.emptyTable
	  | otherwise
	  = (tda [theclass "section1"] << toHtml "Synopsis") </>
	    s15 </>
            (tda [theclass "body"] << vanillaTable <<
  	        abovesSep s8 (map (processExport True inst_maps) 
			(filter forSummary exports))
	    )

	-- if the documentation doesn't begin with a section header, then
	-- add one ("Documentation").
	maybe_doc_hdr
	     | not (no_doc_at_all) = 
		case exports of
		   [] -> Html.emptyTable
		   ExportGroup _ _ _ : _ -> Html.emptyTable
		   _ -> tda [ theclass "section1" ] << toHtml "Documentation"
	     | otherwise  = Html.emptyTable

	body = map (processExport False inst_maps) exports

ppModuleContents :: [ExportItem] -> HtmlTable
ppModuleContents exports
  | length sections < 2 = Html.emptyTable
  | otherwise           = tda [theclass "section4"] << bold << toHtml "Contents"
  		           </> td << dlist << concatHtml sections
 where
  (sections, _leftovers{-should be []-}) = process 0 exports

  process :: Int -> [ExportItem] -> ([Html],[ExportItem])
  process n [] = ([], [])
  process n items@(ExportGroup lev id doc : rest) 
    | lev <= n  = ( [], items )
    | otherwise = ( html:sections, rest2 )
    where
	html = (dterm << anchor ! [href ('#':id)] << docToHtml doc)
		 +++ mk_subsections subsections
	(subsections, rest1) = process lev rest
	(sections,    rest2) = process n   rest1
  process n (_ : rest) = process n rest

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

processExport :: Bool -> InstMaps -> ExportItem -> HtmlTable
processExport summary inst_maps (ExportGroup lev id doc)
  = ppDocGroup lev (anchor ! [name id] << docToHtml doc)
processExport summary inst_maps (ExportDecl x decl)
  = doDecl summary inst_maps x decl
processExport summary inst_maps (ExportDoc doc)
  = docBox (docToHtml doc)
processExport summary inst_maps (ExportModule (Module mod))
  = declBox (toHtml "module" <+> ppHsModule mod)

forSummary (ExportGroup _ _ _) = False
forSummary (ExportDoc _) = False
forSummary _ = True

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
		declBox html_decl </> docBox (docToHtml doc)

doDecl :: Bool -> InstMaps -> HsQName -> HsDecl -> HtmlTable
doDecl summary inst_maps x decl = do_decl decl
  where
     do_decl (HsTypeSig _ [nm] ty doc) 
	= ppFunSig summary nm ty doc

     do_decl (HsForeignImport _ _ _ _ n ty doc)
	= ppFunSig summary n ty doc

     do_decl (HsTypeDecl _ nm args ty doc)
	= declWithDoc summary doc (
	      hsep ([keyword "type", ppHsBinder summary nm]
		 ++ map ppHsName args) <+> equals <+> ppHsType ty)

     do_decl (HsNewTypeDecl loc ctx nm args con drv doc)
	= ppHsDataDecl summary inst_maps True{-is newtype-} x
		(HsDataDecl loc ctx nm args [con] drv doc)
	  -- print it as a single-constructor datatype

     do_decl decl@(HsDataDecl loc ctx nm args cons drv doc)
	= ppHsDataDecl summary inst_maps False{-not newtype-} x decl

     do_decl decl@(HsClassDecl{})
	= ppHsClassDecl summary inst_maps x decl

     do_decl (HsDocGroup loc lev str)
	= if summary then Html.emptyTable 
		     else ppDocGroup lev (docToHtml str)

     do_decl _ = error ("do_decl: " ++ show decl)


ppTypeSig summary nm ty = ppHsBinder summary nm <+> toHtml "::" <+> ppHsType ty

-- -----------------------------------------------------------------------------
-- Data & newtype declarations

ppShortDataDecl :: Bool -> Bool -> HsDecl -> Html
ppShortDataDecl summary is_newty 
	(HsDataDecl loc ctx nm args [con] drv _doc) =
   ppHsDataHeader summary is_newty nm args      
     <+> equals <+> ppShortConstr summary con
ppShortDataDecl summary is_newty
	(HsDataDecl loc ctx nm args cons drv _doc) = 
   vanillaTable << (
	declBox (ppHsDataHeader summary is_newty nm args) </>
	tda [theclass "body"] << vanillaTable << (
	  aboves (zipWith do_constr ('=':repeat '|') cons)
        )
   )
  where do_constr c con = declBox (toHtml [c] <+> ppShortConstr summary con)

-- The rest of the cases:

ppHsDataDecl summary (_, ty_inst_map) is_newty 
     x decl@(HsDataDecl loc ctx nm args cons drv doc)
  | summary = declWithDoc summary doc (ppShortDataDecl summary is_newty decl)

  | otherwise
        = header </> 
	    tda [theclass "body"] << vanillaTable << (
		datadoc </> 
		constr_bit </>
		instances_bit
            )
  where
	header = declBox (ppHsDataHeader False is_newty nm args)

	constr_table
	 	| any isRecDecl cons  = spacedTable5
	  	| otherwise           = spacedTable1

	datadoc | isJust doc = ndocBox (docToHtml (fromJust doc))
	  	| otherwise  = Html.emptyTable

	constr_bit 
		| null cons = Html.emptyTable
		| otherwise = 
			constr_hdr </>
			(tda [theclass "body"] << constr_table << 
			 aboves (map ppSideBySideConstr cons)
			)

	instances = lookupFM ty_inst_map x

	instances_bit
	   = case instances of
		Nothing -> Html.emptyTable
		Just [] -> Html.emptyTable
		Just is -> 
		 inst_hdr </>
		 tda [theclass "body"] << spacedTable1 << (
			aboves (map (declBox.ppInstHead) is)
		  )

isRecDecl (HsRecDecl pos nm tvs ctxt fields maybe_doc) = True
isRecDecl _ = False

ppShortConstr :: Bool -> HsConDecl -> Html
ppShortConstr summary (HsConDecl pos nm tvs ctxt typeList _maybe_doc) = 
   ppHsConstrHdr tvs ctxt +++
	hsep (ppHsBinder summary nm : map ppHsBangType typeList)
ppShortConstr summary (HsRecDecl pos nm tvs ctxt fields maybe_doc) =
   ppHsConstrHdr tvs ctxt +++
   ppHsBinder summary nm <+>
   braces (vanillaTable << aboves (map (ppShortField summary) fields))

ppHsConstrHdr tvs ctxt
 = (if null tvs then noHtml else keyword "forall" <+> 
				 hsep (map ppHsName tvs) <+> 
				 toHtml ". ")
   +++
   (if null ctxt then noHtml else ppHsContext ctxt <+> toHtml "=> ")

ppSideBySideConstr (HsConDecl pos nm tvs ctxt typeList doc) =
  declBox (hsep ((ppHsConstrHdr tvs ctxt +++ 
		ppHsBinder False nm) : map ppHsBangType typeList)) <->
  maybeRDocBox doc
ppSideBySideConstr (HsRecDecl pos nm tvs ctxt fields doc) =
  declBox (ppHsConstrHdr tvs ctxt +++ ppHsBinder False nm) <->
  maybeRDocBox doc </>
  (tda [theclass "body"] << spacedTable1 <<
     aboves (map ppSideBySideField fields))

ppSideBySideField (HsFieldDecl ns ty doc) =
  declBox (hsep (punctuate comma (map (ppHsBinder False) ns))
	   <+> toHtml "::" <+> ppHsBangType ty) <->
  maybeRDocBox doc

ppHsFullConstr (HsConDecl pos nm tvs ctxt typeList doc) = 
     declWithDoc False doc (
	hsep ((ppHsConstrHdr tvs ctxt +++ 
		ppHsBinder False nm) : map ppHsBangType typeList)
      )
ppHsFullConstr (HsRecDecl pos nm tvs ctxt fields doc) =
   td << vanillaTable << (
     case doc of
       Nothing  -> aboves [hdr, fields_html]
       Just doc -> aboves [hdr, constr_doc, fields_html]
   )

  where hdr = declBox (ppHsConstrHdr tvs ctxt +++ ppHsBinder False nm)

	constr_doc	
	  | isJust doc = docBox (docToHtml (fromJust doc))
	  | otherwise  = Html.emptyTable

	fields_html = 
	   td << 
	      table ! [width "100%", cellpadding 0, cellspacing 8] << (
		   aboves (map ppFullField (concat (map expandField fields)))
		)


ppShortField summary (HsFieldDecl ns ty _doc) 
  = tda [theclass "recfield"] << (
	  hsep (punctuate comma (map (ppHsBinder summary) ns))
	    <+> toHtml "::" <+> ppHsBangType ty
   )

ppFullField (HsFieldDecl [n] ty doc) 
  = declWithDoc False doc (
	ppHsBinder False n <+> toHtml "::" <+> ppHsBangType ty
    )
ppFullField _ = error "ppFullField"

expandField (HsFieldDecl ns ty doc) = [ HsFieldDecl [n] ty doc | n <- ns ]

ppHsDataHeader summary is_newty nm args = 
  (if is_newty then keyword "newtype" else keyword "data") <+> 
	ppHsBinder summary nm <+> hsep (map ppHsName args)

ppHsBangType :: HsBangType -> Html
ppHsBangType (HsBangedTy ty) = char '!' +++ ppHsAType ty
ppHsBangType (HsUnBangedTy ty) = ppHsAType ty

-- -----------------------------------------------------------------------------
-- Class declarations

ppClassHdr summ [] n tvs fds = 
  keyword "class"
	<+> ppHsBinder summ n <+> hsep (map ppHsName tvs)
	<+> ppFds fds
ppClassHdr summ ctxt n tvs fds = 
  keyword "class" <+> ppHsContext ctxt <+> darrow
	<+> ppHsBinder summ n <+> hsep (map ppHsName tvs)
	<+> ppFds fds

ppFds fds =
  if null fds then noHtml else 
	char '|' <+> hsep (punctuate comma (map fundep fds))
  where
	fundep (vars1,vars2) = hsep (map ppHsName vars1) <+> toHtml "->" <+>
			       hsep (map ppHsName vars2)

ppShortClassDecl summary inst_maps 
	decl@(HsClassDecl loc ctxt nm tvs fds decls doc) = 
  if null decls
    then declBox hdr
    else declBox (hdr <+> keyword "where")
	    </> 
           (tda [theclass "body"] << 
	     vanillaTable << 
	       aboves [ ppFunSig summary n ty doc 
		      | HsTypeSig _ [n] ty doc <- decls
		      ]
          )
         
   where
	hdr = ppClassHdr summary ctxt nm tvs fds

ppHsClassDecl summary inst_maps@(cls_inst_map, _) orig_c 
	decl@(HsClassDecl loc ctxt nm tvs fds decls doc)
  | summary = ppShortClassDecl summary inst_maps decl

  | otherwise
        = header </>
		tda [theclass "body"] << vanillaTable << (
		   classdoc </> methods_bit </> instances_bit
		)

   where 
	header
	   | null decls = declBox hdr
	   | otherwise  = declBox (hdr <+> keyword "where")

	hdr = ppClassHdr summary ctxt nm tvs fds

	classdoc
	   | Just d <- doc = ndocBox (docToHtml d)
	   | otherwise     = Html.emptyTable

	methods_bit
	   | null decls = Html.emptyTable
	   | otherwise  = 
		s8 </> meth_hdr </>
		tda [theclass "body"] << vanillaTable << (
	       		abovesSep s8 [ ppFunSig summary n ty doc 
			             | HsTypeSig _ [n] ty doc <- decls
			             ]
			)

	instances_bit
	   = case instances of
		Nothing -> Html.emptyTable
		Just [] -> Html.emptyTable
		Just is -> 
		 s8 </> inst_hdr </>
		 tda [theclass "body"] << spacedTable1 << (
			aboves (map (declBox.ppInstHead) is)
		  )

	instances = lookupFM cls_inst_map orig_c


ppInstHead	       :: InstHead -> Html
ppInstHead ([],asst)   =  ppHsAsst asst
ppInstHead (ctxt,asst) =  ppHsContext ctxt <+> darrow <+> ppHsAsst asst

-- ----------------------------------------------------------------------------
-- Type signatures

ppFunSig summary nm ty doc
  | summary || no_arg_docs ty = 
      declWithDoc summary doc (ppTypeSig summary nm ty)

  | otherwise   = 
	declBox (ppHsBinder False nm) </>
	(tda [theclass "body"] << vanillaTable <<  (
	   do_args dcolon ty </>
	   (if (isJust doc) 
		then ndocBox (docToHtml (fromJust doc))
		else Html.emptyTable)
	))
  where
	no_arg_docs (HsForAllType _ _ ty) = no_arg_docs ty
	no_arg_docs (HsTyFun (HsTyDoc _ _) _) = False
	no_arg_docs (HsTyFun _ r) = no_arg_docs r
	no_arg_docs (HsTyDoc _ _) = False
 	no_arg_docs _ = True

	do_args :: Html -> HsType -> HtmlTable
	do_args leader (HsForAllType (Just tvs) ctxt ty)
	  = (declBox (
		leader <+> 
		hsep (keyword "forall" : map ppHsName tvs ++ [toHtml "."]) <+>
		ppHsContext ctxt)
	      <-> rdocBox noHtml) </> 
	    do_args darrow ty
	do_args leader (HsForAllType Nothing ctxt ty)
	  = (declBox (leader <+> ppHsContext ctxt)
		<-> rdocBox noHtml) </> 
	    do_args darrow ty
	do_args leader (HsTyFun (HsTyDoc ty doc) r)
	  = (declBox (leader <+> ppHsBType ty) <-> rdocBox (docToHtml doc)) </>
	    do_args arrow r
	do_args leader (HsTyFun ty r)
	  = (declBox (leader <+> ppHsBType ty) <-> rdocBox noHtml) </>
	    do_args arrow r
	do_args leader (HsTyDoc ty doc)
	  = (declBox (leader <+> ppHsBType ty) <-> rdocBox (docToHtml doc))
	do_args leader ty
	  = declBox (leader <+> ppHsBType ty) <-> rdocBox (noHtml)

-- -----------------------------------------------------------------------------
-- Types and contexts

ppHsAsst	    :: (HsQName,[HsType]) -> Html
ppHsAsst (c,args)   =  ppHsQName c <+> hsep (map ppHsAType args)

ppHsContext	    :: HsContext -> Html
ppHsContext []      =  empty
ppHsContext context =  parenList (map ppHsAsst context)

ppHsForAll Nothing context = 
  hsep [ ppHsContext context, darrow ]
ppHsForAll (Just tvs) [] = 
  hsep (keyword "forall" : map ppHsName tvs ++ [toHtml "."])
ppHsForAll (Just tvs) context =
  hsep (keyword "forall" : map ppHsName tvs ++ 
	  [toHtml ".", ppHsContext context, darrow])

ppHsType :: HsType -> Html
ppHsType (HsForAllType maybe_tvs context htype) =
  ppHsForAll maybe_tvs context <+> ppHsType htype
ppHsType (HsTyFun a b) = hsep [ppHsBType a, toHtml "->", ppHsType b]
ppHsType t = ppHsBType t

ppHsBType (HsTyDoc ty doc) = ppHsBType ty
ppHsBType (HsTyApp (HsTyCon (Qual _ (HsTyClsName (HsSpecial "[]")))) b )
  = brackets $ ppHsType b
ppHsBType (HsTyApp a b) = ppHsBType a <+> ppHsAType b
ppHsBType t = ppHsAType t

ppHsAType :: HsType -> Html
ppHsAType (HsTyTuple True l)  = parenList . map ppHsType $ l
ppHsAType (HsTyTuple False l) = ubxParenList . map ppHsType $ l
ppHsAType (HsTyVar name) = ppHsName name
ppHsAType (HsTyCon name)
  | name == fun_tycon_name = parens $ ppHsQName name
  | otherwise              = ppHsQName name
ppHsAType (HsTyApp (HsTyCon (Qual _ (HsTyClsName (HsSpecial "[]")))) b )
  = brackets $ ppHsType b
ppHsAType t = parens $ ppHsType t

-- -----------------------------------------------------------------------------
-- Names

linkTarget :: HsName -> Html
linkTarget nm = anchor ! [name (hsNameStr nm)] << toHtml ""

ppHsQName :: HsQName -> Html
ppHsQName (UnQual str) = ppHsName str
ppHsQName n@(Qual mod str)
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
ppHsBinder False nm = linkTarget nm +++ bold << ppHsBinder' nm

ppHsBinder' (HsTyClsName id) = ppHsBindIdent id
ppHsBinder' (HsVarName id)   = ppHsBindIdent id

ppHsBindIdent :: HsIdentifier -> Html
ppHsBindIdent (HsIdent str)   =  toHtml str
ppHsBindIdent (HsSymbol str)  =  parens (toHtml str)
ppHsBindIdent (HsSpecial str) =  toHtml str

linkId :: Module -> HsName -> String
linkId (Module mod) str = moduleHtmlFile fp mod ++ '#': hsNameStr str
  where fp = case lookupFM html_xrefs (Module mod) of
		Just fp -> fp 
		Nothing -> ""

ppHsModule :: String -> Html
ppHsModule mod = anchor ! [href (moduleHtmlFile fp mod)] << toHtml mod
  where fp = case lookupFM html_xrefs (Module mod) of
		Just fp -> fp 
		Nothing -> ""

-- -----------------------------------------------------------------------------
-- * Doc Markup

htmlMarkup = Markup {
  markupParagraph     = paragraph,
  markupEmpty	      = toHtml "",
  markupString        = toHtml,
  markupAppend        = (+++),
  markupIdentifier    = tt . ppHsQName . head,
  markupModule        = ppHsModule,
  markupEmphasis      = emphasize . toHtml,
  markupMonospaced    = tt . toHtml,
  markupUnorderedList = ulist . concatHtml . map (li <<),
  markupOrderedList   = olist . concatHtml . map (li <<),
  markupCodeBlock     = pre,
  markupURL	      = \url -> anchor ! [href url] << toHtml url
  }

-- If the doc is a single paragraph, don't surround it with <P> (this causes
-- ugly extra whitespace with some browsers).
docToHtml (DocParagraph p) = docToHtml p
docToHtml (DocCodeBlock p) = docToHtml (DocMonospaced p)
docToHtml doc = markup htmlMarkup doc

-- -----------------------------------------------------------------------------
-- * Misc

hsep :: [Html] -> Html
hsep [] = noHtml
hsep htmls = foldr1 (\a b -> a+++" "+++b) htmls

infixr 8 <+>
a <+> b = Html (getHtmlElements (toHtml a) ++ HtmlString " ": getHtmlElements (toHtml b))

keyword s = thespan ! [theclass "keyword"] << toHtml s

equals = char '='
comma  = char ','

char c = toHtml [c]
empty  = noHtml

parens p        = char '(' +++ p +++ char ')'
brackets p      = char '[' +++ p +++ char ']'
braces p        = char '{' +++ p +++ char '}'

punctuate :: Html -> [Html] -> [Html]
punctuate p []     = []
punctuate p (d:ds) = go d ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d +++ p) : go e es

abovesSep :: HtmlTable -> [HtmlTable] -> HtmlTable
abovesSep p []     = Html.emptyTable
abovesSep p (d:ds) = go d ds
                   where
                     go d [] = d
                     go d (e:es) = d </> p </> go e es

parenList :: [Html] -> Html
parenList = parens . hsep . punctuate comma

ubxParenList :: [Html] -> Html
ubxParenList = ubxparens . hsep . punctuate comma

ubxparens p = toHtml "(#" +++ p +++ toHtml "#)"

text   = strAttr "TEXT"

-- a box for displaying code
declBox :: Html -> HtmlTable
declBox html = tda [theclass "decl"] << html

-- a box for displaying documentation, 
-- indented and with a little padding at the top
docBox :: Html -> HtmlTable
docBox html = tda [theclass "doc"] << html

-- a box for displaying documentation, not indented.
ndocBox :: Html -> HtmlTable
ndocBox html = tda [theclass "ndoc"] << html

-- a box for displaying documentation, padded on the left a little
rdocBox :: Html -> HtmlTable
rdocBox html = tda [theclass "rdoc"] << html

maybeRDocBox :: Maybe Doc -> HtmlTable
maybeRDocBox Nothing = rdocBox (noHtml)
maybeRDocBox (Just doc) = rdocBox (docToHtml doc)

-- a box for the buttons at the top of the page
topButBox html = tda [theclass "topbut"] << html

-- a vanilla table has width 100%, no border, no padding, no spacing
-- a narrow table is the same but without width 100%.
vanillaTable = table ! [theclass "vanilla", cellspacing 0, cellpadding 0]
narrowTable  = table ! [theclass "narrow",  cellspacing 0, cellpadding 0]

spacedTable1 = table ! [theclass "vanilla",  cellspacing 1, cellpadding 0]
spacedTable5 = table ! [theclass "vanilla",  cellspacing 5, cellpadding 0]

constr_hdr = tda [ theclass "section4" ] << toHtml "Constructors"
meth_hdr   = tda [ theclass "section4" ] << toHtml "Methods"
inst_hdr   = tda [ theclass "section4" ] << toHtml "Instances"

dcolon = toHtml "::"
arrow  = toHtml "->"
darrow = toHtml "=>"

s8, s15 :: HtmlTable
s8  = tda [ theclass "s8" ]  << noHtml
s15 = tda [ theclass "s15" ] << noHtml
