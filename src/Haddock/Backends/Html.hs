--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.Backends.Html ( 
  ppHtml, copyHtmlBits, 
  ppHtmlIndex, ppHtmlContents,
  ppHtmlHelpFiles
) where


import Prelude hiding (div)

import Haddock.Backends.DevHelp
import Haddock.Backends.HH
import Haddock.Backends.HH2
import Haddock.ModuleTree
import Haddock.Types
import Haddock.Version
import Haddock.Utils
import Haddock.Utils.Html
import Haddock.GHC.Utils
import qualified Haddock.Utils.Html as Html

import Control.Exception     ( bracket )
import Control.Monad         ( when, unless )
import Data.Char             ( isUpper, toUpper )
import Data.List             ( sortBy )
import Data.Maybe            ( fromJust, isJust, mapMaybe, fromMaybe )
import Foreign.Marshal.Alloc ( allocaBytes )
import System.IO             ( IOMode(..), hClose, hGetBuf, hPutBuf, openFile )
import Data.Map              ( Map )
import qualified Data.Map as Map hiding ( Map )

import GHC hiding ( NoLink )
import Name
import Module
import PackageConfig         ( stringToPackageId )
import RdrName hiding ( Qual )
import SrcLoc   
import FastString            ( unpackFS )
import BasicTypes            ( IPName(..), Boxity(..) )
import Type                  ( Kind )
import Outputable            ( ppr, defaultUserStyle, showSDoc )

-- the base, module and entity URLs for the source code and wiki links.
type SourceURLs = (Maybe String, Maybe String, Maybe String)
type WikiURLs = (Maybe String, Maybe String, Maybe String)

-- -----------------------------------------------------------------------------
-- Generating HTML documentation

ppHtml	:: String
	-> Maybe String				-- package
	-> [Interface]
	-> FilePath			-- destination directory
	-> Maybe (GHC.HsDoc GHC.RdrName)    -- prologue text, maybe
	-> Maybe String		        -- the Html Help format (--html-help)
	-> SourceURLs			-- the source URL (--source)
	-> WikiURLs			-- the wiki URL (--wiki)
	-> Maybe String			-- the contents URL (--use-contents)
	-> Maybe String			-- the index URL (--use-index)
	-> IO ()

ppHtml doctitle maybe_package hmods odir prologue maybe_html_help_format
	maybe_source_url maybe_wiki_url
	maybe_contents_url maybe_index_url =  do
  let
	visible_hmods = filter visible hmods
	visible i = OptHide `notElem` hmod_options i

  when (not (isJust maybe_contents_url)) $ 
    ppHtmlContents odir doctitle maybe_package
        maybe_html_help_format maybe_index_url maybe_source_url maybe_wiki_url
	visible_hmods
	False -- we don't want to display the packages in a single-package contents
	prologue

  when (not (isJust maybe_index_url)) $ 
    ppHtmlIndex odir doctitle maybe_package maybe_html_help_format
      maybe_contents_url maybe_source_url maybe_wiki_url visible_hmods
    
  when (not (isJust maybe_contents_url && isJust maybe_index_url)) $ 
	ppHtmlHelpFiles doctitle maybe_package hmods odir maybe_html_help_format []

  mapM_ (ppHtmlModule odir doctitle
	   maybe_source_url maybe_wiki_url
	   maybe_contents_url maybe_index_url) visible_hmods

ppHtmlHelpFiles	
    :: String                   -- doctitle
    -> Maybe String				-- package
	-> [Interface]
	-> FilePath                 -- destination directory
	-> Maybe String             -- the Html Help format (--html-help)
	-> [FilePath]               -- external packages paths
	-> IO ()
ppHtmlHelpFiles doctitle maybe_package hmods odir maybe_html_help_format pkg_paths =  do
  let
	visible_hmods = filter visible hmods
	visible i = OptHide `notElem` hmod_options i

  -- Generate index and contents page for Html Help if requested
  case maybe_html_help_format of
    Nothing        -> return ()
    Just "mshelp"  -> ppHHProject odir doctitle maybe_package visible_hmods pkg_paths
    Just "mshelp2" -> do
		ppHH2Files      odir maybe_package visible_hmods pkg_paths
		ppHH2Collection odir doctitle maybe_package
    Just "devhelp" -> ppDevHelpFile odir doctitle maybe_package visible_hmods
    Just format    -> fail ("The "++format++" format is not implemented")

copyFile :: FilePath -> FilePath -> IO ()
copyFile fromFPath toFPath =
	(bracket (openFile fromFPath ReadMode) hClose $ \hFrom ->
	 bracket (openFile toFPath WriteMode) hClose $ \hTo ->
	 allocaBytes bufferSize $ \buffer ->
		copyContents hFrom hTo buffer)
	where
		bufferSize = 1024
		
		copyContents hFrom hTo buffer = do
			count <- hGetBuf hFrom buffer bufferSize
			when (count > 0) $ do
				hPutBuf hTo buffer count
				copyContents hFrom hTo buffer


copyHtmlBits :: FilePath -> FilePath -> Maybe FilePath -> IO ()
copyHtmlBits odir libdir maybe_css = do
  let 
	libhtmldir = pathJoin [libdir, "html"]
	css_file = case maybe_css of
			Nothing -> pathJoin [libhtmldir, cssFile]
			Just f  -> f
	css_destination = pathJoin [odir, cssFile]
	copyLibFile f = do
	   copyFile (pathJoin [libhtmldir, f]) (pathJoin [odir, f])
  copyFile css_file css_destination
  mapM_ copyLibFile [ iconFile, plusFile, minusFile, jsFile ]

footer :: HtmlTable
footer = 
  tda [theclass "botbar"] << 
	( toHtml "Produced by" <+> 
	  (anchor ! [href projectUrl] << toHtml projectName) <+>
	  toHtml ("version " ++ projectVersion)
	)
   
srcButton :: SourceURLs -> Maybe Interface -> HtmlTable
srcButton (Just src_base_url, _, _) Nothing =
  topButBox (anchor ! [href src_base_url] << toHtml "Source code")

srcButton (_, Just src_module_url, _) (Just hmod) =
  let url = spliceURL (Just $ hmod_orig_filename hmod)
                      (Just $ hmod_mod hmod) Nothing src_module_url
   in topButBox (anchor ! [href url] << toHtml "Source code")

srcButton _ _ =
  Html.emptyTable
 
spliceURL :: Maybe FilePath -> Maybe Module -> Maybe GHC.Name -> String -> String
spliceURL maybe_file maybe_mod maybe_name url = run url
 where
  file = fromMaybe "" maybe_file
  mod = case maybe_mod of
          Nothing           -> ""
          Just mod -> moduleString mod 
  
  (name, kind) =
    case maybe_name of
      Nothing             -> ("","")
      Just n | isValOcc (nameOccName n) -> (escapeStr (getOccString n), "v")
             | otherwise -> (escapeStr (getOccString n), "t")

  run "" = ""
  run ('%':'M':rest) = mod ++ run rest
  run ('%':'F':rest) = file ++ run rest
  run ('%':'N':rest) = name ++ run rest
  run ('%':'K':rest) = kind ++ run rest

  run ('%':'{':'M':'O':'D':'U':'L':'E':'}':rest) = mod ++ run rest
  run ('%':'{':'F':'I':'L':'E':'}':rest)         = file ++ run rest
  run ('%':'{':'N':'A':'M':'E':'}':rest)         = name ++ run rest
  run ('%':'{':'K':'I':'N':'D':'}':rest)         = kind ++ run rest

  run ('%':'{':'M':'O':'D':'U':'L':'E':'/':'.':'/':c:'}':rest) =
    map (\x -> if x == '.' then c else x) mod ++ run rest

  run (c:rest) = c : run rest
  
wikiButton :: WikiURLs -> Maybe Module -> HtmlTable
wikiButton (Just wiki_base_url, _, _) Nothing =
  topButBox (anchor ! [href wiki_base_url] << toHtml "User Comments")

wikiButton (_, Just wiki_module_url, _) (Just mod) =
  let url = spliceURL Nothing (Just mod) Nothing wiki_module_url
   in topButBox (anchor ! [href url] << toHtml "User Comments")

wikiButton _ _ =
  Html.emptyTable

contentsButton :: Maybe String -> HtmlTable
contentsButton maybe_contents_url 
  = topButBox (anchor ! [href url] << toHtml "Contents")
  where url = case maybe_contents_url of
			Nothing -> contentsHtmlFile
			Just url -> url

indexButton :: Maybe String -> HtmlTable
indexButton maybe_index_url 
  = topButBox (anchor ! [href url] << toHtml "Index")
  where url = case maybe_index_url of
			Nothing -> indexHtmlFile
			Just url -> url

simpleHeader :: String -> Maybe String -> Maybe String
             -> SourceURLs -> WikiURLs -> HtmlTable
simpleHeader doctitle maybe_contents_url maybe_index_url
  maybe_source_url maybe_wiki_url = 
  (tda [theclass "topbar"] << 
     vanillaTable << (
       (td << 
  	image ! [src "haskell_icon.gif", width "16", height 16, alt " " ]
       ) <->
       (tda [theclass "title"] << toHtml doctitle) <->
	srcButton maybe_source_url Nothing <->
        wikiButton maybe_wiki_url Nothing <->
	contentsButton maybe_contents_url <-> indexButton maybe_index_url
   ))

pageHeader :: String -> Interface -> String
    -> SourceURLs -> WikiURLs
    -> Maybe String -> Maybe String -> HtmlTable
pageHeader mdl hmod doctitle
           maybe_source_url maybe_wiki_url
           maybe_contents_url maybe_index_url =
  (tda [theclass "topbar"] << 
    vanillaTable << (
       (td << 
  	image ! [src "haskell_icon.gif", width "16", height 16, alt " "]
       ) <->
       (tda [theclass "title"] << toHtml doctitle) <->
	srcButton maybe_source_url (Just hmod) <->
	wikiButton maybe_wiki_url (Just $ hmod_mod hmod) <->
	contentsButton maybe_contents_url <->
	indexButton maybe_index_url
    )
   ) </>
   tda [theclass "modulebar"] <<
	(vanillaTable << (
	  (td << font ! [size "6"] << toHtml mdl) <->
	  moduleInfo hmod
	)
    )

moduleInfo :: Interface -> HtmlTable
moduleInfo hmod = 
   let
      info = hmod_info hmod

      doOneEntry :: (String, (GHC.HaddockModInfo GHC.Name) -> Maybe String) -> Maybe HtmlTable
      doOneEntry (fieldName,field) = case field info of
         Nothing -> Nothing
         Just fieldValue -> 
            Just ((tda [theclass "infohead"] << toHtml fieldName)
               <-> (tda [theclass "infoval"]) << toHtml fieldValue)
     
      entries :: [HtmlTable]
      entries = mapMaybe doOneEntry [
         ("Portability",GHC.hmi_portability),
         ("Stability",GHC.hmi_stability),
         ("Maintainer",GHC.hmi_maintainer)
         ]
   in
      case entries of
         [] -> Html.emptyTable
         _ -> tda [align "right"] << narrowTable << (foldl1 (</>) entries)

-- ---------------------------------------------------------------------------
-- Generate the module contents

ppHtmlContents
   :: FilePath
   -> String
   -> Maybe String
   -> Maybe String
   -> Maybe String
   -> SourceURLs
   -> WikiURLs
   -> [Interface] -> Bool -> Maybe (GHC.HsDoc GHC.RdrName)
   -> IO ()
ppHtmlContents odir doctitle
  maybe_package maybe_html_help_format maybe_index_url
  maybe_source_url maybe_wiki_url modules showPkgs prologue = do
  let tree = mkModuleTree showPkgs
         [(hmod_mod mod, toDescription mod) | mod <- modules]
      html = 
	header 
		(documentCharacterEncoding +++
		 thetitle (toHtml doctitle) +++
		 styleSheet +++
		 (script ! [src jsFile, thetype "text/javascript"] $ noHtml)) +++
        body << vanillaTable << (
   	    simpleHeader doctitle Nothing maybe_index_url
                         maybe_source_url maybe_wiki_url </>
	    ppPrologue doctitle prologue </>
	    ppModuleTree doctitle tree </>
	    s15 </>
	    footer
	  )
  writeFile (pathJoin [odir, contentsHtmlFile]) (renderHtml html)
  
  -- Generate contents page for Html Help if requested
  case maybe_html_help_format of
    Nothing        -> return ()
    Just "mshelp"  -> ppHHContents  odir doctitle maybe_package tree
    Just "mshelp2" -> ppHH2Contents odir doctitle maybe_package tree
    Just "devhelp" -> return ()
    Just format    -> fail ("The "++format++" format is not implemented")

ppPrologue :: String -> Maybe (GHC.HsDoc GHC.RdrName) -> HtmlTable
ppPrologue title Nothing = Html.emptyTable
ppPrologue title (Just doc) = 
  (tda [theclass "section1"] << toHtml title) </>
  docBox (rdrDocToHtml doc)

ppModuleTree :: String -> [ModuleTree] -> HtmlTable
ppModuleTree _ ts = 
  tda [theclass "section1"] << toHtml "Modules" </>
  td << vanillaTable2 << htmlTable
  where
    genTable htmlTable id []     = (htmlTable,id)
    genTable htmlTable id (x:xs) = genTable (htmlTable </> u) id' xs      
      where
        (u,id') = mkNode [] x 0 id

    (htmlTable,_) = genTable emptyTable 0 ts

mkNode :: [String] -> ModuleTree -> Int -> Int -> (HtmlTable,Int)
mkNode ss (Node s leaf pkg short ts) depth id = htmlNode
  where
    htmlNode = case ts of
      [] -> (td_pad_w 1.25 depth << htmlModule  <-> shortDescr <-> htmlPkg,id)
      _  -> (td_w depth << (collapsebutton id_s +++ htmlModule) <-> shortDescr <-> htmlPkg </> 
                (td_subtree << sub_tree), id')

    mod_width = 50::Int {-em-}

    td_pad_w pad depth = 
	tda [thestyle ("padding-left: " ++ show pad ++ "em;" ++
		       "width: " ++ show (mod_width - depth*2) ++ "em")]

    td_w depth = 
	tda [thestyle ("width: " ++ show (mod_width - depth*2) ++ "em")]

    td_subtree =
	tda [thestyle ("padding: 0; padding-left: 2em")]

    shortDescr :: HtmlTable
    shortDescr = case short of
	Nothing -> td empty
	Just doc -> tda [theclass "rdoc"] (origDocToHtml doc)

    htmlModule 
      | leaf      = ppModule (mkModule (stringToPackageId pkgName) 
                                       (mkModuleName mdl)) ""
      | otherwise = toHtml s

    -- ehm.. TODO: change the ModuleTree type
    (htmlPkg, pkgName) = case pkg of
      Nothing -> (td << empty, "")
      Just p  -> (td << toHtml p, p)

    mdl = foldr (++) "" (s' : map ('.':) ss')
    (s':ss') = reverse (s:ss)
	 -- reconstruct the module name
    
    id_s = "n:" ++ show id
    
    (sub_tree,id') = genSubTree emptyTable (id+1) ts
    
    genSubTree :: HtmlTable -> Int -> [ModuleTree] -> (Html,Int)
    genSubTree htmlTable id [] = (sub_tree,id)
      where
        sub_tree = collapsed vanillaTable2 id_s htmlTable
    genSubTree htmlTable id (x:xs) = genSubTree (htmlTable </> u) id' xs      
      where
        (u,id') = mkNode (s:ss) x (depth+1) id

-- The URL for source and wiki links, and the current module
type LinksInfo = (SourceURLs, WikiURLs, Interface)


-- ---------------------------------------------------------------------------
-- Generate the index

ppHtmlIndex :: FilePath
            -> String 
            -> Maybe String
            -> Maybe String
            -> Maybe String
            -> SourceURLs
            -> WikiURLs
            -> [Interface] 
            -> IO ()
ppHtmlIndex odir doctitle maybe_package maybe_html_help_format
  maybe_contents_url maybe_source_url maybe_wiki_url modules = do
  let html = 
	header (documentCharacterEncoding +++
		thetitle (toHtml (doctitle ++ " (Index)")) +++
		styleSheet) +++
        body << vanillaTable << (
	    simpleHeader doctitle maybe_contents_url Nothing
                         maybe_source_url maybe_wiki_url </>
	    index_html
	   )

  when split_indices $
    mapM_ (do_sub_index index) initialChars

  writeFile (pathJoin [odir, indexHtmlFile]) (renderHtml html)
  
    -- Generate index and contents page for Html Help if requested
  case maybe_html_help_format of
    Nothing        -> return ()
    Just "mshelp"  -> ppHHIndex  odir maybe_package modules
    Just "mshelp2" -> ppHH2Index odir maybe_package modules
    Just "devhelp" -> return ()
    Just format    -> fail ("The "++format++" format is not implemented")
 where
  split_indices = length index > 50

  index_html
    | split_indices = 
	tda [theclass "section1"] << 
	      	toHtml ("Index") </>
	indexInitialLetterLinks
   | otherwise =
	td << table ! [cellpadding 0, cellspacing 5] <<
	  aboves (map indexElt index) 
 	
  indexInitialLetterLinks = 
	td << table ! [cellpadding 0, cellspacing 5] <<
	    besides [ td << anchor ! [href (subIndexHtmlFile c)] <<
			 toHtml [c]
		    | c <- initialChars
                    , any ((==c) . toUpper . head . fst) index ]

  do_sub_index this_ix c
    = unless (null index_part) $
        writeFile (pathJoin [odir, subIndexHtmlFile c]) (renderHtml html)
    where 
      html = header (documentCharacterEncoding +++
		thetitle (toHtml (doctitle ++ " (Index)")) +++
		styleSheet) +++
             body << vanillaTable << (
	        simpleHeader doctitle maybe_contents_url Nothing
                             maybe_source_url maybe_wiki_url </>
		indexInitialLetterLinks </>
	        tda [theclass "section1"] << 
	      	toHtml ("Index (" ++ c:")") </>
	        td << table ! [cellpadding 0, cellspacing 5] <<
	      	  aboves (map indexElt index_part) 
	       )

      index_part = [(n,stuff) | (n,stuff) <- this_ix, toUpper (head n) == c]

  index :: [(String, Map GHC.Name [(Module,Bool)])]
  index = sortBy cmp (Map.toAscList full_index)
    where cmp (n1,_) (n2,_) = n1 `compare` n2

  -- for each name (a plain string), we have a number of original HsNames that
  -- it can refer to, and for each of those we have a list of modules
  -- that export that entity.  Each of the modules exports the entity
  -- in a visible or invisible way (hence the Bool).
  full_index :: Map String (Map GHC.Name [(Module,Bool)])
  full_index = Map.fromListWith (flip (Map.unionWith (++)))
		(concat (map getHModIndex modules))

  getHModIndex hmod = 
    [ (getOccString name, 
	Map.fromList [(name, [(mdl, name `elem` hmod_visible_exports hmod)])])
    | name <- hmod_exports hmod ]
    where mdl = hmod_mod hmod

  indexElt :: (String, Map GHC.Name [(Module,Bool)]) -> HtmlTable
  indexElt (str, entities) = 
     case Map.toAscList entities of
	[(nm,entries)] ->  
	    tda [ theclass "indexentry" ] << toHtml str <-> 
			indexLinks nm entries
	many_entities ->
	    tda [ theclass "indexentry" ] << toHtml str </> 
		aboves (map doAnnotatedEntity (zip [1..] many_entities))

  doAnnotatedEntity (j,(nm,entries))
	= tda [ theclass "indexannot" ] << 
		toHtml (show j) <+> parens (ppAnnot (nameOccName nm)) <->
		 indexLinks nm entries

  ppAnnot n | not (isValOcc n) = toHtml "Type/Class"
            | isDataOcc n      = toHtml "Data Constructor"
            | otherwise        = toHtml "Function"

  indexLinks nm entries = 
     tda [ theclass "indexlinks" ] << 
	hsep (punctuate comma 
	[ if visible then
	     linkId mod (Just nm) << toHtml (moduleString mod)
	  else
	     toHtml (moduleString mod)
	| (mod, visible) <- entries ])

  initialChars = [ 'A'..'Z' ] ++ ":!#$%&*+./<=>?@\\^|-~"

-- ---------------------------------------------------------------------------
-- Generate the HTML page for a module

ppHtmlModule
	:: FilePath -> String
	-> SourceURLs -> WikiURLs
	-> Maybe String -> Maybe String
	-> Interface -> IO ()
ppHtmlModule odir doctitle
  maybe_source_url maybe_wiki_url
  maybe_contents_url maybe_index_url hmod = do
  let 
      mod = hmod_mod hmod
      mdl = moduleString mod
      html = 
	header (documentCharacterEncoding +++
		thetitle (toHtml mdl) +++
		styleSheet +++
		(script ! [src jsFile, thetype "text/javascript"] $ noHtml)) +++
        body << vanillaTable << (
	    pageHeader mdl hmod doctitle
		maybe_source_url maybe_wiki_url
		maybe_contents_url maybe_index_url </> s15 </>
	    hmodToHtml maybe_source_url maybe_wiki_url hmod </> s15 </>
	    footer
         )
  writeFile (pathJoin [odir, moduleHtmlFile mod]) (renderHtml html)

hmodToHtml :: SourceURLs -> WikiURLs -> Interface -> HtmlTable
hmodToHtml maybe_source_url maybe_wiki_url hmod
  = abovesSep s15 (contents: description: synopsis: maybe_doc_hdr: bdy)
  where
        docMap = hmod_rn_doc_map hmod
 
	exports = numberSectionHeadings (hmod_rn_export_items hmod)

	has_doc (ExportDecl _ _ doc _) = isJust doc
	has_doc (ExportNoDecl _ _ _) = False
	has_doc (ExportModule _) = False
	has_doc _ = True

	no_doc_at_all = not (any has_doc exports)

 	contents = td << vanillaTable << ppModuleContents exports

	description
          = case hmod_rn_doc hmod of
              Nothing -> Html.emptyTable
              Just doc -> (tda [theclass "section1"] << toHtml "Description") </>
                          docBox (docToHtml doc)

	-- omit the synopsis if there are no documentation annotations at all
	synopsis
	  | no_doc_at_all = Html.emptyTable
	  | otherwise
	  = (tda [theclass "section1"] << toHtml "Synopsis") </>
	    s15 </>
            (tda [theclass "body"] << vanillaTable <<
  	        abovesSep s8 (map (processExport True linksInfo docMap)
			(filter forSummary exports))
	    )

	-- if the documentation doesn't begin with a section header, then
	-- add one ("Documentation").
	maybe_doc_hdr
	    = case exports of		   
		   [] -> Html.emptyTable
		   ExportGroup _ _ _ : _ -> Html.emptyTable
		   _ -> tda [ theclass "section1" ] << toHtml "Documentation"

	bdy  = map (processExport False linksInfo docMap) exports
	linksInfo = (maybe_source_url, maybe_wiki_url, hmod)

ppModuleContents :: [ExportItem DocName] -> HtmlTable
ppModuleContents exports
  | length sections == 0 = Html.emptyTable
  | otherwise            = tda [theclass "section4"] << bold << toHtml "Contents"
  		           </> td << dlist << concatHtml sections
 where
  (sections, _leftovers{-should be []-}) = process 0 exports

  process :: Int -> [ExportItem DocName] -> ([Html],[ExportItem DocName])
  process _ [] = ([], [])
  process n items@(ExportGroup lev id0 doc : rest) 
    | lev <= n  = ( [], items )
    | otherwise = ( html:secs, rest2 )
    where
	html = (dterm << linkedAnchor id0 << docToHtml doc)
		 +++ mk_subsections ssecs
	(ssecs, rest1) = process lev rest
	(secs,  rest2) = process n   rest1
  process n (_ : rest) = process n rest

  mk_subsections [] = noHtml
  mk_subsections ss = ddef << dlist << concatHtml ss

-- we need to assign a unique id to each section heading so we can hyperlink
-- them from the contents:
numberSectionHeadings :: [ExportItem DocName] -> [ExportItem DocName]
numberSectionHeadings exports = go 1 exports
  where go :: Int -> [ExportItem DocName] -> [ExportItem DocName]
        go _ [] = []
	go n (ExportGroup lev _ doc : es) 
	  = ExportGroup lev (show n) doc : go (n+1) es
	go n (other:es)
	  = other : go n es

processExport :: Bool -> LinksInfo -> DocMap -> (ExportItem DocName) -> HtmlTable
processExport _ _ _ (ExportGroup lev id0 doc)
  = ppDocGroup lev (namedAnchor id0 << docToHtml doc)
processExport summary links docMap (ExportDecl x decl doc insts)
  = doDecl summary links x decl doc insts docMap
processExport summmary _ _ (ExportNoDecl _ y [])
  = declBox (ppDocName y)
processExport summmary _ _ (ExportNoDecl _ y subs)
  = declBox (ppDocName y <+> parenList (map ppDocName subs))
processExport _ _ _ (ExportDoc doc)
  = docBox (docToHtml doc)
processExport _ _ _ (ExportModule mod)
  = declBox (toHtml "module" <+> ppModule mod "")

forSummary :: (ExportItem DocName) -> Bool
forSummary (ExportGroup _ _ _) = False
forSummary (ExportDoc _)       = False
forSummary _                    = True

ppDocGroup :: Int -> Html -> HtmlTable
ppDocGroup lev doc
  | lev == 1  = tda [ theclass "section1" ] << doc
  | lev == 2  = tda [ theclass "section2" ] << doc
  | lev == 3  = tda [ theclass "section3" ] << doc
  | otherwise = tda [ theclass "section4" ] << doc

declWithDoc :: Bool -> LinksInfo -> SrcSpan -> Name -> Maybe (HsDoc DocName) -> Html -> HtmlTable
declWithDoc True  _     _   _  _          html_decl = declBox html_decl
declWithDoc False links loc nm Nothing    html_decl = topDeclBox links loc nm html_decl
declWithDoc False links loc nm (Just doc) html_decl = 
		topDeclBox links loc nm html_decl </> docBox (docToHtml doc)

doDecl :: Bool -> LinksInfo -> Name -> LHsDecl DocName -> 
          Maybe (HsDoc DocName) -> [InstHead DocName] -> DocMap -> HtmlTable
doDecl summary links x (L loc d) mbDoc instances docMap = doDecl d
  where
    doDecl (TyClD d) = doTyClD d 
    doDecl (SigD s) = ppSig summary links loc mbDoc s
    doDecl (ForD d) = ppFor summary links loc mbDoc d

    doTyClD d0@(TyData {}) = ppDataDecl summary links instances x loc mbDoc d0
    doTyClD d0@(TySynonym {}) = ppTySyn summary links loc mbDoc d0
    doTyClD d0@(ClassDecl {}) = ppClassDecl summary links instances x loc mbDoc docMap d0

ppSig :: Bool -> LinksInfo -> SrcSpan -> Maybe (HsDoc DocName) -> Sig DocName -> HtmlTable
ppSig summary links loc mbDoc (TypeSig lname ltype) 
  | summary || noArgDocs t = 
    declWithDoc summary links loc n mbDoc (ppTypeSig summary n t)
  | otherwise = topDeclBox links loc n (ppBinder False n) </>
    (tda [theclass "body"] << vanillaTable <<  (
      do_args dcolon t </>
        (case mbDoc of 
          Just doc -> ndocBox (docToHtml doc)
          Nothing -> Html.emptyTable)
	))

  where 
  t = unLoc ltype
  NoLink n = unLoc lname

  noLArgDocs (L _ t) = noArgDocs t
  noArgDocs (HsForAllTy _ _ _ t) = noLArgDocs t
  noArgDocs (HsFunTy (L _ (HsDocTy _ _)) _) = False 
  noArgDocs (HsFunTy _ r) = noLArgDocs r
  noArgDocs (HsDocTy _ _) = False
  noArgDocs _ = True

  do_largs leader (L _ t) = do_args leader t  
  do_args :: Html -> (HsType DocName) -> HtmlTable
  do_args leader (HsForAllTy Explicit tvs lctxt ltype)
    = (argBox (
        leader <+> 
        hsep (keyword "forall" : ppTyVars tvs ++ [dot]) <+>
        ppLContextNoArrow lctxt)
          <-> rdocBox noHtml) </> 
          do_largs darrow ltype
  do_args leader (HsForAllTy Implicit _ lctxt ltype)
    = (argBox (leader <+> ppLContextNoArrow lctxt)
        <-> rdocBox noHtml) </> 
        do_largs darrow ltype
  do_args leader (HsFunTy (L _ (HsDocTy lt ldoc)) r)
    = (argBox (leader <+> ppLType lt) <-> rdocBox (docToHtml (unLoc ldoc)))
        </> do_largs arrow r
  do_args leader (HsFunTy lt r)
    = (argBox (leader <+> ppLType lt) <-> rdocBox noHtml) </> do_largs arrow r
  do_args leader (HsDocTy lt ldoc)
    = (argBox (leader <+> ppLType lt) <-> rdocBox (docToHtml (unLoc ldoc)))
  do_args leader t
    = argBox (leader <+> ppType t) <-> rdocBox (noHtml)

ppTyVars tvs = map ppName (tyvarNames tvs)

tyvarNames = map f 
  where f x = let NoLink n = hsTyVarName (unLoc x) in n
  
ppFor summary links loc mbDoc (ForeignImport lname ltype _)
  = ppSig summary links loc mbDoc (TypeSig lname ltype)
ppFor _ _ _ _ _ = error "ppFor"

-- we skip type patterns for now
ppTySyn summary links loc mbDoc (TySynonym lname ltyvars _ ltype) 
  = declWithDoc summary links loc n mbDoc (
    hsep ([keyword "type", ppBinder summary n]
    ++ ppTyVars ltyvars) <+> equals <+> ppLType ltype)
  where NoLink n = unLoc lname

ppLType (L _ t) = ppType t

ppTypeSig :: Bool -> Name -> (HsType DocName) -> Html
ppTypeSig summary nm ty = ppBinder summary nm <+> dcolon <+> ppType ty

--------------------------------------------------------------------------------
-- Contexts 
--------------------------------------------------------------------------------

ppLContext        = ppContext        . unLoc
ppLContextNoArrow = ppContextNoArrow . unLoc

ppContextNoArrow :: HsContext DocName -> Html
ppContextNoArrow []  = empty
ppContextNoArrow cxt = pp_hs_context (map unLoc cxt) 

ppContextNoLocs :: [HsPred DocName] -> Html
ppContextNoLocs []  = empty
ppContextNoLocs cxt = pp_hs_context cxt <+> darrow  

ppContext :: HsContext DocName -> Html
ppContext cxt = ppContextNoLocs (map unLoc cxt)

pp_hs_context []  = empty
pp_hs_context [p] = ppPred p
pp_hs_context cxt = parenList (map ppPred cxt) 

ppLPred = ppPred . unLoc

ppPred (HsClassP n ts) = ppDocName n <+> hsep (map ppLType ts)
-- TODO: find out what happened to the Dupable/Linear distinction
ppPred (HsIParam (IPName n) t) 
  = toHtml "?" +++ ppDocName n <+> dcolon <+> ppLType t

-- -----------------------------------------------------------------------------
-- Class declarations

ppClassHdr summ (L _ []) n tvs fds = 
  keyword "class"
	<+> ppBinder summ n <+> hsep (ppTyVars tvs)
	<+> ppFds fds
ppClassHdr summ lctxt n tvs fds = 
  keyword "class" <+> ppLContext lctxt
	<+> ppBinder summ n <+> hsep (ppTyVars tvs)
	<+> ppFds fds

ppFds fds =
  if null fds then noHtml else 
	char '|' <+> hsep (punctuate comma (map (fundep . unLoc) fds))
  where
	fundep (vars1,vars2) = hsep (map ppDocName vars1) <+> toHtml "->" <+>
			       hsep (map ppDocName vars2)

ppShortClassDecl :: Bool -> LinksInfo -> TyClDecl DocName -> SrcSpan -> DocMap -> HtmlTable
ppShortClassDecl summary links (ClassDecl lctxt lname tvs fds sigs _ ats _) loc docMap = 
  if null sigs && null ats
    then (if summary then declBox else topDeclBox links loc nm) hdr
    else (if summary then declBox else topDeclBox links loc nm) (hdr <+> keyword "where")
	    </> 
           (tda [theclass "body"] << 
	     vanillaTable << 
         aboves ([ ppAT summary at | L _ at <- ats ] ++
	        [ ppSig summary links loc mbDoc sig  
		      | L _ sig@(TypeSig (L _ (NoLink n)) ty) <- sigs, let mbDoc = Map.lookup n docMap ])
          )
  where
    hdr = ppClassHdr summary lctxt nm tvs fds
    NoLink nm = unLoc lname
    
    ppAT summary at = case at of
      TyData {} -> topDeclBox links loc nm (ppDataHeader summary at)
      _ -> error "associated type synonyms or type families not supported yet"

-- we skip ATs for now
ppClassDecl :: Ord key => Bool -> LinksInfo -> [InstHead DocName] -> key -> SrcSpan ->
                          Maybe (HsDoc DocName) -> DocMap -> TyClDecl DocName -> 
                          HtmlTable
ppClassDecl summary links instances orig_c loc mbDoc docMap
	decl@(ClassDecl lctxt lname ltyvars lfds lsigs _ _ _)
  | summary = ppShortClassDecl summary links decl loc docMap
  | otherwise
    = classheader </>
      tda [theclass "body"] << vanillaTable << (
        classdoc </> methodsBit </> instancesBit
      )
  where 
    classheader
      | null lsigs = topDeclBox links loc nm hdr
      | otherwise  = topDeclBox links loc nm (hdr <+> keyword "where")

    NoLink nm = unLoc lname
    ctxt = unLoc lctxt

    hdr = ppClassHdr summary lctxt nm ltyvars lfds
    
    classdoc = case mbDoc of
      Nothing -> Html.emptyTable
      Just d -> ndocBox (docToHtml d)

    methodsBit
      | null lsigs = Html.emptyTable
      | otherwise  = 
        s8 </> methHdr </>
        tda [theclass "body"] << vanillaTable << (
          abovesSep s8 [ ppSig summary links loc mbDoc sig
                         | L _ sig@(TypeSig n _) <- lsigs, 
                         let mbDoc = Map.lookup (orig n) docMap ]
        )

    instId = collapseId nm
    instancesBit
      | null instances = Html.emptyTable
      | otherwise 
        =  s8 </> instHdr instId </>
           tda [theclass "body"] << 
             collapsed thediv instId (
             spacedTable1 << (
               aboves (map (declBox . ppInstHead) instances)
             ))

ppInstHead :: InstHead DocName -> Html
ppInstHead ([],   n, ts) = ppAsst n ts 
ppInstHead (ctxt, n, ts) = ppContextNoLocs ctxt <+> ppAsst n ts 

ppAsst n ts = ppDocName n <+> hsep (map ppParendType ts)

-- -----------------------------------------------------------------------------
-- Data & newtype declarations

orig (L _ (NoLink name)) = name
orig _ = error "orig"

-- TODO: print contexts
ppShortDataDecl :: Bool -> LinksInfo -> SrcSpan -> 
                   Maybe (HsDoc DocName) -> TyClDecl DocName -> Html
ppShortDataDecl summary links loc mbDoc dataDecl 

  | [lcon] <- cons, ResTyH98 <- resTy = 
    ppDataHeader summary dataDecl 
    <+> equals <+> ppShortConstr summary (unLoc lcon)

  | [] <- cons = ppDataHeader summary dataDecl

  | otherwise = vanillaTable << (
      case resTy of 
        ResTyH98 -> dataHeader </> 
          tda [theclass "body"] << vanillaTable << (
            aboves (zipWith doConstr ('=':repeat '|') cons)
          )
        ResTyGADT _ -> dataHeader </> 
          tda [theclass "body"] << vanillaTable << (
            aboves (map doGADTConstr cons)
          )
    )
  
  where
    dataHeader = 
      (if summary then declBox else topDeclBox links loc name)
      ((ppDataHeader summary dataDecl) <+> 
      case resTy of ResTyGADT _ -> keyword "where"; _ -> empty)

    doConstr c con = declBox (toHtml [c] <+> ppShortConstr summary (unLoc con))
    doGADTConstr con = declBox (ppShortConstr summary (unLoc con))

    name      = orig (tcdLName dataDecl)
    context   = unLoc (tcdCtxt dataDecl)
    newOrData = tcdND dataDecl
    tyVars    = tyvarNames (tcdTyVars dataDecl)
    mbKSig    = tcdKindSig dataDecl
    cons      = tcdCons dataDecl
    resTy     = (con_res . unLoc . head) cons 

ppDataDecl :: Ord key => Bool -> LinksInfo -> [InstHead DocName] -> key -> 
              SrcSpan -> Maybe (HsDoc DocName) -> TyClDecl DocName -> HtmlTable
ppDataDecl summary links instances x loc mbDoc dataDecl
  
  | summary = declWithDoc summary links loc name mbDoc 
              (ppShortDataDecl summary links loc mbDoc dataDecl)
  
  | otherwise = dataHeader </> 
    tda [theclass "body"] << vanillaTable << (
      datadoc </> 
      constrBit </>
      instancesBit
    )
  
  where
    name      = orig (tcdLName dataDecl)
    context   = unLoc (tcdCtxt dataDecl)
    newOrData = tcdND dataDecl
    tyVars    = tyvarNames (tcdTyVars dataDecl)
    mbKSig    = tcdKindSig dataDecl
    cons      = tcdCons dataDecl
    resTy     = (con_res . unLoc . head) cons 
      
    dataHeader = 
      (if summary then declBox else topDeclBox links loc name)
      ((ppDataHeader summary dataDecl) <+> whereBit)

    whereBit 
      | null cons = empty 
      | otherwise = case resTy of 
        ResTyGADT _ -> keyword "where"
        _ -> empty                         

    constrTable
      | any isRecCon cons = spacedTable5
      | otherwise         = spacedTable1

    datadoc = case mbDoc of
      Just doc -> ndocBox (docToHtml doc)
      Nothing -> Html.emptyTable

    constrBit 
      | null cons = Html.emptyTable
      | otherwise = constrHdr </> ( 
          tda [theclass "body"] << constrTable << 
	  aboves (map ppSideBySideConstr cons)
        )

    instId = collapseId name

    instancesBit
      | null instances = Html.emptyTable
      | otherwise 
        = instHdr instId </>
	  tda [theclass "body"] << 
          collapsed thediv instId (
            spacedTable1 << (
              aboves (map (declBox . ppInstHead) instances)
            )
          )

isRecCon lcon = case con_details (unLoc lcon) of 
  RecCon _ -> True
  _ -> False

ppShortConstr :: Bool -> ConDecl DocName -> Html
ppShortConstr summary con = case con_res con of 

  ResTyH98 -> case con_details con of 
    PrefixCon args -> header +++ hsep (ppBinder summary name : map ppLType args)
    RecCon fields -> header +++ ppBinder summary name <+>
      braces (vanillaTable << aboves (map (ppShortField summary) fields))
    InfixCon arg1 arg2 -> header +++ 
      hsep [ppLType arg1, ppBinder summary name, ppLType arg2]    

  ResTyGADT resTy -> case con_details con of 
    PrefixCon args -> doGADTCon args resTy
    RecCon _ -> error "GADT records not suported"
    InfixCon arg1 arg2 -> doGADTCon [arg1, arg2] resTy 
    
  where
    doGADTCon args resTy = ppBinder summary name <+> dcolon <+> hsep [
                             ppForAll forall ltvs lcontext,
                             ppLType (foldr mkFunTy resTy args) ]

    header   = ppConstrHdr forall tyVars context
    name     = orig (con_name con)
    ltvs     = con_qvars con
    tyVars   = tyvarNames ltvs 
    lcontext = con_cxt con
    context  = unLoc (con_cxt con)
    forall   = con_explicit con
    mkFunTy a b = noLoc (HsFunTy a b)

ppConstrHdr :: HsExplicitForAll -> [Name] -> HsContext DocName -> Html
ppConstrHdr forall tvs ctxt
 = (if null tvs then noHtml else ppForall)
   +++
   (if null ctxt then noHtml else ppContext ctxt <+> toHtml "=> ")
  where
    ppForall = case forall of 
      Explicit -> keyword "forall" <+> hsep (map ppName tvs) <+> toHtml ". "
      Implicit -> empty

ppSideBySideConstr :: LConDecl DocName -> HtmlTable
ppSideBySideConstr (L _ con) = case con_res con of 
 
  ResTyH98 -> case con_details con of 

    PrefixCon args -> 
      argBox (hsep ((header +++ ppBinder False name) : map ppLType args)) 
      <-> maybeRDocBox mbLDoc  

    RecCon fields -> 
      argBox (header +++ ppBinder False name) <->
      maybeRDocBox mbLDoc </>
      (tda [theclass "body"] << spacedTable1 <<
      aboves (map ppSideBySideField fields))

    InfixCon arg1 arg2 -> 
      argBox (hsep [header+++ppLType arg1, ppBinder False name, ppLType arg2])
      <-> maybeRDocBox mbLDoc
 
  ResTyGADT resTy -> case con_details con of
    PrefixCon args -> doGADTCon args resTy
    RecCon _ -> error "GADT records not supported"
    InfixCon arg1 arg2 -> doGADTCon [arg1, arg2] resTy 

 where 
    doGADTCon args resTy = argBox (ppBinder False name <+> dcolon <+> hsep [
                               ppForAll forall ltvs (con_cxt con),
                               ppLType (foldr mkFunTy resTy args) ]
                            ) <-> maybeRDocBox mbLDoc


    header  = ppConstrHdr forall tyVars context
    name    = orig (con_name con)
    ltvs    = con_qvars con
    tyVars  = tyvarNames (con_qvars con)
    context = unLoc (con_cxt con)
    forall  = con_explicit con
    mbLDoc  = con_doc con
    mkFunTy a b = noLoc (HsFunTy a b)

ppSideBySideField :: ConDeclField DocName -> HtmlTable
ppSideBySideField (ConDeclField lname ltype mbLDoc) =
  argBox (ppBinder False (orig lname)
    <+> dcolon <+> ppLType ltype) <->
  maybeRDocBox mbLDoc

{-
ppHsFullConstr :: HsConDecl -> Html
ppHsFullConstr (HsConDecl _ nm tvs ctxt typeList doc) = 
     declWithDoc False doc (
	hsep ((ppHsConstrHdr tvs ctxt +++ 
		ppHsBinder False nm) : map ppHsBangType typeList)
      )
ppHsFullConstr (HsRecDecl _ nm tvs ctxt fields doc) =
   td << vanillaTable << (
     case doc of
       Nothing -> aboves [hdr, fields_html]
       Just _  -> aboves [hdr, constr_doc, fields_html]
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
-}

ppShortField :: Bool -> ConDeclField DocName -> HtmlTable
ppShortField summary (ConDeclField lname ltype _) 
  = tda [theclass "recfield"] << (
      ppBinder summary (orig lname)
      <+> dcolon <+> ppLType ltype
    )

{-
ppFullField :: HsFieldDecl -> Html
ppFullField (HsFieldDecl [n] ty doc) 
  = declWithDoc False doc (
	ppHsBinder False n <+> dcolon <+> ppHsBangType ty
    )
ppFullField _ = error "ppFullField"

expandField :: HsFieldDecl -> [HsFieldDecl]
expandField (HsFieldDecl ns ty doc) = [ HsFieldDecl [n] ty doc | n <- ns ]
-}

-- | Print the LHS of a data/newtype declaration.
-- Currently doesn't handle 'data instance' decls or kind signatures
ppDataHeader :: Bool -> TyClDecl DocName -> Html
ppDataHeader summary decl 
  | not (isDataDecl decl) = error "ppDataHeader: illegal argument"
  | otherwise = 
    -- newtype or data
    (if tcdND decl == NewType then keyword "newtype" else keyword "data") <+> 
    -- context
    ppLContext (tcdCtxt decl) <+>
    -- T a b c ..., or a :+: b  
    (if isConSym name 
      then ppName (tyvars!!0) <+> ppBinder summary name <+> ppName (tyvars!!1)
      else ppBinder summary name <+> hsep (map ppName tyvars))
  where 
    tyvars = tyvarNames $ tcdTyVars decl
    name = orig $ tcdLName decl

-- ----------------------------------------------------------------------------
-- Types and contexts

ppKind k = toHtml $ showSDoc (ppr k)

{-
ppForAll Implicit _ lctxt = ppCtxtPart lctxt
ppForAll Explicit ltvs lctxt = 
  hsep (keyword "forall" : ppTyVars ltvs ++ [dot]) <+> ppCtxtPart lctxt 
-}

ppBang HsStrict = toHtml "!"
ppBang HsUnbox  = toHtml "!!"

tupleParens Boxed   = parenList
tupleParens Unboxed = ubxParenList 
{-
ppType :: HsType DocName -> Html
ppType t = case t of
  t@(HsForAllTy expl ltvs lcontext ltype) -> ppForAllTy t <+> ppLType ltype
  HsTyVar n -> ppDocName n
  HsBangTy HsStrict lt -> toHtml "!" <+> ppLType lt
  HsBangTy HsUnbox lt -> toHtml "!!" <+> ppLType lt
  HsAppTy a b -> ppLType a <+> ppLType b 
  HsFunTy a b -> hsep [ppLType a, toHtml "->", ppLType b]
  HsListTy t -> brackets $ ppLType t
  HsPArrTy t -> toHtml "[:" +++ ppLType t +++ toHtml ":]"
  HsTupleTy Boxed ts -> parenList $ map ppLType ts
  HsTupleTy Unboxed ts -> ubxParenList $ map ppLType ts
  HsOpTy a n b -> ppLType a <+> ppLDocName n <+> ppLType b
  HsParTy t -> parens $ ppLType t
  HsNumTy n -> toHtml (show n)
  HsPredTy p -> ppPred p
  HsKindSig t k -> hsep [ppLType t, dcolon, ppKind k]
  HsSpliceTy _ -> error "ppType"
  HsDocTy t _ -> ppLType t
-}
--------------------------------------------------------------------------------
-- Rendering of HsType 
--------------------------------------------------------------------------------

pREC_TOP = (0 :: Int)   -- type in ParseIface.y in GHC
pREC_FUN = (1 :: Int)   -- btype in ParseIface.y in GHC
                        -- Used for LH arg of (->)
pREC_OP  = (2 :: Int)   -- Used for arg of any infix operator
                        -- (we don't keep their fixities around)
pREC_CON = (3 :: Int)   -- Used for arg of type applicn:
                        -- always parenthesise unless atomic

maybeParen :: Int           -- Precedence of context
           -> Int           -- Precedence of top-level operator
           -> Html -> Html  -- Wrap in parens if (ctxt >= op)
maybeParen ctxt_prec op_prec p | ctxt_prec >= op_prec = parens p
                               | otherwise            = p

ppType ty       = ppr_mono_ty pREC_TOP (prepare ty)
ppParendType ty = ppr_mono_ty pREC_CON ty

-- Before printing a type
-- (a) Remove outermost HsParTy parens
-- (b) Drop top-level for-all type variables in user style
--     since they are implicit in Haskell
prepare (HsParTy ty) = prepare (unLoc ty)
prepare ty           = ty

ppForAll exp tvs cxt 
  | show_forall = forall_part <+> ppLContext cxt
  | otherwise   = ppLContext cxt
  where
    show_forall = not (null tvs) && is_explicit
    is_explicit = case exp of {Explicit -> True; Implicit -> False}
    forall_part = hsep (keyword "forall" : ppTyVars tvs) +++ dot 

ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)

ppr_mono_ty ctxt_prec (HsForAllTy exp tvs ctxt ty)
  = maybeParen ctxt_prec pREC_FUN $
    hsep [ppForAll exp tvs ctxt, ppr_mono_lty pREC_TOP ty]

-- gaw 2004
ppr_mono_ty ctxt_prec (HsBangTy b ty)     = ppBang b +++ ppLType ty
ppr_mono_ty ctxt_prec (HsTyVar name)      = ppDocName name
ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)   = ppr_fun_ty ctxt_prec ty1 ty2
ppr_mono_ty ctxt_prec (HsTupleTy con tys) = tupleParens con (map ppLType tys)
ppr_mono_ty ctxt_prec (HsKindSig ty kind) = parens (ppr_mono_lty pREC_TOP ty <+> dcolon <+> ppKind kind)
ppr_mono_ty ctxt_prec (HsListTy ty)       = brackets (ppr_mono_lty pREC_TOP ty)
ppr_mono_ty ctxt_prec (HsPArrTy ty)       = pabrackets (ppr_mono_lty pREC_TOP ty)
ppr_mono_ty ctxt_prec (HsPredTy pred)     = parens (ppPred pred)
ppr_mono_ty ctxt_prec (HsNumTy n)         = toHtml (show n) -- generics only
ppr_mono_ty ctxt_prec (HsSpliceTy s)      = error "ppr_mono_ty-haddock"

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty, ppr_mono_lty pREC_CON arg_ty]

ppr_mono_ty ctxt_prec (HsOpTy ty1 op ty2)
  = maybeParen ctxt_prec pREC_OP $
    ppr_mono_lty pREC_OP ty1 <+> ppLDocName op <+> ppr_mono_lty pREC_OP ty2

ppr_mono_ty ctxt_prec (HsParTy ty)
  = parens (ppr_mono_lty pREC_TOP ty)

ppr_mono_ty ctxt_prec (HsDocTy ty doc)
  = ppLType ty

ppr_fun_ty ctxt_prec ty1 ty2
  = let p1 = ppr_mono_lty pREC_FUN ty1
        p2 = ppr_mono_lty pREC_TOP ty2
    in
    maybeParen ctxt_prec pREC_FUN $
    hsep [p1, arrow <+> p2]

-- ----------------------------------------------------------------------------
-- Names

ppOccName :: OccName -> Html
ppOccName name = toHtml $ occNameString name

ppRdrName :: RdrName -> Html
ppRdrName = ppOccName . rdrNameOcc

ppLDocName (L _ d) = ppDocName d

ppDocName :: DocName -> Html
ppDocName (Link name) = linkId (nameModule name) (Just name) << ppName name
ppDocName (NoLink name) = toHtml (getOccString name)

linkTarget :: Name -> Html
linkTarget name = namedAnchor (anchorNameStr name) << toHtml "" 

ppName :: Name -> Html
ppName name = toHtml (getOccString name)

ppBinder :: Bool -> Name -> Html
-- The Bool indicates whether we are generating the summary, in which case
-- the binder will be a link to the full definition.
ppBinder True nm = linkedAnchor (anchorNameStr nm) << ppBinder' nm
ppBinder False nm = linkTarget nm +++ bold << ppBinder' nm

ppBinder' :: Name -> Html
ppBinder' name 
  | isVarSym name = parens $ toHtml (getOccString name)
  | otherwise = toHtml (getOccString name)             

linkId :: Module -> Maybe Name -> Html -> Html
linkId mod mbName = anchor ! [href hr]
  where 
    hr = case mbName of
      Nothing   -> moduleHtmlFile mod
      Just name -> nameHtmlRef mod name

ppModule :: Module -> String -> Html
ppModule mod ref = anchor ! [href ((moduleHtmlFile mod) ++ ref)] 
                   << toHtml (moduleString mod)

-- -----------------------------------------------------------------------------
-- * Doc Markup

parHtmlMarkup :: (a -> Html) -> DocMarkup a Html
parHtmlMarkup ppId = Markup {
  markupParagraph     = paragraph,
  markupEmpty	      = toHtml "",
  markupString        = toHtml,
  markupAppend        = (+++),
  markupIdentifier    = tt . ppId . head,
  markupModule        = \m -> ppModule (mkModuleNoPkg m) "",
  markupEmphasis      = emphasize . toHtml,
  markupMonospaced    = tt . toHtml,
  markupUnorderedList = ulist . concatHtml . map (li <<),
  markupOrderedList   = olist . concatHtml . map (li <<),
  markupDefList       = dlist . concatHtml . map markupDef,
  markupCodeBlock     = pre,
  markupURL	      = \url -> anchor ! [href url] << toHtml url,
  markupAName	      = \aname -> namedAnchor aname << toHtml ""
  }

markupDef (a,b) = dterm << a +++ ddef << b

htmlMarkup = parHtmlMarkup ppDocName
htmlOrigMarkup = parHtmlMarkup ppName
htmlRdrMarkup = parHtmlMarkup ppRdrName

-- If the doc is a single paragraph, don't surround it with <P> (this causes
-- ugly extra whitespace with some browsers).
docToHtml :: GHC.HsDoc DocName -> Html
docToHtml doc = markup htmlMarkup (unParagraph (markup htmlCleanup doc))

origDocToHtml :: GHC.HsDoc GHC.Name -> Html
origDocToHtml doc = markup htmlOrigMarkup (unParagraph (markup htmlCleanup doc))

rdrDocToHtml doc = markup htmlRdrMarkup (unParagraph (markup htmlCleanup doc))

-- If there is a single paragraph, then surrounding it with <P>..</P>
-- can add too much whitespace in some browsers (eg. IE).  However if
-- we have multiple paragraphs, then we want the extra whitespace to
-- separate them.  So we catch the single paragraph case and transform it
-- here.
unParagraph (GHC.DocParagraph d) = d
--NO: This eliminates line breaks in the code block:  (SDM, 6/5/2003)
--unParagraph (DocCodeBlock d) = (DocMonospaced d)
unParagraph doc              = doc

htmlCleanup :: DocMarkup a (GHC.HsDoc a)
htmlCleanup = idMarkup { 
  markupUnorderedList = GHC.DocUnorderedList . map unParagraph,
  markupOrderedList   = GHC.DocOrderedList   . map unParagraph
  } 

-- -----------------------------------------------------------------------------
-- * Misc

hsep :: [Html] -> Html
hsep [] = noHtml
hsep htmls = foldr1 (\a b -> a+++" "+++b) htmls

infixr 8 <+>
(<+>) :: Html -> Html -> Html
a <+> b = Html (getHtmlElements (toHtml a) ++ HtmlString " ": getHtmlElements (toHtml b))

keyword :: String -> Html
keyword s = thespan ! [theclass "keyword"] << toHtml s

equals, comma :: Html
equals = char '='
comma  = char ','

char :: Char -> Html
char c = toHtml [c]

empty :: Html
empty  = noHtml

parens, brackets, braces :: Html -> Html
parens h        = char '(' +++ h +++ char ')'
brackets h      = char '[' +++ h +++ char ']'
pabrackets h    = toHtml "[:" +++ h +++ toHtml ":]"
braces h        = char '{' +++ h +++ char '}'

punctuate :: Html -> [Html] -> [Html]
punctuate _ []     = []
punctuate h (d0:ds) = go d0 ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d +++ h) : go e es

abovesSep :: HtmlTable -> [HtmlTable] -> HtmlTable
abovesSep _ []      = Html.emptyTable
abovesSep h (d0:ds) = go d0 ds
                   where
                     go d [] = d
                     go d (e:es) = d </> h </> go e es

parenList :: [Html] -> Html
parenList = parens . hsep . punctuate comma

ubxParenList :: [Html] -> Html
ubxParenList = ubxparens . hsep . punctuate comma

ubxparens :: Html -> Html
ubxparens h = toHtml "(#" +++ h +++ toHtml "#)"

{-
text :: Html
text   = strAttr "TEXT"
-}

-- a box for displaying code
declBox :: Html -> HtmlTable
declBox html = tda [theclass "decl"] << html

-- a box for top level documented names
-- it adds a source and wiki link at the right hand side of the box
topDeclBox :: LinksInfo -> SrcSpan -> Name -> Html -> HtmlTable
topDeclBox ((_,_,Nothing), (_,_,Nothing), _) _ _ html = declBox html
topDeclBox ((_,_,maybe_source_url), (_,_,maybe_wiki_url), hmod)
           loc name html =
  tda [theclass "topdecl"] <<
  (        table ! [theclass "declbar"] <<
	    ((tda [theclass "declname"] << html)
             <-> srcLink
             <-> wikiLink)
  )
  where srcLink =
          case maybe_source_url of
            Nothing  -> Html.emptyTable
            Just url -> tda [theclass "declbut"] <<
                          let url' = spliceURL (Just fname) (Just mod)
                                               (Just name) url
                           in anchor ! [href url'] << toHtml "Source"
        wikiLink =
          case maybe_wiki_url of
            Nothing  -> Html.emptyTable
            Just url -> tda [theclass "declbut"] <<
                          let url' = spliceURL (Just fname) (Just mod)
                                               (Just name) url
                           in anchor ! [href url'] << toHtml "Comments"
  
        mod = hmod_mod hmod
        fname = unpackFS (srcSpanFile loc)

-- a box for displaying an 'argument' (some code which has text to the
-- right of it).  Wrapping is not allowed in these boxes, whereas it is
-- in a declBox.
argBox :: Html -> HtmlTable
argBox html = tda [theclass "arg"] << html

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

maybeRDocBox :: Maybe (GHC.LHsDoc DocName) -> HtmlTable
maybeRDocBox Nothing = rdocBox (noHtml)
maybeRDocBox (Just ldoc) = rdocBox (docToHtml (unLoc ldoc))

-- a box for the buttons at the top of the page
topButBox :: Html -> HtmlTable
topButBox html = tda [theclass "topbut"] << html

-- a vanilla table has width 100%, no border, no padding, no spacing
-- a narrow table is the same but without width 100%.
vanillaTable, narrowTable :: Html -> Html
vanillaTable = table ! [theclass "vanilla", cellspacing 0, cellpadding 0]
vanillaTable2 = table ! [theclass "vanilla2", cellspacing 0, cellpadding 0]
narrowTable  = table ! [theclass "narrow",  cellspacing 0, cellpadding 0]

spacedTable1, spacedTable5 :: Html -> Html
spacedTable1 = table ! [theclass "vanilla",  cellspacing 1, cellpadding 0]
spacedTable5 = table ! [theclass "vanilla",  cellspacing 5, cellpadding 0]

constrHdr, methHdr :: HtmlTable
constrHdr  = tda [ theclass "section4" ] << toHtml "Constructors"
methHdr    = tda [ theclass "section4" ] << toHtml "Methods"

instHdr :: String -> HtmlTable
instHdr id = 
  tda [ theclass "section4" ] << (collapsebutton id +++ toHtml " Instances")

dcolon, arrow, darrow :: Html
dcolon = toHtml "::"
arrow  = toHtml "->"
darrow = toHtml "=>"
dot    = toHtml "."

s8, s15 :: HtmlTable
s8  = tda [ theclass "s8" ]  << noHtml
s15 = tda [ theclass "s15" ] << noHtml

namedAnchor :: String -> Html -> Html
namedAnchor n = anchor ! [name (escapeStr n)]

--
-- A section of HTML which is collapsible via a +/- button.
--

-- TODO: Currently the initial state is non-collapsed. Change the 'minusFile'
-- below to a 'plusFile' and the 'display:block;' to a 'display:none;' when we
-- use cookies from JavaScript to have a more persistent state.

collapsebutton :: String -> Html
collapsebutton id = 
  image ! [ src minusFile, theclass "coll", onclick ("toggle(this,'" ++ id ++ "')"), alt "show/hide" ]

collapsed :: (HTML a) => (Html -> Html) -> String -> a -> Html
collapsed fn id html =
  fn ! [identifier id, thestyle "display:block;"] << html

-- A quote is a valid part of a Haskell identifier, but it would interfere with
-- the ECMA script string delimiter used in collapsebutton above.
collapseId :: Name -> String
collapseId nm = "i:" ++ escapeStr (getOccString nm)

linkedAnchor :: String -> Html -> Html
linkedAnchor frag = anchor ! [href hr]
   where hr | null frag = ""
            | otherwise = '#': escapeStr frag

documentCharacterEncoding :: Html
documentCharacterEncoding =
   meta ! [httpequiv "Content-Type", content "text/html; charset=UTF-8"]

styleSheet :: Html
styleSheet =
   thelink ! [href cssFile, rel "stylesheet", thetype "text/css"]
