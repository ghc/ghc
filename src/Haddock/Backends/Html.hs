
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

import Haddock.DocName
import Haddock.Backends.DevHelp
import Haddock.Backends.HH
import Haddock.Backends.HH2
import Haddock.ModuleTree
import Haddock.Types hiding ( Doc )
import Haddock.Version
import Haddock.Utils
import Haddock.Utils.Html hiding ( name, title, p )
import qualified Haddock.Utils.Html as Html
import Haddock.GHC.Utils
import qualified Haddock.Utils.Html as Html

import Control.Exception     ( bracket )
import Control.Monad         ( when, join )
import Data.Char             ( toUpper )
import Data.List             ( sortBy, groupBy )
import Data.Maybe
import Foreign.Marshal.Alloc ( allocaBytes )
import System.IO             ( IOMode(..), hClose, hGetBuf, hPutBuf, openFile )
import Data.Map              ( Map )
import qualified Data.Map as Map hiding ( Map )
import Data.Function
import Data.Ord              ( comparing )

#if __GLASGOW_HASKELL__ >= 609
import GHC hiding ( NoLink, moduleInfo )
#else
import GHC hiding ( NoLink )
#endif
import Name
import Module
import RdrName hiding ( Qual, is_explicit )
import SrcLoc   
import FastString            ( unpackFS )
import BasicTypes            ( IPName(..), Boxity(..) )
import Outputable            ( ppr, showSDoc, Outputable )

-- the base, module and entity URLs for the source code and wiki links.
type SourceURLs = (Maybe String, Maybe String, Maybe String)
type WikiURLs = (Maybe String, Maybe String, Maybe String)


-- convenient short-hands
type Doc = HsDoc DocName


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

ppHtml doctitle maybe_package ifaces odir prologue maybe_html_help_format
	maybe_source_url maybe_wiki_url
	maybe_contents_url maybe_index_url =  do
  let
	visible_ifaces = filter visible ifaces
	visible i = OptHide `notElem` ifaceOptions i
  when (not (isJust maybe_contents_url)) $ 
    ppHtmlContents odir doctitle maybe_package
        maybe_html_help_format maybe_index_url maybe_source_url maybe_wiki_url
        (map toInstalledIface visible_ifaces)
	False -- we don't want to display the packages in a single-package contents
	prologue

  when (not (isJust maybe_index_url)) $ 
    ppHtmlIndex odir doctitle maybe_package maybe_html_help_format
      maybe_contents_url maybe_source_url maybe_wiki_url 
      (map toInstalledIface visible_ifaces)
    
  when (not (isJust maybe_contents_url && isJust maybe_index_url)) $ 
	ppHtmlHelpFiles doctitle maybe_package ifaces odir maybe_html_help_format []

  mapM_ (ppHtmlModule odir doctitle
	   maybe_source_url maybe_wiki_url
	   maybe_contents_url maybe_index_url) visible_ifaces

ppHtmlHelpFiles	
    :: String                   -- doctitle
    -> Maybe String				-- package
	-> [Interface]
	-> FilePath                 -- destination directory
	-> Maybe String             -- the Html Help format (--html-help)
	-> [FilePath]               -- external packages paths
	-> IO ()
ppHtmlHelpFiles doctitle maybe_package ifaces odir maybe_html_help_format pkg_paths =  do
  let
	visible_ifaces = filter visible ifaces
	visible i = OptHide `notElem` ifaceOptions i

  -- Generate index and contents page for Html Help if requested
  case maybe_html_help_format of
    Nothing        -> return ()
    Just "mshelp"  -> ppHHProject odir doctitle maybe_package visible_ifaces pkg_paths
    Just "mshelp2" -> do
		ppHH2Files      odir maybe_package visible_ifaces pkg_paths
		ppHH2Collection odir doctitle maybe_package
    Just "devhelp" -> ppDevHelpFile odir doctitle maybe_package visible_ifaces
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
  mapM_ copyLibFile [ iconFile, plusFile, minusFile, jsFile, framesFile ]

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

srcButton (_, Just src_module_url, _) (Just iface) =
  let url = spliceURL (Just $ ifaceOrigFilename iface)
                      (Just $ ifaceMod iface) Nothing Nothing src_module_url
   in topButBox (anchor ! [href url] << toHtml "Source code")

srcButton _ _ =
  Html.emptyTable
 
spliceURL :: Maybe FilePath -> Maybe Module -> Maybe GHC.Name -> 
             Maybe SrcSpan -> String -> String
spliceURL maybe_file maybe_mod maybe_name maybe_loc url = run url
 where
  file = fromMaybe "" maybe_file
  mdl = case maybe_mod of
          Nothing           -> ""
          Just m -> moduleString m
  
  (name, kind) =
    case maybe_name of
      Nothing             -> ("","")
      Just n | isValOcc (nameOccName n) -> (escapeStr (getOccString n), "v")
             | otherwise -> (escapeStr (getOccString n), "t")

  line = case maybe_loc of
    Nothing -> ""
    Just span_ -> show $ srcSpanStartLine span_

  run "" = ""
  run ('%':'M':rest) = mdl  ++ run rest
  run ('%':'F':rest) = file ++ run rest
  run ('%':'N':rest) = name ++ run rest
  run ('%':'K':rest) = kind ++ run rest
  run ('%':'L':rest) = line ++ run rest
  run ('%':'%':rest) = "%" ++ run rest

  run ('%':'{':'M':'O':'D':'U':'L':'E':'}':rest) = mdl  ++ run rest
  run ('%':'{':'F':'I':'L':'E':'}':rest)         = file ++ run rest
  run ('%':'{':'N':'A':'M':'E':'}':rest)         = name ++ run rest
  run ('%':'{':'K':'I':'N':'D':'}':rest)         = kind ++ run rest

  run ('%':'{':'M':'O':'D':'U':'L':'E':'/':'.':'/':c:'}':rest) =
    map (\x -> if x == '.' then c else x) mdl ++ run rest

  run ('%':'{':'F':'I':'L':'E':'/':'/':'/':c:'}':rest) =
    map (\x -> if x == '/' then c else x) file ++ run rest

  run ('%':'{':'L':'I':'N':'E':'}':rest)         = line ++ run rest

  run (c:rest) = c : run rest
  
wikiButton :: WikiURLs -> Maybe Module -> HtmlTable
wikiButton (Just wiki_base_url, _, _) Nothing =
  topButBox (anchor ! [href wiki_base_url] << toHtml "User Comments")

wikiButton (_, Just wiki_module_url, _) (Just mdl) =
  let url = spliceURL Nothing (Just mdl) Nothing Nothing wiki_module_url
   in topButBox (anchor ! [href url] << toHtml "User Comments")

wikiButton _ _ =
  Html.emptyTable

contentsButton :: Maybe String -> HtmlTable
contentsButton maybe_contents_url 
  = topButBox (anchor ! [href url] << toHtml "Contents")
  where url = maybe contentsHtmlFile id maybe_contents_url

indexButton :: Maybe String -> HtmlTable
indexButton maybe_index_url 
  = topButBox (anchor ! [href url] << toHtml "Index")
  where url = maybe indexHtmlFile id maybe_index_url

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
pageHeader mdl iface doctitle
           maybe_source_url maybe_wiki_url
           maybe_contents_url maybe_index_url =
  (tda [theclass "topbar"] << 
    vanillaTable << (
       (td << 
  	image ! [src "haskell_icon.gif", width "16", height 16, alt " "]
       ) <->
       (tda [theclass "title"] << toHtml doctitle) <->
	srcButton maybe_source_url (Just iface) <->
	wikiButton maybe_wiki_url (Just $ ifaceMod iface) <->
	contentsButton maybe_contents_url <->
	indexButton maybe_index_url
    )
   ) </>
   tda [theclass "modulebar"] <<
	(vanillaTable << (
	  (td << font ! [size "6"] << toHtml mdl) <->
	  moduleInfo iface
	)
    )

moduleInfo :: Interface -> HtmlTable
moduleInfo iface = 
   let
      info = ifaceInfo iface

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
   -> [InstalledInterface] -> Bool -> Maybe (GHC.HsDoc GHC.RdrName)
   -> IO ()
ppHtmlContents odir doctitle
  maybe_package maybe_html_help_format maybe_index_url
  maybe_source_url maybe_wiki_url ifaces showPkgs prologue = do
  let tree = mkModuleTree showPkgs
         [(instMod iface, toInstalledDescription iface) | iface <- ifaces]
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

  -- XXX: think of a better place for this?
  ppHtmlContentsFrame odir doctitle ifaces
  
  -- Generate contents page for Html Help if requested
  case maybe_html_help_format of
    Nothing        -> return ()
    Just "mshelp"  -> ppHHContents  odir doctitle maybe_package tree
    Just "mshelp2" -> ppHH2Contents odir doctitle maybe_package tree
    Just "devhelp" -> return ()
    Just format    -> fail ("The "++format++" format is not implemented")

ppPrologue :: String -> Maybe (GHC.HsDoc GHC.RdrName) -> HtmlTable
ppPrologue _ Nothing = Html.emptyTable
ppPrologue title (Just doc) = 
  (tda [theclass "section1"] << toHtml title) </>
  docBox (rdrDocToHtml doc)

ppModuleTree :: String -> [ModuleTree] -> HtmlTable
ppModuleTree _ ts = 
  tda [theclass "section1"] << toHtml "Modules" </>
  td << vanillaTable2 << htmlTable
  where
    genTable tbl id_ []     = (tbl, id_)
    genTable tbl id_ (x:xs) = genTable (tbl </> u) id' xs      
      where
        (u,id') = mkNode [] x 0 id_

    (htmlTable,_) = genTable emptyTable 0 ts

mkNode :: [String] -> ModuleTree -> Int -> Int -> (HtmlTable,Int)
mkNode ss (Node s leaf pkg short ts) depth id_ = htmlNode
  where
    htmlNode = case ts of
      [] -> (td_pad_w 1.25 depth << htmlModule  <-> shortDescr <-> htmlPkg,id_)
      _  -> (td_w depth << (collapsebutton id_s +++ htmlModule) <-> shortDescr <-> htmlPkg </> 
                (td_subtree << sub_tree), id')

    mod_width = 50::Int {-em-}

    td_pad_w pad depth_ = 
	tda [thestyle ("padding-left: " ++ show pad ++ "em;" ++
		       "width: " ++ show (mod_width - depth_*2) ++ "em")]

    td_w depth_ = 
	tda [thestyle ("width: " ++ show (mod_width - depth_*2) ++ "em")]

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
    
    id_s = "n:" ++ show id_
    
    (sub_tree,id') = genSubTree emptyTable (id_+1) ts
    
    genSubTree :: HtmlTable -> Int -> [ModuleTree] -> (Html,Int)
    genSubTree htmlTable id__ [] = (sub_tree_, id__)
      where
        sub_tree_ = collapsed vanillaTable2 id_s htmlTable
    genSubTree htmlTable id__ (x:xs) = genSubTree (htmlTable </> u) id__' xs      
      where
        (u,id__') = mkNode (s:ss) x (depth+1) id__

-- The URL for source and wiki links, and the current module
type LinksInfo = (SourceURLs, WikiURLs)

-- | Turn a module tree into a flat list of full module names.  E.g.,
-- @
--  A
--  +-B
--  +-C
-- @
-- becomes
-- @["A", "A.B", "A.B.C"]@
flatModuleTree :: [InstalledInterface] -> [Html]
flatModuleTree ifaces =
    map (uncurry ppModule' . head)
            . groupBy ((==) `on` fst)
            . sortBy (comparing fst)
            $ mods
  where
    mods = [ (moduleString mdl, mdl) | mdl <- map instMod ifaces ]
    ppModule' txt mdl =
      anchor ! [href ((moduleHtmlFile mdl)), target mainFrameName]
        << toHtml txt

ppHtmlContentsFrame :: FilePath -> String -> [InstalledInterface] -> IO ()
ppHtmlContentsFrame odir doctitle ifaces = do
  let mods = flatModuleTree ifaces
      html =
        header
            (documentCharacterEncoding +++
	     thetitle (toHtml doctitle) +++
	     styleSheet +++
	     (script ! [src jsFile, thetype "text/javascript"] $ noHtml)) +++
        body << vanillaTable << Html.p << (
            foldr (+++) noHtml (map (+++br) mods))
  writeFile (pathJoin [odir, frameIndexHtmlFile]) (renderHtml html)

-- ---------------------------------------------------------------------------
-- Generate the index

ppHtmlIndex :: FilePath
            -> String 
            -> Maybe String
            -> Maybe String
            -> Maybe String
            -> SourceURLs
            -> WikiURLs
            -> [InstalledInterface] 
            -> IO ()
ppHtmlIndex odir doctitle maybe_package maybe_html_help_format
  maybe_contents_url maybe_source_url maybe_wiki_url ifaces = do
  let html = 
        header (documentCharacterEncoding +++
                thetitle (toHtml (doctitle ++ " (Index)")) +++
        styleSheet +++
        (script ! [src jsFile, thetype "text/javascript"] $ noHtml)) +++
        body << vanillaTable << (
            simpleHeader doctitle maybe_contents_url Nothing
                         maybe_source_url maybe_wiki_url </>
        search_box </> index_html
           )

  writeFile (pathJoin [odir, indexHtmlFile]) (renderHtml html)
  
    -- Generate index and contents page for Html Help if requested
  case maybe_html_help_format of
    Nothing        -> return ()
    Just "mshelp"  -> ppHHIndex  odir maybe_package ifaces
    Just "mshelp2" -> ppHH2Index odir maybe_package ifaces
    Just "devhelp" -> return ()
    Just format    -> fail ("The "++format++" format is not implemented")
 where
  -- colspan 2, marginheight 5
  search_box :: HtmlTable
  search_box = tda [colspan 2, thestyle "padding-top:5px;"] << search
    where
      search :: Html
      search = form ! [strAttr "onsubmit" "full_search(); return false;", action ""] << (
                    "Search: "
                    +++ input ! [identifier "searchbox", strAttr "onkeyup" "quick_search()"]
                    +++ " " +++ input ! [value "Search", thetype "submit"]
                    +++ " " +++ thespan ! [identifier "searchmsg"] << " ")

  index_html = td << setTrClass (table ! [identifier "indexlist", cellpadding 0, cellspacing 5] <<
          aboves (map indexElt index))

  setTrClass :: Html -> Html
  setTrClass (Html xs) = Html $ map f xs
      where
          f (HtmlTag name attrs inner)
               | map toUpper name == "TR" = HtmlTag name (theclass "indexrow":attrs) inner
               | otherwise = HtmlTag name attrs (setTrClass inner)
          f x = x

  index :: [(String, Map GHC.Name [(Module,Bool)])]
  index = sortBy cmp (Map.toAscList full_index)
    where cmp (n1,_) (n2,_) = map toUpper n1 `compare` map toUpper n2

  -- for each name (a plain string), we have a number of original HsNames that
  -- it can refer to, and for each of those we have a list of modules
  -- that export that entity.  Each of the modules exports the entity
  -- in a visible or invisible way (hence the Bool).
  full_index :: Map String (Map GHC.Name [(Module,Bool)])
  full_index = Map.fromListWith (flip (Map.unionWith (++)))
               (concat (map getIfaceIndex ifaces))

  getIfaceIndex iface = 
    [ (getOccString name
       , Map.fromList [(name, [(mdl, name `elem` instVisibleExports iface)])])
       | name <- instExports iface ]
    where mdl = instMod iface

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
	     linkId mdl (Just nm) << toHtml (moduleString mdl)
	  else
	     toHtml (moduleString mdl)
	| (mdl, visible) <- entries ])

-- ---------------------------------------------------------------------------
-- Generate the HTML page for a module

ppHtmlModule
	:: FilePath -> String
	-> SourceURLs -> WikiURLs
	-> Maybe String -> Maybe String
	-> Interface -> IO ()
ppHtmlModule odir doctitle
  maybe_source_url maybe_wiki_url
  maybe_contents_url maybe_index_url iface = do
  let 
      mdl = ifaceMod iface
      mdl_str = moduleString mdl
      html = 
	header (documentCharacterEncoding +++
		thetitle (toHtml mdl_str) +++
		styleSheet +++
		(script ! [src jsFile, thetype "text/javascript"] $ noHtml) +++
                (script ! [thetype "text/javascript"]
                     -- XXX: quoting errors possible?
                     << Html [HtmlString ("window.onload = function () {setSynopsis(\"mini_" 
                                ++ moduleHtmlFile mdl ++ "\")};")])
               ) +++
        body << vanillaTable << (
	    pageHeader mdl_str iface doctitle
		maybe_source_url maybe_wiki_url
		maybe_contents_url maybe_index_url </> s15 </>
	    ifaceToHtml maybe_source_url maybe_wiki_url iface </> s15 </>
	    footer
         )
  writeFile (pathJoin [odir, moduleHtmlFile mdl]) (renderHtml html)
  ppHtmlModuleMiniSynopsis odir doctitle iface

ppHtmlModuleMiniSynopsis :: FilePath -> String -> Interface -> IO ()
ppHtmlModuleMiniSynopsis odir _doctitle iface = do
  let mdl = ifaceMod iface
      html =
        header
          (documentCharacterEncoding +++
	   thetitle (toHtml $ moduleString mdl) +++
	   styleSheet +++
	   (script ! [src jsFile, thetype "text/javascript"] $ noHtml)) +++
        body << thediv ! [ theclass "outer" ] << (
           (thediv ! [theclass "mini-topbar"]
             << toHtml (moduleString mdl)) +++
           miniSynopsis mdl iface)
  writeFile (pathJoin [odir, "mini_" ++ moduleHtmlFile mdl]) (renderHtml html)

ifaceToHtml :: SourceURLs -> WikiURLs -> Interface -> HtmlTable
ifaceToHtml maybe_source_url maybe_wiki_url iface
  = abovesSep s15 (contents ++ description: synopsis: maybe_doc_hdr: bdy)
  where
    docMap = ifaceRnDocMap iface
 
    exports = numberSectionHeadings (ifaceRnExportItems iface)

    has_doc (ExportDecl _ doc _ _) = isJust doc
    has_doc (ExportNoDecl _ _) = False
    has_doc (ExportModule _) = False
    has_doc _ = True

    no_doc_at_all = not (any has_doc exports)

    contents = case ppModuleContents exports of
                   Nothing -> []
                   Just x -> [td << vanillaTable << x]

    description
          = case ifaceRnDoc iface of
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
    linksInfo = (maybe_source_url, maybe_wiki_url)

miniSynopsis :: Module -> Interface -> Html
miniSynopsis mdl iface =
    thediv ! [ theclass "mini-synopsis" ]
      << hsep (map (processForMiniSynopsis mdl) $ exports)

  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)

processForMiniSynopsis :: Module -> ExportItem DocName -> Html
processForMiniSynopsis mdl (ExportDecl (L _loc decl0) _doc _ _insts) =
  thediv ! [theclass "decl" ] <<
  case decl0 of
    TyClD d@(TyFamily{}) -> ppTyFamHeader True False d
    TyClD d@(TyData{tcdTyPats = ps})
      | Nothing <- ps    -> keyword "data" <++> ppTyClBinderWithVarsMini mdl d
      | Just _ <- ps     -> keyword "data" <++> keyword "instance"
                                           <++> ppTyClBinderWithVarsMini mdl d
    TyClD d@(TySynonym{tcdTyPats = ps})
      | Nothing <- ps    -> keyword "type" <++> ppTyClBinderWithVarsMini mdl d
      | Just _ <- ps     -> keyword "type" <++> keyword "instance"
                                           <++> ppTyClBinderWithVarsMini mdl d
    TyClD d@(ClassDecl {}) ->
                            keyword "class" <++> ppTyClBinderWithVarsMini mdl d
    SigD (TypeSig (L _ n) (L _ _)) ->
        let nm = docNameOcc n
        in ppNameMini mdl nm
    _ -> noHtml
processForMiniSynopsis _ (ExportGroup lvl _id txt) =
  let heading
        | lvl == 1  = h1
        | lvl == 2  = h2
        | lvl >= 3  = h3
        | otherwise = error "bad group level"
  in heading << docToHtml txt
processForMiniSynopsis _ _ = noHtml

ppNameMini :: Module -> OccName -> Html
ppNameMini mdl nm =
    anchor ! [ href ( moduleHtmlFile mdl ++ "#"
                      ++ (escapeStr (anchorNameStr nm)))
             , target mainFrameName ]
      << ppBinder' nm

ppTyClBinderWithVarsMini :: Module -> TyClDecl DocName -> Html
ppTyClBinderWithVarsMini mdl decl =
  let n = unLoc $ tcdLName decl
      ns = tyvarNames $ tcdTyVars decl
  in ppTypeApp n ns (ppNameMini mdl . docNameOcc) ppTyName

ppModuleContents :: [ExportItem DocName] -> Maybe HtmlTable
ppModuleContents exports
  | length sections == 0 = Nothing
  | otherwise            = Just (tda [theclass "section4"] << bold << toHtml "Contents"
  		                 </> td << dlist << concatHtml sections)
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
processExport summary links docMap (ExportDecl decl doc subdocs insts)
  = ppDecl summary links decl doc insts docMap subdocs
processExport _ _ _ (ExportNoDecl y [])
  = declBox (ppDocName y)
processExport _ _ _ (ExportNoDecl y subs)
  = declBox (ppDocName y <+> parenList (map ppDocName subs))
processExport _ _ _ (ExportDoc doc)
  = docBox (docToHtml doc)
processExport _ _ _ (ExportModule mdl)
  = declBox (toHtml "module" <+> ppModule mdl "")

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

declWithDoc :: Bool -> LinksInfo -> SrcSpan -> DocName -> Maybe (HsDoc DocName) -> Html -> HtmlTable
declWithDoc True  _     _   _  _          html_decl = declBox html_decl
declWithDoc False links loc nm Nothing    html_decl = topDeclBox links loc nm html_decl
declWithDoc False links loc nm (Just doc) html_decl = 
		topDeclBox links loc nm html_decl </> docBox (docToHtml doc)


-- TODO: use DeclInfo DocName or something
ppDecl :: Bool -> LinksInfo -> LHsDecl DocName -> 
          Maybe (HsDoc DocName) -> [InstHead DocName] -> DocMap -> [(DocName, Maybe (HsDoc DocName))] -> HtmlTable
ppDecl summ links (L loc decl) mbDoc instances docMap subdocs = case decl of
  TyClD d@(TyFamily {})          -> ppTyFam summ False links loc mbDoc d
  TyClD d@(TyData {})
    | Nothing <- tcdTyPats d     -> ppDataDecl summ links instances loc mbDoc d
    | Just _  <- tcdTyPats d     -> ppDataInst summ links loc mbDoc d 
  TyClD d@(TySynonym {})
    | Nothing <- tcdTyPats d     -> ppTySyn summ links loc mbDoc d
    | Just _  <- tcdTyPats d     -> ppTyInst summ False links loc mbDoc d
  TyClD d@(ClassDecl {})         -> ppClassDecl summ links instances loc mbDoc docMap subdocs d
  SigD (TypeSig (L _ n) (L _ t)) -> ppFunSig summ links loc mbDoc n t
  ForD d                         -> ppFor summ links loc mbDoc d
  InstD _                        -> Html.emptyTable
  _                              -> error "declaration not supported by ppDecl"

ppFunSig :: Bool -> LinksInfo -> SrcSpan -> Maybe (HsDoc DocName) ->
            DocName -> HsType DocName -> HtmlTable
ppFunSig summary links loc mbDoc docname typ =
  ppTypeOrFunSig summary links loc docname typ mbDoc 
    (ppTypeSig summary occname typ, ppBinder False occname, dcolon)
  where
    occname = nameOccName . docNameOrig $ docname

ppTypeOrFunSig :: Bool -> LinksInfo -> SrcSpan -> DocName -> HsType DocName ->
                  Maybe (HsDoc DocName) -> (Html, Html, Html) -> HtmlTable
ppTypeOrFunSig summary links loc docname typ doc (pref1, pref2, sep)
  | summary || noArgDocs typ = declWithDoc summary links loc docname doc pref1
  | otherwise = topDeclBox links loc docname pref2 </>
    (tda [theclass "body"] << vanillaTable <<  (
      do_args sep typ </>
        (case doc of
          Just d -> ndocBox (docToHtml d)
          Nothing -> Html.emptyTable)
	))
  where 
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


ppTyVars :: [Located (HsTyVarBndr DocName)] -> [Html]
ppTyVars tvs = map ppTyName (tyvarNames tvs)


tyvarNames :: [Located (HsTyVarBndr DocName)] -> [Name]
tyvarNames = map (docNameOrig . hsTyVarName . unLoc)
  

ppFor :: Bool -> LinksInfo -> SrcSpan -> Maybe Doc -> ForeignDecl DocName -> HtmlTable
ppFor summary links loc mbDoc (ForeignImport (L _ name) (L _ typ) _)
  = ppFunSig summary links loc mbDoc name typ
ppFor _ _ _ _ _ = error "ppFor"


-- we skip type patterns for now
ppTySyn :: Bool -> LinksInfo -> SrcSpan -> Maybe Doc -> TyClDecl DocName -> HtmlTable
ppTySyn summary links loc mbDoc (TySynonym (L _ name) ltyvars _ ltype) 
  = ppTypeOrFunSig summary links loc name (unLoc ltype) mbDoc 
                   (full, hdr, spaceHtml +++ equals)
  where
    hdr  = hsep ([keyword "type", ppBinder summary occ] ++ ppTyVars ltyvars)
    full = hdr <+> equals <+> ppLType ltype
    occ  = docNameOcc name
ppTySyn _ _ _ _ _ = error "declaration not supported by ppTySyn"


ppTypeSig :: Bool -> OccName -> HsType DocName -> Html
ppTypeSig summary nm ty = ppBinder summary nm <+> dcolon <+> ppType ty


ppTyName :: Name -> Html
ppTyName name
  | isNameSym name = parens (ppName name)
  | otherwise = ppName name


--------------------------------------------------------------------------------
-- Type families
--------------------------------------------------------------------------------


ppTyFamHeader :: Bool -> Bool -> TyClDecl DocName -> Html
ppTyFamHeader summary associated decl =

  (case tcdFlavour decl of
     TypeFamily
       | associated -> keyword "type"
       | otherwise  -> keyword "type family"
     DataFamily
       | associated -> keyword "data"
       | otherwise  -> keyword "data family"
  ) <+>

  ppTyClBinderWithVars summary decl <+>

  case tcdKind decl of
    Just kind -> dcolon <+> ppKind kind 
    Nothing -> empty


ppTyFam :: Bool -> Bool -> LinksInfo -> SrcSpan -> Maybe (HsDoc DocName) ->
              TyClDecl DocName -> HtmlTable
ppTyFam summary associated links loc mbDoc decl
  
  | summary = declWithDoc summary links loc docname mbDoc 
              (ppTyFamHeader True associated decl)
  
  | associated, isJust mbDoc         = header_ </> bodyBox << doc 
  | associated                       = header_ 
  | null instances, isJust mbDoc     = header_ </> bodyBox << doc
  | null instances                   = header_
  | isJust mbDoc                     = header_ </> bodyBox << (doc </> instancesBit)
  | otherwise                        = header_ </> bodyBox << instancesBit

  where
    docname = tcdName decl

    header_ = topDeclBox links loc docname (ppTyFamHeader summary associated decl)

    doc = ndocBox . docToHtml . fromJust $ mbDoc 

    instId = collapseId (docNameOrig docname)

    instancesBit = instHdr instId </>
  	  tda [theclass "body"] << 
            collapsed thediv instId (
              spacedTable1 << (
                aboves (map (declBox . ppInstHead) instances)
              )
            )

    -- TODO: get the instances
    instances = []


--------------------------------------------------------------------------------
-- Indexed data types
--------------------------------------------------------------------------------


ppDataInst :: a
ppDataInst = undefined


--------------------------------------------------------------------------------
-- Indexed newtypes
--------------------------------------------------------------------------------

-- TODO
-- ppNewTyInst = undefined


--------------------------------------------------------------------------------
-- Indexed types
--------------------------------------------------------------------------------

 
ppTyInst :: Bool -> Bool -> LinksInfo -> SrcSpan -> Maybe (HsDoc DocName) ->
            TyClDecl DocName -> HtmlTable
ppTyInst summary associated links loc mbDoc decl
  
  | summary = declWithDoc summary links loc docname mbDoc
              (ppTyInstHeader True associated decl)
  
  | isJust mbDoc = header_ </> bodyBox << doc 
  | otherwise    = header_

  where
    docname = tcdName decl

    header_ = topDeclBox links loc docname (ppTyInstHeader summary associated decl)

    doc = case mbDoc of
      Just d -> ndocBox (docToHtml d)
      Nothing -> Html.emptyTable


ppTyInstHeader :: Bool -> Bool -> TyClDecl DocName -> Html
ppTyInstHeader _ _ decl =

  keyword "type instance" <+>

  ppAppNameTypes (tcdName decl) typeArgs
  where
    typeArgs = map unLoc . fromJust . tcdTyPats $ decl


--------------------------------------------------------------------------------
-- Associated Types
--------------------------------------------------------------------------------
    

ppAssocType :: Bool -> LinksInfo -> Maybe (HsDoc DocName) -> LTyClDecl DocName -> HtmlTable
ppAssocType summ links doc (L loc decl) = 
  case decl of
    TyFamily  {} -> ppTyFam summ True links loc doc decl
    TySynonym {} -> ppTySyn summ links loc doc decl
    _            -> error "declaration type not supported by ppAssocType" 


--------------------------------------------------------------------------------
-- TyClDecl helpers
--------------------------------------------------------------------------------


-- | Print a type family / newtype / data / class binder and its variables 
ppTyClBinderWithVars :: Bool -> TyClDecl DocName -> Html
ppTyClBinderWithVars summ decl = 
  ppAppDocNameNames summ (unLoc $ tcdLName decl) (tyvarNames $ tcdTyVars decl)


--------------------------------------------------------------------------------
-- Type applications
--------------------------------------------------------------------------------


-- | Print an application of a DocName and a list of HsTypes
ppAppNameTypes :: DocName -> [HsType DocName] -> Html
ppAppNameTypes n ts = ppTypeApp n ts ppDocName ppParendType


-- | Print an application of a DocName and a list of Names 
ppAppDocNameNames :: Bool -> DocName -> [Name] -> Html
ppAppDocNameNames summ n ns = 
  ppTypeApp n ns (ppBinder summ . docNameOcc) ppTyName


-- | General printing of type applications
ppTypeApp :: DocName -> [a] -> (DocName -> Html) -> (a -> Html) -> Html
ppTypeApp n (t1:t2:rest) ppDN ppT
  | operator, not . null $ rest = parens opApp <+> hsep (map ppT rest)
  | operator                    = opApp
  where
    operator = isNameSym . docNameOrig $ n
    opApp = ppT t1 <+> ppDN n <+> ppT t2

ppTypeApp n ts ppDN ppT = ppDN n <+> hsep (map ppT ts)


-------------------------------------------------------------------------------
-- Contexts 
-------------------------------------------------------------------------------


ppLContext, ppLContextNoArrow :: Located (HsContext DocName) -> Html
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


pp_hs_context :: [HsPred DocName] -> Html
pp_hs_context []  = empty
pp_hs_context [p] = ppPred p
pp_hs_context cxt = parenList (map ppPred cxt) 


ppPred :: HsPred DocName -> Html
ppPred (HsClassP n ts) = ppAppNameTypes n (map unLoc ts)
ppPred (HsEqualP t1 t2) = ppLType t1 <+> toHtml "~" <+> ppLType t2
ppPred (HsIParam (IPName n) t) 
  = toHtml "?" +++ ppDocName n <+> dcolon <+> ppLType t


-------------------------------------------------------------------------------
-- Class declarations
-------------------------------------------------------------------------------


ppClassHdr :: Bool -> Located [LHsPred DocName] -> DocName
           -> [Located (HsTyVarBndr DocName)] -> [Located ([DocName], [DocName])]
           -> Html
ppClassHdr summ lctxt n tvs fds = 
  keyword "class" 
  <+> (if not . null . unLoc $ lctxt then ppLContext lctxt else empty)
  <+> ppAppDocNameNames summ n (tyvarNames $ tvs)
	<+> ppFds fds


ppFds :: [Located ([DocName], [DocName])] -> Html
ppFds fds =
  if null fds then noHtml else 
	char '|' <+> hsep (punctuate comma (map (fundep . unLoc) fds))
  where
	fundep (vars1,vars2) = hsep (map ppDocName vars1) <+> toHtml "->" <+>
			       hsep (map ppDocName vars2)


ppShortClassDecl :: Bool -> LinksInfo -> TyClDecl DocName -> SrcSpan -> [(DocName, Maybe (HsDoc DocName))] -> HtmlTable
ppShortClassDecl summary links (ClassDecl lctxt lname tvs fds sigs _ ats _) loc subdocs = 
  if null sigs && null ats
    then (if summary then declBox else topDeclBox links loc nm) hdr
    else (if summary then declBox else topDeclBox links loc nm) (hdr <+> keyword "where")
	    </> 
      (
				bodyBox <<
					aboves
					(
						[ ppAssocType summary links doc at | at <- ats
                                                , let doc = join $ lookup (tcdName $ unL at) subdocs ]  ++

						[ ppFunSig summary links loc doc n typ
						| L _ (TypeSig (L _ n) (L _ typ)) <- sigs
						, let doc = join $ lookup n subdocs ] 
					)
				)
  where
    hdr = ppClassHdr summary lctxt (unLoc lname) tvs fds
    nm  = unLoc lname
ppShortClassDecl _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"
    


ppClassDecl :: Bool -> LinksInfo -> [InstHead DocName] -> SrcSpan ->
               Maybe (HsDoc DocName) -> DocMap -> [(DocName, Maybe (HsDoc DocName))] -> TyClDecl DocName -> 
               HtmlTable
ppClassDecl summary links instances loc mbDoc _ subdocs
	decl@(ClassDecl lctxt lname ltyvars lfds lsigs _ ats _)
  | summary = ppShortClassDecl summary links decl loc subdocs
  | otherwise = classheader </> bodyBox << (classdoc </> body_ </> instancesBit)
  where 
    classheader
      | null lsigs = topDeclBox links loc nm hdr
      | otherwise  = topDeclBox links loc nm (hdr <+> keyword "where")

    nm   = unLoc $ tcdLName decl

    hdr = ppClassHdr summary lctxt (unLoc lname) ltyvars lfds
    
    classdoc = case mbDoc of
      Nothing -> Html.emptyTable
      Just d -> ndocBox (docToHtml d)

    body_
      | null lsigs, null ats = Html.emptyTable
      | null ats  = s8 </> methHdr </> bodyBox << methodTable
      | otherwise = s8 </> atHdr </> bodyBox << atTable </> 
                    s8 </> methHdr </> bodyBox << methodTable 
 
    methodTable =
      abovesSep s8 [ ppFunSig summary links loc doc n typ
                   | L _ (TypeSig (L _ n) (L _ typ)) <- lsigs
                   , let doc = join $ lookup n subdocs ]

    atTable = abovesSep s8 $ [ ppAssocType summary links doc at | at <- ats
                             , let doc = join $ lookup (tcdName $ unL at) subdocs ]

    instId = collapseId (docNameOrig nm)
    instancesBit
      | null instances = Html.emptyTable
      | otherwise 
        =  s8 </> instHdr instId </>
           tda [theclass "body"] << 
             collapsed thediv instId (
             spacedTable1 << (
               aboves (map (declBox . ppInstHead) instances)
             ))
ppClassDecl _ _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"


ppInstHead :: InstHead DocName -> Html
ppInstHead ([],   n, ts) = ppAppNameTypes n ts 
ppInstHead (ctxt, n, ts) = ppContextNoLocs ctxt <+> ppAppNameTypes n ts 


-- -----------------------------------------------------------------------------
-- Data & newtype declarations


-- TODO: print contexts
ppShortDataDecl :: Bool -> LinksInfo -> SrcSpan -> TyClDecl DocName -> Html
ppShortDataDecl summary links loc dataDecl 

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
      (if summary then declBox else topDeclBox links loc docname)
      ((ppDataHeader summary dataDecl) <+> 
      case resTy of ResTyGADT _ -> keyword "where"; _ -> empty)

    doConstr c con = declBox (toHtml [c] <+> ppShortConstr summary (unLoc con))
    doGADTConstr con = declBox (ppShortConstr summary (unLoc con))

    docname   = unLoc . tcdLName $ dataDecl
    cons      = tcdCons dataDecl
    resTy     = (con_res . unLoc . head) cons 

ppDataDecl :: Bool -> LinksInfo -> [InstHead DocName] -> 
              SrcSpan -> Maybe (HsDoc DocName) -> TyClDecl DocName -> HtmlTable
ppDataDecl summary links instances loc mbDoc dataDecl
  
  | summary = declWithDoc summary links loc docname mbDoc 
              (ppShortDataDecl summary links loc dataDecl)
  
  | otherwise
      = (if validTable then (</>) else const) header_ $
	      tda [theclass "body"] << vanillaTable << (
		      datadoc </> 
		      constrBit </>
		      instancesBit
        )


  where
    docname   = unLoc . tcdLName $ dataDecl
    cons      = tcdCons dataDecl
    resTy     = (con_res . unLoc . head) cons 
      
    header_ = topDeclBox links loc docname (ppDataHeader summary dataDecl
             <+> whereBit)

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

    instId = collapseId (docNameOrig docname)

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

    validTable = isJust mbDoc || not (null cons) || not (null instances)


isRecCon :: Located (ConDecl a) -> Bool
isRecCon lcon = case con_details (unLoc lcon) of 
  RecCon _ -> True
  _ -> False


ppShortConstr :: Bool -> ConDecl DocName -> Html
ppShortConstr summary con = case con_res con of 

  ResTyH98 -> case con_details con of 
    PrefixCon args -> header_ +++ hsep (ppBinder summary occ : map ppLParendType args)
    RecCon fields -> header_ +++ ppBinder summary occ <+>
      braces (vanillaTable << aboves (map (ppShortField summary) fields))
    InfixCon arg1 arg2 -> header_ +++ 
      hsep [ppLParendType arg1, ppBinder summary occ, ppLParendType arg2]    

  ResTyGADT resTy -> case con_details con of 
    PrefixCon args -> doGADTCon args resTy
    RecCon _ -> error "GADT records not suported"
    InfixCon arg1 arg2 -> doGADTCon [arg1, arg2] resTy 
    
  where
    doGADTCon args resTy = ppBinder summary occ <+> dcolon <+> hsep [
                             ppForAll forall ltvs lcontext,
                             ppLType (foldr mkFunTy resTy args) ]

    header_  = ppConstrHdr forall tyVars context
    occ      = docNameOcc . unLoc . con_name $ con
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
   (if null ctxt then noHtml else ppContextNoArrow ctxt <+> toHtml "=> ")
  where
    ppForall = case forall of 
      Explicit -> keyword "forall" <+> hsep (map ppName tvs) <+> toHtml ". "
      Implicit -> empty

ppSideBySideConstr :: LConDecl DocName -> HtmlTable
ppSideBySideConstr (L _ con) = case con_res con of 
 
  ResTyH98 -> case con_details con of 

    PrefixCon args -> 
      argBox (hsep ((header_ +++ ppBinder False occ) : map ppLParendType args)) 
      <-> maybeRDocBox mbLDoc  

    RecCon fields -> 
      argBox (header_ +++ ppBinder False occ) <->
      maybeRDocBox mbLDoc </>
      (tda [theclass "body"] << spacedTable1 <<
      aboves (map ppSideBySideField fields))

    InfixCon arg1 arg2 -> 
      argBox (hsep [header_+++ppLParendType arg1, ppBinder False occ, ppLParendType arg2])
      <-> maybeRDocBox mbLDoc
 
  ResTyGADT resTy -> case con_details con of
    PrefixCon args -> doGADTCon args resTy
    RecCon _ -> error "GADT records not supported"
    InfixCon arg1 arg2 -> doGADTCon [arg1, arg2] resTy 

 where 
    doGADTCon args resTy = argBox (ppBinder False occ <+> dcolon <+> hsep [
                               ppForAll forall ltvs (con_cxt con),
                               ppLType (foldr mkFunTy resTy args) ]
                            ) <-> maybeRDocBox mbLDoc


    header_ = ppConstrHdr forall tyVars context
    occ     = docNameOcc . unLoc . con_name $ con
    ltvs    = con_qvars con
    tyVars  = tyvarNames (con_qvars con)
    context = unLoc (con_cxt con)
    forall  = con_explicit con
    mbLDoc  = con_doc con
    mkFunTy a b = noLoc (HsFunTy a b)

ppSideBySideField :: ConDeclField DocName -> HtmlTable
ppSideBySideField (ConDeclField (L _ name) ltype mbLDoc) =
  argBox (ppBinder False (docNameOcc name)
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
ppShortField summary (ConDeclField (L _ name) ltype _) 
  = tda [theclass "recfield"] << (
      ppBinder summary (docNameOcc name)
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

-- | Print the LHS of a data\/newtype declaration.
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
    ppTyClBinderWithVars summary decl


-- ----------------------------------------------------------------------------
-- Types and contexts


ppKind :: Outputable a => a -> Html
ppKind k = toHtml $ showSDoc (ppr k)


{-
ppForAll Implicit _ lctxt = ppCtxtPart lctxt
ppForAll Explicit ltvs lctxt = 
  hsep (keyword "forall" : ppTyVars ltvs ++ [dot]) <+> ppCtxtPart lctxt 
-}


ppBang :: HsBang -> Html
ppBang HsNoBang = empty 
ppBang HsStrict = toHtml "!"
ppBang HsUnbox  = toHtml "!" -- unboxed args is an implementation detail,
                             -- so we just show the strictness annotation


tupleParens :: Boxity -> [Html] -> Html
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


pREC_TOP, pREC_FUN, pREC_OP, pREC_CON :: Int

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


ppLType, ppLParendType :: Located (HsType DocName) -> Html
ppLType       = ppType . unLoc
ppLParendType = ppParendType . unLoc


ppType, ppParendType :: HsType DocName -> Html
ppType ty       = ppr_mono_ty pREC_TOP ty
ppParendType ty = ppr_mono_ty pREC_CON ty


-- Drop top-level for-all type variables in user style
-- since they are implicit in Haskell

ppForAll :: HsExplicitForAll -> [Located (HsTyVarBndr DocName)]
         -> Located (HsContext DocName) -> Html
ppForAll expl tvs cxt 
  | show_forall = forall_part <+> ppLContext cxt
  | otherwise   = ppLContext cxt
  where
    show_forall = not (null tvs) && is_explicit
    is_explicit = case expl of {Explicit -> True; Implicit -> False}
    forall_part = hsep (keyword "forall" : ppTyVars tvs) +++ dot 


ppr_mono_lty :: Int -> LHsType DocName -> Html
ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)


ppr_mono_ty :: Int -> HsType DocName -> Html
ppr_mono_ty ctxt_prec (HsForAllTy expl tvs ctxt ty)
  = maybeParen ctxt_prec pREC_FUN $
    hsep [ppForAll expl tvs ctxt, ppr_mono_lty pREC_TOP ty]

-- gaw 2004
ppr_mono_ty _         (HsBangTy b ty)     = ppBang b +++ ppLParendType ty
ppr_mono_ty _         (HsTyVar name)      = ppDocName name
ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)   = ppr_fun_ty ctxt_prec ty1 ty2
ppr_mono_ty _         (HsTupleTy con tys) = tupleParens con (map ppLType tys)
ppr_mono_ty _         (HsKindSig ty kind) = parens (ppr_mono_lty pREC_TOP ty <+> dcolon <+> ppKind kind)
ppr_mono_ty _         (HsListTy ty)       = brackets (ppr_mono_lty pREC_TOP ty)
ppr_mono_ty _         (HsPArrTy ty)       = pabrackets (ppr_mono_lty pREC_TOP ty)
ppr_mono_ty _         (HsPredTy p)        = parens (ppPred p)
ppr_mono_ty _         (HsNumTy n)         = toHtml (show n) -- generics only
ppr_mono_ty _         (HsSpliceTy _)      = error "ppr_mono_ty-haddock"

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty, ppr_mono_lty pREC_CON arg_ty]

ppr_mono_ty ctxt_prec (HsOpTy ty1 op ty2)
  = maybeParen ctxt_prec pREC_FUN $
    ppr_mono_lty pREC_OP ty1 <+> ppr_op <+> ppr_mono_lty pREC_OP ty2
  where
    ppr_op = if not (isSymOcc occName) then quote (ppLDocName op) else ppLDocName op
    occName = docNameOcc . unLoc $ op

ppr_mono_ty ctxt_prec (HsParTy ty)
--  = parens (ppr_mono_lty pREC_TOP ty)
  = ppr_mono_lty ctxt_prec ty

ppr_mono_ty ctxt_prec (HsDocTy ty _)
  = ppr_mono_lty ctxt_prec ty


ppr_fun_ty :: Int -> LHsType DocName -> LHsType DocName -> Html 
ppr_fun_ty ctxt_prec ty1 ty2
  = let p1 = ppr_mono_lty pREC_FUN ty1
        p2 = ppr_mono_lty pREC_TOP ty2
    in
    maybeParen ctxt_prec pREC_FUN $
    hsep [p1, arrow <+> p2]


-- ----------------------------------------------------------------------------
-- Names

ppOccName :: OccName -> Html
ppOccName = toHtml . occNameString

ppRdrName :: RdrName -> Html
ppRdrName = ppOccName . rdrNameOcc

ppLDocName :: Located DocName -> Html
ppLDocName (L _ d) = ppDocName d

ppDocName :: DocName -> Html
ppDocName (Documented name mdl) = 
  linkIdOcc mdl (Just occName) << ppOccName occName
    where occName = nameOccName name
ppDocName (Undocumented name) = toHtml (getOccString name)

linkTarget :: OccName -> Html
linkTarget n = namedAnchor (anchorNameStr n) << toHtml "" 

ppName :: Name -> Html
ppName name = toHtml (getOccString name)


ppBinder :: Bool -> OccName -> Html
-- The Bool indicates whether we are generating the summary, in which case
-- the binder will be a link to the full definition.
ppBinder True n = linkedAnchor (anchorNameStr n) << ppBinder' n
ppBinder False n = linkTarget n +++ bold << ppBinder' n


ppBinder' :: OccName -> Html
ppBinder' n
  | isVarSym n = parens $ ppOccName n
  | otherwise  = ppOccName n


linkId :: Module -> Maybe Name -> Html -> Html
linkId mdl mbName = linkIdOcc mdl (fmap nameOccName mbName)


linkIdOcc :: Module -> Maybe OccName -> Html -> Html
linkIdOcc mdl mbName = anchor ! [href uri]
  where 
    uri = case mbName of
      Nothing   -> moduleHtmlFile mdl
      Just name -> nameHtmlRef mdl name

ppModule :: Module -> String -> Html
ppModule mdl ref = anchor ! [href ((moduleHtmlFile mdl) ++ ref)] 
                   << toHtml (moduleString mdl)

-- -----------------------------------------------------------------------------
-- * Doc Markup

parHtmlMarkup :: (a -> Html) -> (a -> Bool) -> DocMarkup a Html
parHtmlMarkup ppId isTyCon = Markup {
  markupParagraph     = paragraph,
  markupEmpty	      = toHtml "",
  markupString        = toHtml,
  markupAppend        = (+++),
  markupIdentifier    = tt . ppId . choose,
  markupModule        = \m -> let (mdl,ref) = break (=='#') m in ppModule (mkModuleNoPackage mdl) ref,
  markupEmphasis      = emphasize . toHtml,
  markupMonospaced    = tt . toHtml,
  markupUnorderedList = ulist . concatHtml . map (li <<),
  markupPic           = \path -> image ! [src path],
  markupOrderedList   = olist . concatHtml . map (li <<),
  markupDefList       = dlist . concatHtml . map markupDef,
  markupCodeBlock     = pre,
  markupURL	      = \url -> anchor ! [href url] << toHtml url,
  markupAName	      = \aname -> namedAnchor aname << toHtml ""
  }
  where
    -- If an id can refer to multiple things, we give precedence to type
    -- constructors.  This should ideally be done during renaming from RdrName
    -- to Name, but since we will move this process from GHC into Haddock in
    -- the future, we fix it here in the meantime.
    -- TODO: mention this rule in the documentation.
    choose [] = error "empty identifier list in HsDoc"
    choose [x] = x
    choose (x:y:_)
      | isTyCon x = x
      | otherwise = y


markupDef :: (HTML a, HTML b) => (a, b) -> Html
markupDef (a,b) = dterm << a +++ ddef << b


htmlMarkup :: DocMarkup DocName Html
htmlMarkup = parHtmlMarkup ppDocName (isTyConName . getName)

htmlOrigMarkup :: DocMarkup Name Html
htmlOrigMarkup = parHtmlMarkup ppName isTyConName

htmlRdrMarkup :: DocMarkup RdrName Html
htmlRdrMarkup = parHtmlMarkup ppRdrName isRdrTc

-- If the doc is a single paragraph, don't surround it with <P> (this causes
-- ugly extra whitespace with some browsers).
docToHtml :: HsDoc DocName -> Html
docToHtml doc = markup htmlMarkup (unParagraph (markup htmlCleanup doc))

origDocToHtml :: HsDoc Name -> Html
origDocToHtml doc = markup htmlOrigMarkup (unParagraph (markup htmlCleanup doc))

rdrDocToHtml :: HsDoc RdrName -> Html
rdrDocToHtml doc = markup htmlRdrMarkup (unParagraph (markup htmlCleanup doc))

-- If there is a single paragraph, then surrounding it with <P>..</P>
-- can add too much whitespace in some browsers (eg. IE).  However if
-- we have multiple paragraphs, then we want the extra whitespace to
-- separate them.  So we catch the single paragraph case and transform it
-- here.
unParagraph :: HsDoc a -> HsDoc a
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

infixr 8 <+>, <++>
(<+>) :: Html -> Html -> Html
a <+> b = Html (getHtmlElements (toHtml a) ++ HtmlString " ": getHtmlElements (toHtml b))

(<++>) :: Html -> Html -> Html
a <++> b = a +++ spaceHtml +++ b

keyword :: String -> Html
keyword s = thespan ! [theclass "keyword"] << toHtml s

equals, comma :: Html
equals = char '='
comma  = char ','

char :: Char -> Html
char c = toHtml [c]

empty :: Html
empty  = noHtml


quote :: Html -> Html
quote h = char '`' +++ h +++ '`'


parens, brackets, pabrackets, braces :: Html -> Html
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
topDeclBox :: LinksInfo -> SrcSpan -> DocName -> Html -> HtmlTable
topDeclBox ((_,_,Nothing), (_,_,Nothing)) _ _ html = declBox html
topDeclBox ((_,_,maybe_source_url), (_,_,maybe_wiki_url))
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
                          let url' = spliceURL (Just fname) (Just origMod)
                                               (Just n) (Just loc) url
                           in anchor ! [href url'] << toHtml "Source"

        wikiLink =
          case maybe_wiki_url of
            Nothing  -> Html.emptyTable
            Just url -> tda [theclass "declbut"] <<
                          let url' = spliceURL (Just fname) (Just mdl)
                                               (Just n) (Just loc) url
                           in anchor ! [href url'] << toHtml "Comments"
  
        -- For source links, we want to point to the original module,
        -- because only that will have the source.  
        -- TODO: do something about type instances. They will point to
        -- the module defining the type family, which is wrong.
        origMod = nameModule n

        -- Name must be documented, otherwise we wouldn't get here
        Documented n mdl = name

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

bodyBox :: Html -> HtmlTable
bodyBox html = tda [theclass "body"] << vanillaTable << html

-- a vanilla table has width 100%, no border, no padding, no spacing
-- a narrow table is the same but without width 100%.
vanillaTable, vanillaTable2, narrowTable :: Html -> Html
vanillaTable  = table ! [theclass "vanilla",  cellspacing 0, cellpadding 0]
vanillaTable2 = table ! [theclass "vanilla2", cellspacing 0, cellpadding 0]
narrowTable   = table ! [theclass "narrow",   cellspacing 0, cellpadding 0]

spacedTable1, spacedTable5 :: Html -> Html
spacedTable1 = table ! [theclass "vanilla",  cellspacing 1, cellpadding 0]
spacedTable5 = table ! [theclass "vanilla",  cellspacing 5, cellpadding 0]

constrHdr, methHdr, atHdr :: HtmlTable
constrHdr  = tda [ theclass "section4" ] << toHtml "Constructors"
methHdr    = tda [ theclass "section4" ] << toHtml "Methods"
atHdr      = tda [ theclass "section4" ] << toHtml "Associated Types"

instHdr :: String -> HtmlTable
instHdr id_ = 
  tda [ theclass "section4" ] << (collapsebutton id_ +++ toHtml " Instances")

dcolon, arrow, darrow, dot :: Html
dcolon = toHtml "::"
arrow  = toHtml "->"
darrow = toHtml "=>"
dot    = toHtml "."

s8, s15 :: HtmlTable
s8  = tda [ theclass "s8" ]  << noHtml
s15 = tda [ theclass "s15" ] << noHtml


-- | Generate a named anchor
--
-- This actually generates two anchor tags, one with the name unescaped, and one
-- with the name URI-escaped. This is needed because Opera 9.52 (and later
-- versions) needs the name to be unescaped, while IE 7 needs it to be escaped.
--
namedAnchor :: String -> Html -> Html
namedAnchor n = (anchor ! [Html.name n]) . (anchor ! [Html.name (escapeStr n)])


--
-- A section of HTML which is collapsible via a +/- button.
--

-- TODO: Currently the initial state is non-collapsed. Change the 'minusFile'
-- below to a 'plusFile' and the 'display:block;' to a 'display:none;' when we
-- use cookies from JavaScript to have a more persistent state.

collapsebutton :: String -> Html
collapsebutton id_ = 
  image ! [ src minusFile, theclass "coll", onclick ("toggle(this,'" ++ id_ ++ "')"), alt "show/hide" ]

collapsed :: (HTML a) => (Html -> Html) -> String -> a -> Html
collapsed fn id_ html =
  fn ! [identifier id_, thestyle "display:block;"] << html

-- A quote is a valid part of a Haskell identifier, but it would interfere with
-- the ECMA script string delimiter used in collapsebutton above.
collapseId :: Name -> String
collapseId nm = "i:" ++ escapeStr (getOccString nm)

linkedAnchor :: String -> Html -> Html
linkedAnchor frag = anchor ! [href hr_]
   where hr_ | null frag = ""
             | otherwise = '#': escapeStr frag

documentCharacterEncoding :: Html
documentCharacterEncoding =
   meta ! [httpequiv "Content-Type", content "text/html; charset=UTF-8"]

styleSheet :: Html
styleSheet =
   thelink ! [href cssFile, rel "stylesheet", thetype "text/css"]
