-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

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
import Haddock.Types hiding ( Doc )
import Haddock.Version
import Haddock.Utils
import Haddock.Utils.Html hiding ( name, title, p )
import qualified Haddock.Utils.Html as Html
import Haddock.GhcUtils

import Control.Exception     ( bracket )
import Control.Monad         ( when, unless, join )
import Data.Char             ( toUpper )
import Data.List             ( sortBy, groupBy )
import Data.Maybe
import Foreign.Marshal.Alloc ( allocaBytes )
import System.IO             ( IOMode(..), hClose, hGetBuf, hPutBuf, openFile )
import System.Directory hiding ( copyFile )
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
	-> Maybe (HsDoc GHC.RdrName)    -- prologue text, maybe
	-> Maybe String		        -- the Html Help format (--html-help)
	-> SourceURLs			-- the source URL (--source)
	-> WikiURLs			-- the wiki URL (--wiki)
	-> Maybe String			-- the contents URL (--use-contents)
	-> Maybe String			-- the index URL (--use-index)
	-> Bool                         -- whether to use unicode in output (--use-unicode)
	-> IO ()

ppHtml doctitle maybe_package ifaces odir prologue maybe_html_help_format
	maybe_source_url maybe_wiki_url
	maybe_contents_url maybe_index_url unicode =  do
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
	   maybe_contents_url maybe_index_url unicode) visible_ifaces

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

      doOneEntry :: (String, (HaddockModInfo GHC.Name) -> Maybe String) -> Maybe HtmlTable
      doOneEntry (fieldName,field) = case field info of
         Nothing -> Nothing
         Just fieldValue -> 
            Just ((tda [theclass "infohead"] << toHtml fieldName)
               <-> (tda [theclass "infoval"]) << toHtml fieldValue)
     
      entries :: [HtmlTable]
      entries = mapMaybe doOneEntry [
         ("Portability",hmi_portability),
         ("Stability",hmi_stability),
         ("Maintainer",hmi_maintainer)
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
   -> [InstalledInterface] -> Bool -> Maybe (HsDoc GHC.RdrName)
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
  createDirectoryIfMissing True odir
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

ppPrologue :: String -> Maybe (HsDoc GHC.RdrName) -> HtmlTable
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

    td_pad_w :: Double -> Int -> Html -> HtmlTable
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
  createDirectoryIfMissing True odir
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
        index_html
           )

  createDirectoryIfMissing True odir

  when split_indices $
    mapM_ (do_sub_index index) initialChars

  writeFile (pathJoin [odir, indexHtmlFile]) (renderHtml html)
  
    -- Generate index and contents page for Html Help if requested
  case maybe_html_help_format of
    Nothing        -> return ()
    Just "mshelp"  -> ppHHIndex  odir maybe_package ifaces
    Just "mshelp2" -> ppHH2Index odir maybe_package ifaces
    Just "devhelp" -> return ()
    Just format    -> fail ("The "++format++" format is not implemented")
 where

  index_html
    | split_indices = 
	tda [theclass "section1"] << 
	      	toHtml ("Index") </>
	indexInitialLetterLinks
    | otherwise =
	td << setTrClass (table ! [identifier "indexlist", cellpadding 0, cellspacing 5] <<
	  aboves (map indexElt index))

  -- an arbitrary heuristic:
  -- too large, and a single-page will be slow to load
  -- too small, and we'll have lots of letter-indexes with only one
  --   or two members in them, which seems inefficient or
  --   unnecessarily hard to use.
  split_indices = length index > 150

  setTrClass :: Html -> Html
  setTrClass (Html xs) = Html $ map f xs
      where
          f (HtmlTag name attrs inner)
               | map toUpper name == "TR" = HtmlTag name (theclass "indexrow":attrs) inner
               | otherwise = HtmlTag name attrs (setTrClass inner)
          f x = x
 	
  indexInitialLetterLinks = 
	td << setTrClass (table ! [cellpadding 0, cellspacing 5] <<
	    besides [ td << anchor ! [href (subIndexHtmlFile c)] <<
			 toHtml [c]
		    | c <- initialChars
                    , any ((==c) . toUpper . head . fst) index ])

  -- todo: what about names/operators that start with Unicode
  -- characters?
  -- Exports beginning with '_' can be listed near the end,
  -- presumably they're not as important... but would be listed
  -- with non-split index!
  initialChars = [ 'A'..'Z' ] ++ ":!#$%&*+./<=>?@\\^|-~" ++ "_"

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
	        td << setTrClass (table ! [identifier "indexlist", cellpadding 0, cellspacing 5] <<
	      	  aboves (map indexElt index_part) )
	       )

      index_part = [(n,stuff) | (n,stuff) <- this_ix, toUpper (head n) == c]


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

  doAnnotatedEntity :: (Integer, (Name, [(Module, Bool)])) -> HtmlTable
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
	-> Maybe String -> Maybe String -> Bool
	-> Interface -> IO ()
ppHtmlModule odir doctitle
  maybe_source_url maybe_wiki_url
  maybe_contents_url maybe_index_url unicode iface = do
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
	    ifaceToHtml maybe_source_url maybe_wiki_url iface unicode </> s15 </>
	    footer
         )
  createDirectoryIfMissing True odir
  writeFile (pathJoin [odir, moduleHtmlFile mdl]) (renderHtml html)
  ppHtmlModuleMiniSynopsis odir doctitle iface unicode

ppHtmlModuleMiniSynopsis :: FilePath -> String -> Interface -> Bool -> IO ()
ppHtmlModuleMiniSynopsis odir _doctitle iface unicode = do
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
           miniSynopsis mdl iface unicode)
  createDirectoryIfMissing True odir
  writeFile (pathJoin [odir, "mini_" ++ moduleHtmlFile mdl]) (renderHtml html)

ifaceToHtml :: SourceURLs -> WikiURLs -> Interface -> Bool -> HtmlTable
ifaceToHtml maybe_source_url maybe_wiki_url iface unicode
  = abovesSep s15 (contents ++ description: synopsis: maybe_doc_hdr: bdy)
  where
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
            abovesSep s8 (map (processExport True linksInfo unicode)
            (filter forSummary exports))
        )

	-- if the documentation doesn't begin with a section header, then
	-- add one ("Documentation").
    maybe_doc_hdr
      = case exports of		   
          [] -> Html.emptyTable
          ExportGroup _ _ _ : _ -> Html.emptyTable
          _ -> tda [ theclass "section1" ] << toHtml "Documentation"

    bdy  = map (processExport False linksInfo unicode) exports
    linksInfo = (maybe_source_url, maybe_wiki_url)

miniSynopsis :: Module -> Interface -> Bool -> Html
miniSynopsis mdl iface unicode =
    thediv ! [ theclass "mini-synopsis" ]
      << hsep (map (processForMiniSynopsis mdl unicode) $ exports)
  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)

processForMiniSynopsis :: Module -> Bool -> ExportItem DocName ->  Html
processForMiniSynopsis mdl unicode (ExportDecl (L _loc decl0) _doc _ _insts) =
  thediv ! [theclass "decl" ] <<
  case decl0 of
    TyClD d@(TyFamily{}) -> ppTyFamHeader True False d unicode
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
processForMiniSynopsis _ _ (ExportGroup lvl _id txt) =
  let heading
        | lvl == 1 = h1
        | lvl == 2 = h2
        | lvl >= 3 = h3
        | otherwise = error "bad group level"
  in heading << docToHtml txt
processForMiniSynopsis _ _ _ = noHtml

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

processExport :: Bool -> LinksInfo -> Bool -> (ExportItem DocName) -> HtmlTable
processExport _ _ _ (ExportGroup lev id0 doc)
  = ppDocGroup lev (namedAnchor id0 << docToHtml doc)
processExport summary links unicode (ExportDecl decl doc subdocs insts)
  = ppDecl summary links decl doc insts subdocs unicode
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
          Maybe (HsDoc DocName) -> [InstHead DocName] -> [(DocName, Maybe (HsDoc DocName))] -> Bool -> HtmlTable
ppDecl summ links (L loc decl) mbDoc instances subdocs unicode = case decl of
  TyClD d@(TyFamily {})          -> ppTyFam summ False links loc mbDoc d unicode
  TyClD d@(TyData {})
    | Nothing <- tcdTyPats d     -> ppDataDecl summ links instances subdocs loc mbDoc d unicode
    | Just _  <- tcdTyPats d     -> ppDataInst summ links loc mbDoc d 
  TyClD d@(TySynonym {})
    | Nothing <- tcdTyPats d     -> ppTySyn summ links loc mbDoc d unicode
    | Just _  <- tcdTyPats d     -> ppTyInst summ False links loc mbDoc d unicode
  TyClD d@(ClassDecl {})         -> ppClassDecl summ links instances loc mbDoc subdocs d unicode
  SigD (TypeSig (L _ n) (L _ t)) -> ppFunSig summ links loc mbDoc n t unicode
  ForD d                         -> ppFor summ links loc mbDoc d unicode
  InstD _                        -> Html.emptyTable
  _                              -> error "declaration not supported by ppDecl"

ppFunSig :: Bool -> LinksInfo -> SrcSpan -> Maybe (HsDoc DocName) ->
            DocName -> HsType DocName -> Bool -> HtmlTable
ppFunSig summary links loc mbDoc docname typ unicode =
  ppTypeOrFunSig summary links loc docname typ mbDoc
    (ppTypeSig summary occname typ unicode, ppBinder False occname, dcolon unicode) unicode
  where
    occname = docNameOcc docname

ppTypeOrFunSig :: Bool -> LinksInfo -> SrcSpan -> DocName -> HsType DocName ->
                  Maybe (HsDoc DocName) -> (Html, Html, Html) -> Bool -> HtmlTable
ppTypeOrFunSig summary links loc docname typ doc (pref1, pref2, sep) unicode
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
          hsep (forallSymbol unicode : ppTyVars tvs ++ [dot]) <+>
          ppLContextNoArrow lctxt unicode)
            <-> rdocBox noHtml) </> 
            do_largs (darrow unicode) ltype
    do_args leader (HsForAllTy Implicit _ lctxt ltype)
      = (argBox (leader <+> ppLContextNoArrow lctxt unicode)
          <-> rdocBox noHtml) </> 
          do_largs (darrow unicode) ltype
--hacl
--    do_args leader (HsFunTy (L _ (HsDocTy lt ldoc)) r)
--      = (argBox (leader <+> ppLType unicode lt) <-> rdocBox (docToHtml (unLoc ldoc)))
--          </> do_largs (arrow unicode) r
    do_args leader (HsFunTy lt r)
      = (argBox (leader <+> ppLType unicode lt) <-> rdocBox noHtml) </> do_largs (arrow unicode) r
--    do_args leader (HsDocTy lt ldoc)
--      = (argBox (leader <+> ppLType unicode lt) <-> rdocBox (docToHtml (unLoc ldoc)))
    do_args leader t
      = argBox (leader <+> ppType unicode t) <-> rdocBox (noHtml)


ppTyVars :: [LHsTyVarBndr DocName] -> [Html]
ppTyVars tvs = map ppTyName (tyvarNames tvs)


tyvarNames :: [LHsTyVarBndr DocName] -> [Name]
tyvarNames = map (getName . hsTyVarName . unLoc)
  

ppFor :: Bool -> LinksInfo -> SrcSpan -> Maybe Doc -> ForeignDecl DocName -> Bool -> HtmlTable
ppFor summary links loc mbDoc (ForeignImport (L _ name) (L _ typ) _) unicode
  = ppFunSig summary links loc mbDoc name typ unicode
ppFor _ _ _ _ _ _ = error "ppFor"


-- we skip type patterns for now
ppTySyn :: Bool -> LinksInfo -> SrcSpan -> Maybe Doc -> TyClDecl DocName -> Bool -> HtmlTable
ppTySyn summary links loc mbDoc (TySynonym (L _ name) ltyvars _ ltype) unicode
  = ppTypeOrFunSig summary links loc name (unLoc ltype) mbDoc 
                   (full, hdr, spaceHtml +++ equals) unicode
  where
    hdr  = hsep ([keyword "type", ppBinder summary occ] ++ ppTyVars ltyvars)
    full = hdr <+> equals <+> ppLType unicode ltype
    occ  = docNameOcc name
ppTySyn _ _ _ _ _ _ = error "declaration not supported by ppTySyn"


ppTypeSig :: Bool -> OccName -> HsType DocName  -> Bool -> Html
ppTypeSig summary nm ty unicode = ppBinder summary nm <+> dcolon unicode <+> ppType unicode ty


ppTyName :: Name -> Html
ppTyName name
  | isNameSym name = parens (ppName name)
  | otherwise = ppName name


--------------------------------------------------------------------------------
-- Type families
--------------------------------------------------------------------------------


ppTyFamHeader :: Bool -> Bool -> TyClDecl DocName -> Bool -> Html
ppTyFamHeader summary associated decl unicode =

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
    Just kind -> dcolon unicode  <+> ppKind kind 
    Nothing -> empty


ppTyFam :: Bool -> Bool -> LinksInfo -> SrcSpan -> Maybe (HsDoc DocName) ->
              TyClDecl DocName -> Bool -> HtmlTable
ppTyFam summary associated links loc mbDoc decl unicode
  
  | summary = declWithDoc summary links loc docname mbDoc 
              (ppTyFamHeader True associated decl unicode)
  
  | associated, isJust mbDoc         = header_ </> bodyBox << doc 
  | associated                       = header_ 
  | null instances, isJust mbDoc     = header_ </> bodyBox << doc
  | null instances                   = header_
  | isJust mbDoc                     = header_ </> bodyBox << (doc </> instancesBit)
  | otherwise                        = header_ </> bodyBox << instancesBit

  where
    docname = tcdName decl

    header_ = topDeclBox links loc docname (ppTyFamHeader summary associated decl unicode)

    doc = ndocBox . docToHtml . fromJust $ mbDoc 

    instId = collapseId (getName docname)

    instancesBit = instHdr instId </>
  	  tda [theclass "body"] << 
            collapsed thediv instId (
              spacedTable1 << (
                aboves (map (declBox . ppInstHead unicode) instances)
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
            TyClDecl DocName -> Bool -> HtmlTable
ppTyInst summary associated links loc mbDoc decl unicode
  
  | summary = declWithDoc summary links loc docname mbDoc
              (ppTyInstHeader True associated decl unicode)
  
  | isJust mbDoc = header_ </> bodyBox << doc 
  | otherwise    = header_

  where
    docname = tcdName decl

    header_ = topDeclBox links loc docname (ppTyInstHeader summary associated decl unicode)

    doc = case mbDoc of
      Just d -> ndocBox (docToHtml d)
      Nothing -> Html.emptyTable


ppTyInstHeader :: Bool -> Bool -> TyClDecl DocName -> Bool -> Html
ppTyInstHeader _ _ decl unicode =
  keyword "type instance" <+>
  ppAppNameTypes (tcdName decl) typeArgs unicode
  where
    typeArgs = map unLoc . fromJust . tcdTyPats $ decl


--------------------------------------------------------------------------------
-- Associated Types
--------------------------------------------------------------------------------
    

ppAssocType :: Bool -> LinksInfo -> Maybe (HsDoc DocName) -> LTyClDecl DocName -> Bool -> HtmlTable
ppAssocType summ links doc (L loc decl) unicode = 
  case decl of
    TyFamily  {} -> ppTyFam summ True links loc doc decl unicode
    TySynonym {} -> ppTySyn summ links loc doc decl unicode
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
ppAppNameTypes :: DocName -> [HsType DocName] -> Bool -> Html
ppAppNameTypes n ts unicode = ppTypeApp n ts ppDocName (ppParendType unicode)


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
    operator = isNameSym . getName $ n
    opApp = ppT t1 <+> ppDN n <+> ppT t2

ppTypeApp n ts ppDN ppT = ppDN n <+> hsep (map ppT ts)


-------------------------------------------------------------------------------
-- Contexts 
-------------------------------------------------------------------------------


ppLContext, ppLContextNoArrow :: Located (HsContext DocName) -> Bool -> Html
ppLContext        = ppContext        . unLoc
ppLContextNoArrow = ppContextNoArrow . unLoc


ppContextNoArrow :: HsContext DocName -> Bool -> Html
ppContextNoArrow []  _ = empty
ppContextNoArrow cxt unicode = pp_hs_context (map unLoc cxt) unicode


ppContextNoLocs :: [HsPred DocName] -> Bool -> Html
ppContextNoLocs []  _ = empty
ppContextNoLocs cxt unicode = pp_hs_context cxt unicode <+> darrow unicode


ppContext :: HsContext DocName -> Bool -> Html
ppContext cxt unicode = ppContextNoLocs (map unLoc cxt) unicode


pp_hs_context :: [HsPred DocName] -> Bool -> Html
pp_hs_context []  _       = empty
pp_hs_context [p] unicode = ppPred unicode p
pp_hs_context cxt unicode = parenList (map (ppPred unicode) cxt)


ppPred :: Bool -> HsPred DocName -> Html
ppPred unicode (HsClassP n ts) = ppAppNameTypes n (map unLoc ts) unicode
ppPred unicode (HsEqualP t1 t2) = ppLType unicode t1 <+> toHtml "~" <+> ppLType unicode t2
ppPred unicode (HsIParam (IPName n) t)
  = toHtml "?" +++ ppDocName n <+> dcolon unicode <+> ppLType unicode t


-------------------------------------------------------------------------------
-- Class declarations
-------------------------------------------------------------------------------


ppClassHdr :: Bool -> Located [LHsPred DocName] -> DocName
           -> [Located (HsTyVarBndr DocName)] -> [Located ([DocName], [DocName])]
           -> Bool -> Html
ppClassHdr summ lctxt n tvs fds unicode = 
  keyword "class" 
  <+> (if not . null . unLoc $ lctxt then ppLContext lctxt unicode else empty)
  <+> ppAppDocNameNames summ n (tyvarNames $ tvs)
	<+> ppFds fds unicode


ppFds :: [Located ([DocName], [DocName])] -> Bool -> Html
ppFds fds unicode =
  if null fds then noHtml else 
	char '|' <+> hsep (punctuate comma (map (fundep . unLoc) fds))
  where
	fundep (vars1,vars2) = hsep (map ppDocName vars1) <+> arrow unicode <+>
			       hsep (map ppDocName vars2)

ppShortClassDecl :: Bool -> LinksInfo -> TyClDecl DocName -> SrcSpan -> [(DocName, Maybe (HsDoc DocName))] -> Bool -> HtmlTable
ppShortClassDecl summary links (ClassDecl lctxt lname tvs fds sigs _ ats _) loc subdocs unicode = 
  if null sigs && null ats
    then (if summary then declBox else topDeclBox links loc nm) hdr
    else (if summary then declBox else topDeclBox links loc nm) (hdr <+> keyword "where")
	    </> 
      (
				bodyBox <<
					aboves
					(
						[ ppAssocType summary links doc at unicode | at <- ats
                                                , let doc = join $ lookup (tcdName $ unL at) subdocs ]  ++

						[ ppFunSig summary links loc doc n typ unicode
						| L _ (TypeSig (L _ n) (L _ typ)) <- sigs
						, let doc = join $ lookup n subdocs ] 
					)
				)
  where
    hdr = ppClassHdr summary lctxt (unLoc lname) tvs fds unicode
    nm  = unLoc lname
ppShortClassDecl _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"
    


ppClassDecl :: Bool -> LinksInfo -> [InstHead DocName] -> SrcSpan
            -> Maybe (HsDoc DocName) -> [(DocName, Maybe (HsDoc DocName))]
            -> TyClDecl DocName -> Bool -> HtmlTable
ppClassDecl summary links instances loc mbDoc subdocs
	decl@(ClassDecl lctxt lname ltyvars lfds lsigs _ ats _) unicode
  | summary = ppShortClassDecl summary links decl loc subdocs unicode
  | otherwise = classheader </> bodyBox << (classdoc </> body_ </> instancesBit)
  where 
    classheader
      | null lsigs = topDeclBox links loc nm (hdr unicode)
      | otherwise  = topDeclBox links loc nm (hdr unicode <+> keyword "where")

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
      abovesSep s8 [ ppFunSig summary links loc doc n typ unicode
                   | L _ (TypeSig (L _ n) (L _ typ)) <- lsigs
                   , let doc = join $ lookup n subdocs ]

    atTable = abovesSep s8 $ [ ppAssocType summary links doc at unicode | at <- ats
                             , let doc = join $ lookup (tcdName $ unL at) subdocs ]

    instId = collapseId (getName nm)
    instancesBit
      | null instances = Html.emptyTable
      | otherwise 
        =  s8 </> instHdr instId </>
           tda [theclass "body"] << 
             collapsed thediv instId (
             spacedTable1 << (
               aboves (map (declBox . ppInstHead unicode) instances)
             ))
ppClassDecl _ _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"


ppInstHead :: Bool -> InstHead DocName -> Html
ppInstHead unicode ([],   n, ts) = ppAppNameTypes n ts unicode
ppInstHead unicode (ctxt, n, ts) = ppContextNoLocs ctxt unicode <+> ppAppNameTypes n ts unicode


-- -----------------------------------------------------------------------------
-- Data & newtype declarations


-- TODO: print contexts
ppShortDataDecl :: Bool -> LinksInfo -> SrcSpan -> TyClDecl DocName -> Bool -> Html
ppShortDataDecl summary links loc dataDecl unicode

  | [lcon] <- cons, ResTyH98 <- resTy = 
    ppDataHeader summary dataDecl unicode
    <+> equals <+> ppShortConstr summary (unLoc lcon) unicode

  | [] <- cons = ppDataHeader summary dataDecl unicode

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
      ((ppDataHeader summary dataDecl unicode) <+> 
      case resTy of ResTyGADT _ -> keyword "where"; _ -> empty)

    doConstr c con = declBox (toHtml [c] <+> ppShortConstr summary (unLoc con) unicode)
    doGADTConstr con = declBox (ppShortConstr summary (unLoc con) unicode)

    docname   = unLoc . tcdLName $ dataDecl
    cons      = tcdCons dataDecl
    resTy     = (con_res . unLoc . head) cons 

ppDataDecl :: Bool -> LinksInfo -> [InstHead DocName] ->
              [(DocName, Maybe (HsDoc DocName))] ->
              SrcSpan -> Maybe (HsDoc DocName) -> TyClDecl DocName -> Bool -> HtmlTable
ppDataDecl summary links instances subdocs loc mbDoc dataDecl unicode
  
  | summary = declWithDoc summary links loc docname mbDoc 
              (ppShortDataDecl summary links loc dataDecl unicode)
  
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
      
    header_ = topDeclBox links loc docname (ppDataHeader summary dataDecl unicode
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
	  aboves (map (ppSideBySideConstr subdocs unicode) cons)
        )

    instId = collapseId (getName docname)

    instancesBit
      | null instances = Html.emptyTable
      | otherwise 
        = instHdr instId </>
	  tda [theclass "body"] << 
          collapsed thediv instId (
            spacedTable1 << (
              aboves (map (declBox . ppInstHead unicode) instances)
            )
          )

    validTable = isJust mbDoc || not (null cons) || not (null instances)


isRecCon :: Located (ConDecl a) -> Bool
isRecCon lcon = case con_details (unLoc lcon) of 
  RecCon _ -> True
  _ -> False


ppShortConstr :: Bool -> ConDecl DocName -> Bool -> Html
ppShortConstr summary con unicode = case con_res con of 
  ResTyH98 -> case con_details con of 
    PrefixCon args -> header_ unicode +++ hsep (ppBinder summary occ : map (ppLParendType unicode) args)
    RecCon fields -> header_ unicode +++ ppBinder summary occ <+>
                                              doRecordFields fields
    InfixCon arg1 arg2 -> header_ unicode +++ 
      hsep [ppLParendType unicode arg1, ppBinder summary occ, ppLParendType unicode arg2]    

  ResTyGADT resTy -> case con_details con of 
    -- prefix & infix could use hsConDeclArgTys if it seemed to
    -- simplify the code.
    PrefixCon args -> doGADTCon args resTy
    -- display GADT records with the new syntax,
    -- Constr :: (Context) => { field :: a, field2 :: b } -> Ty (a, b)
    -- (except each field gets its own line in docs, to match
    -- non-GADT records)
    RecCon fields -> ppBinder summary occ <+> dcolon unicode <+> hsep [
                            ppForAll forall ltvs lcontext unicode,
                            doRecordFields fields,
                            arrow unicode <+> ppLType unicode resTy ]
    InfixCon arg1 arg2 -> doGADTCon [arg1, arg2] resTy 
    
  where
    doRecordFields fields = braces (vanillaTable <<
                        aboves (map (ppShortField summary unicode) fields))
    doGADTCon args resTy = ppBinder summary occ <+> dcolon unicode <+> hsep [
                             ppForAll forall ltvs lcontext unicode,
                             ppLType unicode (foldr mkFunTy resTy args) ]

    header_  = ppConstrHdr forall tyVars context
    occ      = docNameOcc . unLoc . con_name $ con
    ltvs     = con_qvars con
    tyVars   = tyvarNames ltvs 
    lcontext = con_cxt con
    context  = unLoc (con_cxt con)
    forall   = con_explicit con
    mkFunTy a b = noLoc (HsFunTy a b)

-- ppConstrHdr is for (non-GADT) existentials constructors' syntax
ppConstrHdr :: HsExplicitForAll -> [Name] -> HsContext DocName -> Bool -> Html
ppConstrHdr forall tvs ctxt unicode
 = (if null tvs then noHtml else ppForall)
   +++
   (if null ctxt then noHtml else ppContextNoArrow ctxt unicode <+> darrow unicode +++ toHtml " ")
  where
    ppForall = case forall of 
      Explicit -> forallSymbol unicode <+> hsep (map ppName tvs) <+> toHtml ". "
      Implicit -> empty

ppSideBySideConstr :: [(DocName, Maybe (HsDoc DocName))] -> Bool -> LConDecl DocName -> HtmlTable
ppSideBySideConstr subdocs unicode (L _ con) = case con_res con of 
 
  ResTyH98 -> case con_details con of 

    PrefixCon args -> 
      argBox (hsep ((header_ unicode +++ ppBinder False occ) : map (ppLParendType unicode) args)) 
      <-> maybeRDocBox mbLDoc  

    RecCon fields -> 
      argBox (header_ unicode +++ ppBinder False occ) <->
      maybeRDocBox mbLDoc
      </>
      doRecordFields fields

    InfixCon arg1 arg2 -> 
      argBox (hsep [header_ unicode+++ppLParendType unicode arg1, ppBinder False occ, ppLParendType unicode arg2])
      <-> maybeRDocBox mbLDoc
 
  ResTyGADT resTy -> case con_details con of
    -- prefix & infix could also use hsConDeclArgTys if it seemed to
    -- simplify the code.
    PrefixCon args -> doGADTCon args resTy
    cd@(RecCon fields) -> doGADTCon (hsConDeclArgTys cd) resTy
                                          </> doRecordFields fields
    InfixCon arg1 arg2 -> doGADTCon [arg1, arg2] resTy 

 where 
    doRecordFields fields =
        (tda [theclass "body"] << spacedTable1 <<
        aboves (map (ppSideBySideField subdocs unicode) fields))
    doGADTCon args resTy = argBox (ppBinder False occ <+> dcolon unicode <+> hsep [
                               ppForAll forall ltvs (con_cxt con) unicode,
                               ppLType unicode (foldr mkFunTy resTy args) ]
                            ) <-> maybeRDocBox mbLDoc


    header_ = ppConstrHdr forall tyVars context
    occ     = docNameOcc . unLoc . con_name $ con
    ltvs    = con_qvars con
    tyVars  = tyvarNames (con_qvars con)
    context = unLoc (con_cxt con)
    forall  = con_explicit con
    -- don't use "con_doc con", in case it's reconstructed from a .hi file,
    -- or also because we want Haddock to do the doc-parsing, not GHC.
    mbLDoc  = fmap noLoc $ join $ lookup (unLoc $ con_name con) subdocs
    mkFunTy a b = noLoc (HsFunTy a b)

ppSideBySideField :: [(DocName, Maybe (HsDoc DocName))] -> Bool -> ConDeclField DocName ->  HtmlTable
ppSideBySideField subdocs unicode (ConDeclField (L _ name) ltype _) =
  argBox (ppBinder False (docNameOcc name)
    <+> dcolon unicode <+> ppLType unicode ltype) <->
  maybeRDocBox mbLDoc
  where
    -- don't use cd_fld_doc for same reason we don't use con_doc above
    mbLDoc = fmap noLoc $ join $ lookup name subdocs

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

ppShortField :: Bool -> Bool -> ConDeclField DocName -> HtmlTable
ppShortField summary unicode (ConDeclField (L _ name) ltype _)
  = tda [theclass "recfield"] << (
      ppBinder summary (docNameOcc name)
      <+> dcolon unicode <+> ppLType unicode ltype
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
ppDataHeader :: Bool -> TyClDecl DocName -> Bool -> Html
ppDataHeader summary decl unicode
  | not (isDataDecl decl) = error "ppDataHeader: illegal argument"
  | otherwise = 
    -- newtype or data
    (if tcdND decl == NewType then keyword "newtype" else keyword "data") <+> 
    -- context
    ppLContext (tcdCtxt decl) unicode <+>
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


ppLType, ppLParendType :: Bool -> Located (HsType DocName) -> Html
ppLType       unicode y = ppType unicode (unLoc y)
ppLParendType unicode y = ppParendType unicode (unLoc y) 


ppType, ppParendType :: Bool -> HsType DocName -> Html
ppType       unicode ty = ppr_mono_ty pREC_TOP ty unicode 
ppParendType unicode ty = ppr_mono_ty pREC_CON ty unicode 


-- Drop top-level for-all type variables in user style
-- since they are implicit in Haskell

ppForAll :: HsExplicitForAll -> [Located (HsTyVarBndr DocName)]
         -> Located (HsContext DocName) -> Bool -> Html
ppForAll expl tvs cxt unicode
  | show_forall = forall_part <+> ppLContext cxt unicode
  | otherwise   = ppLContext cxt unicode
  where
    show_forall = not (null tvs) && is_explicit
    is_explicit = case expl of {Explicit -> True; Implicit -> False}
    forall_part = hsep (forallSymbol unicode : ppTyVars tvs) +++ dot 


ppr_mono_lty :: Int -> LHsType DocName -> Bool -> Html
ppr_mono_lty ctxt_prec ty unicode = ppr_mono_ty ctxt_prec (unLoc ty) unicode 


ppr_mono_ty :: Int -> HsType DocName -> Bool -> Html
ppr_mono_ty ctxt_prec (HsForAllTy expl tvs ctxt ty) unicode 
  = maybeParen ctxt_prec pREC_FUN $
    hsep [ppForAll expl tvs ctxt unicode, ppr_mono_lty pREC_TOP ty unicode]

-- gaw 2004
ppr_mono_ty _         (HsBangTy b ty)     u = ppBang b +++ ppLParendType u ty
ppr_mono_ty _         (HsTyVar name)      _ = ppDocName name
ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)   u = ppr_fun_ty ctxt_prec ty1 ty2 u
ppr_mono_ty _         (HsTupleTy con tys) u = tupleParens con (map (ppLType u) tys)
ppr_mono_ty _         (HsKindSig ty kind) u = parens (ppr_mono_lty pREC_TOP ty u <+> dcolon u <+> ppKind kind)
ppr_mono_ty _         (HsListTy ty)       u = brackets (ppr_mono_lty pREC_TOP ty u)
ppr_mono_ty _         (HsPArrTy ty)       u = pabrackets (ppr_mono_lty pREC_TOP ty u)
ppr_mono_ty _         (HsPredTy p)        u = parens (ppPred u p)
ppr_mono_ty _         (HsNumTy n)         _ = toHtml (show n) -- generics only
ppr_mono_ty _         (HsSpliceTy _)      _ = error "ppr_mono_ty-haddock"
#if __GLASGOW_HASKELL__ >= 611
ppr_mono_ty _         (HsRecTy _)         _ = error "ppr_mono_ty HsRecTy"
#endif

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty) unicode 
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty unicode, ppr_mono_lty pREC_CON arg_ty unicode]

ppr_mono_ty ctxt_prec (HsOpTy ty1 op ty2) unicode 
  = maybeParen ctxt_prec pREC_FUN $
    ppr_mono_lty pREC_OP ty1 unicode <+> ppr_op <+> ppr_mono_lty pREC_OP ty2 unicode
  where
    ppr_op = if not (isSymOcc occName) then quote (ppLDocName op) else ppLDocName op
    occName = docNameOcc . unLoc $ op

ppr_mono_ty ctxt_prec (HsParTy ty) unicode 
--  = parens (ppr_mono_lty pREC_TOP ty)
  = ppr_mono_lty ctxt_prec ty unicode

ppr_mono_ty ctxt_prec (HsDocTy ty _) unicode 
  = ppr_mono_lty ctxt_prec ty unicode


ppr_fun_ty :: Int -> LHsType DocName -> LHsType DocName -> Bool -> Html 
ppr_fun_ty ctxt_prec ty1 ty2 unicode
  = let p1 = ppr_mono_lty pREC_FUN ty1 unicode
        p2 = ppr_mono_lty pREC_TOP ty2 unicode
    in
    maybeParen ctxt_prec pREC_FUN $
    hsep [p1, arrow unicode <+> p2]


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
unParagraph (DocParagraph d) = d
--NO: This eliminates line breaks in the code block:  (SDM, 6/5/2003)
--unParagraph (DocCodeBlock d) = (DocMonospaced d)
unParagraph doc              = doc

htmlCleanup :: DocMarkup a (HsDoc a)
htmlCleanup = idMarkup { 
  markupUnorderedList = DocUnorderedList . map unParagraph,
  markupOrderedList   = DocOrderedList   . map unParagraph
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

maybeRDocBox :: Maybe (LHsDoc DocName) -> HtmlTable
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

dcolon, arrow, darrow, forallSymbol :: Bool -> Html
dcolon unicode = toHtml (if unicode then "∷" else "::")
arrow  unicode = toHtml (if unicode then "→" else "->")
darrow unicode = toHtml (if unicode then "⇒" else "=>")
forallSymbol unicode = if unicode then toHtml "∀" else keyword "forall"


dot :: Html
dot = toHtml "."


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
