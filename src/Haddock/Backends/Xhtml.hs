-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml ( 
  ppHtml, copyHtmlBits, 
  ppHtmlIndex, ppHtmlContents,
  ppHtmlHelpFiles
) where


import Prelude hiding (div)

import Haddock.Backends.DevHelp
import Haddock.Backends.HH
import Haddock.Backends.HH2
import Haddock.Backends.Xhtml.Decl
import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Layout
import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Util
import Haddock.ModuleTree
import Haddock.Types
import Haddock.Version
import Haddock.Utils
import Text.XHtml hiding ( name, title, p, quote )
import qualified Text.XHtml as Html
import Haddock.GhcUtils

import Control.Exception     ( bracket )
import Control.Monad         ( when, unless )
import Control.Monad.Instances ( ) -- for Functor Either a
import Data.Char             ( toUpper )
import Data.List             ( sortBy, groupBy )
import Data.Maybe
import Foreign.Marshal.Alloc ( allocaBytes )
import System.FilePath hiding ( (</>) )
import System.IO             ( IOMode(..), hClose, hGetBuf, hPutBuf, openFile )
import System.Directory hiding ( copyFile )
import Data.Map              ( Map )
import qualified Data.Map as Map hiding ( Map )
import Data.Function
import Data.Ord              ( comparing )

import GHC hiding ( NoLink, moduleInfo )
import Name
import Module



-- -----------------------------------------------------------------------------
-- Generating HTML documentation

ppHtml  :: String
        -> Maybe String                         -- package
        -> [Interface]
        -> FilePath                     -- destination directory
        -> Maybe (Doc GHC.RdrName)    -- prologue text, maybe
        -> Maybe String                 -- the Html Help format (--html-help)
        -> SourceURLs                   -- the source URL (--source)
        -> WikiURLs                     -- the wiki URL (--wiki)
        -> Maybe String                 -- the contents URL (--use-contents)
        -> Maybe String                 -- the index URL (--use-index)
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
    -> Maybe String                             -- package
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
copyHtmlBits odir libdir _maybe_css = do
  let 
        libhtmldir = joinPath [libdir, "html"]
        {-
        css_file = case maybe_css of
                        Nothing -> joinPath [libhtmldir, 'x':cssFile]
                        Just f  -> f
        css_destination = joinPath [odir, cssFile]
        -}
        copyLibFile f = do
           copyFile (joinPath [libhtmldir, f]) (joinPath [odir, f])
  --copyFile css_file css_destination
  mapM_ copyLibFile cssFiles
  mapM_ copyLibFile [ iconFile, plusFile, minusFile, jsFile, framesFile ]

footer :: Html
footer =
  divFooter << paragraph << (
    "Produced by " +++ 
    (anchor ! [href projectUrl] << toHtml projectName) +++
    (" version " ++ projectVersion)
    )
   
srcButton :: SourceURLs -> Maybe Interface -> Maybe Html
srcButton (Just src_base_url, _, _) Nothing =
  Just (anchor ! [href src_base_url] << "Source code")
srcButton (_, Just src_module_url, _) (Just iface) =
  let url = spliceURL (Just $ ifaceOrigFilename iface)
                      (Just $ ifaceMod iface) Nothing Nothing src_module_url
   in Just (anchor ! [href url] << "Source code")
srcButton _ _ =
  Nothing
 
  
wikiButton :: WikiURLs -> Maybe Module -> Maybe Html
wikiButton (Just wiki_base_url, _, _) Nothing =
  Just (anchor ! [href wiki_base_url] << "User Comments")

wikiButton (_, Just wiki_module_url, _) (Just mdl) =
  let url = spliceURL Nothing (Just mdl) Nothing Nothing wiki_module_url
   in Just (anchor ! [href url] << "User Comments")

wikiButton _ _ =
  Nothing

contentsButton :: Maybe String -> Maybe Html
contentsButton maybe_contents_url 
  = Just (anchor ! [href url] << "Contents")
  where url = maybe contentsHtmlFile id maybe_contents_url

indexButton :: Maybe String -> Maybe Html
indexButton maybe_index_url 
  = Just (anchor ! [href url] << "Index")
  where url = maybe indexHtmlFile id maybe_index_url

simpleHeader :: String -> Maybe String -> Maybe String
             -> SourceURLs -> WikiURLs -> Html
simpleHeader doctitle maybe_contents_url maybe_index_url
  maybe_source_url maybe_wiki_url = 
  divPackageHeader << (
    sectionName << nonEmpty doctitle +++
    unordList (catMaybes [ 
      srcButton maybe_source_url Nothing,
      wikiButton maybe_wiki_url Nothing,
      contentsButton maybe_contents_url,
      indexButton maybe_index_url
      ] ++ stylePickers) ! [theclass "links"]
  )

pageHeader :: String -> Interface -> String
    -> SourceURLs -> WikiURLs
    -> Maybe String -> Maybe String -> Html
pageHeader mdl iface doctitle
           maybe_source_url maybe_wiki_url
           maybe_contents_url maybe_index_url =
  divPackageHeader << (
    sectionName << nonEmpty doctitle +++
    unordList (catMaybes [ 
        srcButton maybe_source_url (Just iface),
        wikiButton maybe_wiki_url (Just $ ifaceMod iface),
        contentsButton maybe_contents_url,
        indexButton maybe_index_url
        ] ++ stylePickers) ! [theclass "links"]
   ) +++
  divModuleHeader << (
    sectionName << mdl +++
    moduleInfo iface
  )

moduleInfo :: Interface -> Html
moduleInfo iface = 
   let
      info = ifaceInfo iface

      doOneEntry :: (String, (HaddockModInfo GHC.Name) -> Maybe String) -> Maybe (String, String)
      doOneEntry (fieldName, field) = field info >>= \a -> return (fieldName, a)

      entries :: [(String, String)]
      entries = mapMaybe doOneEntry [
         ("Portability",hmi_portability),
         ("Stability",hmi_stability),
         ("Maintainer",hmi_maintainer)
         ]
   in
      case entries of
         [] -> noHtml
         _ -> defList entries ! [theclass "info"]

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
   -> [InstalledInterface] -> Bool -> Maybe (Doc GHC.RdrName)
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
        body << (
            simpleHeader doctitle Nothing maybe_index_url
                             maybe_source_url maybe_wiki_url +++
            vanillaTable << (
               ppPrologue doctitle prologue </>
               ppModuleTree doctitle tree) +++
            footer
          )
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, contentsHtmlFile]) (renderToString html)

  -- XXX: think of a better place for this?
  ppHtmlContentsFrame odir doctitle ifaces
  
  -- Generate contents page for Html Help if requested
  case maybe_html_help_format of
    Nothing        -> return ()
    Just "mshelp"  -> ppHHContents  odir doctitle maybe_package tree
    Just "mshelp2" -> ppHH2Contents odir doctitle maybe_package tree
    Just "devhelp" -> return ()
    Just format    -> fail ("The "++format++" format is not implemented")

ppPrologue :: String -> Maybe (Doc GHC.RdrName) -> HtmlTable
ppPrologue _ Nothing = emptyTable
ppPrologue title (Just doc) = 
  (tda [theclass "section1"] << toHtml title) </>
  (tda [theclass "doc"] << (rdrDocToHtml doc))

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
        Nothing -> cell $ td empty
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
    
    id_s = "n." ++ show id_
    
    (sub_tree,id') = genSubTree emptyTable (id_+1) ts
    
    genSubTree :: HtmlTable -> Int -> [ModuleTree] -> (Html,Int)
    genSubTree htmlTable id__ [] = (sub_tree_, id__)
      where
        sub_tree_ = collapsed vanillaTable2 id_s htmlTable
    genSubTree htmlTable id__ (x:xs) = genSubTree (htmlTable </> u) id__' xs      
      where
        (u,id__') = mkNode (s:ss) x (depth+1) id__


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
  writeFile (joinPath [odir, frameIndexHtmlFile]) (renderToString html)

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
        body << (
            simpleHeader doctitle maybe_contents_url Nothing
                         maybe_source_url maybe_wiki_url +++
            vanillaTable << index_html
           )

  createDirectoryIfMissing True odir

  when split_indices $
    mapM_ (do_sub_index index) initialChars

  writeFile (joinPath [odir, indexHtmlFile]) (renderToString html)
  
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
        cell $ td << setTrClass (table ! [identifier "indexlist", cellpadding 0, cellspacing 5] <<
          aboves (map indexElt index))

  -- an arbitrary heuristic:
  -- too large, and a single-page will be slow to load
  -- too small, and we'll have lots of letter-indexes with only one
  --   or two members in them, which seems inefficient or
  --   unnecessarily hard to use.
  split_indices = length index > 150

  setTrClass :: Html -> Html
  setTrClass = id
  -- XHtml is more strict about not allowing you to poke inside a structure
  -- hence this approach won't work for now -- since the whole table is
  -- going away soon, this is just disabled for now.
{-
  setTrClass (Html xs) = Html $ map f xs
      where
          f (HtmlTag name attrs inner)
               | map toUpper name == "TR" = HtmlTag name (theclass "indexrow":attrs) inner
               | otherwise = HtmlTag name attrs (setTrClass inner)
          f x = x
-}      
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
        writeFile (joinPath [odir, subIndexHtmlFile c]) (renderToString html)
    where 
      html = header (documentCharacterEncoding +++
                thetitle (toHtml (doctitle ++ " (Index)")) +++
                styleSheet) +++
             body << (
                simpleHeader doctitle maybe_contents_url Nothing
                             maybe_source_url maybe_wiki_url +++
                vanillaTable << (
                          indexInitialLetterLinks </>
                tda [theclass "section1"] << 
                toHtml ("Index (" ++ c:")") </>
                td << setTrClass (table ! [identifier "indexlist", cellpadding 0, cellspacing 5] <<
                  aboves (map indexElt index_part) )
               ))

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
                     -- NB: Within XHTML, the content of script tags needs to be
                     -- a CDATA section. Will break if the generated name could 
                     -- have "]]>" in it!
                     << primHtml (
                      "//<![CDATA[\nwindow.onload = function () {setSynopsis(\"mini_"
                         ++ moduleHtmlFile mdl ++ "\")};\n//]]>\n")
                )
               ) +++
        body << (
          pageHeader mdl_str iface doctitle
                maybe_source_url maybe_wiki_url
                maybe_contents_url maybe_index_url +++
          ifaceToHtml maybe_source_url maybe_wiki_url iface unicode +++
          footer)
         
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, moduleHtmlFile mdl]) (renderToString html)
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
  writeFile (joinPath [odir, "mini_" ++ moduleHtmlFile mdl]) (renderToString html)

ifaceToHtml :: SourceURLs -> WikiURLs -> Interface -> Bool -> Html
ifaceToHtml maybe_source_url maybe_wiki_url iface unicode
  = ppModuleContents exports +++
    description +++
    synopsis +++
    divInterface (maybe_doc_hdr +++ bdy)
  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)

    -- todo: if something has only sub-docs, or fn-args-docs, should
    -- it be measured here and thus prevent omitting the synopsis?
    has_doc (ExportDecl _ doc _ _) = isJust (fst doc)
    has_doc (ExportNoDecl _ _) = False
    has_doc (ExportModule _) = False
    has_doc _ = True

    no_doc_at_all = not (any has_doc exports)

    description
          = case ifaceRnDoc iface of
              Nothing -> noHtml
              Just doc -> divDescription $
                            sectionName << "Description" +++ docToHtml doc

        -- omit the synopsis if there are no documentation annotations at all
    synopsis
      | no_doc_at_all = noHtml
      | otherwise
      = divSynposis $
            sectionName << "Synopsis" +++
            shortDeclList (
                mapMaybe (processExport True linksInfo unicode) exports
            )

        -- if the documentation doesn't begin with a section header, then
        -- add one ("Documentation").
    maybe_doc_hdr
      = case exports of            
          [] -> noHtml
          ExportGroup _ _ _ : _ -> noHtml
          _ -> h1 << "Documentation"

    bdy =
      foldr (+++) noHtml $
        mapMaybe (processExport False linksInfo unicode) exports
          
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
      | Nothing <- ps    -> keyword "data" <+> ppTyClBinderWithVarsMini mdl d
      | Just _ <- ps     -> keyword "data" <+> keyword "instance"
                                           <+> ppTyClBinderWithVarsMini mdl d
    TyClD d@(TySynonym{tcdTyPats = ps})
      | Nothing <- ps    -> keyword "type" <+> ppTyClBinderWithVarsMini mdl d
      | Just _ <- ps     -> keyword "type" <+> keyword "instance"
                                           <+> ppTyClBinderWithVarsMini mdl d
    TyClD d@(ClassDecl {}) ->
                            keyword "class" <+> ppTyClBinderWithVarsMini mdl d
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

ppModuleContents :: [ExportItem DocName] -> Html
ppModuleContents exports
  | null sections = noHtml
  | otherwise     = contentsDiv
 where
  contentsDiv = divTableOfContents << (
    sectionName << "Contents" +++
    unordList sections)
    
  (sections, _leftovers{-should be []-}) = process 0 exports

  process :: Int -> [ExportItem DocName] -> ([Html],[ExportItem DocName])
  process _ [] = ([], [])
  process n items@(ExportGroup lev id0 doc : rest) 
    | lev <= n  = ( [], items )
    | otherwise = ( html:secs, rest2 )
    where
        html = linkedAnchor id0 << docToHtml doc +++ mk_subsections ssecs
        (ssecs, rest1) = process lev rest
        (secs,  rest2) = process n   rest1
  process n (_ : rest) = process n rest

  mk_subsections [] = noHtml
  mk_subsections ss = unordList ss

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

processExport :: Bool -> LinksInfo -> Bool -> (ExportItem DocName) -> Maybe Html
processExport summary _ _ (ExportGroup lev id0 doc)
  = nothingIf summary $ groupTag lev << namedAnchor id0 << docToHtml doc
processExport summary links unicode (ExportDecl decl doc subdocs insts)
  = processDecl summary $ ppDecl summary links decl doc insts subdocs unicode
processExport summary _ _ (ExportNoDecl y [])
  = processDeclOneLiner summary $ ppDocName y
processExport summary _ _ (ExportNoDecl y subs)
  = processDeclOneLiner summary $ ppDocName y +++ parenList (map ppDocName subs)
processExport summary _ _ (ExportDoc doc)
  = nothingIf summary $ docToHtml doc
processExport summary _ _ (ExportModule mdl)
  = processDeclOneLiner summary $ toHtml "module" <+> ppModule mdl ""

nothingIf :: Bool -> a -> Maybe a
nothingIf True _ = Nothing
nothingIf False a = Just a

processDecl :: Bool -> Html -> Maybe Html
processDecl True = Just
processDecl False = Just . divTopDecl

processDeclOneLiner :: Bool -> Html -> Maybe Html
processDeclOneLiner True = Just
processDeclOneLiner False = Just . divTopDecl . declElem

groupTag :: Int -> Html -> Html
groupTag lev
  | lev == 1  = h1
  | lev == 2  = h2
  | lev == 3  = h3
  | otherwise = h4



