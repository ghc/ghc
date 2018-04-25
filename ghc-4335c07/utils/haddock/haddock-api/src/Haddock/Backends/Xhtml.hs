-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html
-- Copyright   :  (c) Simon Marlow      2003-2006,
--                    David Waern       2006-2009,
--                    Mark Lentczner    2010,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE CPP, NamedFieldPuns #-}
module Haddock.Backends.Xhtml (
  ppHtml, copyHtmlBits,
  ppHtmlIndex, ppHtmlContents,
) where


import Prelude hiding (div)

import Haddock.Backends.Xhtml.Decl
import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Layout
import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Themes
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils
import Haddock.ModuleTree
import Haddock.Types
import Haddock.Version
import Haddock.Utils
import Haddock.Utils.Json
import Text.XHtml hiding ( name, title, p, quote )
import Haddock.GhcUtils

import Control.Monad         ( when, unless )
import Data.Char             ( toUpper, isSpace )
import Data.List             ( sortBy, isPrefixOf, intercalate, intersperse )
import Data.Maybe
import System.FilePath hiding ( (</>) )
import System.Directory
import Data.Map              ( Map )
import qualified Data.Map as Map hiding ( Map )
import qualified Data.Set as Set hiding ( Set )
import Data.Ord              ( comparing )

import DynFlags (Language(..))
import GHC hiding ( NoLink, moduleInfo,LexicalFixity(..) )
import Name

--------------------------------------------------------------------------------
-- * Generating HTML documentation
--------------------------------------------------------------------------------

ppHtml :: DynFlags
       -> String                       -- ^ Title
       -> Maybe String                 -- ^ Package
       -> [Interface]
       -> [InstalledInterface]         -- ^ Reexported interfaces
       -> FilePath                     -- ^ Destination directory
       -> Maybe (MDoc GHC.RdrName)     -- ^ Prologue text, maybe
       -> Themes                       -- ^ Themes
       -> Maybe String                 -- ^ The mathjax URL (--mathjax)
       -> SourceURLs                   -- ^ The source URL (--source)
       -> WikiURLs                     -- ^ The wiki URL (--wiki)
       -> Maybe String                 -- ^ The contents URL (--use-contents)
       -> Maybe String                 -- ^ The index URL (--use-index)
       -> Bool                         -- ^ Whether to use unicode in output (--use-unicode)
       -> QualOption                   -- ^ How to qualify names
       -> Bool                         -- ^ Output pretty html (newlines and indenting)
       -> Bool                         -- ^ Also write Quickjump index
       -> IO ()

ppHtml dflags doctitle maybe_package ifaces reexported_ifaces odir prologue
        themes maybe_mathjax_url maybe_source_url maybe_wiki_url
        maybe_contents_url maybe_index_url unicode
        qual debug withQuickjump =  do
  let
    visible_ifaces = filter visible ifaces
    visible i = OptHide `notElem` ifaceOptions i

  when (isNothing maybe_contents_url) $
    ppHtmlContents dflags odir doctitle maybe_package
        themes maybe_mathjax_url maybe_index_url maybe_source_url maybe_wiki_url
        (map toInstalledIface visible_ifaces ++ reexported_ifaces)
        False -- we don't want to display the packages in a single-package contents
        prologue debug (makeContentsQual qual)

  when (isNothing maybe_index_url) $ do
    ppHtmlIndex odir doctitle maybe_package
      themes maybe_mathjax_url maybe_contents_url maybe_source_url maybe_wiki_url
      (map toInstalledIface visible_ifaces ++ reexported_ifaces) debug

    when withQuickjump $
      ppJsonIndex odir maybe_source_url maybe_wiki_url unicode qual
        visible_ifaces

  mapM_ (ppHtmlModule odir doctitle themes
           maybe_mathjax_url maybe_source_url maybe_wiki_url
           maybe_contents_url maybe_index_url unicode qual debug) visible_ifaces


copyHtmlBits :: FilePath -> FilePath -> Themes -> Bool -> IO ()
copyHtmlBits odir libdir themes withQuickjump = do
  let
    libhtmldir = joinPath [libdir, "html"]
    copyCssFile f = copyFile f (combine odir (takeFileName f))
    copyLibFile f = copyFile (joinPath [libhtmldir, f]) (joinPath [odir, f])
  mapM_ copyCssFile (cssFiles themes)
  copyLibFile haddockJsFile
  copyCssFile (joinPath [libhtmldir, quickJumpCssFile])
  when withQuickjump (copyLibFile jsQuickJumpFile)
  return ()


headHtml :: String -> Themes -> Maybe String -> Html
headHtml docTitle themes mathjax_url =
  header << [
    meta ! [httpequiv "Content-Type", content "text/html; charset=UTF-8"],
    thetitle << docTitle,
    styleSheet themes,
    thelink ! [ rel "stylesheet", thetype "text/css", href quickJumpCssFile] << noHtml,
    script ! [src haddockJsFile, emptyAttr "async", thetype "text/javascript"] << noHtml,
    script ! [src mjUrl, thetype "text/javascript"] << noHtml
    ]
  where
    mjUrl = maybe "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML" id mathjax_url


srcButton :: SourceURLs -> Maybe Interface -> Maybe Html
srcButton (Just src_base_url, _, _, _) Nothing =
  Just (anchor ! [href src_base_url] << "Source")
srcButton (_, Just src_module_url, _, _) (Just iface) =
  let url = spliceURL (Just $ ifaceOrigFilename iface)
                      (Just $ ifaceMod iface) Nothing Nothing src_module_url
   in Just (anchor ! [href url] << "Source")
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
  where url = fromMaybe contentsHtmlFile maybe_contents_url


indexButton :: Maybe String -> Maybe Html
indexButton maybe_index_url
  = Just (anchor ! [href url] << "Index")
  where url = fromMaybe indexHtmlFile maybe_index_url


bodyHtml :: String -> Maybe Interface
    -> SourceURLs -> WikiURLs
    -> Maybe String -> Maybe String
    -> Html -> Html
bodyHtml doctitle iface
           maybe_source_url maybe_wiki_url
           maybe_contents_url maybe_index_url
           pageContent =
  body << [
    divPackageHeader << [
      unordList (catMaybes [
        srcButton maybe_source_url iface,
        wikiButton maybe_wiki_url (ifaceMod <$> iface),
        contentsButton maybe_contents_url,
        indexButton maybe_index_url])
            ! [theclass "links", identifier "page-menu"],
      nonEmptySectionName << doctitle
      ],
    divContent << pageContent,
    divFooter << paragraph << (
      "Produced by " +++
      (anchor ! [href projectUrl] << toHtml projectName) +++
      (" version " ++ projectVersion)
      )
    ]

moduleInfo :: Interface -> Html
moduleInfo iface =
   let
      info = ifaceInfo iface

      doOneEntry :: (String, HaddockModInfo GHC.Name -> Maybe String) -> Maybe HtmlTable
      doOneEntry (fieldName, field) =
        field info >>= \a -> return (th << fieldName <-> td << a)

      entries :: [HtmlTable]
      entries = maybeToList copyrightsTable ++ mapMaybe doOneEntry [
          ("License",hmi_license),
          ("Maintainer",hmi_maintainer),
          ("Stability",hmi_stability),
          ("Portability",hmi_portability),
          ("Safe Haskell",hmi_safety),
          ("Language", lg)
          ] ++ extsForm
        where
          lg inf = case hmi_language inf of
            Nothing -> Nothing
            Just Haskell98 -> Just "Haskell98"
            Just Haskell2010 -> Just "Haskell2010"

          multilineRow :: String -> [String] -> HtmlTable
          multilineRow title xs = (th ! [valign "top"]) << title <-> td << (toLines xs)
            where toLines = mconcat . intersperse br . map toHtml

          copyrightsTable :: Maybe HtmlTable
          copyrightsTable = fmap (multilineRow "Copyright" . split) (hmi_copyright info)
            where split = map (trim . filter (/= ',')) . lines

          extsForm
            | OptShowExtensions `elem` ifaceOptions iface =
              let fs = map (dropOpt . show) (hmi_extensions info)
              in case map stringToHtml fs of
                [] -> []
                [x] -> extField x -- don't use a list for a single extension
                xs -> extField $ unordList xs ! [theclass "extension-list"]
            | otherwise = []
            where
              extField x = return $ th << "Extensions" <-> td << x
              dropOpt x = if "Opt_" `isPrefixOf` x then drop 4 x else x
   in
      case entries of
         [] -> noHtml
         _ -> table ! [theclass "info"] << aboves entries


--------------------------------------------------------------------------------
-- * Generate the module contents
--------------------------------------------------------------------------------


ppHtmlContents
   :: DynFlags
   -> FilePath
   -> String
   -> Maybe String
   -> Themes
   -> Maybe String
   -> Maybe String
   -> SourceURLs
   -> WikiURLs
   -> [InstalledInterface] -> Bool -> Maybe (MDoc GHC.RdrName)
   -> Bool
   -> Qualification  -- ^ How to qualify names
   -> IO ()
ppHtmlContents dflags odir doctitle _maybe_package
  themes mathjax_url maybe_index_url
  maybe_source_url maybe_wiki_url ifaces showPkgs prologue debug qual = do
  let tree = mkModuleTree dflags showPkgs
         [(instMod iface, toInstalledDescription iface)
         | iface <- ifaces
         , not (instIsSig iface)]
      sig_tree = mkModuleTree dflags showPkgs
         [(instMod iface, toInstalledDescription iface)
         | iface <- ifaces
         , instIsSig iface]
      html =
        headHtml doctitle themes mathjax_url +++
        bodyHtml doctitle Nothing
          maybe_source_url maybe_wiki_url
          Nothing maybe_index_url << [
            ppPrologue qual doctitle prologue,
            ppSignatureTree qual sig_tree,
            ppModuleTree qual tree
          ]
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, contentsHtmlFile]) (renderToString debug html)


ppPrologue :: Qualification -> String -> Maybe (MDoc GHC.RdrName) -> Html
ppPrologue _ _ Nothing = noHtml
ppPrologue qual title (Just doc) =
  divDescription << (h1 << title +++ docElement thediv (rdrDocToHtml qual doc))


ppSignatureTree :: Qualification -> [ModuleTree] -> Html
ppSignatureTree qual ts =
  divModuleList << (sectionName << "Signatures" +++ mkNodeList qual [] "n" ts)


ppModuleTree :: Qualification -> [ModuleTree] -> Html
ppModuleTree _ [] = mempty
ppModuleTree qual ts =
  divModuleList << (sectionName << "Modules" +++ mkNodeList qual [] "n" ts)


mkNodeList :: Qualification -> [String] -> String -> [ModuleTree] -> Html
mkNodeList qual ss p ts = case ts of
  [] -> noHtml
  _ -> unordList (zipWith (mkNode qual ss) ps ts)
  where
    ps = [ p ++ '.' : show i | i <- [(1::Int)..]]


mkNode :: Qualification -> [String] -> String -> ModuleTree -> Html
mkNode qual ss p (Node s leaf _pkg srcPkg short ts) =
  htmlModule <+> shortDescr +++ htmlPkg +++ subtree
  where
    modAttrs = case (ts, leaf) of
      (_:_, Nothing) -> collapseControl p "module"
      (_,   _    ) -> [theclass "module"]

    cBtn = case (ts, leaf) of
      (_:_, Just _) -> thespan ! collapseControl p "" << spaceHtml
      (_,   _   ) -> noHtml
      -- We only need an explicit collapser button when the module name
      -- is also a leaf, and so is a link to a module page. Indeed, the
      -- spaceHtml is a minor hack and does upset the layout a fraction.

    htmlModule = thespan ! modAttrs << (cBtn +++
      case leaf of
        Just m -> ppModule m
        Nothing -> toHtml s
      )

    shortDescr = maybe noHtml (origDocToHtml qual) short
    htmlPkg = maybe noHtml (thespan ! [theclass "package"] <<) srcPkg

    subtree =
      if null ts then noHtml else
      collapseDetails p DetailsOpen (
        thesummary ! [ theclass "hide-when-js-enabled" ] << "Submodules" +++
        mkNodeList qual (s:ss) p ts
      )



--------------------------------------------------------------------------------
-- * Generate the index
--------------------------------------------------------------------------------

ppJsonIndex :: FilePath
           -> SourceURLs                   -- ^ The source URL (--source)
           -> WikiURLs                     -- ^ The wiki URL (--wiki)
           -> Bool
           -> QualOption
           -> [Interface]
           -> IO ()
ppJsonIndex odir maybe_source_url maybe_wiki_url unicode qual_opt ifaces = do
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, indexJsonFile])
            (encodeToString modules)

  where
    modules :: Value
    modules = Array (concatMap goInterface ifaces)

    goInterface :: Interface -> [Value]
    goInterface iface =
        concatMap (goExport mdl qual) (ifaceRnExportItems iface)
      where
        aliases = ifaceModuleAliases iface
        qual    = makeModuleQual qual_opt aliases mdl
        mdl     = ifaceMod iface

    goExport :: Module -> Qualification -> ExportItem DocNameI -> [Value]
    goExport mdl qual item
      | Just item_html <- processExport True links_info unicode qual item
      = [ Object
            [ "display_html" .= String (showHtmlFragment item_html)
            , "name"         .= String (intercalate " " (map nameString names))
            , "module"       .= String (moduleString mdl)
            , "link"         .= String (fromMaybe "" (listToMaybe (map (nameLink mdl) names)))
            ]
        ]
      | otherwise = []
      where
        names = exportName item ++ exportSubs item

    exportSubs :: ExportItem name -> [IdP name]
    exportSubs ExportDecl { expItemSubDocs } = map fst expItemSubDocs
    exportSubs _ = []

    exportName :: ExportItem name -> [IdP name]
    exportName ExportDecl { expItemDecl } = getMainDeclBinder (unLoc expItemDecl)
    exportName ExportNoDecl { expItemName } = [expItemName]
    exportName _ = []

    nameString :: NamedThing name => name -> String
    nameString = occNameString . nameOccName . getName

    nameLink :: NamedThing name => Module -> name -> String
    nameLink mdl = moduleNameUrl' (moduleName mdl) . nameOccName . getName

    links_info = (maybe_source_url, maybe_wiki_url)

ppHtmlIndex :: FilePath
            -> String
            -> Maybe String
            -> Themes
            -> Maybe String
            -> Maybe String
            -> SourceURLs
            -> WikiURLs
            -> [InstalledInterface]
            -> Bool
            -> IO ()
ppHtmlIndex odir doctitle _maybe_package themes
  maybe_mathjax_url maybe_contents_url maybe_source_url maybe_wiki_url ifaces debug = do
  let html = indexPage split_indices Nothing
              (if split_indices then [] else index)

  createDirectoryIfMissing True odir

  when split_indices $ do
    mapM_ (do_sub_index index) initialChars
    -- Let's add a single large index as well for those who don't know exactly what they're looking for:
    let mergedhtml = indexPage False Nothing index
    writeFile (joinPath [odir, subIndexHtmlFile merged_name]) (renderToString debug mergedhtml)

  writeFile (joinPath [odir, indexHtmlFile]) (renderToString debug html)

  where
    indexPage showLetters ch items =
      headHtml (doctitle ++ " (" ++ indexName ch ++ ")") themes maybe_mathjax_url +++
      bodyHtml doctitle Nothing
        maybe_source_url maybe_wiki_url
        maybe_contents_url Nothing << [
          if showLetters then indexInitialLetterLinks else noHtml,
          if null items then noHtml else
            divIndex << [sectionName << indexName ch, buildIndex items]
          ]

    indexName ch = "Index" ++ maybe "" (\c -> " - " ++ [c]) ch
    merged_name = "All"

    buildIndex items = table << aboves (map indexElt items)

    -- an arbitrary heuristic:
    -- too large, and a single-page will be slow to load
    -- too small, and we'll have lots of letter-indexes with only one
    --   or two members in them, which seems inefficient or
    --   unnecessarily hard to use.
    split_indices = length index > 150

    indexInitialLetterLinks =
      divAlphabet <<
         unordList (map (\str -> anchor ! [href (subIndexHtmlFile str)] << str) $
                        [ [c] | c <- initialChars
                              , any ((==c) . toUpper . head . fst) index ] ++
                        [merged_name])

    -- todo: what about names/operators that start with Unicode
    -- characters?
    -- Exports beginning with '_' can be listed near the end,
    -- presumably they're not as important... but would be listed
    -- with non-split index!
    initialChars = [ 'A'..'Z' ] ++ ":!#$%&*+./<=>?@\\^|-~" ++ "_"

    do_sub_index this_ix c
      = unless (null index_part) $
          writeFile (joinPath [odir, subIndexHtmlFile [c]]) (renderToString debug html)
      where
        html = indexPage True (Just c) index_part
        index_part = [(n,stuff) | (n,stuff) <- this_ix, toUpper (head n) == c]


    index :: [(String, Map GHC.Name [(Module,Bool)])]
    index = sortBy cmp (Map.toAscList full_index)
      where cmp (n1,_) (n2,_) = comparing (map toUpper) n1 n2

    -- for each name (a plain string), we have a number of original HsNames that
    -- it can refer to, and for each of those we have a list of modules
    -- that export that entity.  Each of the modules exports the entity
    -- in a visible or invisible way (hence the Bool).
    full_index :: Map String (Map GHC.Name [(Module,Bool)])
    full_index = Map.fromListWith (flip (Map.unionWith (++)))
                 (concatMap getIfaceIndex ifaces)

    getIfaceIndex iface =
      [ (getOccString name
         , Map.fromList [(name, [(mdl, name `Set.member` visible)])])
         | name <- instExports iface ]
      where
        mdl = instMod iface
        visible = Set.fromList (instVisibleExports iface)

    indexElt :: (String, Map GHC.Name [(Module,Bool)]) -> HtmlTable
    indexElt (str, entities) =
       case Map.toAscList entities of
          [(nm,entries)] ->
              td ! [ theclass "src" ] << toHtml str <->
                          indexLinks nm entries
          many_entities ->
              td ! [ theclass "src" ] << toHtml str <-> td << spaceHtml </>
                  aboves (zipWith (curry doAnnotatedEntity) [1..] many_entities)

    doAnnotatedEntity :: (Integer, (Name, [(Module, Bool)])) -> HtmlTable
    doAnnotatedEntity (j,(nm,entries))
          = td ! [ theclass "alt" ] <<
                  toHtml (show j) <+> parens (ppAnnot (nameOccName nm)) <->
                   indexLinks nm entries

    ppAnnot n | not (isValOcc n) = toHtml "Type/Class"
              | isDataOcc n      = toHtml "Data Constructor"
              | otherwise        = toHtml "Function"

    indexLinks nm entries =
       td ! [ theclass "module" ] <<
          hsep (punctuate comma
          [ if visible then
               linkId mdl (Just nm) << toHtml (moduleString mdl)
            else
               toHtml (moduleString mdl)
          | (mdl, visible) <- entries ])


--------------------------------------------------------------------------------
-- * Generate the HTML page for a module
--------------------------------------------------------------------------------


ppHtmlModule
        :: FilePath -> String -> Themes
        -> Maybe String -> SourceURLs -> WikiURLs
        -> Maybe String -> Maybe String -> Bool -> QualOption
        -> Bool -> Interface -> IO ()
ppHtmlModule odir doctitle themes
  maybe_mathjax_url maybe_source_url maybe_wiki_url
  maybe_contents_url maybe_index_url unicode qual debug iface = do
  let
      mdl = ifaceMod iface
      aliases = ifaceModuleAliases iface
      mdl_str = moduleString mdl
      mdl_str_annot = mdl_str ++ if ifaceIsSig iface
                                    then " (signature)"
                                    else ""
      mdl_str_linked
        | ifaceIsSig iface
        = mdl_str +++ " (signature" +++
                       sup << ("[" +++ anchor ! [href signatureDocURL] << "?" +++ "]" ) +++
                       ")"
        | otherwise
        = toHtml mdl_str
      real_qual = makeModuleQual qual aliases mdl
      html =
        headHtml mdl_str_annot themes maybe_mathjax_url +++
        bodyHtml doctitle (Just iface)
          maybe_source_url maybe_wiki_url
          maybe_contents_url maybe_index_url << [
            divModuleHeader << (moduleInfo iface +++ (sectionName << mdl_str_linked)),
            ifaceToHtml maybe_source_url maybe_wiki_url iface unicode real_qual
          ]

  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, moduleHtmlFile mdl]) (renderToString debug html)

signatureDocURL :: String
signatureDocURL = "https://wiki.haskell.org/Module_signature"


ifaceToHtml :: SourceURLs -> WikiURLs -> Interface -> Bool -> Qualification -> Html
ifaceToHtml maybe_source_url maybe_wiki_url iface unicode qual
  = ppModuleContents qual exports (not . null $ ifaceRnOrphanInstances iface) +++
    description +++
    synopsis +++
    divInterface (maybe_doc_hdr +++ bdy +++ orphans)
  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)

    -- todo: if something has only sub-docs, or fn-args-docs, should
    -- it be measured here and thus prevent omitting the synopsis?
    has_doc ExportDecl { expItemMbDoc = (Documentation mDoc mWarning, _) } = isJust mDoc || isJust mWarning
    has_doc (ExportNoDecl _ _) = False
    has_doc (ExportModule _) = False
    has_doc _ = True

    no_doc_at_all = not (any has_doc exports)

    description | isNoHtml doc = doc
                | otherwise    = divDescription $ sectionName << "Description" +++ doc
                where doc = docSection Nothing qual (ifaceRnDoc iface)

        -- omit the synopsis if there are no documentation annotations at all
    synopsis
      | no_doc_at_all = noHtml
      | otherwise
      = divSynopsis $
            collapseDetails "syn" DetailsClosed (
              thesummary << "Synopsis" +++
              shortDeclList (
                  mapMaybe (processExport True linksInfo unicode qual) exports
              ) ! collapseToggle "syn" ""
            )

        -- if the documentation doesn't begin with a section header, then
        -- add one ("Documentation").
    maybe_doc_hdr
      = case exports of
          [] -> noHtml
          ExportGroup {} : _ -> noHtml
          _ -> h1 << "Documentation"

    bdy =
      foldr (+++) noHtml $
        mapMaybe (processExport False linksInfo unicode qual) exports

    orphans =
      ppOrphanInstances linksInfo (ifaceRnOrphanInstances iface) False unicode qual

    linksInfo = (maybe_source_url, maybe_wiki_url)


ppModuleContents :: Qualification
                 -> [ExportItem DocNameI]
                 -> Bool -- ^ Orphans sections
                 -> Html
ppModuleContents qual exports orphan
  | null sections && not orphan  = noHtml
  | otherwise                    = contentsDiv
 where
  contentsDiv = divTableOfContents << (
    sectionName << "Contents" +++
    unordList (sections ++ orphanSection))

  (sections, _leftovers{-should be []-}) = process 0 exports
  orphanSection
    | orphan =  [ linkedAnchor "section.orphans" << "Orphan instances" ]
    | otherwise = []

  process :: Int -> [ExportItem DocNameI] -> ([Html],[ExportItem DocNameI])
  process _ [] = ([], [])
  process n items@(ExportGroup lev id0 doc : rest)
    | lev <= n  = ( [], items )
    | otherwise = ( html:secs, rest2 )
    where
      html = linkedAnchor (groupId id0)
             << docToHtmlNoAnchors (Just id0) qual (mkMeta doc) +++ mk_subsections ssecs
      (ssecs, rest1) = process lev rest
      (secs,  rest2) = process n   rest1
  process n (_ : rest) = process n rest

  mk_subsections [] = noHtml
  mk_subsections ss = unordList ss

-- we need to assign a unique id to each section heading so we can hyperlink
-- them from the contents:
numberSectionHeadings :: [ExportItem DocNameI] -> [ExportItem DocNameI]
numberSectionHeadings = go 1
  where go :: Int -> [ExportItem DocNameI] -> [ExportItem DocNameI]
        go _ [] = []
        go n (ExportGroup lev _ doc : es)
          = ExportGroup lev (show n) doc : go (n+1) es
        go n (other:es)
          = other : go n es


processExport :: Bool -> LinksInfo -> Bool -> Qualification
              -> ExportItem DocNameI -> Maybe Html
processExport _ _ _ _ ExportDecl { expItemDecl = L _ (InstD _) } = Nothing -- Hide empty instances
processExport summary _ _ qual (ExportGroup lev id0 doc)
  = nothingIf summary $ groupHeading lev id0 << docToHtml (Just id0) qual (mkMeta doc)
processExport summary links unicode qual (ExportDecl decl pats doc subdocs insts fixities splice)
  = processDecl summary $ ppDecl summary links decl pats doc insts fixities subdocs splice unicode qual
processExport summary _ _ qual (ExportNoDecl y [])
  = processDeclOneLiner summary $ ppDocName qual Prefix True y
processExport summary _ _ qual (ExportNoDecl y subs)
  = processDeclOneLiner summary $
      ppDocName qual Prefix True y
      +++ parenList (map (ppDocName qual Prefix True) subs)
processExport summary _ _ qual (ExportDoc doc)
  = nothingIf summary $ docSection_ Nothing qual doc
processExport summary _ _ _ (ExportModule mdl)
  = processDeclOneLiner summary $ toHtml "module" <+> ppModule mdl


nothingIf :: Bool -> a -> Maybe a
nothingIf True _ = Nothing
nothingIf False a = Just a


processDecl :: Bool -> Html -> Maybe Html
processDecl True = Just
processDecl False = Just . divTopDecl

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

processDeclOneLiner :: Bool -> Html -> Maybe Html
processDeclOneLiner True = Just
processDeclOneLiner False = Just . divTopDecl . declElem

groupHeading :: Int -> String -> Html -> Html
groupHeading lev id0 = groupTag lev ! [identifier (groupId id0)]

groupTag :: Int -> Html -> Html
groupTag lev
  | lev == 1  = h1
  | lev == 2  = h2
  | lev == 3  = h3
  | otherwise = h4
