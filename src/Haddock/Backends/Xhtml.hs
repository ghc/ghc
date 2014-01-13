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
{-# LANGUAGE CPP #-}
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
import Text.XHtml hiding ( name, title, p, quote )
import Haddock.GhcUtils

import Control.Monad         ( when, unless )
#if !MIN_VERSION_base(4,7,0)
import Control.Monad.Instances ( ) -- for Functor Either a
#endif
import Data.Char             ( toUpper )
import Data.Functor          ( (<$>) )
import Data.List             ( sortBy, groupBy, intercalate, isPrefixOf )
import Data.Maybe
import System.FilePath hiding ( (</>) )
import System.Directory
import Data.Map              ( Map )
import qualified Data.Map as Map hiding ( Map )
import qualified Data.Set as Set hiding ( Set )
import Data.Function
import Data.Ord              ( comparing )

import DynFlags (Language(..))
import GHC hiding ( NoLink, moduleInfo )
import Name
import Module

--------------------------------------------------------------------------------
-- * Generating HTML documentation
--------------------------------------------------------------------------------


ppHtml :: String
       -> Maybe String                 -- ^ Package
       -> [Interface]
       -> FilePath                     -- ^ Destination directory
       -> Maybe (Doc GHC.RdrName)      -- ^ Prologue text, maybe
       -> Themes                       -- ^ Themes
       -> SourceURLs                   -- ^ The source URL (--source)
       -> WikiURLs                     -- ^ The wiki URL (--wiki)
       -> Maybe String                 -- ^ The contents URL (--use-contents)
       -> Maybe String                 -- ^ The index URL (--use-index)
       -> Bool                         -- ^ Whether to use unicode in output (--use-unicode)
       -> QualOption                   -- ^ How to qualify names
       -> Bool                         -- ^ Output pretty html (newlines and indenting)
       -> IO ()

ppHtml doctitle maybe_package ifaces odir prologue
        themes maybe_source_url maybe_wiki_url
        maybe_contents_url maybe_index_url unicode
        qual debug =  do
  let
    visible_ifaces = filter visible ifaces
    visible i = OptHide `notElem` ifaceOptions i

  when (isNothing maybe_contents_url) $
    ppHtmlContents odir doctitle maybe_package
        themes maybe_index_url maybe_source_url maybe_wiki_url
        (map toInstalledIface visible_ifaces)
        False -- we don't want to display the packages in a single-package contents
        prologue debug (makeContentsQual qual)

  when (isNothing maybe_index_url) $
    ppHtmlIndex odir doctitle maybe_package
      themes maybe_contents_url maybe_source_url maybe_wiki_url
      (map toInstalledIface visible_ifaces) debug

  mapM_ (ppHtmlModule odir doctitle themes
           maybe_source_url maybe_wiki_url
           maybe_contents_url maybe_index_url unicode qual debug) visible_ifaces


copyHtmlBits :: FilePath -> FilePath -> Themes -> IO ()
copyHtmlBits odir libdir themes = do
  let
    libhtmldir = joinPath [libdir, "html"]
    copyCssFile f = copyFile f (combine odir (takeFileName f))
    copyLibFile f = copyFile (joinPath [libhtmldir, f]) (joinPath [odir, f])
  mapM_ copyCssFile (cssFiles themes)
  mapM_ copyLibFile [ jsFile, framesFile ]


headHtml :: String -> Maybe String -> Themes -> Html
headHtml docTitle miniPage themes =
  header << [
    meta ! [httpequiv "Content-Type", content "text/html; charset=UTF-8"],
    thetitle << docTitle,
    styleSheet themes,
    script ! [src jsFile, thetype "text/javascript"] << noHtml,
    script ! [thetype "text/javascript"]
        -- NB: Within XHTML, the content of script tags needs to be
        -- a <![CDATA[ section. Will break if the miniPage name could
        -- have "]]>" in it!
      << primHtml (
          "//<![CDATA[\nwindow.onload = function () {pageLoad();"
          ++ setSynopsis ++ "};\n//]]>\n")
    ]
  where
    setSynopsis = maybe "" (\p -> "setSynopsis(\"" ++ p ++ "\");") miniPage


srcButton :: SourceURLs -> Maybe Interface -> Maybe Html
srcButton (Just src_base_url, _, _) Nothing =
  Just (anchor ! [href src_base_url] << "Source")
srcButton (_, Just src_module_url, _) (Just iface) =
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
      entries = mapMaybe doOneEntry [
          ("Copyright",hmi_copyright),
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
   :: FilePath
   -> String
   -> Maybe String
   -> Themes
   -> Maybe String
   -> SourceURLs
   -> WikiURLs
   -> [InstalledInterface] -> Bool -> Maybe (Doc GHC.RdrName)
   -> Bool
   -> Qualification  -- ^ How to qualify names
   -> IO ()
ppHtmlContents odir doctitle _maybe_package
  themes maybe_index_url
  maybe_source_url maybe_wiki_url ifaces showPkgs prologue debug qual = do
  let tree = mkModuleTree showPkgs
         [(instMod iface, toInstalledDescription iface) | iface <- ifaces]
      html =
        headHtml doctitle Nothing themes +++
        bodyHtml doctitle Nothing
          maybe_source_url maybe_wiki_url
          Nothing maybe_index_url << [
            ppPrologue qual doctitle prologue,
            ppModuleTree qual tree
          ]
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, contentsHtmlFile]) (renderToString debug html)

  -- XXX: think of a better place for this?
  ppHtmlContentsFrame odir doctitle themes ifaces debug


ppPrologue :: Qualification -> String -> Maybe (Doc GHC.RdrName) -> Html
ppPrologue _ _ Nothing = noHtml
ppPrologue qual title (Just doc) =
  divDescription << (h1 << title +++ docElement thediv (rdrDocToHtml qual doc))


ppModuleTree :: Qualification -> [ModuleTree] -> Html
ppModuleTree qual ts =
  divModuleList << (sectionName << "Modules" +++ mkNodeList qual [] "n" ts)


mkNodeList :: Qualification -> [String] -> String -> [ModuleTree] -> Html
mkNodeList qual ss p ts = case ts of
  [] -> noHtml
  _ -> unordList (zipWith (mkNode qual ss) ps ts)
  where
    ps = [ p ++ '.' : show i | i <- [(1::Int)..]]


mkNode :: Qualification -> [String] -> String -> ModuleTree -> Html
mkNode qual ss p (Node s leaf pkg short ts) =
  htmlModule +++ shortDescr +++ htmlPkg +++ subtree
  where
    modAttrs = case (ts, leaf) of
      (_:_, False) -> collapseControl p True "module"
      (_,   _    ) -> [theclass "module"]

    cBtn = case (ts, leaf) of
      (_:_, True) -> thespan ! collapseControl p True "" << spaceHtml
      (_,   _   ) -> noHtml
      -- We only need an explicit collapser button when the module name
      -- is also a leaf, and so is a link to a module page. Indeed, the
      -- spaceHtml is a minor hack and does upset the layout a fraction.

    htmlModule = thespan ! modAttrs << (cBtn +++
      if leaf
        then ppModule (mkModule (stringToPackageId (fromMaybe "" pkg))
                                       (mkModuleName mdl))
        else toHtml s
      )

    mdl = intercalate "." (reverse (s:ss))

    shortDescr = maybe noHtml (origDocToHtml qual) short
    htmlPkg = maybe noHtml (thespan ! [theclass "package"] <<) pkg

    subtree = mkNodeList qual (s:ss) p ts ! collapseSection p True ""


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
      anchor ! [href (moduleHtmlFile mdl), target mainFrameName]
        << toHtml txt


ppHtmlContentsFrame :: FilePath -> String -> Themes
  -> [InstalledInterface] -> Bool -> IO ()
ppHtmlContentsFrame odir doctitle themes ifaces debug = do
  let mods = flatModuleTree ifaces
      html =
        headHtml doctitle Nothing themes +++
        miniBody << divModuleList <<
          (sectionName << "Modules" +++
           ulist << [ li ! [theclass "module"] << m | m <- mods ])
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, frameIndexHtmlFile]) (renderToString debug html)


--------------------------------------------------------------------------------
-- * Generate the index
--------------------------------------------------------------------------------


ppHtmlIndex :: FilePath
            -> String
            -> Maybe String
            -> Themes
            -> Maybe String
            -> SourceURLs
            -> WikiURLs
            -> [InstalledInterface]
            -> Bool
            -> IO ()
ppHtmlIndex odir doctitle _maybe_package themes
  maybe_contents_url maybe_source_url maybe_wiki_url ifaces debug = do
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
      headHtml (doctitle ++ " (" ++ indexName ch ++ ")") Nothing themes +++
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
        -> SourceURLs -> WikiURLs
        -> Maybe String -> Maybe String -> Bool -> QualOption
        -> Bool -> Interface -> IO ()
ppHtmlModule odir doctitle themes
  maybe_source_url maybe_wiki_url
  maybe_contents_url maybe_index_url unicode qual debug iface = do
  let
      mdl = ifaceMod iface
      aliases = ifaceModuleAliases iface
      mdl_str = moduleString mdl
      real_qual = makeModuleQual qual aliases mdl
      html =
        headHtml mdl_str (Just $ "mini_" ++ moduleHtmlFile mdl) themes +++
        bodyHtml doctitle (Just iface)
          maybe_source_url maybe_wiki_url
          maybe_contents_url maybe_index_url << [
            divModuleHeader << (moduleInfo iface +++ (sectionName << mdl_str)),
            ifaceToHtml maybe_source_url maybe_wiki_url iface unicode real_qual
          ]

  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, moduleHtmlFile mdl]) (renderToString debug html)
  ppHtmlModuleMiniSynopsis odir doctitle themes iface unicode real_qual debug

ppHtmlModuleMiniSynopsis :: FilePath -> String -> Themes
  -> Interface -> Bool -> Qualification -> Bool -> IO ()
ppHtmlModuleMiniSynopsis odir _doctitle themes iface unicode qual debug = do
  let mdl = ifaceMod iface
      html =
        headHtml (moduleString mdl) Nothing themes +++
        miniBody <<
          (divModuleHeader << sectionName << moduleString mdl +++
           miniSynopsis mdl iface unicode qual)
  createDirectoryIfMissing True odir
  writeFile (joinPath [odir, "mini_" ++ moduleHtmlFile mdl]) (renderToString debug html)


ifaceToHtml :: SourceURLs -> WikiURLs -> Interface -> Bool -> Qualification -> Html
ifaceToHtml maybe_source_url maybe_wiki_url iface unicode qual
  = ppModuleContents qual exports +++
    description +++
    synopsis +++
    divInterface (maybe_doc_hdr +++ bdy)
  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)

    -- todo: if something has only sub-docs, or fn-args-docs, should
    -- it be measured here and thus prevent omitting the synopsis?
    has_doc (ExportDecl _ (Documentation mDoc mWarning, _) _ _) = isJust mDoc || isJust mWarning
    has_doc (ExportNoDecl _ _) = False
    has_doc (ExportModule _) = False
    has_doc _ = True

    no_doc_at_all = not (any has_doc exports)

    description | isNoHtml doc = doc
                | otherwise    = divDescription $ sectionName << "Description" +++ doc
                where doc = docSection qual (ifaceRnDoc iface)

        -- omit the synopsis if there are no documentation annotations at all
    synopsis
      | no_doc_at_all = noHtml
      | otherwise
      = divSynposis $
            paragraph ! collapseControl "syn" False "caption" << "Synopsis" +++
            shortDeclList (
                mapMaybe (processExport True linksInfo unicode qual) exports
            ) ! (collapseSection "syn" False "" ++ collapseToggle "syn")

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

    linksInfo = (maybe_source_url, maybe_wiki_url)


miniSynopsis :: Module -> Interface -> Bool -> Qualification -> Html
miniSynopsis mdl iface unicode qual =
    divInterface << concatMap (processForMiniSynopsis mdl unicode qual) exports
  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)


processForMiniSynopsis :: Module -> Bool -> Qualification -> ExportItem DocName
                       -> [Html]
processForMiniSynopsis mdl unicode qual (ExportDecl (L _loc decl0) _doc _ _insts) =
  ((divTopDecl <<).(declElem <<)) <$> case decl0 of
    TyClD d -> let b = ppTyClBinderWithVarsMini mdl d in case d of
        (FamDecl decl)    -> [ppTyFamHeader True False decl unicode qual]
        (DataDecl{})   -> [keyword "data" <+> b]
        (SynDecl{})    -> [keyword "type" <+> b]
        (ClassDecl {}) -> [keyword "class" <+> b]
        _ -> []
    SigD (TypeSig lnames (L _ _)) ->
      map (ppNameMini mdl . nameOccName . getName . unLoc) lnames
    _ -> []
processForMiniSynopsis _ _ qual (ExportGroup lvl _id txt) =
  [groupTag lvl << docToHtml qual txt]
processForMiniSynopsis _ _ _ _ = []


ppNameMini :: Module -> OccName -> Html
ppNameMini mdl nm =
    anchor ! [ href (moduleNameUrl mdl nm)
             , target mainFrameName ]
      << ppBinder' nm


ppTyClBinderWithVarsMini :: Module -> TyClDecl DocName -> Html
ppTyClBinderWithVarsMini mdl decl =
  let n = tcdName decl
      ns = tyvarNames $ tcdTyVars decl -- it's safe to use tcdTyVars, see code above
  in ppTypeApp n ns (ppNameMini mdl . nameOccName . getName) ppTyName


ppModuleContents :: Qualification -> [ExportItem DocName] -> Html
ppModuleContents qual exports
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
        html = linkedAnchor (groupId id0) << docToHtml qual doc +++ mk_subsections ssecs
        (ssecs, rest1) = process lev rest
        (secs,  rest2) = process n   rest1
  process n (_ : rest) = process n rest

  mk_subsections [] = noHtml
  mk_subsections ss = unordList ss


-- we need to assign a unique id to each section heading so we can hyperlink
-- them from the contents:
numberSectionHeadings :: [ExportItem DocName] -> [ExportItem DocName]
numberSectionHeadings = go 1
  where go :: Int -> [ExportItem DocName] -> [ExportItem DocName]
        go _ [] = []
        go n (ExportGroup lev _ doc : es)
          = ExportGroup lev (show n) doc : go (n+1) es
        go n (other:es)
          = other : go n es


processExport :: Bool -> LinksInfo -> Bool -> Qualification
              -> ExportItem DocName -> Maybe Html
processExport summary _ _ qual (ExportGroup lev id0 doc)
  = nothingIf summary $ groupHeading lev id0 << docToHtml qual doc
processExport summary links unicode qual (ExportDecl decl doc subdocs insts)
  = processDecl summary $ ppDecl summary links decl doc insts subdocs unicode qual
processExport summary _ _ qual (ExportNoDecl y [])
  = processDeclOneLiner summary $ ppDocName qual y
processExport summary _ _ qual (ExportNoDecl y subs)
  = processDeclOneLiner summary $
      ppDocName qual y +++ parenList (map (ppDocName qual) subs)
processExport summary _ _ qual (ExportDoc doc)
  = nothingIf summary $ docSection_ qual doc
processExport summary _ _ _ (ExportModule mdl)
  = processDeclOneLiner summary $ toHtml "module" <+> ppModule mdl


nothingIf :: Bool -> a -> Maybe a
nothingIf True _ = Nothing
nothingIf False a = Just a


processDecl :: Bool -> Html -> Maybe Html
processDecl True = Just
processDecl False = Just . divTopDecl


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
