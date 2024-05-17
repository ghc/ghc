{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
module Haddock.Backends.Xhtml
  ( ppHtml
  , copyHtmlBits
  , ppHtmlIndex
  , ppHtmlContents
  , ppJsonIndex
  ) where

import Prelude hiding (div)

import Haddock.Backends.Xhtml.Decl
import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Layout
import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Themes
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils
import Haddock.GhcUtils
import Haddock.InterfaceFile (PackageInfo (..), PackageInterfaces (..), ppPackageInfo)
import Haddock.ModuleTree
import Haddock.Options (Visibility (..))
import Haddock.Types
import Haddock.Utils
import Haddock.Utils.Json
import Haddock.Version
import Text.XHtml hiding (name, p, quote, title)
import qualified Text.XHtml as XHtml

import Control.DeepSeq (force)
import Control.Monad (unless, when)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Builder as Builder
import Data.Char (isSpace, toUpper)
import Data.Either (partitionEithers)
import Data.Foldable (foldl', traverse_)
import Data.List (intersperse, isPrefixOf, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord (comparing)
import qualified Data.Set as Set hiding (Set)
import System.Directory
import System.FilePath hiding ((</>))
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import GHC hiding (LexicalFixity (..), NoLink, anchor, moduleInfo)
import GHC.Types.Name
import GHC.Unit.State

--------------------------------------------------------------------------------

-- * Generating HTML documentation

--------------------------------------------------------------------------------

ppHtml
  :: UnitState
  -> String
  -- ^ Title
  -> Maybe String
  -- ^ Package
  -> [Interface]
  -> [InstalledInterface]
  -- ^ Reexported interfaces
  -> FilePath
  -- ^ Destination directory
  -> Maybe (MDoc GHC.RdrName)
  -- ^ Prologue text, maybe
  -> Themes
  -- ^ Themes
  -> Maybe String
  -- ^ The mathjax URL (--mathjax)
  -> SourceURLs
  -- ^ The source URL (--source)
  -> WikiURLs
  -- ^ The wiki URL (--wiki)
  -> BaseURL
  -- ^ The base URL (--base-url)
  -> Maybe String
  -- ^ The contents URL (--use-contents)
  -> Maybe String
  -- ^ The index URL (--use-index)
  -> Bool
  -- ^ Whether to use unicode in output (--use-unicode)
  -> Maybe String
  -- ^ Package name
  -> PackageInfo
  -- ^ Package info
  -> QualOption
  -- ^ How to qualify names
  -> Bool
  -- ^ Output pretty html (newlines and indenting)
  -> Bool
  -- ^ Also write Quickjump index
  -> IO ()
ppHtml
  state
  doctitle
  maybe_package
  ifaces
  reexported_ifaces
  odir
  prologue
  themes
  maybe_mathjax_url
  maybe_source_url
  maybe_wiki_url
  maybe_base_url
  maybe_contents_url
  maybe_index_url
  unicode
  pkg
  packageInfo
  qual
  debug
  withQuickjump = do
    let
      visible_ifaces = filter visible ifaces
      visible i = OptHide `notElem` ifaceOptions i

    when (isNothing maybe_contents_url) $
      ppHtmlContents
        state
        odir
        doctitle
        maybe_package
        themes
        maybe_mathjax_url
        maybe_index_url
        maybe_source_url
        maybe_wiki_url
        withQuickjump
        [ PackageInterfaces
            { piPackageInfo = packageInfo
            , piVisibility = Visible
            , piInstalledInterfaces =
                map toInstalledIface visible_ifaces
                  ++ reexported_ifaces
            }
        ]
        False -- we don't want to display the packages in a single-package contents
        prologue
        debug
        pkg
        (makeContentsQual qual)

    when (isNothing maybe_index_url) $ do
      ppHtmlIndex
        odir
        doctitle
        maybe_package
        themes
        maybe_mathjax_url
        maybe_contents_url
        maybe_source_url
        maybe_wiki_url
        withQuickjump
        (map toInstalledIface visible_ifaces ++ reexported_ifaces)
        debug

    when withQuickjump $
      ppJsonIndex
        odir
        maybe_source_url
        maybe_wiki_url
        unicode
        pkg
        qual
        visible_ifaces
        []

    mapM_
      ( ppHtmlModule
          odir
          doctitle
          themes
          maybe_mathjax_url
          maybe_source_url
          maybe_wiki_url
          maybe_base_url
          maybe_contents_url
          maybe_index_url
          withQuickjump
          unicode
          pkg
          qual
          debug
      )
      visible_ifaces

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

headHtml :: String -> Themes -> Maybe String -> Maybe String -> Html
headHtml docTitle themes mathjax_url base_url =
  header
    ! (maybe [] (\url -> [identifier "head", strAttr "data-base-url" url]) base_url)
    << [ meta ! [httpequiv "Content-Type", content "text/html; charset=UTF-8"]
       , meta ! [XHtml.name "viewport", content "width=device-width, initial-scale=1"]
       , thetitle << docTitle
       , styleSheet base_url themes
       , thelink
          ! [ rel "stylesheet"
            , thetype "text/css"
            , href (withBaseURL base_url quickJumpCssFile)
            ]
          << noHtml
       , thelink ! [rel "stylesheet", thetype "text/css", href fontUrl] << noHtml
       , script
          ! [ src (withBaseURL base_url haddockJsFile)
            , emptyAttr "async"
            , thetype "text/javascript"
            ]
          << noHtml
       , script ! [thetype "text/x-mathjax-config"] << primHtml mjConf
       , script ! [src mjUrl, thetype "text/javascript"] << noHtml
       ]
  where
    fontUrl = "https://fonts.googleapis.com/css?family=PT+Sans:400,400i,700"
    mjUrl = fromMaybe "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS-MML_HTMLorMML" mathjax_url
    mjConf =
      unwords
        [ "MathJax.Hub.Config({"
        , "tex2jax: {"
        , "processClass: \"mathjax\","
        , "ignoreClass: \".*\""
        , "}"
        , "});"
        ]

quickJumpButtonLi
  :: Bool
  -- ^ With Quick Jump?
  -> Maybe Html
-- The TypeScript should replace this <li> element, given its id. However, in
-- case it does not, the element is given content here too.
quickJumpButtonLi True =
  Just $
    li
      ! [identifier "quick-jump-button"]
      << anchor
      ! [href "#"]
      << "Quick Jump"
quickJumpButtonLi False = Nothing

srcButton :: SourceURLs -> Maybe Interface -> Maybe Html
srcButton (Just src_base_url, _, _, _) Nothing =
  Just (anchor ! [href src_base_url] << "Source")
srcButton (_, Just src_module_url, _, _) (Just iface) =
  let url = spliceURL (Just $ ifaceMod iface) Nothing Nothing src_module_url
   in Just (anchor ! [href url] << "Source")
srcButton _ _ =
  Nothing

wikiButton :: WikiURLs -> Maybe Module -> Maybe Html
wikiButton (Just wiki_base_url, _, _) Nothing =
  Just (anchor ! [href wiki_base_url] << "User Comments")
wikiButton (_, Just wiki_module_url, _) (Just mdl) =
  let url = spliceURL (Just mdl) Nothing Nothing wiki_module_url
   in Just (anchor ! [href url] << "User Comments")
wikiButton _ _ =
  Nothing

contentsButton :: Maybe String -> Maybe Html
contentsButton maybe_contents_url =
  Just (anchor ! [href url] << "Contents")
  where
    url = fromMaybe contentsHtmlFile maybe_contents_url

indexButton :: Maybe String -> Maybe Html
indexButton maybe_index_url =
  Just (anchor ! [href url] << "Index")
  where
    url = fromMaybe indexHtmlFile maybe_index_url

bodyHtml
  :: String
  -> Maybe Interface
  -> SourceURLs
  -> WikiURLs
  -> Maybe String
  -> Maybe String
  -> Bool
  -- ^ With Quick Jump?
  -> Html
  -> Html
bodyHtml
  doctitle
  iface
  maybe_source_url
  maybe_wiki_url
  maybe_contents_url
  maybe_index_url
  withQuickjump
  pageContent =
    body
      << [ divPackageHeader
            << [ nonEmptySectionName << doctitle
               , ulist
                  ! [theclass "links", identifier "page-menu"]
                  << catMaybes (quickJumpButtonLi withQuickjump : otherButtonLis)
               ]
         , divContent << pageContent
         , divFooter
            << paragraph
            << ( "Produced by "
                  +++ (anchor ! [href projectUrl] << toHtml projectName)
                  +++ (" version " ++ projectVersion)
               )
         ]
    where
      otherButtonLis =
        (fmap . fmap)
          (li <<)
          [ srcButton maybe_source_url iface
          , wikiButton maybe_wiki_url (ifaceMod <$> iface)
          , contentsButton maybe_contents_url
          , indexButton maybe_index_url
          ]

moduleInfo :: Interface -> Html
moduleInfo iface =
  let
    info = ifaceInfo iface

    doOneEntry :: (String, HaddockModInfo GHC.Name -> Maybe String) -> Maybe HtmlTable
    doOneEntry (fldNm, fld) =
      fld info >>= \a -> return (th << fldNm <-> td << a)

    entries :: [HtmlTable]
    entries =
      maybeToList copyrightsTable
        ++ mapMaybe
          doOneEntry
          [ ("License", hmi_license)
          , ("Maintainer", hmi_maintainer)
          , ("Stability", hmi_stability)
          , ("Portability", hmi_portability)
          , ("Safe Haskell", hmi_safety)
          , ("Language", lg)
          ]
        ++ extsForm
      where
        lg inf = fmap show (hmi_language inf)

        multilineRow :: String -> [String] -> HtmlTable
        multilineRow title xs = (th ! [valign "top"]) << title <-> td << (toLines xs)
          where
            toLines = mconcat . intersperse br . map toHtml

        copyrightsTable :: Maybe HtmlTable
        copyrightsTable = fmap (multilineRow "Copyright" . split) (hmi_copyright info)
          where
            split = map (trim . filter (/= ',')) . lines

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
  :: UnitState
  -> FilePath
  -> String
  -> Maybe String
  -> Themes
  -> Maybe String
  -> Maybe String
  -> SourceURLs
  -> WikiURLs
  -> Bool
  -- ^ With Quick Jump?
  -> [PackageInterfaces]
  -> Bool
  -> Maybe (MDoc GHC.RdrName)
  -> Bool
  -> Maybe Package
  -- ^ Current package
  -> Qualification
  -- ^ How to qualify names
  -> IO ()
ppHtmlContents
  state
  odir
  doctitle
  _maybe_package
  themes
  mathjax_url
  maybe_index_url
  maybe_source_url
  maybe_wiki_url
  withQuickjump
  packages
  showPkgs
  prologue
  debug
  pkg
  qual = do
    let trees =
          [ ( piPackageInfo pinfo
            , mkModuleTree
                state
                showPkgs
                [ (instMod iface, toInstalledDescription iface)
                | iface <- piInstalledInterfaces pinfo
                , not (instIsSig iface)
                ]
            )
          | pinfo <- packages
          ]
        sig_trees =
          [ ( piPackageInfo pinfo
            , mkModuleTree
                state
                showPkgs
                [ (instMod iface, toInstalledDescription iface)
                | iface <- piInstalledInterfaces pinfo
                , instIsSig iface
                ]
            )
          | pinfo <- packages
          ]
        html =
          headHtml doctitle themes mathjax_url Nothing
            +++ bodyHtml
              doctitle
              Nothing
              maybe_source_url
              maybe_wiki_url
              Nothing
              maybe_index_url
              withQuickjump
            << [ ppPrologue pkg qual doctitle prologue
               , ppSignatureTrees pkg qual sig_trees
               , ppModuleTrees pkg qual trees
               ]
    createDirectoryIfMissing True odir
    writeUtf8File (joinPath [odir, contentsHtmlFile]) (renderToString debug html)
    where
      -- Extract a module's short description.
      toInstalledDescription :: InstalledInterface -> Maybe (MDoc Name)
      toInstalledDescription = fmap mkMeta . hmi_description . instInfo

ppPrologue :: Maybe Package -> Qualification -> String -> Maybe (MDoc GHC.RdrName) -> Html
ppPrologue _ _ _ Nothing = noHtml
ppPrologue pkg qual title (Just doc) =
  divDescription << (h1 << title +++ docElement thediv (rdrDocToHtml pkg qual doc))

ppSignatureTrees :: Maybe Package -> Qualification -> [(PackageInfo, [ModuleTree])] -> Html
ppSignatureTrees _ _ tss | all (null . snd) tss = mempty
ppSignatureTrees pkg qual [(info, ts)] =
  divPackageList << (sectionName << "Signatures" +++ ppSignatureTree pkg qual "n" info ts)
ppSignatureTrees pkg qual tss =
  divModuleList
    << ( sectionName
          << "Signatures"
          +++ concatHtml
            [ ppSignatureTree pkg qual ("n." ++ show i ++ ".") info ts
            | (i, (info, ts)) <- zip [(1 :: Int) ..] tss
            ]
       )

ppSignatureTree :: Maybe Package -> Qualification -> String -> PackageInfo -> [ModuleTree] -> Html
ppSignatureTree _ _ _ _ [] = mempty
ppSignatureTree pkg qual p info ts =
  divModuleList << (sectionName << ppPackageInfo info +++ mkNodeList pkg qual [] p ts)

ppModuleTrees :: Maybe Package -> Qualification -> [(PackageInfo, [ModuleTree])] -> Html
ppModuleTrees _ _ tss | all (null . snd) tss = mempty
ppModuleTrees pkg qual [(info, ts)] =
  divModuleList << (sectionName << "Modules" +++ ppModuleTree pkg qual "n" info ts)
ppModuleTrees pkg qual tss =
  divPackageList
    << ( sectionName
          << "Packages"
          +++ concatHtml
            [ ppModuleTree pkg qual ("n." ++ show i ++ ".") info ts
            | (i, (info, ts)) <- zip [(1 :: Int) ..] tss
            ]
       )

ppModuleTree :: Maybe Package -> Qualification -> String -> PackageInfo -> [ModuleTree] -> Html
ppModuleTree _ _ _ _ [] = mempty
ppModuleTree pkg qual p info ts =
  divModuleList << (sectionName << ppPackageInfo info +++ mkNodeList pkg qual [] p ts)

mkNodeList :: Maybe Package -> Qualification -> [String] -> String -> [ModuleTree] -> Html
mkNodeList pkg qual ss p ts = case ts of
  [] -> noHtml
  _ -> unordList (zipWith (mkNode pkg qual ss) ps ts)
  where
    ps = [p ++ '.' : show i | i <- [(1 :: Int) ..]]

mkNode :: Maybe Package -> Qualification -> [String] -> String -> ModuleTree -> Html
mkNode pkg qual ss p (Node s leaf _pkg srcPkg short ts) =
  htmlModule <+> shortDescr +++ htmlPkg +++ subtree
  where
    modAttrs = case (ts, leaf) of
      (_ : _, Nothing) -> collapseControl p "module"
      (_, _) -> [theclass "module"]

    cBtn = case (ts, leaf) of
      (_ : _, Just _) -> thespan ! collapseControl p "" << spaceHtml
      ([], Just _) -> thespan ! [theclass "noexpander"] << spaceHtml
      (_, _) -> noHtml
    -- We only need an explicit collapser button when the module name
    -- is also a leaf, and so is a link to a module page. Indeed, the
    -- spaceHtml is a minor hack and does upset the layout a fraction.

    htmlModule =
      thespan
        ! modAttrs
        << ( cBtn
              +++ case leaf of
                Just m -> ppModule m
                Nothing -> toHtml s
           )

    shortDescr = maybe noHtml (origDocToHtml pkg qual) short
    htmlPkg = maybe noHtml (thespan ! [theclass "package"] <<) srcPkg

    subtree =
      if null ts
        then noHtml
        else
          collapseDetails
            p
            DetailsOpen
            ( thesummary
                ! [theclass "hide-when-js-enabled"]
                << "Submodules"
                +++ mkNodeList pkg qual (s : ss) p ts
            )

--------------------------------------------------------------------------------

-- * Generate the index

--------------------------------------------------------------------------------

data JsonIndexEntry = JsonIndexEntry
  { jieHtmlFragment :: String
  , jieName :: String
  , jieModule :: String
  , jieLink :: String
  }
  deriving (Show)

instance ToJSON JsonIndexEntry where
  toJSON
    JsonIndexEntry
      { jieHtmlFragment
      , jieName
      , jieModule
      , jieLink
      } =
      Object
        [ "display_html" .= String jieHtmlFragment
        , "name" .= String jieName
        , "module" .= String jieModule
        , "link" .= String jieLink
        ]

instance FromJSON JsonIndexEntry where
  parseJSON = withObject "JsonIndexEntry" $ \v ->
    JsonIndexEntry
      <$> v .: "display_html"
      <*> v .: "name"
      <*> v .: "module"
      <*> v .: "link"

ppJsonIndex
  :: FilePath
  -> SourceURLs
  -- ^ The source URL (--source)
  -> WikiURLs
  -- ^ The wiki URL (--wiki)
  -> Bool
  -> Maybe Package
  -> QualOption
  -> [Interface]
  -> [FilePath]
  -- ^ file paths to interface files
  -- (--read-interface)
  -> IO ()
ppJsonIndex odir maybe_source_url maybe_wiki_url unicode pkg qual_opt ifaces installedIfacesPaths = do
  createDirectoryIfMissing True odir
  (errors, installedIndexes) <-
    partitionEithers
      <$> traverse
        ( \ifaceFile -> do
            let indexFile =
                  takeDirectory ifaceFile
                    FilePath.</> "doc-index.json"
            a <- doesFileExist indexFile
            if a
              then
                bimap (indexFile,) (map (fixLink ifaceFile))
                  <$> eitherDecodeFile @[JsonIndexEntry] indexFile
              else return (Right [])
        )
        installedIfacesPaths
  traverse_
    (\(indexFile, err) -> putStrLn $ "haddock: Coudn't parse " ++ indexFile ++ ": " ++ err)
    errors
  IO.withBinaryFile (joinPath [odir, indexJsonFile]) IO.WriteMode $ \h ->
    Builder.hPutBuilder
      h
      (encodeToBuilder (encodeIndexes (concat installedIndexes)))
  where
    encodeIndexes :: [JsonIndexEntry] -> Value
    encodeIndexes installedIndexes =
      toJSON
        ( concatMap fromInterface ifaces
            ++ installedIndexes
        )

    fromInterface :: Interface -> [JsonIndexEntry]
    fromInterface iface =
      mkIndex mdl qual `mapMaybe` ifaceRnExportItems iface
      where
        qual = makeModuleQual qual_opt mdl
        mdl = ifaceMod iface

    mkIndex :: Module -> Qualification -> ExportItem DocNameI -> Maybe JsonIndexEntry
    mkIndex mdl qual item
      | Just item_html <- processExport True links_info unicode pkg qual item =
          Just
            JsonIndexEntry
              { jieHtmlFragment = showHtmlFragment item_html
              , jieName = unwords (map getOccString names)
              , jieModule = moduleString mdl
              , jieLink = fromMaybe "" (listToMaybe (map (nameLink mdl) names))
              }
      | otherwise = Nothing
      where
        names = exportName item ++ exportSubs item

    exportSubs :: ExportItem DocNameI -> [IdP DocNameI]
    exportSubs (ExportDecl (RnExportD{rnExpDExpD = ExportD{expDSubDocs}})) = map fst expDSubDocs
    exportSubs _ = []

    exportName :: ExportItem DocNameI -> [IdP DocNameI]
    exportName (ExportDecl (RnExportD{rnExpDExpD = ExportD{expDDecl}})) = getMainDeclBinderI (unLoc expDDecl)
    exportName ExportNoDecl{expItemName} = [expItemName]
    exportName _ = []

    nameLink :: NamedThing name => Module -> name -> String
    nameLink mdl = moduleNameUrl' (moduleName mdl) . nameOccName . getName

    links_info = (maybe_source_url, maybe_wiki_url)

    -- update link using relative path to output directory
    fixLink
      :: FilePath
      -> JsonIndexEntry
      -> JsonIndexEntry
    fixLink ifaceFile jie =
      jie
        { jieLink =
            makeRelative odir (takeDirectory ifaceFile)
              FilePath.</> jieLink jie
        }

ppHtmlIndex
  :: FilePath
  -> String
  -> Maybe String
  -> Themes
  -> Maybe String
  -> Maybe String
  -> SourceURLs
  -> WikiURLs
  -> Bool
  -- ^ With Quick Jump?
  -> [InstalledInterface]
  -> Bool
  -> IO ()
ppHtmlIndex
  odir
  doctitle
  _maybe_package
  themes
  maybe_mathjax_url
  maybe_contents_url
  maybe_source_url
  maybe_wiki_url
  withQuickjump
  ifaces
  debug = do
    let html =
          indexPage
            split_indices
            Nothing
            (if split_indices then [] else index)

    createDirectoryIfMissing True odir

    when split_indices $ do
      mapM_ (do_sub_index index) initialChars
      -- Let's add a single large index as well for those who don't know exactly what they're looking for:
      let mergedhtml = indexPage False Nothing index
      writeUtf8File (joinPath [odir, subIndexHtmlFile merged_name]) (renderToString debug mergedhtml)

    writeUtf8File (joinPath [odir, indexHtmlFile]) (renderToString debug html)
    where
      indexPage showLetters ch items =
        headHtml (doctitle ++ " (" ++ indexName ch ++ ")") themes maybe_mathjax_url Nothing
          +++ bodyHtml
            doctitle
            Nothing
            maybe_source_url
            maybe_wiki_url
            maybe_contents_url
            Nothing
            withQuickjump
          << [ if showLetters then indexInitialLetterLinks else noHtml
             , if null items
                then noHtml
                else divIndex << [sectionName << indexName ch, buildIndex items]
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
        divAlphabet
          << unordList
            ( map (\str -> anchor ! [href (subIndexHtmlFile str)] << str) $
                [ [c] | c <- initialChars, any ((== c) . toUpper . head . fst) index
                ]
                  ++ [merged_name]
            )

      -- todo: what about names/operators that start with Unicode
      -- characters?
      -- Exports beginning with '_' can be listed near the end,
      -- presumably they're not as important... but would be listed
      -- with non-split index!
      initialChars = ['A' .. 'Z'] ++ ":!#$%&*+./<=>?@\\^|-~" ++ "_"

      do_sub_index this_ix c =
        unless (null index_part) $
          writeUtf8File (joinPath [odir, subIndexHtmlFile [c]]) (renderToString debug html)
        where
          html = indexPage True (Just c) index_part
          index_part = [(n, stuff) | (n, stuff) <- this_ix, toUpper (head n) == c]

      index :: [(String, Map GHC.Name [(Module, Bool)])]
      index = sortBy cmp (Map.toAscList full_index)
        where
          cmp (n1, _) (n2, _) = comparing (map toUpper) n1 n2

      -- for each name (a plain string), we have a number of original HsNames that
      -- it can refer to, and for each of those we have a list of modules
      -- that export that entity.  Each of the modules exports the entity
      -- in a visible or invisible way (hence the Bool).
      full_index :: Map String (Map GHC.Name [(Module, Bool)])
      full_index = foldl' f Map.empty ifaces
        where
          f
            :: Map String (Map Name [(Module, Bool)])
            -> InstalledInterface
            -> Map String (Map Name [(Module, Bool)])
          f !idx iface =
            Map.unionWith
              (Map.unionWith (\a b -> let !x = force $ a ++ b in x))
              idx
              (getIfaceIndex iface)

      getIfaceIndex :: InstalledInterface -> Map String (Map Name [(Module, Bool)])
      getIfaceIndex iface =
        foldl' f Map.empty (instExports iface)
        where
          f
            :: Map String (Map Name [(Module, Bool)])
            -> Name
            -> Map String (Map Name [(Module, Bool)])
          f !idx name =
            let !vis = name `Set.member` visible
             in Map.insertWith
                  (Map.unionWith (++))
                  (getOccString name)
                  (Map.singleton name [(mdl, vis)])
                  idx

          mdl = instMod iface
          visible = Set.fromList (instVisibleExports iface)

      indexElt :: (String, Map GHC.Name [(Module, Bool)]) -> HtmlTable
      indexElt (str, entities) =
        case Map.toAscList entities of
          [(nm, entries)] ->
            td
              ! [theclass "src"]
              << toHtml str
              <-> indexLinks nm entries
          many_entities ->
            td
              ! [theclass "src"]
              << toHtml str
              <-> td
              << spaceHtml
              </> aboves (zipWith (curry doAnnotatedEntity) [1 ..] many_entities)

      doAnnotatedEntity :: (Integer, (Name, [(Module, Bool)])) -> HtmlTable
      doAnnotatedEntity (j, (nm, entries)) =
        td
          ! [theclass "alt"]
          << toHtml (show j)
          <+> parens (ppAnnot (nameOccName nm))
          <-> indexLinks nm entries

      ppAnnot n
        | not (isValOcc n) = toHtml "Type/Class"
        | isDataOcc n = toHtml "Data Constructor"
        | otherwise = toHtml "Function"

      indexLinks nm entries =
        td
          ! [theclass "module"]
          << hsep
            ( punctuate
                comma
                [ if visible
                  then linkId mdl (Just nm) << toHtml (moduleString mdl)
                  else toHtml (moduleString mdl)
                | (mdl, visible) <- entries
                ]
            )

--------------------------------------------------------------------------------

-- * Generate the HTML page for a module

--------------------------------------------------------------------------------

ppHtmlModule
  :: FilePath
  -> String
  -> Themes
  -> Maybe String
  -> SourceURLs
  -> WikiURLs
  -> BaseURL
  -> Maybe String
  -> Maybe String
  -> Bool
  -- ^ With Quick Jump?
  -> Bool
  -> Maybe Package
  -> QualOption
  -> Bool
  -> Interface
  -> IO ()
ppHtmlModule
  odir
  doctitle
  themes
  maybe_mathjax_url
  maybe_source_url
  maybe_wiki_url
  maybe_base_url
  maybe_contents_url
  maybe_index_url
  withQuickjump
  unicode
  pkg
  qual
  debug
  iface = do
    let
      mdl = ifaceMod iface
      mdl_str = moduleString mdl
      mdl_str_annot =
        mdl_str
          ++ if ifaceIsSig iface
            then " (signature)"
            else ""
      mdl_str_linked
        | ifaceIsSig iface =
            mdl_str
              +++ " (signature"
              +++ sup
              << ("[" +++ anchor ! [href signatureDocURL] << "?" +++ "]")
              +++ ")"
        | otherwise =
            toHtml mdl_str
      real_qual = makeModuleQual qual mdl
      html =
        headHtml mdl_str_annot themes maybe_mathjax_url maybe_base_url
          +++ bodyHtml
            doctitle
            (Just iface)
            maybe_source_url
            maybe_wiki_url
            maybe_contents_url
            maybe_index_url
            withQuickjump
          << [ divModuleHeader << (moduleInfo iface +++ (sectionName << mdl_str_linked))
             , ifaceToHtml maybe_source_url maybe_wiki_url iface unicode pkg real_qual
             ]

    createDirectoryIfMissing True odir
    writeUtf8File (joinPath [odir, moduleHtmlFile mdl]) (renderToString debug html)

signatureDocURL :: String
signatureDocURL = "https://wiki.haskell.org/Module_signature"

ifaceToHtml :: SourceURLs -> WikiURLs -> Interface -> Bool -> Maybe Package -> Qualification -> Html
ifaceToHtml maybe_source_url maybe_wiki_url iface unicode pkg qual =
  ppModuleContents pkg qual exports (not . null $ ifaceRnOrphanInstances iface)
    +++ description
    +++ synopsis
    +++ divInterface (maybe_doc_hdr +++ bdy +++ orphans)
  where
    exports = numberSectionHeadings (ifaceRnExportItems iface)

    -- todo: if something has only sub-docs, or fn-args-docs, should
    -- it be measured here and thus prevent omitting the synopsis?
    has_doc
      ( ExportDecl
          ( RnExportD
              { rnExpDExpD =
                ExportD
                  { expDMbDoc =
                    (Documentation mDoc mWarn, _)
                  }
              }
            )
        ) = isJust mDoc || isJust mWarn
    has_doc (ExportNoDecl _ _) = False
    has_doc (ExportModule _) = False
    has_doc _ = True

    no_doc_at_all = not (any has_doc exports)

    description
      | isNoHtml doc = doc
      | otherwise = divDescription $ sectionName << "Description" +++ doc
      where
        doc = docSection Nothing pkg qual (ifaceRnDoc iface)

    -- omit the synopsis if there are no documentation annotations at all
    synopsis
      | no_doc_at_all = noHtml
      | otherwise =
          divSynopsis $
            collapseDetails
              "syn"
              DetailsClosed
              ( thesummary
                  << "Synopsis"
                  +++ shortDeclList
                    ( mapMaybe (processExport True linksInfo unicode pkg qual) exports
                    )
                  ! collapseToggle "syn" ""
              )

    -- if the documentation doesn't begin with a section header, then
    -- add one ("Documentation").
    maybe_doc_hdr =
      case exports of
        [] -> noHtml
        ExportGroup{} : _ -> noHtml
        _ -> h1 << "Documentation"

    bdy =
      foldr (+++) noHtml $
        mapMaybe (processExport False linksInfo unicode pkg qual) exports

    orphans =
      ppOrphanInstances linksInfo (ifaceRnOrphanInstances iface) False unicode pkg qual

    linksInfo = (maybe_source_url, maybe_wiki_url)

ppModuleContents
  :: Maybe Package
  -- ^ This package
  -> Qualification
  -> [ExportItem DocNameI]
  -> Bool
  -- ^ Orphans sections
  -> Html
ppModuleContents pkg qual exports orphan
  | null sections && not orphan = noHtml
  | otherwise = contentsDiv
  where
    contentsDiv =
      divTableOfContents
        << ( divContentsList
              << ( (sectionName << "Contents")
                    ! [strAttr "onclick" "window.scrollTo(0,0)"]
                    +++ unordList (sections ++ orphanSection)
                 )
           )

    (sections, _leftovers {-should be []-}) = process 0 exports
    orphanSection
      | orphan = [linkedAnchor "section.orphans" << "Orphan instances"]
      | otherwise = []

    process :: Int -> [ExportItem DocNameI] -> ([Html], [ExportItem DocNameI])
    process _ [] = ([], [])
    process n items@(ExportGroup lev id0 doc : rest)
      | lev <= n = ([], items)
      | otherwise = (html : secs, rest2)
      where
        html =
          linkedAnchor (groupId id0)
            << docToHtmlNoAnchors (Just id0) pkg qual (mkMeta doc)
            +++ mk_subsections ssecs
        (ssecs, rest1) = process lev rest
        (secs, rest2) = process n rest1
    process n (_ : rest) = process n rest

    mk_subsections [] = noHtml
    mk_subsections ss = unordList ss

-- we need to assign a unique id to each section heading so we can hyperlink
-- them from the contents:
numberSectionHeadings :: [ExportItem DocNameI] -> [ExportItem DocNameI]
numberSectionHeadings = go 1
  where
    go :: Int -> [ExportItem DocNameI] -> [ExportItem DocNameI]
    go _ [] = []
    go n (ExportGroup lev _ doc : es) =
      case collectAnchors doc of
        [] -> ExportGroup lev (show n) doc : go (n + 1) es
        (a : _) -> ExportGroup lev a doc : go (n + 1) es
    go n (other : es) =
      other : go n es

    collectAnchors :: DocH (Wrap (ModuleName, OccName)) (Wrap DocName) -> [String]
    collectAnchors (DocAppend a b) = collectAnchors a ++ collectAnchors b
    collectAnchors (DocAName a) = [a]
    collectAnchors _ = []

processExport
  :: Bool
  -> LinksInfo
  -> Bool
  -> Maybe Package
  -> Qualification
  -> ExportItem DocNameI
  -> Maybe Html
processExport
  _
  _
  _
  _
  _
  ( ExportDecl
      ( RnExportD
          { rnExpDExpD =
            ExportD
              { expDDecl = L _ (InstD{})
              }
          }
        )
    ) =
    Nothing -- Hide empty instances
processExport
  summary
  links
  unicode
  pkg
  qual
  ( ExportDecl
      ( RnExportD
          { rnExpDExpD =
            ExportD decl pats doc subdocs insts fixities splice
          }
        )
    ) =
    processDecl summary $ ppDecl summary links decl pats doc insts fixities subdocs splice unicode pkg qual
processExport summary _ _ pkg qual (ExportGroup lev id0 doc) =
  nothingIf summary $ groupHeading lev id0 << docToHtmlNoAnchors (Just id0) pkg qual (mkMeta doc)
processExport summary _ _ _ qual (ExportNoDecl y []) =
  processDeclOneLiner summary $ ppDocName qual Prefix True y
processExport summary _ _ _ qual (ExportNoDecl y subs) =
  processDeclOneLiner summary $
    ppDocName qual Prefix True y
      +++ parenList (map (ppDocName qual Prefix True) subs)
processExport summary _ _ pkg qual (ExportDoc doc) =
  nothingIf summary $ docSection_ Nothing pkg qual doc
processExport summary _ _ _ _ (ExportModule mdl) =
  processDeclOneLiner summary $ toHtml "module" <+> ppModule mdl

nothingIf :: Bool -> a -> Maybe a
nothingIf True _ = Nothing
nothingIf False a = Just a

processDecl :: Bool -> Html -> Maybe Html
processDecl True = Just
processDecl False = Just . divTopDecl

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

processDeclOneLiner :: Bool -> Html -> Maybe Html
processDeclOneLiner True = Just
processDeclOneLiner False = Just . divTopDecl . declElem

groupHeading :: Int -> String -> Html -> Html
groupHeading lev id0 = linkedAnchor grpId . groupTag lev ! [identifier grpId]
  where
    grpId = groupId id0

groupTag :: Int -> Html -> Html
groupTag lev
  | lev == 1 = h1
  | lev == 2 = h2
  | lev == 3 = h3
  | otherwise = h4
