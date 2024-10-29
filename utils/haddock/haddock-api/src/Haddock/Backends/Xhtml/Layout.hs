-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Backends.Html.Layout
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
module Haddock.Backends.Xhtml.Layout
  ( miniBody
  , divPackageHeader
  , divContent
  , divModuleHeader
  , divFooter
  , divTableOfContents
  , divDescription
  , divSynopsis
  , divInterface
  , divIndex
  , divAlphabet
  , divPackageList
  , divModuleList
  , divContentsList
  , sectionName
  , nonEmptySectionName
  , shortDeclList
  , shortSubDecls
  , divTopDecl
  , SubDecl
  , subArguments
  , subAssociatedTypes
  , subConstructors
  , subPatterns
  , subEquations
  , subFields
  , subInstances
  , subOrphanInstances
  , subInstHead
  , subInstDetails
  , subFamInstDetails
  , subMethods
  , subDefaults
  , subMinimal
  , subTableSrc
  , topDeclElem
  , declElem
  ) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC
import GHC.Types.Name (nameOccName)
import Text.XHtml hiding (name, quote, title)

import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils
import Haddock.Types
import Haddock.Utils (makeAnchorId, nameAnchorId)

--------------------------------------------------------------------------------

-- * Sections of the document

--------------------------------------------------------------------------------

miniBody :: Html -> Html
miniBody = body ! [identifier "mini"]

sectionDiv :: String -> Html -> Html
sectionDiv i = thediv ! [identifier i]

sectionName :: Html -> Html
sectionName = paragraph ! [theclass "caption"]

-- | Make an element that always has at least something (a non-breaking space).
-- If it would have otherwise been empty, then give it the class ".empty".
nonEmptySectionName :: Html -> Html
nonEmptySectionName c
  | isNoHtml c = thespan ! [theclass "caption empty"] $ spaceHtml
  | otherwise = thespan ! [theclass "caption"] $ c

divPackageHeader
  , divContent
  , divModuleHeader
  , divFooter
  , divTableOfContents
  , divDescription
  , divSynopsis
  , divInterface
  , divIndex
  , divAlphabet
  , divPackageList
  , divModuleList
  , divContentsList
    :: Html -> Html
divPackageHeader = sectionDiv "package-header"
divContent = sectionDiv "content"
divModuleHeader = sectionDiv "module-header"
divFooter = sectionDiv "footer"
divTableOfContents = sectionDiv "table-of-contents"
divContentsList = sectionDiv "contents-list"
divDescription = sectionDiv "description"
divSynopsis = sectionDiv "synopsis"
divInterface = sectionDiv "interface"
divIndex = sectionDiv "index"
divAlphabet = sectionDiv "alphabet"
divModuleList = sectionDiv "module-list"
divPackageList = sectionDiv "module-list"

--------------------------------------------------------------------------------

-- * Declaration containers

--------------------------------------------------------------------------------

shortDeclList :: [Html] -> Html
shortDeclList items = ulist << map (li ! [theclass "src short"] <<) items

shortSubDecls :: Bool -> [Html] -> Html
shortSubDecls inst items = ulist ! [theclass c] << map (i <<) items
  where
    i
      | inst = li ! [theclass "inst"]
      | otherwise = li
    c
      | inst = "inst"
      | otherwise = "subs"

divTopDecl :: Html -> Html
divTopDecl = thediv ! [theclass "top"]

type SubDecl = (Html, Maybe (MDoc DocName), [Html])

divSubDecls :: HTML a => String -> a -> Maybe Html -> Html
divSubDecls cssClass captionName = maybe noHtml wrap
  where
    wrap = (subSection <<) . (subCaption +++)
    subSection = thediv ! [theclass $ unwords ["subs", cssClass]]
    subCaption = paragraph ! [theclass "caption"] << captionName

subDlist :: Maybe Package -> Qualification -> [SubDecl] -> Maybe Html
subDlist _ _ [] = Nothing
subDlist pkg qual decls = Just $ ulist << map subEntry decls
  where
    subEntry (decl, mdoc, subs) =
      li
        << ( define
              ! [theclass "src"]
              << decl
              +++ docElement thediv
              << (fmap (docToHtml Nothing pkg qual) mdoc +++ subs)
           )

subTable :: Maybe Package -> Qualification -> [SubDecl] -> Maybe Html
subTable _ _ [] = Nothing
subTable pkg qual decls = Just $ table << aboves (concatMap subRow decls)
  where
    subRow (decl, mdoc, subs) =
      ( td
          ! [theclass "src"]
          << decl
          <-> docElement td
          << fmap (docToHtml Nothing pkg qual) mdoc
      )
        : map (cell . (td <<)) subs

-- | Sub table with source information (optional).
subTableSrc
  :: Maybe Package
  -> Qualification
  -> LinksInfo
  -> Bool
  -> [(SubDecl, Maybe Module, Located DocName)]
  -> Maybe Html
subTableSrc _ _ _ _ [] = Nothing
subTableSrc pkg qual lnks splice decls = Just $ table << aboves (concatMap subRow decls)
  where
    subRow ((decl, mdoc, subs), mdl, L loc dn) =
      ( td
          ! [theclass "src clearfix"]
          << (thespan ! [theclass "inst-left"] << decl)
          <+> linkHtml loc mdl dn
          <-> docElement td
          << fmap (docToHtml Nothing pkg qual) mdoc
      )
        : map (cell . (td <<)) subs

    linkHtml :: SrcSpan -> Maybe Module -> DocName -> Html
    linkHtml loc mdl dn = links lnks loc splice mdl dn

subBlock :: [Html] -> Maybe Html
subBlock [] = Nothing
subBlock hs = Just $ toHtml hs

subArguments :: Maybe Package -> Qualification -> [SubDecl] -> Html
subArguments pkg qual = divSubDecls "arguments" "Arguments" . subTable pkg qual

subAssociatedTypes :: [Html] -> Html
subAssociatedTypes = divSubDecls "associated-types" "Associated Types" . subBlock

subConstructors :: Maybe Package -> Qualification -> [SubDecl] -> Html
subConstructors pkg qual = divSubDecls "constructors" "Constructors" . subTable pkg qual

subPatterns :: Maybe Package -> Qualification -> [SubDecl] -> Html
subPatterns pkg qual = divSubDecls "bundled-patterns" "Bundled Patterns" . subTable pkg qual

subFields :: Maybe Package -> Qualification -> [SubDecl] -> Html
subFields pkg qual = divSubDecls "fields" "Fields" . subDlist pkg qual

subEquations :: Maybe Package -> Qualification -> [SubDecl] -> Html
subEquations pkg qual = divSubDecls "equations" "Equations" . subTable pkg qual

-- | Generate collapsible sub table for instance declarations, with source
subInstances
  :: Maybe Package
  -> Qualification
  -> String
  -- ^ Class name, used for anchor generation
  -> LinksInfo
  -> Bool
  -> [(SubDecl, Maybe Module, Located DocName)]
  -> Html
subInstances pkg qual nm lnks splice = maybe noHtml wrap . instTable
  where
    wrap contents = subSection (hdr +++ collapseDetails id_ DetailsOpen (summary +++ contents))
    instTable = subTableSrc pkg qual lnks splice
    subSection = thediv ! [theclass "subs instances"]
    hdr = h4 ! collapseControl id_ "instances" << "Instances"
    summary = thesummary ! [theclass "hide-when-js-enabled"] << "Instances details"
    id_ = makeAnchorId $ "i:" ++ nm

subOrphanInstances
  :: Maybe Package
  -> Qualification
  -> LinksInfo
  -> Bool
  -> [(SubDecl, Maybe Module, Located DocName)]
  -> Html
subOrphanInstances pkg qual lnks splice = maybe noHtml wrap . instTable
  where
    wrap = ((h1 << "Orphan instances") +++)
    instTable = fmap (thediv ! [identifier ("section." ++ id_)] <<) . subTableSrc pkg qual lnks splice
    id_ = makeAnchorId "orphans"

subInstHead
  :: String
  -- ^ Instance unique id (for anchor generation)
  -> Html
  -- ^ Header content (instance name and type)
  -> Html
subInstHead iid hdr =
  expander noHtml <+> hdr
  where
    expander = thespan ! collapseControl (instAnchorId iid) "instance"

subInstDetails
  :: String
  -- ^ Instance unique id (for anchor generation)
  -> [Html]
  -- ^ Associated type contents
  -> [Html]
  -- ^ Method contents (pretty-printed signatures)
  -> Html
  -- ^ Source module
  -> Html
subInstDetails iid ats mets mdl =
  subInstSection iid << (p mdl <+> subAssociatedTypes ats <+> subMethods mets)

subFamInstDetails
  :: String
  -- ^ Instance unique id (for anchor generation)
  -> Html
  -- ^ Type or data family instance
  -> Html
  -- ^ Source module TODO: use this
  -> Html
subFamInstDetails iid fi mdl =
  subInstSection iid << (p mdl <+> (thediv ! [theclass "src"] << fi))

subInstSection
  :: String
  -- ^ Instance unique id (for anchor generation)
  -> Html
  -> Html
subInstSection iid contents = collapseDetails (instAnchorId iid) DetailsClosed (summary +++ contents)
  where
    summary = thesummary ! [theclass "hide-when-js-enabled"] << "Instance details"

instAnchorId :: String -> String
instAnchorId iid = makeAnchorId $ "i:" ++ iid

subMethods :: [Html] -> Html
subMethods = divSubDecls "methods" "Methods" . subBlock

subDefaults :: [Html] -> Html
subDefaults = divSubDecls "default" "" . subBlock

subMinimal :: Html -> Html
subMinimal = divSubDecls "minimal" "Minimal complete definition" . Just . declElem

-- a box for displaying code
declElem :: Html -> Html
declElem = paragraph ! [theclass "src"]

-- a box for top level documented names
-- it adds a source and wiki link at the right hand side of the box
topDeclElem :: LinksInfo -> SrcSpan -> Bool -> DocName -> Html -> Html
topDeclElem lnks loc splice name html =
  declElem << (html <+> links lnks loc splice Nothing name)

-- FIXME: is it ok to simply take the first name?

-- | Adds a source and wiki link at the right hand side of the box.
-- Name must be documented, otherwise we wouldn't get here.
links :: LinksInfo -> SrcSpan -> Bool -> Maybe Module -> DocName -> Html
links ((_, _, sourceMap, lineMap), (_, _, maybe_wiki_url)) loc splice mdl' docName@(Documented n mdl) =
  srcLink <+> wikiLink <+> (selfLink ! [theclass "selflink"] << "#")
  where
    selfLink = linkedAnchor (nameAnchorId (nameOccName (getName docName)))

    srcLink =
      let nameUrl = Map.lookup origPkg sourceMap
          lineUrl = Map.lookup origPkg lineMap
          mUrl
            | splice = lineUrl
            -- Use the lineUrl as a backup
            | otherwise = maybe lineUrl Just nameUrl
       in case mUrl of
            Nothing -> noHtml
            Just url ->
              let url' = spliceURL (Just origMod) (Just n) (Just loc) url
               in anchor ! [href url', theclass "link"] << "Source"

    wikiLink =
      case maybe_wiki_url of
        Nothing -> noHtml
        Just url ->
          let url' = spliceURL (Just mdl) (Just n) (Just loc) url
           in anchor ! [href url', theclass "link"] << "Comments"

    -- For source links, we want to point to the original module,
    -- because only that will have the source.
    --
    -- 'mdl'' is a way of "overriding" the module. Without it, instances
    -- will point to the module defining the class/family, which is wrong.
    origMod = fromMaybe (nameModule n) mdl'
    origPkg = moduleUnit origMod
links _ _ _ _ _ = noHtml
