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
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.Layout (
  miniBody,

  divPackageHeader, divContent, divModuleHeader, divFooter,
  divTableOfContents, divDescription, divSynposis, divInterface,
  divIndex, divAlphabet, divModuleList,

  sectionName,
  nonEmptySectionName,

  shortDeclList,
  shortSubDecls,

  divTopDecl,

  SubDecl,
  subArguments,
  subAssociatedTypes,
  subConstructors,
  subEquations,
  subFields,
  subInstances,
  subMethods,
  subMinimal,

  topDeclElem, declElem,
) where


import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils
import Haddock.Types
import Haddock.Utils (makeAnchorId)

import qualified Data.Map as Map
import Text.XHtml hiding ( name, title, p, quote )

import FastString            ( unpackFS )
import GHC

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
  | isNoHtml c = paragraph ! [theclass "caption empty"] $ spaceHtml
  | otherwise  = paragraph ! [theclass "caption"]       $ c


divPackageHeader, divContent, divModuleHeader, divFooter,
  divTableOfContents, divDescription, divSynposis, divInterface,
  divIndex, divAlphabet, divModuleList
    :: Html -> Html

divPackageHeader    = sectionDiv "package-header"
divContent          = sectionDiv "content"
divModuleHeader     = sectionDiv "module-header"
divFooter           = sectionDiv "footer"
divTableOfContents  = sectionDiv "table-of-contents"
divDescription      = sectionDiv "description"
divSynposis         = sectionDiv "synopsis"
divInterface        = sectionDiv "interface"
divIndex            = sectionDiv "index"
divAlphabet         = sectionDiv "alphabet"
divModuleList       = sectionDiv "module-list"


--------------------------------------------------------------------------------
-- * Declaration containers
--------------------------------------------------------------------------------


shortDeclList :: [Html] -> Html
shortDeclList items = ulist << map (li ! [theclass "src short"] <<) items


shortSubDecls :: Bool -> [Html] -> Html
shortSubDecls inst items = ulist ! [theclass c] << map (i <<) items
  where i | inst      = li ! [theclass "inst"]
          | otherwise = li
        c | inst      = "inst"
          | otherwise = "subs"


divTopDecl :: Html -> Html
divTopDecl = thediv ! [theclass "top"]


type SubDecl = (Html, Maybe (MDoc DocName), [Html])


divSubDecls :: (HTML a) => String -> a -> Maybe Html -> Html
divSubDecls cssClass captionName = maybe noHtml wrap
  where
    wrap = (subSection <<) . (subCaption +++)
    subSection = thediv ! [theclass $ unwords ["subs", cssClass]]
    subCaption = paragraph ! [theclass "caption"] << captionName


subDlist :: Qualification -> [SubDecl] -> Maybe Html
subDlist _ [] = Nothing
subDlist qual decls = Just $ dlist << map subEntry decls +++ clearDiv
  where
    subEntry (decl, mdoc, subs) =
      dterm ! [theclass "src"] << decl
      +++
      docElement ddef << (fmap (docToHtml Nothing qual) mdoc +++ subs)

    clearDiv = thediv ! [ theclass "clear" ] << noHtml


subTable :: Qualification -> [SubDecl] -> Maybe Html
subTable _ [] = Nothing
subTable qual decls = Just $ table << aboves (concatMap subRow decls)
  where
    subRow (decl, mdoc, subs) =
      (td ! [theclass "src"] << decl
       <->
       docElement td << fmap (docToHtml Nothing qual) mdoc)
      : map (cell . (td <<)) subs

-- | Sub table with source information (optional).
subTableSrc :: Qualification -> LinksInfo -> Bool -> DocName -> [(SubDecl,SrcSpan)] -> Maybe Html
subTableSrc _ _  _ _ [] = Nothing
subTableSrc qual lnks splice dn decls = Just $ table << aboves (concatMap subRow decls)
  where
    subRow ((decl, mdoc, subs),loc) =
      (td ! [theclass "src"] << decl
      <+> linkHtml loc
      <->
      docElement td << fmap (docToHtml Nothing qual) mdoc
      )
      : map (cell . (td <<)) subs
    linkHtml loc@(RealSrcSpan _) = links lnks loc splice dn
    linkHtml _ = noHtml

subBlock :: [Html] -> Maybe Html
subBlock [] = Nothing
subBlock hs = Just $ toHtml hs


subArguments :: Qualification -> [SubDecl] -> Html
subArguments qual = divSubDecls "arguments" "Arguments" . subTable qual


subAssociatedTypes :: [Html] -> Html
subAssociatedTypes = divSubDecls "associated-types" "Associated Types" . subBlock


subConstructors :: Qualification -> [SubDecl] -> Html
subConstructors qual = divSubDecls "constructors" "Constructors" . subTable qual


subFields :: Qualification -> [SubDecl] -> Html
subFields qual = divSubDecls "fields" "Fields" . subDlist qual


subEquations :: Qualification -> [SubDecl] -> Html
subEquations qual = divSubDecls "equations" "Equations" . subTable qual


-- | Generate sub table for instance declarations, with source
subInstances :: Qualification
             -> String -- ^ Class name, used for anchor generation
             -> LinksInfo -> Bool -> DocName
             -> [(SubDecl,SrcSpan)] -> Html
subInstances qual nm lnks splice dn = maybe noHtml wrap . instTable
  where
    wrap = (subSection <<) . (subCaption +++)
    instTable = fmap (thediv ! collapseSection id_ True [] <<) . subTableSrc qual lnks splice dn
    subSection = thediv ! [theclass "subs instances"]
    subCaption = paragraph ! collapseControl id_ True "caption" << "Instances"
    id_ = makeAnchorId $ "i:" ++ nm

subMethods :: [Html] -> Html
subMethods = divSubDecls "methods" "Methods" . subBlock

subMinimal :: Html -> Html
subMinimal = divSubDecls "minimal" "Minimal complete definition" . Just . declElem


-- a box for displaying code
declElem :: Html -> Html
declElem = paragraph ! [theclass "src"]


-- a box for top level documented names
-- it adds a source and wiki link at the right hand side of the box
topDeclElem :: LinksInfo -> SrcSpan -> Bool -> [DocName] -> Html -> Html
topDeclElem lnks loc splice names html =
    declElem << (html <+> (links lnks loc splice $ head names))
        -- FIXME: is it ok to simply take the first name?

-- | Adds a source and wiki link at the right hand side of the box.
-- Name must be documented, otherwise we wouldn't get here.
links :: LinksInfo -> SrcSpan -> Bool -> DocName -> Html
links ((_,_,sourceMap,lineMap), (_,_,maybe_wiki_url)) loc splice (Documented n mdl) =
   (srcLink <+> wikiLink)
  where srcLink = let nameUrl = Map.lookup origPkg sourceMap
                      lineUrl = Map.lookup origPkg lineMap
                      mUrl | splice    = lineUrl
                                        -- Use the lineUrl as a backup
                           | otherwise = maybe lineUrl Just nameUrl in
          case mUrl of
            Nothing  -> noHtml
            Just url -> let url' = spliceURL (Just fname) (Just origMod)
                                               (Just n) (Just loc) url
                          in anchor ! [href url', theclass "link"] << "Source"

        wikiLink =
          case maybe_wiki_url of
            Nothing  -> noHtml
            Just url -> let url' = spliceURL (Just fname) (Just mdl)
                                               (Just n) (Just loc) url
                          in anchor ! [href url', theclass "link"] << "Comments"

        -- For source links, we want to point to the original module,
        -- because only that will have the source.
        -- TODO: do something about type instances. They will point to
        -- the module defining the type family, which is wrong.
        origMod = nameModule n
        origPkg = modulePackageKey origMod

        fname = case loc of
          RealSrcSpan l -> unpackFS (srcSpanFile l)
          UnhelpfulSpan _ -> error "links: UnhelpfulSpan"
links _ _ _ _ = noHtml
