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

  shortDeclList,
  shortSubDecls,

  divTopDecl,

  SubDecl,
  subArguments,
  subAssociatedTypes,
  subConstructors,
  subFields,
  subInstances,
  subMethods,

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


shortSubDecls :: [Html] -> Html
shortSubDecls items = ulist ! [theclass "subs"] << map (li <<) items


divTopDecl :: Html -> Html
divTopDecl = thediv ! [theclass "top"]


type SubDecl = (Html, Maybe (Doc DocName), [Html])


divSubDecls :: (HTML a) => String -> a -> Maybe Html -> Html
divSubDecls cssClass captionName = maybe noHtml wrap
  where
    wrap = (subSection <<) . (subCaption +++)
    subSection = thediv ! [theclass $ unwords ["subs", cssClass]]
    subCaption = paragraph ! [theclass "caption"] << captionName


subDlist :: [SubDecl] -> Maybe Html
subDlist [] = Nothing
subDlist decls = Just $ dlist << map subEntry decls +++ clearDiv
  where
    subEntry (decl, mdoc, subs) =
      dterm ! [theclass "src"] << decl
      +++
      docElement ddef << (fmap docToHtml mdoc +++ subs)
    clearDiv = thediv ! [ theclass "clear" ] << noHtml


subTable :: [SubDecl] -> Maybe Html
subTable [] = Nothing
subTable decls = Just $ table << aboves (concatMap subRow decls)
  where
    subRow (decl, mdoc, subs) =
      (td ! [theclass "src"] << decl
       <->
       docElement td << fmap docToHtml mdoc)
      : map (cell . (td <<)) subs


subBlock :: [Html] -> Maybe Html
subBlock [] = Nothing
subBlock hs = Just $ toHtml hs


subArguments :: [SubDecl] -> Html
subArguments = divSubDecls "arguments" "Arguments" . subTable


subAssociatedTypes :: [Html] -> Html
subAssociatedTypes = divSubDecls "associated-types" "Associated Types" . subBlock


subConstructors :: [SubDecl] -> Html
subConstructors = divSubDecls "constructors" "Constructors" . subTable


subFields :: [SubDecl] -> Html
subFields = divSubDecls "fields" "Fields" . subDlist


subInstances :: String -> [SubDecl] -> Html
subInstances nm = maybe noHtml wrap . instTable
  where
    wrap = (subSection <<) . (subCaption +++)
    instTable = fmap (thediv ! collapseSection id_ True [] <<) . subTable
    subSection = thediv ! [theclass $ "subs instances"]
    subCaption = paragraph ! collapseControl id_ True "caption" << "Instances"
    id_ = makeAnchorId $ "i:" ++ nm

subMethods :: [Html] -> Html
subMethods = divSubDecls "methods" "Methods" . subBlock


-- a box for displaying code
declElem :: Html -> Html
declElem = paragraph ! [theclass "src"]


-- a box for top level documented names
-- it adds a source and wiki link at the right hand side of the box
topDeclElem :: LinksInfo -> SrcSpan -> DocName -> Html -> Html
topDeclElem ((_,_,sourceMap), (_,_,maybe_wiki_url)) loc name html =
    declElem << (html +++ srcLink +++ wikiLink)
  where srcLink =
          case Map.lookup origPkg sourceMap of
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
        origPkg = modulePackageId origMod

        -- Name must be documented, otherwise we wouldn't get here
        Documented n mdl = name

        fname = unpackFS (srcSpanFile loc)

