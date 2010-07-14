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
  divPackageHeader, divModuleHeader, divFooter,
  divTableOfContents, divDescription, divSynposis, divInterface, 
  
  sectionName,
  
  shortDeclList,
  divTopDecl,
  
  SubDecl,
  subConstructors, subFields,
  
  topDeclElem, declElem,
  
  instHdr, atHdr, methHdr,
  argBox, ndocBox, rdocBox, maybeRDocBox,
  
  vanillaTable, vanillaTable2, spacedTable1, spacedTable5  
) where

import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Util
import Haddock.Types

import Text.XHtml hiding ( name, title, p, quote )

import FastString            ( unpackFS )
import GHC

-- Sections of the document

divPackageHeader, divModuleHeader, divFooter :: Html -> Html
divPackageHeader = thediv ! [identifier "package-header"]
divModuleHeader  = thediv ! [identifier "module-header"]
divFooter        = thediv ! [identifier "footer"]

divTableOfContents, divDescription, divSynposis, divInterface :: Html -> Html
divTableOfContents = thediv ! [identifier "table-of-contents"]
divDescription     = thediv ! [identifier "description"]
divSynposis        = thediv ! [identifier "synopsis"]
divInterface       = thediv ! [identifier "interface"]

-- | The name of a section, used directly after opening a section
sectionName :: Html -> Html
sectionName = paragraph ! [theclass "caption"]


-- | Declaration containers 

shortDeclList :: [Html] -> Html
shortDeclList items = ulist << map (li ! [theclass "src short"] <<) items

divTopDecl :: Html -> Html
divTopDecl = thediv ! [theclass "top"]


type SubDecl = (Html, Maybe (Doc DocName), [Html])

divSubDecls :: String -> String -> Maybe Html -> Html
divSubDecls cssClass captionName = maybe noHtml wrap
  where
    wrap = (subSection <<) . (subCaption +++)
    subSection = thediv ! [theclass $ unwords ["subs", cssClass]]
    subCaption = paragraph ! [theclass "caption"] << captionName

subDlist :: [SubDecl] -> Maybe Html
subDlist [] = Nothing
subDlist decls = Just $ dlist << map subEntry decls
  where
    subEntry (decl, mdoc, subs) =
      dterm ! [theclass "src"] << decl
      +++
      ddef << (fmap docToHtml mdoc `with` subs)
      
    Nothing  `with` [] = spaceHtml
    ma       `with` bs = ma +++ bs

subTable :: [SubDecl] -> Maybe Html
subTable [] = Nothing
subTable decls = Just $ table << aboves (concatMap subRow decls)
  where
    subRow (decl, mdoc, subs) =
      (td ! [theclass "src"] << decl
       <->
       td << nonEmpty (fmap docToHtml mdoc))
      : map (cell . (td <<)) subs
      
subConstructors :: [(Html, Maybe (Doc DocName), [Html])] -> Html
subConstructors = divSubDecls "constructors" "Constructors" . subTable

subFields :: [(Html, Maybe (Doc DocName), [Html])] -> Html
subFields = divSubDecls "fields" "Fields" . subTable


-- a box for displaying code
declElem :: Html -> Html
declElem = paragraph ! [theclass "src"]

-- a box for top level documented names
-- it adds a source and wiki link at the right hand side of the box
topDeclElem :: LinksInfo -> SrcSpan -> DocName -> Html -> Html
topDeclElem ((_,_,maybe_source_url), (_,_,maybe_wiki_url)) loc name html = 
    declElem << (html +++ srcLink +++ wikiLink)
  where srcLink =
          case maybe_source_url of
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

        -- Name must be documented, otherwise we wouldn't get here
        Documented n mdl = name

        fname = unpackFS (srcSpanFile loc)



-- a box for displaying an 'argument' (some code which has text to the
-- right of it).  Wrapping is not allowed in these boxes, whereas it is
-- in a declBox.
argBox :: Html -> HtmlTable
argBox html = tda [theclass "arg"] << html


-- a box for displaying documentation, not indented.
ndocBox :: Html -> HtmlTable
ndocBox html = tda [theclass "ndoc"] << html

-- a box for displaying documentation, padded on the left a little
rdocBox :: Html -> HtmlTable
rdocBox html = tda [theclass "rdoc"] << html

maybeRDocBox :: Maybe (Doc DocName) -> HtmlTable
maybeRDocBox Nothing = rdocBox (noHtml)
maybeRDocBox (Just doc) = rdocBox (docToHtml doc)


-- a vanilla table has width 100%, no border, no padding, no spacing
vanillaTable, vanillaTable2 :: Html -> Html
vanillaTable  = table ! [theclass "vanilla",  cellspacing 0, cellpadding 0]
vanillaTable2 = table ! [theclass "vanilla2", cellspacing 0, cellpadding 0]

spacedTable1, spacedTable5 :: Html -> Html
spacedTable1 = table ! [theclass "vanilla",  cellspacing 1, cellpadding 0]
spacedTable5 = table ! [theclass "vanilla",  cellspacing 5, cellpadding 0]

methHdr, atHdr :: Html
methHdr    = h5 << "Methods"
atHdr      = h5 << "Associated Types"

instHdr :: String -> Html
instHdr id_ = 
  h5 << (collapsebutton id_ +++ toHtml " Instances")
