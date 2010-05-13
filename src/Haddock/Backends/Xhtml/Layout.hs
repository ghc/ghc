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
module Haddock.Backends.Xhtml.Layout where

import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Util
import Haddock.Types

import Text.XHtml hiding ( name, title, p, quote )

import FastString            ( unpackFS )
import GHC


declWithDoc :: Bool -> LinksInfo -> SrcSpan -> DocName -> Maybe (Doc DocName) -> Html -> HtmlTable
declWithDoc True  _     _   _  _          html_decl = declBox html_decl
declWithDoc False links loc nm Nothing    html_decl = topDeclBox links loc nm html_decl
declWithDoc False links loc nm (Just doc) html_decl = 
                topDeclBox links loc nm html_decl </> docBox (docToHtml doc)



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
            Nothing  -> emptyTable
            Just url -> tda [theclass "declbut"] <<
                          let url' = spliceURL (Just fname) (Just origMod)
                                               (Just n) (Just loc) url
                           in anchor ! [href url'] << toHtml "Source"

        wikiLink =
          case maybe_wiki_url of
            Nothing  -> emptyTable
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

maybeRDocBox :: Maybe (Doc DocName) -> HtmlTable
maybeRDocBox Nothing = rdocBox (noHtml)
maybeRDocBox (Just doc) = rdocBox (docToHtml doc)


bodyBox :: Html -> HtmlTable
bodyBox html = tda [theclass "body"] << vanillaTable << html

-- a vanilla table has width 100%, no border, no padding, no spacing
vanillaTable, vanillaTable2 :: Html -> Html
vanillaTable  = table ! [theclass "vanilla",  cellspacing 0, cellpadding 0]
vanillaTable2 = table ! [theclass "vanilla2", cellspacing 0, cellpadding 0]

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
