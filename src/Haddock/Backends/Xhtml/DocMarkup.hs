-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html.DocMarkup
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.DocMarkup (
  docToHtml,
  rdrDocToHtml,
  origDocToHtml,

  docElement, docSection, maybeDocSection,
) where


import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Utils
import Haddock.GhcUtils
import Haddock.Types
import Haddock.Utils

import Text.XHtml hiding ( name, title, p, quote )

import GHC
import Name
import RdrName


parHtmlMarkup :: (a -> Html) -> (a -> Bool) -> DocMarkup a Html
parHtmlMarkup ppId isTyCon = Markup {
  markupEmpty         = noHtml,
  markupString        = toHtml,
  markupParagraph     = paragraph,
  markupAppend        = (+++),
  markupIdentifier    = thecode . ppId . choose,
  markupModule        = \m -> let (mdl,ref) = break (=='#') m
                              in ppModuleRef (mkModuleNoPackage mdl) ref,
  markupEmphasis      = emphasize,
  markupMonospaced    = thecode,
  markupUnorderedList = unordList,
  markupOrderedList   = ordList,
  markupDefList       = defList,
  markupCodeBlock     = pre,
  markupURL           = \url -> anchor ! [href url] << url,
  markupAName         = \aname -> namedAnchor aname << "",
  markupPic           = \path -> image ! [src path],
  markupExample       = examplesToHtml
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

    examplesToHtml l = (pre $ concatHtml $ map exampleToHtml l) ! [theclass "screen"]

    exampleToHtml (Example expression result) = htmlExample
      where
        htmlExample = htmlPrompt +++ htmlExpression +++ (toHtml $ unlines result)
        htmlPrompt = (thecode . toHtml $ "ghci> ") ! [theclass "prompt"]
        htmlExpression = (strong . thecode . toHtml $ expression ++ "\n") ! [theclass "userinput"]


-- If the doc is a single paragraph, don't surround it with <P> (this causes
-- ugly extra whitespace with some browsers).  FIXME: Does this still apply?
docToHtml :: Doc DocName -> Html
docToHtml = markup fmt . cleanup
  where fmt = parHtmlMarkup ppDocName (isTyConName . getName)


origDocToHtml :: Doc Name -> Html
origDocToHtml = markup fmt . cleanup
  where fmt = parHtmlMarkup ppName isTyConName


rdrDocToHtml :: Doc RdrName -> Html
rdrDocToHtml = markup fmt . cleanup
  where fmt = parHtmlMarkup ppRdrName isRdrTc


docElement :: (ADDATTRS a) => a -> a
docElement = (! [theclass "doc"])


docSection :: Doc DocName -> Html
docSection = (docElement thediv <<) . docToHtml


maybeDocSection :: Maybe (Doc DocName) -> Html
maybeDocSection = maybe noHtml docSection


cleanup :: Doc a -> Doc a
cleanup = markup fmtUnParagraphLists
  where
    -- If there is a single paragraph, then surrounding it with <P>..</P>
    -- can add too much whitespace in some browsers (eg. IE).  However if
    -- we have multiple paragraphs, then we want the extra whitespace to
    -- separate them.  So we catch the single paragraph case and transform it
    -- here. We don't do this in code blocks as it eliminates line breaks.
    unParagraph :: Doc a -> Doc a
    unParagraph (DocParagraph d) = d
    unParagraph doc              = doc

    fmtUnParagraphLists :: DocMarkup a (Doc a)
    fmtUnParagraphLists = idMarkup {
      markupUnorderedList = DocUnorderedList . map unParagraph,
      markupOrderedList   = DocOrderedList   . map unParagraph
      }
