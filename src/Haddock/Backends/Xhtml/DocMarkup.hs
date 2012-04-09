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

  docElement, docSection, docSection_,
) where


import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Utils
import Haddock.Types
import Haddock.Utils

import Text.XHtml hiding ( name, title, p, quote )
import Data.Maybe (fromMaybe)

import GHC


parHtmlMarkup :: Qualification -> (a -> Html) -> DocMarkup a Html
parHtmlMarkup qual ppId = Markup {
  markupEmpty                = noHtml,
  markupString               = toHtml,
  markupParagraph            = paragraph,
  markupAppend               = (+++),
  markupIdentifier           = thecode . ppId,
  markupIdentifierUnchecked  = thecode . ppUncheckedLink qual,
  markupModule               = \m -> let (mdl,ref) = break (=='#') m
                                     in ppModuleRef (mkModuleName mdl) ref,
  markupWarning              = thediv ! [theclass "warning"],
  markupEmphasis             = emphasize,
  markupMonospaced           = thecode,
  markupUnorderedList        = unordList,
  markupOrderedList          = ordList,
  markupDefList              = defList,
  markupCodeBlock            = pre,
  markupHyperlink            = \(Hyperlink url mLabel) -> anchor ! [href url] << fromMaybe url mLabel,
  markupAName                = \aname -> namedAnchor aname << "",
  markupPic                  = \path -> image ! [src path],
  markupProperty             = pre . toHtml,
  markupExample              = examplesToHtml
  }
  where
    examplesToHtml l = pre (concatHtml $ map exampleToHtml l) ! [theclass "screen"]

    exampleToHtml (Example expression result) = htmlExample
      where
        htmlExample = htmlPrompt +++ htmlExpression +++ toHtml (unlines result)
        htmlPrompt = (thecode . toHtml $ ">>> ") ! [theclass "prompt"]
        htmlExpression = (strong . thecode . toHtml $ expression ++ "\n") ! [theclass "userinput"]


-- If the doc is a single paragraph, don't surround it with <P> (this causes
-- ugly extra whitespace with some browsers).  FIXME: Does this still apply?
docToHtml :: Qualification -> Doc DocName -> Html
docToHtml qual = markup fmt . cleanup
  where fmt = parHtmlMarkup qual (ppDocName qual)


origDocToHtml :: Qualification -> Doc Name -> Html
origDocToHtml qual = markup fmt . cleanup
  where fmt = parHtmlMarkup qual ppName


rdrDocToHtml :: Qualification -> Doc RdrName -> Html
rdrDocToHtml qual = markup fmt . cleanup
  where fmt = parHtmlMarkup qual ppRdrName


docElement :: (Html -> Html) -> Html -> Html
docElement el content_ =
  if isNoHtml content_
    then el ! [theclass "doc empty"] << spaceHtml
    else el ! [theclass "doc"] << content_


docSection :: Qualification -> Documentation DocName -> Html
docSection qual = maybe noHtml (docSection_ qual) . combineDocumentation


docSection_ :: Qualification -> Doc DocName -> Html
docSection_ qual = (docElement thediv <<) . docToHtml qual


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
