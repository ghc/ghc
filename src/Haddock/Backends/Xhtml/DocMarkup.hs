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
  docToHtmlNoAnchors,

  docElement, docSection, docSection_,
) where

import Control.Applicative ((<$>))

import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Utils
import Haddock.Types
import Haddock.Utils
import Haddock.Doc (combineDocumentation)

import Text.XHtml hiding ( name, p, quote )
import Data.Maybe (fromMaybe)

import GHC

parHtmlMarkup :: Qualification -> Bool
              -> (Bool -> a -> Html) -> DocMarkup a Html
parHtmlMarkup qual insertAnchors ppId = Markup {
  markupEmpty                = noHtml,
  markupString               = toHtml,
  markupParagraph            = paragraph,
  markupAppend               = (+++),
  markupIdentifier           = thecode . ppId insertAnchors,
  markupIdentifierUnchecked  = thecode . ppUncheckedLink qual,
  markupModule               = \m -> let (mdl,ref) = break (=='#') m
                                         -- Accomodate for old style
                                         -- foo\#bar anchors
                                         mdl' = case reverse mdl of
                                           '\\':_ -> init mdl
                                           _ -> mdl
                                     in ppModuleRef (mkModuleName mdl') ref,
  markupWarning              = thediv ! [theclass "warning"],
  markupEmphasis             = emphasize,
  markupBold                 = strong,
  markupMonospaced           = thecode,
  markupUnorderedList        = unordList,
  markupOrderedList          = ordList,
  markupDefList              = defList,
  markupCodeBlock            = pre,
  markupHyperlink            = \(Hyperlink url mLabel)
                               -> if insertAnchors
                                  then anchor ! [href url]
                                       << fromMaybe url mLabel
                                  else toHtml $ fromMaybe url mLabel,
  markupAName                = \aname -> namedAnchor aname << "",
  markupPic                  = \(Picture uri t) -> image ! ([src uri] ++ fromMaybe [] (return . title <$> t)),
  markupProperty             = pre . toHtml,
  markupExample              = examplesToHtml,
  markupHeader               = \(Header l t) -> makeHeader l t
  }
  where
    makeHeader :: Int -> Html -> Html
    makeHeader 1 mkup = h1 mkup
    makeHeader 2 mkup = h2 mkup
    makeHeader 3 mkup = h3 mkup
    makeHeader 4 mkup = h4 mkup
    makeHeader 5 mkup = h5 mkup
    makeHeader 6 mkup = h6 mkup
    makeHeader l _ = error $ "Somehow got a header level `" ++ show l ++ "' in DocMarkup!"


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
  where fmt = parHtmlMarkup qual True (ppDocName qual Raw)

-- | Same as 'docToHtml' but it doesn't insert the 'anchor' element
-- in links. This is used to generate the Contents box elements.
docToHtmlNoAnchors :: Qualification -> Doc DocName -> Html
docToHtmlNoAnchors qual = markup fmt . cleanup
  where fmt = parHtmlMarkup qual False (ppDocName qual Raw)

origDocToHtml :: Qualification -> Doc Name -> Html
origDocToHtml qual = markup fmt . cleanup
  where fmt = parHtmlMarkup qual True (const $ ppName Raw)


rdrDocToHtml :: Qualification -> Doc RdrName -> Html
rdrDocToHtml qual = markup fmt . cleanup
  where fmt = parHtmlMarkup qual True (const ppRdrName)


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
