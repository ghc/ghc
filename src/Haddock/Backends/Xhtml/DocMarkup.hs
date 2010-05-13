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
module Haddock.Backends.Xhtml.DocMarkup where

import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Util
import Haddock.GhcUtils
import Haddock.Types
import Haddock.Utils

import Text.XHtml hiding ( name, title, p, quote )

import GHC
import Name
import RdrName


parHtmlMarkup :: (a -> Html) -> (a -> Bool) -> DocMarkup a Html
parHtmlMarkup ppId isTyCon = Markup {
  markupParagraph     = paragraph,
  markupEmpty         = toHtml "",
  markupString        = toHtml,
  markupAppend        = (+++),
  markupIdentifier    = tt . ppId . choose,
  markupModule        = \m -> let (mdl,ref) = break (=='#') m in ppModule (mkModuleNoPackage mdl) ref,
  markupEmphasis      = emphasize . toHtml,
  markupMonospaced    = tt . toHtml,
  markupUnorderedList = ulist . concatHtml . map (li <<),
  markupPic           = \path -> image ! [src path],
  markupOrderedList   = olist . concatHtml . map (li <<),
  markupDefList       = dlist . concatHtml . map markupDef,
  markupCodeBlock     = pre,
  markupURL           = \url -> anchor ! [href url] << toHtml url,
  markupAName         = \aname -> namedAnchor aname << toHtml "",
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


markupDef :: (HTML a, HTML b) => (a, b) -> Html
markupDef (a,b) = dterm << a +++ ddef << b


htmlMarkup :: DocMarkup DocName Html
htmlMarkup = parHtmlMarkup ppDocName (isTyConName . getName)

htmlOrigMarkup :: DocMarkup Name Html
htmlOrigMarkup = parHtmlMarkup ppName isTyConName

htmlRdrMarkup :: DocMarkup RdrName Html
htmlRdrMarkup = parHtmlMarkup ppRdrName isRdrTc

-- If the doc is a single paragraph, don't surround it with <P> (this causes
-- ugly extra whitespace with some browsers).
docToHtml :: Doc DocName -> Html
docToHtml doc = markup htmlMarkup (markup htmlCleanup doc)

origDocToHtml :: Doc Name -> Html
origDocToHtml doc = markup htmlOrigMarkup (markup htmlCleanup doc)

rdrDocToHtml :: Doc RdrName -> Html
rdrDocToHtml doc = markup htmlRdrMarkup (markup htmlCleanup doc)

-- If there is a single paragraph, then surrounding it with <P>..</P>
-- can add too much whitespace in some browsers (eg. IE).  However if
-- we have multiple paragraphs, then we want the extra whitespace to
-- separate them.  So we catch the single paragraph case and transform it
-- here.
unParagraph :: Doc a -> Doc a
unParagraph (DocParagraph d) = d
--NO: This eliminates line breaks in the code block:  (SDM, 6/5/2003)
--unParagraph (DocCodeBlock d) = (DocMonospaced d)
unParagraph doc              = doc

htmlCleanup :: DocMarkup a (Doc a)
htmlCleanup = idMarkup { 
  markupUnorderedList = DocUnorderedList . map unParagraph,
  markupOrderedList   = DocOrderedList   . map unParagraph
  } 
