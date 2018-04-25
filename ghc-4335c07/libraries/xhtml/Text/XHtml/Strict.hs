-- | Produces XHTML 1.0 Strict.
module Text.XHtml.Strict (
     -- * Data types
     Html, HtmlAttr,
     -- * Classes
     HTML(..), ADDATTRS(..), CHANGEATTRS(..),
     -- * Primitives and basic combinators
     (<<), concatHtml, (+++), 
     noHtml, isNoHtml, tag, itag,
     htmlAttrPair, emptyAttr, intAttr, strAttr, htmlAttr,
     primHtml, stringToHtmlString,
     docType,
     -- * Rendering
     showHtml, renderHtml, renderHtmlWithLanguage, prettyHtml,
     showHtmlFragment, renderHtmlFragment, prettyHtmlFragment,
     module Text.XHtml.Strict.Elements,
     module Text.XHtml.Strict.Attributes,
     module Text.XHtml.Extras
  ) where

import Text.XHtml.Internals
import Text.XHtml.Strict.Elements
import Text.XHtml.Strict.Attributes
import Text.XHtml.Extras

-- | The @DOCTYPE@ for XHTML 1.0 Strict.
docType :: String
docType = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
          ++ " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

-- | Output the HTML without adding newlines or spaces within the markup.
--   This should be the most time and space efficient way to
--   render HTML, though the ouput is quite unreadable.
showHtml :: HTML html => html -> String
showHtml = showHtmlInternal docType

-- | Outputs indented HTML. Because space matters in
--   HTML, the output is quite messy.
renderHtml :: HTML html => html -> String
renderHtml = renderHtmlInternal docType

-- | Outputs indented XHTML. Because space matters in
--   HTML, the output is quite messy.
renderHtmlWithLanguage :: HTML html
                       => String -- ^ The code of the "dominant" language of the webpage.
                       -> html -- ^ All the 'Html', including a header.
                       -> String
renderHtmlWithLanguage l theHtml =
    docType ++ "\n" ++ renderHtmlFragment code  ++ "\n"
  where
    code = tag "html" ! [ strAttr "xmlns" "http://www.w3.org/1999/xhtml"
                           , strAttr "lang" l
                           , strAttr "xml:lang" l
                           ] << theHtml

-- | Outputs indented HTML, with indentation inside elements.
--   This can change the meaning of the HTML document, and 
--   is mostly useful for debugging the HTML output.
--   The implementation is inefficient, and you are normally
--   better off using 'showHtml' or 'renderHtml'.
prettyHtml :: HTML html => html -> String
prettyHtml = prettyHtmlInternal docType
