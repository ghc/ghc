-- | Produces XHTML 1.0 Frameset.
module Text.XHtml.Frameset (
     -- * Data types
     Html, HtmlAttr,
     -- * Classes
     HTML(..), ADDATTRS(..), CHANGEATTRS(..),
     -- * Primitives and basic combinators
     (<<), concatHtml, (+++), 
     noHtml, isNoHtml, tag, itag,
     htmlAttrPair, emptyAttr, intAttr, strAttr, htmlAttr,
     primHtml, 
     -- * Rendering
     showHtml, renderHtml, prettyHtml, 
     showHtmlFragment, renderHtmlFragment, prettyHtmlFragment,
     module Text.XHtml.Strict.Elements,
     module Text.XHtml.Frameset.Elements,
     module Text.XHtml.Strict.Attributes,
     module Text.XHtml.Frameset.Attributes,
     module Text.XHtml.Extras
  ) where

import Text.XHtml.Internals

import Text.XHtml.Strict.Elements
import Text.XHtml.Frameset.Elements
import Text.XHtml.Strict.Attributes
import Text.XHtml.Frameset.Attributes

import Text.XHtml.Extras

docType :: String
docType =
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"" ++
     " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"

-- | Output the HTML without adding newlines or spaces within the markup.
--   This should be the most time and space efficient way to
--   render HTML, though the ouput is quite unreadable.
showHtml :: HTML html => html -> String
showHtml = showHtmlInternal docType

-- | Outputs indented HTML. Because space matters in
--   HTML, the output is quite messy.
renderHtml :: HTML html => html -> String
renderHtml = renderHtmlInternal docType

-- | Outputs indented HTML, with indentation inside elements.
--   This can change the meaning of the HTML document, and 
--   is mostly useful for debugging the HTML output.
--   The implementation is inefficient, and you are normally
--   better off using 'showHtml' or 'renderHtml'.
prettyHtml :: HTML html => html -> String
prettyHtml = prettyHtmlInternal docType
