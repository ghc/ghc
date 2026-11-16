-- | Utility functions for the blaze tests
--
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Tests.Util
    ( renderUsingString
    , renderUsingText
    , renderUsingUtf8
    , p, div, table, img, br, area
    , id, class_, name
    ) where

import Prelude hiding (div, id)
import Text.Blaze.Internal

import Blaze.ByteString.Builder as B (toLazyByteString)
import Blaze.ByteString.Builder.Char.Utf8 as B (fromString)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as LB
import qualified Text.Blaze.Renderer.String as String (renderMarkup)
import qualified Text.Blaze.Renderer.Text as Text (renderMarkup)
import qualified Text.Blaze.Renderer.Utf8 as Utf8 (renderMarkup)

-- | Render Markup to an UTF-8 encoded ByteString using the String renderer
--
renderUsingString :: Markup -> LB.ByteString
renderUsingString = toLazyByteString . fromString . String.renderMarkup

-- | Render Markup to an UTF-8 encoded ByteString using the Text renderer
--
renderUsingText :: Markup -> LB.ByteString
renderUsingText = encodeUtf8 . Text.renderMarkup

-- | Render HTML to an UTF-8 encoded ByteString using the Utf8 renderer
--
renderUsingUtf8 :: Markup -> LB.ByteString
renderUsingUtf8 = Utf8.renderMarkup

-- Some definitions for HTML combinators to enable testing

p :: Markup   -- ^ Inner HTML.
  -> Markup   -- ^ Resulting HTML.
p = Parent "p" "<p" "</p>"

div :: Markup   -- ^ Inner HTML.
    -> Markup   -- ^ Resulting HTML.
div = Parent "div" "<div" "</div>"

table :: Markup   -- ^ Inner HTML.
      -> Markup   -- ^ Resulting HTML.
table = Parent "table" "<table" "</table>"

img :: Markup   -- ^ Resulting HTML.
img = Leaf "img" "<img" ">" ()

br :: Markup   -- ^ Resulting HTML.
br = Leaf "br" "<br" ">" ()

area :: Markup   -- ^ Resulting HTML.
area = Leaf "area" "<area" ">" ()

class_ :: AttributeValue  -- ^ Attribute value.
       -> Attribute       -- ^ Resulting attribute.
class_ = attribute "class" " class=\""

id :: AttributeValue  -- ^ Attribute value.
   -> Attribute       -- ^ Resulting attribute.
id = attribute "id" " id=\""

name :: AttributeValue  -- ^ Attribute value.
     -> Attribute       -- ^ Resulting attribute.
name = attribute "name" " name=\""
