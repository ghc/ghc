module Haddock.Backends.Hyperlinker.Renderer where

import Haddock.Backends.Hyperlinker.Parser

import Data.Monoid
import Text.XHtml

render :: [Token] -> Html
render = body . pre . foldr (<>) noHtml . map renderToken

renderToken :: Token -> Html
renderToken (Token t v _) = thespan (toHtml v) ! tokenAttrs t

tokenAttrs :: TokenType -> [HtmlAttr]
tokenAttrs TkIdentifier = [theclass "hs-identifier"]
tokenAttrs TkKeyword = [theclass "hs-keyword"]
tokenAttrs TkString = [theclass "hs-string"]
tokenAttrs TkChar = [theclass "hs-char"]
tokenAttrs TkNumber = [theclass "hs-number"]
tokenAttrs TkOperator = [theclass "hs-operator"]
tokenAttrs TkGlyph = [theclass "hs-glyph"]
tokenAttrs TkSpecial = [theclass "hs-special"]
tokenAttrs TkSpace = []
tokenAttrs TkComment = [theclass "hs-comment"]
tokenAttrs TkCpp = [theclass "hs-cpp"]
tokenAttrs TkUnknown = []
