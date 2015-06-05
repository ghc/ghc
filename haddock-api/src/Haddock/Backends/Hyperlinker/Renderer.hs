module Haddock.Backends.Hyperlinker.Renderer where

import Haddock.Backends.Hyperlinker.Parser

import Data.Monoid
import Text.XHtml (Html, HtmlAttr, (!))
import qualified Text.XHtml as Html

render :: Maybe FilePath -> [Token] -> Html
render css tokens = header css <> body tokens

body :: [Token] -> Html
body = Html.body . Html.pre . mconcat . map token

header :: Maybe FilePath -> Html
header Nothing = Html.noHtml
header (Just css) =
    Html.header $ Html.thelink Html.noHtml ! attrs
  where
    attrs =
        [ Html.rel "stylesheet"
        , Html.href css
        , Html.thetype "text/css"
        ]

token :: Token -> Html
token (Token t v _) = Html.thespan (Html.toHtml v) ! tokenAttrs t

tokenAttrs :: TokenType -> [HtmlAttr]
tokenAttrs TkIdentifier = [Html.theclass "hs-identifier"]
tokenAttrs TkKeyword = [Html.theclass "hs-keyword"]
tokenAttrs TkString = [Html.theclass "hs-string"]
tokenAttrs TkChar = [Html.theclass "hs-char"]
tokenAttrs TkNumber = [Html.theclass "hs-number"]
tokenAttrs TkOperator = [Html.theclass "hs-operator"]
tokenAttrs TkGlyph = [Html.theclass "hs-glyph"]
tokenAttrs TkSpecial = [Html.theclass "hs-special"]
tokenAttrs TkSpace = []
tokenAttrs TkComment = [Html.theclass "hs-comment"]
tokenAttrs TkCpp = [Html.theclass "hs-cpp"]
tokenAttrs TkPragma = [Html.theclass "hs-pragma"]
tokenAttrs TkUnknown = []
