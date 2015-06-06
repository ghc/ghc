module Haddock.Backends.Hyperlinker.Renderer (render) where

import Haddock.Backends.Hyperlinker.Parser
import Haddock.Backends.Hyperlinker.Ast

import qualified GHC
import qualified Name as GHC

import Data.Monoid
import Text.XHtml (Html, HtmlAttr, (!))
import qualified Text.XHtml as Html

render :: Maybe FilePath -> [RichToken] -> Html
render css tokens = header css <> body tokens

body :: [RichToken] -> Html
body = Html.body . Html.pre . mconcat . map richToken

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

richToken :: RichToken -> Html
richToken (RichToken t Nothing) = token t
richToken (RichToken t (Just name)) = Html.anchor (token t) ! nameAttrs name

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

nameAttrs :: GHC.Name -> [HtmlAttr]
nameAttrs name =
    [ Html.href (maybe "" id mmod ++ "#" ++ ident)
    , Html.theclass "varid-reference"
    ]
  where
    mmod = GHC.moduleNameString . GHC.moduleName <$> GHC.nameModule_maybe name
    ident = GHC.occNameString . GHC.nameOccName $ name
