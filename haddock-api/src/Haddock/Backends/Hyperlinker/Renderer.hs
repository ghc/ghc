module Haddock.Backends.Hyperlinker.Renderer (render) where

import Haddock.Backends.Hyperlinker.Parser
import Haddock.Backends.Hyperlinker.Ast

import qualified GHC
import qualified Name as GHC

import Data.List
import Data.Monoid

import Text.XHtml (Html, HtmlAttr, (!))
import qualified Text.XHtml as Html

type StyleClass = String

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
richToken (RichToken tok Nothing) =
    tokenSpan tok ! attrs
  where
    attrs = [ multiclass . tokenStyle . tkType $ tok ]
richToken (RichToken tok (Just det)) =
    Html.anchor content ! (anchorAttrs . rtkName) det
  where
    content = tokenSpan tok ! [ multiclass style]
    style = (tokenStyle . tkType) tok ++ (richTokenStyle . rtkType) det

tokenSpan :: Token -> Html
tokenSpan = Html.thespan . Html.toHtml . tkValue

richTokenStyle :: RichTokenType -> [StyleClass]
richTokenStyle RtkVar = ["hs-var"]
richTokenStyle RtkType = ["hs-type"]

tokenStyle :: TokenType -> [StyleClass]
tokenStyle TkIdentifier = ["hs-identifier"]
tokenStyle TkKeyword = ["hs-keyword"]
tokenStyle TkString = ["hs-string"]
tokenStyle TkChar = ["hs-char"]
tokenStyle TkNumber = ["hs-number"]
tokenStyle TkOperator = ["hs-operator"]
tokenStyle TkGlyph = ["hs-glyph"]
tokenStyle TkSpecial = ["hs-special"]
tokenStyle TkSpace = []
tokenStyle TkComment = ["hs-comment"]
tokenStyle TkCpp = ["hs-cpp"]
tokenStyle TkPragma = ["hs-pragma"]
tokenStyle TkUnknown = []

multiclass :: [StyleClass] -> HtmlAttr
multiclass = Html.theclass . intercalate " "

anchorAttrs :: GHC.Name -> [HtmlAttr]
anchorAttrs name =
    [ Html.href (maybe "" id mmod ++ "#" ++ ident)
    , Html.theclass "varid-reference"
    ]
  where
    mmod = GHC.moduleNameString . GHC.moduleName <$> GHC.nameModule_maybe name
    ident = GHC.occNameString . GHC.nameOccName $ name
