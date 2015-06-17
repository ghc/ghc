module Haddock.Backends.Hyperlinker.Renderer (render) where

import Haddock.Backends.Hyperlinker.Parser
import Haddock.Backends.Hyperlinker.Ast

import qualified GHC
import qualified Name as GHC
import qualified Unique as GHC

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
    externalAnchor det . internalAnchor det . hyperlink det $ content
  where
    content = tokenSpan tok ! [ multiclass style]
    style = (tokenStyle . tkType) tok ++ richTokenStyle det

tokenSpan :: Token -> Html
tokenSpan = Html.thespan . Html.toHtml . tkValue

richTokenStyle :: TokenDetails -> [StyleClass]
richTokenStyle (RtkVar _) = ["hs-var"]
richTokenStyle (RtkType _) = ["hs-type"]
richTokenStyle _ = []

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

externalAnchor :: TokenDetails -> Html -> Html
externalAnchor (RtkDecl name) content =
    Html.anchor content ! [ Html.name $ externalAnchorIdent name ]
externalAnchor _ content = content

internalAnchor :: TokenDetails -> Html -> Html
internalAnchor (RtkBind name) content =
    Html.anchor content ! [ Html.name $ internalAnchorIdent name ]
internalAnchor _ content = content

externalAnchorIdent :: GHC.Name -> String
externalAnchorIdent = GHC.occNameString . GHC.nameOccName

internalAnchorIdent :: GHC.Name -> String
internalAnchorIdent = ("local-" ++) . show . GHC.getKey . GHC.nameUnique

hyperlink :: TokenDetails -> Html -> Html
hyperlink details =
    if GHC.isInternalName $ name
    then internalHyperlink name
    else externalHyperlink name
  where
    name = rtkName details

internalHyperlink :: GHC.Name -> Html -> Html
internalHyperlink name content =
    Html.anchor content ! [ Html.href $ "#" ++ internalAnchorIdent name ]

externalHyperlink :: GHC.Name -> Html -> Html
externalHyperlink name content =
    Html.anchor content ! [ Html.href $ maybe "" id mmod ++ ".html#" ++ ident ]
  where
    mmod = GHC.moduleNameString . GHC.moduleName <$> GHC.nameModule_maybe name
    ident = externalAnchorIdent name
