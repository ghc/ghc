module Haddock.Backends.Hyperlinker.Renderer (render) where

import Haddock.Backends.Hyperlinker.Parser
import Haddock.Backends.Hyperlinker.Ast
import Haddock.Backends.Hyperlinker.Utils
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils

import qualified GHC
import qualified Name as GHC
import qualified Unique as GHC

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid

import Text.XHtml (Html, HtmlAttr, (!))
import qualified Text.XHtml as Html

type StyleClass = String

render :: Maybe FilePath -> Maybe FilePath -> SourceURLs -> [RichToken] -> Html
render mcss mjs urls tokens = header mcss mjs <> body urls tokens

body :: SourceURLs -> [RichToken] -> Html
body urls = Html.body . Html.pre . mconcat . map (richToken urls)

header :: Maybe FilePath -> Maybe FilePath -> Html
header mcss mjs
    | isNothing mcss && isNothing mjs = Html.noHtml
header mcss mjs =
    Html.header $ css mcss <> js mjs
  where
    css Nothing = Html.noHtml
    css (Just cssFile) = Html.thelink Html.noHtml !
        [ Html.rel "stylesheet"
        , Html.thetype "text/css"
        , Html.href cssFile
        ]
    js Nothing = Html.noHtml
    js (Just jsFile) = Html.script Html.noHtml !
        [ Html.thetype "text/javascript"
        , Html.src jsFile
        ]

richToken :: SourceURLs -> RichToken -> Html
richToken _ (RichToken tok Nothing) =
    tokenSpan tok ! attrs
  where
    attrs = [ multiclass . tokenStyle . tkType $ tok ]
richToken urls (RichToken tok (Just det)) =
    externalAnchor det . internalAnchor det . hyperlink urls det $ content
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

hyperlink :: SourceURLs -> TokenDetails -> Html -> Html
hyperlink urls details = case rtkName details of
    Left name ->
        if GHC.isInternalName name
        then internalHyperlink name
        else externalNameHyperlink urls name
    Right name -> externalModHyperlink name

internalHyperlink :: GHC.Name -> Html -> Html
internalHyperlink name content =
    Html.anchor content ! [ Html.href $ "#" ++ internalAnchorIdent name ]

externalNameHyperlink :: SourceURLs -> GHC.Name -> Html -> Html
externalNameHyperlink urls name =
    case Map.lookup key $ srcNameUrlMap urls of
        Just url -> externalNameHyperlink' url name
        Nothing -> id
  where
    key = GHC.modulePackageKey . GHC.nameModule $ name

externalNameHyperlink' :: String -> GHC.Name -> Html -> Html
externalNameHyperlink' url name content =
    Html.anchor content ! [ Html.href $ href ]
  where
    mdl = GHC.nameModule name
    href = spliceURL Nothing (Just mdl) (Just name) Nothing url

externalModHyperlink :: GHC.ModuleName -> Html -> Html
externalModHyperlink _ = id -- TODO
