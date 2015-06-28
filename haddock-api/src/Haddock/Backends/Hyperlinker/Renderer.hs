module Haddock.Backends.Hyperlinker.Renderer (render) where

import Haddock.Types
import Haddock.Backends.Hyperlinker.Parser
import Haddock.Backends.Hyperlinker.Ast
import Haddock.Backends.Hyperlinker.Utils

import qualified GHC
import qualified Name as GHC
import qualified Unique as GHC

import System.FilePath.Posix ((</>))

import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Map as Map

import Text.XHtml (Html, HtmlAttr, (!))
import qualified Text.XHtml as Html

type StyleClass = String

render :: Maybe FilePath -> Maybe FilePath
       -> GHC.PackageKey -> SrcMap -> [RichToken]
       -> Html
render mcss mjs pkg srcs tokens = header mcss mjs <> body pkg srcs tokens

body :: GHC.PackageKey -> SrcMap -> [RichToken] -> Html
body pkg srcs = Html.body . Html.pre . mconcat . map (richToken pkg srcs)

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
    js (Just scriptFile) = Html.script Html.noHtml !
        [ Html.thetype "text/javascript"
        , Html.src scriptFile
        ]

richToken :: GHC.PackageKey -> SrcMap -> RichToken -> Html
richToken _ _ (RichToken tok Nothing) =
    tokenSpan tok ! attrs
  where
    attrs = [ multiclass . tokenStyle . tkType $ tok ]
richToken pkg srcs (RichToken tok (Just det)) =
    externalAnchor det . internalAnchor det . hyperlink pkg srcs det $ content
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
externalAnchorIdent = hypSrcNameUrl

internalAnchorIdent :: GHC.Name -> String
internalAnchorIdent = ("local-" ++) . show . GHC.getKey . GHC.nameUnique

hyperlink :: GHC.PackageKey -> SrcMap -> TokenDetails -> Html -> Html
hyperlink pkg srcs details = case rtkName details of
    Left name ->
        if GHC.isInternalName name
        then internalHyperlink name
        else externalNameHyperlink pkg srcs name
    Right name -> externalModHyperlink name

internalHyperlink :: GHC.Name -> Html -> Html
internalHyperlink name content =
    Html.anchor content ! [ Html.href $ "#" ++ internalAnchorIdent name ]

externalNameHyperlink :: GHC.PackageKey -> SrcMap -> GHC.Name -> Html -> Html
externalNameHyperlink pkg srcs name content
    | namePkg == pkg = Html.anchor content !
        [ Html.href $ hypSrcModuleNameUrl mdl name ]
    | Just path <- Map.lookup namePkg srcs = Html.anchor content !
        [ Html.href $ path </> hypSrcModuleNameUrl mdl name ]
    | otherwise = content
  where
    mdl = GHC.nameModule name
    namePkg = GHC.modulePackageKey mdl

externalModHyperlink :: GHC.ModuleName -> Html -> Html
externalModHyperlink mdl content =
    Html.anchor content ! [ Html.href $ hypSrcModuleUrl' mdl ]
