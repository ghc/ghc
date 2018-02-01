{-# LANGUAGE RecordWildCards #-}


module Haddock.Backends.Hyperlinker.Renderer (render) where


import Haddock.Backends.Hyperlinker.Types
import Haddock.Backends.Hyperlinker.Utils

import qualified GHC
import qualified Name as GHC
import qualified Unique as GHC

import System.FilePath.Posix ((</>))

import Data.List
import Data.Maybe
import qualified Data.Map as Map

import Text.XHtml (Html, HtmlAttr, (!))
import qualified Text.XHtml as Html


type StyleClass = String


render :: Maybe FilePath -> Maybe FilePath -> SrcMap -> [RichToken]
       -> Html
render mcss mjs srcs tokens = header mcss mjs <> body srcs tokens


data TokenGroup
    = GrpNormal Token
    | GrpRich TokenDetails [Token]


-- | Group consecutive tokens pointing to the same element.
--
-- We want to render qualified identifiers as one entity. For example,
-- @Bar.Baz.foo@ consists of 5 tokens (@Bar@, @.@, @Baz@, @.@, @foo@) but for
-- better user experience when highlighting and clicking links, these tokens
-- should be regarded as one identifier. Therefore, before rendering we must
-- group consecutive elements pointing to the same 'GHC.Name' (note that even
-- dot token has it if it is part of qualified name).
groupTokens :: [RichToken] -> [TokenGroup]
groupTokens [] = []
groupTokens ((RichToken tok Nothing):rest) = (GrpNormal tok):(groupTokens rest)
groupTokens ((RichToken tok (Just det)):rest) =
    let (grp, rest') = span same rest
    in (GrpRich det (tok:(map rtkToken grp))):(groupTokens rest')
  where
    same (RichToken _ (Just det')) = det == det'
    same _ = False


body :: SrcMap -> [RichToken] -> Html
body srcs tokens =
    Html.body . Html.pre $ hypsrc
  where
    hypsrc = mconcat . map (tokenGroup srcs) . groupTokens $ tokens


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


tokenGroup :: SrcMap -> TokenGroup -> Html
tokenGroup _ (GrpNormal tok@(Token { .. }))
    | tkType == TkSpace = renderSpace (GHC.srcSpanStartLine tkSpan) tkValue
    | otherwise = tokenSpan tok ! attrs
  where
    attrs = [ multiclass . tokenStyle $ tkType ]
tokenGroup srcs (GrpRich det tokens) =
    externalAnchor det . internalAnchor det . hyperlink srcs det $ content
  where
    content = mconcat . map (richToken det) $ tokens


richToken :: TokenDetails -> Token -> Html
richToken det tok =
    tokenSpan tok ! [ multiclass style ]
  where
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

hyperlink :: SrcMap -> TokenDetails -> Html -> Html
hyperlink srcs details = case rtkName details of
    Left name ->
        if GHC.isInternalName name
        then internalHyperlink name
        else externalNameHyperlink srcs name
    Right name -> externalModHyperlink srcs name

internalHyperlink :: GHC.Name -> Html -> Html
internalHyperlink name content =
    Html.anchor content ! [ Html.href $ "#" ++ internalAnchorIdent name ]

externalNameHyperlink :: SrcMap -> GHC.Name -> Html -> Html
externalNameHyperlink srcs name content = case Map.lookup mdl srcs of
    Just SrcLocal -> Html.anchor content !
        [ Html.href $ hypSrcModuleNameUrl mdl name ]
    Just (SrcExternal path) -> Html.anchor content !
        [ Html.href $ path </> hypSrcModuleNameUrl mdl name ]
    Nothing -> content
  where
    mdl = GHC.nameModule name

externalModHyperlink :: SrcMap -> GHC.ModuleName -> Html -> Html
externalModHyperlink srcs name content =
    let srcs' = Map.mapKeys GHC.moduleName srcs in
    case Map.lookup name srcs' of
      Just SrcLocal -> Html.anchor content !
        [ Html.href $ hypSrcModuleUrl' name ]
      Just (SrcExternal path) -> Html.anchor content !
        [ Html.href $ path </> hypSrcModuleUrl' name ]
      Nothing -> content


renderSpace :: Int -> String -> Html
renderSpace _ [] = Html.noHtml
renderSpace line ('\n':rest) = mconcat
    [ Html.thespan . Html.toHtml $ "\n"
    , lineAnchor (line + 1)
    , renderSpace (line + 1) rest
    ]
renderSpace line space =
    let (hspace, rest) = span (/= '\n') space
    in (Html.thespan . Html.toHtml) hspace <> renderSpace line rest


lineAnchor :: Int -> Html
lineAnchor line = Html.anchor Html.noHtml ! [ Html.name $ hypSrcLineUrl line ]
