{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Haddock.Backends.Hyperlinker.Renderer (render) where

import Haddock.Backends.Hyperlinker.Types
import Haddock.Backends.Hyperlinker.Utils

import qualified Data.ByteString as BS

import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils (emptyNodeInfo, isEvidenceContext)
import GHC.Types.Name (Name, getOccString, isInternalName, nameModule, nameUnique)
import GHC.Types.SrcLoc
import GHC.Types.Unique (getKey)
import GHC.Unit.Module (ModuleName, moduleNameString)
import GHC.Utils.Encoding (utf8DecodeByteString)

import System.FilePath.Posix ((</>))

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.XHtml (Html, HtmlAttr, (!))
import qualified Text.XHtml as Html

type StyleClass = String

-- | Produce the HTML corresponding to a hyperlinked Haskell source
render
  :: Maybe FilePath
  -- ^ path to the CSS file
  -> Maybe FilePath
  -- ^ path to the JS file
  -> SrcMaps
  -- ^ Paths to sources
  -> HieAST PrintedType
  -- ^ ASTs from @.hie@ files
  -> [Token]
  -- ^ tokens to render
  -> Html
render mcss mjs srcs ast tokens = header mcss mjs <> body srcs ast tokens

body :: SrcMaps -> HieAST PrintedType -> [Token] -> Html
body srcs ast tokens = Html.body . Html.pre $ hypsrc
  where
    hypsrc = renderWithAst srcs ast tokens

header :: Maybe FilePath -> Maybe FilePath -> Html
header Nothing Nothing = Html.noHtml
header mcss mjs = Html.header $ css mcss <> js mjs
  where
    css Nothing = Html.noHtml
    css (Just cssFile) =
      Html.thelink Html.noHtml
        ! [ Html.rel "stylesheet"
          , Html.thetype "text/css"
          , Html.href cssFile
          ]
    js Nothing = Html.noHtml
    js (Just scriptFile) =
      Html.script Html.noHtml
        ! [ Html.thetype "text/javascript"
          , Html.src scriptFile
          ]

splitTokens :: HieAST PrintedType -> [Token] -> ([Token], [Token], [Token])
splitTokens ast toks = (before, during, after)
  where
    (before, rest) = span leftOf toks
    (during, after) = span inAst rest
    leftOf t = realSrcSpanEnd (tkSpan t) <= realSrcSpanStart nodeSp
    inAst t = nodeSp `containsSpan` tkSpan t
    nodeSp = nodeSpan ast

-- | Turn a list of tokens into hyperlinked sources, threading in relevant link
-- information from the 'HieAST'.
renderWithAst :: SrcMaps -> HieAST PrintedType -> [Token] -> Html
renderWithAst srcs Node{..} toks = anchored $ case toks of
  [tok] | nodeSpan == tkSpan tok -> richToken srcs nodeInfo tok
  -- NB: the GHC lexer lexes backquoted identifiers and parenthesized operators
  -- as multiple tokens.
  --
  --  * @a `elem` b@ turns into @[a, `, elem, `, b]@ (excluding space tokens)
  --  * @(+) 1 2@    turns into @[(, +, ), 1, 2]@    (excluding space tokens)
  --
  -- However, the HIE ast considers @`elem`@ and @(+)@ to be single nodes. In
  -- order to make sure these get hyperlinked properly, we intercept these
  -- special sequences of tokens and merge them into just one identifier or
  -- operator token.
  [BacktickTok s1, tok@Token{tkType = TkIdentifier}, BacktickTok s2]
    | realSrcSpanStart s1 == realSrcSpanStart nodeSpan
    , realSrcSpanEnd s2 == realSrcSpanEnd nodeSpan ->
        richToken
          srcs
          nodeInfo
          ( Token
              { tkValue = "`" <> tkValue tok <> "`"
              , tkType = TkOperator
              , tkSpan = nodeSpan
              }
          )
  [OpenParenTok s1, tok@Token{tkType = TkOperator}, CloseParenTok s2]
    | realSrcSpanStart s1 == realSrcSpanStart nodeSpan
    , realSrcSpanEnd s2 == realSrcSpanEnd nodeSpan ->
        richToken
          srcs
          nodeInfo
          ( Token
              { tkValue = "(" <> tkValue tok <> ")"
              , tkType = TkOperator
              , tkSpan = nodeSpan
              }
          )
  _ -> go nodeChildren toks
  where
    nodeInfo = maybe emptyNodeInfo id (Map.lookup SourceInfo $ getSourcedNodeInfo sourcedNodeInfo)
    go _ [] = mempty
    go [] xs = foldMap renderToken xs
    go (cur : rest) xs =
      foldMap renderToken before <> renderWithAst srcs cur during <> go rest after
      where
        (before, during, after) = splitTokens cur xs
    anchored c = Map.foldrWithKey anchorOne c (nodeIdentifiers nodeInfo)
    anchorOne n dets c = externalAnchor n d $ internalAnchor n d c
      where
        d = identInfo dets

renderToken :: Token -> Html
renderToken Token{..}
  | BS.null tkValue = mempty
  | tkType == TkSpace = renderSpace (srcSpanStartLine tkSpan) tkValue'
  | otherwise = tokenSpan ! [multiclass style]
  where
    tkValue' = filterCRLF $ utf8DecodeByteString tkValue
    style = tokenStyle tkType
    tokenSpan = Html.thespan (Html.toHtml tkValue')

-- | Given information about the source position of definitions, render a token
richToken :: SrcMaps -> NodeInfo PrintedType -> Token -> Html
richToken srcs details Token{..}
  | tkType == TkSpace = renderSpace (srcSpanStartLine tkSpan) tkValue'
  | otherwise = annotate details $ linked content
  where
    tkValue' = filterCRLF $ utf8DecodeByteString tkValue
    content = tokenSpan ! [multiclass style]
    tokenSpan = Html.thespan (Html.toHtml tkValue')
    style = tokenStyle tkType ++ concatMap (richTokenStyle (null (nodeType details))) contexts

    contexts = concatMap (Set.elems . identInfo) . Map.elems . nodeIdentifiers $ details

    -- pick an arbitrary non-evidence identifier to hyperlink with
    identDet = Map.lookupMin $ Map.filter notEvidence $ nodeIdentifiers details
    notEvidence = not . any isEvidenceContext . identInfo

    -- If we have name information, we can make links
    linked = case identDet of
      Just (n, _) -> hyperlink srcs n
      Nothing -> id

-- | Remove CRLFs from source
filterCRLF :: String -> String
filterCRLF ('\r' : '\n' : cs) = '\n' : filterCRLF cs
filterCRLF (c : cs) = c : filterCRLF cs
filterCRLF [] = []

annotate :: NodeInfo PrintedType -> Html -> Html
annotate ni content =
  Html.thespan (annot <> content) ! [Html.theclass "annot"]
  where
    annot
      | not (null annotation) =
          Html.thespan (Html.toHtml annotation) ! [Html.theclass "annottext"]
      | otherwise = mempty
    annotation = typ ++ identTyps
    typ = unlines (nodeType ni)
    typedIdents =
      [ (n, t) | (n, c@(identType -> Just t)) <- Map.toList $ nodeIdentifiers ni, not (any isEvidenceContext $ identInfo c)
      ]
    identTyps
      | length typedIdents > 1 || null (nodeType ni) =
          concatMap (\(n, t) -> printName n ++ " :: " ++ t ++ "\n") typedIdents
      | otherwise = ""

    printName :: Either ModuleName Name -> String
    printName = either moduleNameString getOccString

richTokenStyle
  :: Bool
  -- ^ are we lacking a type annotation?
  -> ContextInfo
  -- ^ in what context did this token show up?
  -> [StyleClass]
richTokenStyle True Use = ["hs-type"]
richTokenStyle False Use = ["hs-var"]
richTokenStyle _ RecField{} = ["hs-var"]
richTokenStyle _ PatternBind{} = ["hs-var"]
richTokenStyle _ MatchBind{} = ["hs-var"]
richTokenStyle _ TyVarBind{} = ["hs-type"]
richTokenStyle _ ValBind{} = ["hs-var"]
richTokenStyle _ TyDecl = ["hs-type"]
richTokenStyle _ ClassTyDecl{} = ["hs-type"]
richTokenStyle _ Decl{} = ["hs-var"]
richTokenStyle _ IEThing{} = [] -- could be either a value or type
richTokenStyle _ EvidenceVarBind{} = []
richTokenStyle _ EvidenceVarUse{} = []

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
multiclass = Html.theclass . unwords

externalAnchor :: Identifier -> Set.Set ContextInfo -> Html -> Html
externalAnchor (Right name) contexts content
  | not (isInternalName name)
  , any isBinding contexts =
      Html.thespan content ! [Html.identifier $ externalAnchorIdent name]
externalAnchor _ _ content = content

isBinding :: ContextInfo -> Bool
isBinding (ValBind RegularBind _ _) = True
isBinding PatternBind{} = True
isBinding Decl{} = True
isBinding (RecField RecFieldDecl _) = True
isBinding TyVarBind{} = True
isBinding ClassTyDecl{} = True
isBinding _ = False

internalAnchor :: Identifier -> Set.Set ContextInfo -> Html -> Html
internalAnchor (Right name) contexts content
  | isInternalName name
  , any isBinding contexts =
      Html.thespan content ! [Html.identifier $ internalAnchorIdent name]
internalAnchor _ _ content = content

externalAnchorIdent :: Name -> String
externalAnchorIdent = hypSrcNameUrl

internalAnchorIdent :: Name -> String
internalAnchorIdent = ("local-" ++) . show . getKey . nameUnique

-- | Generate the HTML hyperlink for an identifier
hyperlink :: SrcMaps -> Identifier -> Html -> Html
hyperlink (srcs, srcs') ident = case ident of
  Right name
    | isInternalName name -> internalHyperlink name
    | otherwise -> externalNameHyperlink name
  Left name -> externalModHyperlink name
  where
    -- In a Nix environment, we have file:// URLs with absolute paths
    makeHyperlinkUrl url | List.isPrefixOf "file://" url = url
    makeHyperlinkUrl url = ".." </> url

    internalHyperlink name content =
      Html.anchor content ! [Html.href $ "#" ++ internalAnchorIdent name]

    externalNameHyperlink name content = case Map.lookup mdl srcs of
      Just SrcLocal ->
        Html.anchor content
          ! [Html.href $ hypSrcModuleNameUrl mdl name]
      Just (SrcExternal path) ->
        let hyperlinkUrl = makeHyperlinkUrl path </> hypSrcModuleNameUrl mdl name
         in Html.anchor content
              ! [Html.href $ spliceURL (Just mdl) (Just name) Nothing hyperlinkUrl]
      Nothing -> content
      where
        mdl = nameModule name

    externalModHyperlink moduleName content =
      case Map.lookup moduleName srcs' of
        Just SrcLocal ->
          Html.anchor content
            ! [Html.href $ hypSrcModuleUrl' moduleName]
        Just (SrcExternal path) ->
          let hyperlinkUrl = makeHyperlinkUrl path </> hypSrcModuleUrl' moduleName
           in Html.anchor content
                ! [Html.href $ spliceURL' (Just moduleName) Nothing Nothing hyperlinkUrl]
        Nothing -> content

renderSpace :: Int -> String -> Html
renderSpace !_ "" = Html.noHtml
renderSpace !line ('\n' : rest) =
  mconcat
    [ Html.thespan (Html.toHtml '\n')
    , lineAnchor (line + 1)
    , renderSpace (line + 1) rest
    ]
renderSpace line space =
  let (hspace, rest) = span (/= '\n') space
   in (Html.thespan . Html.toHtml) hspace <> renderSpace line rest

lineAnchor :: Int -> Html
lineAnchor line = Html.thespan Html.noHtml ! [Html.identifier $ hypSrcLineUrl line]
