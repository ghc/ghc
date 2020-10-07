{-# LANGUAGE DeriveDataTypeable #-}

module GHC.Parser.Annotation (
  getAnnotation, getAndRemoveAnnotation,
  getAnnotationComments,getAndRemoveAnnotationComments,
  ApiAnns(..),
  ApiAnnKey,
  AnnKeywordId(..),
  AddAnn(..),mkParensApiAnn,
  AnnotationComment(..),
  IsUnicodeSyntax(..),
  unicodeAnn,
  HasE(..),
  LRdrName -- Exists for haddocks only
  ) where

import GHC.Prelude

import GHC.Types.Name.Reader
import GHC.Utils.Outputable
import GHC.Types.SrcLoc
import qualified Data.Map as Map
import Data.Data


{-
Note [Api annotations]
~~~~~~~~~~~~~~~~~~~~~~
Given a parse tree of a Haskell module, how can we reconstruct
the original Haskell source code, retaining all whitespace and
source code comments?  We need to track the locations of all
elements from the original source: this includes keywords such as
'let' / 'in' / 'do' etc as well as punctuation such as commas and
braces, and also comments.  We collectively refer to this
metadata as the "API annotations".

Rather than annotate the resulting parse tree with these locations
directly (this would be a major change to some fairly core data
structures in GHC), we instead capture locations for these elements in a
structure separate from the parse tree, and returned in the
pm_annotations field of the ParsedModule type.

The full ApiAnns type is

> data ApiAnns =
>  ApiAnns
>    { apiAnnItems :: Map.Map ApiAnnKey [RealSrcSpan],
>      apiAnnEofPos :: Maybe RealSrcSpan,
>      apiAnnComments :: Map.Map RealSrcSpan [RealLocated AnnotationComment],
>      apiAnnRogueComments :: [RealLocated AnnotationComment]
>    }

NON-COMMENT ELEMENTS

Intuitively, every AST element directly contains a bag of keywords
(keywords can show up more than once in a node: a semicolon i.e. newline
can show up multiple times before the next AST element), each of which
needs to be associated with its location in the original source code.

Consequently, the structure that records non-comment elements is logically
a two level map, from the RealSrcSpan of the AST element containing it, to
a map from keywords ('AnnKeyWord') to all locations of the keyword directly
in the AST element:

> type ApiAnnKey = (RealSrcSpan,AnnKeywordId)
>
> Map.Map ApiAnnKey [RealSrcSpan]

So

> let x = 1 in 2 *x

would result in the AST element

  L span (HsLet (binds for x = 1) (2 * x))

and the annotations

  (span,AnnLet) having the location of the 'let' keyword
  (span,AnnEqual) having the location of the '=' sign
  (span,AnnIn)  having the location of the 'in' keyword

For any given element in the AST, there is only a set number of
keywords that are applicable for it (e.g., you'll never see an
'import' keyword associated with a let-binding.)  The set of allowed
keywords is documented in a comment associated with the constructor
of a given AST element, although the ground truth is in GHC.Parser
and GHC.Parser.PostProcess (which actually add the annotations; see #13012).

COMMENT ELEMENTS

Every comment is associated with a *located* AnnotationComment.
We associate comments with the lowest (most specific) AST element
enclosing them:

> Map.Map RealSrcSpan [RealLocated AnnotationComment]

PARSER STATE

There are three fields in PState (the parser state) which play a role
with annotations.

>  annotations :: [(ApiAnnKey,[RealSrcSpan])],
>  comment_q :: [RealLocated AnnotationComment],
>  annotations_comments :: [(RealSrcSpan,[RealLocated AnnotationComment])]

The 'annotations' and 'annotations_comments' fields are simple: they simply
accumulate annotations that will end up in 'ApiAnns' at the end
(after they are passed to Map.fromList).

The 'comment_q' field captures comments as they are seen in the token stream,
so that when they are ready to be allocated via the parser they are
available (at the time we lex a comment, we don't know what the enclosing
AST node of it is, so we can't associate it with a RealSrcSpan in
annotations_comments).

PARSER EMISSION OF ANNOTATIONS

The parser interacts with the lexer using the function

> addAnnotation :: RealSrcSpan -> AnnKeywordId -> RealSrcSpan -> P ()

which takes the AST element RealSrcSpan, the annotation keyword and the
target RealSrcSpan.

This adds the annotation to the `annotations` field of `PState` and
transfers any comments in `comment_q` WHICH ARE ENCLOSED by
the RealSrcSpan of this element to the `annotations_comments`
field.  (Comments which are outside of this annotation are deferred
until later. 'allocateComments' in 'Lexer' is responsible for
making sure we only attach comments that actually fit in the 'SrcSpan'.)

The wiki page describing this feature is
https://gitlab.haskell.org/ghc/ghc/wikis/api-annotations

-}
-- ---------------------------------------------------------------------

-- If you update this, update the Note [Api annotations] above
data ApiAnns =
  ApiAnns
    { apiAnnItems :: Map.Map ApiAnnKey [RealSrcSpan],
      apiAnnEofPos :: Maybe RealSrcSpan,
      apiAnnComments :: Map.Map RealSrcSpan [RealLocated AnnotationComment],
      apiAnnRogueComments :: [RealLocated AnnotationComment]
    }

-- If you update this, update the Note [Api annotations] above
type ApiAnnKey = (RealSrcSpan,AnnKeywordId)


-- ---------------------------------------------------------------------

-- | Encapsulated call to addAnnotation, requiring only the SrcSpan of
--   the AST construct the annotation belongs to; together with the
--   AnnKeywordId, this is the key of the annotation map.
--
--   This type is useful for places in the parser where it is not yet
--   known what SrcSpan an annotation should be added to.  The most
--   common situation is when we are parsing a list: the annotations
--   need to be associated with the AST element that *contains* the
--   list, not the list itself.  'AddAnn' lets us defer adding the
--   annotations until we finish parsing the list and are now parsing
--   the enclosing element; we then apply the 'AddAnn' to associate
--   the annotations.  Another common situation is where a common fragment of
--   the AST has been factored out but there is no separate AST node for
--   this fragment (this occurs in class and data declarations). In this
--   case, the annotation belongs to the parent data declaration.
--
--   The usual way an 'AddAnn' is created is using the 'mj' ("make jump")
--   function, and then it can be discharged using the 'ams' function.
data AddAnn = AddAnn AnnKeywordId SrcSpan

-- |Given a 'SrcSpan' that surrounds a 'HsPar' or 'HsParTy', generate
-- 'AddAnn' values for the opening and closing bordering on the start
-- and end of the span
mkParensApiAnn :: SrcSpan -> [AddAnn]
mkParensApiAnn (UnhelpfulSpan _)  = []
mkParensApiAnn (RealSrcSpan ss _) = [AddAnn AnnOpenP lo,AddAnn AnnCloseP lc]
  where
    f = srcSpanFile ss
    sl = srcSpanStartLine ss
    sc = srcSpanStartCol ss
    el = srcSpanEndLine ss
    ec = srcSpanEndCol ss
    lo = RealSrcSpan (mkRealSrcSpan (realSrcSpanStart ss)        (mkRealSrcLoc f sl (sc+1))) Nothing
    lc = RealSrcSpan (mkRealSrcSpan (mkRealSrcLoc f el (ec - 1)) (realSrcSpanEnd ss))        Nothing

-- ---------------------------------------------------------------------
-- | Retrieve a list of annotation 'SrcSpan's based on the 'SrcSpan'
-- of the annotated AST element, and the known type of the annotation.
getAnnotation :: ApiAnns -> RealSrcSpan -> AnnKeywordId -> [RealSrcSpan]
getAnnotation anns span ann =
  case Map.lookup ann_key ann_items of
    Nothing -> []
    Just ss -> ss
  where ann_items = apiAnnItems anns
        ann_key = (span,ann)

-- | Retrieve a list of annotation 'SrcSpan's based on the 'SrcSpan'
-- of the annotated AST element, and the known type of the annotation.
-- The list is removed from the annotations.
getAndRemoveAnnotation :: ApiAnns -> RealSrcSpan -> AnnKeywordId
                       -> ([RealSrcSpan],ApiAnns)
getAndRemoveAnnotation anns span ann =
  case Map.lookup ann_key ann_items of
    Nothing -> ([],anns)
    Just ss -> (ss,anns{ apiAnnItems = Map.delete ann_key ann_items })
  where ann_items = apiAnnItems anns
        ann_key = (span,ann)

-- |Retrieve the comments allocated to the current 'SrcSpan'
--
--  Note: A given 'SrcSpan' may appear in multiple AST elements,
--  beware of duplicates
getAnnotationComments :: ApiAnns -> RealSrcSpan -> [RealLocated AnnotationComment]
getAnnotationComments anns span =
  case Map.lookup span (apiAnnComments anns) of
    Just cs -> cs
    Nothing -> []

-- |Retrieve the comments allocated to the current 'SrcSpan', and
-- remove them from the annotations
getAndRemoveAnnotationComments :: ApiAnns -> RealSrcSpan
                               -> ([RealLocated AnnotationComment],ApiAnns)
getAndRemoveAnnotationComments anns span =
  case Map.lookup span ann_comments of
    Just cs -> (cs, anns{ apiAnnComments = Map.delete span ann_comments })
    Nothing -> ([], anns)
  where ann_comments = apiAnnComments anns

-- --------------------------------------------------------------------

-- | API Annotations exist so that tools can perform source to source
-- conversions of Haskell code. They are used to keep track of the
-- various syntactic keywords that are not captured in the existing
-- AST.
--
-- The annotations, together with original source comments are made
-- available in the @'pm_annotations'@ field of @'GHC.ParsedModule'@.
-- Comments are only retained if @'Opt_KeepRawTokenStream'@ is set in
-- @'GHC.Driver.Session.DynFlags'@ before parsing.
--
-- The wiki page describing this feature is
-- https://gitlab.haskell.org/ghc/ghc/wikis/api-annotations
--
-- Note: in general the names of these are taken from the
-- corresponding token, unless otherwise noted
-- See note [Api annotations] above for details of the usage
data AnnKeywordId
    = AnnAnyclass
    | AnnAs
    | AnnAt
    | AnnBang  -- ^ '!'
    | AnnBackquote -- ^ '`'
    | AnnBy
    | AnnCase -- ^ case or lambda case
    | AnnClass
    | AnnClose -- ^  '\#)' or '\#-}'  etc
    | AnnCloseB -- ^ '|)'
    | AnnCloseBU -- ^ '|)', unicode variant
    | AnnCloseC -- ^ '}'
    | AnnCloseQ  -- ^ '|]'
    | AnnCloseQU -- ^ '|]', unicode variant
    | AnnCloseP -- ^ ')'
    | AnnCloseS -- ^ ']'
    | AnnColon
    | AnnComma -- ^ as a list separator
    | AnnCommaTuple -- ^ in a RdrName for a tuple
    | AnnDarrow -- ^ '=>'
    | AnnDarrowU -- ^ '=>', unicode variant
    | AnnData
    | AnnDcolon -- ^ '::'
    | AnnDcolonU -- ^ '::', unicode variant
    | AnnDefault
    | AnnDeriving
    | AnnDo
    | AnnDot    -- ^ '.'
    | AnnDotdot -- ^ '..'
    | AnnElse
    | AnnEqual
    | AnnExport
    | AnnFamily
    | AnnForall
    | AnnForallU -- ^ Unicode variant
    | AnnForeign
    | AnnFunId -- ^ for function name in matches where there are
               -- multiple equations for the function.
    | AnnGroup
    | AnnHeader -- ^ for CType
    | AnnHiding
    | AnnIf
    | AnnImport
    | AnnIn
    | AnnInfix -- ^ 'infix' or 'infixl' or 'infixr'
    | AnnInstance
    | AnnLam
    | AnnLarrow     -- ^ '<-'
    | AnnLarrowU    -- ^ '<-', unicode variant
    | AnnLet
    | AnnLollyU     -- ^ The '⊸' unicode arrow
    | AnnMdo
    | AnnMinus -- ^ '-'
    | AnnModule
    | AnnPercentOne -- ^ '%1' -- for HsLinearArrow
    | AnnNewtype
    | AnnName -- ^ where a name loses its location in the AST, this carries it
    | AnnOf
    | AnnOpen    -- ^ '(\#' or '{-\# LANGUAGE' etc
    | AnnOpenB   -- ^ '(|'
    | AnnOpenBU  -- ^ '(|', unicode variant
    | AnnOpenC   -- ^ '{'
    | AnnOpenE   -- ^ '[e|' or '[e||'
    | AnnOpenEQ  -- ^ '[|'
    | AnnOpenEQU -- ^ '[|', unicode variant
    | AnnOpenP   -- ^ '('
    | AnnOpenS   -- ^ '['
    | AnnDollar          -- ^ prefix '$'   -- TemplateHaskell
    | AnnDollarDollar    -- ^ prefix '$$'  -- TemplateHaskell
    | AnnPackageName
    | AnnPattern
    | AnnPercent -- ^ '%' -- for HsExplicitMult
    | AnnProc
    | AnnQualified
    | AnnRarrow -- ^ '->'
    | AnnRarrowU -- ^ '->', unicode variant
    | AnnRec
    | AnnRole
    | AnnSafe
    | AnnSemi -- ^ ';'
    | AnnSimpleQuote -- ^ '''
    | AnnSignature
    | AnnStatic -- ^ 'static'
    | AnnStock
    | AnnThen
    | AnnThTyQuote -- ^ double '''
    | AnnTilde -- ^ '~'
    | AnnType
    | AnnUnit -- ^ '()' for types
    | AnnUsing
    | AnnVal  -- ^ e.g. INTEGER
    | AnnValStr  -- ^ String value, will need quotes when output
    | AnnVbar -- ^ '|'
    | AnnVia -- ^ 'via'
    | AnnWhere
    | Annlarrowtail -- ^ '-<'
    | AnnlarrowtailU -- ^ '-<', unicode variant
    | Annrarrowtail -- ^ '->'
    | AnnrarrowtailU -- ^ '->', unicode variant
    | AnnLarrowtail -- ^ '-<<'
    | AnnLarrowtailU -- ^ '-<<', unicode variant
    | AnnRarrowtail -- ^ '>>-'
    | AnnRarrowtailU -- ^ '>>-', unicode variant
    deriving (Eq, Ord, Data, Show)

instance Outputable AnnKeywordId where
  ppr x = text (show x)

-- ---------------------------------------------------------------------

data AnnotationComment =
  -- Documentation annotations
    AnnDocCommentNext  String     -- ^ something beginning '-- |'
  | AnnDocCommentPrev  String     -- ^ something beginning '-- ^'
  | AnnDocCommentNamed String     -- ^ something beginning '-- $'
  | AnnDocSection      Int String -- ^ a section heading
  | AnnDocOptions      String     -- ^ doc options (prune, ignore-exports, etc)
  | AnnLineComment     String     -- ^ comment starting by "--"
  | AnnBlockComment    String     -- ^ comment in {- -}
    deriving (Eq, Ord, Data, Show)
-- Note: these are based on the Token versions, but the Token type is
-- defined in GHC.Parser.Lexer and bringing it in here would create a loop

instance Outputable AnnotationComment where
  ppr x = text (show x)

-- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
--             'GHC.Parser.Annotation.AnnClose','GHC.Parser.Annotation.AnnComma',
--             'GHC.Parser.Annotation.AnnRarrow'
--             'GHC.Parser.Annotation.AnnTilde'
--   - May have 'GHC.Parser.Annotation.AnnComma' when in a list
type LRdrName = Located RdrName


-- | Certain tokens can have alternate representations when unicode syntax is
-- enabled. This flag is attached to those tokens in the lexer so that the
-- original source representation can be reproduced in the corresponding
-- 'ApiAnnotation'
data IsUnicodeSyntax = UnicodeSyntax | NormalSyntax
    deriving (Eq, Ord, Data, Show)

-- | Convert a normal annotation into its unicode equivalent one
unicodeAnn :: AnnKeywordId -> AnnKeywordId
unicodeAnn AnnForall     = AnnForallU
unicodeAnn AnnDcolon     = AnnDcolonU
unicodeAnn AnnLarrow     = AnnLarrowU
unicodeAnn AnnRarrow     = AnnRarrowU
unicodeAnn AnnDarrow     = AnnDarrowU
unicodeAnn Annlarrowtail = AnnlarrowtailU
unicodeAnn Annrarrowtail = AnnrarrowtailU
unicodeAnn AnnLarrowtail = AnnLarrowtailU
unicodeAnn AnnRarrowtail = AnnRarrowtailU
unicodeAnn AnnOpenB      = AnnOpenBU
unicodeAnn AnnCloseB     = AnnCloseBU
unicodeAnn AnnOpenEQ     = AnnOpenEQU
unicodeAnn AnnCloseQ     = AnnCloseQU
unicodeAnn ann           = ann


-- | Some template haskell tokens have two variants, one with an `e` the other
-- not:
--
-- >  [| or [e|
-- >  [|| or [e||
--
-- This type indicates whether the 'e' is present or not.
data HasE = HasE | NoE
     deriving (Eq, Ord, Data, Show)
