{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.Parser.Annotation (
  -- * Out-of-tree API Annotations. Exist for the duration of !5158,
  -- * will be removed by !2418
  getAnnotation, getAndRemoveAnnotation,
  getAnnotationComments,getAndRemoveAnnotationComments,
  ApiAnns(..),
  ApiAnnKey,
  AddAnn(..), mkParensApiAnn,

  -- * Core API Annotation types
  AnnKeywordId(..),
  AnnotationComment(..),
  IsUnicodeSyntax(..),
  unicodeAnn,
  HasE(..),

  -- * In-tree Api Annotations
  AddApiAnn(..),
  AnnAnchor(..), annAnchorRealSrcSpan,
  DeltaPos(..),

  ApiAnn, ApiAnn'(..), Anchor(..), AnchorOperation(..),
  spanAsAnchor, realSpanAsAnchor,
  noAnn,

  -- ** Comments in Annotations

  ApiAnnComments(..), LAnnotationComment, com, noCom,
  getFollowingComments, setFollowingComments, setPriorComments,
  ApiAnnCO,

  -- ** Annotations in 'GenLocated'
  LocatedA, LocatedL, LocatedC, LocatedN, LocatedAn, LocatedP,
  SrcSpanAnnA, SrcSpanAnnL, SrcSpanAnnP, SrcSpanAnnC, SrcSpanAnnN, SrcSpanAnn'(..),

  -- ** Annotation data types used in 'GenLocated'

  AnnListItem(..), AnnList(..),
  AnnParen(..), ParenType(..), parenTypeKws,
  AnnPragma(..),
  AnnContext(..),
  NameAnn(..), NameAdornment(..),
  NoApiAnns(..),
  AnnSortKey(..),

  -- ** Trailing annotations in lists
  TrailingAnn(..), addTrailingAnnToA, addTrailingAnnToL, addTrailingCommaToN,

  -- ** Utilities for converting between different 'GenLocated' when
  -- ** we do not care about the annotations.
  la2na, na2la, n2l, l2n, l2l, la2la,
  reLoc, reLocA, reLocL, reLocC, reLocN,

  la2r, realSrcSpan,

  -- ** Building up annotations
  extraToAnnList, reAnn,
  reAnnL, reAnnC,
  addAnns, addAnnsA, widenSpan, widenAnchor, widenAnchorR, widenLocatedAn,

  -- ** Querying annotations
  getLocAnn,
  apiAnnAnns, apiAnnAnnsL,
  annParen2AddApiAnn,

  -- ** Working with locations of annotations
  sortLocatedA,
  mapLocA,
  combineLocsA,
  combineSrcSpansA,
  addCLocA, addCLocAA,

  -- ** Constructing 'GenLocated' annotation types when we do not care about annotations.
  noLocA, getLocA,
  noSrcSpanA,
  noAnnSrcSpan,

  -- ** Working with comments in annotations
  noComments, comment, addCommentsToSrcAnn, setCommentsSrcAnn,
  addCommentsToApiAnn, setCommentsApiAnn,
  transferComments,

  placeholderRealSpan,
  ) where

import GHC.Prelude

import Data.Data
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Semigroup
import GHC.Data.FastString
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Utils.Binary
import GHC.Utils.Outputable hiding ( (<>) )
import GHC.Utils.Panic

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

NON-COMMENT ELEMENTS

Intuitively, every AST element directly contains a bag of keywords
(keywords can show up more than once in a node: a semicolon i.e. newline
can show up multiple times before the next AST element), each of which
needs to be associated with its location in the original source code.

These keywords are recorded directly in the AST element in which they
occur, for the GhcPs phase.

For any given element in the AST, there is only a set number of
keywords that are applicable for it (e.g., you'll never see an
'import' keyword associated with a let-binding.)  The set of allowed
keywords is documented in a comment associated with the constructor
of a given AST element, although the ground truth is in GHC.Parser
and GHC.Parser.PostProcess (which actually add the annotations).

COMMENT ELEMENTS

We associate comments with the lowest (most specific) AST element
enclosing them:

PARSER STATE

There is one field in PState (the parser state) which play a role
with annotations.

>  comment_q :: [LAnnotationComment],

The 'comment_q' field captures comments as they are seen in the token stream,
so that when they are ready to be allocated via the parser they are
available.

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

-- This section should be removed when we move to the new APi Annotations


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

-- End of section to be removed with new API Annotations
-- --------------------------------------------------------------------

-- | API Annotations exist so that tools can perform source to source
-- conversions of Haskell code. They are used to keep track of the
-- various syntactic keywords that are not captured in the existing
-- AST.
--
-- The annotations, together with original source comments are made available in
-- the @'pm_annotations'@ field of @'GHC.Driver.Env.HsParsedModule'@.
-- Comments are only retained if @'Opt_KeepRawTokenStream'@ is set.
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
    | AnnClosePH -- ^ '\#)'
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
    | AnnNewtype
    | AnnName -- ^ where a name loses its location in the AST, this carries it
    | AnnOf
    | AnnOpen    -- ^ '{-\# LANGUAGE' etc
    | AnnOpenB   -- ^ '(|'
    | AnnOpenBU  -- ^ '(|', unicode variant
    | AnnOpenC   -- ^ '{'
    | AnnOpenE   -- ^ '[e|' or '[e||'
    | AnnOpenEQ  -- ^ '[|'
    | AnnOpenEQU -- ^ '[|', unicode variant
    | AnnOpenP   -- ^ '('
    | AnnOpenS   -- ^ '['
    | AnnOpenPH  -- ^ '(\#'
    | AnnDollar          -- ^ prefix '$'   -- TemplateHaskell
    | AnnDollarDollar    -- ^ prefix '$$'  -- TemplateHaskell
    | AnnPackageName
    | AnnPattern
    | AnnPercent    -- ^ '%'  -- for HsExplicitMult
    | AnnPercentOne -- ^ '%1' -- for HsLinearArrow
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

-- ---------------------------------------------------------------------

-- | Captures an annotation, storing the @'AnnKeywordId'@ and its
-- location.  The parser only ever inserts @'AnnAnchor'@ fields with a
-- RealSrcSpan being the original location of the annotation in the
-- source file.
-- The @'AnnAnchor'@ can also store a delta position if the AST has been
-- modified and needs to be pretty printed again.
-- The usual way an 'AddApiAnn' is created is using the 'mj' ("make
-- jump") function, and then it can be inserted into the appropriate
-- annotation.
data AddApiAnn = AddApiAnn AnnKeywordId AnnAnchor deriving (Data,Show,Eq,Ord)

-- | The anchor for an @'AnnKeywordId'@. The Parser inserts the @'AR'@
-- variant, giving the exact location of the original item in the
-- parsed source.  This can be replace by the @'AD'@ version, to
-- provide a position for the item relative to the end of the previous
-- item in the source.  This is useful when editing an AST prior to
-- exact printing the changed one.
data AnnAnchor = AR RealSrcSpan
               | AD DeltaPos
               deriving (Data,Show,Eq,Ord)

-- | Relative position, line then column.  If 'deltaLine' is zero then
-- 'deltaColumn' gives the number of spaces between the end of the
-- preceding output element and the start of the one this is attached
-- to, on the same line.  If 'deltaLine' is > 0, then it is the number
-- of lines to advance, and 'deltaColumn' is the start column on the
-- new line.
data DeltaPos =
  DP
    { deltaLine   :: !Int,
      deltaColumn :: !Int
    } deriving (Show,Eq,Ord,Data)


annAnchorRealSrcSpan :: AnnAnchor -> RealSrcSpan
annAnchorRealSrcSpan (AR r) = r
annAnchorRealSrcSpan (AD _) = placeholderRealSpan

instance Outputable AnnAnchor where
  ppr (AR r) = text "AR" <+> ppr r
  ppr (AD d) = text "AD" <+> ppr d

instance Outputable AddApiAnn where
  ppr (AddApiAnn kw ss) = text "AddApiAnn" <+> ppr kw <+> ppr ss

-- ---------------------------------------------------------------------

{-
Note [In-tree Api annotations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC 7.10 brought in the concept of API Annotations,
https://gitlab.haskell.org/ghc/ghc/-/wikis/api-annotations:

  The hsSyn AST does not directly capture the locations of certain
  keywords and punctuation, such as 'let', 'in', 'do', etc.

  These locations are required by any tools wanting to parse a haskell
  file, transform the AST in some way, and then regenerate the
  original layout for the unchaged parts."

These were returned in a separate data structure, linked to the main
AST via a combination of SrcSpan and constructor name.

This indirect linkage kept the AST uncluttered, but made working with
the annotations complex, as two separate data structures had to be
changed at the same time in a coherent way.

From GHC 9.2.1, these annotations are captured directly in the AST,
using the types in this file, and the Trees That Grow (TTG) extension
points for GhcPs.

See Note [XRec and Anno in the AST] for details of how this is done.
-}

-- | The API Annotations are now kept in the HsSyn AST for the GhcPs
--   phase. We do not always have API Annotations though, only for
--   parsed code. This type captures that, and allows the
--   representation decision to be easily revisited as it evolves.
--
-- A goal of the annotations is that an AST can be edited, including
-- moving subtrees from one place to another, duplicating them, and so
-- on.  This means that each fragment must be self-contained.  To this
-- end, each annotated fragment keeps track of the anchor position it
-- was originally captured at, being simply the start span of the
-- topmost element of the ast fragment.  This gives us a way to later
-- re-calculate all Located items in this layer of the AST, as well as
-- any annotations captured. The comments associated with the AST
-- fragment are also captured here.
--
-- The 'ann' type parameter allows this general structure to be
-- specialised to the specific set of locations of original API
-- Annotation elements.  So for 'HsLet' we have
--
--    type instance XLet GhcPs = ApiAnn' AnnsLet
--    data AnnsLet
--      = AnnsLet {
--          alLet :: AnnAnchor,
--          alIn :: AnnAnchor
--          } deriving Data
--
-- The spacing between the items under the scope of a given ApiAnn' is
-- derived from the original 'Anchor'.  But there is no requirement
-- that the items included in the sub-element have a "matching"
-- location in their relative anchors. This allows us to freely move
-- elements around, and stitch together new AST fragments out of old
-- ones, and have them still printed out in a reasonable way.
data ApiAnn' ann
  = ApiAnn { entry   :: Anchor
           -- ^ Base location for the start of the syntactic element
           -- holding the annotations.
           , anns     :: ann -- ^ Annotations added by the Parser
           , comments :: ApiAnnComments
              -- ^ Comments enclosed in the SrcSpan of the element
              -- this `ApiAnn'` is attached to
           }
  | ApiAnnNotUsed -- ^ No Annotation for generated code,
                  -- e.g. from TH, deriving, etc.
        deriving (Data, Eq, Functor)

-- | An 'Anchor' records the base location for the start of the
-- syntactic element holding the annotations, and is used as the point
-- of reference for calculating delta positions for contained
-- annotations.  If an AST element is moved or deleted, the original
-- location is also tracked, for printing the source without gaps.
data Anchor = Anchor        { anchor :: RealSrcSpan
                                 -- ^ Base location for the start of
                                 -- the syntactic element holding
                                 -- the annotations.
                            , anchor_op :: AnchorOperation }
        deriving (Data, Eq, Show)

-- | If tools modify the parsed source, the 'MovedAnchor' variant can
-- directly provide the spacing for this item relative to the previous
-- one when printing. This allows AST fragments with a particular
-- anchor to be freely moved, without worrying about recalculating the
-- appropriate anchor span.
data AnchorOperation = UnchangedAnchor
                     | MovedAnchor DeltaPos
        deriving (Data, Eq, Show)


spanAsAnchor :: SrcSpan -> Anchor
spanAsAnchor s  = Anchor (realSrcSpan s) UnchangedAnchor

realSpanAsAnchor :: RealSrcSpan -> Anchor
realSpanAsAnchor s  = Anchor s UnchangedAnchor

-- ---------------------------------------------------------------------

-- | When we are parsing we add comments that belong a particular AST
-- element, and print them together with the element, interleaving
-- them into the output stream.  But when editin the AST, to move
-- fragments around, it is useful to be able to first separate the
-- comments into those occuring before the AST element and those
-- following it.  The 'AnnCommentsBalanced' constructor is used to do
-- this. The GHC parser will only insert the 'AnnComments' form.
data ApiAnnComments = AnnComments
                        { priorComments :: ![LAnnotationComment] }
                    | AnnCommentsBalanced
                        { priorComments :: ![LAnnotationComment]
                        , followingComments :: ![LAnnotationComment] }
        deriving (Data, Eq)

type LAnnotationComment = GenLocated Anchor AnnotationComment

noCom :: ApiAnnComments
noCom = AnnComments []

com :: [LAnnotationComment] -> ApiAnnComments
com cs = AnnComments cs

-- ---------------------------------------------------------------------

-- | This type is the most direct mapping of the previous API
-- Annotations model. It captures the containing `SrcSpan' in its
-- `entry` `Anchor`, has a list of `AddApiAnn` as before, and keeps
-- track of the comments associated with the anchor.
type ApiAnn = ApiAnn' [AddApiAnn]

-- ---------------------------------------------------------------------
-- Annotations attached to a 'SrcSpan'.
-- ---------------------------------------------------------------------

-- | The 'SrcSpanAnn\'' type wraps a normal 'SrcSpan', together with
-- an extra annotation type. This is mapped to a specific `GenLocated`
-- usage in the AST through the `XRec` and `Anno` type families.
data SrcSpanAnn' a = SrcSpanAnn { ann :: a, locA :: SrcSpan }
        deriving (Data, Eq)
-- See Note [XRec and Anno in the AST]

-- | We mostly use 'SrcSpanAnn\'' with an 'ApiAnn\''
type SrcAnn ann = SrcSpanAnn' (ApiAnn' ann)
-- AZ: is SrcAnn the right abbreviation here? Any better suggestions?

-- AZ: should we rename LocatedA to LocatedL?  The name comes from
-- this being the most common usage, and hence being the default
-- annotation. It also has a matching set if utility functions such as
-- locA, noLocA, etc.  LocatedL would then need a new name, but it is
-- relatively rare, and captures a list having an openinc and closing
-- adorment, such as parens, braces, etc.
type LocatedA = GenLocated SrcSpanAnnA
type LocatedN = GenLocated SrcSpanAnnN

type LocatedL = GenLocated SrcSpanAnnL
type LocatedP = GenLocated SrcSpanAnnP
type LocatedC = GenLocated SrcSpanAnnC

type SrcSpanAnnA = SrcAnn AnnListItem
type SrcSpanAnnN = SrcAnn NameAnn

type SrcSpanAnnL = SrcAnn AnnList
type SrcSpanAnnP = SrcAnn AnnPragma
type SrcSpanAnnC = SrcAnn AnnContext

-- | General representation of a 'GenLocated' type carrying a
-- parameterised annotation type.
type LocatedAn an = GenLocated (SrcAnn an)

{-
Note [XRec and Anno in the AST]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The API annotations are now captured directly inside the AST, using
TTG extension points. However certain annotations need to be captured
on the Located versions too.  While there is a general form for these,
captured in the type SrcSpanAnn', there are also specific usages in
different contexts.

Some of the particular use cases are

1) RdrNames, which can have additional items such as backticks or parens

2) Items which occur in lists, and the annotation relates purely
to its usage inside a list.

See the section above this note for the rest.

The Anno type family maps the specific SrcSpanAnn' variant for a given
item.

So

  type instance XRec (GhcPass p) a = GenLocated (Anno a) a
  type instance Anno RdrName = SrcSpanAnnN
  type LocatedN = GenLocated SrcSpanAnnN

meaning we can have type LocatedN RdrName

-}

-- ---------------------------------------------------------------------
-- Annotations for items in a list
-- ---------------------------------------------------------------------

-- | Captures the location of punctuation occuring between items,
-- normally in a list.  It is captured as a trailing annotation.
data TrailingAnn
  = AddSemiAnn AnnAnchor    -- ^ Trailing ';'
  | AddCommaAnn AnnAnchor   -- ^ Trailing ','
  | AddVbarAnn AnnAnchor    -- ^ Trailing '|'
  | AddRarrowAnn AnnAnchor  -- ^ Trailing '->'
  | AddRarrowAnnU AnnAnchor -- ^ Trailing '->', unicode variant
  deriving (Data,Show,Eq, Ord)

instance Outputable TrailingAnn where
  ppr (AddSemiAnn ss)    = text "AddSemiAnn"    <+> ppr ss
  ppr (AddCommaAnn ss)   = text "AddCommaAnn"   <+> ppr ss
  ppr (AddVbarAnn ss)    = text "AddVbarAnn"    <+> ppr ss
  ppr (AddRarrowAnn ss)  = text "AddRarrowAnn"  <+> ppr ss
  ppr (AddRarrowAnnU ss) = text "AddRarrowAnnU" <+> ppr ss

-- | Annotation for items appearing in a list. They can have one or
-- more trailing punctuations items, such as commas or semicolons.
data AnnListItem
  = AnnListItem {
      lann_trailing  :: [TrailingAnn]
      }
  deriving (Data, Eq)

-- ---------------------------------------------------------------------
-- Annotations for the context of a list of items
-- ---------------------------------------------------------------------

-- | Annotation for the "container" of a list. This captures
-- surrounding items such as braces if present, and introductory
-- keywords such as 'where'.
data AnnList
  = AnnList {
      -- TODO:AZ: should we distinguish AnnList variants for lists
      -- with layout and without?
      al_anchor    :: Maybe Anchor, -- ^ start point of a list having layout
      al_open      :: Maybe AddApiAnn,
      al_close     :: Maybe AddApiAnn,
      al_rest      :: [AddApiAnn], -- ^ context, such as 'where' keyword
      al_trailing  :: [TrailingAnn]
      } deriving (Data,Eq)

-- ---------------------------------------------------------------------
-- Annotations for parenthesised elements, such as tuples, lists
-- ---------------------------------------------------------------------

-- | API Annotation for an item having surrounding "brackets", such as
-- tuples or lists
data AnnParen
  = AnnParen {
      ap_adornment :: ParenType,
      ap_open      :: AnnAnchor,
      ap_close     :: AnnAnchor
      } deriving (Data)

-- | Detail of the "brackets" used in an 'AnnParen' API Annotation.
data ParenType
  = AnnParens       -- ^ '(', ')'
  | AnnParensHash   -- ^ '(#', '#)'
  | AnnParensSquare -- ^ '[', ']'
  deriving (Eq, Ord, Data)

-- | Maps the 'ParenType' to the related opening and closing
-- AnnKeywordId. Used when actually printing the item.
parenTypeKws :: ParenType -> (AnnKeywordId, AnnKeywordId)
parenTypeKws AnnParens       = (AnnOpenP, AnnCloseP)
parenTypeKws AnnParensHash   = (AnnOpenPH, AnnClosePH)
parenTypeKws AnnParensSquare = (AnnOpenS, AnnCloseS)

-- ---------------------------------------------------------------------

-- | API Annotation for the 'Context' data type.
data AnnContext
  = AnnContext {
      ac_darrow    :: Maybe (IsUnicodeSyntax, AnnAnchor),
                      -- ^ location and encoding of the '=>', if present.
      ac_open      :: [AnnAnchor], -- ^ zero or more opening parentheses.
      ac_close     :: [AnnAnchor]  -- ^ zero or more closing parentheses.
      } deriving (Data)


-- ---------------------------------------------------------------------
-- Annotations for names
-- ---------------------------------------------------------------------

-- | API Annotations for a 'RdrName'.  There are many kinds of
-- adornment that can be attached to a given 'RdrName'. This type
-- captures them, as detailed on the individual constructors.
data NameAnn
  -- | Used for a name with an adornment, so '`foo`', '(bar)'
  = NameAnn {
      nann_adornment :: NameAdornment,
      nann_open      :: AnnAnchor,
      nann_name      :: AnnAnchor,
      nann_close     :: AnnAnchor,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for @(,,,)@, or @(#,,,#)#
  | NameAnnCommas {
      nann_adornment :: NameAdornment,
      nann_open      :: AnnAnchor,
      nann_commas    :: [AnnAnchor],
      nann_close     :: AnnAnchor,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for @()@, @(##)@, @[]@
  | NameAnnOnly {
      nann_adornment :: NameAdornment,
      nann_open      :: AnnAnchor,
      nann_close     :: AnnAnchor,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for @->@, as an identifier
  | NameAnnRArrow {
      nann_name      :: AnnAnchor,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for an item with a leading @'@. The annotation for
  -- unquoted item is stored in 'nann_quoted'.
  | NameAnnQuote {
      nann_quote     :: AnnAnchor,
      nann_quoted    :: SrcSpanAnnN,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used when adding a 'TrailingAnn' to an existing 'LocatedN'
  -- which has no Api Annotation (via the 'ApiAnnNotUsed' constructor.
  | NameAnnTrailing {
      nann_trailing  :: [TrailingAnn]
      }
  deriving (Data, Eq)

-- | A 'NameAnn' can capture the locations of surrounding adornments,
-- such as parens or backquotes. This data type identifies what
-- particular pair are being used.
data NameAdornment
  = NameParens -- ^ '(' ')'
  | NameParensHash -- ^ '(#' '#)'
  | NameBackquotes -- ^ '`'
  | NameSquare -- ^ '[' ']'
  deriving (Eq, Ord, Data)

-- ---------------------------------------------------------------------

-- | API Annotation used for capturing the locations of annotations in
-- pragmas.
data AnnPragma
  = AnnPragma {
      apr_open      :: AddApiAnn,
      apr_close     :: AddApiAnn,
      apr_rest      :: [AddApiAnn]
      } deriving (Data,Eq)

-- ---------------------------------------------------------------------
-- | Captures the sort order of sub elements. This is needed when the
-- sub-elements have been split (as in a HsLocalBind which holds separate
-- binds and sigs) or for infix patterns where the order has been
-- re-arranged. It is captured explicitly so that after the Delta phase a
-- SrcSpan is used purely as an index into the annotations, allowing
-- transformations of the AST including the introduction of new Located
-- items or re-arranging existing ones.
data AnnSortKey
  = NoAnnSortKey
  | AnnSortKey [RealSrcSpan]
  deriving (Data, Eq)

-- ---------------------------------------------------------------------


-- | Helper function used in the parser to add a 'TrailingAnn' items
-- to an existing annotation.
addTrailingAnnToL :: SrcSpan -> TrailingAnn -> ApiAnnComments
                  -> ApiAnn' AnnList -> ApiAnn' AnnList
addTrailingAnnToL s t cs ApiAnnNotUsed
  = ApiAnn (spanAsAnchor s) (AnnList (Just $ spanAsAnchor s) Nothing Nothing [] [t]) cs
addTrailingAnnToL _ t cs n = n { anns = addTrailing (anns n)
                               , comments = comments n <> cs }
  where
    addTrailing n = n { al_trailing = t : al_trailing n }

-- | Helper function used in the parser to add a 'TrailingAnn' items
-- to an existing annotation.
addTrailingAnnToA :: SrcSpan -> TrailingAnn -> ApiAnnComments
                  -> ApiAnn' AnnListItem -> ApiAnn' AnnListItem
addTrailingAnnToA s t cs ApiAnnNotUsed
  = ApiAnn (spanAsAnchor s) (AnnListItem [t]) cs
addTrailingAnnToA _ t cs n = n { anns = addTrailing (anns n)
                               , comments = comments n <> cs }
  where
    addTrailing n = n { lann_trailing = t : lann_trailing n }

-- | Helper function used in the parser to add a comma location to an
-- existing annotation.
addTrailingCommaToN :: SrcSpan -> ApiAnn' NameAnn -> AnnAnchor -> ApiAnn' NameAnn
addTrailingCommaToN s ApiAnnNotUsed l
  = ApiAnn (spanAsAnchor s) (NameAnnTrailing [AddCommaAnn l]) noCom
addTrailingCommaToN _ n l = n { anns = addTrailing (anns n) l }
  where
    addTrailing :: NameAnn -> AnnAnchor -> NameAnn
    addTrailing n l = n { nann_trailing = AddCommaAnn l : nann_trailing n }

-- ---------------------------------------------------------------------

-- |Helper function (temporary) during transition of names
--  Discards any annotations
l2n :: LocatedAn a1 a2 -> LocatedN a2
l2n (L la a) = L (noAnnSrcSpan (locA la)) a

n2l :: LocatedN a -> LocatedA a
n2l (L la a) = L (na2la la) a

-- |Helper function (temporary) during transition of names
--  Discards any annotations
la2na :: SrcSpanAnn' a -> SrcSpanAnnN
la2na l = noAnnSrcSpan (locA l)

-- |Helper function (temporary) during transition of names
--  Discards any annotations
la2la :: LocatedAn ann1 a2 -> LocatedAn ann2 a2
la2la (L la a) = L (noAnnSrcSpan (locA la)) a

l2l :: SrcSpanAnn' a -> SrcAnn ann
l2l l = noAnnSrcSpan (locA l)

-- |Helper function (temporary) during transition of names
--  Discards any annotations
na2la :: SrcSpanAnn' a -> SrcAnn ann
na2la l = noAnnSrcSpan (locA l)

reLoc :: LocatedAn a e -> Located e
reLoc (L (SrcSpanAnn _ l) a) = L l a

reLocA :: Located e -> LocatedAn ann e
reLocA (L l a) = (L (SrcSpanAnn ApiAnnNotUsed l) a)

reLocL :: LocatedN e -> LocatedA e
reLocL (L l a) = (L (na2la l) a)

reLocC :: LocatedN e -> LocatedC e
reLocC (L l a) = (L (na2la l) a)

reLocN :: LocatedN a -> Located a
reLocN (L (SrcSpanAnn _ l) a) = L l a

-- ---------------------------------------------------------------------

realSrcSpan :: SrcSpan -> RealSrcSpan
realSrcSpan (RealSrcSpan s _) = s
realSrcSpan _ = mkRealSrcSpan l l -- AZ temporary
  where
    l = mkRealSrcLoc (fsLit "foo") (-1) (-1)

la2r :: SrcSpanAnn' a -> RealSrcSpan
la2r l = realSrcSpan (locA l)

extraToAnnList :: AnnList -> [AddApiAnn] -> AnnList
extraToAnnList (AnnList a o c e t) as = AnnList a o c (e++as) t

reAnn :: [TrailingAnn] -> ApiAnnComments -> Located a -> LocatedA a
reAnn anns cs (L l a) = L (SrcSpanAnn (ApiAnn (spanAsAnchor l) (AnnListItem anns) cs) l) a

reAnnC :: AnnContext -> ApiAnnComments -> Located a -> LocatedC a
reAnnC anns cs (L l a) = L (SrcSpanAnn (ApiAnn (spanAsAnchor l) anns cs) l) a

reAnnL :: ann -> ApiAnnComments -> Located e -> GenLocated (SrcAnn ann) e
reAnnL anns cs (L l a) = L (SrcSpanAnn (ApiAnn (spanAsAnchor l) anns cs) l) a

getLocAnn :: Located a  -> SrcSpanAnnA
getLocAnn (L l _) = SrcSpanAnn ApiAnnNotUsed l


getLocA :: GenLocated (SrcSpanAnn' a) e -> SrcSpan
getLocA (L (SrcSpanAnn _ l) _) = l

noLocA :: a -> LocatedAn an a
noLocA = L (SrcSpanAnn ApiAnnNotUsed noSrcSpan)

noAnnSrcSpan :: SrcSpan -> SrcAnn ann
noAnnSrcSpan l = SrcSpanAnn ApiAnnNotUsed l

noSrcSpanA :: SrcAnn ann
noSrcSpanA = noAnnSrcSpan noSrcSpan

-- | Short form for 'ApiAnnNotUsed'
noAnn :: ApiAnn' a
noAnn = ApiAnnNotUsed


addAnns :: ApiAnn -> [AddApiAnn] -> ApiAnnComments -> ApiAnn
addAnns (ApiAnn l as1 cs) as2 cs2
  = ApiAnn (widenAnchor l (as1 ++ as2)) (as1 ++ as2) (cs <> cs2)
addAnns ApiAnnNotUsed [] (AnnComments []) = ApiAnnNotUsed
addAnns ApiAnnNotUsed [] (AnnCommentsBalanced [] []) = ApiAnnNotUsed
addAnns ApiAnnNotUsed as cs = ApiAnn (Anchor placeholderRealSpan UnchangedAnchor) as cs

-- AZ:TODO use widenSpan here too
addAnnsA :: SrcSpanAnnA -> [TrailingAnn] -> ApiAnnComments -> SrcSpanAnnA
addAnnsA (SrcSpanAnn (ApiAnn l as1 cs) loc) as2 cs2
  = SrcSpanAnn (ApiAnn l (AnnListItem (lann_trailing as1 ++ as2)) (cs <> cs2)) loc
addAnnsA (SrcSpanAnn ApiAnnNotUsed loc) [] (AnnComments [])
  = SrcSpanAnn ApiAnnNotUsed loc
addAnnsA (SrcSpanAnn ApiAnnNotUsed loc) [] (AnnCommentsBalanced [] [])
  = SrcSpanAnn ApiAnnNotUsed loc
addAnnsA (SrcSpanAnn ApiAnnNotUsed loc) as cs
  = SrcSpanAnn (ApiAnn (spanAsAnchor loc) (AnnListItem as) cs) loc

-- | The annotations need to all come after the anchor.  Make sure
-- this is the case.
widenSpan :: SrcSpan -> [AddApiAnn] -> SrcSpan
widenSpan s as = foldl combineSrcSpans s (go as)
  where
    go [] = []
    go (AddApiAnn _ (AR s):rest) = RealSrcSpan s Nothing : go rest
    go (AddApiAnn _ (AD _):rest) = go rest

-- | The annotations need to all come after the anchor.  Make sure
-- this is the case.
widenRealSpan :: RealSrcSpan -> [AddApiAnn] -> RealSrcSpan
widenRealSpan s as = foldl combineRealSrcSpans s (go as)
  where
    go [] = []
    go (AddApiAnn _ (AR s):rest) = s : go rest
    go (AddApiAnn _ (AD _):rest) =     go rest

widenAnchor :: Anchor -> [AddApiAnn] -> Anchor
widenAnchor (Anchor s op) as = Anchor (widenRealSpan s as) op

widenAnchorR :: Anchor -> RealSrcSpan -> Anchor
widenAnchorR (Anchor s op) r = Anchor (combineRealSrcSpans s r) op

widenLocatedAn :: SrcSpanAnn' an -> [AddApiAnn] -> SrcSpanAnn' an
widenLocatedAn (SrcSpanAnn a l) as = SrcSpanAnn a (widenSpan l as)

apiAnnAnnsL :: ApiAnn' a -> [a]
apiAnnAnnsL ApiAnnNotUsed = []
apiAnnAnnsL (ApiAnn _ anns _) = [anns]

apiAnnAnns :: ApiAnn -> [AddApiAnn]
apiAnnAnns ApiAnnNotUsed = []
apiAnnAnns (ApiAnn _ anns _) = anns

annParen2AddApiAnn :: ApiAnn' AnnParen -> [AddApiAnn]
annParen2AddApiAnn ApiAnnNotUsed = []
annParen2AddApiAnn (ApiAnn _ (AnnParen pt o c) _)
  = [AddApiAnn ai o, AddApiAnn ac c]
  where
    (ai,ac) = parenTypeKws pt

-- TODO: enable when we migrate
-- apiAnnComments :: ApiAnn' an -> ApiAnnComments
-- apiAnnComments ApiAnnNotUsed = AnnComments []
-- apiAnnComments (ApiAnn _ _ cs) = cs

-- ---------------------------------------------------------------------
-- sortLocatedA :: [LocatedA a] -> [LocatedA a]
sortLocatedA :: [GenLocated (SrcSpanAnn' a) e] -> [GenLocated (SrcSpanAnn' a) e]
sortLocatedA = sortBy (leftmost_smallest `on` getLocA)

mapLocA :: (a -> b) -> GenLocated SrcSpan a -> GenLocated (SrcAnn ann) b
mapLocA f (L l a) = L (noAnnSrcSpan l) (f a)

-- AZ:TODO: move this somewhere sane

combineLocsA :: Semigroup a => GenLocated (SrcSpanAnn' a) e1 -> GenLocated (SrcSpanAnn' a) e2 -> SrcSpanAnn' a
combineLocsA (L a _) (L b _) = combineSrcSpansA a b

combineSrcSpansA :: Semigroup a => SrcSpanAnn' a -> SrcSpanAnn' a -> SrcSpanAnn' a
combineSrcSpansA (SrcSpanAnn aa la) (SrcSpanAnn ab lb)
  = SrcSpanAnn (aa <> ab) (combineSrcSpans la lb)

-- | Combine locations from two 'Located' things and add them to a third thing
addCLocA :: GenLocated (SrcSpanAnn' a) e1 -> GenLocated SrcSpan e2 -> e3 -> GenLocated (SrcAnn ann) e3
addCLocA a b c = L (noAnnSrcSpan $ combineSrcSpans (locA $ getLoc a) (getLoc b)) c

addCLocAA :: GenLocated (SrcSpanAnn' a1) e1 -> GenLocated (SrcSpanAnn' a2) e2 -> e3 -> GenLocated (SrcAnn ann) e3
addCLocAA a b c = L (noAnnSrcSpan $ combineSrcSpans (locA $ getLoc a) (locA $ getLoc b)) c

-- ---------------------------------------------------------------------
-- Utilities for manipulating ApiAnnComments
-- ---------------------------------------------------------------------

getFollowingComments :: ApiAnnComments -> [LAnnotationComment]
getFollowingComments (AnnComments _) = []
getFollowingComments (AnnCommentsBalanced _ cs) = cs

setFollowingComments :: ApiAnnComments -> [LAnnotationComment] -> ApiAnnComments
setFollowingComments (AnnComments ls) cs           = AnnCommentsBalanced ls cs
setFollowingComments (AnnCommentsBalanced ls _) cs = AnnCommentsBalanced ls cs

setPriorComments :: ApiAnnComments -> [LAnnotationComment] -> ApiAnnComments
setPriorComments (AnnComments _) cs            = AnnComments cs
setPriorComments (AnnCommentsBalanced _ ts) cs = AnnCommentsBalanced cs ts

-- ---------------------------------------------------------------------
-- Comment-only annotations
-- ---------------------------------------------------------------------

-- TODO:AZ I think ApiAnnCO is not needed
type ApiAnnCO = ApiAnn' NoApiAnns -- ^ Api Annotations for comments only

data NoApiAnns = NoApiAnns
  deriving (Data,Eq,Ord)

noComments ::ApiAnnCO
noComments = ApiAnn (Anchor placeholderRealSpan UnchangedAnchor) NoApiAnns noCom

-- TODO:AZ get rid of this
placeholderRealSpan :: RealSrcSpan
placeholderRealSpan = realSrcLocSpan (mkRealSrcLoc (mkFastString "placeholder") (-1) (-1))

comment :: RealSrcSpan -> ApiAnnComments -> ApiAnnCO
comment loc cs = ApiAnn (Anchor loc UnchangedAnchor) NoApiAnns cs

-- ---------------------------------------------------------------------
-- Utilities for managing comments in an `ApiAnn' a` structure.
-- ---------------------------------------------------------------------

-- | Add additional comments to a 'SrcAnn', used for manipulating the
-- AST prior to exact printing the changed one.
addCommentsToSrcAnn :: (Monoid ann) => SrcAnn ann -> ApiAnnComments -> SrcAnn ann
addCommentsToSrcAnn (SrcSpanAnn ApiAnnNotUsed loc) cs
  = SrcSpanAnn (ApiAnn (Anchor (realSrcSpan loc) UnchangedAnchor) mempty cs) loc
addCommentsToSrcAnn (SrcSpanAnn (ApiAnn a an cs) loc) cs'
  = SrcSpanAnn (ApiAnn a an (cs <> cs')) loc

-- | Replace any existing comments on a 'SrcAnn', used for manipulating the
-- AST prior to exact printing the changed one.
setCommentsSrcAnn :: (Monoid ann) => SrcAnn ann -> ApiAnnComments -> SrcAnn ann
setCommentsSrcAnn (SrcSpanAnn ApiAnnNotUsed loc) cs
  = SrcSpanAnn (ApiAnn (Anchor (realSrcSpan loc) UnchangedAnchor) mempty cs) loc
setCommentsSrcAnn (SrcSpanAnn (ApiAnn a an _) loc) cs
  = SrcSpanAnn (ApiAnn a an cs) loc

-- | Add additional comments, used for manipulating the
-- AST prior to exact printing the changed one.
addCommentsToApiAnn :: (Monoid a)
  => SrcSpan -> ApiAnn' a -> ApiAnnComments -> ApiAnn' a
addCommentsToApiAnn loc ApiAnnNotUsed cs
  = ApiAnn (Anchor (realSrcSpan loc) UnchangedAnchor) mempty cs
addCommentsToApiAnn _ (ApiAnn a an ocs) ncs = ApiAnn a an (ocs <> ncs)

-- | Replace any existing comments, used for manipulating the
-- AST prior to exact printing the changed one.
setCommentsApiAnn :: (Monoid a)
  => SrcSpan -> ApiAnn' a -> ApiAnnComments -> ApiAnn' a
setCommentsApiAnn loc ApiAnnNotUsed cs
  = ApiAnn (Anchor (realSrcSpan loc) UnchangedAnchor) mempty cs
setCommentsApiAnn _ (ApiAnn a an _) cs = ApiAnn a an cs

-- | Transfer comments from the annotations in one 'SrcAnn' to those
-- in another.  The originals are not changed.  This is used when
-- manipulating an AST prior to exact printing,
transferComments :: (Monoid ann)
  => SrcAnn ann -> SrcAnn ann -> (SrcAnn ann,  SrcAnn ann)
transferComments from@(SrcSpanAnn ApiAnnNotUsed _) to = (from, to)
transferComments (SrcSpanAnn (ApiAnn a an cs) l) to
  = ((SrcSpanAnn (ApiAnn a an noCom) l), addCommentsToSrcAnn to cs)

-- ---------------------------------------------------------------------
-- Semigroup instances, to allow easy combination of annotaion elements
-- ---------------------------------------------------------------------

instance (Semigroup an) => Semigroup (SrcSpanAnn' an) where
  (SrcSpanAnn a1 l1) <> (SrcSpanAnn a2 l2) = SrcSpanAnn (a1 <> a2) (combineSrcSpans l1 l2)
   -- The critical part about the location is its left edge, and all
   -- annotations must follow it. So we combine them which yields the
   -- largest span

instance (Semigroup a) => Semigroup (ApiAnn' a) where
  ApiAnnNotUsed <> x = x
  x <> ApiAnnNotUsed = x
  (ApiAnn l1 a1 b1) <> (ApiAnn l2 a2 b2) = ApiAnn (l1 <> l2) (a1 <> a2) (b1 <> b2)
   -- The critical part about the anchor is its left edge, and all
   -- annotations must follow it. So we combine them which yields the
   -- largest span

instance Ord Anchor where
  compare (Anchor s1 _) (Anchor s2 _) = compare s1 s2

instance Semigroup Anchor where
  Anchor r1 o1 <> Anchor r2 _ = Anchor (combineRealSrcSpans r1 r2) o1

instance Semigroup ApiAnnComments where
  AnnComments cs1 <> AnnComments cs2 = AnnComments (cs1 ++ cs2)
  AnnComments cs1 <> AnnCommentsBalanced cs2 as2 = AnnCommentsBalanced (cs1 ++ cs2) as2
  AnnCommentsBalanced cs1 as1 <> AnnComments cs2 = AnnCommentsBalanced (cs1 ++ cs2) as1
  AnnCommentsBalanced cs1 as1 <> AnnCommentsBalanced cs2 as2 = AnnCommentsBalanced (cs1 ++ cs2) (as1++as2)


instance (Monoid a) => Monoid (ApiAnn' a) where
  mempty = ApiAnnNotUsed

instance Semigroup AnnListItem where
  (AnnListItem l1) <> (AnnListItem l2) = AnnListItem (l1 <> l2)

instance Monoid AnnListItem where
  mempty = AnnListItem []


instance Semigroup AnnList where
  (AnnList a1 o1 c1 r1 t1) <> (AnnList a2 o2 c2 r2 t2)
    = AnnList (a1 <> a2) (c o1 o2) (c c1 c2) (r1 <> r2) (t1 <> t2)
    where
      -- Left biased combination for the open and close annotations
      c Nothing x = x
      c x Nothing = x
      c f _       = f

instance Monoid AnnList where
  mempty = AnnList Nothing Nothing Nothing [] []

instance Semigroup NameAnn where
  _ <> _ = panic "semigroup nameann"

instance Monoid NameAnn where
  mempty = NameAnnTrailing []


instance Semigroup AnnSortKey where
  NoAnnSortKey <> x = x
  x <> NoAnnSortKey = x
  AnnSortKey ls1 <> AnnSortKey ls2 = AnnSortKey (ls1 <> ls2)

instance Monoid AnnSortKey where
  mempty = NoAnnSortKey

instance (Outputable a) => Outputable (ApiAnn' a) where
  ppr (ApiAnn l a c)  = text "ApiAnn" <+> ppr l <+> ppr a <+> ppr c
  ppr ApiAnnNotUsed = text "ApiAnnNotUsed"

instance Outputable Anchor where
  ppr (Anchor a o)        = text "Anchor" <+> ppr a <+> ppr o

instance Outputable AnchorOperation where
  ppr UnchangedAnchor   = text "UnchangedAnchor"
  ppr (MovedAnchor d)   = text "MovedAnchor" <+> ppr d

instance Outputable DeltaPos where
  ppr (DP l c) = text "DP" <+> ppr l <+> ppr c

instance Outputable (GenLocated Anchor AnnotationComment) where
  ppr (L l c) = text "L" <+> ppr l <+> ppr c

instance Outputable ApiAnnComments where
  ppr (AnnComments cs) = text "AnnComments" <+> ppr cs
  ppr (AnnCommentsBalanced cs ts) = text "AnnCommentsBalanced" <+> ppr cs <+> ppr ts

instance (NamedThing (Located a)) => NamedThing (LocatedAn an a) where
  getName (L l a) = getName (L (locA l) a)

instance Outputable AnnContext where
  ppr (AnnContext a o c) = text "AnnContext" <+> ppr a <+> ppr o <+> ppr c

instance Outputable AnnSortKey where
  ppr NoAnnSortKey    = text "NoAnnSortKey"
  ppr (AnnSortKey ls) = text "AnnSortKey" <+> ppr ls

instance Outputable IsUnicodeSyntax where
  ppr = text . show

instance Binary a => Binary (LocatedL a) where
  -- We do not serialise the annotations
    put_ bh (L l x) = do
            put_ bh (locA l)
            put_ bh x

    get bh = do
            l <- get bh
            x <- get bh
            return (L (noAnnSrcSpan l) x)

instance (Outputable a) => Outputable (SrcSpanAnn' a) where
  ppr (SrcSpanAnn a l) = text "SrcSpanAnn" <+> ppr a <+> ppr l

instance (Outputable a, Outputable e)
     => Outputable (GenLocated (SrcSpanAnn' a) e) where
  ppr = pprLocated

instance Outputable AnnListItem where
  ppr (AnnListItem ts) = text "AnnListItem" <+> ppr ts

instance Outputable NameAdornment where
  ppr NameParens     = text "NameParens"
  ppr NameParensHash = text "NameParensHash"
  ppr NameBackquotes = text "NameBackquotes"
  ppr NameSquare     = text "NameSquare"

instance Outputable NameAnn where
  ppr (NameAnn a o n c t)
    = text "NameAnn" <+> ppr a <+> ppr o <+> ppr n <+> ppr c <+> ppr t
  ppr (NameAnnCommas a o n c t)
    = text "NameAnnCommas" <+> ppr a <+> ppr o <+> ppr n <+> ppr c <+> ppr t
  ppr (NameAnnOnly a o c t)
    = text "NameAnnOnly" <+> ppr a <+> ppr o <+> ppr c <+> ppr t
  ppr (NameAnnRArrow n t)
    = text "NameAnnRArrow" <+> ppr n <+> ppr t
  ppr (NameAnnQuote q n t)
    = text "NameAnnQuote" <+> ppr q <+> ppr n <+> ppr t
  ppr (NameAnnTrailing t)
    = text "NameAnnTrailing" <+> ppr t

instance Outputable AnnList where
  ppr (AnnList a o c r t)
    = text "AnnList" <+> ppr a <+> ppr o <+> ppr c <+> ppr r <+> ppr t

instance Outputable AnnPragma where
  ppr (AnnPragma o c r) = text "AnnPragma" <+> ppr o <+> ppr c <+> ppr r
