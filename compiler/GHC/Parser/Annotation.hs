{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.Parser.Annotation (
  -- * Core Exact Print Annotation types
  AnnKeywordId(..),
  EpaComment(..), EpaCommentTok(..),
  IsUnicodeSyntax(..),
  unicodeAnn,
  HasE(..),

  -- * In-tree Exact Print Annotations
  AddEpAnn(..),
  EpaLocation(..), epaLocationRealSrcSpan, epaLocationFromSrcAnn,
  TokenLocation(..),
  DeltaPos(..), deltaPos, getDeltaLine,

  EpAnn(..), Anchor(..), AnchorOperation(..),
  spanAsAnchor, realSpanAsAnchor,
  noAnn,

  -- ** Comments in Annotations

  EpAnnComments(..), LEpaComment, emptyComments,
  getFollowingComments, setFollowingComments, setPriorComments,
  EpAnnCO,

  -- ** Annotations in 'GenLocated'
  LocatedA, LocatedL, LocatedC, LocatedN, LocatedAn, LocatedP,
  SrcSpanAnnA, SrcSpanAnnL, SrcSpanAnnP, SrcSpanAnnC, SrcSpanAnnN,
  SrcSpanAnn'(..), SrcAnn,

  -- ** Annotation data types used in 'GenLocated'

  AnnListItem(..), AnnList(..),
  AnnParen(..), ParenType(..), parenTypeKws,
  AnnPragma(..),
  AnnContext(..),
  NameAnn(..), NameAdornment(..),
  NoEpAnns(..),
  AnnSortKey(..),

  -- ** Trailing annotations in lists
  TrailingAnn(..), trailingAnnToAddEpAnn,
  addTrailingAnnToA, addTrailingAnnToL, addTrailingCommaToN,

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
  epAnnAnns, epAnnAnnsL,
  annParen2AddEpAnn,
  epAnnComments,

  -- ** Working with locations of annotations
  sortLocatedA,
  mapLocA,
  combineLocsA,
  combineSrcSpansA,
  addCLocA, addCLocAA,

  -- ** Constructing 'GenLocated' annotation types when we do not care
  -- about annotations.
  noLocA, getLocA,
  noSrcSpanA,
  noAnnSrcSpan,

  -- ** Working with comments in annotations
  noComments, comment, addCommentsToSrcAnn, setCommentsSrcAnn,
  addCommentsToEpAnn, setCommentsEpAnn,
  transferAnnsA, commentsOnlyA, removeCommentsA,

  placeholderRealSpan,
  ) where

import GHC.Prelude

import Data.Data
import Data.Function (on)
import Data.List (sortBy)
import Data.Semigroup
import GHC.Data.FastString
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Hs.DocString
import GHC.Utils.Binary
import GHC.Utils.Outputable hiding ( (<>) )
import GHC.Utils.Panic
import qualified GHC.Data.Strict as Strict

{-
Note [exact print annotations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a parse tree of a Haskell module, how can we reconstruct
the original Haskell source code, retaining all whitespace and
source code comments?  We need to track the locations of all
elements from the original source: this includes keywords such as
'let' / 'in' / 'do' etc as well as punctuation such as commas and
braces, and also comments.  We collectively refer to this
metadata as the "exact print annotations".

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
enclosing them

PARSER STATE

There are three fields in PState (the parser state) which play a role
with annotation comments.

>  comment_q :: [LEpaComment],
>  header_comments :: Maybe [LEpaComment],
>  eof_pos :: Maybe (RealSrcSpan, RealSrcSpan), -- pos, gap to prior token

The 'comment_q' field captures comments as they are seen in the token stream,
so that when they are ready to be allocated via the parser they are
available.

The 'header_comments' capture the comments coming at the top of the
source file.  They are moved there from the `comment_q` when comments
are allocated for the first top-level declaration.

The 'eof_pos' captures the final location in the file, and the
location of the immediately preceding token to the last location, so
that the exact-printer can work out how far to advance to add the
trailing whitespace.

PARSER EMISSION OF ANNOTATIONS

The parser interacts with the lexer using the functions

> getCommentsFor      :: (MonadP m) => SrcSpan -> m EpAnnComments
> getPriorCommentsFor :: (MonadP m) => SrcSpan -> m EpAnnComments
> getFinalCommentsFor :: (MonadP m) => SrcSpan -> m EpAnnComments

The 'getCommentsFor' function is the one used most often.  It takes
the AST element SrcSpan and removes and returns any comments in the
'comment_q' that are inside the span. 'allocateComments' in 'Lexer' is
responsible for making sure we only return comments that actually fit
in the 'SrcSpan'.

The 'getPriorCommentsFor' function is used for top-level declarations,
and removes and returns any comments in the 'comment_q' that either
precede or are included in the given SrcSpan. This is to ensure that
preceding documentation comments are kept together with the
declaration they belong to.

The 'getFinalCommentsFor' function is called right at the end when EOF
is hit. This drains the 'comment_q' completely, and returns the
'header_comments', remaining 'comment_q' entries and the
'eof_pos'. These values are inserted into the 'HsModule' AST element.

The wiki page describing this feature is
https://gitlab.haskell.org/ghc/ghc/wikis/api-annotations

-}

-- --------------------------------------------------------------------

-- | Exact print annotations exist so that tools can perform source to
-- source conversions of Haskell code. They are used to keep track of
-- the various syntactic keywords that are not otherwise captured in the
-- AST.
--
-- The wiki page describing this feature is
-- https://gitlab.haskell.org/ghc/ghc/wikis/api-annotations
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/in-tree-api-annotations
--
-- Note: in general the names of these are taken from the
-- corresponding token, unless otherwise noted
-- See Note [exact print annotations] above for details of the usage
data AnnKeywordId
    = AnnAnyclass
    | AnnAs
    | AnnAt
    | AnnBang  -- ^ '!'
    | AnnBackquote -- ^ '`'
    | AnnBy
    | AnnCase -- ^ case or lambda case
    | AnnCases -- ^ lambda cases
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
    | AnnOpen    -- ^ '{-\# DEPRECATED' etc. Opening of pragmas where
                 -- the capitalisation of the string can be changed by
                 -- the user. The actual text used is stored in a
                 -- 'SourceText' on the relevant pragma item.
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

-- | Certain tokens can have alternate representations when unicode syntax is
-- enabled. This flag is attached to those tokens in the lexer so that the
-- original source representation can be reproduced in the corresponding
-- 'EpAnnotation'
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

data EpaComment =
  EpaComment
    { ac_tok :: EpaCommentTok
    , ac_prior_tok :: RealSrcSpan
    -- ^ The location of the prior token, used in exact printing.  The
    -- 'EpaComment' appears as an 'LEpaComment' containing its
    -- location.  The difference between the end of the prior token
    -- and the start of this location is used for the spacing when
    -- exact printing the comment.
    }
    deriving (Eq, Data, Show)

data EpaCommentTok =
  -- Documentation annotations
    EpaDocComment      HsDocString -- ^ a docstring that can be pretty printed using pprHsDocString
  | EpaDocOptions      String     -- ^ doc options (prune, ignore-exports, etc)
  | EpaLineComment     String     -- ^ comment starting by "--"
  | EpaBlockComment    String     -- ^ comment in {- -}
  | EpaEofComment                 -- ^ empty comment, capturing
                                  -- location of EOF

  -- See #19697 for a discussion of EpaEofComment's use and how it
  -- should be removed in favour of capturing it in the location for
  -- 'Located HsModule' in the parser.

    deriving (Eq, Data, Show)
-- Note: these are based on the Token versions, but the Token type is
-- defined in GHC.Parser.Lexer and bringing it in here would create a loop

instance Outputable EpaComment where
  ppr x = text (show x)

-- ---------------------------------------------------------------------

-- | Captures an annotation, storing the @'AnnKeywordId'@ and its
-- location.  The parser only ever inserts @'EpaLocation'@ fields with a
-- RealSrcSpan being the original location of the annotation in the
-- source file.
-- The @'EpaLocation'@ can also store a delta position if the AST has been
-- modified and needs to be pretty printed again.
-- The usual way an 'AddEpAnn' is created is using the 'mj' ("make
-- jump") function, and then it can be inserted into the appropriate
-- annotation.
data AddEpAnn = AddEpAnn AnnKeywordId EpaLocation deriving (Data,Eq)

-- | The anchor for an @'AnnKeywordId'@. The Parser inserts the
-- @'EpaSpan'@ variant, giving the exact location of the original item
-- in the parsed source.  This can be replaced by the @'EpaDelta'@
-- version, to provide a position for the item relative to the end of
-- the previous item in the source.  This is useful when editing an
-- AST prior to exact printing the changed one. The list of comments
-- in the @'EpaDelta'@ variant captures any comments between the prior
-- output and the thing being marked here, since we cannot otherwise
-- sort the relative order.
data EpaLocation = EpaSpan !RealSrcSpan
                 | EpaDelta !DeltaPos ![LEpaComment]
               deriving (Data,Eq)

-- | Tokens embedded in the AST have an EpaLocation, unless they come from
-- generated code (e.g. by TH).
data TokenLocation = NoTokenLoc | TokenLoc !EpaLocation
               deriving (Data,Eq)

-- | Spacing between output items when exact printing.  It captures
-- the spacing from the current print position on the page to the
-- position required for the thing about to be printed.  This is
-- either on the same line in which case is is simply the number of
-- spaces to emit, or it is some number of lines down, with a given
-- column offset.  The exact printing algorithm keeps track of the
-- column offset pertaining to the current anchor position, so the
-- `deltaColumn` is the additional spaces to add in this case.  See
-- https://gitlab.haskell.org/ghc/ghc/wikis/api-annotations for
-- details.
data DeltaPos
  = SameLine { deltaColumn :: !Int }
  | DifferentLine
      { deltaLine   :: !Int, -- ^ deltaLine should always be > 0
        deltaColumn :: !Int
      } deriving (Show,Eq,Ord,Data)

-- | Smart constructor for a 'DeltaPos'. It preserves the invariant
-- that for the 'DifferentLine' constructor 'deltaLine' is always > 0.
deltaPos :: Int -> Int -> DeltaPos
deltaPos l c = case l of
  0 -> SameLine c
  _ -> DifferentLine l c

getDeltaLine :: DeltaPos -> Int
getDeltaLine (SameLine _) = 0
getDeltaLine (DifferentLine r _) = r

-- | Used in the parser only, extract the 'RealSrcSpan' from an
-- 'EpaLocation'. The parser will never insert a 'DeltaPos', so the
-- partial function is safe.
epaLocationRealSrcSpan :: EpaLocation -> RealSrcSpan
epaLocationRealSrcSpan (EpaSpan r) = r
epaLocationRealSrcSpan (EpaDelta _ _) = panic "epaLocationRealSrcSpan"

epaLocationFromSrcAnn :: SrcAnn ann -> EpaLocation
epaLocationFromSrcAnn (SrcSpanAnn EpAnnNotUsed l) = EpaSpan (realSrcSpan l)
epaLocationFromSrcAnn (SrcSpanAnn (EpAnn anc _ _) _) = EpaSpan (anchor anc)

instance Outputable EpaLocation where
  ppr (EpaSpan r) = text "EpaSpan" <+> ppr r
  ppr (EpaDelta d cs) = text "EpaDelta" <+> ppr d <+> ppr cs

instance Outputable AddEpAnn where
  ppr (AddEpAnn kw ss) = text "AddEpAnn" <+> ppr kw <+> ppr ss

-- ---------------------------------------------------------------------

-- | The exact print annotations (EPAs) are kept in the HsSyn AST for
--   the GhcPs phase. We do not always have EPAs though, only for code
--   that has been parsed as they do not exist for generated
--   code. This type captures that they may be missing.
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
-- specialised to the specific set of locations of original exact
-- print annotation elements.  So for 'HsLet' we have
--
--    type instance XLet GhcPs = EpAnn AnnsLet
--    data AnnsLet
--      = AnnsLet {
--          alLet :: EpaLocation,
--          alIn :: EpaLocation
--          } deriving Data
--
-- The spacing between the items under the scope of a given EpAnn is
-- normally derived from the original 'Anchor'.  But if a sub-element
-- is not in its original position, the required spacing can be
-- directly captured in the 'anchor_op' field of the 'entry' Anchor.
-- This allows us to freely move elements around, and stitch together
-- new AST fragments out of old ones, and have them still printed out
-- in a precise way.
data EpAnn ann
  = EpAnn { entry   :: !Anchor
           -- ^ Base location for the start of the syntactic element
           -- holding the annotations.
           , anns     :: !ann -- ^ Annotations added by the Parser
           , comments :: !EpAnnComments
              -- ^ Comments enclosed in the SrcSpan of the element
              -- this `EpAnn` is attached to
           }
  | EpAnnNotUsed -- ^ No Annotation for generated code,
                  -- e.g. from TH, deriving, etc.
        deriving (Data, Eq, Functor)

-- | An 'Anchor' records the base location for the start of the
-- syntactic element holding the annotations, and is used as the point
-- of reference for calculating delta positions for contained
-- annotations.
-- It is also normally used as the reference point for the spacing of
-- the element relative to its container. If it is moved, that
-- relationship is tracked in the 'anchor_op' instead.

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
-- them into the output stream.  But when editing the AST to move
-- fragments around it is useful to be able to first separate the
-- comments into those occuring before the AST element and those
-- following it.  The 'EpaCommentsBalanced' constructor is used to do
-- this. The GHC parser will only insert the 'EpaComments' form.
data EpAnnComments = EpaComments
                        { priorComments :: ![LEpaComment] }
                    | EpaCommentsBalanced
                        { priorComments :: ![LEpaComment]
                        , followingComments :: ![LEpaComment] }
        deriving (Data, Eq)

type LEpaComment = GenLocated Anchor EpaComment

emptyComments :: EpAnnComments
emptyComments = EpaComments []

-- ---------------------------------------------------------------------
-- Annotations attached to a 'SrcSpan'.
-- ---------------------------------------------------------------------

-- | The 'SrcSpanAnn\'' type wraps a normal 'SrcSpan', together with
-- an extra annotation type. This is mapped to a specific `GenLocated`
-- usage in the AST through the `XRec` and `Anno` type families.

-- Important that the fields are strict as these live inside L nodes which
-- are live for a long time.
data SrcSpanAnn' a = SrcSpanAnn { ann :: !a, locA :: !SrcSpan }
        deriving (Data, Eq)
-- See Note [XRec and Anno in the AST]

-- | We mostly use 'SrcSpanAnn\'' with an 'EpAnn\''
type SrcAnn ann = SrcSpanAnn' (EpAnn ann)

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

The exact print annotations are captured directly inside the AST, using
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
  = AddSemiAnn EpaLocation    -- ^ Trailing ';'
  | AddCommaAnn EpaLocation   -- ^ Trailing ','
  | AddVbarAnn EpaLocation    -- ^ Trailing '|'
  deriving (Data, Eq)

instance Outputable TrailingAnn where
  ppr (AddSemiAnn ss)    = text "AddSemiAnn"    <+> ppr ss
  ppr (AddCommaAnn ss)   = text "AddCommaAnn"   <+> ppr ss
  ppr (AddVbarAnn ss)    = text "AddVbarAnn"    <+> ppr ss

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
      al_anchor    :: Maybe Anchor, -- ^ start point of a list having layout
      al_open      :: Maybe AddEpAnn,
      al_close     :: Maybe AddEpAnn,
      al_rest      :: [AddEpAnn], -- ^ context, such as 'where' keyword
      al_trailing  :: [TrailingAnn] -- ^ items appearing after the
                                    -- list, such as '=>' for a
                                    -- context
      } deriving (Data,Eq)

-- ---------------------------------------------------------------------
-- Annotations for parenthesised elements, such as tuples, lists
-- ---------------------------------------------------------------------

-- | exact print annotation for an item having surrounding "brackets", such as
-- tuples or lists
data AnnParen
  = AnnParen {
      ap_adornment :: ParenType,
      ap_open      :: EpaLocation,
      ap_close     :: EpaLocation
      } deriving (Data)

-- | Detail of the "brackets" used in an 'AnnParen' exact print annotation.
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

-- | Exact print annotation for the 'Context' data type.
data AnnContext
  = AnnContext {
      ac_darrow    :: Maybe (IsUnicodeSyntax, EpaLocation),
                      -- ^ location and encoding of the '=>', if present.
      ac_open      :: [EpaLocation], -- ^ zero or more opening parentheses.
      ac_close     :: [EpaLocation]  -- ^ zero or more closing parentheses.
      } deriving (Data)


-- ---------------------------------------------------------------------
-- Annotations for names
-- ---------------------------------------------------------------------

-- | exact print annotations for a 'RdrName'.  There are many kinds of
-- adornment that can be attached to a given 'RdrName'. This type
-- captures them, as detailed on the individual constructors.
data NameAnn
  -- | Used for a name with an adornment, so '`foo`', '(bar)'
  = NameAnn {
      nann_adornment :: NameAdornment,
      nann_open      :: EpaLocation,
      nann_name      :: EpaLocation,
      nann_close     :: EpaLocation,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for @(,,,)@, or @(#,,,#)#
  | NameAnnCommas {
      nann_adornment :: NameAdornment,
      nann_open      :: EpaLocation,
      nann_commas    :: [EpaLocation],
      nann_close     :: EpaLocation,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for @(# | | #)@
  | NameAnnBars {
      nann_adornment :: NameAdornment,
      nann_open      :: EpaLocation,
      nann_bars      :: [EpaLocation],
      nann_close     :: EpaLocation,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for @()@, @(##)@, @[]@
  | NameAnnOnly {
      nann_adornment :: NameAdornment,
      nann_open      :: EpaLocation,
      nann_close     :: EpaLocation,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for @->@, as an identifier
  | NameAnnRArrow {
      nann_name      :: EpaLocation,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for an item with a leading @'@. The annotation for
  -- unquoted item is stored in 'nann_quoted'.
  | NameAnnQuote {
      nann_quote     :: EpaLocation,
      nann_quoted    :: SrcSpanAnnN,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used when adding a 'TrailingAnn' to an existing 'LocatedN'
  -- which has no Api Annotation (via the 'EpAnnNotUsed' constructor.
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

-- | exact print annotation used for capturing the locations of
-- annotations in pragmas.
data AnnPragma
  = AnnPragma {
      apr_open      :: AddEpAnn,
      apr_close     :: AddEpAnn,
      apr_rest      :: [AddEpAnn]
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

-- | Convert a 'TrailingAnn' to an 'AddEpAnn'
trailingAnnToAddEpAnn :: TrailingAnn -> AddEpAnn
trailingAnnToAddEpAnn (AddSemiAnn ss)    = AddEpAnn AnnSemi ss
trailingAnnToAddEpAnn (AddCommaAnn ss)   = AddEpAnn AnnComma ss
trailingAnnToAddEpAnn (AddVbarAnn ss)    = AddEpAnn AnnVbar ss

-- | Helper function used in the parser to add a 'TrailingAnn' items
-- to an existing annotation.
addTrailingAnnToL :: SrcSpan -> TrailingAnn -> EpAnnComments
                  -> EpAnn AnnList -> EpAnn AnnList
addTrailingAnnToL s t cs EpAnnNotUsed
  = EpAnn (spanAsAnchor s) (AnnList (Just $ spanAsAnchor s) Nothing Nothing [] [t]) cs
addTrailingAnnToL _ t cs n = n { anns = addTrailing (anns n)
                               , comments = comments n <> cs }
  where
    -- See Note [list append in addTrailing*]
    addTrailing n = n { al_trailing = al_trailing n ++ [t]}

-- | Helper function used in the parser to add a 'TrailingAnn' items
-- to an existing annotation.
addTrailingAnnToA :: SrcSpan -> TrailingAnn -> EpAnnComments
                  -> EpAnn AnnListItem -> EpAnn AnnListItem
addTrailingAnnToA s t cs EpAnnNotUsed
  = EpAnn (spanAsAnchor s) (AnnListItem [t]) cs
addTrailingAnnToA _ t cs n = n { anns = addTrailing (anns n)
                               , comments = comments n <> cs }
  where
    -- See Note [list append in addTrailing*]
    addTrailing n = n { lann_trailing = lann_trailing n ++ [t] }

-- | Helper function used in the parser to add a comma location to an
-- existing annotation.
addTrailingCommaToN :: SrcSpan -> EpAnn NameAnn -> EpaLocation -> EpAnn NameAnn
addTrailingCommaToN s EpAnnNotUsed l
  = EpAnn (spanAsAnchor s) (NameAnnTrailing [AddCommaAnn l]) emptyComments
addTrailingCommaToN _ n l = n { anns = addTrailing (anns n) l }
  where
    -- See Note [list append in addTrailing*]
    addTrailing :: NameAnn -> EpaLocation -> NameAnn
    addTrailing n l = n { nann_trailing = nann_trailing n ++ [AddCommaAnn l]}

{-
Note [list append in addTrailing*]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The addTrailingAnnToL, addTrailingAnnToA and addTrailingCommaToN
functions are used to add a separator for an item when it occurs in a
list.  So they are used to capture a comma, vbar, semicolon and similar.

In general, a given element will have zero or one of these.  In
extreme (test) cases, there may be multiple semicolons.

In exact printing we sometimes convert the EpaLocation variant for an
trailing annotation to the EpaDelta variant, which cannot be sorted.

Hence it is critical that these annotations are captured in the order
they appear in the original source file.

And so we use the less efficient list append to preserve the order,
knowing that in most cases the original list is empty.
-}

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
reLocA (L l a) = (L (SrcSpanAnn EpAnnNotUsed l) a)

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

extraToAnnList :: AnnList -> [AddEpAnn] -> AnnList
extraToAnnList (AnnList a o c e t) as = AnnList a o c (e++as) t

reAnn :: [TrailingAnn] -> EpAnnComments -> Located a -> LocatedA a
reAnn anns cs (L l a) = L (SrcSpanAnn (EpAnn (spanAsAnchor l) (AnnListItem anns) cs) l) a

reAnnC :: AnnContext -> EpAnnComments -> Located a -> LocatedC a
reAnnC anns cs (L l a) = L (SrcSpanAnn (EpAnn (spanAsAnchor l) anns cs) l) a

reAnnL :: ann -> EpAnnComments -> Located e -> GenLocated (SrcAnn ann) e
reAnnL anns cs (L l a) = L (SrcSpanAnn (EpAnn (spanAsAnchor l) anns cs) l) a

getLocAnn :: Located a  -> SrcSpanAnnA
getLocAnn (L l _) = SrcSpanAnn EpAnnNotUsed l


getLocA :: GenLocated (SrcSpanAnn' a) e -> SrcSpan
getLocA (L (SrcSpanAnn _ l) _) = l

noLocA :: a -> LocatedAn an a
noLocA = L (SrcSpanAnn EpAnnNotUsed noSrcSpan)

noAnnSrcSpan :: SrcSpan -> SrcAnn ann
noAnnSrcSpan l = SrcSpanAnn EpAnnNotUsed l

noSrcSpanA :: SrcAnn ann
noSrcSpanA = noAnnSrcSpan noSrcSpan

-- | Short form for 'EpAnnNotUsed'
noAnn :: EpAnn a
noAnn = EpAnnNotUsed


addAnns :: EpAnn [AddEpAnn] -> [AddEpAnn] -> EpAnnComments -> EpAnn [AddEpAnn]
addAnns (EpAnn l as1 cs) as2 cs2
  = EpAnn (widenAnchor l (as1 ++ as2)) (as1 ++ as2) (cs <> cs2)
addAnns EpAnnNotUsed [] (EpaComments []) = EpAnnNotUsed
addAnns EpAnnNotUsed [] (EpaCommentsBalanced [] []) = EpAnnNotUsed
addAnns EpAnnNotUsed as cs = EpAnn (Anchor placeholderRealSpan UnchangedAnchor) as cs

-- AZ:TODO use widenSpan here too
addAnnsA :: SrcSpanAnnA -> [TrailingAnn] -> EpAnnComments -> SrcSpanAnnA
addAnnsA (SrcSpanAnn (EpAnn l as1 cs) loc) as2 cs2
  = SrcSpanAnn (EpAnn l (AnnListItem (lann_trailing as1 ++ as2)) (cs <> cs2)) loc
addAnnsA (SrcSpanAnn EpAnnNotUsed loc) [] (EpaComments [])
  = SrcSpanAnn EpAnnNotUsed loc
addAnnsA (SrcSpanAnn EpAnnNotUsed loc) [] (EpaCommentsBalanced [] [])
  = SrcSpanAnn EpAnnNotUsed loc
addAnnsA (SrcSpanAnn EpAnnNotUsed loc) as cs
  = SrcSpanAnn (EpAnn (spanAsAnchor loc) (AnnListItem as) cs) loc

-- | The annotations need to all come after the anchor.  Make sure
-- this is the case.
widenSpan :: SrcSpan -> [AddEpAnn] -> SrcSpan
widenSpan s as = foldl combineSrcSpans s (go as)
  where
    go [] = []
    go (AddEpAnn _ (EpaSpan s):rest) = RealSrcSpan s Strict.Nothing : go rest
    go (AddEpAnn _ (EpaDelta _ _):rest) = go rest

-- | The annotations need to all come after the anchor.  Make sure
-- this is the case.
widenRealSpan :: RealSrcSpan -> [AddEpAnn] -> RealSrcSpan
widenRealSpan s as = foldl combineRealSrcSpans s (go as)
  where
    go [] = []
    go (AddEpAnn _ (EpaSpan s):rest) = s : go rest
    go (AddEpAnn _ (EpaDelta _ _):rest) =     go rest

widenAnchor :: Anchor -> [AddEpAnn] -> Anchor
widenAnchor (Anchor s op) as = Anchor (widenRealSpan s as) op

widenAnchorR :: Anchor -> RealSrcSpan -> Anchor
widenAnchorR (Anchor s op) r = Anchor (combineRealSrcSpans s r) op

widenLocatedAn :: SrcSpanAnn' an -> [AddEpAnn] -> SrcSpanAnn' an
widenLocatedAn (SrcSpanAnn a l) as = SrcSpanAnn a (widenSpan l as)

epAnnAnnsL :: EpAnn a -> [a]
epAnnAnnsL EpAnnNotUsed = []
epAnnAnnsL (EpAnn _ anns _) = [anns]

epAnnAnns :: EpAnn [AddEpAnn] -> [AddEpAnn]
epAnnAnns EpAnnNotUsed = []
epAnnAnns (EpAnn _ anns _) = anns

annParen2AddEpAnn :: EpAnn AnnParen -> [AddEpAnn]
annParen2AddEpAnn EpAnnNotUsed = []
annParen2AddEpAnn (EpAnn _ (AnnParen pt o c) _)
  = [AddEpAnn ai o, AddEpAnn ac c]
  where
    (ai,ac) = parenTypeKws pt

epAnnComments :: EpAnn an -> EpAnnComments
epAnnComments EpAnnNotUsed = EpaComments []
epAnnComments (EpAnn _ _ cs) = cs

-- ---------------------------------------------------------------------
-- sortLocatedA :: [LocatedA a] -> [LocatedA a]
sortLocatedA :: [GenLocated (SrcSpanAnn' a) e] -> [GenLocated (SrcSpanAnn' a) e]
sortLocatedA = sortBy (leftmost_smallest `on` getLocA)

mapLocA :: (a -> b) -> GenLocated SrcSpan a -> GenLocated (SrcAnn ann) b
mapLocA f (L l a) = L (noAnnSrcSpan l) (f a)

-- AZ:TODO: move this somewhere sane

combineLocsA :: Semigroup a => GenLocated (SrcAnn a) e1 -> GenLocated (SrcAnn a) e2 -> SrcAnn a
combineLocsA (L a _) (L b _) = combineSrcSpansA a b

combineSrcSpansA :: Semigroup a => SrcAnn a -> SrcAnn a -> SrcAnn a
combineSrcSpansA (SrcSpanAnn aa la) (SrcSpanAnn ab lb)
  = case SrcSpanAnn (aa <> ab) (combineSrcSpans la lb) of
      SrcSpanAnn EpAnnNotUsed l -> SrcSpanAnn EpAnnNotUsed l
      SrcSpanAnn (EpAnn anc an cs) l ->
        SrcSpanAnn (EpAnn (widenAnchorR anc (realSrcSpan l)) an cs) l

-- | Combine locations from two 'Located' things and add them to a third thing
addCLocA :: GenLocated (SrcSpanAnn' a) e1 -> GenLocated SrcSpan e2 -> e3 -> GenLocated (SrcAnn ann) e3
addCLocA a b c = L (noAnnSrcSpan $ combineSrcSpans (locA $ getLoc a) (getLoc b)) c

addCLocAA :: GenLocated (SrcSpanAnn' a1) e1 -> GenLocated (SrcSpanAnn' a2) e2 -> e3 -> GenLocated (SrcAnn ann) e3
addCLocAA a b c = L (noAnnSrcSpan $ combineSrcSpans (locA $ getLoc a) (locA $ getLoc b)) c

-- ---------------------------------------------------------------------
-- Utilities for manipulating EpAnnComments
-- ---------------------------------------------------------------------

getFollowingComments :: EpAnnComments -> [LEpaComment]
getFollowingComments (EpaComments _) = []
getFollowingComments (EpaCommentsBalanced _ cs) = cs

setFollowingComments :: EpAnnComments -> [LEpaComment] -> EpAnnComments
setFollowingComments (EpaComments ls) cs           = EpaCommentsBalanced ls cs
setFollowingComments (EpaCommentsBalanced ls _) cs = EpaCommentsBalanced ls cs

setPriorComments :: EpAnnComments -> [LEpaComment] -> EpAnnComments
setPriorComments (EpaComments _) cs            = EpaComments cs
setPriorComments (EpaCommentsBalanced _ ts) cs = EpaCommentsBalanced cs ts

-- ---------------------------------------------------------------------
-- Comment-only annotations
-- ---------------------------------------------------------------------

type EpAnnCO = EpAnn NoEpAnns -- ^ Api Annotations for comments only

data NoEpAnns = NoEpAnns
  deriving (Data,Eq,Ord)

noComments ::EpAnnCO
noComments = EpAnn (Anchor placeholderRealSpan UnchangedAnchor) NoEpAnns emptyComments

-- TODO:AZ get rid of this
placeholderRealSpan :: RealSrcSpan
placeholderRealSpan = realSrcLocSpan (mkRealSrcLoc (mkFastString "placeholder") (-1) (-1))

comment :: RealSrcSpan -> EpAnnComments -> EpAnnCO
comment loc cs = EpAnn (Anchor loc UnchangedAnchor) NoEpAnns cs

-- ---------------------------------------------------------------------
-- Utilities for managing comments in an `EpAnn a` structure.
-- ---------------------------------------------------------------------

-- | Add additional comments to a 'SrcAnn', used for manipulating the
-- AST prior to exact printing the changed one.
addCommentsToSrcAnn :: (Monoid ann) => SrcAnn ann -> EpAnnComments -> SrcAnn ann
addCommentsToSrcAnn (SrcSpanAnn EpAnnNotUsed loc) cs
  = SrcSpanAnn (EpAnn (Anchor (realSrcSpan loc) UnchangedAnchor) mempty cs) loc
addCommentsToSrcAnn (SrcSpanAnn (EpAnn a an cs) loc) cs'
  = SrcSpanAnn (EpAnn a an (cs <> cs')) loc

-- | Replace any existing comments on a 'SrcAnn', used for manipulating the
-- AST prior to exact printing the changed one.
setCommentsSrcAnn :: (Monoid ann) => SrcAnn ann -> EpAnnComments -> SrcAnn ann
setCommentsSrcAnn (SrcSpanAnn EpAnnNotUsed loc) cs
  = SrcSpanAnn (EpAnn (Anchor (realSrcSpan loc) UnchangedAnchor) mempty cs) loc
setCommentsSrcAnn (SrcSpanAnn (EpAnn a an _) loc) cs
  = SrcSpanAnn (EpAnn a an cs) loc

-- | Add additional comments, used for manipulating the
-- AST prior to exact printing the changed one.
addCommentsToEpAnn :: (Monoid a)
  => SrcSpan -> EpAnn a -> EpAnnComments -> EpAnn a
addCommentsToEpAnn loc EpAnnNotUsed cs
  = EpAnn (Anchor (realSrcSpan loc) UnchangedAnchor) mempty cs
addCommentsToEpAnn _ (EpAnn a an ocs) ncs = EpAnn a an (ocs <> ncs)

-- | Replace any existing comments, used for manipulating the
-- AST prior to exact printing the changed one.
setCommentsEpAnn :: (Monoid a)
  => SrcSpan -> EpAnn a -> EpAnnComments -> EpAnn a
setCommentsEpAnn loc EpAnnNotUsed cs
  = EpAnn (Anchor (realSrcSpan loc) UnchangedAnchor) mempty cs
setCommentsEpAnn _ (EpAnn a an _) cs = EpAnn a an cs

-- | Transfer comments and trailing items from the annotations in the
-- first 'SrcSpanAnnA' argument to those in the second.
transferAnnsA :: SrcSpanAnnA -> SrcSpanAnnA -> (SrcSpanAnnA,  SrcSpanAnnA)
transferAnnsA from@(SrcSpanAnn EpAnnNotUsed _) to = (from, to)
transferAnnsA (SrcSpanAnn (EpAnn a an cs) l) to
  = ((SrcSpanAnn (EpAnn a mempty emptyComments) l), to')
  where
    to' = case to of
      (SrcSpanAnn EpAnnNotUsed loc)
        ->  SrcSpanAnn (EpAnn (Anchor (realSrcSpan loc) UnchangedAnchor) an cs) loc
      (SrcSpanAnn (EpAnn a an' cs') loc)
        -> SrcSpanAnn (EpAnn a (an' <> an) (cs' <> cs)) loc

-- | Remove the exact print annotations payload, leaving only the
-- anchor and comments.
commentsOnlyA :: Monoid ann => SrcAnn ann -> SrcAnn ann
commentsOnlyA (SrcSpanAnn EpAnnNotUsed loc) = SrcSpanAnn EpAnnNotUsed loc
commentsOnlyA (SrcSpanAnn (EpAnn a _ cs) loc) = (SrcSpanAnn (EpAnn a mempty cs) loc)

-- | Remove the comments, leaving the exact print annotations payload
removeCommentsA :: SrcAnn ann -> SrcAnn ann
removeCommentsA (SrcSpanAnn EpAnnNotUsed loc) = SrcSpanAnn EpAnnNotUsed loc
removeCommentsA (SrcSpanAnn (EpAnn a an _) loc)
  = (SrcSpanAnn (EpAnn a an emptyComments) loc)

-- ---------------------------------------------------------------------
-- Semigroup instances, to allow easy combination of annotaion elements
-- ---------------------------------------------------------------------

instance (Semigroup an) => Semigroup (SrcSpanAnn' an) where
  (SrcSpanAnn a1 l1) <> (SrcSpanAnn a2 l2) = SrcSpanAnn (a1 <> a2) (combineSrcSpans l1 l2)
   -- The critical part about the location is its left edge, and all
   -- annotations must follow it. So we combine them which yields the
   -- largest span

instance (Semigroup a) => Semigroup (EpAnn a) where
  EpAnnNotUsed <> x = x
  x <> EpAnnNotUsed = x
  (EpAnn l1 a1 b1) <> (EpAnn l2 a2 b2) = EpAnn (l1 <> l2) (a1 <> a2) (b1 <> b2)
   -- The critical part about the anchor is its left edge, and all
   -- annotations must follow it. So we combine them which yields the
   -- largest span

instance Ord Anchor where
  compare (Anchor s1 _) (Anchor s2 _) = compare s1 s2

instance Semigroup Anchor where
  Anchor r1 o1 <> Anchor r2 _ = Anchor (combineRealSrcSpans r1 r2) o1

instance Semigroup EpAnnComments where
  EpaComments cs1 <> EpaComments cs2 = EpaComments (cs1 ++ cs2)
  EpaComments cs1 <> EpaCommentsBalanced cs2 as2 = EpaCommentsBalanced (cs1 ++ cs2) as2
  EpaCommentsBalanced cs1 as1 <> EpaComments cs2 = EpaCommentsBalanced (cs1 ++ cs2) as1
  EpaCommentsBalanced cs1 as1 <> EpaCommentsBalanced cs2 as2 = EpaCommentsBalanced (cs1 ++ cs2) (as1++as2)


instance (Monoid a) => Monoid (EpAnn a) where
  mempty = EpAnnNotUsed

instance Semigroup NoEpAnns where
  _ <> _ = NoEpAnns

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

instance (Outputable a) => Outputable (EpAnn a) where
  ppr (EpAnn l a c)  = text "EpAnn" <+> ppr l <+> ppr a <+> ppr c
  ppr EpAnnNotUsed = text "EpAnnNotUsed"

instance Outputable NoEpAnns where
  ppr NoEpAnns = text "NoEpAnns"

instance Outputable Anchor where
  ppr (Anchor a o)        = text "Anchor" <+> ppr a <+> ppr o

instance Outputable AnchorOperation where
  ppr UnchangedAnchor   = text "UnchangedAnchor"
  ppr (MovedAnchor d)   = text "MovedAnchor" <+> ppr d

instance Outputable DeltaPos where
  ppr (SameLine c) = text "SameLine" <+> ppr c
  ppr (DifferentLine l c) = text "DifferentLine" <+> ppr l <+> ppr c

instance Outputable (GenLocated Anchor EpaComment) where
  ppr (L l c) = text "L" <+> ppr l <+> ppr c

instance Outputable EpAnnComments where
  ppr (EpaComments cs) = text "EpaComments" <+> ppr cs
  ppr (EpaCommentsBalanced cs ts) = text "EpaCommentsBalanced" <+> ppr cs <+> ppr ts

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

instance (Outputable a, OutputableBndr e)
     => OutputableBndr (GenLocated (SrcSpanAnn' a) e) where
  pprInfixOcc = pprInfixOcc . unLoc
  pprPrefixOcc = pprPrefixOcc . unLoc

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
  ppr (NameAnnBars a o n b t)
    = text "NameAnnBars" <+> ppr a <+> ppr o <+> ppr n <+> ppr b <+> ppr t
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
