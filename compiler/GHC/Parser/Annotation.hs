{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module GHC.Parser.Annotation (
  -- * Core Exact Print Annotation types
  AnnKeywordId(..),
  EpToken(..), EpUniToken(..),
  getEpTokenSrcSpan, getEpTokenLocs, getEpTokenLoc,
  TokDcolon, TokDarrow, TokRarrow, TokForall,
  EpLayout(..),
  EpaComment(..), EpaCommentTok(..),
  IsUnicodeSyntax(..),
  unicodeAnn,
  HasE(..),

  -- * In-tree Exact Print Annotations
  AddEpAnn(..), addEpAnnLoc,
  EpaLocation, EpaLocation'(..), epaLocationRealSrcSpan,
  TokenLocation(..),
  DeltaPos(..), deltaPos, getDeltaLine,

  EpAnn(..),
  anchor,
  spanAsAnchor, realSpanAsAnchor,
  noSpanAnchor,
  NoAnn(..),

  -- ** Comments in Annotations

  EpAnnComments(..), LEpaComment, NoCommentsLocation, NoComments(..), emptyComments,
  epaToNoCommentsLocation, noCommentsToEpaLocation,
  getFollowingComments, setFollowingComments, setPriorComments,
  EpAnnCO,

  -- ** Annotations in 'GenLocated'
  LocatedA, LocatedL, LocatedC, LocatedN, LocatedAn, LocatedP,
  LocatedLC, LocatedLS, LocatedLW, LocatedLI,
  SrcSpanAnnA, SrcSpanAnnL, SrcSpanAnnP, SrcSpanAnnC, SrcSpanAnnN,
  SrcSpanAnnLC, SrcSpanAnnLW, SrcSpanAnnLS, SrcSpanAnnLI,
  LocatedE,

  -- ** Annotation data types used in 'GenLocated'

  AnnListItem(..), AnnList(..),
  AnnParen(..), ParenType(..), parenTypeKws,
  AnnPragma(..),
  AnnContext(..),
  NameAnn(..), NameAdornment(..),
  NoEpAnns(..),
  AnnSortKey(..), DeclTag(..), BindTag(..),

  -- ** Trailing annotations in lists
  TrailingAnn(..), trailingAnnToAddEpAnn,
  addTrailingAnnToA, addTrailingAnnToL, addTrailingCommaToN,
  noTrailingN,

  -- ** Utilities for converting between different 'GenLocated' when
  -- ** we do not care about the annotations.
  l2l, la2la,
  reLoc,
  HasLoc(..), getHasLocList,

  srcSpan2e, realSrcSpan,

  -- ** Building up annotations
  reAnnL, reAnnC,
  addAnns, addAnnsA, widenSpan, widenSpanL, widenSpanT, widenAnchor, widenAnchorT, widenAnchorS,
  widenLocatedAn, widenLocatedAnL,
  listLocation,

  -- ** Querying annotations
  getLocAnn,
  annParen2AddEpAnn,
  epAnnComments,

  -- ** Working with locations of annotations
  sortLocatedA,
  mapLocA,
  combineLocsA,
  combineSrcSpansA,
  addCLocA,

  -- ** Constructing 'GenLocated' annotation types when we do not care
  -- about annotations.
  HasAnnotation(..),
  locA,
  noLocA,
  getLocA,
  noSrcSpanA,

  -- ** Working with comments in annotations
  noComments, comment, addCommentsToEpAnn, setCommentsEpAnn,
  transferAnnsA, transferAnnsOnlyA, transferCommentsOnlyA,
  transferPriorCommentsA, transferFollowingA,
  commentsOnlyA, removeCommentsA,

  placeholderRealSpan,
  ) where

import GHC.Prelude

import Data.Data
import Data.Function (on)
import Data.List (sortBy, foldl1')
import Data.Semigroup
import GHC.Data.FastString
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Hs.DocString
import GHC.Utils.Outputable hiding ( (<>) )
import GHC.Utils.Panic
import qualified GHC.Data.Strict as Strict
import GHC.Types.SourceText (SourceText (NoSourceText))

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

-- | A token stored in the syntax tree. For example, when parsing a
-- let-expression, we store @EpToken "let"@ and @EpToken "in"@.
-- The locations of those tokens can be used to faithfully reproduce
-- (exactprint) the original program text.
data EpToken (tok :: Symbol)
  = NoEpTok
  | EpTok !EpaLocation

instance KnownSymbol tok => Outputable (EpToken tok) where
  ppr _ = text (symbolVal (Proxy @tok))

-- | With @UnicodeSyntax@, there might be multiple ways to write the same
-- token. For example an arrow could be either @->@ or @→@. This choice must be
-- recorded in order to exactprint such tokens, so instead of @EpToken "->"@ we
-- introduce @EpUniToken "->" "→"@.
data EpUniToken (tok :: Symbol) (utok :: Symbol)
  = NoEpUniTok
  | EpUniTok !EpaLocation !IsUnicodeSyntax

deriving instance Eq (EpToken tok)
deriving instance Eq (EpUniToken tok utok)
deriving instance KnownSymbol tok => Data (EpToken tok)
deriving instance (KnownSymbol tok, KnownSymbol utok) => Data (EpUniToken tok utok)

instance (KnownSymbol tok, KnownSymbol utok) => Outputable (EpUniToken tok utok) where
  ppr NoEpUniTok                 = text $ symbolVal (Proxy @tok)
  ppr (EpUniTok _ NormalSyntax)  = text $ symbolVal (Proxy @tok)
  ppr (EpUniTok _ UnicodeSyntax) = text $ symbolVal (Proxy @utok)

getEpTokenSrcSpan :: EpToken tok -> SrcSpan
getEpTokenSrcSpan NoEpTok = noSrcSpan
getEpTokenSrcSpan (EpTok EpaDelta{}) = noSrcSpan
getEpTokenSrcSpan (EpTok (EpaSpan span)) = span

getEpTokenLocs :: [EpToken tok] -> [EpaLocation]
getEpTokenLocs ls = concatMap go ls
  where
    go NoEpTok   = []
    go (EpTok l) = [l]

getEpTokenLoc :: EpToken tok -> EpaLocation
getEpTokenLoc NoEpTok   = noAnn
getEpTokenLoc (EpTok l) = l

-- TODO:AZ: check we have all of the unicode tokens
type TokDcolon = EpUniToken "::" "∷"
type TokDarrow = EpUniToken "=>"  "⇒"
type TokRarrow = EpUniToken "->" "→"
type TokForall = EpUniToken "forall" "∀"

-- | Layout information for declarations.
data EpLayout =

    -- | Explicit braces written by the user.
    --
    -- @
    -- class C a where { foo :: a; bar :: a }
    -- @
    EpExplicitBraces !(EpToken "{") !(EpToken "}")
  |
    -- | Virtual braces inserted by the layout algorithm.
    --
    -- @
    -- class C a where
    --   foo :: a
    --   bar :: a
    -- @
    EpVirtualBraces
      !Int -- ^ Layout column (indentation level, begins at 1)
  |
    -- | Empty or compiler-generated blocks do not have layout information
    -- associated with them.
    EpNoLayout

deriving instance Data EpLayout

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

addEpAnnLoc :: AddEpAnn -> EpaLocation
addEpAnnLoc (AddEpAnn _ l) = l

type EpaLocation = EpaLocation' [LEpaComment]

epaToNoCommentsLocation :: EpaLocation -> NoCommentsLocation
epaToNoCommentsLocation (EpaSpan ss) = EpaSpan ss
epaToNoCommentsLocation (EpaDelta ss dp []) = EpaDelta ss dp NoComments
epaToNoCommentsLocation (EpaDelta _ _ _ ) = panic "epaToNoCommentsLocation"

noCommentsToEpaLocation :: NoCommentsLocation -> EpaLocation
noCommentsToEpaLocation (EpaSpan ss) = EpaSpan ss
noCommentsToEpaLocation (EpaDelta ss dp NoComments) = EpaDelta ss dp []

-- | Tokens embedded in the AST have an EpaLocation, unless they come from
-- generated code (e.g. by TH).
data TokenLocation = NoTokenLoc | TokenLoc !EpaLocation
               deriving (Data,Eq)

instance Outputable a => Outputable (GenLocated TokenLocation a) where
  ppr (L _ x) = ppr x

-- | Used in the parser only, extract the 'RealSrcSpan' from an
-- 'EpaLocation'. The parser will never insert a 'DeltaPos', so the
-- partial function is safe.
epaLocationRealSrcSpan :: EpaLocation -> RealSrcSpan
epaLocationRealSrcSpan (EpaSpan (RealSrcSpan r _)) = r
epaLocationRealSrcSpan _ = panic "epaLocationRealSrcSpan"

instance Outputable AddEpAnn where
  ppr (AddEpAnn kw ss) = text "AddEpAnn" <+> ppr kw <+> ppr ss

-- ---------------------------------------------------------------------

-- | The exact print annotations (EPAs) are kept in the HsSyn AST for
--   the GhcPs phase. They are usually inserted into the AST by the parser,
--   and in case of generated code (e.g. by TemplateHaskell) they are usually
--   initialized using 'NoAnn' type class.
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
-- print annotation elements.  For example
--
-- @
-- type SrcSpannAnnA = EpAnn AnnListItem
-- @
--
-- is a commonly used type alias that specializes the 'ann' type parameter to
-- 'AnnListItem'.
--
-- The spacing between the items under the scope of a given EpAnn is
-- normally derived from the original 'Anchor'.  But if a sub-element
-- is not in its original position, the required spacing can be
-- captured using an appropriate 'EpaDelta' value for the 'entry' Anchor.
-- This allows us to freely move elements around, and stitch together
-- new AST fragments out of old ones, and have them still printed out
-- in a precise way.
data EpAnn ann
  = EpAnn { entry   :: !EpaLocation
           -- ^ Base location for the start of the syntactic element
           -- holding the annotations.
           , anns     :: !ann -- ^ Annotations added by the Parser
           , comments :: !EpAnnComments
              -- ^ Comments enclosed in the SrcSpan of the element
              -- this `EpAnn` is attached to
           }
        deriving (Data, Eq, Functor)
-- See Note [XRec and Anno in the AST]

anchor :: (EpaLocation' a) -> RealSrcSpan
anchor (EpaSpan (RealSrcSpan r _)) = r
anchor _ = panic "anchor"

spanAsAnchor :: SrcSpan -> (EpaLocation' a)
spanAsAnchor ss  = EpaSpan ss

realSpanAsAnchor :: RealSrcSpan -> (EpaLocation' a)
realSpanAsAnchor s = EpaSpan (RealSrcSpan s Strict.Nothing)

noSpanAnchor :: (NoAnn a) => EpaLocation' a
noSpanAnchor =  EpaDelta noSrcSpan (SameLine 0) noAnn

-- ---------------------------------------------------------------------

-- | When we are parsing we add comments that belong to a particular AST
-- element, and print them together with the element, interleaving
-- them into the output stream.  But when editing the AST to move
-- fragments around it is useful to be able to first separate the
-- comments into those occurring before the AST element and those
-- following it.  The 'EpaCommentsBalanced' constructor is used to do
-- this. The GHC parser will only insert the 'EpaComments' form.
data EpAnnComments = EpaComments
                        { priorComments :: ![LEpaComment] }
                    | EpaCommentsBalanced
                        { priorComments :: ![LEpaComment]
                        , followingComments :: ![LEpaComment] }
        deriving (Data, Eq)

type LEpaComment = GenLocated NoCommentsLocation EpaComment

emptyComments :: EpAnnComments
emptyComments = EpaComments []

-- ---------------------------------------------------------------------
-- Annotations attached to a 'SrcSpan'.
-- ---------------------------------------------------------------------

type LocatedA = GenLocated SrcSpanAnnA
type LocatedN = GenLocated SrcSpanAnnN

type LocatedL = GenLocated SrcSpanAnnL
type LocatedLC = GenLocated SrcSpanAnnLC
type LocatedLS = GenLocated SrcSpanAnnLS
type LocatedLW = GenLocated SrcSpanAnnLW
type LocatedLI = GenLocated SrcSpanAnnLI
type LocatedP = GenLocated SrcSpanAnnP
type LocatedC = GenLocated SrcSpanAnnC

type SrcSpanAnnA = EpAnn AnnListItem
type SrcSpanAnnN = EpAnn NameAnn

type SrcSpanAnnL = EpAnn (AnnList ())
type SrcSpanAnnLC = EpAnn (AnnList [EpToken ","])
type SrcSpanAnnLS = EpAnn (AnnList ())
type SrcSpanAnnLW = EpAnn (AnnList (EpToken "where"))
type SrcSpanAnnLI = EpAnn (AnnList (EpToken "hiding", [EpToken ","]))
type SrcSpanAnnP = EpAnn AnnPragma
type SrcSpanAnnC = EpAnn AnnContext

type LocatedE = GenLocated EpaLocation

-- | General representation of a 'GenLocated' type carrying a
-- parameterised annotation type.
type LocatedAn an = GenLocated (EpAnn an)

{-
Note [XRec and Anno in the AST]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The exact print annotations are captured directly inside the AST, using
TTG extension points. However certain annotations need to be captured
on the Located versions too.  There is a general form for these,
captured in the type 'EpAnn ann' with the specific usage captured in
the 'ann' parameter in different contexts.

Some of the particular use cases are

1) RdrNames, which can have additional items such as backticks or parens

2) Items which occur in lists, and the annotation relates purely
to its usage inside a list.

See the section above this note for the rest.

The Anno type family maps to the specific EpAnn variant for a given
item.

So

  type instance XRec (GhcPass p) a = XRecGhc a
  type XRecGhc a = GenLocated (Anno a) a

  type instance Anno RdrName = SrcSpanAnnN
  type LocatedN = GenLocated SrcSpanAnnN

meaning we can have type LocatedN RdrName

-}

-- ---------------------------------------------------------------------
-- Annotations for items in a list
-- ---------------------------------------------------------------------

-- | Captures the location of punctuation occurring between items,
-- normally in a list.  It is captured as a trailing annotation.
data TrailingAnn
  = AddSemiAnn    { ta_location :: EpaLocation }  -- ^ Trailing ';'
  | AddCommaAnn   { ta_location :: EpaLocation }  -- ^ Trailing ','
  | AddVbarAnn    { ta_location :: EpaLocation }  -- ^ Trailing '|'
  | AddDarrowAnn  { ta_location :: EpaLocation }  -- ^ Trailing '=>'
  | AddDarrowUAnn { ta_location :: EpaLocation }  -- ^ Trailing '⇒'
  deriving (Data, Eq)

instance Outputable TrailingAnn where
  ppr (AddSemiAnn ss)    = text "AddSemiAnn"    <+> ppr ss
  ppr (AddCommaAnn ss)   = text "AddCommaAnn"   <+> ppr ss
  ppr (AddVbarAnn ss)    = text "AddVbarAnn"    <+> ppr ss
  ppr (AddDarrowAnn ss)  = text "AddDarrowAnn"  <+> ppr ss
  ppr (AddDarrowUAnn ss) = text "AddDarrowUAnn" <+> ppr ss

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
data AnnList a
  = AnnList {
      al_anchor    :: !(Maybe EpaLocation), -- ^ start point of a list having layout
      al_open      :: !(Maybe AddEpAnn),
      al_close     :: !(Maybe AddEpAnn),
      al_semis     :: [EpToken ";"], -- decls
      al_rest      :: !a,
      al_trailing  :: ![TrailingAnn] -- ^ items appearing after the
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
  deriving (Eq, Ord, Data, Show)

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
  -- | Used for @(,,,)@, or @(#,,,#)@
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
      nann_unicode   :: Bool,
      nann_mopen     :: Maybe EpaLocation,
      nann_name      :: EpaLocation,
      nann_mclose    :: Maybe EpaLocation,
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
  -- which has no Api Annotation.
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
      apr_open      :: EpaLocation,
      apr_close     :: EpaLocation,
      apr_squares   :: (EpToken "[", EpToken "]"),
      apr_loc1      :: EpaLocation,
      apr_loc2      :: EpaLocation,
      apr_type      :: EpToken "type",
      apr_module    :: EpToken "module"
      } deriving (Data,Eq)

-- ---------------------------------------------------------------------

-- | Captures the sort order of sub elements for `ValBinds`,
-- `ClassDecl`, `ClsInstDecl`
data AnnSortKey tag
  -- See Note [AnnSortKey] below
  = NoAnnSortKey
  | AnnSortKey [tag]
  deriving (Data, Eq)

-- | Used to track of interleaving of binds and signatures for ValBind
data BindTag
  -- See Note [AnnSortKey] below
  = BindTag
  | SigDTag
  deriving (Eq,Data,Ord,Show)

-- | Used to track interleaving of class methods, class signatures,
-- associated types and associate type defaults in `ClassDecl` and
-- `ClsInstDecl`.
data DeclTag
  -- See Note [AnnSortKey] below
  = ClsMethodTag
  | ClsSigTag
  | ClsAtTag
  | ClsAtdTag
  deriving (Eq,Data,Ord,Show)

{-
Note [AnnSortKey]
~~~~~~~~~~~~~~~~~

For some constructs in the ParsedSource we have mixed lists of items
that can be freely intermingled.

An example is the binds in a where clause, captured in

    ValBinds
        (XValBinds idL idR)
        (LHsBindsLR idL idR) [LSig idR]

This keeps separate ordered collections of LHsBind GhcPs and LSig GhcPs.

But there is no constraint on the original source code as to how these
should appear, so they can have all the signatures first, then their
binds, or grouped with a signature preceding each bind.

   fa :: Int
   fa = 1

   fb :: Char
   fb = 'c'

Or

   fa :: Int
   fb :: Char

   fb = 'c'
   fa = 1

When exact printing these, we need to restore the original order. As
initially parsed we have the SrcSpan, and can sort on those. But if we
have modified the AST prior to printing, we cannot rely on the
SrcSpans for order any more.

The bag of LHsBind GhcPs is physically ordered, as is the list of LSig
GhcPs. So in effect we have a list of binds in the order we care
about, and a list of sigs in the order we care about. The only problem
is to know how to merge the lists.

This is where AnnSortKey comes in, which we store in the TTG extension
point for ValBinds.

    data AnnSortKey tag
      = NoAnnSortKey
      | AnnSortKey [tag]

When originally parsed, with SrcSpans we can rely on, we do not need
any extra information, so we tag it with NoAnnSortKey.

If the binds and signatures are updated in any way, such that we can
no longer rely on their SrcSpans (e.g. they are copied from elsewhere,
parsed from scratch for insertion, have a fake SrcSpan), we use
`AnnSortKey [BindTag]` to keep track.

    data BindTag
      = BindTag
      | SigDTag

We use it as a merge selector, and have one entry for each bind and
signature.

So for the first example we have

  binds: fa = 1 , fb = 'c'
  sigs:  fa :: Int, fb :: Char
  tags: SigDTag, BindTag, SigDTag, BindTag

so we draw first from the signatures, then the binds, and same again.

For the second example we have

  binds: fb = 'c', fa = 1
  sigs:  fa :: Int, fb :: Char
  tags: SigDTag, SigDTag, BindTag, BindTag

so we draw two signatures, then two binds.

We do similar for ClassDecl and ClsInstDecl, but we have four
different lists we must manage. For this we use DeclTag.

-}

-- ---------------------------------------------------------------------

-- | Convert a 'TrailingAnn' to an 'AddEpAnn'
trailingAnnToAddEpAnn :: TrailingAnn -> AddEpAnn
trailingAnnToAddEpAnn (AddSemiAnn ss)    = AddEpAnn AnnSemi ss
trailingAnnToAddEpAnn (AddCommaAnn ss)   = AddEpAnn AnnComma ss
trailingAnnToAddEpAnn (AddVbarAnn ss)    = AddEpAnn AnnVbar ss
trailingAnnToAddEpAnn (AddDarrowUAnn ss) = AddEpAnn AnnDarrowU ss
trailingAnnToAddEpAnn (AddDarrowAnn ss)  = AddEpAnn AnnDarrow ss

-- | Helper function used in the parser to add a 'TrailingAnn' items
-- to an existing annotation.
addTrailingAnnToL :: TrailingAnn -> EpAnnComments
                  -> EpAnn (AnnList a) -> EpAnn (AnnList a)
addTrailingAnnToL t cs n = n { anns = addTrailing (anns n)
                               , comments = comments n <> cs }
  where
    -- See Note [list append in addTrailing*]
    addTrailing n = n { al_trailing = al_trailing n ++ [t]}

-- | Helper function used in the parser to add a 'TrailingAnn' items
-- to an existing annotation.
addTrailingAnnToA :: TrailingAnn -> EpAnnComments
                  -> EpAnn AnnListItem -> EpAnn AnnListItem
addTrailingAnnToA t cs n = n { anns = addTrailing (anns n)
                               , comments = comments n <> cs }
  where
    -- See Note [list append in addTrailing*]
    addTrailing n = n { lann_trailing = lann_trailing n ++ [t] }

-- | Helper function used in the parser to add a comma location to an
-- existing annotation.
addTrailingCommaToN :: EpAnn NameAnn -> EpaLocation -> EpAnn NameAnn
addTrailingCommaToN  n l = n { anns = addTrailing (anns n) l }
  where
    -- See Note [list append in addTrailing*]
    addTrailing :: NameAnn -> EpaLocation -> NameAnn
    addTrailing n l = n { nann_trailing = nann_trailing n ++ [AddCommaAnn l]}

noTrailingN :: SrcSpanAnnN -> SrcSpanAnnN
noTrailingN s = s { anns = (anns s) { nann_trailing = [] } }

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

-- |Helper function for converting annotation types.
--  Discards any annotations
l2l :: (HasLoc a, HasAnnotation b) => a -> b
l2l a = noAnnSrcSpan (getHasLoc a)

-- |Helper function for converting annotation types.
--  Discards any annotations
la2la :: (HasLoc l, HasAnnotation l2) => GenLocated l a -> GenLocated l2 a
la2la (L la a) = L (noAnnSrcSpan (getHasLoc la)) a

locA :: (HasLoc a) => a -> SrcSpan
locA = getHasLoc

reLoc :: (HasLoc (GenLocated a e), HasAnnotation b)
      => GenLocated a e -> GenLocated b e
reLoc (L la a) = L (noAnnSrcSpan $ locA (L la a) ) a


-- ---------------------------------------------------------------------

class HasAnnotation e where
  noAnnSrcSpan :: SrcSpan -> e

instance HasAnnotation SrcSpan where
  noAnnSrcSpan l = l

instance HasAnnotation EpaLocation where
  noAnnSrcSpan l = EpaSpan l

instance (NoAnn ann) => HasAnnotation (EpAnn ann) where
  noAnnSrcSpan l = EpAnn (spanAsAnchor l) noAnn emptyComments

noLocA :: (HasAnnotation e) => a -> GenLocated e a
noLocA = L (noAnnSrcSpan noSrcSpan)

getLocA :: (HasLoc a) => GenLocated a e -> SrcSpan
getLocA = getHasLoc

noSrcSpanA :: (HasAnnotation e) => e
noSrcSpanA = noAnnSrcSpan noSrcSpan

-- ---------------------------------------------------------------------

class NoAnn a where
  -- | equivalent of `mempty`, but does not need Semigroup
  noAnn :: a

-- ---------------------------------------------------------------------

class HasLoc a where
  -- ^ conveniently calculate locations for things without locations attached
  getHasLoc :: a -> SrcSpan

instance (HasLoc l) => HasLoc (GenLocated l a) where
  getHasLoc (L l _) = getHasLoc l

instance HasLoc SrcSpan where
  getHasLoc l = l

instance (HasLoc a) => (HasLoc (Maybe a)) where
  getHasLoc (Just a) = getHasLoc a
  getHasLoc Nothing = noSrcSpan

instance HasLoc (EpAnn a) where
  getHasLoc (EpAnn l _ _) = getHasLoc l

instance HasLoc EpaLocation where
  getHasLoc (EpaSpan l) = l
  getHasLoc (EpaDelta l _ _) = l

instance HasLoc (EpToken tok) where
  getHasLoc = getEpTokenSrcSpan

instance HasLoc (EpUniToken tok utok) where
  getHasLoc NoEpUniTok = noSrcSpan
  getHasLoc (EpUniTok l _) = getHasLoc l

getHasLocList :: HasLoc a => [a] -> SrcSpan
getHasLocList [] = noSrcSpan
getHasLocList xs = foldl1' combineSrcSpans $ map getHasLoc xs

-- ---------------------------------------------------------------------

realSrcSpan :: SrcSpan -> RealSrcSpan
realSrcSpan (RealSrcSpan s _) = s
realSrcSpan _ = mkRealSrcSpan l l -- AZ temporary
  where
    l = mkRealSrcLoc (fsLit "realSrcSpan") (-1) (-1)

srcSpan2e :: SrcSpan -> EpaLocation
srcSpan2e ss@(RealSrcSpan _ _) = EpaSpan ss
srcSpan2e span = EpaSpan (RealSrcSpan (realSrcSpan span) Strict.Nothing)

reAnnC :: AnnContext -> EpAnnComments -> Located a -> LocatedC a
reAnnC anns cs (L l a) = L (EpAnn (spanAsAnchor l) anns cs) a

reAnnL :: ann -> EpAnnComments -> Located e -> GenLocated (EpAnn ann) e
reAnnL anns cs (L l a) = L (EpAnn (spanAsAnchor l) anns cs) a

getLocAnn :: Located a  -> SrcSpanAnnA
getLocAnn (L l _) = noAnnSrcSpan l

addAnns :: EpAnn [AddEpAnn] -> [AddEpAnn] -> EpAnnComments -> EpAnn [AddEpAnn]
addAnns (EpAnn l as1 cs) as2 cs2
  = EpAnn (widenAnchor l (as1 ++ as2)) (as1 ++ as2) (cs <> cs2)

-- AZ:TODO use widenSpan here too
addAnnsA :: SrcSpanAnnA -> [TrailingAnn] -> EpAnnComments -> SrcSpanAnnA
addAnnsA (EpAnn l as1 cs) as2 cs2
  = EpAnn l (AnnListItem (lann_trailing as1 ++ as2)) (cs <> cs2)

-- | The annotations need to all come after the anchor.  Make sure
-- this is the case.
widenSpan :: SrcSpan -> [AddEpAnn] -> SrcSpan
widenSpan s as = foldl combineSrcSpans s (go as)
  where
    go [] = []
    go (AddEpAnn _ (EpaSpan (RealSrcSpan s mb)):rest) = RealSrcSpan s mb : go rest
    go (AddEpAnn _ (EpaSpan _):rest) = go rest
    go (AddEpAnn _ (EpaDelta _ _ _):rest) = go rest

-- | The annotations need to all come after the anchor.  Make sure
-- this is the case.
widenSpanL :: SrcSpan -> [EpaLocation] -> SrcSpan
widenSpanL s as = foldl combineSrcSpans s (go as)
  where
    go [] = []
    go ((EpaSpan (RealSrcSpan s mb):rest)) = RealSrcSpan s mb : go rest
    go ((EpaSpan _):rest) = go rest
    go ((EpaDelta _ _ _):rest) = go rest

widenSpanT :: SrcSpan -> EpToken tok -> SrcSpan
widenSpanT l (EpTok loc) = widenSpanL l [loc]
widenSpanT l NoEpTok = l

-- | The annotations need to all come after the anchor.  Make sure
-- this is the case.
widenRealSpan :: RealSrcSpan -> [AddEpAnn] -> RealSrcSpan
widenRealSpan s as = foldl combineRealSrcSpans s (go as)
  where
    go [] = []
    go (AddEpAnn _ (EpaSpan (RealSrcSpan s _)):rest) = s : go rest
    go (AddEpAnn _ _:rest) = go rest

realSpanFromAnns :: [AddEpAnn] -> Strict.Maybe RealSrcSpan
realSpanFromAnns as = go Strict.Nothing as
  where
    combine Strict.Nothing r  = Strict.Just r
    combine (Strict.Just l) r = Strict.Just $ combineRealSrcSpans l r

    go acc [] = acc
    go acc (AddEpAnn _ (EpaSpan (RealSrcSpan s _b)):rest) = go (combine acc s) rest
    go acc (AddEpAnn _ _             :rest) = go acc rest

bufSpanFromAnns :: [AddEpAnn] -> Strict.Maybe BufSpan
bufSpanFromAnns as =  go Strict.Nothing as
  where
    combine Strict.Nothing r  = Strict.Just r
    combine (Strict.Just l) r = Strict.Just $ combineBufSpans l r

    go acc [] = acc
    go acc (AddEpAnn _ (EpaSpan (RealSrcSpan _ (Strict.Just mb))):rest) = go (combine acc mb) rest
    go acc (AddEpAnn _ _:rest) = go acc rest

listLocation :: [LocatedAn an a] -> EpaLocation
listLocation as = EpaSpan (go noSrcSpan as)
  where
    combine l r = combineSrcSpans l r

    go acc [] = acc
    go acc (L (EpAnn (EpaSpan s) _ _) _:rest) = go (combine acc s) rest
    go acc (_:rest) = go acc rest

widenAnchor :: EpaLocation -> [AddEpAnn] -> EpaLocation
widenAnchor (EpaSpan (RealSrcSpan s mb)) as
  = EpaSpan (RealSrcSpan (widenRealSpan s as) (liftA2 combineBufSpans mb  (bufSpanFromAnns as)))
widenAnchor (EpaSpan us) _ = EpaSpan us
widenAnchor a@EpaDelta{} as = case (realSpanFromAnns as) of
                                    Strict.Nothing -> a
                                    Strict.Just r -> EpaSpan (RealSrcSpan r Strict.Nothing)

widenAnchorT :: EpaLocation -> EpToken tok -> EpaLocation
widenAnchorT (EpaSpan ss) (EpTok l) = widenAnchorS l ss
widenAnchorT ss _ = ss

widenAnchorS :: EpaLocation -> SrcSpan -> EpaLocation
widenAnchorS (EpaSpan (RealSrcSpan s mbe)) (RealSrcSpan r mbr)
  = EpaSpan (RealSrcSpan (combineRealSrcSpans s r) (liftA2 combineBufSpans mbe mbr))
widenAnchorS (EpaSpan us) _ = EpaSpan us
widenAnchorS EpaDelta{} (RealSrcSpan r mb) = EpaSpan (RealSrcSpan r mb)
widenAnchorS anc _ = anc

widenLocatedAn :: EpAnn an -> [AddEpAnn] -> EpAnn an
widenLocatedAn (EpAnn (EpaSpan l) a cs) as = EpAnn (spanAsAnchor l') a cs
  where
    l' = widenSpan l as
widenLocatedAn (EpAnn anc a cs) _as = EpAnn anc a cs

widenLocatedAnL :: EpAnn an -> [EpaLocation] -> EpAnn an
widenLocatedAnL (EpAnn (EpaSpan l) a cs) as = EpAnn (spanAsAnchor l') a cs
  where
    l' = widenSpanL l as
widenLocatedAnL (EpAnn anc a cs) _as = EpAnn anc a cs

annParen2AddEpAnn :: AnnParen -> [AddEpAnn]
annParen2AddEpAnn (AnnParen pt o c)
  = [AddEpAnn ai o, AddEpAnn ac c]
  where
    (ai,ac) = parenTypeKws pt

epAnnComments :: EpAnn an -> EpAnnComments
epAnnComments (EpAnn _ _ cs) = cs

-- ---------------------------------------------------------------------
sortLocatedA :: (HasLoc (EpAnn a)) => [GenLocated (EpAnn a) e] -> [GenLocated (EpAnn a) e]
sortLocatedA = sortBy (leftmost_smallest `on` getLocA)

mapLocA :: (NoAnn ann) => (a -> b) -> GenLocated SrcSpan a -> GenLocated (EpAnn ann) b
mapLocA f (L l a) = L (noAnnSrcSpan l) (f a)

-- AZ:TODO: move this somewhere sane
combineLocsA :: Semigroup a => GenLocated (EpAnn a) e1 -> GenLocated (EpAnn a) e2 -> EpAnn a
combineLocsA (L a _) (L b _) = combineSrcSpansA a b

combineSrcSpansA :: Semigroup a => EpAnn a -> EpAnn a -> EpAnn a
combineSrcSpansA aa ab = aa <> ab

-- | Combine locations from two 'Located' things and add them to a third thing
addCLocA :: (HasLoc a, HasLoc b, HasAnnotation l)
         => a -> b -> c -> GenLocated l c
addCLocA a b c = L (noAnnSrcSpan $ combineSrcSpans (getHasLoc a) (getHasLoc b)) c

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
noComments = EpAnn noSpanAnchor NoEpAnns emptyComments

-- TODO:AZ get rid of this
placeholderRealSpan :: RealSrcSpan
placeholderRealSpan = realSrcLocSpan (mkRealSrcLoc (mkFastString "placeholder") (-1) (-1))

comment :: RealSrcSpan -> EpAnnComments -> EpAnnCO
comment loc cs = EpAnn (EpaSpan (RealSrcSpan loc Strict.Nothing)) NoEpAnns cs

-- ---------------------------------------------------------------------
-- Utilities for managing comments in an `EpAnn a` structure.
-- ---------------------------------------------------------------------

-- | Add additional comments to a 'EpAnn', used for manipulating the
-- AST prior to exact printing the changed one.
addCommentsToEpAnn :: (NoAnn ann) => EpAnn ann -> EpAnnComments -> EpAnn ann
addCommentsToEpAnn (EpAnn a an cs) cs' = EpAnn a an (cs <> cs')

-- | Replace any existing comments on a 'EpAnn', used for manipulating the
-- AST prior to exact printing the changed one.
setCommentsEpAnn :: (NoAnn ann) => EpAnn ann -> EpAnnComments -> EpAnn ann
setCommentsEpAnn (EpAnn a an _) cs = (EpAnn a an cs)

-- | Transfer comments and trailing items from the annotations in the
-- first 'SrcSpanAnnA' argument to those in the second.
transferAnnsA :: SrcSpanAnnA -> SrcSpanAnnA -> (SrcSpanAnnA,  SrcSpanAnnA)
transferAnnsA (EpAnn a an cs) (EpAnn a' an' cs')
  = (EpAnn a noAnn emptyComments, EpAnn a' (an' <> an) (cs' <> cs))

-- | Transfer trailing items but not comments from the annotations in the
-- first 'SrcSpanAnnA' argument to those in the second.
transferFollowingA :: SrcSpanAnnA -> SrcSpanAnnA -> (SrcSpanAnnA,  SrcSpanAnnA)
transferFollowingA (EpAnn a1 an1 cs1) (EpAnn a2 an2 cs2)
  = (EpAnn a1 noAnn cs1', EpAnn a2 (an1 <> an2) cs2')
  where
    pc = priorComments cs1
    fc = getFollowingComments cs1
    cs1' = setPriorComments emptyComments pc
    cs2' = setFollowingComments cs2 fc

-- | Transfer trailing items from the annotations in the
-- first 'SrcSpanAnnA' argument to those in the second.
transferAnnsOnlyA :: SrcSpanAnnA -> SrcSpanAnnA -> (SrcSpanAnnA,  SrcSpanAnnA)
transferAnnsOnlyA (EpAnn a an cs) (EpAnn a' an' cs')
  = (EpAnn a noAnn cs, EpAnn a' (an' <> an) cs')

-- | Transfer comments from the annotations in the
-- first 'SrcSpanAnnA' argument to those in the second.
transferCommentsOnlyA :: EpAnn a -> EpAnn b -> (EpAnn a,  EpAnn b)
transferCommentsOnlyA (EpAnn a an cs) (EpAnn a' an' cs')
  = (EpAnn a an emptyComments, EpAnn a' an' (cs <> cs'))

-- | Transfer prior comments only from the annotations in the
-- first 'SrcSpanAnnA' argument to those in the second.
transferPriorCommentsA :: SrcSpanAnnA -> SrcSpanAnnA -> (SrcSpanAnnA,  SrcSpanAnnA)
transferPriorCommentsA (EpAnn a1 an1 cs1) (EpAnn a2 an2 cs2)
  = (EpAnn a1 an1 cs1', EpAnn a2 an2 cs2')
  where
    pc = priorComments cs1
    fc = getFollowingComments cs1
    cs1' = setFollowingComments emptyComments fc
    cs2' = setPriorComments cs2 (priorComments cs2 <> pc)


-- | Remove the exact print annotations payload, leaving only the
-- anchor and comments.
commentsOnlyA :: NoAnn ann => EpAnn ann -> EpAnn ann
commentsOnlyA (EpAnn a _ cs) = EpAnn a noAnn cs

-- | Remove the comments, leaving the exact print annotations payload
removeCommentsA :: EpAnn ann -> EpAnn ann
removeCommentsA (EpAnn a an _) = EpAnn a an emptyComments

-- ---------------------------------------------------------------------
-- Semigroup instances, to allow easy combination of annotation elements
-- ---------------------------------------------------------------------

instance (Semigroup a) => Semigroup (EpAnn a) where
  (EpAnn l1 a1 b1) <> (EpAnn l2 a2 b2) = EpAnn (l1 <> l2) (a1 <> a2) (b1 <> b2)
   -- The critical part about the anchor is its left edge, and all
   -- annotations must follow it. So we combine them which yields the
   -- largest span

instance Semigroup EpaLocation where
  EpaSpan s1       <> EpaSpan s2        = EpaSpan (combineSrcSpans s1 s2)
  EpaSpan s1       <> _                 = EpaSpan s1
  _                <> EpaSpan s2        = EpaSpan s2
  EpaDelta s1 dp1 cs1 <> EpaDelta s2 _dp2 cs2 = EpaDelta (combineSrcSpans s1 s2) dp1 (cs1<>cs2)

instance Semigroup EpAnnComments where
  EpaComments cs1 <> EpaComments cs2 = EpaComments (cs1 ++ cs2)
  EpaComments cs1 <> EpaCommentsBalanced cs2 as2 = EpaCommentsBalanced (cs1 ++ cs2) as2
  EpaCommentsBalanced cs1 as1 <> EpaComments cs2 = EpaCommentsBalanced (cs1 ++ cs2) as1
  EpaCommentsBalanced cs1 as1 <> EpaCommentsBalanced cs2 as2 = EpaCommentsBalanced (cs1 ++ cs2) (as1++as2)

instance Semigroup AnnListItem where
  (AnnListItem l1) <> (AnnListItem l2) = AnnListItem (l1 <> l2)

instance Semigroup (AnnSortKey tag) where
  NoAnnSortKey <> x = x
  x <> NoAnnSortKey = x
  AnnSortKey ls1 <> AnnSortKey ls2 = AnnSortKey (ls1 <> ls2)

instance Monoid (AnnSortKey tag) where
  mempty = NoAnnSortKey

-- ---------------------------------------------------------------------
-- NoAnn instances
-- ---------------------------------------------------------------------

instance NoAnn EpaLocation where
  noAnn = EpaDelta noSrcSpan (SameLine 0) []

instance NoAnn AnnKeywordId where
  noAnn = Annlarrowtail  {- gotta pick one -}

instance NoAnn AddEpAnn where
  noAnn = AddEpAnn noAnn noAnn

instance NoAnn [a] where
  noAnn = []

instance NoAnn (Maybe a) where
  noAnn = Nothing

instance (NoAnn a, NoAnn b) => NoAnn (a, b) where
  noAnn = (noAnn, noAnn)

instance (NoAnn a, NoAnn b, NoAnn c) => NoAnn (a, b, c) where
  noAnn = (noAnn, noAnn, noAnn)

instance (NoAnn a, NoAnn b, NoAnn c, NoAnn d) => NoAnn (a, b, c, d) where
  noAnn = (noAnn, noAnn, noAnn, noAnn)

instance NoAnn Bool where
  noAnn = False

instance NoAnn () where
  noAnn = ()

instance (NoAnn ann) => NoAnn (EpAnn ann) where
  noAnn = EpAnn noSpanAnchor noAnn emptyComments

instance NoAnn NoEpAnns where
  noAnn = NoEpAnns

instance NoAnn AnnListItem where
  noAnn = AnnListItem []

instance NoAnn AnnContext where
  noAnn = AnnContext Nothing [] []

instance NoAnn a => NoAnn (AnnList a) where
  noAnn = AnnList Nothing Nothing Nothing noAnn noAnn []

instance NoAnn NameAnn where
  noAnn = NameAnnTrailing []

instance NoAnn AnnPragma where
  noAnn = AnnPragma noAnn noAnn noAnn noAnn noAnn noAnn noAnn

instance NoAnn AnnParen where
  noAnn = AnnParen AnnParens noAnn noAnn

instance NoAnn (EpToken s) where
  noAnn = NoEpTok

instance NoAnn (EpUniToken s t) where
  noAnn = NoEpUniTok

instance NoAnn SourceText where
  noAnn = NoSourceText

-- ---------------------------------------------------------------------

instance (Outputable a) => Outputable (EpAnn a) where
  ppr (EpAnn l a c)  = text "EpAnn" <+> ppr l <+> ppr a <+> ppr c

instance Outputable NoEpAnns where
  ppr NoEpAnns = text "NoEpAnns"

instance Outputable (GenLocated NoCommentsLocation EpaComment) where
  ppr (L l c) = text "L" <+> ppr l <+> ppr c

instance Outputable EpAnnComments where
  ppr (EpaComments cs) = text "EpaComments" <+> ppr cs
  ppr (EpaCommentsBalanced cs ts) = text "EpaCommentsBalanced" <+> ppr cs <+> ppr ts

instance (NamedThing (Located a)) => NamedThing (LocatedAn an a) where
  getName (L l a) = getName (L (locA l) a)

instance Outputable AnnContext where
  ppr (AnnContext a o c) = text "AnnContext" <+> ppr a <+> ppr o <+> ppr c

instance Outputable BindTag where
  ppr tag = text $ show tag

instance Outputable DeclTag where
  ppr tag = text $ show tag

instance Outputable tag => Outputable (AnnSortKey tag) where
  ppr NoAnnSortKey    = text "NoAnnSortKey"
  ppr (AnnSortKey ls) = text "AnnSortKey" <+> ppr ls

instance Outputable IsUnicodeSyntax where
  ppr = text . show

instance (Outputable a, Outputable e)
     => Outputable (GenLocated (EpAnn a) e) where
  ppr = pprLocated

instance (Outputable a, OutputableBndr e)
     => OutputableBndr (GenLocated (EpAnn a) e) where
  pprInfixOcc = pprInfixOcc . unLoc
  pprPrefixOcc = pprPrefixOcc . unLoc

instance (Outputable e)
     => Outputable (GenLocated EpaLocation e) where
  ppr = pprLocated

instance Outputable ParenType where
  ppr t = text (show t)

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
  ppr (NameAnnRArrow u o n c t)
    = text "NameAnnRArrow" <+> ppr u <+> ppr o <+> ppr n <+> ppr c <+> ppr t
  ppr (NameAnnQuote q n t)
    = text "NameAnnQuote" <+> ppr q <+> ppr n <+> ppr t
  ppr (NameAnnTrailing t)
    = text "NameAnnTrailing" <+> ppr t

instance (Outputable a) => Outputable (AnnList a) where
  ppr (AnnList anc o c s a t)
    = text "AnnList" <+> ppr anc <+> ppr o <+> ppr c <+> ppr s <+> ppr a <+> ppr t

instance Outputable AnnPragma where
  ppr (AnnPragma o c s l ca t m)
    = text "AnnPragma" <+> ppr o <+> ppr c <+> ppr s <+> ppr l
                       <+> ppr ca <+> ppr ca <+> ppr t <+> ppr m
