{-# LANGUAGE DeriveDataTypeable #-}

module ApiAnnotation (
  getAnnotation, getAndRemoveAnnotation,
  getAnnotationComments,getAndRemoveAnnotationComments,
  ApiAnns,
  ApiAnnKey,
  AnnKeywordId(..),
  AnnotationComment(..),
  LRdrName -- Exists for haddocks only
  ) where

import RdrName
import Outputable
import SrcLoc
import qualified Data.Map as Map
import Data.Data


{-
Note [Api annotations]
~~~~~~~~~~~~~~~~~~~~~~
In order to do source to source conversions using the GHC API, the
locations of all elements of the original source needs to be tracked.
The includes keywords such as 'let' / 'in' / 'do' etc as well as
punctuation such as commas and braces, and also comments.

These are captured in a structure separate from the parse tree, and
returned in the pm_annotations field of the ParsedModule type.

The non-comment annotations are stored indexed to the SrcSpan of the
AST element containing them, together with a AnnKeywordId value
identifying the specific keyword being captured.

> type ApiAnnKey = (SrcSpan,AnnKeywordId)
>
> Map.Map ApiAnnKey SrcSpan

So

> let X = 1 in 2 *x

would result in the AST element

  L span (HsLet (binds for x = 1) (2 * x))

and the annotations

  (span,AnnLet) having the location of the 'let' keyword
  (span,AnnIn)  having the location of the 'in' keyword


The comments are indexed to the SrcSpan of the lowest AST element
enclosing them

> Map.Map SrcSpan [Located AnnotationComment]

So the full ApiAnns type is

> type ApiAnns = ( Map.Map ApiAnnKey SrcSpan
>                , Map.Map SrcSpan [Located AnnotationComment])


This is done in the lexer / parser as follows.


The PState variable in the lexer has the following variables added

>  annotations :: [(ApiAnnKey,[SrcSpan])],
>  comment_q :: [Located AnnotationComment],
>  annotations_comments :: [(SrcSpan,[Located AnnotationComment])]

The first and last store the values that end up in the ApiAnns value
at the end via Map.fromList

The comment_q captures comments as they are seen in the token stream,
so that when they are ready to be allocated via the parser they are
available.

The parser interacts with the lexer using the function

> addAnnotation :: SrcSpan -> AnnKeywordId -> SrcSpan -> P ()

which takes the AST element SrcSpan, the annotation keyword and the
target SrcSpan.

This adds the annotation to the `annotations` field of `PState` and
transfers any comments in `comment_q` to the `annotations_comments`
field.

Parser
------

The parser implements a number of helper types and methods for the
capture of annotations

> type AddAnn = (SrcSpan -> P ())
>
> mj :: AnnKeywordId -> Located e -> (SrcSpan -> P ())
> mj a l = (\s -> addAnnotation s a (gl l))

AddAnn represents the addition of an annotation a to a provided
SrcSpan, and `mj` constructs an AddAnn value.

> ams :: Located a -> [AddAnn] -> P (Located a)
> ams a@(L l _) bs = (mapM_ (\a -> a l) bs) >> return a

So the production in Parser.y for the HsLet AST element is

        | 'let' binds 'in' exp    {% ams (sLL $1 $> $ HsLet (snd $ unLoc $2) $4)
                                         (mj AnnLet $1:mj AnnIn $3
                                           :(fst $ unLoc $2)) }

This adds an AnnLet annotation for 'let', an AnnIn for 'in', as well
as any annotations that may arise in the binds. This will include open
and closing braces if they are used to delimit the let expressions.

The wiki page describing this feature is
https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations

-}
-- ---------------------------------------------------------------------

type ApiAnns = ( Map.Map ApiAnnKey [SrcSpan]
               , Map.Map SrcSpan [Located AnnotationComment])

type ApiAnnKey = (SrcSpan,AnnKeywordId)


-- | Retrieve a list of annotation 'SrcSpan's based on the 'SrcSpan'
-- of the annotated AST element, and the known type of the annotation.
getAnnotation :: ApiAnns -> SrcSpan -> AnnKeywordId -> [SrcSpan]
getAnnotation (anns,_) span ann
   = case Map.lookup (span,ann) anns of
       Nothing -> []
       Just ss -> ss

-- | Retrieve a list of annotation 'SrcSpan's based on the 'SrcSpan'
-- of the annotated AST element, and the known type of the annotation.
-- The list is removed from the annotations.
getAndRemoveAnnotation :: ApiAnns -> SrcSpan -> AnnKeywordId
                       -> ([SrcSpan],ApiAnns)
getAndRemoveAnnotation (anns,cs) span ann
   = case Map.lookup (span,ann) anns of
       Nothing -> ([],(anns,cs))
       Just ss -> (ss,(Map.delete (span,ann) anns,cs))

-- |Retrieve the comments allocated to the current 'SrcSpan'
--
--  Note: A given 'SrcSpan' may appear in multiple AST elements,
--  beware of duplicates
getAnnotationComments :: ApiAnns -> SrcSpan -> [Located AnnotationComment]
getAnnotationComments (_,anns) span =
  case Map.lookup span anns of
    Just cs -> cs
    Nothing -> []

-- |Retrieve the comments allocated to the current 'SrcSpan', and
-- remove them from the annotations
getAndRemoveAnnotationComments :: ApiAnns -> SrcSpan
                               -> ([Located AnnotationComment],ApiAnns)
getAndRemoveAnnotationComments (anns,canns) span =
  case Map.lookup span canns of
    Just cs -> (cs,(anns,Map.delete span canns))
    Nothing -> ([],(anns,canns))

-- --------------------------------------------------------------------

-- | API Annotations exist so that tools can perform source to source
-- conversions of Haskell code. They are used to keep track of the
-- various syntactic keywords that are not captured in the existing
-- AST.
--
-- The annotations, together with original source comments are made
-- available in the @'pm_annotations'@ field of @'GHC.ParsedModule'@.
-- Comments are only retained if @'Opt_KeepRawTokenStream'@ is set in
-- @'DynFlags.DynFlags'@ before parsing.
--
-- The wiki page describing this feature is
-- https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations
--
-- Note: in general the names of these are taken from the
-- corresponding token, unless otherwise noted
-- See note [Api annotations] above for details of the usage
data AnnKeywordId
    = AnnAs
    | AnnAt
    | AnnBang  -- ^ '!'
    | AnnBackquote -- ^ '`'
    | AnnBy
    | AnnCase -- ^ case or lambda case
    | AnnClass
    | AnnClose -- ^  '\#)' or '\#-}'  etc
    | AnnCloseC -- ^ '}'
    | AnnCloseP -- ^ ')'
    | AnnCloseS -- ^ ']'
    | AnnColon
    | AnnComma -- ^ as a list separator
    | AnnCommaTuple -- ^ in a RdrName for a tuple
    | AnnDarrow -- ^ '=>'
    | AnnData
    | AnnDcolon -- ^ '::'
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
    | AnnLet
    | AnnMdo
    | AnnMinus -- ^ '-'
    | AnnModule
    | AnnNewtype
    | AnnName -- ^ where a name loses its location in the AST, this carries it
    | AnnOf
    | AnnOpen   -- ^ '(\#' or '{-\# LANGUAGE' etc
    | AnnOpenC   -- ^ '{'
    | AnnOpenP   -- ^ '('
    | AnnOpenS   -- ^ '['
    | AnnPackageName
    | AnnPattern
    | AnnProc
    | AnnQualified
    | AnnRarrow -- ^ '->'
    | AnnRec
    | AnnRole
    | AnnSafe
    | AnnSemi -- ^ ';'
    | AnnSimpleQuote -- ^ '''
    | AnnStatic -- ^ 'static'
    | AnnThen
    | AnnThIdSplice -- ^ '$'
    | AnnThIdTySplice -- ^ '$$'
    | AnnTilde -- ^ '~'
    | AnnTildehsh -- ^ '~#'
    | AnnType
    | AnnUnit -- ^ '()' for types
    | AnnUsing
    | AnnVal  -- ^ e.g. INTEGER
    | AnnValStr  -- ^ String value, will need quotes when output
    | AnnVbar -- ^ '|'
    | AnnWhere
    | Annlarrowtail -- ^ '-<'
    | Annrarrowtail -- ^ '->'
    | AnnLarrowtail -- ^ '-<<'
    | AnnRarrowtail -- ^ '>>-'
    | AnnEofPos
    deriving (Eq,Ord,Data,Typeable,Show)

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
  | AnnDocOptionsOld   String     -- ^ doc options declared "-- # ..."-style
  | AnnLineComment     String     -- ^ comment starting by "--"
  | AnnBlockComment    String     -- ^ comment in {- -}
    deriving (Eq,Ord,Data,Typeable,Show)
-- Note: these are based on the Token versions, but the Token type is
-- defined in Lexer.x and bringing it in here would create a loop

instance Outputable AnnotationComment where
  ppr x = text (show x)

-- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
--             'ApiAnnotation.AnnClose','ApiAnnotation.AnnComma',
--             'ApiAnnotation.AnnRarrow','ApiAnnotation.AnnTildehsh',
--             'ApiAnnotation.AnnTilde'
--   - May have 'ApiAnnotation.AnnComma' when in a list
type LRdrName = Located RdrName
