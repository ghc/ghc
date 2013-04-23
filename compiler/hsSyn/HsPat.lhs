%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PatSyntax]{Abstract Haskell syntax---patterns}

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}

module HsPat (
        Pat(..), InPat, OutPat, LPat,

        HsConDetails(..),
        HsConPatDetails, hsConPatArgs,
        HsRecFields(..), HsRecField(..), hsRecFields,

        mkPrefixConPat, mkCharLitPat, mkNilPat,

        isBangHsBind, isLiftedPatBind,
        isBangLPat, hsPatNeedsParens,
        isIrrefutableHsPat,

        pprParendLPat
    ) where

import {-# SOURCE #-} HsExpr            (SyntaxExpr, LHsExpr, pprLExpr)

-- friends:
import HsBinds
import HsLit
import HsTypes
import TcEvidence
import BasicTypes
-- others:
import PprCore          ( {- instance OutputableBndr TyVar -} )
import TysWiredIn
import Var
import DataCon
import TyCon
import Outputable
import Type
import SrcLoc
import FastString
-- libraries:
import Data.Data hiding (TyCon)
import Data.Maybe
\end{code}


\begin{code}
type InPat id  = LPat id        -- No 'Out' constructors
type OutPat id = LPat id        -- No 'In' constructors

type LPat id = Located (Pat id)

data Pat id
  =     ------------ Simple patterns ---------------
    WildPat     PostTcType              -- Wild card
        -- The sole reason for a type on a WildPat is to
        -- support hsPatType :: Pat Id -> Type

  | VarPat      id                      -- Variable
  | LazyPat     (LPat id)               -- Lazy pattern
  | AsPat       (Located id) (LPat id)  -- As pattern
  | ParPat      (LPat id)               -- Parenthesised pattern
                                        -- See Note [Parens in HsSyn] in HsExpr
  | BangPat     (LPat id)               -- Bang pattern

        ------------ Lists, tuples, arrays ---------------
  | ListPat     [LPat id]                            -- Syntactic list
                PostTcType                           -- The type of the elements
                (Maybe (PostTcType, SyntaxExpr id))  -- For rebindable syntax
                   -- For OverloadedLists a Just (ty,fn) gives
                   -- overall type of the pattern, and the toList
                   -- function to convert the scrutinee to a list value

  | TuplePat    [LPat id]               -- Tuple
                Boxity                  -- UnitPat is TuplePat []
                PostTcType
        -- You might think that the PostTcType was redundant, but it's essential
        --      data T a where
        --        T1 :: Int -> T Int
        --      f :: (T a, a) -> Int
        --      f (T1 x, z) = z
        -- When desugaring, we must generate
        --      f = /\a. \v::a.  case v of (t::T a, w::a) ->
        --                       case t of (T1 (x::Int)) ->
        -- Note the (w::a), NOT (w::Int), because we have not yet
        -- refined 'a' to Int.  So we must know that the second component
        -- of the tuple is of type 'a' not Int.  See selectMatchVar

  | PArrPat     [LPat id]               -- Syntactic parallel array
                PostTcType              -- The type of the elements

        ------------ Constructor patterns ---------------
  | ConPatIn    (Located id)
                (HsConPatDetails id)

  | ConPatOut {
        pat_con   :: Located DataCon,
        pat_tvs   :: [TyVar],           -- Existentially bound type variables (tyvars only)
        pat_dicts :: [EvVar],           -- Ditto *coercion variables* and *dictionaries*
                                        -- One reason for putting coercion variable here, I think,
                                        --      is to ensure their kinds are zonked
        pat_binds :: TcEvBinds,         -- Bindings involving those dictionaries
        pat_args  :: HsConPatDetails id,
        pat_ty    :: Type               -- The type of the pattern
    }

        ------------ View patterns ---------------
  | ViewPat       (LHsExpr id)
                  (LPat id)
                  PostTcType        -- The overall type of the pattern
                                    -- (= the argument type of the view function)
                                    -- for hsPatType.

        ------------ Quasiquoted patterns ---------------
        -- See Note [Quasi-quote overview] in TcSplice
  | QuasiQuotePat   (HsQuasiQuote id)

        ------------ Literal and n+k patterns ---------------
  | LitPat          HsLit               -- Used for *non-overloaded* literal patterns:
                                        -- Int#, Char#, Int, Char, String, etc.

  | NPat                -- Used for all overloaded literals,
                        -- including overloaded strings with -XOverloadedStrings
                    (HsOverLit id)              -- ALWAYS positive
                    (Maybe (SyntaxExpr id))     -- Just (Name of 'negate') for negative
                                                -- patterns, Nothing otherwise
                    (SyntaxExpr id)             -- Equality checker, of type t->t->Bool

  | NPlusKPat       (Located id)        -- n+k pattern
                    (HsOverLit id)      -- It'll always be an HsIntegral
                    (SyntaxExpr id)     -- (>=) function, of type t->t->Bool
                    (SyntaxExpr id)     -- Name of '-' (see RnEnv.lookupSyntaxName)

        ------------ Pattern type signatures ---------------
  | SigPatIn        (LPat id)                   -- Pattern with a type signature
                    (HsWithBndrs (LHsType id))  -- Signature can bind both kind and type vars

  | SigPatOut       (LPat id)           -- Pattern with a type signature
                    Type

        ------------ Pattern coercions (translation only) ---------------
  | CoPat       HsWrapper               -- If co :: t1 ~ t2, p :: t2,
                                        -- then (CoPat co p) :: t1
                (Pat id)                -- Why not LPat?  Ans: existing locn will do
                Type                    -- Type of whole pattern, t1
        -- During desugaring a (CoPat co pat) turns into a cast with 'co' on
        -- the scrutinee, followed by a match on 'pat'
  deriving (Data, Typeable)
\end{code}

HsConDetails is use for patterns/expressions *and* for data type declarations

\begin{code}
data HsConDetails arg rec
  = PrefixCon [arg]             -- C p1 p2 p3
  | RecCon    rec               -- C { x = p1, y = p2 }
  | InfixCon  arg arg           -- p1 `C` p2
  deriving (Data, Typeable)

type HsConPatDetails id = HsConDetails (LPat id) (HsRecFields id (LPat id))

hsConPatArgs :: HsConPatDetails id -> [LPat id]
hsConPatArgs (PrefixCon ps)   = ps
hsConPatArgs (RecCon fs)      = map hsRecFieldArg (rec_flds fs)
hsConPatArgs (InfixCon p1 p2) = [p1,p2]
\end{code}

However HsRecFields is used only for patterns and expressions
(not data type declarations)

\begin{code}
data HsRecFields id arg         -- A bunch of record fields
                                --      { x = 3, y = True }
        -- Used for both expressions and patterns
  = HsRecFields { rec_flds   :: [HsRecField id arg],
                  rec_dotdot :: Maybe Int }  -- Note [DotDot fields]
  deriving (Data, Typeable)

-- Note [DotDot fields]
-- ~~~~~~~~~~~~~~~~~~~~
-- The rec_dotdot field means this:
--   Nothing => the normal case
--   Just n  => the group uses ".." notation,
--
-- In the latter case:
--
--   *before* renamer: rec_flds are exactly the n user-written fields
--
--   *after* renamer:  rec_flds includes *all* fields, with
--                     the first 'n' being the user-written ones
--                     and the remainder being 'filled in' implicitly

data HsRecField id arg = HsRecField {
        hsRecFieldId  :: Located id,
        hsRecFieldArg :: arg,           -- Filled in by renamer
        hsRecPun      :: Bool           -- Note [Punning]
  } deriving (Data, Typeable)

-- Note [Punning]
-- ~~~~~~~~~~~~~~
-- If you write T { x, y = v+1 }, the HsRecFields will be
--      HsRecField x x True ...
--      HsRecField y (v+1) False ...
-- That is, for "punned" field x is expanded (in the renamer)
-- to x=x; but with a punning flag so we can detect it later
-- (e.g. when pretty printing)
--
-- If the original field was qualified, we un-qualify it, thus
--    T { A.x } means T { A.x = x }

hsRecFields :: HsRecFields id arg -> [id]
hsRecFields rbinds = map (unLoc . hsRecFieldId) (rec_flds rbinds)
\end{code}

%************************************************************************
%*                                                                      *
%*              Printing patterns
%*                                                                      *
%************************************************************************

\begin{code}
instance (OutputableBndr name) => Outputable (Pat name) where
    ppr = pprPat

pprPatBndr :: OutputableBndr name => name -> SDoc
pprPatBndr var                  -- Print with type info if -dppr-debug is on
  = getPprStyle $ \ sty ->
    if debugStyle sty then
        parens (pprBndr LambdaBind var)         -- Could pass the site to pprPat
                                                -- but is it worth it?
    else
        pprPrefixOcc var

pprParendLPat :: (OutputableBndr name) => LPat name -> SDoc
pprParendLPat (L _ p) = pprParendPat p

pprParendPat :: (OutputableBndr name) => Pat name -> SDoc
pprParendPat p | hsPatNeedsParens p = parens (pprPat p)
               | otherwise          = pprPat p

pprPat :: (OutputableBndr name) => Pat name -> SDoc
pprPat (VarPat var)       = pprPatBndr var
pprPat (WildPat _)        = char '_'
pprPat (LazyPat pat)      = char '~' <> pprParendLPat pat
pprPat (BangPat pat)      = char '!' <> pprParendLPat pat
pprPat (AsPat name pat)   = hcat [pprPrefixOcc (unLoc name), char '@', pprParendLPat pat]
pprPat (ViewPat expr pat _) = hcat [pprLExpr expr, text " -> ", ppr pat]
pprPat (ParPat pat)         = parens (ppr pat)
pprPat (ListPat pats _ _)     = brackets (interpp'SP pats)
pprPat (PArrPat pats _)     = paBrackets (interpp'SP pats)
pprPat (TuplePat pats bx _) = tupleParens (boxityNormalTupleSort bx) (interpp'SP pats)

pprPat (ConPatIn con details) = pprUserCon (unLoc con) details
pprPat (ConPatOut { pat_con = con, pat_tvs = tvs, pat_dicts = dicts,
                    pat_binds = binds, pat_args = details })
  = getPprStyle $ \ sty ->      -- Tiresome; in TcBinds.tcRhs we print out a
    if debugStyle sty then      -- typechecked Pat in an error message,
                                -- and we want to make sure it prints nicely
        ppr con <> braces (sep [ hsep (map pprPatBndr (tvs ++ dicts))
                               , ppr binds])
                <+> pprConArgs details
    else pprUserCon (unLoc con) details

pprPat (LitPat s)           = ppr s
pprPat (NPat l Nothing  _)  = ppr l
pprPat (NPat l (Just _) _)  = char '-' <> ppr l
pprPat (NPlusKPat n k _ _)  = hcat [ppr n, char '+', ppr k]
pprPat (QuasiQuotePat qq)   = ppr qq
pprPat (CoPat co pat _)     = pprHsWrapper (ppr pat) co
pprPat (SigPatIn pat ty)    = ppr pat <+> dcolon <+> ppr ty
pprPat (SigPatOut pat ty)   = ppr pat <+> dcolon <+> ppr ty

pprUserCon :: (OutputableBndr con, OutputableBndr id) => con -> HsConPatDetails id -> SDoc
pprUserCon c (InfixCon p1 p2) = ppr p1 <+> pprInfixOcc c <+> ppr p2
pprUserCon c details          = pprPrefixOcc c <+> pprConArgs details

pprConArgs ::  OutputableBndr id => HsConPatDetails id -> SDoc
pprConArgs (PrefixCon pats) = sep (map pprParendLPat pats)
pprConArgs (InfixCon p1 p2) = sep [pprParendLPat p1, pprParendLPat p2]
pprConArgs (RecCon rpats)   = ppr rpats

instance (OutputableBndr id, Outputable arg)
      => Outputable (HsRecFields id arg) where
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Nothing })
        = braces (fsep (punctuate comma (map ppr flds)))
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Just n })
        = braces (fsep (punctuate comma (map ppr (take n flds) ++ [dotdot])))
        where
          dotdot = ptext (sLit "..") <+> ifPprDebug (ppr (drop n flds))

instance (OutputableBndr id, Outputable arg)
      => Outputable (HsRecField id arg) where
  ppr (HsRecField { hsRecFieldId = f, hsRecFieldArg = arg,
                    hsRecPun = pun })
    = ppr f <+> (ppUnless pun $ equals <+> ppr arg)
\end{code}


%************************************************************************
%*                                                                      *
%*              Building patterns
%*                                                                      *
%************************************************************************

\begin{code}
mkPrefixConPat :: DataCon -> [OutPat id] -> Type -> OutPat id
-- Make a vanilla Prefix constructor pattern
mkPrefixConPat dc pats ty
  = noLoc $ ConPatOut { pat_con = noLoc dc, pat_tvs = [], pat_dicts = [],
                        pat_binds = emptyTcEvBinds, pat_args = PrefixCon pats,
                        pat_ty = ty }

mkNilPat :: Type -> OutPat id
mkNilPat ty = mkPrefixConPat nilDataCon [] ty

mkCharLitPat :: Char -> OutPat id
mkCharLitPat c = mkPrefixConPat charDataCon [noLoc $ LitPat (HsCharPrim c)] charTy
\end{code}


%************************************************************************
%*                                                                      *
%* Predicates for checking things about pattern-lists in EquationInfo   *
%*                                                                      *
%************************************************************************

\subsection[Pat-list-predicates]{Look for interesting things in patterns}

Unlike in the Wadler chapter, where patterns are either ``variables''
or ``constructors,'' here we distinguish between:
\begin{description}
\item[unfailable:]
Patterns that cannot fail to match: variables, wildcards, and lazy
patterns.

These are the irrefutable patterns; the two other categories
are refutable patterns.

\item[constructor:]
A non-literal constructor pattern (see next category).

\item[literal patterns:]
At least the numeric ones may be overloaded.
\end{description}

A pattern is in {\em exactly one} of the above three categories; `as'
patterns are treated specially, of course.

The 1.3 report defines what ``irrefutable'' and ``failure-free'' patterns are.
\begin{code}
isBangLPat :: LPat id -> Bool
isBangLPat (L _ (BangPat {})) = True
isBangLPat (L _ (ParPat p))   = isBangLPat p
isBangLPat _                  = False

isBangHsBind :: HsBind id -> Bool
-- A pattern binding with an outermost bang
-- Defined in this module because HsPat is above HsBinds in the import graph
isBangHsBind (PatBind { pat_lhs = p }) = isBangLPat p
isBangHsBind _                         = False

isLiftedPatBind :: HsBind id -> Bool
-- A pattern binding with a compound pattern, not just a variable
--    (I# x)       yes
--    (# a, b #)   no, even if a::Int#
--    x            no, even if x::Int#
-- We want to warn about a missing bang-pattern on the yes's
isLiftedPatBind (PatBind { pat_lhs = p }) = isLiftedLPat p
isLiftedPatBind _                         = False

isLiftedLPat :: LPat id -> Bool
isLiftedLPat (L _ (ParPat p))   = isLiftedLPat p
isLiftedLPat (L _ (BangPat p))  = isLiftedLPat p
isLiftedLPat (L _ (AsPat _ p))  = isLiftedLPat p
isLiftedLPat (L _ (TuplePat _ Unboxed _)) = False
isLiftedLPat (L _ (VarPat {}))            = False
isLiftedLPat (L _ (WildPat {}))           = False
isLiftedLPat _                            = True

isIrrefutableHsPat :: OutputableBndr id => LPat id -> Bool
-- (isIrrefutableHsPat p) is true if matching against p cannot fail,
-- in the sense of falling through to the next pattern.
--      (NB: this is not quite the same as the (silly) defn
--      in 3.17.2 of the Haskell 98 report.)
--
-- isIrrefutableHsPat returns False if it's in doubt; specifically
-- on a ConPatIn it doesn't know the size of the constructor family
-- But if it returns True, the pattern is definitely irrefutable
isIrrefutableHsPat pat
  = go pat
  where
    go (L _ pat) = go1 pat

    go1 (WildPat {})        = True
    go1 (VarPat {})         = True
    go1 (LazyPat {})        = True
    go1 (BangPat pat)       = go pat
    go1 (CoPat _ pat _)     = go1 pat
    go1 (ParPat pat)        = go pat
    go1 (AsPat _ pat)       = go pat
    go1 (ViewPat _ pat _)   = go pat
    go1 (SigPatIn pat _)    = go pat
    go1 (SigPatOut pat _)   = go pat
    go1 (TuplePat pats _ _) = all go pats
    go1 (ListPat {}) = False
    go1 (PArrPat {})        = False     -- ?

    go1 (ConPatIn {})       = False     -- Conservative
    go1 (ConPatOut{ pat_con = L _ con, pat_args = details })
        =  isJust (tyConSingleDataCon_maybe (dataConTyCon con))
           -- NB: tyConSingleDataCon_maybe, *not* isProductTyCon, because
           -- the latter is false of existentials. See Trac #4439
        && all go (hsConPatArgs details)

    go1 (LitPat {})    = False
    go1 (NPat {})      = False
    go1 (NPlusKPat {}) = False

    go1 (QuasiQuotePat {}) = urk pat    -- Gotten rid of by renamer, before
                                        -- isIrrefutablePat is called

    urk pat = pprPanic "isIrrefutableHsPat:" (ppr pat)

hsPatNeedsParens :: Pat a -> Bool
hsPatNeedsParens (NPlusKPat {})      = True
hsPatNeedsParens (QuasiQuotePat {})  = True
hsPatNeedsParens (ConPatIn _ ds)     = conPatNeedsParens ds
hsPatNeedsParens p@(ConPatOut {})    = conPatNeedsParens (pat_args p)
hsPatNeedsParens (SigPatIn {})       = True
hsPatNeedsParens (SigPatOut {})      = True
hsPatNeedsParens (ViewPat {})        = True
hsPatNeedsParens (CoPat {})          = True
hsPatNeedsParens (WildPat {})        = False
hsPatNeedsParens (VarPat {})         = False
hsPatNeedsParens (LazyPat {})        = False
hsPatNeedsParens (BangPat {})        = False
hsPatNeedsParens (ParPat {})         = False
hsPatNeedsParens (AsPat {})          = False
hsPatNeedsParens (TuplePat {})       = False
hsPatNeedsParens (ListPat {})        = False
hsPatNeedsParens (PArrPat {})        = False
hsPatNeedsParens (LitPat {})         = False
hsPatNeedsParens (NPat {})           = False

conPatNeedsParens :: HsConDetails a b -> Bool
conPatNeedsParens (PrefixCon args) = not (null args)
conPatNeedsParens (InfixCon {})    = True
conPatNeedsParens (RecCon {})      = True
\end{code}

