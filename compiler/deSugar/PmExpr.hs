{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

Haskell expressions (as used by the pattern matching checker) and utilities.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module PmExpr (
        PmExpr(..), PmLit(..), PmAltCon(..), SimpleEq, ComplexEq, toComplex,
        eqPmLit, pmExprToAlt, isNotPmExprOther, lhsExprToPmExpr, hsExprToPmExpr,
        substComplexEq
    ) where

#include "HsVersions.h"

import GhcPrelude

import BasicTypes (SourceText)
import FastString (FastString, unpackFS)
import HsSyn
import Id
import Name
import DataCon
import ConLike
import TcType (Type, isStringTy)
import TysWiredIn
import Outputable
import Util
import SrcLoc

{-
%************************************************************************
%*                                                                      *
                         Lifted Expressions
%*                                                                      *
%************************************************************************
-}

{- Note [PmExprOther in PmExpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since there is no plan to extend the (currently pretty naive) term oracle in
the near future, instead of playing with the verbose (HsExpr Id), we lift it to
PmExpr. All expressions the term oracle does not handle are wrapped by the
constructor PmExprOther. Note that we do not perform substitution in
PmExprOther. Because of this, we do not even print PmExprOther, since they may
refer to variables that are otherwise substituted away.
-}

-- ----------------------------------------------------------------------------
-- ** Types

-- | Lifted expressions for pattern match checking.
data PmExpr = PmExprVar   Name
            | PmExprCon   ConLike [PmExpr]
            | PmExprLit   PmLit
            | PmExprOther (HsExpr GhcTc)  -- Note [PmExprOther in PmExpr]


mkPmExprData :: DataCon -> [PmExpr] -> PmExpr
mkPmExprData dc args = PmExprCon (RealDataCon dc) args

-- | Literals (simple and overloaded ones) for pattern match checking.
data PmLit = PmSLit (HsLit GhcTc)                               -- simple
           | PmOLit Bool {- is it negated? -} (HsOverLit GhcTc) -- overloaded

-- | Equality between literals for pattern match checking.
eqPmLit :: PmLit -> PmLit -> Bool
eqPmLit (PmSLit    l1) (PmSLit    l2) = l1 == l2
eqPmLit (PmOLit b1 l1) (PmOLit b2 l2) = b1 == b2 && l1 == l2
  -- See Note [Undecidable Equality for Overloaded Literals]
eqPmLit _              _              = False

-- | Represents a match against a 'ConLike' or literal. We mostly use it to
-- to encode shapes for a variable that immediately lead to a refutation.
--
-- See Note [Refutable shapes] in TmOracle. Really similar to 'CoreSyn.AltCon'.
data PmAltCon = PmAltConLike ConLike [Type]
              -- ^ The types are the argument types of the 'ConLike' application
              | PmAltLit     PmLit

-- | This instance won't compare the argument types of the 'ConLike', as we
-- carry them for recovering COMPLETE match groups only. The 'PmRefutEnv' below
-- should never have different 'PmAltConLike's with the same 'ConLike' for the
-- same variable. See Note [Refutable shapes] in TmOracle.
instance Eq PmAltCon where
  PmAltConLike cl1 _ == PmAltConLike cl2 _ = cl1 == cl2
  PmAltLit l1        == PmAltLit l2        = eqPmLit l1 l2
  _                  == _                  = False

pmExprToAlt :: PmExpr -> Maybe PmAltCon
-- Note how this deliberately chooses bogus argument types for PmAltConLike.
-- This is only safe for doing lookup in a 'PmRefutEnv'!
pmExprToAlt (PmExprCon cl _) = Just (PmAltConLike cl [])
pmExprToAlt (PmExprLit l)    = Just (PmAltLit l)
pmExprToAlt _                = Nothing

{- Note [Undecidable Equality for Overloaded Literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Equality on overloaded literals is undecidable in the general case. Consider
the following example:

  instance Num Bool where
    ...
    fromInteger 0 = False -- C-like representation of booleans
    fromInteger _ = True

    f :: Bool -> ()
    f 1 = ()        -- Clause A
    f 2 = ()        -- Clause B

Clause B is redundant but to detect this, we should be able to solve the
constraint: False ~ (fromInteger 2 ~ fromInteger 1) which means that we
have to look through function `fromInteger`, whose implementation could
be anything. This poses difficulties for:

1. The expressive power of the check.
   We cannot expect a reasonable implementation of pattern matching to detect
   that fromInteger 2 ~ fromInteger 1 is True, unless we unfold function
   fromInteger. This puts termination at risk and is undecidable in the
   general case.

2. Performance.
   Having an unresolved constraint False ~ (fromInteger 2 ~ fromInteger 1)
   lying around could become expensive really fast. Ticket #11161 illustrates
   how heavy use of overloaded literals can generate plenty of those
   constraints, effectively undermining the term oracle's performance.

3. Error nessages/Warnings.
   What should our message for `f` above be? A reasonable approach would be
   to issue:

     Pattern matches are (potentially) redundant:
       f 2 = ...    under the assumption that 1 == 2

   but seems to complex and confusing for the user.

We choose to treat overloaded literals that look different as different. The
impact of this is the following:

  * Redundancy checking is rather conservative, since it cannot see that clause
    B above is redundant.

  * We have instant equality check for overloaded literals (we do not rely on
    the term oracle which is rather expensive, both in terms of performance and
    memory). This significantly improves the performance of functions `covered`
    `uncovered` and `divergent` in deSugar/Check.hs and effectively addresses
    #11161.

  * The warnings issued are simpler.

  * We do not play on the safe side, strictly speaking. The assumption that
    1 /= 2 makes the redundancy check more conservative but at the same time
    makes its dual (exhaustiveness check) unsafe. This we can live with, mainly
    for two reasons:
    1. At the moment we do not use the results of the check during compilation
       where this would be a disaster (could result in runtime errors even if
       our function was deemed exhaustive).
    2. Pattern matcing on literals can never be considered exhaustive unless we
       have a catch-all clause. Hence, this assumption affects mainly the
       appearance of the warnings and is, in practice safe.
-}

-- | Term equalities
type SimpleEq  = (Id, PmExpr) -- We always use this orientation
type ComplexEq = (PmExpr, PmExpr)

-- | Lift a `SimpleEq` to a `ComplexEq`
toComplex :: SimpleEq -> ComplexEq
toComplex (x,e) = (PmExprVar (idName x), e)

-- ----------------------------------------------------------------------------
-- ** Predicates on PmExpr

-- | Check if an expression is lifted or not
isNotPmExprOther :: PmExpr -> Bool
isNotPmExprOther (PmExprOther _) = False
isNotPmExprOther _expr           = True

-- ----------------------------------------------------------------------------
-- ** Substitution in PmExpr

-- | We return a boolean along with the expression. Hence, if substitution was
-- a no-op, we know that the expression still cannot progress.
substPmExpr :: Name -> PmExpr -> PmExpr -> (PmExpr, Bool)
substPmExpr x e1 e =
  case e of
    PmExprVar z | x == z    -> (e1, True)
                | otherwise -> (e, False)
    PmExprCon c ps -> let (ps', bs) = mapAndUnzip (substPmExpr x e1) ps
                      in  (PmExprCon c ps', or bs)
    _other_expr    -> (e, False) -- The rest are terminals (We silently ignore
                                 -- Other). See Note [PmExprOther in PmExpr]

-- | Substitute in a complex equality. We return (Left eq) if the substitution
-- affected the equality or (Right eq) if nothing happened.
substComplexEq :: Name -> PmExpr -> ComplexEq -> Either ComplexEq ComplexEq
substComplexEq x e (ex, ey)
  | bx || by  = Left  (ex', ey')
  | otherwise = Right (ex', ey')
  where
    (ex', bx) = substPmExpr x e ex
    (ey', by) = substPmExpr x e ey

-- -----------------------------------------------------------------------
-- ** Lift source expressions (HsExpr Id) to PmExpr

lhsExprToPmExpr :: LHsExpr GhcTc -> PmExpr
lhsExprToPmExpr (dL->L _ e) = hsExprToPmExpr e

hsExprToPmExpr :: HsExpr GhcTc -> PmExpr

hsExprToPmExpr (HsVar        _ x) = PmExprVar (idName (unLoc x))
hsExprToPmExpr (HsConLikeOut _ c) = PmExprVar (conLikeName c)

-- Desugar literal strings as a list of characters. For other literal values,
-- keep it as it is.
-- See `translatePat` in Check.hs (the `NPat` and `LitPat` case), and
-- Note [Translate Overloaded Literal for Exhaustiveness Checking].
hsExprToPmExpr (HsOverLit _ olit)
  | OverLit (OverLitTc False ty) (HsIsString src s) _ <- olit, isStringTy ty
  = stringExprToList src s
  | otherwise = PmExprLit (PmOLit False olit)
hsExprToPmExpr (HsLit     _ lit)
  | HsString src s <- lit
  = stringExprToList src s
  | otherwise = PmExprLit (PmSLit lit)

hsExprToPmExpr e@(NegApp _ (dL->L _ neg_expr) _)
  | PmExprLit (PmOLit False olit) <- hsExprToPmExpr neg_expr
    -- NB: DON'T simply @(NegApp (NegApp olit))@ as @x@. when extension
    -- @RebindableSyntax@ enabled, (-(-x)) may not equals to x.
  = PmExprLit (PmOLit True olit)
  | otherwise = PmExprOther e

hsExprToPmExpr (HsPar _ (dL->L _ e)) = hsExprToPmExpr e

hsExprToPmExpr e@(ExplicitTuple _ ps boxity)
  | all tupArgPresent ps = mkPmExprData tuple_con tuple_args
  | otherwise            = PmExprOther e
  where
    tuple_con  = tupleDataCon boxity (length ps)
    tuple_args = [ lhsExprToPmExpr e | (dL->L _ (Present _ e)) <- ps ]

hsExprToPmExpr e@(ExplicitList _  mb_ol elems)
  | Nothing <- mb_ol = foldr cons nil (map lhsExprToPmExpr elems)
  | otherwise        = PmExprOther e {- overloaded list: No PmExprApp -}
  where
    cons x xs = mkPmExprData consDataCon [x,xs]
    nil       = mkPmExprData nilDataCon  []

-- we want this but we would have to make everything monadic :/
-- ./compiler/deSugar/DsMonad.hs:397:dsLookupDataCon :: Name -> DsM DataCon
--
-- hsExprToPmExpr (RecordCon   c _ binds) = do
--   con  <- dsLookupDataCon (unLoc c)
--   args <- mapM lhsExprToPmExpr (hsRecFieldsArgs binds)
--   return (PmExprCon con args)
hsExprToPmExpr e@(RecordCon {}) = PmExprOther e

hsExprToPmExpr (HsTick           _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsBinTick      _ _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsTickPragma _ _ _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsSCC          _ _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsCoreAnn      _ _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (ExprWithTySig    _ e _) = lhsExprToPmExpr e
hsExprToPmExpr (HsWrap           _ _ e) =  hsExprToPmExpr e
hsExprToPmExpr e = PmExprOther e -- the rest are not handled by the oracle

stringExprToList :: SourceText -> FastString -> PmExpr
stringExprToList src s = foldr cons nil (map charToPmExpr (unpackFS s))
  where
    cons x xs      = mkPmExprData consDataCon [x,xs]
    nil            = mkPmExprData nilDataCon  []
    charToPmExpr c = PmExprLit (PmSLit (HsChar src c))

{-
%************************************************************************
%*                                                                      *
                            Pretty printing
%*                                                                      *
%************************************************************************
-}

instance Outputable PmLit where
  ppr (PmSLit     l) = pmPprHsLit l
  ppr (PmOLit neg l) = (if neg then char '-' else empty) <> ppr l

instance Outputable PmExpr where
  ppr = go (0 :: Int)
    where
      go _    (PmExprLit l)       = ppr l
      go _    (PmExprVar v)       = ppr v
      go _    (PmExprOther e)     = angleBrackets (ppr e)
      go _    (PmExprCon (RealDataCon dc) args)
        | isTupleDataCon dc = parens $ comma_sep $ map ppr args
        | dc == consDataCon = brackets $ comma_sep $ map ppr (list_cells args)
        where
          comma_sep = fsep . punctuate comma
          list_cells (hd:tl) = hd : list_cells tl
          list_cells _       = []
      go prec (PmExprCon cl args)
        = cparen (null args || prec > 0) (hcat (ppr cl:map (go 1) args))

instance Outputable PmAltCon where
  ppr (PmAltConLike cl tys) = ppr cl <+> char '@' <> ppr tys
  ppr (PmAltLit l)          = ppr l
