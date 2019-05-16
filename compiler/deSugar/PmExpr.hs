{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

Haskell expressions (as used by the pattern matching checker) and utilities.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module PmExpr (
        PmLit(..), PmLitValue(..), PmAltCon(..),
        pmAltConType, PmEquality(..), eqPmAltCon,
        pmLitType, literalToPmLit, negatePmLit, overloadPmLit,
        pmLitAsStringLit, coreExprAsPmLit
    ) where

#include "HsVersions.h"

import GhcPrelude

import Util
import FastString
import Id
import Name
import DataCon
import ConLike
import Outputable
import Maybes
import Type
import TyCon
import Literal
import CoreSyn
import CoreUtils (exprType)
import PrelNames
import TysWiredIn
import TysPrim

import Numeric (fromRat)
import Data.Foldable (find)
import Data.Ratio

{-
%************************************************************************
%*                                                                      *
                         Lifted Expressions
%*                                                                      *
%************************************************************************
-}

-- ----------------------------------------------------------------------------
-- ** Types

-- | Literals (simple and overloaded ones) for pattern match checking.
--
-- See Note [Undecidable Equality for PmAltCons]
data PmLit = PmLit
           { pm_lit_ty  :: Type
           , pm_lit_val :: PmLitValue }

data PmLitValue
  = PmLitInt Integer
  | PmLitRat Rational
  | PmLitChar Char
  -- We won't actually see PmLitString in the oracle since we desugar strings to
  -- lists
  | PmLitString FastString
  | PmLitOverInt Int {- How often Negated? -} Integer
  | PmLitOverRat Int {- How often Negated? -} Rational
  | PmLitOverString FastString

-- | Undecidable semantic equality result.
-- See Note [Undecidable Equality for PmAltCons]
data PmEquality
  = Equal
  | Disjoint
  | PossiblyOverlap
  deriving (Eq, Show)

-- | When 'PmEquality' can be decided. @True <=> Equal@, @False <=> Disjoint@.
decEquality :: Bool -> PmEquality
decEquality True  = Equal
decEquality False = Disjoint

-- | Undecidable equality for values represented by 'PmLit's.
-- See Note [Undecidable Equality for PmAltCons]
--
-- * @Just True@ ==> Surely equal
-- * @Just False@ ==> Surely different (non-overlapping, even!)
-- * @Nothing@ ==> Equality relation undecidable
eqPmLit :: PmLit -> PmLit -> PmEquality
eqPmLit (PmLit t1 v1) (PmLit t2 v2)
  -- no haddock | pprTrace "eqPmLit" (ppr t1 <+> ppr v1 $$ ppr t2 <+> ppr v2) False = undefined
  | not (t1 `eqType` t2) = Disjoint
  | otherwise            = go v1 v2
  where
    go (PmLitInt i1)        (PmLitInt i2)        = decEquality (i1 == i2)
    go (PmLitRat r1)        (PmLitRat r2)        = decEquality (r1 == r2)
    go (PmLitChar c1)       (PmLitChar c2)       = decEquality (c1 == c2)
    go (PmLitString s1)     (PmLitString s2)     = decEquality (s1 == s2)
    go (PmLitOverInt n1 i1) (PmLitOverInt n2 i2)
      | n1 == n2 && i1 == i2                     = Equal
    go (PmLitOverRat n1 r1) (PmLitOverRat n2 r2)
      | n1 == n2 && r1 == r2                     = Equal
    go (PmLitOverString s1) (PmLitOverString s2)
      | s1 == s2                                 = Equal
    go _                    _                    = PossiblyOverlap

-- | Syntactic equality.
instance Eq PmLit where
  a == b = eqPmLit a b == Equal

-- | Type of a 'PmLit'
pmLitType :: PmLit -> Type
pmLitType (PmLit ty _) = ty

-- | Type of a 'PmAltCon'
pmAltConType :: PmAltCon -> [Type] -> Type
pmAltConType (PmAltLit lit)     _arg_tys = ASSERT( null _arg_tys ) pmLitType lit
pmAltConType (PmAltConLike con) arg_tys  = conLikeResTy con arg_tys

-- | Undecidable equality for values represented by 'ConLike's.
-- See Note [Undecidable Equality for PmAltCons].
-- 'PatSynCon's aren't enforced to be generative, so two syntactically different
-- 'PatSynCon's might match the exact same values. Without looking into and
-- reasoning about the pattern synonym's definition, we can't decide if their
-- sets of matched values is different.
--
-- * @Just True@ ==> Surely equal
-- * @Just False@ ==> Surely different (non-overlapping, even!)
-- * @Nothing@ ==> Equality relation undecidable
eqConLike :: ConLike -> ConLike -> PmEquality
eqConLike (RealDataCon dc1) (RealDataCon dc2) = decEquality (dc1 == dc2)
eqConLike (PatSynCon psc1)  (PatSynCon psc2)
  | psc1 == psc2
  = Equal
eqConLike _                 _                 = PossiblyOverlap

-- | Represents the head of a match against a 'ConLike' or literal.
-- Really similar to 'CoreSyn.AltCon'.
data PmAltCon = PmAltConLike ConLike
              | PmAltLit     PmLit

-- | We can't in general decide whether two 'PmAltCon's match the same set of
-- values. In addition to the reasons in 'eqPmLit' and 'eqConLike', a
-- 'PmAltConLike' might or might not represent the same value as a 'PmAltLit'.
-- See Note [Undecidable Equality for PmAltCons].
--
-- * @Just True@ ==> Surely equal
-- * @Just False@ ==> Surely different (non-overlapping, even!)
-- * @Nothing@ ==> Equality relation undecidable
--
-- Examples (omitting some constructor wrapping):
--
-- * @eqPmAltCon (LitInt 42) (LitInt 1) == Just False@: Lit equality is
--   decidable
-- * @eqPmAltCon (DataCon A) (DataCon B) == Just False@: DataCon equality is
--   decidable
-- * @eqPmAltCon (LitOverInt 42) (LitOverInt 1) == Nothing@: OverLit equality
--   is undecidable
-- * @eqPmAltCon (PatSyn PA) (PatSyn PB) == Nothing@: PatSyn equality is
--   undecidable
-- * @eqPmAltCon (DataCon I#) (LitInt 1) == Nothing@: DataCon to Lit
--   comparisons are undecidable without reasoning about the wrapped @Int#@
-- * @eqPmAltCon (LitOverInt 1) (LitOverInt 1) == Just True@: We assume
--   reflexivity for overloaded literals
-- * @eqPmAltCon (PatSyn PA) (PatSyn PA) == Just True@: We assume reflexivity
--   for Pattern Synonyms
eqPmAltCon :: PmAltCon -> PmAltCon -> PmEquality
eqPmAltCon (PmAltConLike cl1) (PmAltConLike cl2) = eqConLike cl1 cl2
eqPmAltCon (PmAltLit     l1)  (PmAltLit     l2)  = eqPmLit l1 l2
eqPmAltCon _                  _                  = PossiblyOverlap

-- | Syntactic equality.
instance Eq PmAltCon where
  a == b = eqPmAltCon a b == Equal

{- Note [Undecidable Equality for PmAltCons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Equality on overloaded literals is undecidable in the general case. Consider
the following example:

  instance Num Bool where
    ...
    fromInteger 0 = False -- C-like representation of booleans
    fromInteger _ = True

    f :: Bool -> ()
    f 1 = ()        -- Clause A
    f 2 = ()        -- Clause B

Clause B is redundant but to detect this, we must decide the constraint:
@fromInteger 2 ~ fromInteger 1@ which means that we
have to look through function @fromInteger@, whose implementation could
be anything. This poses difficulties for:

1. The expressive power of the check.
   We cannot expect a reasonable implementation of pattern matching to detect
   that @fromInteger 2 ~ fromInteger 1@ is True, unless we unfold function
   fromInteger. This puts termination at risk and is undecidable in the
   general case.

2. Error messages/Warnings.
   What should our message for @f@ above be? A reasonable approach would be
   to issue:

     Pattern matches are (potentially) redundant:
       f 2 = ...    under the assumption that 1 == 2

   but seems to complex and confusing for the user.

We choose to equate only obviously equal overloaded literals, in all other cases
we signal undecidability by returning Nothing from 'eqPmAltCons'. We do
better for non-overloaded literals, because we know their fromInteger/fromString
implementation is actually injective, allowing us to simplify the constraint
@fromInteger 1 ~ fromInteger 2@ to @1 ~ 2@, which is trivially unsatisfiable.

The impact of this treatment of overloaded literals is the following:

  * Redundancy checking is rather conservative, since it cannot see that clause
    B above is redundant.

  * We have instant equality check for overloaded literals (we do not rely on
    the term oracle which is rather expensive, both in terms of performance and
    memory). This significantly improves the performance of functions `covered`
    `uncovered` and `divergent` in deSugar/Check.hs and effectively addresses
    #11161.

  * The warnings issued are simpler.

Similar reasoning applies to pattern synonyms: In contrast to data constructors,
which are generative, constraints like F a ~ G b for two different pattern
synonyms F and G aren't immediately unsatisfiable. We assume F a ~ F a, though.
-}

literalToPmLit :: Type -> Literal -> Maybe PmLit
literalToPmLit ty l = PmLit ty <$> go l
  where
    go (LitChar c)       = Just (PmLitChar c)
    go (LitFloat r)      = Just (PmLitRat r)
    go (LitDouble r)     = Just (PmLitRat r)
    go (LitString s)     = Just (PmLitString (mkFastStringByteString s))
    go (LitNumber _ i _) = Just (PmLitInt i)
    go _                 = Nothing

negatePmLit :: PmLit -> Maybe PmLit
negatePmLit (PmLit ty v) = PmLit ty <$> go v
  where
    go (PmLitInt i)       = Just (PmLitInt (-i))
    go (PmLitRat r)       = Just (PmLitRat (-r))
    go (PmLitOverInt n i) = Just (PmLitOverInt (n+1) i)
    go (PmLitOverRat n r) = Just (PmLitOverRat (n+1) r)
    go _                  = Nothing

overloadPmLit :: Type -> PmLit -> Maybe PmLit
overloadPmLit ty (PmLit _ v) = PmLit ty <$> go v
  where
    go (PmLitInt i)          = Just (PmLitOverInt 0 i)
    go (PmLitRat r)          = Just (PmLitOverRat 0 r)
    go (PmLitString s)
      | ty `eqType` stringTy = Just v
      | otherwise            = Just (PmLitOverString s)
    go _               = Nothing

pmLitAsStringLit :: PmLit -> Maybe FastString
pmLitAsStringLit (PmLit _ (PmLitString s)) = Just s
pmLitAsStringLit _                         = Nothing

-- ----------------------------------------------------------------------------
-- ** Predicates on PmExpr

-- -----------------------------------------------------------------------
-- ** Lift source expressions (HsExpr Id) to PmExpr

coreExprAsPmLit :: CoreExpr -> Maybe PmLit
-- coreExprAsPmLit e | pprTrace "coreExprAsPmLit" (ppr e) False = undefined
coreExprAsPmLit (Tick _t e) = coreExprAsPmLit e
coreExprAsPmLit (Lit l) = literalToPmLit (literalType l) l
coreExprAsPmLit e = case collectArgs e of
  (Var x, [Lit l])
    | Just dc <- isDataConWorkId_maybe x
    , dc `elem` [intDataCon, wordDataCon, charDataCon, floatDataCon, doubleDataCon]
    -> literalToPmLit (exprType e) l
  (Var x, [_ty, Lit n, Lit d])
    | Just dc <- isDataConWorkId_maybe x
    , dataConName dc == ratioDataConName
    -- HACK: just assume we have a literal double. This case only occurs for
    --       overloaded lits anyway, so we immediately override type information
    -> literalToPmLit (exprType e) (mkLitDouble (litValue n % litValue d))
  (Var x, args)
    -- Take care of -XRebindableSyntax. The last argument should be the (only)
    -- integer literal, otherwise we can't really do much about it.
    | [Lit l] <- dropWhile (not . is_lit) args
    -- getOccFS because of -XRebindableSyntax
    , getOccFS (idName x) == getOccFS fromIntegerName
    -> literalToPmLit (literalType l) l >>= overloadPmLit (exprType e)
  (Var x, args)
    -- Similar to fromInteger case
    | [r] <- dropWhile (not . is_ratio) args
    , getOccFS (idName x) == getOccFS fromRationalName
    -> coreExprAsPmLit r >>= overloadPmLit (exprType e)
  (Var x, [Type _ty, _dict, s])
    | idName x == fromStringName
    -- NB: Calls coreExprAsPmLit and then overloadPmLit, so that we return PmLitOverStrings
    -> coreExprAsPmLit s >>= overloadPmLit (exprType e)
  -- These last two cases handle String literals
  (Var x, [Type ty])
    | Just dc <- isDataConWorkId_maybe x
    , dc == nilDataCon
    , ty `eqType` charTy
    -> literalToPmLit stringTy (mkLitString "")
  (Var x, [Lit l])
    | idName x `elem` [unpackCStringName, unpackCStringUtf8Name]
    -> literalToPmLit stringTy l
  _ -> Nothing
  where
    is_lit Lit{} = True
    is_lit _     = False
    is_ratio (Type _) = False
    is_ratio r
      | Just (tc, _) <- splitTyConApp_maybe (exprType r)
      = tyConName tc == ratioTyConName
      | otherwise
      = False

{-
%************************************************************************
%*                                                                      *
                            Pretty printing
%*                                                                      *
%************************************************************************
-}

instance Outputable PmLitValue where
  ppr (PmLitInt i)        = ppr i
  ppr (PmLitRat r)        = ppr (double (fromRat r)) -- good enough
  ppr (PmLitChar c)       = pprHsChar c
  ppr (PmLitString s)     = pprHsString s
  ppr (PmLitOverInt n i)  = minuses n (ppr i)
  ppr (PmLitOverRat n r)  = minuses n (ppr (double (fromRat r)))
  ppr (PmLitOverString s) = pprHsString s

-- Take care of negated literals
minuses :: Int -> SDoc -> SDoc
minuses n sdoc = iterate (\sdoc -> parens (char '-' <> sdoc)) sdoc !! n

instance Outputable PmLit where
  ppr (PmLit ty v) = ppr v <> suffix
    where
      -- Some ad-hoc hackery for displaying proper lit suffixes based on type
      tbl = [ (intPrimTy, primIntSuffix)
            , (int64PrimTy, primInt64Suffix)
            , (wordPrimTy, primWordSuffix)
            , (word64PrimTy, primWord64Suffix)
            , (charPrimTy, primCharSuffix)
            , (floatPrimTy, primFloatSuffix)
            , (doublePrimTy, primDoubleSuffix) ]
      suffix = fromMaybe empty (snd <$> find (eqType ty . fst) tbl)

instance Outputable PmAltCon where
  ppr (PmAltConLike cl) = ppr cl
  ppr (PmAltLit l)      = ppr l

instance Outputable PmEquality where
  ppr = text . show
