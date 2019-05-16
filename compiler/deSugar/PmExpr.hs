{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

Haskell expressions (as used by the pattern matching checker) and utilities.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module PmExpr (
        PmExpr(..), PmLit(..), PmAltCon(..), TmVarCt(..),
        pmAltConType, decEqPmAltCon,
        pmLitType, literalToPmLit, negatePmLit, overloadPmLit,
        pmLitAsStringLit, hsOverLitAsHsLit,
        mkPmExprData, pmExprToDataConApp
    ) where

#include "HsVersions.h"

import GhcPrelude

import Util
import DynFlags
import BasicTypes (IntegralLit(..), negateIntegralLit)
import FastString
import HsSyn
import Id
import DataCon
import ConLike
import TcType (Type, eqType, isStringTy, isIntTy, isIntegerTy, isWordTy)
import TysPrim
import Outputable
import Literal
import Maybes

import Numeric (fromRat)
import Data.Foldable (find)

{-
%************************************************************************
%*                                                                      *
                         Lifted Expressions
%*                                                                      *
%************************************************************************
-}

-- ----------------------------------------------------------------------------
-- ** Types

-- | Lifted expressions for pattern match checking.
data PmExpr = PmExprVar   Id
            | PmExprCon   PmAltCon [Id]

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

-- | Undecidable equality for values represented by 'PmLit's.
-- See Note [Undecidable Equality for PmAltCons]
--
-- * @Just True@ ==> Surely equal
-- * @Just False@ ==> Surely different (non-overlapping, even!)
-- * @Nothing@ ==> Equality relation undecidable
decEqPmLit :: PmLit -> PmLit -> Maybe Bool
decEqPmLit (PmLit t1 v1) (PmLit t2 v2)
  -- no haddock | pprTrace "decEqPmLit" (ppr t1 <+> ppr v1 $$ ppr t2 <+> ppr v2) False = undefined
  | not (t1 `eqType` t2) = Just False
  | otherwise            = go v1 v2
  where
    go (PmLitInt i1)        (PmLitInt i2)        = Just (i1 == i2)
    go (PmLitRat r1)        (PmLitRat r2)        = Just (r1 == r2)
    go (PmLitChar c1)       (PmLitChar c2)       = Just (c1 == c2)
    go (PmLitString s1)     (PmLitString s2)     = Just (s1 == s2)
    go (PmLitOverInt n1 i1) (PmLitOverInt n2 i2)
      | n1 == n2 && i1 == i2                     = Just True
    go (PmLitOverRat n1 r1) (PmLitOverRat n2 r2)
      | n1 == n2 && r1 == r2                     = Just True
    go (PmLitOverString s1) (PmLitOverString s2)
      | s1 == s2                                 = Just True
    go _                    _                    = Nothing

-- | Syntactic equality.
instance Eq PmLit where
  a == b = decEqPmLit a b == Just True

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
decEqConLike :: ConLike -> ConLike -> Maybe Bool
decEqConLike (RealDataCon dc1) (RealDataCon dc2) = Just (dc1 == dc2)
decEqConLike (PatSynCon psc1)  (PatSynCon psc2)
  | psc1 == psc2
  = Just True
decEqConLike _                 _                 = Nothing

-- | Represents a match against a 'ConLike' or literal. We mostly use it to
-- to encode shapes for a variable that immediately lead to a refutation.
--
-- See Note [Refutable shapes] in TmOracle. Really similar to 'CoreSyn.AltCon'.
data PmAltCon = PmAltConLike ConLike
              | PmAltLit     PmLit

-- | We can't in general decide whether two 'PmAltCon's match the same set of
-- values. In addition to the reasons in 'decEqPmLit' and 'decEqConLike', a
-- 'PmAltConLike' might or might not represent the same value as a 'PmAltLit'.
-- See Note [Undecidable Equality for PmAltCons].
--
-- * @Just True@ ==> Surely equal
-- * @Just False@ ==> Surely different (non-overlapping, even!)
-- * @Nothing@ ==> Equality relation undecidable
--
-- Examples (omitting some constructor wrapping):
--
-- * @decEqPmAltCon (LitInt 42) (LitInt 1) == Just False@: Lit equality is
--   decidable
-- * @decEqPmAltCon (DataCon A) (DataCon B) == Just False@: DataCon equality is
--   decidable
-- * @decEqPmAltCon (LitOverInt 42) (LitOverInt 1) == Nothing@: OverLit equality
--   is undecidable
-- * @decEqPmAltCon (PatSyn PA) (PatSyn PB) == Nothing@: PatSyn equality is
--   undecidable
-- * @decEqPmAltCon (DataCon I#) (LitInt 1) == Nothing@: DataCon to Lit
--   comparisons are undecidable without reasoning about the wrapped @Int#@
-- * @decEqPmAltCon (LitOverInt 1) (LitOverInt 1) == Just True@: We assume
--   reflexivity for overloaded literals
-- * @decEqPmAltCon (PatSyn PA) (PatSyn PA) == Just True@: We assume reflexivity
--   for Pattern Synonyms
decEqPmAltCon :: PmAltCon -> PmAltCon -> Maybe Bool
decEqPmAltCon (PmAltConLike cl1) (PmAltConLike cl2) = decEqConLike cl1 cl2
decEqPmAltCon (PmAltLit     l1)  (PmAltLit     l2)  = decEqPmLit l1 l2
decEqPmAltCon _                  _                  = Nothing

-- | Syntactic equality.
instance Eq PmAltCon where
  a == b = decEqPmAltCon a b == Just True

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
we signal undecidability by returning Nothing from 'decEqPmAltCons'. We do
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

mkPmExprData :: DataCon -> [Id] -> PmExpr
mkPmExprData dc args = PmExprCon (PmAltConLike (RealDataCon dc)) args

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
    go (PmLitInt i)    = Just (PmLitOverInt 0 i)
    go (PmLitRat r)    = Just (PmLitOverRat 0 r)
    go (PmLitString s) = Just (PmLitOverString s)
    go _               = Nothing

pmLitAsStringLit :: PmLit -> Maybe FastString
pmLitAsStringLit (PmLit _ (PmLitString s)) = Just s
pmLitAsStringLit _                         = Nothing

-- | A term constraint. @TVC x e@ encodes that @x@ is equal to @e@.
data TmVarCt = TVC !Id !PmExpr

instance Outputable TmVarCt where
  ppr (TVC x e) = ppr x <+> char '~' <+> ppr e

-- ----------------------------------------------------------------------------
-- ** Predicates on PmExpr

-- -----------------------------------------------------------------------
-- ** Lift source expressions (HsExpr Id) to PmExpr

-- | Mimics 'MatchLit.tidyNPat', but solely for purposes of better pattern match
-- warnings. See Note [Translate Overloaded Literals for Exhaustiveness Checking]
hsOverLitAsHsLit :: DynFlags -> HsOverLit GhcTc -> Bool -> Type -> Maybe (HsLit GhcTc)
hsOverLitAsHsLit dflags (OverLit (OverLitTc False ty) val _) negated outer_ty
  | types_equal, isStringTy   ty, HsIsString src s <- val, not negated
  = Just (HsString src s)
  | types_equal,  isIntTy     ty, HsIntegral i <- val
  , let ni = apply_negation i
  , inIntRange dflags (il_value ni)
  = Just (HsInt noExtField ni)
  | types_equal,  isWordTy    ty, HsIntegral i <- val
  , let ni = apply_negation i
  , inWordRange dflags (il_value ni)
  -- this is a type error... But as long as we do this consistently, this
  -- shouldn't be a problem. The proper solution would be to return a HsExpr,
  -- but that would require non-trivial boilerplate at the two call sites.
  = Just (HsWordPrim (il_text ni) (il_value ni))
  | types_equal,  isIntegerTy ty, HsIntegral i <- val
  , let ni = apply_negation i
  = Just (HsInteger (il_text ni) (il_value ni) ty)
  where
    types_equal = eqType outer_ty ty
    apply_negation
      | negated   = negateIntegralLit
      | otherwise = id
hsOverLitAsHsLit _ _ _ _ = Nothing

{- Note [Translate Overloaded Literals for Exhaustiveness Checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Due to Note [Undecidable Equality for PmAltCons], the exhaustiveness checker can
do a much better job for primitive literals than for overloaded literals. So,
ideally, we'd run exhaustiveness checks /after/ we had translated part of the
overloaded literals ('HsOverLit') to their primitive counterpart ('HsLit') by
use of 'MatchLit.tidyNPat'. But since that is not the case, we have to mimic its
logic in 'translateNPat'.

But that only concerns patterns! We surely have to make sure to translate
expressions in a similar way in 'hsExprToPmExpr'. Thus, both 'translateNPat' and
'hsExprToPmExpr' call out to 'hsOverLitAsHsLit'.

Before #14546, the translations in 'hsExprToPmExpr' and 'translateNPat' were out
of sync, so the resulting comparisons between 'PmSLit' and 'PmOLit' could never
return True. But the fix in #14546 translated many occurrences where we could
have 'PmSLit's into 'PmOLit's, resulting in a loss of precision due to
Note [Undecidable Equality for PmAltCons].

Now, we translate the literal value to match and the literal patterns
consistently and try to turn them into plain literals as often as possible:

  * For integral literals, we parse both the integral literal value and
    the patterns as HsInt/HsWordPrim/HsInteger. For example:

      case 0::Int of
          0 -> putStrLn "A"
          1 -> putStrLn "B"
          _ -> putStrLn "C"

    Note that we can decide now that the last two clauses are redundant.

  * For string literals, we parse the string literals as HsString. When
    OverloadedStrings is enabled and applicable for the data type, it will be
    turned into a HsOverLit HsIsString. For example:

      case "foo" of
          "foo" -> putStrLn "A"
          "bar" -> putStrLn "B"
          "baz" -> putStrLn "C"

    Even in the presence of -XOverloadedStrings, if @"foo" :: String@, the
    overloaded string and its patterns will be turned into a list of character
    literals, while for any other data type that satisfies 'IsString', this will
    turn into the respective 'PmOLit'.

    The desugaring of actual Strings to lists is so that we catch the redundant
    pattern in following case:

      case "foo" of
          ('f':_) -> putStrLn "A"
          "bar" -> putStrLn "B"

    For overloaded strings, we can still capture the exhaustiveness of pattern
    "foo" and the redundancy of pattern "bar" and "baz" in the following code:

      {-# LANGUAGE OverloadedStrings #-}
      data D = ...
      instance IsString D where ...
      main = do
        case "foo" :: D of
            "foo" -> putStrLn "A"
            "bar" -> putStrLn "B"
            "baz" -> putStrLn "C"
-}

-- | Return @Just@ a 'DataCon' application or @Nothing@, otherwise.
pmExprToDataConApp :: PmExpr -> Maybe (DataCon, [Id])
pmExprToDataConApp (PmExprCon (PmAltConLike (RealDataCon c)) es) = Just (c, es)
pmExprToDataConApp _                                             = Nothing

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

instance Outputable PmExpr where
  ppr (PmExprVar v) = ppr v
  ppr (pmExprToDataConApp -> Just (dc, args))
    | isUnboxedTupleCon dc
    = text "(#" <+> (fsep $ punctuate comma $ map ppr args) <+> text "#)"
    | isTupleDataCon dc
    = parens $ fsep $ punctuate comma $ map ppr args
  ppr (PmExprCon cl args) = hsep (ppr cl:map ppr args)
