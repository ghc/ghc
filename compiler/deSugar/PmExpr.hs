{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

Haskell expressions (as used by the pattern matching checker) and utilities.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PmExpr (
        PmExpr(..), PmLit(..), PmAltCon(..), TmVarCt(..), pmExprFVs, pmLitType,
        pmAltConArity, isNotPmExprOther, hsOverLitAsHsLit, lhsExprToPmExpr,
        hsExprToPmExpr, mkPmExprLit, decEqPmAltCon, PmExprList(..), pmExprAsList
    ) where

#include "HsVersions.h"

import GhcPrelude

import Util
import BasicTypes (SourceText, IntegralLit(..), negateIntegralLit)
import FastString (FastString, unpackFS)
import HsSyn
import Id
import Name
import NameSet
import DataCon
import ConLike
import TcEvidence (isErasableHsWrapper)
import TcType (Type, eqType, isStringTy, isIntTy, isIntegerTy, isWordTy)
import TcHsSyn (hsLitType)
import TysWiredIn
import Outputable
import SrcLoc
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)

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
            | PmExprCon   PmAltCon [PmExpr]
            | PmExprOther (HsExpr GhcTc)  -- Note [PmExprOther in PmExpr]

-- | Literals (simple and overloaded ones) for pattern match checking.
--
-- See Note [Undecidable Equality for PmAltCons]
data PmLit = PmSLit (HsLit GhcTc)                               -- simple
           | PmOLit Bool {- is it negated? -} (HsOverLit GhcTc) -- overloaded

-- | Undecidable equality for values represented by 'PmLit's.
-- See Note [Undecidable Equality for PmAltCons]
--
-- * @Just True@ ==> Surely equal
-- * @Just False@ ==> Surely different (non-overlapping, even!)
-- * @Nothing@ ==> Equality relation undecidable
decEqPmLit :: PmLit -> PmLit -> Maybe Bool
decEqPmLit (PmSLit    l1) (PmSLit    l2) = Just (l1 == l2)
decEqPmLit (PmOLit b1 l1) (PmOLit b2 l2)
  | b1 == b2, l1 == l2
  = Just True
decEqPmLit _              _              = Nothing

-- | Syntactic equality.
instance Eq PmLit where
  a == b = decEqPmLit a b == Just True

pmExprFVs :: PmExpr -> NameSet
pmExprFVs (PmExprVar x)      = unitNameSet x
pmExprFVs (PmExprCon _ args) = unionNameSets (map pmExprFVs args)
pmExprFVs (PmExprOther _)    = emptyNameSet

-- | Type of a PmLit
pmLitType :: PmLit -> Type
pmLitType (PmSLit   lit) = hsLitType   lit
pmLitType (PmOLit _ lit) = overLitType lit

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
decEqPmAltCon :: PmAltCon -> PmAltCon -> Maybe Bool
decEqPmAltCon (PmAltConLike cl1) (PmAltConLike cl2) = decEqConLike cl1 cl2
decEqPmAltCon (PmAltLit     l1)  (PmAltLit     l2)  = decEqPmLit l1 l2
decEqPmAltCon _                  _                  = Nothing

-- | Syntactic equality.
instance Eq PmAltCon where
  a == b = decEqPmAltCon a b == Just True

pmAltConArity :: PmAltCon -> Int
pmAltConArity (PmAltConLike con) = conLikeArity con
pmAltConArity (PmAltLit _)       = 0

mkPmExprData :: DataCon -> [PmExpr] -> PmExpr
mkPmExprData dc args = PmExprCon (PmAltConLike (RealDataCon dc)) args

mkPmExprLit :: PmLit -> PmExpr
mkPmExprLit l = PmExprCon (PmAltLit l) []

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
synonyms F and G aren't immediately unsatisfiable. We know F a ~ F a, though.
-}

-- | A term constraint. @TVC x e@ encodes that @x@ is equal to @e@.
data TmVarCt = TVC !Id !PmExpr

instance Outputable TmVarCt where
  ppr (TVC x e) = ppr x <+> char '~' <+> ppr e

-- ----------------------------------------------------------------------------
-- ** Predicates on PmExpr

-- | Check if an expression is lifted or not
isNotPmExprOther :: PmExpr -> Bool
isNotPmExprOther (PmExprOther _) = False
isNotPmExprOther _expr           = True

-- -----------------------------------------------------------------------
-- ** Lift source expressions (HsExpr Id) to PmExpr

-- | Mimics 'MatchLit.tidyNPat', but solely for purposes of better pattern match
-- warnings. See Note [Translate Overloaded Literals for Exhaustiveness Checking]
hsOverLitAsHsLit :: HsOverLit GhcTc -> Bool -> Type -> Maybe (HsLit GhcTc)
hsOverLitAsHsLit (OverLit (OverLitTc False ty) val _) negated outer_ty
  | types_equal, isStringTy    ty, HsIsString src s <- val, not negated
  = Just (HsString src s)
  | types_equal,  isIntTy       ty, HsIntegral i <- val
  = Just (HsInt noExtField (apply_negation i))
  | types_equal,  isWordTy      ty, HsIntegral i <- val
  , let ni = apply_negation i
  -- this is a type error... But as long as we do this consistently, this
  -- shouldn't be a problem. The proper solution would be to return a HsExpr,
  -- but that would require non-trivial boilerplate at the two call sites.
  = Just (HsWordPrim (il_text ni) (il_value ni))
  | types_equal,  isIntegerTy   ty, HsIntegral i <- val
  , let ni = apply_negation i
  = Just (HsInteger (il_text ni) (il_value ni) ty)
  where
    types_equal = eqType outer_ty ty
    apply_negation
      | negated   = negateIntegralLit
      | otherwise = id
hsOverLitAsHsLit _ _ _ = Nothing

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

lhsExprToPmExpr :: LHsExpr GhcTc -> PmExpr
lhsExprToPmExpr (dL->L _ e) = hsExprToPmExpr e

hsExprToPmExpr :: HsExpr GhcTc -> PmExpr

-- Translating HsVar to flexible meta variables in the unification problem is
-- morally wrong, but it does the right thing for now.
-- In contrast to the situation in pattern matches, HsVars in expression syntax
-- are object language variables, most similar to rigid variables with an
-- unknown solution. The correct way would be to handle them through PmExprOther
-- and identify syntactically equal occurrences by the same rigid meta variable,
-- but we can't compare the wrapped HsExpr for equality. Hence we are stuck with
-- this hack.
hsExprToPmExpr (HsVar        _ x) = PmExprVar (idName (unLoc x))

-- Translating HsConLikeOut to a flexible meta variable is misleading.
-- For an example why, consider `consAreRigid` in
-- `testsuite/tests/pmcheck/should_compile/PmExprVars.hs`.
-- hsExprToPmExpr (HsConLikeOut _ c) = PmExprVar (conLikeName c)

-- See Note [Translate Overloaded Literals for Exhaustiveness Checking]
hsExprToPmExpr (HsOverLit _ olit)
  | Just lit <- hsOverLitAsHsLit olit False (overLitType olit)
  = hsExprToPmExpr (HsLit noExtField lit)
  | otherwise
  = PmExprCon (PmAltLit (PmOLit False olit)) []
hsExprToPmExpr (HsLit     _ lit)
  | HsString src s <- lit
  = stringExprToList src s
  | otherwise = PmExprCon (PmAltLit (PmSLit lit)) []

hsExprToPmExpr e@(NegApp _ (dL->L _ neg_expr) _)
  | PmExprCon (PmAltLit (PmOLit False olit)) _ <- hsExprToPmExpr neg_expr
    -- NB: DON'T simply @(NegApp (NegApp olit))@ as @x@. when extension
    -- @RebindableSyntax@ enabled, (-(-x)) may not equals to x.
  = PmExprCon (PmAltLit (PmOLit True olit)) []
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
hsExprToPmExpr (HsWrap           _ w e)
  -- A dictionary application spoils e and we have no choice but to return an
  -- PmExprOther. Same thing for other stuff that can't erased in the
  -- compilation process. Otherwise this bites in
  -- teststuite/tests/pmcheck/should_compile/PmExprVars.hs.
  | isErasableHsWrapper w = hsExprToPmExpr e
hsExprToPmExpr e = PmExprOther e -- the rest are not handled by the oracle

stringExprToList :: SourceText -> FastString -> PmExpr
stringExprToList src s = foldr cons nil (map charToPmExpr (unpackFS s))
  where
    cons x xs      = mkPmExprData consDataCon [x,xs]
    nil            = mkPmExprData nilDataCon  []
    charToPmExpr c = PmExprCon (PmAltLit (PmSLit (HsChar src c))) []

-- | Return @Just@ a 'DataCon' application or @Nothing@, otherwise.
pmExprToDataConApp :: PmExpr -> Maybe (DataCon, [PmExpr])
pmExprToDataConApp (PmExprCon (PmAltConLike (RealDataCon c)) es) = Just (c, es)
pmExprToDataConApp _                                             = Nothing

-- | The result of 'pmExprAsList'.
data PmExprList
  = NilTerminated [PmExpr]
  | WcVarTerminated (NonEmpty PmExpr) Name

-- | Extract a list of 'PmExpr's out of a sequence of cons cells, optionally
-- terminated by a wildcard variable instead of @[]@. Some examples:
--
-- * @pmExprAsList (1:2:[]) == Just ('NilTerminated' [1,2])@, a regular,
--   @[]@-terminated list. Should be pretty-printed as @[1,2]@.
-- * @pmExprAsList (1:2:x) == Just ('WcVarTerminated' [1,2] x)@, a list prefix
--   ending in a wildcard variable x (of list type). Should be pretty-printed as
--   (1:2:_).
-- * @pmExprAsList [] == Just ('NilTerminated' [])@
pmExprAsList :: PmExpr -> Maybe PmExprList
pmExprAsList = go []
  where
    go rev_pref (PmExprVar x)
      | Just pref <- nonEmpty (reverse rev_pref)
      = Just (WcVarTerminated pref x)
    go rev_pref (pmExprToDataConApp -> Just (c, es))
      | c == nilDataCon
      = ASSERT( null es ) Just (NilTerminated (reverse rev_pref))
      | c == consDataCon
      = ASSERT( length es == 2 ) go (es !! 0 : rev_pref) (es !! 1)
    go _ _
      = Nothing

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

instance Outputable PmAltCon where
  ppr (PmAltConLike cl) = ppr cl
  ppr (PmAltLit l)      = ppr l

instance Outputable PmExpr where
  ppr = go (0 :: Int)
    where
      go _    (PmExprVar v) = ppr v
      go _    (PmExprOther e) = angleBrackets (ppr e)
      go _    (pmExprAsList -> Just pm_expr_list) = case pm_expr_list of
        NilTerminated list
          | Just lits <- as_string list
          -> doubleQuotes $ hcat $ map ppr lits
          | otherwise
          -> brackets $ fsep $ punctuate comma $ map ppr list
        WcVarTerminated pref x
          | Just lits <- as_string (toList pref)
          -> hcat [doubleQuotes $ hcat $ map ppr lits, text "++", ppr x]
          | otherwise
          -> parens $ fcat $ punctuate colon $ map ppr (toList pref) ++ [ppr x]
      go _    (pmExprToDataConApp -> Just (dc, args))
        | isTupleDataCon dc = parens $ fsep $ punctuate comma $ map ppr args
      go prec (PmExprCon cl args)
        = cparen (notNull args && prec > 0) (hsep (ppr cl:map (go 1) args))

      as_string = traverse as_char_lit
      as_char_lit (PmExprCon (PmAltLit lit) [])
        | isStringTy (pmLitType lit) = Just lit
      as_char_lit _ = Nothing
