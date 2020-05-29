{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>
        Sebastian Graf <sgraf1337@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

-- | Types used through-out pattern match checking. This module is mostly there
-- to be imported from "GHC.Tc.Types". The exposed API is that of
-- "GHC.HsToCore.PmCheck.Oracle" and "GHC.HsToCore.PmCheck".
module GHC.HsToCore.PmCheck.Types (
        -- * Representations for Literals and AltCons
        PmLit(..), PmLitValue(..), PmAltCon(..), pmLitType, pmAltConType,

        -- ** Equality on 'PmAltCon's
        PmEquality(..), eqPmAltCon,

        -- ** Operations on 'PmLit'
        literalToPmLit, negatePmLit, overloadPmLit,
        pmLitAsStringLit, coreExprAsPmLit,

        -- * Caching partially matched COMPLETE sets
        ConLikeSet, PossibleMatches(..),

        -- * PmAltConSet
        PmAltConSet, emptyPmAltConSet, isEmptyPmAltConSet, elemPmAltConSet,
        extendPmAltConSet, pmAltConSetElems,

        -- * A 'DIdEnv' where entries may be shared
        Shared(..), SharedDIdEnv(..), emptySDIE, lookupSDIE, sameRepresentativeSDIE,
        setIndirectSDIE, setEntrySDIE, traverseSDIE,

        -- * The pattern match oracle
        VarInfo(..), TmState(..), TyState(..), Delta(..),
        Deltas(..), initDeltas, liftDeltasM
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Utils.Misc
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Types.Var (EvVar)
import GHC.Types.Id
import GHC.Types.Var.Env
import GHC.Types.Unique.DSet
import GHC.Types.Unique.DFM
import GHC.Types.Name
import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Utils.Outputable
import GHC.Data.List.SetOps (unionLists)
import GHC.Data.Maybe
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Types.Literal
import GHC.Core
import GHC.Core.Map
import GHC.Core.Utils (exprType)
import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Tc.Utils.TcType (evVarPred)

import Numeric (fromRat)
import Data.Foldable (find)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ratio
import qualified Data.Semigroup as Semi

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
-- Really similar to 'GHC.Core.AltCon'.
data PmAltCon = PmAltConLike ConLike
              | PmAltLit     PmLit

data PmAltConSet = PACS !ConLikeSet ![PmLit]

emptyPmAltConSet :: PmAltConSet
emptyPmAltConSet = PACS emptyUniqDSet []

isEmptyPmAltConSet :: PmAltConSet -> Bool
isEmptyPmAltConSet (PACS cls lits) = isEmptyUniqDSet cls && null lits

-- | Whether there is a 'PmAltCon' in the 'PmAltConSet' that compares 'Equal' to
-- the given 'PmAltCon' according to 'eqPmAltCon'.
elemPmAltConSet :: PmAltCon -> PmAltConSet -> Bool
elemPmAltConSet (PmAltConLike cl) (PACS cls _   ) = elementOfUniqDSet cl cls
elemPmAltConSet (PmAltLit lit)    (PACS _   lits) = elem lit lits

extendPmAltConSet :: PmAltConSet -> PmAltCon -> PmAltConSet
extendPmAltConSet (PACS cls lits) (PmAltConLike cl)
  = PACS (addOneToUniqDSet cls cl) lits
extendPmAltConSet (PACS cls lits) (PmAltLit lit)
  = PACS cls (unionLists lits [lit])

pmAltConSetElems :: PmAltConSet -> [PmAltCon]
pmAltConSetElems (PACS cls lits)
  = map PmAltConLike (uniqDSetToList cls) ++ map PmAltLit lits

instance Outputable PmAltConSet where
  ppr = ppr . pmAltConSetElems

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

-- | Type of a 'PmAltCon'
pmAltConType :: PmAltCon -> [Type] -> Type
pmAltConType (PmAltLit lit)     _arg_tys = ASSERT( null _arg_tys ) pmLitType lit
pmAltConType (PmAltConLike con) arg_tys  = conLikeResTy con arg_tys

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
    `uncovered` and `divergent` in "GHC.HsToCore.PmCheck" and effectively addresses
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

type ConLikeSet = UniqDSet ConLike

-- | A data type caching the results of 'completeMatchConLikes' with support for
-- deletion of constructors that were already matched on.
data PossibleMatches
  = PM (NonEmpty.NonEmpty ConLikeSet)
  -- ^ Each ConLikeSet is a (subset of) the constructors in a COMPLETE set
  -- 'NonEmpty' because the empty case would mean that the type has no COMPLETE
  -- set at all, for which we have 'NoPM'.
  | NoPM
  -- ^ No COMPLETE set for this type (yet). Think of overloaded literals.

instance Outputable PossibleMatches where
  ppr (PM cs) = ppr (NonEmpty.toList cs)
  ppr NoPM = text "<NoPM>"

-- | Either @Indirect x@, meaning the value is represented by that of @x@, or
-- an @Entry@ containing containing the actual value it represents.
data Shared a
  = Indirect Id
  | Entry a

-- | A 'DIdEnv' in which entries can be shared by multiple 'Id's.
-- Merge equivalence classes of two Ids by 'setIndirectSDIE' and set the entry
-- of an Id with 'setEntrySDIE'.
newtype SharedDIdEnv a
  = SDIE { unSDIE :: DIdEnv (Shared a) }

emptySDIE :: SharedDIdEnv a
emptySDIE = SDIE emptyDVarEnv

lookupReprAndEntrySDIE :: SharedDIdEnv a -> Id -> (Id, Maybe a)
lookupReprAndEntrySDIE sdie@(SDIE env) x = case lookupDVarEnv env x of
  Nothing           -> (x, Nothing)
  Just (Indirect y) -> lookupReprAndEntrySDIE sdie y
  Just (Entry a)    -> (x, Just a)

-- | @lookupSDIE env x@ looks up an entry for @x@, looking through all
-- 'Indirect's until it finds a shared 'Entry'.
lookupSDIE :: SharedDIdEnv a -> Id -> Maybe a
lookupSDIE sdie x = snd (lookupReprAndEntrySDIE sdie x)

-- | Check if two variables are part of the same equivalence class.
sameRepresentativeSDIE :: SharedDIdEnv a -> Id -> Id -> Bool
sameRepresentativeSDIE sdie x y =
  fst (lookupReprAndEntrySDIE sdie x) == fst (lookupReprAndEntrySDIE sdie y)

-- | @setIndirectSDIE env x y@ sets @x@'s 'Entry' to @Indirect y@, thereby
-- merging @x@'s equivalence class into @y@'s. This will discard all info on
-- @x@!
setIndirectSDIE :: SharedDIdEnv a -> Id -> Id -> SharedDIdEnv a
setIndirectSDIE sdie@(SDIE env) x y =
  SDIE $ extendDVarEnv env (fst (lookupReprAndEntrySDIE sdie x)) (Indirect y)

-- | @setEntrySDIE env x a@ sets the 'Entry' @x@ is associated with to @a@,
-- thereby modifying its whole equivalence class.
setEntrySDIE :: SharedDIdEnv a -> Id -> a -> SharedDIdEnv a
setEntrySDIE sdie@(SDIE env) x a =
  SDIE $ extendDVarEnv env (fst (lookupReprAndEntrySDIE sdie x)) (Entry a)

traverseSDIE :: Applicative f => (a -> f b) -> SharedDIdEnv a -> f (SharedDIdEnv b)
traverseSDIE f = fmap (SDIE . listToUDFM) . traverse g . udfmToList . unSDIE
  where
    g (u, Indirect y) = pure (u,Indirect y)
    g (u, Entry a)    = (u,) . Entry <$> f a

instance Outputable a => Outputable (Shared a) where
  ppr (Indirect x) = ppr x
  ppr (Entry a)    = ppr a

instance Outputable a => Outputable (SharedDIdEnv a) where
  ppr (SDIE env) = ppr env

-- | The term oracle state. Stores 'VarInfo' for encountered 'Id's. These
-- entries are possibly shared when we figure out that two variables must be
-- equal, thus represent the same set of values.
--
-- See Note [TmState invariants] in Oracle.
data TmState
  = TmSt
  { ts_facts :: !(SharedDIdEnv VarInfo)
  -- ^ Facts about term variables. Deterministic env, so that we generate
  -- deterministic error messages.
  , ts_reps  :: !(CoreMap Id)
  -- ^ An environment for looking up whether we already encountered semantically
  -- equivalent expressions that we want to represent by the same 'Id'
  -- representative.
  }

-- | Information about an 'Id'. Stores positive ('vi_pos') facts, like @x ~ Just 42@,
-- and negative ('vi_neg') facts, like "x is not (:)".
-- Also caches the type ('vi_ty'), the 'PossibleMatches' of a COMPLETE set
-- ('vi_cache').
--
-- Subject to Note [The Pos/Neg invariant] in PmOracle.
data VarInfo
  = VI
  { vi_ty  :: !Type
  -- ^ The type of the variable. Important for rejecting possible GADT
  -- constructors or incompatible pattern synonyms (@Just42 :: Maybe Int@).

  , vi_pos :: ![(PmAltCon, [TyVar], [Id])]
  -- ^ Positive info: 'PmAltCon' apps it is (i.e. @x ~ [Just y, PatSyn z]@), all
  -- at the same time (i.e. conjunctive).  We need a list because of nested
  -- pattern matches involving pattern synonym
  --    case x of { Just y -> case x of PatSyn z -> ... }
  -- However, no more than one RealDataCon in the list, otherwise contradiction
  -- because of generativity.

  , vi_neg :: !PmAltConSet
  -- ^ Negative info: A list of 'PmAltCon's that it cannot match.
  -- Example, assuming
  --
  -- @
  --     data T = Leaf Int | Branch T T | Node Int T
  -- @
  --
  -- then @x /~ [Leaf, Node]@ means that @x@ cannot match a @Leaf@ or @Node@,
  -- and hence can only match @Branch@. Is orthogonal to anything from 'vi_pos',
  -- in the sense that 'eqPmAltCon' returns @PossiblyOverlap@ for any pairing
  -- between 'vi_pos' and 'vi_neg'.

  -- See Note [Why record both positive and negative info?]
  -- It's worth having an actual set rather than a simple association list,
  -- because files like Cabal's `LicenseId` define relatively huge enums
  -- that lead to quadratic or worse behavior.

  , vi_cache :: !PossibleMatches
  -- ^ A cache of the associated COMPLETE sets. At any time a superset of
  -- possible constructors of each COMPLETE set. So, if it's not in here, we
  -- can't possibly match on it. Complementary to 'vi_neg'. We still need it
  -- to recognise completion of a COMPLETE set efficiently for large enums.
  }

-- | Not user-facing.
instance Outputable TmState where
  ppr (TmSt state reps) = ppr state $$ ppr reps

-- | Not user-facing.
instance Outputable VarInfo where
  ppr (VI ty pos neg cache)
    = braces (hcat (punctuate comma [ppr ty, ppr pos, ppr neg, ppr cache]))

-- | Initial state of the term oracle.
initTmState :: TmState
initTmState = TmSt emptySDIE emptyCoreMap

-- | The type oracle state. A poor man's 'GHC.Tc.Solver.Monad.InsertSet': The invariant is
-- that all constraints in there are mutually compatible.
newtype TyState = TySt (Bag EvVar)

-- | Not user-facing.
instance Outputable TyState where
  ppr (TySt evs)
    = braces $ hcat $ punctuate comma $ map (ppr . evVarPred) $ bagToList evs

initTyState :: TyState
initTyState = TySt emptyBag

-- | An inert set of canonical (i.e. mutually compatible) term and type
-- constraints.
data Delta = MkDelta { delta_ty_st :: TyState    -- Type oracle; things like a~Int
                     , delta_tm_st :: TmState }  -- Term oracle; things like x~Nothing

-- | An initial delta that is always satisfiable
initDelta :: Delta
initDelta = MkDelta initTyState initTmState

instance Outputable Delta where
  ppr delta = hang (text "Delta") 2 $ vcat [
      -- intentionally formatted this way enable the dev to comment in only
      -- the info she needs
      ppr (delta_tm_st delta),
      ppr (delta_ty_st delta)
    ]

-- | A disjunctive bag of 'Delta's, representing a refinement type.
newtype Deltas = MkDeltas (Bag Delta)

initDeltas :: Deltas
initDeltas = MkDeltas (unitBag initDelta)

instance Outputable Deltas where
  ppr (MkDeltas deltas) = ppr deltas

instance Semigroup Deltas where
  MkDeltas l <> MkDeltas r = MkDeltas (l `unionBags` r)

liftDeltasM :: Monad m => (Delta -> m (Maybe Delta)) -> Deltas -> m Deltas
liftDeltasM f (MkDeltas ds) = MkDeltas . catBagMaybes <$> (traverse f ds)
