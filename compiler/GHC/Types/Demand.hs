{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

-- | A language to express the evaluation context of an expression as a
-- 'Demand' and track how an expression evaluates free variables and arguments
-- in turn as a 'DmdType'.
--
-- Lays out the abstract domain for "GHC.Core.Opt.DmdAnal".
module GHC.Types.Demand (
    -- * Demands
    Card(..), Demand(..), SubDemand(Prod), mkProd, viewProd,
    -- ** Algebra
    absDmd, topDmd, botDmd, seqDmd, topSubDmd,
    -- *** Least upper bound
    lubCard, lubDmd, lubSubDmd,
    -- *** Plus
    plusCard, plusDmd, plusSubDmd,
    -- *** Multiply
    multCard, multDmd, multSubDmd,
    -- ** Predicates on @Card@inalities and @Demand@s
    isAbs, isUsedOnce, isStrict,
    isAbsDmd, isUsedOnceDmd, isStrUsedDmd, isStrictDmd,
    isTopDmd, isSeqDmd, isWeakDmd,
    -- ** Special demands
    evalDmd,
    -- *** Demands used in PrimOp signatures
    lazyApply1Dmd, lazyApply2Dmd, strictOnceApply1Dmd, strictManyApply1Dmd,
    -- ** Other @Demand@ operations
    oneifyCard, oneifyDmd, strictifyDmd, strictifyDictDmd, mkWorkerDemand,
    peelCallDmd, peelManyCalls, mkCalledOnceDmd, mkCalledOnceDmds,
    addCaseBndrDmd,
    -- ** Extracting one-shot information
    argOneShots, argsOneShots, saturatedByOneShots,

    -- * Demand environments
    DmdEnv, emptyDmdEnv,
    keepAliveDmdEnv, reuseEnv,

    -- * Divergence
    Divergence(..), topDiv, botDiv, exnDiv, lubDivergence, isDeadEndDiv,

    -- * Demand types
    DmdType(..), dmdTypeDepth,
    -- ** Algebra
    nopDmdType, botDmdType,
    lubDmdType, plusDmdType, multDmdType,
    -- *** PlusDmdArg
    PlusDmdArg, mkPlusDmdArg, toPlusDmdArg,
    -- ** Other operations
    peelFV, findIdDemand, addDemand, splitDmdTy, deferAfterPreciseException,
    keepAliveDmdType,

    -- * Demand signatures
    StrictSig(..), mkStrictSigForArity, mkClosedStrictSig,
    splitStrictSig, strictSigDmdEnv, hasDemandEnvSig,
    nopSig, botSig, isTopSig, isDeadEndSig, appIsDeadEnd,
    -- ** Handling arity adjustments
    prependArgsStrictSig, etaConvertStrictSig,

    -- * Demand transformers from demand signatures
    DmdTransformer, dmdTransformSig, dmdTransformDataConSig, dmdTransformDictSelSig,

    -- * Trim to a type shape
    TypeShape(..), trimToType,

    -- * @seq@ing stuff
    seqDemand, seqDemandList, seqDmdType, seqStrictSig,

    -- * Zapping usage information
    zapUsageDemand, zapDmdEnvSig, zapUsedOnceDemand, zapUsedOnceSig
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Var ( Var, Id )
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Unique.FM
import GHC.Types.Basic
import GHC.Data.Maybe   ( orElse )

import GHC.Core.Type    ( Type )
import GHC.Core.TyCon   ( isNewTyCon, isClassTyCon )
import GHC.Core.DataCon ( splitDataProductType_maybe )
import GHC.Core.Multiplicity    ( scaledThing )

import GHC.Utils.Binary
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

{-
************************************************************************
*                                                                      *
           Card: Combining Strictness and Usage
*                                                                      *
************************************************************************
-}

{- Note [Evaluation cardinalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The demand analyser uses an /evaluation cardinality/ of type Card,
to specify how many times a term is evaluated.  A cardinality C_lu
represents an /interval/ [l..u], meaning
    C_lu means evaluated /at least/ 'l' times and
                         /at most/  'u' times

* The lower bound corresponds to /strictness/
  Hence 'l' is either 0 (lazy)
                   or 1 (strict)

* The upper bound corresponds to /usage/
  Hence 'u' is either 0 (not used at all),
                   or 1 (used at most once)
                   or n (no information)

Intervals describe sets, so the underlying lattice is the powerset lattice.

Usually l<=u, but we also have C_10, the interval [1,0], the empty interval,
denoting the empty set.   This is the bottom element of the lattice.

See Note [Demand notation] for the notation we use for each of the constructors.
-}


-- | Describes an interval of /evaluation cardinalities/.
-- See Note [Evaluation cardinalities]
data Card
  = C_00 -- ^ {0}     Absent.
  | C_01 -- ^ {0,1}   Used at most once.
  | C_0N -- ^ {0,1,n} Every possible cardinality; the top element.
  | C_11 -- ^ {1}     Strict and used once.
  | C_1N -- ^ {1,n}   Strict and used (possibly) many times.
  | C_10 -- ^ {}      The empty interval; the bottom element of the lattice.
  deriving Eq

_botCard, topCard :: Card
_botCard = C_10
topCard = C_0N

-- | True <=> lower bound is 1.
isStrict :: Card -> Bool
isStrict C_10 = True
isStrict C_11 = True
isStrict C_1N = True
isStrict _    = False

-- | True <=> upper bound is 0.
isAbs :: Card -> Bool
isAbs C_00 = True
isAbs C_10 = True -- Bottom cardinality is also absent
isAbs _    = False

-- | True <=> upper bound is 1.
isUsedOnce :: Card -> Bool
isUsedOnce C_0N = False
isUsedOnce C_1N = False
isUsedOnce _    = True

-- | Intersect with [0,1].
oneifyCard :: Card -> Card
oneifyCard C_0N = C_01
oneifyCard C_1N = C_11
oneifyCard c    = c

-- | Denotes '∪' on 'Card'.
lubCard :: Card -> Card -> Card
-- Handle C_10 (bot)
lubCard C_10 n    = n    -- bot
lubCard n    C_10 = n    -- bot
-- Handle C_0N (top)
lubCard C_0N _    = C_0N -- top
lubCard _    C_0N = C_0N -- top
-- Handle C_11
lubCard C_00 C_11 = C_01 -- {0} ∪ {1} = {0,1}
lubCard C_11 C_00 = C_01 -- {0} ∪ {1} = {0,1}
lubCard C_11 n    = n    -- {1} is a subset of all other intervals
lubCard n    C_11 = n    -- {1} is a subset of all other intervals
-- Handle C_1N
lubCard C_1N C_1N = C_1N -- reflexivity
lubCard _    C_1N = C_0N -- {0} ∪ {1,n} = top
lubCard C_1N _    = C_0N -- {0} ∪ {1,n} = top
-- Handle C_01
lubCard C_01 _    = C_01 -- {0} ∪ {0,1} = {0,1}
lubCard _    C_01 = C_01 -- {0} ∪ {0,1} = {0,1}
-- Handle C_00
lubCard C_00 C_00 = C_00 -- reflexivity

-- | Denotes '+' on 'Card'.
plusCard :: Card -> Card -> Card
-- Handle C_00
plusCard C_00 n    = n    -- {0}+n = n
plusCard n    C_00 = n    -- {0}+n = n
-- Handle C_10
plusCard C_10 C_01 = C_11 -- These follow by applying + to lower and upper
plusCard C_10 C_0N = C_1N -- bounds individually
plusCard C_10 n    = n
plusCard C_01 C_10 = C_11
plusCard C_0N C_10 = C_1N
plusCard n    C_10 = n
-- Handle the rest (C_01, C_0N, C_11, C_1N)
plusCard C_01 C_01 = C_0N -- The upper bound is at least 1, so upper bound of
plusCard C_01 C_0N = C_0N -- the result must be 1+1 ~= N.
plusCard C_0N C_01 = C_0N -- But for the lower bound we have 4 cases where
plusCard C_0N C_0N = C_0N -- 0+0 ~= 0 (as opposed to 1), so we match on these.
plusCard _    _    = C_1N -- Otherwise we return {1,n}

-- | Denotes '*' on 'Card'.
multCard :: Card -> Card -> Card
-- Handle C_11 (neutral element)
multCard C_11 c    = c
multCard c    C_11 = c
-- Handle C_00 (annihilating element)
multCard C_00 _    = C_00
multCard _    C_00 = C_00
-- Handle C_10
multCard C_10 c    = if isStrict c then C_10 else C_00
multCard c    C_10 = if isStrict c then C_10 else C_00
-- Handle reflexive C_1N, C_01
multCard C_1N C_1N = C_1N
multCard C_01 C_01 = C_01
-- Handle C_0N and the rest (C_01, C_1N):
multCard _    _    = C_0N

{-
************************************************************************
*                                                                      *
           Demand: Evaluation contexts
*                                                                      *
************************************************************************
-}

-- | A demand describes a /scaled evaluation context/, e.g. how many times
-- and how deep the denoted thing is evaluated.
--
-- The "how many" component is represented by a 'Card'inality.
-- The "how deep" component is represented by a 'SubDemand'.
-- Examples (using Note [Demand notation]):
--
--   * 'seq' puts demand @1A@ on its first argument: It evaluates the argument
--     strictly (@1@), but not any deeper (@A@).
--   * 'fst' puts demand @1P(1L,A)@ on its argument: It evaluates the argument
--     pair strictly and the first component strictly, but no nested info
--     beyond that (@L@). Its second argument is not used at all.
--   * '$' puts demand @1C1(L)@ on its first argument: It calls (@C@) the
--     argument function with one argument, exactly once (@1@). No info
--     on how the result of that call is evaluated (@L@).
--   * 'maybe' puts demand @MCM(L)@ on its second argument: It evaluates
--     the argument function at most once ((M)aybe) and calls it once when
--     it is evaluated.
--   * @fst p + fst p@ puts demand @SP(SL,A)@ on @p@: It's @1P(1L,A)@
--     multiplied by two, so we get @S@ (used at least once, possibly multiple
--     times).
--
-- This data type is quite similar to @'Scaled' 'SubDemand'@, but it's scaled
-- by 'Card', which is an /interval/ on 'Multiplicity', the upper bound of
-- which could be used to infer uniqueness types.
data Demand
  = !Card :* !SubDemand
  deriving Eq

-- | A sub-demand describes an /evaluation context/, e.g. how deep the
-- denoted thing is evaluated. See 'Demand' for examples.
--
-- The nested 'SubDemand' @d@ of a 'Call' @Cn(d)@ is /relative/ to a single such call.
-- E.g. The expression @f 1 2 + f 3 4@ puts call demand @SCS(C1(L))@ on @f@:
-- @f@ is called exactly twice (@S@), each time exactly once (@1@) with an
-- additional argument.
--
-- The nested 'Demand's @dn@ of a 'Prod' @P(d1,d2,...)@ apply /absolutely/:
-- If @dn@ is a used once demand (cf. 'isUsedOnce'), then that means that
-- the denoted sub-expression is used once in the entire evaluation context
-- described by the surrounding 'Demand'. E.g., @LP(ML)@ means that the
-- field of the denoted expression is used at most once, although the
-- entire expression might be used many times.
--
-- See Note [Call demands are relative]
-- and Note [Demand notation].
data SubDemand
  = Poly !Card
  -- ^ Polymorphic demand, the denoted thing is evaluated arbitrarily deep,
  -- with the specified cardinality at every level.
  -- Expands to 'Call' via 'viewCall' and to 'Prod' via 'viewProd'.
  --
  -- @Poly n@ is semantically equivalent to @Prod [n :* Poly n, ...]@ or
  -- @Call n (Poly n)@. 'mkCall' and 'mkProd' do these rewrites.
  --
  -- In Note [Demand notation]: @L === P(L,L,...)@ and @L === CL(L)@,
  --                            @1 === P(1,1,...)@ and @1 === C1(1)@, and so on.
  --
  -- We only really use 'Poly' with 'C_10' (B), 'C_00' (A), 'C_0N' (L) and
  -- sometimes 'C_1N' (S), but it's simpler to treat it uniformly than to
  -- have a special constructor for each of the three cases.
  | Call !Card !SubDemand
  -- ^ @Call n sd@ describes the evaluation context of @n@ function
  -- applications, where every individual result is evaluated according to @sd@.
  -- @sd@ is /relative/ to a single call, cf. Note [Call demands are relative].
  -- Used only for values of function type. Use the smart constructor 'mkCall'
  -- whenever possible!
  | Prod ![Demand]
  -- ^ @Prod ds@ describes the evaluation context of a case scrutinisation
  -- on an expression of product type, where the product components are
  -- evaluated according to @ds@.
  deriving Eq

poly00, poly01, poly0N, poly11, poly1N, poly10 :: SubDemand
topSubDmd, botSubDmd, seqSubDmd :: SubDemand
poly00 = Poly C_00
poly01 = Poly C_01
poly0N = Poly C_0N
poly11 = Poly C_11
poly1N = Poly C_1N
poly10 = Poly C_10
topSubDmd = poly0N
botSubDmd = poly10
seqSubDmd = poly00

polyDmd :: Card -> Demand
polyDmd C_00 = C_00 :* poly00
polyDmd C_01 = C_01 :* poly01
polyDmd C_0N = C_0N :* poly0N
polyDmd C_11 = C_11 :* poly11
polyDmd C_1N = C_1N :* poly1N
polyDmd C_10 = C_10 :* poly10

-- | A smart constructor for 'Prod', applying rewrite rules along the semantic
-- equality @Prod [polyDmd n, ...] === polyDmd n@, simplifying to 'Poly'
-- 'SubDemand's when possible. Note that this degrades boxity information! E.g. a
-- polymorphic demand will never unbox.
mkProd :: [Demand] -> SubDemand
mkProd [] = seqSubDmd
mkProd ds@(n:*sd : _)
  | want_to_simplify n, all (== polyDmd n) ds = sd
  | otherwise                                 = Prod ds
  where
    -- We only want to simplify absent and bottom demands and unbox the others.
    -- See also Note [L should win] and Note [Don't optimise LP(L,L,...) to L].
    want_to_simplify C_00 = True
    want_to_simplify C_10 = True
    want_to_simplify _    = False

-- | @viewProd n sd@ interprets @sd@ as a 'Prod' of arity @n@, expanding 'Poly'
-- demands as necessary.
viewProd :: Arity -> SubDemand -> Maybe [Demand]
-- It's quite important that this function is optimised well;
-- it is used by lubSubDmd and plusSubDmd. Note the strict
-- application to 'polyDmd':
viewProd n (Prod ds)   | ds `lengthIs` n = Just ds
-- Note the strict application to replicate: This makes sure we don't allocate
-- a thunk for it, inlines it and lets case-of-case fire at call sites.
viewProd n (Poly card)                   = Just $! (replicate n $! polyDmd card)
viewProd _ _                             = Nothing
{-# INLINE viewProd #-} -- we want to fuse away the replicate and the allocation
                        -- for Arity. Otherwise, #18304 bites us.

-- | A smart constructor for 'Call', applying rewrite rules along the semantic
-- equality @Call n (Poly n) === Poly n@, simplifying to 'Poly' 'SubDemand's
-- when possible.
mkCall :: Card -> SubDemand -> SubDemand
mkCall n cd@(Poly m) | n == m = cd
mkCall n cd                   = Call n cd

-- | @viewCall sd@ interprets @sd@ as a 'Call', expanding 'Poly' demands as
-- necessary.
viewCall :: SubDemand -> Maybe (Card, SubDemand)
viewCall (Call n sd)    = Just (n, sd)
viewCall sd@(Poly card) = Just (card, sd)
viewCall _              = Nothing

topDmd, absDmd, botDmd, seqDmd :: Demand
topDmd = polyDmd C_0N
absDmd = polyDmd C_00
botDmd = polyDmd C_10
seqDmd = C_11 :* seqSubDmd

-- | Denotes '∪' on 'SubDemand'.
lubSubDmd :: SubDemand -> SubDemand -> SubDemand
-- Handle Prod
lubSubDmd (Prod ds1) (viewProd (length ds1) -> Just ds2) =
  Prod $ strictZipWith lubDmd ds2 ds1 -- try to fuse with ds2
-- Handle Call
lubSubDmd (Call n1 d1) (viewCall -> Just (n2, d2))
  -- See Note [Call demands are relative]
  | isAbs n1  = mkCall (lubCard n1 n2) (lubSubDmd botSubDmd d2)
  | isAbs n2  = mkCall (lubCard n1 n2) (lubSubDmd d1 botSubDmd)
  | otherwise = mkCall (lubCard n1 n2) (lubSubDmd d1        d2)
-- Handle Poly
lubSubDmd (Poly n1)  (Poly n2) = Poly (lubCard n1 n2)
-- Make use of reflexivity (so we'll match the Prod or Call cases again).
lubSubDmd sd1@Poly{} sd2       = lubSubDmd sd2 sd1
-- Otherwise (Call `lub` Prod) return Top
lubSubDmd _          _         = topSubDmd

-- | Denotes '∪' on 'Demand'.
lubDmd :: Demand -> Demand -> Demand
lubDmd (n1 :* sd1) (n2 :* sd2) = lubCard n1 n2 :* lubSubDmd sd1 sd2

-- | Denotes '+' on 'SubDemand'.
plusSubDmd :: SubDemand -> SubDemand -> SubDemand
-- Handle Prod
plusSubDmd (Prod ds1) (viewProd (length ds1) -> Just ds2) =
  Prod $ zipWith plusDmd ds2 ds1 -- try to fuse with ds2
-- Handle Call
plusSubDmd (Call n1 d1) (viewCall -> Just (n2, d2))
  -- See Note [Call demands are relative]
  | isAbs n1  = mkCall (plusCard n1 n2) (lubSubDmd botSubDmd d2)
  | isAbs n2  = mkCall (plusCard n1 n2) (lubSubDmd d1 botSubDmd)
  | otherwise = mkCall (plusCard n1 n2) (lubSubDmd d1        d2)
-- Handle Poly
plusSubDmd (Poly n1)  (Poly n2) = Poly (plusCard n1 n2)
-- Make use of reflexivity (so we'll match the Prod or Call cases again).
plusSubDmd sd1@Poly{} sd2       = plusSubDmd sd2 sd1
-- Otherwise (Call `lub` Prod) return Top
plusSubDmd _          _         = topSubDmd

-- | Denotes '+' on 'Demand'.
plusDmd :: Demand -> Demand -> Demand
plusDmd (n1 :* sd1) (n2 :* sd2) = plusCard n1 n2 :* plusSubDmd sd1 sd2

-- | The trivial cases of the @mult*@ functions.
-- If @multTrivial n abs a = ma@, we have the following outcomes
-- depending on @n@:
--
--   * 'C_11' => multiply by one, @ma = Just a@
--   * 'C_00', 'C_10' (e.g. @'isAbs' n@) => return the absent thing,
--      @ma = Just abs@
--   * Otherwise ('C_01', 'C_*N') it's not a trivial case, @ma = Nothing@.
multTrivial :: Card -> a -> a -> Maybe a
multTrivial C_11 _   a           = Just a
multTrivial n    abs _ | isAbs n = Just abs
multTrivial _    _   _           = Nothing

multSubDmd :: Card -> SubDemand -> SubDemand
multSubDmd n sd
  | Just sd' <- multTrivial n seqSubDmd sd = sd'
multSubDmd n (Poly n')    = Poly (multCard n n')
multSubDmd n (Call n' sd) = mkCall (multCard n n') sd -- See Note [Call demands are relative]
multSubDmd n (Prod ds)    = Prod (map (multDmd n) ds)

multDmd :: Card -> Demand -> Demand
multDmd n    dmd
  | Just dmd' <- multTrivial n absDmd dmd = dmd'
multDmd n (m :* dmd) = multCard n m :* multSubDmd n dmd

-- | Used to suppress pretty-printing of an uninformative demand
isTopDmd :: Demand -> Bool
isTopDmd dmd = dmd == topDmd

isAbsDmd :: Demand -> Bool
isAbsDmd (n :* _) = isAbs n

-- | Contrast with isStrictUsedDmd. See Note [Strict demands]
isStrictDmd :: Demand -> Bool
isStrictDmd (n :* _) = isStrict n

-- | Not absent and used strictly. See Note [Strict demands]
isStrUsedDmd :: Demand -> Bool
isStrUsedDmd (n :* _) = isStrict n && not (isAbs n)

isSeqDmd :: Demand -> Bool
isSeqDmd (C_11 :* sd) = sd == seqSubDmd
isSeqDmd (C_1N :* sd) = sd == seqSubDmd -- I wonder if we need this case.
isSeqDmd _            = False

-- | Is the value used at most once?
isUsedOnceDmd :: Demand -> Bool
isUsedOnceDmd (n :* _) = isUsedOnce n

-- | We try to avoid tracking weak free variable demands in strictness
-- signatures for analysis performance reasons.
-- See Note [Lazy and unleashable free variables] in "GHC.Core.Opt.DmdAnal".
isWeakDmd :: Demand -> Bool
isWeakDmd dmd@(n :* _) = not (isStrict n) && is_plus_idem_dmd dmd
  where
    -- @is_plus_idem_* thing@ checks whether @thing `plus` thing = thing@,
    -- e.g. if @thing@ is idempotent wrt. to @plus@.
    is_plus_idem_card c = plusCard c c == c
    -- is_plus_idem_dmd dmd = plusDmd dmd dmd == dmd
    is_plus_idem_dmd (n :* sd) = is_plus_idem_card n && is_plus_idem_sub_dmd sd
    -- is_plus_idem_sub_dmd sd = plusSubDmd sd sd == sd
    is_plus_idem_sub_dmd (Poly n)   = is_plus_idem_card n
    is_plus_idem_sub_dmd (Prod ds)  = all is_plus_idem_dmd ds
    is_plus_idem_sub_dmd (Call n _) = is_plus_idem_card n -- See Note [Call demands are relative]

evalDmd :: Demand
evalDmd = C_1N :* topSubDmd

-- | First argument of 'GHC.Exts.maskAsyncExceptions#': @1C1(L)@.
-- Called exactly once.
strictOnceApply1Dmd :: Demand
strictOnceApply1Dmd = C_11 :* mkCall C_11 topSubDmd

-- | First argument of 'GHC.Exts.atomically#': @SCS(L)@.
-- Called at least once, possibly many times.
strictManyApply1Dmd :: Demand
strictManyApply1Dmd = C_1N :* mkCall C_1N topSubDmd

-- | First argument of catch#: @MCM(L)@.
-- Evaluates its arg lazily, but then applies it exactly once to one argument.
lazyApply1Dmd :: Demand
lazyApply1Dmd = C_01 :* mkCall C_01 topSubDmd

-- | Second argument of catch#: @MCM(C1(L))@.
-- Calls its arg lazily, but then applies it exactly once to an additional argument.
lazyApply2Dmd :: Demand
lazyApply2Dmd = C_01 :* mkCall C_01 (mkCall C_11 topSubDmd)

-- | Make a 'Demand' evaluated at-most-once.
oneifyDmd :: Demand -> Demand
oneifyDmd (n :* sd) = oneifyCard n :* sd

-- | Make a 'Demand' evaluated at-least-once (e.g. strict).
strictifyDmd :: Demand -> Demand
strictifyDmd (n :* sd) = plusCard C_10 n :* sd

-- | If the argument is a used non-newtype dictionary, give it strict demand.
-- Also split the product type & demand and recur in order to similarly
-- strictify the argument's contained used non-newtype superclass dictionaries.
-- We use the demand as our recursive measure to guarantee termination.
strictifyDictDmd :: Type -> Demand -> Demand
strictifyDictDmd ty (n :* Prod ds)
  | not (isAbs n)
  , Just field_tys <- as_non_newtype_dict ty
  = C_1N :* -- main idea: ensure it's strict
      if all (not . isAbsDmd) ds
        then topSubDmd -- abstract to strict w/ arbitrary component use,
                         -- since this smells like reboxing; results in CBV
                         -- boxed
                         --
                         -- TODO revisit this if we ever do boxity analysis
        else Prod (zipWith strictifyDictDmd field_tys ds)
  where
    -- | Return a TyCon and a list of field types if the given
    -- type is a non-newtype dictionary type
    as_non_newtype_dict ty
      | Just (tycon, _arg_tys, _data_con, map scaledThing -> inst_con_arg_tys)
          <- splitDataProductType_maybe ty
      , not (isNewTyCon tycon)
      , isClassTyCon tycon
      = Just inst_con_arg_tys
      | otherwise
      = Nothing
strictifyDictDmd _  dmd = dmd

-- | Wraps the 'SubDemand' with a one-shot call demand: @d@ -> @C1(d)@.
mkCalledOnceDmd :: SubDemand -> SubDemand
mkCalledOnceDmd sd = mkCall C_11 sd

-- | @mkCalledOnceDmds n d@ returns @C1(C1...(C1 d))@ where there are @n@ @C1@'s.
mkCalledOnceDmds :: Arity -> SubDemand -> SubDemand
mkCalledOnceDmds arity sd = iterate mkCalledOnceDmd sd !! arity

-- | Peels one call level from the sub-demand, and also returns how many
-- times we entered the lambda body.
peelCallDmd :: SubDemand -> (Card, SubDemand)
peelCallDmd sd = viewCall sd `orElse` (topCard, topSubDmd)

-- Peels multiple nestings of 'Call' sub-demands and also returns
-- whether it was unsaturated in the form of a 'Card'inality, denoting
-- how many times the lambda body was entered.
-- See Note [Demands from unsaturated function calls].
peelManyCalls :: Int -> SubDemand -> Card
peelManyCalls 0 _                          = C_11
-- See Note [Call demands are relative]
peelManyCalls n (viewCall -> Just (m, sd)) = m `multCard` peelManyCalls (n-1) sd
peelManyCalls _ _                          = C_0N

-- See Note [Demand on the worker] in GHC.Core.Opt.WorkWrap
mkWorkerDemand :: Int -> Demand
mkWorkerDemand n = C_01 :* go n
  where go 0 = topSubDmd
        go n = Call C_01 $ go (n-1)

addCaseBndrDmd :: SubDemand -- On the case binder
               -> [Demand]  -- On the components of the constructor
               -> [Demand]  -- Final demands for the components of the constructor
addCaseBndrDmd (Poly n) alt_dmds
  | isAbs n   = alt_dmds
-- See Note [Demand on case-alternative binders]
addCaseBndrDmd sd       alt_dmds = zipWith plusDmd ds alt_dmds -- fuse ds!
  where
    Just ds = viewProd (length alt_dmds) sd -- Guaranteed not to be a call

argsOneShots :: StrictSig -> Arity -> [[OneShotInfo]]
-- ^ See Note [Computing one-shot info]
argsOneShots (StrictSig (DmdType _ arg_ds _)) n_val_args
  | unsaturated_call = []
  | otherwise = go arg_ds
  where
    unsaturated_call = arg_ds `lengthExceeds` n_val_args

    go []               = []
    go (arg_d : arg_ds) = argOneShots arg_d `cons` go arg_ds

    -- Avoid list tail like [ [], [], [] ]
    cons [] [] = []
    cons a  as = a:as

argOneShots :: Demand          -- ^ depending on saturation
            -> [OneShotInfo]
-- ^ See Note [Computing one-shot info]
argOneShots (_ :* sd) = go sd -- See Note [Call demands are relative]
  where
    go (Call n sd)
      | isUsedOnce n = OneShotLam    : go sd
      | otherwise    = NoOneShotInfo : go sd
    go _    = []

-- |
-- @saturatedByOneShots n CM(CM(...)) = True@
--   <=>
-- There are at least n nested CM(..) calls.
-- See Note [Demand on the worker] in GHC.Core.Opt.WorkWrap
saturatedByOneShots :: Int -> Demand -> Bool
saturatedByOneShots n (_ :* sd) = isUsedOnce (peelManyCalls n sd)

{- Note [Strict demands]
~~~~~~~~~~~~~~~~~~~~~~~~
'isStrUsedDmd' returns true only of demands that are
   both strict
   and  used

In particular, it is False for <B> (i.e. strict and not used,
cardinality C_10), which can and does arise in, say (#7319)
   f x = raise# <some exception>
Then 'x' is not used, so f gets strictness <B> -> .
Now the w/w generates
   fx = let x <B> = absentError "unused"
        in raise <some exception>
At this point we really don't want to convert to
   fx = case absentError "unused" of x -> raise <some exception>
Since the program is going to diverge, this swaps one error for another,
but it's really a bad idea to *ever* evaluate an absent argument.
In #7319 we get
   T7319.exe: Oops!  Entered absent arg w_s1Hd{v} [lid] [base:GHC.Base.String{tc 36u}]

Note [Call demands are relative]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The expression @if b then 0 else f 1 2 + f 3 4@ uses @f@ according to the demand
@LCL(C1(P(L)))@, meaning

  "f is called multiple times or not at all (CL), but each time it
   is called, it's called with *exactly one* (C1) more argument.
   Whenever it is called with two arguments, we have no info on how often
   the field of the product result is used (L)."

So the 'SubDemand' nested in a 'Call' demand is relative to exactly one call.
And that extends to the information we have how its results are used in each
call site. Consider (#18903)

  h :: Int -> Int
  h m =
    let g :: Int -> (Int,Int)
        g 1 = (m, 0)
        g n = (2 * n, 2 `div` n)
        {-# NOINLINE g #-}
    in case m of
      1 -> 0
      2 -> snd (g m)
      _ -> uncurry (+) (g m)

We want to give @g@ the demand @MCM(P(MP(L),1P(L)))@, so we see that in each call
site of @g@, we are strict in the second component of the returned pair.

This relative cardinality leads to an otherwise unexpected call to 'lubSubDmd'
in 'plusSubDmd', but if you do the math it's just the right thing.

There's one more subtlety: Since the nested demand is relative to exactly one
call, in the case where we have *at most zero calls* (e.g. CA(...)), the premise
is hurt and we can assume that the nested demand is 'botSubDmd'. That ensures
that @g@ above actually gets the @1P(L)@ demand on its second pair component,
rather than the lazy @MP(L)@ if we 'lub'bed with an absent demand.

Demand on case-alternative binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The demand on a binder in a case alternative comes
  (a) From the demand on the binder itself
  (b) From the demand on the case binder
Forgetting (b) led directly to #10148.

Example. Source code:
  f x@(p,_) = if p then foo x else True

  foo (p,True) = True
  foo (p,q)    = foo (q,p)

After strictness analysis:
  f = \ (x_an1 [Dmd=1P(1L,ML)] :: (Bool, Bool)) ->
      case x_an1
      of wild_X7 [Dmd=MP(ML,ML)]
      { (p_an2 [Dmd=1L], ds_dnz [Dmd=A]) ->
      case p_an2 of _ {
        False -> GHC.Types.True;
        True -> foo wild_X7 }

It's true that ds_dnz is *itself* absent, but the use of wild_X7 means
that it is very much alive and demanded.  See #10148 for how the
consequences play out.

This is needed even for non-product types, in case the case-binder
is used but the components of the case alternative are not.

Note [Don't optimise LP(L,L,...) to L]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These two SubDemands:
   LP(L,L) (@Prod [topDmd, topDmd]@)   and   L (@topSubDmd@)
are semantically equivalent, but we do not turn the former into
the latter, for a regrettable-subtle reason.  Consider
  f p1@(x,y) = (y,x)
  g h p2@(_,_) = h p
We want to unbox @p1@ of @f@, but not @p2@ of @g@, because @g@ only uses
@p2@ boxed and we'd have to rebox. So we give @p1@ demand LP(L,L) and @p2@
demand @L@ to inform 'GHC.Core.Opt.WorkWrap.Utils.wantToUnbox', which will
say "unbox" for @p1@ and "don't unbox" for @p2@.

So the solution is: don't aggressively collapse @Prod [topDmd, topDmd]@ to
@topSubDmd@; instead leave it as-is. In effect we are using the UseDmd to do a
little bit of boxity analysis.  Not very nice.

Note [L should win]
~~~~~~~~~~~~~~~~~~~
Both in 'lubSubDmd' and 'plusSubDmd' we want @L `plusSubDmd` LP(..))@ to be @L@.
Why?  Because U carries the implication the whole thing is used, box and all,
so we don't want to w/w it, cf. Note [Don't optimise LP(L,L,...) to L].
If we use it both boxed and unboxed, then we are definitely using the box,
and so we are quite likely to pay a reboxing cost. So we make U win here.
TODO: Investigate why since 2013, we don't.

Example is in the Buffer argument of GHC.IO.Handle.Internals.writeCharBuffer

Baseline: (A) Not making Used win (LP(..) wins)
Compare with: (B) making Used win for lub and both

            Min          -0.3%     -5.6%    -10.7%    -11.0%    -33.3%
            Max          +0.3%    +45.6%    +11.5%    +11.5%     +6.9%
 Geometric Mean          -0.0%     +0.5%     +0.3%     +0.2%     -0.8%

Baseline: (B) Making L win for both lub and both
Compare with: (C) making L win for plus, but LP(..) win for lub

            Min          -0.1%     -0.3%     -7.9%     -8.0%     -6.5%
            Max          +0.1%     +1.0%    +21.0%    +21.0%     +0.5%
 Geometric Mean          +0.0%     +0.0%     -0.0%     -0.1%     -0.1%

Note [Computing one-shot info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a call
    f (\pqr. e1) (\xyz. e2) e3
where f has usage signature
    <CM(CL(CM(L)))><CM(L)><L>
Then argsOneShots returns a [[OneShotInfo]] of
    [[OneShot,NoOneShotInfo,OneShot],  [OneShot]]
The occurrence analyser propagates this one-shot infor to the
binders \pqr and \xyz;
see Note [Use one-shot information] in "GHC.Core.Opt.OccurAnal".
-}

{- *********************************************************************
*                                                                      *
                 Divergence: Whether evaluation surely diverges
*                                                                      *
********************************************************************* -}

-- | 'Divergence' characterises whether something surely diverges.
-- Models a subset lattice of the following exhaustive set of divergence
-- results:
--
-- [n] nontermination (e.g. loops)
-- [i] throws imprecise exception
-- [p] throws precise exceTtion
-- [c] converges (reduces to WHNF).
--
-- The different lattice elements correspond to different subsets, indicated by
-- juxtaposition of indicators (e.g. __nc__ definitely doesn't throw an
-- exception, and may or may not reduce to WHNF).
--
-- @
--             Dunno (nipc)
--                  |
--            ExnOrDiv (nip)
--                  |
--            Diverges (ni)
-- @
--
-- As you can see, we don't distinguish __n__ and __i__.
-- See Note [Precise exceptions and strictness analysis] for why __p__ is so
-- special compared to __i__.
data Divergence
  = Diverges -- ^ Definitely throws an imprecise exception or diverges.
  | ExnOrDiv -- ^ Definitely throws a *precise* exception, an imprecise
             --   exception or diverges. Never converges, hence 'isDeadEndDiv'!
             --   See scenario 1 in Note [Precise exceptions and strictness analysis].
  | Dunno    -- ^ Might diverge, throw any kind of exception or converge.
  deriving Eq

lubDivergence :: Divergence -> Divergence -> Divergence
lubDivergence Diverges div      = div
lubDivergence div      Diverges = div
lubDivergence ExnOrDiv ExnOrDiv = ExnOrDiv
lubDivergence _        _        = Dunno
-- This needs to commute with defaultFvDmd, i.e.
-- defaultFvDmd (r1 `lubDivergence` r2) = defaultFvDmd r1 `lubDmd` defaultFvDmd r2
-- (See Note [Default demand on free variables and arguments] for why)

-- | See Note [Asymmetry of 'plus*'], which concludes that 'plusDivergence'
-- needs to be symmetric.
-- Strictly speaking, we should have @plusDivergence Dunno Diverges = ExnOrDiv@.
-- But that regresses in too many places (every infinite loop, basically) to be
-- worth it and is only relevant in higher-order scenarios
-- (e.g. Divergence of @f (throwIO blah)@).
-- So 'plusDivergence' currently is 'glbDivergence', really.
plusDivergence :: Divergence -> Divergence -> Divergence
plusDivergence Dunno    Dunno    = Dunno
plusDivergence Diverges _        = Diverges
plusDivergence _        Diverges = Diverges
plusDivergence _        _        = ExnOrDiv

-- | In a non-strict scenario, we might not force the Divergence, in which case
-- we might converge, hence Dunno.
multDivergence :: Card -> Divergence -> Divergence
multDivergence n _ | not (isStrict n) = Dunno
multDivergence _ d                    = d

topDiv, exnDiv, botDiv :: Divergence
topDiv = Dunno
exnDiv = ExnOrDiv
botDiv = Diverges

-- | True if the 'Divergence' indicates that evaluation will not return.
-- See Note [Dead ends].
isDeadEndDiv :: Divergence -> Bool
isDeadEndDiv Diverges = True
isDeadEndDiv ExnOrDiv = True
isDeadEndDiv Dunno    = False

-- See Notes [Default demand on free variables and arguments]
-- and Scenario 1 in [Precise exceptions and strictness analysis]
defaultFvDmd :: Divergence -> Demand
defaultFvDmd Dunno    = absDmd
defaultFvDmd ExnOrDiv = absDmd -- This is the whole point of ExnOrDiv!
defaultFvDmd Diverges = botDmd -- Diverges

defaultArgDmd :: Divergence -> Demand
-- TopRes and BotRes are polymorphic, so that
--      BotRes === (Bot -> BotRes) === ...
--      TopRes === (Top -> TopRes) === ...
-- This function makes that concrete
-- Also see Note [Default demand on free variables and arguments]
defaultArgDmd Dunno    = topDmd
-- NB: not botDmd! We don't want to mask the precise exception by forcing the
-- argument. But it is still absent.
defaultArgDmd ExnOrDiv = absDmd
defaultArgDmd Diverges = botDmd

{- Note [Precise vs imprecise exceptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An exception is considered to be /precise/ when it is thrown by the 'raiseIO#'
primop. It follows that all other primops (such as 'raise#' or
division-by-zero) throw /imprecise/ exceptions. Note that the actual type of
the exception thrown doesn't have any impact!

GHC undertakes some effort not to apply an optimisation that would mask a
/precise/ exception with some other source of nontermination, such as genuine
divergence or an imprecise exception, so that the user can reliably
intercept the precise exception with a catch handler before and after
optimisations.

See also the wiki page on precise exceptions:
https://gitlab.haskell.org/ghc/ghc/wikis/exceptions/precise-exceptions
Section 5 of "Tackling the awkward squad" talks about semantic concerns.
Imprecise exceptions are actually more interesting than precise ones (which are
fairly standard) from the perspective of semantics. See the paper "A Semantics
for Imprecise Exceptions" for more details.

Note [Dead ends]
~~~~~~~~~~~~~~~~
We call an expression that either diverges or throws a precise or imprecise
exception a "dead end". We used to call such an expression just "bottoming",
but with the measures we take to preserve precise exception semantics
(see Note [Precise exceptions and strictness analysis]), that is no longer
accurate: 'exnDiv' is no longer the bottom of the Divergence lattice.

Yet externally to demand analysis, we mostly care about being able to drop dead
code etc., which is all due to the property that such an expression never
returns, hence we consider throwing a precise exception to be a dead end.
See also 'isDeadEndDiv'.

Note [Precise exceptions and strictness analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have to take care to preserve precise exception semantics in strictness
analysis (#17676). There are two scenarios that need careful treatment.

The fixes were discussed at
https://gitlab.haskell.org/ghc/ghc/wikis/fixing-precise-exceptions

Recall that raiseIO# raises a *precise* exception, in contrast to raise# which
raises an *imprecise* exception. See Note [Precise vs imprecise exceptions].

Scenario 1: Precise exceptions in case alternatives
---------------------------------------------------
Unlike raise# (which returns botDiv), we want raiseIO# to return exnDiv.
Here's why. Consider this example from #13380 (similarly #17676):
  f x y | x>0       = raiseIO# Exc
        | y>0       = return 1
        | otherwise = return 2
Is 'f' strict in 'y'? One might be tempted to say yes! But that plays fast and
loose with the precise exception; after optimisation, (f 42 (error "boom"))
turns from throwing the precise Exc to throwing the imprecise user error
"boom". So, the defaultFvDmd of raiseIO# should be lazy (topDmd), which can be
achieved by giving it divergence exnDiv.
See Note [Default demand on free variables and arguments].

Why don't we just give it topDiv instead of introducing exnDiv?
Because then the simplifier will fail to discard raiseIO#'s continuation in
  case raiseIO# x s of { (# s', r #) -> <BIG> }
which we'd like to optimise to
  case raiseIO# x s of {}
Hence we came up with exnDiv. The default FV demand of exnDiv is lazy (and
its default arg dmd is absent), but otherwise (in terms of 'isDeadEndDiv') it
behaves exactly as botDiv, so that dead code elimination works as expected.
This is tracked by T13380b.

Scenario 2: Precise exceptions in case scrutinees
-------------------------------------------------
Consider (more complete examples in #148, #1592, testcase strun003)

  case foo x s of { (# s', r #) -> y }

Is this strict in 'y'? Often not! If @foo x s@ might throw a precise exception
(ultimately via raiseIO#), then we must not force 'y', which may fail to
terminate or throw an imprecise exception, until we have performed @foo x s@.

So we have to 'deferAfterPreciseException' (which 'lub's with 'exnDmdType' to
model the exceptional control flow) when @foo x s@ may throw a precise
exception. Motivated by T13380{d,e,f}.
See Note [Which scrutinees may throw precise exceptions] in "GHC.Core.Opt.DmdAnal".

We have to be careful not to discard dead-end Divergence from case
alternatives, though (#18086):

  m = putStrLn "foo" >> error "bar"

'm' should still have 'exnDiv', which is why it is not sufficient to lub with
'nopDmdType' (which has 'topDiv') in 'deferAfterPreciseException'.

Historical Note: This used to be called the "IO hack". But that term is rather
a bad fit because
1. It's easily confused with the "State hack", which also affects IO.
2. Neither "IO" nor "hack" is a good description of what goes on here, which
   is deferring strictness results after possibly throwing a precise exception.
   The "hack" is probably not having to defer when we can prove that the
   expression may not throw a precise exception (increasing precision of the
   analysis), but that's just a favourable guess.

Note [Exceptions and strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to smart about catching exceptions, but we aren't anymore.
See #14998 for the way it's resolved at the moment.

Here's a historic breakdown:

Apparently, exception handling prim-ops didn't use to have any special
strictness signatures, thus defaulting to nopSig, which assumes they use their
arguments lazily. Joachim was the first to realise that we could provide richer
information. Thus, in 0558911f91c (Dec 13), he added signatures to
primops.txt.pp indicating that functions like `catch#` and `catchRetry#` call
their argument, which is useful information for usage analysis. Still with a
'Lazy' strictness demand (i.e. 'lazyApply1Dmd'), though, and the world was fine.

In 7c0fff4 (July 15), Simon argued that giving `catch#` et al. a
'strictApply1Dmd' leads to substantial performance gains. That was at the cost
of correctness, as #10712 proved. So, back to 'lazyApply1Dmd' in
28638dfe79e (Dec 15).

Motivated to reproduce the gains of 7c0fff4 without the breakage of #10712,
Ben opened #11222. Simon made the demand analyser "understand catch" in
9915b656 (Jan 16) by adding a new 'catchArgDmd', which basically said to call
its argument strictly, but also swallow any thrown exceptions in
'multDivergence'. This was realized by extending the 'Str' constructor of
'ArgStr' with a 'ExnStr' field, indicating that it catches the exception, and
adding a 'ThrowsExn' constructor to the 'Divergence' lattice as an element
between 'Dunno' and 'Diverges'. Then along came #11555 and finally #13330,
so we had to revert to 'lazyApply1Dmd' again in 701256df88c (Mar 17).

This left the other variants like 'catchRetry#' having 'catchArgDmd', which is
where #14998 picked up. Item 1 was concerned with measuring the impact of also
making `catchRetry#` and `catchSTM#` have 'lazyApply1Dmd'. The result was that
there was none. We removed the last usages of 'catchArgDmd' in 00b8ecb7
(Apr 18). There was a lot of dead code resulting from that change, that we
removed in ef6b283 (Jan 19): We got rid of 'ThrowsExn' and 'ExnStr' again and
removed any code that was dealing with the peculiarities.

Where did the speed-ups vanish to? In #14998, item 3 established that
turning 'catch#' strict in its first argument didn't bring back any of the
alleged performance benefits. Item 2 of that ticket finally found out that it
was entirely due to 'catchException's new (since #11555) definition, which
was simply

    catchException !io handler = catch io handler

While 'catchException' is arguably the saner semantics for 'catch', it is an
internal helper function in "GHC.IO". Its use in
"GHC.IO.Handle.Internals.do_operation" made for the huge allocation differences:
Remove the bang and you find the regressions we originally wanted to avoid with
'catchArgDmd'. See also #exceptions_and_strictness# in "GHC.IO".

So history keeps telling us that the only possibly correct strictness annotation
for the first argument of 'catch#' is 'lazyApply1Dmd', because 'catch#' really
is not strict in its argument: Just try this in GHCi

  :set -XScopedTypeVariables
  import Control.Exception
  catch undefined (\(_ :: SomeException) -> putStrLn "you'll see this")

Any analysis that assumes otherwise will be broken in some way or another
(beyond `-fno-pendantic-bottoms`).

But then #13380 and #17676 suggest (in Mar 20) that we need to re-introduce a
subtly different variant of `ThrowsExn` (which we call `ExnOrDiv` now) that is
only used by `raiseIO#` in order to preserve precise exceptions by strictness
analysis, while not impacting the ability to eliminate dead code.
See Note [Precise exceptions and strictness analysis].

Note [Default demand on free variables and arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Free variables not mentioned in the environment of a 'DmdType'
are demanded according to the demand type's Divergence:
  * In a Diverges (botDiv) context, that demand is botDmd
    (strict and absent).
  * In all other contexts, the demand is absDmd (lazy and absent).
This is recorded in 'defaultFvDmd'.

Similarly, we can eta-expand demand types to get demands on excess arguments
not accounted for in the type, by consulting 'defaultArgDmd':
  * In a Diverges (botDiv) context, that demand is again botDmd.
  * In a ExnOrDiv (exnDiv) context, that demand is absDmd: We surely diverge
    before evaluating the excess argument, but don't want to eagerly evaluate
    it (cf. Note [Precise exceptions and strictness analysis]).
  * In a Dunno context (topDiv), the demand is topDmd, because
    it's perfectly possible to enter the additional lambda and evaluate it
    in unforeseen ways (so, not absent).

Note [Bottom CPR iff Dead-Ending Divergence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Both CPR analysis and Demand analysis handle recursive functions by doing
fixed-point iteration. To find the *least* (e.g., most informative) fixed-point,
iteration starts with the bottom element of the semantic domain. Diverging
functions generally have the bottom element as their least fixed-point.

One might think that CPR analysis and Demand analysis then agree in when a
function gets a bottom denotation. E.g., whenever it has 'botCpr', it should
also have 'botDiv'. But that is not the case, because strictness analysis has to
be careful around precise exceptions, see Note [Precise vs imprecise exceptions].

So Demand analysis gives some diverging functions 'exnDiv' (which is *not* the
bottom element) when the CPR signature says 'botCpr', and that's OK. Here's an
example (from #18086) where that is the case:

ioTest :: IO ()
ioTest = do
  putStrLn "hi"
  undefined

However, one can loosely say that we give a function 'botCpr' whenever its
'Divergence' is 'exnDiv' or 'botDiv', i.e., dead-ending. But that's just
a consequence of fixed-point iteration, it's not important that they agree.

************************************************************************
*                                                                      *
           Demand environments and types
*                                                                      *
************************************************************************
-}

-- Subject to Note [Default demand on free variables and arguments]
type DmdEnv = VarEnv Demand

emptyDmdEnv :: DmdEnv
emptyDmdEnv = emptyVarEnv

multDmdEnv :: Card -> DmdEnv -> DmdEnv
multDmdEnv n env
  | Just env' <- multTrivial n emptyDmdEnv env = env'
  | otherwise                                  = mapVarEnv (multDmd n) env

reuseEnv :: DmdEnv -> DmdEnv
reuseEnv = multDmdEnv C_1N

-- | @keepAliveDmdType dt vs@ makes sure that the Ids in @vs@ have
-- /some/ usage in the returned demand types -- they are not Absent.
-- See Note [Absence analysis for stable unfoldings and RULES]
--     in "GHC.Core.Opt.DmdAnal".
keepAliveDmdEnv :: DmdEnv -> IdSet -> DmdEnv
keepAliveDmdEnv env vs
  = nonDetStrictFoldVarSet add env vs
  where
    add :: Id -> DmdEnv -> DmdEnv
    add v env = extendVarEnv_C add_dmd env v topDmd

    add_dmd :: Demand -> Demand -> Demand
    -- If the existing usage is Absent, make it used
    -- Otherwise leave it alone
    add_dmd dmd _ | isAbsDmd dmd = topDmd
                  | otherwise    = dmd

-- | Characterises how an expression
--    * Evaluates its free variables ('dt_env')
--    * Evaluates its arguments ('dt_args')
--    * Diverges on every code path or not ('dt_div')
data DmdType
  = DmdType
  { dt_env  :: !DmdEnv     -- ^ Demand on explicitly-mentioned free variables
  , dt_args :: ![Demand]   -- ^ Demand on arguments
  , dt_div  :: !Divergence -- ^ Whether evaluation diverges.
                          -- See Note [Demand type Divergence]
  }

instance Eq DmdType where
  (==) (DmdType fv1 ds1 div1)
       (DmdType fv2 ds2 div2) = nonDetUFMToList fv1 == nonDetUFMToList fv2
         -- It's OK to use nonDetUFMToList here because we're testing for
         -- equality and even though the lists will be in some arbitrary
         -- Unique order, it is the same order for both
                              && ds1 == ds2 && div1 == div2

-- | Compute the least upper bound of two 'DmdType's elicited /by the same
-- incoming demand/!
lubDmdType :: DmdType -> DmdType -> DmdType
lubDmdType d1 d2
  = DmdType lub_fv lub_ds lub_div
  where
    n = max (dmdTypeDepth d1) (dmdTypeDepth d2)
    (DmdType fv1 ds1 r1) = etaExpandDmdType n d1
    (DmdType fv2 ds2 r2) = etaExpandDmdType n d2

    lub_fv  = plusVarEnv_CD lubDmd fv1 (defaultFvDmd r1) fv2 (defaultFvDmd r2)
    lub_ds  = zipWithEqual "lubDmdType" lubDmd ds1 ds2
    lub_div = lubDivergence r1 r2

type PlusDmdArg = (DmdEnv, Divergence)

mkPlusDmdArg :: DmdEnv -> PlusDmdArg
mkPlusDmdArg env = (env, topDiv)

toPlusDmdArg :: DmdType -> PlusDmdArg
toPlusDmdArg (DmdType fv _ r) = (fv, r)

plusDmdType :: DmdType -> PlusDmdArg -> DmdType
plusDmdType (DmdType fv1 ds1 r1) (fv2, t2)
    -- See Note [Asymmetry of 'plus*']
    -- 'plus' takes the argument/result info from its *first* arg,
    -- using its second arg just for its free-var info.
  = DmdType (plusVarEnv_CD plusDmd fv1 (defaultFvDmd r1) fv2 (defaultFvDmd t2))
            ds1
            (r1 `plusDivergence` t2)

botDmdType :: DmdType
botDmdType = DmdType emptyDmdEnv [] botDiv

-- | The demand type of doing nothing (lazy, absent, no Divergence
-- information). Note that it is ''not'' the top of the lattice (which would be
-- "may use everything"), so it is (no longer) called topDmdType.
nopDmdType :: DmdType
nopDmdType = DmdType emptyDmdEnv [] topDiv

isTopDmdType :: DmdType -> Bool
isTopDmdType (DmdType env args div)
  = div == topDiv && null args && isEmptyVarEnv env

-- | The demand type of an unspecified expression that is guaranteed to
-- throw a (precise or imprecise) exception or diverge.
exnDmdType :: DmdType
exnDmdType = DmdType emptyDmdEnv [] exnDiv

dmdTypeDepth :: DmdType -> Arity
dmdTypeDepth = length . dt_args

-- | This makes sure we can use the demand type with n arguments after eta
-- expansion, where n must not be lower than the demand types depth.
-- It appends the argument list with the correct 'defaultArgDmd'.
etaExpandDmdType :: Arity -> DmdType -> DmdType
etaExpandDmdType n d@DmdType{dt_args = ds, dt_div = div}
  | n == depth = d
  | n >  depth = d{dt_args = inc_ds}
  | otherwise  = pprPanic "etaExpandDmdType: arity decrease" (ppr n $$ ppr d)
  where depth = length ds
        -- Arity increase:
        --  * Demands on FVs are still valid
        --  * Demands on args also valid, plus we can extend with defaultArgDmd
        --    as appropriate for the given Divergence
        --  * Divergence is still valid:
        --    - A dead end after 2 arguments stays a dead end after 3 arguments
        --    - The remaining case is Dunno, which is already topDiv
        inc_ds = take n (ds ++ repeat (defaultArgDmd div))

-- | A conservative approximation for a given 'DmdType' in case of an arity
-- decrease. Currently, it's just nopDmdType.
decreaseArityDmdType :: DmdType -> DmdType
decreaseArityDmdType _ = nopDmdType

splitDmdTy :: DmdType -> (Demand, DmdType)
-- Split off one function argument
-- We already have a suitable demand on all
-- free vars, so no need to add more!
splitDmdTy ty@DmdType{dt_args=dmd:args} = (dmd, ty{dt_args=args})
splitDmdTy ty@DmdType{dt_div=div}       = (defaultArgDmd div, ty)

multDmdType :: Card -> DmdType -> DmdType
multDmdType n (DmdType fv args res_ty)
  = -- pprTrace "multDmdType" (ppr n $$ ppr fv $$ ppr (multDmdEnv n fv)) $
    DmdType (multDmdEnv n fv)
            (map (multDmd n) args)
            (multDivergence n res_ty)

peelFV :: DmdType -> Var -> (DmdType, Demand)
peelFV (DmdType fv ds res) id = -- pprTrace "rfv" (ppr id <+> ppr dmd $$ ppr fv)
                               (DmdType fv' ds res, dmd)
  where
  -- Force these arguments so that old `Env` is not retained.
  !fv' = fv `delVarEnv` id
  -- See Note [Default demand on free variables and arguments]
  !dmd  = lookupVarEnv fv id `orElse` defaultFvDmd res

addDemand :: Demand -> DmdType -> DmdType
addDemand dmd (DmdType fv ds res) = DmdType fv (dmd:ds) res

findIdDemand :: DmdType -> Var -> Demand
findIdDemand (DmdType fv _ res) id
  = lookupVarEnv fv id `orElse` defaultFvDmd res

-- | When e is evaluated after executing an IO action that may throw a precise
-- exception, we act as if there is an additional control flow path that is
-- taken if e throws a precise exception. The demand type of this control flow
-- path
--   * is lazy and absent ('topDmd') in all free variables and arguments
--   * has 'exnDiv' 'Divergence' result
-- So we can simply take a variant of 'nopDmdType', 'exnDmdType'.
-- Why not 'nopDmdType'? Because then the result of 'e' can never be 'exnDiv'!
-- That means failure to drop dead-ends, see #18086.
-- See Note [Precise exceptions and strictness analysis]
deferAfterPreciseException :: DmdType -> DmdType
deferAfterPreciseException = lubDmdType exnDmdType

-- | See 'keepAliveDmdEnv'.
keepAliveDmdType :: DmdType -> VarSet -> DmdType
keepAliveDmdType (DmdType fvs ds res) vars =
  DmdType (fvs `keepAliveDmdEnv` vars) ds res

{-
Note [Demand type Divergence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In contrast to StrictSigs, DmdTypes are elicited under a specific incoming demand.
This is described in detail in Note [Understanding DmdType and StrictSig].
Here, we'll focus on what that means for a DmdType's Divergence in a higher-order
scenario.

Consider
  err x y = x `seq` y `seq` error (show x)
this has a strictness signature of
  <1L><1L>b
meaning that we don't know what happens when we call err in weaker contexts than
C1(C1(L)), like @err `seq` ()@ (1A) and @err 1 `seq` ()@ (CS(A)). We
may not unleash the botDiv, hence assume topDiv. Of course, in
@err 1 2 `seq` ()@ the incoming demand CS(CS(A)) is strong enough and we see
that the expression diverges.

Now consider a function
  f g = g 1 2
with signature <C1(C1(L))>, and the expression
  f err `seq` ()
now f puts a strictness demand of C1(C1(L)) onto its argument, which is unleashed
on err via the App rule. In contrast to weaker head strictness, this demand is
strong enough to unleash err's signature and hence we see that the whole
expression diverges!

Note [Asymmetry of 'plus*']
~~~~~~~~~~~~~~~~~~~~~~~~~~~
'plus' for DmdTypes is *asymmetrical*, because there can only one
be one type contributing argument demands!  For example, given (e1 e2), we get
a DmdType dt1 for e1, use its arg demand to analyse e2 giving dt2, and then do
(dt1 `plusType` dt2). Similarly with
  case e of { p -> rhs }
we get dt_scrut from the scrutinee and dt_rhs from the RHS, and then
compute (dt_rhs `plusType` dt_scrut).

We
 1. combine the information on the free variables,
 2. take the demand on arguments from the first argument
 3. combine the termination results, as in plusDivergence.

Since we don't use argument demands of the second argument anyway, 'plus's
second argument is just a 'PlusDmdType'.

But note that the argument demand types are not guaranteed to be observed in
left to right order. For example, analysis of a case expression will pass the
demand type for the alts as the left argument and the type for the scrutinee as
the right argument. Also, it is not at all clear if there is such an order;
consider the LetUp case, where the RHS might be forced at any point while
evaluating the let body.
Therefore, it is crucial that 'plusDivergence' is symmetric!

Note [Demands from unsaturated function calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a demand transformer d1 -> d2 -> r for f.
If a sufficiently detailed demand is fed into this transformer,
e.g <C1(C1(L))> arising from "f x1 x2" in a strict, use-once context,
then d1 and d2 is precisely the demand unleashed onto x1 and x2 (similar for
the free variable environment) and furthermore the result information r is the
one we want to use.

An anonymous lambda is also an unsaturated function all (needs one argument,
none given), so this applies to that case as well.

But the demand fed into f might be less than C1(C1(L)). Then we have to
'multDmdType' the announced demand type. Examples:
 * Not strict enough, e.g. C1(C1(L)):
   - We have to multiply all argument and free variable demands with C_01,
     zapping strictness.
   - We have to multiply divergence with C_01. If r says that f Diverges for sure,
     then this holds when the demand guarantees that two arguments are going to
     be passed. If the demand is lower, we may just as well converge.
     If we were tracking definite convergence, than that would still hold under
     a weaker demand than expected by the demand transformer.
 * Used more than once, e.g. CS(C1(L)):
   - Multiply with C_1N. Even if f puts a used-once demand on any of its argument
     or free variables, if we call f multiple times, we may evaluate this
     argument or free variable multiple times.

In dmdTransformSig, we call peelManyCalls to find out the 'Card'inality with
which we have to multiply and then call multDmdType with that.

Similarly, dmdTransformDictSelSig and dmdAnal, when analyzing a Lambda, use
peelCallDmd, which peels only one level, but also returns the demand put on the
body of the function.
-}


{-
************************************************************************
*                                                                      *
                     Demand signatures
*                                                                      *
************************************************************************

In a let-bound Id we record its demand signature.
In principle, this demand signature is a demand transformer, mapping
a demand on the Id into a DmdType, which gives
        a) the free vars of the Id's value
        b) the Id's arguments
        c) an indication of the result of applying
           the Id to its arguments

However, in fact we store in the Id an extremely emascuated demand
transfomer, namely

                a single DmdType
(Nevertheless we dignify StrictSig as a distinct type.)

This DmdType gives the demands unleashed by the Id when it is applied
to as many arguments as are given in by the arg demands in the DmdType.
Also see Note [Demand type Divergence] for the meaning of a Divergence in a
strictness signature.

If an Id is applied to less arguments than its arity, it means that
the demand on the function at a call site is weaker than the vanilla
call demand, used for signature inference. Therefore we place a top
demand on all arguments. Otherwise, the demand is specified by Id's
signature.

For example, the demand transformer described by the demand signature
        StrictSig (DmdType {x -> <1L>} <A><1P(L,L)>)
says that when the function is applied to two arguments, it
unleashes demand 1L on the free var x, A on the first arg,
and 1P(L,L) on the second.

If this same function is applied to one arg, all we can say is that it
uses x with 1L, and its arg with demand 1P(L,L).

Note [Understanding DmdType and StrictSig]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Demand types are sound approximations of an expression's semantics relative to
the incoming demand we put the expression under. Consider the following
expression:

    \x y -> x `seq` (y, 2*x)

Here is a table with demand types resulting from different incoming demands we
put that expression under. Note the monotonicity; a stronger incoming demand
yields a more precise demand type:

    incoming demand   |  demand type
    --------------------------------
    1A                  |  <L><L>{}
    C1(C1(L))           |  <1P(L)><L>{}
    C1(C1(1P(1P(L),A))) |  <1P(A)><A>{}

Note that in the first example, the depth of the demand type was *higher* than
the arity of the incoming call demand due to the anonymous lambda.
The converse is also possible and happens when we unleash demand signatures.
In @f x y@, the incoming call demand on f has arity 2. But if all we have is a
demand signature with depth 1 for @f@ (which we can safely unleash, see below),
the demand type of @f@ under a call demand of arity 2 has a *lower* depth of 1.

So: Demand types are elicited by putting an expression under an incoming (call)
demand, the arity of which can be lower or higher than the depth of the
resulting demand type.
In contrast, a demand signature summarises a function's semantics *without*
immediately specifying the incoming demand it was produced under. Despite StrSig
being a newtype wrapper around DmdType, it actually encodes two things:

  * The threshold (i.e., minimum arity) to unleash the signature
  * A demand type that is sound to unleash when the minimum arity requirement is
    met.

Here comes the subtle part: The threshold is encoded in the wrapped demand
type's depth! So in mkStrictSigForArity we make sure to trim the list of
argument demands to the given threshold arity. Call sites will make sure that
this corresponds to the arity of the call demand that elicited the wrapped
demand type. See also Note [What are demand signatures?].
-}

-- | The depth of the wrapped 'DmdType' encodes the arity at which it is safe
-- to unleash. Better construct this through 'mkStrictSigForArity'.
-- See Note [Understanding DmdType and StrictSig]
newtype StrictSig
  = StrictSig DmdType
  deriving Eq

-- | Turns a 'DmdType' computed for the particular 'Arity' into a 'StrictSig'
-- unleashable at that arity. See Note [Understanding DmdType and StrictSig]
mkStrictSigForArity :: Arity -> DmdType -> StrictSig
mkStrictSigForArity arity dmd_ty@(DmdType fvs args div)
  | arity < dmdTypeDepth dmd_ty = StrictSig (DmdType fvs (take arity args) div)
  | otherwise                   = StrictSig (etaExpandDmdType arity dmd_ty)

mkClosedStrictSig :: [Demand] -> Divergence -> StrictSig
mkClosedStrictSig ds res = mkStrictSigForArity (length ds) (DmdType emptyDmdEnv ds res)

splitStrictSig :: StrictSig -> ([Demand], Divergence)
splitStrictSig (StrictSig (DmdType _ dmds res)) = (dmds, res)

strictSigDmdEnv :: StrictSig -> DmdEnv
strictSigDmdEnv (StrictSig (DmdType env _ _)) = env

hasDemandEnvSig :: StrictSig -> Bool
hasDemandEnvSig = not . isEmptyVarEnv . strictSigDmdEnv

botSig :: StrictSig
botSig = StrictSig botDmdType

nopSig :: StrictSig
nopSig = StrictSig nopDmdType

isTopSig :: StrictSig -> Bool
isTopSig (StrictSig ty) = isTopDmdType ty

-- | True if the signature diverges or throws an exception in a saturated call.
-- See Note [Dead ends].
isDeadEndSig :: StrictSig -> Bool
isDeadEndSig (StrictSig (DmdType _ _ res)) = isDeadEndDiv res

-- | Returns true if an application to n args would diverge or throw an
-- exception.
--
-- If a function having 'botDiv' is applied to a less number of arguments than
-- its syntactic arity, we cannot say for sure that it is going to diverge.
-- Hence this function conservatively returns False in that case.
-- See Note [Dead ends].
appIsDeadEnd :: StrictSig -> Int -> Bool
appIsDeadEnd (StrictSig (DmdType _ ds res)) n
  = isDeadEndDiv res && not (lengthExceeds ds n)

prependArgsStrictSig :: Int -> StrictSig -> StrictSig
-- ^ Add extra ('topDmd') arguments to a strictness signature.
-- In contrast to 'etaConvertStrictSig', this /prepends/ additional argument
-- demands. This is used by FloatOut.
prependArgsStrictSig new_args sig@(StrictSig dmd_ty@(DmdType env dmds res))
  | new_args == 0       = sig
  | isTopDmdType dmd_ty = sig
  | new_args < 0        = pprPanic "prependArgsStrictSig: negative new_args"
                                   (ppr new_args $$ ppr sig)
  | otherwise           = StrictSig (DmdType env dmds' res)
  where
    dmds' = replicate new_args topDmd ++ dmds

etaConvertStrictSig :: Arity -> StrictSig -> StrictSig
-- ^ We are expanding (\x y. e) to (\x y z. e z) or reducing from the latter to
-- the former (when the Simplifier identifies a new join points, for example).
-- In contrast to 'prependArgsStrictSig', this /appends/ extra arg demands if
-- necessary.
-- This works by looking at the 'DmdType' (which was produced under a call
-- demand for the old arity) and trying to transfer as many facts as we can to
-- the call demand of new arity.
-- An arity increase (resulting in a stronger incoming demand) can retain much
-- of the info, while an arity decrease (a weakening of the incoming demand)
-- must fall back to a conservative default.
etaConvertStrictSig arity (StrictSig dmd_ty)
  | arity < dmdTypeDepth dmd_ty = StrictSig $ decreaseArityDmdType dmd_ty
  | otherwise                   = StrictSig $ etaExpandDmdType arity dmd_ty

{-
************************************************************************
*                                                                      *
                     Demand transformers
*                                                                      *
************************************************************************
-}

-- | A /demand transformer/ is a monotone function from an incoming evaluation
-- context ('SubDemand') to a 'DmdType', describing how the denoted thing
-- (i.e. expression, function) uses its arguments and free variables, and
-- whether it diverges.
--
-- See Note [Understanding DmdType and StrictSig]
-- and Note [What are demand signatures?].
type DmdTransformer = SubDemand -> DmdType

-- | Extrapolate a demand signature ('StrictSig') into a 'DmdTransformer'.
--
-- Given a function's 'StrictSig' and a 'SubDemand' for the evaluation context,
-- return how the function evaluates its free variables and arguments.
dmdTransformSig :: StrictSig -> DmdTransformer
dmdTransformSig (StrictSig dmd_ty@(DmdType _ arg_ds _)) sd
  = multDmdType (peelManyCalls (length arg_ds) sd) dmd_ty
    -- see Note [Demands from unsaturated function calls]
    -- and Note [What are demand signatures?]

-- | A special 'DmdTransformer' for data constructors that feeds product
-- demands into the constructor arguments.
dmdTransformDataConSig :: Arity -> DmdTransformer
dmdTransformDataConSig arity sd = case go arity sd of
  Just dmds -> DmdType emptyDmdEnv dmds topDiv
  Nothing   -> nopDmdType -- Not saturated
  where
    go 0 sd                            = viewProd arity sd
    go n (viewCall -> Just (C_11, sd)) = go (n-1) sd  -- strict calls only!
    go _ _                             = Nothing

-- | A special 'DmdTransformer' for dictionary selectors that feeds the demand
-- on the result into the indicated dictionary component (if saturated).
dmdTransformDictSelSig :: StrictSig -> DmdTransformer
-- NB: This currently doesn't handle newtype dictionaries and it's unclear how
-- it could without additional parameters.
dmdTransformDictSelSig (StrictSig (DmdType _ [(_ :* sig_sd)] _)) call_sd
   | (n, sd') <- peelCallDmd call_sd
   , Prod sig_ds  <- sig_sd
   = multDmdType n $
     DmdType emptyDmdEnv [C_11 :* Prod (map (enhance sd') sig_ds)] topDiv
   | otherwise
   = nopDmdType -- See Note [Demand transformer for a dictionary selector]
  where
    enhance sd old | isAbsDmd old = old
                   | otherwise    = C_11 :* sd  -- This is the one!

dmdTransformDictSelSig sig sd = pprPanic "dmdTransformDictSelSig: no args" (ppr sig $$ ppr sd)

{-
Note [What are demand signatures?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Demand analysis interprets expressions in the abstract domain of demand
transformers. Given a (sub-)demand that denotes the evaluation context, the
abstract transformer of an expression gives us back a demand type denoting
how other things (like arguments and free vars) were used when the expression
was evaluated. Here's an example:

  f x y =
    if x + expensive
      then \z -> z + y * ...
      else \z -> z * ...

The abstract transformer (let's call it F_e) of the if expression (let's
call it e) would transform an incoming (undersaturated!) head demand 1A into
a demand type like {x-><1L>,y-><L>}<L>. In pictures:

     Demand ---F_e---> DmdType
     <1A>              {x-><1L>,y-><L>}<L>

Let's assume that the demand transformers we compute for an expression are
correct wrt. to some concrete semantics for Core. How do demand signatures fit
in? They are strange beasts, given that they come with strict rules when to
it's sound to unleash them.

Fortunately, we can formalise the rules with Galois connections. Consider
f's strictness signature, {}<1L><L>. It's a single-point approximation of
the actual abstract transformer of f's RHS for arity 2. So, what happens is that
we abstract *once more* from the abstract domain we already are in, replacing
the incoming Demand by a simple lattice with two elements denoting incoming
arity: A_2 = {<2, >=2} (where '<2' is the top element and >=2 the bottom
element). Here's the diagram:

     A_2 -----f_f----> DmdType
      ^                   |
      | α               γ |
      |                   v
  SubDemand --F_f----> DmdType

With
  α(C1(C1(_))) = >=2
  α(_)         =  <2
  γ(ty)        =  ty
and F_f being the abstract transformer of f's RHS and f_f being the abstracted
abstract transformer computable from our demand signature simply by

  f_f(>=2) = {}<1L><L>
  f_f(<2)  = multDmdType C_0N {}<1L><L>

where multDmdType makes a proper top element out of the given demand type.

In practice, the A_n domain is not just a simple Bool, but a Card, which is
exactly the Card with which we have to multDmdType. The Card for arity n
is computed by calling @peelManyCalls n@, which corresponds to α above.

Note [Demand transformer for a dictionary selector]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we evaluate (op dict-expr) under demand 'd', then we can push the demand 'd'
into the appropriate field of the dictionary. What *is* the appropriate field?
We just look at the strictness signature of the class op, which will be
something like: P(AAA1AAAAA).  Then replace the '1' by the demand 'd'.

For single-method classes, which are represented by newtypes the signature
of 'op' won't look like P(...), so matching on Prod will fail.
That's fine: if we are doing strictness analysis we are also doing inlining,
so we'll have inlined 'op' into a cast.  So we can bale out in a conservative
way, returning nopDmdType.

It is (just.. #8329) possible to be running strictness analysis *without*
having inlined class ops from single-method classes.  Suppose you are using
ghc --make; and the first module has a local -O0 flag.  So you may load a class
without interface pragmas, ie (currently) without an unfolding for the class
ops.   Now if a subsequent module in the --make sweep has a local -O flag
you might do strictness analysis, but there is no inlining for the class op.
This is weird, so I'm not worried about whether this optimises brilliantly; but
it should not fall over.
-}

-- | Remove the demand environment from the signature.
zapDmdEnvSig :: StrictSig -> StrictSig
zapDmdEnvSig (StrictSig (DmdType _ ds r)) = mkClosedStrictSig ds r

zapUsageDemand :: Demand -> Demand
-- Remove the usage info, but not the strictness info, from the demand
zapUsageDemand = kill_usage $ KillFlags
    { kf_abs         = True
    , kf_used_once   = True
    , kf_called_once = True
    }

-- | Remove all `C_01 :*` info (but not `CM` sub-demands) from the demand
zapUsedOnceDemand :: Demand -> Demand
zapUsedOnceDemand = kill_usage $ KillFlags
    { kf_abs         = False
    , kf_used_once   = True
    , kf_called_once = False
    }

-- | Remove all `C_01 :*` info (but not `CM` sub-demands) from the strictness
--   signature
zapUsedOnceSig :: StrictSig -> StrictSig
zapUsedOnceSig (StrictSig (DmdType env ds r))
    = StrictSig (DmdType env (map zapUsedOnceDemand ds) r)

data KillFlags = KillFlags
    { kf_abs         :: Bool
    , kf_used_once   :: Bool
    , kf_called_once :: Bool
    }

kill_usage_card :: KillFlags -> Card -> Card
kill_usage_card kfs C_00 | kf_abs kfs       = C_0N
kill_usage_card kfs C_10 | kf_abs kfs       = C_1N
kill_usage_card kfs C_01 | kf_used_once kfs = C_0N
kill_usage_card kfs C_11 | kf_used_once kfs = C_1N
kill_usage_card _   n                       = n

kill_usage :: KillFlags -> Demand -> Demand
kill_usage kfs (n :* sd) = kill_usage_card kfs n :* kill_usage_sd kfs sd

kill_usage_sd :: KillFlags -> SubDemand -> SubDemand
kill_usage_sd kfs (Call n sd)
  | kf_called_once kfs      = mkCall (lubCard C_1N n) (kill_usage_sd kfs sd)
  | otherwise               = mkCall n                (kill_usage_sd kfs sd)
kill_usage_sd kfs (Prod ds) = Prod (map (kill_usage kfs) ds)
kill_usage_sd _   sd        = sd

{- *********************************************************************
*                                                                      *
               TypeShape and demand trimming
*                                                                      *
********************************************************************* -}


data TypeShape -- See Note [Trimming a demand to a type]
               --     in GHC.Core.Opt.DmdAnal
  = TsFun TypeShape
  | TsProd [TypeShape]
  | TsUnk

trimToType :: Demand -> TypeShape -> Demand
-- See Note [Trimming a demand to a type] in GHC.Core.Opt.DmdAnal
trimToType (n :* sd) ts
  = n :* go sd ts
  where
    go (Prod ds)   (TsProd tss)
      | equalLength ds tss    = Prod (zipWith trimToType ds tss)
    go (Call n sd) (TsFun ts) = mkCall n (go sd ts)
    go sd@Poly{}   _          = sd
    go _           _          = topSubDmd

{-
************************************************************************
*                                                                      *
                     'seq'ing demands
*                                                                      *
************************************************************************
-}

seqDemand :: Demand -> ()
seqDemand (_ :* sd) = seqSubDemand sd

seqSubDemand :: SubDemand -> ()
seqSubDemand (Prod ds)   = seqDemandList ds
seqSubDemand (Call _ sd) = seqSubDemand sd
seqSubDemand (Poly _)    = ()

seqDemandList :: [Demand] -> ()
seqDemandList = foldr (seq . seqDemand) ()

seqDmdType :: DmdType -> ()
seqDmdType (DmdType env ds res) =
  seqDmdEnv env `seq` seqDemandList ds `seq` res `seq` ()

seqDmdEnv :: DmdEnv -> ()
seqDmdEnv env = seqEltsUFM seqDemandList env

seqStrictSig :: StrictSig -> ()
seqStrictSig (StrictSig ty) = seqDmdType ty

{-
************************************************************************
*                                                                      *
                     Outputable and Binary instances
*                                                                      *
************************************************************************
-}

{- Note [Demand notation]
~~~~~~~~~~~~~~~~~~~~~~~~~
This Note should be kept up to date with the documentation of `-fstrictness`
in the user's guide.

For pretty-printing demands, we use quite a compact notation with some
abbreviations. Here's the BNF:

  card ::= B    {}
        |  A    {0}
        |  M    {0,1}
        |  L    {0,1,n}
        |  1    {1}
        |  S    {1,n}

  d    ::= card sd                  The :* constructor, just juxtaposition
        |  card                     abbreviation: Same as "card card",
                                                  in code @polyDmd card@

  sd   ::= card                     @Poly card@
        |  P(d,d,..)                @Prod [d1,d2,..]@
        |  Ccard(sd)                @Call card sd@

So, L can denote a 'Card', polymorphic 'SubDemand' or polymorphic 'Demand',
but it's always clear from context which "overload" is meant. It's like
return-type inference of e.g. 'read'.

Examples are in the haddock for 'Demand'.

This is the syntax for demand signatures:

  div ::= <empty>      topDiv
       |  x            exnDiv
       |  b            botDiv

  sig ::= {x->dx,y->dy,z->dz...}<d1><d2><d3>...<dn>div
                  ^              ^   ^   ^      ^   ^
                  |              |   |   |      |   |
                  |              \---+---+------/   |
                  |                  |              |
             demand on free        demand on      divergence
               variables           arguments      information
           (omitted if empty)                     (omitted if
                                                no information)


-}

-- | See Note [Demand notation]
-- Current syntax was discussed in #19016.
instance Outputable Card where
  ppr C_00 = char 'A' -- "Absent"
  ppr C_01 = char 'M' -- "Maybe"
  ppr C_0N = char 'L' -- "Lazy"
  ppr C_11 = char '1' -- "exactly 1"
  ppr C_1N = char 'S' -- "Strict"
  ppr C_10 = char 'B' -- "Bottom"

-- | See Note [Demand notation]
instance Outputable Demand where
  ppr dmd@(n :* sd)
    | isAbs n          = ppr n   -- If absent, sd is arbitrary
    | dmd == polyDmd n = ppr n   -- Print UU as just U
    | otherwise        = ppr n <> ppr sd

-- | See Note [Demand notation]
instance Outputable SubDemand where
  ppr (Poly sd)   = ppr sd
  ppr (Call n sd) = char 'C' <> ppr n <> parens (ppr sd)
  ppr (Prod ds)   = char 'P' <> parens (fields ds)
    where
      fields []     = empty
      fields [x]    = ppr x
      fields (x:xs) = ppr x <> char ',' <> fields xs

instance Outputable Divergence where
  ppr Diverges = char 'b' -- for (b)ottom
  ppr ExnOrDiv = char 'x' -- for e(x)ception
  ppr Dunno    = empty

instance Outputable DmdType where
  ppr (DmdType fv ds res)
    = hsep [hcat (map (angleBrackets . ppr) ds) <> ppr res,
            if null fv_elts then empty
            else braces (fsep (map pp_elt fv_elts))]
    where
      pp_elt (uniq, dmd) = ppr uniq <> text "->" <> ppr dmd
      fv_elts = nonDetUFMToList fv
        -- It's OK to use nonDetUFMToList here because we only do it for
        -- pretty printing

instance Outputable StrictSig where
   ppr (StrictSig ty) = ppr ty

instance Outputable TypeShape where
  ppr TsUnk        = text "TsUnk"
  ppr (TsFun ts)   = text "TsFun" <> parens (ppr ts)
  ppr (TsProd tss) = parens (hsep $ punctuate comma $ map ppr tss)

instance Binary Card where
  put_ bh C_00 = putByte bh 0
  put_ bh C_01 = putByte bh 1
  put_ bh C_0N = putByte bh 2
  put_ bh C_11 = putByte bh 3
  put_ bh C_1N = putByte bh 4
  put_ bh C_10 = putByte bh 5
  get bh = do
    h <- getByte bh
    case h of
      0 -> return C_00
      1 -> return C_01
      2 -> return C_0N
      3 -> return C_11
      4 -> return C_1N
      5 -> return C_10
      _ -> pprPanic "Binary:Card" (ppr (fromIntegral h :: Int))

instance Binary Demand where
  put_ bh (n :* sd) = put_ bh n *> put_ bh sd
  get bh = (:*) <$> get bh <*> get bh

instance Binary SubDemand where
  put_ bh (Poly sd)   = putByte bh 0 *> put_ bh sd
  put_ bh (Call n sd) = putByte bh 1 *> put_ bh n *> put_ bh sd
  put_ bh (Prod ds)   = putByte bh 2 *> put_ bh ds
  get bh = do
    h <- getByte bh
    case h of
      0 -> Poly <$> get bh
      1 -> mkCall <$> get bh <*> get bh
      2 -> Prod <$> get bh
      _ -> pprPanic "Binary:SubDemand" (ppr (fromIntegral h :: Int))

instance Binary StrictSig where
  put_ bh (StrictSig aa) = put_ bh aa
  get bh = StrictSig <$> get bh

instance Binary DmdType where
  -- Ignore DmdEnv when spitting out the DmdType
  put_ bh (DmdType _ ds dr) = put_ bh ds *> put_ bh dr
  get bh = DmdType emptyDmdEnv <$> get bh <*> get bh

instance Binary Divergence where
  put_ bh Dunno    = putByte bh 0
  put_ bh ExnOrDiv = putByte bh 1
  put_ bh Diverges = putByte bh 2
  get bh = do
    h <- getByte bh
    case h of
      0 -> return Dunno
      1 -> return ExnOrDiv
      2 -> return Diverges
      _ -> pprPanic "Binary:Divergence" (ppr (fromIntegral h :: Int))
