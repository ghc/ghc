{-# LANGUAGE CPP #-}

-- | Constructed Product Result analysis. Identifies functions that surely
-- return heap-allocated records on every code path, so that we can eliminate
-- said heap allocation by performing a worker/wrapper split.
--
-- See https://www.microsoft.com/en-us/research/publication/constructed-product-result-analysis-haskell/.
-- CPR analysis should happen after strictness analysis.
-- See Note [Phase ordering].
module GHC.Core.Opt.CprAnal ( cprAnalProgram ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Driver.Session
import GHC.Types.Demand
import GHC.Types.Cpr
import GHC.Core
import GHC.Core.Seq
import GHC.Utils.Outputable
import GHC.Builtin.Names ( runRWKey )
import GHC.Types.Var.Env
import GHC.Types.Basic
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Core.DataCon
import GHC.Core.Multiplicity
import GHC.Core.Utils   ( exprIsHNF, dumpIdInfoOfProgram )
import GHC.Core.Type
import GHC.Core.FamInstEnv
import GHC.Core.Opt.WorkWrap.Utils
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Logger  ( Logger, dumpIfSet_dyn, DumpFormat (..) )
import GHC.Data.Graph.UnVar -- for UnVarSet
import GHC.Data.Maybe   ( isNothing )

import Control.Monad ( guard )
import Data.List ( mapAccumL )

import GHC.Driver.Ppr
_ = pprTrace -- Tired of commenting out the import all the time

{- Note [Constructed Product Result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The goal of Constructed Product Result analysis is to identify functions that
surely return heap-allocated records on every code path, so that we can
eliminate said heap allocation by performing a worker/wrapper split.

@swap@ below is such a function:

  swap (a, b) = (b, a)

A @case@ on an application of @swap@, like
@case swap (10, 42) of (a, b) -> a + b@ could cancel away
(by case-of-known-constructor) if we "inlined" @swap@ and simplified. We then
say that @swap@ has the CPR property.

We can't inline recursive functions, but similar reasoning applies there:

  f x n = case n of
    0 -> (x, 0)
    _ -> f (x+1) (n-1)

Inductively, @case f 1 2 of (a, b) -> a + b@ could cancel away the constructed
product with the case. So @f@, too, has the CPR property. But we can't really
"inline" @f@, because it's recursive. Also, non-recursive functions like @swap@
might be too big to inline (or even marked NOINLINE). We still want to exploit
the CPR property, and that is exactly what the worker/wrapper transformation
can do for us:

  $wf x n = case n of
    0 -> case (x, 0) of -> (a, b) -> (# a, b #)
    _ -> case f (x+1) (n-1) of (a, b) -> (# a, b #)
  f x n = case $wf x n of (# a, b #) -> (a, b)

where $wf readily simplifies (by case-of-known-constructor and inlining @f@) to:

  $wf x n = case n of
    0 -> (# x, 0 #)
    _ -> $wf (x+1) (n-1)

Now, a call site like @case f 1 2 of (a, b) -> a + b@ can inline @f@ and
eliminate the heap-allocated pair constructor.

Note [Phase ordering]
~~~~~~~~~~~~~~~~~~~~~
We need to perform strictness analysis before CPR analysis, because that might
unbox some arguments, in turn leading to more constructed products.
Ideally, we would want the following pipeline:

1. Strictness
2. worker/wrapper (for strictness)
3. CPR
4. worker/wrapper (for CPR)

Currently, we omit 2. and anticipate the results of worker/wrapper.
See Note [CPR for binders that will be unboxed]
and Note [Optimistic field binder CPR].
An additional w/w pass would simplify things, but probably add slight overhead.
So currently we have

1. Strictness
2. CPR
3. worker/wrapper (for strictness and CPR)
-}

--
-- * Analysing programs
--

cprAnalProgram :: Logger -> DynFlags -> FamInstEnvs -> CoreProgram -> IO CoreProgram
cprAnalProgram logger dflags fam_envs binds = do
  let env            = emptyAnalEnv fam_envs
  let binds_plus_cpr = snd $ mapAccumL cprAnalTopBind env binds
  dumpIfSet_dyn logger dflags Opt_D_dump_cpr_signatures "Cpr signatures" FormatText $
    dumpIdInfoOfProgram (ppr . cprInfo) binds_plus_cpr
  -- See Note [Stamp out space leaks in demand analysis] in GHC.Core.Opt.DmdAnal
  seqBinds binds_plus_cpr `seq` return binds_plus_cpr

-- Analyse a (group of) top-level binding(s)
cprAnalTopBind :: AnalEnv
               -> CoreBind
               -> (AnalEnv, CoreBind)
cprAnalTopBind env (NonRec id rhs)
  = (env', NonRec id' rhs')
  where
    (id', rhs', env') = cprAnalBind TopLevel env id rhs

cprAnalTopBind env (Rec pairs)
  = (env', Rec pairs')
  where
    (env', pairs') = cprFix TopLevel env pairs

--
-- * Analysing expressions
--

-- | The abstract semantic function ⟦_⟧ : Expr -> Env -> A from
-- "Constructed Product Result Analysis for Haskell"
cprAnal, cprAnal'
  :: AnalEnv
  -> CoreExpr            -- ^ expression to be denoted by a 'CprType'
  -> (CprType, CoreExpr) -- ^ the updated expression and its 'CprType'

cprAnal env e = -- pprTraceWith "cprAnal" (\res -> ppr (fst (res)) $$ ppr e) $
                  cprAnal' env e

cprAnal' _ (Lit lit)     = (topCprType, Lit lit)
cprAnal' _ (Type ty)     = (topCprType, Type ty)      -- Doesn't happen, in fact
cprAnal' _ (Coercion co) = (topCprType, Coercion co)

cprAnal' env (Cast e co)
  = (cpr_ty, Cast e' co)
  where
    (cpr_ty, e') = cprAnal env e

cprAnal' env (Tick t e)
  = (cpr_ty, Tick t e')
  where
    (cpr_ty, e') = cprAnal env e

cprAnal' env e@(Var{})
  = cprAnalApp env e [] []
cprAnal' env e@(App{})
  = cprAnalApp env e [] []

cprAnal' env (Lam var body)
  | isTyVar var
  , (body_ty, body') <- cprAnal env body
  = (body_ty, Lam var body')
  | otherwise
  = (lam_ty, Lam var body')
  where
    -- See Note [CPR for binders that will be unboxed]
    env'             = extendSigEnvForArg env var
    (body_ty, body') = cprAnal env' body
    lam_ty           = abstractCprTy body_ty

cprAnal' env (Case scrut case_bndr ty alts)
  = (res_ty, Case scrut' case_bndr ty alts')
  where
    (scrut_ty, scrut') = cprAnal env scrut
    env'               = extendSigEnv env case_bndr (CprSig scrut_ty)
    (alt_tys, alts')   = mapAndUnzip (cprAnalAlt env' scrut_ty) alts
    res_ty             = foldl' lubCprType botCprType alt_tys

cprAnal' env (Let (NonRec id rhs) body)
  = (body_ty, Let (NonRec id' rhs') body')
  where
    (id', rhs', env') = cprAnalBind NotTopLevel env id rhs
    (body_ty, body')  = cprAnal env' body

cprAnal' env (Let (Rec pairs) body)
  = body_ty `seq` (body_ty, Let (Rec pairs') body')
  where
    (env', pairs')   = cprFix NotTopLevel env pairs
    (body_ty, body') = cprAnal env' body

cprAnalAlt
  :: AnalEnv
  -> CprType -- ^ CPR type of the scrutinee
  -> Alt Var -- ^ current alternative
  -> (CprType, Alt Var)
cprAnalAlt env scrut_ty (Alt con bndrs rhs)
  = (rhs_ty, Alt con bndrs rhs')
  where
    env_alt
      | DataAlt dc <- con
      , let ids = filter isId bndrs
      , CprType arity cpr <- scrut_ty
      , ASSERT( arity == 0 ) True
      = case unpackConFieldsCpr dc cpr of
          AllFieldsSame field_cpr
            | let sig = mkCprSig 0 field_cpr
            -> extendSigEnvAllSame env ids sig
          ForeachField field_cprs
            | let sigs = zipWith (mkCprSig . idArity) ids field_cprs
            -> extendSigEnvList env (zipEqual "cprAnalAlt" ids sigs)
      | otherwise
      = env
    (rhs_ty, rhs') = cprAnal env_alt rhs

--
-- * CPR transformer
--

cprAnalApp :: AnalEnv -> CoreExpr -> [CoreArg] -> [CprType] -> (CprType, CoreExpr)
cprAnalApp env e args' arg_tys
  -- Collect CprTypes for (value) args (inlined collectArgs):
  | App fn arg <- e, isTypeArg arg -- Don't analyse Type args
  = cprAnalApp env fn (arg:args') arg_tys
  | App fn arg <- e
  , (arg_ty, arg') <- cprAnal env arg
  = cprAnalApp env fn (arg':args') (arg_ty:arg_tys)

  | Var fn <- e
  = (cprTransform env fn arg_tys, mkApps e args')

  | otherwise -- e is not an App and not a Var
  , (e_ty, e') <- cprAnal env e
  = (applyCprTy e_ty (length arg_tys), mkApps e' args')

cprTransform :: AnalEnv   -- ^ The analysis environment
             -> Id        -- ^ The function
             -> [CprType] -- ^ info about incoming /value/ arguments
             -> CprType   -- ^ The demand type of the application
cprTransform env id args
  = -- pprTrace "cprTransform" (vcat [ppr id, ppr args, ppr sig])
    sig
  where
    sig
      -- Top-level binding, local let-binding, lambda arg or case binder
      | Just sig <- lookupSigEnv env id
      = applyCprTy (getCprSig sig) (length args)
      -- CPR transformers for special Ids
      | Just cpr_ty <- cprTransformSpecial id args
      = cpr_ty
      -- See Note [CPR for data structures]
      | Just rhs <- cprDataStructureUnfolding_maybe id
      = fst $ cprAnal env rhs
      -- Imported function or data con worker
      | isGlobalId id
      = applyCprTy (getCprSig (idCprInfo id)) (length args)
      | otherwise
      = topCprType

-- | CPR transformers for special Ids
cprTransformSpecial :: Id -> [CprType] -> Maybe CprType
cprTransformSpecial id args
  -- See Note [Simplification of runRW#] in GHC.CoreToStg.Prep
  | idUnique id == runRWKey -- `runRW (\s -> e)`
  , [arg] <- args           -- `\s -> e` has CPR type `arg` (e.g. `. -> 2`)
  = Just $ applyCprTy arg 1 -- `e` has CPR type `2`
  | otherwise
  = Nothing

--
-- * Bindings
--

-- Recursive bindings
cprFix :: TopLevelFlag
       -> AnalEnv                    -- Does not include bindings for this binding
       -> [(Id,CoreExpr)]
       -> (AnalEnv, [(Id,CoreExpr)]) -- Binders annotated with CPR info
cprFix top_lvl orig_env orig_pairs
  = loop 1 init_env init_pairs
  where
    init_sig id rhs
      -- See Note [CPR for data structures]
      | isDataStructure id rhs = topCprSig
      | otherwise              = mkCprSig 0 botCpr
    -- See Note [Initialising strictness] in GHC.Core.Opt.DmdAnal
    orig_virgin = ae_virgin orig_env
    init_pairs | orig_virgin  = [(setIdCprInfo id (init_sig id rhs), rhs) | (id, rhs) <- orig_pairs ]
               | otherwise    = orig_pairs
    init_env = extendSigEnvFromIds orig_env (map fst init_pairs)

    -- The fixed-point varies the idCprInfo field of the binders and and their
    -- entries in the AnalEnv, and terminates if that annotation does not change
    -- any more.
    loop :: Int -> AnalEnv -> [(Id,CoreExpr)] -> (AnalEnv, [(Id,CoreExpr)])
    loop n env pairs
      | found_fixpoint = (reset_env', pairs')
      | otherwise      = loop (n+1) env' pairs'
      where
        -- In all but the first iteration, delete the virgin flag
        -- See Note [Initialising strictness] in GHC.Core.Opt.DmdAnal
        (env', pairs') = step (applyWhen (n/=1) nonVirgin env) pairs
        -- Make sure we reset the virgin flag to what it was when we are stable
        reset_env'     = env'{ ae_virgin = orig_virgin }
        found_fixpoint = map (idCprInfo . fst) pairs' == map (idCprInfo . fst) pairs

    step :: AnalEnv -> [(Id, CoreExpr)] -> (AnalEnv, [(Id, CoreExpr)])
    step env pairs = mapAccumL go env pairs
      where
        go env (id, rhs) = (env', (id', rhs'))
          where
            (id', rhs', env') = cprAnalBind top_lvl env id rhs

-- | Process the RHS of the binding for a sensible arity, add the CPR signature
-- to the Id, and augment the environment with the signature as well.
cprAnalBind
  :: TopLevelFlag
  -> AnalEnv
  -> Id
  -> CoreExpr
  -> (Id, CoreExpr, AnalEnv)
cprAnalBind top_lvl env id rhs
  -- See Note [CPR for data structures]
  | isDataStructure id rhs
  = (id,  rhs,  env) -- Data structure => no code => need to analyse rhs
  | otherwise
  = (id', rhs', env')
  where
    (rhs_ty, rhs')  = cprAnal env rhs
    -- possibly trim thunk CPR info
    rhs_ty'
      -- See Note [CPR for thunks]
      | stays_thunk = trimCprTy rhs_ty
      -- See Note [CPR for sum types]
      | returns_sum = trimCprTy rhs_ty
      | otherwise   = rhs_ty
    -- See Note [Arity trimming for CPR signatures]
    sig  = mkCprSigForArity (idArity id) rhs_ty'
    id'  = setIdCprInfo id sig
    env' = extendSigEnv env id sig

    -- See Note [CPR for thunks]
    stays_thunk = is_thunk && not_strict
    is_thunk    = not (exprIsHNF rhs) && not (isJoinId id)
    not_strict  = not (isStrUsedDmd (idDemandInfo id))
    -- See Note [CPR for sum types]
    (_, ret_ty) = splitPiTys (idType id)
    not_a_prod  = isNothing (splitArgType_maybe (ae_fam_envs env) ret_ty)
    returns_sum = not (isTopLevel top_lvl) && not_a_prod

isDataStructure :: Id -> CoreExpr -> Bool
-- See Note [CPR for data structures]
isDataStructure id rhs =
  idArity id == 0 && exprIsHNF rhs

-- | Returns an expandable unfolding
-- (See Note [exprIsExpandable] in "GHC.Core.Utils") that has
-- So effectively is a constructor application.
cprDataStructureUnfolding_maybe :: Id -> Maybe CoreExpr
cprDataStructureUnfolding_maybe id = do
  -- There are only FinalPhase Simplifier runs after CPR analysis
  guard (activeInFinalPhase (idInlineActivation id))
  unf <- expandUnfolding_maybe (idUnfolding id)
  guard (isDataStructure id unf)
  return unf

{- Note [Arity trimming for CPR signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Although it doesn't affect correctness of the analysis per se, we have to trim
CPR signatures to idArity. Here's what might happen if we don't:

  f x = if expensive
          then \y. Box y
          else \z. Box z
  g a b = f a b

The two lambdas will have a CPR type of @1m@ (so construct a product after
applied to one argument). Thus, @f@ will have a CPR signature of @2m@
(constructs a product after applied to two arguments).
But WW will never eta-expand @f@! In this case that would amount to possibly
duplicating @expensive@ work.

(Side note: Even if @f@'s 'idArity' happened to be 2, it would not do so, see
Note [Don't eta expand in w/w].)

So @f@ will not be worker/wrappered. But @g@ also inherited its CPR signature
from @f@'s, so it *will* be WW'd:

  f x = if expensive
          then \y. Box y
          else \z. Box z
  $wg a b = case f a b of Box x -> x
  g a b = Box ($wg a b)

And the case in @g@ can never cancel away, thus we introduced extra reboxing.
Hence we always trim the CPR signature of a binding to idArity.
-}

data AnalEnv
  = AE
  { ae_sigs   :: SigEnv
  -- ^ Current approximation of signatures for local ids
  , ae_virgin :: Bool
  -- ^ True only on every first iteration in a fixed-point
  -- iteration. See Note [Initialising strictness] in "GHC.Core.Opt.DmdAnal"
  , ae_fam_envs :: FamInstEnvs
  -- ^ Needed when expanding type families and synonyms of product types.
  }

instance Outputable AnalEnv where
  ppr (AE { ae_sigs = env, ae_virgin = virgin })
    = text "AE" <+> braces (vcat
         [ text "ae_virgin =" <+> ppr virgin
         , text "ae_sigs =" <+> ppr env ])

-- | An environment storing 'CprSig's for local Ids.
-- Puts binders with 'topCprSig' in a space-saving 'IntSet'.
-- See Note [Efficient Top sigs in SigEnv].
data SigEnv
  = SE
  { se_tops :: !UnVarSet
  -- ^ All these Ids have 'topCprSig'. Like a 'VarSet', but more efficient.
  , se_sigs :: !(VarEnv CprSig)
  -- ^ Ids that have something other than 'topCprSig'.
  }

instance Outputable SigEnv where
  ppr (SE { se_tops = tops, se_sigs = sigs })
    = text "SE" <+> braces (vcat
         [ text "se_tops =" <+> ppr tops
         , text "se_sigs =" <+> ppr sigs ])

emptyAnalEnv :: FamInstEnvs -> AnalEnv
emptyAnalEnv fam_envs
  = AE
  { ae_sigs = SE emptyUnVarSet emptyVarEnv
  , ae_virgin = True
  , ae_fam_envs = fam_envs
  }

modifySigEnv :: (SigEnv -> SigEnv) -> AnalEnv -> AnalEnv
modifySigEnv f env = env { ae_sigs = f (ae_sigs env) }

lookupSigEnv :: AnalEnv -> Id -> Maybe CprSig
-- See Note [Efficient Top sigs in SigEnv]
lookupSigEnv AE{ae_sigs = SE tops sigs} id
  | id `elemUnVarSet` tops = Just topCprSig
  | otherwise              = lookupVarEnv sigs id

extendSigEnv :: AnalEnv -> Id -> CprSig -> AnalEnv
-- See Note [Efficient Top sigs in SigEnv]
extendSigEnv env id sig
  | isTopCprSig sig
  = modifySigEnv (\se -> se{se_tops = extendUnVarSet id (se_tops se)}) env
  | otherwise
  = modifySigEnv (\se -> se{se_sigs = extendVarEnv (se_sigs se) id sig}) env

-- | Extend an environment with the (Id, CPR sig) pairs
extendSigEnvList :: AnalEnv -> [(Id, CprSig)] -> AnalEnv
extendSigEnvList env ids_cprs
  = foldl' (\env (id, sig) -> extendSigEnv env id sig) env ids_cprs

-- | Extend an environment with the CPR sigs attached to the ids
extendSigEnvFromIds :: AnalEnv -> [Id] -> AnalEnv
extendSigEnvFromIds env ids
  = foldl' (\env id -> extendSigEnv env id (idCprInfo id)) env ids

-- | Extend an environment with the same CPR sig for all ids
extendSigEnvAllSame :: AnalEnv -> [Id] -> CprSig -> AnalEnv
extendSigEnvAllSame env ids sig
  = foldl' (\env id -> extendSigEnv env id sig) env ids

nonVirgin :: AnalEnv -> AnalEnv
nonVirgin env = env { ae_virgin = False }

-- | A version of 'extendSigEnv' for a binder of which we don't see the RHS
-- needed to compute a 'CprSig' (e.g. lambdas and DataAlt field binders).
-- In this case, we can still look at their demand to attach CPR signatures
-- anticipating the unboxing done by worker/wrapper.
-- See Note [CPR for binders that will be unboxed].
extendSigEnvForArg :: AnalEnv -> Id -> AnalEnv
extendSigEnvForArg env id
  = extendSigEnv env id (CprSig (argCprType env (idType id) (idDemandInfo id)))

-- | Produces a 'CprType' according to how a strict argument will be unboxed.
-- Examples:
--
--   * A head-strict demand @1L@ on @Int@ would translate to @1@
--   * A product demand @1P(1L,L)@ on @(Int, Bool)@ would translate to @1(1,)@
--   * A product demand @1P(1L,L)@ on @(a , Bool)@ would translate to @1(,)@,
--     because the unboxing strategy would not unbox the @a@.
argCprType :: AnalEnv -> Type -> Demand -> CprType
argCprType env arg_ty dmd = CprType 0 (go arg_ty dmd)
  where
    go ty dmd
      | Unbox (DataConPatContext { dcpc_dc = dc, dcpc_tc_args = tc_args }) ds
          <- wantToUnbox (ae_fam_envs env) no_inlineable_prag ty dmd
      -- No existentials; see Note [Which types are unboxed?])
      -- Otherwise we'd need to call dataConRepInstPat here and thread a
      -- UniqSupply. So argCprType is a bit less aggressive than it could
      -- be, for the sake of coding convenience.
      , null (dataConExTyCoVars dc)
      , let arg_tys = map scaledThing (dataConInstArgTys dc tc_args)
      = ConCpr (dataConTag dc) (zipWith go arg_tys ds)
      | otherwise
      = topCpr
    -- Rather than maintaining in AnalEnv whether we are in an INLINEABLE
    -- function, we just assume that we aren't. That flag is only relevant
    -- to Note [Do not unpack class dictionaries], the few unboxing
    -- opportunities on dicts it prohibits are probably irrelevant to CPR.
    no_inlineable_prag = False

{- Note [Safe abortion in the fixed-point iteration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Fixed-point iteration may fail to terminate. But we cannot simply give up and
return the environment and code unchanged! We still need to do one additional
round, to ensure that all expressions have been traversed at least once, and any
unsound CPR annotations have been updated.

Note [Efficient Top sigs in SigEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's pretty common for binders in the SigEnv to have a 'topCprSig'.
Wide records with 100 fields like in T9675 even will generate code where the
majority of binders has Top signature. To save some allocations, we store
those binders with a Top signature in a separate UnVarSet (which is an IntSet
with a convenient Var-tailored API).

Why store top signatures at all in the SigEnv? After all, when 'cprTransform'
encounters a locally-bound Id without an entry in the SigEnv, it should behave
as if that binder has a Top signature!
Well, the problem is when case binders should have a Top signatures. They always
have an unfolding and thus look to 'cprTransform' as if they bind a data
structure, Note [CPR for data structures], and thus would always have the CPR
property. So we need some mechanism to separate data structures from case
binders with a Top signature, and the UnVarSet provides that in the least
convoluted way I can think of.

Note [CPR for binders that will be unboxed]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a lambda-bound variable will be unboxed by worker/wrapper (so it must be
demanded strictly), then give it a CPR signature. Here's a concrete example
('f1' in test T10482a), assuming h is strict:

  f1 :: Int -> Int
  f1 x = case h x of
          A -> x
          B -> f1 (x-1)
          C -> x+1

If we notice that 'x' is used strictly, we can give it the CPR
property; and hence f1 gets the CPR property too.  It's sound (doesn't
change strictness) to give it the CPR property because by the time 'x'
is returned (case A above), it'll have been evaluated (by the wrapper
of 'h' in the example).

Moreover, if f itself is strict in x, then we'll pass x unboxed to
f1, and so the boxed version *won't* be available; in that case it's
very helpful to give 'x' the CPR property.

This is all done in 'extendSigEnvForArg'.

Note that

  * Whether or not something unboxes is decided by 'wantToUnbox', else we may
    get over-optimistic CPR results (e.g., from \(x :: a) -> x!).

  * If the demand unboxes deeply, we can give the binder a /nested/ CPR
    property, e.g.

      g :: (Int, Int) -> Int
      g p = case p of
        (x, y) | x < 0     -> 0
               | otherwise -> x

    `x` should have the CPR property because it will be unboxed. We do so
    by giving `p` the Nested CPR property `1(1,)`, indicating that we not only
    have `p` available unboxed, but also its field `x`. Analysis of the Case
    will then transfer the CPR property to `x`.

    Before we were able to express Nested CPR, we used to guess which field
    binders should get the CPR property.
    See Historic Note [Optimistic field binder CPR].

  * See Note [CPR examples]

Historic Note [Optimistic field binder CPR]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note describes how we used to guess whether fields have the CPR property
before we were able to express Nested CPR for arguments.

Consider

  data T a = MkT a
  f :: T Int -> Int
  f x = ... (case x of
    MkT y -> y) ...

And assume we know from strictness analysis that `f` is strict in `x` and its
field `y` and we unbox both. Then we give `x` the CPR property according
to Note [CPR for binders that will be unboxed]. But `x`'s sole field `y`
likewise will be unboxed and it should also get the CPR property. We'd
need a *nested* CPR property here for `x` to express that and unwrap one level
when we analyse the Case to give the CPR property to `y`.

Lacking Nested CPR, we have to guess a bit, by looking for

  (A) Flat CPR on the scrutinee
  (B) A variable scrutinee. Otherwise surely it can't be a parameter.
  (C) Strict demand on the field binder `y` (or it binds a strict field)

While (A) is a necessary condition to give a field the CPR property, there are
ways in which (B) and (C) are too lax, leading to unsound analysis results and
thus reboxing in the wrapper:

  (b) We could scrutinise some other variable than a parameter, like in

        g :: T Int -> Int
        g x = let z = foo x in -- assume `z` has CPR property
              case z of MkT y -> y

      Lacking Nested CPR and multiple levels of unboxing, only the outer box
      of `z` will be available and a case on `y` won't actually cancel away.
      But it's simple, and nothing terrible happens if we get it wrong. e.g.
      #10694.

  (c) A strictly used field binder doesn't mean the function is strict in it.

        h :: T Int -> Int -> Int
        h !x 0 = 0
        h  x 0 = case x of MkT y -> y

      Here, `y` is used strictly, but the field of `x` certainly is not and
      consequently will not be available unboxed.
      Why not look at the demand of `x` instead to determine whether `y` is
      unboxed? Because the 'idDemandInfo' on `x` will not have been propagated
      to its occurrence in the scrutinee when CprAnal runs directly after
      DmdAnal.

We used to give the case binder the CPR property unconditionally instead of
deriving it from the case scrutinee.
See Historical Note [Optimistic case binder CPR].

Historical Note [Optimistic case binder CPR]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to give the case binder the CPR property unconditionally, which is too
optimistic (#19232). Here are the details:

Inside the alternative, the case binder always has the CPR property, meaning
that a case on it will successfully cancel.
Example:
  f True  x = case x of y { I# x' -> if x' ==# 3
                                     then y
                                     else I# 8 }
  f False x = I# 3
By giving 'y' the CPR property, we ensure that 'f' does too, so we get
  f b x = case fw b x of { r -> I# r }
  fw True  x = case x of y { I# x' -> if x' ==# 3 then x' else 8 }
  fw False x = 3
Of course there is the usual risk of re-boxing: we have 'x' available boxed
and unboxed, but we return the unboxed version for the wrapper to box. If the
wrapper doesn't cancel with its caller, we'll end up re-boxing something that
we did have available in boxed form.

Note [CPR for sum types]
~~~~~~~~~~~~~~~~~~~~~~~~
At the moment we do not do CPR for let-bindings that
   * non-top level
   * bind a sum type
Reason: I found that in some benchmarks we were losing let-no-escapes,
which messed it all up.  Example
   let j = \x. ....
   in case y of
        True  -> j False
        False -> j True
If we w/w this we get
   let j' = \x. ....
   in case y of
        True  -> case j' False of { (# a #) -> Just a }
        False -> case j' True of { (# a #) -> Just a }
Notice that j' is not a let-no-escape any more.

However this means in turn that the *enclosing* function
may be CPR'd (via the returned Justs).  But in the case of
sums, there may be Nothing alternatives; and that messes
up the sum-type CPR.

Conclusion: only do this for products.  It's still not
guaranteed OK for products, but sums definitely lose sometimes.

Note [CPR for thunks]
~~~~~~~~~~~~~~~~~~~~~
If the rhs is a thunk, we usually forget the CPR info, because
it is presumably shared (else it would have been inlined, and
so we'd lose sharing if w/w'd it into a function).  E.g.

        let r = case expensive of
                  (a,b) -> (b,a)
        in ...

If we marked r as having the CPR property, then we'd w/w into

        let $wr = \() -> case expensive of
                            (a,b) -> (# b, a #)
            r = case $wr () of
                  (# b,a #) -> (b,a)
        in ...

But now r is a thunk, which won't be inlined, so we are no further ahead.
But consider

        f x = let r = case expensive of (a,b) -> (b,a)
              in if foo r then r else (x,x)

Does f have the CPR property?  Well, no.

However, if the strictness analyser has figured out (in a previous
iteration) that it's strict, then we DON'T need to forget the CPR info.
Instead we can retain the CPR info and do the thunk-splitting transform
(see WorkWrap.splitThunk).

This made a big difference to PrelBase.modInt, which had something like
        modInt = \ x -> let r = ... -> I# v in
                        ...body strict in r...
r's RHS isn't a value yet; but modInt returns r in various branches, so
if r doesn't have the CPR property then neither does modInt
Another case I found in practice (in Complex.magnitude), looks like this:
                let k = if ... then I# a else I# b
                in ... body strict in k ....
(For this example, it doesn't matter whether k is returned as part of
the overall result; but it does matter that k's RHS has the CPR property.)
Left to itself, the simplifier will make a join point thus:
                let $j k = ...body strict in k...
                if ... then $j (I# a) else $j (I# b)
With thunk-splitting, we get instead
                let $j x = let k = I#x in ...body strict in k...
                in if ... then $j a else $j b
This is much better; there's a good chance the I# won't get allocated.

But what about botCpr? Consider
    lvl = error "boom"
    fac -1 = lvl
    fac 0 = 1
    fac n = n * fac (n-1)
fac won't have the CPR property here when we trim every thunk! But the
assumption is that error cases are rarely entered and we are diverging anyway,
so WW doesn't hurt.

Should we also trim CPR on DataCon application bindings?
See Note [CPR for data structures]!

Note [CPR for data structures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Long static data structures (whether top-level or not) like

  xs = x1 : xs1
  xs1 = x2 : xs2
  xs2 = x3 : xs3

should not get CPR signatures (#18154), because they

  * Never get WW'd, so their CPR signature should be irrelevant after analysis
    (in fact the signature might even be harmful for that reason)
  * Would need to be inlined/expanded to see their constructed product
  * Recording CPR on them blows up interface file sizes and is redundant with
    their unfolding. In case of Nested CPR, this blow-up can be quadratic!
    Reason: the CPR info for xs1 contains the CPR info for xs; the CPR info
    for xs2 contains that for xs1. And so on.

Hence we don't analyse or annotate data structures in 'cprAnalBind'. To
implement this, the isDataStructure guard is triggered for bindings that satisfy

  (1) idArity id == 0 (otherwise it's a function)
  (2) exprIsHNF rhs   (otherwise it's a thunk, Note [CPR for thunks] applies)

But we can't just stop giving DataCon application bindings the CPR *property*,
for example

  fac 0 = I# 1#
  fac n = n * fac (n-1)

fac certainly has the CPR property and should be WW'd! But FloatOut will
transform the first clause to

  lvl = I# 1#
  fac 0 = lvl

If lvl doesn't have the CPR property, fac won't either. But lvl is a data
structure, and hence (see above) will not have a CPR signature. So instead, when
'cprAnal' meets a variable lacking a CPR signature to extrapolate into a CPR
transformer, 'cprTransform' instead tries to get its unfolding (via
'cprDataStructureUnfolding_maybe'), and analyses that instead.

In practice, GHC generates a lot of (nested) TyCon and KindRep bindings, one
for each data declaration. They should not have CPR signatures (blow up!).

There is a perhaps surprising special case: KindRep bindings satisfy
'isDataStructure' (so no CPR signature), but are marked NOINLINE at the same
time (see the noinline wrinkle in Note [Grand plan for Typeable]). So there is
no unfolding for 'cprDataStructureUnfolding_maybe' to look through and we'll
return topCprType. And that is fine! We should refrain to look through NOINLINE
data structures in general, as a constructed product could never be exposed
after WW.

It's also worth pointing out how ad-hoc this is: If we instead had

    f1 x = x:[]
    f2 x = x : f1 x
    f3 x = x : f2 x
    ...

we still give every function an every deepening CPR signature. But it's very
uncommon to find code like this, whereas the long static data structures from
the beginning of this Note are very common because of GHC's strategy of ANF'ing
data structure RHSs.

Note [CPR examples]
~~~~~~~~~~~~~~~~~~~~
Here are some examples (stranal/should_compile/T10482a) of the
usefulness of Note [Optimistic field binder CPR].  The main
point: all of these functions can have the CPR property.

    ------- f1 -----------
    -- x is used strictly by h, so it'll be available
    -- unboxed before it is returned in the True branch

    f1 :: Int -> Int
    f1 x = case h x x of
            True  -> x
            False -> f1 (x-1)

    ------- f3 -----------
    -- h is strict in x, so x will be unboxed before it
    -- is rerturned in the otherwise case.

    data T3 = MkT3 Int Int

    f1 :: T3 -> Int
    f1 (MkT3 x y) | h x y     = f3 (MkT3 x (y-1))
                  | otherwise = x
-}
