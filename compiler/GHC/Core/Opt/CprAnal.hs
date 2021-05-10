
-- | Constructed Product Result analysis. Identifies functions that surely
-- return heap-allocated records on every code path, so that we can eliminate
-- said heap allocation by performing a worker/wrapper split.
--
-- See https://www.microsoft.com/en-us/research/publication/constructed-product-result-analysis-haskell/.
-- CPR analysis should happen after strictness analysis.
-- See Note [Phase ordering].
module GHC.Core.Opt.CprAnal ( cprAnalProgram ) where

import GHC.Prelude

import GHC.Driver.Session

import GHC.Builtin.Names ( runRWKey )

import GHC.Types.Var.Env
import GHC.Types.Basic
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Demand
import GHC.Types.Cpr

import GHC.Core.FamInstEnv
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Core.Utils
import GHC.Core
import GHC.Core.Seq
import GHC.Core.Opt.WorkWrap.Utils

import GHC.Data.Graph.UnVar -- for UnVarSet

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Logger  ( Logger, putDumpFileMaybe, DumpFormat (..) )

import Data.List ( mapAccumL )

{- Note [Constructed Product Result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The goal of Constructed Product Result analysis is to identify functions that
surely return heap-allocated records on every code path, so that we can
eliminate said heap allocation by performing a worker/wrapper split
(via 'GHC.Core.Opt.WorkWrap.Utils.mkWWcpr_entry').

`swap` below is such a function:
```
  swap (a, b) = (b, a)
```
A `case` on an application of `swap`, like
`case swap (10, 42) of (a, b) -> a + b` could cancel away
(by case-of-known-constructor) if we \"inlined\" `swap` and simplified. We then
say that `swap` has the CPR property.

We can't inline recursive functions, but similar reasoning applies there:
```
  f x n = case n of
    0 -> (x, 0)
    _ -> f (x+1) (n-1)
```
Inductively, `case f 1 2 of (a, b) -> a + b` could cancel away the constructed
product with the case. So `f`, too, has the CPR property. But we can't really
"inline" `f`, because it's recursive. Also, non-recursive functions like `swap`
might be too big to inline (or even marked NOINLINE). We still want to exploit
the CPR property, and that is exactly what the worker/wrapper transformation
can do for us:
```
  $wf x n = case n of
    0 -> case (x, 0) of -> (a, b) -> (# a, b #)
    _ -> case f (x+1) (n-1) of (a, b) -> (# a, b #)
  f x n = case $wf x n of (# a, b #) -> (a, b)
```
where $wf readily simplifies (by case-of-known-constructor and inlining `f`) to:
```
  $wf x n = case n of
    0 -> (# x, 0 #)
    _ -> $wf (x+1) (n-1)
```
Now, a call site like `case f 1 2 of (a, b) -> a + b` can inline `f` and
eliminate the heap-allocated pair constructor.

Note [Nested CPR]
~~~~~~~~~~~~~~~~~
We can apply Note [Constructed Product Result] deeper than just the top-level
result constructor of a function, e.g.,
```
  g x
    | even x = (x+1,x+2) :: (Int, Int)
    | odd  x = (x+2,x+3)
```
Not only does `g` return a constructed pair, the pair components /also/ have the
CPR property. We can split `g` for its /nested/ CPR property, as follows:
```
  $wg (x :: Int#)
    | .. x .. = (# x +# 1#, x +# 2# #) :: (# Int#, Int# #)
    | .. x .. = (# x +# 2#, x +# 3# #)
  g (I# x) = case $wf x of (# y, z #) -> (I# y, I# z)
```
Note however that in the following we will only unbox the second component,
even if `foo` has the CPR property:
```
  h x
    | even x = (foo x, x+2) :: (Int, Int)
    | odd  x = (x+2,   x+3)
    -- where `foo` has the CPR property
```
Why can't we also unbox `foo x`? Because in order to do so, we have to evaluate
it and that might diverge, so we cannot give `h` the nested CPR property in the
first component of the result.

The Right Thing is to do a termination analysis, to see if we can guarantee that
`foo` terminates quickly, in which case we can speculatively evaluate `foo x` and
hence give `h` a nested CPR property.  That is done in !1866.  But for now we
have an incredibly simple termination analysis; an expression terminates fast
iff it is in HNF: see `exprTerminates`. We call `exprTerminates` in
`cprTransformDataConWork`, which is the main function figuring out whether it's
OK to propagate nested CPR info (in `extract_nested_cpr`).

In addition to `exprTerminates`, `extract_nested_cpr` also looks at the
`StrictnessMark` of the corresponding constructor field. Example:
```
  data T a = MkT !a
  h2 x
    | even x = MkT (foo x) :: T Int
    | odd  x = MkT (x+2)
    -- where `foo` has the CPR property
```
Regardless of whether or not `foo` terminates, we may unbox the strict field,
because it has to be evaluated (the Core for `MkT (foo x)` will look more like
`case foo x of y { __DEFAULT -> MkT y }`).

Surprisingly, there are local binders with a strict demand that *do not*
terminate quickly in a sense that is useful to us! The following function
demonstrates that:
```
  j x = (let t = x+1 in t+t, 42)
```
Here, `t` is used strictly, *but only within its scope in the first pair
component*. `t` satisfies Note [CPR for binders that will be unboxed], so it has
the CPR property, nevertheless we may not unbox `j` deeply lest evaluation of
`x` diverges. The termination analysis must say "Might diverge" for `t` and we
won't unbox the first pair component.
There are a couple of tests in T18174 that show case Nested CPR. Some of them
only work with the termination analysis from !1866.

Giving the (Nested) CPR property to deep data structures can lead to loss of
sharing; see Note [CPR for data structures can destroy sharing].

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
See Note [CPR for binders that will be unboxed].
An additional w/w pass would simplify things, but probably add slight overhead.
So currently we have

1. Strictness
2. CPR
3. worker/wrapper (for strictness and CPR)
-}

--
-- * Analysing programs
--

cprAnalProgram :: Logger -> FamInstEnvs -> CoreProgram -> IO CoreProgram
cprAnalProgram logger fam_envs binds = do
  let env            = emptyAnalEnv fam_envs
  let binds_plus_cpr = snd $ mapAccumL cprAnalTopBind env binds
  putDumpFileMaybe logger Opt_D_dump_cpr_signatures "Cpr signatures" FormatText $
    dumpIdInfoOfProgram False (ppr . cprSigInfo) binds_plus_cpr
  -- See Note [Stamp out space leaks in demand analysis] in GHC.Core.Opt.DmdAnal
  seqBinds binds_plus_cpr `seq` return binds_plus_cpr

-- Analyse a (group of) top-level binding(s)
cprAnalTopBind :: AnalEnv
               -> CoreBind
               -> (AnalEnv, CoreBind)
cprAnalTopBind env (NonRec id rhs)
  = (env', NonRec id' rhs')
  where
    (id', rhs', env') = cprAnalBind env id rhs

cprAnalTopBind env (Rec pairs)
  = (env', Rec pairs')
  where
    (env', pairs') = cprFix env pairs

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
  = cprAnalApp env e []
cprAnal' env e@(App{})
  = cprAnalApp env e []

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
    (id', rhs', env') = cprAnalBind env id rhs
    (body_ty, body')  = cprAnal env' body

cprAnal' env (Let (Rec pairs) body)
  = body_ty `seq` (body_ty, Let (Rec pairs') body')
  where
    (env', pairs')   = cprFix env pairs
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
      , assert (arity == 0 ) True
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

data TermFlag -- Better than using a Bool
  = Terminates
  | MightDiverge

-- See Note [Nested CPR]
exprTerminates :: CoreExpr -> TermFlag
exprTerminates e
  | exprIsHNF e = Terminates -- A /very/ simple termination analysis.
  | otherwise   = MightDiverge

cprAnalApp :: AnalEnv -> CoreExpr -> [(CprType, CoreArg)] -> (CprType, CoreExpr)
-- Main function that takes care of /nested/ CPR. See Note [Nested CPR]
cprAnalApp env e arg_infos = go e arg_infos []
  where
    go e arg_infos args'
      -- Collect CprTypes for (value) args (inlined collectArgs):
      | App fn arg <- e, isTypeArg arg -- Don't analyse Type args
      = go fn arg_infos (arg:args')
      | App fn arg <- e
      , arg_info@(_arg_ty, arg') <- cprAnal env arg
      -- See Note [Nested CPR] on the need for termination analysis
      = go fn (arg_info:arg_infos) (arg':args')

      | Var fn <- e
      = (cprTransform env fn arg_infos, mkApps e args')

      | (e_ty, e') <- cprAnal env e -- e is not an App and not a Var
      = (applyCprTy e_ty (length arg_infos), mkApps e' args')

cprTransform :: AnalEnv               -- ^ The analysis environment
             -> Id                    -- ^ The function
             -> [(CprType, CoreArg)]  -- ^ info about incoming /value/ arguments
             -> CprType               -- ^ The demand type of the application
cprTransform env id args
  -- Any local binding, except for data structure bindings
  -- See Note [Efficient Top sigs in SigEnv]
  | Just sig <- lookupSigEnv env id
  = applyCprTy (getCprSig sig) (length args)
  -- See Note [CPR for data structures]
  | Just rhs <- cprDataStructureUnfolding_maybe id
  = fst $ cprAnal env rhs
  -- Some (mostly global, known-key) Ids have bespoke CPR transformers
  | Just cpr_ty <- cprTransformBespoke id args
  = cpr_ty
  -- Other local Ids that respond True to 'isDataStructure' but don't have an
  -- expandable unfolding, such as NOINLINE bindings. They all get a top sig
  | isLocalId id
  = assertPpr (isDataStructure id) (ppr id) topCprType
  -- See Note [CPR for DataCon wrappers]
  | isDataConWrapId id, let rhs = uf_tmpl (realIdUnfolding id)
  = fst $ cprAnalApp env rhs args
  -- DataCon worker
  | Just con <- isDataConWorkId_maybe id
  = cprTransformDataConWork (ae_fam_envs env) con args
  -- Imported function
  | otherwise
  = applyCprTy (getCprSig (idCprSig id)) (length args)

-- | Precise, hand-written CPR transformers for select Ids
cprTransformBespoke :: Id -> [(CprType, CoreArg)] -> Maybe CprType
cprTransformBespoke id args
  -- See Note [Simplification of runRW#] in GHC.CoreToStg.Prep
  | idUnique id == runRWKey    -- `runRW (\s -> e)`
  , [(arg_ty, _arg)] <- args   -- `\s -> e` has CPR type `arg` (e.g. `. -> 2`)
  = Just $ applyCprTy arg_ty 1 -- `e` has CPR type `2`
  | otherwise
  = Nothing

-- | Get a (possibly nested) 'CprType' for an application of a 'DataCon' worker,
-- given a saturated number of 'CprType's for its field expressions.
-- Implements the Nested part of Note [Nested CPR].
cprTransformDataConWork :: FamInstEnvs -> DataCon -> [(CprType, CoreArg)] -> CprType
cprTransformDataConWork fam_envs con args
  | null (dataConExTyCoVars con)  -- No existentials
  , wkr_arity <= mAX_CPR_SIZE -- See Note [Trimming to mAX_CPR_SIZE]
  , args `lengthIs` wkr_arity
  , isRecDataCon fam_envs fuel con /= DefinitelyRecursive -- See Note [CPR for recursive data constructors]
  -- , pprTrace "cprTransformDataConWork" (ppr con <+> ppr wkr_arity <+> ppr args) True
  = CprType 0 (ConCpr (dataConTag con) (strictZipWith extract_nested_cpr args wkr_str_marks))
  | otherwise
  = topCprType
  where
    fuel = 3 -- If we can unbox more than 3 constructors to find a
             -- recursive occurrence, then we can just as well unbox it
             -- See Note [CPR for recursive data constructors], point (4)
    wkr_arity = dataConRepArity con
    wkr_str_marks = dataConRepStrictness con
    -- See Note [Nested CPR]
    extract_nested_cpr (CprType 0 cpr, arg) str
      | MarkedStrict <- str              = cpr
      | Terminates <- exprTerminates arg = cpr
    extract_nested_cpr _ _               = topCpr -- intervening lambda or doesn't terminate

-- | See Note [Trimming to mAX_CPR_SIZE].
mAX_CPR_SIZE :: Arity
mAX_CPR_SIZE = 10

--
-- * Bindings
--

-- Recursive bindings
cprFix :: AnalEnv                    -- Does not include bindings for this binding
       -> [(Id,CoreExpr)]
       -> (AnalEnv, [(Id,CoreExpr)]) -- Binders annotated with CPR info
cprFix orig_env orig_pairs
  = loop 1 init_env init_pairs
  where
    init_sig id
      -- See Note [CPR for data structures]
      | isDataStructure id = topCprSig
      | otherwise          = mkCprSig 0 botCpr
    -- See Note [Initialising strictness] in GHC.Core.Opt.DmdAnal
    orig_virgin = ae_virgin orig_env
    init_pairs | orig_virgin  = [(setIdCprSig id (init_sig id), rhs) | (id, rhs) <- orig_pairs ]
               | otherwise    = orig_pairs
    init_env = extendSigEnvFromIds orig_env (map fst init_pairs)

    -- The fixed-point varies the idCprSig field of the binders and and their
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
        found_fixpoint = map (idCprSig . fst) pairs' == map (idCprSig . fst) pairs

    step :: AnalEnv -> [(Id, CoreExpr)] -> (AnalEnv, [(Id, CoreExpr)])
    step env pairs = mapAccumL go env pairs
      where
        go env (id, rhs) = (env', (id', rhs'))
          where
            (id', rhs', env') = cprAnalBind env id rhs

-- | Process the RHS of the binding for a sensible arity, add the CPR signature
-- to the Id, and augment the environment with the signature as well.
cprAnalBind
  :: AnalEnv
  -> Id
  -> CoreExpr
  -> (Id, CoreExpr, AnalEnv)
cprAnalBind env id rhs
  | isDFunId id -- Never give DFuns the CPR property; we'll never save allocs.
  = (id,  rhs,  extendSigEnv env id topCprSig)
  -- See Note [CPR for data structures]
  | isDataStructure id
  = (id,  rhs,  env) -- Data structure => no code => no need to analyse rhs
  | otherwise
  = (id', rhs', env')
  where
    (rhs_ty, rhs')  = cprAnal env rhs
    -- possibly trim thunk CPR info
    rhs_ty'
      -- See Note [CPR for thunks]
      | stays_thunk = trimCprTy rhs_ty
      | otherwise   = rhs_ty
    -- See Note [Arity trimming for CPR signatures]
    sig  = mkCprSigForArity (idArity id) rhs_ty'
    id'  = setIdCprSig id sig
    env' = extendSigEnv env id sig

    -- See Note [CPR for thunks]
    stays_thunk = is_thunk && not_strict
    is_thunk    = not (exprIsHNF rhs) && not (isJoinId id)
    not_strict  = not (isStrUsedDmd (idDemandInfo id))

isDataStructure :: Id -> Bool
-- See Note [CPR for data structures]
isDataStructure id =
  not (isJoinId id) && idArity id == 0 && isEvaldUnfolding (idUnfolding id)

-- | Returns an expandable unfolding
-- (See Note [exprIsExpandable] in "GHC.Core.Utils") that has
-- So effectively is a constructor application.
cprDataStructureUnfolding_maybe :: Id -> Maybe CoreExpr
cprDataStructureUnfolding_maybe id
  -- There are only FinalPhase Simplifier runs after CPR analysis
  | activeInFinalPhase (idInlineActivation id)
  , isDataStructure id
  = expandUnfolding_maybe (idUnfolding id)
  | otherwise
  = Nothing

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

Note [CPR for DataCon wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to give DataCon wrappers a (necessarily flat) CPR signature in
'GHC.Types.Id.Make.mkDataConRep'. Now we transform DataCon wrappers simply by
analysing their unfolding. A few reasons for the change:

  1. DataCon wrappers are generally inlined in the Final phase (so before CPR),
     all leftover occurrences are in a boring context like `f x y = $WMkT y x`.
     It's simpler to analyse the unfolding anew at every such call site, and the
     unfolding will be pretty cheap to analyse. Also they occur seldom enough
     that performance-wise it doesn't matter.
  2. 'GHC.Types.Id.Make' no longer precomputes CPR signatures for DataCon
     *workers*, because their transformers need to adapt to CPR for their
     arguments in 'cprTransformDataConWork' to enable Note [Nested CPR].
     Better keep it all in this module! The alternative would be that
     'GHC.Types.Id.Make' depends on DmdAnal.
  3. In the future, Nested CPR could take a better account of incoming args
     in cprAnalApp and do some beta-reduction on the fly, like !1866 did. If
     any of those args had the CPR property, then we'd even get Nested CPR for
     DataCon wrapper calls, for free. Not so if we simply give the wrapper a
     single CPR sig in 'GHC.Types.Id.Make.mkDataConRep'!

Note [Trimming to mAX_CPR_SIZE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not treat very big tuples as CPR-ish:

  a) For a start, we get into trouble because there aren't
     "enough" unboxed tuple types (a tiresome restriction,
     but hard to fix),
  b) More importantly, big unboxed tuples get returned mainly
     on the stack, and are often then allocated in the heap
     by the caller. So doing CPR for them may in fact make
     things worse, especially if the wrapper doesn't cancel away
     and we move to the stack in the worker and then to the heap
     in the wrapper.

So we (nested) CPR for functions that would otherwise pass more than than
'mAX_CPR_SIZE' fields.
That effect is exacerbated for the unregisterised backend, where we
don't have any hardware registers to return the fields in. Returning
everything on the stack results in much churn and increases compiler
allocation by 15% for T15164 in a validate build.
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
  = foldl' (\env id -> extendSigEnv env id (idCprSig id)) env ids

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
  = extendSigEnv env id (CprSig (argCprType (idDemandInfo id)))

-- | Produces a 'CprType' according to how a strict argument will be unboxed.
-- Examples:
--
--   * A head-strict demand @1!L@ would translate to @1@
--   * A product demand @1!P(1!L,L)@ would translate to @1(1,)@
--   * A product demand @1!P(1L,L)@ would translate to @1(,)@,
--     because the first field will not be unboxed.
argCprType :: Demand -> CprType
argCprType dmd = CprType 0 (go dmd)
  where
    go (n :* sd)
      | isAbs n               = topCpr
      | Prod Unboxed ds <- sd = ConCpr fIRST_TAG (strictMap go ds)
      | Poly Unboxed _  <- sd = ConCpr fIRST_TAG []
      | otherwise             = topCpr

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

  * Whether or not something unboxes is decided by 'wantToUnboxArg', else we may
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

should not get (nested) CPR signatures (#18154), because they

  * Never get WW'd, so their CPR signature should be irrelevant after analysis
    (in fact the signature might even be harmful for that reason)
  * Would need to be inlined/expanded to see their constructed product
  * BUT MOST IMPORTANTLY, Problem P1:
    Recording CPR on them blows up interface file sizes and is redundant with
    their unfolding. In case of Nested CPR, this blow-up can be quadratic!
    Reason: the CPR info for xs1 contains the CPR info for xs; the CPR info
    for xs2 contains that for xs1. And so on.
    By contrast, the size of unfoldings and types stays linear. That's why
    quadratic blowup is problematic; it makes an asymptotic difference.

Hence (Solution S1) we don't give data structure bindings a CPR *signature* and
hence don't to analyse them in 'cprAnalBind'.
What do we mean by "data structure binding"? Answer:

  (1) idArity id == 0    (otherwise it's a function)
  (2) is eval'd          (otherwise it's a thunk, Note [CPR for thunks] applies)
  (3) not (isJoinId id)  (otherwise it's a function and its more efficient to
                          analyse it just once rather than at each call site)

But (S1) leads to a new Problem P2: We can't just stop giving DataCon application
bindings the CPR *property*, for example the factorial function after FloatOut

  lvl = I# 1#
  fac 0 = lvl
  fac n = n * fac (n-1)

lvl is a data structure, and hence (see above) will not have a CPR *signature*.
But if lvl doesn't have the CPR *property*, fac won't either and we allocate a
box for the result on every iteration of the loop.

So (Solution S2) when 'cprAnal' meets a variable lacking a CPR signature to
extrapolate into a CPR transformer, 'cprTransform' tries to get its unfolding
(via 'cprDataStructureUnfolding_maybe'), and analyses that instead.

The Result R1: Everything behaves as if there was a CPR signature, but without
the blowup in interface files.

There is one exception to (R1):

  x   = (y, z); {-# NOINLINE x #-}
  f p = (y, z); {-# NOINLINE f #-}

While we still give the NOINLINE *function* 'f' the CPR property (and WW
accordingly, see Note [Worker/wrapper for NOINLINE functions]), we won't
give the NOINLINE *data structure* 'x' the CPR property, because it lacks an
unfolding. In particular, KindRep bindings are NOINLINE data structures (see
the noinline wrinkle in Note [Grand plan for Typeable]). We'll behave as if the
bindings had 'topCprSig', and that is fine, as a case on the binding would never
cancel away after WW!

It's also worth pointing out how ad-hoc (S1) is: If we instead had

    f1 x = x:[]
    f2 x = x : f1 x
    f3 x = x : f2 x
    ...

we still give every function an ever deepening CPR signature. But it's very
uncommon to find code like this, whereas the long static data structures from
the beginning of this Note are very common because of GHC's strategy of ANF'ing
data structure RHSs.

Note [CPR for data structures can destroy sharing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Note [CPR for data structures], we argued that giving data structure bindings
the CPR property is useful to give functions like fac the CPR property:

  lvl = I# 1#
  fac 0 = lvl
  fac n = n * fac (n-1)

Worker/wrappering fac for its CPR property means we get a very fast worker
function with type Int# -> Int#, without any heap allocation at all.

But consider what happens if we call `map fac (replicate n 0)`, where the
wrapper doesn't cancel away: Then we rebox the result of $wfac *on each call*,
n times, instead of reusing the static thunk for 1, e.g. an asymptotic increase
in allocations. If you twist it just right, you can actually write programs that
that take O(n) space if you do CPR and O(1) if you don't:

  fac :: Int -> Int
  fac 0 = 1 -- this clause will trigger CPR and destroy sharing for O(n) space
  -- fac 0 = lazy 1 -- this clause will prevent CPR and run in O(1) space
  fac n = n * fac (n-1)

  const0 :: Int -> Int
  const0 n = signum n - 1 -- will return 0 for [1..n]
  {-# NOINLINE const0 #-}

  main = print $ foldl' (\acc n -> acc + lazy n) 0 $ map (fac . const0) [1..100000000]

Generally, this kind of asymptotic increase in allocation can happen whenever we
give a data structure the CPR property that is bound outside of a recursive
function. So far we don't have a convincing remedy; giving fac the CPR property
is just too attractive. #19309 documents a futile idea. #13331 tracks the
general issue of WW destroying sharing and also contains above reproducer.
#19326 is about CPR destroying sharing in particular.

With Nested CPR, sharing can also be lost within the same "lambda level", for
example:

  f (I# x) = let y = I# (x*#x) in (y, y)

Nestedly unboxing would destroy the box shared through 'y'. (Perhaps we can call
this "internal sharing", in contrast to "external sharing" beyond lambda or even
loop levels above.) But duplicate occurrences like that are pretty rare and may
never lead to an asymptotic difference in allocations of 'f'.

Note [CPR for recursive data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note [CPR for data structures can destroy sharing] gives good reasons not to
give shared data structure bindings the CPR property. But we shouldn't even
give *functions* that return *recursive* data constructor applications the CPR
property. Here's an example for why:

  c = C# 'a'
  replicateC :: Int -> [Int]
  replicateC 1 = [c]
  replicateC n = c : replicateC (n-1)

What happens if we give `replicateC` the (nested) CPR property? We get a WW
split for 'replicateC', the wrapper of which is certain to inline, like this:

  replicateC (I# n) = case $wreplicateC n of (# x, xs #) -> C# x : xs
  $wreplicateC 1# = (# 'a', [] #)
  $wreplicateC n  = (# 'a', replicateC (I# (n -# 1#)) #)

Eliminating the shared 'c' binding in the process. And then

  * We *might* save allocation of the topmost (of most likely several) (:)
    constructor if it cancels away at the call site. Similarly for the 'C#'
    constructor.
  * But we will now re-allocate the C# box on every iteration of the loop,
    because we separated the character literal from the C# application.
    That means n times as many C# allocations as before. Yikes!!
  * We make all other call sites where the wrapper inlines a bit larger, most of
    them for no gain. But this shouldn't matter much.
  * The inlined wrapper may inhibit eta-expansion in some cases. Here's how:
    If the wrapper is inlined in a strict arg position, the Simplifier will
    transform as follows

      f (replicateC n)
      ==> { inline }
      f (case $wreplicateC n of (# x, xs #) -> (C# x, xs))
      ==> { strict arg }
      case $wreplicateC n of (# x, xs #) -> f (C# x, xs)

    Now we can't float out the case anymore. In fact, we can't even float out
    `$wreplicateC n`, because it returns an unboxed tuple.
    This can inhibit eta-expansion if we later find out that `f` has arity > 1
    (such as when we define `foldl` in terms of `foldr`). #19970 shows how
    abstaining from worker/wrappering made a difference of -20% in reptile. So
    while WW'ing for CPR didn't make the program slower directly, the resulting
    program got much harder to optimise because of the returned unboxed tuple
    (which can't easily float because unlifted).

`replicateC` comes up in T5536, which regresses significantly if CPR'd nestedly.

What can we do about it?

 A. Don't CPR functions that return a *recursive data type* (the list in this
    case). This is the solution we adopt. Rationale: the benefit of CPR on
    recursive data structures is slight, because it only affects the outer layer
    of a potentially massive data structure.
 B. Don't CPR any *recursive function*. That would be quite conservative, as it
    would also affect e.g. the factorial function.
 C. Flat CPR only for recursive functions. This prevents the asymptotic
    worsening part arising through unsharing the C# box, but it's still quite
    conservative.
 D. No CPR at occurrences of shared data structure in hot paths (e.g. the use of
    `c` in the second eqn of `replicateC`). But we'd need to know which paths
    were hot. We want such static branch frequency estimates in #20378.

We adopt solution (A) It is ad-hoc, but appears to work reasonably well.
Deciding what a "recursive data constructor" is is quite tricky and ad-hoc, too:
See Note [Detecting recursive data constructors]. We don't have to be perfect
and can simply keep on unboxing if unsure.

Note [Detecting recursive data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What qualifies as a "recursive data constructor" as per
Note [CPR for recursive data constructors]? That is up to
'GHC.Core.Opt.WorkWrapW.Utils.isRecDataCon' to decide. It does a DFS search over
the field types of the DataCon and looks for term-level recursion into the data
constructor's type constructor. A few perhaps surprising points:

  1. It deems any function type as non-recursive, because it's unlikely that
     a recursion through a function type builds up a recursive data structure.
  2. It doesn't look into kinds or coercion types because there's nothing to unbox.
     Same for promoted data constructors.
  3. We don't care whether a NewTyCon or DataTyCon App is fully saturated or not;
     we simply look at its definition/DataCons and its field tys. Any recursive arg
     occs will have been detected before (see the invariant of 'go_tc_app').
     This is so that we expand the `ST` in `StateT Int (ST s) a`.
  4. We don't recurse deeper than 3 (at the moment of this writing) TyCons and
     assume the DataCon is non-recursive after that. One reason is guaranteed
     constant-time efficiency; the other is that it's fair to say that a recursion
     over 3 or more TyCons doesn't really count as a list-like data structure
     anymore and a bit of unboxing doesn't hurt much.
  5. It checks TyConApps like `T <huge> <type>` by eagerly checking the
     potentially huge argument types *before* it tries to expand the
     DataCons/NewTyCon/TyFams/etc. so that it doesn't need to re-check those
     argument types after having been substituted into every occurrence of
     the the respective TyCon parameter binders. It's like call-by-value vs.
     call-by-name: Eager checking of argument types means we only need to check
     them exactly once.
     There's one exception to that rule, namely when we are able to reduce a
     TyFam by considering argument types. Then we pay the price of potentially
     checking the same type arg twice (or more, if the TyFam is recursive).
     It should hardly matter.
  6. As a result of keeping the implementation simple, it says "recursive"
     for `data T = MkT [T]`, even though we could argue that the inner recursion
     (through the `[]` TyCon) by way of which `T` is recursive will already be
     "broken" and thus never unboxed. Consequently, it might be OK to CPR a
     function returning `T`. Lacking arguments for or against the current simple
     behavior, we stick to it.
  7. When the search hits an abstract TyCon (one without visible DataCons, e.g.,
     from an .hs-boot file), it returns 'Nothing' for "inconclusive", the same
     as when we run out of fuel. If there is ever a recursion through an
     abstract TyCon, then it's not part of the same function we are looking at,
     so we can treat it as if it wasn't recursive.

Here are a few examples of data constructors or data types with a single data
con and the answers of our function:

  data T = T (Int, (Bool, Char))               NonRec
  (:)                                          Rec
  []                                           NonRec
  data U = U [Int]                             NonRec
  data U2 = U2 [U2]                            Rec     (see point (6))
  data T1 = T1 T2; data T2 = T2 T1             Rec
  newtype Fix f = Fix (f (Fix f))              Rec
  data N = N (Fix (Either Int))                NonRec
  data M = M (Fix (Either M))                  Rec
  data F = F (F -> Int)                        NonRec  (see point (1))
  data G = G (Int -> G)                        NonRec  (see point (1))
  newtype MyM s a = MyM (StateT Int (ST s) a   NonRec
  type S = (Int, Bool)                         NonRec

  { type family E a where
      E Int = Char
      E (a,b) = (E a, E b)
      E Char = Blub
    data Blah = Blah (E (Int, (Int, Int)))     NonRec  (see point (5))
    data Blub = Blub (E (Char, Int))           Rec
    data Blub2 = Blub2 (E (Bool, Int))     }   Rec, because stuck

  { data T1 = T1 T2; data T2 = T2 T3;
    ... data T5 = T5 T1                    }   Nothing (out of fuel)  (see point (4))

  { module A where -- A.hs-boot
      data T
    module B where
      import {-# SOURCE #-} A
      data U = MkU T
      f :: T -> U
      f t = MkU t                              Nothing (T is abstract)  (see point (7))
    module A where -- A.hs
      import B
      data T = MkT U }

These examples are tested by the testcase RecDataConCPR.

I've played with the idea to make points (1) through (3) of 'isRecDataCon'
configurable like (4) to enable more re-use throughout the compiler, but haven't
found a killer app for that yet, so ultimately didn't do that.

Note [CPR examples]
~~~~~~~~~~~~~~~~~~~
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

Lacking Nested CPR (hence this Note is historic now that we have Nested CPR), we
have to guess a bit, by looking for

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
See Historic Note [Optimistic case binder CPR].

Historic Note [Optimistic case binder CPR]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

-}
