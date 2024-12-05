{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


                        -----------------
                        A demand analysis
                        -----------------
-}


module GHC.Core.Opt.DmdAnal
   ( DmdAnalOpts(..)
   , dmdAnalProgram
   )
where

import GHC.Prelude

import GHC.Types.Demand   -- All of it

import GHC.Core
import GHC.Core.DataCon
import GHC.Core.Utils
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Predicate( isEqualityClass, isCTupleClass )
import GHC.Core.FVs      ( rulesRhsFreeIds, bndrRuleAndUnfoldingIds )
import GHC.Core.Coercion ( Coercion )
import GHC.Core.TyCo.FVs     ( coVarsOfCos )
import GHC.Core.TyCo.Compare ( eqType )
import GHC.Core.Multiplicity ( scaledThing )
import GHC.Core.FamInstEnv
import GHC.Core.Opt.Arity ( typeArity )
import GHC.Core.Opt.WorkWrap.Utils

import GHC.Builtin.Names
import GHC.Builtin.PrimOps
import GHC.Builtin.Types.Prim ( realWorldStatePrimTy )

import GHC.Types.Unique.Set
import GHC.Types.Unique.MemoFun
import GHC.Types.RepType
import GHC.Types.ForeignCall ( isSafeForeignCall )
import GHC.Types.Id
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Basic

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable

import Data.List        ( mapAccumL )

{-
************************************************************************
*                                                                      *
\subsection{Top level stuff}
*                                                                      *
************************************************************************
-}

-- | Options for the demand analysis
data DmdAnalOpts = DmdAnalOpts
   { dmd_strict_dicts    :: !Bool
   -- ^ Value of `-fdicts-strict` (on by default).
   -- When set, all functons are implicitly strict in dictionary args.
   , dmd_do_boxity       :: !Bool
   -- ^ Governs whether the analysis should update boxity signatures.
   -- See Note [Don't change boxity without worker/wrapper].
   , dmd_unbox_width     :: !Int
   -- ^ Value of `-fdmd-unbox-width`.
   -- See Note [Unboxed demand on function bodies returning small products]
   , dmd_max_worker_args :: !Int
   -- ^ Value of `-fmax-worker-args`.
   -- Don't unbox anything if we end up with more than this many args.
   }

-- This is a strict alternative to (,)
-- See Note [Space Leaks in Demand Analysis]
data WithDmdType a = WithDmdType !DmdType !a

getAnnotated :: WithDmdType a -> a
getAnnotated (WithDmdType _ a) = a

data DmdResult a b = R !a !b

-- | Outputs a new copy of the Core program in which binders have been annotated
-- with demand and strictness information.
--
-- Note: use `seqBinds` on the result to avoid leaks due to laziness (cf Note
-- [Stamp out space leaks in demand analysis])
dmdAnalProgram :: DmdAnalOpts -> FamInstEnvs -> [CoreRule] -> CoreProgram -> CoreProgram
dmdAnalProgram opts fam_envs rules binds
  = getAnnotated $ go (emptyAnalEnv opts fam_envs) binds
  where
    -- See Note [Analysing top-level bindings]
    -- and Note [Why care for top-level demand annotations?]
    go _   []     = WithDmdType nopDmdType []
    go env (b:bs) = cons_up $ dmdAnalBind TopLevel env topSubDmd b anal_body
      where
        anal_body env'
          | WithDmdType body_ty bs' <- go env' bs
          = WithDmdType (body_ty `plusDmdType` keep_alive_roots env' (bindersOf b)) bs'

    cons_up :: WithDmdType (DmdResult b [b]) -> WithDmdType [b]
    cons_up (WithDmdType dmd_ty (R b' bs')) = WithDmdType dmd_ty (b' : bs')

    keep_alive_roots :: AnalEnv -> [Id] -> DmdEnv
    -- See Note [Absence analysis for stable unfoldings and RULES]
    -- Here we keep alive "roots", e.g., exported ids and stuff mentioned in
    -- orphan RULES
    keep_alive_roots env ids = plusDmdEnvs (map (demandRoot env) (filter is_root ids))

    is_root :: Id -> Bool
    is_root id = isExportedId id || elemVarSet id rule_fvs

    rule_fvs :: IdSet
    rule_fvs = rulesRhsFreeIds rules

demandRoot :: AnalEnv -> Id -> DmdEnv
-- See Note [Absence analysis for stable unfoldings and RULES]
demandRoot env id = fst (dmdAnalStar env topDmd (Var id))

demandRoots :: AnalEnv -> [Id] -> DmdEnv
-- See Note [Absence analysis for stable unfoldings and RULES]
demandRoots env roots = plusDmdEnvs (map (demandRoot env) roots)

demandRootSet :: AnalEnv -> IdSet -> DmdEnv
demandRootSet env ids = demandRoots env (nonDetEltsUniqSet ids)
  -- It's OK to use nonDetEltsUniqSet here because plusDmdType is commutative

-- | We attach useful (e.g. not 'topDmd') 'idDemandInfo' to top-level bindings
-- that satisfy this function.
--
-- Basically, we want to know how top-level *functions* are *used*
-- (e.g. called). The information will always be lazy.
-- Any other top-level bindings are boring.
--
-- See also Note [Why care for top-level demand annotations?].
isInterestingTopLevelFn :: Id -> Bool
-- SG tried to set this to True and got a +2% ghc/alloc regression in T5642
-- (which is dominated by the Simplifier) at no gain in analysis precision.
-- If there was a gain, that regression might be acceptable.
-- Plus, we could use LetUp for thunks and share some code with local let
-- bindings.
isInterestingTopLevelFn id = typeArity (idType id) > 0

{- Note [Stamp out space leaks in demand analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The demand analysis pass outputs a new copy of the Core program in
which binders have been annotated with demand and strictness
information. It's tiresome to ensure that this information is fully
evaluated everywhere that we produce it, so we just run a single
seqBinds over the output before returning it, to ensure that there are
no references holding on to the input Core program.

This makes a ~30% reduction in peak memory usage when compiling
DynFlags (cf #9675 and #13426).

This is particularly important when we are doing late demand analysis,
since we don't do a seqBinds at any point thereafter. Hence code
generation would hold on to an extra copy of the Core program, via
unforced thunks in demand or strictness information; and it is the
most memory-intensive part of the compilation process, so this added
seqBinds makes a big difference in peak memory usage.

Note [Don't change boxity without worker/wrapper]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (T21754)
  f n = n+1
  {-# NOINLINE f #-}
With `-fno-worker-wrapper`, we should not give `f` a boxity signature that says
that it unboxes its argument! Client modules would never be able to cancel away
the box for n. Likewise we shouldn't give `f` the CPR property.

Similarly, in the last run of DmdAnal before codegen (which does not have a
worker/wrapper phase) we should not change boxity in any way. Remember: an
earlier result of the demand analyser, complete with worker/wrapper, has aleady
given a demand signature (with boxity info) to the function.
(The "last run" is mainly there to attach demanded-once info to let-bindings.)

In general, we should not run Note [Boxity analysis] unless worker/wrapper
follows to exploit the boxity and make sure that calling modules can observe the
reported boxity.

Hence DmdAnal is configured by a flag `dmd_do_boxity` that is True only
if worker/wrapper follows after DmdAnal. If it is not set, and the signature
is not subject to Note [Boxity for bottoming functions], DmdAnal tries
to transfer over the previous boxity to the new demand signature, in
`setIdDmdAndBoxSig`.

Why isn't CprAnal configured with a similar flag? Because if we aren't going to
do worker/wrapper we don't run CPR analysis at all. (see GHC.Core.Opt.Pipeline)

It might be surprising that we only try to preserve *arg* boxity, not boxity on
FVs. But FV demands won't make it into interface files anyway, so it's a waste
of energy.
Besides, W/W zaps the `DmdEnv` portion of a signature, so we don't know the old
boxity to begin with; see Note [Zapping DmdEnv after Demand Analyzer].

Note [Analysing top-level bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a CoreProgram like
  e1 = ...
  n1 = ...
  e2 = \a b -> ... fst (n1 a b) ...
  n2 = \c d -> ... snd (e2 c d) ...
  ...
where e* are exported, but n* are not.
Intuitively, we can see that @n1@ is only ever called with two arguments
and in every call site, the first component of the result of the call
is evaluated. Thus, we'd like it to have idDemandInfo @LC(L,C(M,P(1L,A))@.
NB: We may *not* give e2 a similar annotation, because it is exported and
external callers might use it in arbitrary ways, expressed by 'topDmd'.
This can then be exploited by Nested CPR and eta-expansion,
see Note [Why care for top-level demand annotations?].

How do we get this result? Answer: By analysing the program as if it was a let
expression of this form:
  let e1 = ... in
  let n1 = ... in
  let e2 = ... in
  let n2 = ... in
  (e1,e2, ...)
E.g. putting all bindings in nested lets and returning all exported binders in a tuple.
Of course, we will not actually build that CoreExpr! Instead we faithfully
simulate analysis of said expression by adding the free variable 'DmdEnv'
of @e*@'s strictness signatures to the 'DmdType' we get from analysing the
nested bindings.

And even then the above form blows up analysis performance in T10370:
If @e1@ uses many free variables, we'll unnecessarily carry their demands around
with us from the moment we analyse the pair to the moment we bubble back up to
the binding for @e1@. So instead we analyse as if we had
  let e1 = ... in
  (e1, let n1 = ... in
  (    let e2 = ... in
  (e2, let n2 = ... in
  (    ...))))
That is, a series of right-nested pairs, where the @fst@ are the exported
binders of the last enclosing let binding and @snd@ continues the nested
lets.

Variables occurring free in RULE RHSs are to be handled the same as exported Ids.
See also Note [Absence analysis for stable unfoldings and RULES].

Note [Why care for top-level demand annotations?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Reading Note [Analysing top-level bindings], you might think that we go through
quite some trouble to get useful demands for top-level bindings. They can never
be strict, for example, so why bother?

First, we get to eta-expand top-level bindings that we weren't able to
eta-expand before without Call Arity. From T18894b:
  module T18894b (f) where
  eta :: Int -> Int -> Int
  eta x = if fst (expensive x) == 13 then \y -> ... else \y -> ...
  f m = ... eta m 2 ... eta 2 m ...
Since only @f@ is exported, we see all call sites of @eta@ and can eta-expand to
arity 2.

The call demands we get for some top-level bindings will also allow Nested CPR
to unbox deeper. From T18894:
  module T18894 (h) where
  g m n = (2 * m, 2 `div` n)
  {-# NOINLINE g #-}
  h :: Int -> Int
  h m = ... snd (g m 2) ... uncurry (+) (g 2 m) ...
Only @h@ is exported, hence we see that @g@ is always called in contexts were we
also force the division in the second component of the pair returned by @g@.
This allows Nested CPR to evaluate the division eagerly and return an I# in its
position.
-}

{-
************************************************************************
*                                                                      *
\subsection{The analyser itself}
*                                                                      *
************************************************************************
-}

-- | Analyse a binding group and its \"body\", e.g. where it is in scope.
--
-- It calls a function that knows how to analyse this \"body\" given
-- an 'AnalEnv' with updated demand signatures for the binding group
-- (reflecting their 'idDmdSigInfo') and expects to receive a
-- 'DmdType' in return, which it uses to annotate the binding group with their
-- 'idDemandInfo'.
dmdAnalBind
  :: TopLevelFlag
  -> AnalEnv
  -> SubDemand                 -- ^ Demand put on the "body"
                               --   (important for join points)
  -> CoreBind
  -> (AnalEnv -> WithDmdType a) -- ^ How to analyse the "body", e.g.
                               --   where the binding is in scope
  -> WithDmdType (DmdResult CoreBind a)
dmdAnalBind top_lvl env dmd bind anal_body = case bind of
  NonRec id rhs
    | not (isId id)
    -> dmdAnalBindType            env         id bind anal_body
    | useLetUp top_lvl id
    -> dmdAnalBindLetUp   top_lvl env_rhs     id rhs anal_body
  _ -> dmdAnalBindLetDown top_lvl env_rhs dmd bind   anal_body
  where
    env_rhs = enterDFun bind env

-- | Annotates uninteresting top level functions ('isInterestingTopLevelFn')
-- with 'topDmd', the rest with the given demand.
setBindIdDemandInfo :: TopLevelFlag -> Id -> Demand -> Id
setBindIdDemandInfo top_lvl id dmd = setIdDemandInfo id $ case top_lvl of
  TopLevel | not (isInterestingTopLevelFn id) -> topDmd
  _                                           -> dmd

-- | Update the demand signature, but be careful not to change boxity info if
-- `dmd_do_boxity` is True or if the signature is bottom.
-- See Note [Don't change boxity without worker/wrapper]
-- and Note [Boxity for bottoming functions].
setIdDmdAndBoxSig :: DmdAnalOpts -> Id -> DmdSig -> Id
setIdDmdAndBoxSig opts id sig = setIdDmdSig id $
  if dmd_do_boxity opts || isBottomingSig sig
    then sig
    else transferArgBoxityDmdSig (idDmdSig id) sig

dmdAnalBindType :: AnalEnv
                -> Var
                -> CoreBind
                -> (AnalEnv -> WithDmdType a)
                -> WithDmdType (DmdResult CoreBind a)
dmdAnalBindType env v bind anal_body
  | WithDmdType ty a <- anal_body (addInScopeAnalEnv env v)
  = WithDmdType ty (R bind a)

-- | Let bindings can be processed in two ways:
-- Down (RHS before body) or Up (body before RHS).
-- This function handles the up variant.
--
-- It is very simple. For  let x = rhs in body
--   * Demand-analyse 'body' in the current environment
--   * Find the demand, 'rhs_dmd' placed on 'x' by 'body'
--   * Demand-analyse 'rhs' in 'rhs_dmd'
--
-- This is used for a non-recursive local let without manifest lambdas (see
-- 'useLetUp').
--
-- This is the LetUp rule in the paper “Higher-Order Cardinality Analysis”.
dmdAnalBindLetUp :: TopLevelFlag
                 -> AnalEnv
                 -> Id
                 -> CoreExpr
                 -> (AnalEnv -> WithDmdType a)
                 -> WithDmdType (DmdResult CoreBind a)
dmdAnalBindLetUp top_lvl env id rhs anal_body = WithDmdType final_ty (R (NonRec id' rhs') (body'))
  where
    WithDmdType body_ty body'   = anal_body (addInScopeAnalEnv env id)
    -- See Note [Bringing a new variable into scope]
    WithDmdType body_ty' id_dmd = findBndrDmd env body_ty id
    -- See Note [Finalising boxity for demand signatures]

    id_dmd'            = finaliseLetBoxity env (idType id) id_dmd
    !id'               = setBindIdDemandInfo top_lvl id id_dmd'
    (rhs_ty, rhs')     = dmdAnalStar env id_dmd' rhs

    -- See Note [Absence analysis for stable unfoldings and RULES]
    rule_fvs           = bndrRuleAndUnfoldingIds id
    final_ty           = body_ty' `plusDmdType` rhs_ty `plusDmdType` demandRootSet env rule_fvs

-- | Let bindings can be processed in two ways:
-- Down (RHS before body) or Up (body before RHS).
-- This function handles the down variant.
--
-- It computes a demand signature (by means of 'dmdAnalRhsSig') and uses
-- that at call sites in the body.
--
-- It is used for toplevel definitions, recursive definitions and local
-- non-recursive definitions that have manifest lambdas (cf. 'useLetUp').
-- Local non-recursive definitions without a lambda are handled with LetUp.
--
-- This is the LetDown rule in the paper “Higher-Order Cardinality Analysis”.
dmdAnalBindLetDown :: TopLevelFlag -> AnalEnv -> SubDemand -> CoreBind -> (AnalEnv -> WithDmdType a) -> WithDmdType (DmdResult CoreBind a)
dmdAnalBindLetDown top_lvl env dmd bind anal_body = case bind of
  NonRec id rhs
    | (env', weak_fv, id1, rhs1) <-
        dmdAnalRhsSig top_lvl NonRecursive env dmd id rhs
    -> do_rest env' weak_fv [(id1, rhs1)] (uncurry NonRec . only)
  Rec pairs
    | (env', weak_fv, pairs') <- dmdFix top_lvl env dmd pairs
    -> do_rest env' weak_fv pairs' Rec
  where
    do_rest env' weak_fv pairs1 build_bind = WithDmdType final_ty (R (build_bind pairs2) body')
      where
        WithDmdType body_ty body'        = anal_body env'
        -- see Note [Lazy and unleashable free variables]
        dmd_ty                          = addWeakFVs body_ty weak_fv
        WithDmdType final_ty id_dmds    = findBndrsDmds env' dmd_ty (strictMap fst pairs1)
        -- Important to force this as build_bind might not force it.
        !pairs2                         = strictZipWith do_one pairs1 id_dmds
        do_one (id', rhs') dmd          = ((,) $! setBindIdDemandInfo top_lvl id' dmd) $! rhs'
        -- If the actual demand is better than the vanilla call
        -- demand, you might think that we might do better to re-analyse
        -- the RHS with the stronger demand.
        -- But (a) That seldom happens, because it means that *every* path in
        --         the body of the let has to use that stronger demand
        -- (b) It often happens temporarily in when fixpointing, because
        --     the recursive function at first seems to place a massive demand.
        --     But we don't want to go to extra work when the function will
        --     probably iterate to something less demanding.
        -- In practice, all the times the actual demand on id2 is more than
        -- the vanilla call demand seem to be due to (b).  So we don't
        -- bother to re-analyse the RHS.

-- | Mimic the effect of 'GHC.Core.Prep.mkFloat', turning non-trivial argument
-- expressions/RHSs into a proper let-bound thunk (lifted) or a case (with
-- unlifted scrutinee).
anticipateANF :: CoreExpr -> Card -> Card
anticipateANF e n
  | exprIsTrivial e                               = n -- trivial expr won't have a binding
  | definitelyUnliftedType (exprType e)
  , not (isAbs n && exprOkForSpeculation e)       = case_bind n
  | otherwise                                     = let_bind  n
  where
    case_bind _ = C_11       -- evaluated exactly once
    let_bind    = oneifyCard -- evaluated at most once

-- Do not process absent demands
-- Otherwise act like in a normal demand analysis
-- See ↦* relation in the Cardinality Analysis paper
dmdAnalStar :: AnalEnv
            -> Demand   -- This one takes a *Demand*
            -> CoreExpr
            -> (DmdEnv, CoreExpr)
dmdAnalStar env (n :* sd) e
  -- NB: (:*) expands AbsDmd and BotDmd as needed
  | WithDmdType dmd_ty e' <- dmdAnal env sd e
  , n' <- anticipateANF e n
      -- See Note [Anticipating ANF in demand analysis]
      -- and Note [Analysing with absent demand]
  = (multDmdEnv n' (discardArgDmds dmd_ty), e')

-- Main Demand Analysis machinery
dmdAnal, dmdAnal' :: AnalEnv
        -> SubDemand         -- The main one takes a *SubDemand*
        -> CoreExpr -> WithDmdType CoreExpr

dmdAnal env d e = -- pprTrace "dmdAnal" (ppr d <+> ppr e) $
                  dmdAnal' env d e

dmdAnal' _ _ (Lit lit)     = WithDmdType nopDmdType (Lit lit)
dmdAnal' _ _ (Type ty)     = WithDmdType nopDmdType (Type ty) -- Doesn't happen, in fact
dmdAnal' _ _ (Coercion co)
  = WithDmdType (noArgsDmdType (coercionDmdEnv co)) (Coercion co)

dmdAnal' env dmd (Var var)
  = WithDmdType (dmdTransform env var dmd) (Var var)

dmdAnal' env dmd (Cast e co)
  = WithDmdType (dmd_ty `plusDmdType` coercionDmdEnv co) (Cast e' co)
  where
    WithDmdType dmd_ty e' = dmdAnal env dmd e

dmdAnal' env dmd (Tick t e)
  = WithDmdType dmd_ty (Tick t e')
  where
    WithDmdType dmd_ty e' = dmdAnal env dmd e

dmdAnal' env dmd (App fun (Type ty))
  = WithDmdType fun_ty (App fun' (Type ty))
  where
    WithDmdType fun_ty fun' = dmdAnal env dmd fun

-- Lots of the other code is there to make this
-- beautiful, compositional, application rule :-)
dmdAnal' env dmd (App fun arg)
  = -- This case handles value arguments (type args handled above)
    -- Crucially, coercions /are/ handled here, because they are
    -- value arguments (#10288)
    let
        call_dmd          = mkCalledOnceDmd dmd
        WithDmdType fun_ty fun' = dmdAnal env call_dmd fun
        (arg_dmd, res_ty) = splitDmdTy fun_ty
        (arg_ty, arg')    = dmdAnalStar env arg_dmd arg
    in
--    pprTrace "dmdAnal:app" (vcat
--         [ text "dmd =" <+> ppr dmd
--         , text "expr =" <+> ppr (App fun arg)
--         , text "fun dmd_ty =" <+> ppr fun_ty
--         , text "arg dmd =" <+> ppr arg_dmd
--         , text "arg dmd_ty =" <+> ppr arg_ty
--         , text "res dmd_ty =" <+> ppr res_ty
--         , text "overall res dmd_ty =" <+> ppr (res_ty `plusDmdType` arg_ty) ])
    WithDmdType (res_ty `plusDmdType` arg_ty) (App fun' arg')

dmdAnal' env dmd (Lam var body)
  | isTyVar var
  = let
        WithDmdType body_ty body' = dmdAnal (addInScopeAnalEnv env var) dmd body
        -- See Note [Bringing a new variable into scope]
    in
    WithDmdType body_ty (Lam var body')

  | otherwise
  = let (n, body_dmd)    = peelCallDmd dmd
          -- body_dmd: a demand to analyze the body

        WithDmdType body_ty body' = dmdAnal (addInScopeAnalEnv env var) body_dmd body
        -- See Note [Bringing a new variable into scope]
        WithDmdType lam_ty var'   = annotateLamIdBndr env body_ty var
        new_dmd_type = multDmdType n lam_ty
    in
    WithDmdType new_dmd_type (Lam var' body')

dmdAnal' env dmd (Case scrut case_bndr ty [Alt alt_con bndrs rhs])
  -- Only one alternative.
  -- If it's a DataAlt, it should be the only constructor of the type and we
  -- can consider its field demands when analysing the scrutinee.
  | want_precise_field_dmds alt_con
  = let
        rhs_env = addInScopeAnalEnvs env (case_bndr:bndrs)
        -- See Note [Bringing a new variable into scope]
        WithDmdType rhs_ty rhs'           = dmdAnal rhs_env dmd rhs
        WithDmdType alt_ty1 fld_dmds      = findBndrsDmds env rhs_ty bndrs
        WithDmdType alt_ty2 case_bndr_dmd = findBndrDmd env alt_ty1 case_bndr
        !case_bndr'                       = setIdDemandInfo case_bndr case_bndr_dmd

        -- Evaluation cardinality on the case binder is irrelevant and a no-op.
        -- What matters is its nested sub-demand!
        -- NB: If case_bndr_dmd is absDmd, boxity will say Unboxed, which is
        -- what we want, because then `seq` will put a `seqDmd` on its scrut.
        (_ :* case_bndr_sd) = strictifyDmd case_bndr_dmd

        -- Compute demand on the scrutinee
        -- FORCE the result, otherwise thunks will end up retaining the
        -- whole DmdEnv
        !(!bndrs', !scrut_sd)
          | DataAlt _ <- alt_con
          -- See Note [Demand on the scrutinee of a product case]
          , let !scrut_sd = scrutSubDmd case_bndr_sd fld_dmds
          -- See Note [Demand on case-alternative binders]
          , let !fld_dmds' = fieldBndrDmds scrut_sd (length fld_dmds)
          , let !bndrs' = setBndrsDemandInfo bndrs fld_dmds'
          = (bndrs', scrut_sd)
          | otherwise
          -- DEFAULT alts. Simply add demands and discard the evaluation
          -- cardinality, as we evaluate the scrutinee exactly once.
          = assert (null bndrs) (bndrs, case_bndr_sd)

        alt_ty3
          -- See Note [Precise exceptions and strictness analysis] in "GHC.Types.Demand"
          | exprMayThrowPreciseException (ae_fam_envs env) scrut
          = deferAfterPreciseException alt_ty2
          | otherwise
          = alt_ty2

        WithDmdType scrut_ty scrut' = dmdAnal env scrut_sd scrut
        res_ty             = alt_ty3 `plusDmdType` discardArgDmds scrut_ty
    in
--    pprTrace "dmdAnal:Case1" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "dmd" <+> ppr dmd
--                                   , text "case_bndr_dmd" <+> ppr (idDemandInfo case_bndr')
--                                   , text "scrut_sd" <+> ppr scrut_sd
--                                   , text "scrut_ty" <+> ppr scrut_ty
--                                   , text "alt_ty" <+> ppr alt_ty2
--                                   , text "res_ty" <+> ppr res_ty ]) $
    WithDmdType res_ty (Case scrut' case_bndr' ty [Alt alt_con bndrs' rhs'])
    where
      want_precise_field_dmds (DataAlt dc)
        | Nothing <- tyConSingleAlgDataCon_maybe $ dataConTyCon dc
        = False    -- Not a product type, even though this is the
                   -- only remaining possible data constructor
        | DefinitelyRecursive <- ae_rec_dc env dc
        = False     -- See Note [Demand analysis for recursive data constructors]
        | otherwise
        = True
      want_precise_field_dmds (LitAlt {}) = False  -- Like the non-product datacon above
      want_precise_field_dmds DEFAULT     = True

dmdAnal' env dmd (Case scrut case_bndr ty alts)
  = let      -- Case expression with multiple alternatives
        WithDmdType scrut_ty scrut' = dmdAnal env topSubDmd scrut

        WithDmdType alt_ty1 case_bndr_dmd = findBndrDmd env alt_ty case_bndr
        !case_bndr'                       = setIdDemandInfo case_bndr case_bndr_dmd
        WithDmdType alt_ty alts'          = dmdAnalSumAlts env dmd case_bndr alts

        fam_envs             = ae_fam_envs env
        alt_ty2
          -- See Note [Precise exceptions and strictness analysis] in "GHC.Types.Demand"
          | exprMayThrowPreciseException fam_envs scrut
          = deferAfterPreciseException alt_ty1
          | otherwise
          = alt_ty1
        res_ty               = scrut_ty `plusDmdType` discardArgDmds alt_ty2

    in
--    pprTrace "dmdAnal:Case2" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "scrut_ty" <+> ppr scrut_ty
--                                   , text "alt_ty1" <+> ppr alt_ty1
--                                   , text "alt_ty2" <+> ppr alt_ty2
--                                   , text "res_ty" <+> ppr res_ty ]) $
    WithDmdType res_ty (Case scrut' case_bndr' ty alts')

dmdAnal' env dmd (Let bind body)
  = WithDmdType final_ty (Let bind' body')
  where
    !(WithDmdType final_ty (R bind' body')) = dmdAnalBind NotTopLevel env dmd bind go'
    go' !env'                 = dmdAnal env' dmd body

-- | A simple, syntactic analysis of whether an expression MAY throw a precise
-- exception when evaluated. It's always sound to return 'True'.
-- See Note [Which scrutinees may throw precise exceptions].
exprMayThrowPreciseException :: FamInstEnvs -> CoreExpr -> Bool
exprMayThrowPreciseException envs e
  | not (forcesRealWorld envs (exprType e))
  = False -- 1. in the Note
  | Var f <- fn
  , Just op    <- isPrimOpId_maybe f
  , op /= RaiseIOOp
  = False -- 2. in the Note
  | Var f <- fn
  , f `hasKey` seqHashKey
  = False -- 3. in the Note
  | Var f <- fn
  , Just fcall <- isFCallId_maybe f
  , not (isSafeForeignCall fcall)
  = False -- 4. in the Note
  | otherwise
  = True  -- _. in the Note
  where
    (fn, _) = collectArgs e

-- | Recognises types that are
--    * @State# RealWorld@
--    * Unboxed tuples with a @State# RealWorld@ field
-- modulo coercions. This will detect 'IO' actions (even post Nested CPR! See
-- T13380e) and user-written variants thereof by their type.
forcesRealWorld :: FamInstEnvs -> Type -> Bool
forcesRealWorld fam_envs ty
  | ty `eqType` realWorldStatePrimTy
  = True
  | Just (tc, tc_args, _co)  <- normSplitTyConApp_maybe fam_envs ty
  , isUnboxedTupleTyCon tc
  , let field_tys = dataConInstArgTys (tyConSingleDataCon tc) tc_args
  = any (eqType realWorldStatePrimTy . scaledThing) field_tys
  | otherwise
  = False

dmdAnalSumAlts :: AnalEnv -> SubDemand -> Id -> [CoreAlt] -> WithDmdType [CoreAlt]
dmdAnalSumAlts _ _ _ [] = WithDmdType botDmdType []
  -- Base case is botDmdType, for empty case alternatives
  -- This is a unit for lubDmdType, and the right result
  -- when there really are no alternatives
dmdAnalSumAlts env dmd case_bndr (alt:alts)
  = let
      WithDmdType cur_ty  alt'  = dmdAnalSumAlt env dmd case_bndr alt
      WithDmdType rest_ty alts' = dmdAnalSumAlts env dmd case_bndr alts
    in WithDmdType (lubDmdType cur_ty rest_ty) (alt':alts')


dmdAnalSumAlt :: AnalEnv -> SubDemand -> Id -> CoreAlt -> WithDmdType CoreAlt
dmdAnalSumAlt env dmd case_bndr (Alt con bndrs rhs)
  | let rhs_env = addInScopeAnalEnvs env (case_bndr:bndrs)
    -- See Note [Bringing a new variable into scope]
  , WithDmdType rhs_ty rhs' <- dmdAnal rhs_env dmd rhs
  , WithDmdType alt_ty dmds <- findBndrsDmds env rhs_ty bndrs
  , let (_ :* case_bndr_sd) = findIdDemand alt_ty case_bndr
        -- See Note [Demand on case-alternative binders]
        -- we can't use the scrut_sd, because it says 'Prod' and we'll use
        -- topSubDmd anyway for scrutinees of sum types.
        scrut_sd = scrutSubDmd case_bndr_sd dmds
        dmds' = fieldBndrDmds scrut_sd (length dmds)
        -- Do not put a thunk into the Alt
        !new_ids            = setBndrsDemandInfo bndrs dmds'
  = -- pprTrace "dmdAnalSumAlt" (ppr con $$ ppr case_bndr $$ ppr dmd $$ ppr alt_ty) $
    WithDmdType alt_ty (Alt con new_ids rhs')

-- See Note [Demand on the scrutinee of a product case]
scrutSubDmd :: SubDemand -> [Demand] -> SubDemand
scrutSubDmd case_sd fld_dmds =
  -- pprTraceWith "scrutSubDmd" (\scrut_sd -> ppr case_sd $$ ppr fld_dmds $$ ppr scrut_sd) $
  case_sd `plusSubDmd` mkProd Unboxed fld_dmds

-- See Note [Demand on case-alternative binders]
fieldBndrDmds :: SubDemand -- on the scrutinee
              -> Arity
              -> [Demand]  -- Final demands for the components of the DataCon
fieldBndrDmds scrut_sd n_flds =
  case viewProd n_flds scrut_sd of
    Just (_, ds) -> ds
    Nothing      -> replicate n_flds topDmd
                      -- Either an arity mismatch or scrut_sd was a call demand.
                      -- See Note [Untyped demand on case-alternative binders]

{-
Note [Anticipating ANF in demand analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When analysing non-complex (e.g., trivial) thunks and complex function
arguments, we have to pretend that the expression is really in administrative
normal form (ANF), the conversion to which is done by CorePrep.

Consider
```
f x = let y = x |> co in y `seq` y `seq` ()
```
E.g., 'y' is a let-binding with a trivial RHS. That may occur if 'y' can't be
inlined, for example. Now, is 'x' used once? It may appear as if that is the
case, since its only occurrence is in 'y's memoised RHS. But actually, CorePrep
will *not* allocate a thunk for 'y', because it is trivial and could just
re-use the memoisation mechanism of 'x'! By saying that 'x' is used once it
becomes a single-entry thunk and a call to 'f' will evaluate it twice.
The same applies to trivial arguments, e.g., `f z` really evaluates `z` twice.

So, somewhat counter-intuitively, trivial arguments and let RHSs will *not* be
memoised. On the other hand, evaluation of non-trivial arguments and let RHSs
*will* be memoised. In fact, consider the effect of conversion to ANF on complex
function arguments (as done by 'GHC.Core.Prep.mkFloat'):
```
f2 (g2 x) ===> let y = g2 x in f2 y                   (if `y` is lifted)
f3 (g3 x) ===> case g3 x of y { __DEFAULT -> f3 y }   (if `y` is not lifted)
```
So if a lifted argument like `g2 x` is complex enough, it will be memoised.
Regardless how many times 'f2' evaluates its parameter, the argument will be
evaluated at most once to WHNF.
Similarly, when an unlifted argument like `g3 x` is complex enough, we will
evaluate it *exactly* once to WHNF, no matter how 'f3' evaluates its parameter.

Note that any evaluation beyond WHNF is not affected by memoisation. So this
Note affects the outer 'Card' of a 'Demand', but not its nested 'SubDemand'.
'anticipateANF' predicts the effect of case-binding and let-binding complex
arguments, as well as the lack of memoisation for trivial let RHSs.
In particular, this takes care of the gripes in
Note [Analysing with absent demand] relating to unlifted types.

Note [Analysing with absent demand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we analyse an expression with demand A.  The "A" means
"absent", so this expression will never be needed. What should happen?
There are several wrinkles:

* We *do* want to analyse the expression regardless.
  Reason: Note [Always analyse in virgin pass]

  But we can post-process the results to ignore all the usage
  demands coming back. This is done by 'multDmdType' with the appropriate
  (absent) evaluation cardinality A or B.

* Nevertheless, which sub-demand should we pick for analysis?
  Since the demand was absent, any would do. Worker/wrapper will replace
  absent bindings with an absent filler anyway, so annotations in the RHS
  of an absent binding don't matter much.
  Picking 'botSubDmd' would be the most useful, but would also look a bit
  misleading in the Core output of DmdAnal, because all nested annotations would
  be bottoming. Better pick 'seqSubDmd', so that we annotate many of those
  nested bindings with A themselves.

* Since we allow unlifted arguments that are not ok-for-speculation,
  we need to be extra careful in the following situation, because unlifted
  values are evaluated even if they are not used. Example from #9254:
     f :: (() -> (# Int#, () #)) -> ()
          -- Strictness signature is
          --    <1C(1,P(A,1L))>
          -- I.e. calls k, but discards first component of result
     f k = case k () of (# _, r #) -> r

     g :: Int -> ()
     g y = f (\n -> (# case y of I# y2 -> y2, n #))

  Here, f's strictness signature says (correctly) that it calls its argument
  function and ignores the first component of its result.

  But in function g, we *will* evaluate the 'case y of ...', because it has type
  Int#. So in the program as written, 'y' will be evaluated. Hence we must
  record this usage of 'y', else 'g' will say 'y' is absent, and will w/w so
  that 'y' is bound to an absent filler (see Note [Absent fillers]), leading
  to a crash when 'y' is evaluated.

  Now, worker/wrapper could be smarter and replace `case y of I# y2 -> y2`
  with a suitable absent filler such as `RUBBISH[IntRep] @Int#`.
  But as long as worker/wrapper isn't equipped to do so, we must be cautious,
  and follow Note [Anticipating ANF in demand analysis]. That is, in
  'dmdAnalStar', we will set the evaluation cardinality to C_11, anticipating
  the case binding of the complex argument `case y of I# y2 -> y2`. This
  cardinlities' only effect is in the call to 'multDmdType', where it makes sure
  that the demand on the arg's free variable 'y' is not absent and strict, so
  that it is ultimately passed unboxed to 'g'.

Note [Always analyse in virgin pass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tricky point: make sure that we analyse in the 'virgin' pass. Consider
   rec { f acc x True  = f (...rec { g y = ...g... }...)
         f acc x False = acc }
In the virgin pass for 'f' we'll give 'f' a very strict (bottom) type.
That might mean that we analyse the sub-expression containing the
E = "...rec g..." stuff in a bottom demand.  Suppose we *didn't analyse*
E, but just returned botType.

Then in the *next* (non-virgin) iteration for 'f', we might analyse E
in a weaker demand, and that will trigger doing a fixpoint iteration
for g.  But *because it's not the virgin pass* we won't start g's
iteration at bottom.  Disaster.  (This happened in $sfibToList' of
nofib/spectral/fibheaps.)

So in the virgin pass we make sure that we do analyse the expression
at least once, to initialise its signatures.

Note [Which scrutinees may throw precise exceptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is the specification of 'exprMayThrowPreciseExceptions',
which is important for Scenario 2 of
Note [Precise exceptions and strictness analysis] in GHC.Types.Demand.

For an expression @f a1 ... an :: ty@ we determine that
  1. False  If ty is *not* @State# RealWorld@ or an unboxed tuple thereof.
            This check is done by 'forcesRealWorld'.
            (Why not simply unboxed pairs as above? This is motivated by
            T13380{d,e}.)
  2. False  If f is a PrimOp, and it is *not* raiseIO#
  3. False  If f is the PrimOp-like `seq#`, cf. Note [seq# magic].
  4. False  If f is an unsafe FFI call ('PlayRisky')
  _. True   Otherwise "give up".

It is sound to return False in those cases, because
  1. We don't give any guarantees for unsafePerformIO, so no precise exceptions
     from pure code.
  2. raiseIO# is the only primop that may throw a precise exception.
  3. `seq#` used to be a primop that did not throw a precise exception.
     We keep it that way for back-compat.
     See the implementation bits of Note [seq# magic] in GHC.Types.Id.Make.
  4. Unsafe FFI calls may not interact with the RTS (to throw, for example).
     See haddock on GHC.Types.ForeignCall.PlayRisky.

We *need* to return False in those cases, because
  1. We would lose too much strictness in pure code, all over the place.
  2. We would lose strictness for primops like getMaskingState#, which
     introduces a substantial regression in
     GHC.IO.Handle.Internals.wantReadableHandle.
  3. `seq#` used to be a PrimOp and we want to stay backwards compatible.
  4. We would lose strictness for code like GHC.Fingerprint.fingerprintData,
     where an intermittent FFI call to c_MD5Init would otherwise lose
     strictness on the arguments len and buf, leading to regressions in T9203
     (2%) and i386's haddock.base (5%). Tested by T13380f.

In !3014 we tried a more sophisticated analysis by introducing ConOrDiv (nic)
to the Divergence lattice, but in practice it turned out to be hard to untaint
from 'topDiv' to 'conDiv', leading to bugs, performance regressions and
complexity that didn't justify the single fixed testcase T13380c.

You might think that we should check for side-effects rather than just for
precise exceptions. Right you are! See Note [Side-effects and strictness]
for why we unfortunately do not.

Note [Demand analysis for recursive data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
T11545 features a single-product, recursive data type
  data A = A A A ... A
    deriving Eq
Naturally, `(==)` is deeply strict in `A` and in fact will never terminate. That
leads to very large (exponential in the depth) demand signatures and fruitless
churn in boxity analysis, demand analysis and worker/wrapper.

So we detect `A` as a recursive data constructor (see
Note [Detecting recursive data constructors]) analysing `case x of A ...`
and simply assume L for the demand on field binders, which is the same code
path as we take for sum types. This code happens in want_precise_field_dmds
in the Case equation for dmdAnal.

Combined with the B demand on the case binder, we get the very small demand
signature <1S><1S>b on `(==)`. This improves ghc/alloc performance on T11545
tenfold! See also Note [CPR for recursive data constructors] which describes the
sibling mechanism in CPR analysis.

Note [Demand on the scrutinee of a product case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When figuring out the demand on the scrutinee of a product case,
we use the demands of the case alternative, i.e. id_dmds.
But note that these include the demand on the case binder;
see Note [Demand on case-alternative binders].
This is crucial. Example:
   f x = case x of y { (a,b) -> k y a }
If we just take scrut_demand = 1P(L,A), then we won't pass x to the
worker, so the worker will rebuild
     x = (a, absent-error)
and that'll crash.

Note [Demand on case-alternative binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The demand on a binder in a case alternative comes
  (a) From the demand on the binder itself
  (b) From the demand on the case binder
Forgetting (b) led directly to #10148.

Example. Source code:
  f x@(p,_) = if p then foo x else True

  foo (p,True) = True
  foo (p,q)    = foo (q,p)

After strictness analysis, forgetting (b):
  f = \ (x_an1 [Dmd=1P(1L,ML)] :: (Bool, Bool)) ->
      case x_an1
      of wild_X7 [Dmd=MP(ML,ML)]
      { (p_an2 [Dmd=1L], ds_dnz [Dmd=A]) ->
      case p_an2 of _ {
        False -> GHC.Types.True;
        True -> foo wild_X7 }

Note that ds_dnz is syntactically dead, but the expression bound to it is
reachable through the case binder wild_X7. Now watch what happens if we inline
foo's wrapper:
  f = \ (x_an1 [Dmd=1P(1L,ML)] :: (Bool, Bool)) ->
      case x_an1
      of _ [Dmd=MP(ML,ML)]
      { (p_an2 [Dmd=1L], ds_dnz [Dmd=A]) ->
      case p_an2 of _ {
        False -> GHC.Types.True;
        True -> $wfoo_soq GHC.Types.True ds_dnz }

Look at that! ds_dnz has come back to life in the call to $wfoo_soq! A second
run of demand analysis would no longer infer ds_dnz to be absent.
But unlike occurrence analysis, which infers properties of the *syntactic*
shape of the program, the results of demand analysis describe expressions
*semantically* and are supposed to be mostly stable across Simplification.
That's why we should better account for (b).
In #10148, we ended up emitting a single-entry thunk instead of an updateable
thunk for a let binder that was an an absent case-alt binder during DmdAnal.

This is needed even for non-product types, in case the case-binder
is used but the components of the case alternative are not.

Note [Untyped demand on case-alternative binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With unsafeCoerce, #8037 and #22039 taught us that the demand on the case binder
may be a call demand or have a different number of fields than the constructor
of the case alternative it is used in. From T22039:

  blarg :: (Int, Int) -> Int
  blarg (x,y) = x+y
  -- blarg :: <1!P(1L,1L)>

  f :: Either Int Int -> Int
  f Left{} = 0
  f e = blarg (unsafeCoerce e)
  ==> { desugars to }
  f = \ (ds_d1nV :: Either Int Int) ->
      case ds_d1nV of wild_X1 {
        Left ds_d1oV -> lvl_s1Q6;
        Right ipv_s1Pl ->
          blarg
            (case unsafeEqualityProof @(*) @(Either Int Int) @(Int, Int) of
             { UnsafeRefl co_a1oT ->
             wild_X1 `cast` (Sub (Sym co_a1oT) :: Either Int Int ~R# (Int, Int))
             })
      }

The case binder `e`/`wild_X1` has demand 1!P(1L,1L), with two fields, from the call
to `blarg`, but `Right` only has one field. Although the code will crash when
executed, we must be able to analyse it in 'fieldBndrDmds' and conservatively
approximate with Top instead of panicking because of the mismatch.
In #22039, this kind of code was guarded behind a safe `cast` and thus dead
code, but nevertheless led to a panic of the compiler.

You might wonder why the same problem doesn't come up when scrutinising a
product type instead of a sum type. It appears that for products, `wild_X1`
will be inlined before DmdAnal.

See also Note [mkWWstr and unsafeCoerce] for a related issue.

Note [Aggregated demand for cardinality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FIXME: This Note should be named [LetUp vs. LetDown] and probably predates
said separation. SG

We use different strategies for strictness and usage/cardinality to
"unleash" demands captured on free variables by bindings. Let us
consider the example:

f1 y = let {-# NOINLINE h #-}
           h = y
       in  (h, h)

We are interested in obtaining cardinality demand U1 on |y|, as it is
used only in a thunk, and, therefore, is not going to be updated any
more. Therefore, the demand on |y|, captured and unleashed by usage of
|h| is U1. However, if we unleash this demand every time |h| is used,
and then sum up the effects, the ultimate demand on |y| will be U1 +
U1 = U. In order to avoid it, we *first* collect the aggregate demand
on |h| in the body of let-expression, and only then apply the demand
transformer:

transf[x](U) = {y |-> U1}

so the resulting demand on |y| is U1.

The situation is, however, different for strictness, where this
aggregating approach exhibits worse results because of the nature of
|both| operation for strictness. Consider the example:

f y c =
  let h x = y |seq| x
   in case of
        True  -> h True
        False -> y

It is clear that |f| is strict in |y|, however, the suggested analysis
will infer from the body of |let| that |h| is used lazily (as it is
used in one branch only), therefore lazy demand will be put on its
free variable |y|. Conversely, if the demand on |h| is unleashed right
on the spot, we will get the desired result, namely, that |f| is
strict in |y|.


************************************************************************
*                                                                      *
                    Demand transformer
*                                                                      *
************************************************************************
-}

dmdTransform :: AnalEnv   -- ^ The analysis environment
             -> Id        -- ^ The variable
             -> SubDemand -- ^ The evaluation context of the var
             -> DmdType   -- ^ The demand type unleashed by the variable in this
                          -- context. The returned DmdEnv includes the demand on
                          -- this function plus demand on its free variables
-- See Note [DmdSig: demand signatures, and demand-sig arity] in "GHC.Types.Demand"
dmdTransform env var sd
  -- Data constructors
  | Just con <- isDataConWorkId_maybe var
  = -- pprTraceWith "dmdTransform:DataCon" (\ty -> ppr con $$ ppr sd $$ ppr ty) $
    dmdTransformDataConSig (dataConRepStrictness con) sd
  -- See Note [DmdAnal for DataCon wrappers]
  | Just rhs <- dataConWrapUnfolding_maybe var
  , WithDmdType dmd_ty _rhs' <- dmdAnal env sd rhs
  = dmd_ty
  -- Dictionary component selectors
  -- Used to be controlled by a flag.
  -- See #18429 for some perf measurements.
  | Just _ <- isClassOpId_maybe var
  = -- pprTrace "dmdTransform:DictSel" (ppr var $$ ppr (idDmdSig var) $$ ppr sd) $
    dmdTransformDictSelSig (idDmdSig var) sd
  -- Imported functions
  | isGlobalId var
  , let res = dmdTransformSig (idDmdSig var) sd
  = -- pprTrace "dmdTransform:import" (vcat [ppr var, ppr (idDmdSig var), ppr sd, ppr res])
    res
  -- Top-level or local let-bound thing for which we use LetDown ('useLetUp').
  -- In that case, we have a strictness signature to unleash in our AnalEnv.
  | Just (sig, top_lvl) <- lookupSigEnv env var
  , let fn_ty = dmdTransformSig sig sd
  = -- pprTrace "dmdTransform:LetDown" (vcat [ppr var, ppr sig, ppr sd, ppr fn_ty]) $
    case top_lvl of
      NotTopLevel -> addVarDmd fn_ty var (C_11 :* sd)
      TopLevel
        | isInterestingTopLevelFn var
        -- Top-level things will be used multiple times or not at
        -- all anyway, hence the `floatifyDmd`: it means we don't
        -- have to track whether @var@ is used strictly or at most
        -- once, because ultimately it never will
        -> addVarDmd fn_ty var (floatifyDmd (C_11 :* sd))
        | otherwise
        -> fn_ty -- don't bother tracking; just annotate with 'topDmd' later
  -- Everything else:
  --   * Local let binders for which we use LetUp (cf. 'useLetUp')
  --   * Lambda binders
  --   * Case and constructor field binders
  | otherwise
  = -- pprTrace "dmdTransform:other" (vcat [ppr var, ppr boxity, ppr sd]) $
    noArgsDmdType (addVarDmdEnv nopDmdEnv var (C_11 :* sd))

{- *********************************************************************
*                                                                      *
                      Binding right-hand sides
*                                                                      *
********************************************************************* -}

-- | An environment in which all demands are weak according to 'isWeakDmd'.
-- See Note [Lazy and unleashable free variables].
type WeakDmds = VarEnv Demand

-- | @dmdAnalRhsSig@ analyses the given RHS to compute a demand signature
-- for the LetDown rule. It works as follows:
--
--  * assuming the weakest possible body sub-demand, L
--  * looking at the definition
--  * determining a strictness signature
--
-- Since it assumed a body sub-demand of L, the resulting signature is
-- applicable at any call site.
dmdAnalRhsSig
  :: TopLevelFlag
  -> RecFlag
  -> AnalEnv -> SubDemand
  -> Id -> CoreExpr
  -> (AnalEnv, WeakDmds, Id, CoreExpr)
-- Process the RHS of the binding, add the strictness signature
-- to the Id, and augment the environment with the signature as well.
-- See Note [NOINLINE and strictness]
dmdAnalRhsSig top_lvl rec_flag env let_sd id rhs
  = -- pprTrace "dmdAnalRhsSig" (ppr id $$ ppr let_dmd $$ ppr rhs_dmds $$ ppr sig $$ ppr weak_fvs) $
    (final_env, weak_fvs, final_id, final_rhs)
  where
    ww_arity = workWrapArity id rhs
      -- See Note [Worker/wrapper arity and join points] point (1)

    body_sd | isJoinId id = let_sd
            | otherwise   = topSubDmd
      -- See Note [Demand analysis for join points]
      -- See Note [Invariants on join points] invariant 2b, in GHC.Core
      --     ww_arity matches the join arity of the join point

    adjusted_body_sd = unboxedWhenSmall env rec_flag (resultType_maybe id) body_sd
      -- See Note [Unboxed demand on function bodies returning small products]

    rhs_sd = mkCalledOnceDmds ww_arity adjusted_body_sd

    WithDmdType rhs_dmd_ty rhs' = dmdAnal env rhs_sd rhs
    DmdType rhs_env rhs_dmds = rhs_dmd_ty
    (final_rhs_dmds, final_rhs) = finaliseArgBoxities env id ww_arity
                                                      rhs_dmds (de_div rhs_env) rhs'

    dmd_sig_arity = ww_arity + strictCallArity body_sd
    sig = mkDmdSigForArity dmd_sig_arity (DmdType sig_env final_rhs_dmds)
          -- strictCallArity is > 0 only for join points
          -- See Note [mkDmdSigForArity]

    opts       = ae_opts env
    final_id   = setIdDmdAndBoxSig opts id sig
    !final_env = extendAnalEnv top_lvl env final_id sig

    -- See Note [Aggregated demand for cardinality]
    -- FIXME: That Note doesn't explain the following lines at all. The reason
    --        is really much different: When we have a recursive function, we'd
    --        have to also consider the free vars of the strictness signature
    --        when checking whether we found a fixed-point. That is expensive;
    --        we only want to check whether argument demands of the sig changed.
    --        reuseEnv makes it so that the FV results are stable as long as the
    --        last argument demands were. Strictness won't change. But used-once
    --        might turn into used-many even if the signature was stable and
    --        we'd have to do an additional iteration. reuseEnv makes sure that
    --        we never get used-once info for FVs of recursive functions.
    --        See #14816 where we try to get rid of reuseEnv.
    rhs_env1 = case rec_flag of
                Recursive    -> reuseEnv rhs_env
                NonRecursive -> rhs_env

    -- See Note [Absence analysis for stable unfoldings and RULES]
    rhs_env2 = rhs_env1 `plusDmdEnv` demandRootSet env (bndrRuleAndUnfoldingIds id)

    -- See Note [Lazy and unleashable free variables]
    !(!sig_env, !weak_fvs) = splitWeakDmds rhs_env2

splitWeakDmds :: DmdEnv -> (DmdEnv, WeakDmds)
splitWeakDmds (DE fvs div) = (DE sig_fvs div, weak_fvs)
  where (!weak_fvs, !sig_fvs) = partitionVarEnv isWeakDmd fvs

-- | The result type after applying 'idArity' many arguments. Returns 'Nothing'
-- when the type doesn't have exactly 'idArity' many arrows.
resultType_maybe :: Id -> Maybe Type
resultType_maybe id
  | (pis,ret_ty) <- splitPiTys (idType id)
  , count isAnonPiTyBinder pis == idArity id
  = Just $! ret_ty
  | otherwise
  = Nothing

unboxedWhenSmall :: AnalEnv -> RecFlag -> Maybe Type -> SubDemand -> SubDemand
-- See Note [Unboxed demand on function bodies returning small products]
unboxedWhenSmall _   _        Nothing       sd = sd
unboxedWhenSmall env rec_flag (Just ret_ty) sd = go 1 ret_ty sd
  where
    -- Magic constant, bounding the depth of optimistic 'Unboxed' flags. We
    -- might want to minmax in the future.
    max_depth | isRec rec_flag = 3 -- So we get at most something as deep as !P(L!P(L!L))
              | otherwise      = 1 -- Otherwise be unbox too deep in T18109, T18174 and others and get a bunch of stack overflows
    go :: Int -> Type -> SubDemand -> SubDemand
    go depth ty sd
      | depth <= max_depth
      , Just (tc, tc_args, _co) <- normSplitTyConApp_maybe (ae_fam_envs env) ty
      , Just dc <- tyConSingleAlgDataCon_maybe tc
      , null (dataConExTyCoVars dc) -- Can't unbox results with existentials
      , dataConRepArity dc <= dmd_unbox_width (ae_opts env)
      , Just (_, ds) <- viewProd (dataConRepArity dc) sd
      , arg_tys <- map scaledThing $ dataConInstArgTys dc tc_args
      , equalLength ds arg_tys
      = mkProd Unboxed $! strictZipWith (go_dmd (depth+1)) arg_tys ds
      | otherwise
      = sd

    go_dmd :: Int -> Type -> Demand -> Demand
    go_dmd depth ty dmd = case dmd of
      AbsDmd  -> AbsDmd
      BotDmd  -> BotDmd
      n :* sd -> n :* go depth ty sd

-- | If given the (local, non-recursive) let-bound 'Id', 'useLetUp' determines
-- whether we should process the binding up (body before rhs) or down (rhs
-- before body).
--
-- We use LetDown if there is a chance to get a useful strictness signature to
-- unleash at call sites. LetDown is generally more precise than LetUp if we can
-- correctly guess how it will be used in the body, that is, for which incoming
-- demand the strictness signature should be computed, which allows us to
-- unleash higher-order demands on arguments at call sites. This is mostly the
-- case when
--
--   * The binding takes any arguments before performing meaningful work (cf.
--     'idArity'), in which case we are interested to see how it uses them.
--   * The binding is a join point, hence acting like a function, not a value.
--     As a big plus, we know *precisely* how it will be used in the body; since
--     it's always tail-called, we can directly unleash the incoming demand of
--     the let binding on its RHS when computing a strictness signature. See
--     [Demand analysis for join points].
--
-- Thus, if the binding is not a join point and its arity is 0, we have a thunk
-- and use LetUp, implying that we have no usable demand signature available
-- when we analyse the let body.
--
-- Since thunk evaluation is memoised, we want to unleash its 'DmdEnv' of free
-- vars at most once, regardless of how many times it was forced in the body.
-- This makes a real difference wrt. usage demands. The other reason is being
-- able to unleash a more precise product demand on its RHS once we know how the
-- thunk was used in the let body.
--
-- Characteristic examples, always assuming a single evaluation:
--
--   * @let x = 2*y in x + x@ => LetUp. Compared to LetDown, we find out that
--     the expression uses @y@ at most once.
--   * @let x = (a,b) in fst x@ => LetUp. Compared to LetDown, we find out that
--     @b@ is absent.
--   * @let f x = x*2 in f y@ => LetDown. Compared to LetUp, we find out that
--     the expression uses @y@ strictly, because we have @f@'s demand signature
--     available at the call site.
--   * @join exit = 2*y in if a then exit else if b then exit else 3*y@ =>
--     LetDown. Compared to LetUp, we find out that the expression uses @y@
--     strictly, because we can unleash @exit@'s signature at each call site.
--   * For a more convincing example with join points, see Note [Demand analysis
--     for join points].
--
useLetUp :: TopLevelFlag -> Var -> Bool
useLetUp top_lvl f = isNotTopLevel top_lvl && idArity f == 0 && not (isJoinId f)

{- Note [Demand analysis for join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   g :: (Int,Int) -> Int
   g (p,q) = p+q

   f :: T -> Int -> Int
   f x p = g (join j y = (p,y)
              in case x of
                   A -> j 3
                   B -> j 4
                   C -> (p,7))

If j was a vanilla function definition, we'd analyse its body with evalDmd, and
think that it was lazy in p.  But for join points we can do better!  We know
that j's body will (if called at all) be evaluated with the demand that consumes
the entire join-binding, in this case the argument demand from g.  Whizzo!  g
evaluates both components of its argument pair, so p will certainly be evaluated
if j is called.

For f to be strict in p, we need /all/ paths to evaluate p; in this case the C
branch does so too, so we are fine.  So, as usual, we need to transport demands
on free variables to the call site(s).  Compare Note [Lazy and unleashable free
variables].

The implementation is easy: see `body_sd` in`dmdAnalRhsSig`.  When analysing
a join point, we can analyse its body (after stripping off the join binders,
here just 'y') with the demand from the entire join-binding (written `let_sd`
here).

Another win for join points!  #13543.

BUT see Note [Worker/wrapper arity and join points].

Note we may analyse the rhs of a join point with a demand that is either
bigger than, or smaller than, the number of lambdas syntactically visible.
* More lambdas than call demands:
       join j x = \p q r -> blah in ...
  in a context with demand Top.

* More call demands than lambdas:
       (join j x = h in ..(j 2)..(j 3)) a b c

Note [Worker/wrapper arity and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    (join j x = \y. error "urk")
    (in case v of              )
    (     A -> j 3             )  x
    (     B -> j 4             )
    (     C -> \y. blah        )

The entire thing is in a C(1,L) context, so we will analyse j's body, namely
   \y. error "urk"
with demand C(C(1,L)).  See `rhs_sd` in `dmdAnalRhsSig`.  That will produce
a demand signature of <A><A>b: and indeed `j` diverges when given two arguments.

BUT we do /not/ want to worker/wrapper `j` with two arguments.  Suppose we have
     join j2 :: Int -> Int -> blah
          j2 x = rhs
     in ...(j2 3)...(j2 4)...

where j2's join-arity is 1, so calls to `j` will all have /one/ argument.
Suppose the entire expression is in a called context (like `j` above) and `j2`
gets the demand signature <1!P(L)><1!P(L)>, that is, strict in both arguments.

we worker/wrapper'd `j2` with two args we'd get
     join $wj2 x# y# = let x = I# x#; y = I# y# in rhs
          j2 x = \y. case x of I# x# -> case y of I# y# -> $wj2 x# y#
     in ...(j2 3)...(j2 4)...
But now `$wj2`is no longer a join point. Boo.

Instead if we w/w at all, we want to do so only with /one/ argument:
     join $wj2 x# = let x = I# x# in rhs
          j2 x = case x of I# x# -> $wj2 x#
     in ...(j2 3)...(j2 4)...
Now all is fine.  BUT in `finaliseArgBoxities` we should trim y's boxity,
to reflect the fact tta we aren't going to unbox `y` at all.

Conclusion:

(1) The "worker/wrapper arity" of an Id is
    * For non-join-points: idArity
    * The join points: the join arity (Id part only of course)
    This is the number of args we will use in worker/wrapper.
    See `ww_arity` in `dmdAnalRhsSig`, and the function `workWrapArity`.

(2) A join point's demand-signature arity may exceed the Id's worker/wrapper
    arity.  See the `arity_ok` assertion in `mkWwBodies`.

(3) In `finaliseArgBoxities`, do trimBoxity on any argument demands beyond
    the worker/wrapper arity.

(4) In WorkWrap.splitFun, make sure we split based on the worker/wrapper
    arity (re)-computed by workWrapArity.

Note [The demand for the RHS of a binding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a binding { f = rhs }, in `dmdAnalRhsSig` we compute a `rhs_sd` in
which to analyse `rhs`.

The demand we use is:

* Ordinary bindings: a call-demand of depth (idArity f).
  Why idArity arguments? Because that's a conservative estimate of how many
  arguments we must feed a function before it does anything interesting with
  them.  Also it elegantly subsumes the trivial RHS and PAP case.  E.g. for
      f = g
  we want to use a threshold arity based on g, not 0!

  idArity is /at least/ the number of manifest lambdas, but might be higher for
  PAPs and trivial RHS (see Note [Demand analysis for trivial right-hand sides]).

* Join points: a call-demand of depth (value-binder subset of JoinArity),
  wrapped around the incoming demand for the entire expression; see
  Note [Demand analysis for join points]

Note that the idArity of a function varies independently of its cardinality
properties (cf. Note [idArity varies independently of dmdTypeDepth]), so we
implicitly encode the arity for when a demand signature is sound to unleash in
its 'dmdTypeDepth', not in its idArity (cf. Note [Understanding DmdType and
DmdSig] in GHC.Types.Demand). It is unsound to unleash a demand signature when
the incoming number of arguments is less than that. See GHC.Types.Demand
Note [DmdSig: demand signatures, and demand-sig arity].

Note that there might, in principle, be functions for which we might want to
analyse for more incoming arguments than idArity. Example:

  f x =
    if expensive
      then \y -> ... y ...
      else \y -> ... y ...

We'd analyse `f` under a unary call demand C(1,L), corresponding to idArity
being 1. That's enough to look under the manifest lambda and find out how a
unary call would use `x`, but not enough to look into the lambdas in the if
branches.

On the other hand, if we analysed for call demand C(1,C(1,L)), we'd get useful
strictness info for `y` (and more precise info on `x`) and possibly CPR
information, but

  * We would no longer be able to unleash the signature at unary call sites

  * Performing the worker/wrapper split based on this information would be
    implicitly eta-expanding `f`, playing fast and loose with divergence and
    even being unsound in the presence of newtypes, so we refrain from doing so.
    Also see Note [Don't eta expand in w/w] in GHC.Core.Opt.WorkWrap.

Since we only compute one signature, we do so for arity 1. Computing multiple
signatures for different arities (i.e., polyvariance) would be entirely
possible, if it weren't for the additional runtime and implementation
complexity.

Note [mkDmdSigForArity]
~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f x = if expensive x
         then \y. blah1
         else \y. blah2
We will analyse the body with demand C(1L), reflecting the single visible
argument x.  But dmdAnal will return a DmdType looking like
    DmdType fvs [x-dmd, y-dmd]
because it has seen two lambdas, \x and \y. Since the length of the argument
demands in a DmdSig gives the "threshold" for applying the signature
(see Note [DmdSig: demand signatures, and demand-sig arity] in GHC.Types.Demand)
we must trim that DmdType to just
    DmdSig (DmdTypte fvs [x-dmd])
when making that DmdType into the DmdSig for f.  This trimming is the job of
`mkDmdSigForArity`.

Alternative.  An alternative would be be to ensure that if
    (dmd_ty, e') = dmdAnal env subdmd e
then the length dmds in dmd_ty is always less than (or maybe equal to?) the
call-depth of subdmd.  To do that we'd need to adjust the Lam case of dmdAnal.
Probably not hard, but a job for another day; see discussion on !12873, #23113,
and #21392.

Note [idArity varies independently of dmdTypeDepth]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, an Id `f` has two independently varying attributes:

* f's idArity, and
* the dmdTypeDepth of f's demand signature

For example, if f's demand signature is <L><L>, f's arity could be
greater than, or less than 2. Why?  Because both are conservative
approximations:

* Arity n means "does no expensive work until applied to at least n args"
  (e.g. (f x1..xm) is cheap to bring to HNF for m<n)

* Dmd sig with n args means "here is how to transform the incoming demand
  when applied to n args".  This is /semantic/ property, unrelated to
  arity. See GHC.Types.Demand Note [Understanding DmdType and DmdSig]

We used to check in GHC.Core.Lint that dmdTypeDepth <= idArity for a let-bound
identifier. But that means we would have to zap demand signatures every time we
reset or decrease arity.

For example, consider the following expression:

    (let go x y = `x` seq ... in go) |> co

`go` might have a strictness signature of `<1L><L>`. The simplifier will identify
`go` as a nullary join point through `joinPointBinding_maybe` and float the
coercion into the binding, leading to an arity decrease:

    join go = (\x y -> `x` seq ...) |> co in go

With the CoreLint check, we would have to zap `go`'s perfectly viable strictness
signature.

However, in the case of a /bottoming/ signature, f : <L><L>b, we /can/
say that f's arity is no greater than 2, because it'd be false to say
that f does no work when applied to 3 args.  Lint checks this constraint,
in `GHC.Core.Lint.lintLetBind`.

Note [Demand analysis for trivial right-hand sides]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    foo = plusInt |> co
where plusInt is an arity-2 function with known strictness.  Clearly
we want plusInt's strictness to propagate to foo!  But because it has
no manifest lambdas, it won't do so automatically, and indeed 'co' might
have type (Int->Int->Int) ~ T.

Fortunately, GHC.Core.Opt.Arity gives 'foo' arity 2, which is enough for LetDown to
forward plusInt's demand signature, and all is well (see Note [Newtype arity] in
GHC.Core.Opt.Arity)! A small example is the test case NewtypeArity.

Note [Absence analysis for stable unfoldings and RULES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Among others, tickets #18638 and #23208 show that it's really important to treat
stable unfoldings as demanded. Consider

   g = blah

   f = \x.  ...no use of g....
   {- f's stable unfolding is f = \x. ...g... -}

If f is ever inlined we use 'g'. But f's current RHS makes no use
of 'g', so if we don't look at the unfolding we'll mark g as Absent,
and transform to

   g = error "Entered absent value"
   f = \x. ...
   {- f's stable unfolding is f = \x. ...g... -}

Now if f is subsequently inlined, we'll use 'g' and ... disaster.

SOLUTION: if f has a stable unfolding, treat every free variable as a
/demand root/, that is: Analyse it as if it was a variable occurring in a
'topDmd' context. This is done in `demandRoot` (which we also use for exported
top-level ids). Do the same for Ids free in the RHS of any RULES for f.

Wrinkles:

  (W1) You may wonder how it can be that f's optimised RHS has somehow
    discarded 'g', but when f is inlined we /don't/ discard g in the same
    way. I think a simple example is
       g = (a,b)
       f = \x.  fst g
       {-# INLINE f #-}

    Now f's optimised RHS will be \x.a, but if we change g to (error "..")
    (since it is apparently Absent) and then inline (\x. fst g) we get
    disaster.  But regardless, #18638 was a more complicated version of
    this, that actually happened in practice.

  (W2) You might wonder why we don't simply take the free vars of the
    unfolding/RULE and map them to topDmd. The reason is that any of the free vars
    might have demand signatures themselves that in turn demand transitive free
    variables and that we hence need to unleash! This came up in #23208.
    Consider

       err :: Int -> b
       err = error "really important message"

       sg :: Int -> Int
       sg _ = case err of {}  -- Str=<1B>b {err:->S}

       g :: a -> a  -- g is exported
       g x = x
       {-# RULES "g" g @Int = sg #-}

    Here, `err` is only demanded by `sg`'s demand signature: It doesn't occur
    in the weak_fvs of `sg`'s RHS at all. Hence when we `demandRoots` `sg`
    because it occurs in the RULEs of `g` (which is exported), we better unleash
    the demand signature of `sg`, too! Before #23208 we simply added a 'topDmd'
    for `sg`, failing to unleash the signature and hence observed an absent
    error instead of the `really important message`.

Note [DmdAnal for DataCon wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We give DataCon wrappers a (necessarily flat) demand signature in
`GHC.Types.Id.Make.mkDataConRep`, so that passes such as the Simplifier can
exploit it via the call to `GHC.Core.Opt.Simplify.Utils.isStrictArgInfo` in
`GHC.Core.Opt.Simplify.Iteration.rebuildCall`. But during DmdAnal, we *ignore*
the demand signature of a DataCon wrapper, and instead analyse its unfolding at
every call site.

The reason is that DataCon *worker*s have very precise demand transformers,
computed by `dmdTransformDataConSig`. It would be awkward if DataCon *wrappers*
would behave much less precisely during DmdAnal. Example:

   data T1 = MkT1 { get_x1 :: Int,  get_y1 :: Int }
   data T2 = MkT2 { get_x2 :: !Int, get_y2 :: Int }
   f1 x y = get_x1 (MkT1 x y)
   f2 x y = get_x2 (MkT2 x y)

Here `MkT1` has no wrapper. `get_x1` puts a demand `!P(1!L,A)` on its argument,
and `dmdTransformDataConSig` will transform that demand to an absent demand on
`y` in `f1` and an unboxing demand on `x`.
But `MkT2` has a wrapper (to evaluate the first field). If demand analysis deals
with `MkT2` only through its demand signature, demand signatures can't transform
an incoming demand `P(1!L,A)` in a useful way, so we won't get an absent demand
on `y` in `f2` or see that `x` can be unboxed. That's a serious loss.

The example above will not actually occur, because $WMkT2 would be inlined.
Nevertheless, we can get interesting sub-demands on DataCon wrapper
applications in boring contexts; see T22241.

You might worry about the efficiency cost of demand-analysing datacon wrappers
at every call site. But in fact they are inlined /anyway/ in the Final phase,
which happens before DmdAnal, so few wrappers remain. And analysing the
unfoldings for the remaining calls (which are those in a boring context) will be
exactly as (in)efficent as if we'd inlined those calls. It turns out to be not
measurable in practice.

See also Note [CPR for DataCon wrappers] in `GHC.Core.Opt.CprAnal`.

Note [Boxity for bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (A)
    indexError :: Show a => (a, a) -> a -> String -> b
    -- Str=<..><1!P(S,S)><1S><S>b
    indexError rng i s = error (show rng ++ show i ++ show s)

    get :: (Int, Int) -> Int -> [a] -> a
    get p@(l,u) i xs
      | l <= i, i < u = xs !! (i-u)
      | otherwise     = indexError p i "get"

The hot path of `get` certainly wants to unbox `p` as well as `l` and
`u`, but the unimportant, diverging error path needs `l::a` and `u::a`
boxed, since `indexError` can't unbox them because they are polymorphic.
This pattern often occurs in performance sensitive code that does
bounds-checking.

So we want to give `indexError` a signature like `<1!P(!S,!S)><1!S><S!S>b`
where the !S (meaning Poly Unboxed C1N) says that the polymorphic arguments
are unboxed (recursively).  The wrapper for `indexError` won't /actually/
unbox them (because their polymorphic type doesn't allow that) but when
demand-analysing /callers/, we'll behave as if that call needs the args
unboxed.

Then at call sites of `indexError`, we will end up doing some
reboxing, because `$windexError` still takes boxed arguments. This
reboxing should usually float into the slow, diverging code path; but
sometimes (sadly) it doesn't: see Note [Reboxed crud for bottoming calls].

Here is another important case (B):
    f x = Just x  -- Suppose f is not inlined for some reason
                  -- Main point: f takes its argument boxed

    wombat x = error (show (f x))

    g :: Bool -> Int -> a
    g True  x = x+1
    g False x = wombat x

Again we want `wombat` to pretend to take its Int-typed argument unboxed,
even though it has to pass it boxed to `f`, so that `g` can take its
argument unboxed (and rebox it before calling `wombat`).

So here's what we do: while summarising `indexError`'s boxity signature in
`finaliseArgBoxities`:

* To address (B), for bottoming functions, we start by using `unboxDeeplyDmd`
  to make all its argument demands unboxed, right to the leaves; regardless
  of what the analysis said.

* To address (A), for bottoming functions, in the DontUnbox case when the
  argument is a type variable, we /refrain/ from using trimBoxity.
  (Remember the previous bullet: we have already doen `unboxDeeplyDmd`.)

Wrinkle:

* Remember Note [No lazy, Unboxed demands in demand signature]. So
  unboxDeeplyDmd doesn't recurse into lazy demands.  It's extremely unusual
  to have lazy demands in the arguments of a bottoming function anyway.
  But it can happen, when the demand analyser gives up because it
  encounters a recursive data type; see Note [Demand analysis for recursive
  data constructors].

Note [Reboxed crud for bottoming calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For functions like `get` in Note [Boxity for bottoming functions], it's clear
that the reboxed crud will be floated inside to the call site of `$windexError`.
But here's an example where that is not the case:
```hs
import GHC.Ix

theresCrud :: Int -> Int -> Int
theresCrud x y = go x
  where
    go 0 = index (0,y) 0
    go 1 = index (x,y) 1
    go n = go (n-1)
    {-# NOINLINE theresCrud #-}
```
If you look at the Core, you'll see that `y` will be reboxed and used in the
two exit join points for the `$windexError` calls, while `x` is only reboxed in the
exit join point for `index (x,y) 1` (happens in lvl below):
```
$wtheresCrud = \ ww ww1 ->
      let { y = I# ww1 } in
      join { lvl2 = ... case lvl1 ww y of wild { }; ... } in
      join { lvl3 = ... case lvl y of wild { }; ... } in
      ...
```
This is currently a bug that we willingly accept and it's documented in #21128.

See also Note [indexError] in base:GHC.Ix, which describes how we use
SPECIALISE to mitigate this problem for indexError.
-}

{- *********************************************************************
*                                                                      *
             Finalising boxity
*                                                                      *
********************************************************************* -}

{- Note [Finalising boxity for demand signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The worker/wrapper pass must strictly adhere to the boxity decisions
encoded in the demand signature, because that is the information that
demand analysis propagates throughout the program. Failing to
implement the strategy laid out in the signature can result in
reboxing in unexpected places. Hence, we must completely anticipate
unboxing decisions during demand analysis and reflect these decisions
in demand annotations. That is the job of 'finaliseArgBoxities',
which is defined here and called from demand analysis.

Here is a list of different Notes it has to take care of:

  * Note [No lazy, Unboxed demands in demand signature] such as `L!P(L)` in
    general, but still allow Note [Unboxing evaluated arguments]
  * Note [No nested Unboxed inside Boxed in demand signature] such as `1P(1!L)`
  * Note [mkWWstr and unsafeCoerce]

NB: Then, the worker/wrapper blindly trusts the boxity info in the
demand signature; that is why 'canUnboxArg' does not look at
strictness -- it is redundant to do so.

Note [Finalising boxity for let-bound Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  let x = e in body
where the demand on 'x' is 1!P(blah).  We want to unbox x according to
Note [Thunk splitting] in GHC.Core.Opt.WorkWrap.  We must do this because
worker/wrapper ignores strictness and looks only at boxity flags; so if
x's demand is L!P(blah) we might still split it (wrongly).  We want to
switch to Boxed on any lazy demand.

That is what finaliseLetBoxity does.  It has no worker-arg budget, so it
is much simpler than finaliseArgBoxities.

Note [No nested Unboxed inside Boxed in demand signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
```
f p@(x,y)
  | even (x+y) = []
  | otherwise  = [p]
```
Demand analysis will infer that the function body puts a demand of `1P(1!L,1!L)`
on 'p', e.g., Boxed on the outside but Unboxed on the inside. But worker/wrapper
can't unbox the pair components without unboxing the pair! So we better say
`1P(1L,1L)` in the demand signature in order not to spread wrong Boxity info.
That happens via the call to trimBoxity in 'finaliseArgBoxities'/'finaliseLetBoxity'.

Note [No lazy, Unboxed demands in demand signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider T19407:

  data Huge = Huge Bool () ... () -- think: DynFlags
  data T = T { h :: Huge, n :: Int }
  f t@(T h _) = g h t
  g (H b _ ... _) t = if b then 1 else n t

The body of `g` puts (approx.) demand `L!P(A,1)` on `t`. But we better
not put that demand in `g`'s demand signature, because worker/wrapper will not
in general unbox a lazy-and-unboxed demand like `L!P(..)`.
(The exception are known-to-be-evaluated arguments like strict fields,
see Note [Unboxing evaluated arguments].)

The program above is an example where spreading misinformed boxity through the
signature is particularly egregious. If we give `g` that signature, then `f`
puts demand `S!P(1!P(1L,A,..),ML)` on `t`. Now we will unbox `t` in `f` it and
we get

  f (T (H b _ ... _) n) = $wf b n
  $wf b n = $wg b (T (H b x ... x) n)
  $wg = ...

Massive reboxing in `$wf`! Solution: Trim boxity on lazy demands in
'trimBoxity', modulo Note [Unboxing evaluated arguments].

Note [Unboxing evaluated arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this program (due to Roman):

    data X a = X !a

    foo :: X Int -> Int -> Int
    foo x@(X a) n = go 0
     where
       go i | i < n     = a + go (i+1)
            | otherwise = 0

We want the worker for 'foo' to look like this:

    $wfoo :: Int# -> Int# -> Int#

with the first argument unboxed, so that it is not eval'd each time around the
'go' loop (which would otherwise happen, since 'foo' is not strict in 'a'). It
is sound for the wrapper to pass an unboxed arg because X is strict
(see Note [Strictness and Unboxing] in "GHC.Core.Opt.DmdAnal"), so its argument
must be evaluated. And if we *don't* pass an unboxed argument, we can't even
repair it by adding a `seq` thus:

    foo (X a) n = a `seq` go 0

because the seq is discarded (very early) since X is strict!

So here's what we do

* Since this has nothing to do with how 'foo' uses 'a', we leave demand
  analysis alone, but account for the additional evaluatedness when
  annotating the binder 'finaliseArgBoxities', which will retain the Unboxed
  boxity on 'a' in the definition of 'foo' in the demand 'L!P(L)'; meaning
  it's used lazily but unboxed nonetheless. This seems to contradict Note
  [No lazy, Unboxed demands in demand signature], but we know that 'a' is
  evaluated and thus can be unboxed.

* When 'finaliseArgBoxities' decides to unbox a record, it will zip the field demands
  together with the respective 'StrictnessMark'. In case of 'x', it will pair
  up the lazy field demand 'L!P(L)' on 'a' with 'MarkedStrict' to account for
  the strict field.

* Said 'StrictnessMark' is passed to the recursive invocation of 'go_args' in
  'finaliseArgBoxities' when deciding whether to unbox 'a'. 'a' was used lazily, but
  since it also says 'MarkedStrict', we'll retain the 'Unboxed' boxity on 'a'.

* Worker/wrapper will consult 'canUnboxArg' for its unboxing decision. It will
  /not/ look at the strictness bits of the demand, only at Boxity flags. As such,
  it will happily unbox 'a' despite the lazy demand on it.

The net effect is that boxity analysis and the w/w transformation are more
aggressive about unboxing the strict arguments of a data constructor than when
looking at strictness info exclusively. It is very much like (Nested) CPR, which
needs its nested fields to be evaluated in order for it to unbox nestedly.

There is the usual danger of reboxing, which as usual we ignore. But
if X is monomorphic, and has an UNPACK pragma, then this optimisation
is even more important.  We don't want the wrapper to rebox an unboxed
argument, and pass an Int to $wfoo!

This works in nested situations like T10482

    data family Bar a
    data instance Bar (a, b) = BarPair !(Bar a) !(Bar b)
    newtype instance Bar Int = Bar Int

    foo :: Bar ((Int, Int), Int) -> Int -> Int
    foo f k = case f of BarPair x y ->
              case burble of
                 True -> case x of
                           BarPair p q -> ...
                 False -> ...

The extra eagerness lets us produce a worker of type:
     $wfoo :: Int# -> Int# -> Int# -> Int -> Int
     $wfoo p# q# y# = ...

even though the `case x` is only lazily evaluated.

--------- Historical note ------------
We used to add data-con strictness demands when demand analysing case
expression. However, it was noticed in #15696 that this misses some cases. For
instance, consider the program (from T10482)

    data family Bar a
    data instance Bar (a, b) = BarPair !(Bar a) !(Bar b)
    newtype instance Bar Int = Bar Int

    foo :: Bar ((Int, Int), Int) -> Int -> Int
    foo f k =
      case f of
        BarPair x y -> case burble of
                          True -> case x of
                                    BarPair p q -> ...
                          False -> ...

We really should be able to assume that `p` is already evaluated since it came
from a strict field of BarPair. This strictness would allow us to produce a
worker of type:

    $wfoo :: Int# -> Int# -> Int# -> Int -> Int
    $wfoo p# q# y# = ...

even though the `case x` is only lazily evaluated

Indeed before we fixed #15696 this would happen since we would float the inner
`case x` through the `case burble` to get:

    foo f k =
      case f of
        BarPair x y -> case x of
                          BarPair p q -> case burble of
                                          True -> ...
                                          False -> ...

However, after fixing #15696 this could no longer happen (for the reasons
discussed in ticket:15696#comment:76). This means that the demand placed on `f`
would then be significantly weaker (since the False branch of the case on
`burble` is not strict in `p` or `q`).

Consequently, we now instead account for data-con strictness in mkWWstr_one,
applying the strictness demands to the final result of DmdAnal. The result is
that we get the strict demand signature we wanted even if we can't float
the case on `x` up through the case on `burble`.

Note [Worker argument budget]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In 'finaliseArgBoxities' we don't want to generate workers with zillions of
argument when, say given a strict record with zillions of fields.  So we
limit the maximum number of worker args ('max_wkr_args') to the maximum of
  - -fmax-worker-args=N
  - The number of args in the original function; if it already has has
    zillions of arguments we don't want to seek /fewer/ args in the worker.
(Maybe we should /add/ them instead of maxing?)

We pursue a "layered" strategy for unboxing: we unbox the top level of the
argument(s), subject to budget; if there are any arguments left we unbox the
next layer, using that depleted budget.
Unboxing an argument *increases* the budget for the inner layer roughly
according to how many registers that argument takes (unboxed tuples take
multiple registers, see below), as determined by 'unariseArity'.
Budget is spent when we have to pass a non-absent field as a parameter.

To achieve this, we use the classic almost-circular programming technique in
which we we write one pass that takes a lazy list of the Budgets for every
layer. The effect is that of a breadth-first search (over argument type and
demand structure) to compute Budgets followed by a depth-first search to
construct the product demands, but laziness allows us to do it all in one
pass and without intermediate data structures.

Suppose we have -fmax-worker-args=4 for the remainder of this Note.
Then consider this example function:

  boxed :: (Int, Int) -> (Int, (Int, Int, Int)) -> Int
  boxed (a,b) (c, (d,e,f)) = a + b + c + d + e + f

With a budget of 4 args to spend (number of args is only 2), we'd be served well
to unbox both pairs, but not the triple. Indeed, that is what the algorithm
computes, and the following pictogram shows how the budget layers are computed.
Each layer is started with `n ~>`, where `n` is the budget at the start of the
layer. We write -n~> when we spend budget (and n is the remaining budget) and
+n~> when we earn budget. We separate unboxed args with ][ and indicate
inner budget threads becoming negative in braces {{}}, so that we see which
unboxing decision we do *not* commit to. Without further ado:

  4 ~> ][     (a,b) -3~>               ][     (c, ...) -2~>
       ][      | |                     ][      |   |
       ][      | +-------------+       ][      |   +-----------------+
       ][      |               |       ][      |                     |
       ][      v               v       ][      v                     v
  2 ~> ][ +3~> a  -2~> ][      b  -1~> ][ +2~> c  -1~> ][        (d, e, f) -0~>
       ][      |       ][      |       ][      |       ][ {{      |  |  |                          }}
       ][      |       ][      |       ][      |       ][ {{      |  |  +----------------+         }}
       ][      v       ][      v       ][      v       ][ {{      v  +------v            v         }}
  0 ~> ][ +1~> I# -0~> ][ +1~> I# -0~> ][ +1~> I# -0~> ][ {{ +1~> d -0~> ][ e -(-1)~> ][ f -(-2)~> }}

Unboxing increments the budget we have on the next layer (because we don't need
to retain the boxed arg), but in turn the inner layer must afford to retain all
non-absent fields, each decrementing the budget. Note how the budget becomes
negative when trying to unbox the triple and the unboxing decision is "rolled
back". This is done by the 'positiveTopBudget' guard.

There's a bit of complication as a result of handling unboxed tuples correctly;
specifically, handling nested unboxed tuples. Consider (#21737)

  unboxed :: (Int, Int) -> (# Int, (# Int, Int, Int #) #) -> Int
  unboxed (a,b) (# c, (# d, e, f #) #) = a + b + c + d + e + f

Recall that unboxed tuples will be flattened to individual arguments during
unarisation. Here, `unboxed` will have 5 arguments at runtime because of the
nested unboxed tuple, which will be flattened to 4 args. So it's best to leave
`(a,b)` boxed (because we already are above our arg threshold), but unbox `c`
through `f` because that doesn't increase the number of args post unarisation.

Note that the challenge is that syntactically, `(# d, e, f #)` occurs in a
deeper layer than `(a, b)`. Treating unboxed tuples as a regular data type, we'd
make the same unboxing decisions as for `boxed` above; although our starting
budget is 5 (Here, the number of args is greater than -fmax-worker-args), it's
not enough to unbox the triple (we'd finish with budget -1). So we'd unbox `a`
through `c`, but not `d` through `f`, which is silly, because then we'd end up
having 6 arguments at runtime, of which `d` through `f` weren't unboxed.

Hence we pretend that the fields of unboxed tuples appear in the same budget
layer as the tuple itself. For example at the top-level, `(# x,y #)` is to be
treated just like two arguments `x` and `y`.
Of course, for that to work, our budget calculations must initialise
'max_wkr_args' to 5, based on the 'unariseArity' of each Core arg: That would be
1 for the pair and 4 for the unboxed pair. Then when we decide whether to unbox
the unboxed pair, we *directly* recurse into the fields, spending our budget
on retaining `c` and (after recursing once more) `d` through `f` as arguments,
depleting our budget completely in the first layer. Pictorially:

  5 ~> ][         (a,b) -4~>             ][         (# c, ... #)
       ][ {{      | |                 }} ][      c  -3~> ][ (# d, e, f #)
       ][ {{      | +-------+         }} ][      |       ][      d  -2~> ][      e  -1~> ][      f  -0~>
       ][ {{      |         |         }} ][      |       ][      |       ][      |       ][      |
       ][ {{      v         v         }} ][      v       ][      v       ][      v       ][      v
  0 ~> ][ {{ +1~> a -0~> ][ b -(-1)~> }} ][ +1~> I# -0~> ][ +1~> I# -0~> ][ +1~> I# -0~> ][ +1~> I# -0~>

As you can see, we have no budget left to justify unboxing `(a,b)` on the second
layer, which is good, because it would increase the number of args. Also note
that we can still unbox `c` through `f` in this layer, because doing so has a
net zero effect on budget.

Note [The OPAQUE pragma and avoiding the reboxing of arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In https://gitlab.haskell.org/ghc/ghc/-/issues/13143 it was identified that when
a function 'f' with a NOINLINE pragma is W/W transformed, then the worker for
'f' should get the NOINLINE annotation, while the wrapper /should/ be inlined.

That's because if the wrapper for 'f' had stayed NOINLINE, then any worker of a
W/W-transformed /caller of/ 'f' would immediately rebox any unboxed arguments
that is applied to the wrapper of 'f'. When the wrapper is inlined, that kind of
reboxing does not happen.

But now we have functions with OPAQUE pragmas, which by definition
(See Note [OPAQUE pragma]) do not get W/W-transformed. So in order to avoid
reboxing workers of any W/W-transformed /callers of/ 'f' we need to strip all
boxity information from 'f' in the demand analysis. This will inform the
W/W-transformation code that boxed arguments of 'f' must definitely be passed
along in boxed form and as such dissuade the creation of reboxing workers.
-}

-- | How many registers does this type take after unarisation?
unariseArity :: Type -> Arity
unariseArity ty = length (typePrimRep ty)

data Budgets = MkB !Arity Budgets   -- An infinite list of arity budgets

earnTopBudget :: Budgets -> Budgets
earnTopBudget (MkB n bg) = MkB (n+1) bg

spendTopBudget :: Arity -> Budgets -> Budgets
spendTopBudget m (MkB n bg) = MkB (n-m) bg

positiveTopBudget :: Budgets -> Bool
positiveTopBudget (MkB n _) = n >= 0

finaliseArgBoxities :: AnalEnv -> Id -> Arity
                    -> [Demand] -> Divergence
                    -> CoreExpr -> ([Demand], CoreExpr)
-- POSTCONDITION:
-- If:    (dmds', rhs') = finaliseArgBoxitities ... dmds .. rhs
-- Then:
--     dmds' is the same as dmds (including length), except for boxity info
--     rhs'  is the same as rhs, except for dmd info on lambda binders
-- NB: For join points, length dmds might be greater than ww_arity
finaliseArgBoxities env fn ww_arity arg_dmds div rhs

  -- Check for an OPAQUE function: see Note [OPAQUE pragma]
  -- In that case, trim off all boxity info from argument demands
  -- and demand info on lambda binders
  -- See Note [The OPAQUE pragma and avoiding the reboxing of arguments]
  | isOpaquePragma (idInlinePragma fn)
  , let trimmed_arg_dmds = map trimBoxity arg_dmds
  = (trimmed_arg_dmds, set_lam_dmds trimmed_arg_dmds rhs)

  -- Check that we have enough visible binders to match the
  -- ww arity; if not, we won't do worker/wrapper
  -- This happens if we have simply  {f = g} or a PAP {f = h 13}
  -- we simply want to give f the same demand signature as g
  -- How can such bindings arise?  Perhaps from {-# NOLINE[2] f #-},
  -- or if the call to `f` is currently not-applied (map f xs).
  -- It's a bit of a corner case.  Anyway for now we pass on the
  -- unadulterated demands from the RHS, without any boxity trimming.
  | ww_arity > count isId bndrs
  = (arg_dmds, rhs)

  -- The normal case
  | otherwise
  = -- pprTrace "finaliseArgBoxities" (
    --   vcat [text "function:" <+> ppr fn
    --        , text "max" <+> ppr max_wkr_args
    --        , text "dmds before:" <+> ppr (map idDemandInfo (filter isId bndrs))
    --        , text "dmds after: " <+>  ppr arg_dmds' ]) $
    (arg_dmds', set_lam_dmds arg_dmds' rhs)
    -- set_lam_dmds: we must attach the final boxities to the lambda-binders
    -- of the function, both because that's kosher, and because CPR analysis
    -- uses the info on the binders directly.
  where
    opts           = ae_opts env
    (bndrs, _body) = collectBinders rhs
       -- NB: in the interesting code path, count isId bndrs >= ww_arity

    arg_triples :: [(Type, StrictnessMark, Demand)]
    arg_triples = take ww_arity $
                  [ (idType bndr, NotMarkedStrict, get_dmd bndr)
                  | bndr <- bndrs, isRuntimeVar bndr ]

    arg_dmds' = ww_arg_dmds ++ map trimBoxity (drop ww_arity arg_dmds)
                -- If ww_arity < length arg_dmds, the leftover ones
                -- will not be w/w'd, so trimBoxity them
                -- See Note [Worker/wrapper arity and join points] point (3)

    -- This is the key line, which uses almost-circular programming
    -- The remaining budget from one layer becomes the initial
    -- budget for the next layer down.  See Note [Worker argument budget]
    (remaining_budget, ww_arg_dmds) = go_args (MkB max_wkr_args remaining_budget) arg_triples
    unarise_arity = sum [ unariseArity (idType b) | b <- bndrs, isId b ]
    max_wkr_args  = dmd_max_worker_args opts `max` unarise_arity
                    -- This is the budget initialisation step of
                    -- Note [Worker argument budget]

    get_dmd :: Id -> Demand
    get_dmd bndr
      | is_bot_fn = unboxDeeplyDmd dmd -- See Note [Boxity for bottoming functions],
      | otherwise = dmd                --     case (B)
      where
        dmd = idDemandInfo bndr

    -- is_bot_fn:  see Note [Boxity for bottoming functions]
    is_bot_fn = div == botDiv

    go_args :: Budgets -> [(Type,StrictnessMark,Demand)] -> (Budgets, [Demand])
    go_args bg triples = mapAccumL go_arg bg triples

    go_arg :: Budgets -> (Type,StrictnessMark,Demand) -> (Budgets, Demand)
    go_arg bg@(MkB bg_top bg_inner) (ty, str_mark, dmd@(n :* _))
      = case wantToUnboxArg env ty str_mark dmd of
          DropAbsent -> (bg, dmd)

          DontUnbox | is_bot_fn, isTyVarTy ty -> (retain_budget, dmd)
                    | otherwise               -> (retain_budget, trimBoxity dmd)
            -- If bot: Keep deep boxity even though WW won't unbox
            -- See Note [Boxity for bottoming functions] case (A)
            -- trimBoxity: see Note [No lazy, Unboxed demands in demand signature]
            where
              retain_budget = spendTopBudget (unariseArity ty) bg
                -- spendTopBudget: spend from our budget the cost of the
                -- retaining the arg
                -- The unboxed case does happen here, for example
                --   app g x = g x :: (# Int, Int #)
                -- here, `x` is used `L`azy and thus Boxed

          DoUnbox triples
            | isUnboxedTupleType ty
            , (bg', dmds') <- go_args bg triples
            -> (bg', n :* (mkProd Unboxed $! dmds'))
                     -- See Note [Worker argument budget]
                     -- unboxed tuples are always unboxed, deeply
                     -- NB: Recurse with bg, *not* bg_inner! The unboxed fields
                     -- are at the same budget layer.

            | isUnboxedSumType ty
            -> pprPanic "Unboxing through unboxed sum" (ppr fn <+> ppr ty)
                     -- We currently don't return DoUnbox for unboxed sums.
                     -- But hopefully we will at some point. When that happens,
                     -- it would still be impossible to predict the effect
                     -- of dropping absent fields and unboxing others on the
                     -- unariseArity of the sum without losing sanity.
                     -- We could overwrite bg_top with the one from
                     -- retain_budget while still unboxing inside the alts as in
                     -- the tuple case for a conservative solution, though.

            | otherwise
            -> (spendTopBudget 1 (MkB bg_top final_bg_inner), final_dmd)
            where
              (bg_inner', dmds') = go_args (earnTopBudget bg_inner) triples
                     -- earnTopBudget: give back the cost of retaining the
                     -- arg we are insted unboxing.
              dmd' = n :* (mkProd Unboxed $! dmds')
              ~(final_bg_inner, final_dmd) -- "~": This match *must* be lazy!
                 | positiveTopBudget bg_inner' = (bg_inner', dmd')
                 | otherwise                   = (bg_inner,  trimBoxity dmd)

    set_lam_dmds :: [Demand] -> CoreExpr -> CoreExpr
    -- Attach the demands to the outer lambdas of this expression
    set_lam_dmds (dmd:dmds) (Lam v e)
      | isTyVar v = Lam v (set_lam_dmds (dmd:dmds) e)
      | otherwise = Lam (v `setIdDemandInfo` dmd) (set_lam_dmds dmds e)
    set_lam_dmds dmds (Cast e co) = Cast (set_lam_dmds dmds e) co
       -- This case happens for an OPAQUE function, which may look like
       --     f = (\x y. blah) |> co
       -- We give it strictness but no boxity (#22502)
    set_lam_dmds _ e = e
       -- In the OPAQUE case, the list of demands at this point might be
       -- non-empty, e.g., when looking at a PAP. Hence don't panic (#22997).

finaliseLetBoxity
  :: AnalEnv
  -> Type                   -- ^ Type of the let-bound Id
  -> Demand                 -- ^ How the Id is used
  -> Demand
-- See Note [Finalising boxity for let-bound Ids]
-- This function is like finaliseArgBoxities, but much simpler because
-- it has no "budget".  It simply unboxes strict demands, and stops
-- when it reaches a lazy one.
finaliseLetBoxity env ty dmd
  = go (ty, NotMarkedStrict, dmd)
  where
    go :: (Type,StrictnessMark,Demand) -> Demand
    go (ty, str, dmd@(n :* _)) =
      case wantToUnboxArg env ty str dmd of
        DropAbsent      -> dmd
        DontUnbox       -> trimBoxity dmd
        DoUnbox triples -> n :* (mkProd Unboxed $! map go triples)

wantToUnboxArg :: AnalEnv -> Type -> StrictnessMark -> Demand
               -> UnboxingDecision [(Type, StrictnessMark, Demand)]
wantToUnboxArg env ty str_mark dmd@(n :* _)
  = case canUnboxArg (ae_fam_envs env) ty dmd of
      DropAbsent -> DropAbsent
      DontUnbox  -> DontUnbox

      DoUnbox (DataConPatContext{ dcpc_dc      = dc
                                , dcpc_tc_args = tc_args
                                , dcpc_args    = dmds })
       -- OK, so we /can/ unbox it; but do we /want/ to?
       | not (isStrict n || isMarkedStrict str_mark)   -- Don't unbox a lazy field
         -- isMarkedStrict: see Note [Unboxing evaluated arguments] in DmdAnal
       -> DontUnbox

       | doNotUnbox ty
       -> DontUnbox  -- See Note [Do not unbox class dictionaries]
                     -- NB: 'ty' has not been normalised, so this will (rightly)
                     --     catch newtype dictionaries too.
                     -- NB: even for bottoming functions, don't unbox dictionaries

       | DefinitelyRecursive <- ae_rec_dc env dc
         -- See Note [Which types are unboxed?]
         -- and Note [Demand analysis for recursive data constructors]
       -> DontUnbox

       | otherwise  -- Bad cases dealt with: we want to unbox!
       -> DoUnbox (zip3 (dubiousDataConInstArgTys dc tc_args)
                        (dataConRepStrictness dc)
                        dmds)


doNotUnbox :: Type -> Bool
-- Do not unbox class dictionaries, except equality classes and tuples
-- Note [Do not unbox class dictionaries]
doNotUnbox arg_ty
  = case tyConAppTyCon_maybe arg_ty of
      Just tc | Just cls <- tyConClass_maybe tc
              -> not (isEqualityClass cls || isCTupleClass cls)
       -- See (DNB2) and (DNB1) in Note [Do not unbox class dictionaries]

      _ -> False

{- Note [Do not unbox class dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We never unbox class dictionaries in worker/wrapper.

1. INLINABLE functions
   If we have
      f :: Ord a => [a] -> Int -> a
      {-# INLINABLE f #-}
   and we worker/wrapper f, we'll get a worker with an INLINABLE pragma
   (see Note [Worker/wrapper for INLINABLE functions] in GHC.Core.Opt.WorkWrap),
   which can still be specialised by the type-class specialiser, something like
      fw :: Ord a => [a] -> Int# -> a

   BUT if f is strict in the Ord dictionary, we might unpack it, to get
      fw :: (a->a->Bool) -> [a] -> Int# -> a
   and the type-class specialiser can't specialise that. An example is #6056.

   Historical note: #14955 describes how I got this fix wrong the first time.
   I got aware of the issue in T5075 by the change in boxity of loop between
   demand analysis runs.

2. -fspecialise-aggressively.  As #21286 shows, the same phenomenon can occur
   occur without INLINABLE, when we use -fexpose-all-unfoldings and
   -fspecialise-aggressively to do vigorous cross-module specialisation.

3. #18421 found that unboxing a dictionary can also make the worker less likely
   to inline; the inlining heuristics seem to prefer to inline a function
   applied to a dictionary over a function applied to a bunch of functions.

TL;DR we /never/ unbox class dictionaries. Unboxing the dictionary, and passing
a raft of higher-order functions isn't a huge win anyway -- you really want to
specialise the function.

Wrinkle (DNB1): we /do/ want to unbox tuple dictionaries (#23398)
     f :: (% Eq a, Show a %) => blah
  with -fdicts-strict it is great to unbox to
     $wf :: Eq a => Show a => blah
  (where I have written out the currying explicitly).  Now we can specialise
  $wf on the Eq or Show dictionary.  Nothing is lost.

  And something is gained.  It is possible that `f` will look like this:
     f = /\a. \d:(% Eq a, Show a %). ... f @a (% sel1 d, sel2 d %)...
  where there is a recurive call to `f`, or to another function that takes the
  same tuple dictionary, but where the tuple is built from the components of
  `d`.  The Simplier does not fix this.  But if we unpacked the dictionary
  we'd get
     $wf = /\a. \(d1:Eq a) (d2:Show a). let d = (% d1, d2 %)
             in ...f @a (% sel1 d, sel2 d %)
  and all the tuple building and taking apart will disappear.

Wrinkle (DNB2): we /do/ want to unbox equality dictionaries,
  for (~), (~~), and Coercible (#23398).  Their payload is a single unboxed
  coercion.  We never want to specialise on `(t1 ~ t2)`.  All that would do is
  to make a copy of the function's RHS with a particular coercion.  Unlike
  normal class methods, that does not unlock any new optimisation
  opportunities in the specialised RHS.
-}

{- *********************************************************************
*                                                                      *
                      Fixpoints
*                                                                      *
********************************************************************* -}

-- Recursive bindings
dmdFix :: TopLevelFlag
       -> AnalEnv                            -- Does not include bindings for this binding
       -> SubDemand
       -> [(Id,CoreExpr)]
       -> (AnalEnv, WeakDmds, [(Id,CoreExpr)]) -- Binders annotated with strictness info
dmdFix top_lvl env let_dmd orig_pairs
  = loop 1 initial_pairs
  where
    opts = ae_opts env
    -- See Note [Initialising strictness]
    initial_pairs | ae_virgin env = [(setIdDmdAndBoxSig opts id botSig, rhs) | (id, rhs) <- orig_pairs ]
                  | otherwise     = orig_pairs

    -- If fixed-point iteration does not yield a result we use this instead
    -- See Note [Safe abortion in the fixed-point iteration]
    abort :: (AnalEnv, WeakDmds, [(Id,CoreExpr)])
    abort = (env, weak_fv', zapped_pairs)
      where (weak_fv, pairs') = step True (zapIdDmdSig orig_pairs)
            -- Note [Lazy and unleashable free variables]
            weak_fvs = plusVarEnvList $ map (de_fvs . dmdSigDmdEnv . idDmdSig . fst) pairs'
            weak_fv'     = plusVarEnv_C plusDmd weak_fv $ mapVarEnv (const topDmd) weak_fvs
            zapped_pairs = zapIdDmdSig pairs'

    -- The fixed-point varies the idDmdSig field of the binders, and terminates if that
    -- annotation does not change any more.
    loop :: Int -> [(Id,CoreExpr)] -> (AnalEnv, WeakDmds, [(Id,CoreExpr)])
    loop n pairs = -- pprTrace "dmdFix" (ppr n <+> vcat [ ppr id <+> ppr (idDmdSig id)
                   --                                   | (id,_) <- pairs]) $
                   loop' n pairs

    loop' n pairs
      | found_fixpoint = (final_anal_env, weak_fv, pairs')
      | n == 10        = abort
      | otherwise      = loop (n+1) pairs'
      where
        found_fixpoint    = map (idDmdSig . fst) pairs' == map (idDmdSig . fst) pairs
        first_round       = n == 1
        (weak_fv, pairs') = step first_round pairs
        final_anal_env    = extendAnalEnvs top_lvl env (map fst pairs')

    step :: Bool -> [(Id, CoreExpr)] -> (WeakDmds, [(Id, CoreExpr)])
    step first_round pairs = (weak_fv, pairs')
      where
        -- In all but the first iteration, delete the virgin flag
        start_env | first_round = env
                  | otherwise   = nonVirgin env

        start = (extendAnalEnvs top_lvl start_env (map fst pairs), emptyVarEnv)

        !((_,!weak_fv), !pairs') = mapAccumL my_downRhs start pairs
                -- mapAccumL: Use the new signature to do the next pair
                -- The occurrence analyser has arranged them in a good order
                -- so this can significantly reduce the number of iterations needed

        my_downRhs (env, weak_fv) (id,rhs)
          = -- pprTrace "my_downRhs" (ppr id $$ ppr (idDmdSig id) $$ ppr sig) $
            ((env', weak_fv'), (id', rhs'))
          where
            !(!env', !weak_fv1, !id', !rhs') = dmdAnalRhsSig top_lvl Recursive env let_dmd id rhs
            !weak_fv'                    = plusVarEnv_C plusDmd weak_fv weak_fv1

    zapIdDmdSig :: [(Id, CoreExpr)] -> [(Id, CoreExpr)]
    zapIdDmdSig pairs = [(setIdDmdSig id nopSig, rhs) | (id, rhs) <- pairs ]

{- Note [Safe abortion in the fixed-point iteration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Fixed-point iteration may fail to terminate. But we cannot simply give up and
return the environment and code unchanged! We still need to do one additional
round, for two reasons:

 * To get information on used free variables (both lazy and strict!)
   (see Note [Lazy and unleashable free variables])
 * To ensure that all expressions have been traversed at least once, and any left-over
   strictness annotations have been updated.

This final iteration does not add the variables to the strictness signature
environment, which effectively assigns them 'nopSig' (see "getStrictness")

Note [Trimming a demand to a type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are two reasons we sometimes trim a demand to match a type.
  1. GADTs
  2. Recursive products and widening

More on both below.  But the bottom line is: we really don't want to
have a binder whose demand is more deeply-nested than its type
"allows". So in findBndrDmd we call trimToType and findTypeShape to
trim the demand on the binder to a form that matches the type

Now to the reasons. For (1) consider
  f :: a -> Bool
  f x = case ... of
          A g1 -> case (x |> g1) of (p,q) -> ...
          B    -> error "urk"

where A,B are the constructors of a GADT.  We'll get a 1P(L,L) demand
on x from the A branch, but that's a stupid demand for x itself, which
has type 'a'. Indeed we get ASSERTs going off (notably in
splitUseProdDmd, #8569).

For (2) consider
  data T = MkT Int T    -- A recursive product
  f :: Int -> T -> Int
  f 0 _         = 0
  f _ (MkT n t) = f n t

Here f is lazy in T, but its *usage* is infinite: P(L,P(L,P(L, ...))).
Notice that this happens because T is a product type, and is recursive.
If we are not careful, we'll fail to iterate to a fixpoint in dmdFix,
and bale out entirely, which is inefficient and over-conservative.

Worse, as we discovered in #18304, the size of the usages we compute
can grow /exponentially/, so even 10 iterations costs far too much.
Especially since we then discard the result.

To avoid this we use the same findTypeShape function as for (1), but
arrange that it trims the demand if it encounters the same type constructor
twice (or three times, etc).  We use our standard RecTcChecker mechanism
for this -- see GHC.Core.Opt.WorkWrap.Utils.findTypeShape.

This is usually call "widening".  We could do it just in dmdFix, but
since are doing this findTypeShape business /anyway/ because of (1),
and it has all the right information to hand, it's extremely
convenient to do it there.

-}

{- *********************************************************************
*                                                                      *
                 Strictness signatures and types
*                                                                      *
********************************************************************* -}

noArgsDmdType :: DmdEnv -> DmdType
noArgsDmdType dmd_env = DmdType dmd_env []

coercionDmdEnv :: Coercion -> DmdEnv
coercionDmdEnv co = coercionsDmdEnv [co]

coercionsDmdEnv :: [Coercion] -> DmdEnv
coercionsDmdEnv cos
  = mkTermDmdEnv $ mapVarEnv (const topDmd) $ getUniqSet $ coVarsOfCos cos
  -- The VarSet from coVarsOfCos is really a VarEnv Var

addVarDmd :: DmdType -> Var -> Demand -> DmdType
addVarDmd (DmdType fv ds) var dmd
  = DmdType (addVarDmdEnv fv var dmd) ds

addWeakFVs :: DmdType -> WeakDmds -> DmdType
addWeakFVs dmd_ty weak_fvs
  = dmd_ty `plusDmdType` mkTermDmdEnv weak_fvs
        -- Using plusDmdType (rather than just plus'ing the envs)
        -- is vital.  Consider
        --      let f = \x -> (x,y)
        --      in  error (f 3)
        -- Here, y is treated as a lazy-fv of f, but we must `plusDmd` that L
        -- demand with the bottom coming up from 'error'
        --
        -- I got a loop in the fixpointer without this, due to an interaction
        -- with the weak_fv filtering in dmdAnalRhsSig.  Roughly, it was
        --      letrec f n x
        --          = letrec g y = x `fatbar`
        --                         letrec h z = z + ...g...
        --                         in h (f (n-1) x)
        --      in ...
        -- In the initial iteration for f, f=Bot
        -- Suppose h is found to be strict in z, but the occurrence of g in its RHS
        -- is lazy.  Now consider the fixpoint iteration for g, esp the demands it
        -- places on its free variables.  Suppose it places none.  Then the
        --      x `fatbar` ...call to h...
        -- will give a x->V demand for x.  That turns into a L demand for x,
        -- which floats out of the defn for h.  Without the modifyEnv, that
        -- L demand doesn't get both'd with the Bot coming up from the inner
        -- call to f.  So we just get an L demand for x for g.

setBndrsDemandInfo :: HasDebugCallStack => [Var] -> [Demand] -> [Var]
setBndrsDemandInfo (b:bs) ds
  | isTyVar b = b : setBndrsDemandInfo bs ds
setBndrsDemandInfo (b:bs) (d:ds) =
    let !new_info = setIdDemandInfo b d
        !vars = setBndrsDemandInfo bs ds
    in new_info : vars
setBndrsDemandInfo [] ds = assert (null ds) []
setBndrsDemandInfo bs _  = pprPanic "setBndrsDemandInfo" (ppr bs)

annotateLamIdBndr :: AnalEnv
                  -> DmdType    -- Demand type of body
                  -> Id         -- Lambda binder
                  -> WithDmdType Id  -- Demand type of lambda
                                     -- and binder annotated with demand

annotateLamIdBndr env dmd_ty id
-- For lambdas we add the demand to the argument demands
-- Only called for Ids
  = assert (isId id) $
    -- pprTrace "annLamBndr" (vcat [ppr id, ppr dmd_ty, ppr final_ty]) $
    WithDmdType main_ty new_id
  where
    new_id  = setIdDemandInfo id dmd
    main_ty = addDemand dmd dmd_ty'
    WithDmdType dmd_ty' dmd = findBndrDmd env dmd_ty id

{- Note [NOINLINE and strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At one point we disabled strictness for NOINLINE functions, on the
grounds that they should be entirely opaque.  But that lost lots of
useful semantic strictness information, so now we analyse them like
any other function, and pin strictness information on them.

That in turn forces us to worker/wrapper them; see
Note [Worker/wrapper for NOINLINE functions] in GHC.Core.Opt.WorkWrap.


Note [Lazy and unleashable free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We put the strict and once-used FVs in the DmdType of the Id, so
that at its call sites we unleash demands on its strict fvs.
An example is 'roll' in imaginary/wheel-sieve2
Something like this:
        roll x = letrec
                     go y = if ... then roll (x-1) else x+1
                 in
                 go ms
We want to see that roll is strict in x, which is because
go is called.   So we put the DmdEnv for x in go's DmdType.

Another example:

        f :: Int -> Int -> Int
        f x y = let t = x+1
            h z = if z==0 then t else
                  if z==1 then x+1 else
                  x + h (z-1)
        in h y

Calling h does indeed evaluate x, but we can only see
that if we unleash a demand on x at the call site for t.

Incidentally, here's a place where lambda-lifting h would
lose the cigar --- we couldn't see the joint strictness in t/x

        ON THE OTHER HAND

We don't want to put *all* the fv's from the RHS into the
DmdType. Because

 * it makes the strictness signatures larger, and hence slows down fixpointing

and

 * it is useless information at the call site anyways:
   For lazy, used-many times fv's we will never get any better result than
   that, no matter how good the actual demand on the function at the call site
   is (unless it is always absent, but then the whole binder is useless).

Therefore we exclude lazy multiple-used fv's from the environment in the
DmdType.

But now the signature lies! (Missing variables are assumed to be absent.) To
make up for this, the code that analyses the binding keeps the demand on those
variable separate (usually called "weak_fv") and adds it to the demand of the
whole binding later.

What if we decide _not_ to store a strictness signature for a binding at all, as
we do when aborting a fixed-point iteration? The we risk losing the information
that the strict variables are being used. In that case, we take all free variables
mentioned in the (unsound) strictness signature, conservatively approximate the
demand put on them (topDmd), and add that to the "weak_fv" returned by "dmdFix".


************************************************************************
*                                                                      *
\subsection{Strictness signatures}
*                                                                      *
************************************************************************
-}


data AnalEnv = AE
  { ae_opts      :: !DmdAnalOpts
  -- ^ Analysis options
  , ae_sigs      :: !SigEnv
  , ae_virgin    :: !Bool
  -- ^ True on first iteration only. See Note [Initialising strictness]
  , ae_fam_envs  :: !FamInstEnvs
  , ae_rec_dc    :: DataCon -> IsRecDataConResult
  -- ^ Memoised result of 'GHC.Core.Opt.WorkWrap.Utils.isRecDataCon'
  }

        -- We use the se_env to tell us whether to
        -- record info about a variable in the DmdEnv
        -- We do so if it's a LocalId, but not top-level
        --
        -- The DmdEnv gives the demand on the free vars of the function
        -- when it is given enough args to satisfy the strictness signature

type SigEnv = VarEnv (DmdSig, TopLevelFlag)

instance Outputable AnalEnv where
  ppr env = text "AE" <+> braces (vcat
         [ text "ae_virgin =" <+> ppr (ae_virgin env)
         , text "ae_sigs =" <+> ppr (ae_sigs env)
         ])

emptyAnalEnv :: DmdAnalOpts -> FamInstEnvs -> AnalEnv
emptyAnalEnv opts fam_envs
    = AE { ae_opts         = opts
         , ae_sigs         = emptySigEnv
         , ae_virgin       = True
         , ae_fam_envs     = fam_envs
         , ae_rec_dc       = memoiseUniqueFun (isRecDataCon fam_envs 3)
         }

-- | Unset the 'dmd_strict_dicts' flag if any of the given bindings is a DFun
-- binding. Part of the mechanism that detects
-- Note [Do not strictify a DFun's parameter dictionaries].
enterDFun :: CoreBind -> AnalEnv -> AnalEnv
enterDFun bind env
  | any isDFunId (bindersOf bind)
  = env { ae_opts = (ae_opts env) { dmd_strict_dicts = False } }
  | otherwise
  = env

emptySigEnv :: SigEnv
emptySigEnv = emptyVarEnv

-- | Extend an environment with the strictness sigs attached to the Ids
extendAnalEnvs :: TopLevelFlag -> AnalEnv -> [Id] -> AnalEnv
extendAnalEnvs top_lvl env vars
  = env { ae_sigs = extendSigEnvs top_lvl (ae_sigs env) vars }

extendSigEnvs :: TopLevelFlag -> SigEnv -> [Id] -> SigEnv
extendSigEnvs top_lvl sigs vars
  = extendVarEnvList sigs [ (var, (idDmdSig var, top_lvl)) | var <- vars]

extendAnalEnv :: TopLevelFlag -> AnalEnv -> Id -> DmdSig -> AnalEnv
extendAnalEnv top_lvl env var sig
  = env { ae_sigs = extendSigEnv top_lvl (ae_sigs env) var sig }

extendSigEnv :: TopLevelFlag -> SigEnv -> Id -> DmdSig -> SigEnv
extendSigEnv top_lvl sigs var sig = extendVarEnv sigs var (sig, top_lvl)

lookupSigEnv :: AnalEnv -> Id -> Maybe (DmdSig, TopLevelFlag)
lookupSigEnv env id = lookupVarEnv (ae_sigs env) id

addInScopeAnalEnv :: AnalEnv -> Var -> AnalEnv
addInScopeAnalEnv env id = env { ae_sigs = delVarEnv (ae_sigs env) id }

addInScopeAnalEnvs :: AnalEnv -> [Var] -> AnalEnv
addInScopeAnalEnvs env ids = env { ae_sigs = delVarEnvList (ae_sigs env) ids }

nonVirgin :: AnalEnv -> AnalEnv
nonVirgin env = env { ae_virgin = False }

findBndrsDmds :: AnalEnv -> DmdType -> [Var] -> WithDmdType [Demand]
-- Return the demands on the Ids in the [Var]
findBndrsDmds env dmd_ty bndrs
  = go dmd_ty bndrs
  where
    go dmd_ty []  = WithDmdType dmd_ty []
    go dmd_ty (b:bs)
      | isId b    = let WithDmdType dmd_ty1 dmds = go dmd_ty bs
                        WithDmdType dmd_ty2 dmd  = findBndrDmd env dmd_ty1 b
                    in WithDmdType dmd_ty2  (dmd : dmds)
      | otherwise = go dmd_ty bs

findBndrDmd :: AnalEnv -> DmdType -> Id -> WithDmdType Demand
-- See Note [Trimming a demand to a type]
findBndrDmd env dmd_ty id
  = -- pprTrace "findBndrDmd" (ppr id $$ ppr dmd_ty $$ ppr starting_dmd $$ ppr dmd') $
    WithDmdType dmd_ty' dmd'
  where
    dmd' = strictify $
           trimToType starting_dmd (findTypeShape fam_envs id_ty)

    (dmd_ty', starting_dmd) = peelFV dmd_ty id

    id_ty = idType id

    strictify dmd
      -- See Note [Making dictionary parameters strict]
      -- and Note [Do not strictify a DFun's parameter dictionaries]
      | dmd_strict_dicts (ae_opts env)
      = strictifyDictDmd id_ty dmd
      | otherwise
      = dmd

    fam_envs = ae_fam_envs env

{- Note [Bringing a new variable into scope]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f x = blah
   g = ...(\f. ...f...)...

In the body of the '\f', any occurrence of `f` refers to the lambda-bound `f`,
not the top-level `f` (which will be in `ae_sigs`).  So it's very important
to delete `f` from `ae_sigs` when we pass a lambda/case/let-up binding of `f`.
Otherwise chaos results (#22718).

Note [Making dictionary parameters strict]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Opt_DictsStrict flag makes GHC use call-by-value for dictionaries.  Why?

* Generally CBV is more efficient.

* A datatype dictionary is always non-bottom and never takes much work to
  compute.  E.g. a DFun from an instance decl always returns a dictionary
  record immediately.  See DFunUnfolding in CoreSyn.
  See also Note [Recursive superclasses] in TcInstDcls.

See #17758 for more background and perf numbers.

Wrinkles:

* A newtype dictionary is *not* always non-bottom.  E.g.
      class C a where op :: a -> a
      instance C Int where op = error "urk"
  Now a value of type (C Int) is just a newtype wrapper (a cast) around
  the error thunk.  Don't strictify these!

* Strictifying DFuns risks destroying the invariant that DFuns never take much
  work to compute, so we don't do it.
  See Note [Do not strictify a DFun's parameter dictionaries] for details.

* Although worker/wrapper *could* unbox strictly used dictionaries, we do not do
  so; see Note [Do not unbox class dictionaries].

The implementation is extremely simple: just make the strictness
analyser strictify the demand on a dictionary binder in
'findBndrDmd' if the binder does not belong to a DFun.

Note [Do not strictify a DFun's parameter dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The typechecker can tie recursive knots involving (non-recursive) DFuns, so
we must not strictify a DFun's parameter dictionaries (#22549).
T22549 has an example involving undecidable instances that <<loop>>s when we
strictify the DFun of, e.g., `$fEqSeqT`:

  Main.$fEqSeqT
    = \@m @a ($dEq :: Eq (m (ViewT m a))) ($dMonad :: Monad m) ->
        GHC.Classes.C:Eq @(SeqT m a) ($c== @m @a $dEq $dMonad)
                                     ($c/= @m @a $dEq $dMonad)

  Rec {
    $dEq_a = Main.$fEqSeqT @Identity @Int $dEq_b Main.$fMonadIdentity
    $dEq_b = ... $dEq_a ... <another strict context due to DFun>
  }

If we make `$fEqSeqT` strict in `$dEq`, we'll collapse the Rec group into a
giant, <<loop>>ing thunk.

To prevent that, we never strictify dictionary params when inside a DFun.
That is implemented by unsetting 'dmd_strict_dicts' when entering a DFun.

See also Note [Speculative evaluation] in GHC.CoreToStg.Prep which has a rather
similar example in #20836. We may never speculate *arguments* of (recursive)
DFun calls, likewise we should not mark *formal parameters* of recursive DFuns
as strict.

Note [Initialising strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See section 9.2 (Finding fixpoints) of the paper.

Our basic plan is to initialise the strictness of each Id in a
recursive group to "bottom", and find a fixpoint from there.  However,
this group B might be inside an *enclosing* recursive group A, in
which case we'll do the entire fixpoint shebang on for each iteration
of A. This can be illustrated by the following example:

Example:

  f [] = []
  f (x:xs) = let g []     = f xs
                 g (y:ys) = y+1 : g ys
              in g (h x)

At each iteration of the fixpoint for f, the analyser has to find a
fixpoint for the enclosed function g. In the meantime, the demand
values for g at each iteration for f are *greater* than those we
encountered in the previous iteration for f. Therefore, we can begin
the fixpoint for g not with the bottom value but rather with the
result of the previous analysis. I.e., when beginning the fixpoint
process for g, we can start from the demand signature computed for g
previously and attached to the binding occurrence of g.

To speed things up, we initialise each iteration of A (the enclosing
one) from the result of the last one, which is neatly recorded in each
binder.  That way we make use of earlier iterations of the fixpoint
algorithm. (Cunning plan.)

But on the *first* iteration we want to *ignore* the current strictness
of the Id, and start from "bottom".  Nowadays the Id can have a current
strictness, because interface files record strictness for nested bindings.
To know when we are in the first iteration, we look at the ae_virgin
field of the AnalEnv.


Note [Final Demand Analyser run]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some of the information that the demand analyser determines is not always
preserved by the simplifier.  For example, the simplifier will happily rewrite
  \y [Demand=MU] let x = y in x + x
to
  \y [Demand=MU] y + y
which is quite a lie: Now y occurs more than just once.

The once-used information is (currently) only used by the code
generator, though.  So:

 * We zap the used-once info in the worker-wrapper;
   see Note [Zapping Used Once info in WorkWrap] in
   GHC.Core.Opt.WorkWrap.
   If it's not reliable, it's better not to have it at all.

 * Just before TidyCore, we add a pass of the demand analyser,
      but WITHOUT subsequent worker/wrapper and simplifier,
   right before TidyCore.  See SimplCore.getCoreToDo.

   This way, correct information finds its way into the module interface
   (strictness signatures!) and the code generator (single-entry thunks!)

Note that, in contrast, the single-call information (C(M,..)) /can/ be
relied upon, as the simplifier tends to be very careful about not
duplicating actual function calls.

Also see #11731.

Note [Space Leaks in Demand Analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ticket: #15455
MR: !5399

In the past the result of demand analysis was not forced until the whole module
had finished being analysed. In big programs, this led to a big build up of thunks
which were all ultimately forced at the end of the analysis.

This was because the return type of the analysis was a lazy pair:
  dmdAnal :: AnalEnv -> SubDemand -> CoreExpr -> (DmdType, CoreExpr)
To avoid space leaks we added extra bangs to evaluate the DmdType component eagerly; but
we were never sure we had added enough.
The easiest way to systematically fix this was to use a strict pair type for the
return value of the analysis so that we can be more confident that the result
is incrementally computed rather than all at the end.

A second, only loosely related point is that
the updating of Ids was not forced because the result of updating
an Id was placed into a lazy field in CoreExpr. This meant that until the end of
demand analysis, the unforced Ids would retain the DmdEnv which the demand information
was fetch from. Now we are quite careful to force Ids before putting them
back into core expressions so that we can garbage-collect the environments more eagerly.
For example see the `Case` branch of `dmdAnal'` where `case_bndr'` is forced
or `dmdAnalSumAlt`.

The net result of all these improvements is the peak live memory usage of compiling
jsaddle-dom decreases about 4GB (from 6.5G to 2.5G). A bunch of bytes allocated benchmarks also
decrease because we allocate a lot fewer thunks which we immediately overwrite and
also runtime for the pass is faster! Overall, good wins.

-}
