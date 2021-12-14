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

import GHC.Core.Opt.WorkWrap.Utils
import GHC.Types.Demand   -- All of it
import GHC.Core
import GHC.Core.Multiplicity ( scaledThing )
import GHC.Utils.Outputable
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Basic
import Data.List        ( mapAccumL )
import GHC.Core.DataCon
import GHC.Types.ForeignCall ( isSafeForeignCall )
import GHC.Types.Id
import GHC.Core.Utils
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Predicate ( isClassPred )
import GHC.Core.FVs      ( rulesRhsFreeIds, bndrRuleAndUnfoldingIds )
import GHC.Core.Coercion ( Coercion )
import GHC.Core.TyCo.FVs ( coVarsOfCos )
import GHC.Core.FamInstEnv
import GHC.Core.Opt.Arity ( typeArity )
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Data.Maybe         ( isJust, orElse )
import GHC.Builtin.PrimOps
import GHC.Builtin.Types.Prim ( realWorldStatePrimTy )
import GHC.Types.Unique.Set

import GHC.Utils.Trace
_ = pprTrace -- Tired of commenting out the import all the time

{-
************************************************************************
*                                                                      *
\subsection{Top level stuff}
*                                                                      *
************************************************************************
-}

-- | Options for the demand analysis
data DmdAnalOpts = DmdAnalOpts
   { dmd_strict_dicts    :: !Bool -- ^ Use strict dictionaries
   , dmd_unbox_width     :: !Int  -- ^ Use strict dictionaries
   , dmd_max_worker_args :: !Int
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
-- Note: use `seqBinds` on the result to avoid leaks due to lazyness (cf Note
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
          = WithDmdType (add_exported_uses env' body_ty (bindersOf b)) bs'

    cons_up :: WithDmdType (DmdResult b [b]) -> WithDmdType [b]
    cons_up (WithDmdType dmd_ty (R b' bs')) = WithDmdType dmd_ty (b' : bs')

    add_exported_uses :: AnalEnv -> DmdType -> [Id] -> DmdType
    add_exported_uses env = foldl' (add_exported_use env)

    -- | If @e@ is denoted by @dmd_ty@, then @add_exported_use _ dmd_ty id@
    -- corresponds to the demand type of @(id, e)@, but is a lot more direct.
    -- See Note [Analysing top-level bindings].
    add_exported_use :: AnalEnv -> DmdType -> Id -> DmdType
    add_exported_use env dmd_ty id
      | isExportedId id || elemVarSet id rule_fvs
      -- See Note [Absence analysis for stable unfoldings and RULES]
      = dmd_ty `plusDmdType` fst (dmdAnalStar env topDmd (Var id))
      | otherwise
      = dmd_ty

    rule_fvs :: IdSet
    rule_fvs = rulesRhsFreeIds rules

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
isInterestingTopLevelFn id =
  typeArity (idType id) `lengthExceeds` 0

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
is evaluated. Thus, we'd like it to have idDemandInfo @LCL(CM(P(1L,A))@.
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
    | useLetUp top_lvl id
    -> dmdAnalBindLetUp   top_lvl env     id rhs anal_body
  _ -> dmdAnalBindLetDown top_lvl env dmd bind   anal_body

-- | Annotates uninteresting top level functions ('isInterestingTopLevelFn')
-- with 'topDmd', the rest with the given demand.
setBindIdDemandInfo :: TopLevelFlag -> Id -> Demand -> Id
setBindIdDemandInfo top_lvl id dmd = setIdDemandInfo id $ case top_lvl of
  TopLevel | not (isInterestingTopLevelFn id) -> topDmd
  _                                           -> dmd

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
    WithDmdType body_ty body'   = anal_body env
    WithDmdType body_ty' id_dmd = findBndrDmd env body_ty id
    -- See Note [Finalising boxity for demand signatures]

    id_dmd'            = finaliseLetBoxity (ae_fam_envs env) (idType id) id_dmd
    !id'               = setBindIdDemandInfo top_lvl id id_dmd'
    (rhs_ty, rhs')     = dmdAnalStar env (dmdTransformThunkDmd rhs id_dmd') rhs

    -- See Note [Absence analysis for stable unfoldings and RULES]
    rule_fvs           = bndrRuleAndUnfoldingIds id
    final_ty           = body_ty' `plusDmdType` rhs_ty `keepAliveDmdType` rule_fvs

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
    | (env', lazy_fv, id1, rhs1) <-
        dmdAnalRhsSig top_lvl NonRecursive env dmd id rhs
    -> do_rest env' lazy_fv [(id1, rhs1)] (uncurry NonRec . only)
  Rec pairs
    | (env', lazy_fv, pairs') <- dmdFix top_lvl env dmd pairs
    -> do_rest env' lazy_fv pairs' Rec
  where
    do_rest env' lazy_fv pairs1 build_bind = WithDmdType final_ty (R (build_bind pairs2) body')
      where
        WithDmdType body_ty body'        = anal_body env'
        -- see Note [Lazy and unleashable free variables]
        dmd_ty                          = addLazyFVs body_ty lazy_fv
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

-- If e is complicated enough to become a thunk, its contents will be evaluated
-- at most once, so oneify it.
dmdTransformThunkDmd :: CoreExpr -> Demand -> Demand
dmdTransformThunkDmd e
  | exprIsTrivial e = id
  | otherwise       = oneifyDmd

-- Do not process absent demands
-- Otherwise act like in a normal demand analysis
-- See ↦* relation in the Cardinality Analysis paper
dmdAnalStar :: AnalEnv
            -> Demand   -- This one takes a *Demand*
            -> CoreExpr -- Should obey the let/app invariant
            -> (PlusDmdArg, CoreExpr)
dmdAnalStar env (n :* sd) e
  -- NB: (:*) expands AbsDmd and BotDmd as needed
  -- See Note [Analysing with absent demand]
  | WithDmdType dmd_ty e' <- dmdAnal env sd e
  = assertPpr (not (isUnliftedType (exprType e)) || exprOkForSpeculation e) (ppr e)
    -- The argument 'e' should satisfy the let/app invariant
    (toPlusDmdArg $ multDmdType n dmd_ty, e')

-- Main Demand Analsysis machinery
dmdAnal, dmdAnal' :: AnalEnv
        -> SubDemand         -- The main one takes a *SubDemand*
        -> CoreExpr -> WithDmdType CoreExpr

dmdAnal env d e = -- pprTrace "dmdAnal" (ppr d <+> ppr e) $
                  dmdAnal' env d e

dmdAnal' _ _ (Lit lit)     = WithDmdType nopDmdType (Lit lit)
dmdAnal' _ _ (Type ty)     = WithDmdType nopDmdType (Type ty) -- Doesn't happen, in fact
dmdAnal' _ _ (Coercion co)
  = WithDmdType (unitDmdType (coercionDmdEnv co)) (Coercion co)

dmdAnal' env dmd (Var var)
  = WithDmdType (dmdTransform env var dmd) (Var var)

dmdAnal' env dmd (Cast e co)
  = WithDmdType (dmd_ty `plusDmdType` mkPlusDmdArg (coercionDmdEnv co)) (Cast e' co)
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
        (arg_ty, arg')    = dmdAnalStar env (dmdTransformThunkDmd arg arg_dmd) arg
    in
--    pprTrace "dmdAnal:app" (vcat
--         [ text "dmd =" <+> ppr dmd
--         , text "expr =" <+> ppr (App fun arg)
--         , text "fun dmd_ty =" <+> ppr fun_ty
--         , text "arg dmd =" <+> ppr arg_dmd
--         , text "arg dmd_ty =" <+> ppr arg_ty
--         , text "res dmd_ty =" <+> ppr res_ty
--         , text "overall res dmd_ty =" <+> ppr (res_ty `bothDmdType` arg_ty) ])
    WithDmdType (res_ty `plusDmdType` arg_ty) (App fun' arg')

dmdAnal' env dmd (Lam var body)
  | isTyVar var
  = let
        WithDmdType body_ty body' = dmdAnal env dmd body
    in
    WithDmdType body_ty (Lam var body')

  | otherwise
  = let (n, body_dmd)    = peelCallDmd dmd
          -- body_dmd: a demand to analyze the body

        WithDmdType body_ty body' = dmdAnal env body_dmd body
        WithDmdType lam_ty var'   = annotateLamIdBndr env body_ty var
        new_dmd_type = multDmdType n lam_ty
    in
    WithDmdType new_dmd_type (Lam var' body')

dmdAnal' env dmd (Case scrut case_bndr ty [Alt alt bndrs rhs])
  -- Only one alternative.
  -- If it's a DataAlt, it should be the only constructor of the type.
  | is_single_data_alt alt
  = let
        WithDmdType rhs_ty rhs'           = dmdAnal env dmd rhs
        WithDmdType alt_ty1 fld_dmds      = findBndrsDmds env rhs_ty bndrs
        WithDmdType alt_ty2 case_bndr_dmd = findBndrDmd env alt_ty1 case_bndr
        !case_bndr'                       = setIdDemandInfo case_bndr case_bndr_dmd
        -- Evaluation cardinality on the case binder is irrelevant and a no-op.
        -- What matters is its nested sub-demand!
        -- NB: If case_bndr_dmd is absDmd, boxity will say Unboxed, which is
        -- what we want, because then `seq` will put a `seqDmd` on its scrut.
        (_ :* case_bndr_sd) = case_bndr_dmd
        -- Compute demand on the scrutinee
        -- FORCE the result, otherwise thunks will end up retaining the
        -- whole DmdEnv
        !(!bndrs', !scrut_sd)
          | DataAlt _ <- alt
          -- See Note [Demand on the scrutinee of a product case]
          -- See Note [Demand on case-alternative binders]
          , (!scrut_sd, fld_dmds') <- addCaseBndrDmd case_bndr_sd fld_dmds
          , let !bndrs' = setBndrsDemandInfo bndrs fld_dmds'
          = (bndrs', scrut_sd)
          | otherwise
          -- __DEFAULT and literal alts. Simply add demands and discard the
          -- evaluation cardinality, as we evaluate the scrutinee exactly once.
          = assert (null bndrs) (bndrs, case_bndr_sd)
        fam_envs                 = ae_fam_envs env
        alt_ty3
          -- See Note [Precise exceptions and strictness analysis] in "GHC.Types.Demand"
          | exprMayThrowPreciseException fam_envs scrut
          = deferAfterPreciseException alt_ty2
          | otherwise
          = alt_ty2

        WithDmdType scrut_ty scrut' = dmdAnal env scrut_sd scrut
        res_ty             = alt_ty3 `plusDmdType` toPlusDmdArg scrut_ty
    in
--    pprTrace "dmdAnal:Case1" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "dmd" <+> ppr dmd
--                                   , text "case_bndr_dmd" <+> ppr (idDemandInfo case_bndr')
--                                   , text "scrut_sd" <+> ppr scrut_sd
--                                   , text "scrut_ty" <+> ppr scrut_ty
--                                   , text "alt_ty" <+> ppr alt_ty2
--                                   , text "res_ty" <+> ppr res_ty ]) $
    WithDmdType res_ty (Case scrut' case_bndr' ty [Alt alt bndrs' rhs'])
    where
      is_single_data_alt (DataAlt dc) = isJust $ tyConSingleAlgDataCon_maybe $ dataConTyCon dc
      is_single_data_alt _            = True




dmdAnal' env dmd (Case scrut case_bndr ty alts)
  = let      -- Case expression with multiple alternatives
        WithDmdType alt_ty alts'     = combineAltDmds alts

        combineAltDmds [] = WithDmdType botDmdType []
        combineAltDmds (a:as) =
          let
            WithDmdType cur_ty a' = dmdAnalSumAlt env dmd case_bndr a
            WithDmdType rest_ty as' = combineAltDmds as
          in WithDmdType (lubDmdType cur_ty rest_ty) (a':as')

        WithDmdType alt_ty1 case_bndr_dmd = findBndrDmd env alt_ty case_bndr
        !case_bndr'                       = setIdDemandInfo case_bndr case_bndr_dmd
        WithDmdType scrut_ty scrut'       = dmdAnal env topSubDmd scrut
                               -- NB: Base case is botDmdType, for empty case alternatives
                               --     This is a unit for lubDmdType, and the right result
                               --     when there really are no alternatives
        fam_envs             = ae_fam_envs env
        alt_ty2
          -- See Note [Precise exceptions and strictness analysis] in "GHC.Types.Demand"
          | exprMayThrowPreciseException fam_envs scrut
          = deferAfterPreciseException alt_ty1
          | otherwise
          = alt_ty1
        res_ty               = alt_ty2 `plusDmdType` toPlusDmdArg scrut_ty

    in
--    pprTrace "dmdAnal:Case2" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "scrut_ty" <+> ppr scrut_ty
--                                   , text "alt_tys" <+> ppr alt_tys
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
  | (Var f, _) <- collectArgs e
  , Just op    <- isPrimOpId_maybe f
  , op /= RaiseIOOp
  = False -- 2. in the Note
  | (Var f, _) <- collectArgs e
  , Just fcall <- isFCallId_maybe f
  , not (isSafeForeignCall fcall)
  = False -- 3. in the Note
  | otherwise
  = True  -- _. in the Note

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

dmdAnalSumAlt :: AnalEnv -> SubDemand -> Id -> Alt Var -> WithDmdType (Alt Var)
dmdAnalSumAlt env dmd case_bndr (Alt con bndrs rhs)
  | WithDmdType rhs_ty rhs' <- dmdAnal env dmd rhs
  , WithDmdType alt_ty dmds <- findBndrsDmds env rhs_ty bndrs
  , let (_ :* case_bndr_sd) = findIdDemand alt_ty case_bndr
        -- See Note [Demand on case-alternative binders]
        -- we can't use the scrut_sd, because it says 'Prod' and we'll use
        -- topSubDmd anyway for scrutinees of sum types.
        (!_scrut_sd, dmds') = addCaseBndrDmd case_bndr_sd dmds
        -- Do not put a thunk into the Alt
        !new_ids            = setBndrsDemandInfo bndrs dmds'
  = WithDmdType alt_ty (Alt con new_ids rhs')

-- Precondition: The SubDemand is not a Call
-- See Note [Demand on the scrutinee of a product case]
-- and Note [Demand on case-alternative binders]
addCaseBndrDmd :: SubDemand -- On the case binder
               -> [Demand]  -- On the fields of the constructor
               -> (SubDemand, [Demand])
                            -- SubDemand on the case binder incl. field demands
                            -- and final demands for the components of the constructor
addCaseBndrDmd case_sd fld_dmds
  | Just (_, ds) <- viewProd (length fld_dmds) scrut_sd
  = (scrut_sd, ds)
  | otherwise
  = pprPanic "was a call demand" (ppr case_sd $$ ppr fld_dmds) -- See the Precondition
  where
    scrut_sd = case_sd `plusSubDmd` mkProd Unboxed fld_dmds

{-
Note [Analysing with absent demand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we analyse an expression with demand A.  The "A" means
"absent", so this expression will never be needed. What should happen?
There are several wrinkles:

* We *do* want to analyse the expression regardless.
  Reason: Note [Always analyse in virgin pass]

  But we can post-process the results to ignore all the usage
  demands coming back. This is done by multDmdType.

* Nevertheless, which sub-demand should we pick for analysis?
  Since the demand was absent, any would do. Worker/wrapper will replace
  absent bindings with an absent filler anyway, so annotations in the RHS
  of an absent binding don't matter much.
  Picking 'botSubDmd' would be the most useful, but would also look a bit
  misleading in the Core output of DmdAnal, because all nested annotations would
  be bottoming. Better pick 'seqSubDmd', so that we annotate many of those
  nested bindings with A themselves.

* In a previous incarnation of GHC we needed to be extra careful in the
  case of an *unlifted type*, because unlifted values are evaluated
  even if they are not used.  Example (see #9254):
     f :: (() -> (# Int#, () #)) -> ()
          -- Strictness signature is
          --    <CS(S(A,SU))>
          -- I.e. calls k, but discards first component of result
     f k = case k () of (# _, r #) -> r

     g :: Int -> ()
     g y = f (\n -> (# case y of I# y2 -> y2, n #))

  Here f's strictness signature says (correctly) that it calls its
  argument function and ignores the first component of its result.
  This is correct in the sense that it'd be fine to (say) modify the
  function so that always returned 0# in the first component.

  But in function g, we *will* evaluate the 'case y of ...', because
  it has type Int#.  So 'y' will be evaluated.  So we must record this
  usage of 'y', else 'g' will say 'y' is absent, and will w/w so that
  'y' is bound to an aBSENT_ERROR thunk.

  However, the argument of toSubDmd always satisfies the let/app
  invariant; so if it is unlifted it is also okForSpeculation, and so
  can be evaluated in a short finite time -- and that rules out nasty
  cases like the one above.  (I'm not quite sure why this was a
  problem in an earlier version of GHC, but it isn't now.)

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
  3. False  If f is an unsafe FFI call ('PlayRisky')
  _. True   Otherwise "give up".

It is sound to return False in those cases, because
  1. We don't give any guarantees for unsafePerformIO, so no precise exceptions
     from pure code.
  2. raiseIO# is the only primop that may throw a precise exception.
  3. Unsafe FFI calls may not interact with the RTS (to throw, for example).
     See haddock on GHC.Types.ForeignCall.PlayRisky.

We *need* to return False in those cases, because
  1. We would lose too much strictness in pure code, all over the place.
  2. We would lose strictness for primops like getMaskingState#, which
     introduces a substantial regression in
     GHC.IO.Handle.Internals.wantReadableHandle.
  3. We would lose strictness for code like GHC.Fingerprint.fingerprintData,
     where an intermittent FFI call to c_MD5Init would otherwise lose
     strictness on the arguments len and buf, leading to regressions in T9203
     (2%) and i386's haddock.base (5%). Tested by T13380f.

In !3014 we tried a more sophisticated analysis by introducing ConOrDiv (nic)
to the Divergence lattice, but in practice it turned out to be hard to untaint
from 'topDiv' to 'conDiv', leading to bugs, performance regressions and
complexity that didn't justify the single fixed testcase T13380c.

Note [Demand on the scrutinee of a product case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When figuring out the demand on the scrutinee of a product case,
we use the demands of the case alternative, i.e. id_dmds.
But note that these include the demand on the case binder;
see Note [Demand on case-alternative binders] in GHC.Types.Demand.
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
-- See Note [What are demand signatures?] in "GHC.Types.Demand"
dmdTransform env var sd
  -- Data constructors
  | isDataConWorkId var
  = dmdTransformDataConSig (idArity var) sd
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
        -- all anyway, hence the multDmd below: It means we don't
        -- have to track whether @var@ is used strictly or at most
        -- once, because ultimately it never will.
        -> addVarDmd fn_ty var (C_0N `multDmd` (C_11 :* sd)) -- discard strictness
        | otherwise
        -> fn_ty -- don't bother tracking; just annotate with 'topDmd' later
  -- Everything else:
  --   * Local let binders for which we use LetUp (cf. 'useLetUp')
  --   * Lambda binders
  --   * Case and constructor field binders
  | otherwise
  = -- pprTrace "dmdTransform:other" (vcat [ppr var, ppr boxity, ppr sd]) $
    unitDmdType (unitVarEnv var (C_11 :* sd))

{- *********************************************************************
*                                                                      *
                      Binding right-hand sides
*                                                                      *
********************************************************************* -}

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
  -> (AnalEnv, DmdEnv, Id, CoreExpr)
-- Process the RHS of the binding, add the strictness signature
-- to the Id, and augment the environment with the signature as well.
-- See Note [NOINLINE and strictness]
dmdAnalRhsSig top_lvl rec_flag env let_dmd id rhs
  = -- pprTrace "dmdAnalRhsSig" (ppr id $$ ppr let_dmd $$ ppr sig $$ ppr lazy_fv) $
    (final_env, lazy_fv, final_id, final_rhs)
  where
    rhs_arity = idArity id
    -- See Note [Demand signatures are computed for a threshold demand based on idArity]

    rhs_dmd = mkCalledOnceDmds rhs_arity body_dmd

    body_dmd
      | isJoinId id
      -- See Note [Demand analysis for join points]
      -- See Note [Invariants on join points] invariant 2b, in GHC.Core
      --     rhs_arity matches the join arity of the join point
      = let_dmd
      | otherwise
      -- See Note [Unboxed demand on function bodies returning small products]
      = unboxedWhenSmall (ae_opts env) (unboxableResultWidth env id) topSubDmd

    -- See Note [Do not unbox class dictionaries]
    WithDmdType rhs_dmd_ty rhs' = dmdAnal env rhs_dmd rhs
    DmdType rhs_fv rhs_dmds rhs_div    = rhs_dmd_ty
    (final_rhs_dmds, final_rhs) = finaliseArgBoxities env id rhs_arity rhs'
                                  `orElse` (rhs_dmds, rhs')

    sig = mkDmdSigForArity rhs_arity (DmdType sig_fv final_rhs_dmds rhs_div)

    final_id   = id `setIdDmdSig` sig
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
    rhs_fv1 = case rec_flag of
                Recursive    -> reuseEnv rhs_fv
                NonRecursive -> rhs_fv

    -- See Note [Absence analysis for stable unfoldings and RULES]
    rhs_fv2 = rhs_fv1 `keepAliveDmdEnv` bndrRuleAndUnfoldingIds id

    -- See Note [Lazy and unleashable free variables]
    !(!lazy_fv, !sig_fv) = partitionVarEnv isWeakDmd rhs_fv2

unboxableResultWidth :: AnalEnv -> Id -> Maybe Arity
unboxableResultWidth env id
  | (pis,ret_ty) <- splitPiTys (idType id)
  , count (not . isNamedBinder) pis == idArity id
  , Just (tc, _tc_args, _co) <- normSplitTyConApp_maybe (ae_fam_envs env) ret_ty
  , Just dc <- tyConSingleAlgDataCon_maybe tc
  , null (dataConExTyCoVars dc) -- Can't unbox results with existentials
  = Just (dataConRepArity dc)
  | otherwise
  = Nothing

unboxedWhenSmall :: DmdAnalOpts -> Maybe Arity -> SubDemand -> SubDemand
-- See Note [Unboxed demand on function bodies returning small products]
unboxedWhenSmall opts mb_n sd
  | Just n <- mb_n
  , n <= dmd_unbox_width opts
  = unboxSubDemand sd
  | otherwise
  = sd

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

If j was a vanilla function definition, we'd analyse its body with
evalDmd, and think that it was lazy in p.  But for join points we can
do better!  We know that j's body will (if called at all) be evaluated
with the demand that consumes the entire join-binding, in this case
the argument demand from g.  Whizzo!  g evaluates both components of
its argument pair, so p will certainly be evaluated if j is called.

For f to be strict in p, we need /all/ paths to evaluate p; in this
case the C branch does so too, so we are fine.  So, as usual, we need
to transport demands on free variables to the call site(s).  Compare
Note [Lazy and unleashable free variables].

The implementation is easy.  When analysing a join point, we can
analyse its body with the demand from the entire join-binding (written
let_dmd here).

Another win for join points!  #13543.

However, note that the strictness signature for a join point can
look a little puzzling.  E.g.

    (join j x = \y. error "urk")
    (in case v of              )
    (     A -> j 3             )  x
    (     B -> j 4             )
    (     C -> \y. blah        )

The entire thing is in a C1(L) context, so j's strictness signature
will be    [A]b
meaning one absent argument, returns bottom.  That seems odd because
there's a \y inside.  But it's right because when consumed in a C1(L)
context the RHS of the join point is indeed bottom.

Note [Demand signatures are computed for a threshold demand based on idArity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We compute demand signatures assuming idArity incoming arguments to approximate
behavior for when we have a call site with at least that many arguments. idArity
is /at least/ the number of manifest lambdas, but might be higher for PAPs and
trivial RHS (see Note [Demand analysis for trivial right-hand sides]).

Because idArity of a function varies independently of its cardinality
properties (cf. Note [idArity varies independently of dmdTypeDepth]), we
implicitly encode the arity for when a demand signature is sound to unleash
in its 'dmdTypeDepth' (cf. Note [Understanding DmdType and DmdSig] in
GHC.Types.Demand). It is unsound to unleash a demand signature when the
incoming number of arguments is less than that.
See Note [What are demand signatures?] in GHC.Types.Demand for more details
on soundness.

Why idArity arguments? Because that's a conservative estimate of how many
arguments we must feed a function before it does anything interesting with them.
Also it elegantly subsumes the trivial RHS and PAP case.

There might be functions for which we might want to analyse for more incoming
arguments than idArity. Example:

  f x =
    if expensive
      then \y -> ... y ...
      else \y -> ... y ...

We'd analyse `f` under a unary call demand C1(L), corresponding to idArity
being 1. That's enough to look under the manifest lambda and find out how a
unary call would use `x`, but not enough to look into the lambdas in the if
branches.

On the other hand, if we analysed for call demand C1(C1(L)), we'd get useful
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

Note [idArity varies independently of dmdTypeDepth]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to check in GHC.Core.Lint that dmdTypeDepth <= idArity for a let-bound
identifier. But that means we would have to zap demand signatures every time we
reset or decrease arity. That's an unnecessary dependency, because

  * The demand signature captures a semantic property that is independent of
    what the binding's current arity is
  * idArity is analysis information itself, thus volatile
  * We already *have* dmdTypeDepth, wo why not just use it to encode the
    threshold for when to unleash the signature
    (cf. Note [Understanding DmdType and DmdSig] in GHC.Types.Demand)

Consider the following expression, for example:

    (let go x y = `x` seq ... in go) |> co

`go` might have a strictness signature of `<1L><L>`. The simplifier will identify
`go` as a nullary join point through `joinPointBinding_maybe` and float the
coercion into the binding, leading to an arity decrease:

    join go = (\x y -> `x` seq ...) |> co in go

With the CoreLint check, we would have to zap `go`'s perfectly viable strictness
signature.

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
Ticket #18638 shows that it's really important to do absence analysis
for stable unfoldings. Consider

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

SOLUTION: if f has a stable unfolding, adjust its DmdEnv (the demands
on its free variables) so that no variable mentioned in its unfolding
is Absent.  This is done by the function Demand.keepAliveDmdEnv.

ALSO: do the same for Ids free in the RHS of any RULES for f.

PS: You may wonder how it can be that f's optimised RHS has somehow
discarded 'g', but when f is inlined we /don't/ discard g in the same
way. I think a simple example is
   g = (a,b)
   f = \x.  fst g
   {-# INLINE f #-}

Now f's optimised RHS will be \x.a, but if we change g to (error "..")
(since it is apparently Absent) and then inline (\x. fst g) we get
disaster.  But regardless, #18638 was a more complicated version of
this, that actually happened in practice.
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
unboxing decisions during demand analysis and reflect these decicions
in demand annotations. That is the job of 'finaliseArgBoxities',
which is defined here and called from demand analysis.

Here is a list of different Notes it has to take care of:

  * Note [No lazy, Unboxed demands in demand signature] such as `L!P(L)` in
    general, but still allow Note [Unboxing evaluated arguments]
  * Note [No nested Unboxed inside Boxed in demand signature] such as `1P(1!L)`
  * Implement fixes for corner cases Note [Do not unbox class dictionaries]
    and Note [mkWWstr and unsafeCoerce]

Then, in worker/wrapper blindly trusts the boxity info in the demand signature
and will not look at strictness info *at all*, in 'wantToUnboxArg'.

Note [Finalising boxity for let-bound Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  let x = e in body
where the demand on 'x' is 1!P(blah).  We want to unbox x according to
Note [Thunk splitting] in GHC.Core.Opt.WorkWrap.  We must do this becuase
worker/wrapper ignores stricness and looks only at boxity flags; so if
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

* Worker/wrapper will consult 'wantToUnboxArg' for its unboxing decision. It will
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

Note [Do not unbox class dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

But in any other situation, a dictionary is just an ordinary value,
and can be unpacked.  So we track the INLINABLE pragma, and discard the boxity
flag in finaliseArgBoxities (see the isClassPred test).

Historical note: #14955 describes how I got this fix wrong the first time.

Note that the simplicity of this fix implies that INLINE functions (such as
wrapper functions after the WW run) will never say that they unbox class
dictionaries. That's not ideal, but not worth losing sleep over, as INLINE
functions will have been inlined by the time we run demand analysis so we'll
see the unboxing around the worker in client modules. I got aware of the issue
in T5075 by the change in boxity of loop between demand analysis runs.

Note [Worker argument budget]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In 'finaliseArgBoxities' we don't want to generate workers with zillions of
argument when, say given a strict record with zillions of fields.  So we
limit the maximum number of worker args to the maximum of
  - -fmax-worker-args=N
  - The number of args in the original function; if it already has has
    zillions of arguments we don't want to seek /fewer/ args in the worker.
(Maybe we should /add/ them instead of maxing?)

We pursue a "layered" strategy for unboxing: we unbox the top level of the
argument(s), subject to budget; if there are any arguments left we unbox the
next layer, using that depleted budget.

To achieve this, we use the classic almost-circular programming technique in
which we we write one pass that takes a lazy list of the Budgets for every
layer.
-}

data Budgets = MkB Arity Budgets   -- An infinite list of arity budgets

incTopBudget :: Budgets -> Budgets
incTopBudget (MkB n bg) = MkB (n+1) bg

positiveTopBudget :: Budgets -> Bool
positiveTopBudget (MkB n _) = n >= 0

finaliseArgBoxities :: AnalEnv -> Id -> Arity -> CoreExpr
                    -> Maybe ([Demand], CoreExpr)
finaliseArgBoxities env fn arity rhs
  | arity > count isId bndrs  -- Can't find enough binders
  = Nothing  -- This happens if we have   f = g
             -- Then there are no binders; we don't worker/wrapper; and we
             -- simply want to give f the same demand signature as g

  | otherwise
  = Just (arg_dmds', add_demands arg_dmds' rhs)
    -- add_demands: we must attach the final boxities to the lambda-binders
    -- of the function, both because that's kosher, and because CPR analysis
    -- uses the info on the binders directly.
  where
    opts            = ae_opts env
    fam_envs        = ae_fam_envs env
    is_inlinable_fn = isStableUnfolding (realIdUnfolding fn)
    (bndrs, _body)  = collectBinders rhs
    max_wkr_args    = dmd_max_worker_args opts `max` arity
                      -- See Note [Worker argument budget]

    -- This is the key line, which uses almost-circular programming
    -- The remaining budget from one layer becomes the initial
    -- budget for the next layer down.  See Note [Worker argument budget]
    (remaining_budget, arg_dmds') = go_args (MkB max_wkr_args remaining_budget) arg_triples

    arg_triples :: [(Type, StrictnessMark, Demand)]
    arg_triples = take arity $
                  map mk_triple $
                  filter isRuntimeVar bndrs

    mk_triple :: Id -> (Type,StrictnessMark,Demand)
    mk_triple bndr | is_cls_arg ty = (ty, NotMarkedStrict, trimBoxity dmd)
                   | otherwise     = (ty, NotMarkedStrict, dmd)
                   where
                     ty  = idType bndr
                     dmd = idDemandInfo bndr

    -- is_cls_arg: see Note [Do not unbox class dictionaries]
    is_cls_arg arg_ty = is_inlinable_fn && isClassPred arg_ty

    go_args :: Budgets -> [(Type,StrictnessMark,Demand)] -> (Budgets, [Demand])
    go_args bg triples = mapAccumL go_arg bg triples

    go_arg :: Budgets -> (Type,StrictnessMark,Demand) -> (Budgets, Demand)
    go_arg bg@(MkB bg_top bg_inner) (ty, str_mark, dmd@(n :* _))
      = case wantToUnboxArg fam_envs ty dmd of
          DropAbsent   -> (bg,                      dmd)
          StopUnboxing -> (MkB (bg_top-1) bg_inner, trimBoxity dmd)

          Unbox DataConPatContext{dcpc_dc=dc, dcpc_tc_args=tc_args} dmds
            -> (MkB (bg_top-1) final_bg_inner, final_dmd)
            where
              dc_arity = dataConRepArity dc
              arg_tys  = dubiousDataConInstArgTys dc tc_args
              (bg_inner', dmds') = go_args (incTopBudget bg_inner) $
                                   zip3 arg_tys (dataConRepStrictness dc) dmds
              dmd' = n :* (mkProd Unboxed $! dmds')
              (final_bg_inner, final_dmd)
                  | dmds `lengthIs` dc_arity
                  , isStrict n || isMarkedStrict str_mark
                     -- isStrict: see Note [No lazy, Unboxed demands in demand signature]
                     -- isMarkedStrict: see Note [Unboxing evaluated arguments]
                  , positiveTopBudget bg_inner'
                  = (bg_inner', dmd')
                  | otherwise
                  = (bg_inner, trimBoxity dmd)

    add_demands :: [Demand] -> CoreExpr -> CoreExpr
    -- Attach the demands to the outer lambdas of this expression
    add_demands [] e = e
    add_demands (dmd:dmds) (Lam v e)
      | isTyVar v = Lam v (add_demands (dmd:dmds) e)
      | otherwise = Lam (v `setIdDemandInfo` dmd) (add_demands dmds e)
    add_demands dmds e = pprPanic "add_demands" (ppr dmds $$ ppr e)

finaliseLetBoxity
  :: FamInstEnvs
  -> Type                   -- ^ Type of the let-bound Id
  -> Demand                 -- ^ How the Id is used
  -> Demand
-- See Note [Finalising boxity for let-bound Ids]
-- This function is like finaliseArgBoxities, but much simpler because
-- it has no "budget".  It simply unboxes strict demands, and stops
-- when it reaches a lazy one.
finaliseLetBoxity env ty dmd
  = go ty NotMarkedStrict dmd
  where
    go ty mark dmd@(n :* _) =
      case wantToUnboxArg env ty dmd of
        DropAbsent   -> dmd
        StopUnboxing -> trimBoxity dmd
        Unbox DataConPatContext{dcpc_dc=dc, dcpc_tc_args=tc_args} dmds
          | isStrict n || isMarkedStrict mark
          , dmds `lengthIs` dataConRepArity dc
          , let arg_tys = dubiousDataConInstArgTys dc tc_args
                dmds'   = strictZipWith3 go arg_tys (dataConRepStrictness dc) dmds
          -> n :* (mkProd Unboxed $! dmds')
          | otherwise
          -> trimBoxity dmd


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
       -> (AnalEnv, DmdEnv, [(Id,CoreExpr)]) -- Binders annotated with strictness info

dmdFix top_lvl env let_dmd orig_pairs
  = loop 1 initial_pairs
  where
    -- See Note [Initialising strictness]
    initial_pairs | ae_virgin env = [(setIdDmdSig id botSig, rhs) | (id, rhs) <- orig_pairs ]
                  | otherwise     = orig_pairs

    -- If fixed-point iteration does not yield a result we use this instead
    -- See Note [Safe abortion in the fixed-point iteration]
    abort :: (AnalEnv, DmdEnv, [(Id,CoreExpr)])
    abort = (env, lazy_fv', zapped_pairs)
      where (lazy_fv, pairs') = step True (zapIdDmdSig orig_pairs)
            -- Note [Lazy and unleashable free variables]
            non_lazy_fvs = plusVarEnvList $ map (dmdSigDmdEnv . idDmdSig . fst) pairs'
            lazy_fv'     = lazy_fv `plusVarEnv` mapVarEnv (const topDmd) non_lazy_fvs
            zapped_pairs = zapIdDmdSig pairs'

    -- The fixed-point varies the idDmdSig field of the binders, and terminates if that
    -- annotation does not change any more.
    loop :: Int -> [(Id,CoreExpr)] -> (AnalEnv, DmdEnv, [(Id,CoreExpr)])
    loop n pairs = -- pprTrace "dmdFix" (ppr n <+> vcat [ ppr id <+> ppr (idDmdSig id)
                   --                                     | (id,_)<- pairs]) $
                   loop' n pairs

    loop' n pairs
      | found_fixpoint = (final_anal_env, lazy_fv, pairs')
      | n == 10        = abort
      | otherwise      = loop (n+1) pairs'
      where
        found_fixpoint    = map (idDmdSig . fst) pairs' == map (idDmdSig . fst) pairs
        first_round       = n == 1
        (lazy_fv, pairs') = step first_round pairs
        final_anal_env    = extendAnalEnvs top_lvl env (map fst pairs')

    step :: Bool -> [(Id, CoreExpr)] -> (DmdEnv, [(Id, CoreExpr)])
    step first_round pairs = (lazy_fv, pairs')
      where
        -- In all but the first iteration, delete the virgin flag
        start_env | first_round = env
                  | otherwise   = nonVirgin env

        start = (extendAnalEnvs top_lvl start_env (map fst pairs), emptyVarEnv)

        !((_,!lazy_fv), !pairs') = mapAccumL my_downRhs start pairs
                -- mapAccumL: Use the new signature to do the next pair
                -- The occurrence analyser has arranged them in a good order
                -- so this can significantly reduce the number of iterations needed

        my_downRhs (env, lazy_fv) (id,rhs)
          = -- pprTrace "my_downRhs" (ppr id $$ ppr (idDmdSig id) $$ ppr sig) $
            ((env', lazy_fv'), (id', rhs'))
          where
            !(!env', !lazy_fv1, !id', !rhs') = dmdAnalRhsSig top_lvl Recursive env let_dmd id rhs
            !lazy_fv'                    = plusVarEnv_C plusDmd lazy_fv lazy_fv1

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

More on both below.  But the botttom line is: we really don't want to
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
Notice that this happens because T is a product type, and is recrusive.
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

unitDmdType :: DmdEnv -> DmdType
unitDmdType dmd_env = DmdType dmd_env [] topDiv

coercionDmdEnv :: Coercion -> DmdEnv
coercionDmdEnv co = coercionsDmdEnv [co]

coercionsDmdEnv :: [Coercion] -> DmdEnv
coercionsDmdEnv cos = mapVarEnv (const topDmd) (getUniqSet $ coVarsOfCos cos)
                      -- The VarSet from coVarsOfCos is really a VarEnv Var

addVarDmd :: DmdType -> Var -> Demand -> DmdType
addVarDmd (DmdType fv ds res) var dmd
  = DmdType (extendVarEnv_C plusDmd fv var dmd) ds res

addLazyFVs :: DmdType -> DmdEnv -> DmdType
addLazyFVs dmd_ty lazy_fvs
  = dmd_ty `plusDmdType` mkPlusDmdArg lazy_fvs
        -- Using plusDmdType (rather than just plus'ing the envs)
        -- is vital.  Consider
        --      let f = \x -> (x,y)
        --      in  error (f 3)
        -- Here, y is treated as a lazy-fv of f, but we must `plusDmd` that L
        -- demand with the bottom coming up from 'error'
        --
        -- I got a loop in the fixpointer without this, due to an interaction
        -- with the lazy_fv filtering in dmdAnalRhsSig.  Roughly, it was
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

{-
Note [Do not strictify the argument dictionaries of a dfun]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The typechecker can tie recursive knots involving dfuns, so we do the
conservative thing and refrain from strictifying a dfun's argument
dictionaries.
-}

setBndrsDemandInfo :: HasCallStack => [Var] -> [Demand] -> [Var]
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
variable separate (usually called "lazy_fv") and adds it to the demand of the
whole binding later.

What if we decide _not_ to store a strictness signature for a binding at all, as
we do when aborting a fixed-point iteration? The we risk losing the information
that the strict variables are being used. In that case, we take all free variables
mentioned in the (unsound) strictness signature, conservatively approximate the
demand put on them (topDmd), and add that to the "lazy_fv" returned by "dmdFix".


************************************************************************
*                                                                      *
\subsection{Strictness signatures}
*                                                                      *
************************************************************************
-}


data AnalEnv = AE
   { ae_opts      :: !DmdAnalOpts -- ^ Analysis options
   , ae_sigs      :: !SigEnv
   , ae_virgin    :: !Bool -- ^ True on first iteration only
                           -- See Note [Initialising strictness]
   , ae_fam_envs  :: !FamInstEnvs
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
         }

emptySigEnv :: SigEnv
emptySigEnv = emptyVarEnv

-- | Extend an environment with the strictness IDs attached to the id
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
      -- See Note [Making dictionaries strict]
      | dmd_strict_dicts (ae_opts env)
             -- We never want to strictify a recursive let. At the moment
             -- findBndrDmd is never called for recursive lets; if that
             -- changes, we need a RecFlag parameter and another guard here.
      = strictifyDictDmd id_ty dmd
      | otherwise
      = dmd

    fam_envs = ae_fam_envs env

{- Note [Making dictionaries strict]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Opt_DictsStrict flag makes GHC use call-by-value for dictionaries.  Why?

* Generally CBV is more efficient.

* Dictionaries are always non-bottom; and never take much work to
  compute.  E.g. a dfun from an instance decl always returns a dicionary
  record immediately.  See DFunUnfolding in CoreSyn.
  See also Note [Recursive superclasses] in TcInstDcls.

* The strictness analyser will then unbox dictionaries and pass the
  methods individually, rather than in a bundle.  If there are a lot of
  methods that might be bad; but worker/wrapper already does throttling.

* A newtype dictionary is *not* always non-bottom.  E.g.
      class C a where op :: a -> a
      instance C Int where op = error "urk"
  Now a value of type (C Int) is just a newtype wrapper (a cast) around
  the error thunk.  Don't strictify these!

See #17758 for more background and perf numbers.

The implementation is extremly simple: just make the strictness
analyser strictify the demand on a dictionary binder in
'findBndrDmd'.

However there is one case where this can make performance worse.
For the principle consider some function at the core level:
    myEq :: Eq a => a -> a -> Bool
    myEq eqDict x y = ((==) eqDict) x y
If we make the dictionary strict then WW can fire turning this into:
    $wmyEq :: (a -> a -> Bool) -> a -> a -> Bool
    $wmyEq eq x y = eq x y
Which *usually* performs better. However if the dictionary is known we
are far more likely to inline a function applied to the dictionary than
to inline one applied to a function. Sometimes this makes just enough
of a difference to stop a function from inlining. This is documented in
#18421.

It's somewhat similar to Note [Do not unbox class dictionaries] although
here our problem is with the inliner, not the specializer.

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

Note that, in contrast, the single-call information (CM(..)) /can/ be
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
