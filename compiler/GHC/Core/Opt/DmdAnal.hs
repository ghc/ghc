{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


                        -----------------
                        A demand analysis
                        -----------------
-}

{-# LANGUAGE CPP #-}

module GHC.Core.Opt.DmdAnal
   ( DmdAnalOpts(..)
   , dmdAnalProgram
   )
where

#include "HsVersions.h"

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
import GHC.Core.FVs      ( rulesRhsFreeIds, bndrRuleAndUnfoldingIds )
import GHC.Core.Coercion ( Coercion, coVarsOfCo )
import GHC.Core.FamInstEnv
import GHC.Core.Opt.Arity ( typeArity )
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Data.Maybe         ( isJust )
import GHC.Builtin.PrimOps
import GHC.Builtin.Types.Prim ( realWorldStatePrimTy )
import GHC.Types.Unique.Set

-- import GHC.Driver.Ppr

{-
************************************************************************
*                                                                      *
\subsection{Top level stuff}
*                                                                      *
************************************************************************
-}

-- | Options for the demand analysis
newtype DmdAnalOpts = DmdAnalOpts
   { dmd_strict_dicts :: Bool -- ^ Use strict dictionaries
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
-- (reflecting their 'idStrictnessInfo') and expects to receive a
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
    WithDmdType body_ty' id_dmd = findBndrDmd env notArgOfDfun body_ty id
    !id'                = setBindIdDemandInfo top_lvl id id_dmd
    (rhs_ty, rhs')     = dmdAnalStar env (dmdTransformThunkDmd rhs id_dmd) rhs

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
dmdAnalStar env (n :* cd) e
  | WithDmdType dmd_ty e'    <- dmdAnal env cd e
  = ASSERT2( not (isUnliftedType (exprType e)) || exprOkForSpeculation e, ppr e )
    -- The argument 'e' should satisfy the let/app invariant
    -- See Note [Analysing with absent demand] in GHC.Types.Demand
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
        WithDmdType lam_ty var'   = annotateLamIdBndr env notArgOfDfun body_ty var
        new_dmd_type = multDmdType n lam_ty
    in
    WithDmdType new_dmd_type (Lam var' body')

dmdAnal' env dmd (Case scrut case_bndr ty [Alt alt bndrs rhs])
  -- Only one alternative.
  -- If it's a DataAlt, it should be the only constructor of the type.
  | is_single_data_alt alt
  = let
        WithDmdType rhs_ty rhs'           = dmdAnal env dmd rhs
        WithDmdType alt_ty1 dmds          = findBndrsDmds env rhs_ty bndrs
        WithDmdType alt_ty2 case_bndr_dmd = findBndrDmd env False alt_ty1 case_bndr
        -- Evaluation cardinality on the case binder is irrelevant and a no-op.
        -- What matters is its nested sub-demand!
        (_ :* case_bndr_sd)      = case_bndr_dmd
        -- Compute demand on the scrutinee
        -- FORCE the result, otherwise thunks will end up retaining the
        -- whole DmdEnv
        !(!bndrs', !scrut_sd)
          | DataAlt _ <- alt
          , id_dmds <- addCaseBndrDmd case_bndr_sd dmds
          -- See Note [Demand on scrutinee of a product case]
          = let !new_info = setBndrsDemandInfo bndrs id_dmds
                !new_prod = mkProd id_dmds
            in (new_info, new_prod)
          | otherwise
          -- __DEFAULT and literal alts. Simply add demands and discard the
          -- evaluation cardinality, as we evaluate the scrutinee exactly once.
          = ASSERT( null bndrs ) (bndrs, case_bndr_sd)
        fam_envs                 = ae_fam_envs env
        alt_ty3
          -- See Note [Precise exceptions and strictness analysis] in "GHC.Types.Demand"
          | exprMayThrowPreciseException fam_envs scrut
          = deferAfterPreciseException alt_ty2
          | otherwise
          = alt_ty2

        WithDmdType scrut_ty scrut' = dmdAnal env scrut_sd scrut
        res_ty             = alt_ty3 `plusDmdType` toPlusDmdArg scrut_ty
        !case_bndr'        = setIdDemandInfo case_bndr case_bndr_dmd
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

        WithDmdType scrut_ty scrut'   = dmdAnal env topSubDmd scrut
        WithDmdType alt_ty1 case_bndr' = annotateBndr env alt_ty case_bndr
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
  | Just DataConPatContext{ dcpc_dc = dc, dcpc_tc_args = tc_args }
      <- splitArgType_maybe fam_envs ty
  , isUnboxedTupleDataCon dc
  , let field_tys = dataConInstArgTys dc tc_args
  = any (eqType realWorldStatePrimTy . scaledThing) field_tys
  | otherwise
  = False

dmdAnalSumAlt :: AnalEnv -> SubDemand -> Id -> Alt Var -> WithDmdType (Alt Var)
dmdAnalSumAlt env dmd case_bndr (Alt con bndrs rhs)
  | WithDmdType rhs_ty rhs' <- dmdAnal env dmd rhs
  , WithDmdType alt_ty dmds <- findBndrsDmds env rhs_ty bndrs
  , let (_ :* case_bndr_sd) = findIdDemand alt_ty case_bndr
        -- See Note [Demand on scrutinee of a product case]
        id_dmds             = addCaseBndrDmd case_bndr_sd dmds
        -- Do not put a thunk into the Alt
        !new_ids  = setBndrsDemandInfo bndrs id_dmds
  = WithDmdType alt_ty (Alt con new_ids rhs')

{-
Note [Analysing with absent demand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we analyse an expression with demand A.  The "A" means
"absent", so this expression will never be needed.  What should happen?
There are several wrinkles:

* We *do* want to analyse the expression regardless.
  Reason: Note [Always analyse in virgin pass]

  But we can post-process the results to ignore all the usage
  demands coming back. This is done by multDmdType.

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

dmdTransform :: AnalEnv         -- ^ The strictness environment
             -> Id              -- ^ The function
             -> SubDemand       -- ^ The demand on the function
             -> DmdType         -- ^ The demand type of the function in this context
                                -- Returned DmdEnv includes the demand on
                                -- this function plus demand on its free variables

-- See Note [What are demand signatures?] in "GHC.Types.Demand"
dmdTransform env var dmd
  -- Data constructors
  | isDataConWorkId var
  = dmdTransformDataConSig (idArity var) dmd
  -- Dictionary component selectors
  -- Used to be controlled by a flag.
  -- See #18429 for some perf measurements.
  | Just _ <- isClassOpId_maybe var
  = -- pprTrace "dmdTransform:DictSel" (ppr var $$ ppr dmd) $
    dmdTransformDictSelSig (idStrictness var) dmd
  -- Imported functions
  | isGlobalId var
  , let res = dmdTransformSig (idStrictness var) dmd
  = -- pprTrace "dmdTransform:import" (vcat [ppr var, ppr (idStrictness var), ppr dmd, ppr res])
    res
  -- Top-level or local let-bound thing for which we use LetDown ('useLetUp').
  -- In that case, we have a strictness signature to unleash in our AnalEnv.
  | Just (sig, top_lvl) <- lookupSigEnv env var
  , let fn_ty = dmdTransformSig sig dmd
  = -- pprTrace "dmdTransform:LetDown" (vcat [ppr var, ppr sig, ppr dmd, ppr fn_ty]) $
    case top_lvl of
      NotTopLevel -> addVarDmd fn_ty var (C_11 :* dmd)
      TopLevel
        | isInterestingTopLevelFn var
        -- Top-level things will be used multiple times or not at
        -- all anyway, hence the multDmd below: It means we don't
        -- have to track whether @var@ is used strictly or at most
        -- once, because ultimately it never will.
        -> addVarDmd fn_ty var (C_0N `multDmd` (C_11 :* dmd)) -- discard strictness
        | otherwise
        -> fn_ty -- don't bother tracking; just annotate with 'topDmd' later
  -- Everything else:
  --   * Local let binders for which we use LetUp (cf. 'useLetUp')
  --   * Lambda binders
  --   * Case and constructor field binders
  | otherwise
  = -- pprTrace "dmdTransform:other" (vcat [ppr var, ppr sig, ppr dmd, ppr res]) $
    unitDmdType (unitVarEnv var (C_11 :* dmd))

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
    (env', lazy_fv, id', rhs')
  where
    rhs_arity = idArity id
    -- See Note [Demand signatures are computed for a threshold demand based on idArity]
    rhs_dmd -- See Note [Demand analysis for join points]
            -- See Note [Invariants on join points] invariant 2b, in GHC.Core
            --     rhs_arity matches the join arity of the join point
            | isJoinId id
            = mkCalledOnceDmds rhs_arity let_dmd
            | otherwise
            = mkCalledOnceDmds rhs_arity topSubDmd

    WithDmdType rhs_dmd_ty rhs' = dmdAnal env rhs_dmd rhs
    DmdType rhs_fv rhs_dmds rhs_div = rhs_dmd_ty

    sig = mkStrictSigForArity rhs_arity (DmdType sig_fv rhs_dmds rhs_div)

    id' = id `setIdStrictness` sig
    !env' = extendAnalEnv top_lvl env id' sig

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
    rhs_fv1 = case rec_flag of
                Recursive    -> reuseEnv rhs_fv
                NonRecursive -> rhs_fv

    -- See Note [Absence analysis for stable unfoldings and RULES]
    rhs_fv2 = rhs_fv1 `keepAliveDmdEnv` bndrRuleAndUnfoldingIds id

    -- See Note [Lazy and unleashable free variables]
    !(!lazy_fv, !sig_fv) = partitionVarEnv isWeakDmd rhs_fv2

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
in its 'dmdTypeDepth' (cf. Note [Understanding DmdType and StrictSig] in
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
    (cf. Note [Understanding DmdType and StrictSig] in GHC.Types.Demand)

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

Historical Note [Product demands for function body]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In 2013 I spotted this example, in shootout/binary_trees:

    Main.check' = \ b z ds. case z of z' { I# ip ->
                                case ds_d13s of
                                  Main.Nil -> z'
                                  Main.Node s14k s14l s14m ->
                                    Main.check' (not b)
                                      (Main.check' b
                                         (case b {
                                            False -> I# (-# s14h s14k);
                                            True  -> I# (+# s14h s14k)
                                          })
                                         s14l)
                                     s14m   }   }   }

Here we *really* want to unbox z, even though it appears to be used boxed in
the Nil case.  Partly the Nil case is not a hot path.  But more specifically,
the whole function gets the CPR property if we do.

That motivated using a demand of C1(C1(C1(P(L,L)))) for the RHS, where
(solely because the result was a product) we used a product demand
(albeit with lazy components) for the body. But that gives very silly
behaviour -- see #17932.   Happily it turns out now to be entirely
unnecessary: we get good results with C1(C1(C1(L))).   So I simply
deleted the special case.
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
       -> (AnalEnv, DmdEnv, [(Id,CoreExpr)]) -- Binders annotated with strictness info

dmdFix top_lvl env let_dmd orig_pairs
  = loop 1 initial_pairs
  where
    -- See Note [Initialising strictness]
    initial_pairs | ae_virgin env = [(setIdStrictness id botSig, rhs) | (id, rhs) <- orig_pairs ]
                  | otherwise     = orig_pairs

    -- If fixed-point iteration does not yield a result we use this instead
    -- See Note [Safe abortion in the fixed-point iteration]
    abort :: (AnalEnv, DmdEnv, [(Id,CoreExpr)])
    abort = (env, lazy_fv', zapped_pairs)
      where (lazy_fv, pairs') = step True (zapIdStrictness orig_pairs)
            -- Note [Lazy and unleashable free variables]
            non_lazy_fvs = plusVarEnvList $ map (strictSigDmdEnv . idStrictness . fst) pairs'
            lazy_fv'     = lazy_fv `plusVarEnv` mapVarEnv (const topDmd) non_lazy_fvs
            zapped_pairs = zapIdStrictness pairs'

    -- The fixed-point varies the idStrictness field of the binders, and terminates if that
    -- annotation does not change any more.
    loop :: Int -> [(Id,CoreExpr)] -> (AnalEnv, DmdEnv, [(Id,CoreExpr)])
    loop n pairs = -- pprTrace "dmdFix" (ppr n <+> vcat [ ppr id <+> ppr (idStrictness id)
                   --                                     | (id,_)<- pairs]) $
                   loop' n pairs

    loop' n pairs
      | found_fixpoint = (final_anal_env, lazy_fv, pairs')
      | n == 10        = abort
      | otherwise      = loop (n+1) pairs'
      where
        found_fixpoint    = map (idStrictness . fst) pairs' == map (idStrictness . fst) pairs
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
          = -- pprTrace "my_downRhs" (ppr id $$ ppr (idStrictness id) $$ ppr sig) $
            ((env', lazy_fv'), (id', rhs'))
          where
            !(!env', !lazy_fv1, !id', !rhs') = dmdAnalRhsSig top_lvl Recursive env let_dmd id rhs
            !lazy_fv'                    = plusVarEnv_C plusDmd lazy_fv lazy_fv1

    zapIdStrictness :: [(Id, CoreExpr)] -> [(Id, CoreExpr)]
    zapIdStrictness pairs = [(setIdStrictness id nopSig, rhs) | (id, rhs) <- pairs ]

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
coercionDmdEnv co = mapVarEnv (const topDmd) (getUniqSet $ coVarsOfCo co)
                    -- The VarSet from coVarsOfCo is really a VarEnv Var

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

setBndrsDemandInfo :: [Var] -> [Demand] -> [Var]
setBndrsDemandInfo (b:bs) ds
  | isTyVar b = b : setBndrsDemandInfo bs ds
setBndrsDemandInfo (b:bs) (d:ds) =
    let !new_info = setIdDemandInfo b d
        !vars = setBndrsDemandInfo bs ds
    in new_info : vars
setBndrsDemandInfo [] ds = ASSERT( null ds ) []
setBndrsDemandInfo bs _  = pprPanic "setBndrsDemandInfo" (ppr bs)

annotateBndr :: AnalEnv -> DmdType -> Var -> WithDmdType Var
-- The returned env has the var deleted
-- The returned var is annotated with demand info
-- according to the result demand of the provided demand type
-- No effect on the argument demands
annotateBndr env dmd_ty var
  | isId var  = WithDmdType dmd_ty' new_id
  | otherwise = WithDmdType dmd_ty  var
  where
    new_id = setIdDemandInfo var dmd
    WithDmdType dmd_ty' dmd = findBndrDmd env False dmd_ty var

annotateLamIdBndr :: AnalEnv
                  -> DFunFlag   -- is this lambda at the top of the RHS of a dfun?
                  -> DmdType    -- Demand type of body
                  -> Id         -- Lambda binder
                  -> WithDmdType Id  -- Demand type of lambda
                                     -- and binder annotated with demand

annotateLamIdBndr env arg_of_dfun dmd_ty id
-- For lambdas we add the demand to the argument demands
-- Only called for Ids
  = ASSERT( isId id )
    -- pprTrace "annLamBndr" (vcat [ppr id, ppr dmd_ty, ppr final_ty]) $
    WithDmdType final_ty new_id
  where
    new_id = setIdDemandInfo id dmd
      -- Watch out!  See note [Lambda-bound unfoldings]
    final_ty = case maybeUnfoldingTemplate (idUnfolding id) of
                 Nothing  -> main_ty
                 Just unf -> main_ty `plusDmdType` unf_ty
                          where
                             (unf_ty, _) = dmdAnalStar env dmd unf

    main_ty = addDemand dmd dmd_ty'
    WithDmdType dmd_ty' dmd = findBndrDmd env arg_of_dfun dmd_ty id

{- Note [NOINLINE and strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At one point we disabled strictness for NOINLINE functions, on the
grounds that they should be entirely opaque.  But that lost lots of
useful semantic strictness information, so now we analyse them like
any other function, and pin strictness information on them.

That in turn forces us to worker/wrapper them; see
Note [Worker-wrapper for NOINLINE functions] in GHC.Core.Opt.WorkWrap.


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


Note [Lambda-bound unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We allow a lambda-bound variable to carry an unfolding, a facility that is used
exclusively for join points; see Note [Case binders and join points].  If so,
we must be careful to demand-analyse the RHS of the unfolding!  Example
   \x. \y{=Just x}. <body>
Then if <body> uses 'y', then transitively it uses 'x', and we must not
forget that fact, otherwise we might make 'x' absent when it isn't.


************************************************************************
*                                                                      *
\subsection{Strictness signatures}
*                                                                      *
************************************************************************
-}

type DFunFlag = Bool  -- indicates if the lambda being considered is in the
                      -- sequence of lambdas at the top of the RHS of a dfun
notArgOfDfun :: DFunFlag
notArgOfDfun = False

data AnalEnv = AE
   { ae_strict_dicts :: !Bool -- ^ Enable strict dict
   , ae_sigs         :: !SigEnv
   , ae_virgin       :: !Bool -- ^ True on first iteration only
                              -- See Note [Initialising strictness]
   , ae_fam_envs     :: !FamInstEnvs
   }

        -- We use the se_env to tell us whether to
        -- record info about a variable in the DmdEnv
        -- We do so if it's a LocalId, but not top-level
        --
        -- The DmdEnv gives the demand on the free vars of the function
        -- when it is given enough args to satisfy the strictness signature

type SigEnv = VarEnv (StrictSig, TopLevelFlag)

instance Outputable AnalEnv where
  ppr env = text "AE" <+> braces (vcat
         [ text "ae_virgin =" <+> ppr (ae_virgin env)
         , text "ae_strict_dicts =" <+> ppr (ae_strict_dicts env)
         , text "ae_sigs =" <+> ppr (ae_sigs env)
         ])

emptyAnalEnv :: DmdAnalOpts -> FamInstEnvs -> AnalEnv
emptyAnalEnv opts fam_envs
    = AE { ae_strict_dicts = dmd_strict_dicts opts
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
  = extendVarEnvList sigs [ (var, (idStrictness var, top_lvl)) | var <- vars]

extendAnalEnv :: TopLevelFlag -> AnalEnv -> Id -> StrictSig -> AnalEnv
extendAnalEnv top_lvl env var sig
  = env { ae_sigs = extendSigEnv top_lvl (ae_sigs env) var sig }

extendSigEnv :: TopLevelFlag -> SigEnv -> Id -> StrictSig -> SigEnv
extendSigEnv top_lvl sigs var sig = extendVarEnv sigs var (sig, top_lvl)

lookupSigEnv :: AnalEnv -> Id -> Maybe (StrictSig, TopLevelFlag)
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
                        WithDmdType dmd_ty2 dmd  = findBndrDmd env False dmd_ty1 b
                    in WithDmdType dmd_ty2  (dmd : dmds)
      | otherwise = go dmd_ty bs

findBndrDmd :: AnalEnv -> Bool -> DmdType -> Id -> WithDmdType Demand
-- See Note [Trimming a demand to a type]
findBndrDmd env arg_of_dfun dmd_ty id
  = -- pprTrace "findBndrDmd" (ppr id $$ ppr dmd_ty $$ ppr starting_dmd $$ ppr dmd') $
    WithDmdType dmd_ty' dmd'
  where
    dmd' = strictify $
           trimToType starting_dmd (findTypeShape fam_envs id_ty)

    (dmd_ty', starting_dmd) = peelFV dmd_ty id

    id_ty = idType id

    strictify dmd
      | ae_strict_dicts env
             -- We never want to strictify a recursive let. At the moment
             -- annotateBndr is only call for non-recursive lets; if that
             -- changes, we need a RecFlag parameter and another guard here.
      , not arg_of_dfun -- See Note [Do not strictify the argument dictionaries of a dfun]
      = strictifyDictDmd id_ty dmd
      | otherwise
      = dmd

    fam_envs = ae_fam_envs env

{- Note [Initialising strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
