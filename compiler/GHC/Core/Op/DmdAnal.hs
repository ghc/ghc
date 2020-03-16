{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


                        -----------------
                        A demand analysis
                        -----------------
-}

{-# LANGUAGE CPP #-}

module GHC.Core.Op.DmdAnal ( dmdAnalProgram ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Driver.Session
import GHC.Types.Demand   -- All of it
import GHC.Core
import GHC.Core.Seq     ( seqBinds )
import GHC.Core.Op.WorkWrap.Lib
import Outputable
import GHC.Types.Var.Env
import GHC.Types.Basic
import Data.List        ( mapAccumL )
import GHC.Core.DataCon
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Core.Utils
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Coercion ( Coercion, coVarsOfCo, instNewTyCon_maybe )
import GHC.Core.FamInstEnv
import Util
import Maybes           ( isJust )
import TysPrim          ( realWorldStatePrimTy )
import ErrUtils         ( dumpIfSet_dyn, DumpFormat (..) )
import GHC.Types.Unique.Set

{-
************************************************************************
*                                                                      *
\subsection{Top level stuff}
*                                                                      *
************************************************************************
-}

dmdAnalProgram :: DynFlags -> FamInstEnvs -> CoreProgram -> IO CoreProgram
dmdAnalProgram dflags fam_envs binds = do
  let env             = emptyAnalEnv dflags fam_envs
  let binds_plus_dmds = snd $ mapAccumL dmdAnalTopBind env binds
  dumpIfSet_dyn dflags Opt_D_dump_str_signatures "Strictness signatures" FormatText $
    dumpIdInfoOfProgram (pprIfaceStrictSig . strictnessInfo) binds_plus_dmds
  -- See Note [Stamp out space leaks in demand analysis]
  seqBinds binds_plus_dmds `seq` return binds_plus_dmds

-- Analyse a (group of) top-level binding(s)
dmdAnalTopBind :: AnalEnv
               -> CoreBind
               -> (AnalEnv, CoreBind)
dmdAnalTopBind env (NonRec id rhs)
  = (extendAnalEnv TopLevel env id' (idStrictness id'), NonRec id' rhs')
  where
    ( _, id', rhs') = dmdAnalRhsLetDown Nothing env cleanEvalDmd id rhs

dmdAnalTopBind env (Rec pairs)
  = (env', Rec pairs')
  where
    (env', _, pairs')  = dmdFix TopLevel env cleanEvalDmd pairs
                -- We get two iterations automatically
                -- c.f. the NonRec case above

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
-}


{-
************************************************************************
*                                                                      *
\subsection{The analyser itself}
*                                                                      *
************************************************************************

Note [Ensure demand is strict]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's important not to analyse e with a lazy demand because
a) When we encounter   case s of (a,b) ->
        we demand s with U(d1d2)... but if the overall demand is lazy
        that is wrong, and we'd need to reduce the demand on s,
        which is inconvenient
b) More important, consider
        f (let x = R in x+x), where f is lazy
   We still want to mark x as demanded, because it will be when we
   enter the let.  If we analyse f's arg with a Lazy demand, we'll
   just mark x as Lazy
c) The application rule wouldn't be right either
   Evaluating (f x) in a L demand does *not* cause
   evaluation of f in a C(L) demand!
-}

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
            -> (BothDmdArg, CoreExpr)
dmdAnalStar env dmd e
  | (dmd_shell, cd) <- toCleanDmd dmd
  , (dmd_ty, e')    <- dmdAnal env cd e
  = ASSERT2( not (isUnliftedType (exprType e)) || exprOkForSpeculation e, ppr e )
    -- The argument 'e' should satisfy the let/app invariant
    -- See Note [Analysing with absent demand] in GHC.Types.Demand
    (postProcessDmdType dmd_shell dmd_ty, e')

-- Main Demand Analsysis machinery
dmdAnal, dmdAnal' :: AnalEnv
        -> CleanDemand         -- The main one takes a *CleanDemand*
        -> CoreExpr -> (DmdType, CoreExpr)

-- The CleanDemand is always strict and not absent
--    See Note [Ensure demand is strict]

dmdAnal env d e = -- pprTrace "dmdAnal" (ppr d <+> ppr e) $
                  dmdAnal' env d e

dmdAnal' _ _ (Lit lit)     = (emptyDmdType conDiv, Lit lit)
dmdAnal' _ _ (Type ty)     = (emptyDmdType conDiv, Type ty) -- Doesn't happen, in fact
dmdAnal' _ _ (Coercion co)
  = (unitDmdType (coercionDmdEnv co), Coercion co)

dmdAnal' env dmd (Var var)
  = (dmdTransform env var dmd, Var var)

dmdAnal' env dmd (Cast e co)
  = (dmd_ty `bothDmdType` mkBothDmdArg (coercionDmdEnv co), Cast e' co)
  where
    (dmd_ty, e') = dmdAnal env dmd e

dmdAnal' env dmd (Tick t e)
  = (dmd_ty, Tick t e')
  where
    (dmd_ty, e') = dmdAnal env dmd e

dmdAnal' env dmd (App fun (Type ty))
  = (fun_ty, App fun' (Type ty))
  where
    (fun_ty, fun') = dmdAnal env dmd fun

-- Lots of the other code is there to make this
-- beautiful, compositional, application rule :-)
dmdAnal' env dmd (App fun arg)
  = -- This case handles value arguments (type args handled above)
    -- Crucially, coercions /are/ handled here, because they are
    -- value arguments (#10288)
    let
        call_dmd          = mkCallDmd dmd
        (fun_ty, fun')    = dmdAnal env call_dmd fun
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
    (res_ty `bothDmdType` arg_ty, App fun' arg')

dmdAnal' env dmd (Lam var body)
  | isTyVar var
  = let
        (body_ty, body') = dmdAnal env dmd body
    in
    (body_ty, Lam var body')

  | otherwise
  = let (body_dmd, defer_and_use) = peelCallDmd dmd
          -- body_dmd: a demand to analyze the body

        (body_ty, body') = dmdAnal env body_dmd body
        (lam_ty, var')   = annotateLamIdBndr env notArgOfDfun body_ty var
    in
    (postProcessUnsat defer_and_use lam_ty, Lam var' body')

dmdAnal' env dmd (Case scrut case_bndr ty [(DataAlt dc, bndrs, rhs)])
  -- Only one alternative with a product constructor
  | let tycon = dataConTyCon dc
  , isJust (isDataProductTyCon_maybe tycon)
  , Just rec_tc' <- checkRecTc (ae_rec_tc env) tycon
  = let
        env_alt                  = env { ae_rec_tc = rec_tc' }
        (rhs_ty, rhs')           = dmdAnal env_alt dmd rhs
        (alt_ty1, dmds)          = findBndrsDmds env rhs_ty bndrs
        (alt_ty2, case_bndr_dmd) = findBndrDmd env False alt_ty1 case_bndr
        id_dmds                  = addCaseBndrDmd case_bndr_dmd dmds
        fam_envs                 = ae_fam_envs env
        -- See Note [Precise exceptions and strictness analysis] in Demand
        alt_ty3
          | mayThrowPreciseException fam_envs (idType case_bndr) scrut_ty
          = deferAfterPreciseException alt_ty2
          | otherwise
          = alt_ty2

        -- Compute demand on the scrutinee
        -- See Note [Demand on scrutinee of a product case]
        scrut_dmd          = mkProdDmd id_dmds
        (scrut_ty, scrut') = dmdAnal env scrut_dmd scrut
        res_ty             = alt_ty3 `bothDmdType` toBothDmdArg scrut_ty
        case_bndr'         = setIdDemandInfo case_bndr case_bndr_dmd
        bndrs'             = setBndrsDemandInfo bndrs id_dmds
    in
--    pprTrace "dmdAnal:Case1" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "dmd" <+> ppr dmd
--                                   , text "case_bndr_dmd" <+> ppr (idDemandInfo case_bndr')
--                                   , text "id_dmds" <+> ppr id_dmds
--                                   , text "scrut_dmd" <+> ppr scrut_dmd
--                                   , text "scrut_ty" <+> ppr scrut_ty
--                                   , text "alt_ty" <+> ppr alt_ty2
--                                   , text "res_ty" <+> ppr res_ty ]) $
    (res_ty, Case scrut' case_bndr' ty [(DataAlt dc, bndrs', rhs')])

dmdAnal' env dmd (Case scrut case_bndr ty alts)
  = let      -- Case expression with multiple alternatives
        (alt_tys, alts')     = mapAndUnzip (dmdAnalAlt env dmd case_bndr) alts
        (scrut_ty, scrut')   = dmdAnal env cleanEvalDmd scrut
        (alt_ty, case_bndr') = annotateBndr env (foldr lubDmdType botDmdType alt_tys) case_bndr
                               -- NB: Base case is botDmdType, for empty case alternatives
                               --     This is a unit for lubDmdType, and the right result
                               --     when there really are no alternatives
        res_ty               = alt_ty `bothDmdType` toBothDmdArg scrut_ty
    in
--    pprTrace "dmdAnal:Case2" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "scrut_ty" <+> ppr scrut_ty
--                                   , text "alt_tys" <+> ppr alt_tys
--                                   , text "alt_ty" <+> ppr alt_ty
--                                   , text "res_ty" <+> ppr res_ty ]) $
    (res_ty, Case scrut' case_bndr' ty alts')

-- Let bindings can be processed in two ways:
-- Down (RHS before body) or Up (body before RHS).
-- The following case handle the up variant.
--
-- It is very simple. For  let x = rhs in body
--   * Demand-analyse 'body' in the current environment
--   * Find the demand, 'rhs_dmd' placed on 'x' by 'body'
--   * Demand-analyse 'rhs' in 'rhs_dmd'
--
-- This is used for a non-recursive local let without manifest lambdas.
-- This is the LetUp rule in the paper “Higher-Order Cardinality Analysis”.
dmdAnal' env dmd (Let (NonRec id rhs) body)
  | useLetUp id
  = (final_ty, Let (NonRec id' rhs') body')
  where
    (body_ty, body')   = dmdAnal env dmd body
    (body_ty', id_dmd) = findBndrDmd env notArgOfDfun body_ty id
    id'                = setIdDemandInfo id id_dmd

    (rhs_ty, rhs')     = dmdAnalStar env (dmdTransformThunkDmd rhs id_dmd) rhs
    final_ty           = body_ty' `bothDmdType` rhs_ty

dmdAnal' env dmd (Let (NonRec id rhs) body)
  = (body_ty2, Let (NonRec id2 rhs') body')
  where
    (lazy_fv, id1, rhs') = dmdAnalRhsLetDown Nothing env dmd id rhs
    env1                 = extendAnalEnv NotTopLevel env id1 (idStrictness id1)
    (body_ty, body')     = dmdAnal env1 dmd body
    (body_ty1, id2)      = annotateBndr env body_ty id1
    body_ty2             = addLazyFVs body_ty1 lazy_fv -- see Note [Lazy and unleashable free variables]

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

dmdAnal' env dmd (Let (Rec pairs) body)
  = let
        (env', lazy_fv, pairs') = dmdFix NotTopLevel env dmd pairs
        (body_ty, body')        = dmdAnal env' dmd body
        body_ty1                = deleteFVs body_ty (map fst pairs)
        body_ty2                = addLazyFVs body_ty1 lazy_fv -- see Note [Lazy and unleashable free variables]
    in
    body_ty2 `seq`
    (body_ty2,  Let (Rec pairs') body')

dmdAnalAlt :: AnalEnv -> CleanDemand -> Id -> Alt Var -> (DmdType, Alt Var)
dmdAnalAlt env dmd case_bndr (con,bndrs,rhs)
  | null bndrs    -- Literals, DEFAULT, and nullary constructors
  , (rhs_ty, rhs') <- dmdAnal env dmd rhs
  = (rhs_ty, (con, [], rhs'))

  | otherwise     -- Non-nullary data constructors
  , (rhs_ty, rhs') <- dmdAnal env dmd rhs
  , (alt_ty, dmds) <- findBndrsDmds env rhs_ty bndrs
  , let case_bndr_dmd = findIdDemand alt_ty case_bndr
        id_dmds       = addCaseBndrDmd case_bndr_dmd dmds
  = (alt_ty, (con, setBndrsDemandInfo bndrs id_dmds, rhs'))

-- | Returns True if an expression of the given Type and DmdType might throw a
-- precise exception. In particular, that implies that the Type
-- 'forcesRealWorld', because otherwise it can't call 'raiseIO#'.
-- Except for black magic like 'unsafePerformIO' and friends, in which all
-- precise exception guarantees are off the table.
-- See Note [Precise exceptions and strictness analysis] in Demand.hs
mayThrowPreciseException :: FamInstEnvs -> Type -> DmdType -> Bool
mayThrowPreciseException fam_envs ty dmd_ty
  = mayThrowPreciseDmdType dmd_ty && forcesRealWorld fam_envs ty

-- | Whether a 'seqDmd' on an expression of the given type may force
-- @State# RealWorld@, incurring a side-effect (ignoring unsafe shenigans like
-- 'unsafePerformIO'). Looks through coercions and recurses into
-- unlifted/strict fields.
forcesRealWorld :: FamInstEnvs -> Type -> Bool
forcesRealWorld fam_envs = go initRecTc
  where
    go :: RecTcChecker -> Type -> Bool
    go rec_tc ty
      -- Found it!
      | ty `eqType`  realWorldStatePrimTy
      = True
      -- search depth-first
      | Just DataConAppContext{ dcac_dc = dc, dcac_arg_tys = field_tys }
          <- deepSplitProductType_maybe fam_envs ty
      -- don't check the same TyCon twice
      , Just rec_tc' <- checkRecTc rec_tc (dataConTyCon dc)
      = any (strict_field_forces rec_tc') field_tys
      | otherwise
      = False

    strict_field_forces rec_tc (field_ty, str_mark) =
      (isMarkedStrict str_mark || isLiftedType_maybe field_ty == Just False)
        && go rec_tc field_ty

-- | Tries to reset the precise exception flag from the 'StrictSig's
-- 'Divergence' if really it not 'mayThrowPreciseException'. Resetting the flag
-- means that the (very conservative) precise exception "taint" won't spread
-- from pure expressions for which the analysis failed to prove absence of
-- precise excpetions.
tryClearPreciseException :: FamInstEnvs -> Type -> StrictSig -> StrictSig
tryClearPreciseException fam_envs ty sig@(StrictSig dmd_ty@(DmdType fvs args div))
  | not (mayThrowPreciseDmdType dmd_ty) -- Why bother clearing if there is nothing to clear?
  = sig
  | otherwise
  = go initRecTc (length args) ty
  where
    -- This looks through foralls, arrows and newtypes like Core.Arity.typeArity
    -- The RecTcChecker tracks newtype unwrappings
    go :: RecTcChecker -> Int -> Type -> StrictSig
    go rec_nts n ty
      | Just (_, ty') <- splitForAllTy_maybe ty -- foralls
      = go rec_nts n     ty'
      | Just (_, ty') <- splitFunTy_maybe ty    -- arrows
      = go rec_nts (n-1) ty'
      | Just (tc,tys) <- splitTyConApp_maybe ty
      , Just (ty', _) <- instNewTyCon_maybe tc tys
      , Just rec_nts' <- checkRecTc rec_nts tc  -- newtypes
      = go rec_nts' n ty'
    go _ 0 ty
      | mayThrowPreciseException fam_envs ty dmd_ty
      = sig
    go _ _ _
      = StrictSig (DmdType fvs args (removeExn div))

{- Note [Demand on the scrutinee of a product case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When figuring out the demand on the scrutinee of a product case,
we use the demands of the case alternative, i.e. id_dmds.
But note that these include the demand on the case binder;
see Note [Demand on case-alternative binders] in GHC.Types.Demand.
This is crucial. Example:
   f x = case x of y { (a,b) -> k y a }
If we just take scrut_demand = U(L,A), then we won't pass x to the
worker, so the worker will rebuild
     x = (a, absent-error)
and that'll crash.

Note [Aggregated demand for cardinality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

dmdTransform :: AnalEnv         -- The strictness environment
             -> Id              -- The function
             -> CleanDemand     -- The demand on the function
             -> DmdType         -- The demand type of the function in this context
        -- Returned DmdEnv includes the demand on
        -- this function plus demand on its free variables

dmdTransform env var dmd
  | isDataConWorkId var                          -- Data constructor
  = dmdTransformDataConSig (idArity var) (idStrictness var) dmd

  | gopt Opt_DmdTxDictSel (ae_dflags env),
    Just _ <- isClassOpId_maybe var -- Dictionary component selector
  = dmdTransformDictSelSig (idStrictness var) dmd

  | isGlobalId var                               -- Imported function
  , let res = dmdTransformSig (idStrictness var) dmd
  = -- pprTrace "dmdTransform" (vcat [ppr var, ppr (idStrictness var), ppr dmd, ppr res])
    res

  | Just (sig, top_lvl) <- lookupSigEnv env var  -- Local letrec bound thing
  , let fn_ty = dmdTransformSig sig dmd
  = -- pprTrace "dmdTransform" (vcat [ppr var, ppr sig, ppr dmd, ppr fn_ty]) $
    if isTopLevel top_lvl
    then fn_ty   -- Don't record top level things
    else addVarDmd fn_ty var (mkOnceUsedDmd dmd)

  | otherwise                                    -- Local non-letrec-bound thing
  = unitDmdType (unitVarEnv var (mkOnceUsedDmd dmd))

{-
************************************************************************
*                                                                      *
\subsection{Bindings}
*                                                                      *
************************************************************************
-}

-- Recursive bindings
dmdFix :: TopLevelFlag
       -> AnalEnv                            -- Does not include bindings for this binding
       -> CleanDemand
       -> [(Id,CoreExpr)]
       -> (AnalEnv, DmdEnv, [(Id,CoreExpr)]) -- Binders annotated with strictness info

dmdFix top_lvl env let_dmd orig_pairs
  = loop 1 initial_pairs
  where
    bndrs = map fst orig_pairs

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
    loop n pairs
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

        start = (extendAnalEnvs top_lvl start_env (map fst pairs), emptyDmdEnv)

        ((_,lazy_fv), pairs') = mapAccumL my_downRhs start pairs
                -- mapAccumL: Use the new signature to do the next pair
                -- The occurrence analyser has arranged them in a good order
                -- so this can significantly reduce the number of iterations needed

        my_downRhs (env, lazy_fv) (id,rhs)
          = ((env', lazy_fv'), (id', rhs'))
          where
            (lazy_fv1, id', rhs') = dmdAnalRhsLetDown (Just bndrs) env let_dmd id rhs
            lazy_fv'              = plusVarEnv_C bothDmd lazy_fv lazy_fv1
            env'                  = extendAnalEnv top_lvl env id (idStrictness id')


    zapIdStrictness :: [(Id, CoreExpr)] -> [(Id, CoreExpr)]
    zapIdStrictness pairs
      = [(setIdStrictnessResetExc env id (emptySig topDiv), rhs) | (id, rhs) <- pairs ]

{-
Note [Safe abortion in the fixed-point iteration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Fixed-point iteration may fail to terminate. But we cannot simply give up and
return the environment and code unchanged! We still need to do one additional
round, for two reasons:

 * To get information on used free variables (both lazy and strict!)
   (see Note [Lazy and unleashable free variables])
 * To ensure that all expressions have been traversed at least once, and any left-over
   strictness annotations have been updated.

This final iteration does not add the variables to the strictness signature
environment, which effectively assigns them 'nopSig' (see "getStrictness")

-}

-- Let bindings can be processed in two ways:
-- Down (RHS before body) or Up (body before RHS).
-- dmdAnalRhsLetDown implements the Down variant:
--  * assuming a demand of <L,U>
--  * looking at the definition
--  * determining a strictness signature
--
-- It is used for toplevel definition, recursive definitions and local
-- non-recursive definitions that have manifest lambdas.
-- Local non-recursive definitions without a lambda are handled with LetUp.
--
-- This is the LetDown rule in the paper “Higher-Order Cardinality Analysis”.
dmdAnalRhsLetDown
  :: Maybe [Id]   -- Just bs <=> recursive, Nothing <=> non-recursive
  -> AnalEnv -> CleanDemand
  -> Id -> CoreExpr
  -> (DmdEnv, Id, CoreExpr)
-- Process the RHS of the binding, add the strictness signature
-- to the Id, and augment the environment with the signature as well.
dmdAnalRhsLetDown rec_flag env let_dmd id rhs
  = (lazy_fv, id', rhs')
  where
    rhs_arity      = idArity id
    rhs_dmd
      -- See Note [Demand analysis for join points]
      -- See Note [Invariants on join points] invariant 2b, in GHC.Core
      --     rhs_arity matches the join arity of the join point
      | isJoinId id
      = mkCallDmds rhs_arity let_dmd
      | otherwise
      -- NB: rhs_arity
      -- See Note [Demand signatures are computed for a threshold demand based on idArity]
      = mkRhsDmd env rhs_arity rhs
    (DmdType rhs_fv rhs_dmds rhs_div, rhs')
                   = dmdAnal env rhs_dmd rhs
    sig            = mkStrictSigForArity rhs_arity (mkDmdType sig_fv rhs_dmds rhs_div)
    id'            = -- pprTraceWith "dmdAnalRhsLetDown" (\sig'-> ppr id <+> ppr sig <+> ppr sig') $
                     setIdStrictnessResetExc env id sig
        -- See Note [NOINLINE and strictness]


    -- See Note [Aggregated demand for cardinality]
    rhs_fv1 = case rec_flag of
                Just bs -> reuseEnv (delVarEnvList rhs_fv bs)
                Nothing -> rhs_fv

    -- See Note [Lazy and unleashable free variables]
    (lazy_fv, sig_fv) = splitFVs is_thunk rhs_fv1
    is_thunk = not (exprIsHNF rhs) && not (isJoinId id)

-- | @mkRhsDmd env rhs_arity rhs@ creates a 'CleanDemand' for
-- unleashing on the given function's @rhs@, by creating
-- a call demand of @rhs_arity@
-- See Historical Note [Product demands for function body]
mkRhsDmd :: AnalEnv -> Arity -> CoreExpr -> CleanDemand
mkRhsDmd _env rhs_arity _rhs = mkCallDmds rhs_arity cleanEvalDmd

-- | If given the let-bound 'Id', 'useLetUp' determines whether we should
-- process the binding up (body before rhs) or down (rhs before body).
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
useLetUp :: Var -> Bool
useLetUp f = idArity f == 0 && not (isJoinId f)

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

The entire thing is in a C(S) context, so j's strictness signature
will be    [A]b
meaning one absent argument, returns bottom.  That seems odd because
there's a \y inside.  But it's right because when consumed in a C(1)
context the RHS of the join point is indeed bottom.

Note [Demand signatures are computed for a threshold demand based on idArity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We compute demand signatures assuming idArity incoming arguments to approximate
behavior for when we have a call site with at least that many arguments. idArity
is /at least/ the number of manifest lambdas, but might be higher for PAPs and
trivial RHS (see Note [Demand analysis for trivial right-hand sides]).

Because idArity of a function varies independently of its cardinality properties
(cf. Note [idArity varies independently of dmdTypeDepth]), we implicitly encode
the arity for when a demand signature is sound to unleash in its 'dmdTypeDepth'
(cf. Note [Understanding DmdType and StrictSig] in GHC.Types.Demand). It is unsound to
unleash a demand signature when the incoming number of arguments is less than
that. See Note [What are demand signatures?] for more details on soundness.

Why idArity arguments? Because that's a conservative estimate of how many
arguments we must feed a function before it does anything interesting with them.
Also it elegantly subsumes the trivial RHS and PAP case.

There might be functions for which we might want to analyse for more incoming
arguments than idArity. Example:

  f x =
    if expensive
      then \y -> ... y ...
      else \y -> ... y ...

We'd analyse `f` under a unary call demand C(S), corresponding to idArity
being 1. That's enough to look under the manifest lambda and find out how a
unary call would use `x`, but not enough to look into the lambdas in the if
branches.

On the other hand, if we analysed for call demand C(C(S)), we'd get useful
strictness info for `y` (and more precise info on `x`) and possibly CPR
information, but

  * We would no longer be able to unleash the signature at unary call sites
  * Performing the worker/wrapper split based on this information would be
    implicitly eta-expanding `f`, playing fast and loose with divergence and
    even being unsound in the presence of newtypes, so we refrain from doing so.
    Also see Note [Don't eta expand in w/w] in GHC.Core.Op.WorkWrap.

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

`go` might have a strictness signature of `<S><L>`. The simplifier will identify
`go` as a nullary join point through `joinPointBinding_maybe` and float the
coercion into the binding, leading to an arity decrease:

    join go = (\x y -> `x` seq ...) |> co in go

With the CoreLint check, we would have to zap `go`'s perfectly viable strictness
signature.

Note [What are demand signatures?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Demand analysis interprets expressions in the abstract domain of demand
transformers. Given an incoming demand we put an expression under, its abstract
transformer gives us back a demand type denoting how other things (like
arguments and free vars) were used when the expression was evaluated.
Here's an example:

  f x y =
    if x + expensive
      then \z -> z + y * ...
      else \z -> z * ...

The abstract transformer (let's call it F_e) of the if expression (let's call it
e) would transform an incoming head demand <S,HU> into a demand type like
{x-><S,1*U>,y-><L,U>}<L,U>. In pictures:

     Demand ---F_e---> DmdType
     <S,HU>            {x-><S,1*U>,y-><L,U>}<L,U>

Let's assume that the demand transformers we compute for an expression are
correct wrt. to some concrete semantics for Core. How do demand signatures fit
in? They are strange beasts, given that they come with strict rules when to
it's sound to unleash them.

Fortunately, we can formalise the rules with Galois connections. Consider
f's strictness signature, {}<S,1*U><L,U>. It's a single-point approximation of
the actual abstract transformer of f's RHS for arity 2. So, what happens is that
we abstract *once more* from the abstract domain we already are in, replacing
the incoming Demand by a simple lattice with two elements denoting incoming
arity: A_2 = {<2, >=2} (where '<2' is the top element and >=2 the bottom
element). Here's the diagram:

     A_2 -----f_f----> DmdType
      ^                   |
      | α               γ |
      |                   v
     Demand ---F_f---> DmdType

With
  α(C1(C1(_))) = >=2 -- example for usage demands, but similar for strictness
  α(_)         =  <2
  γ(ty)        =  ty
and F_f being the abstract transformer of f's RHS and f_f being the abstracted
abstract transformer computable from our demand signature simply by

  f_f(>=2) = {}<S,1*U><L,U>
  f_f(<2)  = postProcessUnsat {}<S,1*U><L,U>

where postProcessUnsat makes a proper top element out of the given demand type.

Note [Demand analysis for trivial right-hand sides]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    foo = plusInt |> co
where plusInt is an arity-2 function with known strictness.  Clearly
we want plusInt's strictness to propagate to foo!  But because it has
no manifest lambdas, it won't do so automatically, and indeed 'co' might
have type (Int->Int->Int) ~ T.

Fortunately, GHC.Core.Arity gives 'foo' arity 2, which is enough for LetDown to
forward plusInt's demand signature, and all is well (see Note [Newtype arity] in
GHC.Core.Arity)! A small example is the test case NewtypeArity.


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

That motivated using a demand of C(C(C(S(L,L)))) for the RHS, where
(solely because the result was a product) we used a product demand
(albeit with lazy components) for the body. But that gives very silly
behaviour -- see #17932.   Happily it turns out now to be entirely
unnecessary: we get good results with C(C(C(S))).   So I simply
deleted the special case.

************************************************************************
*                                                                      *
\subsection{Strictness signatures and types}
*                                                                      *
************************************************************************
-}

unitDmdType :: DmdEnv -> DmdType
unitDmdType dmd_env = DmdType dmd_env [] conDiv

coercionDmdEnv :: Coercion -> DmdEnv
coercionDmdEnv co = mapVarEnv (const topDmd) (getUniqSet $ coVarsOfCo co)
                    -- The VarSet from coVarsOfCo is really a VarEnv Var

addVarDmd :: DmdType -> Var -> Demand -> DmdType
addVarDmd (DmdType fv ds res) var dmd
  = DmdType (extendVarEnv_C bothDmd fv var dmd) ds res

addLazyFVs :: DmdType -> DmdEnv -> DmdType
addLazyFVs dmd_ty lazy_fvs
  = dmd_ty `bothDmdType` mkBothDmdArg lazy_fvs
        -- Using bothDmdType (rather than just both'ing the envs)
        -- is vital.  Consider
        --      let f = \x -> (x,y)
        --      in  error (f 3)
        -- Here, y is treated as a lazy-fv of f, but we must `bothDmd` that L
        -- demand with the bottom coming up from 'error'
        --
        -- I got a loop in the fixpointer without this, due to an interaction
        -- with the lazy_fv filtering in dmdAnalRhsLetDown.  Roughly, it was
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
setBndrsDemandInfo (b:bs) (d:ds)
  | isTyVar b = b : setBndrsDemandInfo bs (d:ds)
  | otherwise = setIdDemandInfo b d : setBndrsDemandInfo bs ds
setBndrsDemandInfo [] ds = ASSERT( null ds ) []
setBndrsDemandInfo bs _  = pprPanic "setBndrsDemandInfo" (ppr bs)

annotateBndr :: AnalEnv -> DmdType -> Var -> (DmdType, Var)
-- The returned env has the var deleted
-- The returned var is annotated with demand info
-- according to the result demand of the provided demand type
-- No effect on the argument demands
annotateBndr env dmd_ty var
  | isId var  = (dmd_ty', setIdDemandInfo var dmd)
  | otherwise = (dmd_ty, var)
  where
    (dmd_ty', dmd) = findBndrDmd env False dmd_ty var

annotateLamIdBndr :: AnalEnv
                  -> DFunFlag   -- is this lambda at the top of the RHS of a dfun?
                  -> DmdType    -- Demand type of body
                  -> Id         -- Lambda binder
                  -> (DmdType,  -- Demand type of lambda
                      Id)       -- and binder annotated with demand

annotateLamIdBndr env arg_of_dfun dmd_ty id
-- For lambdas we add the demand to the argument demands
-- Only called for Ids
  = ASSERT( isId id )
    -- pprTrace "annLamBndr" (vcat [ppr id, ppr _dmd_ty]) $
    (final_ty, setIdDemandInfo id dmd)
  where
      -- Watch out!  See note [Lambda-bound unfoldings]
    final_ty = case maybeUnfoldingTemplate (idUnfolding id) of
                 Nothing  -> main_ty
                 Just unf -> main_ty `bothDmdType` unf_ty
                          where
                             (unf_ty, _) = dmdAnalStar env dmd unf

    main_ty = addDemand dmd dmd_ty'
    (dmd_ty', dmd) = findBndrDmd env arg_of_dfun dmd_ty id

deleteFVs :: DmdType -> [Var] -> DmdType
deleteFVs (DmdType fvs dmds res) bndrs
  = DmdType (delVarEnvList fvs bndrs) dmds res

{-
Note [NOINLINE and strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The strictness analyser used to have a HACK which ensured that NOINLNE
things were not strictness-analysed.  The reason was unsafePerformIO.
Left to itself, the strictness analyser would discover this strictness
for unsafePerformIO:
        unsafePerformIO:  C(U(AV))
But then consider this sub-expression
        unsafePerformIO (\s -> let r = f x in
                               case writeIORef v r s of (# s1, _ #) ->
                               (# s1, r #)
The strictness analyser will now find that r is sure to be eval'd,
and may then hoist it out.  This makes tests/lib/should_run/memo002
deadlock.

Solving this by making all NOINLINE things have no strictness info is overkill.
In particular, it's overkill for runST, which is perfectly respectable.
Consider
        f x = runST (return x)
This should be strict in x.

So the new plan is to define unsafePerformIO using the 'lazy' combinator:

        unsafePerformIO (IO m) = lazy (case m realWorld# of (# _, r #) -> r)

Remember, 'lazy' is a wired-in identity-function Id, of type a->a, which is
magically NON-STRICT, and is inlined after strictness analysis.  So
unsafePerformIO will look non-strict, and that's what we want.

Now we don't need the hack in the strictness analyser.  HOWEVER, this
decision does mean that even a NOINLINE function is not entirely
opaque: some aspect of its implementation leaks out, notably its
strictness.  For example, if you have a function implemented by an
error stub, but which has RULES, you may want it not to be eliminated
in favour of error!

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

data AnalEnv
  = AE { ae_dflags :: DynFlags
       , ae_sigs   :: SigEnv
       , ae_virgin :: Bool    -- True on first iteration only
                              -- See Note [Initialising strictness]
       , ae_rec_tc :: RecTcChecker
       , ae_fam_envs :: FamInstEnvs
 }

        -- We use the se_env to tell us whether to
        -- record info about a variable in the DmdEnv
        -- We do so if it's a LocalId, but not top-level
        --
        -- The DmdEnv gives the demand on the free vars of the function
        -- when it is given enough args to satisfy the strictness signature

type SigEnv = VarEnv (StrictSig, TopLevelFlag)

instance Outputable AnalEnv where
  ppr (AE { ae_sigs = env, ae_virgin = virgin })
    = text "AE" <+> braces (vcat
         [ text "ae_virgin =" <+> ppr virgin
         , text "ae_sigs =" <+> ppr env ])

emptyAnalEnv :: DynFlags -> FamInstEnvs -> AnalEnv
emptyAnalEnv dflags fam_envs
    = AE { ae_dflags = dflags
         , ae_sigs = emptySigEnv
         , ae_virgin = True
         , ae_rec_tc = initRecTc
         , ae_fam_envs = fam_envs
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

findBndrsDmds :: AnalEnv -> DmdType -> [Var] -> (DmdType, [Demand])
-- Return the demands on the Ids in the [Var]
findBndrsDmds env dmd_ty bndrs
  = go dmd_ty bndrs
  where
    go dmd_ty []  = (dmd_ty, [])
    go dmd_ty (b:bs)
      | isId b    = let (dmd_ty1, dmds) = go dmd_ty bs
                        (dmd_ty2, dmd)  = findBndrDmd env False dmd_ty1 b
                    in (dmd_ty2, dmd : dmds)
      | otherwise = go dmd_ty bs

findBndrDmd :: AnalEnv -> Bool -> DmdType -> Id -> (DmdType, Demand)
-- See Note [Trimming a demand to a type] in GHC.Types.Demand
findBndrDmd env arg_of_dfun dmd_ty id
  = (dmd_ty', dmd')
  where
    dmd' = strictify $
           trimToType starting_dmd (findTypeShape fam_envs id_ty)

    (dmd_ty', starting_dmd) = peelFV dmd_ty id

    id_ty = idType id

    strictify dmd
      | gopt Opt_DictsStrict (ae_dflags env)
             -- We never want to strictify a recursive let. At the moment
             -- annotateBndr is only call for non-recursive lets; if that
             -- changes, we need a RecFlag parameter and another guard here.
      , not arg_of_dfun -- See Note [Do not strictify the argument dictionaries of a dfun]
      = strictifyDictDmd id_ty dmd
      | otherwise
      = dmd

    fam_envs = ae_fam_envs env

setIdStrictnessResetExc :: AnalEnv -> Id -> StrictSig -> Id
setIdStrictnessResetExc env id sig
  = setIdStrictness id (tryClearPreciseException (ae_fam_envs env) (idType id) sig)

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
  \y [Demand=1*U] let x = y in x + x
to
  \y [Demand=1*U] y + y
which is quite a lie.

The once-used information is (currently) only used by the code
generator, though.  So:

 * We zap the used-once info in the worker-wrapper;
   see Note [Zapping Used Once info in WorkWrap] in
   GHC.Core.Op.WorkWrap.
   If it's not reliable, it's better not to have it at all.

 * Just before TidyCore, we add a pass of the demand analyser,
      but WITHOUT subsequent worker/wrapper and simplifier,
   right before TidyCore.  See SimplCore.getCoreToDo.

   This way, correct information finds its way into the module interface
   (strictness signatures!) and the code generator (single-entry thunks!)

Note that, in contrast, the single-call information (C1(..)) /can/ be
relied upon, as the simplifier tends to be very careful about not
duplicating actual function calls.

Also see #11731.
-}
