{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


                        -----------------
                        A demand analysis
                        -----------------
-}

{-# LANGUAGE CPP #-}

module DmdAnal ( dmdAnalProgram ) where

#include "HsVersions.h"

import DynFlags
import WwLib            ( findTypeShape, deepSplitProductType_maybe )
import Demand   -- All of it
import CoreSyn
import Outputable
import VarEnv
import BasicTypes
import Data.List
import DataCon
import Id
import MkId             ( dataConWorkStrictSig )
import CoreUtils        ( exprIsHNF, exprType, exprIsTrivial )
import TyCon
import Type
import Coercion         ( Coercion, coVarsOfCo )
import FamInstEnv
import Util
import Maybes           ( isJust )
import TysWiredIn
import TysPrim          ( realWorldStatePrimTy )
import ErrUtils         ( dumpIfSet_dyn )
import Name             ( getName, stableNameCmp )
import Data.Function    ( on )

{-
************************************************************************
*                                                                      *
\subsection{Top level stuff}
*                                                                      *
************************************************************************
-}

dmdAnalProgram :: DynFlags -> FamInstEnvs -> CoreProgram -> IO CoreProgram
dmdAnalProgram dflags fam_envs binds
  = do {
        let { binds_plus_dmds = do_prog binds } ;
        dumpIfSet_dyn dflags Opt_D_dump_str_signatures
                      "Strictness signatures" $
            dumpStrSig binds_plus_dmds ;
        return binds_plus_dmds
    }
  where
    do_prog :: CoreProgram -> CoreProgram
    do_prog binds = snd $ mapAccumL dmdAnalTopBind (emptyAnalEnv dflags fam_envs) binds

-- Analyse a (group of) top-level binding(s)
dmdAnalTopBind :: AnalEnv
               -> CoreBind
               -> (AnalEnv, CoreBind)
dmdAnalTopBind sigs (NonRec id rhs)
  = (extendAnalEnv TopLevel sigs id2 (idStrictness id2), NonRec id2 rhs2)
  where
    ( _, _,   rhs1) = dmdAnalRhsLetDown TopLevel Nothing sigs             id rhs
    ( _, id2, rhs2) = dmdAnalRhsLetDown TopLevel Nothing (nonVirgin sigs) id rhs1
        -- Do two passes to improve CPR information
        -- See Note [CPR for thunks]
        -- See Note [Optimistic CPR in the "virgin" case]
        -- See Note [Initial CPR for strict binders]

dmdAnalTopBind sigs (Rec pairs)
  = (sigs', Rec pairs')
  where
    (sigs', _, pairs')  = dmdFix TopLevel sigs pairs
                -- We get two iterations automatically
                -- c.f. the NonRec case above

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
-- See |-* relation in the companion paper
dmdAnalStar :: AnalEnv
            -> Demand   -- This one takes a *Demand*
            -> CoreExpr -> (BothDmdArg, CoreExpr)
dmdAnalStar env dmd e
  | (defer_and_use, cd) <- toCleanDmd dmd (exprType e)
  , (dmd_ty, e')        <- dmdAnal env cd e
  = (postProcessDmdType defer_and_use dmd_ty, e')

-- Main Demand Analsysis machinery
dmdAnal, dmdAnal' :: AnalEnv
        -> CleanDemand         -- The main one takes a *CleanDemand*
        -> CoreExpr -> (DmdType, CoreExpr)

-- The CleanDemand is always strict and not absent
--    See Note [Ensure demand is strict]

dmdAnal env d e = -- pprTrace "dmdAnal" (ppr d <+> ppr e) $
                  dmdAnal' env d e

dmdAnal' _ _ (Lit lit)     = (nopDmdType, Lit lit)
dmdAnal' _ _ (Type ty)     = (nopDmdType, Type ty)      -- Doesn't happen, in fact
dmdAnal' _ _ (Coercion co)
  = (unitDmdType (coercionDmdEnv co), Coercion co)

dmdAnal' env dmd (Var var)
  = (dmdTransform env var dmd, Var var)

dmdAnal' env dmd (Cast e co)
  = (dmd_ty `bothDmdType` mkBothDmdArg (coercionDmdEnv co), Cast e' co)
  where
    (dmd_ty, e') = dmdAnal env dmd e

{-       ----- I don't get this, so commenting out -------
    to_co        = pSnd (coercionKind co)
    dmd'
      | Just tc <- tyConAppTyCon_maybe to_co
      , isRecursiveTyCon tc = cleanEvalDmd
      | otherwise           = dmd
        -- This coerce usually arises from a recursive
        -- newtype, and we don't want to look inside them
        -- for exactly the same reason that we don't look
        -- inside recursive products -- we might not reach
        -- a fixpoint.  So revert to a vanilla Eval demand
-}

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
    -- value arguments (Trac #10288)
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

dmdAnal' env dmd (ConApp dc args)
  = -- pprTrace "dmdAnal:ConApp" (vcat
    --      [ text "dmd =" <+> ppr dmd
    --      , text "dc =" <+> ppr dc
    --      , text "arity =" <+> ppr (dataConRepArity dc)
    --      , text "dc_strictSig =" <+> ppr dc_strictSig
    --      , text "dmd' =" <+> ppr dmd'
    --      , text "final_ty =" <+> ppr final_ty
    --      ])
    (final_ty, ConApp dc args')
  where
    dc_strictSig = dataConWorkStrictSig dc
    dmd' = dmdTransformSatDataConSig (dataConRepArity dc) dc_strictSig dmd
    -- TODO: unpack dmd'
    (final_ty, args') = go dmd' args

    go :: DmdType -> [CoreArg] -> (DmdType, [CoreArg])
    go fun_ty [] = (fun_ty, [])

    go fun_ty (Type ty : args)
       = (args_ty, Type ty : args')
      where
        (args_ty, args') = go fun_ty args

    go fun_ty (arg : args)
       = (args_ty `bothDmdType` arg_ty, arg': args')
      where
        (arg_dmd, res_ty) = splitDmdTy fun_ty
        (arg_ty, arg')    = dmdAnalStar env (dmdTransformThunkDmd arg arg_dmd) arg
        (args_ty, args') = go res_ty args


-- this is an anonymous lambda, since @dmdAnalRhsLetDown@ uses @collectBinders@
dmdAnal' env dmd (Lam var body)
  | isTyVar var
  = let
        (body_ty, body') = dmdAnal env dmd body
    in
    (body_ty, Lam var body')

  | otherwise
  = let (body_dmd, defer_and_use) = peelCallDmd dmd
          -- body_dmd: a demand to analyze the body

        env'             = extendSigsWithLam env var
        (body_ty, body') = dmdAnal env' body_dmd body
        (lam_ty, var')   = annotateLamIdBndr env notArgOfDfun body_ty var
    in
    (postProcessUnsat defer_and_use lam_ty, Lam var' body')

dmdAnal' env dmd (Case scrut case_bndr ty [(DataAlt dc, bndrs, rhs)])
  -- Only one alternative with a product constructor
  | let tycon = dataConTyCon dc
  , isJust (isDataProductTyCon_maybe tycon)
  , Just rec_tc' <- checkRecTc (ae_rec_tc env) tycon
  = let
        env_w_tc                 = env { ae_rec_tc = rec_tc' }
        env_alt                  = extendEnvForProdAlt env_w_tc scrut case_bndr dc bndrs
        (rhs_ty, rhs')           = dmdAnal env_alt dmd rhs
        (alt_ty1, dmds)          = findBndrsDmds env rhs_ty bndrs
        (alt_ty2, case_bndr_dmd) = findBndrDmd env False alt_ty1 case_bndr
        id_dmds                  = addCaseBndrDmd case_bndr_dmd dmds
        alt_ty3 | io_hack_reqd scrut dc bndrs = deferAfterIO alt_ty2
                | otherwise                   = alt_ty2

        -- Compute demand on the scrutinee
        -- See Note [Demand on scrutinee of a product case]
        scrut_dmd          = mkProdDmd (addDataConStrictness dc id_dmds)
        (scrut_ty, scrut') = dmdAnal env scrut_dmd scrut
        res_ty             = alt_ty3 `bothDmdType` toBothDmdArg scrut_ty
        case_bndr'         = setIdDemandInfo case_bndr case_bndr_dmd
        bndrs'             = setBndrsDemandInfo bndrs id_dmds
    in
--    pprTrace "dmdAnal:Case1" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "dmd" <+> ppr dmd
--                                   , text "case_bndr_dmd" <+> ppr (idDemandInfo case_bndr')
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
  | useLetUp rhs
  , Nothing <- unpackTrivial rhs
      -- dmdAnalRhsLetDown treats trivial right hand sides specially
      -- so if we have a trival right hand side, fall through to that.
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
    (lazy_fv, id1, rhs') = dmdAnalRhsLetDown NotTopLevel Nothing env id rhs
    env1                 = extendAnalEnv NotTopLevel env id1 (idStrictness id1)
    (body_ty, body')     = dmdAnal env1 dmd body
    (body_ty1, id2)      = annotateBndr env body_ty id1
    body_ty2             = addLazyFVs body_ty1 lazy_fv -- see Note [Lazy and unleasheable free variables]

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
        (env', lazy_fv, pairs') = dmdFix NotTopLevel env pairs
        (body_ty, body')        = dmdAnal env' dmd body
        body_ty1                = deleteFVs body_ty (map fst pairs)
        body_ty2                = addLazyFVs body_ty1 lazy_fv -- see Note [Lazy and unleasheable free variables]
    in
    body_ty2 `seq`
    (body_ty2,  Let (Rec pairs') body')

io_hack_reqd :: CoreExpr -> DataCon -> [Var] -> Bool
-- See Note [IO hack in the demand analyser]
io_hack_reqd scrut con bndrs
  | (bndr:_) <- bndrs
  , con == tupleDataCon Unboxed 2
  , idType bndr `eqType` realWorldStatePrimTy
  , (fun, _) <- collectArgs scrut
  = case fun of
      Var f -> not (isPrimOpId f)
      _     -> True
  | otherwise
  = False

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


{- Note [IO hack in the demand analyser]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's a hack here for I/O operations.  Consider
     case foo x s of { (# s, r #) -> y }
Is this strict in 'y'?  Normally yes, but what if 'foo' is an I/O
operation that simply terminates the program (not in an erroneous way)?
In that case we should not evaluate 'y' before the call to 'foo'.
Hackish solution: spot the IO-like situation and add a virtual branch,
as if we had
     case foo x s of
        (# s, r #) -> y
        other      -> return ()
So the 'y' isn't necessarily going to be evaluated

A more complete example (Trac #148, #1592) where this shows up is:
     do { let len = <expensive> ;
        ; when (...) (exitWith ExitSuccess)
        ; print len }

However, consider
  f x s = case getMaskingState# s of
            (# s, r #) ->
          case x of I# x2 -> ...

Here it is terribly sad to make 'f' lazy in 's'.  After all,
getMaskingState# is not going to diverge or throw an exception!  This
situation actually arises in GHC.IO.Handle.Internals.wantReadableHandle
(on an MVar not an Int), and made a material difference.

So if the scrutinee is a primop call, we *don't* apply the
state hack:
  - If is a simple, terminating one like getMaskingState,
    applying the hack is over-conservative.
  - If the primop is raise# then it returns bottom, so
    the case alternatives are already discarded.
  - If the primop can raise a non-IO exception, like
    divide by zero or seg-fault (eg writing an array
    out of bounds) then we don't mind evaluating 'x' first.

Note [Demand on the scrutinee of a product case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When figuring out the demand on the scrutinee of a product case,
we use the demands of the case alternative, i.e. id_dmds.
But note that these include the demand on the case binder;
see Note [Demand on case-alternative binders] in Demand.hs.
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
  = let res = dmdTransformSig (idStrictness var) dmd in
--    pprTrace "dmdTransform" (vcat [ppr var, ppr (idStrictness var), ppr dmd, ppr res])
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
       -> [(Id,CoreExpr)]
       -> (AnalEnv, DmdEnv, [(Id,CoreExpr)]) -- Binders annotated with stricness info

dmdFix top_lvl env orig_pairs
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
            -- Note [Lazy and unleasheable free variables]
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
            (lazy_fv1, id', rhs') = dmdAnalRhsLetDown top_lvl (Just bndrs) env id rhs
            lazy_fv'              = plusVarEnv_C bothDmd lazy_fv lazy_fv1
            env'                  = extendAnalEnv top_lvl env id (idStrictness id')


    zapIdStrictness :: [(Id, CoreExpr)] -> [(Id, CoreExpr)]
    zapIdStrictness pairs = [(setIdStrictness id nopSig, rhs) | (id, rhs) <- pairs ]

{-
Note [Safe abortion in the fixed-point iteration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Fixed-point iteration may fail to terminate. But we cannot simply give up and
return the environment and code unchanged! We still need to do one additional
round, for two reasons:

 * To get information on used free variables (both lazy and strict!)
   (see Note [Lazy and unleasheable free variables])
 * To ensure that all expressions have been traversed at least once, and any left-over
   strictness annotations have been updated.

This final iteration does not add the variables to the strictness signature
environment, which effectively assigns them 'nopSig' (see "getStrictness")

-}

-- Trivial RHS
-- See Note [Demand analysis for trivial right-hand sides]
dmdAnalTrivialRhs ::
    AnalEnv -> Id -> CoreExpr -> Var ->
    (DmdEnv, Id, CoreExpr)
dmdAnalTrivialRhs env id rhs fn
  = (fn_fv, set_idStrictness env id fn_str, rhs)
  where
    fn_str = getStrictness env fn
    fn_fv | isLocalId fn = unitVarEnv fn topDmd
          | otherwise    = emptyDmdEnv
    -- Note [Remember to demand the function itself]
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- fn_fv: don't forget to produce a demand for fn itself
    -- Lacking this caused Trac #9128
    -- The demand is very conservative (topDmd), but that doesn't
    -- matter; trivial bindings are usually inlined, so it only
    -- kicks in for top-level bindings and NOINLINE bindings

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
dmdAnalRhsLetDown :: TopLevelFlag
           -> Maybe [Id]   -- Just bs <=> recursive, Nothing <=> non-recursive
           -> AnalEnv -> Id -> CoreExpr
           -> (DmdEnv, Id, CoreExpr)
-- Process the RHS of the binding, add the strictness signature
-- to the Id, and augment the environment with the signature as well.
dmdAnalRhsLetDown top_lvl rec_flag env id rhs
  | Just fn <- unpackTrivial rhs   -- See Note [Demand analysis for trivial right-hand sides]
  = dmdAnalTrivialRhs env id rhs fn

  | otherwise
  = (lazy_fv, id', mkLams bndrs' body')
  where
    (bndrs, body)    = collectBinders rhs
    env_body         = foldl extendSigsWithLam env bndrs
    (body_ty, body') = dmdAnal env_body body_dmd body
    body_ty'         = removeDmdTyArgs body_ty -- zap possible deep CPR info
    (DmdType rhs_fv rhs_dmds rhs_res, bndrs')
                     = annotateLamBndrs env (isDFunId id) body_ty' bndrs
    sig_ty           = mkStrictSig (mkDmdType sig_fv rhs_dmds rhs_res')
    id'              = set_idStrictness env id sig_ty
        -- See Note [NOINLINE and strictness]

    -- See Note [Product demands for function body]
    body_dmd = case deepSplitProductType_maybe (ae_fam_envs env) (exprType body) of
                 Nothing            -> cleanEvalDmd
                 Just (dc, _, _, _) -> cleanEvalProdDmd (dataConRepArity dc)

    -- See Note [Aggregated demand for cardinality]
    rhs_fv1 = case rec_flag of
                Just bs -> reuseEnv (delVarEnvList rhs_fv bs)
                Nothing -> rhs_fv

    -- See Note [Lazy and unleashable free variables]
    (lazy_fv, sig_fv) = splitFVs is_thunk rhs_fv1

    rhs_res'  = trimCPRInfo trim_all trim_sums rhs_res
    trim_all  = is_thunk && not_strict
    trim_sums = not (isTopLevel top_lvl) -- See Note [CPR for sum types]

    -- See Note [CPR for thunks]
    is_thunk = not (exprIsHNF rhs)
    not_strict
       =  isTopLevel top_lvl  -- Top level and recursive things don't
       || isJust rec_flag     -- get their demandInfo set at all
       || not (isStrictDmd (idDemandInfo id) || ae_virgin env)
          -- See Note [Optimistic CPR in the "virgin" case]

unpackTrivial :: CoreExpr -> Maybe Id
-- Returns (Just v) if the arg is really equal to v, modulo
-- casts, type applications etc
-- See Note [Demand analysis for trivial right-hand sides]
unpackTrivial (Var v)                 = Just v
unpackTrivial (Cast e _)              = unpackTrivial e
unpackTrivial (Lam v e) | isTyVar v   = unpackTrivial e
unpackTrivial (App e a) | isTypeArg a = unpackTrivial e
unpackTrivial _                       = Nothing

-- | If given the RHS of a let-binding, this 'useLetUp' determines
-- whether we should process the binding up (body before rhs) or
-- down (rhs before body).
--
-- We use LetDown if there is a chance to get a useful strictness signature.
-- This is the case when there are manifest value lambdas.
useLetUp :: CoreExpr -> Bool
useLetUp (Lam v e) | isTyVar v = useLetUp e
useLetUp (Lam _ _)             = False
useLetUp _                     = True


{-
Note [Demand analysis for trivial right-hand sides]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        foo = plusInt |> co
where plusInt is an arity-2 function with known strictness.  Clearly
we want plusInt's strictness to propagate to foo!  But because it has
no manifest lambdas, it won't do so automatically, and indeed 'co' might
have type (Int->Int->Int) ~ T, so we *can't* eta-expand.  So we have a
special case for right-hand sides that are "trivial", namely variables,
casts, type applications, and the like.

Note that this can mean that 'foo' has an arity that is smaller than that
indicated by its demand info.  e.g. if co :: (Int->Int->Int) ~ T, then
foo's arity will be zero (see Note [exprArity invariant] in CoreArity),
but its demand signature will be that of plusInt. A small example is the
test case of Trac #8963.


Note [Product demands for function body]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This example comes from shootout/binary_trees:

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

So for the demand on the body of a RHS we use a product demand if it's
a product type.

************************************************************************
*                                                                      *
\subsection{Strictness signatures and types}
*                                                                      *
************************************************************************
-}

unitDmdType :: DmdEnv -> DmdType
unitDmdType dmd_env = DmdType dmd_env [] topRes

coercionDmdEnv :: Coercion -> DmdEnv
coercionDmdEnv co = mapVarEnv (const topDmd) (coVarsOfCo co)
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

annotateLamBndrs :: AnalEnv -> DFunFlag -> DmdType -> [Var] -> (DmdType, [Var])
annotateLamBndrs env args_of_dfun ty bndrs = mapAccumR annotate ty bndrs
  where
    annotate dmd_ty bndr
      | isId bndr = annotateLamIdBndr env args_of_dfun dmd_ty bndr
      | otherwise = (dmd_ty, bndr)

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

The difficulty with this is that we need the strictness type to
look at the body... but we now need the body to calculate the demand
on the variable, so we can decide whether its strictness type should
have a CPR in it or not.  Simple solution:
        a) use strictness info from the previous iteration
        b) make sure we do at least 2 iterations, by doing a second
           round for top-level non-recs.  Top level recs will get at
           least 2 iterations except for totally-bottom functions
           which aren't very interesting anyway.

NB: strictly_demanded is never true of a top-level Id, or of a recursive Id.

Note [Optimistic CPR in the "virgin" case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Demand and strictness info are initialized by top elements. However,
this prevents from inferring a CPR property in the first pass of the
analyser, so we keep an explicit flag ae_virgin in the AnalEnv
datatype.

We can't start with 'not-demanded' (i.e., top) because then consider
        f x = let
                  t = ... I# x
              in
              if ... then t else I# y else f x'

In the first iteration we'd have no demand info for x, so assume
not-demanded; then we'd get TopRes for f's CPR info.  Next iteration
we'd see that t was demanded, and so give it the CPR property, but by
now f has TopRes, so it will stay TopRes.  Instead, by checking the
ae_virgin flag at the first time round, we say 'yes t is demanded' the
first time.

However, this does mean that for non-recursive bindings we must
iterate twice to be sure of not getting over-optimistic CPR info,
in the case where t turns out to be not-demanded.  This is handled
by dmdAnalTopBind.


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

Note [Lazy and unleasheable free variables]
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


Note [Lamba-bound unfoldings]
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

getStrictness :: AnalEnv -> Id -> StrictSig
getStrictness env fn
  | isGlobalId fn                        = idStrictness fn
  | Just (sig, _) <- lookupSigEnv env fn = sig
  | otherwise                            = nopSig

nonVirgin :: AnalEnv -> AnalEnv
nonVirgin env = env { ae_virgin = False }

extendSigsWithLam :: AnalEnv -> Id -> AnalEnv
-- Extend the AnalEnv when we meet a lambda binder
extendSigsWithLam env id
  | isId id
  , isStrictDmd (idDemandInfo id) || ae_virgin env
       -- See Note [Optimistic CPR in the "virgin" case]
       -- See Note [Initial CPR for strict binders]
  , Just (dc,_,_,_) <- deepSplitProductType_maybe (ae_fam_envs env) $ idType id
  = extendAnalEnv NotTopLevel env id (cprProdSig (dataConRepArity dc))

  | otherwise
  = env

extendEnvForProdAlt :: AnalEnv -> CoreExpr -> Id -> DataCon -> [Var] -> AnalEnv
-- See Note [CPR in a product case alternative]
extendEnvForProdAlt env scrut case_bndr dc bndrs
  = foldl do_con_arg env1 ids_w_strs
  where
    env1 = extendAnalEnv NotTopLevel env case_bndr case_bndr_sig

    ids_w_strs    = filter isId bndrs `zip` dataConRepStrictness dc
    case_bndr_sig = cprProdSig (dataConRepArity dc)
    fam_envs      = ae_fam_envs env

    do_con_arg env (id, str)
       | let is_strict = isStrictDmd (idDemandInfo id) || isMarkedStrict str
       , ae_virgin env || (is_var_scrut && is_strict)  -- See Note [CPR in a product case alternative]
       , Just (dc,_,_,_) <- deepSplitProductType_maybe fam_envs $ idType id
       = extendAnalEnv NotTopLevel env id (cprProdSig (dataConRepArity dc))
       | otherwise
       = env

    is_var_scrut = is_var scrut
    is_var (Cast e _) = is_var e
    is_var (Var v)    = isLocalId v
    is_var _          = False

addDataConStrictness :: DataCon -> [Demand] -> [Demand]
-- See Note [Add demands for strict constructors]
addDataConStrictness con ds
  = ASSERT2( equalLength strs ds, ppr con $$ ppr strs $$ ppr ds )
    zipWith add ds strs
  where
    strs = dataConRepStrictness con
    add dmd str | isMarkedStrict str
                , not (isAbsDmd dmd) = dmd `bothDmd` seqDmd
                | otherwise          = dmd

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
-- See Note [Trimming a demand to a type] in Demand.hs
findBndrDmd env arg_of_dfun dmd_ty id
  = (dmd_ty', dmd')
  where
    dmd' = killUsageDemand (ae_dflags env) $
           strictify $
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

set_idStrictness :: AnalEnv -> Id -> StrictSig -> Id
set_idStrictness env id sig
  = setIdStrictness id (killUsageSig (ae_dflags env) sig)

dumpStrSig :: CoreProgram -> SDoc
dumpStrSig binds = vcat (map printId ids)
  where
  ids = sortBy (stableNameCmp `on` getName) (concatMap getIds binds)
  getIds (NonRec i _) = [ i ]
  getIds (Rec bs)     = map fst bs
  printId id | isExportedId id = ppr id <> colon <+> pprIfaceStrictSig (idStrictness id)
             | otherwise       = empty

{- Note [CPR in a product case alternative]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a case alternative for a product type, we want to give some of the
binders the CPR property.  Specifically

 * The case binder; inside the alternative, the case binder always has
   the CPR property, meaning that a case on it will successfully cancel.
   Example:
        f True  x = case x of y { I# x' -> if x' ==# 3
                                           then y
                                           else I# 8 }
        f False x = I# 3

   By giving 'y' the CPR property, we ensure that 'f' does too, so we get
        f b x = case fw b x of { r -> I# r }
        fw True  x = case x of y { I# x' -> if x' ==# 3 then x' else 8 }
        fw False x = 3

   Of course there is the usual risk of re-boxing: we have 'x' available
   boxed and unboxed, but we return the unboxed version for the wrapper to
   box.  If the wrapper doesn't cancel with its caller, we'll end up
   re-boxing something that we did have available in boxed form.

 * Any strict binders with product type, can use
   Note [Initial CPR for strict binders].  But we can go a little
   further. Consider

      data T = MkT !Int Int

      f2 (MkT x y) | y>0       = f2 (MkT x (y-1))
                   | otherwise = x

   For $wf2 we are going to unbox the MkT *and*, since it is strict, the
   first agument of the MkT; see Note [Add demands for strict constructors].
   But then we don't want box it up again when returning it!  We want
   'f2' to have the CPR property, so we give 'x' the CPR property.

 * It's a bit delicate because if this case is scrutinising something other
   than an argument the original function, we really don't have the unboxed
   version available.  E.g
      g v = case foo v of
              MkT x y | y>0       -> ...
                      | otherwise -> x
   Here we don't have the unboxed 'x' available.  Hence the
   is_var_scrut test when making use of the strictness annoatation.
   Slightly ad-hoc, because even if the scrutinee *is* a variable it
   might not be a onre of the arguments to the original function, or a
   sub-component thereof.  But it's simple, and nothing terrible
   happens if we get it wrong.  e.g. Trac #10694.

Note [Add demands for strict constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this program (due to Roman):

    data X a = X !a

    foo :: X Int -> Int -> Int
    foo (X a) n = go 0
     where
       go i | i < n     = a + go (i+1)
            | otherwise = 0

We want the worker for 'foo' too look like this:

    $wfoo :: Int# -> Int# -> Int#

with the first argument unboxed, so that it is not eval'd each time
around the 'go' loop (which would otherwise happen, since 'foo' is not
strict in 'a').  It is sound for the wrapper to pass an unboxed arg
because X is strict, so its argument must be evaluated.  And if we
*don't* pass an unboxed argument, we can't even repair it by adding a
`seq` thus:

    foo (X a) n = a `seq` go 0

because the seq is discarded (very early) since X is strict!

We achieve the effect using addDataConStrictness.  It is called at a
case expression, such as the pattern match on (X a) in the example
above.  After computing how 'a' is used in the alternatives, we add an
extra 'seqDmd' to it.  The case alternative isn't itself strict in the
sub-components, but simply evaluating the scrutinee to HNF does force
those sub-components.

If the argument is not used at all in the alternative (i.e. it is
Absent), then *don't* add a 'seqDmd'.  If we do, it makes it look used
and hence it'll be passed to the worker when it doesn't need to be.
Hence the isAbsDmd test in addDataConStrictness.

There is the usual danger of reboxing, which as usual we ignore. But
if X is monomorphic, and has an UNPACK pragma, then this optimisation
is even more important.  We don't want the wrapper to rebox an unboxed
argument, and pass an Int to $wfoo!


Note [Initial CPR for strict binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CPR is initialized for a lambda binder in an optimistic manner, i.e,
if the binder is used strictly and at least some of its components as
a product are used, which is checked by the value of the absence
demand.

If the binder is marked demanded with a strict demand, then give it a
CPR signature. Here's a concrete example ('f1' in test T10482a),
assuming h is strict:

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

Note that

  * We only want to do this for something that definitely
    has product type, else we may get over-optimistic CPR results
    (e.g. from \x -> x!).

  * See Note [CPR examples]

Note [CPR examples]
~~~~~~~~~~~~~~~~~~~~
Here are some examples (stranal/should_compile/T10482a) of the
usefulness of Note [CPR in a product case alternative].  The main
point: all of these functions can have the CPR property.

    ------- f1 -----------
    -- x is used strictly by h, so it'll be available
    -- unboxed before it is returned in the True branch

    f1 :: Int -> Int
    f1 x = case h x x of
            True  -> x
            False -> f1 (x-1)


    ------- f2 -----------
    -- x is a strict field of MkT2, so we'll pass it unboxed
    -- to $wf2, so it's available unboxed.  This depends on
    -- the case expression analysing (a subcomponent of) one
    -- of the original arguments to the function, so it's
    -- a bit more delicate.

    data T2 = MkT2 !Int Int

    f2 :: T2 -> Int
    f2 (MkT2 x y) | y>0       = f2 (MkT2 x (y-1))
                  | otherwise = x


    ------- f3 -----------
    -- h is strict in x, so x will be unboxed before it
    -- is rerturned in the otherwise case.

    data T3 = MkT3 Int Int

    f1 :: T3 -> Int
    f1 (MkT3 x y) | h x y     = f3 (MkT3 x (y-1))
                  | otherwise = x


    ------- f4 -----------
    -- Just like f2, but MkT4 can't unbox its strict
    -- argument automatically, as f2 can

    data family Foo a
    newtype instance Foo Int = Foo Int

    data T4 a = MkT4 !(Foo a) Int

    f4 :: T4 Int -> Int
    f4 (MkT4 x@(Foo v) y) | y>0       = f4 (MkT4 x (y-1))
                          | otherwise = v


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
  \y [Demand=1*U] let x = y in x + x
to
  \y [Demand=1*U] y + y
which is quite a lie.

The once-used information is (currently) only used by the code
generator, though.  So:

 * We zap the used-once info in the worker-wrapper;
   see Note [Zapping Used Once info in WorkWrap] in WorkWrap. If it's
   not reliable, it's better not to have it at all.

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
