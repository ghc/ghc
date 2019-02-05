{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


                        -----------------
                        A demand analysis
                        -----------------
-}

{-# LANGUAGE CPP #-}

module CprAnal ( cprAnalProgram ) where

#include "HsVersions.h"

import GhcPrelude

import DynFlags
import WwLib            ( deepSplitProductType_maybe )
import Demand   -- All of it
import CoreSyn
import CoreSeq          ( seqBinds )
import Outputable
import VarEnv
import BasicTypes
import Data.List
import DataCon
import Id
import CoreUtils        ( exprIsHNF )
import TyCon
import Type
import FamInstEnv
import Util
import Maybes           ( isJust )
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

cprAnalProgram :: DynFlags -> FamInstEnvs -> CoreProgram -> IO CoreProgram
cprAnalProgram dflags fam_envs binds
  = do {
        let { binds_plus_cpr = do_prog binds } ;
        dumpIfSet_dyn dflags Opt_D_dump_str_signatures
                      "Strictness signatures after CPR analsis" $
            dumpStrSig binds_plus_cpr ;
        -- See Note [Stamp out space leaks in demand analysis] in DmdAnal.hs
        seqBinds binds_plus_cpr `seq` return binds_plus_cpr
    }
  where
    do_prog :: CoreProgram -> CoreProgram
    do_prog binds = snd $ mapAccumL cprAnalTopBind (emptyAnalEnv dflags fam_envs) binds

-- Analyse a (group of) top-level binding(s)
cprAnalTopBind :: AnalEnv
               -> CoreBind
               -> (AnalEnv, CoreBind)
cprAnalTopBind env (NonRec id rhs)
  = (extendAnalEnv TopLevel env id' (get_idCprInfo id'), NonRec id' rhs')
  where
    (id', rhs') = cprAnalRhsLetDown TopLevel env 0 id rhs

cprAnalTopBind env (Rec pairs)
  = (env', Rec pairs')
  where
    (env', pairs') = cprFix TopLevel env 0 pairs

{-
************************************************************************
*                                                                      *
\subsection{The analyser itself}
*                                                                      *
************************************************************************
-}

-- Main CPR Analysis machinery
cprAnal, cprAnal'
  :: AnalEnv
  -> Arity               -- ^ number of arguments the expression is applied to
  -> CoreExpr            -- ^ expression to be denoted by a 'CprType'
  -> (CprType, CoreExpr) -- ^ the updated expression and its 'CprType'

cprAnal env n e = -- pprTrace "cprAnal" (ppr n <+> ppr e) $
                  cprAnal' env n e

cprAnal' _ _ (Lit lit)     = (topCprType, Lit lit)
cprAnal' _ _ (Type ty)     = (topCprType, Type ty)      -- Doesn't happen, in fact
cprAnal' _ _ (Coercion co) = (topCprType, Coercion co)

cprAnal' env n (Var var)   = (cprTransform env var n, Var var)

cprAnal' env n (Cast e co)
  = (cpr_ty, Cast e' co)
  where
    (cpr_ty, e') = cprAnal env n e

cprAnal' env n (Tick t e)
  = (cpr_ty, Tick t e')
  where
    (cpr_ty, e') = cprAnal env n e

cprAnal' env n (App fun (Type ty))
  = (fun_ty, App fun' (Type ty))
  where
    (fun_ty, fun') = cprAnal env n fun

cprAnal' env n (App fun arg)
  = (res_ty, App fun' arg')
  where
    (fun_ty, fun') = cprAnal env (n+1) fun
    -- In contrast to DmdAnal, there is no useful (non-nested) CPR info to be
    -- had by looking into the CprType of arg.
    (_, arg')      = cprAnal env 0     arg
    res_ty         = applyCprTy fun_ty

-- this is an anonymous lambda, since @cprAnalRhsLetDown@ uses @collectBinders@
cprAnal' env n (Lam var body)
  | isTyVar var
  , (body_ty, body') <- cprAnal env n body
  = (body_ty, Lam var body')
  | otherwise
  = (lam_ty', Lam var body')
  where
    body_n           = max 0 (n-1)
    env'             = extendSigsWithLam env var
    (body_ty, body') = cprAnal env' body_n body
    lam_ty           = abstractCprTy body_ty
    is_unsat         = n == 0
    lam_ty'
      | is_unsat = topCprType
      | otherwise = lam_ty

cprAnal' env n (Case scrut case_bndr ty alts)
  = (res_ty, Case scrut' case_bndr ty alts')
  where
    (_, scrut')      = cprAnal env 0 scrut
    (alt_tys, alts') = mapAndUnzip (cprAnalAlt env scrut case_bndr n) alts
    res_ty           = foldl' lubCprType botCprType alt_tys
--    pprTrace "cprAnal:Case" (vcat [ text "scrut" <+> ppr scrut
--                                  , text "dmd" <+> ppr dmd
--                                  , text "case_bndr_dmd" <+> ppr (idDemandInfo case_bndr')
--                                  , text "scrut_dmd" <+> ppr scrut_dmd
--                                  , text "scrut_ty" <+> ppr scrut_ty
--                                  , text "alt_ty" <+> ppr alt_ty2
--                                  , text "res_ty" <+> ppr res_ty ]) $

cprAnal' env n (Let (NonRec id rhs) body)
  = (body_ty, Let (NonRec id' rhs') body')
  where
    (id', rhs')      = cprAnalRhsLetDown NotTopLevel env n id rhs
    env'             = extendAnalEnv NotTopLevel env id' (get_idCprInfo id')
    (body_ty, body') = cprAnal env' n body

cprAnal' env n (Let (Rec pairs) body)
  = let
        (env', pairs')   = cprFix NotTopLevel env n pairs
        (body_ty, body') = cprAnal env' n body
    in
    body_ty `seq`
    (body_ty,  Let (Rec pairs') body')

cprAnalAlt
  :: AnalEnv
  -> CoreExpr -- ^ scrutinee
  -> Id       -- ^ case binder
  -> Arity    -- ^ incoming arity
  -> Alt Var  -- ^ current alternative
  -> (CprType, Alt Var)
cprAnalAlt env scrut case_bndr n (con@(DataAlt dc),bndrs,rhs)
  -- See 'extendEnvForDataAlt' and Note [CPR in a DataAlt case alternative]
  = (rhs_ty, (con, bndrs, rhs'))
  where
    env_alt        = extendEnvForDataAlt env scrut case_bndr dc bndrs
    (rhs_ty, rhs') = cprAnal env_alt n rhs
cprAnalAlt env _ _ n (con,bndrs,rhs)
  = (rhs_ty, (con, bndrs, rhs'))
  where
    (rhs_ty, rhs') = cprAnal env n rhs

{-
************************************************************************
*                                                                      *
                    Demand transformer
*                                                                      *
************************************************************************
-}

cprTransformSig :: CprType -> Arity -> CprType
cprTransformSig ty arty
  -- We are only interested in CPR here, so this is OK. TODO: Clean up
  | arty >= ct_arty ty = ty
  | otherwise = topCprType

arityToCallDemand :: Arity -> CleanDemand
arityToCallDemand n = iterate mkCallDmd (strictenDmd topDmd) !! n

cprTransform :: AnalEnv         -- The strictness environment
             -> Id              -- The function
             -> Arity           -- The demand on the function
             -> CprType         -- The demand type of the function in this context
        -- Returned DmdEnv includes the demand on
        -- this function plus demand on its free variables

cprTransform env var arty
  | isDataConWorkId var                          -- Data constructor
  -- TODO
  = dmdTypeToCprType (dmdTransformDataConSig (idArity var) (idStrictness var) (arityToCallDemand arty))
  | gopt Opt_DmdTxDictSel (ae_dflags env),
    Just _ <- isClassOpId_maybe var -- Dictionary component selector
  = -- dmdTransformDictSelSig (idStrictness var) dmd
    topCprType -- dmdTransformDictSelSig always returns a topRes

  | isGlobalId var                               -- Imported function
  , let res = cprTransformSig (get_idCprInfo var) arty
  = -- pprTrace "cprTransform" (vcat [ppr var, ppr (idStrictness var), ppr dmd, ppr res])
    res

  | Just sig <- lookupSigEnv env var  -- Local letrec bound thing
  , let fn_ty = cprTransformSig sig arty
  = -- pprTrace "cprTransform" (vcat [ppr var, ppr sig, ppr arty, ppr fn_ty]) $
    fn_ty

  | otherwise                                    -- Local non-letrec-bound thing
  = topCprType

{-
************************************************************************
*                                                                      *
\subsection{Bindings}
*                                                                      *
************************************************************************
-}

-- Recursive bindings
cprFix :: TopLevelFlag
       -> AnalEnv                            -- Does not include bindings for this binding
       -> Arity
       -> [(Id,CoreExpr)]
       -> (AnalEnv, [(Id,CoreExpr)]) -- Binders annotated with stricness info

cprFix top_lvl env let_arty orig_pairs
  = loop 1 initial_pairs
  where
    -- See Note [Initialising strictness] in DmdAnal.hs
    initial_pairs | ae_virgin env = [(setIdCprInfo id botCpr, rhs) | (id, rhs) <- orig_pairs ]
                  | otherwise     = orig_pairs

    -- If fixed-point iteration does not yield a result we use this instead
    -- See Note [Safe abortion in the fixed-point iteration]
    abort :: (AnalEnv, [(Id,CoreExpr)])
    abort = (env, zapped_pairs)
      where pairs' = step True (zapIdCprInfo orig_pairs)
            zapped_pairs = zapIdCprInfo pairs'

    -- The fixed-point varies the idCprInfo field of the binders, and terminates if that
    -- annotation does not change any more.
    loop :: Int -> [(Id,CoreExpr)] -> (AnalEnv, [(Id,CoreExpr)])
    loop n pairs
      | found_fixpoint = (final_anal_env, pairs')
      | n == 10        = abort
      | otherwise      = loop (n+1) pairs'
      where
        found_fixpoint    = map (idCprInfo . fst) pairs' == map (idCprInfo . fst) pairs
        first_round       = n == 1
        pairs'            = step first_round pairs
        final_anal_env    = extendAnalEnvs top_lvl env (map fst pairs')

    step :: Bool -> [(Id, CoreExpr)] -> [(Id, CoreExpr)]
    step first_round pairs = pairs'
      where
        -- In all but the first iteration, delete the virgin flag
        start_env | first_round = env
                  | otherwise   = nonVirgin env

        start = extendAnalEnvs top_lvl start_env (map fst pairs)

        (_, pairs') = mapAccumL my_downRhs start pairs
                -- mapAccumL: Use the new signature to do the next pair
                -- The occurrence analyser has arranged them in a good order
                -- so this can significantly reduce the number of iterations needed

        my_downRhs env (id,rhs)
          = (env', (id', rhs'))
          where
            (id', rhs') = cprAnalRhsLetDown top_lvl env let_arty id rhs
            env'        = extendAnalEnv top_lvl env id (get_idCprInfo id')


    zapIdCprInfo :: [(Id, CoreExpr)] -> [(Id, CoreExpr)]
    zapIdCprInfo pairs = [(setIdCprInfo id topCpr, rhs) | (id, rhs) <- pairs ]

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
environment, which effectively assigns them 'nopSig' (see "getCprType")

-}

-- Trivial RHS
-- See Note [Demand analysis for trivial right-hand sides]
cprAnalTrivialRhs ::
    AnalEnv -> Id -> CoreExpr -> Var ->
    (Id, CoreExpr)
cprAnalTrivialRhs env id rhs fn
  = (set_idCprInfo id fn_str, rhs)
  where
    fn_str = getCprType env fn

-- Let bindings are processed in the following way (cf. LetDown from the paper
-- “Higher-Order Cardinality Analysis”):
--
--  * assuming a demand of <L,U>
--  * looking at the definition
--  * determining a strictness signature
cprAnalRhsLetDown :: TopLevelFlag
           -> AnalEnv -> Arity
           -> Id -> CoreExpr
           -> (Id, CoreExpr)
-- Process the RHS of the binding, add the strictness signature
-- to the Id, and augment the environment with the signature as well.
cprAnalRhsLetDown top_lvl env let_arty id rhs
  | Just fn <- unpackTrivial rhs   -- See Note [Demand analysis for trivial right-hand sides]
  = cprAnalTrivialRhs env id rhs fn

  | otherwise
  = (id', mkLams bndrs body')
  where
    (bndrs, body, body_arty)
       = case isJoinId_maybe id of
           Just join_arity  -- See Note [Demand analysis for join points]
                   | (bndrs, body) <- collectNBinders join_arity rhs
                   -> (bndrs, body, let_arty)

           Nothing | (bndrs, body) <- collectBinders rhs
                   -> (bndrs, body, 0)

    env_body         = foldl' extendSigsWithLam env bndrs
    (body_ty, body') = cprAnal env_body body_arty body
    -- zap possible deep CPR info
    body_ty'         = removeCprTyArgs body_ty
    -- account for lambdas
    n_val_lams       = length (filter isId bndrs)
    rhs_ty           = iterate abstractCprTy body_ty' !! n_val_lams
    -- possibly trim thunk CPR info
    sig_ty           = trimCprTy trim_all trim_sums rhs_ty
    id'              = set_idCprInfo id sig_ty

    -- See Note [CPR for thunks]
    -- See Note [CPR for sum types]
    trim_all  = is_thunk && not_strict
    trim_sums = not (isTopLevel top_lvl)
    is_thunk = not (exprIsHNF rhs) && not (isJoinId id)
    not_strict = not (isStrictDmd (idDemandInfo id))

unpackTrivial :: CoreExpr -> Maybe Id
-- Returns (Just v) if the arg is really equal to v, modulo
-- casts, type applications etc
-- See Note [Demand analysis for trivial right-hand sides]
unpackTrivial (Var v)                 = Just v
unpackTrivial (Cast e _)              = unpackTrivial e
unpackTrivial (Lam v e) | isTyVar v   = unpackTrivial e
unpackTrivial (App e a) | isTypeArg a = unpackTrivial e
unpackTrivial _                       = Nothing

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

Another win for join points!  Trac #13543.

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

************************************************************************
*                                                                      *
\subsection{Strictness signatures and types}
*                                                                      *
************************************************************************
-}

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

************************************************************************
*                                                                      *
\subsection{Strictness signatures}
*                                                                      *
************************************************************************
-}

-- | The abstract domain $A_t$ from the original 'CPR for Haskell' paper.
data CprType
  = CprType
  { ct_arty :: !Arity     -- ^ Number of arguments the denoted expression eats
                          --   before returning the 'ct_cpr'
  , ct_cpr  :: !CPRResult -- ^ 'CPRResult' eventually unleashed when applied to
                          --   'ct_arty' arguments
  }

topCprType :: CprType
topCprType = CprType 0 topCpr

botCprType :: CprType
botCprType = CprType 0 botCpr -- TODO: Figure out if arity 0 does what we want

prodCprType :: Arity -> CprType
prodCprType _con_arty = CprType 0 prodCpr

sumCprType :: ConTag -> CprType
sumCprType con_tag = CprType 0 (sumCpr con_tag)

lubCprType :: CprType -> CprType -> CprType
lubCprType ty1@(CprType n1 cpr1) ty2@(CprType n2 cpr2)
  -- The arity of bottom CPR types can be extended arbitrarily.
  | isBotCpr cpr1 && n1 <= n2 = ty2
  | isBotCpr cpr2 && n2 <= n1 = ty1
  -- There might be non-bottom CPR types with mismatching arities.
  -- Consider test DmdAnalGADTs. We want to return top in these cases.
  | n1 == n2                  = CprType n1 (lubCPR cpr1 cpr2)
  | otherwise                 = topCprType

applyCprTy :: CprType -> CprType
applyCprTy (CprType n res)
  | n > 0        = CprType (n-1) res
  | isBotCpr res = botCprType
  | otherwise    = topCprType

abstractCprTy :: CprType -> CprType
abstractCprTy (CprType n res)
  | isTopCpr res = topCprType
  | otherwise    = CprType (n+1) res

removeCprTyArgs :: CprType -> CprType
removeCprTyArgs ty@(CprType 0 _) = ty
removeCprTyArgs _                = topCprType

trimCprTy :: Bool -> Bool -> CprType -> CprType
trimCprTy trim_all trim_sums (CprType arty res) = CprType arty (trimCpr trim_all trim_sums res)

dmdTypeToCprType :: DmdType -> CprType
dmdTypeToCprType (DmdType _ args dr) = CprType (length args) (dmdResToCpr dr)

instance Outputable CprType where
  ppr (CprType arty res) = ppr arty <> ppr res

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

type SigEnv = VarEnv CprType

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
extendSigEnvs _top_lvl sigs vars
  = extendVarEnvList sigs [ (var, get_idCprInfo var) | var <- vars]

extendAnalEnv :: TopLevelFlag -> AnalEnv -> Id -> CprType -> AnalEnv
extendAnalEnv _top_lvl env var sig
  = env { ae_sigs = extendSigEnv _top_lvl (ae_sigs env) var sig }

extendSigEnv :: TopLevelFlag -> SigEnv -> Id -> CprType -> SigEnv
extendSigEnv _top_lvl sigs var sig = extendVarEnv sigs var sig

lookupSigEnv :: AnalEnv -> Id -> Maybe CprType
lookupSigEnv env id = lookupVarEnv (ae_sigs env) id

getCprType :: AnalEnv -> Id -> CprType
getCprType env fn
  | isGlobalId fn                   = get_idCprInfo fn
  | Just sig <- lookupSigEnv env fn = sig
  | otherwise                       = topCprType

nonVirgin :: AnalEnv -> AnalEnv
nonVirgin env = env { ae_virgin = False }

extendSigsWithLam :: AnalEnv -> Id -> AnalEnv
-- Extend the AnalEnv when we meet a lambda binder
extendSigsWithLam env id
  | isId id
  , isStrictDmd (idDemandInfo id)
       -- See Note [Initial CPR for strict binders]
  , Just (dc,_,_,_) <- deepSplitProductType_maybe (ae_fam_envs env) $ idType id
  = extendAnalEnv NotTopLevel env id (prodCprType (dataConRepArity dc))
  | otherwise
  = env

extendEnvForDataAlt :: AnalEnv -> CoreExpr -> Id -> DataCon -> [Var] -> AnalEnv
-- See Note [CPR in a DataAlt case alternative]
extendEnvForDataAlt env scrut case_bndr dc bndrs
  = foldl' do_con_arg env' ids_w_strs
  where
    env' = extendAnalEnv NotTopLevel env case_bndr case_bndr_sig

    ids_w_strs    = filter isId bndrs `zip` dataConRepStrictness dc

    tycon          = dataConTyCon dc
    is_product     = isJust (isDataProductTyCon_maybe tycon)
    is_sum         = isJust (isDataSumTyCon_maybe tycon)
    case_bndr_sig
      | is_product = prodCprType (dataConRepArity dc)
      | is_sum     = sumCprType  (dataConTag dc)
      -- Any of the constructors had existentials. This is a little too
      -- conservative (after all, we only care about the particular data con),
      -- but there is no easy way to write is_sum and this won't happen much.
      | otherwise  = topCprType

    -- We could have much deeper CPR info here with Nested CPR, which could
    -- propagate available unboxed things from the scrutinee, getting rid of
    -- the is_var_scrut heuristic. See Note [CPR in a DataAlt case alternative].
    -- Giving strict binders the CPR property only makes sense for products, as
    -- the arguments in Note [Initial CPR for strict binders] don't apply to
    -- sums (yet); we lack WW for strict binders of sum type.
    do_con_arg env (id, str)
       | let is_strict = isStrictDmd (idDemandInfo id) || isMarkedStrict str
       , is_var_scrut && is_strict
       , let fam_envs = ae_fam_envs env
       , Just (dc,_,_,_) <- deepSplitProductType_maybe fam_envs $ idType id
       = extendAnalEnv NotTopLevel env id (prodCprType (dataConRepArity dc))
       | otherwise
       = env

    is_var_scrut = is_var scrut
    is_var (Cast e _) = is_var e
    is_var (Var v)    = isLocalId v
    is_var _          = False

set_idCprInfo :: Id -> CprType -> Id
set_idCprInfo id ty = setIdCprInfo id (ct_cpr ty)
-- TODO: Check that `ct_arty res == dmdTypeDepth == idArity`

get_idCprInfo :: Id -> CprType
get_idCprInfo id = CprType (idArity id) (idCprInfo id)

dumpStrSig :: CoreProgram -> SDoc
dumpStrSig binds = vcat (map printId ids)
  where
  ids = sortBy (stableNameCmp `on` getName) (concatMap getIds binds)
  getIds (NonRec i _) = [ i ]
  getIds (Rec bs)     = map fst bs
  printId id | isExportedId id = ppr id <> colon <+> pprIfaceStrictSig (idStrictness id)
             | otherwise       = empty

{- Note [CPR in a DataAlt case alternative]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a case alternative, we want to give some of the binders the CPR property.
Specifically

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
   first argument of the MkT; see Note [Add demands for strict constructors].
   But then we don't want box it up again when returning it!  We want
   'f2' to have the CPR property, so we give 'x' the CPR property.

 * It's a bit delicate because if this case is scrutinising something other
   than an argument the original function, we really don't have the unboxed
   version available.  E.g
      g v = case foo v of
              MkT x y | y>0       -> ...
                      | otherwise -> x
   Here we don't have the unboxed 'x' available.  Hence the
   is_var_scrut test when making use of the strictness annotation.
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
usefulness of Note [CPR in a DataAlt case alternative].  The main
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
-}
