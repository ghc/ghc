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
        -- See Note [Stamp out space leaks in demand analysis]
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
        -- See Note [Initial CPR for strict binders]

cprAnalTopBind env (Rec pairs)
  = (env', Rec pairs')
  where
    (env', pairs') = cprFix TopLevel env 0 pairs
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
DynFlags (cf Trac #9675 and #13426).

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

-- Main Demand Analsysis machinery
cprAnal, cprAnal' :: AnalEnv
        -> Arity
        -> CoreExpr -> (CprType, CoreExpr)

-- The CleanDemand is always strict and not absent
--    See Note [Ensure demand is strict]

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

cprAnal' env n (Case scrut case_bndr ty [(DataAlt dc, bndrs, rhs)])
  -- Only one alternative with a product constructor
  | let tycon = dataConTyCon dc
  , isJust (isDataProductTyCon_maybe tycon)
  , Just rec_tc' <- checkRecTc (ae_rec_tc env) tycon
  = let
        env_w_tc                 = env { ae_rec_tc = rec_tc' }
        env_alt                  = extendEnvForProdAlt env_w_tc scrut case_bndr dc bndrs
        (rhs_ty, rhs')           = cprAnal env_alt n rhs
        -- Compute demand on the scrutinee
        -- See Note [Demand on scrutinee of a product case]
        (_, scrut') = cprAnal env 0 scrut
        res_ty             = rhs_ty
    in
--    pprTrace "cprAnal:Case1" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "dmd" <+> ppr dmd
--                                   , text "case_bndr_dmd" <+> ppr (idDemandInfo case_bndr')
--                                   , text "scrut_dmd" <+> ppr scrut_dmd
--                                   , text "scrut_ty" <+> ppr scrut_ty
--                                   , text "alt_ty" <+> ppr alt_ty2
--                                   , text "res_ty" <+> ppr res_ty ]) $
    (res_ty, Case scrut' case_bndr ty [(DataAlt dc, bndrs, rhs')])

cprAnal' env n (Case scrut case_bndr ty alts)
  = let      -- Case expression with multiple alternatives
        (alt_tys, alts') = mapAndUnzip (cprAnalAlt env n case_bndr) alts
        (_, scrut')      = cprAnal env 0 scrut
        res_ty           = foldr lubCprType botCprType alt_tys
                               -- NB: Base case is botCprType, for empty case alternatives
                               --     This is a unit for lubCprType, and the right result
                               --     when there really are no alternatives
    in
--    pprTrace "cprAnal:Case2" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "scrut_ty" <+> ppr scrut_ty
--                                   , text "alt_tys" <+> ppr alt_tys
--                                   , text "alt_ty" <+> ppr alt_ty
--                                   , text "res_ty" <+> ppr res_ty ]) $
    (res_ty, Case scrut' case_bndr ty alts')

cprAnal' env n (Let (NonRec id rhs) body)
  = (body_ty, Let (NonRec id1 rhs') body')
  where
    (id1, rhs')      = cprAnalRhsLetDown NotTopLevel env n id rhs
    env1             = extendAnalEnv NotTopLevel env id1 (get_idCprInfo id1)
    (body_ty, body') = cprAnal env1 n body

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

cprAnal' env n (Let (Rec pairs) body)
  = let
        (env', pairs')   = cprFix NotTopLevel env n pairs
        (body_ty, body') = cprAnal env' n body
    in
    body_ty `seq`
    (body_ty,  Let (Rec pairs') body')

cprAnalAlt :: AnalEnv -> Arity -> Id -> Alt Var -> (CprType, Alt Var)
cprAnalAlt env n _case_bndr (con,bndrs,rhs)
  = (rhs_ty, (con, bndrs, rhs'))
  where
    -- Assymetry with the product case, see
    -- Note [CPR in a product case alternative]
    (rhs_ty, rhs') = cprAnal env n rhs


{- Note [IO hack in the demand analyser]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's a hack here for I/O operations.  Consider

     case foo x s of { (# s', r #) -> y }

Is this strict in 'y'? Often not! If foo x s performs some observable action
(including raising an exception with raiseIO#, modifying a mutable variable, or
even ending the program normally), then we must not force 'y' (which may fail
to terminate) until we have performed foo x s.

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
  - If it is a simple, terminating one like getMaskingState,
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
    -- See Note [Initialising strictness]
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

-- Let bindings can be processed in the following way (cf. LetDown from the
-- paper “Higher-Order Cardinality Analysis”):
--
--  * assuming a demand of <L,U>
--  * looking at the definition
--  * determining a strictness signature
--
-- This is the LetDown rule in the paper “Higher-Order Cardinality Analysis”.
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
        -- See Note [NOINLINE and strictness]

    trim_all  = is_thunk && not_strict
    trim_sums = not (isTopLevel top_lvl) -- See Note [CPR for sum types]

    -- See Note [CPR for thunks]
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
by cprAnalTopBind.


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
demand put on them (topDmd), and add that to the "lazy_fv" returned by "cprFix".


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

extendEnvForProdAlt :: AnalEnv -> CoreExpr -> Id -> DataCon -> [Var] -> AnalEnv
-- See Note [CPR in a product case alternative]
extendEnvForProdAlt env scrut case_bndr dc bndrs
  = foldl' do_con_arg env1 ids_w_strs
  where
    env1 = extendAnalEnv NotTopLevel env case_bndr case_bndr_sig

    ids_w_strs    = filter isId bndrs `zip` dataConRepStrictness dc
    case_bndr_sig = prodCprType (dataConRepArity dc)
    fam_envs      = ae_fam_envs env

    -- This could go away with nested CPR, where we have proper termination
    -- information.
    do_con_arg env (id, str)
       | let is_strict = isStrictDmd (idDemandInfo id) || isMarkedStrict str
       , ae_virgin env || (is_var_scrut && is_strict)  -- See Note [CPR in a product case alternative]
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
