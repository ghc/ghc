%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

			-----------------
			A demand analysis
			-----------------

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}

module DmdAnal ( dmdAnalProgram ) where

#include "HsVersions.h"

import Var		( isTyVar )
import DynFlags
import WwLib            ( deepSplitProductType_maybe )
import Demand	-- All of it
import CoreSyn
import Outputable
import VarEnv
import BasicTypes	
import FastString
import Data.List
import DataCon
import Id
import CoreUtils	( exprIsHNF, exprType, exprIsTrivial )
-- import PprCore	
import TyCon
import Type		( eqType )
-- import Pair
-- import Coercion         ( coercionKind )
import Util
import Maybes		( isJust )
import TysWiredIn	( unboxedPairDataCon )
import TysPrim		( realWorldStatePrimTy )
import ErrUtils         ( dumpIfSet_dyn )
import Name             ( getName, stableNameCmp )
import Data.Function    ( on )
\end{code}

%************************************************************************
%*									*
\subsection{Top level stuff}
%*									*
%************************************************************************

\begin{code}
dmdAnalProgram :: DynFlags -> CoreProgram -> IO CoreProgram
dmdAnalProgram dflags binds
  = do {
	let { binds_plus_dmds = do_prog binds } ;
        dumpIfSet_dyn dflags Opt_D_dump_strsigs "Strictness signatures" $
            dumpStrSig binds_plus_dmds ;
	return binds_plus_dmds
    }
  where
    do_prog :: CoreProgram -> CoreProgram
    do_prog binds = snd $ mapAccumL dmdAnalTopBind (emptyAnalEnv dflags) binds

-- Analyse a (group of) top-level binding(s)
dmdAnalTopBind :: AnalEnv
	       -> CoreBind 
	       -> (AnalEnv, CoreBind)
dmdAnalTopBind sigs (NonRec id rhs)
  = (extendAnalEnv TopLevel sigs id sig, NonRec id2 rhs2)
  where
    (  _, _, _,   rhs1) = dmdAnalRhs TopLevel Nothing sigs             id rhs
    (sig, _, id2, rhs2) = dmdAnalRhs TopLevel Nothing (nonVirgin sigs) id rhs1
    	-- Do two passes to improve CPR information
    	-- See comments with ignore_cpr_info in mk_sig_ty
    	-- and with extendSigsWithLam

dmdAnalTopBind sigs (Rec pairs)
  = (sigs', Rec pairs')
  where
    (sigs', _, pairs')  = dmdFix TopLevel sigs pairs
		-- We get two iterations automatically
		-- c.f. the NonRec case above
\end{code}

%************************************************************************
%*									*
\subsection{The analyser itself}	
%*									*
%************************************************************************

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

\begin{code}
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
            -> Demand 	-- This one takes a *Demand*
            -> CoreExpr -> (DmdType, CoreExpr)
dmdAnalStar env dmd e 
  | (cd, defer_and_use) <- toCleanDmd dmd
  , (dmd_ty, e')        <- dmdAnal env cd e
  = (postProcessDmdTypeM defer_and_use dmd_ty, e')

-- Main Demand Analsysis machinery
dmdAnal :: AnalEnv
        -> CleanDemand 	       -- The main one takes a *CleanDemand*
        -> CoreExpr -> (DmdType, CoreExpr)

-- The CleanDemand is always strict and not absent
--    See Note [Ensure demand is strict]

dmdAnal _ _ (Lit lit)     = (nopDmdType, Lit lit)
dmdAnal _ _ (Type ty)     = (nopDmdType, Type ty)	-- Doesn't happen, in fact
dmdAnal _ _ (Coercion co) = (nopDmdType, Coercion co)

dmdAnal env dmd (Var var)
  = (dmdTransform env var dmd, Var var)

dmdAnal env dmd (Cast e co)
  = (dmd_ty, Cast e' co)
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

dmdAnal env dmd (Tick t e)
  = (dmd_ty, Tick t e')
  where
    (dmd_ty, e') = dmdAnal env dmd e

dmdAnal env dmd (App fun (Type ty))
  = (fun_ty, App fun' (Type ty))
  where
    (fun_ty, fun') = dmdAnal env dmd fun

dmdAnal sigs dmd (App fun (Coercion co))
  = (fun_ty, App fun' (Coercion co))
  where
    (fun_ty, fun') = dmdAnal sigs dmd fun

-- Lots of the other code is there to make this
-- beautiful, compositional, application rule :-)
dmdAnal env dmd (App fun arg)	-- Non-type arguments
  = let				-- [Type arg handled above]
        call_dmd          = mkCallDmd dmd
	(fun_ty, fun') 	  = dmdAnal env call_dmd fun
	(arg_dmd, res_ty) = splitDmdTy fun_ty
        (arg_ty, arg') 	  = dmdAnalStar env (dmdTransformThunkDmd arg arg_dmd) arg
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

-- this is an anonymous lambda, since @dmdAnalRhs@ uses @collectBinders@
dmdAnal env dmd (Lam var body)
  | isTyVar var
  = let 
	(body_ty, body') = dmdAnal env dmd body
    in
    (body_ty, Lam var body')

  | otherwise
  = let (body_dmd, defer_and_use@(_,one_shot)) = peelCallDmd dmd
          -- body_dmd  - a demand to analyze the body
          -- one_shot  - one-shotness of the lambda
          --             hence, cardinality of its free vars

	env'		 = extendSigsWithLam env var
	(body_ty, body') = dmdAnal env' body_dmd body
	(lam_ty, var')   = annotateLamIdBndr env notArgOfDfun body_ty one_shot var
    in
    (postProcessDmdType defer_and_use lam_ty, Lam var' body')

dmdAnal env dmd (Case scrut case_bndr ty [alt@(DataAlt dc, _, _)])
  -- Only one alternative with a product constructor
  | let tycon = dataConTyCon dc
  , isProductTyCon tycon 
  , Just rec_tc' <- checkRecTc (ae_rec_tc env) tycon
  = let
        env_w_tc              = env { ae_rec_tc = rec_tc' }
	env_alt	              = extendAnalEnv NotTopLevel env_w_tc case_bndr case_bndr_sig
	(alt_ty, alt')	      = dmdAnalAlt env_alt dmd alt
	(alt_ty1, case_bndr') = annotateBndr env alt_ty case_bndr
	(_, bndrs', _)	      = alt'
	case_bndr_sig	      = cprProdSig
		-- Inside the alternative, the case binder has the CPR property.
		-- Meaning that a case on it will successfully cancel.
		-- Example:
		--	f True  x = case x of y { I# x' -> if x' ==# 3 then y else I# 8 }
		--	f False x = I# 3
		--	
		-- We want f to have the CPR property:
		--	f b x = case fw b x of { r -> I# r }
		--	fw True  x = case x of y { I# x' -> if x' ==# 3 then x' else 8 }
		--	fw False x = 3

	-- Figure out whether the demand on the case binder is used, and use
	-- that to set the scrut_dmd.  This is utterly essential.
	-- Consider	f x = case x of y { (a,b) -> k y a }
	-- If we just take scrut_demand = U(L,A), then we won't pass x to the
	-- worker, so the worker will rebuild 
	--	x = (a, absent-error)
	-- and that'll crash.
	-- So at one stage I had:
	--	dead_case_bndr		 = isAbsDmd (idDemandInfo case_bndr')
	--	keepity | dead_case_bndr = Drop
	--		| otherwise	 = Keep		
	--
	-- But then consider
	--	case x of y { (a,b) -> h y + a }
	-- where h : U(LL) -> T
	-- The above code would compute a Keep for x, since y is not Abs, which is silly
	-- The insight is, of course, that a demand on y is a demand on the
	-- scrutinee, so we need to `both` it with the scrut demand
        
	scrut_dmd1 = mkProdDmd [idDemandInfo b | b <- bndrs', isId b]
        scrut_dmd2 = strictenDmd (idDemandInfo case_bndr')
        scrut_dmd  = scrut_dmd1 `bothCleanDmd` scrut_dmd2

	(scrut_ty, scrut') = dmdAnal env scrut_dmd scrut
        res_ty             = alt_ty1 `bothDmdType` scrut_ty
    in
--    pprTrace "dmdAnal:Case1" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "dmd" <+> ppr dmd
--                                   , text "case_bndr_dmd" <+> ppr (idDemandInfo case_bndr')
--                                   , text "scrut_dmd" <+> ppr scrut_dmd
--                                   , text "scrut_ty" <+> ppr scrut_ty
--                                   , text "alt_ty" <+> ppr alt_ty1
--                                   , text "res_ty" <+> ppr res_ty ]) $
    (res_ty, Case scrut' case_bndr' ty [alt'])

dmdAnal env dmd (Case scrut case_bndr ty alts)
  = let      -- Case expression with multiple alternatives
	(alt_tys, alts')     = mapAndUnzip (dmdAnalAlt env dmd) alts
	(scrut_ty, scrut')   = dmdAnal env cleanEvalDmd scrut
	(alt_ty, case_bndr') = annotateBndr env (foldr lubDmdType botDmdType alt_tys) case_bndr
        res_ty               = alt_ty `bothDmdType` scrut_ty
    in
--    pprTrace "dmdAnal:Case2" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "scrut_ty" <+> ppr scrut_ty
--                                   , text "alt_tys" <+> ppr alt_tys
--                                   , text "alt_ty" <+> ppr alt_ty
--                                   , text "res_ty" <+> ppr res_ty ]) $
    (res_ty, Case scrut' case_bndr' ty alts')

dmdAnal env dmd (Let (NonRec id rhs) body)
  = (body_ty2, Let (NonRec id2 annotated_rhs) body')                    
  where
    (sig, lazy_fv, id1, rhs') = dmdAnalRhs NotTopLevel Nothing env id rhs
    (body_ty, body') 	      = dmdAnal (extendAnalEnv NotTopLevel env id sig) dmd body
    (body_ty1, id2)           = annotateBndr env body_ty id1
    body_ty2		      = addLazyFVs body_ty1 lazy_fv

    -- Annotate top-level lambdas at RHS basing on the aggregated demand info
    -- See Note [Annotating lambdas at right-hand side] 
    annotated_rhs = annLamWithShotness (idDemandInfo id2) rhs'   

	-- If the actual demand is better than the vanilla call
	-- demand, you might think that we might do better to re-analyse 
	-- the RHS with the stronger demand.
	-- But (a) That seldom happens, because it means that *every* path in 
	-- 	   the body of the let has to use that stronger demand
	-- (b) It often happens temporarily in when fixpointing, because
	--     the recursive function at first seems to place a massive demand.
	--     But we don't want to go to extra work when the function will
	--     probably iterate to something less demanding.  
	-- In practice, all the times the actual demand on id2 is more than
	-- the vanilla call demand seem to be due to (b).  So we don't
	-- bother to re-analyse the RHS.

dmdAnal env dmd (Let (Rec pairs) body)
  = let
	(env', lazy_fv, pairs') = dmdFix NotTopLevel env pairs
	(body_ty, body')        = dmdAnal env' dmd body
        body_ty1                = deleteFVs body_ty (map fst pairs)
	body_ty2	        = addLazyFVs body_ty1 lazy_fv 
    in
    body_ty2 `seq`
    (body_ty2,  Let (Rec pairs') body')

annLamWithShotness :: Demand -> CoreExpr -> CoreExpr
annLamWithShotness d e
  | Just u <- cleanUseDmd_maybe d
  = go u e
  | otherwise = e
  where
    go u e
      | Just (c, u') <- peelUseCall u
      , Lam bndr body <- e
      = if isTyVar bndr 
        then Lam bndr                    (go u  body)
        else Lam (setOneShotness c bndr) (go u' body)
      | otherwise
      = e

setOneShotness :: Count -> Id -> Id
setOneShotness One  bndr = setOneShotLambda bndr
setOneShotness Many bndr = bndr

dmdAnalAlt :: AnalEnv -> CleanDemand -> Alt Var -> (DmdType, Alt Var)
dmdAnalAlt env dmd (con,bndrs,rhs)
  = let 
	(rhs_ty, rhs')   = dmdAnal env dmd rhs
        rhs_ty'          = addDataConPatDmds con bndrs rhs_ty
	(alt_ty, bndrs') = annotateBndrs env rhs_ty' bndrs
	final_alt_ty | io_hack_reqd = deferAfterIO alt_ty
		     | otherwise    = alt_ty

        -- Note [IO hack in the demand analyser]
        --
	-- There's a hack here for I/O operations.  Consider
	-- 	case foo x s of { (# s, r #) -> y }
	-- Is this strict in 'y'.  Normally yes, but what if 'foo' is an I/O
	-- operation that simply terminates the program (not in an erroneous way)?
	-- In that case we should not evaluate y before the call to 'foo'.
	-- Hackish solution: spot the IO-like situation and add a virtual branch,
	-- as if we had
	-- 	case foo x s of 
	--	   (# s, r #) -> y 
	--	   other      -> return ()
	-- So the 'y' isn't necessarily going to be evaluated
	--
	-- A more complete example (Trac #148, #1592) where this shows up is:
	--	do { let len = <expensive> ;
	--	   ; when (...) (exitWith ExitSuccess)
	--	   ; print len }

	io_hack_reqd = con == DataAlt unboxedPairDataCon &&
		       idType (head bndrs) `eqType` realWorldStatePrimTy
    in	
    (final_alt_ty, (con, bndrs', rhs'))
\end{code}

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

Note [Annotating lambdas at right-hand side]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Let us take a look at the following example:

g f = let x = 100
          h = \y -> f x y
       in h 5

One can see that |h| is called just once, therefore the RHS of h can
be annotated as a one-shot lambda. This is done by the function
annLamWithShotness *a posteriori*, i.e., basing on the aggregated
usage demand on |h| from the body of |let|-expression, which is C1(U)
in this case.

In other words, for locally-bound lambdas we can infer
one-shotness. 

\begin{code}
addDataConPatDmds :: AltCon -> [Var] -> DmdType -> DmdType
-- See Note [Add demands for strict constructors]
addDataConPatDmds DEFAULT    _ dmd_ty = dmd_ty
addDataConPatDmds (LitAlt _) _ dmd_ty = dmd_ty
addDataConPatDmds (DataAlt con) bndrs dmd_ty
  = foldr add dmd_ty str_bndrs 
  where
    add bndr dmd_ty = addVarDmd dmd_ty bndr seqDmd
    str_bndrs = [ b | (b,s) <- zipEqual "addDataConPatBndrs"
                                   (filter isId bndrs)
                                   (dataConRepStrictness con)
                    , isMarkedStrict s ]
\end{code}

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
around the loop (which would otherwise happen, since 'foo' is not
strict in 'a'.  It is sound for the wrapper to pass an unboxed arg
because X is strict, so its argument must be evaluated.  And if we
*don't* pass an unboxed argument, we can't even repair it by adding a
`seq` thus:

    foo (X a) n = a `seq` go 0

because the seq is discarded (very early) since X is strict!

There is the usual danger of reboxing, which as usual we ignore. But 
if X is monomorphic, and has an UNPACK pragma, then this optimisation
is even more important.  We don't want the wrapper to rebox an unboxed
argument, and pass an Int to $wfoo!

%************************************************************************
%*									*
                    Demand transformer
%*									*
%************************************************************************

\begin{code}
dmdTransform :: AnalEnv		-- The strictness environment
	     -> Id		-- The function
	     -> CleanDemand	-- The demand on the function
	     -> DmdType		-- The demand type of the function in this context
	-- Returned DmdEnv includes the demand on 
	-- this function plus demand on its free variables

dmdTransform env var dmd
  | isDataConWorkId var		                 -- Data constructor
  = dmdTransformDataConSig 
       (idArity var) (idStrictness var) dmd

  | gopt Opt_DmdTxDictSel (ae_dflags env),
    Just _ <- isClassOpId_maybe var -- Dictionary component selector
  = dmdTransformDictSelSig (idStrictness var) dmd

  | isGlobalId var	                         -- Imported function
  = let res = dmdTransformSig (idStrictness var) dmd in
--    pprTrace "dmdTransform" (vcat [ppr var, ppr (idStrictness var), ppr dmd, ppr res])
    res

  | Just (sig, top_lvl) <- lookupSigEnv env var  -- Local letrec bound thing
  , let fn_ty = dmdTransformSig sig dmd
  = -- pprTrace "dmdTransform" (vcat [ppr var, ppr sig, ppr dmd, ppr fn_ty]) $
    if isTopLevel top_lvl
    then fn_ty   -- Don't record top level things
    else addVarDmd fn_ty var (mkOnceUsedDmd dmd)

  | otherwise	 		                 -- Local non-letrec-bound thing
  = unitVarDmd var (mkOnceUsedDmd dmd)

\end{code}

%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}

-- Recursive bindings
dmdFix :: TopLevelFlag
       -> AnalEnv 		-- Does not include bindings for this binding
       -> [(Id,CoreExpr)]
       -> (AnalEnv, DmdEnv,
	   [(Id,CoreExpr)])	-- Binders annotated with stricness info

dmdFix top_lvl env orig_pairs
  = (updSigEnv env (sigEnv final_env), lazy_fv, pairs')
     -- Return to original virgin state, keeping new signatures
  where
    bndrs        = map fst orig_pairs
    initial_env = addInitialSigs top_lvl env bndrs
    (final_env, lazy_fv, pairs') = loop 1 initial_env orig_pairs
    
    loop :: Int
	 -> AnalEnv			-- Already contains the current sigs
	 -> [(Id,CoreExpr)] 		
	 -> (AnalEnv, DmdEnv, [(Id,CoreExpr)])
    loop n env pairs
      = -- pprTrace "dmd loop" (ppr n <+> ppr bndrs $$ ppr env) $
        loop' n env pairs

    loop' n env pairs
      | found_fixpoint
      = (env', lazy_fv, pairs')
		-- Note: return pairs', not pairs.   pairs' is the result of 
		-- processing the RHSs with sigs (= sigs'), whereas pairs 
		-- is the result of processing the RHSs with the *previous* 
		-- iteration of sigs.

      | n >= 10
      = -- pprTrace "dmdFix loop" (ppr n <+> (vcat 
        --                 [ text "Sigs:" <+> ppr [ (id,lookupVarEnv (sigEnv env) id, 
        --                                              lookupVarEnv (sigEnv env') id) 
        --                                          | (id,_) <- pairs],
        --                   text "env:" <+> ppr env,
        --                   text "binds:" <+> pprCoreBinding (Rec pairs)]))
	(env, lazy_fv, orig_pairs)	-- Safe output
		-- The lazy_fv part is really important!  orig_pairs has no strictness
		-- info, including nothing about free vars.  But if we have
		--	letrec f = ....y..... in ...f...
		-- where 'y' is free in f, we must record that y is mentioned, 
		-- otherwise y will get recorded as absent altogether

      | otherwise
      = loop (n+1) (nonVirgin env') pairs'
      where
	found_fixpoint = all (same_sig (sigEnv env) (sigEnv env')) bndrs 

	((env',lazy_fv), pairs') = mapAccumL my_downRhs (env, emptyDmdEnv) pairs
		-- mapAccumL: Use the new signature to do the next pair
		-- The occurrence analyser has arranged them in a good order
		-- so this can significantly reduce the number of iterations needed
	
        my_downRhs (env, lazy_fv) (id,rhs)
          = ((env', lazy_fv'), (id', rhs'))
          where
	    (sig, lazy_fv1, id', rhs') = dmdAnalRhs top_lvl (Just bndrs) env id rhs
	    lazy_fv'		       = plusVarEnv_C bothDmd lazy_fv lazy_fv1
            env'                       = extendAnalEnv top_lvl env id sig
	   
    same_sig sigs sigs' var = lookup sigs var == lookup sigs' var
    lookup sigs var = case lookupVarEnv sigs var of
			Just (sig,_) -> sig
                        Nothing      -> pprPanic "dmdFix" (ppr var)

-- Non-recursive bindings
dmdAnalRhs :: TopLevelFlag 
           -> Maybe [Id]   -- Just bs <=> recursive, Nothing <=> non-recursive
	   -> AnalEnv -> Id -> CoreExpr
	   -> (StrictSig,  DmdEnv, Id, CoreExpr)
-- Process the RHS of the binding, add the strictness signature
-- to the Id, and augment the environment with the signature as well.
dmdAnalRhs top_lvl rec_flag env id rhs
  | Just fn <- unpackTrivial rhs   -- See Note [Trivial right-hand sides]
  , let fn_str = getStrictness env fn
  = (fn_str, emptyDmdEnv, set_idStrictness env id fn_str, rhs)

  | otherwise
  = (sig_ty, lazy_fv, id', mkLams bndrs' body')
  where
    (bndrs, body)        = collectBinders rhs
    env_body             = foldl extendSigsWithLam env bndrs
    (DmdType body_fv _      body_res, body')  = dmdAnal env_body body_dmd body
    (DmdType rhs_fv rhs_dmds rhs_res, bndrs') = annotateLamBndrs env (isDFunId id)
                                                  (DmdType body_fv [] body_res) bndrs
    sig_ty               = mkStrictSig (mkDmdType sig_fv rhs_dmds rhs_res')
    id'		         = set_idStrictness env id sig_ty
	-- See Note [NOINLINE and strictness]

    -- See Note [Product demands for function body]
    body_dmd = case deepSplitProductType_maybe (exprType body) of
                 Nothing            -> cleanEvalDmd
                 Just (dc, _, _, _) -> cleanEvalProdDmd (dataConRepArity dc)

    -- See Note [Lazy and unleashable free variables]
    -- See Note [Aggregated demand for cardinality]
    rhs_fv1 = case rec_flag of
                Just bs -> useEnv (delVarEnvList rhs_fv bs)
                Nothing -> rhs_fv

    (lazy_fv, sig_fv) = splitFVs is_thunk rhs_fv1

    rhs_res' | returnsCPR rhs_res
             , discard_cpr_info   = topRes
             | otherwise          = rhs_res

    discard_cpr_info = nested_sum || (is_thunk && not_strict)
    nested_sum     -- See Note [CPR for sum types ]
        = not (isTopLevel top_lvl || returnsCPRProd rhs_res) 

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
-- See Note [Trivial right-hand sides]
unpackTrivial (Var v)                 = Just v
unpackTrivial (Cast e _)              = unpackTrivial e
unpackTrivial (Lam v e) | isTyVar v   = unpackTrivial e
unpackTrivial (App e a) | isTypeArg a = unpackTrivial e
unpackTrivial _                       = Nothing
\end{code}

Note [Trivial right-hand sides]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
	foo = plusInt |> co
where plusInt is an arity-2 function with known strictness.  Clearly
we want plusInt's strictness to propagate to foo!  But because it has
no manifest lambdas, it won't do so automatically.  So we have a 
special case for right-hand sides that are "trivial", namely variables,
casts, type applications, and the like. 

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

%************************************************************************
%*									*
\subsection{Strictness signatures and types}
%*									*
%************************************************************************

\begin{code}
unitVarDmd :: Var -> Demand -> DmdType
unitVarDmd var dmd 
  = DmdType (unitVarEnv var dmd) [] topRes

addVarDmd :: DmdType -> Var -> Demand -> DmdType
addVarDmd (DmdType fv ds res) var dmd
  = DmdType (extendVarEnv_C bothDmd fv var dmd) ds res

addLazyFVs :: DmdType -> DmdEnv -> DmdType
addLazyFVs dmd_ty lazy_fvs
  = dmd_ty `bothDmdType` mkDmdType lazy_fvs [] topRes
	-- Using bothDmdType (rather than just both'ing the envs)
        -- is vital.  Consider
	--	let f = \x -> (x,y)
	--	in  error (f 3)
	-- Here, y is treated as a lazy-fv of f, but we must `bothDmd` that L
	-- demand with the bottom coming up from 'error'
	-- 
	-- I got a loop in the fixpointer without this, due to an interaction
	-- with the lazy_fv filtering in dmdAnalRhs.  Roughly, it was
	--	letrec f n x 
	--	    = letrec g y = x `fatbar` 
	--			   letrec h z = z + ...g...
	--			   in h (f (n-1) x)
	-- 	in ...
	-- In the initial iteration for f, f=Bot
	-- Suppose h is found to be strict in z, but the occurrence of g in its RHS
	-- is lazy.  Now consider the fixpoint iteration for g, esp the demands it
	-- places on its free variables.  Suppose it places none.  Then the
	-- 	x `fatbar` ...call to h...
	-- will give a x->V demand for x.  That turns into a L demand for x,
	-- which floats out of the defn for h.  Without the modifyEnv, that
	-- L demand doesn't get both'd with the Bot coming up from the inner
	-- call to f.  So we just get an L demand for x for g.
\end{code}

Note [do not strictify the argument dictionaries of a dfun]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The typechecker can tie recursive knots involving dfuns, so we do the
conservative thing and refrain from strictifying a dfun's argument
dictionaries.

\begin{code}
annotateBndr :: AnalEnv -> DmdType -> Var -> (DmdType, Var)
-- The returned env has the var deleted
-- The returned var is annotated with demand info
-- according to the result demand of the provided demand type
-- No effect on the argument demands
annotateBndr env dmd_ty var
  | isTyVar var = (dmd_ty, var)
  | otherwise   = (dmd_ty', set_idDemandInfo env var dmd')
  where
    (dmd_ty', dmd) = peelFV dmd_ty var

    dmd' | gopt Opt_DictsStrict (ae_dflags env)
             -- We never want to strictify a recursive let. At the moment
             -- annotateBndr is only call for non-recursive lets; if that
             -- changes, we need a RecFlag parameter and another guard here.
         = strictifyDictDmd (idType var) dmd
         | otherwise = dmd

annotateBndrs :: AnalEnv -> DmdType -> [Var] -> (DmdType, [Var])
annotateBndrs env = mapAccumR (annotateBndr env)

annotateLamBndrs :: AnalEnv -> DFunFlag -> DmdType -> [Var] -> (DmdType, [Var])
annotateLamBndrs env args_of_dfun ty bndrs = mapAccumR annotate ty bndrs
  where
    annotate dmd_ty bndr
      | isId bndr = annotateLamIdBndr env args_of_dfun dmd_ty Many bndr
      | otherwise = (dmd_ty, bndr)

annotateLamIdBndr :: AnalEnv
                  -> DFunFlag   -- is this lambda at the top of the RHS of a dfun?
                  -> DmdType    -- Demand type of body
                  -> Count      -- One-shot-ness of the lambda
		  -> Id         -- Lambda binder
		  -> (DmdType,  -- Demand type of lambda
		      Id)	-- and binder annotated with demand

annotateLamIdBndr env arg_of_dfun dmd_ty one_shot id
-- For lambdas we add the demand to the argument demands
-- Only called for Ids
  = ASSERT( isId id )
    -- pprTrace "annLamBndr" (vcat [ppr id, ppr _dmd_ty]) $
    (final_ty, setOneShotness one_shot (set_idDemandInfo env id dmd'))
  where
      -- Watch out!  See note [Lambda-bound unfoldings]
    final_ty = case maybeUnfoldingTemplate (idUnfolding id) of
                 Nothing  -> main_ty
                 Just unf -> main_ty `bothDmdType` unf_ty
                          where
                             (unf_ty, _) = dmdAnalStar env dmd unf

    main_ty = addDemand dmd dmd_ty'
    (dmd_ty', dmd) = peelFV dmd_ty id

    dmd' | gopt Opt_DictsStrict (ae_dflags env),
           -- see Note [do not strictify the argument dictionaries of a dfun]
           not arg_of_dfun
         = strictifyDictDmd (idType id) dmd
         | otherwise = dmd

deleteFVs :: DmdType -> [Var] -> DmdType
deleteFVs (DmdType fvs dmds res) bndrs
  = DmdType (delVarEnvList fvs bndrs) dmds res
\end{code}

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
DmdType, because that makes fixpointing very slow --- the 
DmdType gets full of lazy demands that are slow to converge.


Note [Lamba-bound unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We allow a lambda-bound variable to carry an unfolding, a facility that is used
exclusively for join points; see Note [Case binders and join points].  If so,
we must be careful to demand-analyse the RHS of the unfolding!  Example
   \x. \y{=Just x}. <body>
Then if <body> uses 'y', then transitively it uses 'x', and we must not
forget that fact, otherwise we might make 'x' absent when it isn't.


%************************************************************************
%*									*
\subsection{Strictness signatures}
%*									*
%************************************************************************

\begin{code}
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
    = ptext (sLit "AE") <+> braces (vcat
         [ ptext (sLit "ae_virgin =") <+> ppr virgin
         , ptext (sLit "ae_sigs =") <+> ppr env ])

emptyAnalEnv :: DynFlags -> AnalEnv
emptyAnalEnv dflags = AE { ae_dflags = dflags, ae_sigs = emptySigEnv
                         , ae_virgin = True, ae_rec_tc = initRecTc }

emptySigEnv :: SigEnv
emptySigEnv = emptyVarEnv

sigEnv :: AnalEnv -> SigEnv
sigEnv = ae_sigs

updSigEnv :: AnalEnv -> SigEnv -> AnalEnv
updSigEnv env sigs = env { ae_sigs = sigs }

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

addInitialSigs :: TopLevelFlag -> AnalEnv -> [Id] -> AnalEnv
-- See Note [Initialising strictness]
addInitialSigs top_lvl env@(AE { ae_sigs = sigs, ae_virgin = virgin }) ids
  = env { ae_sigs = extendVarEnvList sigs [ (id, (init_sig id, top_lvl))
                                          | id <- ids ] }
  where
    init_sig | virgin    = \_ -> botSig
             | otherwise = idStrictness

nonVirgin :: AnalEnv -> AnalEnv
nonVirgin env = env { ae_virgin = False }

extendSigsWithLam :: AnalEnv -> Id -> AnalEnv
-- Extend the AnalEnv when we meet a lambda binder
extendSigsWithLam env id
  | isId id
  , isStrictDmd (idDemandInfo id) || ae_virgin env  
       -- See Note [Optimistic CPR in the "virgin" case]
       -- See Note [Initial CPR for strict binders]
  , Just {} <- deepSplitProductType_maybe $ idType id
  = extendAnalEnv NotTopLevel env id cprProdSig 

  | otherwise 
  = env

set_idDemandInfo :: AnalEnv -> Id -> Demand -> Id
set_idDemandInfo env id dmd 
  = setIdDemandInfo id (zapDemand (ae_dflags env) dmd)

set_idStrictness :: AnalEnv -> Id -> StrictSig -> Id
set_idStrictness env id sig
  = setIdStrictness id (zapStrictSig (ae_dflags env) sig)

dumpStrSig :: CoreProgram -> SDoc
dumpStrSig binds = vcat (map printId ids)
  where
  ids = sortBy (stableNameCmp `on` getName) (concatMap getIds binds)
  getIds (NonRec i _) = [ i ]
  getIds (Rec bs)     = map fst bs
  printId id | isExportedId id = ppr id <> colon <+> pprIfaceStrictSig (idStrictness id)
             | otherwise       = empty

\end{code}

Note [Initial CPR for strict binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CPR is initialized for a lambda binder in an optimistic manner, i.e,
if the binder is used strictly and at least some of its components as
a product are used, which is checked by the value of the absence
demand.

If the binder is marked demanded with a strict demand, then give it a
CPR signature, because in the likely event that this is a lambda on a
fn defn [we only use this when the lambda is being consumed with a
call demand], it'll be w/w'd and so it will be CPR-ish.  E.g.

	f = \x::(Int,Int).  if ...strict in x... then
				x
			    else
				(a,b)
We want f to have the CPR property because x does, by the time f has been w/w'd

Also note that we only want to do this for something that definitely
has product type, else we may get over-optimistic CPR results
(e.g. from \x -> x!).


Note [Initialising strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See section 9.2 (Finding fixpoints) of the paper.

Our basic plan is to initialise the strictness of each Id in a
recursive group to "bottom", and find a fixpoint from there.  However,
this group B might be inside an *enclosing* recursiveb group A, in
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
