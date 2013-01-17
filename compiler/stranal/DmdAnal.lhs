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
import DataCon		( dataConTyCon, dataConRepStrictness, isMarkedStrict )
import Id
import CoreUtils	( exprIsHNF, exprIsTrivial )
import PprCore	
import UniqFM		( filterUFM )
import TyCon
import Pair
import Type		( eqType, tyConAppTyCon_maybe )
import Coercion         ( coercionKind )
import Util
import Maybes		( orElse )
import TysWiredIn	( unboxedPairDataCon )
import TysPrim		( realWorldStatePrimTy )
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
	return binds_plus_dmds
    }
  where
    do_prog :: CoreProgram -> CoreProgram
    do_prog binds = snd $ mapAccumL (dmdAnalTopBind dflags) emptySigEnv binds

-- Analyse a (group of) top-level binding(s)
dmdAnalTopBind :: DynFlags
               -> SigEnv
	       -> CoreBind 
	       -> (SigEnv, CoreBind)
dmdAnalTopBind dflags sigs (NonRec id rhs)
  = (sigs2, NonRec id2 rhs2)
  where
    (    _, _, (_,   rhs1)) = dmdAnalRhs dflags TopLevel NonRecursive (virgin sigs)    (id, rhs)
    (sigs2, _, (id2, rhs2)) = dmdAnalRhs dflags TopLevel NonRecursive (nonVirgin sigs) (id, rhs1)
    	-- Do two passes to improve CPR information
    	-- See comments with ignore_cpr_info in mk_sig_ty
    	-- and with extendSigsWithLam

dmdAnalTopBind dflags sigs (Rec pairs)
  = (sigs', Rec pairs')
  where
    (sigs', _, pairs')  = dmdFix dflags TopLevel (virgin sigs) pairs
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

Note [Always analyse in virgin pass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tricky point: make sure that we analyse in the 'virgin' pass. Consider
   rec { f acc x True  = f (...rec { g y = ...g... }...)
         f acc x False = acc }
In the virgin pass for 'f' we'll give 'f' a very strict (bottom) type.
That might mean that we analyse the sub-expression containing the 
E = "...rec g..." stuff in a bottom demand.  Suppose we *didn't analyse*
E, but just retuned botType.  

Then in the *next* (non-virgin) iteration for 'f', we might analyse E
in a weaker demand, and that will trigger doing a fixpoint iteration
for g.  But *because it's not the virgin pass* we won't start g's
iteration at bottom.  Disaster.  (This happened in $sfibToList' of 
nofib/spectral/fibheaps.)

So in the virgin pass we make sure that we do analyse the expression
at least once, to initialise its signatures.

\begin{code}
evalDmdAnal :: DynFlags -> AnalEnv -> CoreExpr -> (DmdType, CoreExpr)
-- See Note [Ensure demand is strict]
evalDmdAnal dflags env e
  | (res_ty, e') <- dmdAnal dflags env evalDmd e
  = (deferType res_ty, e')

simpleDmdAnal :: DynFlags -> AnalEnv -> DmdType -> CoreExpr -> (DmdType, CoreExpr)
simpleDmdAnal dflags env res_ty e
  | ae_virgin env -- See Note [Always analyse in virgin pass]
  , (_discarded_res_ty, e') <- dmdAnal dflags env evalDmd e
  = (res_ty, e')
  | otherwise
  = (res_ty, e)

dmdAnal :: DynFlags -> AnalEnv -> Demand -> CoreExpr -> (DmdType, CoreExpr)
dmdAnal dflags env dmd e 
  | isBotDmd dmd  	  = simpleDmdAnal dflags env botDmdType e
  | isAbsDmd dmd          = simpleDmdAnal dflags env topDmdType e
  | not (isStrictDmd dmd) = evalDmdAnal   dflags env            e

dmdAnal _ _ _ (Lit lit)     = (topDmdType, Lit lit)
dmdAnal _ _ _ (Type ty)     = (topDmdType, Type ty)	-- Doesn't happen, in fact
dmdAnal _ _ _ (Coercion co) = (topDmdType, Coercion co)

dmdAnal _ env dmd (Var var)
  = (dmdTransform env var dmd, Var var)

dmdAnal dflags env dmd (Cast e co)
  = (dmd_ty, Cast e' co)
  where
    (dmd_ty, e') = dmdAnal dflags env dmd' e
    to_co        = pSnd (coercionKind co)
    dmd'
      | Just tc <- tyConAppTyCon_maybe to_co
      , isRecursiveTyCon tc = evalDmd
      | otherwise           = dmd
	-- This coerce usually arises from a recursive
        -- newtype, and we don't want to look inside them
	-- for exactly the same reason that we don't look
	-- inside recursive products -- we might not reach
	-- a fixpoint.  So revert to a vanilla Eval demand

dmdAnal dflags env dmd (Tick t e)
  = (dmd_ty, Tick t e')
  where
    (dmd_ty, e') = dmdAnal dflags env dmd e

dmdAnal dflags env dmd (App fun (Type ty))
  = (fun_ty, App fun' (Type ty))
  where
    (fun_ty, fun') = dmdAnal dflags env dmd fun

dmdAnal dflags sigs dmd (App fun (Coercion co))
  = (fun_ty, App fun' (Coercion co))
  where
    (fun_ty, fun') = dmdAnal dflags sigs dmd fun

-- Lots of the other code is there to make this
-- beautiful, compositional, application rule :-)
dmdAnal dflags env dmd (App fun arg)	-- Non-type arguments
  = let	     		                -- [Type arg handled above]
	(fun_ty, fun') 	  = dmdAnal dflags env (mkCallDmd dmd) fun
	(arg_ty, arg') 	  = dmdAnal dflags env arg_dmd arg
	(arg_dmd, res_ty) = splitDmdTy fun_ty
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

dmdAnal dflags env dmd (Lam var body)
  | isTyVar var
  = let   
	(body_ty, body') = dmdAnal dflags env dmd body
    in
    (body_ty, Lam var body')

  | Just body_dmd <- peelCallDmd dmd	-- A call demand: good!
  = let	
	env'		 = extendSigsWithLam env var
	(body_ty, body') = dmdAnal dflags env' body_dmd body
	(lam_ty, var')   = annotateLamIdBndr dflags env body_ty var
    in
    (lam_ty, Lam var' body')

  | otherwise	-- Not enough demand on the lambda; but do the body
  = let		-- anyway to annotate it and gather free var info
	(body_ty, body') = dmdAnal dflags env evalDmd body
	(lam_ty, var')   = annotateLamIdBndr dflags env body_ty var
    in
    (deferType lam_ty, Lam var' body')

dmdAnal dflags env dmd (Case scrut case_bndr ty [alt@(DataAlt dc, _, _)])
  -- Only one alternative with a product constructor
  | let tycon = dataConTyCon dc
  , isProductTyCon tycon
  , not (isRecursiveTyCon tycon)
  = let
	env_alt	              = extendAnalEnv NotTopLevel env case_bndr case_bndr_sig
	(alt_ty, alt')	      = dmdAnalAlt dflags env_alt dmd alt
	(alt_ty1, case_bndr') = annotateBndr alt_ty case_bndr
	(_, bndrs', _)	      = alt'
	case_bndr_sig	      = cprSig
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

	alt_dmd 	   = mkProdDmd [idDemandInfo b | b <- bndrs', isId b]
        scrut_dmd 	   = alt_dmd `bothDmd`
			     idDemandInfo case_bndr'

	(scrut_ty, scrut') = dmdAnal dflags env scrut_dmd scrut
        res_ty             = alt_ty1 `bothDmdType` scrut_ty
    in
--    pprTrace "dmdAnal:Case1" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "dmd" <+> ppr dmd
--                                   , text "alt_dmd" <+> ppr alt_dmd
--                                   , text "case_bndr_dmd" <+> ppr (idDemandInfo case_bndr')
--                                   , text "scrut_dmd" <+> ppr scrut_dmd
--                                   , text "scrut_ty" <+> ppr scrut_ty
--                                   , text "alt_ty" <+> ppr alt_ty1
--                                   , text "res_ty" <+> ppr res_ty ]) $
    (res_ty, Case scrut' case_bndr' ty [alt'])

dmdAnal dflags env dmd (Case scrut case_bndr ty alts)
  = let      -- Case expression with multiple alternatives
	(alt_tys, alts')        = mapAndUnzip (dmdAnalAlt dflags env dmd) alts
	(scrut_ty, scrut')      = dmdAnal dflags env evalDmd scrut
	(alt_ty, case_bndr')	= annotateBndr (foldr lubDmdType botDmdType alt_tys) case_bndr
        res_ty                  = alt_ty `bothDmdType` scrut_ty
    in
--    pprTrace "dmdAnal:Case2" (vcat [ text "scrut" <+> ppr scrut
--                                   , text "scrut_ty" <+> ppr scrut_ty
--                                   , text "alt_ty" <+> ppr alt_ty
--                                   , text "res_ty" <+> ppr res_ty ]) $
    (res_ty, Case scrut' case_bndr' ty alts')

dmdAnal dflags env dmd (Let (NonRec id rhs) body)
  = let
	(sigs', lazy_fv, (id1, rhs')) = dmdAnalRhs dflags NotTopLevel NonRecursive env (id, rhs)
	(body_ty, body') 	      = dmdAnal dflags (updSigEnv env sigs') dmd body
	(body_ty1, id2)    	      = annotateBndr body_ty id1
	body_ty2		      = addLazyFVs body_ty1 lazy_fv
    in
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
    (body_ty2, Let (NonRec id2 rhs') body')    

dmdAnal dflags env dmd (Let (Rec pairs) body)
  = let
	bndrs			 = map fst pairs
	(sigs', lazy_fv, pairs') = dmdFix dflags NotTopLevel env pairs
	(body_ty, body')         = dmdAnal dflags (updSigEnv env sigs') dmd body
	body_ty1		 = addLazyFVs body_ty lazy_fv
    in
    sigs' `seq` body_ty `seq`
    let
	(body_ty2, _) = annotateBndrs body_ty1 bndrs
		-- Don't bother to add demand info to recursive
		-- binders as annotateBndr does; 
		-- being recursive, we can't treat them strictly.
		-- But we do need to remove the binders from the result demand env
    in
    (body_ty2,  Let (Rec pairs') body')


dmdAnalAlt :: DynFlags -> AnalEnv -> Demand -> Alt Var -> (DmdType, Alt Var)
dmdAnalAlt dflags env dmd (con,bndrs,rhs)
  = let 
	(rhs_ty, rhs')   = dmdAnal dflags env dmd rhs
        rhs_ty'          = addDataConPatDmds con bndrs rhs_ty
	(alt_ty, bndrs') = annotateBndrs rhs_ty' bndrs
	final_alt_ty | io_hack_reqd = alt_ty `lubDmdType` topDmdType
		     | otherwise    = alt_ty

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

addDataConPatDmds :: AltCon -> [Var] -> DmdType -> DmdType
-- See Note [Add demands for strict constructors]
addDataConPatDmds DEFAULT    _ dmd_ty = dmd_ty
addDataConPatDmds (LitAlt _) _ dmd_ty = dmd_ty
addDataConPatDmds (DataAlt con) bndrs dmd_ty
  = foldr add dmd_ty str_bndrs 
  where
    add bndr dmd_ty = addVarDmd dmd_ty bndr absDmd
    str_bndrs = [ b | (b,s) <- zipEqual "addDataConPatBndrs"
                                   (filter isId bndrs)
                                   (dataConRepStrictness con)
                    , isMarkedStrict s ]

\end{code}

%************************************************************************
%*									*
                    Demand transformer
%*									*
%************************************************************************

\begin{code}
dmdTransform :: AnalEnv		-- The strictness environment
	     -> Id		-- The function
	     -> Demand		-- The demand on the function
	     -> DmdType		-- The demand type of the function in this context
	-- Returned DmdEnv includes the demand on 
	-- this function plus demand on its free variables

dmdTransform env var dmd
  | isDataConWorkId var		                 -- Data constructor
  = dmdTransformDataConSig 
       (idArity var) (idStrictness var) dmd

  | isGlobalId var	                         -- Imported function
  = dmdTransformSig (idStrictness var) dmd

  | Just (sig, top_lvl) <- lookupSigEnv env var  -- Local letrec bound thing
  , let fn_ty = dmdTransformSig sig dmd
  = if isTopLevel top_lvl           
    then fn_ty	-- Don't record top level things
    else addVarDmd fn_ty var dmd

  | otherwise	 		                 -- Local non-letrec-bound thing
  = unitVarDmd var dmd
\end{code}

%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}

-- Recursive bindings
dmdFix :: DynFlags
       -> TopLevelFlag
       -> AnalEnv 		-- Does not include bindings for this binding
       -> [(Id,CoreExpr)]
       -> (SigEnv, DmdEnv,
	   [(Id,CoreExpr)])	-- Binders annotated with stricness info

dmdFix dflags top_lvl env orig_pairs
  = loop 1 initial_env orig_pairs
  where
    bndrs        = map fst orig_pairs
    initial_env = addInitialSigs top_lvl env bndrs
    
    loop :: Int
	 -> AnalEnv			-- Already contains the current sigs
	 -> [(Id,CoreExpr)] 		
	 -> (SigEnv, DmdEnv, [(Id,CoreExpr)])
    loop n env pairs
      = -- pprTrace "dmd loop" (ppr n <+> ppr bndrs $$ ppr env) $
        loop' n env pairs

    loop' n env pairs
      | found_fixpoint
      = (sigs', lazy_fv, pairs')
		-- Note: return pairs', not pairs.   pairs' is the result of 
		-- processing the RHSs with sigs (= sigs'), whereas pairs 
		-- is the result of processing the RHSs with the *previous* 
		-- iteration of sigs.

      | n >= 10  
      = pprTrace "dmdFix loop" (ppr n <+> (vcat 
			[ text "Sigs:" <+> ppr [ (id,lookupVarEnv sigs id, lookupVarEnv sigs' id) 
                                               | (id,_) <- pairs],
			  text "env:" <+> ppr env,
			  text "binds:" <+> pprCoreBinding (Rec pairs)]))
	(sigEnv env, lazy_fv, orig_pairs)	-- Safe output
		-- The lazy_fv part is really important!  orig_pairs has no strictness
		-- info, including nothing about free vars.  But if we have
		--	letrec f = ....y..... in ...f...
		-- where 'y' is free in f, we must record that y is mentioned, 
		-- otherwise y will get recorded as absent altogether

      | otherwise
      = loop (n+1) (nonVirgin sigs') pairs'
      where
        sigs = sigEnv env
	found_fixpoint = all (same_sig sigs sigs') bndrs 

	((sigs',lazy_fv), pairs') = mapAccumL my_downRhs (sigs, emptyDmdEnv) pairs
		-- mapAccumL: Use the new signature to do the next pair
		-- The occurrence analyser has arranged them in a good order
		-- so this can significantly reduce the number of iterations needed
	
        my_downRhs (sigs,lazy_fv) (id,rhs)
          = ((sigs', lazy_fv'), pair')
          where
	    (sigs', lazy_fv1, pair') = dmdAnalRhs dflags top_lvl Recursive (updSigEnv env sigs) (id,rhs)
	    lazy_fv'		     = plusVarEnv_C bothDmd lazy_fv lazy_fv1
	   
    same_sig sigs sigs' var = lookup sigs var == lookup sigs' var
    lookup sigs var = case lookupVarEnv sigs var of
			Just (sig,_) -> sig
                        Nothing      -> pprPanic "dmdFix" (ppr var)

-- Non-recursive bindings
dmdAnalRhs :: DynFlags -> TopLevelFlag -> RecFlag
	-> AnalEnv -> (Id, CoreExpr)
	-> (SigEnv,  DmdEnv, (Id, CoreExpr))
-- Process the RHS of the binding, add the strictness signature
-- to the Id, and augment the environment with the signature as well.
dmdAnalRhs dflags top_lvl rec_flag env (id, rhs)
 = (sigs', lazy_fv, (id', rhs'))
 where
  arity		     = idArity id   -- The idArity should be up to date
				    -- The simplifier was run just beforehand
  (rhs_dmd_ty, rhs') = dmdAnal dflags env (vanillaCall arity) rhs
  (lazy_fv, sig_ty)  = WARN( arity /= dmdTypeDepth rhs_dmd_ty && not (exprIsTrivial rhs), ppr id )
                       -- The RHS can be eta-reduced to just a variable, 
                       -- in which case we should not complain. 
		       mkSigTy top_lvl rec_flag env id rhs rhs_dmd_ty
  id'		     = id `setIdStrictness` sig_ty
  sigs'		     = extendSigEnv top_lvl (sigEnv env) id sig_ty

\end{code}

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
addLazyFVs (DmdType fv ds res) lazy_fvs
  = DmdType both_fv1 ds res
  where
    both_fv = plusVarEnv_C bothDmd fv lazy_fvs
    both_fv1 = modifyEnv (isBotRes res) (`bothDmd` botDmd) lazy_fvs fv both_fv
	-- This modifyEnv is vital.  Consider
	--	let f = \x -> (x,y)
	--	in  error (f 3)
	-- Here, y is treated as a lazy-fv of f, but we must `bothDmd` that L
	-- demand with the bottom coming up from 'error'
	-- 
	-- I got a loop in the fixpointer without this, due to an interaction
	-- with the lazy_fv filtering in mkSigTy.  Roughly, it was
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
	--
	-- A better way to say this is that the lazy-fv filtering should give the
	-- same answer as putting the lazy fv demands in the function's type.


removeFV :: DmdEnv -> Var -> DmdResult -> (DmdEnv, Demand)
removeFV fv id res = -- pprTrace "rfv" (ppr id <+> ppr dmd $$ ppr fv)
                     (fv', dmd)
		where
		  fv' = fv `delVarEnv` id
		  dmd = lookupVarEnv fv id `orElse` deflt
                  -- See note [Default demand for variables]
	 	  deflt | isBotRes res = botDmd
		        | otherwise    = absDmd
\end{code}

Note [Default demand for variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the variable is not mentioned in the environment of a demand type,
its demand is taken to be a result demand of the type: either L or the
bottom. Both are safe from the semantical pont of view, however, for
the safe result we also have absent demand set to Abs, which makes it
possible to safely ignore non-mentioned variables (their joint demand
is <L,A>).

\begin{code}
annotateBndr :: DmdType -> Var -> (DmdType, Var)
-- The returned env has the var deleted
-- The returned var is annotated with demand info
-- according to the result demand of the provided demand type
-- No effect on the argument demands
annotateBndr dmd_ty@(DmdType fv ds res) var
  | isTyVar var = (dmd_ty, var)
  | otherwise   = (DmdType fv' ds res, setIdDemandInfo var dmd)
  where
    (fv', dmd) = removeFV fv var res

annotateBndrs :: DmdType -> [Var] -> (DmdType, [Var])
annotateBndrs = mapAccumR annotateBndr

annotateLamIdBndr :: DynFlags
                  -> AnalEnv
                  -> DmdType 	-- Demand type of body
		  -> Id 	-- Lambda binder
		  -> (DmdType, 	-- Demand type of lambda
		      Id)	-- and binder annotated with demand	

annotateLamIdBndr dflags env (DmdType fv ds res) id
-- For lambdas we add the demand to the argument demands
-- Only called for Ids
  = ASSERT( isId id )
    (final_ty, setIdDemandInfo id dmd)
  where
      -- Watch out!  See note [Lambda-bound unfoldings]
    final_ty = case maybeUnfoldingTemplate (idUnfolding id) of
                 Nothing  -> main_ty
                 Just unf -> main_ty `bothDmdType` unf_ty
                          where
                             (unf_ty, _) = dmdAnal dflags env dmd unf
    
    main_ty = DmdType fv' (dmd:ds) res

    (fv', dmd) = removeFV fv id res

mkSigTy :: TopLevelFlag -> RecFlag -> AnalEnv -> Id -> 
           CoreExpr -> DmdType -> (DmdEnv, StrictSig)
mkSigTy top_lvl rec_flag env id rhs (DmdType fv dmds res)
  = (lazy_fv, mkStrictSig dmd_ty)
	-- See Note [NOINLINE and strictness]
  where
    dmd_ty = mkDmdType strict_fv dmds res'

    -- See Note [Lazy and strict free variables]
    lazy_fv   = filterUFM (not . isStrictDmd) fv
    strict_fv = filterUFM isStrictDmd         fv

    ignore_cpr_info = not (exprIsHNF rhs || thunk_cpr_ok)
    res' = if returnsCPR res && ignore_cpr_info 
	   then topRes
           else res 

    -- Is it okay or not to assign CPR 
    -- (not okay in the first pass)
    thunk_cpr_ok   -- See Note [CPR for thunks]
        | isTopLevel top_lvl       	= False	-- Top level things don't get
				                -- their demandInfo set at all
	| isRec rec_flag	   	= False	-- Ditto recursive things
        | ae_virgin env            	= True  -- Optimistic, first time round
        -- See Note [Optimistic CPR in the "virgin" case]
	| isStrictDmd (idDemandInfo id) = True
	| otherwise 		        = False	
\end{code}

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

Note [Lazy and strict free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We put the strict FVs in the DmdType of the Id, so 
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
data AnalEnv
  = AE { ae_sigs   :: SigEnv
       , ae_virgin :: Bool }  -- True on first iteration only
		              -- See Note [Initialising strictness]
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

addInitialSigs :: TopLevelFlag -> AnalEnv -> [Id] -> AnalEnv
-- See Note [Initialising strictness]
addInitialSigs top_lvl env@(AE { ae_sigs = sigs, ae_virgin = virgin }) ids
  = env { ae_sigs = extendVarEnvList sigs [ (id, (init_sig id, top_lvl))
                                          | id <- ids ] }
  where
    init_sig | virgin    = \_ -> botSig
             | otherwise = idStrictness

virgin, nonVirgin :: SigEnv -> AnalEnv
virgin    sigs = AE { ae_sigs = sigs, ae_virgin = True }
nonVirgin sigs = AE { ae_sigs = sigs, ae_virgin = False }

extendSigsWithLam :: AnalEnv -> Id -> AnalEnv
-- Extend the AnalEnv when we meet a lambda binder
extendSigsWithLam env id
  | ae_virgin env   -- See Note [Optimistic CPR in the "virgin" case]
  = extendAnalEnv NotTopLevel env id cprSig

  | isStrictDmd dmd_info  -- Might be bottom, first time round
  , Just {} <- deepSplitProductType_maybe $ idType id
  = extendAnalEnv NotTopLevel env id cprSig
       -- See Note [Initial CPR for strict binders]

  | otherwise = env
  where
    dmd_info = idDemandInfo id
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
