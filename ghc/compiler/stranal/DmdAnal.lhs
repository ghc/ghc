%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

			-----------------
			A demand analysis
			-----------------

\begin{code}
module DmdAnal ( dmdAnalPgm, dmdAnalTopRhs, 
		 both {- needed by WwLib -}
   ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlags, DynFlag(..), opt_MaxWorkerArgs )
import NewDemand	-- All of it
import CoreSyn
import PprCore	
import CoreUtils	( exprIsValue, exprArity )
import DataCon		( dataConTyCon )
import TyCon		( isProductTyCon, isRecursiveTyCon )
import Id		( Id, idType, idDemandInfo, idInlinePragma,
			  isDataConId, isGlobalId, idArity,
			  idNewStrictness, idNewStrictness_maybe, getNewStrictness, setIdNewStrictness,
			  idNewDemandInfo, setIdNewDemandInfo, newStrictnessFromOld )
import IdInfo		( newDemand )
import Var		( Var )
import VarEnv
import UniqFM		( plusUFM_C, addToUFM_Directly, lookupUFM_Directly,
			  keysUFM, minusUFM, ufmToList, filterUFM )
import Type		( isUnLiftedType )
import CoreLint		( showPass, endPass )
import Util		( mapAndUnzip, mapAccumL, mapAccumR, lengthIs, equalLength )
import BasicTypes	( Arity, TopLevelFlag(..), isTopLevel, isNeverActive )
import Maybes		( orElse, expectJust )
import Outputable
\end{code}

To think about

* set a noinline pragma on bottoming Ids

* Consider f x = x+1 `fatbar` error (show x)
  We'd like to unbox x, even if that means reboxing it in the error case.

\begin{code}
instance Outputable TopLevelFlag where
  ppr flag = empty
\end{code}

%************************************************************************
%*									*
\subsection{Top level stuff}
%*									*
%************************************************************************

\begin{code}
dmdAnalPgm :: DynFlags -> [CoreBind] -> IO [CoreBind]
dmdAnalPgm dflags binds
  = do {
	showPass dflags "Demand analysis" ;
	let { binds_plus_dmds = do_prog binds ;
	      dmd_changes = get_changes binds_plus_dmds } ;
	endPass dflags "Demand analysis" 
	 	Opt_D_dump_stranal binds_plus_dmds ;
#ifdef DEBUG
	-- Only if DEBUG is on, because only then is the old strictness analyser run
	printDump (text "Changes in demands" $$ dmd_changes) ;
#endif
	return binds_plus_dmds
    }
  where
    do_prog :: [CoreBind] -> [CoreBind]
    do_prog binds = snd $ mapAccumL dmdAnalTopBind emptySigEnv binds

dmdAnalTopBind :: SigEnv
	       -> CoreBind 
	       -> (SigEnv, CoreBind)
dmdAnalTopBind sigs (NonRec id rhs)
  = let
	(    _, _, (_,   rhs1)) = dmdAnalRhs TopLevel sigs (id, rhs)
	(sigs2, _, (id2, rhs2)) = dmdAnalRhs TopLevel sigs (id, rhs1)
		-- Do two passes to improve CPR information
		-- See the comments with mkSigTy.ignore_cpr_info below
    in
    (sigs2, NonRec id2 rhs2)    

dmdAnalTopBind sigs (Rec pairs)
  = let
	(sigs', _, pairs')  = dmdFix TopLevel sigs pairs
		-- We get two iterations automatically
    in
    (sigs', Rec pairs')
\end{code}

\begin{code}
dmdAnalTopRhs :: CoreExpr -> (StrictSig, CoreExpr)
-- Analyse the RHS and return
--	a) appropriate strictness info
--	b) the unfolding (decorated with stricntess info)
dmdAnalTopRhs rhs
  = (sig, rhs')
  where
    arity	   = exprArity rhs
    (rhs_ty, rhs') = dmdAnal emptySigEnv (vanillaCall arity) rhs
    sig		   = mkTopSigTy rhs rhs_ty
\end{code}

%************************************************************************
%*									*
\subsection{The analyser itself}	
%*									*
%************************************************************************

\begin{code}
dmdAnal :: SigEnv -> Demand -> CoreExpr -> (DmdType, CoreExpr)

dmdAnal sigs Abs  e = (topDmdType, e)
dmdAnal sigs Bot  e = (botDmdType, e)

dmdAnal sigs Lazy e = let 
			(res_ty, e') = dmdAnal sigs Eval e
		      in
		      (deferType res_ty, e')
	-- It's important not to analyse e with a lazy demand because
	-- a) When we encounter   case s of (a,b) -> 
	--	we demand s with U(d1d2)... but if the overall demand is lazy
	--	that is wrong, and we'd need to reduce the demand on s,
	--	which is inconvenient
	-- b) More important, consider
	--	f (let x = R in x+x), where f is lazy
	--    We still want to mark x as demanded, because it will be when we
	--    enter the let.  If we analyse f's arg with a Lazy demand, we'll
	--    just mark x as Lazy
	-- c) The application rule wouldn't be right either
	--    Evaluating (f x) in a L demand does *not* cause
	--    evaluation of f in a C(L) demand!


dmdAnal sigs dmd (Lit lit)
  = (topDmdType, Lit lit)

dmdAnal sigs dmd (Var var)
  = (dmdTransform sigs var dmd, Var var)

dmdAnal sigs dmd (Note n e)
  = (dmd_ty, Note n e')
  where
    (dmd_ty, e') = dmdAnal sigs dmd' e	
    dmd' = case n of
	     Coerce _ _ -> Eval	  -- This coerce usually arises from a recursive
	     other	-> dmd 	  -- newtype, and we don't want to look inside them
				  -- for exactly the same reason that we don't look
				  -- inside recursive products -- we might not reach
				  -- a fixpoint.  So revert to a vanilla Eval demand

dmdAnal sigs dmd (App fun (Type ty))
  = (fun_ty, App fun' (Type ty))
  where
    (fun_ty, fun') = dmdAnal sigs dmd fun

-- Lots of the other code is there to make this
-- beautiful, compositional, application rule :-)
dmdAnal sigs dmd e@(App fun arg)	-- Non-type arguments
  = let				-- [Type arg handled above]
	(fun_ty, fun') 	  = dmdAnal sigs (Call dmd) fun
	(arg_ty, arg') 	  = dmdAnal sigs arg_dmd arg
	(arg_dmd, res_ty) = splitDmdTy fun_ty
    in
    (res_ty `bothType` arg_ty, App fun' arg')

dmdAnal sigs dmd (Lam var body)
  | isTyVar var
  = let   
	(body_ty, body') = dmdAnal sigs dmd body
    in
    (body_ty, Lam var body')

  | Call body_dmd <- dmd	-- A call demand: good!
  = let	
	(body_ty, body') = dmdAnal sigs body_dmd body
	(lam_ty, var')   = annotateLamIdBndr body_ty var
    in
    (lam_ty, Lam var' body')

  | otherwise	-- Not enough demand on the lambda; but do the body
  = let		-- anyway to annotate it and gather free var info
	(body_ty, body') = dmdAnal sigs Eval body
	(lam_ty, var')   = annotateLamIdBndr body_ty var
    in
    (deferType lam_ty, Lam var' body')

dmdAnal sigs dmd (Case scrut case_bndr [alt@(DataAlt dc,bndrs,rhs)])
  | let tycon = dataConTyCon dc,
    isProductTyCon tycon,
    not (isRecursiveTyCon tycon)
  = let
	sigs_alt	      = extendSigEnv NotTopLevel sigs case_bndr case_bndr_sig
	(alt_ty, alt')	      = dmdAnalAlt sigs_alt dmd alt
	(alt_ty1, case_bndr') = annotateBndr alt_ty case_bndr
	(_, bndrs', _)	      = alt'
	case_bndr_sig	      = StrictSig (mkDmdType emptyVarEnv [] RetCPR)
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
	--	dead_case_bndr		 = isAbsentDmd (idNewDemandInfo case_bndr')
	--	keepity | dead_case_bndr = Drop
	--		| otherwise	 = Keep		
	--
	-- But then consider
	--	case x of y { (a,b) -> h y + a }
	-- where h : U(LL) -> T
	-- The above code would compute a Keep for x, since y is not Abs, which is silly
	-- The insight is, of course, that a demand on y is a demand on the
	-- scrutinee, so we need to `both` it with the scrut demand

        scrut_dmd 	   = mkSeq Drop [idNewDemandInfo b | b <- bndrs', isId b]
				   `both`
			     idNewDemandInfo case_bndr'

	(scrut_ty, scrut') = dmdAnal sigs scrut_dmd scrut
    in
    (alt_ty1 `bothType` scrut_ty, Case scrut' case_bndr' [alt'])

dmdAnal sigs dmd (Case scrut case_bndr alts)
  = let
	(alt_tys, alts')        = mapAndUnzip (dmdAnalAlt sigs dmd) alts
	(scrut_ty, scrut')      = dmdAnal sigs Eval scrut
	(alt_ty, case_bndr')	= annotateBndr (foldr1 lubType alt_tys) case_bndr
    in
--    pprTrace "dmdAnal:Case" (ppr alts $$ ppr alt_tys)
    (alt_ty `bothType` scrut_ty, Case scrut' case_bndr' alts')

dmdAnal sigs dmd (Let (NonRec id rhs) body) 
  = let
	(sigs', lazy_fv, (id1, rhs')) = dmdAnalRhs NotTopLevel sigs (id, rhs)
	(body_ty, body') 	      = dmdAnal sigs' dmd body
	(body_ty1, id2)    	      = annotateBndr body_ty id1
	body_ty2		      = addLazyFVs body_ty1 lazy_fv
    in
    (let vanilla_dmd = vanillaCall (idArity id)
	 actual_dmd  = idNewDemandInfo id2
     in
     if not (vanilla_dmd `betterDemand` actual_dmd) then
	pprTrace "dmdLet: better demand" (ppr id <+> vcat [text "vanilla" <+> ppr vanilla_dmd,
							   text "actual" <+> ppr actual_dmd])
     else \x -> x)
    (body_ty2, Let (NonRec id2 rhs') body')    

dmdAnal sigs dmd (Let (Rec pairs) body) 
  = let
	bndrs			 = map fst pairs
	(sigs', lazy_fv, pairs') = dmdFix NotTopLevel sigs pairs
	(body_ty, body')         = dmdAnal sigs' dmd body
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


dmdAnalAlt sigs dmd (con,bndrs,rhs) 
  = let 
	(rhs_ty, rhs')   = dmdAnal sigs dmd rhs
	(alt_ty, bndrs') = annotateBndrs rhs_ty bndrs
    in
    (alt_ty, (con, bndrs', rhs'))
\end{code}

%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}
dmdFix :: TopLevelFlag
       -> SigEnv 		-- Does not include bindings for this binding
       -> [(Id,CoreExpr)]
       -> (SigEnv, DmdEnv,
	   [(Id,CoreExpr)])	-- Binders annotated with stricness info

dmdFix top_lvl sigs orig_pairs
  = loop 1 initial_sigs orig_pairs
  where
    bndrs        = map fst orig_pairs
    initial_sigs = extendSigEnvList sigs [(id, (initial_sig id, top_lvl)) | id <- bndrs]
    
    loop :: Int
	 -> SigEnv			-- Already contains the current sigs
	 -> [(Id,CoreExpr)] 		
	 -> (SigEnv, DmdEnv, [(Id,CoreExpr)])
    loop n sigs pairs
      | all (same_sig sigs sigs') bndrs 
      = (sigs', lazy_fv, pairs')
		-- Note: use pairs', not pairs.   pairs' is the result of 
		-- processing the RHSs with sigs (= sigs'), whereas pairs 
		-- is the result of processing the RHSs with the *previous* 
		-- iteration of sigs.
      | n >= 10       = pprTrace "dmdFix loop" (ppr n <+> (vcat 
				[ text "Sigs:" <+> ppr [(id,lookup sigs id, lookup sigs' id) | (id,_) <- pairs],
				  text "env:" <+> ppr (ufmToList sigs),
				  text "binds:" <+> pprCoreBinding (Rec pairs)]))
			      (emptySigEnv, emptyDmdEnv, orig_pairs)	-- Safe output
      | otherwise    = loop (n+1) sigs' pairs'
      where
		-- Use the new signature to do the next pair
		-- The occurrence analyser has arranged them in a good order
		-- so this can significantly reduce the number of iterations needed
	((sigs',lazy_fv), pairs') = mapAccumL (my_downRhs top_lvl) (sigs, emptyDmdEnv) pairs
	
    my_downRhs top_lvl (sigs,lazy_fv) (id,rhs)
	= -- pprTrace "downRhs {" (ppr id <+> (ppr old_sig))
	  -- (new_sig `seq` 
	  --    pprTrace "downRhsEnd" (ppr id <+> ppr new_sig <+> char '}' ) 
	  ((sigs', lazy_fv'), pair')
	  --	 )
 	where
	  (sigs', lazy_fv1, pair') = dmdAnalRhs top_lvl sigs (id,rhs)
	  lazy_fv'		   = plusUFM_C both lazy_fv lazy_fv1   
	  -- old_sig   		   = lookup sigs id
	  -- new_sig  	   	   = lookup sigs' id
	   
	-- Get an initial strictness signature from the Id
	-- itself.  That way we make use of earlier iterations
	-- of the fixpoint algorithm.  (Cunning plan.)
	-- Note that the cunning plan extends to the DmdEnv too,
	-- since it is part of the strictness signature
    initial_sig id = idNewStrictness_maybe id `orElse` botSig

    same_sig sigs sigs' var = lookup sigs var == lookup sigs' var
    lookup sigs var = case lookupVarEnv sigs var of
			Just (sig,_) -> sig

dmdAnalRhs :: TopLevelFlag 
	-> SigEnv -> (Id, CoreExpr)
	-> (SigEnv,  DmdEnv, (Id, CoreExpr))
-- Process the RHS of the binding, add the strictness signature
-- to the Id, and augment the environment with the signature as well.

dmdAnalRhs top_lvl sigs (id, rhs)
 = (sigs', lazy_fv, (id', rhs'))
 where
  arity		     = idArity id   -- The idArity should be up to date
				    -- The simplifier was run just beforehand
  (rhs_dmd_ty, rhs') = dmdAnal sigs (vanillaCall arity) rhs
  (lazy_fv, sig_ty)  = WARN( arity /= dmdTypeDepth rhs_dmd_ty, ppr id )
		       mkSigTy id rhs rhs_dmd_ty
  id'		     = id `setIdNewStrictness` sig_ty
  sigs'		     = extendSigEnv top_lvl sigs id sig_ty
\end{code}

%************************************************************************
%*									*
\subsection{Strictness signatures and types}
%*									*
%************************************************************************

\begin{code}
mkTopSigTy :: CoreExpr -> DmdType -> StrictSig
	-- Take a DmdType and turn it into a StrictSig
	-- NB: not used for never-inline things; hence False
mkTopSigTy rhs dmd_ty = snd (mk_sig_ty False False rhs dmd_ty)

mkSigTy :: Id -> CoreExpr -> DmdType -> (DmdEnv, StrictSig)
mkSigTy id rhs dmd_ty = mk_sig_ty (isNeverActive (idInlinePragma id))
				  (isStrictDmd (idNewDemandInfo id))
				  rhs dmd_ty

mk_sig_ty never_inline strictly_demanded rhs (DmdType fv dmds res) 
  | never_inline && not (isBotRes res)
	-- 			HACK ALERT
	-- Don't strictness-analyse NOINLINE things.  Why not?  Because
	-- the NOINLINE says "don't expose any of the inner workings at the call 
	-- site" and the strictness is certainly an inner working.
	--
	-- More concretely, the demand analyser discovers the following strictness
	-- for unsafePerformIO:  C(U(AV))
	-- But then consider
	--	unsafePerformIO (\s -> let r = f x in 
	--			       case writeIORef v r s of (# s1, _ #) ->
	--			       (# s1, r #)
	-- The strictness analyser will find that the binding for r is strict,
	-- (becuase of uPIO's strictness sig), and so it'll evaluate it before 
	-- doing the writeIORef.  This actually makes tests/lib/should_run/memo002
	-- get a deadlock!  
	--
	-- Solution: don't expose the strictness of unsafePerformIO.
	--
	-- But we do want to expose the strictness of error functions, 
	-- which are also often marked NOINLINE
	--	{-# NOINLINE foo #-}
	--	foo x = error ("wubble buggle" ++ x)
	-- So (hack, hack) we only drop the strictness for non-bottom things
	-- This is all very unsatisfactory.
  = (deferEnv fv, topSig)

  | otherwise
  = (lazy_fv, mkStrictSig dmd_ty)
  where
    dmd_ty = DmdType strict_fv final_dmds res'

    lazy_fv   = filterUFM (not . isStrictDmd) fv
    strict_fv = filterUFM isStrictDmd         fv
	-- We put the strict FVs in the DmdType of the Id, so 
	-- that at its call sites we unleash demands on its strict fvs.
	-- An example is 'roll' in imaginary/wheel-sieve2
	-- Something like this:
	--	roll x = letrec 
	--		     go y = if ... then roll (x-1) else x+1
	--		 in 
	--		 go ms
	-- We want to see that roll is strict in x, which is because
	-- go is called.   So we put the DmdEnv for x in go's DmdType.
	--
	-- Another example:
	--	f :: Int -> Int -> Int
	--	f x y = let t = x+1
	--	    h z = if z==0 then t else 
	--		  if z==1 then x+1 else
	--		  x + h (z-1)
	--	in
	--	h y
	-- Calling h does indeed evaluate x, but we can only see
	-- that if we unleash a demand on x at the call site for t.
	--
	-- Incidentally, here's a place where lambda-lifting h would
	-- lose the cigar --- we couldn't see the joint strictness in t/x
	--
	--	ON THE OTHER HAND
	-- We don't want to put *all* the fv's from the RHS into the
	-- DmdType, because that makes fixpointing very slow --- the 
	-- DmdType gets full of lazy demands that are slow to converge.

    final_dmds = setUnpackStrategy dmds
	-- Set the unpacking strategy
	
    res' = case res of
		RetCPR | ignore_cpr_info -> TopRes
		other	 		 -> res
    ignore_cpr_info = is_thunk && not strictly_demanded
    is_thunk	    = not (exprIsValue rhs)
	-- If the rhs is a thunk, we forget the CPR info, because
	-- it is presumably shared (else it would have been inlined, and 
	-- so we'd lose sharing if w/w'd it into a function.
	--
	-- Also, if the strictness analyser has figured out (in a previous iteration)
	-- that it's strict, the let-to-case transformation will happen, so again 
	-- it's good.
	-- This made a big difference to PrelBase.modInt, which had something like
	--	modInt = \ x -> let r = ... -> I# v in
	--			...body strict in r...
	-- r's RHS isn't a value yet; but modInt returns r in various branches, so
	-- if r doesn't have the CPR property then neither does modInt
	-- Another case I found in practice (in Complex.magnitude), looks like this:
	-- 		let k = if ... then I# a else I# b
	--		in ... body strict in k ....
	-- (For this example, it doesn't matter whether k is returned as part of
	-- the overall result.)  Left to itself, the simplifier will make a join
	-- point thus:
	--		let $j k = ...body strict in k...
	--		if ... then $j (I# a) else $j (I# b)
	-- 
	--
	-- The difficulty with this is that we need the strictness type to
	-- look at the body... but we now need the body to calculate the demand
	-- on the variable, so we can decide whether its strictness type should
	-- have a CPR in it or not.  Simple solution: 
	--	a) use strictness info from the previous iteration
	--	b) make sure we do at least 2 iterations, by doing a second
	--	   round for top-level non-recs.  Top level recs will get at
	--	   least 2 iterations except for totally-bottom functions
	--	   which aren't very interesting anyway.
	--
	-- NB: strictly_demanded is never true of a top-level Id, or of a recursive Id.
\end{code}

The unpack strategy determines whether we'll *really* unpack the argument,
or whether we'll just remember its strictness.  If unpacking would give
rise to a *lot* of worker args, we may decide not to unpack after all.

\begin{code}
setUnpackStrategy :: [Demand] -> [Demand]
setUnpackStrategy ds
  = snd (go (opt_MaxWorkerArgs - nonAbsentArgs ds) ds)
  where
    go :: Int 			-- Max number of args available for sub-components of [Demand]
       -> [Demand]
       -> (Int, [Demand])	-- Args remaining after subcomponents of [Demand] are unpacked

    go n (Seq keep cs : ds) 
	| n' >= 0    = Seq keep cs' `cons` go n'' ds
        | otherwise  = Eval `cons` go n ds
	where
	  (n'',cs') = go n' cs
	  n' = n + box - non_abs_args
	  box = case keep of
		   Keep -> 0
		   Drop -> 1	-- Add one to the budget if we drop the top-level arg
	  non_abs_args = nonAbsentArgs cs
		-- Delete # of non-absent args to which we'll now be committed
				
    go n (d:ds) = d `cons` go n ds
    go n []     = (n,[])

    cons d (n,ds) = (n, d:ds)

nonAbsentArgs :: [Demand] -> Int
nonAbsentArgs []	 = 0
nonAbsentArgs (Abs : ds) = nonAbsentArgs ds
nonAbsentArgs (d   : ds) = 1 + nonAbsentArgs ds
\end{code}


%************************************************************************
%*									*
\subsection{Strictness signatures and types}
%*									*
%************************************************************************

\begin{code}
splitDmdTy :: DmdType -> (Demand, DmdType)
-- Split off one function argument
-- We already have a suitable demand on all
-- free vars, so no need to add more!
splitDmdTy (DmdType fv (dmd:dmds) res_ty) = (dmd, DmdType fv dmds res_ty)
splitDmdTy ty@(DmdType fv [] TopRes)      = (Lazy, ty)
splitDmdTy ty@(DmdType fv [] BotRes)      = (Bot,  ty)
	-- NB: Bot not Abs
splitDmdTy ty@(DmdType fv [] RetCPR)   	  = panic "splitDmdTy"
	-- We should not be applying a product as a function!
\end{code}

\begin{code}
unitVarDmd var dmd = DmdType (unitVarEnv var dmd) [] TopRes

addVarDmd top_lvl dmd_ty@(DmdType fv ds res) var dmd
  | isTopLevel top_lvl = dmd_ty		-- Don't record top level things
  | otherwise	       = DmdType (extendVarEnv fv var dmd) ds res

addLazyFVs (DmdType fv ds res) lazy_fvs
  = DmdType both_fv1 ds res
  where
    both_fv = (plusUFM_C both fv lazy_fvs)
    both_fv1 = modifyEnv (isBotRes res) (`both` Bot) lazy_fvs fv both_fv
	-- This modifyEnv is vital.  Consider
	--	let f = \x -> (x,y)
	--	in  error (f 3)
	-- Here, y is treated as a lazy-fv of f, but we must `both` that L
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

annotateBndr :: DmdType -> Var -> (DmdType, Var)
-- The returned env has the var deleted
-- The returned var is annotated with demand info
-- No effect on the argument demands
annotateBndr dmd_ty@(DmdType fv ds res) var
  | isTyVar var = (dmd_ty, var)
  | otherwise   = (DmdType fv' ds res, 
		   setIdNewDemandInfo var (argDemand var dmd))
  where
    (fv', dmd) = removeFV fv var res

annotateBndrs = mapAccumR annotateBndr

annotateLamIdBndr dmd_ty@(DmdType fv ds res) id
-- For lambdas we add the demand to the argument demands
-- Only called for Ids
  = ASSERT( isId id )
    (DmdType fv' (hacked_dmd:ds) res, setIdNewDemandInfo id hacked_dmd)
  where
    (fv', dmd) = removeFV fv id res
    hacked_dmd = argDemand id dmd
	-- This call to argDemand is vital, because otherwise we label
	-- a lambda binder with demand 'B'.  But in terms of calling
	-- conventions that's Abs, because we don't pass it.  But
	-- when we do a w/w split we get
	--	fw x = (\x y:B -> ...) x (error "oops")
	-- And then the simplifier things the 'B' is a strict demand
	-- and evaluates the (error "oops").  Sigh

removeFV fv var res = (fv', dmd)
		where
		  fv' = fv `delVarEnv` var
		  dmd = lookupVarEnv fv var `orElse` deflt
	 	  deflt | isBotRes res = Bot
		        | otherwise    = Abs
\end{code}

%************************************************************************
%*									*
\subsection{Strictness signatures}
%*									*
%************************************************************************

\begin{code}
type SigEnv  = VarEnv (StrictSig, TopLevelFlag)
	-- We use the SigEnv to tell us whether to
	-- record info about a variable in the DmdEnv
	-- We do so if it's a LocalId, but not top-level
	--
	-- The DmdEnv gives the demand on the free vars of the function
	-- when it is given enough args to satisfy the strictness signature

emptySigEnv  = emptyVarEnv

extendSigEnv :: TopLevelFlag -> SigEnv -> Id -> StrictSig -> SigEnv
extendSigEnv top_lvl env var sig = extendVarEnv env var (sig, top_lvl)

extendSigEnvList = extendVarEnvList

dmdTransform :: SigEnv		-- The strictness environment
	     -> Id		-- The function
	     -> Demand		-- The demand on the function
	     -> DmdType		-- The demand type of the function in this context
	-- Returned DmdEnv includes the demand on 
	-- this function plus demand on its free variables

dmdTransform sigs var dmd

------ 	DATA CONSTRUCTOR
  | isDataConId var,		-- Data constructor
    Seq k ds <- res_dmd		-- and the demand looks inside its fields
  = let 
	StrictSig dmd_ty    = idNewStrictness var	-- It must have a strictness sig
	DmdType _ _ con_res = dmd_ty
	arity		    = idArity var
    in
    if arity == call_depth then		-- Saturated, so unleash the demand
	let 
		-- ds can be empty, when we are just seq'ing the thing
		-- If so we must make up a suitable bunch of demands
	   dmd_ds | null ds   = replicate arity Abs
		  | otherwise = ASSERT( ds `lengthIs` arity ) ds

	   arg_ds = case k of
			Keep  -> bothLazy_s dmd_ds
			Drop  -> dmd_ds
			Defer -> pprTrace "dmdTransform: surprising!" (ppr var) 
					-- I don't think this can happen
				 dmd_ds
		-- Important!  If we Keep the constructor application, then
		-- we need the demands the constructor places (always lazy)
		-- If not, we don't need to.  For example:
		--	f p@(x,y) = (p,y)	-- S(AL)
		--	g a b     = f (a,b)
		-- It's vital that we don't calculate Absent for a!
	in
	mkDmdType emptyDmdEnv arg_ds con_res
		-- Must remember whether it's a product, hence con_res, not TopRes
    else
	topDmdType

------ 	IMPORTED FUNCTION
  | isGlobalId var,		-- Imported function
    let StrictSig dmd_ty = getNewStrictness var
  = if dmdTypeDepth dmd_ty <= call_depth then	-- Saturated, so unleash the demand
	dmd_ty
    else
	topDmdType

------ 	LOCAL LET/REC BOUND THING
  | Just (StrictSig dmd_ty, top_lvl) <- lookupVarEnv sigs var
  = let
	fn_ty | dmdTypeDepth dmd_ty <= call_depth = dmd_ty 
	      | otherwise   		          = deferType dmd_ty
	-- NB: it's important to use deferType, and not just return topDmdType
	-- Consider	let { f x y = p + x } in f 1
	-- The application isn't saturated, but we must nevertheless propagate 
	--	a lazy demand for p!  
    in
    addVarDmd top_lvl fn_ty var dmd

------ 	LOCAL NON-LET/REC BOUND THING
  | otherwise	 		-- Default case
  = unitVarDmd var dmd

  where
    (call_depth, res_dmd) = splitCallDmd dmd
\end{code}


%************************************************************************
%*									*
\subsection{Demands}
%*									*
%************************************************************************

\begin{code}
splitCallDmd :: Demand -> (Int, Demand)
splitCallDmd (Call d) = case splitCallDmd d of
			  (n, r) -> (n+1, r)
splitCallDmd d	      = (0, d)

vanillaCall :: Arity -> Demand
vanillaCall 0 = Eval
vanillaCall n = Call (vanillaCall (n-1))

deferType :: DmdType -> DmdType
deferType (DmdType fv _ _) = DmdType (deferEnv fv) [] TopRes
	-- Notice that we throw away info about both arguments and results
	-- For example,   f = let ... in \x -> x
	-- We don't want to get a stricness type V->T for f.
	-- Peter??

deferEnv :: DmdEnv -> DmdEnv
deferEnv fv = mapVarEnv defer fv

---------------
bothLazy :: Demand -> Demand
bothLazy   = both Lazy
bothLazy_s :: [Demand] -> [Demand]
bothLazy_s = map bothLazy


----------------
argDemand :: Id -> Demand -> Demand
argDemand id dmd | isUnLiftedType (idType id) = unliftedArgDemand dmd
		 | otherwise	    	      = liftedArgDemand   dmd

liftedArgDemand :: Demand -> Demand
-- The 'Defer' demands are just Lazy at function boundaries
-- Ugly!  Ask John how to improve it.
liftedArgDemand (Seq Defer ds) = Lazy
liftedArgDemand (Seq k     ds) = Seq k (map liftedArgDemand ds)
					-- Urk! Don't have type info here
liftedArgDemand Err	       = Eval	-- Args passed to a bottoming function
liftedArgDemand Bot	       = Abs	-- Don't pass args that are consumed by bottom/err
liftedArgDemand d	       = d

unliftedArgDemand :: Demand -> Demand
-- Same idea, but for unlifted types the domain is much simpler:
-- Either we use it (Lazy) or we don't (Abs)
unliftedArgDemand Bot   = Abs
unliftedArgDemand Abs   = Abs
unliftedArgDemand other = Lazy
\end{code}

\begin{code}
betterStrictness :: StrictSig -> StrictSig -> Bool
betterStrictness (StrictSig t1) (StrictSig t2) = betterDmdType t1 t2

betterDmdType t1 t2 = (t1 `lubType` t2) == t2

betterDemand :: Demand -> Demand -> Bool
-- If d1 `better` d2, and d2 `better` d2, then d1==d2
betterDemand d1 d2 = (d1 `lub` d2) == d2

squashDmdEnv (StrictSig (DmdType fv ds res)) = StrictSig (DmdType emptyDmdEnv ds res)
\end{code}

\begin{code}
-------------------------
-- Consider (if x then y else []) with demand V
-- Then the first branch gives {y->V} and the second
-- *implicitly* has {y->A}.  So we must put {y->(V `lub` A)}
-- in the result env.
lubType (DmdType fv1 ds1 r1) (DmdType fv2 ds2 r2)
  = DmdType lub_fv2 (zipWith lub ds1 ds2) (r1 `lubRes` r2)
  where
    lub_fv  = plusUFM_C lub fv1 fv2
    lub_fv1 = modifyEnv (not (isBotRes r1)) defer fv2 fv1 lub_fv
    lub_fv2 = modifyEnv (not (isBotRes r2)) defer fv1 fv2 lub_fv1
	-- lub is the identity for Bot

-----------------------------------
-- (t1 `bothType` t2) takes the argument/result info from t1,
-- using t2 just for its free-var info
-- NB: Don't forget about r2!  It might be BotRes, which is
--     a bottom demand on all the in-scope variables.
-- Peter: can this be done more neatly?
bothType (DmdType fv1 ds1 r1) (DmdType fv2 ds2 r2)
  = DmdType both_fv2 ds1 (r1 `bothRes` r2)
  where
    both_fv  = plusUFM_C both fv1 fv2
    both_fv1 = modifyEnv (isBotRes r1) (`both` Bot) fv2 fv1 both_fv
    both_fv2 = modifyEnv (isBotRes r2) (`both` Bot) fv1 fv2 both_fv1
	-- both is the identity for Abs
\end{code}


\begin{code}
lubRes BotRes r      = r
lubRes r      BotRes = r
lubRes RetCPR RetCPR = RetCPR
lubRes r1     r2     = TopRes

-- If either diverges, the whole thing does
-- Otherwise take CPR info from the first
bothRes r1 BotRes = BotRes
bothRes r1 r2     = r1
\end{code}

\begin{code}
-- A Seq can have an empty list of demands, in the polymorphic case.
lubs [] ds2 = ds2
lubs ds1 [] = ds1
lubs ds1 ds2 = ASSERT( equalLength ds1 ds2 ) zipWith lub ds1 ds2

-----------------------------------
-- A Seq can have an empty list of demands, in the polymorphic case.
boths [] ds2  = ds2
boths ds1 []  = ds1
boths ds1 ds2 = ASSERT( equalLength ds1 ds2 ) zipWith both ds1 ds2
\end{code}

\begin{code}
modifyEnv :: Bool			-- No-op if False
	  -> (Demand -> Demand)		-- The zapper
	  -> DmdEnv -> DmdEnv		-- Env1 and Env2
	  -> DmdEnv -> DmdEnv		-- Transform this env
	-- Zap anything in Env1 but not in Env2
	-- Assume: dom(env) includes dom(Env1) and dom(Env2)

modifyEnv need_to_modify zapper env1 env2 env
  | need_to_modify = foldr zap env (keysUFM (env1 `minusUFM` env2))
  | otherwise	   = env
  where
    zap uniq env = addToUFM_Directly env uniq (zapper current_val)
		 where
		   current_val = expectJust "modifyEnv" (lookupUFM_Directly env uniq)
\end{code}


%************************************************************************
%*									*
\subsection{LUB and BOTH}
%*									*
%************************************************************************


\begin{code}
lub :: Demand -> Demand -> Demand

lub Bot d = d

lub Err Bot        = Err 
lub Err Abs        = Lazy	-- E.g. f x = if ... then True else error x
lub Err (Seq k ds) 
  | null ds	   = Seq (case k of { Drop -> Keep; other -> k }) []
			-- Yuk
  | not (null ds)  = Seq k [Err `lub` d | d <- ds]
			-- E.g. f x = if ... then fst x else error x
			-- We *cannot* use the (lub Err d = d) case,
			-- else we'd get U(VA) for x's demand!!
lub Err d   	   = d 

lub Lazy d = Lazy

lub Abs  d = defer d

lub Eval Abs		   	       = Lazy
lub Eval Lazy		   	       = Lazy
lub Eval (Seq Defer ds)		       = Lazy	-- Essential!
lub Eval (Seq Drop ds) | not (null ds) = Seq Drop [Lazy | d <- ds]
lub Eval d   		   	       = Eval
	-- For the Seq Drop case, consider
	--	f n []     = n
	--	f n (x:xs) = f (n+x) xs
	-- Here we want to do better than just V for n.  It's
	-- unboxed in the (x:xs) case, and we might be prepared to
	-- rebox it in the [] case.
	-- But if we don't use *any* of the components, give up
	-- and revert to V

lub (Call d1) (Call d2) = Call (lub d1 d2)
lub d1@(Call _) d2	= d2 `lub` d1

lub (Seq k1 ds1) (Seq k2 ds2)
  = Seq (k1 `lub_keep` k2) (lub_ds k1 ds1 k2 ds2)
  where
	------------------
    lub_ds Keep ds1 Keep ds2		     = ds1 `lubs` ds2
    lub_ds Keep ds1 non_keep ds2 | null ds1  = [Lazy | d <- ds2]
			         | otherwise = bothLazy_s ds1 `lubs` ds2

    lub_ds non_keep ds1 Keep ds2 | null ds2  = [Lazy | d <- ds1]
			         | otherwise = ds1 `lubs` bothLazy_s ds2

    lub_ds k1 ds1 k2 ds2		     = ds1 `lubs` ds2

	------------------
	-- Note that (Keep `lub` Drop) is Drop, not Keep
	-- Why not?  See the example above with (lub Eval d).
    lub_keep Keep k     = k

    lub_keep Drop Defer = Defer
    lub_keep Drop k	= Drop

    lub_keep Defer k	= Defer

lub d1@(Seq _ _) d2 = d2 `lub` d1


defer :: Demand -> Demand
-- Computes (Abs `lub` d)
-- For the Bot case consider
--	f x y = if ... then x else error x
--   Then for y we get Abs `lub` Bot, and we really
--   want Abs overall
defer Bot	    = Abs
defer Abs	    = Abs
defer (Seq Keep ds) = Lazy
defer (Seq _    ds) = Seq Defer ds
defer d	   	    = Lazy

---------------
both :: Demand -> Demand -> Demand

both Bot Bot 	    = Bot
both Bot Abs	    = Bot
both Bot (Seq k ds) 
  | not (null ds)   = Seq (case k of { Defer -> Drop; other -> k })
			  [both Bot d | d <- ds]
	-- E.g. f x = if ... then error (fst x) else fst x
	-- This equation helps results slightly, 
	-- but is not necessary for soundness
both Bot d   	    = Err

both Err d = Err

both Abs d   = d

both Lazy Bot 	     	 = Err
both Lazy Err 	     	 = Err
both Lazy Eval 	     	 = Eval
both Lazy (Call d)       = Call d
both Lazy (Seq Defer ds) = Lazy
both Lazy (Seq k ds)     = Seq Keep ds
both Lazy d	         = Lazy

-- For the (Eval `both` Bot) case, consider
--	f x = error x
-- From 'error' itself we get demand Bot on x
-- From the arg demand on x we get Eval
-- So we want Eval `both` Bot to be Err.
-- That's what Err is *for*
both Eval Bot	     = Err
both Eval Err	     = Err
both Eval (Seq k ds) = Seq Keep ds
both Eval d	     = Eval

both (Call d1)   (Call d2) = Call (d1 `both` d2)
both d1@(Call _) d2	   = d2 `both` d1

both (Seq k1 ds1) (Seq k2 ds2)
  = Seq (k1 `both_keep` k2) (both_ds k1 ds1 k2 ds2)
  where
	----------------
    both_keep Keep k2 = Keep

    both_keep Drop Keep = Keep
    both_keep Drop k2   = Drop

    both_keep Defer k2  = k2

	----------------
    both_ds Defer ds1 Defer     ds2 = ds1 `boths` ds2
    both_ds Defer ds1 non_defer ds2 = map defer ds1 `boths` ds2

    both_ds non_defer ds1 Defer ds2 = ds1 `boths` map defer ds2

    both_ds k1 ds1 k2 ds2	    = ds1 `boths` ds2

both d1@(Seq _ _) d2 = d2 `both` d1
\end{code}


%************************************************************************
%*									*
\subsection{Miscellaneous
%*									*
%************************************************************************


\begin{code}
get_changes binds = vcat (map get_changes_bind binds)

get_changes_bind (Rec pairs) = vcat (map get_changes_pr pairs)
get_changes_bind (NonRec id rhs) = get_changes_pr (id,rhs)

get_changes_pr (id,rhs) 
  = get_changes_var id $$ get_changes_expr rhs

get_changes_var var
  | isId var  = get_changes_str var $$ get_changes_dmd var
  | otherwise = empty

get_changes_expr (Type t)     = empty
get_changes_expr (Var v)      = empty
get_changes_expr (Lit l)      = empty
get_changes_expr (Note n e)   = get_changes_expr e
get_changes_expr (App e1 e2)  = get_changes_expr e1 $$ get_changes_expr e2
get_changes_expr (Lam b e)    = {- get_changes_var b $$ -} get_changes_expr e
get_changes_expr (Let b e)    = get_changes_bind b $$ get_changes_expr e
get_changes_expr (Case e b a) = get_changes_expr e $$ {- get_changes_var b $$ -} vcat (map get_changes_alt a)

get_changes_alt (con,bs,rhs) = {- vcat (map get_changes_var bs) $$ -} get_changes_expr rhs

get_changes_str id
  | new_better && old_better = empty
  | new_better	       	     = message "BETTER"
  | old_better	       	     = message "WORSE"
  | otherwise	       	     = message "INCOMPARABLE" 
  where
    message word = text word <+> text "strictness for" <+> ppr id <+> info
    info = (text "Old" <+> ppr old) $$ (text "New" <+> ppr new)
    new = squashDmdEnv (idNewStrictness id)	-- Don't report diffs in the env
    old = newStrictnessFromOld id
    old_better = old `betterStrictness` new
    new_better = new `betterStrictness` old

get_changes_dmd id
  | isUnLiftedType (idType id) = empty	-- Not useful
  | new_better && old_better = empty
  | new_better	       	     = message "BETTER"
  | old_better	       	     = message "WORSE"
  | otherwise	       	     = message "INCOMPARABLE" 
  where
    message word = text word <+> text "demand for" <+> ppr id <+> info
    info = (text "Old" <+> ppr old) $$ (text "New" <+> ppr new)
    new = liftedArgDemand (idNewDemandInfo id)	-- To avoid spurious improvements
    old = newDemand (idDemandInfo id)
    new_better = new `betterDemand` old 
    old_better = old `betterDemand` new
\end{code}
