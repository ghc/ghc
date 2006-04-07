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

import DynFlags		( DynFlags, DynFlag(..) )
import StaticFlags	( opt_MaxWorkerArgs )
import NewDemand	-- All of it
import CoreSyn
import PprCore	
import CoreUtils	( exprIsHNF, exprIsTrivial, exprArity )
import DataCon		( dataConTyCon )
import TyCon		( isProductTyCon, isRecursiveTyCon )
import Id		( Id, idType, idInlinePragma,
			  isDataConWorkId, isGlobalId, idArity,
#ifdef OLD_STRICTNESS
			  idDemandInfo,  idStrictness, idCprInfo, idName,
#endif
			  idNewStrictness, idNewStrictness_maybe,
			  setIdNewStrictness, idNewDemandInfo,
			  idNewDemandInfo_maybe,
			  setIdNewDemandInfo
			)
#ifdef OLD_STRICTNESS
import IdInfo 		( newStrictnessFromOld, newDemand )
#endif
import Var		( Var )
import VarEnv
import TysWiredIn	( unboxedPairDataCon )
import TysPrim		( realWorldStatePrimTy )
import UniqFM		( plusUFM_C, addToUFM_Directly, lookupUFM_Directly,
			  keysUFM, minusUFM, ufmToList, filterUFM )
import Type		( isUnLiftedType, coreEqType )
import CoreLint		( showPass, endPass )
import Util		( mapAndUnzip, mapAccumL, mapAccumR, lengthIs )
import BasicTypes	( Arity, TopLevelFlag(..), isTopLevel, isNeverActive,
			  RecFlag(..), isRec )
import Maybes		( orElse, expectJust )
import Outputable
\end{code}

To think about

* set a noinline pragma on bottoming Ids

* Consider f x = x+1 `fatbar` error (show x)
  We'd like to unbox x, even if that means reboxing it in the error case.


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
	let { binds_plus_dmds = do_prog binds } ;

	endPass dflags "Demand analysis" 
	 	Opt_D_dump_stranal binds_plus_dmds ;
#ifdef OLD_STRICTNESS
	-- Only if OLD_STRICTNESS is on, because only then is the old
	-- strictness analyser run
	let { dmd_changes = get_changes binds_plus_dmds } ;
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
	(    _, _, (_,   rhs1)) = dmdAnalRhs TopLevel NonRecursive sigs (id, rhs)
	(sigs2, _, (id2, rhs2)) = dmdAnalRhs TopLevel NonRecursive sigs (id, rhs1)
		-- Do two passes to improve CPR information
		-- See comments with ignore_cpr_info in mk_sig_ty
		-- and with extendSigsWithLam
    in
    (sigs2, NonRec id2 rhs2)    

dmdAnalTopBind sigs (Rec pairs)
  = let
	(sigs', _, pairs')  = dmdFix TopLevel sigs pairs
		-- We get two iterations automatically
		-- c.f. the NonRec case above
    in
    (sigs', Rec pairs')
\end{code}

\begin{code}
dmdAnalTopRhs :: CoreExpr -> (StrictSig, CoreExpr)
-- Analyse the RHS and return
--	a) appropriate strictness info
--	b) the unfolding (decorated with stricntess info)
dmdAnalTopRhs rhs
  = (sig, rhs2)
  where
    call_dmd	   = vanillaCall (exprArity rhs)
    (_,      rhs1) = dmdAnal emptySigEnv call_dmd rhs
    (rhs_ty, rhs2) = dmdAnal emptySigEnv call_dmd rhs1
    sig		   = mkTopSigTy rhs rhs_ty
	-- Do two passes; see notes with extendSigsWithLam
	-- Otherwise we get bogus CPR info for constructors like
	-- 	newtype T a = MkT a
	-- The constructor looks like (\x::T a -> x), modulo the coerce
	-- extendSigsWithLam will optimistically give x a CPR tag the 
	-- first time, which is wrong in the end.
\end{code}

%************************************************************************
%*									*
\subsection{The analyser itself}	
%*									*
%************************************************************************

\begin{code}
dmdAnal :: SigEnv -> Demand -> CoreExpr -> (DmdType, CoreExpr)

dmdAnal sigs Abs  e = (topDmdType, e)

dmdAnal sigs dmd e 
  | not (isStrictDmd dmd)
  = let 
	(res_ty, e') = dmdAnal sigs evalDmd e
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
	     Coerce _ _ -> evalDmd  -- This coerce usually arises from a recursive
	     other	-> dmd 	    -- newtype, and we don't want to look inside them
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
	sigs'		 = extendSigsWithLam sigs var
	(body_ty, body') = dmdAnal sigs' body_dmd body
	(lam_ty, var')   = annotateLamIdBndr body_ty var
    in
    (lam_ty, Lam var' body')

  | otherwise	-- Not enough demand on the lambda; but do the body
  = let		-- anyway to annotate it and gather free var info
	(body_ty, body') = dmdAnal sigs evalDmd body
	(lam_ty, var')   = annotateLamIdBndr body_ty var
    in
    (deferType lam_ty, Lam var' body')

dmdAnal sigs dmd (Case scrut case_bndr ty [alt@(DataAlt dc,bndrs,rhs)])
  | let tycon = dataConTyCon dc,
    isProductTyCon tycon,
    not (isRecursiveTyCon tycon)
  = let
	sigs_alt	      = extendSigEnv NotTopLevel sigs case_bndr case_bndr_sig
	(alt_ty, alt')	      = dmdAnalAlt sigs_alt dmd alt
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

        scrut_dmd 	   = Eval (Prod [idNewDemandInfo b | b <- bndrs', isId b])
				   `both`
			     idNewDemandInfo case_bndr'

	(scrut_ty, scrut') = dmdAnal sigs scrut_dmd scrut
    in
    (alt_ty1 `bothType` scrut_ty, Case scrut' case_bndr' ty [alt'])

dmdAnal sigs dmd (Case scrut case_bndr ty alts)
  = let
	(alt_tys, alts')        = mapAndUnzip (dmdAnalAlt sigs dmd) alts
	(scrut_ty, scrut')      = dmdAnal sigs evalDmd scrut
	(alt_ty, case_bndr')	= annotateBndr (foldr1 lubType alt_tys) case_bndr
    in
--    pprTrace "dmdAnal:Case" (ppr alts $$ ppr alt_tys)
    (alt_ty `bothType` scrut_ty, Case scrut' case_bndr' ty alts')

dmdAnal sigs dmd (Let (NonRec id rhs) body) 
  = let
	(sigs', lazy_fv, (id1, rhs')) = dmdAnalRhs NotTopLevel NonRecursive sigs (id, rhs)
	(body_ty, body') 	      = dmdAnal sigs' dmd body
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
	final_alt_ty | io_hack_reqd = alt_ty `lubType` topDmdType
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
	-- A more complete example where this shows up is:
	--	do { let len = <expensive> ;
	--	   ; when (...) (exitWith ExitSuccess)
	--	   ; print len }

	io_hack_reqd = con == DataAlt unboxedPairDataCon &&
		       idType (head bndrs) `coreEqType` realWorldStatePrimTy
    in	
    (final_alt_ty, (con, bndrs', rhs'))
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
    initial_sigs = extendSigEnvList sigs [(id, (initialSig id, top_lvl)) | id <- bndrs]
    
    loop :: Int
	 -> SigEnv			-- Already contains the current sigs
	 -> [(Id,CoreExpr)] 		
	 -> (SigEnv, DmdEnv, [(Id,CoreExpr)])
    loop n sigs pairs
      | found_fixpoint
      = (sigs', lazy_fv, pairs')
		-- Note: use pairs', not pairs.   pairs' is the result of 
		-- processing the RHSs with sigs (= sigs'), whereas pairs 
		-- is the result of processing the RHSs with the *previous* 
		-- iteration of sigs.

      | n >= 10  = pprTrace "dmdFix loop" (ppr n <+> (vcat 
				[ text "Sigs:" <+> ppr [(id,lookup sigs id, lookup sigs' id) | (id,_) <- pairs],
				  text "env:" <+> ppr (ufmToList sigs),
				  text "binds:" <+> pprCoreBinding (Rec pairs)]))
			      (emptySigEnv, lazy_fv, orig_pairs)	-- Safe output
			-- The lazy_fv part is really important!  orig_pairs has no strictness
			-- info, including nothing about free vars.  But if we have
			--	letrec f = ....y..... in ...f...
			-- where 'y' is free in f, we must record that y is mentioned, 
			-- otherwise y will get recorded as absent altogether

      | otherwise    = loop (n+1) sigs' pairs'
      where
	found_fixpoint = all (same_sig sigs sigs') bndrs 
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
	  (sigs', lazy_fv1, pair') = dmdAnalRhs top_lvl Recursive sigs (id,rhs)
	  lazy_fv'		   = plusUFM_C both lazy_fv lazy_fv1   
	  -- old_sig   		   = lookup sigs id
	  -- new_sig  	   	   = lookup sigs' id
	   
    same_sig sigs sigs' var = lookup sigs var == lookup sigs' var
    lookup sigs var = case lookupVarEnv sigs var of
			Just (sig,_) -> sig

	-- Get an initial strictness signature from the Id
	-- itself.  That way we make use of earlier iterations
	-- of the fixpoint algorithm.  (Cunning plan.)
	-- Note that the cunning plan extends to the DmdEnv too,
	-- since it is part of the strictness signature
initialSig id = idNewStrictness_maybe id `orElse` botSig

dmdAnalRhs :: TopLevelFlag -> RecFlag
	-> SigEnv -> (Id, CoreExpr)
	-> (SigEnv,  DmdEnv, (Id, CoreExpr))
-- Process the RHS of the binding, add the strictness signature
-- to the Id, and augment the environment with the signature as well.

dmdAnalRhs top_lvl rec_flag sigs (id, rhs)
 = (sigs', lazy_fv, (id', rhs'))
 where
  arity		     = idArity id   -- The idArity should be up to date
				    -- The simplifier was run just beforehand
  (rhs_dmd_ty, rhs') = dmdAnal sigs (vanillaCall arity) rhs
  (lazy_fv, sig_ty)  = WARN( arity /= dmdTypeDepth rhs_dmd_ty && not (exprIsTrivial rhs), ppr id )
				-- The RHS can be eta-reduced to just a variable, 
				-- in which case we should not complain. 
		       mkSigTy top_lvl rec_flag id rhs rhs_dmd_ty
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

mkSigTy :: TopLevelFlag -> RecFlag -> Id -> CoreExpr -> DmdType -> (DmdEnv, StrictSig)
mkSigTy top_lvl rec_flag id rhs dmd_ty 
  = mk_sig_ty never_inline thunk_cpr_ok rhs dmd_ty
  where
    never_inline = isNeverActive (idInlinePragma id)
    maybe_id_dmd = idNewDemandInfo_maybe id
	-- Is Nothing the first time round

    thunk_cpr_ok
	| isTopLevel top_lvl       = False	-- Top level things don't get
						-- their demandInfo set at all
	| isRec rec_flag	   = False	-- Ditto recursive things
	| Just dmd <- maybe_id_dmd = isStrictDmd dmd
	| otherwise 		   = True	-- Optimistic, first time round
						-- See notes below
\end{code}

The thunk_cpr_ok stuff [CPR-AND-STRICTNESS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the rhs is a thunk, we usually forget the CPR info, because
it is presumably shared (else it would have been inlined, and 
so we'd lose sharing if w/w'd it into a function.

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

The Nothing case in thunk_cpr_ok [CPR-AND-STRICTNESS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Demand info now has a 'Nothing' state, just like strictness info.
The analysis works from 'dangerous' towards a 'safe' state; so we 
start with botSig for 'Nothing' strictness infos, and we start with
"yes, it's demanded" for 'Nothing' in the demand info.  The
fixpoint iteration will sort it all out.

We can't start with 'not-demanded' because then consider
	f x = let 
		  t = ... I# x
	      in
	      if ... then t else I# y else f x'

In the first iteration we'd have no demand info for x, so assume
not-demanded; then we'd get TopRes for f's CPR info.  Next iteration
we'd see that t was demanded, and so give it the CPR property, but by
now f has TopRes, so it will stay TopRes.  Instead, with the Nothing
setting the first time round, we say 'yes t is demanded' the first
time.

However, this does mean that for non-recursive bindings we must
iterate twice to be sure of not getting over-optimistic CPR info,
in the case where t turns out to be not-demanded.  This is handled
by dmdAnalTopBind.


\begin{code}
mk_sig_ty never_inline thunk_cpr_ok rhs (DmdType fv dmds res) 
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
    ignore_cpr_info = not (exprIsHNF rhs || thunk_cpr_ok)
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

    go n (Eval (Prod cs) : ds) 
	| n' >= 0   = Eval (Prod cs') `cons` go n'' ds
        | otherwise = Box (Eval (Prod cs)) `cons` go n ds
	where
	  (n'',cs') = go n' cs
	  n' = n + 1 - non_abs_args
		-- Add one to the budget 'cos we drop the top-level arg
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
splitDmdTy ty@(DmdType fv [] res_ty)      = (resTypeArgDmd res_ty, ty)
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
  | otherwise   = (DmdType fv' ds res, setIdNewDemandInfo var dmd)
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
    hacked_dmd = argDemand dmd
	-- This call to argDemand is vital, because otherwise we label
	-- a lambda binder with demand 'B'.  But in terms of calling
	-- conventions that's Abs, because we don't pass it.  But
	-- when we do a w/w split we get
	--	fw x = (\x y:B -> ...) x (error "oops")
	-- And then the simplifier things the 'B' is a strict demand
	-- and evaluates the (error "oops").  Sigh

removeFV fv id res = (fv', zapUnlifted id dmd)
		where
		  fv' = fv `delVarEnv` id
		  dmd = lookupVarEnv fv id `orElse` deflt
	 	  deflt | isBotRes res = Bot
		        | otherwise    = Abs

-- For unlifted-type variables, we are only 
-- interested in Bot/Abs/Box Abs
zapUnlifted is Bot = Bot
zapUnlifted id Abs = Abs
zapUnlifted id dmd | isUnLiftedType (idType id) = lazyDmd
		   | otherwise			= dmd
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

extendSigsWithLam :: SigEnv -> Id -> SigEnv
-- Extend the SigEnv when we meet a lambda binder
-- If the binder is marked demanded with a product demand, then give it a CPR 
-- signature, because in the likely event that this is a lambda on a fn defn 
-- [we only use this when the lambda is being consumed with a call demand],
-- it'll be w/w'd and so it will be CPR-ish.  E.g.
--	f = \x::(Int,Int).  if ...strict in x... then
--				x
--			    else
--				(a,b)
-- We want f to have the CPR property because x does, by the time f has been w/w'd
--
-- Also note that we only want to do this for something that
-- definitely has product type, else we may get over-optimistic 
-- CPR results (e.g. from \x -> x!).

extendSigsWithLam sigs id
  = case idNewDemandInfo_maybe id of
	Nothing	              -> extendVarEnv sigs id (cprSig, NotTopLevel)
		-- Optimistic in the Nothing case;
		-- See notes [CPR-AND-STRICTNESS]
	Just (Eval (Prod ds)) -> extendVarEnv sigs id (cprSig, NotTopLevel)
	other                 -> sigs


dmdTransform :: SigEnv		-- The strictness environment
	     -> Id		-- The function
	     -> Demand		-- The demand on the function
	     -> DmdType		-- The demand type of the function in this context
	-- Returned DmdEnv includes the demand on 
	-- this function plus demand on its free variables

dmdTransform sigs var dmd

------ 	DATA CONSTRUCTOR
  | isDataConWorkId var		-- Data constructor
  = let 
	StrictSig dmd_ty    = idNewStrictness var	-- It must have a strictness sig
	DmdType _ _ con_res = dmd_ty
	arity		    = idArity var
    in
    if arity == call_depth then		-- Saturated, so unleash the demand
	let 
		-- Important!  If we Keep the constructor application, then
		-- we need the demands the constructor places (always lazy)
		-- If not, we don't need to.  For example:
		--	f p@(x,y) = (p,y)	-- S(AL)
		--	g a b     = f (a,b)
		-- It's vital that we don't calculate Absent for a!
	   dmd_ds = case res_dmd of
			Box (Eval ds) -> mapDmds box ds
			Eval ds	      -> ds
			other	      -> Poly Top

		-- ds can be empty, when we are just seq'ing the thing
		-- If so we must make up a suitable bunch of demands
	   arg_ds = case dmd_ds of
		      Poly d  -> replicate arity d
		      Prod ds -> ASSERT( ds `lengthIs` arity ) ds

	in
	mkDmdType emptyDmdEnv arg_ds con_res
		-- Must remember whether it's a product, hence con_res, not TopRes
    else
	topDmdType

------ 	IMPORTED FUNCTION
  | isGlobalId var,		-- Imported function
    let StrictSig dmd_ty = idNewStrictness var
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
vanillaCall 0 = evalDmd
vanillaCall n = Call (vanillaCall (n-1))

deferType :: DmdType -> DmdType
deferType (DmdType fv _ _) = DmdType (deferEnv fv) [] TopRes
	-- Notice that we throw away info about both arguments and results
	-- For example,   f = let ... in \x -> x
	-- We don't want to get a stricness type V->T for f.
	-- Peter??

deferEnv :: DmdEnv -> DmdEnv
deferEnv fv = mapVarEnv defer fv


----------------
argDemand :: Demand -> Demand
-- The 'Defer' demands are just Lazy at function boundaries
-- Ugly!  Ask John how to improve it.
argDemand Top 	    = lazyDmd
argDemand (Defer d) = lazyDmd
argDemand (Eval ds) = Eval (mapDmds argDemand ds)
argDemand (Box Bot) = evalDmd
argDemand (Box d)   = box (argDemand d)
argDemand Bot	    = Abs	-- Don't pass args that are consumed (only) by bottom
argDemand d	    = d
\end{code}

\begin{code}
-------------------------
-- Consider (if x then y else []) with demand V
-- Then the first branch gives {y->V} and the second
--  *implicitly* has {y->A}.  So we must put {y->(V `lub` A)}
-- in the result env.
lubType (DmdType fv1 ds1 r1) (DmdType fv2 ds2 r2)
  = DmdType lub_fv2 (lub_ds ds1 ds2) (r1 `lubRes` r2)
  where
    lub_fv  = plusUFM_C lub fv1 fv2
    lub_fv1 = modifyEnv (not (isBotRes r1)) absLub fv2 fv1 lub_fv
    lub_fv2 = modifyEnv (not (isBotRes r2)) absLub fv1 fv2 lub_fv1
	-- lub is the identity for Bot

	-- Extend the shorter argument list to match the longer
    lub_ds (d1:ds1) (d2:ds2) = lub d1 d2 : lub_ds ds1 ds2
    lub_ds []	    []	     = []
    lub_ds ds1	    []	     = map (`lub` resTypeArgDmd r2) ds1
    lub_ds []	    ds2	     = map (resTypeArgDmd r1 `lub`) ds2

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

lub Bot  	d2 = d2
lub Abs  	d2 = absLub d2
lub Top 	d2 = Top
lub (Defer ds1) d2 = defer (Eval ds1 `lub` d2)

lub (Call d1)   (Call d2)    = Call (d1 `lub` d2)
lub d1@(Call _) (Box d2)     = d1 `lub` d2	-- Just strip the box
lub d1@(Call _) d2@(Eval _)  = d2		-- Presumably seq or vanilla eval
lub d1@(Call _) d2	     = d2 `lub` d1	-- Bot, Abs, Top

-- For the Eval case, we use these approximation rules
-- Box Bot	 <= Eval (Box Bot ...)
-- Box Top	 <= Defer (Box Bot ...)
-- Box (Eval ds) <= Eval (map Box ds)
lub (Eval ds1)  (Eval ds2)  	  = Eval (ds1 `lubs` ds2)
lub (Eval ds1)  (Box Bot)   	  = Eval (mapDmds (`lub` Box Bot) ds1)
lub (Eval ds1)  (Box (Eval ds2)) = Eval (ds1 `lubs` mapDmds box ds2)
lub (Eval ds1)  (Box Abs)        = deferEval (mapDmds (`lub` Box Bot) ds1)
lub d1@(Eval _) d2	          = d2 `lub` d1	-- Bot,Abs,Top,Call,Defer

lub (Box d1)   (Box d2) = box (d1 `lub` d2)
lub d1@(Box _)  d2	= d2 `lub` d1

lubs = zipWithDmds lub

---------------------
-- box is the smart constructor for Box
-- It computes <B,bot> & d
-- INVARIANT: (Box d) => d = Bot, Abs, Eval
-- Seems to be no point in allowing (Box (Call d))
box (Call d)  = Call d	-- The odd man out.  Why?
box (Box d)   = Box d
box (Defer _) = lazyDmd
box Top       = lazyDmd	-- Box Abs and Box Top
box Abs       = lazyDmd	-- are the same <B,L>
box d 	      = Box d	-- Bot, Eval

---------------
defer :: Demand -> Demand

-- defer is the smart constructor for Defer
-- The idea is that (Defer ds) = <U(ds), L>
--
-- It specifies what happens at a lazy function argument
-- or a lambda; the L* operator
-- Set the strictness part to L, but leave
-- the boxity side unaffected
-- It also ensures that Defer (Eval [LLLL]) = L

defer Bot	 = Abs
defer Abs	 = Abs
defer Top	 = Top
defer (Call _)	 = lazyDmd	-- Approximation here?
defer (Box _)	 = lazyDmd
defer (Defer ds) = Defer ds
defer (Eval ds)  = deferEval ds

-- deferEval ds = defer (Eval ds)
deferEval ds | allTop ds = Top
	     | otherwise  = Defer ds

---------------------
absLub :: Demand -> Demand
-- Computes (Abs `lub` d)
-- For the Bot case consider
--	f x y = if ... then x else error x
--   Then for y we get Abs `lub` Bot, and we really
--   want Abs overall
absLub Bot  	  = Abs
absLub Abs  	  = Abs
absLub Top 	  = Top
absLub (Call _)   = Top
absLub (Box _)    = Top
absLub (Eval ds)  = Defer (absLubs ds)	-- Or (Defer ds)?
absLub (Defer ds) = Defer (absLubs ds)	-- Or (Defer ds)?

absLubs = mapDmds absLub

---------------
both :: Demand -> Demand -> Demand

both Abs d2 = d2

both Bot Bot 	   = Bot
both Bot Abs 	   = Bot 
both Bot (Eval ds) = Eval (mapDmds (`both` Bot) ds)
	-- Consider
	--	f x = error x
	-- From 'error' itself we get demand Bot on x
	-- From the arg demand on x we get 
	--	x :-> evalDmd = Box (Eval (Poly Abs))
	-- So we get  Bot `both` Box (Eval (Poly Abs))
	--	    = Seq Keep (Poly Bot)
	--
	-- Consider also
	--	f x = if ... then error (fst x) else fst x
	-- Then we get (Eval (Box Bot, Bot) `lub` Eval (SA))
	--	= Eval (SA)
	-- which is what we want.
both Bot d = errDmd

both Top Bot 	     = errDmd
both Top Abs 	     = Top
both Top Top 	     = Top
both Top (Box d)    = Box d
both Top (Call d)   = Call d
both Top (Eval ds)  = Eval (mapDmds (`both` Top) ds)
both Top (Defer ds) 	-- = defer (Top `both` Eval ds)
			-- = defer (Eval (mapDmds (`both` Top) ds))
		     = deferEval (mapDmds (`both` Top) ds)


both (Box d1) 	(Box d2)    = box (d1 `both` d2)
both (Box d1) 	d2@(Call _) = box (d1 `both` d2)
both (Box d1) 	d2@(Eval _) = box (d1 `both` d2)
both (Box d1) 	(Defer d2)  = Box d1
both d1@(Box _) d2	    = d2 `both` d1

both (Call d1) 	 (Call d2)   = Call (d1 `both` d2)
both (Call d1) 	 (Eval ds2)  = Call d1	-- Could do better for (Poly Bot)?
both (Call d1) 	 (Defer ds2) = Call d1	-- Ditto
both d1@(Call _) d2	     = d1 `both` d1

both (Eval ds1)    (Eval  ds2) = Eval (ds1 `boths` ds2)
both (Eval ds1)    (Defer ds2) = Eval (ds1 `boths` mapDmds defer ds2)
both d1@(Eval ds1) d2	       = d2 `both` d1

both (Defer ds1) (Defer ds2) = deferEval (ds1 `boths` ds2)
both d1@(Defer ds1) d2	     = d2 `both` d1
 
boths = zipWithDmds both
\end{code}



%************************************************************************
%*									*
\subsection{Miscellaneous
%*									*
%************************************************************************


\begin{code}
#ifdef OLD_STRICTNESS
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
    new = squashSig (idNewStrictness id)	-- Don't report spurious diffs that the old
						-- strictness analyser can't track
    old = newStrictnessFromOld (idName id) (idArity id) (idStrictness id) (idCprInfo id)
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
    new = squashDmd (argDemand (idNewDemandInfo id))	-- To avoid spurious improvements
							-- A bit of a hack
    old = newDemand (idDemandInfo id)
    new_better = new `betterDemand` old 
    old_better = old `betterDemand` new

betterStrictness :: StrictSig -> StrictSig -> Bool
betterStrictness (StrictSig t1) (StrictSig t2) = betterDmdType t1 t2

betterDmdType t1 t2 = (t1 `lubType` t2) == t2

betterDemand :: Demand -> Demand -> Bool
-- If d1 `better` d2, and d2 `better` d2, then d1==d2
betterDemand d1 d2 = (d1 `lub` d2) == d2

squashSig (StrictSig (DmdType fv ds res))
  = StrictSig (DmdType emptyDmdEnv (map squashDmd ds) res)
  where
	-- squash just gets rid of call demands
	-- which the old analyser doesn't track
squashDmd (Call d)   = evalDmd
squashDmd (Box d)    = Box (squashDmd d)
squashDmd (Eval ds)  = Eval (mapDmds squashDmd ds)
squashDmd (Defer ds) = Defer (mapDmds squashDmd ds)
squashDmd d          = d
#endif
\end{code}
