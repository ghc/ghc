%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

			-----------------
			A demand analysis
			-----------------

\begin{code}
module DmdAnal ( dmdAnalPgm ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlags, DynFlag(..), opt_MaxWorkerArgs )
import NewDemand	-- All of it
import CoreSyn
import CoreUtils	( exprIsValue, exprArity )
import DataCon		( dataConTyCon )
import TyCon		( isProductTyCon, isRecursiveTyCon )
import Id		( Id, idType, idInfo, idArity, idCprInfo, idDemandInfo,
			  modifyIdInfo, isDataConId, isImplicitId, isGlobalId,
			  idNewStrictness, idNewStrictness_maybe, getNewStrictness, setIdNewStrictness,
			  idNewDemandInfo, setIdNewDemandInfo, newStrictnessFromOld )
import IdInfo		( newDemand )
import Var		( Var )
import VarEnv
import UniqFM		( plusUFM_C, addToUFM_Directly, lookupUFM_Directly,
			  keysUFM, minusUFM, ufmToList, filterUFM )
import Type		( isUnLiftedType )
import CoreLint		( showPass, endPass )
import ErrUtils		( dumpIfSet_dyn )
import Util		( mapAndUnzip, mapAccumL, mapAccumR, zipWithEqual )
import BasicTypes	( Arity, TopLevelFlag(..), isTopLevel )
import Maybes		( orElse, expectJust )
import Outputable
import FastTypes
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
  | isImplicitId id		-- Don't touch the info on constructors, selectors etc
  = (sigs, NonRec id rhs)	-- It's pre-computed in MkId.lhs
  | otherwise
  = let
	(sigs', _, (id', rhs')) = downRhs TopLevel sigs (id, rhs)
    in
    (sigs', NonRec id' rhs')    

dmdAnalTopBind sigs (Rec pairs)
  = let
	(sigs', _, pairs')  = dmdFix TopLevel sigs pairs
    in
    (sigs', Rec pairs')
\end{code}


%************************************************************************
%*									*
\subsection{The analyser itself}	
%*									*
%************************************************************************

\begin{code}
dmdAnal :: SigEnv -> Demand -> CoreExpr -> (DmdType, CoreExpr)

dmdAnal sigs Abs  e = (topDmdType, e)

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

dmdAnal sigs dmd (App fun arg)	-- Non-type arguments
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
	bndr_ids		 = filter isId bndrs
	(alt_ty, alt')		 = dmdAnalAlt sigs dmd alt
	(alt_ty1, case_bndr')    = annotateBndr alt_ty case_bndr
	(_, bndrs', _)		 = alt'

	-- Figure out whether the case binder is used, and use
	-- that to set the keepity of the demand.  This is utterly essential.
	-- Consider	f x = case x of y { (a,b) -> k y a }
	-- If we just take scrut_demand = U(L,A), then we won't pass x to the
	-- worker, so the worker will rebuild 
	--	x = (a, absent-error)
	-- and that'll crash.
	dead_case_bndr		 = isAbsentDmd (idNewDemandInfo case_bndr')
	keepity | dead_case_bndr = Drop
		| otherwise	 = Keep		

        scrut_dmd 		 = Seq keepity Now [idNewDemandInfo b | b <- bndrs', isId b]
	(scrut_ty, scrut')       = dmdAnal sigs scrut_dmd scrut
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
	(sigs', lazy_fv, (id1, rhs')) = downRhs NotTopLevel sigs (id, rhs)
	(body_ty, body') 	      = dmdAnal sigs' dmd body
	(body_ty1, id2)    	      = annotateBndr body_ty id1
	body_ty2		      = addLazyFVs body_ty1 lazy_fv
    in
--    pprTrace "dmdLet" (ppr id <+> ppr (sig,rhs_env))
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

dmdFix top_lvl sigs pairs
  = loop 1 initial_sigs pairs
  where
    bndrs        = map fst pairs
    initial_sigs = extendSigEnvList sigs [(id, (initial_sig id, top_lvl)) | id <- bndrs]
    
    loop :: Int
	 -> SigEnv			-- Already contains the current sigs
	 -> [(Id,CoreExpr)] 		
	 -> (SigEnv, DmdEnv, [(Id,CoreExpr)])
    loop n sigs pairs
      | all (same_sig sigs sigs') bndrs = (sigs', lazy_fv, pairs')
		-- Note: use pairs', not pairs.   pairs' is the result of 
		-- processing the RHSs with sigs (= sigs'), whereas pairs 
		-- is the result of processing the RHSs with the *previous* 
		-- iteration of sigs.
      | n >= 5		    = pprTrace "dmdFix" (ppr n <+> (vcat 
				[ text "Sigs:" <+> ppr [(id,lookup sigs id, lookup sigs' id) | (id,_) <- pairs],
				  text "env:" <+> ppr (ufmToList sigs),
				  text "binds:" <+> ppr pairs]))
			      (loop (n+1) sigs' pairs')
      | otherwise	    = {- pprTrace "dmdFixLoop" (ppr id_sigs) -} (loop (n+1) sigs' pairs')
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
	  (sigs', lazy_fv1, pair') = downRhs top_lvl sigs (id,rhs)
	  lazy_fv'		   = plusUFM_C both lazy_fv lazy_fv1   
	  old_sig   		   = lookup sigs id
	  new_sig  	   	   = lookup sigs' id
	   
	-- Get an initial strictness signature from the Id
	-- itself.  That way we make use of earlier iterations
	-- of the fixpoint algorithm.  (Cunning plan.)
	-- Note that the cunning plan extends to the DmdEnv too,
	-- since it is part of the strictness signature
    initial_sig id = idNewStrictness_maybe id `orElse` botSig

    same_sig sigs sigs' var = lookup sigs var == lookup sigs' var
    lookup sigs var = case lookupVarEnv sigs var of
			Just (sig,_) -> sig

downRhs :: TopLevelFlag 
	-> SigEnv -> (Id, CoreExpr)
	-> (SigEnv,  DmdEnv, (Id, CoreExpr))
-- Process the RHS of the binding, add the strictness signature
-- to the Id, and augment the environment with the signature as well.

downRhs top_lvl sigs (id, rhs)
 = (sigs', lazy_fv, (id', rhs'))
 where
  arity		    = exprArity rhs   -- The idArity may not be up to date
  (rhs_ty, rhs')    = dmdAnal sigs (vanillaCall arity) rhs
  (lazy_fv, sig_ty) = mkSigTy id arity rhs rhs_ty
  id'		    = id `setIdNewStrictness` sig_ty
  sigs'		    = extendSigEnv top_lvl sigs id sig_ty
\end{code}

%************************************************************************
%*									*
\subsection{Strictness signatures and types}
%*									*
%************************************************************************

\begin{code}
mkSigTy :: Id -> Arity -> CoreExpr -> DmdType -> (DmdEnv, StrictSig)
-- Take a DmdType and turn it into a StrictSig
mkSigTy id arity rhs (DmdType fv dmds res) 
  = (lazy_fv, mkStrictSig id arity dmd_ty)
  where
    dmd_ty = DmdType strict_fv lazified_dmds res'

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

    lazified_dmds = map lazify dmds
	-- Get rid of defers in the arguments
    final_dmds = setUnpackStrategy lazified_dmds
	-- Set the unpacking strategy
	
    res' = case (dmds, res) of
		([], RetCPR) | not (exprIsValue rhs) -> TopRes
		other	 			     -> res
	-- If the rhs is a thunk, we forget the CPR info, because
	-- it is presumably shared (else it would have been inlined, and 
	-- so we'd lose sharing if w/w'd it into a function.
	--
	--	DONE IN OLD CPR ANALYSER, BUT NOT YET HERE
	-- Also, if the strictness analyser has figured out that it's strict,
	-- the let-to-case transformation will happen, so again it's good.
	-- (CPR analysis runs before the simplifier has had a chance to do
	--  the let-to-case transform.)
	-- This made a big difference to PrelBase.modInt, which had something like
	--	modInt = \ x -> let r = ... -> I# v in
	--			...body strict in r...
	-- r's RHS isn't a value yet; but modInt returns r in various branches, so
	-- if r doesn't have the CPR property then neither does modInt
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

    go n (Seq keep _ cs : ds) 
	| n' >= 0    = Seq keep Now cs' `cons` go n'' ds
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
splitDmdTy (DmdType fv (dmd:dmds) res_ty) = (dmd, DmdType fv dmds res_ty)
splitDmdTy ty@(DmdType fv [] TopRes)         = (topDmd, ty)
splitDmdTy ty@(DmdType fv [] BotRes)         = (Abs,    ty)
	-- We already have a suitable demand on all
	-- free vars, so no need to add more!
splitDmdTy (DmdType fv [] RetCPR)   	  = panic "splitDmdTy"
\end{code}

\begin{code}
unitVarDmd var dmd = DmdType (unitVarEnv var dmd) [] TopRes

addVarDmd top_lvl dmd_ty@(DmdType fv ds res) var dmd
  | isTopLevel top_lvl = dmd_ty		-- Don't record top level things
  | otherwise	       = DmdType (extendVarEnv fv var dmd) ds res

addLazyFVs (DmdType fv ds res) lazy_fvs
  = DmdType (plusUFM_C both fv lazy_fvs) ds res

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
    (DmdType fv' (dmd:ds) res, setIdNewDemandInfo id dmd)
  where
    (fv', dmd) = removeFV fv id res

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
    Seq k Now ds <- res_dmd,	-- and the demand looks inside its fields
    let StrictSig dmd_ty = idNewStrictness var	-- It must have a strictness sig
  = if dmdTypeDepth dmd_ty == length ds then	-- Saturated, so unleash the demand
	-- ds can be empty, when we are just seq'ing the thing
	mkDmdType emptyDmdEnv ds (dmdTypeRes dmd_ty)
		-- Need to extract whether it's a product, hence dmdTypeRes
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
deferType (DmdType fv _ _) = DmdType (mapVarEnv defer fv) [] TopRes
	-- Notice that we throw away info about both arguments and results
	-- For example,   f = let ... in \x -> x
	-- We don't want to get a stricness type V->T for f.

defer :: Demand -> Demand
-- c.f. `lub` Abs
defer Abs	   = Abs
defer (Seq k _ ds) = Seq k Defer ds
defer other	   = Lazy

lazify :: Demand -> Demand
-- The 'Defer' demands are just Lazy at function boundaries
lazify (Seq k Defer ds) = Lazy
lazify (Seq k Now   ds) = Seq k Now (map lazify ds)
lazify Bot		= Abs	-- Don't pass args that are consumed by bottom
lazify d		= d
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


%************************************************************************
%*									*
\subsection{LUB and BOTH}
%*									*
%************************************************************************

\begin{code}
lub :: Demand -> Demand -> Demand

lub Bot  d = d

lub Lazy d = Lazy

lub Err Bot = Err 
lub Err d   = d 

lub Abs Bot 	     = Abs
lub Abs Err 	     = Abs
lub Abs Abs 	     = Abs    
lub Abs (Seq k _ ds) = Seq k Defer ds	-- Very important ('radicals' example)
lub Abs d 	     = Lazy

lub Eval Abs	        = Lazy
lub Eval Lazy	        = Lazy
lub Eval (Seq k Now ds) = Seq Keep Now ds
lub Eval d    	        = Eval

lub (Call d1) (Call d2) = Call (lub d1 d2)

lub (Seq k1 l1 ds1) (Seq k2 l2 ds2) = Seq (k1 `vee` k2) (l1 `or_defer` l2) (lubs ds1 ds2)

-- The last clauses deal with the remaining cases for Call and Seq
lub d1@(Call _) d2@(Seq _ _ _) = pprPanic "lub" (ppr d1 $$ ppr d2)
lub d1 d2		       = lub d2 d1

-- A Seq can have an empty list of demands, in the polymorphic case.
lubs [] ds2 = ds2
lubs ds1 [] = ds1
lubs ds1 ds2 = ASSERT( length ds1 == length ds2 ) zipWith lub ds1 ds2

or_defer Now Now = Now
or_defer _   _   = Defer

-------------------------
-- Consider (if x then y else []) with demand V
-- Then the first branch gives {y->V} and the second
-- *implicitly* has {y->A}.  So we must put {y->(V `lub` A)}
-- in the result env.
lubType (DmdType fv1 ds1 r1) (DmdType fv2 ds2 r2)
  = DmdType lub_fv2 (zipWith lub ds1 ds2) (r1 `lubRes` r2)
  where
    lub_fv  = plusUFM_C lub fv1 fv2
    lub_fv1 = modifyEnv (not (isBotRes r1)) (Abs `lub`) fv2 fv1 lub_fv
    lub_fv2 = modifyEnv (not (isBotRes r2)) (Abs `lub`) fv1 fv2 lub_fv1
	-- lub is the identity for Bot

-------------------------
lubRes BotRes r      = r
lubRes r      BotRes = r
lubRes RetCPR RetCPR = RetCPR
lubRes r1     r2     = TopRes

-----------------------------------
vee :: Keepity -> Keepity -> Keepity
vee Drop Drop = Drop
vee k1   k2   = Keep

-----------------------------------
both :: Demand -> Demand -> Demand

-- The normal one
-- both Bot d = Bot

-- The experimental one
-- The idea is that (error x) places on x
--	both demand Bot (like on all free vars)
--	and demand Eval (for the arg to error)
-- and we want the result to be Eval.
both Bot Bot = Bot
both Bot Abs = Bot
both Bot d   = d

both Abs d   = d

both Err Bot = Err
both Err Abs = Err
both Err d   = d

both Lazy Bot 	       = Lazy
both Lazy Abs 	       = Lazy
both Lazy Err 	       = Lazy 
both Lazy (Seq k l ds) = Seq Keep l ds
both Lazy d	       = d
  -- Notice that the Seq case ensures that we have the
  -- boxed value.  The equation originally said
  --	both (Seq k Now ds) = Seq Keep Now ds
  -- but it's important that the Keep is switched on even
  -- for a deferred demand.  Otherwise a (Seq Drop Now [])
  -- might both'd with the result, and then we won't pass
  -- the boxed value.  Here's an example:
  --	(x-1) `seq` (x+1, x)
  -- From the (x+1, x) we get (U*(V) `both` L), which must give S*(V)
  -- From (x-1) we get U(V). Combining, we must get S(V).
  -- If we got U*(V) from the pair, we'd end up with U(V), and that
  -- can be a disaster if a component of the data structure is absent.
  -- [Disaster = enter an absent argument.]

both Eval (Seq k l ds) = Seq Keep Now ds
both Eval (Call d)     = Call d
both Eval d   	       = Eval

both (Seq k1 Defer ds1) (Seq k2 Defer ds2) = Seq (k1 `vee` k2) Defer (boths ds1  ds2)
both (Seq k1 l1 ds1)    (Seq k2 l2 ds2)    = Seq (k1 `vee` k2) Now   (boths ds1' ds2')
					   where
					     ds1' = case l1 of { Now -> ds1; Defer -> map defer ds1 }
					     ds2' = case l2 of { Now -> ds2; Defer -> map defer ds2 }

both (Call d1) (Call d2) = Call (d1 `both` d2)

-- The last clauses deal with the remaining cases for Call and Seq
both d1@(Call _) d2@(Seq _ _ _) = pprPanic "both" (ppr d1 $$ ppr d2)
both d1 d2		        = both d2 d1

-----------------------------------
-- A Seq can have an empty list of demands, in the polymorphic case.
boths [] ds2  = ds2
boths ds1 []  = ds1
boths ds1 ds2 = ASSERT( length ds1 == length ds2 ) zipWith both ds1 ds2

-----------------------------------
bothRes :: DmdResult -> DmdResult -> DmdResult
-- Left-biased for CPR info
bothRes BotRes _ = BotRes
bothRes _ BotRes = BotRes
bothRes r1 _     = r1

-----------------------------------
-- (t1 `bothType` t2) takes the argument/result info from t1,
-- using t2 just for its free-var info
bothType (DmdType fv1 ds1 r1) (DmdType fv2 ds2 r2)
  = DmdType both_fv2 ds1 r1
  where
    both_fv  = plusUFM_C both fv1 fv2
    both_fv1 = modifyEnv (isBotRes r1) (`both` Bot) fv2 fv1 both_fv
    both_fv2 = modifyEnv (isBotRes r2) (`both` Bot) fv1 fv2 both_fv1
	-- both is the identity for Abs
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
\subsection{Miscellaneous
%*									*
%************************************************************************


\begin{code}
get_changes binds = vcat (map get_changes_bind binds)

get_changes_bind (Rec pairs) = vcat (map get_changes_pr pairs)
get_changes_bind (NonRec id rhs) = get_changes_pr (id,rhs)

get_changes_pr (id,rhs) 
  | isImplicitId id = empty  -- We don't look inside these
  | otherwise	    = get_changes_var id $$ get_changes_expr rhs

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
    new = lazify (idNewDemandInfo id)	-- Lazify to avoid spurious improvements
    old = newDemand (idDemandInfo id)
    new_better = new `betterDemand` old 
    old_better = old `betterDemand` new
\end{code}
