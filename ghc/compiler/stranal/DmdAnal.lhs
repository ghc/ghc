%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

			-----------------
			A demand analysis
			-----------------

\begin{code}
module DmdAnal ( dmdAnalPgm ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlags, DynFlag(..) )
import NewDemand	-- All of it
import CoreSyn
import CoreUtils	( exprIsValue, exprArity )
import DataCon		( dataConTyCon )
import TyCon		( isProductTyCon, isRecursiveTyCon )
import Id		( Id, idType, idInfo, idArity, idStrictness, idCprInfo, idDemandInfo,
			  modifyIdInfo, isDataConId, isImplicitId, isGlobalId )
import IdInfo		( newStrictnessInfo, setNewStrictnessInfo, mkNewStrictnessInfo,
			  newDemandInfo, setNewDemandInfo, newDemand
			)
import Var		( Var )
import VarEnv
import UniqFM		( plusUFM_C, addToUFM_Directly, lookupUFM_Directly,
			  keysUFM, minusUFM, ufmToList )
import Type		( isUnLiftedType )
import CoreLint		( showPass, endPass )
import ErrUtils		( dumpIfSet_dyn )
import Util		( mapAndUnzip, mapAccumL, mapAccumR, zipWithEqual )
import BasicTypes	( Arity, TopLevelFlag(..), isTopLevel )
import Maybes		( orElse, expectJust )
import Outputable
import FastTypes
\end{code}

ToDo:	set a noinline pragma on bottoming Ids
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
#ifndef DEBUG

dmdAnalPgm dflags binds = return binds

#else

dmdAnalPgm dflags binds
  = do {
	showPass dflags "Demand analysis" ;
	let { binds_plus_dmds = do_prog binds ;
	      dmd_changes = get_changes binds_plus_dmds } ;
	endPass dflags "Demand analysis" 
	 	Opt_D_dump_stranal binds_plus_dmds ;
	printDump (text "Changes in demands" $$ dmd_changes) ;
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
	(sigs', (id', rhs')) = downRhs TopLevel sigs (id, rhs)
    in
    (sigs', NonRec id' rhs')    

dmdAnalTopBind sigs (Rec pairs)
  = let
	(sigs', pairs')  = dmdFix TopLevel sigs pairs
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
    (dmd_ty, e') = dmdAnal sigs dmd e	

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

  | otherwise
  = let
	body_dmd = case dmd of
			Call dmd -> dmd
			other	 -> Lazy	-- Conservative

	(body_ty, body') = dmdAnal sigs body_dmd body
	(lam_ty, var') = annotateLamIdBndr body_ty var
    in
    (lam_ty, Lam var' body')

dmdAnal sigs dmd (Case scrut case_bndr [alt@(DataAlt dc,bndrs,rhs)])
  | let tycon = dataConTyCon dc,
    isProductTyCon tycon,
    not (isRecursiveTyCon tycon)
  = let
	bndr_ids		= filter isId bndrs
	(alt_ty, alt')		= dmdAnalAlt sigs dmd alt
	(alt_ty1, case_bndr')   = annotateBndr alt_ty case_bndr
	(_, bndrs', _)		= alt'
        scrut_dmd 		= Seq Drop Now [idNewDemandInfo b | b <- bndrs', isId b]
	(scrut_ty, scrut')      = dmdAnal sigs scrut_dmd scrut
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
	(sigs', (id1, rhs')) = downRhs NotTopLevel sigs (id, rhs)
	(body_ty, body')     = dmdAnal sigs' dmd body
	(body_ty1, id2)      = annotateBndr body_ty id1
    in
--    pprTrace "dmdLet" (ppr id <+> ppr (sig,rhs_env))
    (body_ty1, Let (NonRec id2 rhs') body')    

dmdAnal sigs dmd (Let (Rec pairs) body) 
  = let
	bndrs		 = map fst pairs
	(sigs', pairs')  = dmdFix NotTopLevel sigs pairs
	(body_ty, body') = dmdAnal sigs' dmd body

		-- I saw occasions where it was really worth using the
		-- call demands on the Ids to propagate demand info
		-- on the free variables.  An example is 'roll' in imaginary/wheel-sieve2
		-- Something like this:
		--	roll x = letrec go y = if ... then roll (x-1) else x+1
		--		 in go ms
		-- We want to see that this is strict in x.
		--
		-- This will happen because sigs' has a binding for 'go' that 
		-- has a demand on x.

	(result_ty, _) = annotateBndrs body_ty bndrs
		-- Don't bother to add demand info to recursive
		-- binders as annotateBndr does; 
		-- being recursive, we can't treat them strictly.
		-- But we do need to remove the binders from the result demand env
    in
    (result_ty,  Let (Rec pairs') body')


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
       -> (SigEnv,
	   [(Id,CoreExpr)])	-- Binders annotated with stricness info

dmdFix top_lvl sigs pairs
  = loop 1 initial_sigs pairs
  where
    bndrs        = map fst pairs
    initial_sigs = extendSigEnvList sigs [(id, (initial_sig id, top_lvl)) | id <- bndrs]
    
    loop :: Int
	 -> SigEnv			-- Already contains the current sigs
	 -> [(Id,CoreExpr)] 		
	 -> (SigEnv, [(Id,CoreExpr)])
    loop n sigs pairs
      | all (same_sig sigs sigs') bndrs = (sigs, pairs)
		-- Note: use pairs, not pairs'.   Since the sigs are the same
		-- there'll be no change, unless this is the very first visit,
		-- and the first iteraion of that visit.  But in that case, the	
		-- function is bottom anyway, there's no point in looking.
      | n >= 5		    = pprTrace "dmdFix" (ppr n <+> ppr pairs)   (loop (n+1) sigs' pairs')
      | otherwise	    = {- pprTrace "dmdFixLoop" (ppr id_sigs) -} (loop (n+1) sigs' pairs')
      where
		-- Use the new signature to do the next pair
		-- The occurrence analyser has arranged them in a good order
		-- so this can significantly reduce the number of iterations needed
	(sigs', pairs') = mapAccumL (downRhs top_lvl) sigs pairs

	   
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
	-> (SigEnv,  (Id, CoreExpr))
-- On the way down, compute a strictness signature 
-- for the function.  Keep its annotated RHS and dmd env
-- for use on the way up
-- The demand-env is that computed for a vanilla call.

downRhs top_lvl sigs (id, rhs)
 = (sigs', (id', rhs'))
 where
  arity		 = exprArity rhs   -- The idArity may not be up to date
  (rhs_ty, rhs') = dmdAnal sigs (vanillaCall arity) rhs
  sig	  	 = mkStrictSig id arity (mkSigTy rhs rhs_ty)
  id'		 = id `setIdNewStrictness` sig
  sigs'		 = extendSigEnv top_lvl sigs id sig

mkSigTy rhs (DmdType fv [] RetCPR) 
	| not (exprIsValue rhs)    = DmdType fv [] TopRes
	-- If the rhs is a thunk, we forget the CPR info, because
	-- it is presumably shared (else it would have been inlined, and 
	-- so we'd lose sharing if w/w'd it into a function.
	--
	-- ** But keep the demand unleashed on the free 
	--    vars when the thing is evaluated! **
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

mkSigTy rhs (DmdType fv dmds res) = DmdType fv (map lazify dmds) res
-- Get rid of defers
\end{code}


%************************************************************************
%*									*
\subsection{Strictness signatures and types}
%*									*
%************************************************************************

\begin{code}
unitVarDmd var dmd = DmdType (unitVarEnv var dmd) [] TopRes

addVarDmd top_lvl dmd_ty@(DmdType fv ds res) var dmd
  | isTopLevel top_lvl = dmd_ty		-- Don't record top level things
  | otherwise	       = DmdType (extendVarEnv fv var dmd) ds res

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
\subsection{Demand types}
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

-------------------------
dmdTypeRes :: DmdType -> DmdResult
dmdTypeRes (DmdType _ _ res_ty) = res_ty
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
    let StrictSig arity dmd_ty = idNewStrictness var	-- It must have a strictness sig
  = if arity == length ds then	-- Saturated, so unleash the demand
	-- ds can be empty, when we are just seq'ing the thing
	mkDmdType emptyDmdEnv ds (dmdTypeRes dmd_ty)
		-- Need to extract whether it's a product
    else
	topDmdType

------ 	IMPORTED FUNCTION
  | isGlobalId var,		-- Imported function
    let StrictSig arity dmd_ty = getNewStrictness var
  = if arity <= depth then	-- Saturated, so unleash the demand
	dmd_ty
    else
	topDmdType

------ 	LOCAL LET/REC BOUND THING
  | Just (StrictSig arity dmd_ty, top_lvl) <- lookupVarEnv sigs var
  = let
	fn_ty = if arity <= depth then dmd_ty else topDmdType
    in
    addVarDmd top_lvl fn_ty var dmd

------ 	LOCAL NON-LET/REC BOUND THING
  | otherwise	 		-- Default case
  = unitVarDmd var dmd

  where
    (depth, res_dmd) = splitCallDmd dmd
\end{code}

\begin{code}
squashDmdEnv (StrictSig a (DmdType fv ds res)) = StrictSig a (DmdType emptyDmdEnv ds res)

betterStrict :: StrictSig -> StrictSig -> Bool
betterStrict (StrictSig ar1 t1) (StrictSig ar2 t2)
  = (ar1 >= ar2) && (t1 `betterDmdType` t2)

betterDmdType t1 t2 = (t1 `lubType` t2) == t2
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
deferType (DmdType fv ds _) = DmdType (mapVarEnv defer fv) ds TopRes
	-- Check this

defer :: Demand -> Demand
-- c.f. `lub` Abs
defer Abs	   = Abs
defer (Seq k _ ds) = Seq k Defer ds
defer other	   = Lazy

lazify :: Demand -> Demand
-- The 'Defer' demands are just Lazy at function boundaries
lazify (Seq k Defer ds) = Lazy
lazify (Seq k Now   ds) = Seq k Now (map lazify ds)
lazify d		= d

betterDemand :: Demand -> Demand -> Bool
-- If d1 `better` d2, and d2 `better` d2, then d1==d2
betterDemand d1 d2 = (d1 `lub` d2) == d2
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

lub (Seq k1 l1 ds1) (Seq k2 l2 ds2) = Seq (k1 `vee` k2) (l1 `or_defer` l2)
				          (zipWithEqual "lub" lub ds1 ds2)

-- The last clauses deal with the remaining cases for Call and Seq
lub d1@(Call _) d2@(Seq _ _ _) = pprPanic "lub" (ppr d1 $$ ppr d2)
lub d1 d2		       = lub d2 d1

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

both Bot d = Bot

both Abs Bot = Bot
both Abs d   = d

both Err Bot = Bot
both Err Abs = Err
both Err d   = d

both Lazy Bot 	         = Bot
both Lazy Abs 	         = Lazy
both Lazy Err 	         = Lazy 
both Lazy (Seq k Now ds) = Seq Keep Now ds
both Lazy d	         = d

both Eval Bot 	       = Bot
both Eval (Seq k l ds) = Seq Keep Now ds
both Eval (Call d)     = Call d
both Eval d   	       = Eval

both (Seq k1 Defer ds1) (Seq k2 Defer ds2) = Seq (k1 `vee` k2) Defer
					         (zipWithEqual "both" both ds1 ds2)
both (Seq k1 l1 ds1) (Seq k2 l2 ds2) = Seq (k1 `vee` k2) Now
					   (zipWithEqual "both" both ds1' ds2')
				     where
					ds1' = case l1 of { Now -> ds1; Defer -> map defer ds1 }
					ds2' = case l2 of { Now -> ds2; Defer -> map defer ds2 }

both (Call d1) (Call d2) = Call (d1 `both` d2)

-- The last clauses deal with the remaining cases for Call and Seq
both d1@(Call _) d2@(Seq _ _ _) = pprPanic "both" (ppr d1 $$ ppr d2)
both d1 d2		        = both d2 d1

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
-- Move these to Id.lhs
idNewStrictness_maybe :: Id -> Maybe StrictSig
idNewStrictness :: Id -> StrictSig

idNewStrictness_maybe id = newStrictnessInfo (idInfo id)
idNewStrictness       id = idNewStrictness_maybe id `orElse` topSig

getNewStrictness :: Id -> StrictSig
-- First tries the "new-strictness" field, and then
-- reverts to the old one. This is just until we have
-- cross-module info for new strictness
getNewStrictness id = idNewStrictness_maybe id `orElse` newStrictnessFromOld id
		      
newStrictnessFromOld :: Id -> StrictSig
newStrictnessFromOld id = mkNewStrictnessInfo id (idArity id) (idStrictness id) (idCprInfo id)

setIdNewStrictness :: Id -> StrictSig -> Id
setIdNewStrictness id sig = modifyIdInfo (`setNewStrictnessInfo` sig) id

idNewDemandInfo :: Id -> Demand
idNewDemandInfo id = newDemandInfo (idInfo id)

setIdNewDemandInfo :: Id -> Demand -> Id
setIdNewDemandInfo id dmd = modifyIdInfo (`setNewDemandInfo` dmd) id
\end{code}

\begin{code}
get_changes binds = vcat (map get_changes_bind binds)

get_changes_bind (Rec pairs) = vcat (map get_changes_pr pairs)
get_changes_bind (NonRec id rhs) = get_changes_pr (id,rhs)

get_changes_pr (id,rhs) = get_changes_var id $$ get_changes_expr rhs

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
get_changes_expr (Case e b a) = get_changes_expr e $$ get_changes_var b $$ vcat (map get_changes_alt a)

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
    old_better = old `betterStrict` new
    new_better = new `betterStrict` old

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
#endif 	/* DEBUG */
\end{code}
