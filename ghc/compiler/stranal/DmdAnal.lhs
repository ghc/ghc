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
import DataCon		( dataConTyCon )
import TyCon		( isProductTyCon, isRecursiveTyCon )
import Id		( Id, idInfo, idArity, idStrictness, idCprInfo, idDemandInfo,
			  modifyIdInfo, isDataConId, isImplicitId )
import IdInfo		( newStrictnessInfo, setNewStrictnessInfo, mkNewStrictnessInfo,
			  newDemandInfo, setNewDemandInfo, newDemand
			)
import Var		( Var )
import VarEnv
import UniqFM		( plusUFM_C, addToUFM_Directly, keysUFM, minusUFM )
import CoreLint		( showPass, endPass )
import ErrUtils		( dumpIfSet_dyn )
import Util		( mapAccumL, mapAccumR, zipWithEqual )
import BasicTypes	( Arity )
import Maybes		( orElse )
import Outputable
import FastTypes
\end{code}

ToDo:	set a noinline pragma on bottoming Ids

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
	(sig, rhs_env, (id', rhs')) = downRhs sigs (id, rhs)
	sigs'			    = extendSigEnv sigs id sig
    in
    (sigs', NonRec id' rhs')    

dmdAnalTopBind sigs (Rec pairs)
  = let
	(sigs', _, pairs')  = dmdFix sigs pairs
    in
    (sigs', Rec pairs')
\end{code}


%************************************************************************
%*									*
\subsection{The analyser itself}	
%*									*
%************************************************************************

\begin{code}
dmdAnal :: SigEnv -> Demand -> CoreExpr -> (DmdType, DmdEnv, CoreExpr)

dmdAnal sigs Abs  e = (DmdRes TopRes, emptyDmdEnv, e)

dmdAnal sigs Lazy e = let 
			(res_ty, dmd_env, e') = dmdAnal sigs Eval e
		      in
		      (res_ty, lazify dmd_env, e')
	-- It's important not to analyse e with a lazy demand because
	-- a) When we encounter   case s of (a,b) -> 
	--	we demand s with U(d1d2)... but if the overall demand is lazy
	--	that is wrong, and we'd need to reduce the demand on s (inconvenient)
	-- b) More important, consider
	--	f (let x = R in x+x), where f is lazy
	--    We still want to mark x as demanded, because it will be when we
	--    enter the let.  If we analyse f's arg with a Lazy demand, we'll
	--    just mark x as Lazy


dmdAnal sigs dmd (Var var)
  = (res_ty, 
     blackHoleEnv res_ty (unitDmdEnv var dmd), 
     Var var)
  where
    res_ty = dmdTransform sigs var dmd

dmdAnal sigs dmd (Lit lit)
  = (topDmdType, emptyDmdEnv, Lit lit)

dmdAnal sigs dmd (Note n e)
  = (dmd_ty, dmd_env, Note n e')
  where
    (dmd_ty, dmd_env, e') = dmdAnal sigs dmd e	

dmdAnal sigs dmd (App fun (Type ty))
  = (fun_ty, fun_env, App fun' (Type ty))
  where
    (fun_ty, fun_env, fun') = dmdAnal sigs dmd fun

dmdAnal sigs dmd (App fun arg)	-- Non-type arguments
  = let				-- [Type arg handled above]
	(fun_ty, fun_env, fun') = dmdAnal sigs (Call dmd) fun
	(arg_ty, arg_env, arg') = dmdAnal sigs arg_dmd arg
	(arg_dmd, res_ty) 	= splitDmdTy fun_ty
    in
    (res_ty, 
     blackHoleEnv res_ty (fun_env `bothEnv` arg_env), 
     App fun' arg')

dmdAnal sigs dmd (Lam var body)
  | isTyVar var
  = let   
	(body_ty, body_env, body') = dmdAnal sigs dmd body
    in
    (body_ty, body_env, Lam var body')

  | otherwise
  = let
	body_dmd = case dmd of
			Call dmd -> dmd
			other	 -> Lazy	-- Conservative

	(body_ty, body_env, body') = dmdAnal sigs body_dmd body
	(lam_env, var') 	   = annotateBndr body_env var
    in
    (DmdFun (idNewDemandInfo var') body_ty,
     body_env `delDmdEnv` var,
     Lam var' body')

dmdAnal sigs dmd (Case scrut case_bndr [alt@(DataAlt dc,bndrs,rhs)])
  | let tycon = dataConTyCon dc,
    isProductTyCon tycon,
    not (isRecursiveTyCon tycon)
  = let
	bndr_ids		= filter isId bndrs
	(alt_ty, alt_env, alt')	= dmdAnalAlt sigs dmd alt
	(_, scrut_env, scrut')  = dmdAnal sigs scrut_dmd scrut
	(alt_env2, case_bndr')  = annotateBndr alt_env case_bndr
	(_, bndrs', _)		= alt'
        scrut_dmd 		= Seq Drop [idNewDemandInfo b | b <- bndrs', isId b]
    in
    (alt_ty,
     alt_env2 `bothEnv` scrut_env,
     Case scrut' case_bndr' [alt'])

dmdAnal sigs dmd (Case scrut case_bndr alts)
  = let
	(alt_tys, alt_envs, alts')    = unzip3 (map (dmdAnalAlt sigs dmd) alts)
	(scrut_ty, scrut_env, scrut') = dmdAnal sigs Eval scrut
	(alt_env2, case_bndr')	      = annotateBndr (foldr1 lubEnv alt_envs) case_bndr
    in
    (foldr1 lubDmdTy alt_tys,
     alt_env2 `bothEnv` scrut_env,
     Case scrut' case_bndr' alts')

dmdAnal sigs dmd (Let (NonRec id rhs) body) 
  | idArity id == 0	-- A thunk; analyse the body first, then the thunk
  = let
	(body_ty, body_env, body') = dmdAnal sigs dmd body
	(rhs_ty, rhs_env, rhs')    = dmdAnal sigs (lookupDmd body_env id) rhs
	(body_env1, id1)	   = annotateBndr body_env id
    in
    (body_ty, body_env1 `bothEnv` rhs_env, Let (NonRec id1 rhs') body')    

  | otherwise	-- A function; analyse the function first, then the body
  = let
	(sig, rhs_env, (id1, rhs')) = downRhs sigs (id, rhs)
	sigs'			    = extendSigEnv sigs id sig
	(body_ty, body_env, body')  = dmdAnal sigs' dmd body
	rhs_env1		    = weaken body_env id rhs_env
	(body_env1, id2)	    = annotateBndr body_env id1
    in
    (body_ty, body_env1 `bothEnv` rhs_env1, Let (NonRec id2 rhs') body')    

dmdAnal sigs dmd (Let (Rec pairs) body) 
  = let
	bndrs			   = map fst pairs
	(sigs', rhs_envs, pairs')  = dmdFix sigs pairs
	(body_ty, body_env, body') = dmdAnal sigs' dmd body

	weakened_rhs_envs = zipWithEqual "dmdAnal:Let" (weaken body_env) bndrs rhs_envs
		-- I saw occasions where it was really worth using the
		-- call demands on the Ids to propagate demand info
		-- on the free variables.  An example is 'roll' in imaginary/wheel-sieve2
		-- Something like this:
		--	roll x = letrec go y = if ... then roll (x-1) else x+1
		--		 in go ms
		-- We want to see that this is strict in x.

 	rhs_env1 = foldr1 bothEnv weakened_rhs_envs

	result_env = delDmdEnvList (body_env `bothEnv` rhs_env1) bndrs
		-- Don't bother to add demand info to recursive
		-- binders as annotateBndr does; 
		-- being recursive, we can't treat them strictly.
		-- But we do need to remove the binders from the result demand env
    in
    (body_ty, result_env, Let (Rec pairs') body')
\end{code}

\begin{code}
dmdAnalAlt sigs dmd (con,bndrs,rhs) 
  = let 
	(rhs_ty, rhs_env, rhs') = dmdAnal sigs dmd rhs
	(alt_env, bndrs')	= annotateBndrs rhs_env bndrs
    in
    (rhs_ty, alt_env, (con, bndrs', rhs'))
\end{code}

%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}
dmdFix :: SigEnv 		-- Does not include bindings for this binding
       -> [(Id,CoreExpr)]
       -> (SigEnv,
	   [DmdEnv], 		-- Demands from RHSs
	   [(Id,CoreExpr)])	-- Binders annotated with stricness info

dmdFix sigs pairs
  = loop (map initial_sig pairs) pairs
  where
    loop id_sigs pairs
      | id_sigs == id_sigs' = (sigs', rhs_envs, pairs')
      | otherwise	    = loop id_sigs' pairs'
      where
	extra_sigs = [(id,sig) | ((id,_),sig) <- pairs `zip` id_sigs]
	sigs'      = extendSigEnvList sigs extra_sigs
	(id_sigs', rhs_envs, pairs') = unzip3 (map (downRhs sigs') pairs) 
	   
	-- Get an initial strictness signature from the Id
	-- itself.  That way we make use of earlier iterations
	-- of the fixpoint algorithm.  (Cunning plan.)
    initial_sig (id,_) = idNewStrictness_maybe id `orElse` botSig


downRhs :: SigEnv -> (Id, CoreExpr)
	-> (StrictSig, DmdEnv, (Id, CoreExpr))
-- On the way down, compute a strictness signature 
-- for the function.  Keep its annotated RHS and dmd env
-- for use on the way up
-- The demand-env is that computed for a vanilla call.

downRhs sigs (id, rhs)
 = (sig, rhs_env, (id', rhs'))
 where
  arity			  = idArity id
  (rhs_ty, rhs_env, rhs') = dmdAnal sigs (vanillaCall arity) rhs
  sig	  	          = mkStrictSig arity rhs_ty
  id'		          = id `setIdNewStrictness` sig
\end{code}


%************************************************************************
%*									*
\subsection{Strictness signatures and types}
%*									*
%************************************************************************

\begin{code}
data DmdEnv
  = DmdEnv (VarEnv Demand)	-- All the explicitly mentioned variables
	   Bool			-- True  <=> all the others are Bot
				-- False <=> all the others are Abs

emptyDmdEnv 	   = DmdEnv emptyVarEnv	 	 False
unitDmdEnv var dmd = DmdEnv (unitVarEnv var dmd) False

lookupDmd :: DmdEnv -> Var -> Demand
lookupDmd (DmdEnv env bh) var = lookupVarEnv env var `orElse` deflt
			      where
				deflt | bh        = Bot
				      | otherwise = Abs

delDmdEnv :: DmdEnv -> Var -> DmdEnv
delDmdEnv (DmdEnv env b) var = DmdEnv (env `delVarEnv` var) b

delDmdEnvList :: DmdEnv -> [Var] -> DmdEnv
delDmdEnvList (DmdEnv env b) vars = DmdEnv (env `delVarEnvList` vars) b


blackHoleEnv :: DmdType -> DmdEnv -> DmdEnv
blackHoleEnv (DmdRes BotRes) (DmdEnv env _) = DmdEnv env True
blackHoleEnv other	     env	    = env

bothEnv (DmdEnv env1 b1) (DmdEnv env2 b2)
  = DmdEnv both_env2 (b1 || b2)
  where
    both_env  = plusUFM_C both env1 env2
    both_env1 = modifyEnv b1 Bot env2 env1 both_env
    both_env2 = modifyEnv b2 Bot env1 env2 both_env1

lubEnv (DmdEnv env1 b1) (DmdEnv env2 b2)
  = DmdEnv lub_env2 (b1 && b2)
  where
    lub_env  = plusUFM_C lub env1 env2
    lub_env1 = modifyEnv (not b1) Lazy env2 env1 lub_env
    lub_env2 = modifyEnv (not b2) Lazy env1 env2 lub_env1

modifyEnv :: Bool				-- No-op if False
	  -> Demand				-- The zap value
	  -> VarEnv Demand -> VarEnv Demand	-- Env1 and Env2
	  -> VarEnv Demand -> VarEnv Demand	-- Transform this env
	-- Zap anything in Env1 but not in Env2
	-- Assume: dom(env) includes dom(Env1) and dom(Env2)

modifyEnv need_to_modify zap_value env1 env2 env
  | need_to_modify = foldr zap env (keysUFM (env1 `minusUFM` env2))
  | otherwise	   = env
  where
    zap uniq env = addToUFM_Directly env uniq zap_value

annotateBndr :: DmdEnv -> Var -> (DmdEnv, Var)
-- The returned env has the var deleted
-- The returned var is annotated with demand info
annotateBndr dmd_env var
  | isTyVar var = (dmd_env,		    var)
  | otherwise   = (dmd_env `delDmdEnv` var, setIdNewDemandInfo var (lookupDmd dmd_env var))

annotateBndrs = mapAccumR annotateBndr

weaken :: DmdEnv	-- How the Id is used in its scope
       -> Id
       -> DmdEnv	-- The RHS env for the Id, assuming a vanilla call demand
       -> DmdEnv	-- The RHS env given the actual demand
-- Consider	let f = \x -> R in B
-- The vanilla call demand is C(V), and that's what we use to 
-- compute f's strictness signature.  If the *actual* demand on
-- f from B is less than this, we must weaken, or lazify, the 
-- demands in R to match this

weaken body_env id rhs_env
  | depth >= idArity id		-- Enough demand
  = rhs_env
  | otherwise			-- Not enough demand
  = lazify rhs_env 
  where
    (depth,_) = splitCallDmd (lookupDmd body_env id)

lazify (DmdEnv env _) = DmdEnv (mapVarEnv (\_ -> Lazy) env) False
\end{code}

%************************************************************************
%*									*
\subsection{Demand types}
%*									*
%************************************************************************

\begin{code}
splitDmdTy :: DmdType -> (Demand, DmdType)
-- Split off one function argument
splitDmdTy (DmdFun dmd res_ty) = (dmd, res_ty)
splitDmdTy (DmdRes TopRes)     = (topDmd, topDmdType)
splitDmdTy (DmdRes BotRes)     = (Abs, DmdRes BotRes)
	-- We already have a suitable demand on all
	-- free vars, so no need to add more!
splitDmdTy (DmdRes RetCPR)     = panic "splitDmdTy"

-------------------------
dmdTypeRes :: DmdType -> Result
dmdTypeRes (DmdFun dmd res_ty) = dmdTypeRes res_ty
dmdTypeRes (DmdRes res)	       = res

-------------------------
lubDmdTy :: DmdType -> DmdType -> DmdType
lubDmdTy (DmdFun d1 t1) (DmdFun d2 t2) = DmdFun (d1 `lub` d2) (t1 `lubDmdTy` t2)
lubDmdTy (DmdRes r1)    (DmdRes r2)    = DmdRes (r1 `lubRes` r2)
lubDmdTy t1	        t2	       = topDmdType

-------------------------
lubRes BotRes r      = r
lubRes r      BotRes = r
lubRes RetCPR RetCPR = RetCPR
lubRes r1     r2     = TopRes
\end{code}


%************************************************************************
%*									*
\subsection{Strictness signatures}
%*									*
%************************************************************************

\begin{code}
type SigEnv  = VarEnv StrictSig
emptySigEnv  = emptyVarEnv
extendSigEnv = extendVarEnv
extendSigEnvList = extendVarEnvList
lookupSig sigs v = case lookupVarEnv sigs v of
			Just sig -> Just sig
			Nothing  -> idNewStrictness_maybe v

dmdTransform :: SigEnv		-- The strictness environment
	     -> Id		-- The function
	     -> Demand		-- The demand on the function
	     -> DmdType		-- The demand type of the function in this context

dmdTransform sigs var dmd
  | isDataConId var,		-- Data constructor
    Seq k ds <- res_dmd,	-- and the demand looks inside its fields
    StrictSig arity dmd_ty <- idNewStrictness var,	-- It must have a strictness sig
    length ds == arity		-- It's saturated
  = mkDmdFun ds (dmdTypeRes dmd_ty)
	-- Need to extract whether it's a product


  | Just (StrictSig arity dmd_ty) <- lookupSig sigs var,
    arity <= depth		-- Saturated function;
  = dmd_ty			-- Unleash the demand!

  | otherwise	 		-- Default case
  = topDmdType

  where
    (depth, res_dmd) = splitCallDmd dmd

betterStrict :: StrictSig -> StrictSig -> Bool
betterStrict (StrictSig ar1 t1) (StrictSig ar2 t2)
  = (ar1 >= ar2) && (t1 `betterDmdType` t2)

betterDmdType t1 t2 = (t1 `lubDmdTy` t2) == t2
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

-----------------------------------
lub :: Demand -> Demand -> Demand

lub Bot  d = d

lub Lazy d = Lazy

lub Err Bot = Err 
lub Err d   = d 

lub Abs Bot = Abs
lub Abs Err = Abs
lub Abs Abs = Abs
lub Abs d   = d

lub Eval Abs	    = Lazy
lub Eval Lazy	    = Lazy
lub Eval (Seq k ds) = Seq Keep ds
lub Eval d    	    = Eval

lub (Call d1) (Call d2) = Call (lub d1 d2)

lub (Seq k1 ds1) (Seq k2 ds2) = Seq (k1 `vee` k2) 
				    (zipWithEqual "lub" lub ds1 ds2)

-- The last clauses deal with the remaining cases for Call and Seq
lub d1@(Call _) d2@(Seq _ _) = pprPanic "lub" (ppr d1 $$ ppr d2)
lub d1 d2		     = lub d2 d1

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

both Lazy Bot 	     = Bot
both Lazy Abs 	     = Lazy
both Lazy Err 	     = Lazy 
both Lazy (Seq k ds) = Seq Keep ds
both Lazy d	     = d

both Eval Bot 	     = Bot
both Eval (Seq k ds) = Seq Keep ds
both Eval (Call d)   = Call d
both Eval d   	     = Eval

both (Seq k1 ds1) (Seq k2 ds2) = Seq (k1 `vee` k2)
				     (zipWithEqual "both" both ds1 ds2)

both (Call d1) (Call d2) = Call (d1 `both` d2)

-- The last clauses deal with the remaining cases for Call and Seq
both d1@(Call _) d2@(Seq _ _) = pprPanic "both" (ppr d1 $$ ppr d2)
both d1 d2		      = both d2 d1

betterDemand :: Demand -> Demand -> Bool
-- If d1 `better` d2, and d2 `better` d2, then d1==d2
betterDemand d1 d2 = (d1 `lub` d2) == d2
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
get_changes_expr (Lam b e)    = get_changes_var b $$ get_changes_expr e
get_changes_expr (Let b e)    = get_changes_bind b $$ get_changes_expr e
get_changes_expr (Case e b a) = get_changes_expr e $$ get_changes_var b $$ vcat (map get_changes_alt a)

get_changes_alt (con,bs,rhs) = vcat (map get_changes_var bs) $$ get_changes_expr rhs

get_changes_str id
  | new_better && old_better = empty
  | new_better	       	     = message "BETTER"
  | old_better	       	     = message "WORSE"
  | otherwise	       	     = message "INCOMPARABLE" 
  where
    message word = text word <+> text "strictness for" <+> ppr id <+> info
    info = (text "Old" <+> ppr old) $$ (text "New" <+> ppr new)
    new = idNewStrictness id
    old = mkNewStrictnessInfo (idArity id) (idStrictness id) (idCprInfo id)
    old_better = old `betterStrict` new
    new_better = new `betterStrict` old

get_changes_dmd id
  | new_better && old_better = empty
  | new_better	       	     = message "BETTER"
  | old_better	       	     = message "WORSE"
  | otherwise	       	     = message "INCOMPARABLE" 
  where
    message word = text word <+> text "demand for" <+> ppr id <+> info
    info = (text "Old" <+> ppr old) $$ (text "New" <+> ppr new)
    new = idNewDemandInfo id
    old = newDemand (idDemandInfo id)
    new_better = new `betterDemand` old 
    old_better = old `betterDemand` new
#endif 	/* DEBUG */
\end{code}

