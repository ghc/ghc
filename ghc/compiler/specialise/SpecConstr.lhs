%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[SpecConstr]{Specialise over constructors}

\begin{code}
module SpecConstr(
	specConstrProgram	
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreLint		( showPass, endPass )
import CoreUtils	( exprType, exprIsConApp_maybe, eqExpr )
import CoreFVs 		( exprsFreeVars )
import DataCon		( isExistentialDataCon )
import PprCore		( pprCoreRules )
import Id		( Id, idName, idSpecialisation, mkUserLocal, mkSysLocal )
import Var		( Var )
import VarEnv
import VarSet
import Name		( nameOccName, nameSrcLoc )
import Rules		( addIdSpecialisations )
import OccName		( mkSpecOcc )
import ErrUtils		( dumpIfSet_dyn )
import CmdLineOpts	( DynFlags, DynFlag(..) )
import Outputable

import Maybes		( orElse )
import Util		( mapAccumL )
import List		( nubBy, partition )
import UniqSupply
import Outputable
\end{code}

-----------------------------------------------------
			Game plan
-----------------------------------------------------

Consider
	drop n []     = []
	drop 0 xs     = []
	drop n (x:xs) = drop (n-1) xs

After the first time round, we could pass n unboxed.  This happens in
numerical code too.  Here's what it looks like in Core:

	drop n xs = case xs of
		      []     -> []
		      (y:ys) -> case n of 
				  I# n# -> case n# of
					     0 -> []
					     _ -> drop (I# (n# -# 1#)) xs

Notice that the recursive call has an explicit constructor as argument.
Noticing this, we can make a specialised version of drop
	
	RULE: drop (I# n#) xs ==> drop' n# xs

	drop' n# xs = let n = I# n# in ...orig RHS...

Now the simplifier will apply the specialisation in the rhs of drop', giving

	drop' n# xs = case xs of
		      []     -> []
		      (y:ys) -> case n# of
				  0 -> []
				  _ -> drop (n# -# 1#) xs

Much better!  

We'd also like to catch cases where a parameter is carried along unchanged,
but evaluated each time round the loop:

	f i n = if i>0 || i>n then i else f (i*2) n

Here f isn't strict in n, but we'd like to avoid evaluating it each iteration.
In Core, by the time we've w/wd (f is strict in i) we get

	f i# n = case i# ># 0 of
		   False -> I# i#
		   True  -> case n of n' { I# n# ->
			    case i# ># n# of
				False -> I# i#
				True  -> f (i# *# 2#) n'

At the call to f, we see that the argument, n is know to be (I# n#),
and n is evaluated elsewhere in the body of f, so we can play the same
trick as above.  However we don't want to do that if the boxed version
of n is needed (else we'd avoid the eval but pay more for re-boxing n).
So in this case we want that the *only* uses of n are in case statements.


So we look for

* A self-recursive function.  Ignore mutual recursion for now, 
  because it's less common, and the code is simpler for self-recursion.

* EITHER

   a) At a recursive call, one or more parameters is an explicit 
      constructor application
	AND
      That same parameter is scrutinised by a case somewhere in 
      the RHS of the function

  OR

    b) At a recursive call, one or more parameters has an unfolding
       that is an explicit constructor application
	AND
      That same parameter is scrutinised by a case somewhere in 
      the RHS of the function
	AND
      Those are the only uses of the parameter


There's a bit of a complication with type arguments.  If the call
site looks like

	f p = ...f ((:) [a] x xs)...

then our specialised function look like

	f_spec x xs = let p = (:) [a] x xs in ....as before....

This only makes sense if either
  a) the type variable 'a' is in scope at the top of f, or
  b) the type variable 'a' is an argument to f (and hence fs)

Actually, (a) may hold for value arguments too, in which case
we may not want to pass them.  Supose 'x' is in scope at f's
defn, but xs is not.  Then we'd like

	f_spec xs = let p = (:) [a] x xs in ....as before....

Similarly (b) may hold too.  If x is already an argument at the
call, no need to pass it again.

Finally, if 'a' is not in scope at the call site, we could abstract
it as we do the term variables:

	f_spec a x xs = let p = (:) [a] x xs in ...as before...

So the grand plan is:

	* abstract the call site to a constructor-only pattern
	  e.g.  C x (D (f p) (g q))  ==>  C s1 (D s2 s3)

	* Find the free variables of the abstracted pattern

	* Pass these variables, less any that are in scope at
	  the fn defn.


NOTICE that we only abstract over variables that are not in scope,
so we're in no danger of shadowing variables used in "higher up"
in f_spec's RHS.


%************************************************************************
%*									*
\subsection{Top level wrapper stuff}
%*									*
%************************************************************************

\begin{code}
specConstrProgram :: DynFlags -> UniqSupply -> [CoreBind] -> IO [CoreBind]
specConstrProgram dflags us binds
  = do
	showPass dflags "SpecConstr"

	let (binds', _) = initUs us (go emptyScEnv binds)

	endPass dflags "SpecConstr" Opt_D_dump_spec binds'

	dumpIfSet_dyn dflags Opt_D_dump_rules "Top-level specialisations"
		  (vcat (map dump_specs (concat (map bindersOf binds'))))

	return binds'
  where
    go env []	        = returnUs []
    go env (bind:binds) = scBind env bind 	`thenUs` \ (env', _, bind') ->
			  go env' binds 	`thenUs` \ binds' ->
			  returnUs (bind' : binds')

dump_specs var = pprCoreRules var (idSpecialisation var)
\end{code}


%************************************************************************
%*									*
\subsection{Environments and such}
%*									*
%************************************************************************

\begin{code}
type ScEnv = VarEnv HowBound

emptyScEnv = emptyVarEnv

data HowBound = RecFun		-- These are the recursive functions for which 
				-- we seek interesting call patterns

	      | RecArg		-- These are those functions' arguments; we are
				-- interested to see if those arguments are scrutinised

	      | Other		-- We track all others so we know what's in scope
				-- This is used in spec_one to check what needs to be
				-- passed as a parameter and what is in scope at the 
				-- function definition site

extendBndrs env bndrs = extendVarEnvList env [(b,Other) | b <- bndrs]
extendBndr  env bndr  = extendVarEnv env bndr Other

data ScUsage
   = SCU {
	calls :: !(IdEnv ([[CoreArg]])),	-- Calls
						-- The functions are a subset of the 
						-- 	RecFuns in the ScEnv

	occs :: !(IdEnv ArgOcc)		-- Information on argument occurrences
     }					-- The variables are a subset of the 
					--	RecArg in the ScEnv

nullUsage = SCU { calls = emptyVarEnv, occs = emptyVarEnv }

combineUsage u1 u2 = SCU { calls = plusVarEnv_C (++) (calls u1) (calls u2),
			   occs  = plusVarEnv_C combineOcc (occs u1) (occs u2) }

combineUsages [] = nullUsage
combineUsages us = foldr1 combineUsage us

data ArgOcc = CaseScrut 
	    | OtherOcc
	    | Both

instance Outputable ArgOcc where
  ppr CaseScrut = ptext SLIT("case-scrut")
  ppr OtherOcc  = ptext SLIT("other-occ")
  ppr Both      = ptext SLIT("case-scrut and other")

combineOcc CaseScrut CaseScrut = CaseScrut
combineOcc OtherOcc  OtherOcc  = OtherOcc
combineOcc _	     _	       = Both
\end{code}


%************************************************************************
%*									*
\subsection{The main recursive function}
%*									*
%************************************************************************

\begin{code}
scExpr :: ScEnv -> CoreExpr -> UniqSM (ScUsage, CoreExpr)
	-- The unique supply is needed when we invent
	-- a new name for the specialised function and its args

scExpr env e@(Type t) = returnUs (nullUsage, e)
scExpr env e@(Lit l)  = returnUs (nullUsage, e)
scExpr env e@(Var v)  = returnUs (varUsage env v OtherOcc, e)
scExpr env (Note n e) = scExpr env e	`thenUs` \ (usg,e') ->
			returnUs (usg, Note n e')
scExpr env (Lam b e)  = scExpr (extendBndr env b) e	`thenUs` \ (usg,e') ->
			returnUs (usg, Lam b e')

scExpr env (Case scrut b alts) 
  = sc_scrut scrut		`thenUs` \ (scrut_usg, scrut') ->
    mapAndUnzipUs sc_alt alts	`thenUs` \ (alts_usgs, alts') ->
    returnUs (combineUsages alts_usgs `combineUsage` scrut_usg,
	      Case scrut' b alts')
  where
    sc_scrut e@(Var v) = returnUs (varUsage env v CaseScrut, e)
    sc_scrut e	       = scExpr env e

    sc_alt (con,bs,rhs) = scExpr env rhs	`thenUs` \ (usg,rhs') ->
			  returnUs (usg, (con,bs,rhs'))
			where
			  env1 = extendBndrs env (b:bs)

scExpr env (Let bind body)
  = scBind env bind	`thenUs` \ (env', bind_usg, bind') ->
    scExpr env' body	`thenUs` \ (body_usg, body') ->
    returnUs (bind_usg `combineUsage` body_usg, Let bind' body')

scExpr env e@(App _ _) 
  = let 
	(fn, args) = collectArgs e
    in
    mapAndUnzipUs (scExpr env) args	`thenUs` \ (usgs, args') ->
    let
	arg_usg = combineUsages usgs
	fn_usg  | Var f <- fn,
		  Just RecFun <- lookupVarEnv env f
		= SCU { calls = unitVarEnv f [args], occs = emptyVarEnv }
		| otherwise
		= nullUsage
    in
    returnUs (arg_usg `combineUsage` fn_usg, mkApps fn args')
	-- Don't bother to look inside fn;
	-- it's almost always a variable

----------------------
scBind :: ScEnv -> CoreBind -> UniqSM (ScEnv, ScUsage, CoreBind)
scBind env (Rec [(fn,rhs)])
  | not (null val_bndrs)
  = scExpr env' body			`thenUs` \ (usg@(SCU { calls = calls, occs = occs }), body') ->
    specialise env fn bndrs body usg	`thenUs` \ (rules, spec_prs) ->
    returnUs (extendBndrs env bndrs,
	      SCU { calls = calls `delVarEnv` fn, occs = occs `delVarEnvList` val_bndrs},
	      Rec ((fn `addIdSpecialisations` rules, mkLams bndrs body') : spec_prs))
  where
    (bndrs,body) = collectBinders rhs
    val_bndrs    = filter isId bndrs
    env' = env `extendVarEnvList` ((fn,RecFun): [(bndr,RecArg) | bndr <- bndrs])

scBind env (Rec prs)
  = mapAndUnzipUs do_one prs	`thenUs` \ (usgs, prs') ->
    returnUs (extendBndrs env (map fst prs), combineUsages usgs, Rec prs')
  where
    do_one (bndr,rhs) = scExpr env rhs	`thenUs` \ (usg, rhs') ->
		        returnUs (usg, (bndr,rhs'))

scBind env (NonRec bndr rhs)
  = scExpr env rhs	`thenUs` \ (usg, rhs') ->
    returnUs (extendBndr env bndr, usg, NonRec bndr rhs')

----------------------
varUsage env v use 
  | Just RecArg <- lookupVarEnv env v = SCU { calls = emptyVarEnv, occs = unitVarEnv v use }
  | otherwise		   	      = nullUsage
\end{code}


%************************************************************************
%*									*
\subsection{The specialiser}
%*									*
%************************************************************************

\begin{code}
specialise :: ScEnv
	   -> Id 			-- Functionn
	   -> [CoreBndr] -> CoreExpr	-- Its RHS
	   -> ScUsage			-- Info on usage
	   -> UniqSM ([CoreRule], 	-- Rules
		      [(Id,CoreExpr)])	-- Bindings

specialise env fn bndrs body (SCU {calls=calls, occs=occs})
  = getUs		`thenUs` \ us ->
    let
	all_calls = lookupVarEnv calls fn `orElse` []

	good_calls :: [[CoreArg]]
	good_calls = [ pats
		     | call_args <- all_calls,
		       length call_args >= n_bndrs,	-- App is saturated
		       let call = (bndrs `zip` call_args),
		       any (good_arg occs) call,
		       let (_, pats) = argsToPats us call_args
		     ]
    in
    pprTrace "specialise" (ppr all_calls $$ ppr good_calls) $
    mapAndUnzipUs (spec_one env fn (mkLams bndrs body)) 
		  (nubBy same_call good_calls `zip` [1..])
  where
    n_bndrs  = length bndrs
    same_call as1 as2 = and (zipWith eqExpr as1 as2)

---------------------
good_arg :: IdEnv ArgOcc -> (CoreBndr, CoreArg) -> Bool
good_arg arg_occs (bndr, arg)
  = case exprIsConApp_maybe arg of			-- exprIsConApp_maybe looks
	Just (dc,_) -> not (isExistentialDataCon dc)	-- through unfoldings
		       && bndr_usg_ok arg_occs bndr arg
	other   -> False

bndr_usg_ok :: IdEnv ArgOcc -> Var -> CoreArg -> Bool
bndr_usg_ok arg_occs bndr arg
  = pprTrace "bndr_ok" (ppr bndr <+> ppr (lookupVarEnv arg_occs bndr)) $
    case lookupVarEnv arg_occs bndr of
	Just CaseScrut -> True			-- Used only by case scrutiny
	Just Both      -> case arg of		-- Used by case and elsewhere
			    App _ _ -> True	-- so the arg should be an explicit con app
			    other   -> False
	other -> False				-- Not used, or used wonkily
    

---------------------
argsToPats :: UniqSupply -> [CoreArg] -> (UniqSupply, [CoreExpr])
argsToPats us args = mapAccumL argToPat us args

argToPat   :: UniqSupply -> CoreArg   -> (UniqSupply, CoreExpr)
--    C a (D (f x) (g y))  ==>  C p1 (D p2 p3)
argToPat us (Type ty) 
  = (us, Type ty)

argToPat us arg
  | Just (dc,args) <- exprIsConApp_maybe arg
  = let
	(us',args') = argsToPats us args
    in
    (us', mkConApp dc args')

argToPat us (Var v)	-- Don't uniqify existing vars,
  = (us, Var v)		-- so that we can spot when we pass them twice

argToPat us arg
  = (us1, Var (mkSysLocal SLIT("sc") (uniqFromSupply us2) (exprType arg)))
  where
    (us1,us2) = splitUniqSupply us

---------------------
spec_one :: ScEnv
	 -> Id					-- Function
	 -> CoreExpr				-- Rhs of the original function
	 -> ([CoreArg], Int)
	 -> UniqSM (CoreRule, (Id,CoreExpr))	-- Rule and binding

{- 
  Example
  
     In-scope: a, x::a   
     f = /\b \y::[(a,b)] -> ....f (b,c) ((:) (a,(b,c)) v (h v))...
	  [c is presumably bound by the (...) part]
  ==>
     f_spec = /\ b c \ v::(a,(b,c)) -> 
		  (...entire RHS of f...) (b,c) ((:) (a,(b,c)) v (h v))
  
     RULE:  forall b c,
		   y::[(a,(b,c))], 
		   v::(a,(b,c)), 
		   h::(a,(b,c))->[(a,(b,c))] .
  
	    f (b,c) ((:) (a,(b,c)) v (h v)) = f_spec b c v
-}

spec_one env fn rhs (pats, n)
  = getUniqueUs 			`thenUs` \ spec_uniq ->
    let 
	fn_name      = idName fn
	fn_loc       = nameSrcLoc fn_name
	spec_occ     = mkSpecOcc (nameOccName fn_name)
	pat_fvs	     = varSetElems (exprsFreeVars pats)
	vars_to_bind = filter not_avail pat_fvs
	not_avail v  = not (v `elemVarEnv` env)
		-- Put the type variables first just for tidiness
	(tvs, ids)   = partition isTyVar vars_to_bind
	bndrs  	     = tvs ++ ids
	
	rule_name = _PK_ ("SC:" ++ showSDoc (ppr fn <> int n))
	spec_rhs  = mkLams bndrs (mkApps rhs pats)
	spec_id   = mkUserLocal spec_occ spec_uniq (exprType spec_rhs) fn_loc
	rule      = Rule rule_name pat_fvs pats (mkVarApps (Var spec_id) bndrs)
    in
    returnUs (rule, (spec_id, spec_rhs))
\end{code}
