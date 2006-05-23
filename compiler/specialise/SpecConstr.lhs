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
import CoreUtils	( exprType, tcEqExpr, mkPiTypes )
import CoreFVs 		( exprsFreeVars )
import CoreSubst	( Subst, mkSubst, substExpr )
import CoreTidy		( tidyRules )
import PprCore		( pprRules )
import WwLib		( mkWorkerArgs )
import DataCon		( dataConRepArity, isVanillaDataCon )
import Type		( tyConAppArgs, tyVarsOfTypes )
import Unify		( coreRefineTys )
import Id		( Id, idName, idType, isDataConWorkId_maybe, 
			  mkUserLocal, mkSysLocal )
import Var		( Var )
import VarEnv
import VarSet
import Name		( nameOccName, nameSrcLoc )
import Rules		( addIdSpecialisations, mkLocalRule, rulesOfBinds )
import OccName		( mkSpecOcc )
import ErrUtils		( dumpIfSet_dyn )
import DynFlags		( DynFlags, DynFlag(..) )
import BasicTypes	( Activation(..) )
import Maybes		( orElse )
import Util		( mapAccumL, lengthAtLeast, notNull )
import List		( nubBy, partition )
import UniqSupply
import Outputable
import FastString
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


What to abstract over
~~~~~~~~~~~~~~~~~~~~~
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
	  the fn defn.  But see Note [Shadowing] below.


NOTICE that we only abstract over variables that are not in scope,
so we're in no danger of shadowing variables used in "higher up"
in f_spec's RHS.


Note [Shadowing]
~~~~~~~~~~~~~~~~
In this pass we gather up usage information that may mention variables
that are bound between the usage site and the definition site; or (more
seriously) may be bound to something different at the definition site.
For example:

	f x = letrec g y v = let x = ... 
			     in ...(g (a,b) x)...

Since 'x' is in scope at the call site, we may make a rewrite rule that 
looks like
	RULE forall a,b. g (a,b) x = ...
But this rule will never match, because it's really a different 'x' at 
the call site -- and that difference will be manifest by the time the
simplifier gets to it.  [A worry: the simplifier doesn't *guarantee*
no-shadowing, so perhaps it may not be distinct?]

Anyway, the rule isn't actually wrong, it's just not useful.  One possibility
is to run deShadowBinds before running SpecConstr, but instead we run the
simplifier.  That gives the simplest possible program for SpecConstr to
chew on; and it virtually guarantees no shadowing.


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
		  (pprRules (tidyRules emptyTidyEnv (rulesOfBinds binds')))

	return binds'
  where
    go env []	        = returnUs []
    go env (bind:binds) = scBind env bind 	`thenUs` \ (env', _, bind') ->
			  go env' binds 	`thenUs` \ binds' ->
			  returnUs (bind' : binds')
\end{code}


%************************************************************************
%*									*
\subsection{Environment: goes downwards}
%*									*
%************************************************************************

\begin{code}
data ScEnv = SCE { scope :: VarEnv HowBound,
			-- Binds all non-top-level variables in scope

		   cons  :: ConstrEnv
	     }

type ConstrEnv = IdEnv ConValue
data ConValue  = CV AltCon [CoreArg]
	-- Variables known to be bound to a constructor
	-- in a particular case alternative


instance Outputable ConValue where
   ppr (CV con args) = ppr con <+> interpp'SP args

refineConstrEnv :: Subst -> ConstrEnv -> ConstrEnv
-- The substitution is a type substitution only
refineConstrEnv subst env = mapVarEnv refine_con_value env
  where
    refine_con_value (CV con args) = CV con (map (substExpr subst) args)

emptyScEnv = SCE { scope = emptyVarEnv, cons = emptyVarEnv }

data HowBound = RecFun		-- These are the recursive functions for which 
				-- we seek interesting call patterns

	      | RecArg		-- These are those functions' arguments; we are
				-- interested to see if those arguments are scrutinised

	      | Other		-- We track all others so we know what's in scope
				-- This is used in spec_one to check what needs to be
				-- passed as a parameter and what is in scope at the 
				-- function definition site

instance Outputable HowBound where
  ppr RecFun = text "RecFun"
  ppr RecArg = text "RecArg"
  ppr Other = text "Other"

lookupScopeEnv env v = lookupVarEnv (scope env) v

extendBndrs env bndrs = env { scope = extendVarEnvList (scope env) [(b,Other) | b <- bndrs] }
extendBndr  env bndr  = env { scope = extendVarEnv (scope env) bndr Other }

    -- When we encounter
    --	case scrut of b
    --	    C x y -> ...
    -- we want to bind b, and perhaps scrut too, to (C x y)
extendCaseBndrs :: ScEnv -> Id -> CoreExpr -> AltCon -> [Var] -> ScEnv
extendCaseBndrs env case_bndr scrut DEFAULT alt_bndrs
  = extendBndrs env (case_bndr : alt_bndrs)

extendCaseBndrs env case_bndr scrut con@(LitAlt lit) alt_bndrs
  = ASSERT( null alt_bndrs ) extendAlt env case_bndr scrut (CV con []) []

extendCaseBndrs env case_bndr scrut con@(DataAlt data_con) alt_bndrs
  | isVanillaDataCon data_con
  = extendAlt env case_bndr scrut (CV con vanilla_args) alt_bndrs
    
  | otherwise	-- GADT
  = extendAlt env1 case_bndr scrut (CV con gadt_args) alt_bndrs
  where
    vanilla_args = map Type (tyConAppArgs (idType case_bndr)) ++
		   map varToCoreExpr alt_bndrs

    gadt_args = map (substExpr subst . varToCoreExpr) alt_bndrs
	-- This call generates some bogus warnings from substExpr,
	-- because it's inconvenient to put all the Ids in scope
	-- Will be fixed when we move to FC

    (alt_tvs, _) = span isTyVar alt_bndrs
    Just (tv_subst, is_local) = coreRefineTys data_con alt_tvs (idType case_bndr)
    subst = mkSubst in_scope tv_subst emptyVarEnv	-- No Id substitition
    in_scope = mkInScopeSet (tyVarsOfTypes (varEnvElts tv_subst))

    env1 | is_local  = env
	 | otherwise = env { cons = refineConstrEnv subst (cons env) }



extendAlt :: ScEnv -> Id -> CoreExpr -> ConValue -> [Var] -> ScEnv
extendAlt env case_bndr scrut val alt_bndrs
  = let 
       env1 = SCE { scope = extendVarEnvList (scope env) [(b,Other) | b <- case_bndr : alt_bndrs],
		    cons  = extendVarEnv     (cons  env) case_bndr val }
    in
    case scrut of
	Var v ->   -- Bind the scrutinee in the ConstrEnv if it's a variable
		   -- Also forget if the scrutinee is a RecArg, because we're
		   -- now in the branch of a case, and we don't want to
		   -- record a non-scrutinee use of v if we have
		   --	case v of { (a,b) -> ...(f v)... }
		 SCE { scope = extendVarEnv (scope env1) v Other,
		       cons  = extendVarEnv (cons env1)  v val }
	other -> env1

    -- When we encounter a recursive function binding
    --	f = \x y -> ...
    -- we want to extend the scope env with bindings 
    -- that record that f is a RecFn and x,y are RecArgs
extendRecBndr env fn bndrs
  =  env { scope = scope env `extendVarEnvList` 
		   ((fn,RecFun): [(bndr,RecArg) | bndr <- bndrs]) }
\end{code}


%************************************************************************
%*									*
\subsection{Usage information: flows upwards}
%*									*
%************************************************************************

\begin{code}
data ScUsage
   = SCU {
	calls :: !(IdEnv ([Call])),	-- Calls
					-- The functions are a subset of the 
					-- 	RecFuns in the ScEnv

	occs :: !(IdEnv ArgOcc)		-- Information on argument occurrences
     }					-- The variables are a subset of the 
					--	RecArg in the ScEnv

type Call = (ConstrEnv, [CoreArg])
	-- The arguments of the call, together with the
	-- env giving the constructor bindings at the call site

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

The main recursive function gathers up usage information, and
creates specialised versions of functions.

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

scExpr env (Case scrut b ty alts) 
  = sc_scrut scrut		`thenUs` \ (scrut_usg, scrut') ->
    mapAndUnzipUs sc_alt alts	`thenUs` \ (alts_usgs, alts') ->
    returnUs (combineUsages alts_usgs `combineUsage` scrut_usg,
	      Case scrut' b ty alts')
  where
    sc_scrut e@(Var v) = returnUs (varUsage env v CaseScrut, e)
    sc_scrut e	       = scExpr env e

    sc_alt (con,bs,rhs) = scExpr env1 rhs	`thenUs` \ (usg,rhs') ->
			  returnUs (usg, (con,bs,rhs'))
			where
			  env1 = extendCaseBndrs env b scrut con bs

scExpr env (Let bind body)
  = scBind env bind	`thenUs` \ (env', bind_usg, bind') ->
    scExpr env' body	`thenUs` \ (body_usg, body') ->
    returnUs (bind_usg `combineUsage` body_usg, Let bind' body')

scExpr env e@(App _ _) 
  = let 
	(fn, args) = collectArgs e
    in
    mapAndUnzipUs (scExpr env) (fn:args)	`thenUs` \ (usgs, (fn':args')) ->
	-- Process the function too.   It's almost always a variable,
	-- but not always.  In particular, if this pass follows float-in,
	-- which it may, we can get 
	--	(let f = ...f... in f) arg1 arg2
    let
	call_usg = case fn of
		 	Var f | Just RecFun <- lookupScopeEnv env f
			      -> SCU { calls = unitVarEnv f [(cons env, args)], 
				       occs  = emptyVarEnv }
			other -> nullUsage
    in
    returnUs (combineUsages usgs `combineUsage` call_usg, mkApps fn' args')


----------------------
scBind :: ScEnv -> CoreBind -> UniqSM (ScEnv, ScUsage, CoreBind)
scBind env (Rec [(fn,rhs)])
  | notNull val_bndrs
  = scExpr env_fn_body body		`thenUs` \ (usg, body') ->
    specialise env fn bndrs body usg	`thenUs` \ (rules, spec_prs) ->
    let
	SCU { calls = calls, occs = occs } = usg
    in
    returnUs (extendBndr env fn,	-- For the body of the letrec, just
					-- extend the env with Other to record 
					-- that it's in scope; no funny RecFun business
	      SCU { calls = calls `delVarEnv` fn, occs = occs `delVarEnvList` val_bndrs},
	      Rec ((fn `addIdSpecialisations` rules, mkLams bndrs body') : spec_prs))
  where
    (bndrs,body) = collectBinders rhs
    val_bndrs    = filter isId bndrs
    env_fn_body	 = extendRecBndr env fn bndrs

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
  | Just RecArg <- lookupScopeEnv env v = SCU { calls = emptyVarEnv, 
						occs = unitVarEnv v use }
  | otherwise		   	        = nullUsage
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
		     | (con_env, call_args) <- all_calls,
		       call_args `lengthAtLeast` n_bndrs,	    -- App is saturated
		       let call = bndrs `zip` call_args,
		       any (good_arg con_env occs) call,    -- At least one arg is a constr app
		       let (_, pats) = argsToPats con_env us call_args
		     ]
    in
    mapAndUnzipUs (spec_one env fn (mkLams bndrs body)) 
		  (nubBy same_call good_calls `zip` [1..])
  where
    n_bndrs  = length bndrs
    same_call as1 as2 = and (zipWith tcEqExpr as1 as2)

---------------------
good_arg :: ConstrEnv -> IdEnv ArgOcc -> (CoreBndr, CoreArg) -> Bool
good_arg con_env arg_occs (bndr, arg)
  = case is_con_app_maybe con_env arg of	
	Just _ ->  bndr_usg_ok arg_occs bndr arg
	other   -> False

bndr_usg_ok :: IdEnv ArgOcc -> Var -> CoreArg -> Bool
bndr_usg_ok arg_occs bndr arg
  = case lookupVarEnv arg_occs bndr of
	Just CaseScrut -> True			-- Used only by case scrutiny
	Just Both      -> case arg of		-- Used by case and elsewhere
			    App _ _ -> True	-- so the arg should be an explicit con app
			    other   -> False
	other -> False				-- Not used, or used wonkily
    

---------------------
spec_one :: ScEnv
	 -> Id					-- Function
	 -> CoreExpr				-- Rhs of the original function
	 -> ([CoreArg], Int)
	 -> UniqSM (CoreRule, (Id,CoreExpr))	-- Rule and binding

-- spec_one creates a specialised copy of the function, together
-- with a rule for using it.  I'm very proud of how short this
-- function is, considering what it does :-).

{- 
  Example
  
     In-scope: a, x::a   
     f = /\b \y::[(a,b)] -> ....f (b,c) ((:) (a,(b,c)) (x,v) (h w))...
	  [c::*, v::(b,c) are presumably bound by the (...) part]
  ==>
     f_spec = /\ b c \ v::(b,c) hw::[(a,(b,c))] ->
		  (...entire RHS of f...) (b,c) ((:) (a,(b,c)) (x,v) hw)
  
     RULE:  forall b::* c::*,		-- Note, *not* forall a, x
		   v::(b,c),
		   hw::[(a,(b,c))] .
  
	    f (b,c) ((:) (a,(b,c)) (x,v) hw) = f_spec b c v hw
-}

spec_one env fn rhs (pats, rule_number)
  = getUniqueUs 		`thenUs` \ spec_uniq ->
    let 
	fn_name      = idName fn
	fn_loc       = nameSrcLoc fn_name
	spec_occ     = mkSpecOcc (nameOccName fn_name)
	pat_fvs	     = varSetElems (exprsFreeVars pats)
	vars_to_bind = filter not_avail pat_fvs
		-- See Note [Shadowing] at the top

	not_avail v  = not (v `elemVarEnv` scope env)
		-- Put the type variables first; the type of a term
		-- variable may mention a type variable
	(tvs, ids)   = partition isTyVar vars_to_bind
	bndrs  	     = tvs ++ ids
	spec_body    = mkApps rhs pats
	body_ty	     = exprType spec_body
	
	(spec_lam_args, spec_call_args) = mkWorkerArgs bndrs body_ty
		-- Usual w/w hack to avoid generating 
		-- a spec_rhs of unlifted type and no args
	
	rule_name = mkFastString ("SC:" ++ showSDoc (ppr fn <> int rule_number))
	spec_rhs  = mkLams spec_lam_args spec_body
	spec_id   = mkUserLocal spec_occ spec_uniq (mkPiTypes spec_lam_args body_ty) fn_loc
	rule_rhs  = mkVarApps (Var spec_id) spec_call_args
	rule      = mkLocalRule rule_name specConstrActivation fn_name bndrs pats rule_rhs
    in
    returnUs (rule, (spec_id, spec_rhs))

-- In which phase should the specialise-constructor rules be active?
-- Originally I made them always-active, but Manuel found that
-- this defeated some clever user-written rules.  So Plan B
-- is to make them active only in Phase 0; after all, currently,
-- the specConstr transformation is only run after the simplifier
-- has reached Phase 0.  In general one would want it to be 
-- flag-controllable, but for now I'm leaving it baked in
--					[SLPJ Oct 01]
specConstrActivation :: Activation
specConstrActivation = ActiveAfter 0	-- Baked in; see comments above
\end{code}

%************************************************************************
%*									*
\subsection{Argument analysis}
%*									*
%************************************************************************

This code deals with analysing call-site arguments to see whether
they are constructor applications.

\begin{code}
    -- argToPat takes an actual argument, and returns an abstracted
    -- version, consisting of just the "constructor skeleton" of the
    -- argument, with non-constructor sub-expression replaced by new
    -- placeholder variables.  For example:
    --    C a (D (f x) (g y))  ==>  C p1 (D p2 p3)

argToPat   :: ConstrEnv -> UniqSupply -> CoreArg -> (UniqSupply, CoreExpr)
argToPat env us (Type ty) 
  = (us, Type ty)

argToPat env us arg
  | Just (CV dc args) <- is_con_app_maybe env arg
  = let
	(us',args') = argsToPats env us args
    in
    (us', mk_con_app dc args')

argToPat env us (Var v)	-- Don't uniqify existing vars,
  = (us, Var v)		-- so that we can spot when we pass them twice

argToPat env us arg
  = (us1, Var (mkSysLocal FSLIT("sc") (uniqFromSupply us2) (exprType arg)))
  where
    (us1,us2) = splitUniqSupply us

argsToPats :: ConstrEnv -> UniqSupply -> [CoreArg] -> (UniqSupply, [CoreExpr])
argsToPats env us args = mapAccumL (argToPat env) us args
\end{code}


\begin{code}
is_con_app_maybe :: ConstrEnv -> CoreExpr -> Maybe ConValue
is_con_app_maybe env (Var v)
  = lookupVarEnv env v
	-- You might think we could look in the idUnfolding here
	-- but that doesn't take account of which branch of a 
	-- case we are in, which is the whole point

is_con_app_maybe env (Lit lit)
  = Just (CV (LitAlt lit) [])

is_con_app_maybe env expr
  = case collectArgs expr of
	(Var fun, args) | Just con <- isDataConWorkId_maybe fun,
			  args `lengthAtLeast` dataConRepArity con
		-- Might be > because the arity excludes type args
		        -> Just (CV (DataAlt con) args)

	other -> Nothing

mk_con_app :: AltCon -> [CoreArg] -> CoreExpr
mk_con_app (LitAlt lit)  []   = Lit lit
mk_con_app (DataAlt con) args = mkConApp con args
\end{code}
