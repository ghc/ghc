%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section{Common subexpression}

\begin{code}
module CSE (
	cseProgram
    ) where

#include "HsVersions.h"

import CmdLineOpts	( DynFlag(..), DynFlags )
import Id		( Id, idType, idWorkerInfo )
import IdInfo		( workerExists )
import CoreUtils	( hashExpr, cheapEqExpr, exprIsBig, mkAltExpr )
import DataCon		( isUnboxedTupleCon )
import Type		( tyConAppArgs )
import Subst		( InScopeSet, uniqAway, emptyInScopeSet, 
			  extendInScopeSet, elemInScopeSet )
import CoreSyn
import VarEnv	
import CoreLint		( showPass, endPass )
import Outputable
import Util		( mapAccumL, lengthExceeds )
import UniqFM
\end{code}


			Simple common sub-expression
			~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we see
	x1 = C a b
	x2 = C x1 b
we build up a reverse mapping:   C a b  -> x1
				 C x1 b -> x2
and apply that to the rest of the program.

When we then see
	y1 = C a b
	y2 = C y1 b
we replace the C a b with x1.  But then we *dont* want to
add   x1 -> y1  to the mapping.  Rather, we want the reverse, y1 -> x1
so that a subsequent binding
	y2 = C y1 b
will get transformed to C x1 b, and then to x2.  

So we carry an extra var->var substitution which we apply *before* looking up in the
reverse mapping.


IMPORTANT NOTE
~~~~~~~~~~~~~~
We have to be careful about shadowing.
For example, consider
	f = \x -> let y = x+x in
	              h = \x -> x+x
	          in ...

Here we must *not* do CSE on the inner x+x!  The simplifier used to guarantee no
shadowing, but it doesn't any more (it proved too hard), so we clone as we go.
We can simply add clones to the substitution already described.

However, we do NOT clone type variables.  It's just too hard, because then we need
to run the substitution over types and IdInfo.  No no no.  Instead, we just throw
away the entire reverse mapping if this unusual situation ever shows up.   
(In fact, I think the simplifier does guarantee no-shadowing for type variables.)


Another important wrinkle
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

	f = \x -> case x of wild { 
			(a:as) -> case a of wild1 {
				    (p,q) -> ...(wild1:as)...

Here, (wild1:as) is morally the same as (a:as) and hence equal to wild.
But that's not quite obvious.  In general we want to keep it as (wild1:as),
but for CSE purpose that's a bad idea.

So we add the binding (wild1 -> a) to the extra var->var mapping.
Notice this is exactly backwards to what the simplifier does, which is
to try to replaces uses of a with uses of wild1

Yet another wrinkle
~~~~~~~~~~~~~~~~~~~
Consider
	case (h x) of y -> ...(h x)...

We'd like to replace (h x) in the alternative, by y.  But because of
the preceding "Another important wrinkle", we only want to add the mapping
	scrutinee -> case binder
to the reverse CSE mapping if the scrutinee is a non-trivial expression.
(If the scrutinee is a simple variable we want to add the mapping
	case binder -> scrutinee 
to the substitution


%************************************************************************
%*									*
\section{Common subexpression}
%*									*
%************************************************************************

\begin{code}
cseProgram :: DynFlags -> [CoreBind] -> IO [CoreBind]

cseProgram dflags binds
  = do {
	showPass dflags "Common sub-expression";
	let { binds' = cseBinds emptyCSEnv binds };
	endPass dflags "Common sub-expression" 	Opt_D_dump_cse binds'	
    }

cseBinds :: CSEnv -> [CoreBind] -> [CoreBind]
cseBinds env []     = []
cseBinds env (b:bs) = (b':bs')
		    where
		      (env1, b') = cseBind  env  b
		      bs'        = cseBinds env1 bs

cseBind :: CSEnv -> CoreBind -> (CSEnv, CoreBind)
cseBind env (NonRec b e) = let (env', (_,e')) = do_one env (b, e)
			   in (env', NonRec b e')
cseBind env (Rec pairs)  = let (env', pairs') = mapAccumL do_one env pairs
			   in (env', Rec pairs')
			 

do_one env (id, rhs) 
  = case lookupCSEnv env rhs' of
	Just other_id -> (extendSubst env' id other_id, (id', Var other_id))
	Nothing       -> (addCSEnvItem env' id' rhs',   (id', rhs'))
  where
    (env', id') = addBinder env id
    rhs' | not (workerExists (idWorkerInfo id)) = cseExpr env' rhs

		-- Hack alert: don't do CSE on wrapper RHSs.
		-- Otherwise we find:
		--	$wf = h
		--	f = \x -> ...$wf...
		-- ===>
		--	f = \x -> ...h...
		-- But the WorkerInfo for f still says $wf, which is now dead!
	  | otherwise = rhs


tryForCSE :: CSEnv -> CoreExpr -> CoreExpr
tryForCSE env (Type t) = Type t
tryForCSE env expr     = case lookupCSEnv env expr' of
			    Just id  -> Var id
 			    Nothing  -> expr'
		       where
	 	         expr' = cseExpr env expr

cseExpr :: CSEnv -> CoreExpr -> CoreExpr
cseExpr env (Type t)		   = Type t
cseExpr env (Lit lit)		   = Lit lit
cseExpr env (Var v)		   = Var (lookupSubst env v)
cseExpr env (App f a)        	   = App (cseExpr env f) (tryForCSE env a)
cseExpr env (Note n e)       	   = Note n (cseExpr env e)
cseExpr env (Lam b e)	     	   = let (env', b') = addBinder env b
				     in Lam b' (cseExpr env' e)
cseExpr env (Let bind e)    	   = let (env', bind') = cseBind env bind
				     in Let bind' (cseExpr env' e)
cseExpr env (Case scrut bndr alts) = Case scrut' bndr' (cseAlts env' scrut' bndr bndr' alts)
				   where
				     scrut' = tryForCSE env scrut
				     (env', bndr') = addBinder env bndr


cseAlts env scrut' bndr bndr' alts
  = map cse_alt alts
  where
    (con_target, alt_env)
	= case scrut' of
		Var v' -> (v',    extendSubst env bndr v')	-- See "another important wrinkle"
								-- map: bndr -> v'

		other ->  (bndr', extendCSEnv env bndr' scrut')	-- See "yet another wrinkle"
								-- map: scrut' -> bndr'

    arg_tys = tyConAppArgs (idType bndr)

    cse_alt (DataAlt con, args, rhs)
	| not (null args || isUnboxedTupleCon con)
		-- Don't try CSE if there are no args; it just increases the number
		-- of live vars.  E.g.
		--	case x of { True -> ....True.... }
		-- Don't replace True by x!  
		-- Hence the 'null args', which also deal with literals and DEFAULT
		-- And we can't CSE on unboxed tuples
	= (DataAlt con, args', tryForCSE new_env rhs)
	where
	  (env', args') = addBinders alt_env args
	  new_env       = extendCSEnv env' con_target (mkAltExpr (DataAlt con) args' arg_tys)

    cse_alt (con, args, rhs)
	= (con, args', tryForCSE env' rhs)
	where
	  (env', args') = addBinders alt_env args
\end{code}


%************************************************************************
%*									*
\section{The CSE envt}
%*									*
%************************************************************************

\begin{code}
data CSEnv = CS CSEMap InScopeSet (IdEnv Id)
			-- Simple substitution

type CSEMap = UniqFM [(Id, CoreExpr)]	-- This is the reverse mapping
	-- It maps the hash-code of an expression to list of (x,e) pairs
	-- This means that it's good to replace e by x
	-- INVARIANT: The expr in the range has already been CSE'd

emptyCSEnv = CS emptyUFM emptyInScopeSet emptyVarEnv

lookupCSEnv :: CSEnv -> CoreExpr -> Maybe Id
lookupCSEnv (CS cs _ _) expr
  = case lookupUFM cs (hashExpr expr) of
	Nothing -> Nothing
	Just pairs -> lookup_list pairs expr

lookup_list :: [(Id,CoreExpr)] -> CoreExpr -> Maybe Id
lookup_list [] expr = Nothing
lookup_list ((x,e):es) expr | cheapEqExpr e expr = Just x
			    | otherwise	   	 = lookup_list es expr

addCSEnvItem env id expr | exprIsBig expr = env
			 | otherwise      = extendCSEnv env id expr
   -- We don't try to CSE big expressions, because they are expensive to compare
   -- (and are unlikely to be the same anyway)

extendCSEnv (CS cs in_scope sub) id expr
  = CS (addToUFM_C combine cs hash [(id, expr)]) in_scope sub
  where
    hash   = hashExpr expr
    combine old new = WARN( result `lengthExceeds` 4, text "extendCSEnv: long list:" <+> ppr result )
		      result
		    where
		      result = new ++ old

lookupSubst (CS _ _ sub) x = case lookupVarEnv sub x of
			       Just y  -> y
			       Nothing -> x

extendSubst (CS cs in_scope sub) x y = CS cs in_scope (extendVarEnv sub x y)

addBinder :: CSEnv -> Id -> (CSEnv, Id)
addBinder env@(CS cs in_scope sub) v
  | not (v `elemInScopeSet` in_scope) = (CS cs (extendInScopeSet in_scope v)  sub,		       v)
  | isId v			      = (CS cs (extendInScopeSet in_scope v') (extendVarEnv sub v v'), v')
  | not (isId v)		      = WARN( True, ppr v )
				        (CS emptyUFM in_scope		      sub,		       v)
	-- This last case is the unusual situation where we have shadowing of
	-- a type variable; we have to discard the CSE mapping
	-- See "IMPORTANT NOTE" at the top 
  where
    v' = uniqAway in_scope v

addBinders :: CSEnv -> [Id] -> (CSEnv, [Id])
addBinders env vs = mapAccumL addBinder env vs
\end{code}
