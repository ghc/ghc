%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section{Common subexpression}

\begin{code}
module CSE (
	cseProgram
    ) where

#include "HsVersions.h"

import DynFlags	( DynFlag(..), DynFlags )
import Id		( Id, idType, idInlinePragma )
import CoreUtils	( hashExpr, cheapEqExpr, exprIsBig, mkAltExpr, exprIsCheap )
import DataCon		( isUnboxedTupleCon )
import Type		( tyConAppArgs )
import CoreSyn
import VarEnv	
import CoreLint		( showPass, endPass )
import Outputable
import BasicTypes	( isAlwaysActive )
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


[Note: SHADOWING]
~~~~~~~~~~~~~~~~~
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

(In fact, I think the simplifier does guarantee no-shadowing for type variables.)


[Note: case binders 1]
~~~~~~~~~~~~~~~~~~~~~~
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

[Note: case binders 2]
~~~~~~~~~~~~~~~~~~~~~~
Consider
	case (h x) of y -> ...(h x)...

We'd like to replace (h x) in the alternative, by y.  But because of
the preceding [Note: case binders 1], we only want to add the mapping
	scrutinee -> case binder
to the reverse CSE mapping if the scrutinee is a non-trivial expression.
(If the scrutinee is a simple variable we want to add the mapping
	case binder -> scrutinee 
to the substitution

[Note: unboxed tuple case binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
	case f x of t { (# a,b #) -> 
	case ... of
	  True -> f x
	  False -> 0 }

We must not replace (f x) by t, because t is an unboxed-tuple binder.
Instead, we shoudl replace (f x) by (# a,b #).  That is, the "reverse mapping" is
	f x --> (# a,b #)
That is why the CSEMap has pairs of expressions.

Note [INLINE and NOINLINE]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We are careful to do no CSE inside functions that the user has marked as
INLINE or NOINLINE.  In terms of Core, that means 

	a) we do not do CSE inside (Note InlineMe e)

	b) we do not do CSE on the RHS of a binding b=e
	   unless b's InlinePragma is AlwaysActive

Here's why (examples from Roman Leshchinskiy).  Consider

	yes :: Int
	{-# NOINLINE yes #-}
	yes = undefined

	no :: Int
	{-# NOINLINE no #-}
	no = undefined

	foo :: Int -> Int -> Int
	{-# NOINLINE foo #-}
	foo m n = n

	{-# RULES "foo/no" foo no = id #-}

	bar :: Int -> Int
	bar = foo yes

We do not expect the rule to fire.  But if we do CSE, then we get
yes=no, and the rule does fire.  Worse, whether we get yes=no or
no=yes depends on the order of the definitions.

In general, CSE should probably never touch things with INLINE pragmas
as this could lead to surprising results.  Consider

	{-# INLINE foo #-}
	foo = <rhs>

	{-# NOINLINE bar #-}
	bar = <rhs>	-- Same rhs as foo

If CSE produces
	foo = bar
then foo will never be inlined (when it should be); but if it produces
	bar = foo
bar will be inlined (when it should not be). Even if we remove INLINE foo,
we'd still like foo to be inlined if rhs is small. This won't happen
with foo = bar.

Not CSE-ing inside INLLINE also solves an annoying bug in CSE. Consider
a worker/wrapper, in which the worker has turned into a single variable:
	$wf = h
	f = \x -> ...$wf...
Now CSE may transoform to
	f = \x -> ...h...
But the WorkerInfo for f still says $wf, which is now dead!  This won't
happen now that we don't look inside INLINEs (which wrappers are).


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
cseBind env (NonRec b e) = let (env', (b',e')) = do_one env (b, e)
			   in (env', NonRec b' e')
cseBind env (Rec pairs)  = let (env', pairs') = mapAccumL do_one env pairs
			   in (env', Rec pairs')
			 

do_one env (id, rhs) 
  = case lookupCSEnv env rhs' of
	Just (Var other_id) -> (extendSubst env' id other_id, 	  (id', Var other_id))
	Just other_expr     -> (env', 			      	  (id', other_expr))
	Nothing             -> (addCSEnvItem env' rhs' (Var id'), (id', rhs'))
  where
    (env', id') = addBinder env id
    rhs' | isAlwaysActive (idInlinePragma id) = cseExpr env' rhs
	 | otherwise			      = rhs
		-- See Note [INLINE and NOINLINE]

tryForCSE :: CSEnv -> CoreExpr -> CoreExpr
tryForCSE env (Type t) = Type t
tryForCSE env expr     = case lookupCSEnv env expr' of
			    Just smaller_expr -> smaller_expr
 			    Nothing  	      -> expr'
		       where
	 	         expr' = cseExpr env expr

cseExpr :: CSEnv -> CoreExpr -> CoreExpr
cseExpr env (Type t)		   = Type t
cseExpr env (Lit lit)		   = Lit lit
cseExpr env (Var v)		   = Var (lookupSubst env v)
cseExpr env (App f a)        	   = App (cseExpr env f) (tryForCSE env a)
cseExpr evn (Note InlineMe e)	   = Note InlineMe e	-- See Note [INLINE and NOINLINE]
cseExpr env (Note n e)       	   = Note n (cseExpr env e)
cseExpr env (Cast e co)            = Cast (cseExpr env e) co
cseExpr env (Lam b e)	     	   = let (env', b') = addBinder env b
				     in Lam b' (cseExpr env' e)
cseExpr env (Let bind e)    	   = let (env', bind') = cseBind env bind
				     in Let bind' (cseExpr env' e)
cseExpr env (Case scrut bndr ty alts) = Case scrut' bndr' ty (cseAlts env' scrut' bndr bndr' alts)
				   where
				     scrut' = tryForCSE env scrut
				     (env', bndr') = addBinder env bndr


cseAlts env scrut' bndr bndr' [(DataAlt con, args, rhs)]
  | isUnboxedTupleCon con
	-- Unboxed tuples are special because the case binder isn't
	-- a real values.  See [Note: unboxed tuple case binders]
  = [(DataAlt con, args', tryForCSE new_env rhs)]
  where
    (env', args') = addBinders env args
    new_env | exprIsCheap scrut' = env'
	    | otherwise 	 = extendCSEnv env' scrut' tup_value
    tup_value = mkAltExpr (DataAlt con) args' (tyConAppArgs (idType bndr))

cseAlts env scrut' bndr bndr' alts
  = map cse_alt alts
  where
    (con_target, alt_env)
	= case scrut' of
		Var v' -> (v',    extendSubst env bndr v')	-- See [Note: case binder 1]
								-- map: bndr -> v'

		other ->  (bndr', extendCSEnv env scrut' (Var  bndr'))	-- See [Note: case binder 2]
									-- map: scrut' -> bndr'

    arg_tys = tyConAppArgs (idType bndr)

    cse_alt (DataAlt con, args, rhs)
	| not (null args)
		-- Don't try CSE if there are no args; it just increases the number
		-- of live vars.  E.g.
		--	case x of { True -> ....True.... }
		-- Don't replace True by x!  
		-- Hence the 'null args', which also deal with literals and DEFAULT
	= (DataAlt con, args', tryForCSE new_env rhs)
	where
	  (env', args') = addBinders alt_env args
	  new_env       = extendCSEnv env' (mkAltExpr (DataAlt con) args' arg_tys)
					   (Var con_target)

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

type CSEMap = UniqFM [(CoreExpr, CoreExpr)]	-- This is the reverse mapping
	-- It maps the hash-code of an expression e to list of (e,e') pairs
	-- This means that it's good to replace e by e'
	-- INVARIANT: The expr in the range has already been CSE'd

emptyCSEnv = CS emptyUFM emptyInScopeSet emptyVarEnv

lookupCSEnv :: CSEnv -> CoreExpr -> Maybe CoreExpr
lookupCSEnv (CS cs _ _) expr
  = case lookupUFM cs (hashExpr expr) of
	Nothing -> Nothing
	Just pairs -> lookup_list pairs expr

lookup_list :: [(CoreExpr,CoreExpr)] -> CoreExpr -> Maybe CoreExpr
lookup_list [] expr = Nothing
lookup_list ((e,e'):es) expr | cheapEqExpr e expr = Just e'
			     | otherwise	  = lookup_list es expr

addCSEnvItem env expr expr' | exprIsBig expr = env
			    | otherwise      = extendCSEnv env expr expr'
   -- We don't try to CSE big expressions, because they are expensive to compare
   -- (and are unlikely to be the same anyway)

extendCSEnv (CS cs in_scope sub) expr expr'
  = CS (addToUFM_C combine cs hash [(expr, expr')]) in_scope sub
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
