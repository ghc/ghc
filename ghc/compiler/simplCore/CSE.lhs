%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section{Common subexpression}

\begin{code}
module CSE (
	cseProgram
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_D_dump_cse, opt_D_verbose_core2core )
import Id		( Id, idType )
import CoreUtils	( hashExpr, cheapEqExpr, exprIsBig )
import Const		( isBoxedDataCon )
import Type		( splitTyConApp_maybe )
import CoreSyn
import VarEnv	
import CoreLint		( beginPass, endPass )
import Outputable
import Util		( mapAccumL )
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

So we carry an extra var->var mapping which we apply *before* looking up in the
reverse mapping.


IMPORTANT NOTE
~~~~~~~~~~~~~~
This pass relies on the no-shadowing invariant, so it must run
immediately after the simplifier.

For example, consider
	f = \x -> let y = x+x in
	              h = \x -> x+x
	          in ...

Here we must *not* do CSE on the inner x+x!


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


Yet another wrinkle
~~~~~~~~~~~~~~~~~~~
Consider
	case (h x) of y -> ...(h x)...

We'd like to replace (h x) in the alternative, by y.  But because of
the preceding "Another important wrinkle", we only want to add the mapping
	scrutinee -> case binder
to the CSE mapping if the scrutinee is a non-trivial expression.


%************************************************************************
%*									*
\section{Common subexpression}
%*									*
%************************************************************************

\begin{code}
cseProgram :: [CoreBind] -> IO [CoreBind]

cseProgram binds
  = do {
	beginPass "Common sub-expression";
	let { binds' = cseBinds emptyCSEnv binds };
	endPass "Common sub-expression" 
	 	(opt_D_dump_cse || opt_D_verbose_core2core)
		binds'	
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
			 

do_one env (id, rhs) = case lookupCSEnv env rhs' of
		    	  Just other_id -> (extendSubst env id other_id, (id, Var other_id))
			  Nothing       -> (addCSEnvItem env id rhs',    (id, rhs'))
		     where
	 	        rhs' = cseExpr env rhs


tryForCSE :: CSEnv -> CoreExpr -> CoreExpr
tryForCSE env (Type t) = Type t
tryForCSE env expr     = case lookupCSEnv env expr' of
			    Just id  -> Var id
 			    Nothing  -> expr'
		       where
	 	         expr' = cseExpr env expr


cseExpr :: CSEnv -> CoreExpr -> CoreExpr
cseExpr env (Var v)		   = Var (lookupSubst env v)
cseExpr env (App f (Type t)) 	   = App (cseExpr env f) (Type t)
cseExpr env (App f a)        	   = App (cseExpr env f) (tryForCSE env a)
cseExpr env expr@(Con con args)    = case lookupCSEnv env expr of
				       Just id  -> Var id
				       Nothing  -> Con con [tryForCSE env arg | arg <- args]
cseExpr env (Note n e)       	   = Note n (cseExpr env e)
cseExpr env (Lam b e)	     	   = Lam b (cseExpr env e)
cseExpr env (Let bind e)    	   = let (env1, bind') = cseBind env bind
				     in Let bind' (cseExpr env1 e)
cseExpr env (Type t)		   = Type t
cseExpr env (Case scrut bndr alts) = Case scrut' bndr (cseAlts env scrut' bndr alts)
				   where
				     scrut' = tryForCSE env scrut


cseAlts env new_scrut bndr alts
  = map cse_alt alts
  where
    (con_target, alt_env)
	= case new_scrut of
		Var v -> (v,    extendSubst env bndr v)		-- See "another important wrinkle"
								-- map: bndr -> v

		other -> (bndr, extendCSEnv env bndr new_scrut)	-- See "yet another wrinkle"
								-- map: new_scrut -> bndr

    arg_tys = case splitTyConApp_maybe (idType bndr) of
		Just (_, arg_tys) -> map Type arg_tys
		other		  -> pprPanic "cseAlts" (ppr bndr)

    cse_alt (con, args, rhs)
	| null args || not (isBoxedDataCon con) = (con, args, cseExpr alt_env rhs)
		-- Don't try CSE if there are no args; it just increases the number
		-- of live vars.  E.g.
		--	case x of { True -> ....True.... }
		-- Don't replace True by x!  
		-- Hence the 'null args', which also deal with literals and DEFAULT
		-- And we can't CSE on unboxed tuples
	| otherwise
	= (con, args, cseExpr (extendCSEnv alt_env con_target (Con con (arg_tys ++ (map varToCoreExpr args)))) rhs)
\end{code}


%************************************************************************
%*									*
\section{The CSE envt}
%*									*
%************************************************************************

\begin{code}
data CSEnv = CS (UniqFM [(Id, CoreExpr)])	-- The expr in the range has already been CSE'd
	        (IdEnv Id)			-- Simple substitution

emptyCSEnv = CS emptyUFM emptyVarEnv

lookupCSEnv :: CSEnv -> CoreExpr -> Maybe Id
lookupCSEnv (CS cs _) expr
  = case lookupUFM cs (hashExpr expr) of
	Nothing -> Nothing
	Just pairs -> lookup_list pairs expr

lookup_list :: [(Id,CoreExpr)] -> CoreExpr -> Maybe Id
lookup_list [] expr = Nothing
lookup_list ((x,e):es) expr | cheapEqExpr e expr = Just x
			    | otherwise	   	 = lookup_list es expr

addCSEnvItem env id expr | exprIsBig expr = env
			 | otherwise      = extendCSEnv env id expr

extendCSEnv (CS cs sub) id expr
  = CS (addToUFM_C combine cs hash [(id, expr)]) sub
  where
    hash   = hashExpr expr
    combine old new = WARN( length result > 4, text "extendCSEnv: long list:" <+> ppr result )
		      result
		    where
		      result = new ++ old

lookupSubst (CS _ sub) x = case lookupVarEnv sub x of
			     Just y  -> y
			     Nothing -> x

extendSubst (CS cs sub) x y = CS cs (extendVarEnv sub x y)
\end{code}
