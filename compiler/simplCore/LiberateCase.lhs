%
% (c) The AQUA Project, Glasgow University, 1994-1998
%
\section[LiberateCase]{Unroll recursion to allow evals to be lifted from a loop}

\begin{code}
module LiberateCase ( liberateCase ) where

#include "HsVersions.h"

import DynFlags
import HscTypes
import CoreLint		( showPass, endPass )
import CoreSyn
import CoreUnfold	( couldBeSmallEnoughToInline )
import Rules		( RuleBase )
import UniqSupply	( UniqSupply )
import SimplMonad	( SimplCount, zeroSimplCount )
import Id
import FamInstEnv
import Type
import Coercion
import TyCon
import VarEnv
import Name		( localiseName )
import Outputable
import Util             ( notNull )
import Data.IORef	( readIORef )
\end{code}

The liberate-case transformation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module walks over @Core@, and looks for @case@ on free variables.
The criterion is:
	if there is case on a free on the route to the recursive call,
	then the recursive call is replaced with an unfolding.

Example

   f = \ t -> case v of
	         V a b -> a : f t

=> the inner f is replaced.

   f = \ t -> case v of
	         V a b -> a : (letrec
				f =  \ t -> case v of
					       V a b -> a : f t
			       in f) t
(note the NEED for shadowing)

=> Simplify

  f = \ t -> case v of
	         V a b -> a : (letrec
				f = \ t -> a : f t
			       in f t)

Better code, because 'a' is  free inside the inner letrec, rather
than needing projection from v.

Other examples we'd like to catch with this kind of transformation

	last []     = error 
	last (x:[]) = x
	last (x:xs) = last xs

We'd like to avoid the redundant pattern match, transforming to

	last [] = error
	last (x:[]) = x
	last (x:(y:ys)) = last' y ys
		where
		  last' y []     = y
		  last' _ (y:ys) = last' y ys

	(is this necessarily an improvement)

Similarly drop:

	drop n [] = []
	drop 0 xs = xs
	drop n (x:xs) = drop (n-1) xs

Would like to pass n along unboxed.
	
Note [Scrutinee with cast]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
    f = \ t -> case (v `cast` co) of
	         V a b -> a : f t

Exactly the same optimistaion (unrolling one call to f) will work here, 
despite the cast.  See mk_alt_env in the Case branch of libCase.


To think about (Apr 94)
~~~~~~~~~~~~~~

Main worry: duplicating code excessively.  At the moment we duplicate
the entire binding group once at each recursive call.  But there may
be a group of recursive calls which share a common set of evaluated
free variables, in which case the duplication is a plain waste.

Another thing we could consider adding is some unfold-threshold thing,
so that we'll only duplicate if the size of the group rhss isn't too
big.

Data types
~~~~~~~~~~

The ``level'' of a binder tells how many
recursive defns lexically enclose the binding
A recursive defn "encloses" its RHS, not its
scope.  For example:
\begin{verbatim}
	letrec f = let g = ... in ...
	in
	let h = ...
	in ...
\end{verbatim}
Here, the level of @f@ is zero, the level of @g@ is one,
and the level of @h@ is zero (NB not one).

Note [Indexed data types]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
	data family T :: * -> *
	data T Int = TI Int

	f :: T Int -> Bool
	f x = case x of { DEFAULT -> <body> }

We would like to change this to
	f x = case x `cast` co of { TI p -> <body> }

so that <body> can make use of the fact that x is already evaluated to
a TI; and a case on a known data type may be more efficient than a
polymorphic one (not sure this is true any longer).  Anyway the former
showed up in Roman's experiments.  Example:
  foo :: FooT Int -> Int -> Int
  foo t n = t `seq` bar n
     where
       bar 0 = 0
       bar n = bar (n - case t of TI i -> i)
Here we'd like to avoid repeated evaluating t inside the loop, by 
taking advantage of the `seq`.

We implement this as part of the liberate-case transformation by 
spotting
	case <scrut> of (x::T) tys { DEFAULT ->  <body> }
where x :: T tys, and T is a indexed family tycon.  Find the
representation type (T77 tys'), and coercion co, and transform to
	case <scrut> `cast` co of (y::T77 tys')
	    DEFAULT -> let x = y `cast` sym co in <body>

The "find the representation type" part is done by looking up in the
family-instance environment.

NB: in fact we re-use x (changing its type) to avoid making a fresh y;
this entails shadowing, but that's ok.

%************************************************************************
%*									*
	 Top-level code
%*									*
%************************************************************************

\begin{code}
liberateCase :: HscEnv -> UniqSupply -> RuleBase -> ModGuts
	     -> IO (SimplCount, ModGuts)
liberateCase hsc_env _ _ guts
  = do	{ let dflags = hsc_dflags hsc_env
	; eps <- readIORef (hsc_EPS hsc_env)
	; let fam_envs = (eps_fam_inst_env eps, mg_fam_inst_env guts)

	; showPass dflags "Liberate case"
	; let { env = initEnv dflags fam_envs
	      ; binds' = do_prog env (mg_binds guts) }
	; endPass dflags "Liberate case" Opt_D_verbose_core2core binds'
			{- no specific flag for dumping -} 
	; return (zeroSimplCount dflags, guts { mg_binds = binds' }) }
  where
    do_prog env [] = []
    do_prog env (bind:binds) = bind' : do_prog env' binds
			     where
			       (env', bind') = libCaseBind env bind
\end{code}


%************************************************************************
%*									*
	 Main payload
%*									*
%************************************************************************

Bindings
~~~~~~~~
\begin{code}
libCaseBind :: LibCaseEnv -> CoreBind -> (LibCaseEnv, CoreBind)

libCaseBind env (NonRec binder rhs)
  = (addBinders env [binder], NonRec binder (libCase env rhs))

libCaseBind env (Rec pairs)
  = (env_body, Rec pairs')
  where
    (binders, rhss) = unzip pairs

    env_body = addBinders env binders

    pairs' = [(binder, libCase env_rhs rhs) | (binder,rhs) <- pairs]

    env_rhs = if all rhs_small_enough rhss then extended_env else env

	-- We extend the rec-env by binding each Id to its rhs, first
	-- processing the rhs with an *un-extended* environment, so
	-- that the same process doesn't occur for ever!
	--
    extended_env = addRecBinds env [ (adjust binder, libCase env_body rhs)
				   | (binder, rhs) <- pairs ]

	-- Two subtle things: 
	-- (a)  Reset the export flags on the binders so
	-- 	that we don't get name clashes on exported things if the 
	-- 	local binding floats out to top level.  This is most unlikely
	-- 	to happen, since the whole point concerns free variables. 
	-- 	But resetting the export flag is right regardless.
	-- 
	-- (b)  Make the name an Internal one.  External Names should never be
	--	nested; if it were floated to the top level, we'd get a name
	-- 	clash at code generation time.
    adjust bndr = setIdNotExported (setIdName bndr (localiseName (idName bndr)))

    rhs_small_enough rhs = couldBeSmallEnoughToInline lIBERATE_BOMB_SIZE rhs
    lIBERATE_BOMB_SIZE   = bombOutSize env
\end{code}


Expressions
~~~~~~~~~~~

\begin{code}
libCase :: LibCaseEnv
	-> CoreExpr
	-> CoreExpr

libCase env (Var v)		= libCaseId env v
libCase env (Lit lit)		= Lit lit
libCase env (Type ty)		= Type ty
libCase env (App fun arg)       = App (libCase env fun) (libCase env arg)
libCase env (Note note body)    = Note note (libCase env body)
libCase env (Cast e co)         = Cast (libCase env e) co

libCase env (Lam binder body)
  = Lam binder (libCase (addBinders env [binder]) body)

libCase env (Let bind body)
  = Let bind' (libCase env_body body)
  where
    (env_body, bind') = libCaseBind env bind

libCase env (Case scrut bndr ty alts)
  = mkCase env (libCase env scrut) bndr ty (map (libCaseAlt env_alts) alts)
  where
    env_alts = addBinders (mk_alt_env scrut) [bndr]
    mk_alt_env (Var scrut_var) = addScrutedVar env scrut_var
    mk_alt_env (Cast scrut _)  = mk_alt_env scrut	-- Note [Scrutinee with cast]
    mk_alt_env otehr	       = env

libCaseAlt env (con,args,rhs) = (con, args, libCase (addBinders env args) rhs)
\end{code}

\begin{code}
mkCase :: LibCaseEnv -> CoreExpr -> Id -> Type -> [CoreAlt] -> CoreExpr
-- See Note [Indexed data types]
mkCase env scrut bndr ty [(DEFAULT,_,rhs)]
  | Just (tycon, tys)   <- splitTyConApp_maybe (idType bndr)
  , [(subst, fam_inst)] <- lookupFamInstEnv (lc_fams env) tycon tys
  = let 
	rep_tc     = famInstTyCon fam_inst
	rep_tys    = map (substTyVar subst) (tyConTyVars rep_tc)
	bndr'      = setIdType bndr (mkTyConApp rep_tc rep_tys)
	Just co_tc = tyConFamilyCoercion_maybe rep_tc
	co	   = mkTyConApp co_tc rep_tys
	bind       = NonRec bndr (Cast (Var bndr') (mkSymCoercion co))
    in mkCase env (Cast scrut co) bndr' ty [(DEFAULT,[],Let bind rhs)]
mkCase env scrut bndr ty alts
  = Case scrut bndr ty alts
\end{code}

Ids
~~~
\begin{code}
libCaseId :: LibCaseEnv -> Id -> CoreExpr
libCaseId env v
  | Just the_bind <- lookupRecId env v	-- It's a use of a recursive thing
  , notNull free_scruts 		-- with free vars scrutinised in RHS
  = Let the_bind (Var v)

  | otherwise
  = Var v

  where
    rec_id_level = lookupLevel env v
    free_scruts  = freeScruts env rec_id_level
\end{code}


%************************************************************************
%*									*
	Utility functions
%*									*
%************************************************************************

\begin{code}
addBinders :: LibCaseEnv -> [CoreBndr] -> LibCaseEnv
addBinders env@(LibCaseEnv { lc_lvl = lvl, lc_lvl_env = lvl_env }) binders
  = env { lc_lvl_env = lvl_env' }
  where
    lvl_env' = extendVarEnvList lvl_env (binders `zip` repeat lvl)

addRecBinds :: LibCaseEnv -> [(Id,CoreExpr)] -> LibCaseEnv
addRecBinds env@(LibCaseEnv {lc_lvl = lvl, lc_lvl_env = lvl_env, 
			     lc_rec_env = rec_env}) pairs
  = env { lc_lvl = lvl', lc_lvl_env = lvl_env', lc_rec_env = rec_env' }
  where
    lvl'     = lvl + 1
    lvl_env' = extendVarEnvList lvl_env [(binder,lvl) | (binder,_) <- pairs]
    rec_env' = extendVarEnvList rec_env [(binder, Rec pairs) | (binder,_) <- pairs]

addScrutedVar :: LibCaseEnv
	      -> Id		-- This Id is being scrutinised by a case expression
	      -> LibCaseEnv

addScrutedVar env@(LibCaseEnv { lc_lvl = lvl, lc_lvl_env = lvl_env, 
				lc_scruts = scruts }) scrut_var
  | bind_lvl < lvl
  = env { lc_scruts = scruts' }
	-- Add to scruts iff the scrut_var is being scrutinised at
	-- a deeper level than its defn

  | otherwise = env
  where
    scruts'  = (scrut_var, lvl) : scruts
    bind_lvl = case lookupVarEnv lvl_env scrut_var of
		 Just lvl -> lvl
		 Nothing  -> topLevel

lookupRecId :: LibCaseEnv -> Id -> Maybe CoreBind
lookupRecId env id = lookupVarEnv (lc_rec_env env) id

lookupLevel :: LibCaseEnv -> Id -> LibCaseLevel
lookupLevel env id
  = case lookupVarEnv (lc_lvl_env env) id of
      Just lvl -> lc_lvl env
      Nothing  -> topLevel

freeScruts :: LibCaseEnv
	   -> LibCaseLevel 	-- Level of the recursive Id
	   -> [Id]		-- Ids that are scrutinised between the binding
				-- of the recursive Id and here
freeScruts env rec_bind_lvl
  = [v | (v,scrut_lvl) <- lc_scruts env, scrut_lvl > rec_bind_lvl]
\end{code}

%************************************************************************
%*									*
	 The environment
%*									*
%************************************************************************

\begin{code}
type LibCaseLevel = Int

topLevel :: LibCaseLevel
topLevel = 0
\end{code}

\begin{code}
data LibCaseEnv
  = LibCaseEnv {
	lc_size :: Int,		-- Bomb-out size for deciding if
				-- potential liberatees are too big.
				-- (passed in from cmd-line args)

	lc_lvl :: LibCaseLevel,	-- Current level

	lc_lvl_env :: IdEnv LibCaseLevel,  
			-- Binds all non-top-level in-scope Ids
			-- (top-level and imported things have
			-- a level of zero)

	lc_rec_env :: IdEnv CoreBind, 
			-- Binds *only* recursively defined ids, 
			-- to their own binding group,
			-- and *only* in their own RHSs

	lc_scruts :: [(Id,LibCaseLevel)],
     			-- Each of these Ids was scrutinised by an
			-- enclosing case expression, with the
			-- specified number of enclosing
			-- recursive bindings; furthermore,
			-- the Id is bound at a lower level
			-- than the case expression.  The order is
			-- insignificant; it's a bag really

	lc_fams :: FamInstEnvs
			-- Instance env for indexed data types 
	}

initEnv :: DynFlags -> FamInstEnvs -> LibCaseEnv
initEnv dflags fams
  = LibCaseEnv { lc_size = libCaseThreshold dflags,
		 lc_lvl = 0,
		 lc_lvl_env = emptyVarEnv, 
		 lc_rec_env = emptyVarEnv,
		 lc_scruts = [],
		 lc_fams = fams }

bombOutSize = lc_size
\end{code}


