%
% (c) The AQUA Project, Glasgow University, 1994
%
\section[LiberateCase]{Unroll recursion to allow evals to be lifted from a loop}


\begin{code}
#include "HsVersions.h"

module LiberateCase ( liberateCase ) where

IMPORT_Trace

import Id		( localiseId, toplevelishId{-debugging-} )
import IdEnv
import Maybes
import Outputable
import PlainCore
import Pretty
import SimplEnv		( UnfoldingGuidance(..) )
import Util
\end{code}

This module walks over @Core@, and looks for @case@ on free variables.
The criterion is:
	if there is case on a free on the route to the recursive call,
	then the recursive call is replaced with an unfolding.

Example

\begin{verbatim}
f = \ t -> case v of
	       V a b -> a : f t 
\end{verbatim}

=> the inner f is replaced.

\begin{verbatim}
f = \ t -> case v of
	       V a b -> a : (letrec
				f =  \ t -> case v of
					       V a b -> a : f t 
			     in f) t 
\end{verbatim}
(note the NEED for shadowing)

=> Run Andr\'e's wonder pass ...
\begin{verbatim}
f = \ t -> case v of
	       V a b -> a : (letrec
				f = \ t -> a : f t 
			     in f t)
\begin{verbatim}
Better code, because 'a' is  free inside the inner letrec, rather
than needing projection from v.


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

\begin{code}
type LibCaseLevel = Int		

topLevel :: LibCaseLevel
topLevel = 0
\end{code}

\begin{code}
data LibCaseEnv 
  = LibCaseEnv
	Int			-- Bomb-out size for deciding if
				-- potential liberatees are too big.
				-- (passed in from cmd-line args)

	LibCaseLevel		-- Current level

	(IdEnv LibCaseLevel)	-- Binds all non-top-level in-scope Ids
				-- (top-level and imported things have
				-- a level of zero)

	(IdEnv PlainCoreBinding)-- Binds *only* recursively defined
				-- Ids, to their own binding group,
				-- and *only* in their own RHSs

	[(Id,LibCaseLevel)]     -- Each of these Ids was scrutinised by an
				-- enclosing case expression, with the
				-- specified number of enclosing
				-- recursive bindings; furthermore,
				-- the Id is bound at a lower level
				-- than the case expression.  The
				-- order is insignificant; it's a bag
				-- really

initEnv :: Int -> LibCaseEnv
initEnv bomb_size = LibCaseEnv bomb_size 0 nullIdEnv nullIdEnv []

bombOutSize (LibCaseEnv bomb_size _ _ _ _) = bomb_size
\end{code}


Programs
~~~~~~~~
\begin{code}
liberateCase :: Int -> [PlainCoreBinding] -> [PlainCoreBinding]
liberateCase bomb_size prog
  = do_prog (initEnv bomb_size) prog
  where
    do_prog env [] = []
    do_prog env (bind:binds) = bind' : do_prog env' binds
			     where
			       (env', bind') = libCaseBind env bind
\end{code}

Bindings
~~~~~~~~

\begin{code}
libCaseBind :: LibCaseEnv -> PlainCoreBinding -> (LibCaseEnv, PlainCoreBinding)

libCaseBind env (CoNonRec binder rhs)
  = (addBinders env [binder], CoNonRec binder (libCase env rhs))

libCaseBind env (CoRec pairs)
  = (env_body, CoRec pairs') 
  where
    (binders, rhss) = unzip pairs

    env_body = addBinders env binders

    pairs' = [(binder, libCase env_rhs rhs) | (binder,rhs) <- pairs]

    env_rhs = if all rhs_small_enough rhss then extended_env else env

	-- We extend the rec-env by binding each Id to its rhs, first
	-- processing the rhs with an *un-extended* environment, so
	-- that the same process doesn't occur for ever!

    extended_env
      = addRecBinds env [ (localiseId binder, libCase env_body rhs)
			| (binder, rhs) <- pairs ]

	-- Why "localiseId" above?  Because we're creating a new local
	-- copy of the original binding.  In particular, the original
	-- binding might have been for a TopLevId, and this copy clearly
	-- will not be top-level!
	
	-- It is enough to change just the binder, because subsequent
	-- simplification will propagate the right info from the binder.

	-- Why does it matter?  Because the codeGen keeps a separate
	-- environment for top-level Ids, and it is disastrous for it
	-- to think that something is top-level when it isn't.

    rhs_small_enough rhs
      = case (calcUnfoldingGuidance True{-sccs OK-} lIBERATE_BOMB_SIZE rhs) of
	  UnfoldNever -> False
	  _ 	      -> True	-- we didn't BOMB, so it must be OK

    lIBERATE_BOMB_SIZE = bombOutSize env
\end{code}


Expressions
~~~~~~~~~~~

\begin{code}
libCase :: LibCaseEnv
	-> PlainCoreExpr
	-> PlainCoreExpr

libCase env (CoLit lit)		 = CoLit lit
libCase env (CoVar v)		 = mkCoLetsNoUnboxed (libCaseId env v) (CoVar v)
libCase env (CoApp fun arg)      = mkCoLetsNoUnboxed (libCaseAtom env arg) (CoApp (libCase env fun) arg)
libCase env (CoTyApp fun ty)     = CoTyApp (libCase env fun) ty
libCase env (CoCon con tys args) = mkCoLetsNoUnboxed (libCaseAtoms env args) (CoCon con tys args)
libCase env (CoPrim op tys args) = mkCoLetsNoUnboxed (libCaseAtoms env args) (CoPrim op tys args)
libCase env (CoTyLam tyvar body) = CoTyLam tyvar (libCase env body)
libCase env (CoSCC cc body)      = CoSCC cc (libCase env body)

libCase env (CoLam binders body)
  = CoLam binders (libCase env' body)
  where
    env' = addBinders env binders

libCase env (CoLet bind body) 
  = CoLet bind' (libCase env_body body)
  where
    (env_body, bind') = libCaseBind env bind

libCase env (CoCase scrut alts)
  = CoCase (libCase env scrut) (libCaseAlts env_alts alts)
  where
    env_alts = case scrut of
		  CoVar scrut_var -> addScrutedVar env scrut_var
		  other		  -> env
\end{code}


Case alternatives
~~~~~~~~~~~~~~~~~

\begin{code}
libCaseAlts env (CoAlgAlts alts deflt)
  = CoAlgAlts (map do_alt alts) (libCaseDeflt env deflt)
  where
    do_alt (con,args,rhs) = (con, args, libCase (addBinders env args) rhs)

libCaseAlts env (CoPrimAlts alts deflt)
  = CoPrimAlts (map do_alt alts) (libCaseDeflt env deflt) 
  where
    do_alt (lit,rhs) = (lit, libCase env rhs)

libCaseDeflt env CoNoDefault 
   = CoNoDefault
libCaseDeflt env (CoBindDefault binder rhs) 
   = CoBindDefault binder (libCase (addBinders env [binder]) rhs)
\end{code}

Atoms and Ids
~~~~~~~~~~~~~
\begin{code}
libCaseAtoms :: LibCaseEnv -> [PlainCoreAtom] -> [PlainCoreBinding]
libCaseAtoms env atoms = concat [libCaseAtom env atom | atom <- atoms]

libCaseAtom :: LibCaseEnv -> PlainCoreAtom -> [PlainCoreBinding]
libCaseAtom env (CoVarAtom arg_id) = libCaseId env arg_id
libCaseAtom env (CoLitAtom lit)    = []

libCaseId :: LibCaseEnv -> Id -> [PlainCoreBinding]
libCaseId env v
  | maybeToBool maybe_rec_bind &&	-- It's a use of a recursive thing
    there_are_free_scruts		-- with free vars scrutinised in RHS
  = [the_bind]

  | otherwise
  = []

  where
    maybe_rec_bind :: Maybe PlainCoreBinding	-- The binding of the recursive thingy
    maybe_rec_bind = lookupRecId env v
    Just the_bind = maybe_rec_bind

    rec_id_level = lookupLevel env v

    there_are_free_scruts = freeScruts env rec_id_level
\end{code}			



Utility functions
~~~~~~~~~~~~~~~~~
\begin{code}
addBinders :: LibCaseEnv -> [Id] -> LibCaseEnv
addBinders (LibCaseEnv bomb lvl lvl_env rec_env scruts) binders
  = LibCaseEnv bomb lvl lvl_env' rec_env scruts
  where
    lvl_env' = growIdEnvList lvl_env (binders `zip` repeat lvl)

addRecBinds :: LibCaseEnv -> [(Id,PlainCoreExpr)] -> LibCaseEnv
addRecBinds (LibCaseEnv bomb lvl lvl_env rec_env scruts) pairs
  = LibCaseEnv bomb lvl' lvl_env' rec_env' scruts
  where
    lvl'     = lvl + 1
    lvl_env' = growIdEnvList lvl_env [(binder,lvl) | (binder,_) <- pairs]
    rec_env' = growIdEnvList rec_env [(binder, CoRec pairs) | (binder,_) <- pairs]

addScrutedVar :: LibCaseEnv 
	      -> Id		-- This Id is being scrutinised by a case expression
	      -> LibCaseEnv	

addScrutedVar env@(LibCaseEnv bomb lvl lvl_env rec_env scruts) scrut_var
  | bind_lvl < lvl
  = LibCaseEnv bomb lvl lvl_env rec_env scruts'
	-- Add to scruts iff the scrut_var is being scrutinised at
	-- a deeper level than its defn 

  | otherwise = env
  where
    scruts'  = (scrut_var, lvl) : scruts
    bind_lvl = case lookupIdEnv lvl_env scrut_var of
		 Just lvl -> lvl
		 Nothing  -> --false: ASSERT(toplevelishId scrut_var)
			     topLevel

lookupRecId :: LibCaseEnv -> Id -> Maybe PlainCoreBinding
lookupRecId (LibCaseEnv bomb lvl lvl_env rec_env scruts) id
#ifndef DEBUG
  = lookupIdEnv rec_env id
#else
  = case (lookupIdEnv rec_env id) of
      xxx@(Just _) -> xxx
      xxx	   -> --false: ASSERT(toplevelishId id)
		      xxx
#endif

lookupLevel :: LibCaseEnv -> Id -> LibCaseLevel
lookupLevel (LibCaseEnv bomb lvl lvl_env rec_env scruts) id
  = case lookupIdEnv lvl_env id of
      Just lvl -> lvl
      Nothing  -> ASSERT(toplevelishId id)
		  topLevel

freeScruts :: LibCaseEnv 
	   -> LibCaseLevel 	-- Level of the recursive Id
	   -> Bool		-- True <=> there is an enclosing case of a variable
				-- bound outside (ie level <=) the recursive Id.
freeScruts (LibCaseEnv bomb lvl lvl_env rec_env scruts) rec_bind_lvl
  = not (null free_scruts)
  where
    free_scruts = [v | (v,lvl) <- scruts, lvl > rec_bind_lvl]
\end{code} 
