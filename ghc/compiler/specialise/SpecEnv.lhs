%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[SpecEnv]{Specialisation info about an @Id@}

\begin{code}
module SpecEnv (
	SpecEnv,
	emptySpecEnv, isEmptySpecEnv,
	addToSpecEnv, matchSpecEnv, unifySpecEnv
    ) where

#include "HsVersions.h"

import Type		( Type, GenType, matchTys, tyVarsOfTypes )
import TyVar		( TyVarEnv, lookupTyVarEnv, tyVarSetToList )
import Unify		( Subst, unifyTyListsX )
import Maybes
import Util		( assertPanic )
\end{code}



%************************************************************************
%*									*
\section{SpecEnv}
%*									*
%************************************************************************

\begin{code}
data SpecEnv value 
  = EmptySE 
  | SpecEnv [([Type], value)]	-- No pair of templates unify with each others
\end{code}

For now we just use association lists.

\begin{code}
emptySpecEnv :: SpecEnv a
emptySpecEnv = EmptySE

isEmptySpecEnv EmptySE = True
isEmptySpecEnv _       = False
\end{code}

@lookupSpecEnv@ looks up in a @SpecEnv@.  Since no pair of templates
unify, the first match must be the only one.

\begin{code}
data SpecEnvResult val
  = Match Subst	val	-- Match, instantiating only
			-- type variables in the template

  | CouldMatch		-- A match could happen if the
			-- some of the type variables in the key
			-- were further instantiated.

  | NoMatch		-- No match possible, regardless of how
			-- the key is further instantiated

-- If the key *unifies* with one of the templates, then the
-- result is Match or CouldMatch, depending on whether any of the 
-- type variables in the key had to be instantiated

unifySpecEnv :: SpecEnv value	-- The envt
	      -> [Type]		-- Key
	      -> SpecEnvResult value
		     

unifySpecEnv EmptySE key = NoMatch
unifySpecEnv (SpecEnv alist) key
  = find alist
  where
    find [] = NoMatch
    find ((tpl, val) : rest)
      = case unifyTyListsX tpl key of
	  Nothing    -> find rest
	  Just subst |  all uninstantiated (tyVarSetToList (tyVarsOfTypes key)) 
		     -> Match subst val
	             |  otherwise
		     -> CouldMatch
		     where
		       uninstantiated tv = case lookupTyVarEnv subst tv of
					     Just xx -> False
					     Nothing -> True

-- matchSpecEnv does a one-way match only, but in return
-- it is more polymorphic than unifySpecEnv

matchSpecEnv :: SpecEnv value	-- The envt
	     -> [GenType flexi]		-- Key
	     -> Maybe (TyVarEnv (GenType flexi), value)
		     
matchSpecEnv EmptySE key = Nothing
matchSpecEnv (SpecEnv alist) key
  = find alist
  where
    find [] = Nothing
    find ((tpl, val) : rest)
      = case matchTys tpl key of
	  Nothing    -> find rest
	  Just (subst, leftovers) -> ASSERT( null leftovers )
				     Just (subst, val)
\end{code}

@addToSpecEnv@ extends a @SpecEnv@, checking for overlaps.

\begin{code}
addToSpecEnv :: SpecEnv value			-- Envt
	      -> [Type] -> value		-- New item
	      -> MaybeErr (SpecEnv value)	-- Success...
		          ([Type], value)	-- Failure: Offending overlap

addToSpecEnv EmptySE         key value = returnMaB (SpecEnv [(key, value)])
addToSpecEnv (SpecEnv alist) key value
  = case filter matches_key alist of
      []        -> returnMaB (SpecEnv ((key,value) : alist))	-- No match
      (bad : _) -> failMaB bad					-- At least one match
  where
    matches_key (tpl, val) = maybeToBool (unifyTyListsX tpl key)
\end{code}
