%************************************************************************
%*									*
\subsection[MatchEnv]{Matching environments}
%*									*
%************************************************************************

\begin{code}
#include "HsVersions.h"

module MatchEnv (
	MatchEnv, nullMEnv, mkMEnv,
	isEmptyMEnv, lookupMEnv, insertMEnv,
	mEnvToList
) where

CHK_Ubiq() -- debugging consistency check

import Maybes	( MaybeErr(..), returnMaB, thenMaB, failMaB )
\end{code}

``Matching'' environments allow you to bind a template to a value;
when you look up in it, you supply a value which is matched against
the template.

\begin{code}
data MatchEnv key value 
  = EmptyME			-- Common, so special-cased
  | ME [(key, value)]
\end{code}

For now we just use association lists. The list is maintained sorted
in order of {\em decreasing specificness} of @key@, so that the first
match will be the most specific.

\begin{code}
nullMEnv :: MatchEnv a b
nullMEnv = EmptyME

isEmptyMEnv EmptyME = True
isEmptyMEnv _	    = False

mkMEnv :: [(key, value)] -> MatchEnv key value
mkMEnv []    = EmptyME
mkMEnv stuff = ME stuff

mEnvToList :: MatchEnv key value -> [(key, value)]
mEnvToList EmptyME    = []
mEnvToList (ME stuff) = stuff
\end{code}

@lookupMEnv@ looks up in a @MatchEnv@.  It simply takes the first
match, which should be the most specific.

\begin{code}
lookupMEnv :: (key1 {- template -} ->	-- Matching function
	       key2 {- instance -} ->
	       Maybe match_info)
	   -> MatchEnv key1 value	-- The envt
	   -> key2			-- Key
	   -> Maybe (value,		-- Value
		     match_info)	-- Match info returned by matching fn
		     

lookupMEnv key_match EmptyME    key = Nothing
lookupMEnv key_match (ME alist) key
  = find alist
  where
    find [] = Nothing
    find ((tpl, val) : rest)
      = case (key_match tpl key) of
	  Nothing	  -> find rest
	  Just match_info -> Just (val,match_info)
\end{code}

@insertMEnv@ extends a match environment, checking for overlaps.

\begin{code}
insertMEnv :: (key {- template -} ->		-- Matching function
	       key {- instance -} ->
	       Maybe match_info)
	   -> MatchEnv key value		-- Envt
	   -> key -> value			-- New item
	   -> MaybeErr (MatchEnv key value)	-- Success...
		       (key, value)		-- Failure: Offending overlap

insertMEnv match_fn EmptyME    key value = returnMaB (ME [(key, value)])
insertMEnv match_fn (ME alist) key value
  = insert alist
  where
    -- insertMEnv has to put the new item in BEFORE any keys which are
    -- LESS SPECIFIC than the new key, and AFTER any keys which are
    -- MORE SPECIFIC The list is maintained in specific-ness order, so
    -- we just stick it in either last, or just before the first key
    -- of which the new key is an instance.  We check for overlap at
    -- that point.

    insert [] = returnMaB (ME [(key, value)])
    insert ls@(r@(t,v) : rest)
      = case (match_fn t key) of
	  Nothing ->
	    -- New key is not an instance of this existing one, so
	    -- continue down the list.
	    insert rest			`thenMaB` \ (ME rest') ->
	    returnMaB (ME(r:rest'))

	  Just match_info ->
	    -- New key *is* an instance of the old one, so check the
	    -- other way round in case of identity.

	    case (match_fn key t) of
	      Just _  -> failMaB r
			 -- Oops; overlap

	      Nothing -> returnMaB (ME ((key,value):ls))
			 -- All ok; insert here
\end{code}
