%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[E]{Main typechecker environment}

\begin{code}
#include "HsVersions.h"

module E (
	E,
	mkE, nullE,
	getE_GlobalVals, getE_TCE, getE_CE,
	plusE_TCE, plusE_CE,

	growE_LVE, plusE_GVE, tvOfE,

	lookupE_Value, lookupE_ValueQuietly,
	lookupE_ClassOpByKey, lookupE_Binder,

	GVE(..), LVE(..),
	plusLVE, nullLVE,
	plusGVE, nullGVE, unitGVE, -- UNUSED: rngGVE,

	-- and to make the interface self-sufficient...
	CE(..), Id, Name, TCE(..), TyVar, Maybe, UniqFM
    ) where

import CE
import TCE
import UniqFM		-- basic env handling code

import AbsPrel		( PrimOp
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType	( getClassOps, extractTyVarsFromTy,
			  getClassBigSig, getClassOpString, TyVar,
			  TyVarTemplate, ClassOp, Class, Arity(..),
			  TauType(..)
			  IF_ATTACK_PRAGMAS(COMMA cmpTyVar COMMA cmpClass)
			)
import Id		( getIdUniType, Id, IdInfo )
import Maybes		( MaybeErr(..), Maybe(..) )
import Name		-- Name(..), etc.
import Outputable	-- def of ppr, etc.
import Pretty		-- to pretty-print error messages
import UniqSet		-- this use of Sets is a HACK (WDP 94/05)
import Unique		-- *Key stuff
import Util
\end{code}


%************************************************************************
%*									*
\subsection{Type declarations}
%*									*
%************************************************************************


\begin{code}
data E
  = MkE	TCE 	    -- type environment                                        
	GVB	    -- "global" value bindings; no free type vars
	LVB	    -- "local" value bindings; may have free type vars      
	CE 	    -- class environment                                       

mkE :: TCE -> CE -> E
mkE tce ce = MkE tce nullGVB nullLVB ce

nullE :: E
nullE = MkE nullTCE nullGVB nullLVB nullCE
\end{code}

The ``local'' and ``global'' bindings, @LVB@ and @GVB@, are
non-exported synonyms.  The important thing is that @GVB@ doesn't
contain any free type variables.  This is used (only) in @tvOfE@,
which extracts free type variables from the environment.  It's quite a
help to have this separation because there may be quite a large bunch
of imported things in the @GVB@, all of which are guaranteed
polymorphic.

\begin{code}
type LVB = UniqFM Id -- Locals just have a Unique
type GVB = UniqFM Id -- Globals might be a prelude thing; hence IdKey

nullLVB = (emptyUFM :: LVB)
nullGVB = (emptyUFM :: GVB)
\end{code}

The ``local'' and ``global'' value environments are not part of @E@ at
all, but is used to provide increments to the value bindings.  GVE are
carries the implication that there are no free type variables.

\begin{code}
type LVE = [(Name, Id)]	-- Maps Names to Ids
type GVE = [(Name, Id)]	-- Maps Names to Ids

nullLVE     = ([] :: LVE)
plusLVE a b = a ++ b
nullGVE     = ([] :: GVE)
unitGVE n i = ( [(n, i)] :: GVE )
-- UNUSED: rngGVE  gve = map snd gve
plusGVE a b = a ++ b
\end{code}

%************************************************************************
%*									*
\subsection{Value environment stuff}
%*									*
%************************************************************************

Looking up things should mostly succeed, because the renamer should
have spotted all out-of-scope names.  The exception is instances.

The ``Quietly'' version is for pragmas, where lookups very well may
fail. @lookup_val@ is the internal function that does the work.

\begin{code}
lookupE_Value	     :: E -> Name -> Id
lookupE_ValueQuietly :: E -> Name -> Maybe Id

lookupE_Value e nm
  = case lookup_val e nm of
      Succeeded id -> id
      Failed (should_panic, msg)
	-> if should_panic then panic msg else error msg

lookupE_ValueQuietly e nm
  = case lookup_val e nm of
      Succeeded id -> Just id
      Failed _	   -> Nothing
\end{code}

\begin{code}
lookup_val (MkE _ gvb lvb ce) name
  = case name of

      WiredInVal id   -> Succeeded id
      PreludeVal key _ -> case (lookupDirectlyUFM gvb key) of
			    Just id -> Succeeded id
			    Nothing -> Failed (False, prelude_err_msg)

      ClassOpName uniq clas_name _ tag -> id_from_env uniq

      -- You might think that top-level ids are guaranteed to have no
      -- free tyvars, so look only in gvb; but you'd be wrong!  When
      -- type-checking the RHS of recursive top-level defns, the name
      -- of the thing is bound to a *monomorphic* type, which is later
      -- generalised.  So we have to look in the LVE too.

      OtherTopId uniq _ -> id_from_env uniq

      -- Short names could be in either GVB or LVB
      Short uniq _      -> id_from_env uniq

      funny_name -> pprPanic "lookup_val: funny Name" (ppr PprDebug funny_name)
  where
    prelude_err_msg = "ERROR: in looking up a built-in Prelude value!\n(This can happen if you use `-fno-implicit-prelude'\nor you hide the system's Prelude.hi in some way.)"

    id_from_env uniq
      = case (lookupDirectlyUFM lvb uniq) of
	  Just id -> Succeeded id
	  Nothing ->
	    case (lookupDirectlyUFM gvb uniq) of
	      Just id -> Succeeded id
	      Nothing -> Failed (True, -- should panic
			  ("lookupE_Value: unbound name: "++(ppShow 80 (ppr PprShowAll name))))
\end{code}

For Prelude things that we reach out and grab, we have only an @Unique@.
\begin{code}
lookupE_ClassOpByKey :: E -> Unique{-ClassKey-} -> FAST_STRING -> Id

lookupE_ClassOpByKey (MkE _ gvb lvb ce) clas_key op_str
  = let
	clas   = lookupCE ce (PreludeClass clas_key bottom)
	bottom = pprPanic ("lookupE_ClassOpByKey: "++(_UNPK_ op_str))
			  (ppAbove (pprUnique clas_key) (ppr PprShowAll (rngCE ce)))

	(clas_tyvar_tmpl, scs, sc_sel_ids, ops, op_sel_ids, defm_ids)
	  = getClassBigSig clas
    in
    case [ op_sel_id | (op, op_sel_id) <- ops `zip` op_sel_ids,
		       op_str == getClassOpString op ] of
      [op] -> op
    -- Seems a rather horrible way to do it (ToDo)
\end{code}

@lookupE_Binder@ is like @lookupE_Value@, but it is used for {\em
binding} occurrences of a variable, rather than {\em uses}.  The
difference is that there should always be an entry in the LVE for
binding occurrences.  Just a sanity check now, really.

\begin{code}
lookupE_Binder :: E -> Name -> Id
lookupE_Binder (MkE _ _ lvb _) name
  = case (lookupDirectlyUFM lvb (name2uniq name)) of
      Just id -> id
      Nothing -> pprPanic "lookupE_Binder: unbound name: " (ppr PprShowAll name)
\end{code}

\begin{code}
getE_GlobalVals :: E -> [Id]
getE_GlobalVals  (MkE tce gvb lvb ce)
  = let
    	result = eltsUFM gvb ++ eltsUFM lvb
    in
    -- pprTrace "Global Ids:" (ppr PprShowAll result)
    result

plusE_GVE :: E -> GVE -> E
plusE_GVE (MkE tce gvb lvb ce) gve
  = let
	new_stuff = listToUFM_Directly [(name2idkey n, i) | (n,i) <- gve ]
    in
    MkE tce (plusUFM gvb new_stuff) lvb ce
  where
    name2idkey (PreludeVal k _) = k
    name2idkey (OtherTopId u _) = u
    name2idkey (ClassOpName u _ _ _) = u

growE_LVE :: E -> LVE -> E
growE_LVE (MkE tce gvb lvb ce) lve
  = let
	new_stuff = listToUFM_Directly [(name2uniq n, i) | (n,i) <- lve ]
    in
    MkE tce gvb (plusUFM lvb new_stuff) ce

-- ToDo: move this elsewhere??
name2uniq (Short u _)   	= u
name2uniq (OtherTopId u _)	= u
name2uniq (ClassOpName u _ _ _) = panic "growE_LVE:name2uniq"
\end{code}

Return the free type variables of an LVE; there are no duplicates in
the result---hence all the @Set@ bozo-ery.  The free tyvars can only
occur in the LVB part.

\begin{code}
tvOfE :: E -> [TyVar]
tvOfE (MkE tce gvb lvb ce) 
  = uniqSetToList (mkUniqSet (
	foldr ((++) . extractTyVarsFromTy . getIdUniType) [] (eltsUFM lvb)
    ))
\end{code}

%************************************************************************
%*									*
%*	
\subsection{Type and class environments}
%*									*
%************************************************************************

\begin{code}
getE_TCE :: E -> TCE
getE_TCE (MkE tce gvb lvb ce) = tce

getE_CE :: E -> CE
getE_CE  (MkE tce gvb lvb ce) = ce

plusE_TCE :: E -> TCE -> E
plusE_TCE (MkE tce gvb lvb ce) tce'
  = MkE (plusTCE tce' tce) gvb lvb ce

plusE_CE :: E -> CE -> E
plusE_CE (MkE tce gvb lvb ce) ce'
  = MkE tce gvb lvb (plusCE ce ce')
\end{code}
