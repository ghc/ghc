%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[SatStgRhs]{Saturates RHSs when they are partial applications}


\begin{display}
Subject: arg satis check
Date: Wed, 29 Apr 92 13:33:58 +0100
From: Simon L Peyton Jones <simonpj>

Andre

Another transformation to consider.  We'd like to avoid
argument-satisfaction checks wherever possible.  So, whenever we have an
STG binding application

	f = vs \ xs -> g e1 ... en

where xs has one or more elements
and
where g is a known function with arity m+n,

then: change it to

	f = vs \ xs++{x1...xm} -> g e1 ... en x1 .. xm

Now g has enough args.   One arg-satisfaction check disappears;
the one for the closure incorporates the one for g.

You might like to consider variants, applying the transformation more
widely.  I concluded that this was the only instance which made
sense, but I could be wrong.

Simon
\end{display}

The algorithm proceeds as follows:
\begin{enumerate}
\item
Gather the arity information of the functions defined in this module
(as @getIdArity@ only knows about the arity of @ImportedIds@).

\item
for every definition of the form
\begin{verbatim}
    v = /\ts -> \vs -> f args
\end{verbatim}
we try to match the arity of \tr{f} with the number of arguments.
If they do not match we insert extra lambdas to make that application
saturated.
\end{enumerate}

This is done for local definitions as well.

\begin{code}
#include "HsVersions.h"

module SatStgRhs ( satStgRhs ) where

import StgSyn

import AbsUniType	( splitTypeWithDictsAsArgs, Class,
			  TyVarTemplate, TauType(..)
			)
import CostCentre
import IdEnv
import Id		( mkSysLocal, getIdUniType, getIdArity, addIdArity )
import IdInfo		-- SIGH: ( arityMaybe, ArityInfo, OptIdInfo(..) )
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import SplitUniq
import Unique
import Util
import Maybes

type Arity = Int
type Count = Int

type ExprArityInfo = Maybe Int	    -- Just n  => This expression has a guaranteed
				    --	    	  arity of n
				    -- Nothing => Don't know how many args it needs
				    
type Id_w_Arity = Id		    -- An Id with correct arity info pinned on it
type SatEnv     = IdEnv Id_w_Arity  -- Binds only local, let(rec)-bound things
\end{code}

This pass 
\begin{itemize}
\item adds extra args where necessary;
\item pins the correct arity on everything.
\end{itemize}

%************************************************************************
%*									*
\subsection{Top-level list of bindings (a ``program'')}
%*									*
%************************************************************************

\begin{code}
satStgRhs :: PlainStgProgram -> SUniqSM PlainStgProgram

satStgRhs p = satProgram nullIdEnv p

satProgram :: SatEnv -> PlainStgProgram -> SUniqSM PlainStgProgram
satProgram env [] = returnSUs []

satProgram env (bind:binds) 
  = satBinding True{-toplevel-} env bind    `thenSUs` \ (env2, bind2) ->
    satProgram env2 binds		    `thenSUs` \ binds2 ->
    returnSUs (bind2 : binds2)
\end{code}

%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}
satBinding :: Bool	-- True <=> top-level
	   -> SatEnv 
	   -> PlainStgBinding 
           -> SUniqSM (SatEnv, PlainStgBinding)

satBinding top env (StgNonRec b rhs)
  = satRhs top env (b, rhs)	`thenSUs` \ (b2, rhs2) ->
    let
	env2 = addOneToIdEnv env b b2
    in
    returnSUs (env2, StgNonRec b2 rhs2)

satBinding top env (StgRec pairs)
  = -- Do it once to get the arities right...
    mapSUs (satRhs top env) pairs   `thenSUs` \ pairs2 ->
    let
	env2 = growIdEnvList env (map fst pairs `zip` map fst pairs2)
    in
    -- Do it again to *use* those arities:
    mapSUs (satRhs top env2) pairs  `thenSUs` \ pairs3 ->

    returnSUs (env2, StgRec pairs3)

satRhs :: Bool -> SatEnv -> (Id, PlainStgRhs) -> SUniqSM (Id_w_Arity, PlainStgRhs)

satRhs top env (b, StgRhsCon cc con args)	-- Nothing much to do here
  = let 
	b2 = b `addIdArity` 0 -- bound to a saturated constructor; hence zero.
    in
    returnSUs (b2, StgRhsCon cc con (lookupArgs env args))

satRhs top env (b, StgRhsClosure cc bi fv u args body)
  = satExpr env body	`thenSUs` \ (arity_info, body2) ->
    let
	num_args = length args
    in
    (case arity_info of
      Nothing ->
	returnSUs (num_args, StgRhsClosure cc bi fv u args body2)

      Just needed_args ->
	ASSERT(needed_args >= 1)

	let  -- the arity we're aiming for is: what we already have ("args")
	     -- plus the ones requested in "arity_info"
	    new_arity = num_args + needed_args

	     -- get type info for this function:
	    (_,all_arg_tys,_) = splitTypeWithDictsAsArgs (getIdUniType b)

	     -- now, we already have "args"; we drop that many types
	    args_we_dont_have_tys = drop num_args all_arg_tys

	     -- finally, we take some of those (up to maybe all of them),
	     -- depending on how many "needed_args"
	    args_to_add_tys = take needed_args args_we_dont_have_tys
	in
	    -- make up names for them
	mapSUs newName args_to_add_tys	`thenSUs` \ nns ->

	    -- and do the business
	let
	    body3  = saturate body2 (map StgVarAtom nns)

	    new_cc -- if we're adding args, we'd better not
		   -- keep calling something a CAF! (what about DICTs? ToDo: WDP 95/02)
	      = if not (isCafCC cc) 
	        then cc -- unchanged
		else if top then subsumedCosts else useCurrentCostCentre
	in
	returnSUs (new_arity, StgRhsClosure new_cc bi fv ReEntrant (args++nns) body3)
    )
				`thenSUs` \ (arity, rhs2) ->
    let 
	b2 = b `addIdArity` arity
    in
    returnSUs (b2, rhs2)
\end{code}

%************************************************************************
%*									*
\subsection{Expressions}
%*									*
%************************************************************************

\begin{code}    
satExpr :: SatEnv -> PlainStgExpr -> SUniqSM (ExprArityInfo, PlainStgExpr)

satExpr env app@(StgApp (StgLitAtom lit) [] lvs) = returnSUs (Nothing, app)

satExpr env app@(StgApp (StgVarAtom f) as lvs)
  = returnSUs (arity_to_return, StgApp (StgVarAtom f2) as2 lvs)
  where
    as2 = lookupArgs env as
    f2  = lookupVar  env f
    arity_to_return = case arityMaybe (getIdArity f2) of
			Nothing      -> Nothing

			Just f_arity -> if remaining_arity > 0 
					then Just remaining_arity
					else Nothing
				     where
					remaining_arity = f_arity - length as
				
satExpr env app@(StgConApp con as lvs)
  = returnSUs (Nothing, StgConApp con (lookupArgs env as) lvs)

satExpr env app@(StgPrimApp op as lvs)
  = returnSUs (Nothing, StgPrimApp op (lookupArgs env as) lvs)

satExpr env (StgSCC ty l e)
  = satExpr env e	 `thenSUs` \ (_, e2) ->
    returnSUs (Nothing, StgSCC ty l e2)

{- OMITTED: Let-no-escapery should come *after* saturation

satExpr (StgLetNoEscape lvs_whole lvs_rhss binds body)
  = satBinding binds	`thenSUs` \ (binds2, c) ->
    satExpr body	`thenSUs` \ (_, body2, c2) ->
    returnSUs (Nothing, StgLetNoEscape lvs_whole lvs_rhss binds2 body2, c + c2)
-}

satExpr env (StgLet binds body)
  = satBinding False{-not top-level-} env binds	`thenSUs` \ (env2, binds2) ->
    satExpr env2 body				`thenSUs` \ (_, body2) ->
    returnSUs (Nothing, StgLet binds2 body2)

satExpr env (StgCase expr lve lva uniq alts)
  = satExpr env expr	`thenSUs` \ (_, expr2) ->
    sat_alts alts	`thenSUs` \ alts2 ->
    returnSUs (Nothing, StgCase expr2 lve lva uniq alts2)
    where
      sat_alts (StgAlgAlts ty alts def)
	= mapSUs sat_alg_alt alts	`thenSUs` \ alts2 ->
	  sat_deflt def			`thenSUs` \ def2 ->
	  returnSUs (StgAlgAlts ty alts2 def2)
	where
	  sat_alg_alt (id, bs, use_mask, e)
	    = satExpr env e `thenSUs` \ (_, e2) ->
	      returnSUs (id, bs, use_mask, e2)

      sat_alts (StgPrimAlts ty alts def)
	= mapSUs sat_prim_alt alts 	`thenSUs` \ alts2 ->
	  sat_deflt def			`thenSUs` \ def2 ->
	  returnSUs (StgPrimAlts ty alts2 def2)
	where
	  sat_prim_alt (l, e)
	    = satExpr env e `thenSUs` \ (_, e2) ->
	      returnSUs (l, e2)

      sat_deflt StgNoDefault
	= returnSUs StgNoDefault

      sat_deflt (StgBindDefault b u expr)
	= satExpr env expr	`thenSUs` \ (_,expr2) ->
	  returnSUs (StgBindDefault b u expr2)
\end{code}

%************************************************************************
%*									*
\subsection{Utility functions}
%*									*
%************************************************************************

\begin{code}
saturate :: PlainStgExpr -> [PlainStgAtom] -> PlainStgExpr

saturate (StgApp f as lvs) ids = StgApp f (as ++ ids) lvs
saturate other 		    _  = panic "SatStgRhs: saturate"
\end{code}

\begin{code}
lookupArgs :: SatEnv -> [PlainStgAtom] -> [PlainStgAtom]
lookupArgs env args = map do args
  where 
    do    (StgVarAtom v)  = StgVarAtom (lookupVar env v)
    do a@(StgLitAtom lit) = a

lookupVar :: SatEnv -> Id -> Id
lookupVar env v = case lookupIdEnv env v of
			Nothing -> v
			Just v2 -> v2

newName :: UniType -> SUniqSM Id
newName ut
  = getSUnique	`thenSUs` \ uniq ->
    returnSUs (mkSysLocal SLIT("sat") uniq ut mkUnknownSrcLoc)
\end{code}
