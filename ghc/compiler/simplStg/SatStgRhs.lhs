%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[SatStgRhs]{Saturates RHSs when they are partial applications}

96/03: This is actually an essential module, as it sets arity info
for the code generator.

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

IMP_Ubiq(){-uitous-}

import StgSyn

import CostCentre	( isCafCC, subsumedCosts, useCurrentCostCentre )
import Id		( idType, getIdArity, addIdArity, mkSysLocal,
			  nullIdEnv, addOneToIdEnv, growIdEnvList,
			  lookupIdEnv, SYN_IE(IdEnv)
			)
import SrcLoc		( noSrcLoc )
import Type		( splitSigmaTy, splitForAllTy, splitFunTyExpandingDicts )
import UniqSupply	( returnUs, thenUs, mapUs, getUnique, SYN_IE(UniqSM) )
import Util		( panic, assertPanic )

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
satStgRhs :: [StgBinding] -> UniqSM [StgBinding]
satStgRhs = panic "satStgRhs"

{- 		NUKED FOR NOW  SLPJ Dec 96


satStgRhs p = satProgram nullIdEnv p

satProgram :: SatEnv -> [StgBinding] -> UniqSM [StgBinding]
satProgram env [] = returnUs []

satProgram env (bind:binds)
  = satBinding True{-toplevel-} env bind    `thenUs` \ (env2, bind2) ->
    satProgram env2 binds		    `thenUs` \ binds2 ->
    returnUs (bind2 : binds2)
\end{code}

%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}
satBinding :: Bool	-- True <=> top-level
	   -> SatEnv
	   -> StgBinding
	   -> UniqSM (SatEnv, StgBinding)

satBinding top env (StgNonRec b rhs)
  = satRhs top env (b, rhs)	`thenUs` \ (b2, rhs2) ->
    let
	env2 = addOneToIdEnv env b b2
    in
    returnUs (env2, StgNonRec b2 rhs2)

satBinding top env (StgRec pairs)
  = -- Do it once to get the arities right...
    mapUs (satRhs top env) pairs   `thenUs` \ pairs2 ->
    let
	env2 = growIdEnvList env (map fst pairs `zip` map fst pairs2)
    in
    -- Do it again to *use* those arities:
    mapUs (satRhs top env2) pairs  `thenUs` \ pairs3 ->

    returnUs (env2, StgRec pairs3)

satRhs :: Bool -> SatEnv -> (Id, StgRhs) -> UniqSM (Id_w_Arity, StgRhs)

satRhs top env (b, StgRhsCon cc con args)	-- Nothing much to do here
  = let
	b2 = b `addIdArity` 0 -- bound to a saturated constructor; hence zero.
    in
    returnUs (b2, StgRhsCon cc con (lookupArgs env args))

satRhs top env (b, StgRhsClosure cc bi fv u args body)
  = satExpr env body	`thenUs` \ (arity_info, body2) ->
    let
	num_args = length args
    in
    (case arity_info of
      Nothing ->
	returnUs (num_args, StgRhsClosure cc bi fv u args body2)

      Just needed_args ->
	ASSERT(needed_args >= 1)

	let  -- the arity we're aiming for is: what we already have ("args")
	     -- plus the ones requested in "arity_info"
	    new_arity = num_args + needed_args

	     -- get type info for this function:
	    (_, rho_ty) = splitForAllTy (idType b)
	    (all_arg_tys, _) = splitFunTyExpandingDicts rho_ty

	     -- now, we already have "args"; we drop that many types
	    args_we_dont_have_tys = drop num_args all_arg_tys

	     -- finally, we take some of those (up to maybe all of them),
	     -- depending on how many "needed_args"
	    args_to_add_tys = take needed_args args_we_dont_have_tys
	in
	    -- make up names for them
	mapUs newName args_to_add_tys	`thenUs` \ nns ->

	    -- and do the business
	let
	    body3  = saturate body2 (map StgVarArg nns)

	    new_cc -- if we're adding args, we'd better not
		   -- keep calling something a CAF! (what about DICTs? ToDo: WDP 95/02)
	      = if not (isCafCC cc)
		then cc -- unchanged
		else if top then subsumedCosts else useCurrentCostCentre
	in
	returnUs (new_arity, StgRhsClosure new_cc bi fv ReEntrant (args++nns) body3)
    )
				`thenUs` \ (arity, rhs2) ->
    let
	b2 = b `addIdArity` arity
    in
    returnUs (b2, rhs2)
\end{code}

%************************************************************************
%*									*
\subsection{Expressions}
%*									*
%************************************************************************

\begin{code}
satExpr :: SatEnv -> StgExpr -> UniqSM (ExprArityInfo, StgExpr)

satExpr env app@(StgApp (StgLitArg lit) [] lvs) = returnUs (Nothing, app)

satExpr env app@(StgApp (StgVarArg f) as lvs)
  = returnUs (arity_to_return, StgApp (StgVarArg f2) as2 lvs)
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

satExpr env app@(StgCon con as lvs)
  = returnUs (Nothing, StgCon con (lookupArgs env as) lvs)

satExpr env app@(StgPrim op as lvs)
  = returnUs (Nothing, StgPrim op (lookupArgs env as) lvs)

satExpr env (StgSCC ty l e)
  = satExpr env e	 `thenUs` \ (_, e2) ->
    returnUs (Nothing, StgSCC ty l e2)

{- OMITTED: Let-no-escapery should come *after* saturation

satExpr (StgLetNoEscape lvs_whole lvs_rhss binds body)
  = satBinding binds	`thenUs` \ (binds2, c) ->
    satExpr body	`thenUs` \ (_, body2, c2) ->
    returnUs (Nothing, StgLetNoEscape lvs_whole lvs_rhss binds2 body2, c + c2)
-}

satExpr env (StgLet binds body)
  = satBinding False{-not top-level-} env binds	`thenUs` \ (env2, binds2) ->
    satExpr env2 body				`thenUs` \ (_, body2) ->
    returnUs (Nothing, StgLet binds2 body2)

satExpr env (StgCase expr lve lva uniq alts)
  = satExpr env expr	`thenUs` \ (_, expr2) ->
    sat_alts alts	`thenUs` \ alts2 ->
    returnUs (Nothing, StgCase expr2 lve lva uniq alts2)
    where
      sat_alts (StgAlgAlts ty alts def)
	= mapUs sat_alg_alt alts	`thenUs` \ alts2 ->
	  sat_deflt def			`thenUs` \ def2 ->
	  returnUs (StgAlgAlts ty alts2 def2)
	where
	  sat_alg_alt (id, bs, use_mask, e)
	    = satExpr env e `thenUs` \ (_, e2) ->
	      returnUs (id, bs, use_mask, e2)

      sat_alts (StgPrimAlts ty alts def)
	= mapUs sat_prim_alt alts 	`thenUs` \ alts2 ->
	  sat_deflt def			`thenUs` \ def2 ->
	  returnUs (StgPrimAlts ty alts2 def2)
	where
	  sat_prim_alt (l, e)
	    = satExpr env e `thenUs` \ (_, e2) ->
	      returnUs (l, e2)

      sat_deflt StgNoDefault
	= returnUs StgNoDefault

      sat_deflt (StgBindDefault b u expr)
	= satExpr env expr	`thenUs` \ (_,expr2) ->
	  returnUs (StgBindDefault b u expr2)
\end{code}

%************************************************************************
%*									*
\subsection{Utility functions}
%*									*
%************************************************************************

\begin{code}
saturate :: StgExpr -> [StgArg] -> StgExpr

saturate (StgApp f as lvs) ids = StgApp f (as ++ ids) lvs
saturate other 		    _  = panic "SatStgRhs: saturate"
\end{code}

\begin{code}
lookupArgs :: SatEnv -> [StgArg] -> [StgArg]
lookupArgs env args = map doo args
  where
    doo    (StgVarArg v)  = StgVarArg (lookupVar env v)
    doo a@(StgLitArg lit) = a

lookupVar :: SatEnv -> Id -> Id
lookupVar env v = case lookupIdEnv env v of
			Nothing -> v
			Just v2 -> v2

newName :: Type -> UniqSM Id
newName ut
  = getUnique	`thenUs` \ uniq ->
    returnUs (mkSysLocal SLIT("sat") uniq ut noSrcLoc)

-}
\end{code}
