%
% (c) The AQUA Project, Glasgow University, 1993-1996
%
\section[SimplEnv]{Environment stuff for the simplifier}

\begin{code}
#include "HsVersions.h"

module SimplEnv (
	nullSimplEnv, combineSimplEnv,
	pprSimplEnv, -- debugging only

	extendTyEnv, extendTyEnvList,
	simplTy, simplTyInId,

	extendIdEnvWithAtom, extendIdEnvWithAtoms,
	extendIdEnvWithClone, extendIdEnvWithClones,
	lookupId,


	markDangerousOccs,
	lookupRhsInfo, lookupOutIdEnv, isEvaluated,
	extendEnvGivenBinding, extendEnvGivenNewRhs,
	extendEnvForRecBinding, extendEnvGivenRhsInfo,

	lookForConstructor,

	getSwitchChecker, switchIsSet, getSimplIntSwitch, switchOffInlining,

	setEnclosingCC, getEnclosingCC,

	-- Types
	SYN_IE(SwitchChecker),
	SimplEnv, 
	SYN_IE(InIdEnv), SYN_IE(InTypeEnv),
	UnfoldConApp,
	RhsInfo(..),

	SYN_IE(InId),  SYN_IE(InBinder),  SYN_IE(InBinding),  SYN_IE(InType),
	SYN_IE(OutId), SYN_IE(OutBinder), SYN_IE(OutBinding), SYN_IE(OutType),

	SYN_IE(InExpr),  SYN_IE(InAlts),  SYN_IE(InDefault),  SYN_IE(InArg),
	SYN_IE(OutExpr), SYN_IE(OutAlts), SYN_IE(OutDefault), SYN_IE(OutArg)
    ) where

IMP_Ubiq(){-uitous-}

IMPORT_DELOOPER(SmplLoop)		-- breaks the MagicUFs / SimplEnv loop

import BinderInfo	( orBinderInfo, andBinderInfo, noBinderInfo,
			  BinderInfo(..){-instances, too-}, FunOrArg, DuplicationDanger, InsideSCC
			)
import CmdLineOpts	( switchIsOn, intSwitchSet, opt_UnfoldingCreationThreshold,
			  SimplifierSwitch(..), SwitchResult(..)
			)
import CoreSyn
import CoreUnfold	( mkFormSummary, exprSmallEnoughToDup, 
			  Unfolding(..), UfExpr, RdrName,
			  SimpleUnfolding(..), FormSummary(..),
			  calcUnfoldingGuidance, UnfoldingGuidance(..)
			)
import CoreUtils	( coreExprCc, unTagBinders )
import CostCentre	( CostCentre, noCostCentre, noCostCentreAttached )
import FiniteMap	-- lots of things
import Id		( idType, getIdUnfolding, getIdStrictness, idWantsToBeINLINEd,
			  applyTypeEnvToId,
			  nullIdEnv, growIdEnvList, rngIdEnv, lookupIdEnv,
			  addOneToIdEnv, modifyIdEnv, mkIdSet, modifyIdEnv_Directly,
			  SYN_IE(IdEnv), SYN_IE(IdSet), GenId )
import Literal		( isNoRepLit, Literal{-instances-} )
import Maybes		( maybeToBool, expectJust )
import Name		( isLocallyDefined )
import OccurAnal	( occurAnalyseExpr )
import Outputable	( Outputable(..){-instances-} )
import PprCore		-- various instances
import PprStyle		( PprStyle(..) )
import PprType		( GenType, GenTyVar )
import Pretty
import Type		( eqTy, applyTypeEnvToTy )
import TyVar		( nullTyVarEnv, addOneToTyVarEnv, growTyVarEnvList,
			  SYN_IE(TyVarEnv), GenTyVar{-instance Eq-}
			)
import Unique		( Unique{-instance Outputable-} )
import UniqFM		( addToUFM_C, ufmToList, eltsUFM
			)
--import UniqSet		-- lots of things
import Usage		( SYN_IE(UVar), GenUsage{-instances-} )
import Util		( zipEqual, thenCmp, cmpList, panic, panic#, assertPanic )

type TypeEnv = TyVarEnv Type
cmpType = panic "cmpType (SimplEnv)"
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-types]{Type declarations}
%*									*
%************************************************************************

\begin{code}
type InId      = Id			-- Not yet cloned
type InBinder  = (InId, BinderInfo)
type InType    = Type			-- Ditto
type InBinding = SimplifiableCoreBinding
type InExpr    = SimplifiableCoreExpr
type InAlts    = SimplifiableCoreCaseAlts
type InDefault = SimplifiableCoreCaseDefault
type InArg     = SimplifiableCoreArg

type OutId	= Id			-- Cloned
type OutBinder	= Id
type OutType	= Type			-- Cloned
type OutBinding	= CoreBinding
type OutExpr	= CoreExpr
type OutAlts	= CoreCaseAlts
type OutDefault	= CoreCaseDefault
type OutArg	= CoreArg

type SwitchChecker = SimplifierSwitch -> SwitchResult
\end{code}

%************************************************************************
%*									*
\subsubsection{The @SimplEnv@ type}
%*									*
%************************************************************************


INVARIANT: we assume {\em no shadowing}.  (ToDo: How can we ASSERT
this? WDP 94/06) This allows us to neglect keeping everything paired
with its static environment.

The environment contains bindings for all
	{\em in-scope,}
	{\em locally-defined}
things.

For such things, any unfolding is found in the environment, not in the
Id.  Unfoldings in the Id itself are used only for imported things
(otherwise we get trouble because we have to simplify the unfoldings
inside the Ids, etc.).

\begin{code}
data SimplEnv
  = SimplEnv
	SwitchChecker
	CostCentre		-- The enclosing cost-centre (when profiling)
	InTypeEnv		-- Maps old type variables to new clones
	InIdEnv			-- Maps locally-bound Ids to new clones
	OutIdEnv		-- Info about the values of OutIds
	ConAppMap		-- Maps constructor applications back to OutIds


nullSimplEnv :: SwitchChecker -> SimplEnv

nullSimplEnv sw_chkr
  = SimplEnv sw_chkr noCostCentre nullTyVarEnv nullIdEnv nullIdEnv nullConApps

combineSimplEnv :: SimplEnv -> SimplEnv -> SimplEnv
combineSimplEnv env@(SimplEnv chkr _       _      _         out_id_env con_apps)
	    new_env@(SimplEnv _    encl_cc ty_env in_id_env _          _       )
  = SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps

pprSimplEnv (SimplEnv _ _ ty_env in_id_env out_id_env con_apps) = panic "pprSimplEnv"
\end{code}


%************************************************************************
%*									*
\subsubsection{Command-line switches}
%*									*
%************************************************************************

\begin{code}
getSwitchChecker :: SimplEnv -> SwitchChecker
getSwitchChecker (SimplEnv chkr _ _ _ _ _) = chkr

switchIsSet :: SimplEnv -> SimplifierSwitch -> Bool
switchIsSet (SimplEnv chkr _ _ _ _ _) switch
  = switchIsOn chkr switch

getSimplIntSwitch :: SwitchChecker -> (Int-> SimplifierSwitch) -> Int
getSimplIntSwitch chkr switch
  = expectJust "getSimplIntSwitch" (intSwitchSet chkr switch)

	-- Crude, but simple
switchOffInlining :: SimplEnv -> SimplEnv
switchOffInlining (SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps)
  = SimplEnv chkr' encl_cc ty_env in_id_env out_id_env con_apps
  where
    chkr' EssentialUnfoldingsOnly = SwBool True
    chkr' other			  = chkr other
\end{code}

%************************************************************************
%*									*
\subsubsection{The ``enclosing cost-centre''}
%*									*
%************************************************************************

\begin{code}
setEnclosingCC :: SimplEnv -> CostCentre -> SimplEnv

setEnclosingCC (SimplEnv chkr _ ty_env in_id_env out_id_env con_apps) encl_cc
  = SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps

getEnclosingCC :: SimplEnv -> CostCentre
getEnclosingCC (SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps) = encl_cc
\end{code}

%************************************************************************
%*									*
\subsubsection{The @TypeEnv@ part}
%*									*
%************************************************************************

\begin{code}
type InTypeEnv = TypeEnv	-- Maps InTyVars to OutTypes

extendTyEnv :: SimplEnv -> TyVar -> Type -> SimplEnv
extendTyEnv (SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps) tyvar ty
  = SimplEnv chkr encl_cc new_ty_env in_id_env out_id_env con_apps
  where
    new_ty_env = addOneToTyVarEnv ty_env tyvar ty

extendTyEnvList :: SimplEnv -> [(TyVar,Type)] -> SimplEnv
extendTyEnvList (SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps) pairs
  = SimplEnv chkr encl_cc new_ty_env in_id_env out_id_env con_apps
  where
    new_ty_env = growTyVarEnvList ty_env pairs

simplTy     (SimplEnv _ _ ty_env _ _ _) ty = applyTypeEnvToTy ty_env ty
simplTyInId (SimplEnv _ _ ty_env _ _ _) id = applyTypeEnvToId ty_env id
\end{code}

%************************************************************************
%*									*
\subsubsection{The ``Id env'' part}
%*									*
%************************************************************************

\begin{code}
type InIdEnv = IdEnv OutArg	-- Maps InIds to their value
				-- Usually this is just the cloned Id, but if
				-- if the orig defn is a let-binding, and
				-- the RHS of the let simplifies to an atom,
				-- we just bind the variable to that atom, and
				-- elide the let.
\end{code}

\begin{code}
lookupId :: SimplEnv -> Id -> OutArg

lookupId (SimplEnv _ _ _ in_id_env _ _) id
  = case (lookupIdEnv in_id_env id) of
      Just atom -> atom
      Nothing   -> VarArg id
\end{code}

\begin{code}
extendIdEnvWithAtom
	:: SimplEnv
	-> InBinder
        -> OutArg{-Val args only, please-}
	-> SimplEnv

extendIdEnvWithAtom (SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps)
		    (in_id,occ_info) atom
  = case atom of
     LitArg _      -> SimplEnv chkr encl_cc ty_env new_in_id_env out_id_env con_apps
     VarArg out_id -> SimplEnv chkr encl_cc ty_env new_in_id_env 
			       (modifyOccInfo out_id_env (uniqueOf out_id, occ_info)) con_apps
--SimplEnv chkr encl_cc ty_env new_in_id_env new_out_id_env con_apps
  where
    new_in_id_env  = addOneToIdEnv in_id_env in_id atom
{-
    new_out_id_env = case atom of
			LitArg _      -> out_id_env
			VarArg out_id -> modifyOccInfo out_id_env (uniqueOf out_id, occ_info)
-}

extendIdEnvWithAtoms :: SimplEnv -> [(InBinder, OutArg)] -> SimplEnv
extendIdEnvWithAtoms = foldr (\ (bndr,val) env -> extendIdEnvWithAtom env bndr val)


extendIdEnvWithClone :: SimplEnv -> InBinder -> OutId -> SimplEnv

extendIdEnvWithClone (SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps)
		     (in_id,_) out_id
  = SimplEnv chkr encl_cc ty_env new_in_id_env out_id_env con_apps
  where
    new_in_id_env = addOneToIdEnv in_id_env in_id (VarArg out_id)

extendIdEnvWithClones :: SimplEnv -> [InBinder] -> [OutId] -> SimplEnv
extendIdEnvWithClones (SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps)
		      in_binders out_ids
  = SimplEnv chkr encl_cc ty_env new_in_id_env out_id_env con_apps
  where
    new_in_id_env = growIdEnvList in_id_env bindings
    bindings      = zipEqual "extendIdEnvWithClones"
		 	     [id | (id,_) <- in_binders]
			     (map VarArg out_ids)
\end{code}

%************************************************************************
%*									*
\subsubsection{The @OutIdEnv@}
%*									*
%************************************************************************


The domain of @OutIdInfo@ is some, but not necessarily all, in-scope @OutId@s;
both locally-bound ones, and perhaps some imported ones too.

\begin{code}
type OutIdEnv = IdEnv (OutId, BinderInfo, RhsInfo)

\end{code}

The "Id" part is just so that we can recover the domain of the mapping, which
IdEnvs don't allow directly.

The @BinderInfo@ tells about the occurrences of the @OutId@.
Anything that isn't in here should be assumed to occur many times.
We keep this info so we can modify it when something changes.

The @RhsInfo@ part tells about the value to which the @OutId@ is bound.

\begin{code}
data RhsInfo = NoRhsInfo
	     | OtherLit [Literal]		-- It ain't one of these
	     | OtherCon [Id]			-- It ain't one of these

	     | InUnfolding SimplEnv		-- Un-simplified unfolding
			   SimpleUnfolding	-- (need to snag envts therefore)

	     | OutUnfolding CostCentre
			    SimpleUnfolding	-- Already-simplified unfolding

lookupOutIdEnv :: SimplEnv -> OutId -> Maybe (OutId,BinderInfo,RhsInfo)
lookupOutIdEnv (SimplEnv _ _ _ _ out_id_env _) id = lookupIdEnv out_id_env id

lookupRhsInfo :: SimplEnv -> OutId -> RhsInfo
lookupRhsInfo env id
  = case lookupOutIdEnv env id of
	Just (_,_,info) -> info
	Nothing		-> NoRhsInfo

modifyOutEnvItem :: (OutId, BinderInfo, RhsInfo)
	         -> (OutId, BinderInfo, RhsInfo) 
	         -> (OutId, BinderInfo, RhsInfo)
modifyOutEnvItem (id, occ, info1) (_, _, info2)
  = case (info1, info2) of
		(OtherLit ls1, OtherLit ls2) -> (id,occ, OtherLit (ls1++ls2))
		(OtherCon cs1, OtherCon cs2) -> (id,occ, OtherCon (cs1++cs2))
		(_,            NoRhsInfo)    -> (id,occ, info1)
		other	       		     -> (id,occ, info2)

--(id, occ, new_info)
{-
  where
    new_info = case (info1, info2) of
		(OtherLit ls1, OtherLit ls2) -> OtherLit (ls1++ls2)
		(OtherCon cs1, OtherCon cs2) -> OtherCon (cs1++cs2)
		(_,            NoRhsInfo)    -> info1
		other	       		     -> info2
-}
\end{code}


\begin{code}
isEvaluated :: RhsInfo -> Bool
isEvaluated (OtherLit _) = True
isEvaluated (OtherCon _) = True
isEvaluated (InUnfolding _  (SimpleUnfolding ValueForm _ expr)) = True
isEvaluated (OutUnfolding _ (SimpleUnfolding ValueForm _ expr)) = True
isEvaluated other = False
\end{code}

@extendUnfoldEnvGivenRhs@ records in the UnfoldEnv info about the RHS
of a new binding.  There is a horrid case we have to take care about,
due to Andr\'e Santos:
@
    type Array_type b   = Array Int b;
    type Descr_type     = (Int,Int);

    tabulate      :: (Int -> x) -> Descr_type -> Array_type x;
    tabulate      f (l,u)             = listArray (l,u) [f i | i <- [l..u]];

    f_iaamain a_xs=
	let {
	    f_aareorder::(Array_type Int) -> (Array_type t1) -> Array_type t1;
	    f_aareorder a_index a_ar=
		let {
		    f_aareorder' a_i= a_ar ! (a_index ! a_i)
		 } in  tabulate f_aareorder' (bounds a_ar);
	    r_index=tabulate ((+) 1) (1,1);
	    arr    = listArray (1,1) a_xs;
	    arg    = f_aareorder r_index arr
	 } in  elems arg
@
Now, when the RHS of arg gets simplified, we inline f_aareorder to get
@
	arg  = let f_aareorder' a_i = arr ! (r_index ! a_i)
	       in tabulate f_aareorder' (bounds arr)
@
Note that r_index is not inlined, because it was bound to a_index which
occurs inside a lambda.

Alas, if elems is inlined, so that (elems arg) becomes (case arg of ...),
then arg is inlined. IF WE USE THE NEW VERSION OF arg, and re-occurrence
analyse it, we won't spot the inside-lambda property of r_index, so r_index
will get inlined inside the lambda.  AARGH.

Solution: when we occurrence-analyse the new RHS we have to go back
and modify the info recorded in the UnfoldEnv for the free vars
of the RHS.  In the example we'd go back and record that r_index is now used
inside a lambda.

\begin{code}
extendEnvGivenNewRhs :: SimplEnv -> OutId -> OutExpr -> SimplEnv
extendEnvGivenNewRhs env out_id rhs
  = extendEnvGivenBinding env noBinderInfo out_id rhs

extendEnvGivenBinding :: SimplEnv -> BinderInfo -> OutId -> OutExpr -> SimplEnv
extendEnvGivenBinding env@(SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps)
	              occ_info out_id rhs
  = let
     s_env = SimplEnv chkr encl_cc ty_env in_id_env out_id_env new_con_apps 
     s_env_uf = SimplEnv chkr encl_cc ty_env in_id_env out_id_env_with_unfolding new_con_apps
    in
    case guidance of 
       -- Cheap and nasty hack to force strict insertion.  
     UnfoldNever -> 
         if isEmptyFM new_con_apps then s_env else s_env
     other       -> 
         if isEmptyFM new_con_apps then s_env_uf else s_env_uf
  where
    new_con_apps = extendConApps con_apps out_id rhs
{-
    new_out_id_env = case guidance of
			UnfoldNever -> out_id_env		-- No new stuff to put in
		        other	    -> out_id_env_with_unfolding
-}
	-- If there is an unfolding, we add rhs-info for out_id,
	-- *and* modify the occ info for rhs's interesting free variables.
	--
	-- If the out_id is already in the OutIdEnv, then just replace the
	-- unfolding, leaving occurrence info alone (this must then
	-- be a call via extendEnvGivenNewRhs).
    out_id_env_with_unfolding = foldl modifyOccInfo env1 full_fv_occ_info
		-- full_fv_occ_info combines the occurrence of the current binder
		-- with the occurrences of its RHS's free variables.
    full_fv_occ_info	      = [ (uniq, fv_occ `andBinderInfo` occ_info) 
				| (uniq,fv_occ) <- ufmToList fv_occ_info
				]
    env1 		      =	addToUFM_C modifyOutEnvItem out_id_env out_id 
					   (out_id, occ_info, rhs_info)

	-- Occurrence-analyse the RHS
	-- The "interesting" free variables we want occurrence info for are those
	-- in the OutIdEnv that have only a single occurrence right now.
    (fv_occ_info, template) = occurAnalyseExpr interesting_fvs rhs
    interesting_fvs	    = mkIdSet [id | (id,OneOcc _ _ _ _ _,_) <- eltsUFM out_id_env]

	-- Compute unfolding details
    rhs_info     = OutUnfolding unf_cc (SimpleUnfolding form_summary guidance template)
    form_summary = mkFormSummary rhs

    guidance = mkSimplUnfoldingGuidance chkr out_id rhs

	-- Compute cost centre for thing
    unf_cc  | noCostCentreAttached expr_cc = encl_cc
	    | otherwise		           = expr_cc
	    where
	      expr_cc =  coreExprCc rhs
\end{code}



Recursive bindings
~~~~~~~~~~~~~~~~~~
We need to be pretty careful when extending 
the environment with RHS info in recursive groups.

Here's a nasty example:

	letrec	r = f x
		t = r
		x = ...t...
	in
	...t...

Here, r occurs exactly once, so we may reasonably inline r in t's RHS.
But the pre-simplified t's rhs is an atom, r, so we may also decide to
inline t everywhere.  But if we do *both* these reasonable things we get

	letrec	r = f x
		t = f x
		x = ...r...
	in
	...t...

Bad news!  (f x) is duplicated!  (The t in the body doesn't get
inlined because by the time the recursive group is done we see that
t's RHS isn't an atom.)

Our solution is this: 
	(a) we inline un-simplified RHSs, and then simplify
	    them in a clone-only environment.  
	(b) we inline only variables and values
This means that


	r = f x 	==>  r = f x
	t = r		==>  t = r
	x = ...t...	==>  x = ...r...
     in			   in
	t		     r

Now t is dead, and we're home.

Most silly x=y  bindings in recursive group will go away.  But not all:

	let y = 1:x
	    x = y

Here, we can't inline x because it's in an argument position. so we'll just replace
with a clone of y.  Instead we'll probably inline y (a small value) to give

	let y = 1:x
	    x = 1:y
	
which is OK if not clever.

\begin{code}
extendEnvForRecBinding env@(SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps)
		       (out_id, ((_,occ_info), old_rhs))
  = case (form_summary, guidance) of
     (_, UnfoldNever)	-> SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps -- No new stuff to put in
     (ValueForm, _)	-> SimplEnv chkr encl_cc ty_env in_id_env out_id_env_with_unfolding con_apps
     (VarForm, _)	-> SimplEnv chkr encl_cc ty_env in_id_env out_id_env_with_unfolding con_apps
     other	    	-> SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps	-- Not a value or variable
     
-- SimplEnv chkr encl_cc ty_env in_id_env new_out_id_env con_apps
  where
{-
    new_out_id_env = case (form_summary, guidance) of
			(_, UnfoldNever)	-> out_id_env		-- No new stuff to put in
			(ValueForm, _)		-> out_id_env_with_unfolding
			(VarForm, _)		-> out_id_env_with_unfolding
		        other	    		-> out_id_env		-- Not a value or variable
-}
	-- If there is an unfolding, we add rhs-info for out_id,
	-- No need to modify occ info because RHS is pre-simplification
    out_id_env_with_unfolding =	addOneToIdEnv out_id_env out_id 
			        (out_id, occ_info, rhs_info)

	-- Compute unfolding details
	-- Note that we use the "old" environment, that just has clones of the rec-bound vars,
	-- in the InUnfolding.  So if we ever use the InUnfolding we'll just inline once.
	-- Only if the thing is still small enough next time round will we inline again.
    rhs_info     = InUnfolding env (SimpleUnfolding form_summary guidance old_rhs)
    form_summary = mkFormSummary old_rhs
    guidance     = mkSimplUnfoldingGuidance chkr out_id (unTagBinders old_rhs)


mkSimplUnfoldingGuidance chkr out_id rhs
  = case calcUnfoldingGuidance inline_prag opt_UnfoldingCreationThreshold rhs of
     UnfoldNever -> UnfoldNever
     v           -> v
  where
    inline_prag = not (switchIsOn chkr IgnoreINLINEPragma) && idWantsToBeINLINEd out_id

extendEnvGivenRhsInfo :: SimplEnv -> OutId -> BinderInfo -> RhsInfo -> SimplEnv
extendEnvGivenRhsInfo env@(SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps)
	              out_id occ_info rhs_info
  = SimplEnv chkr encl_cc ty_env in_id_env new_out_id_env con_apps
  where
    new_out_id_env = addToUFM_C modifyOutEnvItem out_id_env out_id 
				(out_id, occ_info, rhs_info)
\end{code}


\begin{code}
modifyOccInfo out_id_env (uniq, new_occ)
  = modifyIdEnv_Directly modify_fn out_id_env uniq
  where
    modify_fn (id,occ,rhs) = (id, orBinderInfo occ new_occ, rhs)

markDangerousOccs (SimplEnv chkr encl_cc ty_env in_id_env out_id_env con_apps) atoms
  = SimplEnv chkr encl_cc ty_env in_id_env new_out_id_env con_apps
  where
    new_out_id_env = foldl (modifyIdEnv modify_fn) out_id_env [v | VarArg v <- atoms]
    modify_fn (id,occ,rhs) = (id, noBinderInfo, rhs)
\end{code}



%************************************************************************
%*									*
\subsubsection{The @ConAppMap@ type}
%*									*
%************************************************************************

The @ConAppMap@ maps applications of constructors (to value atoms)
back to an association list that says "if the constructor was applied
to one of these lists-of-Types, then this OutId is your man (in a
non-gender-specific sense)".  I.e., this is a reversed mapping for
(part of) the main OutIdEnv

\begin{code}
type ConAppMap = FiniteMap UnfoldConApp [([Type], OutId)]

data UnfoldConApp
  = UCA		OutId			-- data constructor
		[OutArg]		-- *value* arguments; see use below
\end{code}

\begin{code}
nullConApps = emptyFM

extendConApps con_apps id (Con con args)
  = addToFM_C (\old new -> new++old) con_apps (UCA con val_args) [(ty_args,id)]
  where
    val_args = filter isValArg args		-- Literals and Ids
    ty_args  = [ty | TyArg ty <- args]		-- Just types

extendConApps con_apps id other_rhs = con_apps
\end{code}

\begin{code}
lookForConstructor (SimplEnv _ _ _ _ _ con_apps) con args
  = case lookupFM con_apps (UCA con val_args) of
	Nothing     -> Nothing

	Just assocs -> case [id | (tys, id) <- assocs, 
				  and (zipWith eqTy tys ty_args)]
		       of
			  []     -> Nothing
			  (id:_) -> Just id
  where
    val_args = filter isValArg args		-- Literals and Ids
    ty_args  = [ty | TyArg ty <- args]		-- Just types

\end{code}

NB: In @lookForConstructor@ we used (before Apr 94) to have a special case
for nullary constructors, but now we only do constructor re-use in
let-bindings the special case isn't necessary any more.

\begin{verbatim}	
  = 	-- Don't re-use nullary constructors; it's a waste.  Consider
	-- let
	-- 	  a = leInt#! p q
	-- in
	-- case a of
	--    True  -> ...
	--    False -> False
	--
	-- Here the False in the second case will get replace by "a", hardly
	-- a good idea
    Nothing
\end{verbatim}


The main thing about @UnfoldConApp@ is that it has @Ord@ defined on
it, so we can use it for a @FiniteMap@ key.

\begin{code}
instance Eq  UnfoldConApp where
    a == b = case (a `cmp` b) of { EQ_ -> True;   _ -> False }
    a /= b = case (a `cmp` b) of { EQ_ -> False;  _ -> True  }

instance Ord UnfoldConApp where
    a <= b = case (a `cmp` b) of { LT_ -> True;  EQ_ -> True;  GT__ -> False }
    a <  b = case (a `cmp` b) of { LT_ -> True;  EQ_ -> False; GT__ -> False }
    a >= b = case (a `cmp` b) of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >  b = case (a `cmp` b) of { LT_ -> False; EQ_ -> False; GT__ -> True  }
    _tagCmp a b = case (a `cmp` b) of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }

instance Ord3 UnfoldConApp where
    cmp = cmp_app

cmp_app (UCA c1 as1) (UCA c2 as2)
  = cmp c1 c2 `thenCmp` cmpList cmp_arg as1 as2
  where
    -- ToDo: make an "instance Ord3 CoreArg"???

    cmp_arg (VarArg   x) (VarArg   y) = x `cmp` y
    cmp_arg (LitArg   x) (LitArg   y) = x `cmp` y
    cmp_arg (TyArg    x) (TyArg    y) = panic# "SimplEnv.cmp_app:TyArgs"
    cmp_arg (UsageArg x) (UsageArg y) = panic# "SimplEnv.cmp_app:UsageArgs"
    cmp_arg x y
      | tag x _LT_ tag y = LT_
      | otherwise	 = GT_
      where
	tag (VarArg   _) = ILIT(1)
	tag (LitArg   _) = ILIT(2)
	tag (TyArg    _) = panic# "SimplEnv.cmp_app:TyArg"
	tag (UsageArg _) = panic# "SimplEnv.cmp_app:UsageArg"
\end{code}



