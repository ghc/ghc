%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplMonad]{The simplifier Monad}

\begin{code}
module SimplMonad (
	InId, InBind, InExpr, InAlt, InArg, InType, InBinder,
	OutId, OutBind, OutExpr, OutAlt, OutArg, OutType, OutBinder,
	OutExprStuff, OutStuff,

	-- The continuation type
	SimplCont(..), DupFlag(..), contIsDupable, contResultType,

	-- The monad
	SimplM,
	initSmpl, returnSmpl, thenSmpl, thenSmpl_,
	mapSmpl, mapAndUnzipSmpl, mapAccumLSmpl,

        -- Unique supply
        getUniqueSmpl, getUniquesSmpl,
	newId, newIds,

	-- Counting
	SimplCount, TickType(..), TickCounts,
	tick, tickUnfold,
	getSimplCount, zeroSimplCount, pprSimplCount, 
	plusSimplCount, isZeroSimplCount,

	-- Switch checker
	SwitchChecker, getSwitchChecker, getSimplIntSwitch,

	-- Cost centres
	getEnclosingCC, setEnclosingCC,

	-- Environments
	InScopeEnv, SubstEnv,
	getInScope, setInScope, extendInScope, extendInScopes, modifyInScope,
	emptySubstEnv, getSubstEnv, setSubstEnv, zapSubstEnv,
	extendIdSubst, extendTySubst,
	getTyEnv, getValEnv,
	getSimplBinderStuff, setSimplBinderStuff,
	switchOffInlining
    ) where

#include "HsVersions.h"

import Id		( Id, mkSysLocal, idMustBeINLINEd )
import IdInfo		( InlinePragInfo(..) )
import Demand		( Demand )
import CoreSyn
import CoreUtils	( IdSubst, SubstCoreExpr, coreExprType, coreAltsType )
import CostCentre	( CostCentreStack, subsumedCCS )
import Var		( TyVar )
import VarEnv
import VarSet
import Type             ( Type, TyVarSubst, funResultTy, fullSubstTy, applyTy )
import UniqSupply	( uniqsFromSupply, uniqFromSupply, splitUniqSupply,
			  UniqSupply
			)
import CmdLineOpts	( SimplifierSwitch(..), SwitchResult(..), intSwitchSet )
import Unique		( Unique )
import Maybes		( expectJust )
import Util		( zipWithEqual )
import Outputable

infixr 9  `thenSmpl`, `thenSmpl_`
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-types]{Type declarations}
%*									*
%************************************************************************

\begin{code}
type InBinder  = CoreBndr
type InId      = Id			-- Not yet cloned
type InType    = Type			-- Ditto
type InBind    = CoreBind
type InExpr    = CoreExpr
type InAlt     = CoreAlt
type InArg     = CoreArg

type OutBinder  = CoreBndr
type OutId	= Id			-- Cloned
type OutType	= Type			-- Cloned
type OutBind	= CoreBind
type OutExpr	= CoreExpr
type OutAlt	= CoreAlt
type OutArg	= CoreArg

type SwitchChecker = SimplifierSwitch -> SwitchResult
\end{code}


%************************************************************************
%*									*
\subsection{The continuation data type}
%*									*
%************************************************************************

\begin{code}
type OutExprStuff = OutStuff (InScopeEnv, OutExpr)
type OutStuff a   = ([OutBind], a)
	-- We return something equivalent to (let b in e), but
	-- in pieces to avoid the quadratic blowup when floating 
	-- incrementally.  Comments just before simplExprB in Simplify.lhs

data SimplCont		-- Strict contexts
  = Stop

  | CoerceIt DupFlag
	     InType SubstEnv
	     SimplCont

  | ApplyTo  DupFlag 
	     InExpr SubstEnv		-- The argument, as yet unsimplified, 
	     SimplCont			-- and its subst-env

  | Select   DupFlag 
	     InId [InAlt] SubstEnv	-- The case binder, alts, and subst-env
	     SimplCont

  | ArgOf    DupFlag				-- An arbitrary strict context: the argument 
  	     (OutExpr -> SimplM OutExprStuff)	-- of a strict function, or a primitive-arg fn
						-- or a PrimOp
	     OutType				-- Type of the result of the whole thing

instance Outputable SimplCont where
  ppr Stop        		     = ptext SLIT("Stop")
  ppr (ApplyTo dup arg se cont)      = (ptext SLIT("ApplyTo") <+> ppr dup <+> ppr arg) $$ ppr cont
  ppr (ArgOf   dup cont_fn _)        = ptext SLIT("ArgOf...") <+> ppr dup
  ppr (Select dup bndr alts se cont) = (ptext SLIT("Select") <+> ppr dup <+> ppr bndr) $$ 
				       (nest 4 (ppr alts)) $$ ppr cont
  ppr (CoerceIt dup ty se cont)	     = (ptext SLIT("CoerceIt") <+> ppr dup <+> ppr ty) $$ ppr cont

data DupFlag = OkToDup | NoDup

instance Outputable DupFlag where
  ppr OkToDup = ptext SLIT("ok")
  ppr NoDup   = ptext SLIT("nodup")

contIsDupable :: SimplCont -> Bool
contIsDupable Stop        		 = True
contIsDupable (ApplyTo  OkToDup _ _ _)   = True
contIsDupable (ArgOf    OkToDup _ _)     = True
contIsDupable (Select   OkToDup _ _ _ _) = True
contIsDupable (CoerceIt OkToDup _ _ _)   = True
contIsDupable other			 = False

contResultType :: InScopeEnv -> Type -> SimplCont -> Type
contResultType in_scope e_ty cont
  = go e_ty cont
  where
    go e_ty Stop		          = e_ty
    go e_ty (ApplyTo _ (Type ty) se cont) = go (applyTy e_ty (simpl se ty))     cont
    go e_ty (ApplyTo _ val_arg _ cont)    = go (funResultTy e_ty)		cont
    go e_ty (ArgOf _ fun cont_ty)         = cont_ty
    go e_ty (CoerceIt _ ty se cont)   	  = go (simpl se ty)	                cont
    go e_ty (Select _ _ alts se cont) 	  = go (simpl se (coreAltsType alts))   cont

    simpl (ty_subst, _) ty = fullSubstTy ty_subst in_scope ty
\end{code}


%************************************************************************
%*									*
\subsection{Monad plumbing}
%*									*
%************************************************************************

For the simplifier monad, we want to {\em thread} a unique supply and a counter.
(Command-line switches move around through the explicitly-passed SimplEnv.)

\begin{code}
type SimplM result		-- We thread the unique supply because
  =  SimplEnv			-- constantly splitting it is rather expensive
  -> UniqSupply
  -> SimplCount 
  -> (result, UniqSupply, SimplCount)

data SimplEnv
  = SimplEnv {
	seChkr     :: SwitchChecker,
	seCC       :: CostCentreStack,	-- The enclosing CCS (when profiling)
	seSubst    :: SubstEnv,		-- The current substitution
	seInScope  :: InScopeEnv	-- Says what's in scope and gives info about it
    }
\end{code}

\begin{code}
initSmpl :: SwitchChecker
	 -> UniqSupply		-- No init count; set to 0
	 -> SimplM a
	 -> (a, SimplCount)

initSmpl chkr us m = case m (emptySimplEnv chkr) us zeroSimplCount of 
			(result, _, count) -> (result, count)


{-# INLINE thenSmpl #-}
{-# INLINE thenSmpl_ #-}
{-# INLINE returnSmpl #-}

returnSmpl :: a -> SimplM a
returnSmpl e env us sc = (e, us, sc)

thenSmpl  :: SimplM a -> (a -> SimplM b) -> SimplM b
thenSmpl_ :: SimplM a -> SimplM b -> SimplM b

thenSmpl m k env us0 sc0
  = case (m env us0 sc0) of 
	(m_result, us1, sc1) -> k m_result env us1 sc1

thenSmpl_ m k env us0 sc0
  = case (m env us0 sc0) of 
	(_, us1, sc1) -> k env us1 sc1
\end{code}


\begin{code}
mapSmpl	    	:: (a -> SimplM b) -> [a] -> SimplM [b]
mapAndUnzipSmpl :: (a -> SimplM (b, c)) -> [a] -> SimplM ([b],[c])

mapSmpl f [] = returnSmpl []
mapSmpl f (x:xs)
  = f x		    `thenSmpl` \ x'  ->
    mapSmpl f xs    `thenSmpl` \ xs' ->
    returnSmpl (x':xs')

mapAndUnzipSmpl f [] = returnSmpl ([],[])
mapAndUnzipSmpl f (x:xs)
  = f x			    `thenSmpl` \ (r1,  r2)  ->
    mapAndUnzipSmpl f xs    `thenSmpl` \ (rs1, rs2) ->
    returnSmpl (r1:rs1, r2:rs2)

mapAccumLSmpl f acc []     = returnSmpl (acc, [])
mapAccumLSmpl f acc (x:xs) = f acc x	`thenSmpl` \ (acc', x') ->
			     mapAccumLSmpl f acc' xs	`thenSmpl` \ (acc'', xs') ->
			     returnSmpl (acc'', x':xs')
\end{code}


%************************************************************************
%*									*
\subsection{The unique supply}
%*									*
%************************************************************************

\begin{code}
getUniqueSmpl :: SimplM Unique
getUniqueSmpl env us sc = case splitUniqSupply us of
				(us1, us2) -> (uniqFromSupply us1, us2, sc)

getUniquesSmpl :: Int -> SimplM [Unique]
getUniquesSmpl n env us sc = case splitUniqSupply us of
				(us1, us2) -> (uniqsFromSupply n us1, us2, sc)
\end{code}


%************************************************************************
%*									*
\subsection{Counting up what we've done}
%*									*
%************************************************************************

\begin{code}
doTickSmpl :: (SimplCount -> SimplCount) -> SimplM ()
doTickSmpl f env us sc = sc' `seq` ((), us, sc')
		       where
			 sc' = f sc

getSimplCount :: SimplM SimplCount
getSimplCount env us sc = (sc, us, sc)
\end{code}


The assoc list isn't particularly costly, because we only use
the number of ticks in ``real life.''

The right thing to do, if you want that to go fast, is thread
a mutable array through @SimplM@.

\begin{code}
data SimplCount
  = SimplCount	!TickCounts
		!UnfoldingHistory

type TickCounts = [(TickType, Int)]	-- Assoc list of all diff kinds of ticks
					-- Kept in increasing order of TickType
					-- Zeros not present

type UnfoldingHistory = (Int,		-- N
		         [Id],	 	-- Last N unfoldings
		         [Id])		-- The MaxUnfoldHistory unfoldings before that

data TickType
  = PreInlineUnconditionally
  | PostInlineUnconditionally
  | UnfoldingDone    
  | MagicUnfold
  | CaseOfCase
  | LetFloatFromLet
  | KnownBranch	     
  | Let2Case 	
  | Case2Let
  | CaseMerge	     
  | CaseElim
  | CaseIdentity
  | EtaExpansion
  | CaseOfError
  | BetaReduction
  | SpecialisationDone
  | FillInCaseDefault
  | LeavesExamined
  deriving (Eq, Ord, Show)

pprSimplCount :: SimplCount -> SDoc
pprSimplCount (SimplCount stuff (_, unf1, unf2))
  = vcat (map ppr_item stuff) 
    $$ (text "Most recent unfoldings (most recent at top):" 
	$$ nest 4 (vcat (map ppr (unf1 ++ unf2))))
  where
    ppr_item (t,n) = text (show t) <+> char '\t' <+> ppr n

zeroSimplCount :: SimplCount
zeroSimplCount = SimplCount [] (0, [], [])

isZeroSimplCount :: SimplCount -> Bool
isZeroSimplCount (SimplCount []                   _) = True
isZeroSimplCount (SimplCount [(LeavesExamined,_)] _) = True
isZeroSimplCount other				     = False

-- incTick is careful to be pretty strict, so we don't
-- get a huge buildup of thunks
incTick :: TickType -> FAST_INT -> TickCounts -> TickCounts
incTick tick_type n []
  = [(tick_type, IBOX(n))]

incTick tick_type n (x@(ttype, I# cnt#) : xs)
  = case tick_type `compare` ttype of
	LT -> 	-- Insert here
		(tick_type, IBOX(n)) : x : xs

	EQ -> 	-- Increment
		case cnt# +# n of
		   incd -> (ttype, IBOX(incd)) : xs

	GT -> 	-- Move on
		rest `seq` x : rest
	   where
		rest = incTick tick_type n xs

-- Second argument is more recent stuff
plusSimplCount :: SimplCount -> SimplCount -> SimplCount
plusSimplCount (SimplCount tc1 uh1) (SimplCount tc2 uh2)
  = SimplCount (plusTickCounts tc1 tc2) (plusUnfolds uh1 uh2)

plusTickCounts :: TickCounts -> TickCounts -> TickCounts
plusTickCounts ts1 [] = ts1
plusTickCounts [] ts2 = ts2
plusTickCounts ((tt1,n1) : ts1) ((tt2,n2) : ts2) 
  = case tt1 `compare` tt2 of
	LT -> (tt1,n1)    : plusTickCounts ts1              ((tt2,n2) : ts2)
	EQ -> (tt1,n1+n2) : plusTickCounts ts1              ts2
	GT -> (tt2,n2)    : plusTickCounts ((tt1,n1) : ts1) ts2

-- Second argument is the more recent stuff
plusUnfolds uh1          (0, h2, t2)  = uh1			-- Nothing recent
plusUnfolds (n1, h1, t1) (n2, h2, []) = (n2, h2, (h1++t1))	-- Small amount recent
plusUnfolds (n1, h1, t1) uh2          = uh2			-- Decent batch recent
\end{code}


Counting-related monad functions:

\begin{code}
tick :: TickType -> SimplM ()

tick tick_type
  = doTickSmpl f
  where
    f (SimplCount stuff unf) = SimplCount (incTick tick_type ILIT(1) stuff) unf

maxUnfoldHistory :: Int
maxUnfoldHistory = 20

tickUnfold :: Id -> SimplM ()
tickUnfold id 
  = doTickSmpl f
  where 
    f (SimplCount stuff (n_unf, unf1, unf2))
      | n_unf >= maxUnfoldHistory = SimplCount new_stuff (1, [id], unf1)
      | otherwise 		  = SimplCount new_stuff (n_unf+1, id:unf1, unf2)
      where
	new_stuff = incTick UnfoldingDone ILIT(1) stuff
\end{code}


%************************************************************************
%*									*
\subsubsection{Command-line switches}
%*									*
%************************************************************************

\begin{code}
getSwitchChecker :: SimplM SwitchChecker
getSwitchChecker env us sc = (seChkr env, us, sc)

getSimplIntSwitch :: SwitchChecker -> (Int-> SimplifierSwitch) -> Int
getSimplIntSwitch chkr switch
  = expectJust "getSimplIntSwitch" (intSwitchSet chkr switch)
\end{code}


@switchOffInlining@ is used to prepare the environment for simplifying
the RHS of an Id that's marked with an INLINE pragma.  It is going to
be inlined wherever they are used, and then all the inlining will take
effect.  Meanwhile, there isn't much point in doing anything to the
as-yet-un-INLINEd rhs.  Furthremore, it's very important to switch off
inlining!  because
	(a) not doing so will inline a worker straight back into its wrapper!

and 	(b) Consider the following example 
	     	let f = \pq -> BIG
	     	in
	     	let g = \y -> f y y
		    {-# INLINE g #-}
	     	in ...g...g...g...g...g...

	Now, if that's the ONLY occurrence of f, it will be inlined inside g,
	and thence copied multiple times when g is inlined.

	Andy disagrees! Example:
		all xs = foldr (&&) True xs
		any p = all . map p  {-# INLINE any #-}
	
	Problem: any won't get deforested, and so if it's exported and
	the importer doesn't use the inlining, (eg passes it as an arg)
	then we won't get deforestation at all.
	We havn't solved this problem yet!

We prepare the envt by simply modifying the in_scope_env, which has all the
unfolding info. At one point we did it by modifying the chkr so that
it said "EssentialUnfoldingsOnly", but that prevented legitmate, and
important, simplifications happening in the body of the RHS.

6/98 update: 

We *don't* prevent inlining from happening for identifiers
that are marked as IMustBeINLINEd. An example of where
doing this is crucial is:
  
   class Bar a => Foo a where
     ...g....
   {-# INLINE f #-}
   f :: Foo a => a -> b
   f x = ....Foo_sc1...
   
If `f' needs to peer inside Foo's superclass, Bar, it refers
to the appropriate super class selector, which is marked as
must-inlineable. We don't generate any code for a superclass
selector, so failing to inline it in the RHS of `f' will
leave a reference to a non-existent id, with bad consequences.

ALSO NOTE that we do all this by modifing the inline-pragma,
not by zapping the unfolding.  The latter may still be useful for
knowing when something is evaluated.

June 98 update: I've gone back to dealing with this by adding
the EssentialUnfoldingsOnly switch.  That doesn't stop essential
unfoldings, nor inlineUnconditionally stuff; and the thing's going
to be inlined at every call site anyway.  Running over the whole
environment seems like wild overkill.

\begin{code}
switchOffInlining :: SimplM a -> SimplM a
switchOffInlining m env@(SimplEnv { seChkr = sw_chkr }) us sc
  = m (env { seChkr = new_chkr  }) us sc
  where
    new_chkr EssentialUnfoldingsOnly = SwBool True
    new_chkr other		     = sw_chkr other
\end{code}


%************************************************************************
%*									*
\subsubsection{The ``enclosing cost-centre''}
%*									*
%************************************************************************

\begin{code}
getEnclosingCC :: SimplM CostCentreStack
getEnclosingCC env us sc = (seCC env, us, sc)

setEnclosingCC :: CostCentreStack -> SimplM a -> SimplM a
setEnclosingCC cc m env us sc = m (env { seCC = cc }) us sc
\end{code}


%************************************************************************
%*									*
\subsubsection{The @SimplEnv@ type}
%*									*
%************************************************************************

\begin{code}
type SubstEnv = (TyVarSubst, IdSubst)
	-- The range of these substitutions is OutType and OutExpr resp
	-- 
	-- The substitution is idempotent
	-- It *must* be applied; things in its domain simply aren't
	-- bound in the result.
	--
	-- The substitution usually maps an Id to its clone,
	-- but if the orig defn is a let-binding, and
	-- the RHS of the let simplifies to an atom,
	-- we just add the binding to the substitution and elide the let.

type InScopeEnv = IdOrTyVarSet
	-- Domain includes *all* in-scope TyVars and Ids
	--
	-- The elements of the set may have better IdInfo than the
	-- occurrences of in-scope Ids, and (more important) they will
	-- have a correctly-substituted type.  So we use a lookup in this
	-- set to replace occurrences

-- INVARIANT:	If t is in the in-scope set, it certainly won't be
-- 		in the domain of the SubstEnv, and vice versa
\end{code}


\begin{code}
emptySubstEnv :: SubstEnv
emptySubstEnv = (emptyVarEnv, emptyVarEnv)

emptySimplEnv :: SwitchChecker -> SimplEnv

emptySimplEnv sw_chkr
  = SimplEnv { seChkr = sw_chkr, seCC = subsumedCCS,
	       seSubst   = emptySubstEnv,
	       seInScope = emptyVarSet }

	-- The top level "enclosing CC" is "SUBSUMED".

getTyEnv :: SimplM (TyVarSubst, InScopeEnv)
getTyEnv (SimplEnv {seSubst = (ty_subst,_), seInScope = in_scope}) us sc
  = ((ty_subst, in_scope), us, sc)

getValEnv :: SimplM (IdSubst, InScopeEnv)
getValEnv (SimplEnv {seSubst = (_, id_subst), seInScope = in_scope}) us sc
  = ((id_subst, in_scope), us, sc)

getInScope :: SimplM InScopeEnv
getInScope env us sc = (seInScope env, us, sc)

setInScope :: InScopeEnv -> SimplM a -> SimplM a
setInScope in_scope m env us sc = m (env {seInScope = in_scope}) us sc

extendInScope :: CoreBndr -> SimplM a -> SimplM a
extendInScope v m env@(SimplEnv {seInScope = in_scope}) us sc
  = m (env {seInScope = extendVarSet in_scope v}) us sc

extendInScopes :: [CoreBndr] -> SimplM a -> SimplM a
extendInScopes vs m env@(SimplEnv {seInScope = in_scope}) us sc
  = m (env {seInScope = foldl extendVarSet in_scope vs}) us sc

modifyInScope :: CoreBndr -> SimplM a -> SimplM a
modifyInScope v m env us sc 
#ifdef DEBUG
  | not (v `elemVarSet` seInScope env )
  = pprTrace "modifyInScope: not in scope:" (ppr v)
    m env us sc
#endif
  | otherwise
  = extendInScope v m env us sc

getSubstEnv :: SimplM SubstEnv
getSubstEnv env us sc = (seSubst env, us, sc)

setSubstEnv :: SubstEnv -> SimplM a -> SimplM a
setSubstEnv subst_env m env us sc = m (env {seSubst = subst_env}) us sc

extendIdSubst :: Id -> SubstCoreExpr -> SimplM a -> SimplM a
extendIdSubst id expr m env@(SimplEnv {seSubst = (ty_subst, id_subst)}) us sc
  = m (env { seSubst = (ty_subst, extendVarEnv id_subst id expr) }) us sc

extendTySubst :: TyVar -> OutType -> SimplM a -> SimplM a
extendTySubst tv ty m env@(SimplEnv {seSubst = (ty_subst, id_subst)}) us sc
  = m (env { seSubst = (extendVarEnv ty_subst tv ty, id_subst) }) us sc

zapSubstEnv :: SimplM a -> SimplM a
zapSubstEnv m env us sc = m (env {seSubst = emptySubstEnv}) us sc

getSimplBinderStuff :: SimplM (TyVarSubst, IdSubst, InScopeEnv, UniqSupply)
getSimplBinderStuff (SimplEnv {seSubst = (ty_subst, id_subst), seInScope = in_scope}) us sc
  = ((ty_subst, id_subst, in_scope, us), us, sc)

setSimplBinderStuff :: (TyVarSubst, IdSubst, InScopeEnv, UniqSupply)
		    -> SimplM a -> SimplM a
setSimplBinderStuff (ty_subst, id_subst, in_scope, us) m env _ sc
  = m (env {seSubst = (ty_subst, id_subst), seInScope = in_scope}) us sc
\end{code}


\begin{code}
newId :: Type -> (Id -> SimplM a) -> SimplM a
	-- Extends the in-scope-env too
newId ty m env@(SimplEnv {seInScope = in_scope}) us sc
  =  case splitUniqSupply us of
	(us1, us2) -> m v (env {seInScope = extendVarSet in_scope v}) us2 sc
		   where
		      v = mkSysLocal SLIT("s") (uniqFromSupply us1) ty

newIds :: [Type] -> ([Id] -> SimplM a) -> SimplM a
newIds tys m env@(SimplEnv {seInScope = in_scope}) us sc
  =  case splitUniqSupply us of
	(us1, us2) -> m vs (env {seInScope = foldl extendVarSet in_scope vs}) us2 sc
		   where
		      vs = zipWithEqual "newIds" (mkSysLocal SLIT("s")) 
					(uniqsFromSupply (length tys) us1) tys
\end{code}

