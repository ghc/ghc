%
% (c) The GRASP/AQUA Project, Glasgow University, 2000
%
\section[FunDeps]{FunDeps - functional dependencies}

It's better to read it as: "if we know these, then we're going to know these"

\begin{code}
module FunDeps (
	oclose, grow, improve, checkInstFDs, checkClsFD, pprFundeps
    ) where

#include "HsVersions.h"

import Var		( TyVar )
import Class		( Class, FunDep, classTvsFds )
import Type		( Type, ThetaType, PredType(..), predTyUnique, tyVarsOfTypes, tyVarsOfPred )
import Subst		( mkSubst, emptyInScopeSet, substTy )
import Unify		( unifyTyListsX )
import Outputable	( Outputable, SDoc, interppSP, ptext, empty, hsep, punctuate, comma )
import VarSet
import VarEnv
import List		( tails )
import ListSetOps	( equivClassesByUniq )
\end{code}


%************************************************************************
%*									*
\subsection{Close type variables}
%*									*
%************************************************************************

(oclose preds tvs) closes the set of type variables tvs, 
wrt functional dependencies in preds.  The result is a superset
of the argument set.  For example, if we have
	class C a b | a->b where ...
then
	oclose [C (x,y) z, C (x,p) q] {x,y} = {x,y,z}
because if we know x and y then that fixes z.

Using oclose
~~~~~~~~~~~~
oclose is used

a) When determining ambiguity.  The type
	forall a,b. C a b => a
is not ambiguous (given the above class decl for C) because
a determines b.  

b) When generalising a type T.  Usually we take FV(T) \ FV(Env),
but in fact we need
	FV(T) \ (FV(Env)+)
where the '+' is the oclosure operation.  Notice that we do not 
take FV(T)+.  This puzzled me for a bit.  Consider

	f = E

and suppose e have that E :: C a b => a, and suppose that b is
free in the environment. Then we quantify over 'a' only, giving
the type forall a. C a b => a.  Since a->b but we don't have b->a,
we might have instance decls like
	instance C Bool Int where ...
	instance C Char Int where ...
so knowing that b=Int doesn't fix 'a'; so we quantify over it.

		---------------
		A WORRY: ToDo!
		---------------
If we have	class C a b => D a b where ....
     		class D a b | a -> b where ...
and the preds are [C (x,y) z], then we want to see the fd in D,
even though it is not explicit in C, giving [({x,y},{z})]

Similarly for instance decls?  E.g. Suppose we have
	instance C a b => Eq (T a b) where ...
and we infer a type t with constraints Eq (T a b) for a particular
expression, and suppose that 'a' is free in the environment.  
We could generalise to
	forall b. Eq (T a b) => t
but if we reduced the constraint, to C a b, we'd see that 'a' determines
b, so that a better type might be
	t (with free constraint C a b) 
Perhaps it doesn't matter, because we'll still force b to be a
particular type at the call sites.  Generalising over too many
variables (provided we don't shadow anything by quantifying over a
variable that is actually free in the envt) may postpone errors; it
won't hide them altogether.


\begin{code}
oclose :: [PredType] -> TyVarSet -> TyVarSet
oclose preds fixed_tvs
  | null tv_fds = fixed_tvs	-- Fast escape hatch for common case
  | otherwise   = loop fixed_tvs
  where
    loop fixed_tvs
	| new_fixed_tvs `subVarSet` fixed_tvs = fixed_tvs
	| otherwise		  	      = loop new_fixed_tvs
	where
	  new_fixed_tvs = foldl extend fixed_tvs tv_fds

    extend fixed_tvs (ls,rs) | ls `subVarSet` fixed_tvs = fixed_tvs `unionVarSet` rs
			     | otherwise		= fixed_tvs

    tv_fds  :: [(TyVarSet,TyVarSet)]
	-- In our example, tv_fds will be [ ({x,y}, {z}), ({x,p},{q}) ]
	-- Meaning "knowing x,y fixes z, knowing x,p fixes q"
    tv_fds  = [ (tyVarsOfTypes xs, tyVarsOfTypes ys)
	      | Class cls tys <- preds,		-- Ignore implicit params
		let (cls_tvs, cls_fds) = classTvsFds cls,
		fd <- cls_fds,
		let (xs,ys) = instFD fd cls_tvs tys
	      ]
\end{code}

\begin{code}
grow :: [PredType] -> TyVarSet -> TyVarSet
grow preds fixed_tvs 
  | null pred_sets = fixed_tvs
  | otherwise	   = loop fixed_tvs
  where
    loop fixed_tvs
	| new_fixed_tvs `subVarSet` fixed_tvs = fixed_tvs
	| otherwise		  	      = loop new_fixed_tvs
	where
	  new_fixed_tvs = foldl extend fixed_tvs pred_sets

    extend fixed_tvs pred_tvs 
	| fixed_tvs `intersectsVarSet` pred_tvs = fixed_tvs `unionVarSet` pred_tvs
	| otherwise			        = fixed_tvs

    pred_sets = [tyVarsOfPred pred | pred <- preds]
\end{code}
    
%************************************************************************
%*									*
\subsection{Generate equations from functional dependencies}
%*									*
%************************************************************************


\begin{code}
----------
type Equation = (Type,Type)	-- These two types should be equal
				-- INVARIANT: they aren't already equal

----------
improve :: InstEnv a		-- Gives instances for given class
	-> [PredType]		-- Current constraints
	-> [Equation]		-- Derived equalities that must also hold
				-- (NB the above INVARIANT for type Equation)

type InstEnv a = Class -> [(TyVarSet, [Type], a)]
-- This is a bit clumsy, because InstEnv is really
-- defined in module InstEnv.  However, we don't want
-- to define it (and ClsInstEnv) here because InstEnv
-- is their home.  Nor do we want to make a recursive
-- module group (InstEnv imports stuff from FunDeps).
\end{code}

Given a bunch of predicates that must hold, such as

	C Int t1, C Int t2, C Bool t3, ?x::t4, ?x::t5

improve figures out what extra equations must hold.
For example, if we have

	class C a b | a->b where ...

then improve will return

	[(t1,t2), (t4,t5)]

NOTA BENE:

  * improve does not iterate.  It's possible that when we make
    t1=t2, for example, that will in turn trigger a new equation.
    This would happen if we also had
	C t1 t7, C t2 t8
    If t1=t2, we also get t7=t8.

    improve does *not* do this extra step.  It relies on the caller
    doing so.

  * The equations unify types that are not already equal.  So there
    is no effect iff the result of improve is empty



\begin{code}
improve inst_env preds
  = [ eqn | group <- equivClassesByUniq predTyUnique preds,
	    eqn   <- checkGroup inst_env group ]

----------
checkGroup :: InstEnv a -> [PredType] -> [Equation]
  -- The preds are all for the same class or implicit param

checkGroup inst_env (IParam _ ty : ips)
  = 	-- For implicit parameters, all the types must match
    [(ty, ty') | IParam _ ty' <- ips, ty /= ty']

checkGroup inst_env clss@(Class cls tys : _)
  = 	-- For classes life is more complicated  
   	-- Suppose the class is like
	--	classs C as | (l1 -> r1), (l2 -> r2), ... where ...
	-- Then FOR EACH PAIR (Class c tys1, Class c tys2) in the list clss
	-- we check whether
	--	U l1[tys1/as] = U l2[tys2/as]
	--  (where U is a unifier)
	-- 
	-- If so, we return the pair
	--	U r1[tys1/as] = U l2[tys2/as]
	--
	-- We need to do something very similar comparing each predicate
	-- with relevant instance decls
    pairwise_eqns ++ instance_eqns

  where
    (cls_tvs, cls_fds) = classTvsFds cls
    cls_inst_env       = inst_env cls

	-- NOTE that we iterate over the fds first; they are typically
	-- empty, which aborts the rest of the loop.
    pairwise_eqns :: [(Type,Type)]
    pairwise_eqns	-- This group comes from pairwise comparison
      = [ eqn | fd <- cls_fds,
	      	Class _ tys1 : rest <- tails clss,
	      	Class _ tys2	<- rest,
	      	eqn <- checkClsFD emptyVarSet fd cls_tvs tys1 tys2
	]

    instance_eqns :: [(Type,Type)]
    instance_eqns	-- This group comes from comparing with instance decls
      = [ eqn | fd <- cls_fds,
	    	(qtvs, tys1, _) <- cls_inst_env,
	    	Class _ tys2    <- clss,
	    	eqn <- checkClsFD qtvs fd cls_tvs tys1 tys2
	]


----------
checkClsFD :: TyVarSet 			-- The quantified type variables, which
					-- can be instantiated to make the types match
	   -> FunDep TyVar -> [TyVar] 	-- One functional dependency from the class
	   -> [Type] -> [Type]
	   -> [Equation]

checkClsFD qtvs fd clas_tvs tys1 tys2
-- We use 'unify' even though we are often only matching
-- unifyTyListsX will only bind variables in qtvs, so it's OK!
  = case unifyTyListsX qtvs ls1 ls2 of
	Nothing   -> []
	Just unif -> [(sr1, sr2) | (r1,r2) <- rs1 `zip` rs2,
				   let sr1 = substTy full_unif r1,
				   let sr2 = substTy full_unif r2,
				   sr1 /= sr2]
		  where
		    full_unif = mkSubst emptyInScopeSet unif
			-- No for-alls in sight; hmm
  where
    (ls1, rs1) = instFD fd clas_tvs tys1
    (ls2, rs2) = instFD fd clas_tvs tys2

instFD :: FunDep TyVar -> [TyVar] -> [Type] -> FunDep Type
instFD (ls,rs) tvs tys
  = (map lookup ls, map lookup rs)
  where
    env       = zipVarEnv tvs tys
    lookup tv = lookupVarEnv_NF env tv
\end{code}

\begin{code}
checkInstFDs :: ThetaType -> Class -> [Type] -> Bool
-- Check that functional dependencies are obeyed in an instance decl
-- For example, if we have 
--	class theta => C a b | a -> b
-- 	instance C t1 t2 
-- Then we require fv(t2) `subset` oclose(fv(t1), theta)

checkInstFDs theta clas inst_taus
  = all fundep_ok fds
  where
    (tyvars, fds) = classTvsFds clas
    fundep_ok fd  = tyVarsOfTypes rs `subVarSet` oclose theta (tyVarsOfTypes ls)
		 where
		   (ls,rs) = instFD fd tyvars inst_taus
\end{code}

%************************************************************************
%*									*
\subsection{Miscellaneous}
%*									*
%************************************************************************

\begin{code}
pprFundeps :: Outputable a => [FunDep a] -> SDoc
pprFundeps [] = empty
pprFundeps fds = hsep (ptext SLIT("|") : punctuate comma (map ppr_fd fds))

ppr_fd (us, vs) = hsep [interppSP us, ptext SLIT("->"), interppSP vs]
\end{code}
