%
% (c) The GRASP/AQUA Project, Glasgow University, 2000
%
\section[FunDeps]{FunDeps - functional dependencies}

It's better to read it as: "if we know these, then we're going to know these"

\begin{code}
module FunDeps (
 	Equation, pprEquation, pprEquationDoc,
	oclose, grow, improve, checkInstFDs, checkClsFD, pprFundeps
    ) where

#include "HsVersions.h"

import Name		( getSrcLoc )
import Var		( Id, TyVar )
import Class		( Class, FunDep, classTvsFds )
import Subst		( mkSubst, emptyInScopeSet, substTy )
import TcType		( Type, ThetaType, SourceType(..), PredType,
			  predTyUnique, mkClassPred, tyVarsOfTypes, tyVarsOfPred,
			  unifyTyListsX, unifyExtendTysX, tcEqType
			)
import PprType		(  )
import VarSet
import VarEnv
import Outputable
import List		( tails )
import Maybes		( maybeToBool )
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
	      | ClassP cls tys <- preds,		-- Ignore implicit params
		let (cls_tvs, cls_fds) = classTvsFds cls,
		fd <- cls_fds,
		let (xs,ys) = instFD fd cls_tvs tys
	      ]
\end{code}

\begin{code}
grow :: [PredType] -> TyVarSet -> TyVarSet
grow preds fixed_tvs 
  | null preds = fixed_tvs
  | otherwise  = loop fixed_tvs
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
type Equation = (TyVarSet, Type, Type)	-- These two types should be equal, for some
					-- substitution of the tyvars in the tyvar set
	-- To "execute" the equation, make fresh type variable for each tyvar in the set,
	-- instantiate the two types with these fresh variables, and then unify.
	--
	-- For example, ({a,b}, (a,Int,b), (Int,z,Bool))
	-- We unify z with Int, but since a and b are quantified we do nothing to them
	-- We usually act on an equation by instantiating the quantified type varaibles
	-- to fresh type variables, and then calling the standard unifier.
	-- 
	-- INVARIANT: they aren't already equal
	--


pprEquationDoc (eqn, doc) = vcat [pprEquation eqn, nest 2 doc]

pprEquation (qtvs, t1, t2) = ptext SLIT("forall") <+> braces (pprWithCommas ppr (varSetElems qtvs))
			     			  <+> ppr t1 <+> ptext SLIT(":=:") <+> ppr t2

----------
improve :: InstEnv Id		-- Gives instances for given class
	-> [(PredType,SDoc)]	-- Current constraints; doc says where they come from
	-> [(Equation,SDoc)]	-- Derived equalities that must also hold
				-- (NB the above INVARIANT for type Equation)
				-- The SDoc explains why the equation holds (for error messages)

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
  = [ eqn | group <- equivClassesByUniq (predTyUnique . fst) preds,
	    eqn   <- checkGroup inst_env group ]

----------
checkGroup :: InstEnv Id -> [(PredType,SDoc)] -> [(Equation, SDoc)]
  -- The preds are all for the same class or implicit param

checkGroup inst_env (p1@(IParam _ ty, _) : ips)
  = 	-- For implicit parameters, all the types must match
    [ ((emptyVarSet, ty, ty'), mkEqnMsg p1 p2) 
    | p2@(IParam _ ty', _) <- ips, not (ty `tcEqType` ty')]

checkGroup inst_env clss@((ClassP cls _, _) : _)
  = 	-- For classes life is more complicated  
   	-- Suppose the class is like
	--	classs C as | (l1 -> r1), (l2 -> r2), ... where ...
	-- Then FOR EACH PAIR (ClassP c tys1, ClassP c tys2) in the list clss
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
    pairwise_eqns :: [(Equation,SDoc)]
    pairwise_eqns	-- This group comes from pairwise comparison
      = [ (eqn, mkEqnMsg p1 p2)
	| fd <- cls_fds,
	  p1@(ClassP _ tys1, _) : rest <- tails clss,
	  p2@(ClassP _ tys2, _)	<- rest,
	  eqn <- checkClsFD emptyVarSet fd cls_tvs tys1 tys2
	]

    instance_eqns :: [(Equation,SDoc)]
    instance_eqns	-- This group comes from comparing with instance decls
      = [ (eqn, mkEqnMsg p1 p2)
	| fd <- cls_fds,
	  (qtvs, tys1, dfun_id)  <- cls_inst_env,
	  let p1 = (mkClassPred cls tys1, 
		    ptext SLIT("arising from the instance declaration at") <+> ppr (getSrcLoc dfun_id)),
	  p2@(ClassP _ tys2, _) <- clss,
	  eqn <- checkClsFD qtvs fd cls_tvs tys1 tys2
	]

mkEqnMsg (pred1,from1) (pred2,from2)
  = vcat [ptext SLIT("When using functional dependencies to combine"),
	  nest 2 (sep [ppr pred1 <> comma, nest 2 from1]), 
	  nest 2 (sep [ppr pred2 <> comma, nest 2 from2])]
 
----------
checkClsFD :: TyVarSet 			-- Quantified type variables; see note below
	   -> FunDep TyVar -> [TyVar] 	-- One functional dependency from the class
	   -> [Type] -> [Type]
	   -> [Equation]

checkClsFD qtvs fd clas_tvs tys1 tys2
-- 'qtvs' are the quantified type variables, the ones which an be instantiated 
-- to make the types match.  For example, given
--	class C a b | a->b where ...
--	instance C (Maybe x) (Tree x) where ..
--
-- and an Inst of form (C (Maybe t1) t2), 
-- then we will call checkClsFD with
--
--	qtvs = {x}, tys1 = [Maybe x,  Tree x]
--		    tys2 = [Maybe t1, t2]
--
-- We can instantiate x to t1, and then we want to force
-- 	(Tree x) [t1/x]  :=:   t2

-- We use 'unify' even though we are often only matching
-- unifyTyListsX will only bind variables in qtvs, so it's OK!
  = case unifyTyListsX qtvs ls1 ls2 of
	Nothing   -> []
	Just unif -> -- pprTrace "checkFD" (vcat [ppr_fd fd,
		     --			       ppr (varSetElems qtvs) <+> (ppr ls1 $$ ppr ls2),
		     --			       ppr unif]) $ 
		     [ (qtvs', substTy full_unif r1, substTy full_unif r2)
		     | (r1,r2) <- rs1 `zip` rs2,
		       not (maybeToBool (unifyExtendTysX qtvs unif r1 r2))]
			-- Don't include any equations that already hold
			-- taking account of the fact that any qtvs that aren't 
			-- already instantiated can be instantiated to anything at all
			-- NB: qtvs, not qtvs' because unifyExtendTysX only tries to
			--     look template tyvars up in the substitution
		  where
		    full_unif = mkSubst emptyInScopeSet unif
			-- No for-alls in sight; hmm

		    qtvs' = filterVarSet (\v -> not (v `elemSubstEnv` unif)) qtvs
			-- qtvs' are the quantified type variables
			-- that have not been substituted out
			--	
			-- Eg. 	class C a b | a -> b
			--	instance C Int [y]
			-- Given constraint C Int z
			-- we generate the equation
			--	({y}, [y], z)
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
