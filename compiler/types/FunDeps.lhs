%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 2000
%

FunDeps - functional dependencies

It's better to read it as: "if we know these, then we're going to know these"

\begin{code}
module FunDeps (
 	Equation, pprEquation, 
	oclose, improveFromInstEnv, improveFromAnother,
	checkInstCoverage, checkFunDeps,
	pprFundeps
    ) where

#include "HsVersions.h"

import Name
import Var
import Class
import TcType
import Unify
import InstEnv
import VarSet
import VarEnv
import Outputable
import Util
import FastString

import Data.List	( nubBy )
import Data.Maybe	( isJust )
\end{code}


%************************************************************************
%*									*
\subsection{Close type variables}
%*									*
%************************************************************************

  oclose(vs,C)	The result of extending the set of tyvars vs
		using the functional dependencies from C

  grow(vs,C)	The result of extend the set of tyvars vs
		using all conceivable links from C.

		E.g. vs = {a}, C = {H [a] b, K (b,Int) c, Eq e}
		Then grow(vs,C) = {a,b,c}

		Note that grow(vs,C) `superset` grow(vs,simplify(C))
		That is, simplfication can only shrink the result of grow.

Notice that
   oclose is conservative 	v `elem` oclose(vs,C)
          one way:     		 => v is definitely fixed by vs

   grow is conservative		if v might be fixed by vs 
          the other way:	=> v `elem` grow(vs,C)

----------------------------------------------------------
(oclose preds tvs) closes the set of type variables tvs, 
wrt functional dependencies in preds.  The result is a superset
of the argument set.  For example, if we have
	class C a b | a->b where ...
then
	oclose [C (x,y) z, C (x,p) q] {x,y} = {x,y,z}
because if we know x and y then that fixes z.

oclose is used (only) when generalising a type T; see extensive
notes in TcSimplify.

Note [Important subtlety in oclose]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (oclose (C Int t) {}), where class C a b | a->b
Then, since a->b, 't' is fully determined by Int, and the
uniform thing is to return {t}.

However, consider
	class D a b c | b->c
	f x = e	  -- 'e' generates constraint (D s Int t)
		  -- \x.e has type s->s
Then, if (oclose (D s Int t) {}) = {t}, we'll make the function
monomorphic in 't', thus
	f :: forall s. D s Int t => s -> s

But if this function is never called, 't' will never be instantiated;
the functional dependencies that fix 't' may well be instance decls in
some importing module.  But the top-level defaulting of unconstrained
type variables will fix t=GHC.Prim.Any, and that's simply a bug.

Conclusion: oclose only returns a type variable as "fixed" if it 
depends on at least one type variable in the input fixed_tvs.

Remember, it's always sound for oclose to return a smaller set.
An interesting example is tcfail093, where we get this inferred type:
    class C a b | a->b
    dup :: forall h. (Call (IO Int) h) => () -> Int -> h
This is perhaps a bit silly, because 'h' is fixed by the (IO Int);
previously GHC rejected this saying 'no instance for Call (IO Int) h'.
But it's right on the borderline. If there was an extra, otherwise
uninvolved type variable, like 's' in the type of 'f' above, then
we must accept the function.  So, for now anyway, we accept 'dup' too.

\begin{code}
oclose :: [PredType] -> TyVarSet -> TyVarSet
oclose preds fixed_tvs
  | null tv_fds 	    = fixed_tvs	   -- Fast escape hatch for common case
  | isEmptyVarSet fixed_tvs = emptyVarSet  -- Note [Important subtlety in oclose]
  | otherwise 		    = loop fixed_tvs
  where
    loop fixed_tvs
	| new_fixed_tvs `subVarSet` fixed_tvs = fixed_tvs
	| otherwise		  	      = loop new_fixed_tvs
	where
	  new_fixed_tvs = foldl extend fixed_tvs tv_fds

    extend fixed_tvs (ls,rs) 
	| not (isEmptyVarSet ls)	-- Note [Important subtlety in oclose]
	, ls `subVarSet` fixed_tvs = fixed_tvs `unionVarSet` rs
	| otherwise		   = fixed_tvs

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

    
%************************************************************************
%*									*
\subsection{Generate equations from functional dependencies}
%*									*
%************************************************************************


\begin{code}
type Equation = (TyVarSet, [(Type, Type)])
-- These pairs of types should be equal, for some
-- substitution of the tyvars in the tyvar set
-- INVARIANT: corresponding types aren't already equal

-- It's important that we have a *list* of pairs of types.  Consider
-- 	class C a b c | a -> b c where ...
--	instance C Int x x where ...
-- Then, given the constraint (C Int Bool v) we should improve v to Bool,
-- via the equation ({x}, [(Bool,x), (v,x)])
-- This would not happen if the class had looked like
--	class C a b c | a -> b, a -> c

-- To "execute" the equation, make fresh type variable for each tyvar in the set,
-- instantiate the two types with these fresh variables, and then unify.
--
-- For example, ({a,b}, (a,Int,b), (Int,z,Bool))
-- We unify z with Int, but since a and b are quantified we do nothing to them
-- We usually act on an equation by instantiating the quantified type varaibles
-- to fresh type variables, and then calling the standard unifier.

pprEquation :: Equation -> SDoc
pprEquation (qtvs, pairs) 
  = vcat [ptext (sLit "forall") <+> braces (pprWithCommas ppr (varSetElems qtvs)),
	  nest 2 (vcat [ ppr t1 <+> ptext (sLit "~") <+> ppr t2 | (t1,t2) <- pairs])]
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
type Pred_Loc = (PredType, SDoc)	-- SDoc says where the Pred comes from

improveFromInstEnv :: (Class -> [Instance]) 
                     -> Pred_Loc 
                     -> [(Equation,Pred_Loc,Pred_Loc)]
-- Improvement from top-level instances 
improveFromInstEnv _inst_env pred 
  = improveOne _inst_env pred []        -- TODO: Refactor to directly use instance_eqnd? 


improveFromAnother :: Pred_Loc
                   -> Pred_Loc
                   -> [(Equation, Pred_Loc, Pred_Loc)]
-- Improvement from another local (given or wanted) constraint
improveFromAnother pred1 pred2 
  = improveOne (\_ -> []) pred1 [pred2] -- TODO: Refactor to directly use pairwise_eqns?


improveOne :: (Class -> [Instance])		-- Gives instances for given class
	   -> Pred_Loc				-- Do improvement triggered by this
	   -> [Pred_Loc]			-- Current constraints 
	   -> [(Equation,Pred_Loc,Pred_Loc)]	-- Derived equalities that must also hold
						-- (NB the above INVARIANT for type Equation)
						-- The Pred_Locs explain which two predicates were
						-- combined (for error messages)
-- Just do improvement triggered by a single, distinguised predicate

improveOne _inst_env pred@(IParam ip ty, _) preds
  = [ ((emptyVarSet, [(ty,ty2)]), pred, p2) 
    | p2@(IParam ip2 ty2, _) <- preds
    , ip==ip2
    , not (ty `tcEqType` ty2)]

improveOne inst_env pred@(ClassP cls tys, _) preds
  | tys `lengthAtLeast` 2
  = instance_eqns ++ pairwise_eqns
	-- NB: we put the instance equations first.   This biases the 
	-- order so that we first improve individual constraints against the
	-- instances (which are perhaps in a library and less likely to be
	-- wrong; and THEN perform the pairwise checks.
	-- The other way round, it's possible for the pairwise check to succeed
	-- and cause a subsequent, misleading failure of one of the pair with an
	-- instance declaration.  See tcfail143.hs for an example
  where
    (cls_tvs, cls_fds) = classTvsFds cls
    instances	       = inst_env cls
    rough_tcs 	       = roughMatchTcs tys

	-- NOTE that we iterate over the fds first; they are typically
	-- empty, which aborts the rest of the loop.
    pairwise_eqns :: [(Equation,Pred_Loc,Pred_Loc)]
    pairwise_eqns	-- This group comes from pairwise comparison
      = [ (eqn, pred, p2)
	| fd <- cls_fds
	, p2@(ClassP cls2 tys2, _) <- preds
	, cls == cls2
	, eqn <- checkClsFD emptyVarSet fd cls_tvs tys tys2
	]

    instance_eqns :: [(Equation,Pred_Loc,Pred_Loc)]
    instance_eqns	-- This group comes from comparing with instance decls
      = [ (eqn, p_inst, pred)
	| fd <- cls_fds		-- Iterate through the fundeps first, 
				-- because there often are none!
	, let trimmed_tcs = trimRoughMatchTcs cls_tvs fd rough_tcs
		-- Trim the rough_tcs based on the head of the fundep.
		-- Remember that instanceCantMatch treats both argumnents
		-- symmetrically, so it's ok to trim the rough_tcs,
		-- rather than trimming each inst_tcs in turn
	, ispec@(Instance { is_tvs = qtvs, is_tys = tys_inst, 
		 	    is_tcs = inst_tcs }) <- instances
	, not (instanceCantMatch inst_tcs trimmed_tcs)
	, eqn <- checkClsFD qtvs fd cls_tvs tys_inst tys
	, let p_inst = (mkClassPred cls tys_inst, 
		        sep [ ptext (sLit "arising from the dependency") <+> quotes (pprFunDep fd)
			    , ptext (sLit "in the instance declaration at") 
			          <+> ppr (getSrcLoc ispec)])
	]

improveOne _ _ _
  = []


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
-- 	(Tree x) [t1/x]  ~   t2
--
-- This function is also used when matching two Insts (rather than an Inst
-- against an instance decl. In that case, qtvs is empty, and we are doing
-- an equality check
-- 
-- This function is also used by InstEnv.badFunDeps, which needs to *unify*
-- For the one-sided matching case, the qtvs are just from the template,
-- so we get matching
--
  = ASSERT2( length tys1 == length tys2     && 
	     length tys1 == length clas_tvs 
	    , ppr tys1 <+> ppr tys2 )

    case tcUnifyTys bind_fn ls1 ls2 of
	Nothing  -> []
	Just subst | isJust (tcUnifyTys bind_fn rs1' rs2') 
			-- Don't include any equations that already hold. 
			-- Reason: then we know if any actual improvement has happened,
			-- 	   in which case we need to iterate the solver
			-- In making this check we must taking account of the fact that any 
			-- qtvs that aren't already instantiated can be instantiated to anything 
			-- at all
		  -> []

		  | otherwise	-- Aha!  A useful equation
		  -> [ (qtvs', zip rs1' rs2')]
		  	-- We could avoid this substTy stuff by producing the eqn
		  	-- (qtvs, ls1++rs1, ls2++rs2)
		  	-- which will re-do the ls1/ls2 unification when the equation is
		  	-- executed.  What we're doing instead is recording the partial
		  	-- work of the ls1/ls2 unification leaving a smaller unification problem
		  where
		    rs1'  = substTys subst rs1 
	 	    rs2'  = substTys subst rs2
		    qtvs' = filterVarSet (`notElemTvSubst` subst) qtvs
			-- qtvs' are the quantified type variables
			-- that have not been substituted out
			--	
			-- Eg. 	class C a b | a -> b
			--	instance C Int [y]
			-- Given constraint C Int z
			-- we generate the equation
			--	({y}, [y], z)
  where
    bind_fn tv | tv `elemVarSet` qtvs = BindMe
	       | otherwise	      = Skolem

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
checkInstCoverage :: Class -> [Type] -> Bool
-- Check that the Coverage Condition is obeyed in an instance decl
-- For example, if we have 
--	class theta => C a b | a -> b
-- 	instance C t1 t2 
-- Then we require fv(t2) `subset` fv(t1)
-- See Note [Coverage Condition] below

checkInstCoverage clas inst_taus
  = all fundep_ok fds
  where
    (tyvars, fds) = classTvsFds clas
    fundep_ok fd  = tyVarsOfTypes rs `subVarSet` tyVarsOfTypes ls
		 where
		   (ls,rs) = instFD fd tyvars inst_taus
\end{code}

Note [Coverage condition]
~~~~~~~~~~~~~~~~~~~~~~~~~
For the coverage condition, we used to require only that 
	fv(t2) `subset` oclose(fv(t1), theta)

Example:
	class Mul a b c | a b -> c where
		(.*.) :: a -> b -> c

	instance Mul Int Int Int where (.*.) = (*)
	instance Mul Int Float Float where x .*. y = fromIntegral x * y
	instance Mul a b c => Mul a [b] [c] where x .*. v = map (x.*.) v

In the third instance, it's not the case that fv([c]) `subset` fv(a,[b]).
But it is the case that fv([c]) `subset` oclose( theta, fv(a,[b]) )

But it is a mistake to accept the instance because then this defn:
	f = \ b x y -> if b then x .*. [y] else y
makes instance inference go into a loop, because it requires the constraint
	Mul a [b] b


%************************************************************************
%*									*
	Check that a new instance decl is OK wrt fundeps
%*									*
%************************************************************************

Here is the bad case:
	class C a b | a->b where ...
	instance C Int Bool where ...
	instance C Int Char where ...

The point is that a->b, so Int in the first parameter must uniquely
determine the second.  In general, given the same class decl, and given

	instance C s1 s2 where ...
	instance C t1 t2 where ...

Then the criterion is: if U=unify(s1,t1) then U(s2) = U(t2).

Matters are a little more complicated if there are free variables in
the s2/t2.  

	class D a b c | a -> b
	instance D a b => D [(a,a)] [b] Int
	instance D a b => D [a]     [b] Bool

The instance decls don't overlap, because the third parameter keeps
them separate.  But we want to make sure that given any constraint
	D s1 s2 s3
if s1 matches 


\begin{code}
checkFunDeps :: (InstEnv, InstEnv) -> Instance
	     -> Maybe [Instance]	-- Nothing  <=> ok
					-- Just dfs <=> conflict with dfs
-- Check wheher adding DFunId would break functional-dependency constraints
-- Used only for instance decls defined in the module being compiled
checkFunDeps inst_envs ispec
  | null bad_fundeps = Nothing
  | otherwise	     = Just bad_fundeps
  where
    (ins_tvs, _, clas, ins_tys) = instanceHead ispec
    ins_tv_set   = mkVarSet ins_tvs
    cls_inst_env = classInstances inst_envs clas
    bad_fundeps  = badFunDeps cls_inst_env clas ins_tv_set ins_tys

badFunDeps :: [Instance] -> Class
	   -> TyVarSet -> [Type]	-- Proposed new instance type
	   -> [Instance]
badFunDeps cls_insts clas ins_tv_set ins_tys 
  = nubBy eq_inst $
    [ ispec | fd <- fds,	-- fds is often empty, so do this first!
	      let trimmed_tcs = trimRoughMatchTcs clas_tvs fd rough_tcs,
	      ispec@(Instance { is_tcs = inst_tcs, is_tvs = tvs, 
				is_tys = tys }) <- cls_insts,
		-- Filter out ones that can't possibly match, 
		-- based on the head of the fundep
	      not (instanceCantMatch inst_tcs trimmed_tcs),	
	      notNull (checkClsFD (tvs `unionVarSet` ins_tv_set) 
				   fd clas_tvs tys ins_tys)
    ]
  where
    (clas_tvs, fds) = classTvsFds clas
    rough_tcs = roughMatchTcs ins_tys
    eq_inst i1 i2 = instanceDFunId i1 == instanceDFunId i2
	-- An single instance may appear twice in the un-nubbed conflict list
	-- because it may conflict with more than one fundep.  E.g.
	--	class C a b c | a -> b, a -> c
	--	instance C Int Bool Bool
	--	instance C Int Char Char
	-- The second instance conflicts with the first by *both* fundeps

trimRoughMatchTcs :: [TyVar] -> FunDep TyVar -> [Maybe Name] -> [Maybe Name]
-- Computing rough_tcs for a particular fundep
--     class C a b c | a -> b where ...
-- For each instance .... => C ta tb tc
-- we want to match only on the type ta; so our
-- rough-match thing must similarly be filtered.  
-- Hence, we Nothing-ise the tb and tc types right here
trimRoughMatchTcs clas_tvs (ltvs, _) mb_tcs
  = zipWith select clas_tvs mb_tcs
  where
    select clas_tv mb_tc | clas_tv `elem` ltvs = mb_tc
                         | otherwise           = Nothing
\end{code}



