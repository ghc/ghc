%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[InstEnv]{Utilities for typechecking instance declarations}

The bits common to TcInstDcls and TcDeriv.

\begin{code}
module InstEnv (
	DFunId, InstEnv,

	emptyInstEnv, extendInstEnv, extendInstEnvList,
	lookupInstEnv, instEnvElts,
	classInstances, simpleDFunClassTyCon, checkFunDeps
    ) where

#include "HsVersions.h"

import Class		( Class, classTvsFds )
import Var		( Id, isTcTyVar )
import VarSet
import Type		( TvSubst )
import TcType		( Type, tcTyConAppTyCon, tcIsTyVarTy,
			  tcSplitDFunTy, tyVarsOfTypes, isExistentialTyVar
			)
import Unify		( tcMatchTys, tcUnifyTys, BindFlag(..) )
import FunDeps		( checkClsFD )
import TyCon		( TyCon )
import Outputable
import UniqFM		( UniqFM, lookupUFM, emptyUFM, addToUFM_C, eltsUFM )
import Id		( idType )
import DynFlags
import Util             ( notNull )
import Maybe		( isJust )
\end{code}


%************************************************************************
%*									*
\subsection{The key types}
%*									*
%************************************************************************

A @ClsInstEnv@ all the instances of that class.  The @Id@ inside a
ClsInstEnv mapping is the dfun for that instance.

If class C maps to a list containing the item ([a,b], [t1,t2,t3], dfun), then

	forall a b, C t1 t2 t3  can be constructed by dfun

or, to put it another way, we have

	instance (...) => C t1 t2 t3,  witnessed by dfun

\begin{code}
type DFunId	= Id
type InstEnv    = UniqFM ClsInstEnv	-- Maps Class to instances for that class

data ClsInstEnv 
  = ClsIE [InstEnvElt]	-- The instances for a particular class, in any order
  	  Bool 		-- True <=> there is an instance of form C a b c
			-- 	If *not* then the common case of looking up
			--	(C a b c) can fail immediately
			-- NB: use tcIsTyVarTy: don't look through newtypes!!
		        		
type InstEnvElt = (TyVarSet, [Type], DFunId)

-- INVARIANTS:
--  * [a,b] must be a superset of the free vars of [t1,t2,t3]
--
--  * The dfun must itself be quantified over [a,b]
--
--  * The template type variables [a,b] are distinct in each item
--	of a ClsInstEnv (so we can safely unify them)

-- Thus, the @ClassInstEnv@ for @Eq@ might contain the following entry:
--	[a] ===> dfun_Eq_List :: forall a. Eq a => Eq [a]
-- The "a" in the pattern must be one of the forall'd variables in
-- the dfun type.


emptyInstEnv :: InstEnv
emptyInstEnv = emptyUFM

instEnvElts :: InstEnv -> [InstEnvElt]
instEnvElts ie = [elt | ClsIE elts _ <- eltsUFM ie, elt <- elts]

classInstances :: (InstEnv,InstEnv) -> Class -> [InstEnvElt]
classInstances (pkg_ie, home_ie) cls 
  = get home_ie ++ get pkg_ie
  where
    get env = case lookupUFM env cls of
		Just (ClsIE insts _) -> insts
		Nothing		     -> []

extendInstEnvList :: InstEnv -> [DFunId] -> InstEnv
extendInstEnvList inst_env dfuns = foldl extendInstEnv inst_env dfuns

extendInstEnv :: InstEnv -> DFunId -> InstEnv
extendInstEnv inst_env dfun_id
  = addToUFM_C add inst_env clas (ClsIE [ins_item] ins_tyvar)
  where
    add (ClsIE cur_insts cur_tyvar) _ = ClsIE (ins_item : cur_insts)
					      (ins_tyvar || cur_tyvar)
    (ins_tvs, _, clas, ins_tys) = tcSplitDFunTy (idType dfun_id)
    ins_tv_set = mkVarSet ins_tvs
    ins_item   = (ins_tv_set, ins_tys, dfun_id)
    ins_tyvar  = all tcIsTyVarTy ins_tys

#ifdef UNUSED
pprInstEnv :: InstEnv -> SDoc
pprInstEnv env
  = vcat [ brackets (pprWithCommas ppr (varSetElems tyvars)) <+> 
	   brackets (pprWithCommas ppr tys) <+> ppr dfun
	 | ClsIE cls_inst_env _ <-  eltsUFM env
	 , (tyvars, tys, dfun) <- cls_inst_env
	 ]
#endif

simpleDFunClassTyCon :: DFunId -> (Class, TyCon)
simpleDFunClassTyCon dfun
  = (clas, tycon)
  where
    (_,_,clas,[ty]) = tcSplitDFunTy (idType dfun)
    tycon  	    = tcTyConAppTyCon ty 
\end{code}		      

%************************************************************************
%*									*
\subsection{Instance environments: InstEnv and ClsInstEnv}
%*									*
%************************************************************************


Notes on overlapping instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In some ClsInstEnvs, overlap is prohibited; that is, no pair of templates unify.

In others, overlap is permitted, but only in such a way that one can make
a unique choice when looking up.  That is, overlap is only permitted if
one template matches the other, or vice versa.  So this is ok:

  [a]  [Int]

but this is not

  (Int,a)  (b,Int)

If overlap is permitted, the list is kept most specific first, so that
the first lookup is the right choice.


For now we just use association lists.

\subsection{Avoiding a problem with overlapping}

Consider this little program:

\begin{pseudocode}
     class C a        where c :: a
     class C a => D a where d :: a

     instance C Int where c = 17
     instance D Int where d = 13

     instance C a => C [a] where c = [c]
     instance ({- C [a], -} D a) => D [a] where d = c

     instance C [Int] where c = [37]

     main = print (d :: [Int])
\end{pseudocode}

What do you think `main' prints  (assuming we have overlapping instances, and
all that turned on)?  Well, the instance for `D' at type `[a]' is defined to
be `c' at the same type, and we've got an instance of `C' at `[Int]', so the
answer is `[37]', right? (the generic `C [a]' instance shouldn't apply because
the `C [Int]' instance is more specific).

Ghc-4.04 gives `[37]', while ghc-4.06 gives `[17]', so 4.06 is wrong.  That
was easy ;-)  Let's just consult hugs for good measure.  Wait - if I use old
hugs (pre-September99), I get `[17]', and stranger yet, if I use hugs98, it
doesn't even compile!  What's going on!?

What hugs complains about is the `D [a]' instance decl.

\begin{pseudocode}
     ERROR "mj.hs" (line 10): Cannot build superclass instance
     *** Instance            : D [a]
     *** Context supplied    : D a
     *** Required superclass : C [a]
\end{pseudocode}

You might wonder what hugs is complaining about.  It's saying that you
need to add `C [a]' to the context of the `D [a]' instance (as appears
in comments).  But there's that `C [a]' instance decl one line above
that says that I can reduce the need for a `C [a]' instance to the
need for a `C a' instance, and in this case, I already have the
necessary `C a' instance (since we have `D a' explicitly in the
context, and `C' is a superclass of `D').

Unfortunately, the above reasoning indicates a premature commitment to the
generic `C [a]' instance.  I.e., it prematurely rules out the more specific
instance `C [Int]'.  This is the mistake that ghc-4.06 makes.  The fix is to
add the context that hugs suggests (uncomment the `C [a]'), effectively
deferring the decision about which instance to use.

Now, interestingly enough, 4.04 has this same bug, but it's covered up
in this case by a little known `optimization' that was disabled in
4.06.  Ghc-4.04 silently inserts any missing superclass context into
an instance declaration.  In this case, it silently inserts the `C
[a]', and everything happens to work out.

(See `basicTypes/MkId:mkDictFunId' for the code in question.  Search for
`Mark Jones', although Mark claims no credit for the `optimization' in
question, and would rather it stopped being called the `Mark Jones
optimization' ;-)

So, what's the fix?  I think hugs has it right.  Here's why.  Let's try
something else out with ghc-4.04.  Let's add the following line:

    d' :: D a => [a]
    d' = c

Everyone raise their hand who thinks that `d :: [Int]' should give a
different answer from `d' :: [Int]'.  Well, in ghc-4.04, it does.  The
`optimization' only applies to instance decls, not to regular
bindings, giving inconsistent behavior.

Old hugs had this same bug.  Here's how we fixed it: like GHC, the
list of instances for a given class is ordered, so that more specific
instances come before more generic ones.  For example, the instance
list for C might contain:
    ..., C Int, ..., C a, ...  
When we go to look for a `C Int' instance we'll get that one first.
But what if we go looking for a `C b' (`b' is unconstrained)?  We'll
pass the `C Int' instance, and keep going.  But if `b' is
unconstrained, then we don't know yet if the more specific instance
will eventually apply.  GHC keeps going, and matches on the generic `C
a'.  The fix is to, at each step, check to see if there's a reverse
match, and if so, abort the search.  This prevents hugs from
prematurely chosing a generic instance when a more specific one
exists.

--Jeff

BUT NOTE [Nov 2001]: we must actually *unify* not reverse-match in
this test.  Suppose the instance envt had
    ..., forall a b. C a a b, ..., forall a b c. C a b c, ...
(still most specific first)
Now suppose we are looking for (C x y Int), where x and y are unconstrained.
	C x y Int  doesn't match the template {a,b} C a a b
but neither does 
	C a a b  match the template {x,y} C x y Int
But still x and y might subsequently be unified so they *do* match.

Simple story: unify, don't match.


%************************************************************************
%*									*
\subsection{Looking up an instance}
%*									*
%************************************************************************

@lookupInstEnv@ looks up in a @InstEnv@, using a one-way match.  Since
the env is kept ordered, the first match must be the only one.  The
thing we are looking up can have an arbitrary "flexi" part.

\begin{code}
lookupInstEnv :: DynFlags
	      -> (InstEnv 	-- External package inst-env
		 ,InstEnv) 	-- Home-package inst-env
	      -> Class -> [Type]		-- What we are looking for
	      -> ([(TvSubst, InstEnvElt)], 	-- Successful matches
		  [Id])				-- These don't match but do unify
	-- The second component of the tuple happens when we look up
	--	Foo [a]
	-- in an InstEnv that has entries for
	--	Foo [Int]
	--	Foo [b]
	-- Then which we choose would depend on the way in which 'a'
	-- is instantiated.  So we report that Foo [b] is a match (mapping b->a)
	-- but Foo [Int] is a unifier.  This gives the caller a better chance of
	-- giving a suitable error messagen

lookupInstEnv dflags (pkg_ie, home_ie) cls tys
  | not (null all_unifs) = (all_matches, all_unifs)	-- This is always an error situation,
							-- so don't attempt to pune the matches
  | otherwise		 = (pruned_matches, [])
  where
    all_tvs       = all tcIsTyVarTy tys
    incoherent_ok = dopt Opt_AllowIncoherentInstances  dflags
    overlap_ok    = dopt Opt_AllowOverlappingInstances dflags
    (home_matches, home_unifs) = lookup_inst_env home_ie cls tys all_tvs
    (pkg_matches,  pkg_unifs)  = lookup_inst_env pkg_ie  cls tys all_tvs
    all_matches = home_matches ++ pkg_matches
    all_unifs | incoherent_ok = []	-- Don't worry about these if incoherent is ok!
	      | otherwise     = home_unifs ++ pkg_unifs

    pruned_matches | overlap_ok = foldr insert_overlapping [] all_matches
		   | otherwise  = all_matches

lookup_inst_env :: InstEnv 			-- The envt
	      	-> Class -> [Type]		-- What we are looking for
		-> Bool 			-- All the [Type] are tyvars
	      	-> ([(TvSubst, InstEnvElt)], 	-- Successful matches
	 	    [Id])			-- These don't match but do unify
lookup_inst_env env key_cls key_tys key_all_tvs
  = case lookupUFM env key_cls of
	Nothing 			    -> ([],[])	-- No instances for this class
	Just (ClsIE insts has_tv_insts)
	  | key_all_tvs && not has_tv_insts -> ([],[])	-- Short cut for common case
		-- The thing we are looking up is of form (C a b c), and
		-- the ClsIE has no instances of that form, so don't bother to search
	  | otherwise -> find insts [] []
  where
    find [] ms us = (ms, us)
    find (item@(tpl_tyvars, tpl, dfun_id) : rest) ms us
      = case tcMatchTys tpl_tyvars tpl key_tys of
	  Just subst -> find rest ((subst,item):ms) us
	  Nothing 
		-- Does not match, so next check whether the things unify
		-- [see notes about overlapping instances above]
	   -> ASSERT2( not (tyVarsOfTypes key_tys `intersectsVarSet` tpl_tyvars),
		       (ppr key_cls <+> ppr key_tys <+> ppr key_all_tvs) $$
		       (ppr dfun_id <+> ppr tpl_tyvars <+> ppr tpl)
		      )
		-- Unification will break badly if the variables overlap
		-- They shouldn't because we allocate separate uniques for them
	      case tcUnifyTys bind_fn tpl key_tys of
	        Just _   -> find rest ms (dfun_id:us)
	        Nothing  -> find rest ms us

    bind_fn tv | isTcTyVar tv && isExistentialTyVar tv = Skolem
	       | otherwise	 		       = BindMe
	-- The key_tys can contain skolem constants, and we can guarantee that those
	-- are never going to be instantiated to anything, so we should not involve
	-- them in the unification test.  Example:
	--	class Foo a where { op :: a -> Int }
	--	instance Foo a => Foo [a] 	-- NB overlap
	--	instance Foo [Int]		-- NB overlap
	-- 	data T = forall a. Foo a => MkT a
	--	f :: T -> Int
	--	f (MkT x) = op [x,x]
	-- The op [x,x] means we need (Foo [a]).  Without the filterVarSet we'd
	-- complain, saying that the choice of instance depended on the instantiation
	-- of 'a'; but of course it isn't *going* to be instantiated.
	--
	-- We do this only for pattern-bound skolems.  For example we reject
	--	g :: forall a => [a] -> Int
	--	g x = op x
	-- on the grounds that the correct instance depends on the instantiation of 'a'

insert_overlapping :: (TvSubst, InstEnvElt) -> [(TvSubst, InstEnvElt)] 
		   -> [(TvSubst, InstEnvElt)]
-- Add a new solution, knocking out strictly less specific ones
insert_overlapping new_item [] = [new_item]
insert_overlapping new_item (item:items)
  | new_beats_old && old_beats_new = item : insert_overlapping new_item items
	-- Duplicate => keep both for error report
  | new_beats_old = insert_overlapping new_item items
	-- Keep new one
  | old_beats_new = item : items
	-- Keep old one
  | otherwise	  = item : insert_overlapping new_item items
	-- Keep both
  where
    new_beats_old = new_item `beats` item
    old_beats_new = item `beats` new_item

    (_, (tvs1, tys1, _)) `beats` (_, (tvs2, tys2, _))
	= isJust (tcMatchTys tvs2 tys2 tys1)	-- A beats B if A is more specific than B	
						-- I.e. if B can be instantiated to match A
\end{code}


%************************************************************************
%*									*
		Functional dependencies
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
checkFunDeps :: (InstEnv, InstEnv) -> DFunId 
	     -> Maybe [DFunId]	-- Nothing  <=> ok
				-- Just dfs <=> conflict with dfs
-- Check wheher adding DFunId would break functional-dependency constraints
checkFunDeps inst_envs dfun
  | null bad_fundeps = Nothing
  | otherwise	     = Just bad_fundeps
  where
    (ins_tvs, _, clas, ins_tys) = tcSplitDFunTy (idType dfun)
    ins_tv_set   = mkVarSet ins_tvs
    cls_inst_env = classInstances inst_envs clas
    bad_fundeps  = badFunDeps cls_inst_env clas ins_tv_set ins_tys

badFunDeps :: [InstEnvElt] -> Class
	   -> TyVarSet -> [Type]	-- Proposed new instance type
	   -> [DFunId]
badFunDeps cls_inst_env clas ins_tv_set ins_tys 
  = [ dfun_id | fd <- fds,
	       (tvs, tys, dfun_id) <- cls_inst_env,
	       notNull (checkClsFD (tvs `unionVarSet` ins_tv_set) fd clas_tvs tys ins_tys)
    ]
  where
    (clas_tvs, fds) = classTvsFds clas
\end{code}
