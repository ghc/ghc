%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[InstEnv]{Utilities for typechecking instance declarations}

The bits common to TcInstDcls and TcDeriv.

\begin{code}
module InstEnv (
	DFunId, ClsInstEnv, InstEnv,

	emptyInstEnv, extendInstEnv, pprInstEnv,
	lookupInstEnv, InstLookupResult(..),
	classInstEnv, simpleDFunClassTyCon
    ) where

#include "HsVersions.h"

import Class		( Class, classTvsFds )
import Var		( TyVar, Id )
import VarSet
import VarEnv
import Maybes		( MaybeErr(..), returnMaB, failMaB, thenMaB, maybeToBool )
import Name		( getSrcLoc, nameModule )
import SrcLoc		( SrcLoc, isGoodSrcLoc )
import TcType		( Type, tcTyConAppTyCon, mkTyVarTy,
			  tcSplitDFunTy, tyVarsOfTypes,
			  matchTys, unifyTyListsX, allDistinctTyVars
			)
import PprType		( pprClassPred )
import FunDeps		( checkClsFD )
import TyCon		( TyCon )
import Outputable
import UniqFM		( UniqFM, lookupWithDefaultUFM, addToUFM, emptyUFM, eltsUFM )
import Id		( idType, idName )
import ErrUtils		( Message )
import CmdLineOpts
import Util             ( notNull )
\end{code}


%************************************************************************
%*									*
\subsection{The key types}
%*									*
%************************************************************************

\begin{code}
type DFunId	= Id

type InstEnv    = UniqFM ClsInstEnv		-- Maps Class to instances for that class

simpleDFunClassTyCon :: DFunId -> (Class, TyCon)
simpleDFunClassTyCon dfun
  = (clas, tycon)
  where
    (_,_,clas,[ty]) = tcSplitDFunTy (idType dfun)
    tycon  	    = tcTyConAppTyCon ty 

pprInstEnv :: InstEnv -> SDoc
pprInstEnv env
  = vcat [ brackets (pprWithCommas ppr (varSetElems tyvars)) <+> 
	   brackets (pprWithCommas ppr tys) <+> ppr dfun
	 | cls_inst_env <-  eltsUFM env
	 , (tyvars, tys, dfun) <- cls_inst_env
	 ]
\end{code}		      

%************************************************************************
%*									*
\subsection{Instance environments: InstEnv and ClsInstEnv}
%*									*
%************************************************************************

\begin{code}
type ClsInstEnv = [(TyVarSet, [Type], DFunId)]	-- The instances for a particular class
	-- INVARIANTs: see notes below

emptyInstEnv :: InstEnv
emptyInstEnv = emptyUFM

classInstEnv :: InstEnv -> Class -> ClsInstEnv
classInstEnv env cls = lookupWithDefaultUFM env [] cls
\end{code}

A @ClsInstEnv@ all the instances of that class.  The @Id@ inside a
ClsInstEnv mapping is the dfun for that instance.

If class C maps to a list containing the item ([a,b], [t1,t2,t3], dfun), then

	forall a b, C t1 t2 t3  can be constructed by dfun

or, to put it another way, we have

	instance (...) => C t1 t2 t3,  witnessed by dfun

There is an important consistency constraint in the elements of a ClsInstEnv:

  * [a,b] must be a superset of the free vars of [t1,t2,t3]

  * The dfun must itself be quantified over [a,b]
 
  * More specific instances come before less specific ones,
    where they overlap

Thus, the @ClassInstEnv@ for @Eq@ might contain the following entry:
	[a] ===> dfun_Eq_List :: forall a. Eq a => Eq [a]
The "a" in the pattern must be one of the forall'd variables in
the dfun type.



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
	      -> InstEnv 		-- The envt
	      -> Class -> [Type]	-- What we are looking for
	      -> InstLookupResult

data InstLookupResult 
  = FoundInst 			-- There is a (template,substitution) pair 
				-- that makes the template match the key, 
				-- and no template is an instance of the key
	TyVarSubstEnv Id

  | NoMatch Bool	-- Boolean is true iff there is at least one
			-- template that matches the key.
			-- (but there are other template(s) that are
			--  instances of the key, so we don't report 
			--  FoundInst)
	-- The NoMatch True case happens when we look up
	--	Foo [a]
	-- in an InstEnv that has entries for
	--	Foo [Int]
	--	Foo [b]
	-- Then which we choose would depend on the way in which 'a'
	-- is instantiated.  So we say there is no match, but identify
	-- it as ambiguous case in the hope of giving a better error msg.
	-- See the notes above from Jeff Lewis

lookupInstEnv dflags env key_cls key_tys
  = find (classInstEnv env key_cls)
  where
    key_vars = tyVarsOfTypes key_tys

    find [] = NoMatch False
    find ((tpl_tyvars, tpl, dfun_id) : rest)
      = case matchTys tpl_tyvars tpl key_tys of
	  Nothing                 ->
		-- Check whether the things unify, so that
		-- we bale out if a later instantiation of this
		-- predicate might match this instance
		-- [see notes about overlapping instances above]
	    case unifyTyListsX (key_vars `unionVarSet` tpl_tyvars) key_tys tpl of
	      Just _ | not (dopt Opt_AllowIncoherentInstances dflags)
		     -> NoMatch (any_match rest)
		-- If we allow incoherent instances we don't worry about the 
		-- test and just blaze on anyhow.  Requested by John Hughes.
	      other  -> find rest

	  Just (subst, leftovers) -> ASSERT( null leftovers )
				     FoundInst subst dfun_id

    any_match rest = or [ maybeToBool (matchTys tvs tpl key_tys)
		        | (tvs,tpl,_) <- rest
			]
\end{code}


%************************************************************************
%*									*
\subsection{Extending an instance environment}
%*									*
%************************************************************************

@extendInstEnv@ extends a @ClsInstEnv@, checking for overlaps.

A boolean flag controls overlap reporting.

True => overlap is permitted, but only if one template matches the other;
        not if they unify but neither is 

\begin{code}
extendInstEnv :: DynFlags -> InstEnv -> [DFunId] -> (InstEnv, [(SrcLoc,Message)])
  -- Similar, but all we have is the DFuns
extendInstEnv dflags env dfun_ids = foldl (addToInstEnv dflags) (env, []) dfun_ids


addToInstEnv :: DynFlags
             -> (InstEnv, [(SrcLoc,Message)])
	     -> DFunId
	     -> (InstEnv, [(SrcLoc,Message)])	-- Resulting InstEnv and augmented error messages

addToInstEnv dflags (inst_env, errs) dfun_id
	-- Check first that the new instance doesn't 
	-- conflict with another.  See notes below about fundeps.
  | notNull bad_fundeps
  = (inst_env, fundep_err : errs)		-- Bad fundeps; report the first only

  | otherwise
  = case insert_into cls_inst_env of 
	Failed err	  -> (inst_env, err : errs)
	Succeeded new_env -> (addToUFM inst_env clas new_env, errs)

  where
    cls_inst_env = classInstEnv inst_env clas
    (ins_tvs, _, clas, ins_tys) = tcSplitDFunTy (idType dfun_id)
    bad_fundeps = badFunDeps cls_inst_env clas ins_tv_set ins_tys
    fundep_err  = fundepErr dfun_id (head bad_fundeps)

    ins_tv_set = mkVarSet ins_tvs
    ins_item   = (ins_tv_set, ins_tys, dfun_id)

    insert_into [] = returnMaB [ins_item]
    insert_into env@(cur_item@(tpl_tvs, tpl_tys, tpl_dfun_id) : rest)
      = case unifyTyListsX (ins_tv_set `unionVarSet` tpl_tvs) tpl_tys ins_tys of
	  Just subst -> insert_unifiable env subst
	  Nothing    -> carry_on cur_item rest

    carry_on cur_item rest = insert_into rest     `thenMaB` \ rest' ->
			     returnMaB (cur_item : rest')

	    -- The two templates unify.  This is acceptable iff
	    -- (a) -fallow-overlapping-instances is on
	    -- (b) one is strictly more specific than the other
	    -- [It's bad if they are identical or incomparable]
    insert_unifiable env@(cur_item@(tpl_tvs, tpl_tys, tpl_dfun_id) : rest) subst
      |  ins_item_more_specific && cur_item_more_specific
      = 	-- Duplicates
	failMaB (dupInstErr dfun_id tpl_dfun_id)

      |  not (dopt Opt_AllowOverlappingInstances dflags)
      || not (ins_item_more_specific || cur_item_more_specific)
      =  	-- Overlap illegal, or the two are incomparable
	 failMaB (overlapErr dfun_id tpl_dfun_id)
	 
      | otherwise
      = 	-- OK, it's acceptable.  Remaining question is whether
		-- we drop it here or compare it with others
	if ins_item_more_specific then
		-- New item is an instance of current item, so drop it here
	    returnMaB (ins_item : env)
	else
	    carry_on cur_item rest

      where
	ins_item_more_specific = allVars subst ins_tvs
      	cur_item_more_specific = allVars subst (varSetElems tpl_tvs)

allVars :: TyVarSubstEnv -> [TyVar] -> Bool
-- True iff all the type vars are mapped to distinct type vars
allVars subst tvs
  = allDistinctTyVars (map lookup tvs) emptyVarSet
  where
    lookup tv = case lookupSubstEnv subst tv of
		  Just (DoneTy ty) -> ty
		  Nothing	   -> mkTyVarTy tv
\end{code}

Functional dependencies
~~~~~~~~~~~~~~~~~~~~~~~
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
badFunDeps :: ClsInstEnv -> Class
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


\begin{code}
dupInstErr dfun1 dfun2 = addInstErr (ptext SLIT("Duplicate instance declarations:"))  dfun1 dfun2
overlapErr dfun1 dfun2 = addInstErr (ptext SLIT("Overlapping instance declarations:")) dfun1 dfun2
fundepErr  dfun1 dfun2 = addInstErr (ptext SLIT("Functional dependencies conflict between instance declarations:")) 
				    dfun1 dfun2

addInstErr :: SDoc -> DFunId -> DFunId -> (SrcLoc, Message)
addInstErr what dfun1 dfun2 
 = (getSrcLoc dfun1, hang what 2 (ppr_dfun dfun1 $$ ppr_dfun dfun2))
  where
   
    ppr_dfun dfun = pp_loc <> colon <+> pprClassPred clas tys
      where
	(_,_,clas,tys) = tcSplitDFunTy (idType dfun)
	loc = getSrcLoc dfun
	mod = nameModule (idName dfun)
	
	-- Worth trying to print a good location... imported dfuns
	-- don't have a useful SrcLoc but we can say which module they come from
	pp_loc | isGoodSrcLoc loc = ppr loc
	       | otherwise	  = ptext SLIT("In module") <+> ppr mod
\end{code}
