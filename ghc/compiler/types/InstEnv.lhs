%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[InstEnv]{Utilities for typechecking instance declarations}

The bits common to TcInstDcls and TcDeriv.

\begin{code}
module InstEnv (
	-- Instance environment
	InstEnv, emptyInstEnv, extendInstEnv,
	lookupInstEnv, InstLookupResult(..),
	classInstEnv, classDataCon, simpleDFunClassTyCon
    ) where

#include "HsVersions.h"

import HscTypes		( InstEnv, ClsInstEnv, DFunId )
import Class		( Class )
import Var		( Id )
import VarSet		( unionVarSet, mkVarSet )
import VarEnv		( TyVarSubstEnv )
import Maybes		( MaybeErr(..), returnMaB, failMaB, thenMaB, maybeToBool )
import Name		( getSrcLoc )
import Type		( Type, splitTyConApp_maybe, 
			  splitSigmaTy, splitDFunTy, tyVarsOfTypes
			)
import PprType		( )
import Class		( classTyCon )
import DataCon		( DataCon )
import TyCon		( TyCon, tyConDataCons )
import Outputable
import Unify		( matchTys, unifyTyListsX )
import UniqFM		( lookupWithDefaultUFM, addToUFM, emptyUFM )
import Id		( idType )
import ErrUtils		( Message )
import CmdLineOpts
\end{code}



A tiny function which doesn't belong anywhere else.
It makes a nasty mutual-recursion knot if you put it in Class.

\begin{code}
simpleDFunClassTyCon :: DFunId -> (Class, TyCon)
simpleDFunClassTyCon dfun
  = (clas, tycon)
  where
    (_,_,clas,[ty]) = splitDFunTy (idType dfun)
    tycon   	    = case splitTyConApp_maybe ty of
			Just (tycon,_) -> tycon

classDataCon :: Class -> DataCon
classDataCon clas = case tyConDataCons (classTyCon clas) of
		      (dict_constr:no_more) -> ASSERT( null no_more ) dict_constr 
\end{code}		      

%************************************************************************
%*									*
\subsection{Instance environments: InstEnv and ClsInstEnv}
%*									*
%************************************************************************

The actual type declarations are in HscTypes.

\begin{code}
emptyInstEnv :: InstEnv
emptyInstEnv = emptyUFM

classInstEnv :: InstEnv -> Class -> ClsInstEnv
classInstEnv env cls = lookupWithDefaultUFM env [] cls
\end{code}

A @ClsInstEnv@ lives inside a class, and identifies all the instances
of that class.  The @Id@ inside a ClsInstEnv mapping is the dfun for
that instance.  

If class C maps to a list containing the item ([a,b], [t1,t2,t3], dfun), then

	forall a b, C t1 t2 t3  can be constructed by dfun

or, to put it another way, we have

	instance (...) => C t1 t2 t3,  witnessed by dfun

There is an important consistency constraint in the elements of a ClsInstEnv:

  * [a,b] must be a superset of the free vars of [t1,t2,t3]

  * The dfun must itself be quantified over [a,b]

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


@lookupInstEnv@ looks up in a @InstEnv@, using a one-way match.  Since
the env is kept ordered, the first match must be the only one.  The
thing we are looking up can have an arbitrary "flexi" part.

\begin{code}
lookupInstEnv :: InstEnv 			-- The envt
	      -> Class -> [Type]	-- Key
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

lookupInstEnv env key_cls key_tys
  = find (classInstEnv env key_cls)
  where
    key_vars = tyVarsOfTypes key_tys

    find [] = NoMatch False
    find ((tpl_tyvars, tpl, val) : rest)
      = case matchTys tpl_tyvars tpl key_tys of
	  Nothing                 ->
	    case matchTys key_vars key_tys tpl of
	      Nothing             -> find rest
	      Just (_, _)         -> NoMatch (any_match rest)
	  Just (subst, leftovers) -> ASSERT( null leftovers )
				     FoundInst subst val

    any_match rest = or [ maybeToBool (matchTys tvs tpl key_tys)
		        | (tvs,tpl,_) <- rest
			]
\end{code}

@addToClsInstEnv@ extends a @ClsInstEnv@, checking for overlaps.

A boolean flag controls overlap reporting.

True => overlap is permitted, but only if one template matches the other;
        not if they unify but neither is 

\begin{code}
extendInstEnv :: DynFlags -> InstEnv -> [DFunId] -> (InstEnv, [Message])
  -- Similar, but all we have is the DFuns
extendInstEnv dflags env infos
  = go env [] infos
  where
    go env msgs []	     = (env, msgs)
    go env msgs (dfun:dfuns) = case addToInstEnv dflags env dfun of
				    Succeeded new_env -> go new_env msgs dfuns
				    Failed dfun'      -> go env (msg:msgs) infos
						     where
							 msg = dupInstErr dfun dfun'


dupInstErr dfun1 dfun2
	-- Overlapping/duplicate instances for given class; msg could be more glamourous
  = hang (ptext SLIT("Duplicate or overlapping instance declarations:"))
       2 (ppr_dfun dfun1 $$ ppr_dfun dfun2)
  where
    ppr_dfun dfun = ppr (getSrcLoc dfun) <> colon <+> ppr tau
		  where
		    (_,_,tau) = splitSigmaTy (idType dfun)

addToInstEnv :: DynFlags
             -> InstEnv	-> DFunId
	     -> MaybeErr InstEnv	-- Success...
		         DFunId		-- Failure: Offending overlap

addToInstEnv dflags inst_env dfun_id
  = case insert_into (classInstEnv inst_env clas) of
	Failed stuff 	  -> Failed stuff
	Succeeded new_env -> Succeeded (addToUFM inst_env clas new_env)
	
  where
    (ins_tvs, _, clas, ins_tys) = splitDFunTy (idType dfun_id)

    ins_tv_set = mkVarSet ins_tvs
    ins_item = (ins_tv_set, ins_tys, dfun_id)

    insert_into [] = returnMaB [ins_item]
    insert_into env@(cur_item@(tpl_tvs, tpl_tys, val) : rest)

	-- FAIL if:
	-- (a) they are the same, or
	-- (b) they unify, and any sort of overlap is prohibited,
	-- (c) they unify but neither is more specific than t'other
      |  identical 
      || (unifiable && not (dopt Opt_AllowOverlappingInstances dflags))
      || (unifiable && not (ins_item_more_specific || cur_item_more_specific))
      =  failMaB val

	-- New item is an instance of current item, so drop it here
      | ins_item_more_specific	= returnMaB (ins_item : env)

	-- Otherwise carry on
      | otherwise  = insert_into rest     `thenMaB` \ rest' ->
                     returnMaB (cur_item : rest')
      where
        unifiable = maybeToBool (unifyTyListsX (ins_tv_set `unionVarSet` tpl_tvs) tpl_tys ins_tys)
        ins_item_more_specific = maybeToBool (matchTys tpl_tvs    tpl_tys ins_tys)
        cur_item_more_specific = maybeToBool (matchTys ins_tv_set ins_tys tpl_tys)
	identical = ins_item_more_specific && cur_item_more_specific
\end{code}


