%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Class Instance environments}

\begin{code}
module InstEnv (
	InstEnv, emptyInstEnv,  addToInstEnv, lookupInstEnv
    ) where

#include "HsVersions.h"

import Var		( TyVar, Id )
import VarSet
import VarEnv		( TyVarSubstEnv )
import Type		( Type, tyVarsOfTypes )
import Unify		( unifyTyListsX, matchTys )
import Outputable
import Maybes
\end{code}


%************************************************************************
%*									*
\section{InstEnv}
%*									*
%************************************************************************

\begin{code}
type InstEnv = [(TyVarSet, [Type], Id)]
\end{code}

In some InstEnvs overlap is prohibited; that is, no pair of templates unify.

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

You might wonder what hugs is complaining about.  It's saying that you need to
add `C [a]' to the context of the `D [a]' instance (as appears in comments).
But there's that `C [a]' instance decl one line above that says that I can
reduce the need for a `C [a]' instance to the need for a `C a' instance, and
in this case, I already have the necessary `C a' instance (since we have `D a'
explicitly in the context, and `C' is a superclass of `D').

Unfortunately, the above reasoning indicates a premature commitment to the
generic `C [a]' instance.  I.e., it prematurely rules out the more specific
instance `C [Int]'.  This is the mistake that ghc-4.06 makes.  The fix is to
add the context that hugs suggests (uncomment the `C [a]'), effectively
deferring the decision about which instance to use.

Now, interestingly enough, 4.04 has this same bug, but it's covered up in this
case by a little known `optimization' that was disabled in 4.06.  Ghc-4.04
silently inserts any missing superclass context into an instance declaration.
In this case, it silently inserts the `C [a]', and everything happens to work
out.

(See `basicTypes/MkId:mkDictFunId' for the code in question.  Search for
`Mark Jones', although Mark claims no credit for the `optimization' in
question, and would rather it stopped being called the `Mark Jones
optimization' ;-)

So, what's the fix?  I think hugs has it right.  Here's why.  Let's try
something else out with ghc-4.04.  Let's add the following line:

    d' :: D a => [a]
    d' = c

Everyone raise their hand who thinks that `d :: [Int]' should give a different
answer from `d' :: [Int]'.  Well, in ghc-4.04, it does.  The `optimization'
only applies to instance decls, not to regular bindings, giving inconsistent
behavior.

Old hugs had this same bug.  Here's how we fixed it: like GHC, the list of
instances for a given class is ordered, so that more specific instances come
before more generic ones.  For example, the instance list for C might contain:
    ..., C Int, ..., C a, ...
When we go to look for a `C Int' instance we'll get that one first.  But what
if we go looking for a `C b' (`b' is unconstrained)?  We'll pass the `C Int'
instance, and keep going.  But if `b' is unconstrained, then we don't know yet
if the more specific instance will eventually apply.  GHC keeps going, and
matches on the generic `C a'.  The fix is to, at each step, check to see if
there's a reverse match, and if so, abort the search.  This prevents hugs
from prematurely chosing a generic instance when a more specific one exists.

--Jeff

\begin{code}
emptyInstEnv :: InstEnv
emptyInstEnv = []

isEmptyInstEnv env = null env
\end{code}

@lookupInstEnv@ looks up in a @InstEnv@, using a one-way match.  Since the env is kept
ordered, the first match must be the only one.
The thing we are looking up can have an
arbitrary "flexi" part.

\begin{code}
lookupInstEnv :: SDoc		-- For error report
	      -> InstEnv 	-- The envt
	      -> [Type]		-- Key
	      -> Maybe (TyVarSubstEnv, Id)

lookupInstEnv doc env key
  = find env
  where
    key_vars = tyVarsOfTypes key
    find [] = Nothing
    find ((tpl_tyvars, tpl, val) : rest)
      = case matchTys tpl_tyvars tpl key of
	  Nothing                 ->
	    case matchTys key_vars key tpl of
	      Nothing             -> find rest
	      Just (_, _)         -> Nothing
	  Just (subst, leftovers) -> ASSERT( null leftovers )
				     Just (subst, val)
\end{code}

@addToInstEnv@ extends a @InstEnv@, checking for overlaps.

A boolean flag controls overlap reporting.

True => overlap is permitted, but only if one template matches the other;
        not if they unify but neither is 

\begin{code}
addToInstEnv :: Bool                            -- True <=> overlap permitted
             -> InstEnv				-- Envt
	     -> [TyVar] -> [Type] -> Id		-- New item
	     -> MaybeErr InstEnv 		-- Success...
		         ([Type], Id)		-- Failure: Offending overlap

addToInstEnv overlap_ok env ins_tvs ins_tys value
  = insert env
  where
    ins_tv_set = mkVarSet ins_tvs
    ins_item = (ins_tv_set, ins_tys, value)

    insert [] = returnMaB [ins_item]
    insert env@(cur_item@(tpl_tvs, tpl_tys, val) : rest)

	-- FAIL if:
	-- (a) they are the same, or
	-- (b) they unify, and any sort of overlap is prohibited,
	-- (c) they unify but neither is more specific than t'other
      |  identical 
      || (unifiable && not overlap_ok)
      || (unifiable && not (ins_item_more_specific || cur_item_more_specific))
      =  failMaB (tpl_tys, val)

	-- New item is an instance of current item, so drop it here
      | ins_item_more_specific	= returnMaB (ins_item : env)

	-- Otherwise carry on
      | otherwise  = insert rest     `thenMaB` \ rest' ->
                     returnMaB (cur_item : rest')
      where
        unifiable = maybeToBool (unifyTyListsX (ins_tv_set `unionVarSet` tpl_tvs) tpl_tys ins_tys)
        ins_item_more_specific = maybeToBool (matchTys tpl_tvs    tpl_tys ins_tys)
        cur_item_more_specific = maybeToBool (matchTys ins_tv_set ins_tys tpl_tys)
	identical = ins_item_more_specific && cur_item_more_specific
\end{code}

