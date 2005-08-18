%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[InstEnv]{Utilities for typechecking instance declarations}

The bits common to TcInstDcls and TcDeriv.

\begin{code}
module InstEnv (
	DFunId, OverlapFlag(..),
	Instance(..), pprInstance, pprInstanceHdr, pprInstances, 
	instanceHead, mkLocalInstance, mkImportedInstance,
	instanceDFunId, setInstanceDFunId, instanceRoughTcs,

	InstEnv, emptyInstEnv, extendInstEnv, 
	extendInstEnvList, lookupInstEnv, instEnvElts,
	classInstances, 
	instanceCantMatch, roughMatchTcs
    ) where

#include "HsVersions.h"

import Class		( Class )
import Var		( Id, TyVar, isTcTyVar )
import VarSet
import Name		( Name, NamedThing(..), getSrcLoc, nameIsLocalOrFrom, nameModule )
import OccName		( OccName )
import NameSet		( unionNameSets, unitNameSet, nameSetToList )
import Type		( TvSubst )
import TcType		( Type, PredType, tcEqType,
			  tcSplitDFunTy, tyVarsOfTypes, isExistentialTyVar,
			  pprThetaArrow, pprClassPred,
			  tyClsNamesOfType, tcSplitTyConApp_maybe
			)
import TyCon		( tyConName )
import Unify		( tcMatchTys, tcUnifyTys, BindFlag(..) )
import Outputable
import UniqFM		( UniqFM, lookupUFM, emptyUFM, addToUFM_C, eltsUFM )
import Id		( idType, idName )
import SrcLoc		( pprDefnLoc )
import Maybe		( isJust, isNothing )
\end{code}


%************************************************************************
%*									*
\subsection{The key types}
%*									*
%************************************************************************

\begin{code}
type DFunId = Id
data Instance 
  = Instance { is_cls  :: Name		-- Class name
	
		-- Used for "rough matching"; see note below
	     , is_tcs  :: [Maybe Name]	-- Top of type args

		-- Used for "proper matching"; see note
	     , is_tvs  :: TyVarSet	-- Template tyvars for full match
	     , is_tys  :: [Type]	-- Full arg types

	     , is_dfun :: DFunId
	     , is_flag :: OverlapFlag

	     , is_orph :: Maybe OccName }

-- The "rough-match" fields
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- The is_cls, is_args fields allow a "rough match" to be done
-- without poking inside the DFunId.  Poking the DFunId forces
-- us to suck in all the type constructors etc it involves,
-- which is a total waste of time if it has no chance of matching
-- So the Name, [Maybe Name] fields allow us to say "definitely
-- does not match", based only on the Name.
--
-- In is_tcs, 
--     Nothing  means that this type arg is a type variable
--
--     (Just n) means that this type arg is a
-- 		TyConApp with a type constructor of n.
--		This is always a real tycon, never a synonym!
--		(Two different synonyms might match, but two
--		different real tycons can't.)
--		NB: newtypes are not transparent, though!
--
-- The "proper-match" fields
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- The is_tvs, is_tys fields are simply cahced values, pulled
-- out (lazily) from the dfun id. They are cached here simply so 
-- that we don't need to decompose the DFunId each time we want 
-- to match it.  The hope is that the fast-match fields mean
-- that we often never poke th proper-match fields
--
-- However, note that:
--  * is_tvs must be a superset of the free vars of is_tys
--
--  * The is_dfun must itself be quantified over exactly is_tvs
--    (This is so that we can use the matching substitution to
--     instantiate the dfun's context.)
--
-- The "orphan" field
-- ~~~~~~~~~~~~~~~~~~
-- An instance is an orphan if its head (after the =>) mentions
-- nothing defined in this module.  
--
--    Just n	The head mentions n, which is defined in this module
--		This is used for versioning; the instance decl is
--		considered part of the defn of n when computing versions
--
--    Nothing 	The head mentions nothing defined in this modle
--
-- If a module contains any orphans, then its interface file is read 
-- regardless, so that its instances are not missed. 
-- 
-- Functional dependencies worsen the situation a bit. Consider
--	class C a b | a -> b
-- In some other module we might have
--    module M where
--	data T = ...
--	instance C Int T where ...
-- This isn't considered an orphan, so we will only read M's interface
-- if something from M is used (e.g. T).  So there's a risk we'll
-- miss the improvement from the instance.  Workaround: import M.

instanceDFunId :: Instance -> DFunId
instanceDFunId = is_dfun

setInstanceDFunId :: Instance -> DFunId -> Instance
setInstanceDFunId ispec dfun
   = ASSERT( idType dfun `tcEqType` idType (is_dfun ispec) )
	-- We need to create the cached fields afresh from
	-- the new dfun id.  In particular, the is_tvs in
	-- the Instance must match those in the dfun!
	-- We assume that the only thing that changes is
	-- the quantified type variables, so the other fields
	-- are ok; hence the assert
     ispec { is_dfun = dfun, is_tvs = mkVarSet tvs, is_tys = tys }
   where 
     (tvs, _, _, tys) = tcSplitDFunTy (idType dfun)

instanceRoughTcs :: Instance -> [Maybe Name]
instanceRoughTcs = is_tcs
\end{code}

\begin{code}
instance NamedThing Instance where
   getName ispec = getName (is_dfun ispec)

instance Outputable Instance where
   ppr = pprInstance

pprInstance :: Instance -> SDoc
-- Prints the Instance as an instance declaration
pprInstance ispec@(Instance { is_flag = flag })
  = hang (pprInstanceHdr ispec)
	2 (ptext SLIT("--") <+> (pprDefnLoc (getSrcLoc ispec)))

-- * pprInstanceHdr is used in VStudio to populate the ClassView tree
pprInstanceHdr :: Instance -> SDoc
-- Prints the Instance as an instance declaration
pprInstanceHdr ispec@(Instance { is_flag = flag })
  = ptext SLIT("instance") <+> ppr flag
    <+> sep [pprThetaArrow theta, pprClassPred clas tys]
  where
    (_, theta, clas, tys) = instanceHead ispec
	-- Print without the for-all, which the programmer doesn't write

pprInstances :: [Instance] -> SDoc
pprInstances ispecs = vcat (map pprInstance ispecs)

instanceHead :: Instance -> ([TyVar], [PredType], Class, [Type])
instanceHead ispec = tcSplitDFunTy (idType (is_dfun ispec))

mkLocalInstance :: DFunId -> OverlapFlag -> Instance
-- Used for local instances, where we can safely pull on the DFunId
mkLocalInstance dfun oflag
  = Instance {	is_flag = oflag, is_dfun = dfun,
		is_tvs = mkVarSet tvs, is_tys = tys,
		is_cls = cls_name, is_tcs = roughMatchTcs tys,
		is_orph = orph }
  where
    (tvs, _, cls, tys) = tcSplitDFunTy (idType dfun)
    mod = nameModule (idName dfun)
    cls_name = getName cls
    tycl_names = foldr (unionNameSets . tyClsNamesOfType) 
		       (unitNameSet cls_name) tys
    orph = case filter (nameIsLocalOrFrom mod) (nameSetToList tycl_names) of
		[]     -> Nothing
		(n:ns) -> Just (getOccName n)

mkImportedInstance :: Name -> [Maybe Name] -> Maybe OccName
		   -> DFunId -> OverlapFlag -> Instance
-- Used for imported instances, where we get the rough-match stuff
-- from the interface file
mkImportedInstance cls mb_tcs orph dfun oflag
  = Instance {	is_flag = oflag, is_dfun = dfun,
		is_tvs = mkVarSet tvs, is_tys = tys,
		is_cls = cls, is_tcs = mb_tcs, is_orph = orph }
  where
    (tvs, _, _, tys) = tcSplitDFunTy (idType dfun)

roughMatchTcs :: [Type] -> [Maybe Name]
roughMatchTcs tys = map rough tys
  where
    rough ty = case tcSplitTyConApp_maybe ty of
		  Just (tc,_) -> Just (tyConName tc)
		  Nothing     -> Nothing

instanceCantMatch :: [Maybe Name] -> [Maybe Name] -> Bool
-- (instanceCantMatch tcs1 tcs2) returns True if tcs1 cannot
-- possibly be instantiated to actual, nor vice versa; 
-- False is non-committal
instanceCantMatch (Just t : ts) (Just a : as) = t/=a || instanceCantMatch ts as
instanceCantMatch ts		as	      =  False	-- Safe

---------------------------------------------------
data OverlapFlag
  = NoOverlap	-- This instance must not overlap another

  | OverlapOk	-- Silently ignore this instance if you find a 
		-- more specific one that matches the constraint
		-- you are trying to resolve
		--
		-- Example: constraint (Foo [Int])
		-- 	    instances  (Foo [Int])
		--		       (Foo [a])	OverlapOk
		-- Since the second instance has the OverlapOk flag,
		-- the first instance will be chosen (otherwise 
		-- its ambiguous which to choose)

  | Incoherent	-- Like OverlapOk, but also ignore this instance 
		-- if it doesn't match the constraint you are
		-- trying to resolve, but could match if the type variables
		-- in the constraint were instantiated
		--
		-- Example: constraint (Foo [b])
		--	    instances  (Foo [Int])	Incoherent
		--		       (Foo [a])
		-- Without the Incoherent flag, we'd complain that
		-- instantiating 'b' would change which instance 
		-- was chosen

instance Outputable OverlapFlag where
   ppr NoOverlap  = empty
   ppr OverlapOk  = ptext SLIT("[overlap ok]")
   ppr Incoherent = ptext SLIT("[incoherent]")
\end{code}


Note [Overlapping instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Overlap is permitted, but only in such a way that one can make
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
		InstEnv, ClsInstEnv
%*									*
%************************************************************************

A @ClsInstEnv@ all the instances of that class.  The @Id@ inside a
ClsInstEnv mapping is the dfun for that instance.

If class C maps to a list containing the item ([a,b], [t1,t2,t3], dfun), then

	forall a b, C t1 t2 t3  can be constructed by dfun

or, to put it another way, we have

	instance (...) => C t1 t2 t3,  witnessed by dfun

\begin{code}
---------------------------------------------------
type InstEnv = UniqFM ClsInstEnv	-- Maps Class to instances for that class

data ClsInstEnv 
  = ClsIE [Instance]	-- The instances for a particular class, in any order
  	  Bool 		-- True <=> there is an instance of form C a b c
			-- 	If *not* then the common case of looking up
			--	(C a b c) can fail immediately

-- INVARIANTS:
--  * The is_tvs are distinct in each Instance
--	of a ClsInstEnv (so we can safely unify them)

-- Thus, the @ClassInstEnv@ for @Eq@ might contain the following entry:
--	[a] ===> dfun_Eq_List :: forall a. Eq a => Eq [a]
-- The "a" in the pattern must be one of the forall'd variables in
-- the dfun type.

emptyInstEnv :: InstEnv
emptyInstEnv = emptyUFM

instEnvElts :: InstEnv -> [Instance]
instEnvElts ie = [elt | ClsIE elts _ <- eltsUFM ie, elt <- elts]

classInstances :: (InstEnv,InstEnv) -> Class -> [Instance]
classInstances (pkg_ie, home_ie) cls 
  = get home_ie ++ get pkg_ie
  where
    get env = case lookupUFM env cls of
		Just (ClsIE insts _) -> insts
		Nothing		     -> []

extendInstEnvList :: InstEnv -> [Instance] -> InstEnv
extendInstEnvList inst_env ispecs = foldl extendInstEnv inst_env ispecs

extendInstEnv :: InstEnv -> Instance -> InstEnv
extendInstEnv inst_env ins_item@(Instance { is_cls = cls_nm, is_tcs = mb_tcs })
  = addToUFM_C add inst_env cls_nm (ClsIE [ins_item] ins_tyvar)
  where
    add (ClsIE cur_insts cur_tyvar) _ = ClsIE (ins_item : cur_insts)
					      (ins_tyvar || cur_tyvar)
    ins_tyvar = not (any isJust mb_tcs)
\end{code}		      


%************************************************************************
%*									*
\subsection{Looking up an instance}
%*									*
%************************************************************************

@lookupInstEnv@ looks up in a @InstEnv@, using a one-way match.  Since
the env is kept ordered, the first match must be the only one.  The
thing we are looking up can have an arbitrary "flexi" part.

\begin{code}
lookupInstEnv :: (InstEnv 	-- External package inst-env
		 ,InstEnv) 	-- Home-package inst-env
	      -> Class -> [Type]		-- What we are looking for
	      -> ([(TvSubst, Instance)], 	-- Successful matches
		  [Instance])			-- These don't match but do unify
	-- The second component of the tuple happens when we look up
	--	Foo [a]
	-- in an InstEnv that has entries for
	--	Foo [Int]
	--	Foo [b]
	-- Then which we choose would depend on the way in which 'a'
	-- is instantiated.  So we report that Foo [b] is a match (mapping b->a)
	-- but Foo [Int] is a unifier.  This gives the caller a better chance of
	-- giving a suitable error messagen

lookupInstEnv (pkg_ie, home_ie) cls tys
  = (pruned_matches, all_unifs)
  where
    rough_tcs  = roughMatchTcs tys
    all_tvs    = all isNothing rough_tcs
    (home_matches, home_unifs) = lookup home_ie 
    (pkg_matches,  pkg_unifs)  = lookup pkg_ie  
    all_matches = home_matches ++ pkg_matches
    all_unifs   = home_unifs   ++ pkg_unifs
    pruned_matches 
	| null all_unifs = foldr insert_overlapping [] all_matches
	| otherwise	 = all_matches	-- Non-empty unifs is always an error situation,
					-- so don't attempt to pune the matches

    --------------
    lookup env = case lookupUFM env cls of
		   Nothing -> ([],[])	-- No instances for this class
		   Just (ClsIE insts has_tv_insts)
			| all_tvs && not has_tv_insts
			-> ([],[])	-- Short cut for common case
			-- The thing we are looking up is of form (C a b c), and
			-- the ClsIE has no instances of that form, so don't bother to search
	
			| otherwise
			-> find [] [] insts

    --------------
    find ms us [] = (ms, us)
    find ms us (item@(Instance { is_tcs = mb_tcs, is_tvs = tpl_tvs, 
				 is_tys = tpl_tys, is_flag = oflag,
				 is_dfun = dfun }) : rest)
	-- Fast check for no match, uses the "rough match" fields
      | instanceCantMatch rough_tcs mb_tcs
      = find ms us rest

      | Just subst <- tcMatchTys tpl_tvs tpl_tys tys
      = find ((subst,item):ms) us rest

	-- Does not match, so next check whether the things unify
	-- See Note [overlapping instances] above
      | Incoherent <- oflag
      = find ms us rest

      | otherwise
      = ASSERT2( not (tyVarsOfTypes tys `intersectsVarSet` tpl_tvs),
		       (ppr cls <+> ppr tys <+> ppr all_tvs) $$
		       (ppr dfun <+> ppr tpl_tvs <+> ppr tpl_tys)
		)
		-- Unification will break badly if the variables overlap
		-- They shouldn't because we allocate separate uniques for them
        case tcUnifyTys bind_fn tpl_tys tys of
	    Just _   -> find ms (item:us) rest
	    Nothing  -> find ms us	   rest

---------------
bind_fn tv | isTcTyVar tv && isExistentialTyVar tv = Skolem
	   | otherwise	 		 	   = BindMe
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

---------------
insert_overlapping :: (TvSubst, Instance) -> [(TvSubst, Instance)] 
		   -> [(TvSubst, Instance)]
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

    (_, instA) `beats` (_, instB)
	= overlap_ok && 
	  isJust (tcMatchTys (is_tvs instB) (is_tys instB) (is_tys instA))
		-- A beats B if A is more specific than B, and B admits overlap
		-- I.e. if B can be instantiated to match A
	where
	  overlap_ok = case is_flag instB of
			NoOverlap -> False
			other	  -> True
\end{code}

