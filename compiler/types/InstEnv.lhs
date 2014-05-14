%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[InstEnv]{Utilities for typechecking instance declarations}

The bits common to TcInstDcls and TcDeriv.

\begin{code}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

module InstEnv (
        DFunId, OverlapFlag(..), InstMatch, ClsInstLookupResult,
        ClsInst(..), DFunInstType, pprInstance, pprInstanceHdr, pprInstances, 
        instanceHead, instanceSig, mkLocalInstance, mkImportedInstance,
        instanceDFunId, tidyClsInstDFun, instanceRoughTcs,

        InstEnv, emptyInstEnv, extendInstEnv, overwriteInstEnv, 
        extendInstEnvList, lookupUniqueInstEnv, lookupInstEnv', lookupInstEnv, instEnvElts,
        classInstances, orphNamesOfClsInst, instanceBindFun,
        instanceCantMatch, roughMatchTcs
    ) where

#include "HsVersions.h"

import Class
import Var
import VarSet
import Name
import NameSet
import TcType
import TyCon
import Unify
import Outputable
import ErrUtils
import BasicTypes
import UniqFM
import Util
import Id
import FastString
import Data.Data        ( Data, Typeable )
import Data.Maybe       ( isJust, isNothing )
\end{code}


%************************************************************************
%*                                                                      *
\subsection{The key types}
%*                                                                      *
%************************************************************************

\begin{code}
data ClsInst 
  = ClsInst {   -- Used for "rough matching"; see Note [Rough-match field]
                -- INVARIANT: is_tcs = roughMatchTcs is_tys
               is_cls_nm :: Name  -- Class name
             , is_tcs  :: [Maybe Name]  -- Top of type args

                -- Used for "proper matching"; see Note [Proper-match fields]
             , is_tvs  :: [TyVar]       -- Fresh template tyvars for full match
                                        -- See Note [Template tyvars are fresh]
             , is_cls  :: Class         -- The real class
             , is_tys  :: [Type]        -- Full arg types (mentioning is_tvs)
                -- INVARIANT: is_dfun Id has type 
                --      forall is_tvs. (...) => is_cls is_tys
                -- (modulo alpha conversion)

             , is_dfun :: DFunId -- See Note [Haddock assumptions]
                    -- See Note [Silent superclass arguments] in TcInstDcls
                    -- for how to map the DFun's type back to the source
                    -- language instance decl

             , is_flag :: OverlapFlag   -- See detailed comments with
                                        -- the decl of BasicTypes.OverlapFlag
    }
  deriving (Data, Typeable)
\end{code}

Note [Template tyvars are fresh]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The is_tvs field of a ClsInst has *completely fresh* tyvars.  
That is, they are
  * distinct from any other ClsInst
  * distinct from any tyvars free in predicates that may
    be looked up in the class instance environment
Reason for freshness: we use unification when checking for overlap
etc, and that requires the tyvars to be distinct.

The invariant is checked by the ASSERT in lookupInstEnv'.

Note [Rough-match field]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The is_cls_nm, is_tcs fields allow a "rough match" to be done
*without* poking inside the DFunId.  Poking the DFunId forces
us to suck in all the type constructors etc it involves,
which is a total waste of time if it has no chance of matching
So the Name, [Maybe Name] fields allow us to say "definitely
does not match", based only on the Name.

In is_tcs, 
    Nothing  means that this type arg is a type variable

    (Just n) means that this type arg is a
                TyConApp with a type constructor of n.
                This is always a real tycon, never a synonym!
                (Two different synonyms might match, but two
                different real tycons can't.)
                NB: newtypes are not transparent, though!

Note [Proper-match fields]
~~~~~~~~~~~~~~~~~~~~~~~~~
The is_tvs, is_cls, is_tys fields are simply cached values, pulled
out (lazily) from the dfun id. They are cached here simply so 
that we don't need to decompose the DFunId each time we want 
to match it.  The hope is that the fast-match fields mean
that we often never poke the proper-match fields.

However, note that:
 * is_tvs must be a superset of the free vars of is_tys

 * is_tvs, is_tys may be alpha-renamed compared to the ones in
   the dfun Id

Note [Haddock assumptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
For normal user-written instances, Haddock relies on

 * the SrcSpan of
 * the Name of
 * the is_dfun of
 * an Instance

being equal to

  * the SrcSpan of
  * the instance head type of
  * the InstDecl used to construct the Instance.

\begin{code}
instanceDFunId :: ClsInst -> DFunId
instanceDFunId = is_dfun

tidyClsInstDFun :: (DFunId -> DFunId) -> ClsInst -> ClsInst
tidyClsInstDFun tidy_dfun ispec
  = ispec { is_dfun = tidy_dfun (is_dfun ispec) }

instanceRoughTcs :: ClsInst -> [Maybe Name]
instanceRoughTcs = is_tcs
\end{code}

\begin{code}
instance NamedThing ClsInst where
   getName ispec = getName (is_dfun ispec)

instance Outputable ClsInst where
   ppr = pprInstance

pprInstance :: ClsInst -> SDoc
-- Prints the ClsInst as an instance declaration
pprInstance ispec
  = hang (pprInstanceHdr ispec)
        2 (ptext (sLit "--") <+> pprDefinedAt (getName ispec))

-- * pprInstanceHdr is used in VStudio to populate the ClassView tree
pprInstanceHdr :: ClsInst -> SDoc
-- Prints the ClsInst as an instance declaration
pprInstanceHdr (ClsInst { is_flag = flag, is_dfun = dfun })
  = getPprStyle $ \ sty ->
    let theta_to_print
          | debugStyle sty = theta
          | otherwise = drop (dfunNSilent dfun) theta
          -- See Note [Silent superclass arguments] in TcInstDcls
    in ptext (sLit "instance") <+> ppr flag
       <+> sep [pprThetaArrowTy theta_to_print, ppr res_ty]
  where
    (_, theta, res_ty) = tcSplitSigmaTy (idType dfun)
       -- Print without the for-all, which the programmer doesn't write

pprInstances :: [ClsInst] -> SDoc
pprInstances ispecs = vcat (map pprInstance ispecs)

instanceHead :: ClsInst -> ([TyVar], Class, [Type])
-- Returns the head, using the fresh tyavs from the ClsInst
instanceHead (ClsInst { is_tvs = tvs, is_tys = tys, is_dfun = dfun })
   = (tvs, cls, tys)
   where
     (_, _, cls, _) = tcSplitDFunTy (idType dfun)

instanceSig :: ClsInst -> ([TyVar], [Type], Class, [Type])
-- Decomposes the DFunId
instanceSig ispec = tcSplitDFunTy (idType (is_dfun ispec))

mkLocalInstance :: DFunId -> OverlapFlag
                -> [TyVar] -> Class -> [Type]
                -> ClsInst
-- Used for local instances, where we can safely pull on the DFunId
mkLocalInstance dfun oflag tvs cls tys
  = ClsInst { is_flag = oflag, is_dfun = dfun
            , is_tvs = tvs
            , is_cls = cls, is_cls_nm = className cls
            , is_tys = tys, is_tcs = roughMatchTcs tys }

mkImportedInstance :: Name -> [Maybe Name]
                   -> DFunId -> OverlapFlag -> ClsInst
-- Used for imported instances, where we get the rough-match stuff
-- from the interface file
-- The bound tyvars of the dfun are guaranteed fresh, because
-- the dfun has been typechecked out of the same interface file
mkImportedInstance cls_nm mb_tcs dfun oflag
  = ClsInst { is_flag = oflag, is_dfun = dfun
            , is_tvs = tvs, is_tys = tys
            , is_cls_nm = cls_nm, is_cls = cls, is_tcs = mb_tcs }
  where
    (tvs, _, cls, tys) = tcSplitDFunTy (idType dfun)

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
instanceCantMatch _             _             =  False  -- Safe
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
%*                                                                      *
                InstEnv, ClsInstEnv
%*                                                                      *
%************************************************************************

A @ClsInstEnv@ all the instances of that class.  The @Id@ inside a
ClsInstEnv mapping is the dfun for that instance.

If class C maps to a list containing the item ([a,b], [t1,t2,t3], dfun), then

        forall a b, C t1 t2 t3  can be constructed by dfun

or, to put it another way, we have

        instance (...) => C t1 t2 t3,  witnessed by dfun

\begin{code}
---------------------------------------------------
type InstEnv = UniqFM ClsInstEnv        -- Maps Class to instances for that class

newtype ClsInstEnv 
  = ClsIE [ClsInst]    -- The instances for a particular class, in any order

instance Outputable ClsInstEnv where
  ppr (ClsIE is) = pprInstances is

-- INVARIANTS:
--  * The is_tvs are distinct in each ClsInst
--      of a ClsInstEnv (so we can safely unify them)

-- Thus, the @ClassInstEnv@ for @Eq@ might contain the following entry:
--      [a] ===> dfun_Eq_List :: forall a. Eq a => Eq [a]
-- The "a" in the pattern must be one of the forall'd variables in
-- the dfun type.

emptyInstEnv :: InstEnv
emptyInstEnv = emptyUFM

instEnvElts :: InstEnv -> [ClsInst]
instEnvElts ie = [elt | ClsIE elts <- eltsUFM ie, elt <- elts]

classInstances :: (InstEnv,InstEnv) -> Class -> [ClsInst]
classInstances (pkg_ie, home_ie) cls 
  = get home_ie ++ get pkg_ie
  where
    get env = case lookupUFM env cls of
                Just (ClsIE insts) -> insts
                Nothing            -> []

-- | Collects the names of concrete types and type constructors that make
-- up the head of a class instance. For instance, given `class Foo a b`:
--
-- `instance Foo (Either (Maybe Int) a) Bool` would yield
--      [Either, Maybe, Int, Bool]
--
-- Used in the implementation of ":info" in GHCi.
orphNamesOfClsInst :: ClsInst -> NameSet
orphNamesOfClsInst = orphNamesOfDFunHead . idType . instanceDFunId

extendInstEnvList :: InstEnv -> [ClsInst] -> InstEnv
extendInstEnvList inst_env ispecs = foldl extendInstEnv inst_env ispecs

extendInstEnv :: InstEnv -> ClsInst -> InstEnv
extendInstEnv inst_env ins_item@(ClsInst { is_cls_nm = cls_nm })
  = addToUFM_C add inst_env cls_nm (ClsIE [ins_item])
  where
    add (ClsIE cur_insts) _ = ClsIE (ins_item : cur_insts)

overwriteInstEnv :: InstEnv -> ClsInst -> InstEnv
overwriteInstEnv inst_env ins_item@(ClsInst { is_cls_nm = cls_nm, is_tys = tys })
  = addToUFM_C add inst_env cls_nm (ClsIE [ins_item])
  where
    add (ClsIE cur_insts) _ = ClsIE (replaceInst cur_insts)
    
    rough_tcs  = roughMatchTcs tys
    replaceInst [] = [ins_item]
    replaceInst (item@(ClsInst { is_tcs = mb_tcs,  is_tvs = tpl_tvs 
                               , is_tys = tpl_tys }) : rest)
    -- Fast check for no match, uses the "rough match" fields
      | instanceCantMatch rough_tcs mb_tcs
      = item : replaceInst rest

      | let tpl_tv_set = mkVarSet tpl_tvs
      , Just _ <- tcMatchTys tpl_tv_set tpl_tys tys
      = ins_item : rest

      | otherwise
      = item : replaceInst rest
\end{code}


%************************************************************************
%*                                                                      *
        Looking up an instance
%*                                                                      *
%************************************************************************

@lookupInstEnv@ looks up in a @InstEnv@, using a one-way match.  Since
the env is kept ordered, the first match must be the only one.  The
thing we are looking up can have an arbitrary "flexi" part.

\begin{code}
type DFunInstType = Maybe Type
        -- Just ty   => Instantiate with this type
        -- Nothing   => Instantiate with any type of this tyvar's kind
        -- See Note [DFunInstType: instantiating types]

type InstMatch = (ClsInst, [DFunInstType])

type ClsInstLookupResult 
     = ( [InstMatch]     -- Successful matches
       , [ClsInst]       -- These don't match but do unify
       , Bool)           -- True if error condition caused by
                         -- SafeHaskell condition.
\end{code}

Note [DFunInstType: instantiating types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A successful match is a ClsInst, together with the types at which
        the dfun_id in the ClsInst should be instantiated
The instantiating types are (Either TyVar Type)s because the dfun
might have some tyvars that *only* appear in arguments
        dfun :: forall a b. C a b, Ord b => D [a]
When we match this against D [ty], we return the instantiating types
        [Just ty, Nothing]
where the 'Nothing' indicates that 'b' can be freely instantiated.  
(The caller instantiates it to a flexi type variable, which will 
 presumably later become fixed via functional dependencies.)

\begin{code}
-- |Look up an instance in the given instance environment. The given class application must match exactly
-- one instance and the match may not contain any flexi type variables.  If the lookup is unsuccessful,
-- yield 'Left errorMessage'.
--
lookupUniqueInstEnv :: (InstEnv, InstEnv) 
                    -> Class -> [Type]
                    -> Either MsgDoc (ClsInst, [Type])
lookupUniqueInstEnv instEnv cls tys
  = case lookupInstEnv instEnv cls tys of
      ([(inst, inst_tys)], _, _) 
             | noFlexiVar -> Right (inst, inst_tys')
             | otherwise  -> Left $ ptext (sLit "flexible type variable:") <+>
                                    (ppr $ mkTyConApp (classTyCon cls) tys)
             where
               inst_tys'  = [ty | Just ty <- inst_tys]
               noFlexiVar = all isJust inst_tys
      _other -> Left $ ptext (sLit "instance not found") <+> (ppr $ mkTyConApp (classTyCon cls) tys)

lookupInstEnv' :: InstEnv          -- InstEnv to look in
               -> Class -> [Type]  -- What we are looking for
               -> ([InstMatch],    -- Successful matches
                   [ClsInst])     -- These don't match but do unify
-- The second component of the result pair happens when we look up
--      Foo [a]
-- in an InstEnv that has entries for
--      Foo [Int]
--      Foo [b]
-- Then which we choose would depend on the way in which 'a'
-- is instantiated.  So we report that Foo [b] is a match (mapping b->a)
-- but Foo [Int] is a unifier.  This gives the caller a better chance of
-- giving a suitable error message

lookupInstEnv' ie cls tys
  = lookup ie
  where
    rough_tcs  = roughMatchTcs tys
    all_tvs    = all isNothing rough_tcs
    --------------
    lookup env = case lookupUFM env cls of
                   Nothing -> ([],[])   -- No instances for this class
                   Just (ClsIE insts) -> find [] [] insts

    --------------
    find ms us [] = (ms, us)
    find ms us (item@(ClsInst { is_tcs = mb_tcs, is_tvs = tpl_tvs
                              , is_tys = tpl_tys, is_flag = oflag }) : rest)
        -- Fast check for no match, uses the "rough match" fields
      | instanceCantMatch rough_tcs mb_tcs
      = find ms us rest

      | Just subst <- tcMatchTys tpl_tv_set tpl_tys tys
      = find ((item, map (lookup_tv subst) tpl_tvs) : ms) us rest

        -- Does not match, so next check whether the things unify
        -- See Note [Overlapping instances] and Note [Incoherent Instances]
      | Incoherent _ <- oflag
      = find ms us rest

      | otherwise
      = ASSERT2( tyVarsOfTypes tys `disjointVarSet` tpl_tv_set,
                 (ppr cls <+> ppr tys <+> ppr all_tvs) $$
                 (ppr tpl_tvs <+> ppr tpl_tys)
                )
                -- Unification will break badly if the variables overlap
                -- They shouldn't because we allocate separate uniques for them
                -- See Note [Template tyvars are fresh]
        case tcUnifyTys instanceBindFun tpl_tys tys of
            Just _   -> find ms (item:us) rest
            Nothing  -> find ms us        rest
      where
        tpl_tv_set = mkVarSet tpl_tvs

    ----------------
    lookup_tv :: TvSubst -> TyVar -> DFunInstType
        -- See Note [DFunInstType: instantiating types]
    lookup_tv subst tv = case lookupTyVar subst tv of
                                Just ty -> Just ty
                                Nothing -> Nothing

---------------
-- This is the common way to call this function.
lookupInstEnv :: (InstEnv, InstEnv)     -- External and home package inst-env
              -> Class -> [Type]   -- What we are looking for
              -> ClsInstLookupResult
 
lookupInstEnv (pkg_ie, home_ie) cls tys
  = (safe_matches, all_unifs, safe_fail)
  where
    (home_matches, home_unifs) = lookupInstEnv' home_ie cls tys
    (pkg_matches,  pkg_unifs)  = lookupInstEnv' pkg_ie  cls tys
    all_matches = home_matches ++ pkg_matches
    all_unifs   = home_unifs   ++ pkg_unifs
    pruned_matches = foldr insert_overlapping [] all_matches
    (safe_matches, safe_fail) = if length pruned_matches == 1 
                        then check_safe (head pruned_matches) all_matches
                        else (pruned_matches, False)
        -- Even if the unifs is non-empty (an error situation)
        -- we still prune the matches, so that the error message isn't
        -- misleading (complaining of multiple matches when some should be
        -- overlapped away)

    -- NOTE [Safe Haskell isSafeOverlap]
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- We restrict code compiled in 'Safe' mode from overriding code
    -- compiled in any other mode. The rationale is that code compiled
    -- in 'Safe' mode is code that is untrusted by the ghc user. So
    -- we shouldn't let that code change the behaviour of code the
    -- user didn't compile in 'Safe' mode since that's the code they
    -- trust. So 'Safe' instances can only overlap instances from the
    -- same module. A same instance origin policy for safe compiled
    -- instances.
    check_safe match@(inst,_) others
        = case isSafeOverlap (is_flag inst) of
                -- most specific isn't from a Safe module so OK
                False -> ([match], False)
                -- otherwise we make sure it only overlaps instances from
                -- the same module
                True -> (go [] others, True)
        where
            go bad [] = match:bad
            go bad (i@(x,_):unchecked) =
                if inSameMod x
                    then go bad unchecked
                    else go (i:bad) unchecked
            
            inSameMod b =
                let na = getName $ getName inst
                    la = isInternalName na
                    nb = getName $ getName b
                    lb = isInternalName nb
                in (la && lb) || (nameModule na == nameModule nb)

---------------
---------------
insert_overlapping :: InstMatch -> [InstMatch] -> [InstMatch]
-- Add a new solution, knocking out strictly less specific ones
insert_overlapping new_item [] = [new_item]
insert_overlapping new_item (item:items)
  | new_beats_old && old_beats_new = item : insert_overlapping new_item items
        -- Duplicate => keep both for error report
  | new_beats_old = insert_overlapping new_item items
        -- Keep new one
  | old_beats_new = item : items
        -- Keep old one
  | incoherent new_item = item : items -- note [Incoherent instances]
        -- Keep old one
  | incoherent item = new_item : items
        -- Keep new one
  | otherwise     = item : insert_overlapping new_item items
        -- Keep both
  where
    new_beats_old = new_item `beats` item
    old_beats_new = item `beats` new_item

    incoherent (inst, _) = case is_flag inst of Incoherent _ -> True
                                                _            -> False

    (instA, _) `beats` (instB, _)
          = overlap_ok && 
            isJust (tcMatchTys (mkVarSet (is_tvs instB)) (is_tys instB) (is_tys instA))
                    -- A beats B if A is more specific than B,
                    -- (ie. if B can be instantiated to match A)
                    -- and overlap is permitted
          where
            -- Overlap permitted if *either* instance permits overlap
            -- This is a change (Trac #3877, Dec 10). It used to
            -- require that instB (the less specific one) permitted overlap.
            overlap_ok = case (is_flag instA, is_flag instB) of
                              (NoOverlap _, NoOverlap _) -> False
                              _                          -> True
\end{code}

Note [Incoherent instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

For some classes, the choise of a particular instance does not matter, any one
is good. E.g. consider

        class D a b where { opD :: a -> b -> String }
        instance D Int b where ...
        instance D a Int where ...

        g (x::Int) = opD x x

For such classes this should work (without having to add an "instance D Int
Int", and using -XOverlappingInstances, which would then work). This is what
-XIncoherentInstances is for: Telling GHC "I don't care which instance you use;
if you can use one, use it."


Should this logic only work when all candidates have the incoherent flag, or
even when all but one have it? The right choice is the latter, which can be
justified by comparing the behaviour with how -XIncoherentInstances worked when
it was only about the unify-check (note [Overlapping instances]):

Example:
        class C a b c where foo :: (a,b,c)
        instance C [a] b Int
        instance [incoherent] [Int] b c
        instance [incoherent] C a Int c
Thanks to the incoherent flags,
        foo :: ([a],b,Int)
works: Only instance one matches, the others just unify, but are marked
incoherent.

So I can write
        (foo :: ([a],b,Int)) :: ([Int], Int, Int).
but if that works then I really want to be able to write
        foo :: ([Int], Int, Int)
as well. Now all three instances from above match. None is more specific than
another, so none is ruled out by the normal overlapping rules. One of them is
not incoherent, but we still want this to compile. Hence the
"all-but-one-logic".

The implementation is in insert_overlapping, where we remove matching
incoherent instances as long as there are are others.



%************************************************************************
%*                                                                      *
        Binding decisions
%*                                                                      *
%************************************************************************

\begin{code}
instanceBindFun :: TyVar -> BindFlag
instanceBindFun tv | isTcTyVar tv && isOverlappableTyVar tv = Skolem
                   | otherwise                              = BindMe
   -- Note [Binding when looking up instances]
\end{code}

Note [Binding when looking up instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When looking up in the instance environment, or family-instance environment,
we are careful about multiple matches, as described above in 
Note [Overlapping instances]

The key_tys can contain skolem constants, and we can guarantee that those
are never going to be instantiated to anything, so we should not involve
them in the unification test.  Example:
        class Foo a where { op :: a -> Int }
        instance Foo a => Foo [a]       -- NB overlap
        instance Foo [Int]              -- NB overlap
        data T = forall a. Foo a => MkT a
        f :: T -> Int
        f (MkT x) = op [x,x]
The op [x,x] means we need (Foo [a]).  Without the filterVarSet we'd
complain, saying that the choice of instance depended on the instantiation
of 'a'; but of course it isn't *going* to be instantiated.

We do this only for isOverlappableTyVar skolems.  For example we reject
        g :: forall a => [a] -> Int
        g x = op x
on the grounds that the correct instance depends on the instantiation of 'a'
