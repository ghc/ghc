{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[InstEnv]{Utilities for typechecking instance declarations}

The bits common to GHC.Tc.TyCl.Instance and GHC.Tc.Deriv.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module GHC.Core.InstEnv (
        DFunId, InstMatch, ClsInstLookupResult,
        PotentialUnifiers(..), getPotentialUnifiers, nullUnifiers,
        OverlapFlag(..), OverlapMode(..), setOverlapModeMaybe,
        ClsInst(..), DFunInstType, pprInstance, pprInstanceHdr, pprInstances,
        instanceHead, instanceSig, mkLocalInstance, mkImportedInstance,
        instanceDFunId, updateClsInstDFuns, updateClsInstDFun,
        fuzzyClsInstCmp, orphNamesOfClsInst,

        InstEnvs(..), VisibleOrphanModules, InstEnv,
        mkInstEnv, emptyInstEnv, unionInstEnv, extendInstEnv,
        filterInstEnv, deleteFromInstEnv, deleteDFunFromInstEnv,
        anyInstEnv,
        identicalClsInstHead,
        extendInstEnvList, lookupUniqueInstEnv, lookupInstEnv, instEnvElts, instEnvClasses, mapInstEnv,
        memberInstEnv,
        instIsVisible,
        classInstances, instanceBindFun,
        classNameInstances,
        instanceCantMatch, roughMatchTcs,
        isOverlappable, isOverlapping, isIncoherent
    ) where

import GHC.Prelude

import GHC.Tc.Utils.TcType -- InstEnv is really part of the type checker,
              -- and depends on TcType in many ways
import GHC.Core ( IsOrphan(..), isOrphan, chooseOrphanAnchor )
import GHC.Core.RoughMap
import GHC.Unit.Module.Env
import GHC.Unit.Types
import GHC.Core.Class
import GHC.Types.Var
import GHC.Types.Unique.DSet
import GHC.Types.Var.Set
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Core.Unify
import GHC.Types.Basic
import GHC.Types.Id
import Data.Data        ( Data )
import Data.Maybe       ( isJust )

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import Data.Semigroup

{-
************************************************************************
*                                                                      *
           ClsInst: the data type for type-class instances
*                                                                      *
************************************************************************
-}

-- | A type-class instance. Note that there is some tricky laziness at work
-- here. See Note [ClsInst laziness and the rough-match fields] for more
-- details.
data ClsInst
  = ClsInst {   -- Used for "rough matching"; see
                -- Note [ClsInst laziness and the rough-match fields]
                -- INVARIANT: is_tcs = KnownTc is_cls_nm : roughMatchTcs is_tys
               is_cls_nm :: Name          -- ^ Class name

             , is_tcs  :: [RoughMatchTc]  -- ^ Top of type args
                          -- The class itself is always
                          -- the first element of this list

               -- | @is_dfun_name = idName . is_dfun@.
               --
               -- We use 'is_dfun_name' for the visibility check,
               -- 'instIsVisible', which needs to know the 'Module' which the
               -- dictionary is defined in. However, we cannot use the 'Module'
               -- attached to 'is_dfun' since doing so would mean we would
               -- potentially pull in an entire interface file unnecessarily.
               -- This was the cause of #12367.
             , is_dfun_name :: Name

                -- Used for "proper matching"; see Note [Proper-match fields]
             , is_tvs  :: [TyVar]       -- Fresh template tyvars for full match
                                        -- See Note [Template tyvars are fresh]
             , is_cls  :: Class         -- The real class
             , is_tys  :: [Type]        -- Full arg types (mentioning is_tvs)
                -- INVARIANT: is_dfun Id has type
                --      forall is_tvs. (...) => is_cls is_tys
                -- (modulo alpha conversion)

             , is_dfun :: DFunId -- See Note [Haddock assumptions]

             , is_flag :: OverlapFlag   -- See detailed comments with
                                        -- the decl of BasicTypes.OverlapFlag
             , is_orphan :: IsOrphan
    }
  deriving Data

-- | A fuzzy comparison function for class instances, intended for sorting
-- instances before displaying them to the user.
fuzzyClsInstCmp :: ClsInst -> ClsInst -> Ordering
fuzzyClsInstCmp x y =
    foldMap cmp (zip (is_tcs x) (is_tcs y))
  where
    cmp (RM_WildCard,  RM_WildCard)   = EQ
    cmp (RM_WildCard,  RM_KnownTc _) = LT
    cmp (RM_KnownTc _, RM_WildCard)   = GT
    cmp (RM_KnownTc x, RM_KnownTc y) = stableNameCmp x y

isOverlappable, isOverlapping, isIncoherent :: ClsInst -> Bool
isOverlappable i = hasOverlappableFlag (overlapMode (is_flag i))
isOverlapping  i = hasOverlappingFlag  (overlapMode (is_flag i))
isIncoherent   i = hasIncoherentFlag   (overlapMode (is_flag i))

{-
Note [ClsInst laziness and the rough-match fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we load 'instance A.C B.T' from A.hi, but suppose that the type B.T is
otherwise unused in the program. Then it's stupid to load B.hi, the data type
declaration for B.T -- and perhaps further instance declarations!

We avoid this as follows:

* is_cls_nm, is_tcs, is_dfun_name are all Names. We can poke them to our heart's
  content.

* Proper-match fields. is_dfun, and its related fields is_tvs, is_cls, is_tys
  contain TyVars, Class, Type, Class etc, and so are all lazy thunks. When we
  poke any of these fields we'll typecheck the DFunId declaration, and hence
  pull in interfaces that it refers to. See Note [Proper-match fields].

* Rough-match fields. During instance lookup, we use the is_cls_nm :: Name and
  is_tcs :: [RoughMatchTc] fields to perform a "rough match", *without* poking
  inside the DFunId. The rough-match fields allow us to say "definitely does not
  match", based only on Names.  See GHC.Core.Unify
  Note [Rough matching in class and family instances]

  This laziness is very important; see #12367. Try hard to avoid pulling on
  the structured fields unless you really need the instance.

* Another place to watch is InstEnv.instIsVisible, which needs the module to
  which the ClsInst belongs. We can get this from is_dfun_name.
-}

{-
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

Note [Proper-match fields]
~~~~~~~~~~~~~~~~~~~~~~~~~
The is_tvs, is_cls, is_tys fields are simply cached values, pulled
out (lazily) from the dfun id. They are cached here simply so
that we don't need to decompose the DFunId each time we want
to match it.  The hope is that the rough-match fields mean
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
-}

instanceDFunId :: ClsInst -> DFunId
instanceDFunId = is_dfun

updateClsInstDFun :: (DFunId -> DFunId) -> ClsInst -> ClsInst
updateClsInstDFun tidy_dfun ispec
  = ispec { is_dfun = tidy_dfun (is_dfun ispec) }

updateClsInstDFuns :: (DFunId -> DFunId) -> InstEnv -> InstEnv
updateClsInstDFuns tidy_dfun (InstEnv rm)
  = InstEnv $ fmap (updateClsInstDFun tidy_dfun) rm

instance NamedThing ClsInst where
   getName ispec = getName (is_dfun ispec)

instance Outputable ClsInst where
   ppr = pprInstance

pprInstance :: ClsInst -> SDoc
-- Prints the ClsInst as an instance declaration
pprInstance ispec
  = hang (pprInstanceHdr ispec)
       2 (vcat [ text "--" <+> pprDefinedAt (getName ispec)
               , whenPprDebug (ppr (is_dfun ispec)) ])

-- * pprInstanceHdr is used in VStudio to populate the ClassView tree
pprInstanceHdr :: ClsInst -> SDoc
-- Prints the ClsInst as an instance declaration
pprInstanceHdr (ClsInst { is_flag = flag, is_dfun = dfun })
  = text "instance" <+> ppr flag <+> pprSigmaType (idType dfun)

pprInstances :: [ClsInst] -> SDoc
pprInstances ispecs = vcat (map pprInstance ispecs)

instanceHead :: ClsInst -> ([TyVar], Class, [Type])
-- Returns the head, using the fresh tyvars from the ClsInst
instanceHead (ClsInst { is_tvs = tvs, is_tys = tys, is_dfun = dfun })
   = (tvs, cls, tys)
   where
     (_, _, cls, _) = tcSplitDFunTy (idType dfun)

-- | Collects the names of concrete types and type constructors that make
-- up the head of a class instance. For instance, given `class Foo a b`:
--
-- `instance Foo (Either (Maybe Int) a) Bool` would yield
--      [Either, Maybe, Int, Bool]
--
-- Used in the implementation of ":info" in GHCi.
--
-- The 'tcSplitSigmaTy' is because of
--      instance Foo a => Baz T where ...
-- The decl is an orphan if Baz and T are both not locally defined,
--      even if Foo *is* locally defined
orphNamesOfClsInst :: ClsInst -> NameSet
orphNamesOfClsInst (ClsInst { is_cls_nm = cls_nm, is_tys = tys })
  = orphNamesOfTypes tys `unionNameSet` unitNameSet cls_nm

instanceSig :: ClsInst -> ([TyVar], [Type], Class, [Type])
-- Decomposes the DFunId
instanceSig ispec = tcSplitDFunTy (idType (is_dfun ispec))

mkLocalInstance :: DFunId -> OverlapFlag
                -> [TyVar] -> Class -> [Type]
                -> ClsInst
-- Used for local instances, where we can safely pull on the DFunId.
-- Consider using newClsInst instead; this will also warn if
-- the instance is an orphan.
mkLocalInstance dfun oflag tvs cls tys
  = ClsInst { is_flag = oflag, is_dfun = dfun
            , is_tvs = tvs
            , is_dfun_name = dfun_name
            , is_cls = cls, is_cls_nm = cls_name
            , is_tys = tys, is_tcs = RM_KnownTc cls_name : roughMatchTcs tys
            , is_orphan = orph
            }
  where
    cls_name = className cls
    dfun_name = idName dfun
    this_mod = assert (isExternalName dfun_name) $ nameModule dfun_name
    is_local name = nameIsLocalOrFrom this_mod name

        -- Compute orphanhood.  See Note [Orphans] in GHC.Core.InstEnv
    (cls_tvs, fds) = classTvsFds cls
    arg_names = [filterNameSet is_local (orphNamesOfType ty) | ty <- tys]

    -- See Note [When exactly is an instance decl an orphan?]
    orph | is_local cls_name   = NotOrphan (nameOccName cls_name)
         | all notOrphan mb_ns = assert (not (null mb_ns)) $ head mb_ns
         | otherwise           = IsOrphan

    notOrphan NotOrphan{} = True
    notOrphan _ = False

    mb_ns :: [IsOrphan]    -- One for each fundep; a locally-defined name
                           -- that is not in the "determined" arguments
    mb_ns | null fds   = [choose_one arg_names]
          | otherwise  = map do_one fds
    do_one (_ltvs, rtvs) = choose_one [ns | (tv,ns) <- cls_tvs `zip` arg_names
                                            , not (tv `elem` rtvs)]

    choose_one nss = chooseOrphanAnchor (unionNameSets nss)

mkImportedInstance :: Name           -- ^ the name of the class
                   -> [RoughMatchTc] -- ^ the rough match signature of the instance
                   -> Name           -- ^ the 'Name' of the dictionary binding
                   -> DFunId         -- ^ the 'Id' of the dictionary.
                   -> OverlapFlag    -- ^ may this instance overlap?
                   -> IsOrphan       -- ^ is this instance an orphan?
                   -> ClsInst
-- Used for imported instances, where we get the rough-match stuff
-- from the interface file
-- The bound tyvars of the dfun are guaranteed fresh, because
-- the dfun has been typechecked out of the same interface file
mkImportedInstance cls_nm mb_tcs dfun_name dfun oflag orphan
  = ClsInst { is_flag = oflag, is_dfun = dfun
            , is_tvs = tvs, is_tys = tys
            , is_dfun_name = dfun_name
            , is_cls_nm = cls_nm, is_cls = cls
            , is_tcs = RM_KnownTc cls_nm : mb_tcs
            , is_orphan = orphan }
  where
    (tvs, _, cls, tys) = tcSplitDFunTy (idType dfun)

{-
Note [When exactly is an instance decl an orphan?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (see GHC.Iface.Make.instanceToIfaceInst, which implements this)
Roughly speaking, an instance is an orphan if its head (after the =>)
mentions nothing defined in this module.

Functional dependencies complicate the situation though. Consider

  module M where { class C a b | a -> b }

and suppose we are compiling module X:

  module X where
        import M
        data T = ...
        instance C Int T where ...

This instance is an orphan, because when compiling a third module Y we
might get a constraint (C Int v), and we'd want to improve v to T.  So
we must make sure X's instances are loaded, even if we do not directly
use anything from X.

More precisely, an instance is an orphan iff

  If there are no fundeps, then at least of the names in
  the instance head is locally defined.

  If there are fundeps, then for every fundep, at least one of the
  names free in a *non-determined* part of the instance head is
  defined in this module.

(Note that these conditions hold trivially if the class is locally
defined.)


************************************************************************
*                                                                      *
                InstEnv, ClsInstEnv
*                                                                      *
************************************************************************

A @ClsInstEnv@ all the instances of that class.  The @Id@ inside a
ClsInstEnv mapping is the dfun for that instance.

If class C maps to a list containing the item ([a,b], [t1,t2,t3], dfun), then

        forall a b, C t1 t2 t3  can be constructed by dfun

or, to put it another way, we have

        instance (...) => C t1 t2 t3,  witnessed by dfun
-}

---------------------------------------------------
{-
Note [InstEnv determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We turn InstEnvs into a list in some places that don't directly affect
the ABI. That happens when we create output for `:info`.
Unfortunately that nondeterminism is nonlocal and it's hard to tell what it
affects without following a chain of functions. It's also easy to accidentally
make that nondeterminism affect the ABI. Furthermore the envs should be
relatively small, so it should be free to use deterministic maps here.
Testing with nofib and validate detected no difference between UniqFM and
UniqDFM. See also Note [Deterministic UniqFM]
-}

-- Internally it's safe to indexable this map by
-- by @Class@, the classes @Name@, the classes @TyCon@
-- or it's @Unique@.
-- This is since:
-- getUnique cls == getUnique (className cls) == getUnique (classTyCon cls)
--
-- We still use Class as key type as it's both the common case
-- and conveys the meaning better. But the implementation of
--InstEnv is a bit more lax internally.
newtype InstEnv = InstEnv (RoughMap ClsInst)      -- Maps Class to instances for that class
  -- See Note [InstEnv determinism]

instance Outputable InstEnv where
  ppr (InstEnv rm) = pprInstances $ elemsRM rm

-- | 'InstEnvs' represents the combination of the global type class instance
-- environment, the local type class instance environment, and the set of
-- transitively reachable orphan modules (according to what modules have been
-- directly imported) used to test orphan instance visibility.
data InstEnvs = InstEnvs {
        ie_global  :: InstEnv,               -- External-package instances
        ie_local   :: InstEnv,               -- Home-package instances
        ie_visible :: VisibleOrphanModules   -- Set of all orphan modules transitively
                                             -- reachable from the module being compiled
                                             -- See Note [Instance lookup and orphan instances]
    }

-- | Set of visible orphan modules, according to what modules have been directly
-- imported.  This is based off of the dep_orphs field, which records
-- transitively reachable orphan modules (modules that define orphan instances).
type VisibleOrphanModules = ModuleSet


-- INVARIANTS:
--  * The is_tvs are distinct in each ClsInst
--      of a ClsInstEnv (so we can safely unify them)

-- Thus, the @ClsInstEnv@ for @Eq@ might contain the following entry:
--      [a] ===> dfun_Eq_List :: forall a. Eq a => Eq [a]
-- The "a" in the pattern must be one of the forall'd variables in
-- the dfun type.

emptyInstEnv :: InstEnv
emptyInstEnv = InstEnv emptyRM

mkInstEnv :: [ClsInst] -> InstEnv
mkInstEnv = extendInstEnvList emptyInstEnv

instEnvElts :: InstEnv -> [ClsInst]
instEnvElts (InstEnv rm) = elemsRM rm
  -- See Note [InstEnv determinism]

instEnvEltsForClass :: InstEnv -> Name -> [ClsInst]
instEnvEltsForClass (InstEnv rm) cls_nm = lookupRM [RML_KnownTc cls_nm] rm

-- N.B. this is not particularly efficient but used only by GHCi.
instEnvClasses :: InstEnv -> UniqDSet Class
instEnvClasses ie = mkUniqDSet $ map is_cls (instEnvElts ie)

-- | Test if an instance is visible, by checking that its origin module
-- is in 'VisibleOrphanModules'.
-- See Note [Instance lookup and orphan instances]
instIsVisible :: VisibleOrphanModules -> ClsInst -> Bool
instIsVisible vis_mods ispec
  -- NB: Instances from the interactive package always are visible. We can't
  -- add interactive modules to the set since we keep creating new ones
  -- as a GHCi session progresses.
  = case nameModule_maybe (is_dfun_name ispec) of
      Nothing -> True
      Just mod | isInteractiveModule mod     -> True
               | IsOrphan <- is_orphan ispec -> mod `elemModuleSet` vis_mods
               | otherwise                   -> True

classInstances :: InstEnvs -> Class -> [ClsInst]
classInstances envs cls = classNameInstances envs (className cls)

classNameInstances :: InstEnvs -> Name -> [ClsInst]
classNameInstances (InstEnvs { ie_global = pkg_ie, ie_local = home_ie, ie_visible = vis_mods }) cls
  = get home_ie ++ get pkg_ie
  where
    get :: InstEnv -> [ClsInst]
    get ie = filter (instIsVisible vis_mods) (instEnvEltsForClass ie cls)

-- | Checks for an exact match of ClsInst in the instance environment.
-- We use this when we do signature checking in "GHC.Tc.Module"
memberInstEnv :: InstEnv -> ClsInst -> Bool
memberInstEnv (InstEnv rm) ins_item@(ClsInst { is_tcs = tcs } ) =
    any (identicalDFunType ins_item) (fst $ lookupRM' (map roughMatchTcToLookup tcs) rm)
 where
  identicalDFunType cls1 cls2 =
    eqType (varType (is_dfun cls1)) (varType (is_dfun cls2))

-- | Makes no particular effort to detect conflicts.
unionInstEnv :: InstEnv -> InstEnv -> InstEnv
unionInstEnv (InstEnv a) (InstEnv b) = InstEnv (a `unionRM` b)

extendInstEnvList :: InstEnv -> [ClsInst] -> InstEnv
extendInstEnvList inst_env ispecs = foldl' extendInstEnv inst_env ispecs

extendInstEnv :: InstEnv -> ClsInst -> InstEnv
extendInstEnv (InstEnv rm) ins_item@(ClsInst { is_tcs = tcs })
  = InstEnv $ insertRM tcs ins_item rm

filterInstEnv :: (ClsInst -> Bool) -> InstEnv -> InstEnv
filterInstEnv pred (InstEnv rm)
  = InstEnv $ filterRM pred rm

anyInstEnv :: (ClsInst -> Bool) -> InstEnv -> Bool
anyInstEnv pred (InstEnv rm)
  = foldRM (\x rest -> pred x || rest) False rm

mapInstEnv :: (ClsInst -> ClsInst) -> InstEnv -> InstEnv
mapInstEnv f (InstEnv rm) = InstEnv (f <$> rm)

deleteFromInstEnv :: InstEnv -> ClsInst -> InstEnv
deleteFromInstEnv (InstEnv rm) ins_item@(ClsInst { is_tcs = tcs })
  = InstEnv $ filterMatchingRM (not . identicalClsInstHead ins_item) tcs rm

deleteDFunFromInstEnv :: InstEnv -> DFunId -> InstEnv
-- Delete a specific instance fron an InstEnv
deleteDFunFromInstEnv (InstEnv rm) dfun
  = InstEnv $ filterMatchingRM (not . same_dfun) [RM_KnownTc (className cls)] rm
  where
    (_, _, cls, _) = tcSplitDFunTy (idType dfun)
    same_dfun (ClsInst { is_dfun = dfun' }) = dfun == dfun'

identicalClsInstHead :: ClsInst -> ClsInst -> Bool
-- ^ True when when the instance heads are the same
-- e.g.  both are   Eq [(a,b)]
-- Used for overriding in GHCi
-- Obviously should be insensitive to alpha-renaming
identicalClsInstHead (ClsInst { is_tcs = rough1, is_tys = tys1 })
                     (ClsInst { is_tcs = rough2, is_tys = tys2 })
  =  not (instanceCantMatch rough1 rough2)  -- Fast check for no match, uses the "rough match" fields;
                                            -- also accounts for class name.
  && isJust (tcMatchTys tys1 tys2)
  && isJust (tcMatchTys tys2 tys1)

{-
************************************************************************
*                                                                      *
        Looking up an instance
*                                                                      *
************************************************************************

@lookupInstEnv@ looks up in a @InstEnv@, using a one-way match.  Since
the env is kept ordered, the first match must be the only one.  The
thing we are looking up can have an arbitrary "flexi" part.

Note [Instance lookup and orphan instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are compiling a module M, and we have a zillion packages
loaded, and we are looking up an instance for C (T W).  If we find a
match in module 'X' from package 'p', should be "in scope"; that is,

  is p:X in the transitive closure of modules imported from M?

The difficulty is that the "zillion packages" might include ones loaded
through earlier invocations of the GHC API, or earlier module loads in GHCi.
They might not be in the dependencies of M itself; and if not, the instances
in them should not be visible.  #2182, #8427.

There are two cases:
  * If the instance is *not an orphan*, then module X defines C, T, or W.
    And in order for those types to be involved in typechecking M, it
    must be that X is in the transitive closure of M's imports.  So we
    can use the instance.

  * If the instance *is an orphan*, the above reasoning does not apply.
    So we keep track of the set of orphan modules transitively below M;
    this is the ie_visible field of InstEnvs, of type VisibleOrphanModules.

    If module p:X is in this set, then we can use the instance, otherwise
    we can't.

Note [Rules for instance lookup]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These functions implement the carefully-written rules in the user
manual section on "overlapping instances". At risk of duplication,
here are the rules.  If the rules change, change this text and the
user manual simultaneously.  The link may be this:
http://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#instance-overlap

The willingness to be overlapped or incoherent is a property of the
instance declaration itself, controlled as follows:

 * An instance is "incoherent"
   if it has an INCOHERENT pragma, or
   if it appears in a module compiled with -XIncoherentInstances.

 * An instance is "overlappable"
   if it has an OVERLAPPABLE or OVERLAPS pragma, or
   if it appears in a module compiled with -XOverlappingInstances, or
   if the instance is incoherent.

 * An instance is "overlapping"
   if it has an OVERLAPPING or OVERLAPS pragma, or
   if it appears in a module compiled with -XOverlappingInstances, or
   if the instance is incoherent.
     compiled with -XOverlappingInstances.

Now suppose that, in some client module, we are searching for an instance
of the target constraint (C ty1 .. tyn). The search works like this.

*  Find all instances `I` that *match* the target constraint; that is, the
   target constraint is a substitution instance of `I`. These instance
   declarations are the *candidates*.

*  Eliminate any candidate `IX` for which both of the following hold:

   -  There is another candidate `IY` that is strictly more specific; that
      is, `IY` is a substitution instance of `IX` but not vice versa.

   -  Either `IX` is *overlappable*, or `IY` is *overlapping*. (This
      "either/or" design, rather than a "both/and" design, allow a
      client to deliberately override an instance from a library,
      without requiring a change to the library.)

-  If exactly one non-incoherent candidate remains, select it. If all
   remaining candidates are incoherent, select an arbitrary one.
   Otherwise the search fails (i.e. when more than one surviving
   candidate is not incoherent).

-  If the selected candidate (from the previous step) is incoherent, the
   search succeeds, returning that candidate.

-  If not, find all instances that *unify* with the target constraint,
   but do not *match* it. Such non-candidate instances might match when
   the target constraint is further instantiated. If all of them are
   incoherent, the search succeeds, returning the selected candidate; if
   not, the search fails.

Notice that these rules are not influenced by flag settings in the
client module, where the instances are *used*. These rules make it
possible for a library author to design a library that relies on
overlapping instances without the client having to know.

Note [Overlapping instances]   (NB: these notes are quite old)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

(See `GHC.Types.Id.Make.mkDictFunId' for the code in question.  Search for
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
prematurely choosing a generic instance when a more specific one
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
-}

type DFunInstType = Maybe Type
        -- Just ty   => Instantiate with this type
        -- Nothing   => Instantiate with any type of this tyvar's kind
        -- See Note [DFunInstType: instantiating types]

type InstMatch = (ClsInst, [DFunInstType])

type ClsInstLookupResult
     = ( [InstMatch]     -- Successful matches
       , PotentialUnifiers  -- These don't match but do unify
       , [InstMatch] )   -- Unsafe overlapped instances under Safe Haskell
                         -- (see Note [Safe Haskell Overlapping Instances] in
                         -- GHC.Tc.Solver).

{-
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

Note [Infinitary substitution in lookup]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  class C a b
  instance C c c
  instance C d (Maybe d)
  [W] C e (Maybe e)

You would think we could just use the second instance, because the first doesn't
unify. But that's just ever so slightly wrong. The reason we check for unifiers
along with matchers is that we don't want the possibility that a type variable
instantiation could cause an instance choice to change. Yet if we have
  type family M = Maybe M
and choose (e |-> M), then both instances match. This is absurd, but we cannot
rule it out. Yet, worrying about this case is awfully inconvenient to users,
and so we pretend the problem doesn't exist, by considering a lookup that runs into
this occurs-check issue to indicate that an instance surely does not apply (i.e.
is like the SurelyApart case). In the brief time that we didn't treat infinitary
substitutions specially, two tickets were filed: #19044 and #19052, both trying
to do Real Work.

Why don't we just exclude any instances that are MaybeApart? Because we might
have a [W] C e (F e), where F is a type family. The second instance above does
not match, but it should be included as a future possibility. Unification will
return MaybeApart MARTypeFamily in this case.

What can go wrong with this design choice? We might get incoherence -- but not
loss of type safety. In particular, if we have [W] C M M (for the M type family
above), then GHC might arbitrarily choose either instance, depending on how
M reduces (or doesn't).

For type families, we can't just ignore the problem (as we essentially do here),
because doing so would give us a hole in the type safety proof (as explored in
Section 6 of "Closed Type Families with Overlapping Equations", POPL'14). This
possibility of an infinitary substitution manifests as closed type families that
look like they should reduce, but don't. Users complain: #9082 and #17311. For
open type families, we actually can have unsoundness if we don't take infinitary
substitutions into account: #8162. But, luckily, for class instances, we just
risk coherence -- not great, but it seems better to give users what they likely
want. (Also, note that this problem existed for the entire decade of 201x without
anyone noticing, so it's manifestly not ruining anyone's day.)
-}

-- |Look up an instance in the given instance environment. The given class application must match exactly
-- one instance and the match may not contain any flexi type variables.  If the lookup is unsuccessful,
-- yield 'Left errorMessage'.
lookupUniqueInstEnv :: InstEnvs
                    -> Class -> [Type]
                    -> Either SDoc (ClsInst, [Type])
lookupUniqueInstEnv instEnv cls tys
  = case lookupInstEnv False instEnv cls tys of
      ([(inst, inst_tys)], _, _)
             | noFlexiVar -> Right (inst, inst_tys')
             | otherwise  -> Left $ text "flexible type variable:" <+>
                                    (ppr $ mkTyConApp (classTyCon cls) tys)
             where
               inst_tys'  = [ty | Just ty <- inst_tys]
               noFlexiVar = all isJust inst_tys
      _other -> Left $ text "instance not found" <+>
                       (ppr $ mkTyConApp (classTyCon cls) tys)

data PotentialUnifiers = NoUnifiers
                       | OneOrMoreUnifiers [ClsInst]
                       -- This list is lazy as we only look at all the unifiers when
                       -- printing an error message. It can be expensive to compute all
                       -- the unifiers because if you are matching something like C a[sk] then
                       -- all instances will unify.

instance Outputable PotentialUnifiers where
  ppr NoUnifiers = text "NoUnifiers"
  ppr xs = ppr (getPotentialUnifiers xs)

instance Semigroup PotentialUnifiers where
  NoUnifiers <> u = u
  u <> NoUnifiers = u
  u1 <> u2 = OneOrMoreUnifiers (getPotentialUnifiers u1 ++ getPotentialUnifiers u2)

instance Monoid PotentialUnifiers where
  mempty = NoUnifiers

getPotentialUnifiers :: PotentialUnifiers -> [ClsInst]
getPotentialUnifiers NoUnifiers = []
getPotentialUnifiers (OneOrMoreUnifiers cls) = cls

nullUnifiers :: PotentialUnifiers -> Bool
nullUnifiers NoUnifiers = True
nullUnifiers _ = False

lookupInstEnv' :: InstEnv          -- InstEnv to look in
               -> VisibleOrphanModules   -- But filter against this
               -> Class -> [Type]  -- What we are looking for
               -> ([InstMatch],    -- Successful matches
                   PotentialUnifiers)      -- These don't match but do unify
                                   -- (no incoherent ones in here)
-- The second component of the result pair happens when we look up
--      Foo [a]
-- in an InstEnv that has entries for
--      Foo [Int]
--      Foo [b]
-- Then which we choose would depend on the way in which 'a'
-- is instantiated.  So we report that Foo [b] is a match (mapping b->a)
-- but Foo [Int] is a unifier.  This gives the caller a better chance of
-- giving a suitable error message

lookupInstEnv' (InstEnv rm) vis_mods cls tys
  = (foldr check_match [] rough_matches, check_unifier rough_unifiers)
  where
    (rough_matches, rough_unifiers) = lookupRM' rough_tcs rm
    rough_tcs  = RML_KnownTc (className cls) : roughMatchTcsLookup tys

    --------------
    check_match :: ClsInst -> [InstMatch] -> [InstMatch]
    check_match item@(ClsInst { is_tvs = tpl_tvs, is_tys = tpl_tys }) acc
      | not (instIsVisible vis_mods item)
      = acc  -- See Note [Instance lookup and orphan instances]

      | Just subst <- tcMatchTys tpl_tys tys
      = ((item, map (lookupTyVar subst) tpl_tvs) : acc)
      | otherwise
      = acc


    check_unifier :: [ClsInst] -> PotentialUnifiers
    check_unifier [] = NoUnifiers
    check_unifier (item@ClsInst { is_tvs = tpl_tvs, is_tys = tpl_tys }:items)
      | not (instIsVisible vis_mods item)
      = check_unifier items  -- See Note [Instance lookup and orphan instances]
      | Just {} <- tcMatchTys tpl_tys tys = check_unifier items
        -- Does not match, so next check whether the things unify
        -- See Note [Overlapping instances]
        -- Ignore ones that are incoherent: Note [Incoherent instances]
      | isIncoherent item
      = check_unifier items

      | otherwise
      = assertPpr (tys_tv_set `disjointVarSet` tpl_tv_set)
                  ((ppr cls <+> ppr tys) $$
                   (ppr tpl_tvs <+> ppr tpl_tys)) $
                -- Unification will break badly if the variables overlap
                -- They shouldn't because we allocate separate uniques for them
                -- See Note [Template tyvars are fresh]
        case tcUnifyTysFG instanceBindFun tpl_tys tys of
          -- We consider MaybeApart to be a case where the instance might
          -- apply in the future. This covers an instance like C Int and
          -- a target like [W] C (F a), where F is a type family.
            SurelyApart              -> check_unifier items
              -- See Note [Infinitary substitution in lookup]
            MaybeApart MARInfinite _ -> check_unifier items
            _                        ->
              OneOrMoreUnifiers (item: getPotentialUnifiers (check_unifier items))

      where
        tpl_tv_set = mkVarSet tpl_tvs
        tys_tv_set = tyCoVarsOfTypes tys

---------------
-- This is the common way to call this function.
lookupInstEnv :: Bool              -- Check Safe Haskell overlap restrictions
              -> InstEnvs          -- External and home package inst-env
              -> Class -> [Type]   -- What we are looking for
              -> ClsInstLookupResult
-- ^ See Note [Rules for instance lookup]
-- ^ See Note [Safe Haskell Overlapping Instances] in "GHC.Tc.Solver"
-- ^ See Note [Safe Haskell Overlapping Instances Implementation] in "GHC.Tc.Solver"
lookupInstEnv check_overlap_safe
              (InstEnvs { ie_global = pkg_ie
                        , ie_local = home_ie
                        , ie_visible = vis_mods })
              cls
              tys
  = (final_matches, final_unifs, unsafe_overlapped)
  where
    (home_matches, home_unifs) = lookupInstEnv' home_ie vis_mods cls tys
    (pkg_matches,  pkg_unifs)  = lookupInstEnv' pkg_ie  vis_mods cls tys
    all_matches = home_matches ++ pkg_matches
    all_unifs   = home_unifs   `mappend` pkg_unifs
    final_matches = pruneOverlappedMatches all_matches
        -- Even if the unifs is non-empty (an error situation)
        -- we still prune the matches, so that the error message isn't
        -- misleading (complaining of multiple matches when some should be
        -- overlapped away)

    unsafe_overlapped
       = case final_matches of
           [match] -> check_safe match
           _       -> []

    -- If the selected match is incoherent, discard all unifiers
    final_unifs = case final_matches of
                    (m:_) | isIncoherent (fst m) -> NoUnifiers
                    _                            -> all_unifs

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
    check_safe (inst,_)
        = case check_overlap_safe && unsafeTopInstance inst of
                -- make sure it only overlaps instances from the same module
                True -> go [] all_matches
                -- most specific is from a trusted location.
                False -> []
        where
            go bad [] = bad
            go bad (i@(x,_):unchecked) =
                if inSameMod x || isOverlappable x
                    then go bad unchecked
                    else go (i:bad) unchecked

            inSameMod b =
                let na = getName $ getName inst
                    la = isInternalName na
                    nb = getName $ getName b
                    lb = isInternalName nb
                in (la && lb) || (nameModule na == nameModule nb)

    -- We consider the most specific instance unsafe when it both:
    --   (1) Comes from a module compiled as `Safe`
    --   (2) Is an orphan instance, OR, an instance for a MPTC
    unsafeTopInstance inst = isSafeOverlap (is_flag inst) &&
        (isOrphan (is_orphan inst) || classArity (is_cls inst) > 1)

---------------


{- Note [Instance overlap and guards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The first step is to find all instances that /match/ the constraint
we are trying to solve.  Next, using pruneOverlapped Matches, we eliminate
from that list of instances any instances that are overlapped.  For example:

(A)   instance                      C [a] where ...
(B)   instance {-# OVERLAPPING #-} C [[a] where ...
(C)   instance C (Maybe a) where

Suppose we are trying to solve C [[Bool]]. The lookup will return a list [A,B]
of the first two instances, since both match.  (The Maybe instance doesn't match,
so the lookup won't return (C).)  Then pruneOverlappedMatches removes (A),
since (B) is more specific.  So we end up with just one match, (B).

However pruneOverlappedMatches is a bit more subtle than you might think (#20946).
Recall how we go about eliminating redundant instances, as described in
Note [Rules for instance lookup].

  - When instance I1 is more specific than instance I2,
  - and either I1 is overlapping or I2 is overlappable,

then we can discard I2 in favour of I1. Note however that, as part of the instance
resolution process, we don't want to immediately discard I2, as it can still be useful.
For example, suppose we are trying to solve C [[Int]], and have instances:

  I1: instance                  C [[Int]]
  I2: instance {-# OVERLAPS #-} C [[a]]

Both instances match. I2 is both overlappable and overlapping (that's what `OVERLAPS`
means). Now I1 is more specific than I2, and I2 is overlappable, so we can discard I2.
However, we should still keep I2 around when looking up instances, because it is
overlapping and `I1` isn't: this means it can be used to eliminate other instances
that I1 can't, such as:

  I3: instance C [a]

I3 is more general than both I1 and I2, but it is not overlappable, and I1
is not overlapping. This means that we must use I2 to discard I3.

To do this, in 'insert_overlapping', on top of keeping track of matching
instances, we also keep track of /guards/, which are instances like I2
which we will discard in the end (because we have a more specific match
that overrides it) but might still be useful for eliminating other instances
(like I3 in this example).


(A) Definition of guarding instances (guards).

    To add a matching instance G as a guard, it must satisfy the following conditions:

      A1. G is overlapped by a more specific match, M,
      A2. M is not overlapping,
      A3. G is overlapping.

    This means that we eliminate G from the set of matches (it is overriden by M),
    but we keep it around until we are done with instance resolution because
    it might still be useful to eliminate other matches.

(B) Guards eliminate matches.

    There are two situations in which guards can eliminate a match:

      B1. We want to add a new instance, but it is overriden by a guard.
          We can immediately discard the instance.

          Example for B1:

            Suppose we want to solve C [[Int]], with instances:

              J1: instance                  C [[Int]]
              J2: instance {-# OVERLAPS #-} C [[a]]
              J3: instance                  C [a]

          Processing them in order: we add J1 as a match, then J2 as a guard.
          Now, when we come across J3, we can immediately discard it because
          it is overriden by the guard J2.

      B2. We have found a new guard. We must use it to discard matches
          we have already found. This is necessary because we must obtain
          the same result whether we process the instance or the guard first.

          Example for B2:

            Suppose we want to solve C [[Int]], with instances:

              K1: instance                  C [[Int]]
              K2: instance                  C [a]
              K3: instance {-# OVERLAPS #-} C [[a]]

            We start by considering K1 and K2. Neither has any overlapping flag set,
            so we end up with two matches, {K1, K2}.
            Next we look at K3: it is overriden by K1, but as K1 is not
            overlapping this means K3 should function as a guard.
            We must then ensure we eliminate K2 from the list of matches,
            as K3 guards against it.

(C) Adding guards.

    When we already have collected some guards, and have come across a new
    guard, we can simply add it to the existing list of guards.
    We don't need to keep the set of guards minimal, as they will simply
    be thrown away at the end: we are only interested in the matches.
    Not having a minimal set of guards does not harm us, but it makes
    the code simpler.
-}

-- | Collect class instance matches, including matches that we know
-- are overridden but might still be useful to override other instances
-- (which we call "guards").
--
-- See Note [Instance overlap and guards].
data InstMatches
  = InstMatches
  { -- | Minimal matches: we have knocked out all strictly more general
    -- matches that are overlapped by a match in this list.
    instMatches :: [InstMatch]

    -- | Guards: matches that we know we won't pick in the end,
    -- but might still be useful for ruling out other instances,
    -- as per #20946. See Note [Instance overlap and guards], (A).
  , instGuards  :: [ClsInst]
  }

instance Outputable InstMatches where
  ppr (InstMatches { instMatches = matches, instGuards = guards })
    = text "InstMatches" <+>
      braces (vcat [ text "instMatches:" <+> ppr matches
                   , text "instGuards:" <+> ppr guards ])

noMatches :: InstMatches
noMatches = InstMatches { instMatches = [], instGuards = [] }

pruneOverlappedMatches :: [InstMatch] -> [InstMatch]
-- ^ Remove from the argument list any InstMatches for which another
-- element of the list is more specific, and overlaps it, using the
-- rules of Nove [Rules for instance lookup]
pruneOverlappedMatches all_matches =
  instMatches $ foldr insert_overlapping noMatches all_matches

-- | Computes whether the first class instance overrides the second,
-- i.e. the first is more specific and can overlap the second.
--
-- More precisely, @instA `overrides` instB@ returns 'True' precisely when:
--
--   - @instA@ is more specific than @instB@,
--   - @instB@ is not more specific than @instA@,
--   - @instA@ is overlapping OR @instB@ is overlappable.
overrides :: ClsInst -> ClsInst -> Bool
new_inst `overrides` old_inst
  =  (new_inst `more_specific_than` old_inst)
  && (not $ old_inst `more_specific_than` new_inst)
  && (isOverlapping new_inst || isOverlappable old_inst)
       -- Overlap permitted if either the more specific instance
       -- is marked as overlapping, or the more general one is
       -- marked as overlappable.
       -- Latest change described in: #9242.
       -- Previous change: #3877, Dec 10.
  where
    -- `instB` can be instantiated to match `instA`
    -- or the two are equal
    instA `more_specific_than` instB
      = isJust (tcMatchTys (is_tys instB) (is_tys instA))

insert_overlapping :: InstMatch -> InstMatches -> InstMatches
-- ^ Add a new solution, knocking out strictly less specific ones
-- See Note [Rules for instance lookup] and Note [Instance overlap and guards].
--
-- /Property/: the order of insertion doesn't matter, i.e.
-- @insert_overlapping inst1 (insert_overlapping inst2 matches)@
-- gives the same result as @insert_overlapping inst2 (insert_overlapping inst1 matches)@.
insert_overlapping
  new_item@(new_inst,_)
  old@(InstMatches { instMatches = old_items, instGuards = guards })
  -- If any of the "guarding" instances override this item, discard it.
  -- See Note [Instance overlap and guards], (B1).
  | any (`overrides` new_inst) guards
  = old
  | otherwise
  = insert_overlapping_new_item old_items

  where
    insert_overlapping_new_item :: [InstMatch] -> InstMatches
    insert_overlapping_new_item []
      = InstMatches { instMatches = [new_item], instGuards = guards }
    insert_overlapping_new_item all_old_items@(old_item@(old_inst,_) : old_items)

      -- New strictly overrides old: throw out the old from the list of matches,
      -- but potentially keep it around as a guard if it can still be used
      -- to eliminate other instances.
      | new_inst `overrides` old_inst
      , InstMatches { instMatches = final_matches
                    , instGuards  = prev_guards }
                    <- insert_overlapping_new_item old_items
      = if isOverlapping new_inst || not (isOverlapping old_inst)
        -- We're adding "new_inst" as a match.
        -- If "new_inst" is not overlapping but "old_inst" is, we should
        -- keep "old_inst" around as a guard.
        -- See Note [Instance overlap and guards], (A).
        then InstMatches { instMatches = final_matches
                         , instGuards  = prev_guards }
        else InstMatches { instMatches = final_matches
                         , instGuards  = old_inst : prev_guards }
        --                               ^^^^^^^^^^^^^^^^^^^^^^
        --                    See Note [Instance overlap and guards], (C).


      -- Old strictly overrides new: throw it out from the list of matches,
      -- but potentially keep it around as a guard if it can still be used
      -- to eliminate other instances.
      | old_inst `overrides` new_inst
      = if isOverlapping old_inst || not (isOverlapping new_inst)
        -- We're discarding "new_inst", as it is overridden by "old_inst".
        -- However, it might still be useful as a guard if "old_inst" is not overlapping
        -- but "new_inst" is.
        -- See Note [Instance overlap and guards], (A).
        then InstMatches { instMatches = all_old_items
                         , instGuards  = guards }
        else InstMatches
                  -- We're adding "new_inst" as a guard, so we must prune out
                  -- any matches it overrides.
                  -- See Note [Instance overlap and guards], (B2)
                { instMatches =
                    filter
                      (\(old_inst,_) -> not (new_inst `overrides` old_inst))
                      all_old_items

                -- See Note [Instance overlap and guards], (C)
                , instGuards = new_inst : guards }

      -- Discard incoherent instances; see Note [Incoherent instances]
      | isIncoherent old_inst -- Old is incoherent; discard it
      = insert_overlapping_new_item old_items
      | isIncoherent new_inst -- New is incoherent; discard it
      = InstMatches { instMatches = all_old_items
                    , instGuards  = guards }

      -- Equal or incomparable, and neither is incoherent; keep both
      | otherwise
      , InstMatches { instMatches = final_matches
                    , instGuards  = final_guards }
                    <- insert_overlapping_new_item old_items
      = InstMatches { instMatches = old_item : final_matches
                    , instGuards  = final_guards }

{-
Note [Incoherent instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
For some classes, the choice of a particular instance does not matter, any one
is good. E.g. consider

        class D a b where { opD :: a -> b -> String }
        instance D Int b where ...
        instance D a Int where ...

        g (x::Int) = opD x x  -- Wanted: D Int Int

For such classes this should work (without having to add an "instance D Int
Int", and using -XOverlappingInstances, which would then work). This is what
-XIncoherentInstances is for: Telling GHC "I don't care which instance you use;
if you can use one, use it."

Should this logic only work when *all* candidates have the incoherent flag, or
even when all but one have it? The right choice is the latter, which can be
justified by comparing the behaviour with how -XIncoherentInstances worked when
it was only about the unify-check (Note [Overlapping instances]):

Example:
        class C a b c where foo :: (a,b,c)
        instance C [a] b Int
        instance [incoherent] [Int] b c
        instance [incoherent] C a Int c
Thanks to the incoherent flags,
        [Wanted]  C [a] b Int
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
incoherent instances as long as there are others.



************************************************************************
*                                                                      *
        Binding decisions
*                                                                      *
************************************************************************
-}

instanceBindFun :: BindFun
instanceBindFun tv _rhs_ty | isOverlappableTyVar tv = Apart
                           | otherwise              = BindMe
   -- Note [Binding when looking up instances]

{-
Note [Binding when looking up instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When looking up in the instance environment, or family-instance environment,
we are careful about multiple matches, as described above in
Note [Overlapping instances]

The target tys can contain skolem constants. For existentials and instance variables,
we can guarantee that those
are never going to be instantiated to anything, so we should not involve
them in the unification test. These are called "super skolems". Example:
        class Foo a where { op :: a -> Int }
        instance Foo a => Foo [a]       -- NB overlap
        instance Foo [Int]              -- NB overlap
        data T = forall a. Foo a => MkT a
        f :: T -> Int
        f (MkT x) = op [x,x]
The op [x,x] means we need (Foo [a]). This `a` will never be instantiated, and
so it is a super skolem. (See the use of tcInstSuperSkolTyVarsX in
GHC.Tc.Gen.Pat.tcDataConPat.) Super skolems respond True to
isOverlappableTyVar, and the use of Apart in instanceBindFun, above, means
that these will be treated as fresh constants in the unification algorithm
during instance lookup. Without this treatment, GHC would complain, saying
that the choice of instance depended on the instantiation of 'a'; but of
course it isn't *going* to be instantiated. Note that it is necessary that
the unification algorithm returns SurelyApart for these super-skolems
for GHC to be able to commit to another instance.

We do this only for super skolems.  For example we reject
        g :: forall a => [a] -> Int
        g x = op x
on the grounds that the correct instance depends on the instantiation of 'a'
-}
