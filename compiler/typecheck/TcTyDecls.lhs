%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1999
%

Analysis functions over data types.  Specficially, detecting recursive types.

This stuff is only used for source-code decls; it's recorded in interface
files for imported data types.

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcTyDecls(
        calcRecFlags, RecTyInfo(..), 
        calcSynCycles, calcClassCycles
    ) where

#include "HsVersions.h"

import TypeRep
import HsSyn
import Class
import Type
import Kind
import HscTypes
import TyCon
import DataCon
import Var
import Name
import NameEnv
import NameSet
import Avail
import Digraph
import BasicTypes
import SrcLoc
import UniqSet
import Maybes( mapCatMaybes, isJust )
import Util ( lengthIs, isSingleton )
import Data.List
\end{code}


%************************************************************************
%*                                                                      *
        Cycles in class and type synonym declarations
%*                                                                      *
%************************************************************************

Checking for class-decl loops is easy, because we don't allow class decls
in interface files.

We allow type synonyms in hi-boot files, but we *trust* hi-boot files,
so we don't check for loops that involve them.  So we only look for synonym
loops in the module being compiled.

We check for type synonym and class cycles on the *source* code.
Main reasons:

  a) Otherwise we'd need a special function to extract type-synonym tycons
     from a type, whereas we already have the free vars pinned on the decl

  b) If we checked for type synonym loops after building the TyCon, we
        can't do a hoistForAllTys on the type synonym rhs, (else we fall into
        a black hole) which seems unclean.  Apart from anything else, it'd mean
        that a type-synonym rhs could have for-alls to the right of an arrow,
        which means adding new cases to the validity checker

        Indeed, in general, checking for cycles beforehand means we need to
        be less careful about black holes through synonym cycles.

The main disadvantage is that a cycle that goes via a type synonym in an
.hi-boot file can lead the compiler into a loop, because it assumes that cycles
only occur entirely within the source code of the module being compiled.
But hi-boot files are trusted anyway, so this isn't much worse than (say)
a kind error.

[  NOTE ----------------------------------------------
If we reverse this decision, this comment came from tcTyDecl1, and should
 go back there
        -- dsHsType, not tcHsKindedType, to avoid a loop.  tcHsKindedType does hoisting,
        -- which requires looking through synonyms... and therefore goes into a loop
        -- on (erroneously) recursive synonyms.
        -- Solution: do not hoist synonyms, because they'll be hoisted soon enough
        --           when they are substituted

We'd also need to add back in this definition

synTyConsOfType :: Type -> [TyCon]
-- Does not look through type synonyms at all
-- Return a list of synonym tycons
synTyConsOfType ty
  = nameEnvElts (go ty)
  where
     go :: Type -> NameEnv TyCon  -- The NameEnv does duplicate elim
     go (TyVarTy v)               = emptyNameEnv
     go (TyConApp tc tys)         = go_tc tc tys
     go (AppTy a b)               = go a `plusNameEnv` go b
     go (FunTy a b)               = go a `plusNameEnv` go b
     go (ForAllTy _ ty)           = go ty

     go_tc tc tys | isSynTyCon tc = extendNameEnv (go_s tys) (tyConName tc) tc
                  | otherwise     = go_s tys
     go_s tys = foldr (plusNameEnv . go) emptyNameEnv tys
---------------------------------------- END NOTE ]

\begin{code}
mkSynEdges :: [LTyClDecl Name] -> [(LTyClDecl Name, Name, [Name])]
mkSynEdges syn_decls = [ (ldecl, name, nameSetToList fvs)
                       | ldecl@(L _ (SynDecl { tcdLName = L _ name
                                            , tcdFVs = fvs })) <- syn_decls ]

calcSynCycles :: [LTyClDecl Name] -> [SCC (LTyClDecl Name)]
calcSynCycles = stronglyConnCompFromEdgedVertices . mkSynEdges
\end{code}

Note [Superclass cycle check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can't allow cycles via superclasses because it would result in the
type checker looping when it canonicalises a class constraint (superclasses
are added during canonicalisation).  More precisely, given a constraint
    C ty1 .. tyn
we want to instantiate all of C's superclasses, transitively, and
that set must be finite.  So if
     class (D b, E b a) => C a b
then when we encounter the constraint
     C ty1 ty2
we'll instantiate the superclasses
     (D ty2, E ty2 ty1)
and then *their* superclasses, and so on.  This set must be finite!

It is OK for superclasses to be type synonyms for other classes, so
must "look through" type synonyms. Eg
     type X a = C [a]
     class X a => C a	-- No!  Recursive superclass!

We want definitions such as:

  class C cls a where cls a => a -> a
  class C D a => D a where

to be accepted, even though a naive acyclicity check would reject the
program as having a cycle between D and its superclass.  Why? Because
when we instantiate 
     D ty1
we get the superclas
     C D ty1
and C has no superclasses, so we have terminated with a finite set.

More precisely, the rule is this: the superclasses sup_C of a class C
are rejected iff:

  C \elem expand(sup_C)

Where expand is defined as follows:

(1)  expand(a ty1 ... tyN) = expand(ty1) \union ... \union expand(tyN)

(2)  expand(D ty1 ... tyN) = {D} 
                             \union sup_D[ty1/x1, ..., tyP/xP] 
                             \union expand(ty(P+1)) ... \union expand(tyN)
           where (D x1 ... xM) is a class, P = min(M,N)

(3)  expand(T ty1 ... tyN) = expand(ty1) \union ... \union expand(tyN)
        where T is not a class

Eqn (1) is conservative; when there's a type variable at the head,
look in all the argument types.  Eqn (2) expands superclasses; the
third component of the union is like Eqn (1).  Eqn (3) happens mainly
when the context is a (constraint) tuple, such as (Eq a, Show a).

Furthermore, expand always looks through type synonyms.

\begin{code}
calcClassCycles :: Class -> [[TyCon]]
calcClassCycles cls 
  = nubBy eqAsCycle $ 
    expandTheta (unitUniqSet cls) [classTyCon cls] (classSCTheta cls) []
  where
    -- The last TyCon in the cycle is always the same as the first
    eqAsCycle xs ys = any (xs ==) (cycles (tail ys))
    cycles xs = take n . map (take n) . tails . cycle $ xs
      where n = length xs

    -- No more superclasses to expand ==> no problems with cycles
    -- See Note [Superclass cycle check]
    expandTheta :: UniqSet Class -- Path of Classes to here in set form
                -> [TyCon]       -- Path to here
                -> ThetaType     -- Superclass work list
                -> [[TyCon]]     -- Input error paths
                -> [[TyCon]]     -- Final error paths
    expandTheta _    _    []           = id
    expandTheta seen path (pred:theta) = expandType seen path pred . expandTheta seen path theta

    expandType seen path (TyConApp tc tys)
      -- Expand unsaturated classes to their superclass theta if they are yet unseen.
      -- If they have already been seen then we have detected an error!
      | Just cls <- tyConClass_maybe tc
      , let (env, remainder) = papp (classTyVars cls) tys
            rest_tys = either (const []) id remainder
      = if cls `elementOfUniqSet` seen
         then (reverse (classTyCon cls:path):) 
              . flip (foldr (expandType seen path)) tys
         else expandTheta (addOneToUniqSet seen cls) (tc:path) 
                          (substTys (mkTopTvSubst env) (classSCTheta cls))
              . flip (foldr (expandType seen path)) rest_tys

      -- For synonyms, try to expand them: some arguments might be
      -- phantoms, after all. We can expand with impunity because at
      -- this point the type synonym cycle check has already happened.
      | Just (tvs, rhs) <- synTyConDefn_maybe tc
      , let (env, remainder) = papp tvs tys
            rest_tys = either (const []) id remainder
      = expandType seen (tc:path) (substTy (mkTopTvSubst env) rhs) 
        . flip (foldr (expandType seen path)) rest_tys

      -- For non-class, non-synonyms, just check the arguments
      | otherwise
      = flip (foldr (expandType seen path)) tys

    expandType _    _    (TyVarTy {})     = id
    expandType _    _    (LitTy {})       = id
    expandType seen path (AppTy t1 t2)    = expandType seen path t1 . expandType seen path t2
    expandType seen path (FunTy t1 t2)    = expandType seen path t1 . expandType seen path t2
    expandType seen path (ForAllTy _tv t) = expandType seen path t

    papp :: [TyVar] -> [Type] -> ([(TyVar, Type)], Either [TyVar] [Type])
    papp []       tys      = ([], Right tys)
    papp tvs      []       = ([], Left tvs)
    papp (tv:tvs) (ty:tys) = ((tv, ty):env, remainder)
      where (env, remainder) = papp tvs tys
\end{code}


%************************************************************************
%*                                                                      *
        Deciding which type constructors are recursive
%*                                                                      *
%************************************************************************

Identification of recursive TyCons
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The knot-tying parameters: @rec_details_list@ is an alist mapping @Name@s to
@TyThing@s.

Identifying a TyCon as recursive serves two purposes

1.  Avoid infinite types.  Non-recursive newtypes are treated as
"transparent", like type synonyms, after the type checker.  If we did
this for all newtypes, we'd get infinite types.  So we figure out for
each newtype whether it is "recursive", and add a coercion if so.  In
effect, we are trying to "cut the loops" by identifying a loop-breaker.

2.  Avoid infinite unboxing.  This is nothing to do with newtypes.
Suppose we have
        data T = MkT Int T
        f (MkT x t) = f t
Well, this function diverges, but we don't want the strictness analyser
to diverge.  But the strictness analyser will diverge because it looks
deeper and deeper into the structure of T.   (I believe there are
examples where the function does something sane, and the strictness
analyser still diverges, but I can't see one now.)

Now, concerning (1), the FC2 branch currently adds a coercion for ALL
newtypes.  I did this as an experiment, to try to expose cases in which
the coercions got in the way of optimisations.  If it turns out that we
can indeed always use a coercion, then we don't risk recursive types,
and don't need to figure out what the loop breakers are.

For newtype *families* though, we will always have a coercion, so they
are always loop breakers!  So you can easily adjust the current
algorithm by simply treating all newtype families as loop breakers (and
indeed type families).  I think.



For newtypes, we label some as "recursive" such that

    INVARIANT: there is no cycle of non-recursive newtypes

In any loop, only one newtype need be marked as recursive; it is
a "loop breaker".  Labelling more than necessary as recursive is OK,
provided the invariant is maintained.

A newtype M.T is defined to be "recursive" iff
        (a) it is declared in an hi-boot file (see RdrHsSyn.hsIfaceDecl)
        (b) it is declared in a source file, but that source file has a
            companion hi-boot file which declares the type
   or   (c) one can get from T's rhs to T via type
            synonyms, or non-recursive newtypes *in M*
             e.g.  newtype T = MkT (T -> Int)

(a) is conservative; declarations in hi-boot files are always
        made loop breakers. That's why in (b) we can restrict attention
        to tycons in M, because any loops through newtypes outside M
        will be broken by those newtypes
(b) ensures that a newtype is not treated as a loop breaker in one place
and later as a non-loop-breaker.  This matters in GHCi particularly, when
a newtype T might be embedded in many types in the environment, and then
T's source module is compiled.  We don't want T's recursiveness to change.

The "recursive" flag for algebraic data types is irrelevant (never consulted)
for types with more than one constructor.


An algebraic data type M.T is "recursive" iff
        it has just one constructor, and
        (a) it is declared in an hi-boot file (see RdrHsSyn.hsIfaceDecl)
        (b) it is declared in a source file, but that source file has a
            companion hi-boot file which declares the type
 or     (c) one can get from its arg types to T via type synonyms,
            or by non-recursive newtypes or non-recursive product types in M
             e.g.  data T = MkT (T -> Int) Bool
Just like newtype in fact

A type synonym is recursive if one can get from its
right hand side back to it via type synonyms.  (This is
reported as an error.)

A class is recursive if one can get from its superclasses
back to it.  (This is an error too.)

Hi-boot types
~~~~~~~~~~~~~
A data type read from an hi-boot file will have an AbstractTyCon as its AlgTyConRhs
and will respond True to isAbstractTyCon. The idea is that we treat these as if one
could get from these types to anywhere.  So when we see

        module Baz where
        import {-# SOURCE #-} Foo( T )
        newtype S = MkS T

then we mark S as recursive, just in case. What that means is that if we see

        import Baz( S )
        newtype R = MkR S

then we don't need to look inside S to compute R's recursiveness.  Since S is imported
(not from an hi-boot file), one cannot get from R back to S except via an hi-boot file,
and that means that some data type will be marked recursive along the way.  So R is
unconditionly non-recursive (i.e. there'll be a loop breaker elsewhere if necessary)

This in turn means that we grovel through fewer interface files when computing
recursiveness, because we need only look at the type decls in the module being
compiled, plus the outer structure of directly-mentioned types.

\begin{code}
data RecTyInfo = RTI { rti_promotable :: Bool
                     , rti_is_rec     :: Name -> RecFlag }

calcRecFlags :: ModDetails -> [TyThing] -> RecTyInfo
-- The 'boot_names' are the things declared in M.hi-boot, if M is the current module.
-- Any type constructors in boot_names are automatically considered loop breakers
calcRecFlags boot_details tyclss
  = RTI { rti_promotable = is_promotable
        , rti_is_rec     = is_rec }
  where
    rec_tycon_names = mkNameSet (map tyConName all_tycons)
    all_tycons = mapCatMaybes getTyCon tyclss
                   -- Recursion of newtypes/data types can happen via
                   -- the class TyCon, so tyclss includes the class tycons

    is_promotable = all (isPromotableTyCon rec_tycon_names) all_tycons

    ----------------- Recursion calculation ----------------
    is_rec n | n `elemNameSet` rec_names = Recursive
             | otherwise                 = NonRecursive

    boot_name_set = availsToNameSet (md_exports boot_details)
    rec_names = boot_name_set     `unionNameSets`
                nt_loop_breakers  `unionNameSets`
                prod_loop_breakers


        -------------------------------------------------
        --                      NOTE
        -- These edge-construction loops rely on
        -- every loop going via tyclss, the types and classes
        -- in the module being compiled.  Stuff in interface
        -- files should be correctly marked.  If not (e.g. a
        -- type synonym in a hi-boot file) we can get an infinite
        -- loop.  We could program round this, but it'd make the code
        -- rather less nice, so I'm not going to do that yet.

    single_con_tycons = [ tc | tc <- all_tycons
                             , not (tyConName tc `elemNameSet` boot_name_set)
                                 -- Remove the boot_name_set because they are 
                                 -- going to be loop breakers regardless.
                             , isSingleton (tyConDataCons tc) ]
        -- Both newtypes and data types, with exactly one data constructor

    (new_tycons, prod_tycons) = partition isNewTyCon single_con_tycons
        -- NB: we do *not* call isProductTyCon because that checks
	--     for vanilla-ness of data constructors; and that depends
	--     on empty existential type variables; and that is figured
	--     out by tcResultType; which uses tcMatchTy; which uses
	--     coreView; which calls coreExpandTyCon_maybe; which uses
	--     the recursiveness of the TyCon.  Result... a black hole.
	-- YUK YUK YUK

        --------------- Newtypes ----------------------
    nt_loop_breakers = mkNameSet (findLoopBreakers nt_edges)
    is_rec_nt tc = tyConName tc  `elemNameSet` nt_loop_breakers
        -- is_rec_nt is a locally-used helper function

    nt_edges = [(t, mk_nt_edges t) | t <- new_tycons]

    mk_nt_edges nt      -- Invariant: nt is a newtype
        = concatMap (mk_nt_edges1 nt) (tcTyConsOfType (new_tc_rhs nt))
                        -- tyConsOfType looks through synonyms

    mk_nt_edges1 _ tc
        | tc `elem` new_tycons = [tc]           -- Loop
                -- At this point we know that either it's a local *data* type,
                -- or it's imported.  Either way, it can't form part of a newtype cycle
        | otherwise = []

        --------------- Product types ----------------------
    prod_loop_breakers = mkNameSet (findLoopBreakers prod_edges)

    prod_edges = [(tc, mk_prod_edges tc) | tc <- prod_tycons]

    mk_prod_edges tc    -- Invariant: tc is a product tycon
        = concatMap (mk_prod_edges1 tc) (dataConOrigArgTys (head (tyConDataCons tc)))

    mk_prod_edges1 ptc ty = concatMap (mk_prod_edges2 ptc) (tcTyConsOfType ty)

    mk_prod_edges2 ptc tc
        | tc `elem` prod_tycons   = [tc]                -- Local product
        | tc `elem` new_tycons    = if is_rec_nt tc     -- Local newtype
                                    then []
                                    else mk_prod_edges1 ptc (new_tc_rhs tc)
                -- At this point we know that either it's a local non-product data type,
                -- or it's imported.  Either way, it can't form part of a cycle
        | otherwise = []

new_tc_rhs :: TyCon -> Type
new_tc_rhs tc = snd (newTyConRhs tc)    -- Ignore the type variables

getTyCon :: TyThing -> Maybe TyCon
getTyCon (ATyCon tc) = Just tc
getTyCon _           = Nothing

findLoopBreakers :: [(TyCon, [TyCon])] -> [Name]
-- Finds a set of tycons that cut all loops
findLoopBreakers deps
  = go [(tc,tc,ds) | (tc,ds) <- deps]
  where
    go edges = [ name
               | CyclicSCC ((tc,_,_) : edges') <- stronglyConnCompFromEdgedVerticesR edges,
                 name <- tyConName tc : go edges']
\end{code}


%************************************************************************
%*                                                                      *
                  Promotion calculation
%*                                                                      *
%************************************************************************

See Note [Checking whether a group is promotable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We only want to promote a TyCon if all its data constructors
are promotable; it'd be very odd to promote some but not others.

But the data constructors may mention this or other TyCons.

So we treat the recursive uses as all OK (ie promotable) and
do one pass to check that each TyCon is promotable.

Currently type synonyms are not promotable, though that
could change.

\begin{code}
isPromotableTyCon :: NameSet -> TyCon -> Bool
isPromotableTyCon rec_tycons tc
  =  isAlgTyCon tc    -- Only algebraic; not even synonyms
                     -- (we could reconsider the latter)
  && ok_kind (tyConKind tc)
  && case algTyConRhs tc of 
       DataTyCon { data_cons = cs } -> all ok_con cs 
       NewTyCon { data_con = c }    -> ok_con c
       AbstractTyCon {}             -> False
       DataFamilyTyCon {}           -> False

  where
    ok_kind kind = all isLiftedTypeKind args && isLiftedTypeKind res
            where  -- Checks for * -> ... -> * -> *
              (args, res) = splitKindFunTys kind

    -- See Note [Promoted data constructors] in TyCon
    ok_con con = all (isLiftedTypeKind . tyVarKind) ex_tvs
              && null eq_spec   -- No constraints
              && null theta
              && all (isPromotableType rec_tycons) orig_arg_tys
       where
         (_, ex_tvs, eq_spec, theta, orig_arg_tys, _) = dataConFullSig con


isPromotableType :: NameSet -> Type -> Bool
-- Must line up with DataCon.promoteType
-- But the function lives here because we must treat the
-- *recursive* tycons as promotable
isPromotableType rec_tcs con_arg_ty
  = go con_arg_ty
  where
    go (TyConApp tc tys) =  tys `lengthIs` tyConArity tc 
                         && (tyConName tc `elemNameSet` rec_tcs 
                             || isJust (promotableTyCon_maybe tc))
                         && all go tys
    go (FunTy arg res) 	 = go arg && go res
    go (TyVarTy {})    	 = True
    go _               	 = False
\end{code}


%************************************************************************
%*                                                                      *
        Miscellaneous funcions
%*                                                                      *
%************************************************************************

These two functions know about type representations, so they could be
in Type or TcType -- but they are very specialised to this module, so
I've chosen to put them here.

\begin{code}
tcTyConsOfType :: Type -> [TyCon]
-- tcTyConsOfType looks through all synonyms, but not through any newtypes.
-- When it finds a Class, it returns the class TyCon.  The reaons it's here
-- (not in Type.lhs) is because it is newtype-aware.
tcTyConsOfType ty
  = nameEnvElts (go ty)
  where
     go :: Type -> NameEnv TyCon  -- The NameEnv does duplicate elim
     go ty | Just ty' <- tcView ty = go ty'
     go (TyVarTy {})               = emptyNameEnv
     go (LitTy {})                 = emptyNameEnv
     go (TyConApp tc tys)          = go_tc tc tys
     go (AppTy a b)                = go a `plusNameEnv` go b
     go (FunTy a b)                = go a `plusNameEnv` go b
     go (ForAllTy _ ty)            = go ty

     go_tc tc tys = extendNameEnv (go_s tys) (tyConName tc) tc
     go_s tys = foldr (plusNameEnv . go) emptyNameEnv tys
\end{code}
