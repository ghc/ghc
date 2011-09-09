%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1999
%

Analysis functions over data types.  Specficially, detecting recursive types.

This stuff is only used for source-code decls; it's recorded in interface
files for imported data types.

\begin{code}
module TcTyDecls(
        calcRecFlags,
        calcSynCycles, calcClassCycles
    ) where

#include "HsVersions.h"

import TypeRep
import HsSyn
import RnHsSyn
import Type
import HscTypes
import TyCon
import DataCon
import Name
import NameEnv
import NameSet
import Digraph
import BasicTypes
import SrcLoc
import Maybes( mapCatMaybes )
import Util ( isSingleton )
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
        from a type, whereas we have extractHsTyNames already

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
mkSynEdges syn_decls = [ (ldecl, unLoc (tcdLName decl),
                                 mk_syn_edges (tcdSynRhs decl))
                       | ldecl@(L _ decl) <- syn_decls ]
  where
    mk_syn_edges rhs = [ tc | tc <- nameSetToList (extractHsTyNames rhs),
                              not (isTyVarName tc) ]

calcSynCycles :: [LTyClDecl Name] -> [SCC (LTyClDecl Name)]
calcSynCycles = stronglyConnCompFromEdgedVertices . mkSynEdges

-- We can't allow cycles via superclasses because it would result in the
-- type checker looping when it canonicalises a class constraint (superclasses
-- are added during canonicalisation)
--
-- It is OK for superclasses to be type synonyms for other classes, so look for cycles
-- through there too.
calcClassCycles :: [LTyClDecl Name] -> [LTyClDecl Name] -> [[LTyClDecl Name]]
calcClassCycles syn_decls alg_decls
  = [decls | CyclicSCC decls <- stronglyConnCompFromEdgedVertices (mkSynEdges syn_decls ++ cls_edges)]
  where
    cls_edges = [ (ldecl, unLoc (tcdLName decl),
                          mk_cls_edges (unLoc (tcdCtxt decl)))
                | ldecl@(L _ decl) <- alg_decls, isClassDecl decl ]

    mk_cls_edges :: HsContext Name -> [Name]
    mk_cls_edges ctxt = [ tc | tc <- nameSetToList (extractHsTyNames_s ctxt)
                             , not (isTyVarName tc) ]
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
calcRecFlags :: ModDetails -> [TyThing] -> (Name -> RecFlag)
-- The 'boot_names' are the things declared in M.hi-boot, if M is the current module.
-- Any type constructors in boot_names are automatically considered loop breakers
calcRecFlags boot_details tyclss
  = is_rec
  where
    is_rec n | n `elemNameSet` rec_names = Recursive
             | otherwise                 = NonRecursive

    boot_name_set = availsToNameSet (md_exports boot_details)
    rec_names = boot_name_set     `unionNameSets`
                nt_loop_breakers  `unionNameSets`
                prod_loop_breakers

    all_tycons = [ tc | tc <- mapCatMaybes getTyCon tyclss
                           -- Recursion of newtypes/data types can happen via
                           -- the class TyCon, so tyclss includes the class tycons
                      , not (tyConName tc `elemNameSet` boot_name_set) ]
                           -- Remove the boot_name_set because they are going
                           -- to be loop breakers regardless.

        -------------------------------------------------
        --                      NOTE
        -- These edge-construction loops rely on
        -- every loop going via tyclss, the types and classes
        -- in the module being compiled.  Stuff in interface
        -- files should be correctly marked.  If not (e.g. a
        -- type synonym in a hi-boot file) we can get an infinite
        -- loop.  We could program round this, but it'd make the code
        -- rather less nice, so I'm not going to do that yet.

    single_con_tycons = filter (isSingleton . tyConDataCons) all_tycons
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
     go (TyVarTy _)                = emptyNameEnv
     go (TyConApp tc tys)          = go_tc tc tys
     go (AppTy a b)                = go a `plusNameEnv` go b
     go (FunTy a b)                = go a `plusNameEnv` go b
     go (ForAllTy _ ty)            = go ty

     go_tc tc tys = extendNameEnv (go_s tys) (tyConName tc) tc
     go_s tys = foldr (plusNameEnv . go) emptyNameEnv tys
\end{code}
