{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1999


Analysis functions over data types.  Specficially, detecting recursive types.

This stuff is only used for source-code decls; it's recorded in interface
files for imported data types.
-}

{-# LANGUAGE CPP #-}

module TcTyDecls(
        calcRecFlags, RecTyInfo(..),
        calcSynCycles,
        checkClassCycles,

        -- * Roles
        RoleAnnots, extractRoleAnnots, emptyRoleAnnots, lookupRoleAnnots,

        -- * Implicits
        tcAddImplicits,

        -- * Record selectors
        mkRecSelBinds, mkOneRecordSelector
    ) where

#include "HsVersions.h"

import TcRnMonad
import TcEnv
import TcTypeable( mkTypeableBinds )
import TcBinds( tcRecSelBinds )
import TyCoRep( Type(..), TyBinder(..), delBinderVar )
import TcType
import TysWiredIn( unitTy )
import MkCore( rEC_SEL_ERROR_ID )
import HsSyn
import Class
import Type
import HscTypes
import TyCon
import ConLike
import DataCon
import Name
import NameEnv
import RdrName ( mkVarUnqual )
import Id
import IdInfo
import VarEnv
import VarSet
import NameSet
import Coercion ( ltRole )
import Digraph
import BasicTypes
import SrcLoc
import Unique ( mkBuiltinUnique )
import Outputable
import Util
import Maybes
import Data.List
import Bag
import FastString

import Control.Monad

{-
************************************************************************
*                                                                      *
        Cycles in type synonym declarations
*                                                                      *
************************************************************************

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

synonymTyConsOfType :: Type -> [TyCon]
-- Does not look through type synonyms at all
-- Return a list of synonym tycons
synonymTyConsOfType ty
  = nameEnvElts (go ty)
  where
     go :: Type -> NameEnv TyCon  -- The NameEnv does duplicate elim
     go (TyVarTy v)               = emptyNameEnv
     go (TyConApp tc tys)         = go_tc tc tys
     go (AppTy a b)               = go a `plusNameEnv` go b
     go (FunTy a b)               = go a `plusNameEnv` go b
     go (ForAllTy _ ty)           = go ty

     go_tc tc tys | isTypeSynonymTyCon tc = extendNameEnv (go_s tys)
                                                          (tyConName tc) tc
                  | otherwise             = go_s tys
     go_s tys = foldr (plusNameEnv . go) emptyNameEnv tys
---------------------------------------- END NOTE ]
-}

mkSynEdges :: [LTyClDecl Name] -> [(LTyClDecl Name, Name, [Name])]
mkSynEdges syn_decls = [ (ldecl, name, nameSetElems fvs)
                       | ldecl@(L _ (SynDecl { tcdLName = L _ name
                                             , tcdFVs = fvs })) <- syn_decls ]

calcSynCycles :: [LTyClDecl Name] -> [SCC (LTyClDecl Name)]
calcSynCycles = stronglyConnCompFromEdgedVertices . mkSynEdges

{- Note [Superclass cycle check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The superclass cycle check for C decides if we can statically
guarantee that expanding C's superclass cycles transitively is
guaranteed to terminate.  This is a Haskell98 requirement,
but one that we lift with -XUndecidableSuperClasses.

The worry is that a superclass cycle could make the type checker loop.
More precisely, with a constraint (Given or Wanted)
    C ty1 .. tyn
one approach is to instantiate all of C's superclasses, transitively.
We can only do so if that set is finite.

This potential loop occurs only through superclasses.  This, for
exmaple, is fine
  class C a where
    op :: C b => a -> b -> b
even though C's full definition uses C.

Making the check static also makes it conservative.  Eg
  type family F a
  class F a => C a
Here an instance of (F a) might mention C:
  type instance F [a] = C a
and now we'd have a loop.

The static check works like this, starting with C
  * Look at C's superclass predicates
  * If any is a type-function application,
    or is headed by a type variable, fail
  * If any has C at the head, fail
  * If any has a type class D at the head,
    make the same test with D

A tricky point is: what if there is a type variable at the head?
Consider this:
   class f (C f) => C f
   class c       => Id c
and now expand superclasses for constraint (C Id):
     C Id
 --> Id (C Id)
 --> C Id
 --> ....
Each step expands superclasses one layer, and clearly does not terminate.
-}

checkClassCycles :: Class -> Maybe SDoc
-- Nothing  <=> ok
-- Just err <=> possible cycle error
checkClassCycles cls
  = do { (definite_cycle, err) <- go (unitNameSet (getName cls))
                                     cls (mkTyVarTys (classTyVars cls))
       ; let herald | definite_cycle = text "Superclass cycle for"
                    | otherwise      = text "Potential superclass cycle for"
       ; return (vcat [ herald <+> quotes (ppr cls)
                      , nest 2 err, hint]) }
  where
    hint = text "Use UndecidableSuperClasses to accept this"

    -- Expand superclasses starting with (C a b), complaining
    -- if you find the same class a second time, or a type function
    -- or predicate headed by a type variable
    --
    -- NB: this code duplicates TcType.transSuperClasses, but
    --     with more error message generation clobber
    -- Make sure the two stay in sync.
    go :: NameSet -> Class -> [Type] -> Maybe (Bool, SDoc)
    go so_far cls tys = firstJusts $
                        map (go_pred so_far) $
                        immSuperClasses cls tys

    go_pred :: NameSet -> PredType -> Maybe (Bool, SDoc)
       -- Nothing <=> ok
       -- Just (True, err)  <=> definite cycle
       -- Just (False, err) <=> possible cycle
    go_pred so_far pred  -- NB: tcSplitTyConApp looks through synonyms
       | Just (tc, tys) <- tcSplitTyConApp_maybe pred
       = go_tc so_far pred tc tys
       | hasTyVarHead pred
       = Just (False, hang (text "one of whose superclass constraints is headed by a type variable:")
                         2 (quotes (ppr pred)))
       | otherwise
       = Nothing

    go_tc :: NameSet -> PredType -> TyCon -> [Type] -> Maybe (Bool, SDoc)
    go_tc so_far pred tc tys
      | isFamilyTyCon tc
      = Just (False, hang (text "one of whose superclass constraints is headed by a type family:")
                        2 (quotes (ppr pred)))
      | Just cls <- tyConClass_maybe tc
      = go_cls so_far cls tys
      | otherwise   -- Equality predicate, for example
      = Nothing

    go_cls :: NameSet -> Class -> [Type] -> Maybe (Bool, SDoc)
    go_cls so_far cls tys
       | cls_nm `elemNameSet` so_far
       = Just (True, text "one of whose superclasses is" <+> quotes (ppr cls))
       | isCTupleClass cls
       = go so_far cls tys
       | otherwise
       = do { (b,err) <- go  (so_far `extendNameSet` cls_nm) cls tys
          ; return (b, text "one of whose superclasses is" <+> quotes (ppr cls)
                       $$ err) }
       where
         cls_nm = getName cls

{-
************************************************************************
*                                                                      *
        Deciding which type constructors are recursive
*                                                                      *
************************************************************************

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

2.  Avoid infinite unboxing.  This has nothing to do with newtypes.
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
-}

data RecTyInfo = RTI { rti_roles      :: Name -> [Role]
                     , rti_is_rec     :: Name -> RecFlag }

calcRecFlags :: SelfBootInfo -> Bool  -- hs-boot file?
             -> RoleAnnots -> [TyCon] -> RecTyInfo
-- The 'boot_names' are the things declared in M.hi-boot, if M is the current module.
-- Any type constructors in boot_names are automatically considered loop breakers
-- Recursion of newtypes/data types can happen via
-- the class TyCon, so all_tycons includes the class tycons
calcRecFlags boot_details is_boot mrole_env all_tycons
  = RTI { rti_roles      = roles
        , rti_is_rec     = is_rec }
  where
    roles = inferRoles is_boot mrole_env all_tycons

    ----------------- Recursion calculation ----------------
    is_rec n | n `elemNameSet` rec_names = Recursive
             | otherwise                 = NonRecursive

    boot_name_set = case boot_details of
                      NoSelfBoot                -> emptyNameSet
                      SelfBoot { sb_tcs = tcs } -> tcs
    rec_names = boot_name_set     `unionNameSet`
                nt_loop_breakers  `unionNameSet`
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
        --     coreView; which calls expandSynTyCon_maybe; which uses
        --     the recursiveness of the TyCon.  Result... a black hole.
        -- YUK YUK YUK

        --------------- Newtypes ----------------------
    nt_loop_breakers = mkNameSet (findLoopBreakers nt_edges)
    is_rec_nt tc = tyConName tc  `elemNameSet` nt_loop_breakers
        -- is_rec_nt is a locally-used helper function

    nt_edges = [(t, mk_nt_edges t) | t <- new_tycons]

    mk_nt_edges nt      -- Invariant: nt is a newtype
        = [ tc | tc <- nameEnvElts (tyConsOfType (new_tc_rhs nt))
                        -- tyConsOfType looks through synonyms
               , tc `elem` new_tycons ]
           -- If not (tc `elem` new_tycons) we know that either it's a local *data* type,
           -- or it's imported.  Either way, it can't form part of a newtype cycle

        --------------- Product types ----------------------
    prod_loop_breakers = mkNameSet (findLoopBreakers prod_edges)

    prod_edges = [(tc, mk_prod_edges tc) | tc <- prod_tycons]

    mk_prod_edges tc    -- Invariant: tc is a product tycon
        = concatMap (mk_prod_edges1 tc) (dataConOrigArgTys (head (tyConDataCons tc)))

    mk_prod_edges1 ptc ty = concatMap (mk_prod_edges2 ptc) (nameEnvElts (tyConsOfType ty))

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

findLoopBreakers :: [(TyCon, [TyCon])] -> [Name]
-- Finds a set of tycons that cut all loops
findLoopBreakers deps
  = go [(tc,tc,ds) | (tc,ds) <- deps]
  where
    go edges = [ name
               | CyclicSCC ((tc,_,_) : edges') <- stronglyConnCompFromEdgedVerticesR edges,
                 name <- tyConName tc : go edges']

{-
************************************************************************
*                                                                      *
        Role annotations
*                                                                      *
************************************************************************
-}

type RoleAnnots = NameEnv (LRoleAnnotDecl Name)

extractRoleAnnots :: TyClGroup Name -> RoleAnnots
extractRoleAnnots (TyClGroup { group_roles = roles })
  = mkNameEnv [ (tycon, role_annot)
              | role_annot@(L _ (RoleAnnotDecl (L _ tycon) _)) <- roles ]

emptyRoleAnnots :: RoleAnnots
emptyRoleAnnots = emptyNameEnv

lookupRoleAnnots :: RoleAnnots -> Name -> Maybe (LRoleAnnotDecl Name)
lookupRoleAnnots = lookupNameEnv

{-
************************************************************************
*                                                                      *
        Role inference
*                                                                      *
************************************************************************

Note [Role inference]
~~~~~~~~~~~~~~~~~~~~~
The role inference algorithm datatype definitions to infer the roles on the
parameters. Although these roles are stored in the tycons, we can perform this
algorithm on the built tycons, as long as we don't peek at an as-yet-unknown
roles field! Ah, the magic of laziness.

First, we choose appropriate initial roles. For families and classes, roles
(including initial roles) are N. For datatypes, we start with the role in the
role annotation (if any), or otherwise use Phantom. This is done in
initialRoleEnv1.

The function irGroup then propagates role information until it reaches a
fixpoint, preferring N over (R or P) and R over P. To aid in this, we have a
monad RoleM, which is a combination reader and state monad. In its state are
the current RoleEnv, which gets updated by role propagation, and an update
bit, which we use to know whether or not we've reached the fixpoint. The
environment of RoleM contains the tycon whose parameters we are inferring, and
a VarEnv from parameters to their positions, so we can update the RoleEnv.
Between tycons, this reader information is missing; it is added by
addRoleInferenceInfo.

There are two kinds of tycons to consider: algebraic ones (excluding classes)
and type synonyms. (Remember, families don't participate -- all their parameters
are N.) An algebraic tycon processes each of its datacons, in turn. Note that
a datacon's universally quantified parameters might be different from the parent
tycon's parameters, so we use the datacon's univ parameters in the mapping from
vars to positions. Note also that we don't want to infer roles for existentials
(they're all at N, too), so we put them in the set of local variables. As an
optimisation, we skip any tycons whose roles are already all Nominal, as there
nowhere else for them to go. For synonyms, we just analyse their right-hand sides.

irType walks through a type, looking for uses of a variable of interest and
propagating role information. Because anything used under a phantom position
is at phantom and anything used under a nominal position is at nominal, the
irType function can assume that anything it sees is at representational. (The
other possibilities are pruned when they're encountered.)

The rest of the code is just plumbing.

How do we know that this algorithm is correct? It should meet the following
specification:

Let Z be a role context -- a mapping from variables to roles. The following
rules define the property (Z |- t : r), where t is a type and r is a role:

Z(a) = r'        r' <= r
------------------------- RCVar
Z |- a : r

---------- RCConst
Z |- T : r               -- T is a type constructor

Z |- t1 : r
Z |- t2 : N
-------------- RCApp
Z |- t1 t2 : r

forall i<=n. (r_i is R or N) implies Z |- t_i : r_i
roles(T) = r_1 .. r_n
---------------------------------------------------- RCDApp
Z |- T t_1 .. t_n : R

Z, a:N |- t : r
---------------------- RCAll
Z |- forall a:k.t : r


We also have the following rules:

For all datacon_i in type T, where a_1 .. a_n are universally quantified
and b_1 .. b_m are existentially quantified, and the arguments are t_1 .. t_p,
then if forall j<=p, a_1 : r_1 .. a_n : r_n, b_1 : N .. b_m : N |- t_j : R,
then roles(T) = r_1 .. r_n

roles(->) = R, R
roles(~#) = N, N

With -dcore-lint on, the output of this algorithm is checked in checkValidRoles,
called from checkValidTycon.

Note [Role-checking data constructor arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data T a where
    MkT :: Eq b => F a -> (a->a) -> T (G a)

Then we want to check the roles at which 'a' is used
in MkT's type.  We want to work on the user-written type,
so we need to take into account
  * the arguments:   (F a) and (a->a)
  * the context:     C a b
  * the result type: (G a)   -- this is in the eq_spec


Note [Coercions in role inference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Is (t |> co1) representationally equal to (t |> co2)? Of course they are! Changing
the kind of a type is totally irrelevant to the representation of that type. So,
we want to totally ignore coercions when doing role inference. This includes omitting
any type variables that appear in nominal positions but only within coercions.
-}

type RoleEnv    = NameEnv [Role]        -- from tycon names to roles

-- This, and any of the functions it calls, must *not* look at the roles
-- field of a tycon we are inferring roles about!
-- See Note [Role inference]
inferRoles :: Bool -> RoleAnnots -> [TyCon] -> Name -> [Role]
inferRoles is_boot annots tycons
  = let role_env  = initialRoleEnv is_boot annots tycons
        role_env' = irGroup role_env tycons in
    \name -> case lookupNameEnv role_env' name of
      Just roles -> roles
      Nothing    -> pprPanic "inferRoles" (ppr name)

initialRoleEnv :: Bool -> RoleAnnots -> [TyCon] -> RoleEnv
initialRoleEnv is_boot annots = extendNameEnvList emptyNameEnv .
                                map (initialRoleEnv1 is_boot annots)

initialRoleEnv1 :: Bool -> RoleAnnots -> TyCon -> (Name, [Role])
initialRoleEnv1 is_boot annots_env tc
  | isFamilyTyCon tc      = (name, map (const Nominal) bndrs)
  | isAlgTyCon tc         = (name, default_roles)
  | isTypeSynonymTyCon tc = (name, default_roles)
  | otherwise             = pprPanic "initialRoleEnv1" (ppr tc)
  where name         = tyConName tc
        bndrs        = tyConBinders tc
        visflags     = map binderVisibility $ take (tyConArity tc) bndrs
        num_exps     = count (== Visible) visflags

          -- if the number of annotations in the role annotation decl
          -- is wrong, just ignore it. We check this in the validity check.
        role_annots
          = case lookupNameEnv annots_env name of
              Just (L _ (RoleAnnotDecl _ annots))
                | annots `lengthIs` num_exps -> map unLoc annots
              _                              -> replicate num_exps Nothing
        default_roles = build_default_roles visflags role_annots

        build_default_roles (Visible : viss) (m_annot : ras)
          = (m_annot `orElse` default_role) : build_default_roles viss ras
        build_default_roles (_inv    : viss) ras
          = Nominal : build_default_roles viss ras
        build_default_roles [] [] = []
        build_default_roles _ _ = pprPanic "initialRoleEnv1 (2)"
                                           (vcat [ppr tc, ppr role_annots])

        default_role
          | isClassTyCon tc               = Nominal
          | is_boot && isAbstractTyCon tc = Representational
          | otherwise                     = Phantom

irGroup :: RoleEnv -> [TyCon] -> RoleEnv
irGroup env tcs
  = let (env', update) = runRoleM env $ mapM_ irTyCon tcs in
    if update
    then irGroup env' tcs
    else env'

irTyCon :: TyCon -> RoleM ()
irTyCon tc
  | isAlgTyCon tc
  = do { old_roles <- lookupRoles tc
       ; unless (all (== Nominal) old_roles) $  -- also catches data families,
                                                -- which don't want or need role inference
         irTcTyVars tc $
         do { mapM_ (irType emptyVarSet) (tyConStupidTheta tc)  -- See #8958
            ; whenIsJust (tyConClass_maybe tc) irClass
            ; mapM_ irDataCon (visibleDataCons $ algTyConRhs tc) }}

  | Just ty <- synTyConRhs_maybe tc
  = irTcTyVars tc $
    irType emptyVarSet ty

  | otherwise
  = return ()

-- any type variable used in an associated type must be Nominal
irClass :: Class -> RoleM ()
irClass cls
  = mapM_ ir_at (classATs cls)
  where
    cls_tvs    = classTyVars cls
    cls_tv_set = mkVarSet cls_tvs

    ir_at at_tc
      = mapM_ (updateRole Nominal) (varSetElems nvars)
      where nvars = (mkVarSet $ tyConTyVars at_tc) `intersectVarSet` cls_tv_set

-- See Note [Role inference]
irDataCon :: DataCon -> RoleM ()
irDataCon datacon
  = setRoleInferenceVars univ_tvs $
    irExTyVars ex_tvs $ \ ex_var_set ->
    mapM_ (irType ex_var_set)
          (map tyVarKind ex_tvs ++ eqSpecPreds eq_spec ++ theta ++ arg_tys)
      -- See Note [Role-checking data constructor arguments]
  where
    (univ_tvs, ex_tvs, eq_spec, theta, arg_tys, _res_ty)
      = dataConFullSig datacon

irType :: VarSet -> Type -> RoleM ()
irType = go
  where
    go lcls (TyVarTy tv)       = unless (tv `elemVarSet` lcls) $
                                 updateRole Representational tv
    go lcls (AppTy t1 t2)      = go lcls t1 >> markNominal lcls t2
    go lcls (TyConApp tc tys)  = do { roles <- lookupRolesX tc
                                    ; zipWithM_ (go_app lcls) roles tys }
    go lcls (ForAllTy (Named tv _) ty)
      = let lcls' = extendVarSet lcls tv in
        markNominal lcls (tyVarKind tv) >> go lcls' ty
    go lcls (ForAllTy (Anon arg) res)
      = go lcls arg >> go lcls res
    go _    (LitTy {})         = return ()
      -- See Note [Coercions in role inference]
    go lcls (CastTy ty _)      = go lcls ty
    go _    (CoercionTy _)     = return ()

    go_app _ Phantom _ = return ()                 -- nothing to do here
    go_app lcls Nominal ty = markNominal lcls ty  -- all vars below here are N
    go_app lcls Representational ty = go lcls ty

irTcTyVars :: TyCon -> RoleM a -> RoleM a
irTcTyVars tc thing
  = setRoleInferenceTc (tyConName tc) $ go (tyConTyVars tc)
  where
    go []       = thing
    go (tv:tvs) = do { markNominal emptyVarSet (tyVarKind tv)
                     ; addRoleInferenceVar tv $ go tvs }

irExTyVars :: [TyVar] -> (TyVarSet -> RoleM a) -> RoleM a
irExTyVars orig_tvs thing = go emptyVarSet orig_tvs
  where
    go lcls []       = thing lcls
    go lcls (tv:tvs) = do { markNominal lcls (tyVarKind tv)
                          ; go (extendVarSet lcls tv) tvs }

markNominal :: TyVarSet   -- local variables
            -> Type -> RoleM ()
markNominal lcls ty = let nvars = get_ty_vars ty `minusVarSet` lcls in
                      mapM_ (updateRole Nominal) (varSetElems nvars)
  where
     -- get_ty_vars gets all the tyvars (no covars!) from a type *without*
     -- recurring into coercions. Recall: coercions are totally ignored during
     -- role inference. See [Coercions in role inference]
    get_ty_vars (TyVarTy tv)     = unitVarSet tv
    get_ty_vars (AppTy t1 t2)    = get_ty_vars t1 `unionVarSet` get_ty_vars t2
    get_ty_vars (TyConApp _ tys) = foldr (unionVarSet . get_ty_vars) emptyVarSet tys
    get_ty_vars (ForAllTy bndr ty)
      = get_ty_vars ty `delBinderVar` bndr
        `unionVarSet` (tyCoVarsOfType $ binderType bndr)
    get_ty_vars (LitTy {})       = emptyVarSet
    get_ty_vars (CastTy ty _)    = get_ty_vars ty
    get_ty_vars (CoercionTy _)   = emptyVarSet

-- like lookupRoles, but with Nominal tags at the end for oversaturated TyConApps
lookupRolesX :: TyCon -> RoleM [Role]
lookupRolesX tc
  = do { roles <- lookupRoles tc
       ; return $ roles ++ repeat Nominal }

-- gets the roles either from the environment or the tycon
lookupRoles :: TyCon -> RoleM [Role]
lookupRoles tc
  = do { env <- getRoleEnv
       ; case lookupNameEnv env (tyConName tc) of
           Just roles -> return roles
           Nothing    -> return $ tyConRoles tc }

-- tries to update a role; won't ever update a role "downwards"
updateRole :: Role -> TyVar -> RoleM ()
updateRole role tv
  = do { var_ns <- getVarNs
       ; name <- getTyConName
       ; case lookupVarEnv var_ns tv of
           Nothing -> pprPanic "updateRole" (ppr name $$ ppr tv $$ ppr var_ns)
           Just n  -> updateRoleEnv name n role }

-- the state in the RoleM monad
data RoleInferenceState = RIS { role_env  :: RoleEnv
                              , update    :: Bool }

-- the environment in the RoleM monad
type VarPositions = VarEnv Int

-- See [Role inference]
newtype RoleM a = RM { unRM :: Maybe Name   -- of the tycon
                            -> VarPositions
                            -> Int          -- size of VarPositions
                            -> RoleInferenceState
                            -> (a, RoleInferenceState) }

instance Functor RoleM where
    fmap = liftM

instance Applicative RoleM where
    pure x = RM $ \_ _ _ state -> (x, state)
    (<*>) = ap

instance Monad RoleM where
  a >>= f  = RM $ \m_info vps nvps state ->
                  let (a', state') = unRM a m_info vps nvps state in
                  unRM (f a') m_info vps nvps state'

runRoleM :: RoleEnv -> RoleM () -> (RoleEnv, Bool)
runRoleM env thing = (env', update)
  where RIS { role_env = env', update = update }
          = snd $ unRM thing Nothing emptyVarEnv 0 state
        state = RIS { role_env  = env
                    , update    = False }

setRoleInferenceTc :: Name -> RoleM a -> RoleM a
setRoleInferenceTc name thing = RM $ \m_name vps nvps state ->
                                ASSERT( isNothing m_name )
                                ASSERT( isEmptyVarEnv vps )
                                ASSERT( nvps == 0 )
                                unRM thing (Just name) vps nvps state

addRoleInferenceVar :: TyVar -> RoleM a -> RoleM a
addRoleInferenceVar tv thing
  = RM $ \m_name vps nvps state ->
    ASSERT( isJust m_name )
    unRM thing m_name (extendVarEnv vps tv nvps) (nvps+1) state

setRoleInferenceVars :: [TyVar] -> RoleM a -> RoleM a
setRoleInferenceVars tvs thing
  = RM $ \m_name _vps _nvps state ->
    ASSERT( isJust m_name )
    unRM thing m_name (mkVarEnv (zip tvs [0..])) (panic "setRoleInferenceVars")
         state

getRoleEnv :: RoleM RoleEnv
getRoleEnv = RM $ \_ _ _ state@(RIS { role_env = env }) -> (env, state)

getVarNs :: RoleM VarPositions
getVarNs = RM $ \_ vps _ state -> (vps, state)

getTyConName :: RoleM Name
getTyConName = RM $ \m_name _ _ state ->
                    case m_name of
                      Nothing   -> panic "getTyConName"
                      Just name -> (name, state)

updateRoleEnv :: Name -> Int -> Role -> RoleM ()
updateRoleEnv name n role
  = RM $ \_ _ _ state@(RIS { role_env = role_env }) -> ((),
         case lookupNameEnv role_env name of
           Nothing -> pprPanic "updateRoleEnv" (ppr name)
           Just roles -> let (before, old_role : after) = splitAt n roles in
                         if role `ltRole` old_role
                         then let roles' = before ++ role : after
                                  role_env' = extendNameEnv role_env name roles' in
                              RIS { role_env = role_env', update = True }
                         else state )


{- *********************************************************************
*                                                                      *
                Building implicits
*                                                                      *
********************************************************************* -}

tcAddImplicits :: [TyCon] -> TcM TcGblEnv
-- Given a [TyCon], add to the TcGblEnv
--   * extend the TypeEnv with their implicitTyThings
--   * extend the TypeEnv with any default method Ids
--   * add bindings for record selectors
--   * add bindings for type representations for the TyThings
tcAddImplicits tycons
  = discardWarnings $
    tcExtendGlobalEnvImplicit implicit_things  $
    tcExtendGlobalValEnv def_meth_ids          $
    do { traceTc "tcAddImplicits" $ vcat
            [ text "tycons" <+> ppr tycons
            , text "implicits" <+> ppr implicit_things ]
       ; gbl_env <- mkTypeableBinds tycons
       ; gbl_env <- setGblEnv gbl_env $
                    tcRecSelBinds (mkRecSelBinds tycons)
       ; return gbl_env }
 where
   implicit_things = concatMap implicitTyConThings tycons
   def_meth_ids    = mkDefaultMethodIds tycons

mkDefaultMethodIds :: [TyCon] -> [Id]
-- We want to put the default-method Ids (both vanilla and generic)
-- into the type environment so that they are found when we typecheck
-- the filled-in default methods of each instance declaration
-- See Note [Default method Ids and Template Haskell]
mkDefaultMethodIds tycons
  = [ mkExportedVanillaId dm_name (mk_dm_ty cls sel_id dm_spec)
    | tc <- tycons
    , Just cls <- [tyConClass_maybe tc]
    , (sel_id, Just (dm_name, dm_spec)) <- classOpItems cls ]
  where
    mk_dm_ty :: Class -> Id -> DefMethSpec Type -> Type
    mk_dm_ty _ sel_id VanillaDM        = idType sel_id
    mk_dm_ty cls _   (GenericDM dm_ty) = mkSpecSigmaTy cls_tvs [pred] dm_ty
       where
         cls_tvs = classTyVars cls
         pred    = mkClassPred cls (mkTyVarTys cls_tvs)

{-
************************************************************************
*                                                                      *
                Building record selectors
*                                                                      *
************************************************************************
-}

{-
Note [Default method Ids and Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (Trac #4169):
   class Numeric a where
     fromIntegerNum :: a
     fromIntegerNum = ...

   ast :: Q [Dec]
   ast = [d| instance Numeric Int |]

When we typecheck 'ast' we have done the first pass over the class decl
(in tcTyClDecls), but we have not yet typechecked the default-method
declarations (because they can mention value declarations).  So we
must bring the default method Ids into scope first (so they can be seen
when typechecking the [d| .. |] quote, and typecheck them later.
-}

{-
************************************************************************
*                                                                      *
                Building record selectors
*                                                                      *
************************************************************************
-}

mkRecSelBinds :: [TyCon] -> HsValBinds Name
-- NB We produce *un-typechecked* bindings, rather like 'deriving'
--    This makes life easier, because the later type checking will add
--    all necessary type abstractions and applications
mkRecSelBinds tycons
  = ValBindsOut binds sigs
  where
    (sigs, binds) = unzip rec_sels
    rec_sels = map mkRecSelBind [ (tc,fld)
                                | tc <- tycons
                                , fld <- tyConFieldLabels tc ]

mkRecSelBind :: (TyCon, FieldLabel) -> (LSig Name, (RecFlag, LHsBinds Name))
mkRecSelBind (tycon, fl)
  = mkOneRecordSelector all_cons (RecSelData tycon) fl
  where
    all_cons     = map RealDataCon (tyConDataCons tycon)

mkOneRecordSelector :: [ConLike] -> RecSelParent -> FieldLabel
                    -> (LSig Name, (RecFlag, LHsBinds Name))
mkOneRecordSelector all_cons idDetails fl
  = (L loc (IdSig sel_id), (NonRecursive, unitBag (L loc sel_bind)))
  where
    loc    = getSrcSpan sel_name
    lbl      = flLabel fl
    sel_name = flSelector fl

    sel_id = mkExportedLocalId rec_details sel_name sel_ty
    rec_details = RecSelId { sel_tycon = idDetails, sel_naughty = is_naughty }

    -- Find a representative constructor, con1
    cons_w_field = conLikesWithFields all_cons [lbl]
    con1 = ASSERT( not (null cons_w_field) ) head cons_w_field

    -- Selector type; Note [Polymorphic selectors]
    field_ty   = conLikeFieldType con1 lbl
    data_tvs   = tyCoVarsOfTypeWellScoped data_ty
    data_tv_set= mkVarSet data_tvs
    is_naughty = not (tyCoVarsOfType field_ty `subVarSet` data_tv_set)
    (field_tvs, field_theta, field_tau) = tcSplitSigmaTy field_ty
    sel_ty | is_naughty = unitTy  -- See Note [Naughty record selectors]
           | otherwise  = mkSpecForAllTys data_tvs          $
                          mkPhiTy (conLikeStupidTheta con1) $   -- Urgh!
                          mkFunTy data_ty                   $
                          mkSpecForAllTys field_tvs         $
                          mkPhiTy field_theta               $
                          -- req_theta is empty for normal DataCon
                          mkPhiTy req_theta                 $
                          field_tau

    -- Make the binding: sel (C2 { fld = x }) = x
    --                   sel (C7 { fld = x }) = x
    --    where cons_w_field = [C2,C7]
    sel_bind = mkTopFunBind Generated sel_lname alts
      where
        alts | is_naughty = [mkSimpleMatch [] unit_rhs]
             | otherwise =  map mk_match cons_w_field ++ deflt
    mk_match con = mkSimpleMatch [L loc (mk_sel_pat con)]
                                 (L loc (HsVar (L loc field_var)))
    mk_sel_pat con = ConPatIn (L loc (getName con)) (RecCon rec_fields)
    rec_fields = HsRecFields { rec_flds = [rec_field], rec_dotdot = Nothing }
    rec_field  = noLoc (HsRecField
                        { hsRecFieldLbl
                           = L loc (FieldOcc (L loc $ mkVarUnqual lbl) sel_name)
                        , hsRecFieldArg = L loc (VarPat (L loc field_var))
                        , hsRecPun = False })
    sel_lname = L loc sel_name
    field_var = mkInternalName (mkBuiltinUnique 1) (getOccName sel_name) loc

    -- Add catch-all default case unless the case is exhaustive
    -- We do this explicitly so that we get a nice error message that
    -- mentions this particular record selector
    deflt | all dealt_with all_cons = []
          | otherwise = [mkSimpleMatch [L loc (WildPat placeHolderType)]
                            (mkHsApp (L loc (HsVar
                                            (L loc (getName rEC_SEL_ERROR_ID))))
                                     (L loc (HsLit msg_lit)))]

        -- Do not add a default case unless there are unmatched
        -- constructors.  We must take account of GADTs, else we
        -- get overlap warning messages from the pattern-match checker
        -- NB: we need to pass type args for the *representation* TyCon
        --     to dataConCannotMatch, hence the calculation of inst_tys
        --     This matters in data families
        --              data instance T Int a where
        --                 A :: { fld :: Int } -> T Int Bool
        --                 B :: { fld :: Int } -> T Int Char
    dealt_with :: ConLike -> Bool
    dealt_with (PatSynCon _) = False -- We can't predict overlap
    dealt_with con@(RealDataCon dc) =
      con `elem` cons_w_field || dataConCannotMatch inst_tys dc

    (univ_tvs, _, eq_spec, _, req_theta, _, data_ty) = conLikeFullSig con1

    eq_subst = mkTvSubstPrs (map eqSpecPair eq_spec)
    inst_tys = substTyVars eq_subst univ_tvs

    unit_rhs = mkLHsTupleExpr []
    msg_lit = HsStringPrim "" (fastStringToByteString lbl)

{-
Note [Polymorphic selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We take care to build the type of a polymorphic selector in the right
order, so that visible type application works.

  data Ord a => T a = MkT { field :: forall b. (Num a, Show b) => (a, b) }

We want

  field :: forall a. Ord a => T a -> forall b. (Num a, Show b) => (a, b)

Note [Naughty record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A "naughty" field is one for which we can't define a record
selector, because an existential type variable would escape.  For example:
        data T = forall a. MkT { x,y::a }
We obviously can't define
        x (MkT v _) = v
Nevertheless we *do* put a RecSelId into the type environment
so that if the user tries to use 'x' as a selector we can bleat
helpfully, rather than saying unhelpfully that 'x' is not in scope.
Hence the sel_naughty flag, to identify record selectors that don't really exist.

In general, a field is "naughty" if its type mentions a type variable that
isn't in the result type of the constructor.  Note that this *allows*
GADT record selectors (Note [GADT record selectors]) whose types may look
like     sel :: T [a] -> a

For naughty selectors we make a dummy binding
   sel = ()
so that the later type-check will add them to the environment, and they'll be
exported.  The function is never called, because the typechecker spots the
sel_naughty field.

Note [GADT record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For GADTs, we require that all constructors with a common field 'f' have the same
result type (modulo alpha conversion).  [Checked in TcTyClsDecls.checkValidTyCon]
E.g.
        data T where
          T1 { f :: Maybe a } :: T [a]
          T2 { f :: Maybe a, y :: b  } :: T [a]
          T3 :: T Int

and now the selector takes that result type as its argument:
   f :: forall a. T [a] -> Maybe a

Details: the "real" types of T1,T2 are:
   T1 :: forall r a.   (r~[a]) => a -> T r
   T2 :: forall r a b. (r~[a]) => a -> b -> T r

So the selector loooks like this:
   f :: forall a. T [a] -> Maybe a
   f (a:*) (t:T [a])
     = case t of
         T1 c   (g:[a]~[c]) (v:Maybe c)       -> v `cast` Maybe (right (sym g))
         T2 c d (g:[a]~[c]) (v:Maybe c) (w:d) -> v `cast` Maybe (right (sym g))
         T3 -> error "T3 does not have field f"

Note the forall'd tyvars of the selector are just the free tyvars
of the result type; there may be other tyvars in the constructor's
type (e.g. 'b' in T2).

Note the need for casts in the result!

Note [Selector running example]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's OK to combine GADTs and type families.  Here's a running example:

        data instance T [a] where
          T1 { fld :: b } :: T [Maybe b]

The representation type looks like this
        data :R7T a where
          T1 { fld :: b } :: :R7T (Maybe b)

and there's coercion from the family type to the representation type
        :CoR7T a :: T [a] ~ :R7T a

The selector we want for fld looks like this:

        fld :: forall b. T [Maybe b] -> b
        fld = /\b. \(d::T [Maybe b]).
              case d `cast` :CoR7T (Maybe b) of
                T1 (x::b) -> x

The scrutinee of the case has type :R7T (Maybe b), which can be
gotten by appying the eq_spec to the univ_tvs of the data con.

-}
