-- The @FamInst@ type: family instance heads

{-# LANGUAGE CPP, GADTs #-}

module FamInst (
        FamInstEnvs, tcGetFamInstEnvs,
        checkFamInstConsistency, tcExtendLocalFamInstEnv,
        tcLookupDataFamInst, tcLookupDataFamInst_maybe,
        tcInstNewTyCon_maybe, tcTopNormaliseNewTypeTF_maybe,
        newFamInst,

        -- * Injectivity
        makeInjectivityErrors, injTyVarsOfType, injTyVarsOfTypes
    ) where

import GhcPrelude

import HscTypes
import FamInstEnv
import InstEnv( roughMatchTcs )
import Coercion
import CoreLint
import TcEvidence
import LoadIface
import TcRnMonad
import SrcLoc
import TyCon
import TcType
import CoAxiom
import DynFlags
import Module
import Outputable
import Util
import RdrName
import DataCon ( dataConName )
import Maybes
import Type
import TyCoRep
import TcMType
import Name
import Pair
import Panic
import VarSet
import Bag( Bag, unionBags, unitBag )
import Control.Monad

#include "HsVersions.h"

{- Note [The type family instance consistency story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To preserve type safety we must ensure that for any given module, all
the type family instances used either in that module or in any module
it directly or indirectly imports are consistent. For example, consider

  module F where
    type family F a

  module A where
    import F( F )
    type instance F Int = Bool
    f :: F Int -> Bool
    f x = x

  module B where
    import F( F )
    type instance F Int = Char
    g :: Char -> F Int
    g x = x

  module Bad where
    import A( f )
    import B( g )
    bad :: Char -> Int
    bad c = f (g c)

Even though module Bad never mentions the type family F at all, by
combining the functions f and g that were type checked in contradictory
type family instance environments, the function bad is able to coerce
from one type to another. So when we type check Bad we must verify that
the type family instances defined in module A are consistent with those
defined in module B.

How do we ensure that we maintain the necessary consistency?

* Call a module which defines at least one type family instance a
  "family instance module". This flag `mi_finsts` is recorded in the
  interface file.

* For every module we calculate the set of all of its direct and
  indirect dependencies that are family instance modules. This list
  `dep_finsts` is also recorded in the interface file so we can compute
  this list for a module from the lists for its direct dependencies.

* When type checking a module M we check consistency of all the type
  family instances that are either provided by its `dep_finsts` or
  defined in the module M itself. This is a pairwise check, i.e., for
  every pair of instances we must check that they are consistent.

  - For family instances coming from `dep_finsts`, this is checked in
    checkFamInstConsistency, called from tcRnImports. See Note
    [Checking family instance consistency] for details on this check
    (and in particular how we avoid having to do all these checks for
    every module we compile).

  - That leaves checking the family instances defined in M itself
    against instances defined in either M or its `dep_finsts`. This is
    checked in `tcExtendLocalFamInstEnv'.

There are four subtle points in this scheme which have not been
addressed yet.

* We have checked consistency of the family instances *defined* by M
  or its imports, but this is not by definition the same thing as the
  family instances *used* by M or its imports.  Specifically, we need to
  ensure when we use a type family instance while compiling M that this
  instance was really defined from either M or one of its imports,
  rather than being an instance that we happened to know about from
  reading an interface file in the course of compiling an unrelated
  module. Otherwise, we'll end up with no record of the fact that M
  depends on this family instance and type safety will be compromised.
  See #13102.

* It can also happen that M uses a function defined in another module
  which is not transitively imported by M. Examples include the
  desugaring of various overloaded constructs, and references inserted
  by Template Haskell splices. If that function's definition makes use
  of type family instances which are not checked against those visible
  from M, type safety can again be compromised. See #13251.

* When a module C imports a boot module B.hs-boot, we check that C's
  type family instances are compatible with those visible from
  B.hs-boot. However, C will eventually be linked against a different
  module B.hs, which might define additional type family instances which
  are inconsistent with C's. This can also lead to loss of type safety.
  See #9562.

* The call to checkFamConsistency for imported functions occurs very
  early (in tcRnImports) and that causes problems if the imported
  instances use type declared in the module being compiled.
  See Note [Loading your own hi-boot file] in LoadIface.
-}

{-
************************************************************************
*                                                                      *
                 Making a FamInst
*                                                                      *
************************************************************************
-}

-- All type variables in a FamInst must be fresh. This function
-- creates the fresh variables and applies the necessary substitution
-- It is defined here to avoid a dependency from FamInstEnv on the monad
-- code.

newFamInst :: FamFlavor -> CoAxiom Unbranched -> TcM FamInst
-- Freshen the type variables of the FamInst branches
newFamInst flavor axiom@(CoAxiom { co_ax_tc = fam_tc })
  = ASSERT2( tyCoVarsOfTypes lhs `subVarSet` tcv_set, text "lhs" <+> pp_ax )
    ASSERT2( tyCoVarsOfType  rhs `subVarSet` tcv_set, text "rhs" <+> pp_ax )
    ASSERT2( lhs_kind `eqType` rhs_kind, text "kind" <+> pp_ax $$ ppr lhs_kind $$ ppr rhs_kind )
    do { (subst, tvs') <- freshenTyVarBndrs tvs
       ; (subst, cvs') <- freshenCoVarBndrsX subst cvs
       ; dflags <- getDynFlags
       ; let lhs'     = substTys subst lhs
             rhs'     = substTy  subst rhs
             tcvs'    = tvs' ++ cvs'
       ; ifErrsM (return ()) $ -- Don't lint when there are errors, because
                               -- errors might mean TcTyCons.
                               -- See Note [Recover from validity error] in TcTyClsDecls
         when (gopt Opt_DoCoreLinting dflags) $
           -- Check that the types involved in this instance are well formed.
           -- Do /not/ expand type synonyms, for the reasons discussed in
           -- Note [Linting type synonym applications].
           case lintTypes dflags tcvs' (rhs':lhs') of
             Nothing       -> pure ()
             Just fail_msg -> pprPanic "Core Lint error" (vcat [ fail_msg
                                                               , ppr fam_tc
                                                               , ppr subst
                                                               , ppr tvs'
                                                               , ppr cvs'
                                                               , ppr lhs'
                                                               , ppr rhs' ])
       ; return (FamInst { fi_fam      = tyConName fam_tc
                         , fi_flavor   = flavor
                         , fi_tcs      = roughMatchTcs lhs
                         , fi_tvs      = tvs'
                         , fi_cvs      = cvs'
                         , fi_tys      = lhs'
                         , fi_rhs      = rhs'
                         , fi_axiom    = axiom }) }
  where
    lhs_kind = tcTypeKind (mkTyConApp fam_tc lhs)
    rhs_kind = tcTypeKind rhs
    tcv_set  = mkVarSet (tvs ++ cvs)
    pp_ax    = pprCoAxiom axiom
    CoAxBranch { cab_tvs = tvs
               , cab_cvs = cvs
               , cab_lhs = lhs
               , cab_rhs = rhs } = coAxiomSingleBranch axiom


{-
************************************************************************
*                                                                      *
        Optimised overlap checking for family instances
*                                                                      *
************************************************************************

Note [Checking family instance consistency]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For any two family instance modules that we import directly or indirectly, we
check whether the instances in the two modules are consistent, *unless* we can
be certain that the instances of the two modules have already been checked for
consistency during the compilation of modules that we import.

Why do we need to check?  Consider
   module X1 where                module X2 where
    data T1                         data T2
    type instance F T1 b = Int      type instance F a T2 = Char
    f1 :: F T1 a -> Int             f2 :: Char -> F a T2
    f1 x = x                        f2 x = x

Now if we import both X1 and X2 we could make (f2 . f1) :: Int -> Char.
Notice that neither instance is an orphan.

How do we know which pairs of modules have already been checked? For each
module M we directly import, we look up the family instance modules that M
imports (directly or indirectly), say F1, ..., FN. For any two modules
among M, F1, ..., FN, we know that the family instances defined in those
two modules are consistent--because we checked that when we compiled M.

For every other pair of family instance modules we import (directly or
indirectly), we check that they are consistent now. (So that we can be
certain that the modules in our `HscTypes.dep_finsts' are consistent.)

There is some fancy footwork regarding hs-boot module loops, see
Note [Don't check hs-boot type family instances too early]

Note [Checking family instance optimization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As explained in Note [Checking family instance consistency]
we need to ensure that every pair of transitive imports that define type family
instances is consistent.

Let's define df(A) = transitive imports of A that define type family instances
+ A, if A defines type family instances

Then for every direct import A, df(A) is already consistent.

Let's name the current module M.

We want to make sure that df(M) is consistent.
df(M) = df(D_1) U df(D_2) U ... U df(D_i) where D_1 .. D_i are direct imports.

We perform the check iteratively, maintaining a set of consistent modules 'C'
and trying to add df(D_i) to it.

The key part is how to ensure that the union C U df(D_i) is consistent.

Let's consider two modules: A and B from C U df(D_i).
There are nine possible ways to choose A and B from C U df(D_i):

             | A in C only      | A in C and B in df(D_i) | A in df(D_i) only
--------------------------------------------------------------------------------
B in C only  | Already checked  | Already checked         | Needs to be checked
             | when checking C  | when checking C         |
--------------------------------------------------------------------------------
B in C and   | Already checked  | Already checked         | Already checked when
B in df(D_i) | when checking C  | when checking C         | checking df(D_i)
--------------------------------------------------------------------------------
B in df(D_i) | Needs to be      | Already checked         | Already checked when
only         | checked          | when checking df(D_i)   | checking df(D_i)

That means to ensure that C U df(D_i) is consistent we need to check every
module from C - df(D_i) against every module from df(D_i) - C and
every module from df(D_i) - C against every module from C - df(D_i).
But since the checks are symmetric it suffices to pick A from C - df(D_i)
and B from df(D_i) - C.

In other words these are the modules we need to check:
  [ (m1, m2) | m1 <- C, m1 not in df(D_i)
             , m2 <- df(D_i), m2 not in C ]

One final thing to note here is that if there's lot of overlap between
subsequent df(D_i)'s then we expect those set differences to be small.
That situation should be pretty common in practice, there's usually
a set of utility modules that every module imports directly or indirectly.

This is basically the idea from #13092, comment:14.
-}

-- This function doesn't check ALL instances for consistency,
-- only ones that aren't involved in recursive knot-tying
-- loops; see Note [Don't check hs-boot type family instances too early].
-- We don't need to check the current module, this is done in
-- tcExtendLocalFamInstEnv.
-- See Note [The type family instance consistency story].
checkFamInstConsistency :: [Module] -> TcM ()
checkFamInstConsistency directlyImpMods
  = do { dflags     <- getDynFlags
       ; (eps, hpt) <- getEpsAndHpt
       ; traceTc "checkFamInstConsistency" (ppr directlyImpMods)
       ; let { -- Fetch the iface of a given module.  Must succeed as
               -- all directly imported modules must already have been loaded.
               modIface mod =
                 case lookupIfaceByModule dflags hpt (eps_PIT eps) mod of
                   Nothing    -> panicDoc "FamInst.checkFamInstConsistency"
                                          (ppr mod $$ pprHPT hpt)
                   Just iface -> iface

               -- Which family instance modules were checked for consistency
               -- when we compiled `mod`?
               -- Itself (if a family instance module) and its dep_finsts.
               -- This is df(D_i) from
               -- Note [Checking family instance optimization]
             ; modConsistent :: Module -> [Module]
             ; modConsistent mod =
                 if mi_finsts (modIface mod) then mod:deps else deps
                 where
                 deps = dep_finsts . mi_deps . modIface $ mod

             ; hmiModule     = mi_module . hm_iface
             ; hmiFamInstEnv = extendFamInstEnvList emptyFamInstEnv
                               . md_fam_insts . hm_details
             ; hpt_fam_insts = mkModuleEnv [ (hmiModule hmi, hmiFamInstEnv hmi)
                                           | hmi <- eltsHpt hpt]

             }

       ; checkMany hpt_fam_insts modConsistent directlyImpMods
       }
  where
    -- See Note [Checking family instance optimization]
    checkMany
      :: ModuleEnv FamInstEnv   -- home package family instances
      -> (Module -> [Module])   -- given A, modules checked when A was checked
      -> [Module]               -- modules to process
      -> TcM ()
    checkMany hpt_fam_insts modConsistent mods = go [] emptyModuleSet mods
      where
      go :: [Module] -- list of consistent modules
         -> ModuleSet -- set of consistent modules, same elements as the
                      -- list above
         -> [Module] -- modules to process
         -> TcM ()
      go _ _ [] = return ()
      go consistent consistent_set (mod:mods) = do
        sequence_
          [ check hpt_fam_insts m1 m2
          | m1 <- to_check_from_mod
            -- loop over toCheckFromMod first, it's usually smaller,
            -- it may even be empty
          , m2 <- to_check_from_consistent
          ]
        go consistent' consistent_set' mods
        where
        mod_deps_consistent =  modConsistent mod
        mod_deps_consistent_set = mkModuleSet mod_deps_consistent
        consistent' = to_check_from_mod ++ consistent
        consistent_set' =
          extendModuleSetList consistent_set to_check_from_mod
        to_check_from_consistent =
          filterOut (`elemModuleSet` mod_deps_consistent_set) consistent
        to_check_from_mod =
          filterOut (`elemModuleSet` consistent_set) mod_deps_consistent
        -- Why don't we just minusModuleSet here?
        -- We could, but doing so means one of two things:
        --
        --   1. When looping over the cartesian product we convert
        --   a set into a non-deterministicly ordered list. Which
        --   happens to be fine for interface file determinism
        --   in this case, today, because the order only
        --   determines the order of deferred checks. But such
        --   invariants are hard to keep.
        --
        --   2. When looping over the cartesian product we convert
        --   a set into a deterministically ordered list - this
        --   adds some additional cost of sorting for every
        --   direct import.
        --
        --   That also explains why we need to keep both 'consistent'
        --   and 'consistentSet'.
        --
        --   See also Note [ModuleEnv performance and determinism].
    check hpt_fam_insts m1 m2
      = do { env1' <- getFamInsts hpt_fam_insts m1
           ; env2' <- getFamInsts hpt_fam_insts m2
           -- We're checking each element of env1 against env2.
           -- The cost of that is dominated by the size of env1, because
           -- for each instance in env1 we look it up in the type family
           -- environment env2, and lookup is cheap.
           -- The code below ensures that env1 is the smaller environment.
           ; let sizeE1 = famInstEnvSize env1'
                 sizeE2 = famInstEnvSize env2'
                 (env1, env2) = if sizeE1 < sizeE2 then (env1', env2')
                                                   else (env2', env1')
           -- Note [Don't check hs-boot type family instances too early]
           -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           -- Family instance consistency checking involves checking that
           -- the family instances of our imported modules are consistent with
           -- one another; this might lead you to think that this process
           -- has nothing to do with the module we are about to typecheck.
           -- Not so!  Consider the following case:
           --
           --   -- A.hs-boot
           --   type family F a
           --
           --   -- B.hs
           --   import {-# SOURCE #-} A
           --   type instance F Int = Bool
           --
           --   -- A.hs
           --   import B
           --   type family F a
           --
           -- When typechecking A, we are NOT allowed to poke the TyThing
           -- for F until we have typechecked the family.  Thus, we
           -- can't do consistency checking for the instance in B
           -- (checkFamInstConsistency is called during renaming).
           -- Failing to defer the consistency check lead to #11062.
           --
           -- Additionally, we should also defer consistency checking when
           -- type from the hs-boot file of the current module occurs on
           -- the left hand side, as we will poke its TyThing when checking
           -- for overlap.
           --
           --   -- F.hs
           --   type family F a
           --
           --   -- A.hs-boot
           --   import F
           --   data T
           --
           --   -- B.hs
           --   import {-# SOURCE #-} A
           --   import F
           --   type instance F T = Int
           --
           --   -- A.hs
           --   import B
           --   data T = MkT
           --
           -- In fact, it is even necessary to defer for occurrences in
           -- the RHS, because we may test for *compatibility* in event
           -- of an overlap.
           --
           -- Why don't we defer ALL of the checks to later?  Well, many
           -- instances aren't involved in the recursive loop at all.  So
           -- we might as well check them immediately; and there isn't
           -- a good time to check them later in any case: every time
           -- we finish kind-checking a type declaration and add it to
           -- a context, we *then* consistency check all of the instances
           -- which mentioned that type.  We DO want to check instances
           -- as quickly as possible, so that we aren't typechecking
           -- values with inconsistent axioms in scope.
           --
           -- See also Note [Tying the knot]
           -- for why we are doing this at all.
           ; let check_now = famInstEnvElts env1
           ; mapM_ (checkForConflicts (emptyFamInstEnv, env2))           check_now
           ; mapM_ (checkForInjectivityConflicts (emptyFamInstEnv,env2)) check_now
 }

getFamInsts :: ModuleEnv FamInstEnv -> Module -> TcM FamInstEnv
getFamInsts hpt_fam_insts mod
  | Just env <- lookupModuleEnv hpt_fam_insts mod = return env
  | otherwise = do { _ <- initIfaceTcRn (loadSysInterface doc mod)
                   ; eps <- getEps
                   ; return (expectJust "checkFamInstConsistency" $
                             lookupModuleEnv (eps_mod_fam_inst_env eps) mod) }
  where
    doc = ppr mod <+> text "is a family-instance module"

{-
************************************************************************
*                                                                      *
        Lookup
*                                                                      *
************************************************************************

-}

-- | If @co :: T ts ~ rep_ty@ then:
--
-- > instNewTyCon_maybe T ts = Just (rep_ty, co)
--
-- Checks for a newtype, and for being saturated
-- Just like Coercion.instNewTyCon_maybe, but returns a TcCoercion
tcInstNewTyCon_maybe :: TyCon -> [TcType] -> Maybe (TcType, TcCoercion)
tcInstNewTyCon_maybe = instNewTyCon_maybe

-- | Like 'tcLookupDataFamInst_maybe', but returns the arguments back if
-- there is no data family to unwrap.
-- Returns a Representational coercion
tcLookupDataFamInst :: FamInstEnvs -> TyCon -> [TcType]
                    -> (TyCon, [TcType], Coercion)
tcLookupDataFamInst fam_inst_envs tc tc_args
  | Just (rep_tc, rep_args, co)
      <- tcLookupDataFamInst_maybe fam_inst_envs tc tc_args
  = (rep_tc, rep_args, co)
  | otherwise
  = (tc, tc_args, mkRepReflCo (mkTyConApp tc tc_args))

tcLookupDataFamInst_maybe :: FamInstEnvs -> TyCon -> [TcType]
                          -> Maybe (TyCon, [TcType], Coercion)
-- ^ Converts a data family type (eg F [a]) to its representation type (eg FList a)
-- and returns a coercion between the two: co :: F [a] ~R FList a.
tcLookupDataFamInst_maybe fam_inst_envs tc tc_args
  | isDataFamilyTyCon tc
  , match : _ <- lookupFamInstEnv fam_inst_envs tc tc_args
  , FamInstMatch { fim_instance = rep_fam@(FamInst { fi_axiom = ax
                                                   , fi_cvs   = cvs })
                 , fim_tys      = rep_args
                 , fim_cos      = rep_cos } <- match
  , let rep_tc = dataFamInstRepTyCon rep_fam
        co     = mkUnbranchedAxInstCo Representational ax rep_args
                                      (mkCoVarCos cvs)
  = ASSERT( null rep_cos ) -- See Note [Constrained family instances] in FamInstEnv
    Just (rep_tc, rep_args, co)

  | otherwise
  = Nothing

-- | 'tcTopNormaliseNewTypeTF_maybe' gets rid of top-level newtypes,
-- potentially looking through newtype /instances/.
--
-- It is only used by the type inference engine (specifically, when
-- solving representational equality), and hence it is careful to unwrap
-- only if the relevant data constructor is in scope.  That's why
-- it get a GlobalRdrEnv argument.
--
-- It is careful not to unwrap data/newtype instances if it can't
-- continue unwrapping.  Such care is necessary for proper error
-- messages.
--
-- It does not look through type families.
-- It does not normalise arguments to a tycon.
--
-- If the result is Just (rep_ty, (co, gres), rep_ty), then
--    co : ty ~R rep_ty
--    gres are the GREs for the data constructors that
--                          had to be in scope
tcTopNormaliseNewTypeTF_maybe :: FamInstEnvs
                              -> GlobalRdrEnv
                              -> Type
                              -> Maybe ((Bag GlobalRdrElt, TcCoercion), Type)
tcTopNormaliseNewTypeTF_maybe faminsts rdr_env ty
-- cf. FamInstEnv.topNormaliseType_maybe and Coercion.topNormaliseNewType_maybe
  = topNormaliseTypeX stepper plus ty
  where
    plus :: (Bag GlobalRdrElt, TcCoercion) -> (Bag GlobalRdrElt, TcCoercion)
         -> (Bag GlobalRdrElt, TcCoercion)
    plus (gres1, co1) (gres2, co2) = ( gres1 `unionBags` gres2
                                     , co1 `mkTransCo` co2 )

    stepper :: NormaliseStepper (Bag GlobalRdrElt, TcCoercion)
    stepper = unwrap_newtype `composeSteppers` unwrap_newtype_instance

    -- For newtype instances we take a double step or nothing, so that
    -- we don't return the representation type of the newtype instance,
    -- which would lead to terrible error messages
    unwrap_newtype_instance rec_nts tc tys
      | Just (tc', tys', co) <- tcLookupDataFamInst_maybe faminsts tc tys
      = mapStepResult (\(gres, co1) -> (gres, co `mkTransCo` co1)) $
        unwrap_newtype rec_nts tc' tys'
      | otherwise = NS_Done

    unwrap_newtype rec_nts tc tys
      | Just con <- newTyConDataCon_maybe tc
      , Just gre <- lookupGRE_Name rdr_env (dataConName con)
           -- This is where we check that the
           -- data constructor is in scope
      = mapStepResult (\co -> (unitBag gre, co)) $
        unwrapNewTypeStepper rec_nts tc tys

      | otherwise
      = NS_Done

{-
************************************************************************
*                                                                      *
        Extending the family instance environment
*                                                                      *
************************************************************************
-}

-- Add new locally-defined family instances, checking consistency with
-- previous locally-defined family instances as well as all instances
-- available from imported modules. This requires loading all of our
-- imports that define family instances (if we haven't loaded them already).
tcExtendLocalFamInstEnv :: [FamInst] -> TcM a -> TcM a

-- If we weren't actually given any instances to add, then we don't want
-- to go to the bother of loading family instance module dependencies.
tcExtendLocalFamInstEnv [] thing_inside = thing_inside

-- Otherwise proceed...
tcExtendLocalFamInstEnv fam_insts thing_inside
 = do { -- Load family-instance modules "below" this module, so that
        -- allLocalFamInst can check for consistency with them
        -- See Note [The type family instance consistency story]
        loadDependentFamInstModules fam_insts

        -- Now add the instances one by one
      ; env <- getGblEnv
      ; (inst_env', fam_insts') <- foldlM addLocalFamInst
                                       (tcg_fam_inst_env env, tcg_fam_insts env)
                                       fam_insts

      ; let env' = env { tcg_fam_insts    = fam_insts'
                       , tcg_fam_inst_env = inst_env' }
      ; setGblEnv env' thing_inside
      }

loadDependentFamInstModules :: [FamInst] -> TcM ()
-- Load family-instance modules "below" this module, so that
-- allLocalFamInst can check for consistency with them
-- See Note [The type family instance consistency story]
loadDependentFamInstModules fam_insts
 = do { env <- getGblEnv
      ; let this_mod = tcg_mod env
            imports  = tcg_imports env

            want_module mod  -- See Note [Home package family instances]
              | mod == this_mod = False
              | home_fams_only  = moduleUnitId mod == moduleUnitId this_mod
              | otherwise       = True
            home_fams_only = all (nameIsHomePackage this_mod . fi_fam) fam_insts

      ; loadModuleInterfaces (text "Loading family-instance modules") $
        filter want_module (imp_finsts imports) }

{- Note [Home package family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Optimization: If we're only defining type family instances
for type families *defined in the home package*, then we
only have to load interface files that belong to the home
package. The reason is that there's no recursion between
packages, so modules in other packages can't possibly define
instances for our type families.

(Within the home package, we could import a module M that
imports us via an hs-boot file, and thereby defines an
instance of a type family defined in this module. So we can't
apply the same logic to avoid reading any interface files at
all, when we define an instances for type family defined in
the current module.
-}

-- Check that the proposed new instance is OK,
-- and then add it to the home inst env
-- This must be lazy in the fam_inst arguments, see Note [Lazy axiom match]
-- in FamInstEnv.hs
addLocalFamInst :: (FamInstEnv,[FamInst])
                -> FamInst
                -> TcM (FamInstEnv, [FamInst])
addLocalFamInst (home_fie, my_fis) fam_inst
        -- home_fie includes home package and this module
        -- my_fies is just the ones from this module
  = do { traceTc "addLocalFamInst" (ppr fam_inst)

           -- Unlike the case of class instances, don't override existing
           -- instances in GHCi; it's unsound. See #7102.

       ; mod <- getModule
       ; traceTc "alfi" (ppr mod)

           -- Fetch imported instances, so that we report
           -- overlaps correctly.
           -- Really we ought to only check consistency with
           -- those instances which are transitively imported
           -- by the current module, rather than every instance
           -- we've ever seen. Fixing this is part of #13102.
       ; eps <- getEps
       ; let inst_envs = (eps_fam_inst_env eps, home_fie)
             home_fie' = extendFamInstEnv home_fie fam_inst

           -- Check for conflicting instance decls and injectivity violations
       ; no_conflict    <- checkForConflicts            inst_envs fam_inst
       ; injectivity_ok <- checkForInjectivityConflicts inst_envs fam_inst

       ; if no_conflict && injectivity_ok then
            return (home_fie', fam_inst : my_fis)
         else
            return (home_fie,  my_fis) }

{-
************************************************************************
*                                                                      *
        Checking an instance against conflicts with an instance env
*                                                                      *
************************************************************************

Check whether a single family instance conflicts with those in two instance
environments (one for the EPS and one for the HPT).
-}

checkForConflicts :: FamInstEnvs -> FamInst -> TcM Bool
checkForConflicts inst_envs fam_inst
  = do { let conflicts = lookupFamInstEnvConflicts inst_envs fam_inst
       ; traceTc "checkForConflicts" $
         vcat [ ppr (map fim_instance conflicts)
              , ppr fam_inst
              -- , ppr inst_envs
         ]
       ; reportConflictInstErr fam_inst conflicts
       ; return (null conflicts) }

-- | Check whether a new open type family equation can be added without
-- violating injectivity annotation supplied by the user. Returns True when
-- this is possible and False if adding this equation would violate injectivity
-- annotation.
checkForInjectivityConflicts :: FamInstEnvs -> FamInst -> TcM Bool
checkForInjectivityConflicts instEnvs famInst
    | isTypeFamilyTyCon tycon
    -- type family is injective in at least one argument
    , Injective inj <- tyConInjectivityInfo tycon = do
    { let axiom = coAxiomSingleBranch fi_ax
          conflicts = lookupFamInstEnvInjectivityConflicts inj instEnvs famInst
          -- see Note [Verifying injectivity annotation] in FamInstEnv
          errs = makeInjectivityErrors fi_ax axiom inj conflicts
    ; mapM_ (\(err, span) -> setSrcSpan span $ addErr err) errs
    ; return (null errs)
    }

    -- if there was no injectivity annotation or tycon does not represent a
    -- type family we report no conflicts
    | otherwise = return True
    where tycon = famInstTyCon famInst
          fi_ax = fi_axiom famInst

-- | Build a list of injectivity errors together with their source locations.
makeInjectivityErrors
   :: CoAxiom br   -- ^ Type family for which we generate errors
   -> CoAxBranch   -- ^ Currently checked equation (represented by axiom)
   -> [Bool]       -- ^ Injectivity annotation
   -> [CoAxBranch] -- ^ List of injectivity conflicts
   -> [(SDoc, SrcSpan)]
makeInjectivityErrors fi_ax axiom inj conflicts
  = ASSERT2( any id inj, text "No injective type variables" )
    let lhs             = coAxBranchLHS axiom
        rhs             = coAxBranchRHS axiom
        fam_tc          = coAxiomTyCon fi_ax
        are_conflicts   = not $ null conflicts
        unused_inj_tvs  = unusedInjTvsInRHS fam_tc inj lhs rhs
        inj_tvs_unused  = not $ and (isEmptyVarSet <$> unused_inj_tvs)
        tf_headed       = isTFHeaded rhs
        bare_variables  = bareTvInRHSViolated lhs rhs
        wrong_bare_rhs  = not $ null bare_variables

        err_builder herald eqns
                        = ( hang herald
                               2 (vcat (map (pprCoAxBranchUser fam_tc) eqns))
                          , coAxBranchSpan (head eqns) )
        errorIf p f     = if p then [f err_builder axiom] else []
     in    errorIf are_conflicts  (conflictInjInstErr     conflicts     )
        ++ errorIf inj_tvs_unused (unusedInjectiveVarsErr unused_inj_tvs)
        ++ errorIf tf_headed       tfHeadedErr
        ++ errorIf wrong_bare_rhs (bareVariableInRHSErr   bare_variables)


-- | Return a list of type variables that the function is injective in and that
-- do not appear on injective positions in the RHS of a family instance
-- declaration. The returned Pair includes invisible vars followed by visible ones
unusedInjTvsInRHS :: TyCon -> [Bool] -> [Type] -> Type -> Pair TyVarSet
-- INVARIANT: [Bool] list contains at least one True value
-- See Note [Verifying injectivity annotation]. This function implements fourth
-- check described there.
-- In theory, instead of implementing this whole check in this way, we could
-- attempt to unify equation with itself.  We would reject exactly the same
-- equations but this method gives us more precise error messages by returning
-- precise names of variables that are not mentioned in the RHS.
unusedInjTvsInRHS tycon injList lhs rhs =
  (`minusVarSet` injRhsVars) <$> injLHSVars
    where
      inj_pairs :: [(Type, ArgFlag)]
      -- All the injective arguments, paired with their visibility
      inj_pairs = ASSERT2( injList `equalLength` lhs
                         , ppr tycon $$ ppr injList $$ ppr lhs )
                  filterByList injList (lhs `zip` tyConArgFlags tycon lhs)

      -- set of type and kind variables in which type family is injective
      invis_lhs, vis_lhs :: [Type]
      (invis_lhs, vis_lhs) = partitionInvisibles inj_pairs

      invis_vars = tyCoVarsOfTypes invis_lhs
      Pair invis_vars' vis_vars = splitVisVarsOfTypes vis_lhs
      injLHSVars
        = Pair (invis_vars `minusVarSet` vis_vars `unionVarSet` invis_vars')
               vis_vars

      -- set of type variables appearing in the RHS on an injective position.
      -- For all returned variables we assume their associated kind variables
      -- also appear in the RHS.
      injRhsVars = injTyVarsOfType rhs

injTyVarsOfType :: TcTauType -> TcTyVarSet
-- Collect all type variables that are either arguments to a type
--   constructor or to /injective/ type families.
-- Determining the overall type determines thes variables
--
-- E.g.   Suppose F is injective in its second arg, but not its first
--        then injVarOfType (Either a (F [b] (a,c))) = {a,c}
--        Determining the overall type determines a,c but not b.
injTyVarsOfType ty
  | Just ty' <- coreView ty -- #12430
  = injTyVarsOfType ty'
injTyVarsOfType (TyVarTy v)
  = unitVarSet v `unionVarSet` injTyVarsOfType (tyVarKind v)
injTyVarsOfType (TyConApp tc tys)
  | isTypeFamilyTyCon tc
   = case tyConInjectivityInfo tc of
        NotInjective  -> emptyVarSet
        Injective inj -> injTyVarsOfTypes (filterByList inj tys)
  | otherwise
  = injTyVarsOfTypes tys
injTyVarsOfType (LitTy {})
  = emptyVarSet
injTyVarsOfType (FunTy arg res)
  = injTyVarsOfType arg `unionVarSet` injTyVarsOfType res
injTyVarsOfType (AppTy fun arg)
  = injTyVarsOfType fun `unionVarSet` injTyVarsOfType arg
-- No forall types in the RHS of a type family
injTyVarsOfType (CastTy ty _)   = injTyVarsOfType ty
injTyVarsOfType (CoercionTy {}) = emptyVarSet
injTyVarsOfType (ForAllTy {})    =
    panic "unusedInjTvsInRHS.injTyVarsOfType"

injTyVarsOfTypes :: [Type] -> VarSet
injTyVarsOfTypes tys = mapUnionVarSet injTyVarsOfType tys

-- | Is type headed by a type family application?
isTFHeaded :: Type -> Bool
-- See Note [Verifying injectivity annotation]. This function implements third
-- check described there.
isTFHeaded ty | Just ty' <- coreView ty
              = isTFHeaded ty'
isTFHeaded ty | (TyConApp tc args) <- ty
              , isTypeFamilyTyCon tc
              = args `lengthIs` tyConArity tc
isTFHeaded _  = False


-- | If a RHS is a bare type variable return a set of LHS patterns that are not
-- bare type variables.
bareTvInRHSViolated :: [Type] -> Type -> [Type]
-- See Note [Verifying injectivity annotation]. This function implements second
-- check described there.
bareTvInRHSViolated pats rhs | isTyVarTy rhs
   = filter (not . isTyVarTy) pats
bareTvInRHSViolated _ _ = []


-- | Type of functions that use error message and a list of axioms to build full
-- error message (with a source location) for injective type families.
type InjErrorBuilder = SDoc -> [CoAxBranch] -> (SDoc, SrcSpan)

-- | Build injecivity error herald common to all injectivity errors.
injectivityErrorHerald :: Bool -> SDoc
injectivityErrorHerald isSingular =
  text "Type family equation" <> s isSingular <+> text "violate" <>
  s (not isSingular) <+> text "injectivity annotation" <>
  if isSingular then dot else colon
  -- Above is an ugly hack.  We want this: "sentence. herald:" (note the dot and
  -- colon).  But if herald is empty we want "sentence:" (note the colon).  We
  -- can't test herald for emptiness so we rely on the fact that herald is empty
  -- only when isSingular is False.  If herald is non empty it must end with a
  -- colon.
    where
      s False = text "s"
      s True  = empty

-- | Build error message for a pair of equations violating an injectivity
-- annotation.
conflictInjInstErr :: [CoAxBranch] -> InjErrorBuilder -> CoAxBranch
                   -> (SDoc, SrcSpan)
conflictInjInstErr conflictingEqns errorBuilder tyfamEqn
  | confEqn : _ <- conflictingEqns
  = errorBuilder (injectivityErrorHerald False) [confEqn, tyfamEqn]
  | otherwise
  = panic "conflictInjInstErr"

-- | Build error message for equation with injective type variables unused in
-- the RHS.
unusedInjectiveVarsErr :: Pair TyVarSet -> InjErrorBuilder -> CoAxBranch
                       -> (SDoc, SrcSpan)
unusedInjectiveVarsErr (Pair invis_vars vis_vars) errorBuilder tyfamEqn
  = let (doc, loc) = errorBuilder (injectivityErrorHerald True $$ msg)
                                  [tyfamEqn]
    in (pprWithExplicitKindsWhen has_kinds doc, loc)
    where
      tvs = invis_vars `unionVarSet` vis_vars
      has_types = not $ isEmptyVarSet vis_vars
      has_kinds = not $ isEmptyVarSet invis_vars

      doc = sep [ what <+> text "variable" <>
                  pluralVarSet tvs <+> pprVarSet tvs (pprQuotedList . scopedSort)
                , text "cannot be inferred from the right-hand side." ]
      what = case (has_types, has_kinds) of
               (True, True)   -> text "Type and kind"
               (True, False)  -> text "Type"
               (False, True)  -> text "Kind"
               (False, False) -> pprPanic "mkUnusedInjectiveVarsErr" $ ppr tvs
      msg = doc $$ text "In the type family equation:"

-- | Build error message for equation that has a type family call at the top
-- level of RHS
tfHeadedErr :: InjErrorBuilder -> CoAxBranch
            -> (SDoc, SrcSpan)
tfHeadedErr errorBuilder famInst
  = errorBuilder (injectivityErrorHerald True $$
                  text "RHS of injective type family equation cannot" <+>
                  text "be a type family:") [famInst]

-- | Build error message for equation that has a bare type variable in the RHS
-- but LHS pattern is not a bare type variable.
bareVariableInRHSErr :: [Type] -> InjErrorBuilder -> CoAxBranch
                     -> (SDoc, SrcSpan)
bareVariableInRHSErr tys errorBuilder famInst
  = errorBuilder (injectivityErrorHerald True $$
                  text "RHS of injective type family equation is a bare" <+>
                  text "type variable" $$
                  text "but these LHS type and kind patterns are not bare" <+>
                  text "variables:" <+> pprQuotedList tys) [famInst]


reportConflictInstErr :: FamInst -> [FamInstMatch] -> TcRn ()
reportConflictInstErr _ []
  = return ()  -- No conflicts
reportConflictInstErr fam_inst (match1 : _)
  | FamInstMatch { fim_instance = conf_inst } <- match1
  , let sorted  = sortWith getSpan [fam_inst, conf_inst]
        fi1     = head sorted
        span    = coAxBranchSpan (coAxiomSingleBranch (famInstAxiom fi1))
  = setSrcSpan span $ addErr $
    hang (text "Conflicting family instance declarations:")
       2 (vcat [ pprCoAxBranchUser (coAxiomTyCon ax) (coAxiomSingleBranch ax)
               | fi <- sorted
               , let ax = famInstAxiom fi ])
 where
   getSpan = getSrcLoc . famInstAxiom
   -- The sortWith just arranges that instances are dislayed in order
   -- of source location, which reduced wobbling in error messages,
   -- and is better for users

tcGetFamInstEnvs :: TcM FamInstEnvs
-- Gets both the external-package inst-env
-- and the home-pkg inst env (includes module being compiled)
tcGetFamInstEnvs
  = do { eps <- getEps; env <- getGblEnv
       ; return (eps_fam_inst_env eps, tcg_fam_inst_env env) }
