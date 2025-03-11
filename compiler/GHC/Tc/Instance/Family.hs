{-# LANGUAGE GADTs, ViewPatterns, LambdaCase #-}

-- | The @FamInst@ type: family instance heads
module GHC.Tc.Instance.Family (
        FamInstEnvs, tcGetFamInstEnvs,
        checkFamInstConsistency, tcExtendLocalFamInstEnv,
        tcLookupDataFamInst, tcLookupDataFamInst_maybe,
        tcInstNewTyCon_maybe, tcTopNormaliseNewTypeTF_maybe,

        -- * Injectivity
        reportInjectivityErrors, reportConflictingInjectivityErrs
    ) where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Driver.Env

import GHC.Core.FamInstEnv
import GHC.Core.Coercion
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom
import GHC.Core.DataCon ( dataConName )
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs

import GHC.Iface.Load

import GHC.Tc.Errors.Types
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType

import GHC.Unit.External
import GHC.Unit.Module
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Deps

import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.Var.Set

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.FV

import GHC.Data.Bag( Bag, unionBags, unitBag )
import GHC.Data.Maybe

import Control.Monad
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE
import Data.Function ( on )

import qualified GHC.LanguageExtensions  as LangExt
import Data.List (sortOn)
import qualified GHC.Unit.Home.Graph as HUG

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
  See Note [Loading your own hi-boot file] in GHC.Iface.Load.
-}

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
certain that the modules in our `GHC.Driver.Env.dep_finsts' are consistent.)

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

Note [Order of type family consistency checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider a module M which imports modules A, B and C, all defining (open) type
family instances.

We can waste a lot of work in type family consistency checking depending on the
order in which the modules are processed.

Suppose for example that C imports A and B. When we compiled C, we will have
checked A and B for consistency against eachother. This means that, when
processing the imports of M to check type family instance consistency:

* if C is processed first, then A and B will not need to be checked for
  consistency against eachother again,
* if we process A and B before C,then the
  consistency checks between A and B will be performed again. This is wasted
  work, as we already performed them for C.

This can make a significant difference. Keeping the nomenclature of the above
example for illustration, we have observed situations in practice in which the
compilation time of M goes from 1 second (the "processing A and B first" case)
down to 80 milliseconds (the "processing C first" case).

Clearly we should engineer that C is checked before B and A, but by what scheme?

A simple one is to observe that if a module M is in the transitive closure of X
then the size of the consistent family set of M is less than or equal to size
of the consistent family set of X.

Therefore, by sorting the imports by the size of the consistent family set and
processing the largest first, we make sure to process modules in topological
order.

For a particular project, without this change we did 40 million checks and with
this change we did 22.9 million checks. This is significant as before this change
type family consistency checks accounted for 26% of total type checker allocations which
was reduced to 15%.

See tickets #25554 for discussion about this exact issue and #25555 for
why we still do redundant checks.

-}

-- We don't need to check the current module, this is done in
-- tcExtendLocalFamInstEnv.
-- See Note [The type family instance consistency story].
checkFamInstConsistency :: [Module] -> TcM ()
checkFamInstConsistency directlyImpMods
  = do { (eps, hug) <- getEpsAndHug
       ; traceTc "checkFamInstConsistency" (ppr directlyImpMods)
       ; let { -- Fetch the iface of a given module.  Must succeed as
               -- all directly imported modules must already have been loaded.
               modIface mod = liftIO $
                 lookupIfaceByModule hug (eps_PIT eps) mod >>= \case
                   Nothing    -> panicDoc "FamInst.checkFamInstConsistency"
                                          (ppr mod $$ ppr (HUG.allUnits hug))
                   Just iface -> pure iface

               -- Which family instance modules were checked for consistency
               -- when we compiled `mod`?
               -- Itself (if a family instance module) and its dep_finsts.
               -- This is df(D_i) from
               -- Note [Checking family instance optimization]
             ; modConsistent :: Module -> TcM [Module]
             ; modConsistent mod = do
                 ifc <- modIface mod
                 deps <- dep_finsts . mi_deps <$> modIface mod
                 pure $
                   if mi_finsts ifc
                      then mod:deps
                      else deps


             -- Sorting the list by size has the effect of performing a topological sort.
             -- See Note [Order of type family consistency checks]
             }

       ; hpt_fam_insts <- liftIO $ HUG.allFamInstances hug
       ; debug_consistent_set <- mapM (\x -> (\y -> (x, length y)) <$> modConsistent x) directlyImpMods
       ; traceTc "init_consistent_set" (ppr debug_consistent_set)
       ; let init_consistent_set = map fst (reverse (sortOn snd debug_consistent_set))
       ; checkMany hpt_fam_insts modConsistent init_consistent_set

       }
  where
    -- See Note [Checking family instance optimization]
    checkMany
      :: ModuleEnv FamInstEnv     -- home package family instances
      -> (Module -> TcM [Module]) -- given A, modules checked when A was checked
      -> [Module]                 -- modules to process
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
        mod_deps_consistent <- modConsistent mod
        let
          mod_deps_consistent_set = mkModuleSet mod_deps_consistent
          consistent' = to_check_from_mod ++ consistent
          consistent_set' =
            extendModuleSetList consistent_set to_check_from_mod
          to_check_from_consistent =
            filterOut (`elemModuleSet` mod_deps_consistent_set) consistent
          to_check_from_mod =
            filterOut (`elemModuleSet` consistent_set) mod_deps_consistent
            -- Why don't we just minusModuleSet here (above)?
            -- We could, but doing so means one of two things:
            --
            --   1. When looping over the cartesian product we convert
            --   a set into a non-deterministically ordered list. Which
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

        traceTc "checkManySize" (vcat [text "mod:" <+> ppr mod
                                      , text "m1:" <+> ppr (length to_check_from_mod)
                                      , text "m2:" <+> ppr (length (to_check_from_consistent))
                                      , text "product:" <+> ppr (length to_check_from_mod * length to_check_from_consistent)
                                      ])
        sequence_
          [ check hpt_fam_insts m1 m2
          | m1 <- to_check_from_mod
            -- loop over toCheckFromMod first, it's usually smaller,
            -- it may even be empty
          , m2 <- to_check_from_consistent
          ]
        go consistent' consistent_set' mods
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

           ; let check_now = famInstEnvElts env1
           ; mapM_ (checkForConflicts (emptyFamInstEnv, env2))           check_now
           ; mapM_ (checkForInjectivityConflicts (emptyFamInstEnv,env2)) check_now
 }

getFamInsts :: ModuleEnv FamInstEnv -> Module -> TcM FamInstEnv
getFamInsts hpt_fam_insts mod
  | Just env <- lookupModuleEnv hpt_fam_insts mod = return env
  | otherwise = do { _ <- initIfaceTcRn (loadSysInterface doc mod)
                   ; eps <- getEps
                   ; return (expectJust $
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
  = assert (null rep_cos) $ -- See Note [Constrained family instances] in ??? (renamed?)
    Just (rep_tc, rep_args, co)

  | otherwise
  = Nothing

-- | 'tcTopNormaliseNewTypeTF_maybe' gets rid of top-level newtypes,
-- potentially looking through newtype /instances/ and type synonyms.
--
-- It is only used by the type inference engine (specifically, when
-- solving representational equality), and hence it is careful to unwrap
-- only if the relevant data constructor is in scope.  That's why
-- it gets a GlobalRdrEnv argument.
--
-- It is careful not to unwrap data/newtype instances nor synonyms
-- if it can't continue unwrapping.  Such care is necessary for proper
-- error messages.
--
-- It does not look through type families.
-- It does not normalise arguments to a tycon.
--
-- If the result is Just ((gres, co), rep_ty), then
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
      = fmap (mkTransCo co) <$> unwrap_newtype rec_nts tc' tys'
      | otherwise = NS_Done

    unwrap_newtype rec_nts tc tys
      | Just con <- newTyConDataCon_maybe tc
      , Just gre <- lookupGRE_Name rdr_env (dataConName con)
           -- This is where we check that the
           -- data constructor is in scope
      = (,) (unitBag gre) <$> unwrapNewTypeStepper rec_nts tc tys

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
              | home_fams_only  = moduleUnit mod == moduleUnit this_mod
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
-- in GHC.Core.FamInstEnv
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
       ; ((), no_errs) <- askNoErrs $
         do { checkForConflicts            inst_envs fam_inst
            ; checkForInjectivityConflicts inst_envs fam_inst
            ; checkInjectiveEquation       fam_inst
            }

       ; if no_errs then
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

-- | Checks to make sure no two family instances overlap.
checkForConflicts :: FamInstEnvs -> FamInst -> TcM ()
checkForConflicts inst_envs fam_inst
  = do { let conflicts = lookupFamInstEnvConflicts inst_envs fam_inst
       ; traceTc "checkForConflicts" $
         vcat [ ppr conflicts
              , ppr fam_inst
              -- , ppr inst_envs
         ]
       ; reportConflictInstErr fam_inst conflicts }

checkForInjectivityConflicts :: FamInstEnvs -> FamInst -> TcM ()
  -- see Note [Verifying injectivity annotation] in GHC.Core.FamInstEnv, check 1B1.
checkForInjectivityConflicts instEnvs famInst
    | isTypeFamilyTyCon tycon   -- as opposed to data family tycon
    , Injective inj <- tyConInjectivityInfo tycon
    = let conflicts = lookupFamInstEnvInjectivityConflicts inj instEnvs famInst in
      reportConflictingInjectivityErrs tycon conflicts (coAxiomSingleBranch (fi_axiom famInst))

    | otherwise
    = return ()

    where tycon = famInstTyCon famInst

-- | Check whether a new open type family equation can be added without
-- violating injectivity annotation supplied by the user. Returns True when
-- this is possible and False if adding this equation would violate injectivity
-- annotation. This looks only at the one equation; it does not look for
-- interaction between equations. Use checkForInjectivityConflicts for that.
-- Does checks (2)-(4) of Note [Verifying injectivity annotation] in "GHC.Core.FamInstEnv".
checkInjectiveEquation :: FamInst -> TcM ()
checkInjectiveEquation famInst
    | isTypeFamilyTyCon tycon
    -- type family is injective in at least one argument
    , Injective inj <- tyConInjectivityInfo tycon = do
    { dflags <- getDynFlags
    ; let axiom = coAxiomSingleBranch fi_ax
          -- see Note [Verifying injectivity annotation] in GHC.Core.FamInstEnv
    ; reportInjectivityErrors dflags fi_ax axiom inj
    }

    -- if there was no injectivity annotation or tycon does not represent a
    -- type family we report no conflicts
    | otherwise
    = return ()

    where tycon = famInstTyCon famInst
          fi_ax = fi_axiom famInst

-- | Report a list of injectivity errors together with their source locations.
-- Looks only at one equation; does not look for conflicts *among* equations.
reportInjectivityErrors
   :: DynFlags
   -> CoAxiom br   -- ^ Type family for which we generate errors
   -> CoAxBranch   -- ^ Currently checked equation (represented by axiom)
   -> [Bool]       -- ^ Injectivity annotation
   -> TcM ()
reportInjectivityErrors dflags fi_ax axiom inj
  = assertPpr (any id inj) (text "No injective type variables") $
    do let lhs             = coAxBranchLHS axiom
           rhs             = coAxBranchRHS axiom
           fam_tc          = coAxiomTyCon fi_ax
           (unused_inj_tvs, unused_vis, undec_inst_flag)
                           = unusedInjTvsInRHS dflags fam_tc lhs rhs
           inj_tvs_unused  = not $ isEmptyVarSet unused_inj_tvs
           tf_headed       = isTFHeaded rhs
           bare_variables  = bareTvInRHSViolated lhs rhs
           wrong_bare_rhs  = not $ null bare_variables

       when inj_tvs_unused $ reportUnusedInjectiveVarsErr fam_tc unused_inj_tvs
                                                          unused_vis undec_inst_flag axiom
       when tf_headed      $ reportTfHeadedErr            fam_tc axiom
       when wrong_bare_rhs $ reportBareVariableInRHSErr   fam_tc bare_variables axiom

-- | Is type headed by a type family application?
isTFHeaded :: Type -> Bool
-- See Note [Verifying injectivity annotation], case 3.
isTFHeaded ty | Just ty' <- coreView ty
              = isTFHeaded ty'
isTFHeaded ty | (TyConApp tc args) <- ty
              , isTypeFamilyTyCon tc
              = args `lengthIs` tyConArity tc
isTFHeaded _  = False


-- | If a RHS is a bare type variable return a set of LHS patterns that are not
-- bare type variables.
bareTvInRHSViolated :: [Type] -> Type -> [Type]
-- See Note [Verifying injectivity annotation], case 2.
bareTvInRHSViolated pats rhs | isTyVarTy rhs
   = filter (not . isTyVarTy) pats
bareTvInRHSViolated _ _ = []

------------------------------------------------------------------
-- Checking for the coverage condition for injective type families
------------------------------------------------------------------

{-
Note [Coverage condition for injective type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Injective Type Families paper describes how we can tell whether
or not a type family equation upholds the injectivity condition.
Briefly, consider the following:

  type family F a b = r | r -> a      -- NB: b is not injective

  type instance F ty1 ty2 = ty3

We need to make sure that all variables mentioned in ty1 are mentioned in ty3
-- that's how we know that knowing ty3 determines ty1. But they can't be
mentioned just anywhere in ty3: they must be in *injective* positions in ty3.
For example:

  type instance F a Int = Maybe (G a)

This is no good, if G is not injective. However, if G is indeed injective,
then this would appear to meet our needs. There is a trap here, though: while
knowing G a does indeed determine a, trying to compute a from G a might not
terminate. This is precisely the same problem that we have with functional
dependencies and their liberal coverage condition. Here is the test case:

  type family G a = r | r -> a
  type instance G [a] = [G a]
  [W] G alpha ~ [alpha]

We see that the equation given applies, because G alpha equals a list. So we
learn that alpha must be [beta] for some beta. We then have

  [W] G [beta] ~ [[beta]]

This can reduce to

  [W] [G beta] ~ [[beta]]

which then decomposes to

  [W] G beta ~ [beta]

right where we started. The equation G [a] = [G a] thus is dangerous: while
it does not violate the injectivity assumption, it might throw us into a loop,
with a particularly dastardly Wanted.

We thus do what functional dependencies do: require -XUndecidableInstances to
accept this.

Checking the coverage condition is not terribly hard, but we also want to produce
a nice error message. A nice error message has at least two properties:

1. If any of the variables involved are invisible or are used in an invisible context,
we want to print invisible arguments (as -fprint-explicit-kinds does).

2. If we fail to accept the equation because we're worried about non-termination,
we want to suggest UndecidableInstances.

To gather the right information, we can talk about the *usage* of a variable. Every
variable is used either visibly or invisibly, and it is either not used at all,
in a context where acceptance requires UndecidableInstances, or in a context that
does not require UndecidableInstances. If a variable is used both visibly and
invisibly, then we want to remember the fact that it was used invisibly: printing
out invisibles will be helpful for the user to understand what is going on.
If a variable is used where we need -XUndecidableInstances and where we don't,
we can similarly just remember the latter.

We thus define Visibility and NeedsUndecInstFlag below. These enumerations are
*ordered*, and we used their Ord instances. We then define VarUsage, which is just a pair
of a Visibility and a NeedsUndecInstFlag. (The visibility is irrelevant when a
variable is NotPresent, but this extra slack in the representation causes no
harm.) We finally define VarUsages as a mapping from variables to VarUsage.
Its Monoid instance combines two maps, using the Semigroup instance of VarUsage
to combine elements that are represented in both maps. In this way, we can
compositionally analyze types (and portions thereof).

To do the injectivity check:

1. We build VarUsages that represent the LHS (rather, the portion of the LHS
that is flagged as injective); each usage on the LHS is NotPresent, because we
have not yet looked at the RHS.

2. We also build a VarUsage for the RHS, done by injTyVarUsages.

3. We then combine these maps. Now, every variable in the injective components of the LHS
will be mapped to its correct usage (either NotPresent or perhaps needing
-XUndecidableInstances in order to be seen as injective).

4. We look up each var used in an injective argument on the LHS in
the map, making a list of tvs that should be determined by the RHS
but aren't.

5. We then return the set of bad variables, whether any of the bad
ones were used invisibly, and whether any bad ones need -XUndecidableInstances.
If -XUndecidableInstances is enabled, than a var that needs the flag
won't be bad, so it won't appear in this list.

6. We use all this information to produce a nice error message, (a) switching
on -fprint-explicit-kinds if appropriate and (b) telling the user about
-XUndecidableInstances if appropriate.

-}

-- | Return the set of type variables that a type family equation is
-- expected to be injective in but is not. Suppose we have @type family
-- F a b = r | r -> a@. Then any variables that appear free in the first
-- argument to F in an equation must be fixed by that equation's RHS.
-- This function returns all such variables that are not indeed fixed.
-- It also returns whether any of these variables appear invisibly
-- and whether -XUndecidableInstances would help.
-- See Note [Coverage condition for injective type families].
unusedInjTvsInRHS :: DynFlags
                  -> TyCon  -- type family
                  -> [Type] -- LHS arguments
                  -> Type   -- the RHS
                  -> ( TyVarSet
                     , HasKinds                     -- YesHasKinds <=> one or more variable is used invisibly
                     , SuggestUndecidableInstances) -- YesSuggestUndecidableInstaces <=> suggest -XUndecidableInstances
-- See Note [Verifying injectivity annotation] in GHC.Core.FamInstEnv.
-- This function implements check (4) described there, further
-- described in Note [Coverage condition for injective type families].
-- In theory (and modulo the -XUndecidableInstances wrinkle),
-- instead of implementing this whole check in this way, we could
-- attempt to unify equation with itself.  We would reject exactly the same
-- equations but this method gives us more precise error messages by returning
-- precise names of variables that are not mentioned in the RHS.
unusedInjTvsInRHS dflags tycon@(tyConInjectivityInfo -> Injective inj_list) lhs rhs =
  -- Note [Coverage condition for injective type families], step 5
  (bad_vars, hasKinds any_invisible, suggestUndecidableInstances suggest_undec)
    where
      undec_inst = xopt LangExt.UndecidableInstances dflags

      inj_lhs = filterByList inj_list lhs
      lhs_vars = tyCoVarsOfTypes inj_lhs

      rhs_inj_vars = fvVarSet $ injectiveVarsOfType undec_inst rhs

      bad_vars = lhs_vars `minusVarSet` rhs_inj_vars

      any_bad = not $ isEmptyVarSet bad_vars

      invis_vars = fvVarSet $ invisibleVarsOfTypes [mkTyConApp tycon lhs, rhs]

      any_invisible = any_bad && (bad_vars `intersectsVarSet` invis_vars)
      suggest_undec = any_bad &&
                      not undec_inst &&
                      (lhs_vars `subVarSet` fvVarSet (injectiveVarsOfType True rhs))

-- When the type family is not injective in any arguments
unusedInjTvsInRHS _ _ _ _ = (emptyVarSet, NoHasKinds, NoSuggestUndecidableInstaces)

---------------------------------------
-- Producing injectivity error messages
---------------------------------------

-- | Report error message for a pair of equations violating an injectivity
-- annotation. No error message if there are no branches.
reportConflictingInjectivityErrs :: TyCon -> [CoAxBranch] -> CoAxBranch -> TcM ()
reportConflictingInjectivityErrs _ [] _ = return ()
reportConflictingInjectivityErrs fam_tc (confEqn1:_) tyfamEqn
  = addErrs [buildInjectivityError (TcRnFamInstNotInjective InjErrRhsOverlap)
                                   fam_tc
                                   (confEqn1 :| [tyfamEqn])]

-- | Report error message for equation with injective type variables unused in
-- the RHS. Note [Coverage condition for injective type families], step 6
reportUnusedInjectiveVarsErr :: TyCon
                             -> TyVarSet
                             -> HasKinds                    -- YesHasKinds <=> print invisible arguments
                             -> SuggestUndecidableInstances -- YesSuggestUndecidableInstaces <=> suggest -XUndecidableInstances
                             -> CoAxBranch
                             -> TcM ()
reportUnusedInjectiveVarsErr fam_tc tvs has_kinds undec_inst tyfamEqn
  = let reason     = InjErrCannotInferFromRhs tvs has_kinds undec_inst
        (loc, dia) = buildInjectivityError (TcRnFamInstNotInjective reason) fam_tc (tyfamEqn :| [])
    in addErrAt loc dia

-- | Report error message for equation that has a type family call at the top
-- level of RHS
reportTfHeadedErr :: TyCon -> CoAxBranch -> TcM ()
reportTfHeadedErr fam_tc branch
  = addErrs [buildInjectivityError (TcRnFamInstNotInjective InjErrRhsCannotBeATypeFam)
                                   fam_tc
                                   (branch :| [])]

-- | Report error message for equation that has a bare type variable in the RHS
-- but LHS pattern is not a bare type variable.
reportBareVariableInRHSErr :: TyCon -> [Type] -> CoAxBranch -> TcM ()
reportBareVariableInRHSErr fam_tc tys branch
  = addErrs [buildInjectivityError (TcRnFamInstNotInjective (InjErrRhsBareTyVar tys))
                                   fam_tc
                                   (branch :| [])]

buildInjectivityError :: (TyCon -> NonEmpty CoAxBranch -> TcRnMessage)
                      -> TyCon
                      -> NonEmpty CoAxBranch
                      -> (SrcSpan, TcRnMessage)
buildInjectivityError mkErr fam_tc branches
  = ( coAxBranchSpan (NE.head branches), mkErr fam_tc branches )

reportConflictInstErr :: FamInst -> [FamInst] -> TcRn ()
reportConflictInstErr _ []
  = return ()  -- No conflicts
reportConflictInstErr fam_inst (conf_inst : _) =
   -- The sortBy just arranges that instances are displayed in order
   -- of source location, which reduced wobbling in error messages,
   -- and is better for users
  let   sorted  = NE.sortBy (SrcLoc.leftmost_smallest `on` getSpan) (fam_inst NE.:| [conf_inst])
        fi1     = NE.head sorted
        span    = coAxBranchSpan (coAxiomSingleBranch (famInstAxiom fi1))
        getSpan = getSrcSpan . famInstAxiom
  in setSrcSpan span $ addErr $ TcRnConflictingFamInstDecls sorted

tcGetFamInstEnvs :: TcM FamInstEnvs
-- Gets both the external-package inst-env
-- and the home-pkg inst env (includes module being compiled)
tcGetFamInstEnvs
  = do { eps <- getEps; env <- getGblEnv
       ; return (eps_fam_inst_env eps, tcg_fam_inst_env env) }
