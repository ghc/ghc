-- (c) The University of Glasgow 2006
--
-- FamInstEnv: Type checked family instance declarations

{-# LANGUAGE CPP, GADTs, ScopedTypeVariables #-}

module FamInstEnv (
        FamInst(..), FamFlavor(..), famInstAxiom, famInstTyCon, famInstRHS,
        famInstsRepTyCons, famInstRepTyCon_maybe, dataFamInstRepTyCon,
        pprFamInst, pprFamInsts,
        mkImportedFamInst,

        FamInstEnvs, FamInstEnv, emptyFamInstEnv, emptyFamInstEnvs,
        extendFamInstEnv, deleteFromFamInstEnv, extendFamInstEnvList,
        identicalFamInstHead, famInstEnvElts, familyInstances, orphNamesOfFamInst,

        -- * CoAxioms
        mkCoAxBranch, mkBranchedCoAxiom, mkUnbranchedCoAxiom, mkSingleCoAxiom,
        computeAxiomIncomps,

        FamInstMatch(..),
        lookupFamInstEnv, lookupFamInstEnvConflicts, lookupFamInstEnvByTyCon,

        isDominatedBy, apartnessCheck,

        -- Injectivity
        InjectivityCheckResult(..),
        lookupFamInstEnvInjectivityConflicts, unusedInjTvsInRHS, isTFHeaded,
        bareTvInRHSViolated, injectiveBranches,

        -- Normalisation
        topNormaliseType, topNormaliseType_maybe,
        normaliseType, normaliseTcApp,
        reduceTyFamApp_maybe, chooseBranch,

        -- Flattening
        flattenTys
    ) where

#include "HsVersions.h"

import InstEnv
import Unify
import Type
import TcType ( orphNamesOfTypes )
import TypeRep
import TyCon
import Coercion
import CoAxiom
import VarSet
import VarEnv
import Name
import UniqFM
import Outputable
import Maybes
import TrieMap
import Unique
import Util
import Var
import Pair
import SrcLoc
import NameSet
import FastString

{-
************************************************************************
*                                                                      *
          Type checked family instance heads
*                                                                      *
************************************************************************

Note [FamInsts and CoAxioms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* CoAxioms and FamInsts are just like
  DFunIds  and ClsInsts

* A CoAxiom is a System-FC thing: it can relate any two types

* A FamInst is a Haskell source-language thing, corresponding
  to a type/data family instance declaration.
    - The FamInst contains a CoAxiom, which is the evidence
      for the instance

    - The LHS of the CoAxiom is always of form F ty1 .. tyn
      where F is a type family
-}

data FamInst  -- See Note [FamInsts and CoAxioms]
  = FamInst { fi_axiom  :: CoAxiom Unbranched -- The new coercion axiom
                                              -- introduced by this family
                                              -- instance
            , fi_flavor :: FamFlavor

            -- Everything below here is a redundant,
            -- cached version of the two things above
            -- except that the TyVars are freshened
            , fi_fam   :: Name          -- Family name

                -- Used for "rough matching"; same idea as for class instances
                -- See Note [Rough-match field] in InstEnv
            , fi_tcs   :: [Maybe Name]  -- Top of type args
                -- INVARIANT: fi_tcs = roughMatchTcs fi_tys

            -- Used for "proper matching"; ditto
            , fi_tvs    :: [TyVar]      -- Template tyvars for full match
                                 -- Like ClsInsts, these variables are always
                                 -- fresh. See Note [Template tyvars are fresh]
                                 -- in InstEnv
                                 -- INVARIANT: fi_tvs = coAxiomTyVars fi_axiom

            , fi_tys    :: [Type]       --   and its arg types

            , fi_rhs    :: Type         --   the RHS, with its freshened vars
            }

data FamFlavor
  = SynFamilyInst         -- A synonym family
  | DataFamilyInst TyCon  -- A data family, with its representation TyCon

-- Obtain the axiom of a family instance
famInstAxiom :: FamInst -> CoAxiom Unbranched
famInstAxiom = fi_axiom

-- Split the left-hand side of the FamInst
famInstSplitLHS :: FamInst -> (TyCon, [Type])
famInstSplitLHS (FamInst { fi_axiom = axiom, fi_tys = lhs })
  = (coAxiomTyCon axiom, lhs)

-- Get the RHS of the FamInst
famInstRHS :: FamInst -> Type
famInstRHS = fi_rhs

-- Get the family TyCon of the FamInst
famInstTyCon :: FamInst -> TyCon
famInstTyCon = coAxiomTyCon . famInstAxiom

-- Return the representation TyCons introduced by data family instances, if any
famInstsRepTyCons :: [FamInst] -> [TyCon]
famInstsRepTyCons fis = [tc | FamInst { fi_flavor = DataFamilyInst tc } <- fis]

-- Extracts the TyCon for this *data* (or newtype) instance
famInstRepTyCon_maybe :: FamInst -> Maybe TyCon
famInstRepTyCon_maybe fi
  = case fi_flavor fi of
       DataFamilyInst tycon -> Just tycon
       SynFamilyInst        -> Nothing

dataFamInstRepTyCon :: FamInst -> TyCon
dataFamInstRepTyCon fi
  = case fi_flavor fi of
       DataFamilyInst tycon -> tycon
       SynFamilyInst        -> pprPanic "dataFamInstRepTyCon" (ppr fi)

{-
************************************************************************
*                                                                      *
        Pretty printing
*                                                                      *
************************************************************************
-}

instance NamedThing FamInst where
   getName = coAxiomName . fi_axiom

instance Outputable FamInst where
   ppr = pprFamInst

-- Prints the FamInst as a family instance declaration
-- NB: FamInstEnv.pprFamInst is used only for internal, debug printing
--     See pprTyThing.pprFamInst for printing for the user
pprFamInst :: FamInst -> SDoc
pprFamInst famInst
  = hang (pprFamInstHdr famInst)
       2 (vcat [ ifPprDebug (ptext (sLit "Coercion axiom:") <+> ppr ax)
               , ifPprDebug (ptext (sLit "RHS:") <+> ppr (famInstRHS famInst)) ])
  where
    ax = fi_axiom famInst

pprFamInstHdr :: FamInst -> SDoc
pprFamInstHdr fi@(FamInst {fi_flavor = flavor})
  = pprTyConSort <+> pp_instance <+> pp_head
  where
    -- For *associated* types, say "type T Int = blah"
    -- For *top level* type instances, say "type instance T Int = blah"
    pp_instance
      | isTyConAssoc fam_tc = empty
      | otherwise           = ptext (sLit "instance")

    (fam_tc, etad_lhs_tys) = famInstSplitLHS fi
    vanilla_pp_head = pprTypeApp fam_tc etad_lhs_tys

    pp_head | DataFamilyInst rep_tc <- flavor
            , isAlgTyCon rep_tc
            , let extra_tvs = dropList etad_lhs_tys (tyConTyVars rep_tc)
            , not (null extra_tvs)
            = getPprStyle $ \ sty ->
              if debugStyle sty
              then vanilla_pp_head   -- With -dppr-debug just show it as-is
              else pprTypeApp fam_tc (etad_lhs_tys ++ mkTyVarTys extra_tvs)
                     -- Without -dppr-debug, eta-expand
                     -- See Trac #8674
                     -- (This is probably over the top now that we use this
                     --  only for internal debug printing; PprTyThing.pprFamInst
                     --  is used for user-level printing.)
            | otherwise
            = vanilla_pp_head

    pprTyConSort = case flavor of
                     SynFamilyInst        -> ptext (sLit "type")
                     DataFamilyInst tycon
                       | isDataTyCon     tycon -> ptext (sLit "data")
                       | isNewTyCon      tycon -> ptext (sLit "newtype")
                       | isAbstractTyCon tycon -> ptext (sLit "data")
                       | otherwise             -> ptext (sLit "WEIRD") <+> ppr tycon

pprFamInsts :: [FamInst] -> SDoc
pprFamInsts finsts = vcat (map pprFamInst finsts)

{-
Note [Lazy axiom match]
~~~~~~~~~~~~~~~~~~~~~~~
It is Vitally Important that mkImportedFamInst is *lazy* in its axiom
parameter. The axiom is loaded lazily, via a forkM, in TcIface. Sometime
later, mkImportedFamInst is called using that axiom. However, the axiom
may itself depend on entities which are not yet loaded as of the time
of the mkImportedFamInst. Thus, if mkImportedFamInst eagerly looks at the
axiom, a dependency loop spontaneously appears and GHC hangs. The solution
is simply for mkImportedFamInst never, ever to look inside of the axiom
until everything else is good and ready to do so. We can assume that this
readiness has been achieved when some other code pulls on the axiom in the
FamInst. Thus, we pattern match on the axiom lazily (in the where clause,
not in the parameter list) and we assert the consistency of names there
also.
-}

-- Make a family instance representation from the information found in an
-- interface file.  In particular, we get the rough match info from the iface
-- (instead of computing it here).
mkImportedFamInst :: Name               -- Name of the family
                  -> [Maybe Name]       -- Rough match info
                  -> CoAxiom Unbranched -- Axiom introduced
                  -> FamInst            -- Resulting family instance
mkImportedFamInst fam mb_tcs axiom
  = FamInst {
      fi_fam    = fam,
      fi_tcs    = mb_tcs,
      fi_tvs    = tvs,
      fi_tys    = tys,
      fi_rhs    = rhs,
      fi_axiom  = axiom,
      fi_flavor = flavor }
  where
     -- See Note [Lazy axiom match]
     ~(CoAxiom { co_ax_branches =
       ~(FirstBranch ~(CoAxBranch { cab_lhs = tys
                                  , cab_tvs = tvs
                                  , cab_rhs = rhs })) }) = axiom

         -- Derive the flavor for an imported FamInst rather disgustingly
         -- Maybe we should store it in the IfaceFamInst?
     flavor = case splitTyConApp_maybe rhs of
                Just (tc, _)
                  | Just ax' <- tyConFamilyCoercion_maybe tc
                  , ax' == axiom
                  -> DataFamilyInst tc
                _ -> SynFamilyInst

{-
************************************************************************
*                                                                      *
                FamInstEnv
*                                                                      *
************************************************************************

Note [FamInstEnv]
~~~~~~~~~~~~~~~~~
A FamInstEnv maps a family name to the list of known instances for that family.

The same FamInstEnv includes both 'data family' and 'type family' instances.
Type families are reduced during type inference, but not data families;
the user explains when to use a data family instance by using contructors
and pattern matching.

Nevertheless it is still useful to have data families in the FamInstEnv:

 - For finding overlaps and conflicts

 - For finding the representation type...see FamInstEnv.topNormaliseType
   and its call site in Simplify

 - In standalone deriving instance Eq (T [Int]) we need to find the
   representation type for T [Int]

Note [Varying number of patterns for data family axioms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For data families, the number of patterns may vary between instances.
For example
   data family T a b
   data instance T Int a = T1 a | T2
   data instance T Bool [a] = T3 a

Then we get a data type for each instance, and an axiom:
   data TInt a = T1 a | T2
   data TBoolList a = T3 a

   axiom ax7   :: T Int ~ TInt   -- Eta-reduced
   axiom ax8 a :: T Bool [a] ~ TBoolList a

These two axioms for T, one with one pattern, one with two.  The reason
for this eta-reduction is decribed in TcInstDcls
   Note [Eta reduction for data family axioms]
-}

type FamInstEnv = UniqFM FamilyInstEnv  -- Maps a family to its instances
     -- See Note [FamInstEnv]

type FamInstEnvs = (FamInstEnv, FamInstEnv)
     -- External package inst-env, Home-package inst-env

newtype FamilyInstEnv
  = FamIE [FamInst]     -- The instances for a particular family, in any order

instance Outputable FamilyInstEnv where
  ppr (FamIE fs) = ptext (sLit "FamIE") <+> vcat (map ppr fs)

-- INVARIANTS:
--  * The fs_tvs are distinct in each FamInst
--      of a range value of the map (so we can safely unify them)

emptyFamInstEnvs :: (FamInstEnv, FamInstEnv)
emptyFamInstEnvs = (emptyFamInstEnv, emptyFamInstEnv)

emptyFamInstEnv :: FamInstEnv
emptyFamInstEnv = emptyUFM

famInstEnvElts :: FamInstEnv -> [FamInst]
famInstEnvElts fi = [elt | FamIE elts <- eltsUFM fi, elt <- elts]

familyInstances :: (FamInstEnv, FamInstEnv) -> TyCon -> [FamInst]
familyInstances (pkg_fie, home_fie) fam
  = get home_fie ++ get pkg_fie
  where
    get env = case lookupUFM env fam of
                Just (FamIE insts) -> insts
                Nothing                      -> []

-- | Collects the names of the concrete types and type constructors that
-- make up the LHS of a type family instance, including the family
-- name itself.
--
-- For instance, given `type family Foo a b`:
-- `type instance Foo (F (G (H a))) b = ...` would yield [Foo,F,G,H]
--
-- Used in the implementation of ":info" in GHCi.
orphNamesOfFamInst :: FamInst -> NameSet
orphNamesOfFamInst fam_inst
  = orphNamesOfTypes (concat (brListMap cab_lhs (coAxiomBranches axiom)))
    `extendNameSet` getName (coAxiomTyCon axiom)
  where
    axiom = fi_axiom fam_inst

extendFamInstEnvList :: FamInstEnv -> [FamInst] -> FamInstEnv
extendFamInstEnvList inst_env fis = foldl extendFamInstEnv inst_env fis

extendFamInstEnv :: FamInstEnv -> FamInst -> FamInstEnv
extendFamInstEnv inst_env
                 ins_item@(FamInst {fi_fam = cls_nm})
  = addToUFM_C add inst_env cls_nm (FamIE [ins_item])
  where
    add (FamIE items) _ = FamIE (ins_item:items)

deleteFromFamInstEnv :: FamInstEnv -> FamInst -> FamInstEnv
-- Used only for overriding in GHCi
deleteFromFamInstEnv inst_env fam_inst@(FamInst {fi_fam = fam_nm})
 = adjustUFM adjust inst_env fam_nm
 where
   adjust :: FamilyInstEnv -> FamilyInstEnv
   adjust (FamIE items)
     = FamIE (filterOut (identicalFamInstHead fam_inst) items)

identicalFamInstHead :: FamInst -> FamInst -> Bool
-- ^ True when the LHSs are identical
-- Used for overriding in GHCi
identicalFamInstHead (FamInst { fi_axiom = ax1 }) (FamInst { fi_axiom = ax2 })
  =  coAxiomTyCon ax1 == coAxiomTyCon ax2
  && brListLength brs1 == brListLength brs2
  && and (brListZipWith identical_branch brs1 brs2)
  where
    brs1 = coAxiomBranches ax1
    brs2 = coAxiomBranches ax2

    identical_branch br1 br2
      =  isJust (tcMatchTys tvs1 lhs1 lhs2)
      && isJust (tcMatchTys tvs2 lhs2 lhs1)
      where
        tvs1 = mkVarSet (coAxBranchTyVars br1)
        tvs2 = mkVarSet (coAxBranchTyVars br2)
        lhs1 = coAxBranchLHS br1
        lhs2 = coAxBranchLHS br2

{-
************************************************************************
*                                                                      *
                Compatibility
*                                                                      *
************************************************************************

Note [Apartness]
~~~~~~~~~~~~~~~~
In dealing with closed type families, we must be able to check that one type
will never reduce to another. This check is called /apartness/. The check
is always between a target (which may be an arbitrary type) and a pattern.
Here is how we do it:

apart(target, pattern) = not (unify(flatten(target), pattern))

where flatten (implemented in flattenTys, below) converts all type-family
applications into fresh variables. (See Note [Flattening].)

Note [Compatibility]
~~~~~~~~~~~~~~~~~~~~
Two patterns are /compatible/ if either of the following conditions hold:
1) The patterns are apart.
2) The patterns unify with a substitution S, and their right hand sides
equal under that substitution.

For open type families, only compatible instances are allowed. For closed
type families, the story is slightly more complicated. Consider the following:

type family F a where
  F Int = Bool
  F a   = Int

g :: Show a => a -> F a
g x = length (show x)

Should that type-check? No. We need to allow for the possibility that 'a'
might be Int and therefore 'F a' should be Bool. We can simplify 'F a' to Int
only when we can be sure that 'a' is not Int.

To achieve this, after finding a possible match within the equations, we have to
go back to all previous equations and check that, under the
substitution induced by the match, other branches are surely apart. (See
Note [Apartness].) This is similar to what happens with class
instance selection, when we need to guarantee that there is only a match and
no unifiers. The exact algorithm is different here because the the
potentially-overlapping group is closed.

As another example, consider this:

type family G x where
  G Int = Bool
  G a   = Double

type family H y
-- no instances

Now, we want to simplify (G (H Char)). We can't, because (H Char) might later
simplify to be Int. So, (G (H Char)) is stuck, for now.

While everything above is quite sound, it isn't as expressive as we'd like.
Consider this:

type family J a where
  J Int = Int
  J a   = a

Can we simplify (J b) to b? Sure we can. Yes, the first equation matches if
b is instantiated with Int, but the RHSs coincide there, so it's all OK.

So, the rule is this: when looking up a branch in a closed type family, we
find a branch that matches the target, but then we make sure that the target
is apart from every previous *incompatible* branch. We don't check the
branches that are compatible with the matching branch, because they are either
irrelevant (clause 1 of compatible) or benign (clause 2 of compatible).
-}

-- See Note [Compatibility]
compatibleBranches :: CoAxBranch -> CoAxBranch -> Bool
compatibleBranches (CoAxBranch { cab_lhs = lhs1, cab_rhs = rhs1 })
                   (CoAxBranch { cab_lhs = lhs2, cab_rhs = rhs2 })
  = case tcUnifyTysFG instanceBindFun lhs1 lhs2 of
      SurelyApart -> True
      Unifiable subst
        | Type.substTy subst rhs1 `eqType` Type.substTy subst rhs2
        -> True
      _ -> False

-- | Result of testing two type family equations for injectiviy.
data InjectivityCheckResult
   = InjectivityAccepted
    -- ^ Either RHSs are distinct or unification of RHSs leads to unification of
    -- LHSs
   | InjectivityUnified CoAxBranch CoAxBranch
    -- ^ RHSs unify but LHSs don't unify under that substitution.  Relevant for
    -- closed type families where equation after unification might be
    -- overlpapped (in which case it is OK if they don't unify).  Constructor
    -- stores axioms after unification.

-- | Check whether two type family axioms don't violate injectivity annotation.
injectiveBranches :: [Bool] -> CoAxBranch -> CoAxBranch
                  -> InjectivityCheckResult
injectiveBranches injectivity
                  ax1@(CoAxBranch { cab_lhs = lhs1, cab_rhs = rhs1 })
                  ax2@(CoAxBranch { cab_lhs = lhs2, cab_rhs = rhs2 })
  -- See Note [Verifying injectivity annotation]. This function implements first
  -- check described there.
  = let getInjArgs  = filterByList injectivity
    in case tcUnifyTyWithTFs True rhs1 rhs2 of -- True = two-way pre-unification
       Nothing -> InjectivityAccepted -- RHS are different, so equations are
                                      -- injective.
       Just subst -> -- RHS unify under a substitution
        let lhs1Subst = Type.substTys subst (getInjArgs lhs1)
            lhs2Subst = Type.substTys subst (getInjArgs lhs2)
        -- If LHSs are equal under the substitution used for RHSs then this pair
        -- of equations does not violate injectivity annotation. If LHSs are not
        -- equal under that substitution then this pair of equations violates
        -- injectivity annotation, but for closed type families it still might
        -- be the case that one LHS after substitution is unreachable.
        in if eqTypes lhs1Subst lhs2Subst
           then InjectivityAccepted
           else InjectivityUnified ( ax1 { cab_lhs = Type.substTys subst lhs1
                                         , cab_rhs = Type.substTy  subst rhs1 })
                                   ( ax2 { cab_lhs = Type.substTys subst lhs2
                                         , cab_rhs = Type.substTy  subst rhs2 })

-- takes a CoAxiom with unknown branch incompatibilities and computes
-- the compatibilities
-- See Note [Storing compatibility] in CoAxiom
computeAxiomIncomps :: CoAxiom br -> CoAxiom br
computeAxiomIncomps ax@(CoAxiom { co_ax_branches = branches })
  = ax { co_ax_branches = go [] branches }
  where
    go :: [CoAxBranch] -> BranchList CoAxBranch br -> BranchList CoAxBranch br
    go prev_branches (FirstBranch br)
      = FirstBranch (br { cab_incomps = mk_incomps br prev_branches })
    go prev_branches (NextBranch br tail)
      = let br' = br { cab_incomps = mk_incomps br prev_branches } in
        NextBranch br' (go (br' : prev_branches) tail)

    mk_incomps :: CoAxBranch -> [CoAxBranch] -> [CoAxBranch]
    mk_incomps br = filter (not . compatibleBranches br)

{-
************************************************************************
*                                                                      *
           Constructing axioms
    These functions are here because tidyType / tcUnifyTysFG
    are not available in CoAxiom
*                                                                      *
************************************************************************

Note [Tidy axioms when we build them]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We print out axioms and don't want to print stuff like
    F k k a b = ...
Instead we must tidy those kind variables.  See Trac #7524.
-}

-- all axiom roles are Nominal, as this is only used with type families
mkCoAxBranch :: [TyVar] -- original, possibly stale, tyvars
             -> [Type]  -- LHS patterns
             -> Type    -- RHS
             -> SrcSpan
             -> CoAxBranch
mkCoAxBranch tvs lhs rhs loc
  = CoAxBranch { cab_tvs     = tvs1
               , cab_lhs     = tidyTypes env lhs
               , cab_roles   = map (const Nominal) tvs1
               , cab_rhs     = tidyType  env rhs
               , cab_loc     = loc
               , cab_incomps = placeHolderIncomps }
  where
    (env, tvs1) = tidyTyVarBndrs emptyTidyEnv tvs
    -- See Note [Tidy axioms when we build them]

-- all of the following code is here to avoid mutual dependencies with
-- Coercion
mkBranchedCoAxiom :: Name -> TyCon -> [CoAxBranch] -> CoAxiom Branched
mkBranchedCoAxiom ax_name fam_tc branches
  = computeAxiomIncomps $
    CoAxiom { co_ax_unique   = nameUnique ax_name
            , co_ax_name     = ax_name
            , co_ax_tc       = fam_tc
            , co_ax_role     = Nominal
            , co_ax_implicit = False
            , co_ax_branches = toBranchList branches }

mkUnbranchedCoAxiom :: Name -> TyCon -> CoAxBranch -> CoAxiom Unbranched
mkUnbranchedCoAxiom ax_name fam_tc branch
  = CoAxiom { co_ax_unique   = nameUnique ax_name
            , co_ax_name     = ax_name
            , co_ax_tc       = fam_tc
            , co_ax_role     = Nominal
            , co_ax_implicit = False
            , co_ax_branches = FirstBranch (branch { cab_incomps = [] }) }

mkSingleCoAxiom :: Role -> Name
                -> [TyVar] -> TyCon -> [Type] -> Type
                -> CoAxiom Unbranched
-- Make a single-branch CoAxiom, incluidng making the branch itself
-- Used for both type family (Nominal) and data family (Representational)
-- axioms, hence passing in the Role
mkSingleCoAxiom role ax_name tvs fam_tc lhs_tys rhs_ty
  = CoAxiom { co_ax_unique   = nameUnique ax_name
            , co_ax_name     = ax_name
            , co_ax_tc       = fam_tc
            , co_ax_role     = role
            , co_ax_implicit = False
            , co_ax_branches = FirstBranch (branch { cab_incomps = [] }) }
  where
    branch = mkCoAxBranch tvs lhs_tys rhs_ty (getSrcSpan ax_name)

{-
************************************************************************
*                                                                      *
                Looking up a family instance
*                                                                      *
************************************************************************

@lookupFamInstEnv@ looks up in a @FamInstEnv@, using a one-way match.
Multiple matches are only possible in case of type families (not data
families), and then, it doesn't matter which match we choose (as the
instances are guaranteed confluent).

We return the matching family instances and the type instance at which it
matches.  For example, if we lookup 'T [Int]' and have a family instance

  data instance T [a] = ..

desugared to

  data :R42T a = ..
  coe :Co:R42T a :: T [a] ~ :R42T a

we return the matching instance '(FamInst{.., fi_tycon = :R42T}, Int)'.
-}

-- when matching a type family application, we get a FamInst,
-- and the list of types the axiom should be applied to
data FamInstMatch = FamInstMatch { fim_instance :: FamInst
                                 , fim_tys      :: [Type]
                                 }
  -- See Note [Over-saturated matches]

instance Outputable FamInstMatch where
  ppr (FamInstMatch { fim_instance = inst
                    , fim_tys      = tys })
    = ptext (sLit "match with") <+> parens (ppr inst) <+> ppr tys

lookupFamInstEnvByTyCon :: FamInstEnvs -> TyCon -> [FamInst]
lookupFamInstEnvByTyCon (pkg_ie, home_ie) fam_tc
  = get pkg_ie ++ get home_ie
  where
    get ie = case lookupUFM ie fam_tc of
               Nothing          -> []
               Just (FamIE fis) -> fis

lookupFamInstEnv
    :: FamInstEnvs
    -> TyCon -> [Type]          -- What we are looking for
    -> [FamInstMatch]           -- Successful matches
-- Precondition: the tycon is saturated (or over-saturated)

lookupFamInstEnv
   = lookup_fam_inst_env match
   where
     match _ tpl_tvs tpl_tys tys = tcMatchTys tpl_tvs tpl_tys tys

lookupFamInstEnvConflicts
    :: FamInstEnvs
    -> FamInst          -- Putative new instance
    -> [FamInstMatch]   -- Conflicting matches (don't look at the fim_tys field)
-- E.g. when we are about to add
--    f : type instance F [a] = a->a
-- we do (lookupFamInstConflicts f [b])
-- to find conflicting matches
--
-- Precondition: the tycon is saturated (or over-saturated)

lookupFamInstEnvConflicts envs fam_inst@(FamInst { fi_axiom = new_axiom })
  = lookup_fam_inst_env my_unify envs fam tys
  where
    (fam, tys) = famInstSplitLHS fam_inst
        -- In example above,   fam tys' = F [b]

    my_unify (FamInst { fi_axiom = old_axiom }) tpl_tvs tpl_tys _
       = ASSERT2( tyVarsOfTypes tys `disjointVarSet` tpl_tvs,
                  (ppr fam <+> ppr tys) $$
                  (ppr tpl_tvs <+> ppr tpl_tys) )
                -- Unification will break badly if the variables overlap
                -- They shouldn't because we allocate separate uniques for them
         if compatibleBranches (coAxiomSingleBranch old_axiom) new_branch
           then Nothing
           else Just noSubst
      -- Note [Family instance overlap conflicts]

    noSubst = panic "lookupFamInstEnvConflicts noSubst"
    new_branch = coAxiomSingleBranch new_axiom

--------------------------------------------------------------------------------
--                 Type family injectivity checking bits                      --
--------------------------------------------------------------------------------

{- Note [Verifying injectivity annotation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Injectivity means that the RHS of a type family uniquely determines the LHS (see
Note [Type inference for type families with injectivity]).  User informs about
injectivity using an injectivity annotation and it is GHC's task to verify that
that annotation is correct wrt. to type family equations. Whenever we see a new
equation of a type family we need to make sure that adding this equation to
already known equations of a type family does not violate injectivity annotation
supplied by the user (see Note [Injectivity annotation]).  Of course if the type
family has no injectivity annotation then no check is required.  But if a type
family has injectivity annotation we need to make sure that the following
conditions hold:

1. For each pair of *different* equations of a type family, one of the following
   conditions holds:

   A:  RHSs are different.

   B1: OPEN TYPE FAMILIES: If the RHSs can be unified under some substitution
       then it must be possible to unify the LHSs under the same substitution.
       Example:

          type family FunnyId a = r | r -> a
          type instance FunnyId Int = Int
          type instance FunnyId a = a

       RHSs of these two equations unify under [ a |-> Int ] substitution.
       Under this substitution LHSs are equal therefore these equations don't
       violate injectivity annotation.

   B2: CLOSED TYPE FAMILIES: If the RHSs can be unified under some
       substitution then either the LHSs unify under the same substitution or
       the LHS of the latter equation is overlapped by earlier equations.
       Example 1:

          type family SwapIntChar a = r | r -> a where
              SwapIntChar Int  = Char
              SwapIntChar Char = Int
              SwapIntChar a    = a

       Say we are checking the last two equations. RHSs unify under [ a |->
       Int ] substitution but LHSs don't. So we apply the substitution to LHS
       of last equation and check whether it is overlapped by any of previous
       equations. Since it is overlapped by the first equation we conclude
       that pair of last two equations does not violate injectivity
       annotation.

   A special case of B is when RHSs unify with an empty substitution ie. they
   are identical.

   If any of the above two conditions holds we conclude that the pair of
   equations does not violate injectivity annotation. But if we find a pair
   of equations where neither of the above holds we report that this pair
   violates injectivity annotation because for a given RHS we don't have a
   unique LHS. (Note that (B) actually implies (A).)

   Note that we only take into account these LHS patterns that were declared
   as injective.

2. If a RHS of a type family equation is a bare type variable then
   all LHS variables (including implicit kind variables) also have to be bare.
   In other words, this has to be a sole equation of that type family and it has
   to cover all possible patterns.  So for example this definition will be
   rejected:

      type family W1 a = r | r -> a
      type instance W1 [a] = a

   If it were accepted we could call `W1 [W1 Int]`, which would reduce to
   `W1 Int` and then by injectivity we could conclude that `[W1 Int] ~ Int`,
   which is bogus.

3. If a RHS of a type family equation is a type family application then the type
   family is rejected as not injective.

4. If a LHS type variable that is declared as injective is not mentioned on
   injective position in the RHS then the type family is rejected as not
   injective.  "Injective position" means either an argument to a type
   constructor or argument to a type family on injective position.

See also Note [Injective type families] in TyCon
-}


-- | Check whether an open type family equation can be added to already existing
-- instance environment without causing conflicts with supplied injectivity
-- annotations.  Returns list of conflicting axioms (type instance
-- declarations).
lookupFamInstEnvInjectivityConflicts
    :: [Bool]         -- injectivity annotation for this type family instance
                      -- INVARIANT: list contains at least one True value
    ->  FamInstEnvs   -- all type instances seens so far
    ->  FamInst       -- new type instance that we're checking
    -> [CoAxBranch]   -- conflicting instance delcarations
lookupFamInstEnvInjectivityConflicts injList (pkg_ie, home_ie)
                             fam_inst@(FamInst { fi_axiom = new_axiom })
  -- See Note [Verifying injectivity annotation]. This function implements
  -- check (1.B1) for open type families described there.
  = lookup_inj_fam_conflicts home_ie ++ lookup_inj_fam_conflicts pkg_ie
    where
      fam        = famInstTyCon fam_inst
      new_branch = coAxiomSingleBranch new_axiom

      -- filtering function used by `lookup_inj_fam_conflicts` to check whether
      -- a pair of equations conflicts with the injectivity annotation.
      isInjConflict (FamInst { fi_axiom = old_axiom })
          | InjectivityAccepted <-
            injectiveBranches injList (coAxiomSingleBranch old_axiom) new_branch
          = False -- no conflict
          | otherwise = True

      lookup_inj_fam_conflicts ie
          | isOpenFamilyTyCon fam, Just (FamIE insts) <- lookupUFM ie fam
          = map (brFromUnbranchedSingleton . co_ax_branches . fi_axiom) $
            filter isInjConflict insts
          | otherwise = []


-- | Return a list of type variables that the function is injective in and that
-- do not appear on injective positions in the RHS of a family instance
-- declaration.
unusedInjTvsInRHS :: [Bool] -> [Type] -> Type -> TyVarSet
-- INVARIANT: [Bool] list contains at least one True value
-- See Note [Verifying injectivity annotation]. This function implements fourth
-- check described there.
-- In theory, instead of implementing this whole check in this way, we could
-- attempt to unify equation with itself.  We would reject exactly the same
-- equations but this method gives us more precise error messages by returning
-- precise names of variables that are not mentioned in the RHS.
unusedInjTvsInRHS injList lhs rhs =
  injLHSVars `minusVarSet` injRhsVars
    where
      -- set of type and kind variables in which type family is injective
      injLHSVars = tyVarsOfTypes (filterByList injList lhs)

      -- set of type variables appearing in the RHS on an injective position.
      -- For all returned variables we assume their associated kind variables
      -- also appear in the RHS.
      injRhsVars = closeOverKinds $ collectInjVars rhs

      -- Collect all type variables that are either arguments to a type
      -- constructor or to injective type families.
      collectInjVars :: Type -> VarSet
      collectInjVars ty | Just (ty1, ty2) <- splitAppTy_maybe ty
        = collectInjVars ty1 `unionVarSet` collectInjVars ty2
      collectInjVars (TyVarTy v)
        = unitVarSet v
      collectInjVars (TyConApp tc tys)
        | isTypeFamilyTyCon tc = collectInjTFVars tys
                                                 (familyTyConInjectivityInfo tc)
        | otherwise            = mapUnionVarSet collectInjVars tys
      collectInjVars (LitTy {})
        = emptyVarSet
      collectInjVars (FunTy arg res)
        = collectInjVars arg `unionVarSet` collectInjVars res
      collectInjVars (AppTy fun arg)
        = collectInjVars fun `unionVarSet` collectInjVars arg
      -- no forall types in the RHS of a type family
      collectInjVars (ForAllTy _ _)    =
          panic "unusedInjTvsInRHS.collectInjVars"

      collectInjTFVars :: [Type] -> Injectivity -> VarSet
      collectInjTFVars _ NotInjective
          = emptyVarSet
      collectInjTFVars tys (Injective injList)
          = mapUnionVarSet collectInjVars (filterByList injList tys)

-- | Is type headed by a type family application?
isTFHeaded :: Type -> Bool
-- See Note [Verifying injectivity annotation]. This function implements third
-- check described there.
isTFHeaded ty | Just ty' <- tcView ty
              = isTFHeaded ty'
isTFHeaded ty | (TyConApp tc args) <- ty
              , isTypeFamilyTyCon tc
              = tyConArity tc == length args
isTFHeaded _  = False

-- | If a RHS is a bare type variable return a set of LHS patterns that are not
-- bare type variables.
bareTvInRHSViolated :: [Type] -> Type -> [Type]
-- See Note [Verifying injectivity annotation]. This function implements second
-- check described there.
bareTvInRHSViolated pats rhs | isTyVarTy rhs
   = filter (not . isTyVarTy) pats
bareTvInRHSViolated _ _ = []

--------------------------------------------------------------------------------
--                    Type family overlap checking bits                       --
--------------------------------------------------------------------------------

{-
Note [Family instance overlap conflicts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- In the case of data family instances, any overlap is fundamentally a
  conflict (as these instances imply injective type mappings).

- In the case of type family instances, overlap is admitted as long as
  the right-hand sides of the overlapping rules coincide under the
  overlap substitution.  eg
       type instance F a Int = a
       type instance F Int b = b
  These two overlap on (F Int Int) but then both RHSs are Int,
  so all is well. We require that they are syntactically equal;
  anything else would be difficult to test for at this stage.
-}

------------------------------------------------------------
-- Might be a one-way match or a unifier
type MatchFun =  FamInst                -- The FamInst template
              -> TyVarSet -> [Type]     --   fi_tvs, fi_tys of that FamInst
              -> [Type]                 -- Target to match against
              -> Maybe TvSubst

lookup_fam_inst_env'          -- The worker, local to this module
    :: MatchFun
    -> FamInstEnv
    -> TyCon -> [Type]        -- What we are looking for
    -> [FamInstMatch]
lookup_fam_inst_env' match_fun ie fam match_tys
  | isOpenFamilyTyCon fam
  , Just (FamIE insts) <- lookupUFM ie fam
  = find insts    -- The common case
  | otherwise = []
  where

    find [] = []
    find (item@(FamInst { fi_tcs = mb_tcs, fi_tvs = tpl_tvs,
                          fi_tys = tpl_tys }) : rest)
        -- Fast check for no match, uses the "rough match" fields
      | instanceCantMatch rough_tcs mb_tcs
      = find rest

        -- Proper check
      | Just subst <- match_fun item (mkVarSet tpl_tvs) tpl_tys match_tys1
      = (FamInstMatch { fim_instance = item
                      , fim_tys      = substTyVars subst tpl_tvs `chkAppend` match_tys2 })
        : find rest

        -- No match => try next
      | otherwise
      = find rest

      where
        (rough_tcs, match_tys1, match_tys2) = split_tys tpl_tys

      -- Precondition: the tycon is saturated (or over-saturated)

    -- Deal with over-saturation
    -- See Note [Over-saturated matches]
    split_tys tpl_tys
      | isTypeFamilyTyCon fam
      = pre_rough_split_tys

      | otherwise
      = let (match_tys1, match_tys2) = splitAtList tpl_tys match_tys
            rough_tcs = roughMatchTcs match_tys1
        in (rough_tcs, match_tys1, match_tys2)

    (pre_match_tys1, pre_match_tys2) = splitAt (tyConArity fam) match_tys
    pre_rough_split_tys
      = (roughMatchTcs pre_match_tys1, pre_match_tys1, pre_match_tys2)

lookup_fam_inst_env           -- The worker, local to this module
    :: MatchFun
    -> FamInstEnvs
    -> TyCon -> [Type]        -- What we are looking for
    -> [FamInstMatch]         -- Successful matches

-- Precondition: the tycon is saturated (or over-saturated)

lookup_fam_inst_env match_fun (pkg_ie, home_ie) fam tys
  =  lookup_fam_inst_env' match_fun home_ie fam tys
  ++ lookup_fam_inst_env' match_fun pkg_ie  fam tys

{-
Note [Over-saturated matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's ok to look up an over-saturated type constructor.  E.g.
     type family F a :: * -> *
     type instance F (a,b) = Either (a->b)

The type instance gives rise to a newtype TyCon (at a higher kind
which you can't do in Haskell!):
     newtype FPair a b = FP (Either (a->b))

Then looking up (F (Int,Bool) Char) will return a FamInstMatch
     (FPair, [Int,Bool,Char])
The "extra" type argument [Char] just stays on the end.

We handle data families and type families separately here:

 * For type families, all instances of a type family must have the
   same arity, so we can precompute the split between the match_tys
   and the overflow tys. This is done in pre_rough_split_tys.

 * For data family instances, though, we need to re-split for each
   instance, because the breakdown might be different for each
   instance.  Why?  Because of eta reduction; see Note [Eta reduction
   for data family axioms] in TcInstDcls.
-}

-- checks if one LHS is dominated by a list of other branches
-- in other words, if an application would match the first LHS, it is guaranteed
-- to match at least one of the others. The RHSs are ignored.
-- This algorithm is conservative:
--   True -> the LHS is definitely covered by the others
--   False -> no information
-- It is currently (Oct 2012) used only for generating errors for
-- inaccessible branches. If these errors go unreported, no harm done.
-- This is defined here to avoid a dependency from CoAxiom to Unify
isDominatedBy :: CoAxBranch -> [CoAxBranch] -> Bool
isDominatedBy branch branches
  = or $ map match branches
    where
      lhs = coAxBranchLHS branch
      match (CoAxBranch { cab_tvs = tvs, cab_lhs = tys })
        = isJust $ tcMatchTys (mkVarSet tvs) tys lhs

{-
************************************************************************
*                                                                      *
                Choosing an axiom application
*                                                                      *
************************************************************************

The lookupFamInstEnv function does a nice job for *open* type families,
but we also need to handle closed ones when normalising a type:
-}

reduceTyFamApp_maybe :: FamInstEnvs
                     -> Role              -- Desired role of result coercion
                     -> TyCon -> [Type]
                     -> Maybe (Coercion, Type)
-- Attempt to do a *one-step* reduction of a type-family application
--    but *not* newtypes
-- Works on type-synonym families always; data-families only if
--     the role we seek is representational
-- It does *not* normlise the type arguments first, so this may not
--     go as far as you want. If you want normalised type arguments,
--     use normaliseTcArgs first.
--
-- The TyCon can be oversaturated.
-- Works on both open and closed families

reduceTyFamApp_maybe envs role tc tys
  | Phantom <- role
  = Nothing

  | case role of
       Representational -> isOpenFamilyTyCon    tc
       _                -> isOpenTypeFamilyTyCon tc
       -- If we seek a representational coercion
       -- (e.g. the call in topNormaliseType_maybe) then we can
       -- unwrap data families as well as type-synonym families;
       -- otherwise only type-synonym families
  , FamInstMatch { fim_instance = fam_inst
                 , fim_tys =      inst_tys } : _ <- lookupFamInstEnv envs tc tys
      -- NB: Allow multiple matches because of compatible overlap
  = let ax     = famInstAxiom fam_inst
        co     = mkUnbranchedAxInstCo role ax inst_tys
        ty     = pSnd (coercionKind co)
    in Just (co, ty)

  | Just ax <- isClosedSynFamilyTyConWithAxiom_maybe tc
  , Just (ind, inst_tys) <- chooseBranch ax tys
  = let co     = mkAxInstCo role ax ind inst_tys
        ty     = pSnd (coercionKind co)
    in Just (co, ty)

  | Just ax           <- isBuiltInSynFamTyCon_maybe tc
  , Just (coax,ts,ty) <- sfMatchFam ax tys
  = let co = mkAxiomRuleCo coax ts []
    in Just (co, ty)

  | otherwise
  = Nothing

-- The axiom can be oversaturated. (Closed families only.)
chooseBranch :: CoAxiom Branched -> [Type] -> Maybe (BranchIndex, [Type])
chooseBranch axiom tys
  = do { let num_pats = coAxiomNumPats axiom
             (target_tys, extra_tys) = splitAt num_pats tys
             branches = coAxiomBranches axiom
       ; (ind, inst_tys) <- findBranch (fromBranchList branches) target_tys
       ; return (ind, inst_tys ++ extra_tys) }

-- The axiom must *not* be oversaturated
findBranch :: [CoAxBranch]             -- branches to check
           -> [Type]                   -- target types
           -> Maybe (BranchIndex, [Type])
findBranch branches target_tys
  = go 0 branches
  where
    go ind (branch@(CoAxBranch { cab_tvs = tpl_tvs, cab_lhs = tpl_lhs
                               , cab_incomps = incomps }) : rest)
      = let in_scope = mkInScopeSet (unionVarSets $
                            map (tyVarsOfTypes . coAxBranchLHS) incomps)
            -- See Note [Flattening] below
            flattened_target = flattenTys in_scope target_tys
        in case tcMatchTys (mkVarSet tpl_tvs) tpl_lhs target_tys of
        Just subst -- matching worked. now, check for apartness.
          |  apartnessCheck flattened_target branch
          -> -- matching worked & we're apart from all incompatible branches.
             -- success
             Just (ind, substTyVars subst tpl_tvs)

        -- failure. keep looking
        _ -> go (ind+1) rest

    -- fail if no branches left
    go _ [] = Nothing

-- | Do an apartness check, as described in the "Closed Type Families" paper
-- (POPL '14). This should be used when determining if an equation
-- ('CoAxBranch') of a closed type family can be used to reduce a certain target
-- type family application.
apartnessCheck :: [Type]     -- ^ /flattened/ target arguments. Make sure
                             -- they're flattened! See Note [Flattening].
                             -- (NB: This "flat" is a different
                             -- "flat" than is used in TcFlatten.)
               -> CoAxBranch -- ^ the candidate equation we wish to use
                             -- Precondition: this matches the target
               -> Bool       -- ^ True <=> equation can fire
apartnessCheck flattened_target (CoAxBranch { cab_incomps = incomps })
  = all (isSurelyApart
         . tcUnifyTysFG instanceBindFun flattened_target
         . coAxBranchLHS) incomps
  where
    isSurelyApart SurelyApart = True
    isSurelyApart _           = False

{-
************************************************************************
*                                                                      *
                Looking up a family instance
*                                                                      *
************************************************************************
-}

topNormaliseType :: FamInstEnvs -> Type -> Type
topNormaliseType env ty = case topNormaliseType_maybe env ty of
                            Just (_co, ty') -> ty'
                            Nothing         -> ty

topNormaliseType_maybe :: FamInstEnvs -> Type -> Maybe (Coercion, Type)

-- ^ Get rid of *outermost* (or toplevel)
--      * type function redex
--      * data family redex
--      * newtypes
-- returning an appropriate Representational coercion.  Specifically, if
--   topNormaliseType_maybe env ty = Maybe (co, ty')
-- then
--   (a) co :: ty ~R ty'
--   (b) ty' is not a newtype, and is not a type-family or data-family redex
--
-- However, ty' can be something like (Maybe (F ty)), where
-- (F ty) is a redex.
--
-- Its a bit like Type.repType, but handles type families too

topNormaliseType_maybe env ty
  = topNormaliseTypeX_maybe stepper ty
  where
    stepper = unwrapNewTypeStepper `composeSteppers` tyFamStepper

    tyFamStepper rec_nts tc tys  -- Try to step a type/data familiy
      = let (args_co, ntys) = normaliseTcArgs env Representational tc tys in
        case reduceTyFamApp_maybe env Representational tc ntys of
          Just (co, rhs) -> NS_Step rec_nts rhs (args_co `mkTransCo` co)
          Nothing        -> NS_Done

---------------
normaliseTcApp :: FamInstEnvs -> Role -> TyCon -> [Type] -> (Coercion, Type)
-- See comments on normaliseType for the arguments of this function
normaliseTcApp env role tc tys
  | isTypeSynonymTyCon tc
  , Just (tenv, rhs, ntys') <- expandSynTyCon_maybe tc ntys
  , (co2, ninst_rhs) <- normaliseType env role (Type.substTy (mkTopTvSubst tenv) rhs)
  = if isReflCo co2 then (args_co,                 mkTyConApp tc ntys)
                    else (args_co `mkTransCo` co2, mkAppTys ninst_rhs ntys')

  | Just (first_co, ty') <- reduceTyFamApp_maybe env role tc ntys
  , (rest_co,nty) <- normaliseType env role ty'
  = (args_co `mkTransCo` first_co `mkTransCo` rest_co, nty)

  | otherwise   -- No unique matching family instance exists;
                -- we do not do anything
  = (args_co, mkTyConApp tc ntys)

  where
    (args_co, ntys) = normaliseTcArgs env role tc tys


---------------
normaliseTcArgs :: FamInstEnvs            -- environment with family instances
                 -> Role                   -- desired role of output coercion
                 -> TyCon -> [Type]        -- tc tys
                 -> (Coercion, [Type])     -- (co, new_tys), where
                                           -- co :: tc tys ~ tc new_tys
normaliseTcArgs env role tc tys
  = (mkTyConAppCo role tc cois, ntys)
  where
    (cois, ntys) = zipWithAndUnzip (normaliseType env) (tyConRolesX role tc) tys

---------------
normaliseType :: FamInstEnvs            -- environment with family instances
               -> Role                   -- desired role of output coercion
               -> Type                   -- old type
               -> (Coercion, Type)       -- (coercion,new type), where
                                        -- co :: old-type ~ new_type
-- Normalise the input type, by eliminating *all* type-function redexes
-- but *not* newtypes (which are visible to the programmer)
-- Returns with Refl if nothing happens
-- Try to not to disturb type synonyms if possible

normaliseType env role (TyConApp tc tys)
  = normaliseTcApp env role tc tys
normaliseType _env role ty@(LitTy {}) = (mkReflCo role ty, ty)
normaliseType env role (AppTy ty1 ty2)
  = let (coi1,nty1) = normaliseType env role    ty1
        (coi2,nty2) = normaliseType env Nominal ty2
    in  (mkAppCo coi1 coi2, mkAppTy nty1 nty2)
normaliseType env role (FunTy ty1 ty2)
  = let (coi1,nty1) = normaliseType env role ty1
        (coi2,nty2) = normaliseType env role ty2
    in  (mkFunCo role coi1 coi2, mkFunTy nty1 nty2)
normaliseType env role (ForAllTy tyvar ty1)
  = let (coi,nty1) = normaliseType env role ty1
    in  (mkForAllCo tyvar coi, ForAllTy tyvar nty1)
normaliseType _  role ty@(TyVarTy _)
  = (mkReflCo role ty,ty)

{-
************************************************************************
*                                                                      *
              Flattening
*                                                                      *
************************************************************************

Note [Flattening]
~~~~~~~~~~~~~~~~~
As described in "Closed type families with overlapping equations"
http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/axioms-extended.pdf
we need to flatten core types before unifying them, when checking for "surely-apart"
against earlier equations of a closed type family.
Flattening means replacing all top-level uses of type functions with
fresh variables, *taking care to preserve sharing*. That is, the type
(Either (F a b) (F a b)) should flatten to (Either c c), never (Either
c d).

Here is a nice example of why it's all necessary:

  type family F a b where
    F Int Bool = Char
    F a   b    = Double
  type family G a         -- open, no instances

How do we reduce (F (G Float) (G Float))? The first equation clearly doesn't match,
while the second equation does. But, before reducing, we must make sure that the
target can never become (F Int Bool). Well, no matter what G Float becomes, it
certainly won't become *both* Int and Bool, so indeed we're safe reducing
(F (G Float) (G Float)) to Double.

This is necessary not only to get more reductions (which we might be
willing to give up on), but for substitutivity. If we have (F x x), we
can see that (F x x) can reduce to Double. So, it had better be the
case that (F blah blah) can reduce to Double, no matter what (blah)
is!  Flattening as done below ensures this.

flattenTys is defined here because of module dependencies.
-}

type FlattenMap = TypeMap TyVar

-- See Note [Flattening]
flattenTys :: InScopeSet -> [Type] -> [Type]
flattenTys in_scope tys = snd $ coreFlattenTys all_in_scope emptyTypeMap tys
  where
    -- when we hit a type function, we replace it with a fresh variable
    -- but, we need to make sure that this fresh variable isn't mentioned
    -- *anywhere* in the types we're flattening, even if locally-bound in
    -- a forall. That way, we can ensure consistency both within and outside
    -- of that forall.
    all_in_scope = in_scope `extendInScopeSetSet` allTyVarsInTys tys

coreFlattenTys :: InScopeSet -> FlattenMap -> [Type] -> (FlattenMap, [Type])
coreFlattenTys in_scope = go []
  where
    go rtys m []         = (m, reverse rtys)
    go rtys m (ty : tys)
      = let (m', ty') = coreFlattenTy in_scope m ty in
        go (ty' : rtys) m' tys

coreFlattenTy :: InScopeSet -> FlattenMap -> Type -> (FlattenMap, Type)
coreFlattenTy in_scope = go
  where
    go m ty | Just ty' <- coreView ty = go m ty'

    go m ty@(TyVarTy {}) = (m, ty)
    go m (AppTy ty1 ty2) = let (m1, ty1') = go m  ty1
                               (m2, ty2') = go m1 ty2 in
                           (m2, AppTy ty1' ty2')
    go m (TyConApp tc tys)
         -- NB: Don't just check if isFamilyTyCon: this catches *data* families,
         -- which are generative and thus can be preserved during flattening
      | not (isGenerativeTyCon tc Nominal)
      = let (m', tv) = coreFlattenTyFamApp in_scope m tc tys in
        (m', mkTyVarTy tv)

      | otherwise
      = let (m', tys') = coreFlattenTys in_scope m tys in
        (m', mkTyConApp tc tys')

    go m (FunTy ty1 ty2) = let (m1, ty1') = go m  ty1
                               (m2, ty2') = go m1 ty2 in
                           (m2, FunTy ty1' ty2')

      -- Note to RAE: this will have to be changed with kind families
    go m (ForAllTy tv ty) = let (m', ty') = go m ty in
                            (m', ForAllTy tv ty')

    go m ty@(LitTy {}) = (m, ty)

coreFlattenTyFamApp :: InScopeSet -> FlattenMap
                    -> TyCon         -- type family tycon
                    -> [Type]        -- args
                    -> (FlattenMap, TyVar)
coreFlattenTyFamApp in_scope m fam_tc fam_args
  = case lookupTypeMap m fam_ty of
      Just tv -> (m, tv)
              -- we need fresh variables here, but this is called far from
              -- any good source of uniques. So, we just use the fam_tc's unique
              -- and trust uniqAway to avoid clashes. Recall that the in_scope set
              -- contains *all* tyvars, even locally bound ones elsewhere in the
              -- overall type, so this really is fresh.
      Nothing -> let tyvar_name   = mkSysTvName (getUnique fam_tc) (fsLit "fl")
                     tv = uniqAway in_scope $ mkTyVar tyvar_name (typeKind fam_ty)
                     m' = extendTypeMap m fam_ty tv in
                 (m', tv)
  where fam_ty = TyConApp fam_tc fam_args

allTyVarsInTys :: [Type] -> VarSet
allTyVarsInTys []       = emptyVarSet
allTyVarsInTys (ty:tys) = allTyVarsInTy ty `unionVarSet` allTyVarsInTys tys

allTyVarsInTy :: Type -> VarSet
allTyVarsInTy = go
  where
    go (TyVarTy tv)      = unitVarSet tv
    go (AppTy ty1 ty2)   = (go ty1) `unionVarSet` (go ty2)
    go (TyConApp _ tys)  = allTyVarsInTys tys
    go (FunTy ty1 ty2)   = (go ty1) `unionVarSet` (go ty2)
    go (ForAllTy tv ty)  = (go (tyVarKind tv)) `unionVarSet`
                           unitVarSet tv `unionVarSet`
                           (go ty) -- don't remove tv
    go (LitTy {})        = emptyVarSet
