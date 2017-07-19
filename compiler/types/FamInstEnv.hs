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
        extendFamInstEnv, extendFamInstEnvList,
        famInstEnvElts, famInstEnvSize, familyInstances,

        -- * CoAxioms
        mkCoAxBranch, mkBranchedCoAxiom, mkUnbranchedCoAxiom, mkSingleCoAxiom,
        mkNewTypeCoAxiom,

        FamInstMatch(..),
        lookupFamInstEnv, lookupFamInstEnvConflicts, lookupFamInstEnvByTyCon,

        isDominatedBy, apartnessCheck,

        -- Injectivity
        InjectivityCheckResult(..),
        lookupFamInstEnvInjectivityConflicts, injectiveBranches,

        -- Normalisation
        topNormaliseType, topNormaliseType_maybe,
        normaliseType, normaliseTcApp,
        reduceTyFamApp_maybe,
        pmTopNormaliseType_maybe,

        -- Flattening
        flattenTys
    ) where

#include "HsVersions.h"

import Unify
import Type
import TyCoRep
import TyCon
import DataCon (DataCon)
import Coercion
import CoAxiom
import VarSet
import VarEnv
import Name
import PrelNames ( eqPrimTyConKey )
import UniqDFM
import Outputable
import Maybes
import TrieMap
import Unique
import Util
import Var
import Pair
import SrcLoc
import FastString
import MonadUtils
import Control.Monad
import Data.List( mapAccumL, find )

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
                 -- INVARIANT: apart from freshening (see below)
                 --    fi_tvs = cab_tvs of the (single) axiom branch
                 --    fi_cvs = cab_cvs ...ditto...
                 --    fi_tys = cab_lhs ...ditto...
                 --    fi_rhs = cab_rhs ...ditto...

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
            , fi_tvs :: [TyVar]      -- Template tyvars for full match
            , fi_cvs :: [CoVar]      -- Template covars for full match
                 -- Like ClsInsts, these variables are always fresh
                 -- See Note [Template tyvars are fresh] in InstEnv

            , fi_tys    :: [Type]       --   The LHS type patterns
            -- May be eta-reduced; see Note [Eta reduction for data families]

            , fi_rhs :: Type         --   the RHS, with its freshened vars
            }

data FamFlavor
  = SynFamilyInst         -- A synonym family
  | DataFamilyInst TyCon  -- A data family, with its representation TyCon

{-
Note [Arity of data families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Data family instances might legitimately be over- or under-saturated.

Under-saturation has two potential causes:
 U1) Eta reduction. See Note [Eta reduction for data families].
 U2) When the user has specified a return kind instead of written out patterns.
     Example:

       data family Sing (a :: k)
       data instance Sing :: Bool -> Type

     The data family tycon Sing has an arity of 2, the k and the a. But
     the data instance has only one pattern, Bool (standing in for k).
     This instance is equivalent to `data instance Sing (a :: Bool)`, but
     without the last pattern, we have an under-saturated data family instance.
     On its own, this example is not compelling enough to add support for
     under-saturation, but U1 makes this feature more compelling.

Over-saturation is also possible:
  O1) If the data family's return kind is a type variable (see also #12369),
      an instance might legitimately have more arguments than the family.
      Example:

        data family Fix :: (Type -> k) -> k
        data instance Fix f = MkFix1 (f (Fix f))
        data instance Fix f x = MkFix2 (f (Fix f x) x)

      In the first instance here, the k in the data family kind is chosen to
      be Type. In the second, it's (Type -> Type).

      However, we require that any over-saturation is eta-reducible. That is,
      we require that any extra patterns be bare unrepeated type variables;
      see Note [Eta reduction for data families]. Accordingly, the FamInst
      is never over-saturated.

Note [Eta reduction for data families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
   data family T a b :: *
   newtype instance T Int a = MkT (IO a) deriving( Monad )
We'd like this to work.

From the 'newtype instance' you might think we'd get:
   newtype TInt a = MkT (IO a)
   axiom ax1 a :: T Int a ~ TInt a   -- The newtype-instance part
   axiom ax2 a :: TInt a ~ IO a      -- The newtype part

But now what can we do?  We have this problem
   Given:   d  :: Monad IO
   Wanted:  d' :: Monad (T Int) = d |> ????
What coercion can we use for the ???

Solution: eta-reduce both axioms, thus:
   axiom ax1 :: T Int ~ TInt
   axiom ax2 :: TInt ~ IO
Now
   d' = d |> Monad (sym (ax2 ; ax1))

This eta reduction happens for data instances as well as newtype
instances. Here we want to eta-reduce the data family axiom.
All this is done in TcInstDcls.tcDataFamInstDecl.

See also Note [Newtype eta] in TyCon.

Bottom line:
  For a FamInst with fi_flavour = DataFamilyInst rep_tc,
  - fi_tvs may be shorter than tyConTyVars of rep_tc.
  - fi_tys may be shorter than tyConArity of the family tycon
       i.e. LHS is unsaturated
  - fi_rhs will be (rep_tc fi_tvs)
       i.e. RHS is un-saturated

  But when fi_flavour = SynFamilyInst,
  - fi_tys has the exact arity of the family tycon
-}

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
  = hang (pprFamInstHdr famInst) 2 (ifPprDebug debug_stuff)
  where
    ax = fi_axiom famInst
    debug_stuff = vcat [ text "Coercion axiom:" <+> ppr ax
                       , text "Tvs:" <+> ppr (fi_tvs famInst)
                       , text "LHS:" <+> ppr (fi_tys famInst)
                       , text "RHS:" <+> ppr (fi_rhs famInst) ]

pprFamInstHdr :: FamInst -> SDoc
pprFamInstHdr fi@(FamInst {fi_flavor = flavor})
  = pprTyConSort <+> pp_instance <+> pp_head
  where
    -- For *associated* types, say "type T Int = blah"
    -- For *top level* type instances, say "type instance T Int = blah"
    pp_instance
      | isTyConAssoc fam_tc = empty
      | otherwise           = text "instance"

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
                     SynFamilyInst        -> text "type"
                     DataFamilyInst tycon
                       | isDataTyCon     tycon -> text "data"
                       | isNewTyCon      tycon -> text "newtype"
                       | isAbstractTyCon tycon -> text "data"
                       | otherwise             -> text "WEIRD" <+> ppr tycon

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
      fi_cvs    = cvs,
      fi_tys    = tys,
      fi_rhs    = rhs,
      fi_axiom  = axiom,
      fi_flavor = flavor }
  where
     -- See Note [Lazy axiom match]
     ~(CoAxBranch { cab_lhs = tys
                  , cab_tvs = tvs
                  , cab_cvs = cvs
                  , cab_rhs = rhs }) = coAxiomSingleBranch axiom

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
the user explains when to use a data family instance by using constructors
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

These two axioms for T, one with one pattern, one with two;
see Note [Eta reduction for data families]

Note [FamInstEnv determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We turn FamInstEnvs into a list in some places that don't directly affect
the ABI. That happens in family consistency checks and when producing output
for `:info`. Unfortunately that nondeterminism is nonlocal and it's hard
to tell what it affects without following a chain of functions. It's also
easy to accidentally make that nondeterminism affect the ABI. Furthermore
the envs should be relatively small, so it should be free to use deterministic
maps here. Testing with nofib and validate detected no difference between
UniqFM and UniqDFM.
See Note [Deterministic UniqFM].
-}

type FamInstEnv = UniqDFM FamilyInstEnv  -- Maps a family to its instances
     -- See Note [FamInstEnv]
     -- See Note [FamInstEnv determinism]

type FamInstEnvs = (FamInstEnv, FamInstEnv)
     -- External package inst-env, Home-package inst-env

newtype FamilyInstEnv
  = FamIE [FamInst]     -- The instances for a particular family, in any order

instance Outputable FamilyInstEnv where
  ppr (FamIE fs) = text "FamIE" <+> vcat (map ppr fs)

-- INVARIANTS:
--  * The fs_tvs are distinct in each FamInst
--      of a range value of the map (so we can safely unify them)

emptyFamInstEnvs :: (FamInstEnv, FamInstEnv)
emptyFamInstEnvs = (emptyFamInstEnv, emptyFamInstEnv)

emptyFamInstEnv :: FamInstEnv
emptyFamInstEnv = emptyUDFM

famInstEnvElts :: FamInstEnv -> [FamInst]
famInstEnvElts fi = [elt | FamIE elts <- eltsUDFM fi, elt <- elts]
  -- See Note [FamInstEnv determinism]

famInstEnvSize :: FamInstEnv -> Int
famInstEnvSize = nonDetFoldUDFM (\(FamIE elt) sum -> sum + length elt) 0
  -- It's OK to use nonDetFoldUDFM here since we're just computing the
  -- size.

familyInstances :: (FamInstEnv, FamInstEnv) -> TyCon -> [FamInst]
familyInstances (pkg_fie, home_fie) fam
  = get home_fie ++ get pkg_fie
  where
    get env = case lookupUDFM env fam of
                Just (FamIE insts) -> insts
                Nothing                      -> []

extendFamInstEnvList :: FamInstEnv -> [FamInst] -> FamInstEnv
extendFamInstEnvList inst_env fis = foldl extendFamInstEnv inst_env fis

extendFamInstEnv :: FamInstEnv -> FamInst -> FamInstEnv
extendFamInstEnv inst_env
                 ins_item@(FamInst {fi_fam = cls_nm})
  = addToUDFM_C add inst_env cls_nm (FamIE [ins_item])
  where
    add (FamIE items) _ = FamIE (ins_item:items)

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
  = case tcUnifyTysFG (const BindMe) lhs1 lhs2 of
      SurelyApart -> True
      Unifiable subst
        | Type.substTyAddInScope subst rhs1 `eqType`
          Type.substTyAddInScope subst rhs2
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
computeAxiomIncomps :: [CoAxBranch] -> [CoAxBranch]
computeAxiomIncomps branches
  = snd (mapAccumL go [] branches)
  where
    go :: [CoAxBranch] -> CoAxBranch -> ([CoAxBranch], CoAxBranch)
    go prev_brs cur_br
       = (cur_br : prev_brs, new_br)
       where
         new_br = cur_br { cab_incomps = mk_incomps prev_brs cur_br }

    mk_incomps :: [CoAxBranch] -> CoAxBranch -> [CoAxBranch]
    mk_incomps prev_brs cur_br
       = filter (not . compatibleBranches cur_br) prev_brs

{-
************************************************************************
*                                                                      *
           Constructing axioms
    These functions are here because tidyType / tcUnifyTysFG
    are not available in CoAxiom

    Also computeAxiomIncomps is too sophisticated for CoAxiom
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
             -> [CoVar] -- possibly stale covars
             -> [Type]  -- LHS patterns
             -> Type    -- RHS
             -> [Role]
             -> SrcSpan
             -> CoAxBranch
mkCoAxBranch tvs cvs lhs rhs roles loc
  = CoAxBranch { cab_tvs     = tvs1
               , cab_cvs     = cvs1
               , cab_lhs     = tidyTypes env lhs
               , cab_roles   = roles
               , cab_rhs     = tidyType  env rhs
               , cab_loc     = loc
               , cab_incomps = placeHolderIncomps }
  where
    (env1, tvs1) = tidyTyCoVarBndrs emptyTidyEnv tvs
    (env,  cvs1) = tidyTyCoVarBndrs env1         cvs
    -- See Note [Tidy axioms when we build them]

-- all of the following code is here to avoid mutual dependencies with
-- Coercion
mkBranchedCoAxiom :: Name -> TyCon -> [CoAxBranch] -> CoAxiom Branched
mkBranchedCoAxiom ax_name fam_tc branches
  = CoAxiom { co_ax_unique   = nameUnique ax_name
            , co_ax_name     = ax_name
            , co_ax_tc       = fam_tc
            , co_ax_role     = Nominal
            , co_ax_implicit = False
            , co_ax_branches = manyBranches (computeAxiomIncomps branches) }

mkUnbranchedCoAxiom :: Name -> TyCon -> CoAxBranch -> CoAxiom Unbranched
mkUnbranchedCoAxiom ax_name fam_tc branch
  = CoAxiom { co_ax_unique   = nameUnique ax_name
            , co_ax_name     = ax_name
            , co_ax_tc       = fam_tc
            , co_ax_role     = Nominal
            , co_ax_implicit = False
            , co_ax_branches = unbranched (branch { cab_incomps = [] }) }

mkSingleCoAxiom :: Role -> Name
                -> [TyVar] -> [CoVar] -> TyCon -> [Type] -> Type
                -> CoAxiom Unbranched
-- Make a single-branch CoAxiom, incluidng making the branch itself
-- Used for both type family (Nominal) and data family (Representational)
-- axioms, hence passing in the Role
mkSingleCoAxiom role ax_name tvs cvs fam_tc lhs_tys rhs_ty
  = CoAxiom { co_ax_unique   = nameUnique ax_name
            , co_ax_name     = ax_name
            , co_ax_tc       = fam_tc
            , co_ax_role     = role
            , co_ax_implicit = False
            , co_ax_branches = unbranched (branch { cab_incomps = [] }) }
  where
    branch = mkCoAxBranch tvs cvs lhs_tys rhs_ty
                          (map (const Nominal) tvs)
                          (getSrcSpan ax_name)

-- | Create a coercion constructor (axiom) suitable for the given
--   newtype 'TyCon'. The 'Name' should be that of a new coercion
--   'CoAxiom', the 'TyVar's the arguments expected by the @newtype@ and
--   the type the appropriate right hand side of the @newtype@, with
--   the free variables a subset of those 'TyVar's.
mkNewTypeCoAxiom :: Name -> TyCon -> [TyVar] -> [Role] -> Type -> CoAxiom Unbranched
mkNewTypeCoAxiom name tycon tvs roles rhs_ty
  = CoAxiom { co_ax_unique   = nameUnique name
            , co_ax_name     = name
            , co_ax_implicit = True  -- See Note [Implicit axioms] in TyCon
            , co_ax_role     = Representational
            , co_ax_tc       = tycon
            , co_ax_branches = unbranched (branch { cab_incomps = [] }) }
  where
    branch = mkCoAxBranch tvs [] (mkTyVarTys tvs) rhs_ty
                          roles (getSrcSpan name)

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
                                 , fim_cos      :: [Coercion]
                                 }
  -- See Note [Over-saturated matches]

instance Outputable FamInstMatch where
  ppr (FamInstMatch { fim_instance = inst
                    , fim_tys      = tys
                    , fim_cos      = cos })
    = text "match with" <+> parens (ppr inst) <+> ppr tys <+> ppr cos

lookupFamInstEnvByTyCon :: FamInstEnvs -> TyCon -> [FamInst]
lookupFamInstEnvByTyCon (pkg_ie, home_ie) fam_tc
  = get pkg_ie ++ get home_ie
  where
    get ie = case lookupUDFM ie fam_tc of
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
     match _ _ tpl_tys tys = tcMatchTys tpl_tys tys

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
       = ASSERT2( tyCoVarsOfTypes tys `disjointVarSet` tpl_tvs,
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
    -> [CoAxBranch]   -- conflicting instance declarations
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
          | isOpenFamilyTyCon fam, Just (FamIE insts) <- lookupUDFM ie fam
          = map (coAxiomSingleBranch . fi_axiom) $
            filter isInjConflict insts
          | otherwise = []


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
              -> Maybe TCvSubst

lookup_fam_inst_env'          -- The worker, local to this module
    :: MatchFun
    -> FamInstEnv
    -> TyCon -> [Type]        -- What we are looking for
    -> [FamInstMatch]
lookup_fam_inst_env' match_fun ie fam match_tys
  | isOpenFamilyTyCon fam
  , Just (FamIE insts) <- lookupUDFM ie fam
  = find insts    -- The common case
  | otherwise = []
  where

    find [] = []
    find (item@(FamInst { fi_tcs = mb_tcs, fi_tvs = tpl_tvs, fi_cvs = tpl_cvs
                        , fi_tys = tpl_tys }) : rest)
        -- Fast check for no match, uses the "rough match" fields
      | instanceCantMatch rough_tcs mb_tcs
      = find rest

        -- Proper check
      | Just subst <- match_fun item (mkVarSet tpl_tvs) tpl_tys match_tys1
      = (FamInstMatch { fim_instance = item
                      , fim_tys      = substTyVars subst tpl_tvs `chkAppend` match_tys2
                      , fim_cos      = ASSERT( all (isJust . lookupCoVar subst) tpl_cvs )
                                       substCoVars subst tpl_cvs
                      })
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
   instance.  Why?  Because of eta reduction; see
   Note [Eta reduction for data families].
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
      match (CoAxBranch { cab_lhs = tys })
        = isJust $ tcMatchTys tys lhs

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
--
-- Always returns a *homogeneous* coercion -- type family reductions are always
-- homogeneous
reduceTyFamApp_maybe envs role tc tys
  | Phantom <- role
  = Nothing

  | case role of
      Representational -> isOpenFamilyTyCon     tc
      _                -> isOpenTypeFamilyTyCon tc
       -- If we seek a representational coercion
       -- (e.g. the call in topNormaliseType_maybe) then we can
       -- unwrap data families as well as type-synonym families;
       -- otherwise only type-synonym families
  , FamInstMatch { fim_instance = FamInst { fi_axiom = ax }
                 , fim_tys      = inst_tys
                 , fim_cos      = inst_cos } : _ <- lookupFamInstEnv envs tc tys
      -- NB: Allow multiple matches because of compatible overlap

  = let co = mkUnbranchedAxInstCo role ax inst_tys inst_cos
        ty = pSnd (coercionKind co)
    in Just (co, ty)

  | Just ax <- isClosedSynFamilyTyConWithAxiom_maybe tc
  , Just (ind, inst_tys, inst_cos) <- chooseBranch ax tys
  = let co = mkAxInstCo role ax ind inst_tys inst_cos
        ty = pSnd (coercionKind co)
    in Just (co, ty)

  | Just ax           <- isBuiltInSynFamTyCon_maybe tc
  , Just (coax,ts,ty) <- sfMatchFam ax tys
  = let co = mkAxiomRuleCo coax (zipWith mkReflCo (coaxrAsmpRoles coax) ts)
    in Just (co, ty)

  | otherwise
  = Nothing

-- The axiom can be oversaturated. (Closed families only.)
chooseBranch :: CoAxiom Branched -> [Type]
             -> Maybe (BranchIndex, [Type], [Coercion])  -- found match, with args
chooseBranch axiom tys
  = do { let num_pats = coAxiomNumPats axiom
             (target_tys, extra_tys) = splitAt num_pats tys
             branches = coAxiomBranches axiom
       ; (ind, inst_tys, inst_cos)
           <- findBranch (fromBranches branches) target_tys
       ; return ( ind, inst_tys `chkAppend` extra_tys, inst_cos ) }

-- The axiom must *not* be oversaturated
findBranch :: [CoAxBranch]             -- branches to check
           -> [Type]                   -- target types
           -> Maybe (BranchIndex, [Type], [Coercion])
    -- coercions relate requested types to returned axiom LHS at role N
findBranch branches target_tys
  = go 0 branches
  where
    go ind (branch@(CoAxBranch { cab_tvs = tpl_tvs, cab_cvs = tpl_cvs
                               , cab_lhs = tpl_lhs
                               , cab_incomps = incomps }) : rest)
      = let in_scope = mkInScopeSet (unionVarSets $
                            map (tyCoVarsOfTypes . coAxBranchLHS) incomps)
            -- See Note [Flattening] below
            flattened_target = flattenTys in_scope target_tys
        in case tcMatchTys tpl_lhs target_tys of
        Just subst -- matching worked. now, check for apartness.
          |  apartnessCheck flattened_target branch
          -> -- matching worked & we're apart from all incompatible branches.
             -- success
             ASSERT( all (isJust . lookupCoVar subst) tpl_cvs )
             Just (ind, substTyVars subst tpl_tvs, substCoVars subst tpl_cvs)

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
         . tcUnifyTysFG (const BindMe) flattened_target
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

Note [Normalising types]
~~~~~~~~~~~~~~~~~~~~~~~~
The topNormaliseType function removes all occurrences of type families
and newtypes from the top-level structure of a type. normaliseTcApp does
the type family lookup and is fairly straightforward. normaliseType is
a little more involved.

The complication comes from the fact that a type family might be used in the
kind of a variable bound in a forall. We wish to remove this type family
application, but that means coming up with a fresh variable (with the new
kind). Thus, we need a substitution to be built up as we recur through the
type. However, an ordinary TCvSubst just won't do: when we hit a type variable
whose kind has changed during normalisation, we need both the new type
variable *and* the coercion. We could conjure up a new VarEnv with just this
property, but a usable substitution environment already exists:
LiftingContexts from the liftCoSubst family of functions, defined in Coercion.
A LiftingContext maps a type variable to a coercion and a coercion variable to
a pair of coercions. Let's ignore coercion variables for now. Because the
coercion a type variable maps to contains the destination type (via
coercionKind), we don't need to store that destination type separately. Thus,
a LiftingContext has what we need: a map from type variables to (Coercion,
Type) pairs.

We also benefit because we can piggyback on the liftCoSubstVarBndr function to
deal with binders. However, I had to modify that function to work with this
application. Thus, we now have liftCoSubstVarBndrCallback, which takes
a function used to process the kind of the binder. We don't wish
to lift the kind, but instead normalise it. So, we pass in a callback function
that processes the kind of the binder.

After that brilliant explanation of all this, I'm sure you've forgotten the
dangling reference to coercion variables. What do we do with those? Nothing at
all. The point of normalising types is to remove type family applications, but
there's no sense in removing these from coercions. We would just get back a
new coercion witnessing the equality between the same types as the original
coercion. Because coercions are irrelevant anyway, there is no point in doing
this. So, whenever we encounter a coercion, we just say that it won't change.
That's what the CoercionTy case is doing within normalise_type.

Note [Normalisation and type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to be a bit careful about normalising in the presence of type
synonyms (Trac #13035).  Suppose S is a type synonym, and we have
   S t1 t2
If S is family-free (on its RHS) we can just normalise t1 and t2 and
reconstruct (S t1' t2').   Expanding S could not reveal any new redexes
because type families are saturated.

But if S has a type family on its RHS we expand /before/ normalising
the args t1, t2.  If we normalise t1, t2 first, we'll re-normalise them
after expansion, and that can lead to /exponential/ behavour; see Trac #13035.

Notice, though, that expanding first can in principle duplicate t1,t2,
which might contain redexes. I'm sure you could conjure up an exponential
case by that route too, but it hasn't happened in practice yet!
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
--   topNormaliseType_maybe env ty = Just (co, ty')
-- then
--   (a) co :: ty ~R ty'
--   (b) ty' is not a newtype, and is not a type-family or data-family redex
--
-- However, ty' can be something like (Maybe (F ty)), where
-- (F ty) is a redex.

topNormaliseType_maybe env ty
  = topNormaliseTypeX stepper mkTransCo ty
  where
    stepper = unwrapNewTypeStepper `composeSteppers` tyFamStepper

    tyFamStepper rec_nts tc tys  -- Try to step a type/data family
      = let (args_co, ntys) = normaliseTcArgs env Representational tc tys in
          -- NB: It's OK to use normaliseTcArgs here instead of
          -- normalise_tc_args (which takes the LiftingContext described
          -- in Note [Normalising types]) because the reduceTyFamApp below
          -- works only at top level. We'll never recur in this function
          -- after reducing the kind of a bound tyvar.

        case reduceTyFamApp_maybe env Representational tc ntys of
          Just (co, rhs) -> NS_Step rec_nts rhs (args_co `mkTransCo` co)
          _              -> NS_Done

---------------
pmTopNormaliseType_maybe :: FamInstEnvs -> Type -> Maybe (Type, [DataCon], Type)
-- ^ Get rid of *outermost* (or toplevel)
--      * type function redex
--      * data family redex
--      * newtypes
--
-- Behaves exactly like `topNormaliseType_maybe`, but instead of returning a
-- coercion, it returns useful information for issuing pattern matching
-- warnings. See Note [Type normalisation for EmptyCase] for details.
pmTopNormaliseType_maybe env typ
  = do ((ty_f,tm_f), ty) <- topNormaliseTypeX stepper comb typ
       return (eq_src_ty ty (typ : ty_f [ty]), tm_f [], ty)
  where
    -- Find the first type in the sequence of rewrites that is a data type,
    -- newtype, or a data family application (not the representation tycon!).
    -- This is the one that is equal (in source Haskell) to the initial type.
    -- If none is found in the list, then all of them are type family
    -- applications, so we simply return the last one, which is the *simplest*.
    eq_src_ty :: Type -> [Type] -> Type
    eq_src_ty ty tys = maybe ty id (find is_alg_or_data_family tys)

    is_alg_or_data_family :: Type -> Bool
    is_alg_or_data_family ty = isClosedAlgType ty || isDataFamilyAppType ty

    -- For efficiency, represent both lists as difference lists.
    -- comb performs the concatenation, for both lists.
    comb (tyf1, tmf1) (tyf2, tmf2) = (tyf1 . tyf2, tmf1 . tmf2)

    stepper = newTypeStepper `composeSteppers` tyFamStepper

    -- A 'NormaliseStepper' that unwraps newtypes, careful not to fall into
    -- a loop. If it would fall into a loop, it produces 'NS_Abort'.
    newTypeStepper :: NormaliseStepper ([Type] -> [Type],[DataCon] -> [DataCon])
    newTypeStepper rec_nts tc tys
      | Just (ty', _co) <- instNewTyCon_maybe tc tys
      = case checkRecTc rec_nts tc of
          Just rec_nts' -> let tyf = ((TyConApp tc tys):)
                               tmf = ((tyConSingleDataCon tc):)
                           in  NS_Step rec_nts' ty' (tyf, tmf)
          Nothing       -> NS_Abort
      | otherwise
      = NS_Done

    tyFamStepper :: NormaliseStepper ([Type] -> [Type], [DataCon] -> [DataCon])
    tyFamStepper rec_nts tc tys  -- Try to step a type/data family
      = let (_args_co, ntys) = normaliseTcArgs env Representational tc tys in
          -- NB: It's OK to use normaliseTcArgs here instead of
          -- normalise_tc_args (which takes the LiftingContext described
          -- in Note [Normalising types]) because the reduceTyFamApp below
          -- works only at top level. We'll never recur in this function
          -- after reducing the kind of a bound tyvar.

        case reduceTyFamApp_maybe env Representational tc ntys of
          Just (_co, rhs) -> NS_Step rec_nts rhs ((rhs:), id)
          _               -> NS_Done

{- Note [Type normalisation for EmptyCase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EmptyCase is an exception for pattern matching, since it is strict. This means
that it boils down to checking whether the type of the scrutinee is inhabited.
Function pmTopNormaliseType_maybe gets rid of the outermost type function/data
family redex and newtypes, in search of an algebraic type constructor, which is
easier to check for inhabitation.

It returns 3 results instead of one, because there are 2 subtle points:
1. Newtypes are isomorphic to the underlying type in core but not in the source
   language,
2. The representational data family tycon is used internally but should not be
   shown to the user

Hence, if pmTopNormaliseType_maybe env ty = Just (src_ty, dcs, core_ty), then
  (a) src_ty is the rewritten type which we can show to the user. That is, the
      type we get if we rewrite type families but not data families or
      newtypes.
  (b) dcs is the list of data constructors "skipped", every time we normalise a
      newtype to it's core representation, we keep track of the source data
      constructor.
  (c) core_ty is the rewritten type. That is,
        pmTopNormaliseType_maybe env ty = Just (src_ty, dcs, core_ty)
      implies
        topNormaliseType_maybe env ty = Just (co, core_ty)
      for some coercion co.

To see how all cases come into play, consider the following example:

  data family T a :: *
  data instance T Int = T1 | T2 Bool
  -- Which gives rise to FC:
  --   data T a
  --   data R:TInt = T1 | T2 Bool
  --   axiom ax_ti : T Int ~R R:TInt

  newtype G1 = MkG1 (T Int)
  newtype G2 = MkG2 G1

  type instance F Int  = F Char
  type instance F Char = G2

In this case pmTopNormaliseType_maybe env (F Int) results in

  Just (G2, [MkG2,MkG1], R:TInt)

Which means that in source Haskell:
  - G2 is equivalent to F Int (in contrast, G1 isn't).
  - if (x : R:TInt) then (MkG2 (MkG1 x) : F Int).
-}

---------------
normaliseTcApp :: FamInstEnvs -> Role -> TyCon -> [Type] -> (Coercion, Type)
-- See comments on normaliseType for the arguments of this function
normaliseTcApp env role tc tys
  = initNormM env role (tyCoVarsOfTypes tys) $
    normalise_tc_app tc tys

-- See Note [Normalising types] about the LiftingContext
normalise_tc_app :: TyCon -> [Type] -> NormM (Coercion, Type)
normalise_tc_app tc tys
  | Just (tenv, rhs, tys') <- expandSynTyCon_maybe tc tys
  , not (isFamFreeTyCon tc)  -- Expand and try again
  = -- A synonym with type families in the RHS
    -- Expand and try again
    -- See Note [Normalisation and type synonyms]
    normalise_type (mkAppTys (substTy (mkTvSubstPrs tenv) rhs) tys')

  | not (isTypeFamilyTyCon tc)
  = -- A synonym with no type families in the RHS; or data type etc
    -- Just normalise the arguments and rebuild
    do { (args_co, ntys) <- normalise_tc_args tc tys
       ; return (args_co, mkTyConApp tc ntys) }

  | otherwise
  = -- A type-family application
    do { env <- getEnv
       ; role <- getRole
       ; (args_co, ntys) <- normalise_tc_args tc tys
       ; case reduceTyFamApp_maybe env role tc ntys of
           Just (first_co, ty')
             -> do { (rest_co,nty) <- normalise_type ty'
                   ; return ( args_co `mkTransCo` first_co `mkTransCo` rest_co
                            , nty ) }
           _ -> -- No unique matching family instance exists;
                -- we do not do anything
                return (args_co, mkTyConApp tc ntys) }

---------------
-- | Normalise arguments to a tycon
normaliseTcArgs :: FamInstEnvs          -- ^ env't with family instances
                -> Role                 -- ^ desired role of output coercion
                -> TyCon                -- ^ tc
                -> [Type]               -- ^ tys
                -> (Coercion, [Type])   -- ^ co :: tc tys ~ tc new_tys
normaliseTcArgs env role tc tys
  = initNormM env role (tyCoVarsOfTypes tys) $
    normalise_tc_args tc tys

normalise_tc_args :: TyCon -> [Type]             -- tc tys
                  -> NormM (Coercion, [Type])    -- (co, new_tys), where
                                                 -- co :: tc tys ~ tc new_tys
normalise_tc_args tc tys
  = do { role <- getRole
       ; (cois, ntys) <- zipWithAndUnzipM normalise_type_role
                                          tys (tyConRolesX role tc)
       ; return (mkTyConAppCo role tc cois, ntys) }
  where
    normalise_type_role ty r = withRole r $ normalise_type ty

---------------
normaliseType :: FamInstEnvs
              -> Role  -- desired role of coercion
              -> Type -> (Coercion, Type)
normaliseType env role ty
  = initNormM env role (tyCoVarsOfType ty) $ normalise_type ty

normalise_type :: Type                     -- old type
               -> NormM (Coercion, Type)   -- (coercion,new type), where
                                         -- co :: old-type ~ new_type
-- Normalise the input type, by eliminating *all* type-function redexes
-- but *not* newtypes (which are visible to the programmer)
-- Returns with Refl if nothing happens
-- Does nothing to newtypes
-- The returned coercion *must* be *homogeneous*
-- See Note [Normalising types]
-- Try to not to disturb type synonyms if possible

normalise_type ty
  = go ty
  where
    go (TyConApp tc tys) = normalise_tc_app tc tys
    go ty@(LitTy {})     = do { r <- getRole
                              ; return (mkReflCo r ty, ty) }
    go (AppTy ty1 ty2)
      = do { (co,  nty1) <- go ty1
           ; (arg, nty2) <- withRole Nominal $ go ty2
           ; return (mkAppCo co arg, mkAppTy nty1 nty2) }
    go (FunTy ty1 ty2)
      = do { (co1, nty1) <- go ty1
           ; (co2, nty2) <- go ty2
           ; r <- getRole
           ; return (mkFunCo r co1 co2, mkFunTy nty1 nty2) }
    go (ForAllTy (TvBndr tyvar vis) ty)
      = do { (lc', tv', h, ki') <- normalise_tyvar_bndr tyvar
           ; (co, nty)          <- withLC lc' $ normalise_type ty
           ; let tv2 = setTyVarKind tv' ki'
           ; return (mkForAllCo tv' h co, ForAllTy (TvBndr tv2 vis) nty) }
    go (TyVarTy tv)    = normalise_tyvar tv
    go (CastTy ty co)
      = do { (nco, nty) <- go ty
           ; lc <- getLC
           ; let co' = substRightCo lc co
           ; return (castCoercionKind nco co co', mkCastTy nty co') }
    go (CoercionTy co)
      = do { lc <- getLC
           ; r <- getRole
           ; let right_co = substRightCo lc co
           ; return ( mkProofIrrelCo r
                         (liftCoSubst Nominal lc (coercionType co))
                         co right_co
                    , mkCoercionTy right_co ) }

normalise_tyvar :: TyVar -> NormM (Coercion, Type)
normalise_tyvar tv
  = ASSERT( isTyVar tv )
    do { lc <- getLC
       ; r  <- getRole
       ; return $ case liftCoSubstTyVar lc r tv of
           Just co -> (co, pSnd $ coercionKind co)
           Nothing -> (mkReflCo r ty, ty) }
  where ty = mkTyVarTy tv

normalise_tyvar_bndr :: TyVar -> NormM (LiftingContext, TyVar, Coercion, Kind)
normalise_tyvar_bndr tv
  = do { lc1 <- getLC
       ; env <- getEnv
       ; let callback lc ki = runNormM (normalise_type ki) env lc Nominal
       ; return $ liftCoSubstVarBndrCallback callback lc1 tv }

-- | a monad for the normalisation functions, reading 'FamInstEnvs',
-- a 'LiftingContext', and a 'Role'.
newtype NormM a = NormM { runNormM ::
                            FamInstEnvs -> LiftingContext -> Role -> a }

initNormM :: FamInstEnvs -> Role
          -> TyCoVarSet   -- the in-scope variables
          -> NormM a -> a
initNormM env role vars (NormM thing_inside)
  = thing_inside env lc role
  where
    in_scope = mkInScopeSet vars
    lc       = emptyLiftingContext in_scope

getRole :: NormM Role
getRole = NormM (\ _ _ r -> r)

getLC :: NormM LiftingContext
getLC = NormM (\ _ lc _ -> lc)

getEnv :: NormM FamInstEnvs
getEnv = NormM (\ env _ _ -> env)

withRole :: Role -> NormM a -> NormM a
withRole r thing = NormM $ \ envs lc _old_r -> runNormM thing envs lc r

withLC :: LiftingContext -> NormM a -> NormM a
withLC lc thing = NormM $ \ envs _old_lc r -> runNormM thing envs lc r

instance Monad NormM where
  ma >>= fmb = NormM $ \env lc r ->
               let a = runNormM ma env lc r in
               runNormM (fmb a) env lc r

instance Functor NormM where
  fmap = liftM
instance Applicative NormM where
  pure x = NormM $ \ _ _ _ -> x
  (<*>)  = ap

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

data FlattenEnv = FlattenEnv { fe_type_map :: TypeMap TyVar
                             , fe_subst    :: TCvSubst }

emptyFlattenEnv :: InScopeSet -> FlattenEnv
emptyFlattenEnv in_scope
  = FlattenEnv { fe_type_map = emptyTypeMap
               , fe_subst    = mkEmptyTCvSubst in_scope }

-- See Note [Flattening]
flattenTys :: InScopeSet -> [Type] -> [Type]
flattenTys in_scope tys = snd $ coreFlattenTys env tys
  where
    -- when we hit a type function, we replace it with a fresh variable
    -- but, we need to make sure that this fresh variable isn't mentioned
    -- *anywhere* in the types we're flattening, even if locally-bound in
    -- a forall. That way, we can ensure consistency both within and outside
    -- of that forall.
    all_in_scope = in_scope `extendInScopeSetSet` allTyVarsInTys tys
    env          = emptyFlattenEnv all_in_scope

coreFlattenTys :: FlattenEnv -> [Type] -> (FlattenEnv, [Type])
coreFlattenTys = go []
  where
    go rtys env []         = (env, reverse rtys)
    go rtys env (ty : tys)
      = let (env', ty') = coreFlattenTy env ty in
        go (ty' : rtys) env' tys

coreFlattenTy :: FlattenEnv -> Type -> (FlattenEnv, Type)
coreFlattenTy = go
  where
    go env ty | Just ty' <- coreView ty = go env ty'

    go env (TyVarTy tv)    = (env, substTyVar (fe_subst env) tv)
    go env (AppTy ty1 ty2) = let (env1, ty1') = go env  ty1
                                 (env2, ty2') = go env1 ty2 in
                             (env2, AppTy ty1' ty2')
    go env (TyConApp tc tys)
         -- NB: Don't just check if isFamilyTyCon: this catches *data* families,
         -- which are generative and thus can be preserved during flattening
      | not (isGenerativeTyCon tc Nominal)
      = let (env', tv) = coreFlattenTyFamApp env tc tys in
        (env', mkTyVarTy tv)

      | otherwise
      = let (env', tys') = coreFlattenTys env tys in
        (env', mkTyConApp tc tys')

    go env (FunTy ty1 ty2) = let (env1, ty1') = go env  ty1
                                 (env2, ty2') = go env1 ty2 in
                             (env2, mkFunTy ty1' ty2')

    go env (ForAllTy (TvBndr tv vis) ty)
      = let (env1, tv') = coreFlattenVarBndr env tv
            (env2, ty') = go env1 ty in
        (env2, ForAllTy (TvBndr tv' vis) ty')

    go env ty@(LitTy {}) = (env, ty)

    go env (CastTy ty co) = let (env1, ty') = go env ty
                                (env2, co') = coreFlattenCo env1 co in
                            (env2, CastTy ty' co')

    go env (CoercionTy co) = let (env', co') = coreFlattenCo env co in
                             (env', CoercionTy co')

-- when flattening, we don't care about the contents of coercions.
-- so, just return a fresh variable of the right (flattened) type
coreFlattenCo :: FlattenEnv -> Coercion -> (FlattenEnv, Coercion)
coreFlattenCo env co
  = (env2, mkCoVarCo covar)
  where
    (env1, kind') = coreFlattenTy env (coercionType co)
    fresh_name    = mkFlattenFreshCoName
    subst1        = fe_subst env1
    in_scope      = getTCvInScope subst1
    covar         = uniqAway in_scope (mkCoVar fresh_name kind')
    env2          = env1 { fe_subst = subst1 `extendTCvInScope` covar }

coreFlattenVarBndr :: FlattenEnv -> TyVar -> (FlattenEnv, TyVar)
coreFlattenVarBndr env tv
  | kind' `eqType` kind
  = ( env { fe_subst = extendTvSubst old_subst tv (mkTyVarTy tv) }
             -- override any previous binding for tv
    , tv)

  | otherwise
  = let new_tv    = uniqAway (getTCvInScope old_subst) (setTyVarKind tv kind')
        new_subst = extendTvSubstWithClone old_subst tv new_tv
    in
    (env' { fe_subst = new_subst }, new_tv)
  where
    kind          = tyVarKind tv
    (env', kind') = coreFlattenTy env kind
    old_subst     = fe_subst env

coreFlattenTyFamApp :: FlattenEnv
                    -> TyCon         -- type family tycon
                    -> [Type]        -- args
                    -> (FlattenEnv, TyVar)
coreFlattenTyFamApp env fam_tc fam_args
  = case lookupTypeMap type_map fam_ty of
      Just tv -> (env, tv)
              -- we need fresh variables here, but this is called far from
              -- any good source of uniques. So, we just use the fam_tc's unique
              -- and trust uniqAway to avoid clashes. Recall that the in_scope set
              -- contains *all* tyvars, even locally bound ones elsewhere in the
              -- overall type, so this really is fresh.
      Nothing -> let tyvar_name = mkFlattenFreshTyName fam_tc
                     tv = uniqAway (getTCvInScope subst) $
                          mkTyVar tyvar_name (typeKind fam_ty)
                     env' = env { fe_type_map = extendTypeMap type_map fam_ty tv
                                , fe_subst = extendTCvInScope subst tv }
                 in (env', tv)
  where fam_ty   = mkTyConApp fam_tc fam_args
        FlattenEnv { fe_type_map = type_map
                   , fe_subst = subst } = env

-- | Get the set of all type variables mentioned anywhere in the list
-- of types. These variables are not necessarily free.
allTyVarsInTys :: [Type] -> VarSet
allTyVarsInTys []       = emptyVarSet
allTyVarsInTys (ty:tys) = allTyVarsInTy ty `unionVarSet` allTyVarsInTys tys

-- | Get the set of all type variables mentioned anywhere in a type.
allTyVarsInTy :: Type -> VarSet
allTyVarsInTy = go
  where
    go (TyVarTy tv)      = unitVarSet tv
    go (TyConApp _ tys)  = allTyVarsInTys tys
    go (AppTy ty1 ty2)   = (go ty1) `unionVarSet` (go ty2)
    go (FunTy ty1 ty2)   = (go ty1) `unionVarSet` (go ty2)
    go (ForAllTy (TvBndr tv _) ty) = unitVarSet tv     `unionVarSet`
                                     go (tyVarKind tv) `unionVarSet`
                                     go ty
                                     -- Don't remove the tv from the set!
    go (LitTy {})        = emptyVarSet
    go (CastTy ty co)    = go ty `unionVarSet` go_co co
    go (CoercionTy co)   = go_co co

    go_co (Refl _ ty)           = go ty
    go_co (TyConAppCo _ _ args) = go_cos args
    go_co (AppCo co arg)        = go_co co `unionVarSet` go_co arg
    go_co (ForAllCo tv h co)
      = unionVarSets [unitVarSet tv, go_co co, go_co h]
    go_co (FunCo _ c1 c2)       = go_co c1 `unionVarSet` go_co c2
    go_co (CoVarCo cv)          = unitVarSet cv
    go_co (AxiomInstCo _ _ cos) = go_cos cos
    go_co (UnivCo p _ t1 t2)    = go_prov p `unionVarSet` go t1 `unionVarSet` go t2
    go_co (SymCo co)            = go_co co
    go_co (TransCo c1 c2)       = go_co c1 `unionVarSet` go_co c2
    go_co (NthCo _ co)          = go_co co
    go_co (LRCo _ co)           = go_co co
    go_co (InstCo co arg)       = go_co co `unionVarSet` go_co arg
    go_co (CoherenceCo c1 c2)   = go_co c1 `unionVarSet` go_co c2
    go_co (KindCo co)           = go_co co
    go_co (SubCo co)            = go_co co
    go_co (AxiomRuleCo _ cs)    = go_cos cs

    go_cos = foldr (unionVarSet . go_co) emptyVarSet

    go_prov UnsafeCoerceProv    = emptyVarSet
    go_prov (PhantomProv co)    = go_co co
    go_prov (ProofIrrelProv co) = go_co co
    go_prov (PluginProv _)      = emptyVarSet
    go_prov (HoleProv _)        = emptyVarSet

mkFlattenFreshTyName :: Uniquable a => a -> Name
mkFlattenFreshTyName unq
  = mkSysTvName (getUnique unq) (fsLit "flt")

mkFlattenFreshCoName :: Name
mkFlattenFreshCoName
  = mkSystemVarName (deriveUnique eqPrimTyConKey 71) (fsLit "flc")
