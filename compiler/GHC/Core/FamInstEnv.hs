{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
-- (c) The University of Glasgow 2006
--
-- FamInstEnv: Type checked family instance declarations

module GHC.Core.FamInstEnv (
        FamInst(..), FamFlavor(..), famInstAxiom, famInstTyCon, famInstRHS,
        famInstsRepTyCons, famInstRepTyCon_maybe, dataFamInstRepTyCon,
        pprFamInst, pprFamInsts,
        mkImportedFamInst,

        FamInstEnvs, FamInstEnv, emptyFamInstEnv, emptyFamInstEnvs,
        unionFamInstEnv, extendFamInstEnv, extendFamInstEnvList,
        famInstEnvElts, famInstEnvSize, familyInstances, familyNameInstances,

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
        topReduceTyFamApp_maybe, reduceTyFamApp_maybe
    ) where

import GHC.Prelude

import GHC.Core.Unify
import GHC.Core.Type as Type
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core.Reduction
import GHC.Core.RoughMap
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Name
import GHC.Data.Maybe
import GHC.Types.Var
import GHC.Types.SrcLoc
import Control.Monad
import Data.List( mapAccumL )
import Data.Array( Array, assocs )

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Data.Bag

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
                -- See Note [Rough matching in class and family instances]
                -- in GHC.Core.Unify
            , fi_tcs   :: [RoughMatchTc]  -- Top of type args
                -- INVARIANT: fi_tcs = roughMatchTcs fi_tys

            -- Used for "proper matching"; ditto
            , fi_tvs :: [TyVar]      -- Template tyvars for full match
            , fi_cvs :: [CoVar]      -- Template covars for full match
                 -- Like ClsInsts, these variables are always fresh
                 -- See Note [Template tyvars are fresh] in GHC.Core.InstEnv

            , fi_tys    :: [Type]       --   The LHS type patterns
            -- May be eta-reduced; see Note [Eta reduction for data families]
            -- in GHC.Core.Coercion.Axiom

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
 U1) Eta reduction. See Note [Eta reduction for data families] in
     GHC.Core.Coercion.Axiom.
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
      see Note [Eta reduction for data families] in GHC.Core.Coercion.Axiom.
      Accordingly, the FamInst is never over-saturated.

Why can we allow such flexibility for data families but not for type families?
Because data families can be decomposed -- that is, they are generative and
injective. A Type family is neither and so always must be applied to all its
arguments.
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

pprFamInst :: FamInst -> SDoc
-- Prints the FamInst as a family instance declaration
-- NB: This function, FamInstEnv.pprFamInst, is used only for internal,
--     debug printing. See GHC.Types.TyThing.Ppr.pprFamInst for printing for the user
pprFamInst (FamInst { fi_flavor = flavor, fi_axiom = ax
                    , fi_tvs = tvs, fi_tys = tys, fi_rhs = rhs })
  = hang (ppr_tc_sort <+> text "instance"
             <+> pprCoAxBranchUser (coAxiomTyCon ax) (coAxiomSingleBranch ax))
       2 (whenPprDebug debug_stuff)
  where
    ppr_tc_sort = case flavor of
                     SynFamilyInst             -> text "type"
                     DataFamilyInst tycon
                       | isDataTyCon     tycon -> text "data"
                       | isNewTyCon      tycon -> text "newtype"
                       | isAbstractTyCon tycon -> text "data"
                       | otherwise             -> text "WEIRD" <+> ppr tycon

    debug_stuff = vcat [ text "Coercion axiom:" <+> ppr ax
                       , text "Tvs:" <+> ppr tvs
                       , text "LHS:" <+> ppr tys
                       , text "RHS:" <+> ppr rhs ]

pprFamInsts :: [FamInst] -> SDoc
pprFamInsts finsts = vcat (map pprFamInst finsts)

{-
Note [Lazy axiom match]
~~~~~~~~~~~~~~~~~~~~~~~
It is Vitally Important that mkImportedFamInst is *lazy* in its axiom
parameter. The axiom is loaded lazily, via a forkM, in GHC.IfaceToCore. Sometime
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
                  -> [RoughMatchTc]     -- Rough match info
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
A FamInstEnv is a RoughMap of instance heads. Specifically, the keys are formed
by the family name and the instance arguments. That is, an instance:

    type instance Fam (Maybe Int) a

would insert into the instance environment an instance with a key of the form

  [RM_KnownTc Fam, RM_KnownTc Maybe, RM_WildCard]

See Note [RoughMap] in GHC.Core.RoughMap.


The same FamInstEnv includes both 'data family' and 'type family' instances.
Type families are reduced during type inference, but not data families;
the user explains when to use a data family instance by using constructors
and pattern matching.

Nevertheless it is still useful to have data families in the FamInstEnv:

 - For finding overlaps and conflicts

 - For finding the representation type...see FamInstEnv.topNormaliseType
   and its call site in GHC.Core.Opt.Simplify

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
see Note [Eta reduction for data families] in GHC.Core.Coercion.Axiom

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

type FamInstEnvs = (FamInstEnv, FamInstEnv)
     -- External package inst-env, Home-package inst-env

data FamInstEnv
  = FamIE !Int -- The number of instances, used to choose the smaller environment
               -- when checking type family consistnecy of home modules.
          !(RoughMap FamInst)
     -- See Note [FamInstEnv]
     -- See Note [FamInstEnv determinism]


instance Outputable FamInstEnv where
  ppr (FamIE _ fs) = text "FamIE" <+> vcat (map ppr $ elemsRM fs)

famInstEnvSize :: FamInstEnv -> Int
famInstEnvSize (FamIE sz _) = sz

-- | Create a 'FamInstEnv' from 'Name' indices.
-- INVARIANTS:
--  * The fs_tvs are distinct in each FamInst
--      of a range value of the map (so we can safely unify them)

emptyFamInstEnvs :: (FamInstEnv, FamInstEnv)
emptyFamInstEnvs = (emptyFamInstEnv, emptyFamInstEnv)

emptyFamInstEnv :: FamInstEnv
emptyFamInstEnv = FamIE 0 emptyRM

famInstEnvElts :: FamInstEnv -> [FamInst]
famInstEnvElts (FamIE _ rm) = elemsRM rm
  -- See Note [FamInstEnv determinism]

  -- It's OK to use nonDetStrictFoldUDFM here since we're just computing the
  -- size.

familyInstances :: (FamInstEnv, FamInstEnv) -> TyCon -> [FamInst]
familyInstances envs tc
  = familyNameInstances envs (tyConName tc)

familyNameInstances :: (FamInstEnv, FamInstEnv) -> Name -> [FamInst]
familyNameInstances (pkg_fie, home_fie) fam
  = get home_fie ++ get pkg_fie
  where
    get :: FamInstEnv -> [FamInst]
    get (FamIE _ env) = lookupRM [RML_KnownTc fam] env


-- | Makes no particular effort to detect conflicts.
unionFamInstEnv :: FamInstEnv -> FamInstEnv -> FamInstEnv
unionFamInstEnv (FamIE sa a) (FamIE sb b) = FamIE (sa + sb) (a `unionRM` b)

extendFamInstEnvList :: FamInstEnv -> [FamInst] -> FamInstEnv
extendFamInstEnvList inst_env fis = foldl' extendFamInstEnv inst_env fis

extendFamInstEnv :: FamInstEnv -> FamInst -> FamInstEnv
extendFamInstEnv (FamIE s inst_env)
                 ins_item@(FamInst {fi_fam = cls_nm})
  = FamIE (s+1) $ insertRM rough_tmpl ins_item inst_env
  where
    rough_tmpl = RM_KnownTc cls_nm : fi_tcs ins_item

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
applications into fresh variables. (See
Note [Flattening type-family applications when matching instances] in GHC.Core.Unify.)

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
no unifiers. The exact algorithm is different here because the
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

Note [Compatibility of eta-reduced axioms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In newtype instances of data families we eta-reduce the axioms,
See Note [Eta reduction for data families] in GHC.Core.Coercion.Axiom. This means that
we sometimes need to test compatibility of two axioms that were eta-reduced to
different degrees, e.g.:


data family D a b c
newtype instance D a Int c = DInt (Maybe a)
  -- D a Int ~ Maybe
  -- lhs = [a, Int]
newtype instance D Bool Int Char = DIntChar Float
  -- D Bool Int Char ~ Float
  -- lhs = [Bool, Int, Char]

These are obviously incompatible. We could detect this by saturating
(eta-expanding) the shorter LHS with fresh tyvars until the lists are of
equal length, but instead we can just remove the tail of the longer list, as
those types will simply unify with the freshly introduced tyvars.

By doing this, in case the LHS are unifiable, the yielded substitution won't
mention the tyvars that appear in the tail we dropped off, and we might try
to test equality RHSes of different kinds, but that's fine since this case
occurs only for data families, where the RHS is a unique tycon and the equality
fails anyway.
-}

-- See Note [Compatibility]
compatibleBranches :: CoAxBranch -> CoAxBranch -> Bool
compatibleBranches (CoAxBranch { cab_lhs = lhs1, cab_rhs = rhs1 })
                   (CoAxBranch { cab_lhs = lhs2, cab_rhs = rhs2 })
  = let (commonlhs1, commonlhs2) = zipAndUnzip lhs1 lhs2
             -- See Note [Compatibility of eta-reduced axioms]
    in case tcUnifyTysFG alwaysBindFun commonlhs1 commonlhs2 of
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
  -- See Note [Verifying injectivity annotation], case 1.
  = let getInjArgs  = filterByList injectivity
    in case tcUnifyTyWithTFs True rhs1 rhs2 of -- True = two-way pre-unification
       Nothing -> InjectivityAccepted
         -- RHS are different, so equations are injective.
         -- This is case 1A from Note [Verifying injectivity annotation]
       Just subst -> -- RHS unify under a substitution
        let lhs1Subst = Type.substTys subst (getInjArgs lhs1)
            lhs2Subst = Type.substTys subst (getInjArgs lhs2)
        -- If LHSs are equal under the substitution used for RHSs then this pair
        -- of equations does not violate injectivity annotation. If LHSs are not
        -- equal under that substitution then this pair of equations violates
        -- injectivity annotation, but for closed type families it still might
        -- be the case that one LHS after substitution is unreachable.
        in if eqTypes lhs1Subst lhs2Subst  -- check case 1B1 from Note.
           then InjectivityAccepted
           else InjectivityUnified ( ax1 { cab_lhs = Type.substTys subst lhs1
                                         , cab_rhs = Type.substTy  subst rhs1 })
                                   ( ax2 { cab_lhs = Type.substTys subst lhs2
                                         , cab_rhs = Type.substTy  subst rhs2 })
                -- payload of InjectivityUnified used only for check 1B2, only
                -- for closed type families

-- takes a CoAxiom with unknown branch incompatibilities and computes
-- the compatibilities
-- See Note [Storing compatibility] in GHC.Core.Coercion.Axiom
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
    are not available in GHC.Core.Coercion.Axiom

    Also computeAxiomIncomps is too sophisticated for CoAxiom
*                                                                      *
************************************************************************

Note [Tidy axioms when we build them]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Like types and classes, we build axioms fully quantified over all
their variables, and tidy them when we build them. For example,
we print out axioms and don't want to print stuff like
    F k k a b = ...
Instead we must tidy those kind variables.  See #7524.

We could instead tidy when we print, but that makes it harder to get
things like injectivity errors to come out right. Danger of
     Type family equation violates injectivity annotation.
     Kind variable ‘k’ cannot be inferred from the right-hand side.
     In the type family equation:
        PolyKindVars @[k1] @[k2] ('[] @k1) = '[] @k2

Note [Always number wildcard types in CoAxBranch]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following example (from the DataFamilyInstanceLHS test case):

  data family Sing (a :: k)
  data instance Sing (_ :: MyKind) where
      SingA :: Sing A
      SingB :: Sing B

If we're not careful during tidying, then when this program is compiled with
-ddump-types, we'll get the following information:

  COERCION AXIOMS
    axiom DataFamilyInstanceLHS.D:R:SingMyKind_0 ::
      Sing _ = DataFamilyInstanceLHS.R:SingMyKind_ _

It's misleading to have a wildcard type appearing on the RHS like
that. To avoid this issue, when building a CoAxiom (which is what eventually
gets printed above), we tidy all the variables in an env that already contains
'_'. Thus, any variable named '_' will be renamed, giving us the nicer output
here:

  COERCION AXIOMS
    axiom DataFamilyInstanceLHS.D:R:SingMyKind_0 ::
      Sing _1 = DataFamilyInstanceLHS.R:SingMyKind_ _1

Which is at least legal syntax.

See also Note [CoAxBranch type variables] in GHC.Core.Coercion.Axiom; note that we
are tidying (changing OccNames only), not freshening, in accordance with
that Note.
-}

-- all axiom roles are Nominal, as this is only used with type families
mkCoAxBranch :: [TyVar] -- original, possibly stale, tyvars
             -> [TyVar] -- Extra eta tyvars
             -> [CoVar] -- possibly stale covars
             -> [Type]  -- LHS patterns
             -> Type    -- RHS
             -> [Role]
             -> SrcSpan
             -> CoAxBranch
mkCoAxBranch tvs eta_tvs cvs lhs rhs roles loc
  = CoAxBranch { cab_tvs     = tvs'
               , cab_eta_tvs = eta_tvs'
               , cab_cvs     = cvs'
               , cab_lhs     = tidyTypes env lhs
               , cab_roles   = roles
               , cab_rhs     = tidyType env rhs
               , cab_loc     = loc
               , cab_incomps = placeHolderIncomps }
  where
    (env1, tvs')     = tidyVarBndrs init_tidy_env tvs
    (env2, eta_tvs') = tidyVarBndrs env1          eta_tvs
    (env,  cvs')     = tidyVarBndrs env2          cvs
    -- See Note [Tidy axioms when we build them]
    -- See also Note [CoAxBranch type variables] in GHC.Core.Coercion.Axiom

    init_occ_env = initTidyOccEnv [mkTyVarOcc "_"]
    init_tidy_env = mkEmptyTidyEnv init_occ_env
    -- See Note [Always number wildcard types in CoAxBranch]

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
                -> [TyVar] -> [TyVar] -> [CoVar]
                -> TyCon -> [Type] -> Type
                -> CoAxiom Unbranched
-- Make a single-branch CoAxiom, including making the branch itself
-- Used for both type family (Nominal) and data family (Representational)
-- axioms, hence passing in the Role
mkSingleCoAxiom role ax_name tvs eta_tvs cvs fam_tc lhs_tys rhs_ty
  = CoAxiom { co_ax_unique   = nameUnique ax_name
            , co_ax_name     = ax_name
            , co_ax_tc       = fam_tc
            , co_ax_role     = role
            , co_ax_implicit = False
            , co_ax_branches = unbranched (branch { cab_incomps = [] }) }
  where
    branch = mkCoAxBranch tvs eta_tvs cvs lhs_tys rhs_ty
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
            , co_ax_implicit = True  -- See Note [Implicit axioms] in GHC.Core.TyCon
            , co_ax_role     = Representational
            , co_ax_tc       = tycon
            , co_ax_branches = unbranched (branch { cab_incomps = [] }) }
  where
    branch = mkCoAxBranch tvs [] [] (mkTyVarTys tvs) rhs_ty
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
    get (FamIE _ rm) = lookupRM [RML_KnownTc (tyConName fam_tc)] rm

lookupFamInstEnv
    :: FamInstEnvs
    -> TyCon -> [Type]          -- What we are looking for
    -> [FamInstMatch]           -- Successful matches
-- Precondition: the tycon is saturated (or over-saturated)

lookupFamInstEnv
   = lookup_fam_inst_env WantMatches

lookupFamInstEnvConflicts
    :: FamInstEnvs
    -> FamInst          -- Putative new instance
    -> [FamInst]   -- Conflicting matches (don't look at the fim_tys field)
-- E.g. when we are about to add
--    f : type instance F [a] = a->a
-- we do (lookupFamInstConflicts f [b])
-- to find conflicting matches
--
-- Precondition: the tycon is saturated (or over-saturated)

lookupFamInstEnvConflicts envs fam_inst
  = lookup_fam_inst_env (WantConflicts fam_inst) envs fam tys
  where
    (fam, tys) = famInstSplitLHS fam_inst

--------------------------------------------------------------------------------
--                 Type family injectivity checking bits                      --
--------------------------------------------------------------------------------

{- Note [Verifying injectivity annotation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Injectivity means that the RHS of a type family uniquely determines the LHS (see
Note [Type inference for type families with injectivity]).  The user informs us about
injectivity using an injectivity annotation and it is GHC's task to verify that
this annotation is correct w.r.t. type family equations. Whenever we see a new
equation of a type family we need to make sure that adding this equation to the
already known equations of a type family does not violate the injectivity annotation
supplied by the user (see Note [Injectivity annotation]).  Of course if the type
family has no injectivity annotation then no check is required.  But if a type
family has injectivity annotation we need to make sure that the following
conditions hold:

1. For each pair of *different* equations of a type family, one of the following
   conditions holds:

   A:  RHSs are different. (Check done in GHC.Core.FamInstEnv.injectiveBranches)

   B1: OPEN TYPE FAMILIES: If the RHSs can be unified under some substitution
       then it must be possible to unify the LHSs under the same substitution.
       Example:

          type family FunnyId a = r | r -> a
          type instance FunnyId Int = Int
          type instance FunnyId a = a

       RHSs of these two equations unify under [ a |-> Int ] substitution.
       Under this substitution LHSs are equal therefore these equations don't
       violate injectivity annotation. (Check done in GHC.Core.FamInstEnv.injectiveBranches)

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
       annotation. (Check done in GHC.Tc.Validity.checkValidCoAxiom#gather_conflicts)

   A special case of B is when RHSs unify with an empty substitution ie. they
   are identical.

   If any of the above two conditions holds we conclude that the pair of
   equations does not violate injectivity annotation. But if we find a pair
   of equations where neither of the above holds we report that this pair
   violates injectivity annotation because for a given RHS we don't have a
   unique LHS. (Note that (B) actually implies (A).)

   Note that we only take into account these LHS patterns that were declared
   as injective.

2. If an RHS of a type family equation is a bare type variable then
   all LHS variables (including implicit kind variables) also have to be bare.
   In other words, this has to be a sole equation of that type family and it has
   to cover all possible patterns.  So for example this definition will be
   rejected:

      type family W1 a = r | r -> a
      type instance W1 [a] = a

   If it were accepted we could call `W1 [W1 Int]`, which would reduce to
   `W1 Int` and then by injectivity we could conclude that `[W1 Int] ~ Int`,
   which is bogus. Checked FamInst.bareTvInRHSViolated.

3. If the RHS of a type family equation is a type family application then the type
   family is rejected as not injective. This is checked by FamInst.isTFHeaded.

4. If a LHS type variable that is declared as injective is not mentioned in an
   injective position in the RHS then the type family is rejected as not
   injective.  "Injective position" means either an argument to a type
   constructor or argument to a type family on injective position.
   There are subtleties here. See Note [Coverage condition for injective type families]
   in GHC.Tc.Instance.Family.

Check (1) must be done for all family instances (transitively) imported. Other
checks (2-4) should be done just for locally written equations, as they are checks
involving just a single equation, not about interactions. Doing the other checks for
imported equations led to #17405, as the behavior of check (4) depends on
-XUndecidableInstances (see Note [Coverage condition for injective type families] in
FamInst), which may vary between modules.

See also Note [Injective type families] in GHC.Core.TyCon
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
lookupFamInstEnvInjectivityConflicts injList fam_inst_envs
                             fam_inst@(FamInst { fi_axiom = new_axiom })
  | not $ isOpenFamilyTyCon fam
  = []

  | otherwise
  -- See Note [Verifying injectivity annotation]. This function implements
  -- check (1.B1) for open type families described there.
  = map (coAxiomSingleBranch . fi_axiom) $
    filter isInjConflict $
    familyInstances fam_inst_envs fam
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
data FamInstLookupMode a where
  -- The FamInst we are trying to find conflicts against
  WantConflicts :: FamInst -> FamInstLookupMode FamInst
  WantMatches  :: FamInstLookupMode FamInstMatch

lookup_fam_inst_env'          -- The worker, local to this module
    :: forall a . FamInstLookupMode a
    -> FamInstEnv
    -> TyCon -> [Type]        -- What we are looking for
    -> [a]
lookup_fam_inst_env' lookup_mode (FamIE _ ie) fam match_tys
  | isOpenFamilyTyCon fam
  , let xs = rm_fun (lookupRM' rough_tmpl ie)   -- The common case
    -- Avoid doing any of the allocation below if there are no instances to look at.
  , not $ null xs
  = mapMaybe' check_fun xs
  | otherwise = []
  where
    rough_tmpl :: [RoughMatchLookupTc]
    rough_tmpl = RML_KnownTc (tyConName fam) : map typeToRoughMatchLookupTc match_tys

    rm_fun :: (Bag FamInst, [FamInst]) -> [FamInst]
    (rm_fun, check_fun) = case lookup_mode of
                            WantConflicts fam_inst -> (snd, unify_fun fam_inst)
                            WantMatches -> (bagToList . fst, match_fun)

    -- Function used for finding unifiers
    unify_fun orig_fam_inst item@(FamInst { fi_axiom = old_axiom, fi_tys = tpl_tys, fi_tvs = tpl_tvs })

       = assertPpr (tyCoVarsOfTypes tys `disjointVarSet` mkVarSet tpl_tvs)
                   ((ppr fam <+> ppr tys) $$
                    (ppr tpl_tvs <+> ppr tpl_tys)) $
                -- Unification will break badly if the variables overlap
                -- They shouldn't because we allocate separate uniques for them
         if compatibleBranches (coAxiomSingleBranch old_axiom) new_branch
           then Nothing
           else Just item
      -- See Note [Family instance overlap conflicts]
      where
        new_branch = coAxiomSingleBranch (famInstAxiom orig_fam_inst)
        (fam, tys) = famInstSplitLHS orig_fam_inst

    -- Function used for checking matches
    match_fun item@(FamInst { fi_tvs = tpl_tvs, fi_cvs = tpl_cvs
                            , fi_tys = tpl_tys }) =  do
      subst <- tcMatchTys tpl_tys match_tys1
      return (FamInstMatch { fim_instance = item
                             , fim_tys      = substTyVars subst tpl_tvs `chkAppend` match_tys2
                             , fim_cos      = assert (all (isJust . lookupCoVar subst) tpl_cvs) $
                                               substCoVars subst tpl_cvs
                             })
        where
          (match_tys1, match_tys2) = split_tys tpl_tys

    -- Precondition: the tycon is saturated (or over-saturated)

    -- Deal with over-saturation
    -- See Note [Over-saturated matches]
    split_tys tpl_tys
      | isTypeFamilyTyCon fam
      = pre_rough_split_tys

      | otherwise
      = let (match_tys1, match_tys2) = splitAtList tpl_tys match_tys
        in (match_tys1, match_tys2)

    (pre_match_tys1, pre_match_tys2) = splitAt (tyConArity fam) match_tys
    pre_rough_split_tys
      = (pre_match_tys1, pre_match_tys2)

lookup_fam_inst_env           -- The worker, local to this module
    :: FamInstLookupMode a
    -> FamInstEnvs
    -> TyCon -> [Type]        -- What we are looking for
    -> [a]         -- Successful matches

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
   Note [Eta reduction for data families] in GHC.Core.Coercion.Axiom.
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
                     -> Maybe Reduction
-- Attempt to do a *one-step* reduction of a type-family application
--    but *not* newtypes
-- Works on type-synonym families always; data-families only if
--     the role we seek is representational
-- It does *not* normalise the type arguments first, so this may not
--     go as far as you want. If you want normalised type arguments,
--     use topReduceTyFamApp_maybe
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
    in Just $ coercionRedn co

  | Just ax <- isClosedSynFamilyTyConWithAxiom_maybe tc
  , Just (ind, inst_tys, inst_cos) <- chooseBranch ax tys
  = let co = mkAxInstCo role ax ind inst_tys inst_cos
    in Just $ coercionRedn co

  | Just ax           <- isBuiltInSynFamTyCon_maybe tc
  , Just (coax,ts,ty) <- sfMatchFam ax tys
  , role == coaxrRole coax
  = let co = mkAxiomRuleCo coax (zipWith mkReflCo (coaxrAsmpRoles coax) ts)
    in Just $ mkReduction co ty

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
           <- findBranch (unMkBranches branches) target_tys
       ; return ( ind, inst_tys `chkAppend` extra_tys, inst_cos ) }

-- The axiom must *not* be oversaturated
findBranch :: Array BranchIndex CoAxBranch
           -> [Type]
           -> Maybe (BranchIndex, [Type], [Coercion])
    -- coercions relate requested types to returned axiom LHS at role N
findBranch branches target_tys
  = foldr go Nothing (assocs branches)
  where
    go :: (BranchIndex, CoAxBranch)
       -> Maybe (BranchIndex, [Type], [Coercion])
       -> Maybe (BranchIndex, [Type], [Coercion])
    go (index, branch) other
      = let (CoAxBranch { cab_tvs = tpl_tvs, cab_cvs = tpl_cvs
                        , cab_lhs = tpl_lhs
                        , cab_incomps = incomps }) = branch
            in_scope = mkInScopeSet (unionVarSets $
                            map (tyCoVarsOfTypes . coAxBranchLHS) incomps)
            -- See Note [Flattening type-family applications when matching instances]
            -- in GHC.Core.Unify
            flattened_target = flattenTys in_scope target_tys
        in case tcMatchTys tpl_lhs target_tys of
        Just subst -- matching worked. now, check for apartness.
          |  apartnessCheck flattened_target branch
          -> -- matching worked & we're apart from all incompatible branches.
             -- success
             assert (all (isJust . lookupCoVar subst) tpl_cvs) $
             Just (index, substTyVars subst tpl_tvs, substCoVars subst tpl_cvs)

        -- failure. keep looking
        _ -> other

-- | Do an apartness check, as described in the "Closed Type Families" paper
-- (POPL '14). This should be used when determining if an equation
-- ('CoAxBranch') of a closed type family can be used to reduce a certain target
-- type family application.
apartnessCheck :: [Type]
  -- ^ /flattened/ target arguments. Make sure they're flattened! See
  -- Note [Flattening type-family applications when matching instances]
  -- in GHC.Core.Unify.
               -> CoAxBranch -- ^ the candidate equation we wish to use
                             -- Precondition: this matches the target
               -> Bool       -- ^ True <=> equation can fire
apartnessCheck flattened_target (CoAxBranch { cab_incomps = incomps })
  = all (isSurelyApart
         . tcUnifyTysFG alwaysBindFun flattened_target
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
LiftingContexts from the liftCoSubst family of functions, defined in GHC.Core.Coercion.
A LiftingContext maps a type variable to a coercion and a coercion variable to
a pair of coercions. Let's ignore coercion variables for now. Because the
coercion a type variable maps to contains the destination type (via
coercionKind), we don't need to store that destination type separately. Thus,
a LiftingContext has what we need: a map from type variables to (Coercion,
Type) pairs.

We also benefit because we can piggyback on the liftCoSubstVarBndr function to
deal with binders. However, I had to modify that function to work with this
application. Thus, we now have liftCoSubstVarBndrUsing, which takes
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
synonyms (#13035).  Suppose S is a type synonym, and we have
   S t1 t2
If S is family-free (on its RHS) we can just normalise t1 and t2 and
reconstruct (S t1' t2').   Expanding S could not reveal any new redexes
because type families are saturated.

But if S has a type family on its RHS we expand /before/ normalising
the args t1, t2.  If we normalise t1, t2 first, we'll re-normalise them
after expansion, and that can lead to /exponential/ behaviour; see #13035.

Notice, though, that expanding first can in principle duplicate t1,t2,
which might contain redexes. I'm sure you could conjure up an exponential
case by that route too, but it hasn't happened in practice yet!
-}

topNormaliseType :: FamInstEnvs -> Type -> Type
topNormaliseType env ty
  = case topNormaliseType_maybe env ty of
      Just redn -> reductionReducedType redn
      Nothing   -> ty

topNormaliseType_maybe :: FamInstEnvs -> Type -> Maybe Reduction

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
--
-- Always operates homogeneously: the returned type has the same kind as the
-- original type, and the returned coercion is always homogeneous.
topNormaliseType_maybe env ty
  = do { ((co, mkind_co), nty) <- topNormaliseTypeX stepper combine ty
       ; let hredn = mkHetReduction (mkReduction co nty) mkind_co
       ; return $ homogeniseHetRedn Representational hredn }
  where
    stepper = unwrapNewTypeStepper' `composeSteppers` tyFamStepper

    combine (c1, mc1) (c2, mc2) = (c1 `mkTransCo` c2, mc1 `mkTransMCo` mc2)

    unwrapNewTypeStepper' :: NormaliseStepper (Coercion, MCoercionN)
    unwrapNewTypeStepper' rec_nts tc tys
      = mapStepResult (, MRefl) $ unwrapNewTypeStepper rec_nts tc tys

      -- second coercion below is the kind coercion relating the original type's kind
      -- to the normalised type's kind
    tyFamStepper :: NormaliseStepper (Coercion, MCoercionN)
    tyFamStepper rec_nts tc tys  -- Try to step a type/data family
      = case topReduceTyFamApp_maybe env tc tys of
          Just (HetReduction (Reduction co rhs) res_co)
            -> NS_Step rec_nts rhs (co, res_co)
          _ -> NS_Done

---------------
normaliseTcApp :: FamInstEnvs -> Role -> TyCon -> [Type] -> Reduction
-- See comments on normaliseType for the arguments of this function
normaliseTcApp env role tc tys
  = initNormM env role (tyCoVarsOfTypes tys) $
    normalise_tc_app tc tys

-- See Note [Normalising types] about the LiftingContext
normalise_tc_app :: TyCon -> [Type] -> NormM Reduction
normalise_tc_app tc tys
  | Just (tenv, rhs, tys') <- expandSynTyCon_maybe tc tys
  , not (isFamFreeTyCon tc)  -- Expand and try again
  = -- A synonym with type families in the RHS
    -- Expand and try again
    -- See Note [Normalisation and type synonyms]
    normalise_type (mkAppTys (substTy (mkTvSubstPrs tenv) rhs) tys')

  | isFamilyTyCon tc
  = -- A type-family application
    do { env <- getEnv
       ; role <- getRole
       ; ArgsReductions redns@(Reductions args_cos ntys) res_co <- normalise_tc_args tc tys
       ; case reduceTyFamApp_maybe env role tc ntys of
           Just redn1
             -> do { redn2 <- normalise_reduction redn1
                   ; let redn3 = mkTyConAppCo role tc args_cos `mkTransRedn` redn2
                   ; return $ assemble_result role redn3 res_co }
           _ -> -- No unique matching family instance exists;
                -- we do not do anything
                return $
                  assemble_result role (mkTyConAppRedn role tc redns) res_co }

  | otherwise
  = -- A synonym with no type families in the RHS; or data type etc
    -- Just normalise the arguments and rebuild
    do { ArgsReductions redns res_co <- normalise_tc_args tc tys
       ; role <- getRole
       ; return $
            assemble_result role (mkTyConAppRedn role tc redns) res_co }

  where
    assemble_result :: Role       -- r, ambient role in NormM monad
                    -> Reduction  -- orig_ty ~r nty, possibly heterogeneous (nty possibly of changed kind)
                    -> MCoercionN -- typeKind(orig_ty) ~N typeKind(nty)
                    -> Reduction  -- orig_ty ~r nty_casted
                                  -- where nty_casted has same kind as orig_ty
    assemble_result r redn kind_co
      = mkCoherenceRightMRedn r redn (mkSymMCo kind_co)

---------------
-- | Try to simplify a type-family application, by *one* step
-- If topReduceTyFamApp_maybe env r F tys = Just (HetReduction (Reduction co rhs) res_co)
-- then    co     :: F tys ~R# rhs
--         res_co :: typeKind(F tys) ~ typeKind(rhs)
-- Type families and data families; always Representational role
topReduceTyFamApp_maybe :: FamInstEnvs -> TyCon -> [Type]
                        -> Maybe HetReduction
topReduceTyFamApp_maybe envs fam_tc arg_tys
  | isFamilyTyCon fam_tc   -- type families and data families
  , Just redn <- reduceTyFamApp_maybe envs role fam_tc ntys
  = Just $
      mkHetReduction
        (mkTyConAppCo role fam_tc args_cos `mkTransRedn` redn)
        res_co
  | otherwise
  = Nothing
  where
    role = Representational
    ArgsReductions (Reductions args_cos ntys) res_co
      = initNormM envs role (tyCoVarsOfTypes arg_tys)
      $ normalise_tc_args fam_tc arg_tys

normalise_tc_args :: TyCon -> [Type] -> NormM ArgsReductions
normalise_tc_args tc tys
  = do { role <- getRole
       ; normalise_args (tyConKind tc) (tyConRolesX role tc) tys }

---------------
normaliseType :: FamInstEnvs
              -> Role  -- desired role of coercion
              -> Type -> Reduction
normaliseType env role ty
  = initNormM env role (tyCoVarsOfType ty) $ normalise_type ty

normalise_type :: Type -> NormM Reduction
-- Normalise the input type, by eliminating *all* type-function redexes
-- but *not* newtypes (which are visible to the programmer)
-- Returns with Refl if nothing happens
-- Does nothing to newtypes
-- The returned coercion *must* be *homogeneous*
-- See Note [Normalising types]
-- Try not to disturb type synonyms if possible

normalise_type ty
  = go ty
  where
    go :: Type -> NormM Reduction
    go (TyConApp tc tys) = normalise_tc_app tc tys
    go ty@(LitTy {})
      = do { r <- getRole
           ; return $ mkReflRedn r ty }
    go (AppTy ty1 ty2) = go_app_tys ty1 [ty2]

    go (FunTy { ft_af = vis, ft_mult = w, ft_arg = ty1, ft_res = ty2 })
      = do { arg_redn <- go ty1
           ; res_redn <- go ty2
           ; w_redn <- withRole Nominal $ go w
           ; r <- getRole
           ; return $ mkFunRedn r vis w_redn arg_redn res_redn }
    go (ForAllTy (Bndr tcvar vis) ty)
      = do { (lc', tv', k_redn) <- normalise_var_bndr tcvar
           ; redn <- withLC lc' $ normalise_type ty
           ; return $ mkForAllRedn vis tv' k_redn redn }
    go (TyVarTy tv)    = normalise_tyvar tv
    go (CastTy ty co)
      = do { redn <- go ty
           ; lc <- getLC
           ; let co' = substRightCo lc co
           ; return $ mkCastRedn2 Nominal ty co redn co'
             --       ^^^^^^^^^^^ uses castCoercionKind2
           }
    go (CoercionTy co)
      = do { lc <- getLC
           ; r <- getRole
           ; let kco = liftCoSubst Nominal lc (coercionType co)
                 co' = substRightCo lc co
           ; return $ mkProofIrrelRedn r kco co co' }

    go_app_tys :: Type   -- function
               -> [Type] -- args
               -> NormM Reduction
    -- cf. GHC.Tc.Solver.Rewrite.rewrite_app_ty_args
    go_app_tys (AppTy ty1 ty2) tys = go_app_tys ty1 (ty2 : tys)
    go_app_tys fun_ty arg_tys
      = do { fun_redn@(Reduction fun_co nfun) <- go fun_ty
           ; case tcSplitTyConApp_maybe nfun of
               Just (tc, xis) ->
                 do { redn <- go (mkTyConApp tc (xis ++ arg_tys))
                   -- rewrite_app_ty_args avoids redundantly processing the xis,
                   -- but that's a much more performance-sensitive function.
                   -- This type normalisation is not called in a loop.
                    ; return $
                        mkAppCos fun_co (map mkNomReflCo arg_tys) `mkTransRedn` redn }
               Nothing ->
                 do { ArgsReductions redns res_co
                        <- normalise_args (typeKind nfun)
                                          (repeat Nominal)
                                          arg_tys
                    ; role <- getRole
                    ; return $
                        mkCoherenceRightMRedn role
                          (mkAppRedns fun_redn redns)
                          (mkSymMCo res_co) } }

normalise_args :: Kind    -- of the function
               -> [Role]  -- roles at which to normalise args
               -> [Type]  -- args
               -> NormM ArgsReductions
-- returns ArgsReductions (Reductions cos xis) res_co,
-- where each xi is the normalised version of the corresponding type,
-- each co is orig_arg ~ xi, and res_co :: kind(f orig_args) ~ kind(f xis).
-- NB: The xis might *not* have the same kinds as the input types,
-- but the resulting application *will* be well-kinded
-- cf. GHC.Tc.Solver.Rewrite.rewrite_args_slow
normalise_args fun_ki roles args
  = do { normed_args <- zipWithM normalise1 roles args
       ; return $ simplifyArgsWorker ki_binders inner_ki fvs roles normed_args }
  where
    (ki_binders, inner_ki) = splitPiTys fun_ki
    fvs = tyCoVarsOfTypes args

    normalise1 role ty
      = withRole role $ normalise_type ty

normalise_tyvar :: TyVar -> NormM Reduction
normalise_tyvar tv
  = assert (isTyVar tv) $
    do { lc <- getLC
       ; r  <- getRole
       ; return $ case liftCoSubstTyVar lc r tv of
           Just co -> coercionRedn co
           Nothing -> mkReflRedn r (mkTyVarTy tv) }

normalise_reduction :: Reduction -> NormM Reduction
normalise_reduction (Reduction co ty)
  = do { redn' <- normalise_type ty
       ; return $ co `mkTransRedn` redn' }

normalise_var_bndr :: TyCoVar -> NormM (LiftingContext, TyCoVar, Reduction)
normalise_var_bndr tcvar
  -- works for both tvar and covar
  = do { lc1 <- getLC
       ; env <- getEnv
       ; let callback lc ki = runNormM (normalise_type ki) env lc Nominal
       ; return $ liftCoSubstVarBndrUsing reductionCoercion callback lc1 tcvar }

-- | a monad for the normalisation functions, reading 'FamInstEnvs',
-- a 'LiftingContext', and a 'Role'.
newtype NormM a = NormM { runNormM ::
                            FamInstEnvs -> LiftingContext -> Role -> a }
    deriving (Functor)

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

instance Applicative NormM where
  pure x = NormM $ \ _ _ _ -> x
  (<*>)  = ap
