-- (c) The University of Glasgow 2012

{-# LANGUAGE CPP, DataKinds, DeriveDataTypeable, GADTs, KindSignatures,
             ScopedTypeVariables, StandaloneDeriving, RoleAnnotations #-}

-- | Module for coercion axioms, used to represent type family instances
-- and newtypes

module CoAxiom (
       BranchFlag, Branched, Unbranched, BranchIndex, Branches,
       manyBranches, unbranched,
       fromBranches, numBranches,
       mapAccumBranches,

       CoAxiom(..), CoAxBranch(..),

       toBranchedAxiom, toUnbranchedAxiom,
       coAxiomName, coAxiomArity, coAxiomBranches,
       coAxiomTyCon, isImplicitCoAxiom, coAxiomNumPats,
       coAxiomNthBranch, coAxiomSingleBranch_maybe, coAxiomRole,
       coAxiomSingleBranch, coAxBranchTyVars, coAxBranchCoVars,
       coAxBranchRoles,
       coAxBranchLHS, coAxBranchRHS, coAxBranchSpan, coAxBranchIncomps,
       placeHolderIncomps,

       Role(..), fsFromRole,

       CoAxiomRule(..), TypeEqn,
       BuiltInSynFamily(..), trivialBuiltInFamily
       ) where

import GhcPrelude

import {-# SOURCE #-} TyCoRep ( Type, pprType )
import {-# SOURCE #-} TyCon ( TyCon )
import Outputable
import FastString
import Name
import Unique
import Var
import Util
import Binary
import Pair
import BasicTypes
import Data.Typeable ( Typeable )
import SrcLoc
import qualified Data.Data as Data
import Data.Array
import Data.List ( mapAccumL )

#include "HsVersions.h"

{-
Note [Coercion axiom branches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In order to allow closed type families, an axiom needs to contain an
ordered list of alternatives, called branches. The kind of the coercion built
from an axiom is determined by which index is used when building the coercion
from the axiom.

For example, consider the axiom derived from the following declaration:

type family F a where
  F [Int] = Bool
  F [a]   = Double
  F (a b) = Char

This will give rise to this axiom:

axF :: {                                         F [Int] ~ Bool
       ; forall (a :: *).                        F [a]   ~ Double
       ; forall (k :: *) (a :: k -> *) (b :: k). F (a b) ~ Char
       }

The axiom is used with the AxiomInstCo constructor of Coercion. If we wish
to have a coercion showing that F (Maybe Int) ~ Char, it will look like

axF[2] <*> <Maybe> <Int> :: F (Maybe Int) ~ Char
-- or, written using concrete-ish syntax --
AxiomInstCo axF 2 [Refl *, Refl Maybe, Refl Int]

Note that the index is 0-based.

For type-checking, it is also necessary to check that no previous pattern
can unify with the supplied arguments. After all, it is possible that some
of the type arguments are lambda-bound type variables whose instantiation may
cause an earlier match among the branches. We wish to prohibit this behavior,
so the type checker rules out the choice of a branch where a previous branch
can unify. See also [Apartness] in FamInstEnv.hs.

For example, the following is malformed, where 'a' is a lambda-bound type
variable:

axF[2] <*> <a> <Bool> :: F (a Bool) ~ Char

Why? Because a might be instantiated with [], meaning that branch 1 should
apply, not branch 2. This is a vital consistency check; without it, we could
derive Int ~ Bool, and that is a Bad Thing.

Note [Branched axioms]
~~~~~~~~~~~~~~~~~~~~~~
Although a CoAxiom has the capacity to store many branches, in certain cases,
we want only one. These cases are in data/newtype family instances, newtype
coercions, and type family instances.
Furthermore, these unbranched axioms are used in a
variety of places throughout GHC, and it would difficult to generalize all of
that code to deal with branched axioms, especially when the code can be sure
of the fact that an axiom is indeed a singleton. At the same time, it seems
dangerous to assume singlehood in various places through GHC.

The solution to this is to label a CoAxiom with a phantom type variable
declaring whether it is known to be a singleton or not. The branches
are stored using a special datatype, declared below, that ensures that the
type variable is accurate.

************************************************************************
*                                                                      *
                    Branches
*                                                                      *
************************************************************************
-}

type BranchIndex = Int  -- The index of the branch in the list of branches
                        -- Counting from zero

-- promoted data type
data BranchFlag = Branched | Unbranched
type Branched = 'Branched
type Unbranched = 'Unbranched
-- By using type synonyms for the promoted constructors, we avoid needing
-- DataKinds and the promotion quote in client modules. This also means that
-- we don't need to export the term-level constructors, which should never be used.

newtype Branches (br :: BranchFlag)
  = MkBranches { unMkBranches :: Array BranchIndex CoAxBranch }
type role Branches nominal

manyBranches :: [CoAxBranch] -> Branches Branched
manyBranches brs = ASSERT( snd bnds >= fst bnds )
                   MkBranches (listArray bnds brs)
  where
    bnds = (0, length brs - 1)

unbranched :: CoAxBranch -> Branches Unbranched
unbranched br = MkBranches (listArray (0, 0) [br])

toBranched :: Branches br -> Branches Branched
toBranched = MkBranches . unMkBranches

toUnbranched :: Branches br -> Branches Unbranched
toUnbranched (MkBranches arr) = ASSERT( bounds arr == (0,0) )
                                MkBranches arr

fromBranches :: Branches br -> [CoAxBranch]
fromBranches = elems . unMkBranches

branchesNth :: Branches br -> BranchIndex -> CoAxBranch
branchesNth (MkBranches arr) n = arr ! n

numBranches :: Branches br -> Int
numBranches (MkBranches arr) = snd (bounds arr) + 1

-- | The @[CoAxBranch]@ passed into the mapping function is a list of
-- all previous branches, reversed
mapAccumBranches :: ([CoAxBranch] -> CoAxBranch -> CoAxBranch)
                  -> Branches br -> Branches br
mapAccumBranches f (MkBranches arr)
  = MkBranches (listArray (bounds arr) (snd $ mapAccumL go [] (elems arr)))
  where
    go :: [CoAxBranch] -> CoAxBranch -> ([CoAxBranch], CoAxBranch)
    go prev_branches cur_branch = ( cur_branch : prev_branches
                                  , f prev_branches cur_branch )


{-
************************************************************************
*                                                                      *
                    Coercion axioms
*                                                                      *
************************************************************************

Note [Storing compatibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During axiom application, we need to be aware of which branches are compatible
with which others. The full explanation is in Note [Compatibility] in
FamInstEnv. (The code is placed there to avoid a dependency from CoAxiom on
the unification algorithm.) Although we could theoretically compute
compatibility on the fly, this is silly, so we store it in a CoAxiom.

Specifically, each branch refers to all other branches with which it is
incompatible. This list might well be empty, and it will always be for the
first branch of any axiom.

CoAxBranches that do not (yet) belong to a CoAxiom should have a panic thunk
stored in cab_incomps. The incompatibilities are properly a property of the
axiom as a whole, and they are computed only when the final axiom is built.

During serialization, the list is converted into a list of the indices
of the branches.
-}

-- | A 'CoAxiom' is a \"coercion constructor\", i.e. a named equality axiom.

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
data CoAxiom br
  = CoAxiom                   -- Type equality axiom.
    { co_ax_unique   :: Unique        -- Unique identifier
    , co_ax_name     :: Name          -- Name for pretty-printing
    , co_ax_role     :: Role          -- Role of the axiom's equality
    , co_ax_tc       :: TyCon         -- The head of the LHS patterns
                                      -- e.g.  the newtype or family tycon
    , co_ax_branches :: Branches br   -- The branches that form this axiom
    , co_ax_implicit :: Bool          -- True <=> the axiom is "implicit"
                                      -- See Note [Implicit axioms]
         -- INVARIANT: co_ax_implicit == True implies length co_ax_branches == 1.
    }

data CoAxBranch
  = CoAxBranch
    { cab_loc      :: SrcSpan       -- Location of the defining equation
                                    -- See Note [CoAxiom locations]
    , cab_tvs      :: [TyVar]       -- Bound type variables; not necessarily fresh
                                    -- See Note [CoAxBranch type variables]
    , cab_cvs      :: [CoVar]       -- Bound coercion variables
                                    -- Always empty, for now.
                                    -- See Note [Constraints in patterns]
                                    -- in TcTyClsDecls
    , cab_roles    :: [Role]        -- See Note [CoAxBranch roles]
    , cab_lhs      :: [Type]        -- Type patterns to match against
                                    -- See Note [CoAxiom saturation]
    , cab_rhs      :: Type          -- Right-hand side of the equality
    , cab_incomps  :: [CoAxBranch]  -- The previous incompatible branches
                                    -- See Note [Storing compatibility]
    }
  deriving Data.Data

toBranchedAxiom :: CoAxiom br -> CoAxiom Branched
toBranchedAxiom (CoAxiom unique name role tc branches implicit)
  = CoAxiom unique name role tc (toBranched branches) implicit

toUnbranchedAxiom :: CoAxiom br -> CoAxiom Unbranched
toUnbranchedAxiom (CoAxiom unique name role tc branches implicit)
  = CoAxiom unique name role tc (toUnbranched branches) implicit

coAxiomNumPats :: CoAxiom br -> Int
coAxiomNumPats = length . coAxBranchLHS . (flip coAxiomNthBranch 0)

coAxiomNthBranch :: CoAxiom br -> BranchIndex -> CoAxBranch
coAxiomNthBranch (CoAxiom { co_ax_branches = bs }) index
  = branchesNth bs index

coAxiomArity :: CoAxiom br -> BranchIndex -> Arity
coAxiomArity ax index
  = length tvs + length cvs
  where
    CoAxBranch { cab_tvs = tvs, cab_cvs = cvs } = coAxiomNthBranch ax index

coAxiomName :: CoAxiom br -> Name
coAxiomName = co_ax_name

coAxiomRole :: CoAxiom br -> Role
coAxiomRole = co_ax_role

coAxiomBranches :: CoAxiom br -> Branches br
coAxiomBranches = co_ax_branches

coAxiomSingleBranch_maybe :: CoAxiom br -> Maybe CoAxBranch
coAxiomSingleBranch_maybe (CoAxiom { co_ax_branches = MkBranches arr })
  | snd (bounds arr) == 0
  = Just $ arr ! 0
  | otherwise
  = Nothing

coAxiomSingleBranch :: CoAxiom Unbranched -> CoAxBranch
coAxiomSingleBranch (CoAxiom { co_ax_branches = MkBranches arr })
  = arr ! 0

coAxiomTyCon :: CoAxiom br -> TyCon
coAxiomTyCon = co_ax_tc

coAxBranchTyVars :: CoAxBranch -> [TyVar]
coAxBranchTyVars = cab_tvs

coAxBranchCoVars :: CoAxBranch -> [CoVar]
coAxBranchCoVars = cab_cvs

coAxBranchLHS :: CoAxBranch -> [Type]
coAxBranchLHS = cab_lhs

coAxBranchRHS :: CoAxBranch -> Type
coAxBranchRHS = cab_rhs

coAxBranchRoles :: CoAxBranch -> [Role]
coAxBranchRoles = cab_roles

coAxBranchSpan :: CoAxBranch -> SrcSpan
coAxBranchSpan = cab_loc

isImplicitCoAxiom :: CoAxiom br -> Bool
isImplicitCoAxiom = co_ax_implicit

coAxBranchIncomps :: CoAxBranch -> [CoAxBranch]
coAxBranchIncomps = cab_incomps

-- See Note [Compatibility checking] in FamInstEnv
placeHolderIncomps :: [CoAxBranch]
placeHolderIncomps = panic "placeHolderIncomps"

{- Note [CoAxiom saturation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* When co

Note [CoAxBranch type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the case of a CoAxBranch of an associated type-family instance,
we use the *same* type variables (where possible) as the
enclosing class or instance.  Consider
   class C a b where
     type F x b
     type F [y] b = ...     -- Second param must be b

   instance C Int [z] where
     type F Int [z] = ...   -- Second param must be [z]

In the CoAxBranch in the instance decl (F Int [z]) we use the
same 'z', so that it's easy to check that that type is the same
as that in the instance header.

Similarly in the CoAxBranch for the default decl for F in the
class decl, we use the same 'b' to make the same check easy.

So, unlike FamInsts, there is no expectation that the cab_tvs
are fresh wrt each other, or any other CoAxBranch.

Note [CoAxBranch roles]
~~~~~~~~~~~~~~~~~~~~~~~
Consider this code:

  newtype Age = MkAge Int
  newtype Wrap a = MkWrap a

  convert :: Wrap Age -> Int
  convert (MkWrap (MkAge i)) = i

We want this to compile to:

  NTCo:Wrap :: forall a. Wrap a ~R a
  NTCo:Age  :: Age ~R Int
  convert = \x -> x |> (NTCo:Wrap[0] NTCo:Age[0])

But, note that NTCo:Age is at role R. Thus, we need to be able to pass
coercions at role R into axioms. However, we don't *always* want to be able to
do this, as it would be disastrous with type families. The solution is to
annotate the arguments to the axiom with roles, much like we annotate tycon
tyvars. Where do these roles get set? Newtype axioms inherit their roles from
the newtype tycon; family axioms are all at role N.

Note [CoAxiom locations]
~~~~~~~~~~~~~~~~~~~~~~~~
The source location of a CoAxiom is stored in two places in the
datatype tree.
  * The first is in the location info buried in the Name of the
    CoAxiom. This span includes all of the branches of a branched
    CoAxiom.
  * The second is in the cab_loc fields of the CoAxBranches.

In the case of a single branch, we can extract the source location of
the branch from the name of the CoAxiom. In other cases, we need an
explicit SrcSpan to correctly store the location of the equation
giving rise to the FamInstBranch.

Note [Implicit axioms]
~~~~~~~~~~~~~~~~~~~~~~
See also Note [Implicit TyThings] in HscTypes
* A CoAxiom arising from data/type family instances is not "implicit".
  That is, it has its own IfaceAxiom declaration in an interface file

* The CoAxiom arising from a newtype declaration *is* "implicit".
  That is, it does not have its own IfaceAxiom declaration in an
  interface file; instead the CoAxiom is generated by type-checking
  the newtype declaration
-}

instance Eq (CoAxiom br) where
    a == b = getUnique a == getUnique b
    a /= b = getUnique a /= getUnique b

instance Uniquable (CoAxiom br) where
    getUnique = co_ax_unique

instance Outputable (CoAxiom br) where
    ppr = ppr . getName

instance NamedThing (CoAxiom br) where
    getName = co_ax_name

instance Typeable br => Data.Data (CoAxiom br) where
    -- don't traverse?
    toConstr _   = abstractConstr "CoAxiom"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "CoAxiom"

instance Outputable CoAxBranch where
  ppr (CoAxBranch { cab_loc = loc
                  , cab_lhs = lhs
                  , cab_rhs = rhs }) =
    text "CoAxBranch" <+> parens (ppr loc) <> colon
      <+> brackets (fsep (punctuate comma (map pprType lhs)))
      <+> text "=>" <+> pprType rhs

{-
************************************************************************
*                                                                      *
                    Roles
*                                                                      *
************************************************************************

Roles are defined here to avoid circular dependencies.
-}

-- See Note [Roles] in Coercion
-- defined here to avoid cyclic dependency with Coercion
--
-- Order of constructors matters: the Ord instance coincides with the *super*typing
-- relation on roles.
data Role = Nominal | Representational | Phantom
  deriving (Eq, Ord, Data.Data)

-- These names are slurped into the parser code. Changing these strings
-- will change the **surface syntax** that GHC accepts! If you want to
-- change only the pretty-printing, do some replumbing. See
-- mkRoleAnnotDecl in RdrHsSyn
fsFromRole :: Role -> FastString
fsFromRole Nominal          = fsLit "nominal"
fsFromRole Representational = fsLit "representational"
fsFromRole Phantom          = fsLit "phantom"

instance Outputable Role where
  ppr = ftext . fsFromRole

instance Binary Role where
  put_ bh Nominal          = putByte bh 1
  put_ bh Representational = putByte bh 2
  put_ bh Phantom          = putByte bh 3

  get bh = do tag <- getByte bh
              case tag of 1 -> return Nominal
                          2 -> return Representational
                          3 -> return Phantom
                          _ -> panic ("get Role " ++ show tag)

{-
************************************************************************
*                                                                      *
                    CoAxiomRule
              Rules for building Evidence
*                                                                      *
************************************************************************

Conditional axioms.  The general idea is that a `CoAxiomRule` looks like this:

    forall as. (r1 ~ r2, s1 ~ s2) => t1 ~ t2

My intention is to reuse these for both (~) and (~#).
The short-term plan is to use this datatype to represent the type-nat axioms.
In the longer run, it may be good to unify this and `CoAxiom`,
as `CoAxiom` is the special case when there are no assumptions.
-}

-- | A more explicit representation for `t1 ~ t2`.
type TypeEqn = Pair Type

-- | For now, we work only with nominal equality.
data CoAxiomRule = CoAxiomRule
  { coaxrName      :: FastString
  , coaxrAsmpRoles :: [Role]    -- roles of parameter equations
  , coaxrRole      :: Role      -- role of resulting equation
  , coaxrProves    :: [TypeEqn] -> Maybe TypeEqn
        -- ^ coaxrProves returns @Nothing@ when it doesn't like
        -- the supplied arguments.  When this happens in a coercion
        -- that means that the coercion is ill-formed, and Core Lint
        -- checks for that.
  }

instance Data.Data CoAxiomRule where
  -- don't traverse?
  toConstr _   = abstractConstr "CoAxiomRule"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "CoAxiomRule"

instance Uniquable CoAxiomRule where
  getUnique = getUnique . coaxrName

instance Eq CoAxiomRule where
  x == y = coaxrName x == coaxrName y

instance Ord CoAxiomRule where
  compare x y = compare (coaxrName x) (coaxrName y)

instance Outputable CoAxiomRule where
  ppr = ppr . coaxrName


-- Type checking of built-in families
data BuiltInSynFamily = BuiltInSynFamily
  { sfMatchFam      :: [Type] -> Maybe (CoAxiomRule, [Type], Type)
  , sfInteractTop   :: [Type] -> Type -> [TypeEqn]
  , sfInteractInert :: [Type] -> Type ->
                       [Type] -> Type -> [TypeEqn]
  }

-- Provides default implementations that do nothing.
trivialBuiltInFamily :: BuiltInSynFamily
trivialBuiltInFamily = BuiltInSynFamily
  { sfMatchFam      = \_ -> Nothing
  , sfInteractTop   = \_ _ -> []
  , sfInteractInert = \_ _ _ _ -> []
  }
