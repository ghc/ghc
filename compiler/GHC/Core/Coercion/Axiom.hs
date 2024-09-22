{-# OPTIONS_GHC -Wno-orphans     #-} -- Outputable
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- (c) The University of Glasgow 2012

-- | Module for coercion axioms, used to represent type family instances
-- and newtypes

module GHC.Core.Coercion.Axiom (
       BranchFlag, Branched, Unbranched, BranchIndex, Branches(..),
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

       CoAxiomRule(..), BuiltInFamRewrite(..), BuiltInFamInjectivity(..), TypeEqn,
       coAxiomRuleArgRoles, coAxiomRuleRole,
       coAxiomRuleBranch_maybe, isNewtypeAxiomRule_maybe,
       BuiltInSynFamily(..), trivialBuiltInFamily
       ) where

import GHC.Prelude

import Language.Haskell.Syntax.Basic (Role(..))

import {-# SOURCE #-} GHC.Core.TyCo.Rep ( Type )
import {-# SOURCE #-} GHC.Core.TyCo.Ppr ( pprType, pprTyVar )
import {-# SOURCE #-} GHC.Core.TyCon    ( TyCon, isNewTyCon )
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Types.Name
import GHC.Types.Unique
import GHC.Types.Var
import GHC.Utils.Misc
import GHC.Utils.Binary
import GHC.Utils.Panic
import GHC.Data.Pair
import GHC.Types.Basic
import Data.Typeable ( Typeable )
import GHC.Types.SrcLoc
import qualified Data.Data as Data
import Data.Array
import Data.List ( mapAccumL )

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

The axiom is used with the AxiomCo constructor of Coercion. If we wish
to have a coercion showing that F (Maybe Int) ~ Char, it will look like

axF[2] <*> <Maybe> <Int> :: F (Maybe Int) ~ Char
-- or, written using concrete-ish syntax --
AxiomRuleCo axF 2 [Refl *, Refl Maybe, Refl Int]

Note that the index is 0-based.

For type-checking, it is also necessary to check that no previous pattern
can unify with the supplied arguments. After all, it is possible that some
of the type arguments are lambda-bound type variables whose instantiation may
cause an earlier match among the branches. We wish to prohibit this behavior,
so the type checker rules out the choice of a branch where a previous branch
can unify. See also [Apartness] in GHC.Core.FamInstEnv.

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

{- Note [BranchIndex]
~~~~~~~~~~~~~~~~~~~~
A CoAxiom has 1 or more branches. Each branch has contains a list
of the free type variables in that branch, the LHS type patterns,
and the RHS type for that branch. When we apply an axiom to a list
of coercions, we must choose which branch of the axiom we wish to
use, as the different branches may have different numbers of free
type variables. (The number of type patterns is always the same
among branches, but that doesn't quite concern us here.)
-}


type BranchIndex = Int         -- Counting from zero
      -- The index of the branch in the list of branches
      -- See Note [BranchIndex]

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
manyBranches brs = assert (snd bnds >= fst bnds )
                   MkBranches (listArray bnds brs)
  where
    bnds = (0, length brs - 1)

unbranched :: CoAxBranch -> Branches Unbranched
unbranched br = MkBranches (listArray (0, 0) [br])

toBranched :: Branches br -> Branches Branched
toBranched = MkBranches . unMkBranches

toUnbranched :: Branches br -> Branches Unbranched
toUnbranched (MkBranches arr) = assert (bounds arr == (0,0) )
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
GHc.Core.FamInstEnv. (The code is placed there to avoid a dependency from
GHC.Core.Coercion.Axiom on the unification algorithm.) Although we could
theoretically compute compatibility on the fly, this is silly, so we store it
in a CoAxiom.

Specifically, each branch refers to all other branches with which it is
incompatible. This list might well be empty, and it will always be for the
first branch of any axiom.

CoAxBranches that do not (yet) belong to a CoAxiom should have a panic thunk
stored in cab_incomps. The incompatibilities are properly a property of the
axiom as a whole, and they are computed only when the final axiom is built.

During serialization, the list is converted into a list of the indices
of the branches.

Note [CoAxioms are homogeneous]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All axioms must be *homogeneous*, meaning that the kind of the LHS must
match the kind of the RHS. In practice, this means:

  Given a CoAxiom { co_ax_tc = ax_tc },
  for every branch CoAxBranch { cab_lhs = lhs, cab_rhs = rhs }:
    typeKind (mkTyConApp ax_tc lhs) `eqType` typeKind rhs

This is checked in FamInstEnv.mkCoAxBranch.
-}

-- | A 'CoAxiom' is a \"coercion constructor\", i.e. a named equality axiom.

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in GHC.Core.Lint
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

-- | A branch of a coercion axiom, which provides the evidence for
-- unwrapping a newtype or a type-family reduction step using a single equation.
data CoAxBranch
  = CoAxBranch
    { cab_loc      :: SrcSpan
        -- ^ Location of the defining equation
        -- See Note [CoAxiom locations]
    , cab_tvs      :: [TyVar]
       -- ^ Bound type variables; not necessarily fresh
       -- See Note [CoAxBranch type variables]
    , cab_eta_tvs  :: [TyVar]
       -- ^ Eta-reduced tyvars
       -- cab_tvs and cab_lhs may be eta-reduced; see
       -- Note [Eta reduction for data families]
    , cab_cvs      :: [CoVar]
      -- ^ Bound coercion variables
       -- Always empty, for now.
       -- See Note [Constraints in patterns]
       -- in GHC.Tc.TyCl
    , cab_roles    :: [Role]
       -- ^ See Note [CoAxBranch roles]
    , cab_lhs      :: [Type]
       -- ^ Type patterns to match against
    , cab_rhs      :: Type
       -- ^ Right-hand side of the equality
       -- See Note [CoAxioms are homogeneous]
    , cab_incomps  :: [CoAxBranch]
       -- ^ The previous incompatible branches
       -- See Note [Storing compatibility]
    }
  deriving Data.Data

toBranchedAxiom :: CoAxiom br -> CoAxiom Branched
toBranchedAxiom ax@(CoAxiom { co_ax_branches = branches })
  = ax { co_ax_branches = toBranched branches }

toUnbranchedAxiom :: CoAxiom br -> CoAxiom Unbranched
toUnbranchedAxiom ax@(CoAxiom { co_ax_branches = branches })
  = ax { co_ax_branches = toUnbranched branches }

coAxiomNumPats :: CoAxiom br -> Int
coAxiomNumPats = length . coAxBranchLHS . (flip coAxiomNthBranch 0)

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

coAxiomNthBranch :: CoAxiom br -> BranchIndex -> CoAxBranch
coAxiomNthBranch (CoAxiom { co_ax_branches = bs }) index
  = branchesNth bs index

coAxiomSingleBranch :: CoAxiom Unbranched -> CoAxBranch
coAxiomSingleBranch (CoAxiom { co_ax_branches = MkBranches arr })
  = arr ! 0

coAxiomSingleBranch_maybe :: CoAxiom br -> Maybe CoAxBranch
coAxiomSingleBranch_maybe (CoAxiom { co_ax_branches = MkBranches arr })
  | snd (bounds arr) == 0
  = Just $ arr ! 0
  | otherwise
  = Nothing

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

-- See Note [Compatibility] in GHC.Core.FamInstEnv
placeHolderIncomps :: [CoAxBranch]
placeHolderIncomps = panic "placeHolderIncomps"

{-
Note [CoAxBranch type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the case of a CoAxBranch of an associated type-family instance,
we use the *same* type variables in cab_tvs (where possible) as the
enclosing class or instance.  Consider

  instance C Int [z] where
     type F Int [z] = ...   -- Second param must be [z]

In the CoAxBranch in the instance decl (F Int [z]) we use the
same 'z', so that it's easy to check that that type is the same
as that in the instance header.

However, I believe that the cab_tvs of any CoAxBranch are distinct
from the cab_tvs of other CoAxBranches in the same CoAxiom.  This is
important when checking for compatiblity and apartness; e.g. see
GHC.Core.FamInstEnv.compatibleBranches.  (The story seems a bit wobbly
here, but it seems to work.)

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
See also Note [Implicit TyThings] in GHC.Types.TyThing
* A CoAxiom arising from data/type family instances is not "implicit".
  That is, it has its own IfaceAxiom declaration in an interface file

* The CoAxiom arising from a newtype declaration *is* "implicit".
  That is, it does not have its own IfaceAxiom declaration in an
  interface file; instead the CoAxiom is generated by type-checking
  the newtype declaration

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

----- Bottom line ------

For a CoAxBranch for a data family instance with representation
TyCon rep_tc:

  - cab_tvs (of its CoAxiom) may be shorter
    than tyConTyVars of rep_tc.

  - cab_lhs may be shorter than tyConArity of the family tycon
       i.e. LHS is unsaturated

  - cab_rhs will be (rep_tc cab_tvs)
       i.e. RHS is un-saturated

  - This eta reduction happens for data instances as well
    as newtype instances. Here we want to eta-reduce the data family axiom.

  - This eta-reduction is done in GHC.Tc.TyCl.Instance.tcDataFamInstDecl.

But for a /type/ family
  - cab_lhs has the exact arity of the family tycon

There are certain situations (e.g., pretty-printing) where it is necessary to
deal with eta-expanded data family instances. For these situations, the
cab_eta_tvs field records the stuff that has been eta-reduced away.
So if we have
    axiom forall a b. F [a->b] = D b a
and cab_eta_tvs is [p,q], then the original user-written definition
looked like
    axiom forall a b p q. F [a->b] p q = D b a p q
(See #9692, #14179, and #15845 for examples of what can go wrong if
we don't eta-expand when showing things to the user.)

See also:

* Note [Newtype eta] in GHC.Core.TyCon.  This is notionally separate
  and deals with the axiom connecting a newtype with its representation
  type; but it too is eta-reduced.
* Note [Implementing eta reduction for data families] in "GHC.Tc.TyCl.Instance". This
  describes the implementation details of this eta reduction happen.
* Note [RoughMap and rm_empty] for how this complicates the RoughMap implementation slightly.
-}

{- *********************************************************************
*                                                                      *
              Instances, especially pretty-printing
*                                                                      *
********************************************************************* -}

instance Eq (CoAxiom br) where
    a == b = getUnique a == getUnique b
    a /= b = getUnique a /= getUnique b

instance Uniquable (CoAxiom br) where
    getUnique = co_ax_unique

instance NamedThing (CoAxiom br) where
    getName = co_ax_name

instance Typeable br => Data.Data (CoAxiom br) where
    -- don't traverse?
    toConstr _   = abstractConstr "CoAxiom"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "CoAxiom"

instance Outputable (CoAxiom br) where
  -- You may want GHC.Core.Coercion.pprCoAxiom instead
  ppr = ppr . getName

instance Outputable CoAxBranch where
  -- This instance doesn't know the name of the type family
  -- If possible, use GHC.Core.Coercion.pprCoAxBranch instead
  ppr (CoAxBranch { cab_tvs = tvs, cab_cvs = cvs
                  , cab_lhs = lhs_tys, cab_rhs = rhs, cab_incomps = incomps })
    = text "CoAxBranch" <+> braces payload
    where
      payload = hang (text "forall" <+> pprWithCommas pprTyVar (tvs ++ cvs) <> dot)
                   2 (vcat [ text "<tycon>" <+> sep (map pprType lhs_tys)
                           , nest 2 (text "=" <+> ppr rhs)
                           , ppUnless (null incomps) $
                             text "incomps:" <+> vcat (map ppr incomps) ])

{-
************************************************************************
*                                                                      *
                    Roles
*                                                                      *
************************************************************************

Roles are defined here to avoid circular dependencies.
-}

-- These names are slurped into the parser code. Changing these strings
-- will change the **surface syntax** that GHC accepts! If you want to
-- change only the pretty-printing, do some replumbing. See
-- mkRoleAnnotDecl in GHC.Parser.PostProcess
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

Note [CoAxiomRule]
~~~~~~~~~~~~~~~~~~
A CoAxiomRule is a built-in axiom, one that we assume to be true:
CoAxiomRules come in four flavours:

* BuiltInFamRew: provides evidence for, say
      (ax1)    3+4 ----> 7
      (ax2)    s+0 ----> s
  The evidence looks like
      AxiomCo ax1 [3,4] :: 3+4 ~ 7
      AxiomCo ax2 [s]   :: s+0 ~ s
  The arguments in the AxiomCo are the /instantiating types/, or
  more generally coercions (see Note [Coercion axioms applied to coercions]
  in GHC.Core.TyCo.Rep).

* BuiltInFamInj: provides evidence for the injectivity of type families
  For example
      (ax3)   g1: a+b ~ 0        --->  a~0
      (ax4)   g2: a+b ~ 0        --->  b~0
      (ax5)   g3: a+b1 ~ a~b2    --->  b1~b2
  The argument to the AxiomCo is the full coercion (always just one).
  So then:
      AxiomCo ax3 [g1] :: a ~ 0
      AxiomCo ax4 [g2] :: b ~ 0
      AxiomCo ax5 [g3] :: b1 ~ b2

* BranchedAxiom: used for closed type families
      type family F a where
        F Int  = Bool
        F Bool = Char
        F a    = a -> Int
  We get one (CoAxiom Branched) for the entire family; when used in an
  AxiomCo we pair it with the BranchIndex to say which branch to pick.

* UnbranchedAxiom: used for several purposes;
    - Newtypes
    - Data family instances
    - Open type family instances

Note [Avoiding allocating lots of CoAxiomRules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CoAxiomRule is a sum type of four alternatives, which is very nice. But
there is a danger of allocating lots of (BuiltInFamRew bif) objects, every
time we (say) need a type-family rewrite.

To avoid this allocation, we cache the appropraite CoAxiomRule inside each
   BuiltInFamRewrite, BuiltInFamInjectivity
making a little circular data structure.  See the `bifrw_axr` field of
BuiltInFamRewrite, and similarly the others.

It's simple to do this, and saves a percent or two of allocation in programs
that do a lot of type-family work.
-}

-- | CoAxiomRule describes a built-in axiom, one that we assume to be true
-- See Note [CoAxiomRule]
data CoAxiomRule
  = BuiltInFamRew  BuiltInFamRewrite                   -- Built-in type-family rewrites
                                                       --    e.g.  3+5 ~ 7

  | BuiltInFamInj  BuiltInFamInjectivity               -- Built-in type-family deductions
                                                       --    e.g.  a+b~0 ==>  a~0
                                                       -- Always unary

  | BranchedAxiom      (CoAxiom Branched) BranchIndex  -- Closed type family

  | UnbranchedAxiom    (CoAxiom Unbranched)            -- Open type family instance,
                                                       --    data family instances
                                                       --    and newtypes

instance Eq CoAxiomRule where
  (BuiltInFamRew  bif1)  == (BuiltInFamRew  bif2)  = bifrw_name  bif1 == bifrw_name bif2
  (BuiltInFamInj bif1)   == (BuiltInFamInj bif2)   = bifinj_name bif1 == bifinj_name bif2
  (UnbranchedAxiom ax1)  == (UnbranchedAxiom ax2)  = getUnique ax1 == getUnique ax2
  (BranchedAxiom ax1 i1) == (BranchedAxiom ax2 i2) = getUnique ax1 == getUnique ax2 && i1 == i2
  _ == _ = False

coAxiomRuleRole :: CoAxiomRule -> Role
coAxiomRuleRole (BuiltInFamRew  {})  = Nominal
coAxiomRuleRole (BuiltInFamInj {})   = Nominal
coAxiomRuleRole (UnbranchedAxiom ax) = coAxiomRole ax
coAxiomRuleRole (BranchedAxiom ax _) = coAxiomRole ax

coAxiomRuleArgRoles :: CoAxiomRule -> [Role]
coAxiomRuleArgRoles (BuiltInFamRew  bif) = replicate (bifrw_arity bif) Nominal
coAxiomRuleArgRoles (BuiltInFamInj {})   = [Nominal]
coAxiomRuleArgRoles (UnbranchedAxiom ax) = coAxBranchRoles (coAxiomSingleBranch ax)
coAxiomRuleArgRoles (BranchedAxiom ax i) = coAxBranchRoles (coAxiomNthBranch ax i)

coAxiomRuleBranch_maybe :: CoAxiomRule -> Maybe (TyCon, Role, CoAxBranch)
coAxiomRuleBranch_maybe (UnbranchedAxiom ax) = Just (co_ax_tc ax, co_ax_role ax, coAxiomSingleBranch ax)
coAxiomRuleBranch_maybe (BranchedAxiom ax i) = Just (co_ax_tc ax, co_ax_role ax, coAxiomNthBranch ax i)
coAxiomRuleBranch_maybe _                    = Nothing

isNewtypeAxiomRule_maybe :: CoAxiomRule -> Maybe (TyCon, CoAxBranch)
isNewtypeAxiomRule_maybe (UnbranchedAxiom ax)
  | let tc = coAxiomTyCon ax, isNewTyCon tc = Just (tc, coAxiomSingleBranch ax)
isNewtypeAxiomRule_maybe _                  = Nothing

instance Data.Data CoAxiomRule where
  -- don't traverse?
  toConstr _   = abstractConstr "CoAxiomRule"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "CoAxiomRule"

instance Outputable CoAxiomRule where
  ppr (BuiltInFamRew  bif) = ppr (bifrw_name bif)
  ppr (BuiltInFamInj bif)  = ppr (bifinj_name bif)
  ppr (UnbranchedAxiom ax) = ppr (coAxiomName ax)
  ppr (BranchedAxiom ax i) = ppr (coAxiomName ax) <> brackets (int i)

{- *********************************************************************
*                                                                      *
                    Built-in families
*                                                                      *
********************************************************************* -}


-- | A more explicit representation for `t1 ~ t2`.
type TypeEqn = Pair Type

-- Type checking of built-in families
data BuiltInSynFamily = BuiltInSynFamily
  { sfMatchFam :: [BuiltInFamRewrite]
  , sfInteract :: [BuiltInFamInjectivity]
    -- If given these type arguments and RHS, returns the equalities that
    -- are guaranteed to hold.  That is, if
    --     (ar, Pair s1 s2)  is an element of  (sfInteract tys ty)
    -- then  AxiomRule ar [co :: F tys ~ ty]  ::  s1~s2
  }

data BuiltInFamInjectivity  -- Argument and result role are always Nominal
  = BIF_Interact
      { bifinj_name :: FastString
      , bifinj_axr  :: CoAxiomRule -- Cached copy of (BuiltInFamINj this-bif)
                                   -- See Note [Avoiding allocating lots of CoAxiomRules]

      , bifinj_proves :: TypeEqn -> Maybe TypeEqn
            -- ^ Always unary: just one TypeEqn argument
            -- Returns @Nothing@ when it doesn't like the supplied argument.
            -- When this happens in a coercion that means that the coercion is
            -- ill-formed, and Core Lint checks for that.
      }

data BuiltInFamRewrite  -- Argument roles and result role are always Nominal
  = BIF_Rewrite
      { bifrw_name   :: FastString
      , bifrw_axr    :: CoAxiomRule -- Cached copy of (BuiltInFamRew this-bif)
                                    -- See Note [Avoiding allocating lots of CoAxiomRules]

      , bifrw_fam_tc :: TyCon       -- Needed for tyConsOfType

      , bifrw_arity  :: Arity       -- Number of type arguments needed
                                    -- to instantiate this axiom

      , bifrw_match :: [Type] -> Maybe ([Type], Type)
           -- coaxrMatch: does this reduce on the given arguments?
           -- If it does, returns (types to instantiate the rule at, rhs type)
           -- That is: mkAxiomCo ax (zipWith mkReflCo coAxiomRuleArgRoles ts)
           --              :: F tys ~N rhs,

      , bifrw_proves :: [TypeEqn] -> Maybe TypeEqn }
           -- length(inst_tys) = bifrw_arity

      -- INVARIANT: bifrw_match and bifrw_proves are related as follows:
      -- If    Just (inst_tys, res_ty) = bifrw_match ax arg_tys
      -- then  * length arg_tys = tyConArity fam_tc
      --       * length inst_tys = bifrw_arity
       --      * bifrw_proves (map (return @Pair) inst_tys) = Just (return @Pair res_ty)


-- Provides default implementations that do nothing.
trivialBuiltInFamily :: BuiltInSynFamily
trivialBuiltInFamily = BuiltInSynFamily { sfMatchFam = [], sfInteract = [] }
