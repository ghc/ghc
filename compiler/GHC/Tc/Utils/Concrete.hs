{-# LANGUAGE MultiWayIf #-}

-- | Checking for representation-polymorphism using the Concrete mechanism.
--
-- This module contains the logic for enforcing the representation-polymorphism
-- invariants by way of emitting constraints.
module GHC.Tc.Utils.Concrete
  ( -- * Ensuring that a type has a fixed runtime representation
    hasFixedRuntimeRep
  , hasFixedRuntimeRep_MustBeRefl
  )
 where

import GHC.Prelude

import GHC.Builtin.Types       ( unliftedTypeKindTyCon, liftedTypeKindTyCon )

import GHC.Core.Coercion       ( Role(..) )
import GHC.Core.Predicate      ( mkIsReflPrimPred )
import GHC.Core.TyCo.Rep       ( Type(TyConApp), mkTyVarTy )
import GHC.Core.Type           ( isConcrete, typeKind )

import GHC.Tc.Types            ( TcM, ThStage(Brack), PendingStuff(TcPending) )
import GHC.Tc.Types.Constraint ( mkNonCanonical )
import GHC.Tc.Types.Evidence   ( TcCoercion )
import GHC.Tc.Types.Origin     ( CtOrigin(..), FRROrigin(..) )
import GHC.Tc.Utils.Monad      ( emitSimple, getStage )
import GHC.Tc.Utils.TcType     ( TcType, TcKind, TcTyVar, MetaInfo(ConcreteTv) )
import GHC.Tc.Utils.TcMType    ( newAnonMetaTyVar, newWanted, emitWantedEq )

import GHC.Types.Basic         ( TypeOrKind(..) )

import GHC.Utils.Misc          ( HasDebugCallStack )
import GHC.Utils.Outputable
import GHC.Utils.Panic         ( assertPpr )

{- Note [Concrete overview]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC ensures that certain types have a fixed runtime representation in the
typechecker, by emitting certain constraints.
Emitting constraints to be solved later allows us to accept more programs:
if we directly inspected the type (using e.g. `typePrimRep`), we might not
have enough information available (e.g. if the type has kind `TYPE r` for
a metavariable `r` which has not yet been filled in.)

We give here an overview of the various moving parts, to serve
as a central point of reference for this topic.

  * Representation polymorphism
    Note [Representation polymorphism invariants] in GHC.Core
    Note [Representation polymorphism checking]

    The first note explains why we require that certain types have
    a fixed runtime representation.

    The second note details why we sometimes need a constraint to
    perform such checks in the typechecker: we might not know immediately
    whether a type has a fixed runtime representation. For example, we might
    need further unification to take place before being able to decide.
    So, instead of checking immediately, we emit a constraint.

  * What does it mean for a type to be concrete?
    Note [Concrete types] explains what it means for a type to be concrete.

    To compute which representation to use for a type, `typePrimRep` expects
    its kind to be concrete: something specific like `BoxedRep Lifted` or
    `IntRep`; certainly not a type involving type variables or type families.

  * What constraints do we emit?
    Note [The Concrete mechanism]

    Instead of simply checking that a type `ty` is concrete (i.e. computing
    'isConcrete`), we emit an equality constraint:

       co :: ty ~# concrete_ty

    where 'concrete_ty' is a concrete metavariable: a metavariable whose 'MetaInfo'
    is 'ConcreteTv', signifying that it can only be unified with a concrete type.

    The Note explains that this allows us to accept more programs. The Note
    also explains that the implementation is happening in two phases
    (PHASE 1 and PHASE 2).
    In PHASE 1 (the current implementation) we only allow trivial evidence
    of the form `co = Refl`.

  * Fixed runtime representation vs fixed RuntimeRep
    Note [Fixed RuntimeRep]

    We currently enforce the representation-polymorphism invariants by checking
    that binders and function arguments have a "fixed RuntimeRep".

    This is slightly less general than we might like, as this rules out
    types with kind `TYPE (BoxedRep l)`: we know that this will be represented
    by a pointer, which should be enough to go on in many situations.

  * When do we emit these constraints?
    Note [hasFixedRuntimeRep]

    We introduce constraints to satisfy the representation-polymorphism
    invariants outlined in Note [Representation polymorphism invariants] in GHC.Core,
    which mostly amounts to the following two cases:

      - checking that a binder has a fixed runtime representation,
      - checking that a function argument has a fixed runtime representation.

    The Note explains precisely how and where these constraints are emitted.

  * Reporting unsolved constraints
    Note [Reporting representation-polymorphism errors] in GHC.Tc.Types.Origin

    When we emit a constraint to enforce a fixed representation, we also provide
    a 'FRROrigin' which gives context about the check being done. This origin gets
    reported to the user if we end up with such an an unsolved Wanted constraint.

Note [Representation polymorphism checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
According to the "Levity Polymorphism" paper (PLDI '17),
there are two places in which we must know that a type has a
fixed runtime representation, as explained in
  Note [Representation polymorphism invariants] in GHC.Core:

  I1. the type of a bound term-level variable,
  I2. the type of an argument to a function.

The paper explains the restrictions more fully, but briefly:
expressions in these contexts need to be stored in registers, and it's
hard (read: impossible) to store something that does not have a
fixed runtime representation.

In practice, we enforce these types to have a /fixed RuntimeRep/, which is slightly
stronger, as explained in Note [Fixed RuntimeRep].

There are two different ways we check whether a given type
has a fixed runtime representation, both in the typechecker:

  1. When typechecking type declarations (e.g. datatypes, typeclass, pattern synonyms),
     under the GHC.Tc.TyCl module hierarchy.
     In these situations, we can immediately reject bad representation polymorphism.

     For instance, the following datatype declaration

       data Foo (r :: RuntimeRep) (a :: TYPE r) = Foo a

     is rejected in GHC.Tc.TyCl.checkValidDataCon upon seeing that the type 'a'
     is representation-polymorphic.

     Such checks are done using `GHC.Tc.Utils.TcMType.checkTypeHasFixedRuntimeRep`,
     with `GHC.Tc.Errors.Types.FixedRuntimeRepProvenance` describing the different
     contexts in which bad representation polymorphism can occur while validity checking.

  2. When typechecking value-level declarations (functions, expressions, patterns, ...),
     under the GHC.Tc.Gen module hierarchy.
     In these situations, the typechecker might need to do some work to figure out
     whether a type has a fixed runtime representation or not. For instance,
     GHC might introduce a metavariable (rr :: RuntimeRep), which is only later
     (through constraint solving) discovered to be equal to FloatRep.

     This is handled by the Concrete mechanism outlined in
     Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete.
     See Note [Concrete overview] in GHC.Tc.Utils.Concrete for an overview
     of the various moving parts.

Note [Concrete types]
~~~~~~~~~~~~~~~~~~~~~
Definition: a type is /concrete/ iff it is:
            - a concrete type constructor (as defined below), or
            - a concrete type variable (see Note [ConcreteTv] below), or
            - an application of a concrete type to another concrete type
            See GHC.Core.Type.isConcrete

Definition: a /concrete type constructor/ is defined by
            - a promoted data constructor
            - a class, data type or newtype
            - a primitive type like Array# or Int#
            - an abstract type as defined in a Backpack signature file
              (see Note [Synonyms implement abstract data] in GHC.Tc.Module)
            In particular, type and data families are not concrete.
            See GHC.Core.TyCon.isConcreteTyCon.

Examples of concrete types:
   Lifted, BoxedRep Lifted, TYPE (BoxedRep Lifted) are all concrete
Examples of non-concrete types
   F Int, TYPE (F Int), TYPE r, a[sk]
   NB: (F Int) is not concrete because F is a type function

The recursive definition of concreteness entails the following property:

Concrete Congruence Property (CCP)
  All sub-trees of a concrete type tree are concrete.

The following property also holds due to the invariant that the kind of a
concrete metavariable is itself concrete (see Note [ConcreteTv]):

Concrete Kinds Property (CKP)
  The kind of a concrete type is concrete.

The CCP and the CKP taken together mean that we never have to inspect
in kinds to check concreteness.

Note [ConcreteTv]
~~~~~~~~~~~~~~~~~
A concrete metavariable is a metavariable whose 'MetaInfo' is 'ConcreteTv'.
Similar to 'TyVarTv's which are type variables which can only be unified with
other type variables, a 'ConcreteTv' type variable is a type variable which can
only be unified with a concrete type (in the sense of Note [Concrete types]).

INVARIANT: the kind of a concrete metavariable is concrete.

This invariant is upheld at the time of creation of a new concrete metavariable.

Concrete metavariables are useful for representation-polymorphism checks:
they allow us to refer to a type whose representation is not yet known but will
be figured out by the typechecker (see Note [The Concrete mechanism]).

Note [The Concrete mechanism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To check (ty :: ki) has a fixed runtime representation, we proceed as follows:

  - Create a new concrete metavariable `concrete_tv`, i.e. a metavariable
    with 'ConcreteTv' 'MetaInfo' (see Note [ConcreteTv]).

  - Emit an equality constraint:

      ki ~# concrete_tv

    The origin for such an equality constraint uses
    `GHC.Tc.Types.Origin.FRROrigin`, so that we can report the appropriate
    representation-polymorphism error if any such constraint goes unsolved.

To solve `ki ~# concrete_ki`, we must unify `concrete_tv := concrete_ki`,
where `concrete_ki` is some concrete type. We can then compute `kindPrimRep`
on `concrete_ki` to compute the representation: this means `ty` indeed
has a fixed runtime representation.

-------------------------
-- PHASE 1 and PHASE 2 --
-------------------------

The Concrete mechanism is being implemented in two separate phases.

In PHASE 1 (currently implemented), we enforce that we only solve the emitted
constraints `co :: ki ~# concrete_tv` with `Refl`. This forbids any program
which requires type family evaluation in order to determine that a 'RuntimeRep'
is fixed. We do this using the `IsRefl#` special predicate (see Note [IsRefl#]);
we only solve `IsRefl# a b` if `a` and `b` are equal (after zonking, but not rewriting).
This means that it is safe to not use the coercion `co` anywhere in the program.
PHASE 1 corresponds to calls to `hasFixedRuntimeRep_MustBeRefl` in the code: on top
of emitting a constraint of the form `ki ~# concrete_tv`, we also emit
`IsRefl# ki concrete_tv` to ensure we only solve the equality constraint using
reflexivity.

In PHASE 2, we lift this restriction. This means we replace a call to
`hasFixedRuntimeRep_MustBeRefl` with a call to `hasFixedRuntimeRep`, and insert the
obtained coercion in the typechecked result. To illustrate what this entails,
recall that the code generator needs to be able to compute 'PrimRep's, so that it
can put function arguments in the correct registers, etc.
As a result, we must insert additional casts in Core to ensure that no type family
reduction is needed to be able to compute 'PrimRep's. For example, the Core

  f = /\ ( a :: F Int ). \ ( x :: a ). some_expression

is problematic when 'F' is a type family: we don't know what runtime representation to use
for 'x', so we can't compile this function (we can't evaluate type family applications
after we are done with typechecking). Instead, we ensure the 'RuntimeRep' is always
explicitly visible:

  f = /\ ( a :: F Int ). \ ( x :: ( a |> kco ) ). some_expression

where 'kco' is the appropriate coercion; for example if `F Int = TYPE Int#`
this would be:

  kco :: F Int ~# TYPE Int#

As `( a |> kco ) :: TYPE Int#`, the code generator knows to use a machine-sized
integer register for `x`, and all is good again.

Example test cases that require PHASE 2: T13105, T17021, T20363b.

Note [Fixed RuntimeRep]
~~~~~~~~~~~~~~~~~~~~~~~
Definition:
  a type `ty :: ki` has a /fixed RuntimeRep/
    <=>
  there exists a concrete type `concrete_ty` (in the sense of Note [Concrete types])
  such that we can solve `ki ~# concrete_ty`.

This definition is crafted to be useful to satisfy the invariants of
Core; see Note [Representation polymorphism invariants] in GHC.Core.

Notice that "fixed RuntimeRep" means (for now anyway) that
  * we know the runtime representation, and
  * we know the levity.

For example (ty :: TYPE (BoxedRep l)), where `l` is a levity variable
is /not/ "fixed RuntimeRep", even though it is always represented by
a heap pointer, because we don't know the levity.  In due course we
will want to make finer distinctions, as explained in the paper
Kinds are Calling Conventions [ICFP'20], but this suffices for now.

Note [hasFixedRuntimeRep]
~~~~~~~~~~~~~~~~~~~~~~~~~
The 'hasFixedRuntimeRep' function is responsible for taking a type 'ty'
and emitting a constraint to ensure that 'ty' has a fixed `RuntimeRep`,
as outlined in Note [The Concrete mechanism].

To do so, we compute the kind 'ki' of 'ty', create a new concrete metavariable
`concrete_tv` of kind `ki`, and emit a constraint `ki ~# concrete_tv`,
which will only be solved if we can prove that 'ty' indeed has a fixed RuntimeRep.

  [Wrinkle: Typed Template Haskell]
    We don't perform any checks when type-checking a typed Template Haskell quote:
    we want to allow representation polymorphic quotes, as long as they are
    monomorphised at splice site.

    Example:

      Module1

        repPolyId :: forall r (a :: TYPE r). CodeQ (a -> a)
        repPolyId = [|| \ x -> x ||]

      Module2

        import Module1

        id1 :: Int -> Int
        id1 = $$repPolyId

        id2 :: Int# -> Int#
        id2 = $$repPolyId

    We implement this skip by inspecting the TH stage in `hasFixedRuntimeRep`.

    A better solution would be to use 'CodeC' constraints, as in the paper
      "Staging With Class", POPL 2022
        by Ningning Xie, Matthew Pickering, Andres Löh, Nicolas Wu, Jeremy Yallop, Meng Wang
    but for the moment, as we will typecheck again when splicing,
    this shouldn't cause any problems in practice.  See ticket #18170.

    Test case: rep-poly/T18170a.

Note [IsRefl#]
~~~~~~~~~~~~~~
`IsRefl# :: forall k. k -> k -> TYPE (TupleRep '[])` is a constraint with no
evidence. `IsRefl# a b' can be solved precisely when `a` and `b` are equal (up to zonking,
but __without__ any rewriting).

That is, if we have a type family `F` with `F Int` reducing to `Int`, we __cannot__ solve
`IsRefl# (F Int) Int`.

What is the point of such a constraint? As outlined in Note [The Concrete mechanism],
to check `ty :: ki` has a fixed RuntimeRep we create a concrete metavariable `concrete_tv`
and emit a Wanted equality constraint

  {co_hole} :: ki ~# concrete_tv

Then, when we fill in the coercion hole with a coercion `co`, we must make use of it
(as that Note explains). If we don't, then we can only safely discard it if it is
reflexive. Therefore, whenever we perform a representation polymorphism check but also
discard the resulting coercion, we also emit a special constraint `IsRefl# ki concrete_tv`.
See 'hasFixedRuntimeRep_MustBeRefl', which calls 'hasFixedRuntimeRep', and thenemits
an 'IsRefl#' constraint to ensure that discarding the coercion is safe.
-}

-- | Like 'hasFixedRuntimeRep', but we insist that the obtained coercion must be 'Refl'.
--
-- This is useful if we are not actually going to use the coercion returned
-- from 'hasFixedRuntimeRep'; it would generally be unsound to allow a non-reflexive
-- coercion but not actually make use of it in a cast. See Note [IsRefl#].
--
-- The goal is to eliminate all uses of this function and replace them with
-- 'hasFixedRuntimeRep', making use of the returned coercion.
hasFixedRuntimeRep_MustBeRefl :: FRROrigin -> TcType -> TcM ()
hasFixedRuntimeRep_MustBeRefl frr_orig ty
  = do { -- STEP 1: check that the type has a fixed 'RuntimeRep'.
         mb_co <- hasFixedRuntimeRep frr_orig ty
         -- STEP 2: ensure that we only solve using a reflexive coercion.
       ; case mb_co of
           -- If the coercion is immediately reflexive: we're OK.
       { Nothing -> return ()
           -- Otherwise: emit an @IsRefl#@ constraint.
           -- This means we are free to discard the coercion.
       ; Just (ki, _co, concrete_kv) ->
    do { isRefl_ctev <- newWanted (FixedRuntimeRepOrigin ty frr_orig)
                                  (Just KindLevel) $
                          mkIsReflPrimPred ki (mkTyVarTy concrete_kv)
       ; emitSimple $ mkNonCanonical isRefl_ctev } } }

-- | Given a type @ty :: ki@, this function ensures that @ty@
-- has a __fixed__ 'RuntimeRep', by emitting a new equality constraint
-- @ki ~ concrete_tv@ for a concrete metavariable @concrete_tv@.
--
-- Returns a coercion @co :: ki ~# concrete_tv@ as evidence.
-- If @ty@ obviously has a fixed 'RuntimeRep', e.g @ki = IntRep@,
-- then this function immediately returns 'Nothing'
-- instead of emitting a new constraint.
hasFixedRuntimeRep :: FRROrigin -- ^ Context to be reported to the user
                                -- if the type ends up not having a fixed
                                -- 'RuntimeRep' (unsolved Wanted constraint).
                   -> TcType    -- ^ The type to check (we only look at its kind).
                   -> TcM (Maybe (TcType, TcCoercion, TcTyVar) )
                        -- ^ @Just ( ki, co, concrete_tv )@
                        -- where @co :: ki ~# concrete_ty@ is evidence that
                        -- the type @ty :: ki@ has a fixed 'RuntimeRep',
                        -- or 'Nothing' if 'ki' responds 'True' to 'isConcrete',
                        -- (i.e. we can take @co = Refl@).
hasFixedRuntimeRep frr_orig ty
  = do { th_stage <- getStage
       ; if
          -- Shortcut: check for 'Type' and 'UnliftedType' type synonyms.
          | TyConApp tc [] <- ki
          , tc == liftedTypeKindTyCon || tc == unliftedTypeKindTyCon
          -> return Nothing

          -- See [Wrinkle: Typed Template Haskell] in Note [hasFixedRuntimeRep].
          | Brack _ (TcPending {}) <- th_stage
          -> return Nothing

          | otherwise
          -- Ensure that the kind 'ki' of 'ty' is concrete.
          -> emitNewConcreteWantedEq_maybe orig ki
              -- NB: the kind of 'ki' is 'Data.Kind.Type', which is concrete.
              -- This means that the invariant required to call
              -- 'newConcreteTyVar' is satisfied.
       }
  where
    ki :: TcKind
    ki = typeKind ty
    orig :: CtOrigin
    orig = FixedRuntimeRepOrigin ty frr_orig

-- | Create a new metavariable, of the given kind, which can only be unified
-- with a concrete type.
--
-- Invariant: the kind must be concrete, as per Note [ConcreteTv].
-- This is checked with an assertion.
newConcreteTyVar :: HasDebugCallStack => TcKind -> TcM TcTyVar
newConcreteTyVar kind =
  assertPpr (isConcrete kind)
    (text "newConcreteTyVar: non-concrete kind" <+> ppr kind)
  $ newAnonMetaTyVar ConcreteTv kind

-- | Create a new concrete metavariable @concrete_tv@ and emit a new non-canonical Wanted
-- equality constraint @ty ~# concrete_tv@ with the given 'CtOrigin'.
--
-- Short-cut: if the type responds 'True' to 'isConcrete', then
-- we already know it is concrete, so don't emit any new constraints,
-- and return 'Nothing'.
--
-- Invariant: the kind of the supplied type must be concrete.
--
-- We also assume the provided type is already at the kind-level.
--(This only matters for error messages.)
emitNewConcreteWantedEq_maybe :: CtOrigin -> TcType -> TcM (Maybe (TcType, TcCoercion, TcTyVar))
emitNewConcreteWantedEq_maybe orig ty
  -- The input type is already concrete: no need to do anything.
  -- Return 'Nothing', indicating that reflexivity is valid evidence.
  | isConcrete ty
  = return Nothing
  -- Otherwise: create a new concrete metavariable and emit a new Wanted equality constraint.
  | otherwise
  = do { concrete_tv <- newConcreteTyVar ki
       ; co <- emitWantedEq orig KindLevel Nominal ty (mkTyVarTy concrete_tv)
                        --       ^^^^^^^^^ we assume 'ty' is at the kind level.
                        -- (For representation polymorphism checks, we are always checking a kind.)
       ; return $ Just (ty, co, concrete_tv) }
  where
    ki :: TcKind
    ki = typeKind ty
