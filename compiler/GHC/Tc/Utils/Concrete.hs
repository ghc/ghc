{-# LANGUAGE MultiWayIf #-}

module GHC.Tc.Utils.Concrete
  ( -- * Creating/emitting 'Concrete#' constraints
    hasFixedRuntimeRep
  , newConcretePrimWanted
    -- * HsWrapper: checking for representation-polymorphism
  , mkWpFun
  )
 where

import GHC.Prelude

import GHC.Core.Coercion
import GHC.Core.TyCo.Rep

import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType  ( mkTyConApp )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Origin ( CtOrigin(..), FRROrigin(..), WpFunOrigin(..) )

import GHC.Builtin.Types      ( unliftedTypeKindTyCon, liftedTypeKindTyCon )
import GHC.Builtin.Types.Prim ( concretePrimTyCon )

import GHC.Types.Basic      ( TypeOrKind(KindLevel) )

import GHC.Core.Type  ( isConcrete, typeKind )

{- Note [Concrete overview]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Special predicates of the form `Concrete# ty` are used
to check, in the typechecker, that certain types have a fixed runtime representation.
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
    Note [Concrete types]
    Note [The Concrete mechanism]

    The predicate 'Concrete# ty' is satisfied when we can produce
    a coercion

      co :: ty ~ concrete_ty

    where 'concrete_ty' consists only of concrete types (no type variables,
    no type families).

    The first note explains more precisely what it means for a type to be concrete.
    The second note explains how this relates to the `Concrete#` predicate,
    and explains that the implementation is happening in two phases (PHASE 1 and PHASE 2).
    In PHASE 1 (the current implementation) we only allow trivial evidence
    of the form `co = Refl`.

  * Fixed runtime representation vs fixed RuntimeRep
    Note [Fixed RuntimeRep]

    We currently enforce the representation-polymorphism invariants by checking
    that binders and function arguments have a "fixed RuntimeRep".
    That is, `ty :: ki` has a "fixed RuntimeRep" if we can solve `Concrete# ki`.

    This is slightly less general than we might like, as this rules out
    types with kind `TYPE (BoxedRep l)`: we know that this will be represented
    by a pointer, which should be enough to go on in many situations.

  * When do we emit 'Concrete#' constraints?
    Note [hasFixedRuntimeRep]

    We introduce 'Concrete#' constraints to satisfy the representation-polymorphism
    invariants outlined in Note [Representation polymorphism invariants] in GHC.Core,
    which mostly amounts to the following two cases:

      - checking that a binder has a fixed runtime representation,
      - checking that a function argument has a fixed runtime representation.

    The note explains precisely how we emit these 'Concrete#' constraints.

  * How do we solve Concrete# constraints?
    Note [Solving Concrete# constraints] in GHC.Tc.Instance.Class

    Concrete# constraints are solved through two mechanisms,
    which are both explained further in the note:

      - by decomposing them, e.g. `Concrete# (TYPE r)` is turned
        into `Concrete# r` (canonicalisation of `Concrete#` constraints),
      - by using 'Concrete' instances (top-level interactions).
        The note explains that the evidence we get from using such 'Concrete'
        instances can only ever be Refl, even in PHASE 2.

  * Reporting unsolved Concrete# constraints
    Note [Reporting representation-polymorphism errors] in GHC.Tc.Types.Origin

    When we emit a 'Concrete#' constraint, we also provide a 'FRROrigin'
    which gives context about the check being done. This origin gets reported
    to the user if we end up with an unsolved Wanted 'Concrete#' constraint.

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

     The idea is that, to guarantee that a type (rr :: RuntimeRep) is
     representation-monomorphic, we emit a 'Concrete# rr' Wanted constraint.
     If GHC can solve this constraint, it means 'rr' is monomorphic, and we
     are OK to proceed. Otherwise, we report this unsolved Wanted in the form
     of a representation-polymorphism error. The different contexts in which
     such constraints arise are enumerated in 'FRROrigin'.

Note [Concrete types]
~~~~~~~~~~~~~~~~~~~~~
Definition: a type is /concrete/
            iff it consists of a tree of concrete type constructors
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
   F Int, TYPE (F Int), TYPE r, a Int
   NB: (F Int) is not concrete because F is a type function

Note [The Concrete mechanism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As explained in (2) in Note [Representation polymorphism checking],
to check (ty :: ki) has a fixed runtime representation,
we emit a `Concrete# ki` constraint, where

  Concrete# :: forall k. k -> TupleRep '[]

Such constraints get solved by decomposition, as per
  Note [Canonical Concrete# constraints] in GHC.Tc.Solver.Canonical.
When unsolved Wanted `Concrete#` constraints remain after typechecking,
we report them as representation-polymorphism errors, using `GHC.Tc.Types.Origin.FRROrigin`
to inform the user of the context in which a fixed-runtime-rep check arose.

--------------
-- EVIDENCE --
--------------

The evidence for a 'Concrete# ty' constraint is a nominal coercion

  co :: ty ~# concrete_ty

where 'concrete_ty' consists only of (non-synonym) type constructors and applications
(after expanding any vanilla type synonyms).

  OK:

    TYPE FloatRep
    TYPE (BoxedRep Lifted)
    Type
    TYPE (TupleRep '[ FloatRep, SumRep '[ IntRep, IntRep ] ])

  Not OK:

    Type variables:

      ty
      TYPE r
      TYPE (BoxedRep l)

    Type family applications:

      TYPE (Id FloatRep)

This is so that we can compute the 'PrimRep's needed to represent the type
using 'runtimeRepPrimRep', which expects to be able to read off the 'RuntimeRep',
as per Note [Getting from RuntimeRep to PrimRep] in GHC.Types.RepType.

Note that the evidence for a `Concrete#` constraint isn't a typeclass dictionary:
like with `(~#)`, the evidence is an (unlifted) nominal coercion, which justifies defining

  Concrete# :: forall k. k -> TYPE (TupleRep '[])

We still need a constraint that users can write in their own programs,
so much like `(~#)` and `(~)` we also define:

  Concrete :: forall k. k -> Constraint

The need for user-facing 'Concrete' constraints is detailed in
  Note [Concrete and Concrete#] in GHC.Builtin.Types.

-------------------------
-- PHASE 1 and PHASE 2 --
-------------------------

The Concrete mechanism is being implemented in two separate phases.

In PHASE 1 (currently implemented), we never allow a 'Concrete#' constraint
to be rewritten (see e.g. GHC.Tc.Solver.Canonical.canConcretePrim).
The only allowable evidence term is Refl, which forbids any program
that requires type family evaluation in order to determine that a 'RuntimeRep' is fixed.
N.B.: we do not currently check that this invariant is upheld: as we are discarding the
evidence in PHASE 1, we no longer have access to the coercion after constraint solving
(which is the point at which we would want to check that the filled in evidence is Refl).

In PHASE 2 (future work), we lift this restriction. To illustrate what this entails,
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

where 'kco' is the evidence for `Concrete# (F Int)`, for example if `F Int = TYPE Int#`
this would be:

  kco :: F Int ~# TYPE Int#

As `( a |> kco ) :: TYPE Int#`, the code generator knows to use a machine-sized
integer register for `x`, and all is good again.

Example test cases that require PHASE 2: T13105, T17021, T20363b.

Note [Fixed RuntimeRep]
~~~~~~~~~~~~~~~~~~~~~~~
Definition:
  a type `ty :: ki` has a /fixed RuntimeRep/
  iff we can solve `Concrete# ki`

In PHASE 1 (see Note [The Concrete mechanism]), this is equivalent to:

  a type `ty :: ki` has a /fixed RuntimeRep/
  iff `ki` is a concrete type (in the sense of Note [Concrete types]).

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
and emitting a 'Concrete#' constraint to ensure that 'ty' has a fixed `RuntimeRep`,
as outlined in Note [The Concrete mechanism].

To do so, we compute the kind 'ki' of 'ty' and emit a 'Concrete# ki' constraint,
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

Note [Solving Concrete# constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The representation polymorphism checks emit 'Concrete# ty' constraints,
as explained in Note [hasFixedRuntimeRep] in GHC.Tc.Utils.Concrete.

The main mechanism through which a `Concrete# ty` constraint is solved
is to directly inspect 'ty' to check that it is a concrete type
such as 'TYPE IntRep' or `TYPE (TupleRep '[ TupleRep '[], FloatRep ])`,
and not, e.g., a skolem type variable.

There are, however, some interactions to take into account:

  1. Decomposition.

      The solving of `Concrete#` constraints works recursively.
      For example, to solve a Wanted `Concrete# (TYPE r)` constraint,
      we decompose it, emitting a new `Concrete# @RuntimeRep r` Wanted constraint,
      and use it to solve the original `Concrete# (TYPE r)` constraint.
      This happens in the canonicaliser -- see GHC.Tc.Solver.Canonical.canDecomposableConcretePrim.

      Note that Decomposition fully solves `Concrete# ty` whenever `ty` is a
      concrete type.   For example:

          Concrete# (TYPE (BoxedRep Lifted))
          ==> (decompose)
          Concrete# (BoxedRep Lifted)
          ==> (decompose)
          Concrete# Lifted
          ==> (decompose)
          <nothing, since Lifted is nullary>

  2. Rewriting.

      In PHASE 1 (as per Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete),
      we don't have to worry about a 'Concrete#' constraint being rewritten.
      We only need to zonk: if e.g. a metavariable, `alpha`, gets unified with `IntRep`,
      we should be able to solve `Concrete# alpha`.

      In PHASE 2, we will need to proceed as in GHC.Tc.Solver.Canonical.canClass:
      if we have a constraint `Concrete# (F ty1)` and a coercion witnessing the reduction of
      `F`, say `co :: F ty1 ~# ty2`, then we will solve `Concrete# (F ty1)` in terms of `Concrete# ty2`,
      by rewriting the evidence for `Concrete# ty2` using `co` (see GHC.Tc.Solver.Canonical.rewriteEvidence).

  3. Backpack

      Abstract 'TyCon's in Backpack signature files are always considered to be concrete.
      This is because of the strong restrictions around how abstract types are allowed
      to be implemented, as laid out in Note [Synonyms implement abstract data] in GHC.Tc.Module.
      In particular, no variables or type family applications are allowed.

      Examples: backpack/should_run/T13955.bkp, rep-poly/RepPolyBackpack2.
-}

-- | Evidence for a `Concrete#` constraint:
-- essentially a 'ConcreteHole' (a coercion hole) that will be filled later,
-- except:
--
--   - we might already have the evidence now; no point in creating a coercion hole
--     in that case;
--   - we sometimes skip the check entirely, e.g. in Typed Template Haskell
--     (see [Wrinkle: Typed Template Haskell] in Note [hasFixedRuntimeRep]).
data ConcreteEvidence
  = ConcreteReflEvidence
    -- ^ We have evidence right now: don't bother creating a 'ConcreteHole'.
  | ConcreteTypedTHNoEvidence
    -- ^ We don't emit 'Concrete#' constraints in Typed Template Haskell.
    -- See [Wrinkle: Typed Template Haskell] in Note [hasFixedRuntimeRep].
  | ConcreteHoleEvidence ConcreteHole
    -- ^ The general form of evidence: a 'ConcreteHole' that should be
    -- filled in later by the constraint solver, as per
    -- Note [Solving Concrete# constraints].

-- | Check that the kind of the provided type is of the form
-- @TYPE rep@ for a __fixed__ 'RuntimeRep' @rep@.
--
-- If this isn't immediately obvious, for instance if the 'RuntimeRep'
-- is hidden under a type-family application such as
--
-- > ty :: TYPE (F x)
--
-- this function will emit a new Wanted 'Concrete#' constraint.
hasFixedRuntimeRep :: FRROrigin -> Type -> TcM ConcreteEvidence
hasFixedRuntimeRep frrOrig ty

  -- Shortcut: check for 'Type' and 'UnliftedType' type synonyms.
  | TyConApp tc [] <- ki
  , tc == liftedTypeKindTyCon || tc == unliftedTypeKindTyCon
  = return ConcreteReflEvidence

  | otherwise
  = do { th_stage <- getStage
       ; if
          -- We have evidence for 'Concrete# ty' right now:
          -- no need to emit a constraint/create an evidence hole.
          | isConcrete ki
          -> return ConcreteReflEvidence

          -- See [Wrinkle: Typed Template Haskell] in Note [hasFixedRuntimeRep].
          | Brack _ (TcPending {}) <- th_stage
          -> return ConcreteTypedTHNoEvidence

          -- Create a new Wanted 'Concrete#' constraint and emit it.
          | otherwise
          -> do { loc <- getCtLocM (FixedRuntimeRepOrigin ty frrOrig) (Just KindLevel)
                ; (hole, _, ct_ev) <- newConcretePrimWanted loc ki
                ; emitSimple $ mkNonCanonical ct_ev
                ; return $ ConcreteHoleEvidence hole } }
  where
    ki :: Kind
    ki = typeKind ty

-- | Create a new 'Concrete#' constraint.
-- Returns the evidence, a metavariable which will be filled in with a
-- guaranteed-concrete type, and a Wanted CtEvidence
newConcretePrimWanted :: CtLoc -> Type -> TcM (ConcreteHole, TcType, CtEvidence)
newConcretePrimWanted loc ty
  = do { let
           ki :: Kind
           ki = typeKind ty
       ; (hole, concrete_ty) <- newConcreteHole ki ty
       ; let
           wantedCtEv :: CtEvidence
           wantedCtEv =
             CtWanted
               { ctev_dest = HoleDest hole
               , ctev_pred = mkTyConApp concretePrimTyCon [ki, ty]
               , ctev_rewriters = emptyRewriterSet
               , ctev_loc  = loc
               }
        ; return (hole, concrete_ty, wantedCtEv) }

{-***********************************************************************
*                                                                       *
                 HsWrapper
*                                                                       *
***********************************************************************-}

-- | Smart constructor to create a 'WpFun' 'HsWrapper'.
--
-- Might emit a 'Concrete#' constraint, to check for
-- representation polymorphism. This is necessary, as 'WpFun' will desugar to
-- a lambda abstraction, whose binder must have a fixed runtime representation.
mkWpFun :: HsWrapper -> HsWrapper
        -> Scaled TcType -- ^ the "from" type of the first wrapper
        -> TcType        -- ^ either type of the second wrapper (used only when the
                         -- second wrapper is the identity)
        -> WpFunOrigin   -- ^ what caused you to want a WpFun?
        -> TcM HsWrapper
mkWpFun WpHole       WpHole       _             _  _ = return $ WpHole
mkWpFun WpHole       (WpCast co2) (Scaled w t1) _  _ = return $ WpCast (mkTcFunCo Representational (multToCo w) (mkTcRepReflCo t1) co2)
mkWpFun (WpCast co1) WpHole       (Scaled w _)  t2 _ = return $ WpCast (mkTcFunCo Representational (multToCo w) (mkTcSymCo co1) (mkTcRepReflCo t2))
mkWpFun (WpCast co1) (WpCast co2) (Scaled w _)  _  _ = return $ WpCast (mkTcFunCo Representational (multToCo w) (mkTcSymCo co1) co2)
mkWpFun co1          co2          t1            _  wpFunOrig
  = do { _concrete_ev <- hasFixedRuntimeRep (FRRWpFun wpFunOrig) (scaledThing t1)
       ; return $ WpFun co1 co2 t1 }
