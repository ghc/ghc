{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ParallelListComp #-}

-- | Checking for representation-polymorphism using the Concrete mechanism.
--
-- This module contains the logic for enforcing the representation-polymorphism
-- invariants by way of emitting constraints.
module GHC.Tc.Utils.Concrete
  ( -- * Ensuring that a type has a fixed runtime representation
    hasFixedRuntimeRep
  , hasFixedRuntimeRep_syntactic

  , unifyConcrete

  , idConcreteTvs
  )
 where

import GHC.Prelude

import GHC.Builtin.Names       ( unsafeCoercePrimName )
import GHC.Builtin.Types

import GHC.Core.Coercion
import GHC.Core.TyCo.Rep
import GHC.Core.Type

import GHC.Data.Bag

import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import {-# SOURCE #-} GHC.Tc.Utils.Unify

import GHC.Types.Basic         ( TypeOrKind(KindLevel) )
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Name.Env
import GHC.Types.Var

import GHC.Utils.Misc          ( HasDebugCallStack )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.FastString     ( FastString, fsLit )

import Control.Monad      ( void )
import Data.Functor       ( ($>) )


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
    'isConcreteType`), we emit an equality constraint:

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
    a 'FixedRuntimeRepOrigin' which gives context about the check being done.
    This origin gets reported to the user if we end up with such an an unsolved Wanted constraint.

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
GHC.Core.Type.isConcreteType checks whether a type meets this definition.

Definition: a /concrete type constructor/ is defined by
            - a promoted data constructor
            - a class, data type or newtype
            - a primitive type like Array# or Int#
            - an abstract type as defined in a Backpack signature file
              (see Note [Synonyms implement abstract data] in GHC.Tc.Module)
            In particular, type and data families are not concrete.
GHC.Core.TyCon.isConcreteTyCon checks whether a TyCon meets this definition.

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
    `GHC.Tc.Types.Origin.FixedRuntimeRepOrigin`, so that we can report the
    appropriate representation-polymorphism error if any such constraint
    goes unsolved.

To solve `ki ~# concrete_ki`, we must unify `concrete_tv := concrete_ki`,
where `concrete_ki` is some concrete type. We can then compute `kindPrimRep`
on `concrete_ki` to compute the representation: this means `ty` indeed
has a fixed runtime representation.

-------------------------
-- PHASE 1 and PHASE 2 --
-------------------------

The Concrete mechanism is being implemented in two separate phases.

In PHASE 1, we enforce that we only solve the emitted constraints
`co :: ki ~# concrete_tv` with `Refl`. This forbids any program
which requires type family evaluation in order to determine that a 'RuntimeRep'
is fixed.
To achieve this, instead of creating a new concrete metavariable, we directly
ensure that 'ki' is concrete, using 'makeTypeConcrete'. If it fails, then
we report an error (even though rewriting might have allowed us to proceed).

In PHASE 2, we lift this restriction. This means we replace a call to
`hasFixedRuntimeRep_syntactic` with a call to `hasFixedRuntimeRep`, and insert the
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

Because we can convert calls from hasFixedRuntimeRep_syntactic to
hasFixedRuntimeRep one at a time, we can migrate from PHASE 1 to PHASE 2
incrementally.

Example test cases that require PHASE 2: T13105, T17021, T20363b.

Note [Fixed RuntimeRep]
~~~~~~~~~~~~~~~~~~~~~~~
Definitions:

  FRR.

    The type `ty :: ki` has a /syntactically fixed RuntimeRep/
    (we also say that `ty` is an `FRRType`)
      <=>
    the kind `ki` is concrete (in the sense of Note [Concrete types])
      <=>
    `typePrimRep ty` (= `kindPrimRep ki`) does not crash
    (assuming that typechecking succeeded, so that all metavariables
    in `ty` have been filled)

  Fixed RuntimeRep.

    The type `ty :: ki` has a /fixed RuntimeRep/
      <=>
    there exists an FRR type `ty'` with `ty ~# ty'`
      <=>
    there exists a concrete type `concrete_ki` such that
    `ki ~ concrete_ki`

These definitions are crafted to be useful to satisfy the invariants of
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

If we can solve the equality constraint, i.e. produce a coercion
`kco :: ki ~# concrete_tv`, then 'hasFixedRuntimeRep' returns the coercion

  co = GRefl ty kco :: ty ~# ty |> kco

The RHS of the coercion `co` is `ty |> kco`. The kind of this type is
concrete (by construction), which means that `ty |> kco` is an FRRType
in the sense of Note [Fixed RuntimeRep], so that we can directely compute
its runtime representation using `typePrimRep`.

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


Note [Representation-polymorphic Ids with no binding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We cannot have representation-polymorphic or levity-polymorphic
function arguments. See Note [Representation polymorphism invariants]
in GHC.Core.  That is checked in 'GHC.Tc.Gen.App.tcInstFun', see the call
to 'matchActualFunTy', which performs the representation-polymorphism
check.

However, some special Ids have representation-polymorphic argument
types. These are all GHC built-ins or data constructors. They have no binding;
instead they have compulsory unfoldings. Specifically, these Ids are:

1. Some wired-in Ids, such as coerce, oneShot and unsafeCoerce# (which is only
   partly wired-in),
2. Representation-polymorphic primops, such as raise#.
3. Representation-polymorphic data constructors: unboxed tuples
   and unboxed sums.
4. Newtype constructors with `UnliftedNewtypes` which have
   a representation-polymorphic argument.

For (1) consider
  badId :: forall r (a :: TYPE r). a -> a
  badId = unsafeCoerce# @r @r @a @a

The (partly) wired-in function
  unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                   (a :: TYPE r1) (b :: TYPE r2).
                   a -> b
has a convenient but representation-polymorphic type. It has no
binding; instead it has a compulsory unfolding, after which we
would have
  badId = /\r /\(a :: TYPE r). \(x::a). ...body of unsafeCorece#...
And this is no good because of that rep-poly \(x::a).  So we want
to reject this.

On the other hand
  goodId :: forall (a :: Type). a -> a
  goodId = unsafeCoerce# @LiftedRep @LiftedRep @a @a

is absolutely fine, because after we inline the unfolding, the \(x::a)
is representation-monomorphic.

Test cases: T14561, RepPolyWrappedVar2.

For primops (2) and unboxed tuples/sums (3), the situation is similar;
they are eta-expanded in CorePrep to be saturated, and that eta-expansion
must not add a representation-polymorphic lambda.

Test cases: T14561b, RepPolyWrappedVar, UnliftedNewtypesCoerceFail.

The Note [Representation-polymorphism checking built-ins] explains how we handle
cases (1) (2) and (3).

For (4), consider a representation-polymorphic newtype with
UnliftedNewtypes:

  type Id :: forall r. TYPE r -> TYPE r
  newtype Id a where { MkId :: a }

  bad :: forall r (a :: TYPE r). a -> Id a
  bad = MkId @r @a             -- Want to reject

  good :: forall (a :: Type). a -> Id a
  good = MkId @LiftedRep @a   -- Want to accept

Test cases: T18481, UnliftedNewtypesLevityBinder

(4) is handled differently than (1) (2) and (3);
see Note [Eta-expanding rep-poly unlifted newtypes].

Note [Representation-polymorphism checking built-ins]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some primops and wired-in functions are representation-polymorphic, but must
only be instantiated at particular, concrete representations.
There are three cases, all for `hasNoBinding` Ids:

* Wired-in Ids.  For example, `seq`
  is a wired-in Id, defined in GHC.Types.Id.Make.seqId, with this type:

  seq :: forall {r} a (b :: TYPE r). a -> b -> b

  It is more like a macro than a regular Id: it has /compulsory/ unfolding, so
  we inline it at every call site.  At those call sites we should instantiate
  `r` with a concrete RuntimeRep, so that the lambda has a concrete representation.
  So somehow the type checker has to ensure that `seq` is called with a concrete
  instantiation for `r`.

  NB: unsafeCoerce# is not quite wired-in (see Note [Wiring in unsafeCoerce#] in GHC.HsToCore),
  but it gets a similar treatment.

* PrimOps. Some representation-polymorphic primops must be called at a concrete
  type.  For example:

  catch# :: forall {r} {l} (k :: TYPE r) (w :: TYPE (BoxedRep l)).
              (State# RealWorld -> (# State# RealWorld, k #) )
           -> (w -> State# RealWorld -> (# State# RealWorld, k #) )
           -> State# RealWorld -> (# State# RealWorld, k #)

  This primop pushes a "catch frame" on the stack, which must "know"
  the return convention of `k`.  So `k` must be concrete, so we know
  what kind of catch-frame to push. (See #21868 for more details.

  So again we want to ensure that `r` is instantiated with a concrete RuntimeRep.

* Unboxed-tuple data constructors.  Consider the unboxed pair data constructor:

  (#,#) :: forall {r1} {r2} (a :: TYPE r1) (b :: TYPE r2). a -> b -> (# a, b #)

  Again, we need concrete `r1` and `r2`. For example, we want to reject

    f :: forall r (a :: TYPE r). a -> (# Int, a #)
    f = (#,#) 3

As pointed out in #21906; we see here that it is not enough to simply check
the representation of the argument types, as for example "k :: TYPE r" in the
type of catch# occurs in negative position but not directly as the type of
an argument.

NB: we specifically *DO NOT* handle representation-polymorphic unlifted newtypes
with this mechanism. See Note [Eta-expanding rep-poly unlifted newtypes] for an
overview of representation-polymorphism checks for those.

To achieve this goal, for these these three kinds of `hasNoBinding` functions:

* We identify the quantified variable `r` as a "concrete quantifier"

* When instantiating a concrete quantifier, such as `r`, at a call site, we
  instantiate with a ConcreteTv meta-tyvar, `r0[conc]`.
  See Note [ConcreteTv] in GHC.Tc.Utils.Concrete.

Now the type checker will ensure that `r0` is instantiated with a concrete
RuntimeRep.

Here are the moving parts:

* In the IdDetails of an Id, we record a mapping from type variable name
  to concreteness information, in the form of a ConcreteTvOrigin.
  See 'idDetailsConcreteTvs'.

  The ConcreteTvOrigin is used to determine which error message to show
  to the user if the type variable gets instantiated to a non-concrete type;
  this is slightly more granular than simply storing a set of type variable names.

* The domain of this NameEnv is the outer forall'd TyVars of that
  Id's type.  (A bit yukky because it means that alpha-renaming that type
  would be invalid.  But we never do that.)  So `seq` has
    Type:       forall {r} a (b :: TYPE r). a -> b -> b
    IdDetails:  RepPolyId [ r :-> ConcreteFRR (FixedRuntimeRepOrigin b (..)) ]

* When instantiating the type of an Id at a call site, at the call to
  GHC.Tc.Utils.Instantiate.instantiateSigma in GHC.Tc.Gen.App.tcInstFun,
  create ConcreteTv metavariables (instead of TauTvs) based on the
  ConcreteTyVars stored in the IdDetails of the Id.

Note that the /only/ place that one of these restricted rep-poly Ids can enter
typechecking is in `tcInferId`, and all the interesting cases then land
in `tcInstFun` where we take care to instantantiate those concrete
type variables correctly.

  Design alternative: in some ways, it would be more kosher for the concrete-ness
  to be stored in the /type/, thus  forall (r[conc] :: RuntimeRep). ty.
  But that pollutes Type for a very narrow use-case; so instead we adopt the
  more ad-hoc solution described above.

Examples:

  ok :: forall (a :: Type) (b :: Type). a -> b -> b
  ok = seq

  bad :: forall s (b :: TYPE s). Int -> b -> b
  bad x = seq x

    Here we will instantiate the RuntimeRep skolem variable r from the type
    of seq to a concrete metavariable rr[conc].
    For 'ok' we will unify rr := LiftedRep, and for 'bad' we will fail to
    solve rr[conc] ~# s[sk] and report a representation-polymorphism error to
    the user.

  type RR :: RuntimeRep
  type family RR where { RR = IntRep }

  tricky1, tricky2 :: forall (b :: TYPE RR). Int -> b -> b
  tricky1 = seq
  tricky2 = seq @RR

    'tricky1' proceeds as above: we instantiate r |-> rr[conc], get a Wanted
    rr[conc] ~# RR, which we solve by rewriting the type family.

    For 'tricky2', we again create a fresh ConcreteTv metavariable rr[conc],
    and we then proceed as if the user had written "seq @rr", but adding an
    additional [W] rr ~ RR to the constraint solving context.

[Wrinkle: VTA]

  We must also handle the case when the user has instantiated the type variables
  themselves, with a visible type application. We do this in GHC.Tc.Gen.App.tcVTA.

  For example:

    type F :: Type -> RuntimeRep
    type family F a where { F Bool = IntRep }

    foo = (# , #) @(F Bool) @FloatRep

  We want to accept "foo" even though "F Bool" is not a concrete RuntimeRep.
  We proceed as follows (see tcVTA):

    - create a fresh concrete metavariable kappa,
    - emit [W] F Bool ~ kappa[conc]
    - pretend the user wrote (#,#) @kappa.

  The solver will then unify kappa := IntRep, after rewriting the type family
  application on the LHS of the Wanted.

  Note that this is a bit of a corner case: only a few built-ins, such as
  unsafeCoerce# and unboxed tuples, have specified (not inferred) RuntimeRep
  quantified variables which can be instantiated by the user with a
  visible type application.
  For example,

    coerce :: forall {r :: RuntimeRep} (a :: TYPE r) (b :: TYPE r)
           .  Coercible a b => a -> b

  does not allow the RuntimeRep argument to be specified by a visible type
  application.
-}

-- | Given a type @ty :: ki@, this function ensures that @ty@
-- has a __fixed__ 'RuntimeRep', by emitting a new equality constraint
-- @ki ~ concrete_tv@ for a concrete metavariable @concrete_tv@.
--
-- Returns a coercion @co :: ty ~# concrete_ty@ as evidence.
-- If @ty@ obviously has a fixed 'RuntimeRep', e.g @ki = IntRep@,
-- then this function immediately returns 'MRefl',
-- without emitting any constraints.
hasFixedRuntimeRep :: HasDebugCallStack
                   => FixedRuntimeRepContext
                        -- ^ Context to be reported to the user
                        -- if the type ends up not having a fixed
                        -- 'RuntimeRep'.
                   -> TcType
                        -- ^ The type to check (we only look at its kind).
                   -> TcM (TcCoercionN, TcTypeFRR)
                        -- ^ @(co, ty')@ where @ty' :: ki'@,
                        -- @ki@ is concrete, and @co :: ty ~# ty'@.
                        -- That is, @ty'@ has a syntactically fixed RuntimeRep
                        -- in the sense of Note [Fixed RuntimeRep].
hasFixedRuntimeRep frr_ctxt ty =
  checkFRR_with (fmap (fmap coToMCo) . unifyConcrete_kind (fsLit "cx") . ConcreteFRR) frr_ctxt ty

-- | Like 'hasFixedRuntimeRep', but we perform an eager syntactic check.
--
-- Throws an error in the 'TcM' monad if the check fails.
--
-- This is useful if we are not actually going to use the coercion returned
-- from 'hasFixedRuntimeRep'; it would generally be unsound to allow a non-reflexive
-- coercion but not actually make use of it in a cast.
--
-- The goal is to eliminate all uses of this function and replace them with
-- 'hasFixedRuntimeRep', making use of the returned coercion. This is what
-- is meant by going from PHASE 1 to PHASE 2, in Note [The Concrete mechanism].
hasFixedRuntimeRep_syntactic :: HasDebugCallStack
                             => FixedRuntimeRepContext
                                  -- ^ Context to be reported to the user
                                  -- if the type does not have a syntactically
                                  -- fixed 'RuntimeRep'.
                             -> TcType
                                  -- ^ The type to check (we only look at its kind).
                             -> TcM ()
hasFixedRuntimeRep_syntactic frr_ctxt ty
  = void $ checkFRR_with ensure_conc frr_ctxt ty
    where
      ensure_conc :: FixedRuntimeRepOrigin -> TcKind -> TcM TcMCoercionN
      ensure_conc frr_orig ki = ensureConcrete frr_orig ki $> MRefl

-- | Internal function to check whether the given type has a fixed 'RuntimeRep'.
--
-- Use 'hasFixedRuntimeRep' to allow rewriting, or 'hasFixedRuntimeRep_syntactic'
-- to perform a syntactic check.
checkFRR_with :: HasDebugCallStack
              => (FixedRuntimeRepOrigin -> TcKind -> TcM TcMCoercionN)
                   -- ^ The check to perform on the kind.
              -> FixedRuntimeRepContext
                   -- ^ The context which required a fixed 'RuntimeRep',
                   -- e.g. an application, a lambda abstraction, ...
              -> TcType
                   -- ^ The type @ty@ to check (the check itself only looks at its kind).
              -> TcM (TcCoercionN, TcTypeFRR)
                  -- ^ Returns @(co, frr_ty)@ with @co :: ty ~# frr_ty@
                  -- and @frr_@ty has a fixed 'RuntimeRep'.
checkFRR_with check_kind frr_ctxt ty
  = do { th_stage <- getStage
       ; if
          -- Shortcut: check for 'Type' and 'UnliftedType' type synonyms.
          | TyConApp tc [] <- ki
          , tc == liftedTypeKindTyCon || tc == unliftedTypeKindTyCon
          -> return refl

          -- See [Wrinkle: Typed Template Haskell] in Note [hasFixedRuntimeRep].
          | Brack _ (TcPending {}) <- th_stage
          -> return refl

          -- Otherwise: ensure that the kind 'ki' of 'ty' is concrete.
          | otherwise
          -> do { kco <- check_kind frr_orig ki
                ; return ( mkGReflRightMCo Nominal ty kco
                         , mkCastTyMCo ty kco ) } }

  where
    refl :: (TcCoercionN, TcType)
    refl = (mkNomReflCo ty, ty)
    ki :: TcKind
    ki = typeKind ty
    frr_orig :: FixedRuntimeRepOrigin
    frr_orig = FixedRuntimeRepOrigin { frr_type = ty, frr_context = frr_ctxt }

-- | Ensure that the given kind @ki@ can unify with a concrete type,
-- in the sense of Note [Concrete types].
--
-- Returns a coercion @co :: ki ~# conc_ki@, where @conc_ki@ is
-- concrete.
--
-- If the kind is already syntactically concrete, this
-- immediately returns a reflexive coercion. Otherwise,
-- it creates a new concrete metavariable @concrete_tv@
-- and emits an equality constraint @ki ~# concrete_tv@,
-- to be handled by the constraint solver.
--
-- Precondition: @ki@ must be of the form @TYPE rep@ or @CONSTRAINT rep@.
unifyConcrete_kind :: HasDebugCallStack
                   => FastString -- ^ name to use when creating concrete metavariables
                   -> ConcreteTvOrigin
                   -> TcKind
                   -> TcM TcCoercionN
unifyConcrete_kind occ_fs conc_orig ki
  | Just (torc, rep) <- sORTKind_maybe ki
  = do { let tc = case torc of
                    TypeLike -> tYPETyCon
                    ConstraintLike -> cONSTRAINTTyCon
       ; rep_co <- unifyConcrete occ_fs conc_orig rep
       ; return $ mkTyConAppCo Nominal tc [rep_co] }
  | otherwise
  = pprPanic "unifyConcrete_kind: kind is not of the form 'TYPE rep' or 'CONSTRAINT rep'" $
      ppr ki <+> dcolon <+> ppr (typeKind ki)


-- | Ensure the given type can be unified with
-- a concrete type, in the sense of Note [Concrete types].
--
-- Returns a coercion @co :: ty ~# conc_ty@, where @conc_ty@ is
-- concrete.
--
-- If the type is already syntactically concrete, this
-- immediately returns a reflexive coercion.
-- Otherwise, it will create new concrete metavariables and emit
-- new Wanted equality constraints, to be handled by the constraint solver.
--
-- Invariant: the kind of the supplied type must be concrete.
--
-- We assume the provided type is already at the kind-level
-- (this only matters for error messages).
unifyConcrete :: FastString -> ConcreteTvOrigin -> TcType -> TcM TcCoercionN
unifyConcrete occ_fs conc_orig ty
  = do { (co, cts) <- makeTypeConcrete occ_fs conc_orig ty
       ; emitSimples cts
       ; return co }

-- | Ensure that the given kind @ki@ is concrete.
--
-- This is an eager syntactic check, and never defers
-- any work to the constraint solver. However,
-- it may perform unification.
--
-- Invariant: the output type is equal to the input type, up to zonking.
ensureConcrete :: HasDebugCallStack
               => FixedRuntimeRepOrigin
               -> TcKind
               -> TcM TcKind
ensureConcrete frr_orig ki
  = do { (co, cts) <- makeTypeConcrete (fsLit "cx") conc_orig ki
       ; let trace_msg = vcat [ text "ty: " <+> ppr ki
                              , text "co:" <+> ppr co ]
       ; if isEmptyBag cts
         then traceTc "ensureConcrete } success" trace_msg
         else do { traceTc "ensureConcrete } failure" trace_msg
                 ; loc <- getCtLocM (FRROrigin frr_orig) (Just KindLevel)
                 ; emitNotConcreteError $
                     NCE_FRR
                       { nce_loc = loc
                       , nce_frr_origin = frr_orig }
             }
       ; return $ coercionRKind co }
  where
    conc_orig :: ConcreteTvOrigin
    conc_orig = ConcreteFRR frr_orig

{-***********************************************************************
%*                                                                      *
                   Concrete type variables of Ids
%*                                                                      *
%**********************************************************************-}

-- | Which type variables of this 'Id' must be concrete when instantiated?
--
-- See Note [Representation-polymorphism checking built-ins]
idConcreteTvs :: TcId -> ConcreteTyVars
idConcreteTvs id

  -- HACK for unsafeCoerce#: because of the way it is not quite wired in,
  -- as described in Note [Wiring in unsafeCoerce#] in GHC.HsToCore, we don't
  -- have access to the correct IdDetails in the typechecker (as we only patch
  -- in the correct information in the desugarer).
  -- So, for the time being, we manually inspect the type of the original,
  -- unpatched Id to retrieve which of its outer forall-d tyvars should be concrete.
  | idName id == unsafeCoercePrimName
  , (a_rep:_b_rep:a:_b:_, _) <- tcSplitForAllTyVars $ idType id
  -- NB: only check the argument representation, not the result representation.
  -- This is because the following is OK:
  --
  --   unsafeCoerceWordRep :: forall {r2} (b :: TYPE r2). Word# -> b
  --   unsafeCoerceWordRep = unsafeCoerce#
  = mkNameEnv
    [(tyVarName a_rep, ConcreteFRR $ FixedRuntimeRepOrigin (mkTyVarTy a)
                                   $ FRRRepPolyId unsafeCoercePrimName RepPolyFunction
                                   $ Argument 1 Top)]

  | otherwise
  = idDetailsConcreteTvs $ idDetails id
