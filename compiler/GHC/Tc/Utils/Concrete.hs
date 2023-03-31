{-# LANGUAGE MultiWayIf #-}

-- | Checking for representation-polymorphism using the Concrete mechanism.
--
-- This module contains the logic for enforcing the representation-polymorphism
-- invariants by way of emitting constraints.
module GHC.Tc.Utils.Concrete
  ( -- * Ensuring that a type has a fixed runtime representation
    hasFixedRuntimeRep
  , hasFixedRuntimeRep_syntactic
  )
 where

import GHC.Prelude

import GHC.Builtin.Types       ( liftedTypeKindTyCon, unliftedTypeKindTyCon )

import GHC.Core.Coercion       ( coToMCo, mkCastTyMCo
                               , mkGReflRightMCo, mkNomReflCo )
import GHC.Core.TyCo.Rep       ( Type(..), MCoercion(..) )
import GHC.Core.TyCon          ( isConcreteTyCon )
import GHC.Core.Type           ( isConcreteType, typeKind, tyVarKind, coreView
                               , mkTyVarTy, mkTyConApp, mkFunTy, mkAppTy )

import GHC.Tc.Types            ( TcM, ThStage(..), PendingStuff(..) )
import GHC.Tc.Types.Constraint ( NotConcreteError(..), NotConcreteReason(..) )
import GHC.Tc.Types.Evidence   ( Role(..), TcCoercionN, TcMCoercionN )
import GHC.Tc.Types.Origin     ( CtOrigin(..), FixedRuntimeRepContext, FixedRuntimeRepOrigin(..) )
import GHC.Tc.Utils.Monad      ( emitNotConcreteError, setTcLevel, getCtLocM, getStage, traceTc )
import GHC.Tc.Utils.TcType     ( TcType, TcKind, TcTypeFRR
                               , MetaInfo(..), ConcreteTvOrigin(..)
                               , isMetaTyVar, metaTyVarInfo, tcTyVarLevel )
import GHC.Tc.Utils.TcMType    ( newConcreteTyVar, isFilledMetaTyVar_maybe, writeMetaTyVar
                               , emitWantedEq )

import GHC.Types.Basic         ( TypeOrKind(..) )
import GHC.Types.Name          ( getOccName )
import GHC.Types.Name.Occurrence( occNameFS )
import GHC.Utils.Misc          ( HasDebugCallStack )
import GHC.Utils.Outputable
import GHC.Data.FastString     ( fsLit )


import Control.Monad      ( void )
import Data.Functor       ( ($>) )
import Data.List.NonEmpty ( NonEmpty((:|)) )

import Control.Monad.Trans.Class      ( lift )
import Control.Monad.Trans.Writer.CPS ( WriterT, runWriterT, tell )

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
hasFixedRuntimeRep frr_ctxt ty = checkFRR_with unifyConcrete frr_ctxt ty

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

-- | Ensure that the given type @ty@ can unify with a concrete type,
-- in the sense of Note [Concrete types].
--
-- Returns a coercion @co :: ty ~# conc_ty@, where @conc_ty@ is
-- concrete.
--
-- If the type is already syntactically concrete, this
-- immediately returns a reflexive coercion. Otherwise,
-- it creates a new concrete metavariable @concrete_tv@
-- and emits an equality constraint @ty ~# concrete_tv@,
-- to be handled by the constraint solver.
--
-- Invariant: the kind of the supplied type must be concrete.
--
-- We assume the provided type is already at the kind-level
-- (this only matters for error messages).
unifyConcrete :: HasDebugCallStack
              => FixedRuntimeRepOrigin -> TcType -> TcM TcMCoercionN
unifyConcrete frr_orig ty
  = do { (ty, errs) <- makeTypeConcrete (ConcreteFRR frr_orig) ty
       ; case errs of
           -- We were able to make the type fully concrete.
         { [] -> return MRefl
           -- The type could not be made concrete; perhaps it contains
           -- a skolem type variable, a type family application, ...
           --
           -- Create a new ConcreteTv metavariable @concrete_tv@
           -- and unify @ty ~# concrete_tv@.
         ; _  ->
    do { conc_tv <- newConcreteTyVar (ConcreteFRR frr_orig) (fsLit "cx") ki
           -- NB: newConcreteTyVar asserts that 'ki' is concrete.
       ; coToMCo <$> emitWantedEq orig KindLevel Nominal ty (mkTyVarTy conc_tv) } } }
  where
    ki :: TcKind
    ki = typeKind ty
    orig :: CtOrigin
    orig = FRROrigin frr_orig

-- | Ensure that the given type is concrete.
--
-- This is an eager syntactic check, and never defers
-- any work to the constraint solver.
--
-- Invariant: the kind of the supplied type must be concrete.
-- Invariant: the output type is equal to the input type,
--            up to zonking.
--
-- We assume the provided type is already at the kind-level
-- (this only matters for error messages).
ensureConcrete :: HasDebugCallStack
               => FixedRuntimeRepOrigin
               -> TcType
               -> TcM TcType
ensureConcrete frr_orig ty
  = do { (ty', errs) <- makeTypeConcrete conc_orig ty
       ; case errs of
          { err:errs ->
              do { traceTc "ensureConcrete } failure" $
                     vcat [ text "ty:" <+> ppr ty
                          , text "ty':" <+> ppr ty' ]
                 ; loc <- getCtLocM (FRROrigin frr_orig) (Just KindLevel)
                 ; emitNotConcreteError $
                     NCE_FRR
                       { nce_loc = loc
                       , nce_frr_origin = frr_orig
                       , nce_reasons = err :| errs }
                 }
          ; [] ->
              traceTc "ensureConcrete } success" $
                vcat [ text "ty: " <+> ppr ty
                     , text "ty':" <+> ppr ty' ] }
        ; return ty' }
  where
    conc_orig :: ConcreteTvOrigin
    conc_orig = ConcreteFRR frr_orig

{-***********************************************************************
%*                                                                      *
                    Making a type concrete
%*                                                                      *
%************************************************************************

Note [Unifying concrete metavariables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unifying concrete metavariables (as defined in Note [ConcreteTv]) is not
an all-or-nothing affair as it is for other sorts of metavariables.

Consider the following unification problem in which all metavariables
are unfilled (and ignoring any TcLevel considerations):

  alpha[conc] ~# TYPE (TupleRep '[ beta[conc], IntRep, gamma[tau] ])

We can't immediately unify `alpha` with the RHS, because the RHS is not
a concrete type (in the sense of Note [Concrete types]). Instead, we
proceed as follows:

  - create a fresh concrete metavariable variable `gamma'[conc]`,
  - write gamma[tau] := gamma'[conc],
  - write alpha[conc] := TYPE (TupleRep '[ beta[conc], IntRep, gamma'[conc] ]).

Thus, in general, to unify `alpha[conc] ~# rhs`, we first try to turn
`rhs` into a concrete type (see the 'makeTypeConcrete' function).
If this succeeds, resulting in a concrete type `rhs'`, we simply fill
`alpha[conc] := rhs'`. If it fails, then syntactic unification fails.

Example 1:

    alpha[conc] ~# TYPE (TupleRep '[ beta[conc], IntRep, gamma[tau] ])

  We proceed by filling metavariables:

    gamma[tau] := gamma[conc]
    alpha[conc] := TYPE (TupleRep '[ beta[conc], IntRep, gamma[conc] ])

  This successfully unifies alpha.

Example 2:

  For a type family `F :: Type -> Type`:

    delta[conc] ~# TYPE (SumRep '[ zeta[tau], a[sk], F omega[tau] ])

  We write zeta[tau] := zeta[conc], and then fail, providing the following
  two reasons:

    - `a[sk]` is not a concrete type variable, so the overall type
      cannot be concrete
    - `F` is not a concrete type constructor, in the sense of
       Note [Concrete types]. So we keep it as is; in particular,
       we /should not/ try to make its argument `omega[tau]` into
       a ConcreteTv.

  Note that making zeta concrete allows us to propagate information.
  For example, after more typechecking, we might try to unify
  `zeta ~# rr[sk]`. If we made zeta a ConcreteTv, we will report
  this unsolved equality using the 'ConcreteTvOrigin' stored in zeta[conc].
  This allows us to report ALL the problems in a representation-polymorphism
  check (instead of only a non-empty subset).
-}

-- | Try to turn the provided type into a concrete type, by ensuring
-- unfilled metavariables are appropriately marked as concrete.
--
-- Returns a zonked type which is "as concrete as possible", and
-- a list of problems encountered when trying to make it concrete.
--
-- INVARIANT: the returned type is equal to the input type, up to zonking.
-- INVARIANT: if this function returns an empty list of 'NotConcreteReasons',
-- then the returned type is concrete, in the sense of Note [Concrete types].
makeTypeConcrete :: ConcreteTvOrigin -> TcType -> TcM (TcType, [NotConcreteReason])
-- TODO: it could be worthwhile to return enough information to continue solving.
-- Consider unifying `alpha[conc] ~# TupleRep '[ beta[tau], F Int ]` for
-- a type family 'F'.
-- This function will concretise `beta[tau] := beta[conc]` and return
-- that `TupleRep '[ beta[conc], F Int ]` is not concrete because of the
-- type family application `F Int`. But we could decompose by setting
-- alpha := TupleRep '[ beta, gamma[conc] ] and emitting `[W] gamma[conc] ~ F Int`.
makeTypeConcrete conc_orig ty =
  do { res@(ty', _) <- runWriterT $ go ty
     ; traceTc "makeTypeConcrete" $
        vcat [ text "ty:" <+> ppr ty
             , text "ty':" <+> ppr ty' ]
     ; return res }
  where
    go :: TcType -> WriterT [NotConcreteReason] TcM TcType
    go ty
      | Just ty <- coreView ty
      = go ty
      | isConcreteType ty
      = pure ty
    go ty@(TyVarTy tv) -- not a ConcreteTv (already handled above)
      = do { mb_filled <- lift $ isFilledMetaTyVar_maybe tv
           ; case mb_filled of
           { Just ty -> go ty
           ; Nothing
               | isMetaTyVar tv
               , TauTv <- metaTyVarInfo tv
               -> -- Change the MetaInfo to ConcreteTv, but retain the TcLevel
               do { kind <- go (tyVarKind tv)
                  ; let occ_fs = occNameFS (getOccName tv)
                        -- occ_fs: preserve the occurrence name of the original tyvar
                        -- This helps in error messages
                  ; lift $
                    do { conc_tv <- setTcLevel (tcTyVarLevel tv) $
                                    newConcreteTyVar conc_orig occ_fs kind
                       ; let conc_ty = mkTyVarTy conc_tv
                       ; writeMetaTyVar tv conc_ty
                       ; return conc_ty } }
               | otherwise
               -- Don't attempt to make other type variables concrete
               -- (e.g. SkolemTv, TyVarTv, CycleBreakerTv, RuntimeUnkTv).
               -> bale_out ty (NonConcretisableTyVar tv) } }
    go ty@(TyConApp tc tys)
      | isConcreteTyCon tc
      = mkTyConApp tc <$> mapM go tys
      | otherwise
      = bale_out ty (NonConcreteTyCon tc tys)
    go (FunTy af w ty1 ty2)
      = do { w <- go w
           ; ty1 <- go ty1
           ; ty2 <- go ty2
           ; return $ mkFunTy af w ty1 ty2 }
    go (AppTy ty1 ty2)
      = do { ty1 <- go ty1
           ; ty2 <- go ty2
           ; return $ mkAppTy ty1 ty2 }
    go ty@(LitTy {})
      = return ty
    go ty@(CastTy cast_ty kco)
      = bale_out ty (ContainsCast cast_ty kco)
    go ty@(ForAllTy tcv body)
      = bale_out ty (ContainsForall tcv body)
    go ty@(CoercionTy co)
      = bale_out ty (ContainsCoercionTy co)

    bale_out :: TcType -> NotConcreteReason -> WriterT [NotConcreteReason] TcM TcType
    bale_out ty reason = do { tell [reason]; return ty }
