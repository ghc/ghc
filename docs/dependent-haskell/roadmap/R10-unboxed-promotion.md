# R10. Promote unboxed types

Serokell links R10 to GHC #14180 and lists homogeneous equality as a
prerequisite. The issue asks whether a type-family pattern can bind the
`Int#` field of the promoted `I#` constructor. The datatype, the constructor
head, and a type-level field value exercise different checks.
[Serokell R10](https://ghc.serokell.io/dh#promote-unboxed-types),
[GHC #14180](https://gitlab.haskell.org/ghc/ghc/-/work_items/14180).

## Source boundary

Use `ghc -XGHC2024 -XMagicHash -fforce-recomp -fno-code`. Put each
rejected declaration in a separate module. Enable `TypeFamilies`
for the family patterns and `UnliftedDatatypes` for `Token`.

```haskell
import Data.Proxy (Proxy (Proxy))
import GHC.Exts (Int#, RuntimeRep (IntRep), TYPE)

type PrimType = Int#
data AtIntRep (a :: TYPE 'IntRep) = AtIntRep
raw :: AtIntRep Int#
raw = AtIntRep

data Packet = Empty | Packed Int#
packet :: Proxy ('Empty :: Packet)
packet = Proxy
type PackedHead = 'Packed
```

The published compiler at `89aa40ec5689` and stock GHC 9.14.1 accept all
of these declarations. They also accept `type RawHead = 'Raw` after
`newtype Raw :: TYPE 'IntRep where Raw :: Int# -> Raw` with
`UnliftedNewtypes`. The `Int#` type and a kind of the form `TYPE 'IntRep`
are available. The promoted constructor head has no supplied field value.

The issue's family is a separate module.

```haskell
import GHC.Exts (Int (I#))

type family MatchInt (i :: Int) :: () where
    MatchInt ('I# x) = '()
```

| Separate probe | Fork | Stock 9.14.1 |
| --- | --- | --- |
| `data RawIndex (i :: Int#) = RawIndex` | GHC-83865 | GHC-83865 |
| `type PromotedToken = 'Token`, with `data Token :: UnliftedType where Token :: Token` | GHC-18872 | GHC-18872 |
| `MatchInt ('I# x) = '()` in a closed family over `Int` | GHC-18872 | GHC-18872 |
| `MatchInt ('I# _) = '()` | GHC-18872 | GHC-18872 |
| `MatchInt ('I# (x :: Int#)) = '()` | GHC-83865 | GHC-83865 |
| `type Three = 'Packed 3#` | GHC-51819 | GHC-58481 |

The kind signature checker requires the written kind to have kind `Type`.
The family pattern binder and wildcard also introduce a kind variable of
kind `Type`. Core Lint checks the same invariant for a type-variable binder.
A local change to the wildcard would have to account for that invariant.
The fork parses `3#` in a type and rejects it in `tc_hs_lit_ty`. Stock
rejects it in the parser. Support for that literal is a separate part of
R07. [Kind checking](../../../compiler/GHC/Tc/Gen/HsType.hs),
[Core binder checking](../../../compiler/GHC/Core/Lint.hs).

### The primitive field and the family axiom

On the public compiler at `7836668a00`, the named binder and wildcard
still give GHC-18872. The annotated binder gives GHC-83865.
`ExperimentalDependentTypes` gives the same diagnostic in each case.
The original issue reports that GHC 8.3 accepted the wildcard.
[GHC #14180 report](https://gitlab.haskell.org/api/v4/projects/ghc%2Fghc/issues/14180).

`'I#` expects an index of kind `Int#`. A term literal `3#` has that type.
The type literal `3` has kind `Natural`. Core Lint rejects its use as an
`'I#` argument. Native index readback rejects the raw literal, the boxed
expression `I# 3#`, and a scoped `Int#` term variable. A raw variable
inserted directly as a runtime index also fails Core Lint.
[Core type literals](../../../compiler/GHC/Core/TyCo/Rep.hs),
[Core index readback](../../../compiler/GHC/Core.hs).

A Core API probe builds the family axiom directly. With a symbolic `x`
of kind `Int#` in the constructor field, the reducer returns `'()` for
`MatchInt ('I# x)` and leaves `MatchInt i` stuck at an unknown `i :: Int`.
Core Lint rejects the axiom binder `x :: Int# :: TYPE 'IntRep`. The
control axiom for `'Just x`, with `x :: Char`, passes Lint and reduces
with a concrete character literal. A source test with `3#` would need
both a type-level `Int#` literal and a rule for binding its kind inside
the axiom. The ordinary `forall` rule requires the binder's kind to
have kind `Type`.
[Core kinding rules](../../../compiler/GHC/Core/Type.hs),
[Core axiom checking](../../../compiler/GHC/Core/Lint.hs).

Eisenberg's thesis separates the sorts selected by `TYPE` and leaves
levity polymorphism out of the formal calculus in section 5.13.4.
Agda gives `Word64` its own primitive type of sort `Set` and provides
conversions to and from `Nat`. Idris 2 can compile a Peano natural as an
`Integer` while keeping its constructor patterns in source. These
examples separate the representation choice from the type of an index.
[Eisenberg, §5.13.4](https://arxiv.org/pdf/1610.07978#page=153),
[Agda machine words](https://agda.readthedocs.io/en/v2.8.0/language/built-ins.html#machine-words),
[Idris 2 natural numbers](https://idris2.readthedocs.io/en/latest/reference/builtins.html#natural-numbers).

Accepted proposal #265 allows boxed unlifted datatypes. Accepted #547
excludes written constructor contexts from promotion. Its review preserves
the promotion of GADTs with indexed result types.
[#265](https://ghc-proposals.readthedocs.io/en/latest/proposals/0265-unlifted-datatypes.html),
[#547 review](https://github.com/ghc-proposals/ghc-proposals/pull/547#discussion_r1012596259).

## Private `Int#` prototype

With `ExperimentalDependentTypes`, the prototype accepts the issue's
`MatchInt ('I# x) = '()`. The same rule accepts `'I# _` and
`'I# (x :: Int#)`. Each spelling reduces at `'I# 3#`. The axiom binds
the raw variable only in the immediate `'I#` field of a closed family
pattern. Its right-hand side cannot refer to that variable.

```haskell
import Data.Type.Equality ((:~:)(Refl))

known :: MatchInt ('I# 3#) :~: '()
known = Refl

-- This separate declaration fails. Use AllowAmbiguousTypes for the check.
unknown :: forall (i :: Int). MatchInt i :~: '()
unknown = Refl
```

A client with the extension disabled imported a provider alias for
`'I# 3#` and proved the same equality using only the provider interface.
An older compiler rejected that interface at its version check. An
ordinary interface from the prototype remained readable by the older
compiler. The binder alone also sets the new interface marker.

The type-level literal uses the term parser's signed `Int#` token and
the term literal's wrapping operation on the target platform. On the
64-bit target, `18446744073709551613#` equals `(-3#)`, and
`4294967293#` stays distinct from `(-3#)`. The signed source probe
enabled `NegativeLiterals`. Core Lint checks that the stored literal
fits the target range.
[Primitive lexer](../../../compiler/GHC/Parser/Lexer.x),
[Term literal elaboration](../../../compiler/GHC/HsToCore/Match/Literal.hs),
[Target-width wrapping](../../../compiler/GHC/Types/Literal.hs).

The ordinary `forall` and raw runtime-value readback checks still
reject the controls above. The same Core API probe now checks the axiom
with a concrete `Int#` index and leaves the unknown `Int` application
stuck. The experiment covers only the primitive constructor in #14180.
R10 still includes other raw fields, unboxed constructors, and the
general promotion rules.

## R11 acceptance check

`RawInt` is a small case of the kind-indexed GADT described in the GHC
homogeneous-equality design. The source already compiles on both compilers.
[GHC phase 2a](https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell/phase2#phase-2a-homogeneous-equality).

```haskell
import Data.Kind (Type)
import GHC.Exts (Int#, RuntimeRep (IntRep), TYPE)

type RawEq :: forall r. TYPE r -> Type
data RawEq a where
    RawInt :: RawEq Int#

type role AtRep nominal
data AtRep (a :: TYPE r) = AtRep

general :: forall r (a :: TYPE r). RawEq a -> AtRep a -> AtRep Int#
general RawInt x = x

sameRep :: forall (a :: TYPE 'IntRep). RawEq a -> AtRep a -> AtRep Int#
sameRep RawInt x = x
```

At `-O0` and `-O2` with Core and STG Lint, the fork’s binder audit reports
one mixed-kind binder for `general`, namely
`(a :: TYPE r) ~# (Int# :: TYPE 'IntRep)`. It reports zero for `sameRep`.
The fork’s private v77 worker trial removed this binder. It lifted
`r ~# 'IntRep` through `TYPE` and bound the transported index proof next.
That worker telescope requires a matched compiler and rebuilt interfaces.
The public worker layout still has the mixed binder. The family patterns
above also need kind formation and binding for the `Int#` field.

### Given equality projections

A same-kind Given such as `Premise a ~ Premise b` can relate indices
`a :: k1` and `b :: k2`, where `Premise` is a nominal, kind-polymorphic
constructor. When the solver projects the equality of the indices, it
binds the nominal kind proof first. It then binds the equality of `a`
and `b` transported to `k1`. Function argument projections use the same
rule when the argument types have different runtime representations.
The second proof keeps the role of the original selector. The input
coercion can still contain heterogeneous expressions.

At `-O0` and `-O2` the separate-provider check found zero mixed binders
in four nominal and representational projection cases. The old solver
found four. The provider interfaces were byte-identical. The change uses
the existing coercion syntax and does not change the constructor worker
or the boxed-equality layout.
[Solver](../../../compiler/GHC/Tc/Solver/Equality.hs).

The same transport applies to both projections of a nominal equality
`f a ~ g b` with matchable heads. The head and argument can each have
endpoints at different kinds. The separate-provider check reduces these
two mixed binders to zero at `-O0` and `-O2`. Representational application
equality still cannot justify the nominal argument proof.

### Deferred heterogeneous equalities

With `-fdefer-type-errors`, a call can require evidence for
`(a :: k1) ~~ (b :: k2)`. GHC now binds a nominal proof that `k2` equals
`k1` and a proof that `a` equals `b` transported to `k1`. The second proof
has the role of the original hole. GHC composes the proofs to fill the
hole. Demanding invalid evidence raises the original deferred error.
The [Core regression](../../../testsuite/tests/plugins/DeferredHeterogeneous.hs)
checks the kinds of the generated binders.

Core Lint checks a constructor whose second equality argument uses its
first. Given a nominal proof `ck :: k1 ~# k2`, the next proof can compare
`a |> ck` and `b` at kind `k2`. Constructor application and pattern
matching both substitute the first proof before checking the second.
The [Core test](../../../testsuite/tests/plugins/CoercionForall.hs)
checks both directions and rejects arguments with the wrong kind,
role or scope. The installed constructor worker still has its old type.

A pattern match on `HRefl` can still bind a mixed-kind coercion.
GADT constructor workers, boxed source constraints, and primitive Core
coercions have separate representations. The review of proposal #547
discusses the distinction for promotion.
[Proposal #547 review](https://github.com/ghc-proposals/ghc-proposals/pull/547#discussion_r1069601568),
[System D, §6.1](https://www.seas.upenn.edu/~sweirich/papers/systemd-submission.pdf#page=19),
and [Agda equality](https://agda.readthedocs.io/en/latest/language/built-ins.html#equality).
