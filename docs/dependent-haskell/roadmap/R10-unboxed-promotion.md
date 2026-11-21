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

Accepted proposal #265 allows boxed unlifted datatypes. Accepted #547
excludes written constructor contexts from promotion. Its review preserves
the promotion of GADTs with indexed result types.
[#265](https://ghc-proposals.readthedocs.io/en/latest/proposals/0265-unlifted-datatypes.html),
[#547 review](https://github.com/ghc-proposals/ghc-proposals/pull/547#discussion_r1012596259).

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

A pattern match on `HRefl` can still bind a mixed-kind coercion.
GADT constructor workers, boxed source constraints, and primitive Core
coercions have separate representations. The review of proposal #547
discusses the distinction for promotion.
[Proposal #547 review](https://github.com/ghc-proposals/ghc-proposals/pull/547#discussion_r1069601568),
[System D, §6.1](https://www.seas.upenn.edu/~sweirich/papers/systemd-submission.pdf#page=19),
and [Agda equality](https://agda.readthedocs.io/en/latest/language/built-ins.html#equality).
