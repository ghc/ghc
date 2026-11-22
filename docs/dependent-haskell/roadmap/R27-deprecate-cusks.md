# R27. Deprecate CUSKs

A complete user-supplied kind (CUSK) is a kind that GHC obtains from
a declaration's header before it checks the body. A recursive use in
the body can then instantiate the type constructor at another kind.
A standalone kind signature states the kind separately and supports
the same use. Serokell lists standalone kind signatures (R26) and
visible dependent quantification (R29) as prerequisites.
[Serokell card](https://ghc.serokell.io/dh#deprecate-cusks)
and [accepted proposal #54](https://ghc-proposals.readthedocs.io/en/latest/proposals/0054-kind-signatures.html).

The proposal review gives a reason for the second prerequisite. The
constructor below uses `Foo` at both a function kind and its domain.
The first argument of `Foo` gives the kind of its second argument.
The replacement signature needs the required binder `forall k ->`.

```haskell
{-# LANGUAGE ExplicitForAll, GADTs, PolyKinds, StandaloneKindSignatures #-}
import Data.Kind (Type)

type Foo :: forall k -> k -> Type
data Foo k a where
    MkFoo :: Foo (k1 -> k2) f -> Foo k1 a -> Foo k2 (f a)
```

[Ryan Scott's review example](https://github.com/ghc-proposals/ghc-proposals/pull/54#issuecomment-329789568).

## Prior implementation and migration checks

GHC introduced `-XCUSKs` in `a5fdd18518`. The current compiler
includes it in Haskell98 and Haskell2010. GHC2021 and GHC2024 omit
it. Enabling `StandaloneKindSignatures` turns CUSKs off unless a later
flag enables them again.
[Language editions](../../../compiler/GHC/Driver/DynFlags.hs)
and [extension implications](../../../compiler/GHC/Driver/Flags.hs).

Vladislav Zavialov's open MR !1602 removes CUSKs from the two older
editions and deprecates the explicit extension flag. Its compiler
change is three lines. A warning on the flag can identify a module
that explicitly requests CUSKs. A module that inherits CUSKs from its
language edition has no such pragma to report. Test the published
patch against those modules before choosing a transition warning.
[Upstream patch at `c8c660f678`](https://github.com/ghc/ghc/commit/c8c660f678bd77fb9d0a855753a165c5a5d1f8e1)
and [GHC MR !1602](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1602).

- [x] Compare legacy declarations with CUSKs enabled, disabled and
  replaced by standalone signatures. Compile an importing client
  that uses the polymorphic kind, even when the provider still builds.
- [x] Compare the unchanged upstream polykinds and standalone-signature
  suites with the proposed defaults. Separate pre-existing failures.
- [x] Compare two Haskell2010 packages and run a client against each
  build. Record their revisions and verify the linked libraries.
- [ ] Extend the impact study to additional releases, including
  a package that relies on an implicit CUSK and a Haskell98 package.
- [ ] Test the transition warning on explicit and inherited uses.
  Include a declaration with a CUSK whose client needs no migration.
- [ ] Recheck the selected migration on the normal compiler and
  document the effect on callers before changing a language default.

### Results for four declarations

Jobs 905 and 911 compared stock GHC 9.14.1 with the fork compiler at
`4f2fc1146f`. Each row used the Haskell2010 edition and three forms
of the declaration. An importing client saw only the provider's
compiled interface. The two compilers gave the following results.

| Declaration | CUSKs | NoCUSKs | Standalone signature with NoCUSKs |
| --- | --- | --- | --- |
| Recursive `T` from the User's Guide, which uses `T Maybe` | Client compiles | Provider rejects, GHC-25897 | Client compiles |
| `Test Int Bool Double` from upstream `T11811` | Client compiles | Provider rejects, GHC-25897 | Client compiles |
| `Foo` from the proposal review above | Client compiles | Provider rejects, GHC-25897 | Client compiles |
| Nonrecursive `data T (a :: k) = MkT` | Client compiles | Client compiles | Client compiles |

On each compiler, the standalone signature preserved the complete kind
printed from the legacy interface for all four declarations. The
nonrecursive control also preserved its kind with CUSKs disabled and
no standalone signature. Thus a warning on every syntactic CUSK would
include at least one declaration in this set that needs no source change.
[User's Guide example](../../users_guide/exts/poly_kinds.rst)
and [upstream T11811](../../../testsuite/tests/typecheck/should_compile/T11811.hs).

To repeat the `T11811` client check with the compiler on your path,
run the [migration example](../examples/check-cusk-migration.sh) from
the repository root.

```sh
bash docs/dependent-haskell/examples/check-cusk-migration.sh "$(command -v ghc)" "$(ghc --print-libdir)"
```

### Unchanged upstream tests

Jobs 911 and 914 compared GHC's `polykinds` and `saks` tests at
`19ff36f2aede`. The comparison verified all 515 input blobs against
Git and ran 317 normal-way registrations on each compiler. The released
fork matched 298 expected outcomes. The compiler with the three-line
upstream patch matched 297. There were no missing libraries or test
framework failures in the final runs.

Only `T20916` changed outcome. Both compilers rejected it with
GHC-17370. The proposed change also emitted GHC-53692 on its explicit
`CUSKs` pragma, which changed the expected diagnostic. The 19 failures
already present on the released compiler had the same diagnostic bodies
after normalizing paths, diff timestamps and the driver name. One of
these, `T19092`, reports an ill-kinded coercion during Core Lint.

This suite uses GHC's default edition unless an input selects another.
It does not measure the incidence of implicit CUSK use in packages
that select Haskell98 or Haskell2010.
[Upstream polykinds suite](https://github.com/ghc/ghc/tree/19ff36f2aede0ca2e554016ff22c71e73aa9803b/testsuite/tests/polykinds)
and [upstream standalone-signature suite](https://github.com/ghc/ghc/tree/19ff36f2aede0ca2e554016ff22c71e73aa9803b/testsuite/tests/saks).

### Haskell2010 package clients

Job 919 built the fork's pinned `binary` (`4cc3fe29c8`) and `deepseq`
(`9e6fc27711`) libraries offline with their original Cabal files.
Both libraries select Haskell2010. The same sources built on the
released and proposed compilers. The binary client round-tripped
`[1,4,9]`. The deepseq client forced a finite list and observed the
exception in a list with an undefined element. Job 922 checked each
linker map. All four runs used the library archive built for that
compiler. These two packages provide controls. The three recursive
declarations above demonstrate code that does need migration.
[Binary library](../../../libraries/binary/binary.cabal)
and [deepseq library](../../../libraries/deepseq/deepseq.cabal).
