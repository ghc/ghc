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
- [x] Extend the impact study to additional releases, including
  a package that relies on an implicit CUSK and a Haskell98 package.
- [x] Test the transition warning on explicit and inherited uses.
  Include a declaration with a CUSK whose client needs no migration.
- [x] Verify each suggested signature on package sources. Explain
  kinds with inaccessible names without suggesting invalid source.
- [x] Recheck the selected migration on the normal compiler and
  document the effect on callers.
- [ ] Decide when to change the legacy editions and how to retire the
  explicit opt-in. Verify the selected transition on the normal compiler.

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

### A published library that relies on implicit CUSKs

Stock GHC 9.14.1 builds `first-class-families-0.8.2.0` with the
package's Haskell2010 settings. Adding `-XNoCUSKs` rejects
`TyEqImpl` in `Fcf.Utils` with GHC-17370. Giving it a standalone
kind signature exposes a second failure in `Case_`, which recurses
at another kind. Enabling `StandaloneKindSignatures` and adding the
following two signatures restores the library build with CUSKs off.

```haskell
type TyEqImpl :: forall k l. k -> l -> Bool
type Case_    :: forall j k. [Match j k] -> j -> k
```

The signatures match the original interface kinds. The same
interface-only client proved the exported equality and case examples
on the original and migrated builds. Both runs printed `True`.
The fork already rejects `Pure` in `Fcf.Combinators` with GHC-83865
on the original sources under both default configurations. The
successful migration result above is for stock GHC.
[Published first-class-families source archive](https://hackage-content.haskell.org/package/first-class-families-0.8.2.0.tar.gz).

Two published Haskell2010 packages depend on this library in sequence.
With `-XNoCUSKs`, each failed when its dependencies first built.
Job 941 built the complete migrated chain on stock GHC 9.14.1.
Each client then compiled with the provider source hidden. Each linker
map identified the archive from the corresponding build.

| Haskell2010 release | Standalone signatures added | Printed kinds compared in the edited modules | Client output before and after |
| --- | ---: | ---: | --- |
| `first-class-families-0.8.2.0` | 2 | 16 | `True` |
| `kind-apply-0.4.0.1` | 10 | 43 | `Just (Right True)` |
| `kind-generics-0.5.0.0` | 2 | 25 | `(Right True,Just 3)` |

All 84 printed kinds matched across the six modules, including which
binders callers can specify. The final client also round-tripped an
`Either Int Bool` through `GenericK`. The ten signatures in
`kind-apply` suffice to restore its build. The minimum number is open.
These are dependent packages from one release chain. A prevalence
estimate would require an independent sample of Haskell2010 packages.
[kind-apply release source](https://hackage-content.haskell.org/package/kind-apply-0.4.0.1.tar.gz)
and [kind-generics release source](https://hackage-content.haskell.org/package/kind-generics-0.5.0.0.tar.gz).

The Haskell98 package `tagged-0.9` also uses `PolyKinds`. It built
unchanged with both fork compilers. A client instantiated tags at
`Bool` and at `Type -> Type` and printed `(3,'x')` on both.
[Published tagged release](https://hackage.haskell.org/package/tagged-0.9).

### The release chain on the normal fork

Jobs 1046, 1050 and 1053 repeated the three-package migration on the
normal Stage 2 fork at `5833e38cbd`. The two sides use identical
matchability changes and unchanged Haskell2010 Cabal files. The
baseline inherits CUSKs. The migration adds the same 2, 10 and 2
standalone kind signatures and builds with `-XNoCUSKs`.

The matchability changes are required even with CUSKs enabled.
`first-class-families` uses the R23 provider migration, including the
`Endo` matchability annotations and the namespace correction.
`kind-apply` adds `%M` to nested arrows in `(:&&:)`, `VZ`, `VS`,
`(:@:)` and `ForAll`. In the minimal `HeadLoT` case, the unannotated
promoted `(:&&:)` carries an unmatchable arrow. The family parameter
requires a matchable arrow. An explicit `%M` passes with either CUSK
setting. `kind-generics` needs no matchability edit.
[R23 first-class-families migration](R23-unsaturated-type-families.md#released-first-class-families-migration).

After those prerequisites, disabling CUSKs reproduces the failures in
each release as its dependencies are migrated. The 2, 10 and 2
signatures restore the chain. All 215 printed kinds match across its
23 interfaces, including 163 locally defined exported constructors.
All five unchanged source-hidden clients pass, including the two
`Endo` uses and `FoldrDefault_`. The upstream `fcf-test` passes on
both builds. The comparison measures the CUSK switch on an already
matchability-compatible provider.
[Exact package sources, patches and build commands](../examples/r27-package-migration/README.md).

### Which declarations would a migration warning identify?

Job 942 built the unmodified release chain on stock GHC 9.14.1 with
each existing missing-signature warning. Neither warning reported any
of the 14 declarations in the migration. Both warned on `Tagged`,
which retained its polymorphic kind without a signature. The kind
checker uses a different path for declarations that already have a
CUSK. Job 950 checked the warning on the extension flag in MR !1602.
It reported all 14 explicit switches across seven modules and none
of the seven modules that inherited the edition default.

The fork provides an opt-in declaration warning. After the
[R23 package migration](R23-unsaturated-type-families.md#released-first-class-families-migration),
the prototype reported both affected declarations in
`first-class-families` and 13 additional declarations. Six printed
kinds contained package-qualified internal matchability names.
Check that the compiler can suggest valid source, or explain the
inferred kind without offering an invalid edit. A warning here can
identify a declaration for inspection. Rebuild with `-XNoCUSKs` and
check the exported kind and client to establish the impact.
[GHC's missing-signature diagnostics](../../users_guide/using-warnings.rst)
and [the upstream flag change](https://github.com/ghc/ghc/commit/c8c660f678bd77fb9d0a855753a165c5a5d1f8e1).

Job 1009 checked a migration one declaration at a time on the
published fork. Adding the `TyEqImpl` signature and enabling
`StandaloneKindSignatures` in `Fcf.Utils` disabled the inherited CUSK
on `Case_`. The compiler reported GHC-25897 at the recursive use in
`Case_`. Two builds passed with the same source-hidden client. In
the first, the module temporarily enabled `CUSKs` after
`StandaloneKindSignatures` and added only the `TyEqImpl` signature.
In the second, it added both required signatures and left CUSKs off.
Thus the advice to enable the extension affects the whole module,
even when the diagnostic points at one declaration.

The opt-in `-Wcusks` warning uses GHC-57439. Job 1041 checked the
package on the integrated normal compiler at `fd19c8fe5f`. On the
R23-compatible package, the normal stage 2 compiler gave five
suggested edits and ten explanations. All five suggested signatures
together compiled under `-XNoCUSKs`, preserved the corresponding
interface declarations and passed the source-hidden client. Two of
the five edits sufficed to restore the build. The warning also reported
the current kind for a zero-argument open family whose arity could
change after adding a signature. The default build was quiet.
The wording reminds the user about the module-wide effect when it
suggests enabling `StandaloneKindSignatures`. Language editions and
their defaults are unchanged on the published branch.

### An explicit opt-out during the transition

The private comparator applies the two driver changes from !1602 to
`fd19c8fe5f`. With Haskell98 or Haskell2010, a module that relied on
the edition to enable CUSKs must migrate or explicitly opt back in.
Jobs 1071 and 1073 checked the three-package chain, source-hidden
clients, edition selection, and the order of the extension flags.
In the 317-test upstream comparison, only `T20916` gained an extra
golden mismatch for the flag deprecation.

Job 1079 found that the proposed warning for `-XNoCUSKs` suggests
an edit that changes behavior. After `-XCUSKs`, `-XNoCUSKs` turns
CUSKs off. Replacing it with `-XNoStandaloneKindSignatures` leaves
CUSKs on. If standalone signatures are enabled, the same replacement
can disable syntax that the module uses. Keep both controls when
revising the advice. The published declaration warning is independent
of this driver change.
[Upstream driver change](https://github.com/ghc/ghc/commit/c8c660f678bd77fb9d0a855753a165c5a5d1f8e1).
