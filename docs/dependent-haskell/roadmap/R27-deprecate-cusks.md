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

- [ ] Compare legacy declarations with CUSKs enabled, disabled and
  replaced by standalone signatures. Compile an importing client
  that uses the polymorphic kind, even when the provider still builds.
- [ ] Compare unchanged programs and diagnostics with the proposed
  defaults. Record the language edition, explicit flags, test set and
  revision. Separate failures already present on the fork.
- [ ] Test the transition warning on explicit and inherited uses.
  Include a declaration with a CUSK whose client needs no migration.
- [ ] Recheck the selected migration on the normal compiler and
  document the effect on callers before changing a language default.
