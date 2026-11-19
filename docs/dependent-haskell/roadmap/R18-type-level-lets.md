# R18. Type-level let-bindings

Serokell's example names the result of a type-level computation and
uses it twice. The card lists dependent Core (R05) and open evaluation
(R16) as prerequisites. [Upstream roadmap](https://ghc.serokell.io/dh#type-level-let-bindings).

```haskell
type F a = let b = G a in H b b
```

With `ExperimentalDependentTypes`, GHC checks the definition of `b`
in the enclosing scope. The two uses in `H b b` receive the same
checked type and share its kind metavariables. In `let b = b in …`,
the right-hand `b` refers to an outer binding. An unused definition
still has to kind-check. Agda uses the same scope rule for local
abbreviations and describes their meaning by substitution. [Agda
let-expressions](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-expressions).

In the example, `G Bool` reduces to `'True` and `H 'True 'True`
reduces to `Ordering`. An ordinary client proves `F Bool :~: Ordering`
from the provider's interface. The client needs no local `let` in
its own source. [Provider](../examples/open-evaluation/TypeLet.hs)
and [interface and scope checks](../../../testsuite/tests/dependent/should_run/NativeTypeLet.sh).

A retained vector result can use `let k = 1 + n in Vec a k` when
`n` comes from an `IORef`. At `n = 2`, the example returns three
characters. Taking the first character also succeeds with an exceptional
tail length. [Runtime example](../Showcase.hs).

The source form supports one variable per binding and nested lets.
Binding groups, local patterns, and combining lets with general
type-level lambdas and cases remain open. Raw Template Haskell type
quotation reports an error at the let.
