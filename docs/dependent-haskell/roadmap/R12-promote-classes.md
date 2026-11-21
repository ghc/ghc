# R12. Promote classes

R12 is open. It depends on R13 (function promotion) and R24 (constrained
type families). M82 and M83 can use checked equations after identifying a
concrete instance method. A type that refers to a method through a caller's
abstract class dictionary remains open.
[Serokell card](https://ghc.serokell.io/dh#promote-classes) and
[local dependencies](../ROADMAP.md#upstream-checklist-and-complete-dependency-index).

## A method in a retained result type

```haskell
class Choose a where
    choose :: a -> Bool

chooseTagged :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
chooseTagged x = tagBool (choose x)
```

`tagBool :: foreach (b :: Bool) -> Tagged b` returns the corresponding
singleton constructor. GHC reports GHC-70566 for the result type.
“Cannot reduce the open call to ‘choose’ in a type.” The explanation is
“The local dependency choose is not an ordinary function binding.”
[Target source](../../../testsuite/tests/dependent/should_run/promoted-class/ChooseTarget.hs).

The associated-family control declares `Chosen a x` and gives a method
the result type `Tagged (Chosen a x)`. The class constraint establishes
the family application's domain. The caller can match the result with
`a` still hidden and `Chosen a x` still stuck. The test compiles the
ordinary client against a stripped interface and checks both Boolean
results from runtime input at O0 and O2.
[Control provider](../../../testsuite/tests/dependent/should_run/promoted-class/ChooseControl.hs),
[ordinary client](../../../testsuite/tests/dependent/should_run/promoted-class/Main.hs),
and [test runner](../../../testsuite/tests/dependent/should_run/NativePromotedClassBoundary.sh).

## Evidence needed

The first acceptance criterion is to typecheck `chooseTagged` while
`a` and its `Choose a` evidence are abstract. The index and the term call
must use the same selector, instantiation, operand and evidence. The
application can stay opaque during type checking. With the constraint
removed, the call must require `Choose a`.

The retained signature checker currently checks the predicates in
`HsQualTy` and builds ordinary constraint arrows with `tcMkPhiTy`.
Those arrows have no evidence binder in the body type. The local
dependency scheduler rejects the selector before it reaches that
checker. Once the class declaration is available, the boundary is
binding or otherwise representing the selected evidence in the type
and in the term body. Core's `TermAppTy` accepts a scoped function
variable at a closed scalar arrow type. Its head cannot represent a
selector applied to an abstract dictionary, and the receiver domain
`a` is open. When checking the term operand, the existing certified
method path requires a closed domain and a selected source target.
Both occurrences need to use the same scoped application.
[Retained signature checker](../../../compiler/GHC/Tc/Gen/HsType.hs#L612),
[local dependency scheduler](../../../compiler/GHC/Tc/Module.hs#L2583),
[Core call indices](../../../compiler/GHC/Core/TyCo/Rep.hs#L358),
and [term operand checker](../../../compiler/GHC/Tc/Gen/App.hs#L3488).

The open #383 discussion sketches a dictionary argument for each
promoted method. Eisenberg's review raises the cost of a separate
translation alongside a future dependent Core. The #509 review
distinguishes its closed evaluator from evaluation under binders.
Morris and Eisenberg §4.2 explains the domain check used by #177.
Eisenberg and Stolarek §4.8 describes the separate associated-family
encoding. The domain check alone supplies no relation between that
family's equations and the ordinary method.
[#383 design](https://github.com/ghc-proposals/ghc-proposals/issues/383#issuecomment-739058810),
[#383 review](https://github.com/ghc-proposals/ghc-proposals/issues/383#issuecomment-746676314),
[#509 review](https://github.com/ghc-proposals/ghc-proposals/pull/509#issuecomment-1159256040),
[#177 review](https://github.com/ghc-proposals/ghc-proposals/pull/177#issuecomment-549766671),
[Constrained Type Families §4.2](https://arxiv.org/pdf/1706.09715#page=8),
and [Promoting Functions to Type Families in Haskell §4.8](https://richarde.dev/papers/2014/promotion/promotion.pdf#page=6).
