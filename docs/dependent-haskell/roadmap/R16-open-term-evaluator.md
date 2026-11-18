# R16. Open term evaluator

The upstream example has a type variable whose value the compiler cannot
know. Identity can return it unchanged. Serokell proposes normalization
by evaluation for this problem. R13 includes this card. The cards for
lambdas, lets and cases in types also depend on R16 and dependent Core
(R05). [Serokell R16](https://ghc.serokell.io/dh#open-term-evaluator)

```haskell
x :: forall a. Num a => id a
x = 42
```

With `ExperimentalDependentTypes` and `ScopedTypeVariables`, the fork
reports GHC-68947 on `id`. A local ordinary identity with a signature
gives the same error. An explicit closed type family with equation
`Identity a = a` accepts this example. That control checks GHC's existing
family reduction. It does not invoke a term evaluator.

## Evidence required

- [ ] Reduce an ordinary polymorphic identity applied to a rigid variable.
      Preserve its binder identity under shadowing. Test a projection whose
      unused argument diverges.
- [ ] Suspend a case on a rigid, unknown scrutinee together with its
      alternatives. Distinguish a rigid variable from an inference variable
      whose later assignment allows the checker to resume the computation.
- [ ] Specify the typed input, lexical environments and which reductions
      produce weak-head normal form. Preserve Haskell's demand behavior.
      A strict pattern cannot discard the need to inspect its argument.
- [ ] Establish the evidence required when an application survives into
      Core. The fork's `TermAppCo` relates corresponding applications of a
      retained function. It supplies congruence alone. A beta step needs a
      checked justification if it changes a type that already appears in Core.

GHC issue #27413 explicitly asks which typed representation to evaluate
and how to handle metavariable holes. Agda's evaluator records why a
reduction is blocked. These are starting points for the first two checks.
[GHC #27413](https://gitlab.haskell.org/ghc/ghc/-/issues/27413)
and [Agda's weak-head reduction](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/TypeChecking/Reduce.hs#L616).
