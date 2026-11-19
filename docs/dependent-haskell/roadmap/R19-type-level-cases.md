# R19. Type-level case expressions

The roadmap calls for constructor matching inside a type expression.
Its prerequisites are dependent Core (R05) and open evaluation (R16).
[Upstream example](https://ghc.serokell.io/dh#type-level-case-expressions).

```haskell
type F b = case b of
  True  -> Int
  False -> Bool
```

With `ExperimentalDependentTypes`, a top-level synonym can select a
result by a constructor or bind its fields. A client with only the
compiled interface can prove `F 'True :~: Int`. For a rigid unknown
scrutinee, two branches with identical result types still leave the
case stuck. [Provider](../examples/open-evaluation/NativeCase.hs)
and [ordinary client](../../../testsuite/tests/dependent/should_run/NativeTypeCaseClient.hs).

A known constructor that has no matching branch also leaves the
case stuck. For `type Pick b = case b of True -> Int`, a client's
attempt to prove `Pick 'False :~: Int` fails at its `Refl`.

GHC checks one equation of a private closed family per alternative.
Each equation receives the lexical captures and scrutinee. An error in
the client can display the checked alternatives using the interface.
The lifting follows the promotion of case expressions described by
Eisenberg and Stolarek. Agda's pattern lambdas provide a related way
to match on an intermediate expression. [Promotion paper, §4.5](https://richarde.dev/papers/2014/promotion/promotion.pdf)
and [Agda pattern lambdas](https://agda.readthedocs.io/en/stable/language/with-abstraction.html#pattern-lambdas).

The supported patterns have a constructor head and distinct variables or
wildcards for fields. General nested cases, catch-all alternatives and
case expressions inside arbitrary types remain open.
