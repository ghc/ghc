# R36. Pun warnings

- [x] Report same-spelled type and term definitions in one top-level group.
- [x] Report same-spelled explicit type and term binders in one match.
- [ ] Check implicit local type binders.
- [ ] Report punned uses, including the cases covered by implicit capture.

With `-Wpun-bindings`, both equations below report `GHC-59431` at the
value binder. The diagnostic also points to the type binder.

```haskell
{-# LANGUAGE ExplicitNamespaces, RequiredTypeArguments, TypeAbstractions #-}

id :: forall a -> a -> a
id (type a) a = a

invisible :: t -> t
invisible @a a = a
```

[Proposal #765](https://ghc-proposals.readthedocs.io/en/latest/proposals/0765-pun-warnings.html)
checks names introduced in the same scope. Appendix A groups the
arguments of the invisible example as binders that can shadow enclosing
names. The [review](https://github.com/ghc-proposals/ghc-proposals/pull/765#discussion_r3738449381)
distinguishes this group from a nested scope. With
`ScopedTypeVariables`, `f @a = \\(a :: a) -> a` has a separate binding
group inside the lambda. Its annotation refers to the outer type
variable. It produces no binding warning.

The old diagnostics still apply. In
`ordinaryShadow a = let f a = a in f a`, the inner value binder shadows
the outer value binder. `-Wname-shadowing` reports `GHC-63397`. In
`g a (_ :: a) = a` with `ScopedTypeVariables`, the annotation can
introduce a type variable implicitly. `-Wterm-variable-capture` reports
`GHC-54201`. If `RequiredTypeArguments` is on, the annotation refers to
the term name and GHC rejects it with `GHC-45510`. Appendix A treats
implicit quantification as unable to shadow an existing name in a single
namespace. Its pattern annotation example leaves tracking this property
per variable for later work.

The proposal's section 1.3.3 says a nested lambda binder also triggers
the binding warning in a punned-use example. Its executable
specification expects one use warning there. The explicit match check
follows the same-scope rule in W1 and the executable specification.
[Local regression](../../../testsuite/tests/rename/should_compile/LocalPunBindings.hs).
