# R36. Pun warnings

- [x] Report same-spelled type and term definitions in one top-level group.
- [x] Report same-spelled explicit type and term binders in one match.
- [ ] Check implicitly introduced type binders for binding puns.
- [x] Report punned global term, type, and bare export uses.
- [x] Report the named list and boxed tuple syntax from proposal #765.
- [x] Report implicit capture at each written type variable use.
- [ ] Report uses shadowed by an explicit binder in another lexical scope.
- [ ] Check global uses under stage-specific imports and remaining syntax forms.

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

With `-Wpun-uses`, a name use can warn even if its module declares
neither of the conflicting names. For example, one module can export
a constructor `T` and another can export a type `T`. Unqualified use
of either name reports `GHC-41293` when both are in scope. This also
applies to a bare export item. An import qualifier disambiguates
only if it selects names from one namespace. A qualifier shared by
both imports still permits the conflict. The renamer uses the
selected name and checks the competing name without recording
another import as used.
[Global regression](../../../testsuite/tests/rename/should_compile/PunUses.hs)
and [explicit controls](../../../testsuite/tests/rename/should_compile/DisambiguatedPunUses.hs).

The parser records which written syntax relies on `ListTuplePuns`.
The warning covers `[]` used as a type constructor and boxed tuple
types such as `(a, b)`, including the unit type. List types such as
`[a]`, term list and tuple syntax, explicit promotion, and named
type constructors such as `List` and `Tuple2` follow their existing
paths without this syntax warning.

When implicit quantification creates a type variable with the
spelling of a term already in scope, `-Wpun-uses` reports each use
at its written location. It takes precedence over
`-Wterm-variable-capture` for that case, as specified in section
1.4. An explicit `forall` can shadow the term name under one
namespace, so its type references stay quiet. An associated type
that inherits the class's existing type variable does not introduce
an implicit binder or trigger this warning.
[Implicit regression](../../../testsuite/tests/rename/should_compile/ImplicitPunUses.hs)
and [associated-type control](../../../testsuite/tests/rename/should_compile/T23434.hs).

The local-use example
`f :: forall a. a -> a; f = \a -> (a :: a)` still needs lexical
scope tracking. The two reader namespaces retain the selected
names but do not record which binding group is closer to a use.
The annotation in `f @a = \(a :: a) -> a` is checked before its
term variable is bound. A test for another same-spelled name in
scope cannot distinguish these cases. The implementation also
leaves explicitly staged imports, non-bare export items, and
types inside type patterns outside the global-name check.
