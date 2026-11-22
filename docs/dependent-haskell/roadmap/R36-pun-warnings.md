# R36. Pun warnings

- [x] Report same-spelled type and term definitions in one module scope.
- [x] Report same-spelled explicit type and term binders in one match.
- [x] Check implicitly introduced type binders for binding puns.
- [x] Report punned global term, type, and bare export uses.
- [x] Report the named list and boxed tuple syntax from proposal #765.
- [x] Report implicit capture at each written type variable use.
- [x] Report uses shadowed by an explicit binder in another lexical scope.
- [x] Check global uses under stage-specific imports and remaining syntax forms.

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
checks names introduced in the same scope. A top-level declaration
splice divides renaming and type checking into batches. Definitions
across those batches still share the module scope, so the later
declaration gets the warning.
[Declaration-splice regression](../../../testsuite/tests/rename/should_compile/ToplevelPunSplices.hs).

Appendix A groups the
arguments of the invisible example as binders that can shadow enclosing
names. The [review](https://github.com/ghc-proposals/ghc-proposals/pull/765#discussion_r3738449381)
distinguishes this group from a nested scope. With
`ScopedTypeVariables`, `f @a = \\(a :: a) -> a` has a separate binding
group inside the lambda. Its annotation refers to the outer type
variable. It produces no binding warning. In contrast,
`freshAnnotation (d :: d) = d` introduces both a term variable and a
fresh implicit type variable in one match. It reports `GHC-59431`.

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
The warning covers written list types such as `[a]`, the type
constructor spelling `[]`, and boxed tuple types such as `(a, b)`.
Proposal #765 section 1.3.4 includes `[a]`. Its executable appendix
omits a duplicate example because it treats `[a]` as `[] a`. Term
list and tuple syntax, promoted lists, and named type constructors
such as `List` and `Tuple2` remain quiet.

When implicit quantification creates a type variable with the
spelling of a term already in scope, `-Wpun-uses` reports each use
at its written location. It takes precedence over
`-Wterm-variable-capture` for that case, as specified in section
1.4. An explicit `forall` can shadow the term name under one
namespace, so its type references stay quiet. An associated type
that inherits the class's existing type variable does not introduce
an implicit binder or trigger this warning. A later occurrence of a
type variable introduced in a pattern signature gives the same warning
if the term binder still takes precedence in a unified namespace.
[Conditional-scope regression](../../../testsuite/tests/rename/should_compile/ConditionalPunUses.hs).
[Implicit regression](../../../testsuite/tests/rename/should_compile/ImplicitPunUses.hs)
and [associated-type control](../../../testsuite/tests/rename/should_compile/T23434.hs).

In `f :: forall a. a -> a; f = \a -> (a :: a)`, the type variable
used in the body annotation is shadowed by the term variable in a
unified namespace. Only the type use warns. In
`f @a = \(a :: a) -> a`, the renamer checks the pattern annotation
before binding the term variable, so it stays quiet. In a match that
introduces an explicit type and term binder with the same spelling,
both names compete in its right-hand side.
[Lexical regression](../../../testsuite/tests/rename/should_compile/LocalScopedPunUses.hs).
A GADT constructor starts a fresh lexical scope. A constructor-local
`forall` can affect the name used by a splice in its signature.
[Constructor scope regression](../../../testsuite/tests/rename/should_compile/GadtPunScope.hs).

An unmarked parent in `T(..)` or `T(X)` still depends on the choice of
namespace. An explicit `type T(..)` selects the type namespace. Child
names are resolved within the selected parent. The same list and tuple
syntax checks apply inside constructor type patterns, as does the check
for bare punned constructor names used as types.
[Export regression](../../../testsuite/tests/rename/should_compile/ExportParentAll.hs)
and [type-pattern regression](../../../testsuite/tests/rename/should_compile/TypePatternPunUses.hs).

Global pun-use checks include normal, quote and splice imports. Proposal
#682 resolves names before checking their levels, so a conflicting name
at another level can still make the unified namespace ambiguous. A
qualifier selecting one namespace disambiguates the use. GHC checks
the level of the name it actually selected as before.
[Staged import regression](../../../testsuite/tests/rename/should_compile/StagedPunUses.hs)
and [proposal #682, name resolution](https://ghc-proposals.readthedocs.io/en/latest/proposals/0682-explicit-level-imports.html#name-resolution-and-ambiguous-variables).

Both warning flags are off by default and included in `-Weverything`.
When `-Wpun-uses` and `-Wterm-variable-capture` are enabled together,
each affected occurrence receives the pun-use warning alone. Disabling
`-Wpun-uses` restores the legacy capture warning.

Declarations inside Template Haskell quotes form their own scope. Both
names in a punned declaration group compete inside the quote. An inner
definition can shadow a name in the enclosing module.
[Declaration quote regression](../../../testsuite/tests/rename/should_compile/DeclarationQuote.hs).

An in-scope record selector function can compete with an implicitly
bound type variable. The warning lists the visible selectors once
at each occurrence. `NoFieldSelectors`, hidden imports, and
qualified-only imports leave an unqualified type variable quiet.
[Selector regression](../../../testsuite/tests/rename/should_compile/RecordSelectorPunUses.hs)
and [quiet controls](../../../testsuite/tests/rename/should_compile/HiddenSelectorPunUses.hs).
