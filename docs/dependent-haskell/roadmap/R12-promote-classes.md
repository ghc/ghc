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
singleton constructor. The public baseline reports GHC-70566 for the result type.
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

## Scope of the selected dictionary

The next source control has two distinct class predicates in reverse
operand order.

```haskell
chooseTaggedBoth :: forall a b. (Choose b, Choose a)
                 => foreach (x :: a) -> foreach (y :: b)
                 -> (Tagged (choose x), Tagged (choose y))
chooseTaggedBoth x y = (tagBool (choose x), tagBool (choose y))
```

With distinct rigid `a` and `b`, each operand fixes the selector's type
argument. Ordinary desugaring takes the evidence parameters in context
order, `db :: Choose b` and then `da :: Choose a`. It emits
`choose @a da x` and `choose @b db y`. A control that removes `Choose b`
fails at the second call. The test compiles the term-only program and
checks that both dictionary fields produce their expected results. The
public baseline reports GHC-70566 at both occurrences in the indexed signature.
[Source controls](../../../testsuite/tests/dependent/should_run/promoted-class/ChooseEvidence.hs).

For this first slice, require a unique direct Given of the instantiated
predicate in the enclosing explicit, complete signature. The evidence
binder scopes over the rest of that signature and its term body. Preserve
this binder through abstraction and instantiate it with the evidence that
the caller supplies. The result index can be a neutral application of the
selector to the instantiated type, that dictionary and the retained
operand. Type equality may compare two such applications by congruence of
their checked parts. It must preserve evidence identity and cannot use
inverse inference, a proof from the class predicate alone, or a fresh
global instance choice. This slice leaves superclass projections and
arbitrary evidence expressions to a later check.

`getLexicalGivens` can see the term implication's evidence parameters.
Binding a predicate as an existing invisible retained argument does not
give the desired representation. Core Lint requires its binder type to
have kind `Type`; a dictionary predicate has kind `Constraint`. The
application checker also resolves invisible retained values through its
own path. The representation needs an evidence binder with the original
constraint application behavior and a checked neutral selector use at a
domain that mentions a type variable. Check scope and evidence
substitution in Core and on interface readback before allowing reduction.
[Term implications](../../../compiler/GHC/Tc/Utils/Unify.hs#L523) and
[retained binder lint](../../../compiler/GHC/Core/Lint.hs#L2236).

Agda's instance arguments supply a precedent for a scoped record
argument and an implicit field projection. Its `elem` example also shows
the equivalent explicit application of the local `Eq A` argument. R12
uses GHC's Given selection and keeps its existing source constraint
syntax. The `@@` notation in #383 illustrates the dictionary argument
and is not proposed source syntax here.
[Agda 2.8 instance arguments](https://agda.readthedocs.io/en/v2.8.0/language/instance-arguments.html#usage)
and [#383 sketch](https://github.com/ghc-proposals/ghc-proposals/issues/383#issuecomment-739058810).

## Private interface experiment on public baseline `526aa52f79`

The private compiler exports `chooseTagged` and `chooseTaggedBoth` as
signatures. Each selector refers to the dictionary and operand bound in
its signature. The reader checks both references using the method's
instantiated type. It derives the index kind from the selector.
These interfaces use the compositional `experimental-scoped-dictionary`
feature. Interfaces with only ordinary exports keep their existing
format. The experiment omits unfolding pragmas for modules with these
types. GHCi runs their bytecode and skips breakpoint stops whose types
include scoped dictionaries. HIE renders the source form of the
constraint and selector. Its type table still uses the exact Core
dictionary identity when sharing entries.

The wire test compiles a client with only the provider interface
available. It calls both exported functions, checks the reversed
dictionary order and both demand cases, and rejects missing or swapped
evidence. The same interface can carry R24's constrained family version
and both R22 package markers. The reader still rejects a family use
without the owner dictionary.
[Wire test](../../../testsuite/tests/dependent/should_run/NativeScopedDictionaryWire.sh).


The private Showcase calls the imported selector under two direct Givens,
one for a Boolean and one for its wrapper with the opposite behavior.
The signatures use existing class constraints and keep the two
`Choose` dictionaries distinct.
[Showcase provider](../examples/open-evaluation/ScopedClassMethod.hs).
[Showcase client](../examples/open-evaluation/ScopedClassMethodClient.hs).


## Reload a scoped dictionary body

A source-hidden Template Haskell client can run the provider from its
`.hi` and persisted `.gbc`. The `.gbc` already contains bytecode.
With `-fwrite-if-simplified-core`, the provider can instead supply its
`.hi` alone. GHC reads the extra Core bindings and generates bytecode.
The regression tests this with interface pragmas ignored, two distinct
dictionaries, and a bottom in each of the two lazy positions.
A second GHCi session reuses the provider interface and regenerates its
bytecode when a changed client splice calls it.
[Persistent bytecode implementation](https://gitlab.haskell.org/ghc/ghc/-/commit/6bd8155c991cfce7cca594bc63ba8f6ca6751667).
[Extra Core help](https://ghc.gitlab.haskell.org/ghc/doc/libraries/ghc-9.15-inplace/src/GHC.Unit.Module.WholeCoreBindings.html).
[Reload test](../../../testsuite/tests/dependent/should_run/NativeScopedDictionaryCoreReload.sh).

A separate `experimental-scoped-dictionary-core` marker identifies
serialized bodies. The reader resolves a dictionary application against
its lexical binder and checks the nominal predicate before constructing
Core. Whole-module replay checks the admitted forms and runs scoped
Core Lint. It uses an empty interactive context. The codec leaves
unfolding pragmas suppressed and keeps the serialization gate for
other retained proofs and erased package expressions.
