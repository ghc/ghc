# R12. Promote classes

R12 is open. It depends on R13 (function promotion) and R24 (constrained
type families). M82 and M83 can use checked equations after identifying a
concrete instance method. The experimental compiler retains a unary method
application through one unique direct Given. It keeps the caller's exact
dictionary in the result type. Superclass selection, recursive evidence
and general class promotion remain open.
[Serokell card](https://ghc.serokell.io/dh#promote-classes) and
[local dependencies](../ROADMAP.md#upstream-checklist-and-complete-dependency-index).

## A checked method with a polymorphic receiver

```haskell
type MappedEither :: forall e a b. (a %U -> b) -> Either e a -> Either e b
type MappedEither f x = Prelude.fmap f x
```

The instance dictionary passes its erased `e` to the worker. The compiler
checks the dictionary field and the worker's final constructor cases.
The exported synonym names the worker's checked family. The two equations
preserve `Left x` and return `Right (f x)` for `Right x`. The baseline
reported GHC-70565 at the open callback in `Prelude.fmap f x`.
The Showcase's ordinary client displays both proofs with `Left 'x'` and
`Right False`. The fixture compiles that client with only the stripped
provider interface.
[Either proof](../examples/open-evaluation/PreludeFmapEitherProof.hs)
and [ordinary client](../examples/open-evaluation/OrdinaryEitherMapping.hs).

The `Result e` test uses an independent `Translate` class. An ordinary
client reads the existing proof from a stripped interface. A client with
a specialized `Translate (Result Type)` instance can prove the equation
for its own worker. Equating that result with the original worker's
result fails with GHC-83865. A fresh call with an abstract `e` and an
applicable coherent or incoherent overlap fails with GHC-70565.
A competing instance for `Maybe` is apart from `Result e`.
[Result provider](../../../testsuite/tests/dependent/should_run/either-fmap-overlap-source/ResultFunctor.hs)
and [downstream test](../../../testsuite/tests/dependent/should_run/NativeOpenEitherFmapDownstream.sh).

A local dictionary Given blocks the default worker check. An active rule
on `fmap` for `Either` reports GHC-83605 for a direct proof, an imported
synonym, or an imported equality proof. A separate rule for `Maybe` fires
in a consumer that still uses the `Either` proof. Rebuild older providers
to add the mandatory method evidence used by these tests.
[Given and rewrite controls](../../../testsuite/tests/dependent/should_run/NativeOpenEitherFmap.sh).

The `polymorphic-source-method` interface feature marks the new receiver
evidence. A new reader can import an old unmarked interface for ordinary
code. Promotion needs the rebuilt provider. An old reader reports GHC-47808
when it imports the new marked interface.
[Interface upgrade check](../examples/open-evaluation/check-polymorphic-method-upgrade.sh).

When the provider source is available, a warm build checks eligible
quantified instances once. The older mapping marker covered ground
instances. An old `GHC.Internal.Data.Either` interface can carry that
marker while its `Functor (Either e)` dictionary lacks the checked
`fmap` method. The new review includes instances with erased parameters
even in ordinary Haskell modules. The next build with the same inputs
reuses the interface.
[Recompilation precedent](../REFERENCES.md#upgrading-checked-method-interfaces).

## A method in a retained result type

```haskell
class Choose a where
    choose :: a -> Bool

chooseTagged :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
chooseTagged x = tagBool (choose x)
```

`tagBool :: foreach (b :: Bool) -> Tagged b` returns the corresponding
tagged Boolean value. Before scoped dictionary support, the baseline reported GHC-70566 for the result type.
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

The compiler typechecks `chooseTagged` while `a` and its
`Choose a` evidence are abstract. The index and the term call
must use the same selector, instantiation, operand and evidence. The
application can stay opaque during type checking. With the constraint
removed, the call must require `Choose a`.

The retained signature checker on the public baseline checks the predicates in
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
baseline reported GHC-70566 at both occurrences before the index could carry a dictionary.
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

## Exported method signatures

A library can export `chooseTagged` and `chooseTaggedBoth` in a
stripped interface. Each selector refers to the dictionary and operand bound in
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


The Showcase calls the imported selector under two direct Givens,
one for a Boolean and one for its wrapper with the opposite behavior.
The signatures use existing class constraints and keep the two
`Choose` dictionaries distinct.
[Showcase provider](../examples/open-evaluation/ScopedClassMethod.hs).
[Showcase client](../examples/open-evaluation/ScopedClassMethodClient.hs).

An ordinary caller can use the selected method inside a vector filter.
At `-O1` and `-O2`, the automatic specialiser keeps a binding that uses
a scoped method. A generated dictionary rule would match any evidence
for that instance type. The result type names the caller's own dictionary.
An independent equality count in the same module still specialises at
`Int`. An empty vector leaves the receiver unevaluated.
[Ordinary client](../../../testsuite/tests/dependent/should_run/promoted-class/ScopedSpecialise.hs)
and [specialisation rules](../../../compiler/GHC/Core/Opt/Specialise.hs).

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

A separate `experimental-scoped-dictionary-core-v3` marker identifies
serialized bodies. The reader resolves a dictionary application against
its lexical binder and checks the nominal predicate before constructing
Core. Whole-module replay checks the admitted forms and runs scoped
Core Lint. It uses an empty interactive context. The codec leaves
unfolding pragmas suppressed and keeps the serialization gate for
other retained proofs and erased package expressions.

An ordinary Haskell 2010 module can call an imported `chooseTagged`
under `Choose a` and match its result. Its own signature exposes only
`Choose a => a -> Bool`. The driver selects scoped Core Lint and
CorePrep from the checked bindings. The same compiled reader uses
distinct dictionaries at two call sites.
[Ordinary reader](../examples/open-evaluation/OrdinaryScopedChoice.hs).


Public interfaces use expression tag 19 for a retained character
and tag 20 for a retained `Natural`. The v3 Core format uses tag 21
for dictionary applications. Earlier private formats used tags 19 and
20 for that application. Readers reject an unknown Core format in the
header. Signature-only interfaces keep their existing marker.

The reload test puts a character, a `Natural`, a dictionary application,
and a data family constructor operand in one optimized provider. A Template Haskell splice
loads its serialized Core through the provider interface. The expected
result is `(True,[955,0,128578],23,True)`.
[Reload test](../../../testsuite/tests/dependent/should_run/NativeScopedDictionaryCoreReload.sh).


## An Ordering result through a direct Given

`compareTagged` retains `compareZero x` at kind `Ordering`. A Haskell
2010 client matches the imported result directly. One instance compares
an integer to zero. The second returns `LT` without forcing its operand.
The client observes all three orderings and passes a bottom to the second
instance. The wire check also rejects forwarding an independent operand
at the same class domain.
[Comparison provider](../examples/open-evaluation/ScopedComparison.hs),
[ordinary client](../examples/open-evaluation/OrdinaryScopedComparison.hs)
and [runner](../examples/open-evaluation/check-scoped-comparison.sh).

The same unary selector path checks a user-defined enumeration,
`Maybe Ordering`, and a result of kind `Maybe a` at the receiver
parameter `a`. The result kind comes from the selector signature.
The current source path requires one retained receiver of the class
parameter's type and one matching direct Given. A binary selector
and a method whose evidence comes through a superclass are outside
that path. A function-valued selector result can occur in a signature.
Passing the selected function to a retained-function binder gives
GHC-83605. That application requires a local function variable.

An ordinary client can pass a result containing a scoped selector to
`snd` or `id` when its inferred monotype has no type family and the
dictionary and receiver stay in scope. The [Showcase client](../examples/open-evaluation/OrdinaryScopedChoice.hs)
reads a length this way. The corresponding test accepts `Proxy 5`
and rejects `Proxy 6` at the ordinary projection.

The direct case works without solving an equality for an unknown result
index. An accessor that requires solving such an equality still reports GHC-83605. Access through a pattern synonym and refining GADT
patterns in the Haskell 2010 reader also remain unsupported.
[Scoped application checking](../../../compiler/GHC/Tc/Gen/App.hs)
and [selector shape and result kind](../../../compiler/GHC/Core.hs).

## A constrained family under the method's Given

One library exports the marked family `Element` and a scoped selector in
the same signature. Its `Collects c` dictionary supplies the formation
premise for the input and output types `Element c`. The result index uses
that same dictionary for `compareOrigin x`. An ordinary client can
supply distinct dictionaries and obtain `((GT,True),(LT,'x'))` from the
stripped interface. Missing or unrelated class evidence and a crossed
runtime index each fail. The formation check opens a leading dictionary
binder. Nested dictionary or selector types that need family formation
remain outside this check.
[Mixed provider](../../../testsuite/tests/dependent/should_run/promoted-class/ScopedFamilyMethodProvider.hs)
and [wire runner](../../../testsuite/tests/dependent/should_run/NativeScopedDictionaryWire.sh).

## A Given from a retained data family match

`foldCell :: foreach (c :: Cell [Bool]) -> At c` matches the retained
value with `Stored x`. The pattern introduces the actual `Pick Bool`
dictionary. The branch can call the imported selector `observe x`
under this Given and return `At c`. It can also call `tag (pick x)`
directly under the same Given. The selector's index stays inside the
branch.

The source-hidden client passes two cells with payload `True` and
observes `(True,False)`. One cell stores the provider's general
dictionary. The other stores the client's more specific dictionary.
The checker fixes the case multiplicity before entering the constructor
implication when the scrutinee is an unrestricted local data family
value. An inner case with an ordinary `Bool` result can pass the field
directly to the retained selector. A linear field or scrutinee still
fails. The checker compares the
instantiated predicates of the lexical Givens before applying a scoped
selector. A branch with two matching dictionaries fails even under
`-fdefer-type-errors`.
[Stored dictionary test](../../../testsuite/tests/dependent/should_run/NativeScopedStoredDictionary.sh).
The [Showcase client](../examples/open-evaluation/StoredDictionaryClient.hs)
compares an ordinary match with a dependent match on the same two
`Stored True` cells. Both matches report `(True,False)`.

In the review of draft #177, Eisenberg asked whether a value of an
associated data family type would always imply its parent class.
Zavialov asked when a constructor would need to store a dictionary.
Here the constructor explicitly requires `Pick a`. The two cells above
show why its consumer must use the dictionary selected at construction.
The [R24 newtype example](R24-constrained-type-families.md#associated-newtype-representation)
covers a constructor with no such constraint.
[Eisenberg's question](https://github.com/ghc-proposals/ghc-proposals/pull/177#issuecomment-431006662)
and [Zavialov's example](https://github.com/ghc-proposals/ghc-proposals/pull/177#issuecomment-431507862).

### Ambiguity in a method-indexed signature

In `inspectPayload :: forall c a. Collects c => foreach (x :: c)
-> a -> TaggedEntry a (compareOrigin x)`, the receiver determines
`c`. The ordinary payload determines `a`. GHC compares the retained
receiver domains before it opens the instantiated dictionary with the
matching Given. It then runs the usual ambiguity check on the body.
A redundant constraint on a fresh type parameter can still produce an
ambiguity error, and `AllowAmbiguousTypes` defers it to the use site.
At a call site, the receiver fixes the predicate of the scoped dictionary.
GHC instantiates other erased parameters in dependency order. Ordinary
constraints can use a functional dependency to fix a payload's encoding.
[Example](../examples/open-evaluation/OrdinaryScopedChoice.hs).
The ordinary test client passes a local receiver. Applying the imported
function directly to a constructor at the method-indexed position still
requires a local binding for that receiver.
The client can bind `let high = True` and call `inspectPayload high 'q'`.
Common subexpression elimination and CorePrep keep a variable for
the receiver until CoreToStg erases the selector index. A client can
discard the payload without forcing it. The Boolean instance comes from the
provider. CSE keeps the local binding for its imported dictionary.
[Ambiguity check](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ambiguous_types.html)
and [client](../../../testsuite/tests/dependent/should_run/promoted-class/ScopedFamilyMethodClient.hs).

A written Template Haskell quotation can use an imported method in
a retained index. Reifying the elaborated signature reports GHC-75721.
The current Template Haskell type representation has no binder for the
selected dictionary.
[Reification check](../../../testsuite/tests/dependent/should_run/NativeScopedDictionaryCoreReload.sh).


### A separate length argument

In `tagAt x 5`, the method `choose x` uses the identity of `x`.
The length has its own index. An ordinary client can supply it as a
literal. A client that expects length `6` gets a type mismatch. An
application that supplies a literal as the method receiver gets a source
diagnostic at that argument.
[Provider](../examples/open-evaluation/ScopedClassMethod.hs)
and [ordinary client](../examples/open-evaluation/OrdinaryScopedChoice.hs).
