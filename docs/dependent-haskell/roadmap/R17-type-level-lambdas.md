# R17. Type-level lambdas

The Serokell card asks for an anonymous function in a type. Its example
applies each of three type constructors to `Int`. The callback must stay
unapplied until `Map` selects a list element. The graph lists R05
(dependent Core) and R16 (open evaluation) as prerequisites or subtasks.
The prototype also uses R23 to pass an unsaturated type family.
[Upstream card](https://ghc.serokell.io/dh#type-level-lambdas).

```haskell
type L = Map (\f -> f Int) [Maybe, Either String, IO]

proof :: L :~: '[Maybe Int, Either String Int, IO Int]
proof = Refl
```

## Source and evidence

A lambda can occupy a whole type right-hand side. For example,
`type Wrap = \a -> Maybe a` and `let wrap = \a -> Either e a in wrap Bool`
use the same spelling as a term lambda. Use parentheses when passing a
lambda as an argument, as in `Map (\a -> Maybe a) xs`. The parsed
tree uses `HsParTy` for written parentheses and `HsTypeLambda` for
the lambda. Exact printing preserves comments and the written arrow.
[Source example](../examples/open-evaluation/TypeLambda.hs) and
[printer fixture](../../../testsuite/tests/printer/RetainedPrint.hs).

M174 checks this source in a top-level synonym with
`ExperimentalDependentTypes` and `DataKinds`. The module that defines
`Map` enables `TypeFamilies`. An ordinary client
can import `L` and check the same proof with only the interfaces.
The list argument can also fix the kinds of an identity lambda over
Booleans and a callback whose result has kind `Bool`. Other checks
cover a captured variable, a shadowed binder, a rejected matchable
arrow, and the module's implicit export list.

A lambda can bind one or more ordinary type variables. An enclosing
synonym can infer its parameter kinds. The renamer accepts lambdas in
supported positions in a synonym body or a closed type family equation.
These include the right-hand-side root, applications, parentheses, kind
ascriptions, and list and tuple elements. Other positions report GHC-27185.
With the extension disabled, the diagnostic is GHC-27184 at the lambda.

Adjacent lambdas share one family telescope. A lambda nested in an
application gets its own family and captures the enclosing equation's
binders. Both preserve lexical shadowing. An inner `q` can shadow an
outer `q` while a captured `a :: q` keeps its original kind. A formerly
implicit synonym kind variable can also appear explicitly in the lifted
body. An ordinary interface-only client checks these cases. Nested
family applications remain subject to the usual termination checks.

A callback defined inside a type let can capture the checked local
value. If the let shadows an outer type parameter, a capture whose
kind mentions that parameter keeps the outer kind.
[Let example](R18-type-level-lets.md).

The compiler lifts each lambda to a private closed family. The
declaration group checks the synonym and generated family together.
Captured variables become parameters. Saturation exposes the family
equation and supplies nominal evidence for beta reduction. A constant
lambda can discard its argument, so its result cannot establish
equality between the arguments of two calls.

A separate signature can use the same lambda with a different binder
name. For example, `forwardErrors` passes its arguments to `withErrors`
while its result still has the unknown index `xs` in
`Fields (Map (\field -> Either e field) xs)`. GHC compares the generated
closures and their checked equations. It then carries that nominal
equality through the common `Map` application. The same rule applies
when `Map` is an open family. The proof preserves
capture bindings and the order of the type case branches. A different
body or a different opaque callback cannot supply that proof.
[Forwarding client](../examples/open-evaluation/TypeLambdaClient.hs)
and [source-hidden regression](../../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh).

The interface records the `scoped-lambda` feature when it stores a
generated lambda root or retains a proof that uses one. An earlier
reader rejects that version at the header. A current reader can still
reduce applications from an earlier unmarked provider at known
indices. Forwarding its opaque callback at an unknown index needs the
provider's closure metadata.

A closed family can also match two equivalent closures against one
repeated variable. For `Same x x = 'True`, GHC proves the argument
conversion before applying the branch axiom. It checks apartness with
the arguments on both sides of that conversion. Unrelated opaque
callbacks keep the family stuck when GHC cannot check apartness.

In `Shadowed (q :: Type) (a :: q)`, the lambda binds a new `q`.
The family header keeps the outer `q` in scope for the kind of `a`.
The family equation uses the inner `q` for its input. At
`Shadowed Bool 'True`, mapping over `Char` yields
`'[Either Char (Proxy 'True)]`. This ordinary client checks both scopes.
[Interface test](../../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh).

Template Haskell can reify the synonym and splice its type back into a
declaration. The reified type contains a reference to the private family.
Two lambdas with the same source span after a `LINE` pragma still get
distinct families. Disabling the extension reports the source lambda.
The generated declaration stays out of an implicit export list.
An unchanged interface client stays warm. Editing the lambda body
from `Either a b` to `Either b a` recompiles the client and rejects
its old proof at `Refl`. [Source example](../examples/open-evaluation/TypeLambda.hs)
and [client regression](../../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh).

The pre-lambda M173 compiler rejects the backslash in the provider.
With the M174 provider interface, it compiles and runs an ordinary
client that proves both `L` and `Shadowed Bool 'True`. M173 already
supports the unsaturated families used in this interface.

## Mapping a row in a value signature

A top-level value signature can give a type family a lambda directly.

```haskell
withErrors :: Proxy e -> Fields xs -> Fields (Map (\a -> Either e a) xs)
```

The lambda binds `a` and captures `e` from the signature. The recursive
call uses the same family as the outer call. A client with the compiled
interface can map a row of unrelated types. Reading the first result
does not force the second field. Template Haskell can reify the type of
a local or imported value and splice the signature into another declaration.
[Source](../examples/open-evaluation/TypeLambda.hs) and
[ordinary client](../../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh).

The source-hidden `forwardErrors` client repeats the callback with
a fresh binder name. The two signatures generate separate private
families. The checked closure equality rule accepts the forwarding
call even with `xs` unknown. Reversing the arguments of `Either`
changes its result and gives GHC-83865. The diagnostic names both
source signatures. Agda likewise identifies lambdas up to renaming
a bound variable.
[Client](../examples/open-evaluation/TypeLambdaClient.hs),
[equality evidence](../../../compiler/GHC/Builtin/CoercionRules.hs) and
[Agda function equality](https://agda.readthedocs.io/en/v2.8.0/language/lambda-abstraction.html).

## Rows under an erased forall

```haskell
type RankedReader (k :: Type) (table :: k) =
  forall (column :: k). Proxy column ->
    Fields (Map (\k -> (Proxy table, Proxy column, k)) '[Char])
```

The result carries the outer table index, the quantified column
index, and the cell. Both indices keep the enclosing kind. The
lambda's own `k` names the cell type. A nested erased forall in a
top-level signature can use the same form. The new path checks
explicitly kinded, splice-free binders in telescope order and
requires distinct telescope names. Retained and splice-dependent
binders keep their existing scope checks.
[Row example](../examples/open-evaluation/TypeLambda.hs) and
[ordinary client](../../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh).

## Capturing a closed family pattern

```haskell
type Prefixes :: Maybe Type -> [Type] -> [Type]
type family Prefixes prefix xs where
  Prefixes ('Just e) xs = Map (\a -> Either e a) xs
  Prefixes 'Nothing xs = Map (\e -> Maybe e) xs
```

The first callback captures `e` from its equation's constructor pattern.
The second equation has its own scope. Its lambda binds a fresh `e`.
`Map` receives an unmatchable callback and applies it to each element.
Normal type family termination checks still apply. This use of `Map`
requires `UndecidableInstances`.

The [provider](../examples/open-evaluation/TypeLambda.hs) and
[interface client](../../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh)
check the two equations. The client runs with the provider source
absent and constructs fields whose types depend on the mapped list.
The scope test rejects a reference to another equation's pattern binder at
the written occurrence.

## Open and associated family instances

A library can declare an open `Columns` family. A different module can
define its own schema and map a lambda over the field types.

```haskell
data Checked e
type instance Columns (Checked e) fields = Map (\a -> Either e a) fields
```

The instance pattern binds `e`. The lambda binds `a`. A client that imports
only the interfaces can construct a row with type `Columns (Checked String) '[Bool]`.
Another instance maps `Maybe` over the fields of an optional row.
An associated equation can use a capture from its enclosing instance
when its left-hand side binds that variable. For example, under
`instance forall e. Associated (Schema e)`, the equation
`type forall xs. AMapped (Schema e) xs = Map (\a -> Either e a) xs`
captures `e` from the instance and binds `xs` in the equation.
An arrow in an instance RHS gives both operands the same lexical scope.
The `Callable` schema uses this to store a function whose result maps
the callback over a row. An ordinary source-hidden client can call it.
GHC still rejects an overlapping instance with a different result.
[Schema library](../examples/open-evaluation/TypeLambda.hs),
[instance module](../examples/open-evaluation/SchemaColumns.hs) and
[GHC open type families](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html#open-type-synonym-families).

## Earlier interface reader

Normal 566 reads stripped interfaces for the new ordinary fields and
instances. Its ordinary clients can construct the fields and call the
row readers. With `UnsaturatedTypeFamilies`, Template Haskell on 566
can reify and splice the open and associated instance RHSs. It can
reify the new field selector. Splicing that selector's reified type
reports GHC-55017 for its private field alias on 566. The current
converter accepts the same splice.

## Matching a lambda argument

A constructor case can form the body of a type lambda. For example,
`Map (\b -> case b of { True -> Int; False -> Bool }) '[ 'True, 'False]`
reduces to `'[Int, Bool]`. A branch can also refer to the original
argument. In `Just _ -> Proxy tag`, the type of `tag` includes the
field that the wildcard matched. In `Nothing -> Proxy tag`, it retains
the kind of the input. GHC checks each branch as a separate equation
of the lifted family. An unknown constructor leaves the family stuck.

The [provider](../examples/open-evaluation/TypeLambda.hs) and its
[interface-only consumer](../../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh)
check the mapped result, lexical shadowing and both `Maybe` cases.

A closed written polymorphic kind can supply the lifted family's standalone
signature for a tail case. The invisible forall and value arrows must be
unmatchable. GHC checks each indexed constructor against that signature and
preserves written specified and inferred binders. The forwarding path
requires splice-free annotations and captures whose declared kinds are
closed and splice-free. An annotation or capture kind containing a type splice
stays on the ordinary inference path, so a case requiring refinements at
different indices still fails there. A splice on a captured synonym binder
runs on the written binder once. The generated capture parameter gets its
kind from the checked application.

When the typechecker expands a lambda in an equality error, it can recover
the public name from the client's signature. For example, if a client
writes `type Named t = Rebuild t`, its failed proof can show
`Named (Opaque t)` on both sides of the actual equality. GHC restores a
synonym only when its expansion has the same family and arguments.
If the mismatch exposes a case inside a larger synonym, GHC can show the
checked alternatives, with the other lambda arguments substituted. A type
lambda does not in general determine its arguments from its result.
[Diagnostic and scope checks](../../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh).

## Prior art

Agda uses ordinary lambda syntax and accepts unannotated binders when
checking an anonymous function. Its pattern lambdas translate to fresh
definitions. The surface spelling above comes from Serokell.
[Agda lambda abstraction](https://agda.readthedocs.io/en/v2.8.0.2/language/lambda-abstraction.html).

Eisenberg and Stolarek lift a lambda to a fresh family and pass its local
variables as parameters. Their implementation in `singletons` uses
defunctionalization for partial application. The fork can pass the family
itself at an unmatchable arrow. [Promoting Functions to Type Families in
Haskell, §4.6](https://richarde.dev/papers/2014/promotion/promotion.pdf)
and [GHC proposal 242](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html).
The [proposal 242 review](https://github.com/ghc-proposals/ghc-proposals/pull/242)
discusses matchability as a way to retain first-order unification for
higher-order operations. The
[proposal 378 review](https://github.com/ghc-proposals/ghc-proposals/pull/378)
discusses using ordinary lexical scope and familiar function syntax
across term and type positions. Proposal 378 is a design sketch. This
placement is part of the fork's experimental syntax.
