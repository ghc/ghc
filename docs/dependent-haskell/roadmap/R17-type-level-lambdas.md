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
synonym bodies and family equation right-hand sides. It descends into
applications, parentheses, kind ascriptions, list and tuple elements,
and both operands of an arrow. Constructor fields and top-level value
signatures can also contain a lambda. The [ranked row example](#rows-under-an-erased-forall)
shows which quantified binders it can capture. A [local value signature](#a-lambda-in-a-local-value-signature)
can capture types from the enclosing signature.
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

## A lambda in a local value signature

A `where` signature can use the same error type as the enclosing
function. Here `e` is in scope at the local signature through
`ScopedTypeVariables`. Each recursive call checks a shorter row.

```haskell
checkRow :: forall (e :: Type) xs.
            Proxy e -> Fields xs -> Fields (Map (\a -> Either e a) xs)
checkRow _ = go
  where
    go :: forall (ys :: [Type]). Fields ys -> Fields (Map (\cell -> Either e cell) ys)
    go End = End
    go (Field x rest) = Field (Right x) (go rest)
```

The renamer captures `e` by its lexical name. Its written kind comes
from the enclosing signature. A row index that appears only outside the
lambda needs no written kind. In the original implementation, omitting the kind
of `e` reports GHC-27185 at the backslash and names `e`. A shadowed kind
that a capture needs can also prevent lifting.
[Maintained row mapper](../examples/open-evaluation/TypeLambda.hs)
and [scope checks](../../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh).

An inferred kind must preserve dependencies at the point where the
lambda is written. For example, an outer `tag :: k` keeps its original
kind when a local signature binds another `k`. The type
`Proxy (\cell -> (Proxy (tag :: k), cell))` then refers to two different
kinds. An early prototype accepted this type and failed only if a client
tried to reduce the function. Job 1743 found the same failure with an
ascription, a dependent constructor and a dependent family. Checking
the callback only on application missed all three formation errors.
The 574-observation compiler also accepts an inferred capture kind in
`taggedCell :: forall k tag. Proxy (tag :: k) -> Bool -> Fields '[(Proxy tag, Bool)]`.
Its local row signature uses both `k` and `tag`. A current and a 572
reader both consumed the stripped provider at `-O0` and `-O2`.
The same formation check applies to an expression annotation. In
`Proxy (\cell -> (At k tag, cell))`, an inner signature that shadows
`k` with an incompatible kind gives GHC-25897 at `tag`, even though the
program never applies the lambda.
[Formation test](../../../testsuite/tests/dependent/should_run/NativeTypeLambdaFormation.hs).

### Forwarding the same callback

The outer and inner lambda may use different parameter names. An
ordinary client can also share the callback through
`type WithError e = \cell -> Either e cell`. If the outer signature
instead uses `type WithError e a = Either e a` and the inner uses a
lambda, GHC-83865 points to `go row` for unknown `xs`. Both callbacks
have an unmatchable arrow. Shortening the named synonym to `Either e`
would change its arrow to matchable.

Job 1725 tested the two callbacks as arguments to a closed family with a
repeated variable. The mixed comparison stays stuck. A differently named
parameter in an otherwise identical generated lambda selects the equal
branch. Agda uses definitional function eta. The #242 review retains
fixed family arities and leaves distinct partial family applications
potentially equal. Its rule does not identify those applications by
comparing their results on a fresh argument.
[Agda function equality](https://agda.readthedocs.io/en/v2.8.0/language/lambda-abstraction.html#lambda-expressions),
[#242 review of arity and apartness](https://github.com/ghc-proposals/ghc-proposals/pull/242#issuecomment-503360362)
and [GHC's partial synonym rule](../../../compiler/GHC/Core/Type.hs).

When a lambda forwards its argument to a named unmatchable family, a
checked conversion can use the family and the supplied arguments directly.
In the following example, `xs` is still unknown at the call to `advance`.

```haskell
update :: forall tag xs. Batch (Map (Stamp tag) xs) -> Int
update row = advance row
  where
    advance :: Batch (Map (\cell -> Stamp tag cell) xs) -> Int
    advance (Batch n) = n + 2
```

The compiler checks the generated lambda's single equation. It verifies
that the named family receives the same captured arguments and the final
lambda parameter. The named family's equations stay opaque. Thus
`\cell -> Discard tag cell` can share the callback `Discard tag`. A
constant lambda `\cell -> tag` still cannot, even if `Discard` returns
`tag` for every argument. The constructor synonym `WithError` above
also remains a separate case.
The example enables `UndecidableInstances` because the generated family
calls `Stamp` with arguments the same size as its own.
[Forwarding regression](../../../testsuite/tests/dependent/should_run/NativeTypeLambdaEta.hs).

The same conversion applies to a captured callback in a partially
applied family. Here `Stage` still needs its cell argument.

```haskell
throughStage :: forall tag. Ticket (Stage (Stamp tag)) -> Int
throughStage ticket = choose @(Stage (\cell -> Stamp tag cell)) ticket
```

A family can also return a callback. The check can compare
`Route (Stamp tag) flag Bool` with the forwarding form when `flag`
is unknown. Both types have the same family head and the same
arguments after the callback. The compiler can prove their equality
without selecting a family equation.

At `-O0` and `-O2`, the source-hidden reader prints `42`. The optimized
reader also inlines the imported equality proof. Interfaces that contain
that proof require `scoped-lambda-eta-v1`. An older reader still accepts
the ordinary family provider. It rejects the proof-bearing interface at
the version header.

## A lambda in a visible type argument

```haskell
messageCells :: forall (e :: Type). e -> Fields '[Either e Int, Either e Bool]
messageCells msg = rowFor @(\cell -> Either e cell) Proxy (Right 42) (Left msg)
```

`rowFor` has an explicit parameter `f :: Type %U -> Type`. The visible
argument chooses the error type for both columns. Its lambda captures
`e` from the enclosing signature. An untyped expression splice can use
the same syntax. The lifter copies complete, splice-free arguments
and preserves the scope of each capture. A free name in a visible type
argument cannot introduce a binder. An ordinary `@_` still selects an
inferred argument. [Row builder](../examples/open-evaluation/TypeLambdaArgument.hs)
and [source-hidden tests](../../../testsuite/tests/dependent/should_run/NativeTypeLambdaTypeApp.sh).

The syntax extends GHC's visible type argument with an anonymous type
function. Agda also allows a caller to pass an anonymous function as an
explicit argument. [GHC TypeApplications](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/exts/type_applications.html),
[Agda lambda expressions](https://agda.readthedocs.io/en/v2.8.0/language/lambda-abstraction.html#lambda-expressions)
and [Agda application of implicit arguments](https://agda.readthedocs.io/en/v2.8.0/language/implicit-arguments.html).

### A type splice in a visible argument

```haskell
messageCells :: forall (e :: Type). e -> Fields '[Either e Int, Either e Bool]
messageCells msg =
    rowFor @($([t| \err cell -> Either err cell |]) e)
        Proxy (Right 42) (Left msg)
```

The quote binds `err` and `cell` in that order. The visible argument
applies the resulting lambda to `e` and passes the remaining function
to `rowFor`. The splice runs once and keeps its finalizer. An unknown
name in the quote is reported there. An ill-kinded body is reported at
the splice. [Source-hidden regression](../../../testsuite/tests/dependent/should_run/NativeTypeLambdaTypeApp.sh).

Proposal #242 supplies the unmatchable arrows for partial application.
Proposal #378 supplies the lexical scoping principle. The two-binder
lambda itself is experimental syntax from the roadmap. [GHC proposal #242](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html),
[GHC proposal #378](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html)
and [Serokell's type-level lambda card](https://ghc.serokell.io/dh#type-level-lambdas).

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

A client can also use `checkedCells` with result type
`Fields '[Either e Bool]`. Changing only the field callback from
`Either e a` to `Either a e` recompiles the unchanged client.
GHC-25897 points to the selector in its definition.
[Recompilation regression](../../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh).

## Earlier interface reader

Normal 566 and 567 read stripped interfaces for the new fields and
family instances. Their ordinary clients can construct the fields and
call the row readers at both `-O0` and `-O2`. With
`UnsaturatedTypeFamilies`, Template Haskell on both readers can reify
and splice the open and associated instance right-hand sides. Both can
also reify the field selector. Splicing its reified type reports
GHC-55017 for the private field alias. Normal 568 accepts that splice.

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

## Row callbacks under a retained binder

```haskell
packRow :: foreach (p :: Columns) -> Fields (Map (\a -> Vec a (size p)) '[Char, Bool])
packRow p = Field (letters p) (Field (flags p) End)
```

The lambda captures `p` from the retained signature. Its helper takes
an erased parameter of kind `Columns`. The signature passes the
original record index, so both fields use the same `size p` equation.
Using `flags q` from a separate record reports an index mismatch.
[Row example](../examples/packed-vector/SiblingColumnRow.hs) and
[source-hidden checks](../../../testsuite/tests/dependent/should_run/NativeRetainedSiblingFields.sh).

An ordinary client can consume a record of unknown length. Give the
row eliminator the shared index in its signature.

```haskell
unpack :: Fields '[Vec Char n, Vec Bool n] -> (String, [Bool])
unpack (Field xs (Field ys End)) = (toList xs, toList ys)

readRow :: Columns -> (String, [Bool])
readRow p = unpack (packRow p)
```

Jobs 1877 and 1880 compared a named schema and the inline lambda at
`-O0` and `-O2`. The unannotated nested pattern worked with a directly
constructed record. For a let-bound record or a caller-supplied record,
both schemas needed the eliminator signature above.
The signature fixes the row and result types before GHC checks the
nested pattern. The length `n` can still be unknown.
[GHC's rules for pattern matching on GADTs](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html).

The supported body has no inner binders. The lambda parameters and
enclosing retained arguments keep their lexical scope. A captured
retained domain must also check as an erased binder kind.
[GHC proposal 378 on term variables in types](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html#term-variables-in-types) ·
[Agda lambda abstraction](https://agda.readthedocs.io/en/v2.8.0.2/language/lambda-abstraction.html).

## Type lambdas in untyped quotations

A declaration quote can generate a schema with an inline callback.

```haskell
$([d|
  type CheckedColumns e xs = Map (\cell -> Either e cell) xs
  |])
```

The callback captures the schema's error type `e`. The quote produces
`LamT` with fresh binder names. The splice lifts and checks the callback
in the group that defines `CheckedColumns`. A type quote spliced into a
top-level value signature and an expression quote spliced into a value
binding can also contain inline lambdas. In the example, the type quote
proves that the generated schema agrees with a directly mapped row.
The expression quote produces the row with that type.
The declaration quote for `labelledColumn` uses a lambda in a local
signature. It captures the enclosing function's label type by its
exact name.
[Source and executable example](../examples/open-evaluation/TypeLambdaQuote.hs).

Job 1902 compared the published 577 reader and the quoted-lambda
reader in separate executables. The shared examples had identical
bytes. Both readers decoded each other's examples. Only the newer
reader decoded the additional `LamT` example. The old reader returned
`Unknown encoding for constructor`. The ordinary quote checks use the
external interpreter that matches the compiler.
[Binary samples](../../../testsuite/tests/dependent/should_run/NativeTypeCaseRetainedThWire.hs).

This follows the Template Haskell translation of a quoted binder
into a fresh name in the quotation monad. The callback's kind stays
subject to the ordinary checks at the splice site.
[Sheard and Peyton Jones, Template Meta-programming for Haskell, §5](https://doi.org/10.1145/636517.636528).


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
