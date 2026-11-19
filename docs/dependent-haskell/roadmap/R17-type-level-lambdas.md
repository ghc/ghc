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

M174 checks this source in a top-level synonym with
`ExperimentalDependentTypes` and `DataKinds`. The module that defines
`Map` enables `TypeFamilies`. An ordinary client
can import `L` and check the same proof with only the interfaces.
The list argument can also fix the kinds of an identity lambda over
Booleans and a callback whose result has kind `Bool`. Other checks
cover a captured variable, a shadowed binder, a rejected matchable
arrow, and the module's implicit export list.

A lambda can bind one or more ordinary type variables. An enclosing
synonym can infer its parameter kinds. The renamer accepts lambdas at the
root of the synonym body or under application, parentheses, and kind
ascription. Other positions report GHC-27185. With the extension
disabled, the diagnostic is GHC-27184 at the lambda.

Adjacent lambdas share one family telescope. A lambda nested in an
application gets its own family and captures the enclosing equation's
binders. Both preserve lexical shadowing. An inner `q` can shadow an
outer `q` while a captured `a :: q` keeps its original kind. A formerly
implicit synonym kind variable can also appear explicitly in the lifted
body. An ordinary interface-only client checks these cases. Nested
family applications remain subject to the usual termination checks.

The compiler lifts each lambda to a private closed family. The
declaration group checks the synonym and generated family together.
Captured variables become parameters. Saturation exposes the family
equation and supplies nominal evidence for beta reduction. A constant
lambda can discard its argument, so its result cannot establish
equality between the arguments of two calls.

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

## Prior art

Agda uses ordinary lambda syntax and accepts unannotated binders when
checking an anonymous function. The surface spelling above comes from
Serokell. [Agda lambda abstraction](https://agda.readthedocs.io/en/v2.8.0.2/language/lambda-abstraction.html).

Eisenberg and Stolarek lift a lambda to a fresh family and pass its local
variables as parameters. Their implementation in `singletons` uses
defunctionalization for partial application. The fork can pass the family
itself at an unmatchable arrow. [Promoting Functions to Type Families in
Haskell, §4.6](https://richarde.dev/papers/2014/promotion/promotion.pdf)
and [GHC proposal 242](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html).
