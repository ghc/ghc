# R14. Data family promotion

The Serokell card is unmarked and has no outgoing dependency edge.
GHC's DataKinds guide excludes data family instance constructors.
[Serokell card](https://ghc.serokell.io/dh#data-family-promotion)
and [GHC User's Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html#overview).

## Simple instance constructors

Discussion #456 uses a kind-indexed tuple family with instances at
different arities. The author wants the existing class-instance behavior
of data families and the ability to use their constructors in types.
Peyton Jones explains how a term constructor wraps its representation
worker with the data family instance coercion. Zavialov's follow-up
reports that Eisenberg's concern about abstracting over a coercion
applies to constructors with constraints. The discussion proposes a
direction and leaves the general design open.
[Discussion #456](https://github.com/ghc-proposals/ghc-proposals/discussions/456)
and [GHC issue #15245](https://gitlab.haskell.org/ghc/ghc/-/issues/15245).

The experiment admits a checked, lifted data instance
constructor with no stored context, existential, or retained binder. Its fields are lifted. The module that names the constructor in
a type enables `ExperimentalDependentTypes` and uses the normal
`DataKinds` syntax. The data instance can come from an ordinary module.

```haskell
data family Tuple :: k
data instance Tuple (a :: Type) (b :: Type) = Tup2 a b
```

A separately compiled module can use `'Tup2 'True 'False` at kind
`Tuple Bool Bool` and match it in a closed type family equation. The test
checks exact equality with `Refl` and reflects the public constructor
with `Type.Reflection.typeRep`. It rejects a wrong result kind, wrong
field equality and wrong field coercion. An ordinary Haskell2010 client
reads the exported proof and type alias at O0 and O2. The previous
public reader also reads them through the existing interface encoding.
Direct promotion in source still requires the experimental flag.
[Provider and clients](../../../testsuite/tests/dependent/should_run/data-family-promotion)
and [test runner](../../../testsuite/tests/dependent/should_run/NativeDataFamilyPromotion.sh).

The promoted constructor uses the original DataCon name and its public
result kind. The runtime wrapper still uses the existing representational
axiom between the family and its representation type. That axiom supplies
no nominal equality between those types. Constructors with a stored
context, existential binder, or newtype representation still give
GHC-64578 in this slice. A type declaration that needs a constructor from an unchecked
same-group data instance also gives GHC-64578.
[Promotion check](../../../compiler/GHC/Tc/Gen/HsType.hs)
and [instance placeholders](../../../compiler/GHC/Tc/Utils/Env.hs).

Agda and Idris both let an indexed datatype constructor target a
particular index. Their vector constructors illustrate the direct use
of such values in dependent programs. Haskell's open data family
instances add a separate representation boundary.
[Agda indexed datatypes](https://agda.readthedocs.io/en/latest/language/data-types.html#indexed-datatypes)
and [Idris vectors](https://idris2.readthedocs.io/en/latest/tutorial/typesfuns.html#vectors).

The [Showcase](../Showcase.hs) reduces
`SecondFlag ('Tup2 'True 'False)` to `'False`. GHC checks the result with
`Refl`.

## Opaque retained domains

A signature can retain a value at a lifted data family application.
`reflexive :: foreach (t :: Tuple Bool Bool) -> t :~: t` needs the
identity of its local argument. It can preserve that identity without
selecting a representation or demanding the value. An ordinary client
calls the function through a stripped interface. The test also forwards
two distinct field pairs and rejects equality between independent
arguments. A stuck ordinary type family still fails the domain check.
[Retained-domain test](../../../testsuite/tests/dependent/should_run/NativeDataFamilyRetainedDomain.sh).

Proposal #378 §§1.4.6–1.4.8 gives local term variables unique identity
and requires an explicit dependent signature. Its Static Subset can
admit a written constructor separately from a local variable. Discussion
#456 explains the representation cast needed by the term constructor.
That cast has representational role.
[Dependent type design](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html#term-variables-in-types)
and [data family discussion](https://github.com/ghc-proposals/ghc-proposals/discussions/456).

## Written constructor operands

An ordinary client can call an imported dependent function with
`Tup2 True False`. The result can name the exact promoted constructor
at the public kind `Tuple Bool Bool`. The check accepts the constructor
wrapper or its worker with the matching data family instance axiom. It
checks both endpoints at representational role and uses the public
constructor directly as the type index. It keeps the original runtime
cast and preserves lazy fields. The [Showcase](../Showcase.hs) names
the exact retained pair in the result type of a call to
[`readPair`](../examples/open-evaluation/RetainedFamilyTuple.hs).

An optimized unfolding that contains a retained data family constructor
operand uses the `retained-data-family-constructor-operand` interface
marker. It can also carry the Natural literal marker. The current reader
checks the constructor fields against the promoted telescope, including
fields that refer to local runtime binders. An older reader refuses the
new marker at the interface header. The test checks the nominal result,
an ordinary source-hidden client, an undemanded field, and rejection of
wrong fields and strict constructors.
[Constructor operand test](../../../testsuite/tests/dependent/should_run/NativeDataFamilyConstructorOperands.sh).

## Result refinements

```haskell
data family C a
data instance C (a, b) where
    Refines :: C (Int, b)
```

The promoted constructor has result kind `C (Int, b)`. GHC uses the
source telescope of the GADT constructor, so the refined parameter
appears directly in the result kind. A separately compiled closed type
family can match `'Refines` and prove its result with `Refl`. Using
`'Refines` at kind `C (Char, Bool)` fails.

An ordinary client can also pass `Refines` to an imported
`foreach (t :: C (Int, Bool))` function. The worker receives the
refined type argument and an exact nominal reflexive proof. The
wrapper still applies the separate representational instance axiom.
The interface reader checks the same operand shape for an optimized
unfolding.
[Promotion and retained-operand tests](../../../testsuite/tests/dependent/should_run/data-family-promotion).

## Next acceptance check

Specify how a promoted family constructor with a stored context can
bind and expose its evidence in a type. Discussion #456 identifies the
coercion binder question for constrained constructors.
[Discussion #456](https://github.com/ghc-proposals/ghc-proposals/discussions/456).
