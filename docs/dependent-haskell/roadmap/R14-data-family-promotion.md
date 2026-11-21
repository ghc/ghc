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
constructor with no context, existential, index refinement, or retained
binder. Its fields are lifted. The module that names the constructor in
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
no nominal equality between those types. Constructors with constraints,
index refinements, or a newtype representation still give GHC-64578 in
this slice. A type declaration that needs a constructor from an unchecked
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

## Next acceptance check

In a separate module, `track :: foreach (t :: Tuple Bool Bool) -> At t`
currently fails with GHC-83605 at the retained domain. Accept that
domain and check `track (Tup2 True False) :: At ('Tup2 'True 'False)`
using the exact imported instance. A different field must fail. This
requires the retained constructor path to account for the checked
runtime wrapper and its representation coercion.
