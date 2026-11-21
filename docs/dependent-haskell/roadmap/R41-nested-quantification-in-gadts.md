# R41. Nested quantification in GADTs

- [x] Accept consecutive constructor contexts before the first field.
- [x] Accept a terminal group of required universal type arguments after prefix fields in a lifted constructor with distinct result parameters and no constructor context or existentials.
- [ ] Accept a terminal required group when an earlier context supplies a dictionary or equality.
- [ ] Support the remaining orders of fields, contexts, and quantifiers in prefix constructors.

The [Showcase](../Showcase.hs) defines a constructor with fields of
types `a :~: Int` and `a`, followed by `forall b ->`. The function
`columnCount` matches a `ColumnSample a a`. Matching the first field
against `Refl` establishes `a ~ Int`. GHC can then check the last
pattern, `(type Int)`, and return the middle field as an `Int`.

Proposal #402 specifies free order for prefix constructor signatures.
Its grammar includes both forms of `forall`. The required form uses
`RequiredTypeArguments`. During review, Simon Peyton Jones asked how
a change to argument order would affect pattern checking. Vladislav
Zavialov made the free-order requirement explicit in the specification.
[Grammar](https://ghc-proposals.readthedocs.io/en/latest/proposals/0402-gadt-syntax.html#grammar-specification)
and [review](https://github.com/ghc-proposals/ghc-proposals/pull/402#discussion_r575067219).

The wrapper accepts arguments in source order. The worker quantifies
its type variables first. A stripped interface records the terminal
group so an importing module reconstructs the wrapper and promoted
constructor in source order. The registered test checks construction,
matching, promotion, and the demand on a strict versus a lazy field
when the constructor is partially applied.
[Provider](../../../testsuite/tests/gadt/TerminalGadtForAllProvider.hs)
and [client](../../../testsuite/tests/gadt/TerminalGadtForAll.hs).
