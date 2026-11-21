# R41. Nested quantification in GADTs

- [x] Accept consecutive constructor contexts before the first field.
- [x] Accept a terminal group of required universal type arguments after prefix fields in a lifted constructor with distinct result parameters and no constructor context or existentials.
- [x] Accept a terminal required group when an earlier context supplies a dictionary or equality.
- [x] Quote, reify, and splice a declaration with a terminal required group. Preserve its field bangs and multiplicities.
- [x] Apply a constructor with a specified type argument and its context after a field. Match its field before introducing the dictionary.
- [x] Match an existential field followed directly by a constructor context.
- [x] Match a second field under a context placed between two prefix fields.
- [x] Place a specified forall between two prefix fields, including existential binders.
- [x] Repeat the specified forall and field groups.
- [x] Interleave specified forall groups, contexts, and fields in a lifted prefix constructor.
- [ ] Support the remaining orders of fields, contexts, and quantifiers in prefix constructors.

The [Showcase](../Showcase.hs) defines a constructor with fields of
types `a :~: Int` and `a`, followed by `forall b ->`. The function
`columnCount` matches a `ColumnSample a a`. Matching the first field
against `Refl` establishes `a ~ Int`. GHC can then check the last
pattern, `(type Int)`, and return the middle field as an `Int`. The constructor `GivenCount`
supplies the same equality from its context.

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


[QuotedColumn](../examples/open-evaluation/QuotedColumn.hs) splices a
constructor declaration with a strict, unrestricted field before a required
type argument. Its consumer takes the column linearly and returns the
field twice. The [interface test](../../../testsuite/tests/gadt/TerminalGadtTh.hs)
also reconstructs a declaration from an imported constructor.

The `Labelled` constructor in the Showcase takes an `Int`, then a
specified type argument and its `Eq` dictionary. Its consumer uses that
dictionary when it matches the constructor. A point-free binding can
keep the complete constructor type. The registered
[client](../../../testsuite/tests/gadt/GadtTrailingContext.hs) supplies
a custom `Eq` instance and tests when a strict field is forced.

The Showcase also defines `EqSample :: a -> Eq a => EqSample`. A
consumer compares the hidden value with itself. It observes the usual
`Bool` and `Double` equality rules, including the result for NaN.


The Showcase's `InteriorCount` puts `a ~ Int` between two fields of
type `CountField a`. The equality is in scope when the second pattern
checks `(type Int)`. The first field remains lazy. The registered
[client](../../../testsuite/tests/gadt/GadtInteriorContext.hs) also
checks fallthrough and saturation of a strict first field. The
[scope test](../../../testsuite/tests/gadt/GadtInteriorContextScope.hs)
rejects the same type pattern in the first field at a rigid index.

This slice places one context between two fields of a lifted prefix
constructor. All type binders precede the fields. Its interface records
the source order over the existing type binders, predicates, and
fields. The worker still takes evidence before its representation fields.
Template Haskell can reify the wrapper type and preserve the context
in the declaration suffix.

A specified forall can also appear between two fields of a lifted
prefix constructor with no constructor context. For example,
`Box :: !Int -> forall a. a -> Box a` determines `a` from the
result. `Pack :: !Int -> forall a. a -> Pack` hides `a`
existentially. The [client](../../../testsuite/tests/gadt/GadtInteriorForAll.hs)
uses both forms. It supplies the type argument after the first value
and tests partial application of the strict prefix. In a pattern,
`Pack n @a x` introduces `a` for the rest of the pattern and
the branch. Evidence from the first field is available when GHC
checks the type pattern. The
[scope test](../../../testsuite/tests/gadt/GadtInteriorForAllScope.hs)
checks that the binder cannot scope over the first field.

Template Haskell preserves this constructor type through `reifyType`.
A declaration quote or reification uses `RetainedGadtC` for the
prefix and stores the specified forall and final field in its
`Type` suffix. This keeps the prefix bang and the final field's
multiplicity when the declaration is spliced.

A prefix constructor can repeat the specified forall and field group.
In `Parcel :: !Int -> forall a. a -> forall a. a -> Parcel`,
the two `a` binders are distinct. The
[client](../../../testsuite/tests/gadt/GadtRepeatedForAll.hs)
supplies different types for the two binders. It also checks that the
strict first field is forced at saturation and that matching leaves
its unused lazy siblings unevaluated. The
[TH test](../../../testsuite/tests/gadt/GadtRepeatedForAllTh.hs)
quotes and reifies multiple groups and splices them with their
original type variable scope and field multiplicities.

A prefix constructor can interleave its specified foralls and contexts.
For example, `Mixed :: !Int -> forall a. Eq a => a -> Mixed a`
binds `a` and supplies its dictionary after the first field. The
[client](../../../testsuite/tests/gadt/GadtOrderedTelescope.hs) also
uses a context before a later forall and checks a constructor with
several context boundaries. Earlier field patterns can supply evidence
for a later context. A field pattern cannot use evidence from a
context that follows it.
