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
- [x] Preserve both a leading and a terminal constructor context.
- [x] Place a required forall between two prefix fields in a lifted constructor.
- [x] Follow a terminal required forall with a constructor context.
- [x] Place inferred and specified type variables in the same forall between prefix fields.
- [x] Preserve a lifted prefix field's bang after a nested quantifier through quotation, reification, and splicing.
- [x] Match an existential field under a context between two prefix fields.
- [x] Place a context and a later inferred and specified group after a leading required binder.
- [x] Quote, reify, and splice that leading-required telescope with its field bangs.
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

The T12087 constructor `MkF2 :: Ord a => a -> Eq a => F2 a`
keeps the two contexts on either side of its field. The same ordered
telescope lets a view pattern use an early `Ord a` dictionary.
A terminal equality is available in the branch after the field
has matched. The source-hidden client and Template Haskell tests
exercise both context positions.

A required forall can also appear between fields. In
`Required :: Int -> forall a -> a -> Required a`, the result
determines `a`. The call `Required 41 (type Bool) True` supplies it
after the first field. In `RequiredPack :: Int -> forall a -> a ->
RequiredPack`, the type is existential. The pattern
`RequiredPack n (type a) (x :: a)` binds `a` after the first field
and scopes it over the second field and the branch. An equality
from an earlier field is in scope when the required type pattern
is checked. The [interface client](../../../testsuite/tests/gadt/GadtInteriorForAll.hs)
checks both forms. The [TH test](../../../testsuite/tests/gadt/GadtInteriorForAllTh.hs)
round-trips the required group as `ForallVisT`.

In `TypeTag :: Int -> forall a -> Typeable a => TypeTag`, the
required type and its dictionary follow the field. A pattern of
the form `TypeTag n (type a)` can use the dictionary in the branch.
An earlier field pattern cannot use it. The
[interface client](../../../testsuite/tests/gadt/TerminalGadtContext.hs)
constructs and matches both an existential and a result-determined
type with `RequiredTypeArguments` disabled in the client.

An invisible group after a field can mark some binders as inferred.
For `InferredPair :: Int -> forall {a} b. Show b => (a, b) -> InferredPair`,
`InferredPair 61 @Bool ('x', True)` fixes `b` to `Bool`. The
`InferredPair n @b (_, x)` pattern also binds the specified
variable. Quotation and reification preserve both binder flags.
[Explicit specificity proposal](https://ghc-proposals.readthedocs.io/en/latest/proposals/0099-explicit-specificity.html),
[interface client](../../../testsuite/tests/gadt/GadtOrderedTelescope.hs),
and [TH test](../../../testsuite/tests/gadt/GadtOrderedTelescopeTh.hs).

An interface with an inferred binder in a group after a field carries
the `inferred-ordered-gadt-binders` feature in its version header.
Older readers reject the header before reading the constructor. At
O0 and O2, ordinary and specified-only controls retained byte-identical
interfaces at all three tested compression levels. The new reader also
accepts the earlier, unmarked inferred group. Rebuilding the provider
writes the new header. The marker leaves the payload encoding unchanged.
[Writer and reader test](../../../testsuite/tests/gadt/GadtInferredTelescopeWire.sh).

A field after a nested quantifier can be strict. For
`Late :: Int -> forall a -> !a -> Late`, a partial application leaves the
first field lazy. Saturating the constructor forces the last field.
Template Haskell uses the ordered list in `RetainedGadtC` to keep the
later field's bang and multiplicity through quotation and reification.
[Late-field test](../../../testsuite/tests/gadt/GadtLateFieldTh.hs).

A prefix constructor can pass an existential type from an initial
binder through an interior context. In
`C :: forall a. Int -> Eq a => a -> T`, the first field is matched
before the dictionary enters scope. The [existential context test](../../../testsuite/tests/gadt/GadtInteriorContext.hs)
uses the dictionary at the later field.

The [Showcase](../Showcase.hs) quotes and splices a constructor with
a leading required binder, followed by `Eq a` and a later binder group.

```haskell
QuotedComparison :: forall a -> Eq a => forall {b} c. Show c
                 => a -> !(b, c) -> QuotedComparison
```

The call supplies `a` with `(type Int)`, and `c` with `@Bool`. The
field types infer `b`. The consumer uses each dictionary in its field
pattern. The [source-hidden TH test](../../../testsuite/tests/gadt/GadtLeadingRequiredTh.hs)
also reifies and splices the declaration. It checks that the partial
application is lazy and that supplying the last field forces it.

The interface header marks two orders that earlier readers reject. These
are an existential binder in the interior-context format and a binder
group after a leading context and before the first value field. The
`extended-ordered-gadt-telescope` marker lets an earlier reader reject
the interface before it starts an external Template Haskell splice.
The existing payload encodings stay the same.
[Writer and reader test](../../../testsuite/tests/gadt/GadtExtendedOrderedWire.sh).

## A context before an invisible binder and the first field

Proposal #402 permits this prefix constructor with ordinary GADT syntax.
The published compiler at `eda3895806` rejects the signature with
GHC-71492 at the suffix starting with `forall b.`.

```haskell
data Box a where
    K :: forall a. Eq a => forall b. a -> forall c. b -> c -> Box a
```

Moving `b` into the outer `forall` makes the constructor compile.
Moving it beside `c` after the first field also works. Changing only
the outer binder to `forall a ->` works with `RequiredTypeArguments`.
These controls isolate the order of the binder and constraint.

With the invisible outer binder, the parser separates `Eq a` from
the following quantifier. With the required outer binder, it keeps
them together. The former suffix then fails the renamer's check.
Job 849 compared six source orders at `-O0` and `-O2`.
For each accepted order, a client imported the stripped interface,
constructed a value, and used its `Eq a` dictionary in a view pattern
on the first field. It distinguished an ordinary `Double` from NaN.
[Proposal #402 grammar](https://ghc-proposals.readthedocs.io/en/latest/proposals/0402-gadt-syntax.html#grammar-specification)
and [parser decomposition](../../../compiler/GHC/Hs/Type.hs).

### Which prefix orders work?

Fix the outer binder at `forall a.` and the result at `T a b c`.
Between them, put one constraint (`Eq a =>`), one field (`a %1 ->`),
one invisible binder (`forall c.`) and one required binder
(`forall b ->`). All 24 orders keep each variable in scope. Proposal
#402 permits all 24. Each client constructs a value from the stripped
provider interface. It then matches the constructor and uses its
dictionary to compare the field with itself. On inputs `3` and NaN,
the expected results are `(True,False)`.

| Compiler | Field | Orders that compile and run |
| --- | --- | ---: |
| Published R19 compiler, `4e0c9b393a` | `a %1 ->` | 8/24 |
| Private parser and Template Haskell repair, `365f7403ce` | `a %1 ->` | 13/24 |
| Same repair | `a ->` | 13/24 |
| Private ordered-suffix repair, `dc7c0472b6` | `a %1 ->` | 24/24 at both `-O0` and `-O2` |
| Same repair | `a ->` | 24/24 at both `-O0` and `-O2` |

Jobs 866 and 869 tested each order separately at `-O0` with Core and
STG Lint. On `365f7403ce`, the same eleven orders fail with either field multiplicity.
Eight report GHC-71492 for a nested group. Three report GHC-51580 for
a required binder. For example, that revision rejects
the first `forall` after the field in this signature.

```haskell
data T a b c where
    K :: forall a. Eq a => a -> forall b -> forall c. T a b c
```

The missed orders put a quantifier or constraint after the last field.
The ordered constructor checker had required another field in the suffix.
The repair at `dc7c0472b6` lets the ordered suffix end with these groups.
It preserves the existing path for terminal groups that already worked.
The 96 construction and pattern checks above used a relinked compiler.
Job 879 found a separate interface failure with no value fields.
The same compiler accepted `K :: forall a. Eq a => forall b. T a b`
in a provider. Its interface reader then rejected the ordered group
when a Template Haskell client reified the constructor. The repair at
`7df3cd2a79` marks that encoding in the interface header. A new reader
can reify and splice the fieldless constructor. An old reader reports
the unsupported encoding before starting the splice interpreter. Job
890 also found four existing constructor interfaces byte-identical at
both optimization levels. Job 901 passed twelve registered GADT tests
with the relinked compiler. The two-stage build uses `9884c35ff0`.

The repair at `365f7403ce` also preserves the earlier `Box` example through
quotation, interface-only reification and splicing at `-O0` and `-O2`.
The 24-order test covers one field and one occurrence of each binder
form. Repeated groups and additional fields have separate scope and
demand tests above.
[Prefix and record grammar](https://ghc-proposals.readthedocs.io/en/latest/proposals/0402-gadt-syntax.html#grammar-specification).
