# R04: Dependent sums

The second field of a dependent pair has a type that depends on the value
of the first field. Serokell lists dependent Core (R05) and existential
quantification (R22) as dependencies. R04 retains the witness at runtime.
An erased existential can hide an index without providing its value.
[Serokell roadmap](https://ghc.serokell.io/dh#dependent-sums) and
[first-class existentials proposal](https://github.com/ghc-proposals/ghc-proposals/pull/473).

## What source programs can express

An ordinary module can return a list's computed length with a vector of
that length. The provider exports `type ListVector a = some (n :: Nat) | Vec a n`.
The client can use that signature directly.

```haskell
fromList :: [a] -> ListVector a
fromList xs = Pair (sourceLength xs) (sourceVector xs)
```

The expected pair type supplies the witness domain `Nat`. GHC checks
the vector against the length of the same list. Using a second unknown
list for the vector fails at the call. Opening the pair can return the
first element without reading the tail. Reading the length leaves
the elements lazy. [Ordinary client](../examples/packed-vector/ListVectorClient.hs)
and [source and demand checks](../../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh).

A package can abstract over the type of the witness as well as the payload
family. The ordinary client uses the same constructor at `Bool`, `Int`,
`Int -> Int` and `IO Int`. Reading a constant payload leaves its
witness unevaluated. At `Bool`, GHC rejects a payload indexed by
`False` when the supplied witness is `True`. A consumer can bind
the stored index and use the payload at that index.
[Pair and eliminator](../examples/packed-vector/DependentPair.hs)
and [separate client](../../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh).

The second projection has type `foreach (q :: DPair a p) -> p (first q)`.
The ordinary first projection supplies its equation through the pair
module's interface. The compiler substitutes the witness type into the
kind of the payload family. Changing the first projection to force
the payload invalidates the equation. [Second projection](../examples/packed-vector/DependentProjection.hs).

An older compiler that accepts `DPair` can leave its first projection
without an equation. The `--abstract-witness` upgrade check rebuilds
the pair owner and a separate worker that renames its type parameters.
A fixed-domain worker and an unrelated module keep their objects and
interfaces. A second build reuses all outputs.
[Upgrade test](../examples/closed-evaluation/check-readback-upgrade.sh).

A GADT can already store a runtime length and a vector indexed by that
length. The same encoding works for a family parameter `p`.

```haskell
data Retained (p :: Nat -> Type) where
  Retain :: foreach (n :: Nat) -> p n -> Retained p

withRetained :: Retained p -> (foreach (n :: Nat) -> p n -> r) -> r
withRetained (Retain n value) k = k n value
```

The consumer chooses a single result type `r`. Inside the match it can
use the payload at the stored index. Stage1 compiled the polymorphic
consumer with Core and STG lint. The program read a constant payload
with an exceptional witness. It also read a zero witness with an
exceptional payload. [Generic package](../examples/packed-vector/RetainedNat.hs).
The [Showcase](../Showcase.hs) uses the same
eliminator with both `Vec Char` and `Const Int`. [Existing vector package](../examples/packed-vector/PackedVecLibrary.hs).

The imported selector `getConst` can infer the erased index from its
argument. An ordinary client can use `withRetained p (\_ c -> getConst c)`
without writing a type application. A selector whose result still depends
on the private index cannot export that index as a caller-chosen type.
The callback can also use `case c of Const x -> x`. Checking the first
argument of `withRetained` fixes the payload family to `Const Int`.
The pattern checker reads the actual index from the scrutinee. A
constructor pattern that returns a vector at a caller-chosen index
still fails.
[Interface and escape controls](../../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh).

## Project a payload at the length of its input package

Agda's `Σ` record gives the second projection a result type indexed by
the first projection of the same pair. The corresponding source test
for the fork uses the existing `PackedVec` type and an ordinary length
projection. [Agda's built-in Sigma record](https://agda.readthedocs.io/en/v2.8.0/language/built-ins.html#the-s-type)
and [Eisenberg's thesis, §8.8](https://www.cis.upenn.edu/~sweirich/papers/eisenberg-thesis.pdf#page=220).

```haskell
payload :: foreach (p :: PackedVec a) -> Vec a (packageLength p)
payload (PackVec _ xs) = xs
```

With an explicit closed `Length` family, the compiler can check
`payload :: foreach (p :: PackedVec a) -> Vec a (Length p)`.
Matching `PackVec` supplies nominal evidence about the actual input.
An ordinary interface-only client can return its vector as a list.
Asking for `Vec a ('S (Length p))` fails at the returned field.
A caller that demands the payload encounters an exception in that
field even if the stored length also raises an exception.
[Projection source](../examples/packed-vector/PackedProjection.hs).

The exported `payload` uses an ordinary imported `packageLength` in its
result type. The compiler checks the worker's case and publishes the
equation that returns its first field. It binds the witness before
checking the kind of the payload. A worker that first forces the vector
has different demand and cannot supply that equation. A rewrite rule
that changes the returned length also prevents its use.
[Ordinary worker](../examples/packed-vector/PackedLength.hs).

- [x] Represent a single retained witness before a payload whose final
  type argument is that witness in the promoted constructor kind.
- [x] Relate the length of the input package to the field bound by the
  match. Only the input package remains in scope in the exported type.
- [x] Check the projection through a compiled interface. Check a
  mismatched index and the two independent field-demand examples.
- [x] Use the ordinary imported first projection in the result signature.

## Abstract over the payload family

The family parameter `p` may itself be a variable. The promoted kind
binds the witness before applying `p` to it. The same `project`
implementation works at `Vec Char` and at `Const Int`.

```haskell
project :: foreach (q :: Retained p) -> p (RetainedWitness.witness q)
project (Retain _ value) = value
```

The checker compares the actual kind of the supplied family with its
declared kind. It requires a lifted payload type. This check preserves
the matchability of the family argument. The result still depends on the
caller’s package when the family is unknown.
The ordinary worker matches `Retain` and returns its first field. The
checker publishes its equation with the declared arrow kind of `p`.
A caller can use the project function through its interface with
`ExperimentalDependentTypes` disabled. Returning the successor
instead of the stored witness fails at the payload.
[Generic projection](../examples/packed-vector/RetainedProjection.hs) and
[ordinary witness](../examples/packed-vector/RetainedWitness.hs).

The upgrade checks rebuild an eligible old witness worker once, then
reuse both the worker and the unchanged consumer. An interface-only
old worker cannot supply the missing equation.
[Upgrade runner](../examples/closed-evaluation/check-readback-upgrade.sh).

## Sums in constructor fields

[StoredVector](../examples/packed-vector/StoredVector.hs) uses one strict
record field of type `some (n :: Nat) | Vec a n`. The field captures the
element type from the data declaration. The same form works in positional
and infix constructor fields. The ordinary client can read the vector
after its producer source has been removed.

The strict field evaluates the outer pair to weak head normal form.
GHC keeps the pair boxed because the payload type depends on its first
field. Unpacking an ordinary product requires a flat list of independent
representation arguments. That operation cannot describe this telescope.

A Haskell98 newtype can wrap a retained sum directly in a positional
or record field. The record selector returns the whole pair, including
the runtime witness. A newtype pattern that ignores its field leaves
the pair unevaluated. GHC derives the representation axiom and roles
through its ordinary newtype checks. A type parameter captured by the
generated payload family has a nominal role. An ordinary source-free
client reifies the selector type with Template Haskell and uses it
to coerce the wrapper to its field.
[Registered source test](../../../testsuite/tests/dependent/should_run/NativeScopedSumSignatureReify.sh).

A GADT-style newtype field can use erased binders from its constructor
signature's outer telescope. GHC checks the constructor result kind
and the normal restriction on existential variables and contexts.
The record selector keeps the checked constructor result type and
Template Haskell reifies the constructor as a GADT record.

A GADT data constructor field can use the same outer erased binders.
The constructor can hide one of those binders from its result type.
A nested sum can use its outer witness for two vectors and a separate
inner witness for a third vector.

```haskell
data Nested a where
    Nested :: (some (n :: Nat) |
               (Vec a n, (some (m :: Nat) | (Vec a n, Vec a m)))) -> Nested a
```

This nested field passes the termination check. The generated outer
family forwards its captures to the inner family and leaves the inner
witness argument open. GHC checks that the inner family cannot refer
back to the outer family.

A later field can also use a retained argument of the same constructor.
In `Later :: foreach (n :: Nat) -> (some (m :: Nat) | (Vec a n, Vec b m)) -> Later a b`,
the sum captures `n` where it occurs in the constructor signature. The
private synonym takes `n` as a parameter. Each earlier binder scopes over
later fields. Matching the constructor gives the consumer the same witness.
Returning its vector at a caller-chosen index fails at the returned field.
A later field can leave the earlier witness unevaluated. A GADT field
can also open its own `foreach` telescope. Its witness can shadow one
from the constructor. The field body uses the new witness and the next
constructor field uses the original witness. Use a named sum if a
written binder kind refers to a name hidden by this shadowing.
Lifting stops at shadowing on the constructor's own argument spine.

The constructor result has the enclosing telescope scope. A witness
bound inside a field cannot escape into that result. Every retained field
occurs in a result argument. The argument uses the field directly or under
one unary promoted constructor of the same index type. More than one
parameter can use the same field. The other result arguments must be
distinct, unchanged erased parameters. A retained field is lazy and has
a supported closed structural domain such
as `[Bool]`, `Ordering`, or `Nat`. A variable or wildcard leaves the
field unevaluated. The constructor pattern introduces the nominal index
equalities after the field binders. Code generation erases the equalities.
An ordinary client can rebuild the constructor at its original index and
read the first cell of a runtime list with an unevaluated tail.
In `rebuild (IndexedList xs)`, the nested constructor supplies the
outer call's index. This also works with `ImpredicativeTypes`.
[Inference and prior art](../REFERENCES.md#nested-constructors-with-a-runtime-result-index).
In the Showcase, `adjust :: (Int -> Int) -> Reading b -> Reading b`
changes a measurement and keeps the calibration index. An ordinary
client writes `adjust (+ 1) (Reading flag n)`. The local helper can
infer the types of both arguments. Reading the flag leaves an undefined
measurement unevaluated. Reading the adjusted measurement leaves an
undefined flag unevaluated.
[Direct constructor](../examples/open-evaluation/DirectIndexedList.hs),
[ordinary client](../examples/open-evaluation/DirectIndexedListClient.hs),
and [source-hidden check](../../../testsuite/tests/dependent/should_run/NativeIndexedFin.sh).

An ordinary field can refer to the retained field that determines the
result index. `SomeVec` stores a vector at that length and leaves its
element type polymorphic. An ordinary client opens a runtime vector
package and reads the first element at two element types. Both vector
tails stay unevaluated. GHC reports unsupported result arguments and
omitted retained fields at the written result type.
[Direct constructor](../examples/packed-vector/DirectIndexedVector.hs),
[ordinary client](../examples/packed-vector/DirectIndexedVectorClient.hs),
and [source-hidden check](../../../testsuite/tests/dependent/should_run/NativeGadtRetainedSum.sh).

An ordinary client can match the stored length before reading the
vector. In the successor branch, the vector's index is a successor.

```haskell
firstDirect :: SomeVec a n -> Maybe a
firstDirect (SomeVec Z _) = Nothing
firstDirect (SomeVec (S _) (VCons x _)) = Just x
```

The Showcase calls this function at zero and at a successor. The zero
case leaves its payload unevaluated. The successor case reads a cell
without evaluating its tail.

The outer constructor also relates its public index to the stored
length. The zero branch can return `n :~: 'Z` without evaluating the
payload. A later argument of type `Vec b n` can use the same refinement.
[Ordinary client](../../../testsuite/tests/dependent/should_run/NativeGadtRetainedSumClient.hs)
and [Agda pattern precedent](../REFERENCES.md#refine-the-public-index-of-a-stored-witness).

A single zero clause is exhaustive at `SomeVec a 'Z`. At `SomeVec a n`,
GHC warns about a missing successor clause. An explicit successor clause
at the known zero index has an inaccessible right-hand side. Its pattern
can still force the stored witness.

A constructor can store a vector and give its successor length as the
result index.

```haskell
data SuccVec a (size :: Nat) where
    SuccVec :: foreach (n :: Nat) -> Vec a n -> SuccVec a ('S n)

predecessor :: SuccVec a ('S n) -> Vec a n
predecessor (SuccVec _ xs) = xs
```

`predecessor` uses the nominal equality of the two successor indices.
It can return the payload without evaluating the stored predecessor.
At `SuccVec a ('S 'Z)`, a zero predecessor pattern is exhaustive.
At an unknown result index it needs a successor predecessor pattern too.
[Showcase constructor](../examples/packed-vector/DirectIndexedVector.hs),
[ordinary client](../examples/packed-vector/DirectIndexedVectorClient.hs),
[source-hidden check](../../../testsuite/tests/dependent/should_run/NativeGadtRetainedSum.sh)
and [Agda successor indices](../REFERENCES.md#put-a-stored-witness-under-a-promoted-constructor).

A rectangular matrix stores the row and column lengths independently.
Both lengths index the payload and the result.

```haskell
data Matrix a (rows :: Nat) (columns :: Nat) where
    Matrix :: foreach (rows :: Nat) (columns :: Nat)
           -> Vec (Vec a columns) rows -> Matrix a rows columns
    MatrixByColumns :: foreach (columns :: Nat) (rows :: Nat)
                    -> Vec (Vec a columns) rows -> Matrix a rows columns
```

The second constructor takes the columns first. Its result still lists
the rows first. An ordinary client opens either form and returns
`Vec (Vec a columns) rows`. It can also read either length without
evaluating the other length or the cells. At zero rows and a successor
column count, the matching patterns are exhaustive.
[Source-hidden check](../../../testsuite/tests/dependent/should_run/NativeGadtRetainedSum.sh)
and [Agda indexed datatypes](../REFERENCES.md#store-two-direct-result-indices).

A square stores one side length. Both result parameters use this length.

```haskell
data SquareMatrix a (rows :: Nat) (columns :: Nat) where
    Square :: foreach (side :: Nat)
           -> Vec (Vec a side) side -> SquareMatrix a side side
```

An ordinary client can return `rows :~: columns` from `Square _ _`.
The match needs the outer constructor. It leaves the side length and
the cells unevaluated. At type `SquareMatrix a 'Z ('S 'Z)`, an empty
case is exhaustive and evaluates its scrutinee. An explicit `Square`
alternative at this type has an inaccessible right-hand side. A length
pattern on a square with compatible indices evaluates the stored length.
[Showcase constructors](../examples/packed-vector/IndexedMatrix.hs),
[ordinary Showcase client](../examples/packed-vector/IndexedMatrixClient.hs), and
[source-hidden check](../../../testsuite/tests/dependent/should_run/NativeGadtRetainedSum.sh).

An explicit equality can also describe the exact stored value.
For example, the constructor
`CheckedList :: foreach (ys :: [Bool]) -> (xs :~: ys) -> CheckedList xs`
stores the runtime list `ys`. Constructing it with `Refl` checks the
public index `xs` against that list. [Indexed list](../examples/open-evaluation/IndexedPrefix.hs).
[Agda 2.8.0 telescopes](https://agda.readthedocs.io/en/v2.8.0/language/telescopes.html).
[Interface and demand test](../../../testsuite/tests/dependent/should_run/NativeGadtRetainedSum.sh).

An ordinary caller can use `withElements` to match the stored `Refl`.
The match gives the callback `xs ~ ys` and leaves both fields of the
list unevaluated. Agda's `inspect` similarly keeps the equality between
a computed expression and the result abstracted by `with`.
[Client](../examples/open-evaluation/IndexedPrefixClient.hs),
[source-hidden check](../examples/open-evaluation/check-source.sh),
and [Agda's inspect idiom](../REFERENCES.md#a-list-indexed-by-its-contents).

Agda's dependent pair uses the stored first component to type the
second field. The GHC newtype wraps that pair as a whole.
[Agda's dependent pair](https://agda.readthedocs.io/en/v2.8.0/language/built-ins.html#the-s-type).

## Payload families for the `some` binder

Serokell writes a retained sum as `some (x :: a) | B`. Its payload can
forget the witness. The bootstrap compiler at `60014cbc8e` accepts the
following representation when the constructor specifies both erased kinds.

```haskell
data UPair (a :: Type) (p :: a %U -> Type) where
  MkPair :: forall (a :: Type) (p :: a %U -> Type).
            foreach (x :: a) -> p x -> UPair a p
```

A source-free ordinary client used one payload family that maps `True`
to `Int` and `False` to `Char`. GHC rejected an `Int` payload at
`False` at the payload expression. Another family maps every witness to
`Int`. The client projected `7` at a bottom-valued witness under `-O1`
with Core and STG lint. A type lambda can also apply a captured matchable
constructor, such as `Proxy`, to the witness.

The compiler elaborates a `some` type synonym to `GHC.Sigma.Sigma`.
It checks the payload under the witness binder. The generated payload
family records its lexical owner in the interface. When two modules
write equivalent sums with different binder names, a checked nominal
coercion can establish their equality. A consumer with only the other
module’s interface can use this proof at `-O0` and `-O1`. Changing a
payload or the priority of overlapping branches rejects the proof.
An unrelated nominal family keeps its own identity.

At `a957f54932`, the self-hosted compiler proved this equality for a
source-free client using `GHC.Sigma` from `base`. At `8b7c23d47a`, a
closed-family equation such as `Same a a = Bool` also accepts two such
sums as its arguments. The matcher constructs evidence between the real
argument and the instantiated equation. It still checks apartness before
using a later equation. An opaque result kind or an unrelated nominal
family can leave the application stuck.
[Repeated argument example](../examples/erased-existentials/PackageEquality.hs).
[Serokell’s R04 card](https://ghc.serokell.io/dh#dependent-sums)
and [type-lambda elaboration](R17-type-level-lambdas.md#source-and-evidence).

## Return a dependent sum from a closed type family

A closed family equation can return a sum directly. The second equation
captures `a` from its own pattern. Each equation can choose its own
payload type.

```haskell
type family Pack (select :: Bool) (a :: Type) :: Type where
    Pack False unused = some (tag :: Bool) | Tagged tag Bool
    Pack True a = some (tag :: Bool) | Tagged tag a
```

An ordinary module imports `Pack` and constructs `Pack True Char` with
`Pair True (TrueTag 'x')`. An ordinary constructor pattern can bind the
actual stored witness.

```haskell
size :: Pack True Char -> Bool
size (Pair tag _) = tag
```

GHC follows family equations at the head of the type and composes their
nominal coercions. For example, `Outer select a = Pack select a`
can expose the package through `Pack`. The final equation can return a
sum directly or name a synonym for it. The pattern demands the outer
pair and leaves an ignored payload lazy. A lazy tuple pattern inside an
ordinary payload field keeps that field lazy. A lazy pattern around the
whole package still fails. So does a match on a stuck family or through
a newtype wrapper.

An imported open family instance can also expose the pair. The test
provider declares `type instance OpenPackage Bool = Outer True Char`.
Its separately compiled ordinary client checks the pattern below. The
match demands the package and leaves the ignored payload lazy.

```haskell
sizeOpen :: OpenPackage Bool -> Bool
sizeOpen (Pair tag _) = tag
```

The pattern checker uses the solver's nominal family rule and follows
only heads. `OpenPackage Char` has no matching instance. Its `Pair` pattern
fails at the constructor. Constrained families still need their separate
premise checks.
[GHC's open-family compatibility rules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html#compatibility-and-apartness-of-type-family-equations).

Each step uses the solver's reduction-depth counter. A recursive
family such as `Loop a = Loop [a]` reaches the configured limit and
reports GHC-40404 at the written `Pair` pattern. The test checks the
same limit for an open instance.

GHC rejects a payload for a different tag at the written payload. An equation that
uses a binder from another equation fails at that variable's occurrence.
The regression checks both errors at `-O0` and `-O2`. The client sees
only the provider's interface during typechecking. An explicit `forall`
also retains a kind variable used only in the kind of a captured binder.
[Provider](../../../testsuite/tests/dependent/should_run/NativeClosedFamilySumProvider.hs),
[ordinary client](../../../testsuite/tests/dependent/should_run/NativeClosedFamilySumClient.hs),
and [regression](../../../testsuite/tests/dependent/should_run/NativeClosedFamilySum.sh).

The Showcase reads a retained vector length without evaluating the vector.
[Vector result family](../examples/packed-vector/StoredVector.hs) and
[ordinary length projection](../examples/packed-vector/ListVectorClient.hs).

The interface contains the `Pack` equations in terms of `GHC.Sigma.Sigma`
and a private payload family for each sum. `--show-iface` displays this
representation. GHCi's `:info` also shows the lowered family equations.
The family termination check still applies. An equation
such as `PackOne a = some (tag :: Bool) | Tagged tag a` passes `a` to a
payload family of the same size, so it requires `UndecidableInstances`.
Returning a named sum with the same payload has the same requirement.

Agda can write a function returning a type whose result uses `Σ A B`,
with `B` chosen from the function's arguments. The standard library
provides the same construction with the syntax `Σ[ x ∈ A ] B`.
[Agda's `Data.Product.Base`](https://agda.github.io/agda-stdlib/v2.4/Data.Product.Base.html#37).
Its `uncurry` matches the pair to pass the witness and its dependent
field to a function.
[Agda's dependent pair elimination](https://agda.github.io/agda-stdlib/v2.4/Data.Product.Base.html#126).

## Return a dependent sum from an open type instance

A top-level type instance can write the sum directly, including for a
family imported from another module.

```haskell
type instance Provider.DirectOpen [a] = some (tag :: Bool) | Tagged tag a
```

Here `a` comes from the instance pattern. The witness `tag` scopes over
`Tagged tag a`. Its domain must already be in scope. The ordinary
instance checks still apply, including compatibility and termination.
Keep the usual `data instance` syntax for data families.

Each equation has a private payload family in the instance module.
An ordinary client sees the instance through its interface and can
construct or match the result. Two instances of one family keep separate
payload families. The regression also captures a variable whose kind
depends on an explicit `forall` binder.
[Instance provider](../../../testsuite/tests/dependent/should_run/NativeClosedFamilySumChain.hs)
and [ordinary client](../../../testsuite/tests/dependent/should_run/NativeClosedFamilySumClient.hs).

## Return a dependent sum from an associated equation

An associated type instance can use the enclosing instance variable
`a` alongside the equation's own `b`.

```haskell
instance forall a. Build [a] where
    type forall b. Result [a] b = some (tag :: Bool) | Tagged tag (a, b)
```

Both variables occur on the family left-hand side. GHC still rejects
an instance-head variable that the family pattern omits. The test also
captures an equation variable whose kind depends on another binder.
[GHC's scope rules for associated instances](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html#scoping-of-class-parameters).

A default has its own scope. In a class with `type Defaulted a`, the
default `type Defaulted b = some (tag :: Bool) | Tagged tag b` captures
`b` from its pattern. GHC checks the default before it instantiates it
in another module. The default still requires distinct variable
patterns.
[GHC's associated default rules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html#associated-type-synonym-defaults).

The ordinary client matches `Pair` on the result of an ordinary
associated instance. The class declaration lives in a module that uses
ordinary `TypeFamilies`. This fork marks the family declared under
`ExperimentalDependentTypes` as constrained.
[Instance and default provider](../../../testsuite/tests/dependent/should_run/NativeAssociatedSumProvider.hs)
and [interface-only client](../../../testsuite/tests/dependent/should_run/NativeAssociatedSumClient.hs).

## Match a constrained associated family result

An ordinary client can use `first` or match `Pair` on the same
`Defaulted Char` result. The class instance provides the domain for
the default. A separate test matches a result that passes through
two constrained families. The GADT pattern `Permit` gives the client
the required `Allowed a` dictionary.

```haskell
readGiven :: Permit a -> Proxy a -> Bool
readGiven Permit ix = case make ix of Pair tag _ -> tag
```

GHC checks each domain in scope at the `Pair` pattern before it
reduces that family head. For `make (Proxy :: Proxy Int)`, the
scrutinee type is inferred and GHC reports the missing `Allowed Int`
at `Pair`, including with `-fdefer-type-errors`. Reduction must
reach the package constructor's parent. A stuck application or a
newtype around a package still fails at the pattern.

The match forces the outer pair and keeps an unused payload lazy.
The regression compares a named sum and direct `some` at `-O0`
and `-O2` with the provider source hidden from the client.

An ordinary reader can also use a nominal equality to expose the
family equation. The same match works after a GADT pattern supplies
that equality.

```haskell
readRefined :: (a ~ Bool) => Shape a Char -> Bool
readRefined pkg = case pkg of Pair tag _ -> tag
```

If lookup gets stuck, GHC rewrites family-free arguments with the
available family-free nominal equalities. It tries that head again
with the same domain check. A representational `Coercible a Bool`
constraint still leaves `Shape a Char` stuck.

The equality can also occur between GADT fields. An ordinary client
uses it to open the later package. The earlier field and the unused
package payload can stay unevaluated. Moving the package field before
the equality leaves its `Choice a` domain unsatisfied.

```haskell
data Staged a where
    Staged :: Proxy a -> (a ~ Bool) => Shape a Char -> Staged a

readStaged :: Staged a -> Bool
readStaged (Staged _ pkg) = case pkg of Pair tag _ -> tag
```

[Provider](../../../testsuite/tests/dependent/should_run/NativeConstrainedSumProvider.hs)
and [ordinary client](../../../testsuite/tests/dependent/should_run/NativeConstrainedSumClient.hs).

## Construct a retained pair in an ordinary argument

An ordinary client with `ImpredicativeTypes` can construct a package at a
known result type, including an imported open family that reduces to a
sum. For example, `consume (Pair True payload)` uses the argument type
of `consume` to select the erased parameters of `Pair`. GHC checks
`payload` against the written witness and keeps an ignored field lazy.
This Quick Look path follows unconstrained family heads. A stuck
family leaves the constructor application unresolved.

## Partition at the original length

The [vector partition](../examples/packed-vector/ConservedPartition.hs) returns
two nested `some` packages. Its payload contains the kept vector, the
dropped vector, and a proof that their lengths add up to the input length.
The [ordinary caller](../examples/packed-vector/ListVectorClient.hs) opens
both packages and appends the vectors at the original length. Each
partition preserves input order. The partition traverses the finite vector
and tests each element. The elements can stay unevaluated when the
predicate ignores them.
[Agda proof and Idris vector partition](../REFERENCES.md#conservation-of-partition-length).

An ordinary checked-split pattern can name the stored suffix in
`(right :: Vec a n)`. The type name `n` refers to the same witness as
the stored field. After matching `Refl`, a later pattern can use
`Vec a ('S n)`. A lazy tuple can bind the alias and defer the proof
match. Separate packages still have separate witnesses. At the
declared original length, `appendVec left right` takes both erased
indices from the operands. Quick Look also accepts
`toList (appendVec left right)` when the declared operands fix every
type argument of the inner call. Separate family variables can still
need an explicit argument.
[Ordinary client](../examples/packed-vector/NamedSplitClient.hs) and
[pattern signature rules in proposal #128](https://ghc-proposals.readthedocs.io/en/latest/proposals/0128-scoped-type-variables-types.html).


`choosePartition` gives an imported `Branch a r` continuation the selected
length and vector. It leaves the other vector and the equality proof
unevaluated. Its required type argument supplies `total` when the result
`r` is independent of length. For an empty input, write
`choosePartition (type 'Z)`. GHC cannot infer `total` from `Split a total`
alone. A continuation that consumes the sum equality also works if its
type fixes `total`, or if the expected result fixes it, as in `Vec a total`.

The Showcase passes a list read through an `IORef` to `fromList` and opens
the result as `Pair total values`. With dependent syntax enabled it can
write `choosePartition (type total)` to select either output. An ordinary
module can instead call a local worker with signature `forall n. Vec a n
-> r` on `values` and pass `(type n)` inside that worker. Naming the term
binder `total` in a type in the ordinary module produces GHC-45510.
In both calls the required type argument is erased.
[Runtime example](../Showcase.hs) and [ordinary selector](../examples/packed-vector/ListVectorClient.hs).

## Scope a split proof from IO

The ordinary `checkedSplitIO` caller takes a split from an IO action and
a second vector at the same, abstract total length. It matches the zero
suffix and the proof in `Pair Z !(left, right, Refl)`. The proof refines
the second argument to a one-cell vector. GHC checks function patterns
from left to right, so the reversed order fails.
[Ordinary caller](../examples/packed-vector/ListVectorClient.hs),
[GHC's GADT pattern rules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/gadt.html)
and [proposal #378 on dependent patterns](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html#dependent-pattern-match).

A lazy tuple can bind the proof as a value while leaving the tuple
unevaluated. To refine the vector, match that value with `case` first.
Putting `Refl` under the lazy pattern supplies no equality to the
second argument. A result also cannot expose a caller-chosen suffix.
Agda's `uncurry` opens a dependent pair by binding both components;
here the equality also depends on which patterns Haskell evaluates.
[Agda's dependent pair eliminator](https://github.com/agda/agda-stdlib/blob/master/src/Data/Product/Base.agda)
and [scope and demand checks](../../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh).

## Prove a split preserves the original vector

`splitAtValue` takes a runtime cut and the original vector.
It returns a retained prefix, a retained suffix, and
`appendVec left right :~: original`. The zero branch stores the
input as the suffix. The successor branch opens the recursive result
and uses its equality to put the original head on the prefix.
The definition has two equations. The zero equation leaves the vector
unevaluated. The successor equation matches `VCons x xs` in its second
argument. The proof field matches the recursive premise only when the
caller demands that field. Removing this match makes GHC report
GHC-05617 at the written `Refl`. It shows the unsolved equality
`appendVec left right ~ xs`.

The first pattern supplies a nominal equality about the second
argument's type. GHC checks which constructors can inhabit that type
before accepting the second pattern as exhaustive. If a second
constructor is possible, the error names it at the pattern. Guards
still receive ordinary GHC coverage checking.
[Missing constructor](../../../testsuite/tests/dependent/should_fail/NativeRetainedSecondColumn.stderr)
and [incomplete guard](../../../testsuite/tests/dependent/should_fail/NativeRetainedSecondGuard.stderr).

A separate ordinary client splits `[True, False, True]` at two and reads
`[True, False]` and `[True]`. After matching the equality it appends
the pieces. Demanding the zero-cut proof leaves an undefined input
unevaluated. Demanding the successor proof evaluates the input spine
up to the cut and leaves the tested head and suffix unevaluated.
The client also checks a cut read from an IORef. The registered test
compiles each module from the preceding modules' interfaces at
`-O0`, `-O1`, and `-O2`.
[Producer](../examples/packed-vector/CertifiedSplit.hs),
[ordinary client](../examples/packed-vector/CertifiedSplitClient.hs),
[regression](../../../testsuite/tests/dependent/should_run/NativeCertifiedSplit.sh)
and [Agda provenance](../REFERENCES.md#prove-that-the-pieces-reconstruct-the-input).

An independent runtime cut uses the existing remainder proof.
`checkedSplitAtValue cut total input` returns `Nothing` when the cut
exceeds the total. A successful result stores the actual suffix length,
the GADT that relates the three lengths, the two vectors, and their contents equality.
The client can inspect the length result without evaluating the vector.
The source-hidden regression checks zero, interior, exact, and excessive
cuts. [Checked split](../examples/packed-vector/CertifiedSplit.hs)
and [ordinary client](../examples/packed-vector/CertifiedSplitClient.hs).

## Repeated retained constructors in linear packages

A package can store a retained natural number, a payload selected by that
number, and a linear field. Two equations can both match `S predecessor`
and select different ordinary fields. GHC shares the constructor case
after checking that the renamed predecessor fields, multiplicities and
equality proofs agree. Each branch can use its own written predecessor
name. Returning evidence for `Z` where the predecessor is unknown gives
GHC-05617.

A source-hidden ordinary client checks that an unused predecessor and
payload remain lazy. A pattern combination that still requires separate
matches on the linear package reports GHC-47619.
[Provider](../../../testsuite/tests/dependent/should_run/NativeLinearRetainedPackageProvider.hs)
and [rejection controls](../../../testsuite/tests/dependent/should_run/NativeLinearRetainedPackageEvidenceReject.hs).

### Consume positions from two independent lists

`TwoPositions a b` stores two lists and two positions. Each position
uses its own list's length. Both positions are linear. The reader
matches the first position before it inspects the second one. Its
right-hand sides can use each checked tail directly. An empty list
has no position. The empty case on that position forces it.

The ordinary client can read unrelated offsets, including finite
offsets on cyclic lists. Checking a negative first offset leaves the
second list unevaluated. GHC can also share a nested `Sigma.Pair`
alternative when its witness, payload and nominal proof align in
both equations. A fallthrough that reuses a consumed package reports
GHC-47619 at the source pattern.
[Library](../examples/qualified-pi/ListCursor.hs) and
[ordinary client](../examples/qualified-pi/CursorClient.hs).
