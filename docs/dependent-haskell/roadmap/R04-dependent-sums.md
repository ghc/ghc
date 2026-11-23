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

## Package a runtime choice and read its payload

`Message` is `some (flag :: Bool) | Payload flag`. One flag selects an
integer payload, and the other selects text. The provider's `make`
returns `Payload flag` at the supplied runtime value. An ordinary client
packages the two fields without a singleton or a visible type argument.

```haskell
pack :: Bool -> Message
pack flag = Pair flag (make flag)

readMessage :: Message -> (Bool, String)
readMessage (Pair False (Number n)) = (False, show n)
readMessage (Pair True (Text text))  = (True, text)
```

The match on the flag determines which payload constructor can occur.
The client reads the flag from an `IORef`. Reading only the first field
of a different pair leaves its undefined payload unevaluated. The
provider and client can be compiled separately. Agda's `Σ` gives
`snd` a type indexed by `fst`. The Haskell example uses the same
relationship between the two stored fields in a constructor match.
[Provider](../examples/packed-vector/RuntimeMessage.hs),
[ordinary client](../examples/packed-vector/RuntimeMessageClient.hs),
and [Agda's built-in Σ](https://agda.readthedocs.io/en/v2.8.0/language/built-ins.html#the-s-type).

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

A package can also have no type parameters. Each value stores its own
length. The constructor takes a runtime `n` and a vector of that length.

```haskell
data CharVector where
    CharVector :: foreach (n :: Nat) -> Vec Char n -> CharVector

sample :: CharVector
sample = CharVector (S Z) (VCons 'r' VNil)
```

An interface-only reader can use the length function in the vector's
result type. GHC rejects a result at the successor of that length.
Reading the length leaves the payload unevaluated. Demanding the vector
can reach an error in that field while the length remains unevaluated.
[Package](../examples/packed-vector/RetainedCharVector.hs).
[Length worker](../examples/packed-vector/RetainedCharLength.hs).
[Dependent projection](../examples/packed-vector/RetainedCharProjection.hs).
[Ordinary client](../examples/packed-vector/RetainedCharClient.hs).

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

## Replace a vector after checking its runtime length

An ordinary module can combine the two projections. This function
keeps the target's length and takes its characters from the source.
It enables `GADTs` to match the equality witness. Its signature uses
ordinary arrows.

```haskell
replaceCells :: CharVector -> CharVector -> Maybe CharVector
replaceCells target source =
  case sameNat (localLength target) (localLength source) of
    Nothing   -> Nothing
    Just Refl -> Just (CharVector (localLength target) (localCells source))
```

When GHC compiles the function, it checks that the replacement's type
is `Vec Char (localLength target)`. The source field has type
`Vec Char (localLength source)`. Comparing the two lengths at runtime
can produce `Refl`, which makes these types interchangeable in the
successful branch. A caller can get its input from `readIORef`.
Different lengths return `Nothing` without reading either payload.

If the function omits the comparison and always constructs the result,
GHC-83865 points to `localCells source`:

```text
Couldn't match type: localLength source
               with: localLength target
```

To render a result, `fmap (\p -> toList (localCells p))` checks each
projection at its own input. Using Prelude composition in `toList . localCells` requires
one intermediate vector type for every input. GHC-83865 points to
`localCells` in that expression.
[Length comparison](../examples/packed-vector/PackedVecLibrary.hs)
and [projections](../examples/packed-vector/RetainedCharLocal.hs).

## Separate named fields and dependent projections

Serokell includes records in which a later field depends on an earlier
field's value. The fork already supports a record selector that returns
a whole dependent pair. In `StoredVector`, `Batch a` has a field of
type `some (n :: Nat) | Vec a n`. That selector returns the length and
the vector together. [StoredVector](../examples/packed-vector/StoredVector.hs).

Two ordinary GADT record fields can also share a hidden index:

```haskell
data Named where
    Named :: { tag :: SLength n, cells :: Vec Char n } -> Named

renderNames :: Named -> String
renderNames Named { cells = xs } = toList xs
```

An ordinary client can construct this record and use the field in the
pattern shown above. It can also consume both fields with a function
polymorphic in `n`. Applying `cells` to the record as a selector would
let the hidden index escape. Job 1728 reports GHC-55876 at that use on
normal 569. Declaring separate Haskell98 fields `storedLength :: Nat`
and `storedCells :: Vec Char storedLength` reports GHC-45510 at the
reference to `storedLength` in the type. An ordinary field label does
not bind a type-level name for its stored value.
[GHC existential record selectors](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html#record-constructors).

An explicit dependent selector has a different result type. The
handwritten `second` for `DPair` returns its payload at `first q`, where
`q` is the same pair. Its separate provider supplies the equation for
`first`. A consumer can check that type without recovering an
existential index from the caller. General named record projections
remain open.
[First projection](../examples/packed-vector/DependentPair.hs),
[second projection](../examples/packed-vector/DependentProjection.hs),
[Agda's built-in Sigma type](https://agda.readthedocs.io/en/v2.8.0/language/built-ins.html#the-s-type)
and [Serokell dependent sums](https://ghc.serokell.io/dh#dependent-sums).

### Keep the package constructor private

A package can already expose one module that exports `CharVector`,
a smart constructor and the two projections. Define the datatype and
projections in separate hidden modules. Re-export the projection names
through the public module. In job 1788 an ordinary client read a vector
through this interface at `-O0` and `-O2`. Both runs printed `(1,"r")`.
The client could neither name the hidden constructor nor import its
module. A dependent client could return `Vec Char (witness p)` from
`cells p`. Job 1793 checks the stronger ordinary-client case with
`ExperimentalDependentTypes` disabled. The claimed signature
`CharVector -> Vec Char 'Z` fails with GHC-83865 at `storedCells p`.
The test registers a package that exposes only the public module.

The 574-observation compiler checks the datatype and both handwritten
projections in one module that exports the constructor. A length worker
only needs the `Nat` type and the package constructor in scope. It can
leave the constructors of the stored vector and its elements private.
This passed all sixteen import combinations in job 1853.
[Same-module projections](../examples/packed-vector/RetainedCharVector.hs).

Still open is defining the datatype and both projections in a single
module that hides the constructor. A caller needs only the signature
`localCells :: foreach (r :: CharVector) -> Vec Char (localLength r)`.
The implementation also needs the equation for
`localLength (CharVector n xs)`. Check that the module can use this
equation internally, that a source-hidden client can consume its public
projection, and that an incorrect result index still fails. Repeat with
interface pragmas omitted and with a provider of ordinary eliminators.
Haskell defines constructor visibility through export lists. Agda
distinguishes the places where an abstract function may reduce.
[Haskell 2010 exports](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html)
and [Agda abstract definitions](https://agda.readthedocs.io/en/v2.8.0/language/abstract-definitions.html).

### Two vector fields at one length

A positional constructor can store two vectors with one runtime length.

```haskell
data Columns where
    Columns :: foreach (n :: Nat) -> Vec Char n -> Vec Bool n -> Columns

size :: Columns -> Nat
size (Columns n _ _) = n

letters :: foreach (p :: Columns) -> Vec Char (size p)
letters (Columns _ xs _) = xs

flags :: foreach (p :: Columns) -> Vec Bool (size p)
flags (Columns _ _ bs) = bs
```

An ordinary client can use both projections as arguments to `zipVec`.

```haskell
readPairs :: Columns -> [(Char, Bool)]
readPairs p = toList (zipVec (letters p) (flags p))
```

With the provider source hidden, the client returned
`[('r',True),('s',False)]` at `-O0` and `-O2`. Reading one field left
the other unevaluated. Using `flags q` as the second argument failed
there. GHC reported both `size p` and `size q`. A projection claiming
`Vec Bool ('S (size p))` also failed.
[Client](../examples/packed-vector/SiblingColumnsClient.hs) and
[positive and negative cases](../../../testsuite/tests/dependent/should_run/NativeRetainedSiblingFields.sh).

Job 1855 checked upgrades with the component compiler and the 572
libraries. Both native and typecheck-only builds recompiled the old
ordinary length worker once. A second build reused every output.
The older reader ran a stripped interface with an ordinary exported
signature. It rejected an interface containing an inline unfolding
that uses the extended constructor. The component compiler inlined
that unfolding and returned `[True]` with the other field undefined.

In Agda, the types of later bindings in a telescope may refer to an
earlier binding. This permits both fields to use the same length.
[Agda telescopes](https://agda.readthedocs.io/en/v2.8.0/language/telescopes.html)
and [Serokell dependent sums](https://ghc.serokell.io/dh#dependent-sums).

### What record dot would need

With the standard GHC interpretation, `packet.cells` uses `getField`.
In `HasField label record field | label record -> field`, the field
label and the record's type determine the result type.
For a fixed `CharVector` type, different record values can store
different lengths. A selector for the vector alone needs the type
`foreach (r :: CharVector) -> Vec Char (storedLength r)`.
The current `HasField` method cannot give its result that dependency
on its argument. Dot selection of a field that returns an entire
dependent pair can still use one fixed result type.
[GHC record field selector polymorphism](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/hasfield.html)
and [proposal #282](https://ghc-proposals.readthedocs.io/en/latest/proposals/0282-record-dot-syntax.html).

Agda binds record fields in order. A later field can use an earlier
field's value in its type. That gives the second projection a result
indexed by the first projection on the same record. The handwritten
Haskell projections establish that relationship before a choice of
named-field syntax or overloading is needed.
[Agda record types](https://agda.readthedocs.io/en/v2.8.0/language/record-types.html)
and [the Serokell card](https://ghc.serokell.io/dh#dependent-sums).

### Named fields at one stored length

```haskell
data NamedColumns = NamedColumns
  { storedLength :: Nat
  , namedLetters :: Vec Char storedLength
  , namedFlags :: Vec Bool storedLength
  }
```

With `ExperimentalDependentTypes`, the two payload types refer to the
stored value of the first field. The generated selector has type
`foreach (p :: NamedColumns) -> Vec Char (storedLength p)`. An ordinary
client can call both payload selectors on the same record and zip the
results. It can also construct the record by naming every field.
[Example](../examples/packed-vector/NamedSiblingColumns.hs).

The declaration uses one constructor, no datatype parameters, and lazy
fields. Each payload uses the witness as its final type argument. Named
patterns list the selected fields in declaration order. Payload selection
uses the generated function. The `HasField` result type depends on the
label and record type, which do not supply a record value. Construct a
new record to replace its witness or payloads.
[Agda record types](https://agda.readthedocs.io/en/v2.8.0/language/record-types.html)
and [GHC proposal #23](https://ghc-proposals.readthedocs.io/en/latest/proposals/0023-overloaded-record-fields.html).


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
occurs in a result argument. The argument uses the field directly or
fills a field of one promoted constructor. The application supplies
every visible field in the promoted constructor's telescope. Each
invisible type argument is closed. The fields match their instantiated
kinds, and the result has its own closed structural kind. A retained
field may occur more than once. The other result
arguments must be distinct, unchanged erased parameters.
A retained field is lazy and has
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

An ordinary client that omits the successor case of `Packed Bool` gets
GHC-62161 at the incomplete equation. The warning names `Packed Bool`
and shows the missing pattern `Pair (S _) _`. Desugaring takes the displayed
alias from the checked argument type. Coverage checking still uses the
lowered type. The source-hidden test also checks the unparameterized
alias `Phantom` and an ordinary alias that still displays as `Maybe Bool`.
[Source-hidden check](../../../testsuite/tests/dependent/should_run/NativeGadtRetainedSum.sh)
and [desugaring](../../../compiler/GHC/HsToCore/Match.hs).

An ordinary client can use `BangPatterns` to evaluate the stored length
before it uses the payload. The test calls its version of `SomeVec`
`DirectVec`. The payload still has the public index `n`.

```haskell
strictDirectCells :: DirectVec a n -> Vec a n
strictDirectCells (DirectVec !n xs) = xs
```

`DirectVec !_ _` evaluates just the witness. At a stored `Z` it accepts
a bottom payload. `Pair !_ _` has the same demand on an existential
witness. A nested pair can leave its independent witness unevaluated.
Parenthesized bangs also keep each witness in scope. Two matches with
the same variable spelling still bind distinct witnesses.
[Ordinary client](../../../testsuite/tests/dependent/should_run/NativeGadtRetainedSumClient.hs),
[source-hidden check](../../../testsuite/tests/dependent/should_run/NativeGadtRetainedSum.sh),
and [pattern precedents](../REFERENCES.md#force-a-stored-witness).

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

A tile can combine both lengths into one public shape index.

```haskell
data Shape = Rectangle Nat Nat

data Tile a (shape :: Shape) where
    Tile :: foreach (rows :: Nat) (columns :: Nat)
         -> Vec (Vec a columns) rows -> Tile a ('Rectangle rows columns)

tileCells :: Tile a ('Rectangle rows columns) -> Vec (Vec a columns) rows
tileCells (Tile _ _ xs) = xs
```

An ordinary client recovers both dimensions through the nominal shape
equality. A wildcard match leaves the dimensions and the cells unevaluated.
Reading one dimension leaves the other unevaluated. Swapping the two
dimensions in the payload type fails at `xs`.
[Showcase constructor](../examples/packed-vector/IndexedMatrix.hs),
[ordinary client](../examples/packed-vector/IndexedMatrixClient.hs),
[source-hidden check](../../../testsuite/tests/dependent/should_run/NativeGadtRetainedSum.sh)
and [indexed shape precedent](../REFERENCES.md#combine-two-stored-fields-in-one-index).

The same example defines a volume whose public shape holds its layer,
row, and column counts. The client reads the first rectangular layer
at a successor layer count and leaves the second layer unevaluated.
In `'Just @Bool flag`, `Bool` fills an invisible type binder and the
stored `flag` fills the visible field. GHC inserts the same `Bool`
argument in `'Just flag` when the field and public `Maybe Bool` kind
determine it.

A provider can declare `HiddenTile a = exists (shape :: Shape). Tile a shape`.
An ordinary client can return `Tile rows columns xs` at that package type
when both dimensions come from runtime inputs. Its expected result fixes
the element type. The actual tile fixes the hidden shape. An ordinary
reader can open the package and match `Tile Z _ _` to return an empty
result with the columns and cells unevaluated. The Showcase reads the
dimensions and cells of a 1-by-2 tile and opens a zero-row tile without
evaluating either missing field.
[Implicit package inference](R22-existential-quantification.md#an-implicit-package-in-an-application)
and [Agda dependent pairs](https://agda.readthedocs.io/en/v2.8.0/language/built-ins.html#the-s-type).

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
This Quick Look path can also follow an associated family when its domain
is already available in the caller's scope. An instance can supply the
domain, as can a dictionary from a prior GADT match. The compiler checks
each domain before using that family equation to select the erased
constructor arguments. Ordinary subsumption still checks the original
expected result. A stuck family or a missing domain leaves the
constructor application unresolved. The runtime message example uses both
domain sources. Its Showcase row selects the text and integer payloads
and reads another stored flag without evaluating its payload.
[Ordinary Showcase client](../examples/packed-vector/RuntimeMessageClient.hs).

## Use an associated family for the payload

A retained sum can use its witness as an argument of a constrained
associated family. For example, `type Packet = some (tag :: Bool)
| Body Bool tag` has a payload whose type depends on the stored tag.
The private family generated by `some` has one variable-only equation.
The class domain of `Body` can depend on captured parameters. It must
stay independent of `tag`. The checker resolves that domain when it
checks the sum type. An associated equation uses its owning class.
An ordinary synonym can use an available instance.

The Paterson conditions for type-family equations still apply.
`checkFamInstRhs` checks that the arguments of each family call on the
right-hand side are strictly smaller than the argument patterns on
the left. `Body Bool tag` fails this size check for both a written
`type NamedBody Bool tag = Body Bool tag` equation and the private
payload equation generated by `some`. Both compile with
`UndecidableInstances`. A diagnostic that names a type lambda for
this `some` expression refers to the generated equation. An ordinary
unmarked closed family still cannot forward a constrained family.

An ordinary interface-only caller can supply the class from a global
instance or from a GADT pattern. Reading the tag leaves an unused
payload unevaluated in both cases. The source controls reject a wrong
payload, a missing domain and an associated equation whose owning class
does not supply the payload domain.
The Showcase reads both runtime tags with undefined payloads.
[Provider](../examples/packed-vector/AssociatedPayloadSum.hs).
[Ordinary client](../examples/packed-vector/AssociatedPayloadSumClient.hs).
[Regression](../../../testsuite/tests/dependent/should_run/NativeAssociatedPayloadSum.sh).

Interfaces that expose the checked payload carry the
`scoped-sum-domain-v1` requirement. It applies to a generated
payload, an inferred type that refers to it, and a re-export. The
567 reader rejects those interfaces at the header. It can still read
an unrelated sum and can open an interface that only re-exports an
unrelated `Bool` binding. A client that uses the re-exported binding
still loads its defining interface. If that interface requires
`scoped-sum-domain-v1`, the 567 reader rejects it there.
A source-hidden control adds a more specific
instance with a new premise. The current reader reports that premise
in both a signature naming the sum and an inferred result that refers
to its payload family.

## Compose the associated payload under a type constructor

A sum can contain `Maybe (Body key tag)` or a pair of payload types.
The generated family must still have one variable-only equation.
The checker follows ordinary algebraic type constructors to find each
constrained associated family. It requires every owner predicate at sum
formation and checks equal predicates once. Each owner parameter must
be determined by the family application. The predicates must stay
independent of the retained witness. An associated equation checks
them from its owner and superclasses.

The named synonym and named associated equation already accept these
payloads. A retained GADT field using a global instance also accepts
them. A retained GADT constructor context with a class constraint
still fails even with a direct payload.

Morris and Eisenberg use qualified types to check the domains of
type-family applications during type formation. Agda's standard
library lets a dependent pair's codomain use `Maybe` and ordinary
products. Here the checker combines class obligations under those
constructors and the runtime client reads `Nothing` without
evaluating its stored tag.
[Constrained Type Families, §4.2](https://jgbm.github.io/pubs/morris-icfp2017-families-extended.pdf).
[Agda dependent products](https://agda.github.io/agda-stdlib/master/Data.Product.Base.html).
[Agda optional values](https://agda.github.io/agda-stdlib/master/Data.Maybe.Base.html).

The component check passes at `-O0` and `-O2` with a source-hidden
ordinary client. The provider includes two families with distinct
owners and a payload that uses the same family twice. The client
checks `Nothing` with an undefined witness and reads both pair
payloads. Separate controls remove each owner and preserve the
localized diagnostic. Repeated owner evidence gives one obligation.
[Provider](../examples/packed-vector/ComposedPayloadSum.hs).
[Ordinary client](../examples/packed-vector/ComposedPayloadSumClient.hs).
[Regression](../../../testsuite/tests/dependent/should_run/NativeComposedPayloadSum.sh).

A direct payload keeps `scoped-sum-domain-v1`. The 569 reader and
the component reader exchange direct interfaces and run both
clients. A composed payload requires `scoped-sum-domain-v2`. The
normal 569 reader rejects the composed producer, a client that
exports an inferred type using the payload, and an affected
re-export at their interface headers.

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
