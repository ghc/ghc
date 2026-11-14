# Packed vector source example

Both M21 compiler stages pass all 316 source observations with Core Lint
and STG Lint. The API/runtime fixture passes 70 package/bound rejections.
All five registered regressions pass. M21 adds 15 observations and
keeps the previous 301 lines in order. Seven new fallback errors and the
previous 23 standalone errors pass in both stages.

At M20, both stages and all five regressions passed 301 observations and
63 package/bound rejections. The complete four-clause worker remains in Main
as a source reference.

At M18, both compiler stages passed the first 249 observations with Core Lint
and STG Lint enabled. These included the previous 209 observations and 40
reverse, arithmetic proof and demand observations. The bootstrap and registered
runtime fixtures passed 47 package/bound rejections. Seven standalone reverse
errors had pinned codes and primary source tokens in both stages. Signed
argument controls and all five focused registered regressions passed. The
consumer imports the library interface with the producer source absent from
its import path.

`PackedVecLibrary.hs` stores a runtime `Nat` and a `Vec` indexed by that same
field. `PackVec` has two ordinary runtime fields. It has no singleton argument.
`withPacked` opens their shared scope for a checked consumer. `repack` constructs
a new package with the same field identity. `ignorePacked` inspects the package
constructor and does not evaluate its fields.

The compiler checks the relation between the fields. It rejects `PackVec m xs`
when `xs :: Vec Int n` and `m` and `n` are independent runtime bindings. Opening
a package gives the consumer a fresh scoped index. Its vector cannot escape as
`Vec Int m` for an unrelated index. Both rejections are regression checks.

The source subset has one package constructor. Retained index fields accept
bare variables or one constructor layer with bare variable subfields.
Ordinary fields admit GHC patterns, including nested vector/proof constructors
and wildcards. The length domain is the closed first-order datatype `Nat`. The element parameter
`a` remains an erased universal. Retained binders have unrestricted multiplicity.
Ordinary fields keep GHC's constructor multiplicities. This example uses
unrestricted eliminators. Strict fields, record fields, existential binders, constructor
contexts remain outside this constructor-field subset. Package equations can
use guards and multiple clauses. Existing Nat recursion and Vec GADT patterns
remain in the program.

`Main.hs` imports the library. It passes lengths 0, 2, and 3 through an IORef to
the same worker. It checks constructor weak-head normal form, field sharing after
repackaging, unused bottom and cyclic fields, demanded payloads, and a bottom
package. It reads one payload constructor at a partial or cyclic length. It does
not fully evaluate an infinite length or a cyclic bottom payload. Exception
checks catch only `ErrorCall` and require the named marker.

## Filter with a computed result length

`filterVec` takes an ordinary Boolean predicate and a vector. It returns
`PackedVec a`, whose stored length depends on the predicate results at runtime.
Its retained branch opens the recursive package:

```haskell
then case filterVec predicate xs of
    PackVec n ys -> PackVec (S n) (VCons x ys)
```

Here `ys :: Vec a n`. The new head gives `VCons x ys :: Vec a (S n)`, so the new
package must store `S n`. Storing `n` instead fails with a located GHC-05617
diagnostic. This checks the relation between the result fields. Correct predicate
selection and element order still need separate checks.

The consumer reads cutoffs 0, 1, and 3 through an IORef. Filtering `[1,2,3]`
with `(> cutoff)` produces lengths 3, 2, and 0. The filter scans the finite
input spine before returning a package. An empty vector does not demand the
predicate. A constant predicate can reject or retain an undefined element.
Reading the stored length does not demand a retained element. Demanding the
element still raises its original error. A partial input spine remains partial.

This follows Agda's `Vec≤` filter structure for its length and vector fields.
See the [pinned references](../../REFERENCES.md#source-vector-filter). The bound
proof is implemented in the following source increment. Productive filtering
of infinite inputs remains separate work.

The same source file checks left and right operator sections. It disables
`PostfixOperators` so a left section keeps its implicit lambda. Both section
forms remain in WHNF with an undefined operator or operand. Applying each
section demands the corresponding error. These checks cover the required
expansion of GHC's temporary section helpers before code generation.

## Filter with a checked length bound

`BoundedVec a limit` stores three runtime fields: a length, a vector at that
length, and a proof that the length is at most `limit`.

```haskell
data LE (n :: Nat) (m :: Nat) where
    LEZ :: LE Z m
    LES :: LE n m -> LE (S n) (S m)

data BoundedVec a (limit :: Nat) where
    PackBounded :: foreach (len :: Nat) ->
                   Vec a len -> LE len limit -> BoundedVec a limit
```

`filterBounded` constructs all three fields from source. Empty input uses
`LEZ`. A retained element increments the length, extends the vector, and uses
`LES proof`. A rejected element keeps the length and vector, then calls
`weakenRight proof` to increase only the proof's upper bound. The three negative
checks separately change the vector length, the proof's lower index, and its
upper index. Each fails at a source location with GHC-05617 or GHC-25897.

The consumer opens the input's `PackedVec` before it calls `filterBounded`.
The input bound therefore refers to the same scoped runtime index as its
vector. The result observer prints the output length, element list and number
of `LES` steps. For runtime cutoffs 0, 1 and 3, these are `(3,[1,2,3],3)`,
`(2,[2,3],2)` and `(0,[],0)`.

The proof is an ordinary lazy field. Package WHNF, stored length and vector
observations can ignore a bottom or cyclic proof. A defined `LES` constructor
can contain a bottom recursive proof. The source checks distinguish proof
WHNF from recursive proof demand. They also check the delayed right-weakening
call used by the rejected branch. A terminating proof over terminating indices
gives the bound interpretation. This does not establish Haskell totality or
prove that the predicate selected the correct elements. Agda's corresponding
bound field is irrelevant; general proof erasure remains separate work here.
See [the exact constructors and weakening lemma](../../REFERENCES.md#source-filter-bound-proof).

The proof observer uses ordinary `evaluate proof >> pure ()`. Supporting the
scoped proof type requires the native checker to retain GHC's nested forall
and dictionary order for `(>>)`. It reuses IALL and `instCall`; no new Core
form or interface format is required.

## Nested package patterns

The source library matches the stored vector directly:

```haskell
headPacked :: PackedVec a -> Maybe a
headPacked (PackVec n VNil) = Nothing
headPacked (PackVec m (VCons x xs)) = Just x
```

Each equation binds a different runtime length. The vector pattern uses that
same length in its type. The compiler checks ordinary fields with `tcConArg`.
It preserves each equation's actual field Ids in a separate case on the
shared package. GHC's failure continuations try those cases in source order.
An earlier successful catchall prevents a later pattern from forcing a field.
The compiler does not merge the two length binders through a let alias.
Agda supplies the [constructor and case-tree precedent](../../REFERENCES.md#source-package-patterns).

`proofDepthBounded` matches `LEZ` and `LES` directly in the proof field.
It does not inspect the stored vector or length. `repackWithoutProof` uses a
wildcard for the proof field. `guardedRepack` uses a runtime guard and returns
the fields from the row that succeeds.

The 23 new observations check filtered input at runtime cutoffs, partial
vector tails, element WHNF, proof demand, wildcard proofs, guard fallback and
catchall order. Named errors show that nested vector/proof patterns demand
their selected field before an unused length. Bottom packages still fail
when their constructor is inspected. A nested vector cannot escape as
`Vec Int m` for an unrelated outer index: the source mutation rejects at
`VCons x xs` with GHC-05617. A wildcard in the retained length field still
rejects with GHC-83605. The constructor-pattern subset below adds one layer
with bare variable subfields. No Core form or interface format changes.

General projections remain separate work. These patterns do not establish
totality or proof erasure.

## Uncons with a runtime predecessor

The source worker inspects the stored length and returns a packed tail:

```haskell
unconsPacked :: PackedVec a -> Maybe (a, PackedVec a)
unconsPacked (PackVec Z _) = Nothing
unconsPacked (PackVec (S k) (VCons x rest)) = Just (x, PackVec k rest)
```

The public worker now matches the stored length directly in each equation.
Its predecessor used explicit nested cases. The `S k` pattern refines the
later vector field to `Vec a (S k)`. The ordinary vector pattern relates its
tail to `k`.
The tail package stores that actual runtime predecessor. Agda's
[indexed uncons and case abstraction](../../REFERENCES.md#source-vector-uncons)
provide the relation and scope precedent. No runtime value is reconstructed
from an erased vector index.

`toListPacked` repeatedly calls this worker and consumes the returned tail
packages. Inputs read through an IORef produce `Nothing`, head 42 with tail
length 1, and head 42 with tail length 2. Draining filtered packages preserves
their finite element order.

The zero branch ignores an undefined vector. The successor branch inspects
the vector constructor. Observing the result, pair and tail package does not
force predecessor, head or tail. Separate observations force each field and
check its named error. A bounded prefix of a specific cyclic input returns
three elements. This is an evaluation observation, not a general productivity
or totality result.

The complete source observer exposed a Core Tidy boundary. It contains
`PackVec (S bottomLength) (VCons bottomElement bottomPayload)`. Tidy globalises
the top-level `bottomLength` Id. Native index leaves must remain local.
The compiler now inserts lazy local aliases before tidying native RHSs and
substitutes expressions, types and coercions together. CorePrep's existing
native guard keeps the aliases. The failing expression remains in the program.
The index grammar, Core forms and interface format stay unchanged.

Earlier tail-packaging probes using the original, successor or an independent
length rejected at the payload with located GHC-25897. Existing retained-field and
scope restrictions remain in force. Some diagnostics still show generated
captured bindings in their relevant-binding details.

## Constructor patterns in retained index fields

The source checker accepts `Z` and `S k` directly in a retained package field.
This subset allows one constructor layer with bare variable subfields.
Parentheses are permitted. Nested index constructors, wildcard index fields,
lazy package patterns and package let patterns remain unsupported. Package
elimination remains unrestricted and uses a single package constructor.
Ordinary payload fields keep their existing GHC pattern support.

For an index constructor pattern, the checker generates an outer as-pattern
with a fresh Id for the whole stored index. Match selection uses that exact
Id. The inner constructor alternative binds checked nominal equality between
that index and the constructor applied to its actual fields. The checker
transports later field types under this equality. Coercion patterns retain
the original outer types. The equality scopes over the later patterns and
RHS, within the selected constructor branch. It does not identify fields
from different equations through plain aliases. See the
[pinned constructor-telescope and matching sources](../../REFERENCES.md#source-package-patterns).

The two `unconsPacked` equations above retain the original field demand.
`PackVec Z _` ignores the vector. An explicit `PackVec Z VNil` pattern instead
demands its constructor. `S k` demands the stored outer Nat constructor and
leaves `k` lazy. A later `VCons x rest` pattern demands the vector constructor,
then leaves its head and tail lazy.

`Main.hs` also checks two independent retained fields and a stored proof:

```haskell
data EqualLengths where
    EqualLengths :: foreach (n :: Nat) -> foreach (m :: Nat) -> (n :~: m) -> EqualLengths

equalTails :: EqualLengths -> PackedVec Int
equalTails (EqualLengths Z Z Refl) = PackVec Z VNil
equalTails (EqualLengths (S k) (S j) Refl) = PackVec k (replicateNative @Int j 42)
```

Both constructor patterns refine their later types. Matching the ordinary
`Refl` field supplies the equality needed to align `k` and `j`. The pattern
demands the proof before constructing the result package. It does not demand
either predecessor merely to obtain that equality.

Each equation retains its own runtime Ids and ordinary failure continuation.
A constructor mismatch tries the next row; a demanded bottom does not.
Match compilation preserves source order across index and vector patterns,
later argument patterns, views and guards. It does not move index patterns
into RHS cases. An earlier mismatch can therefore
skip a later view or guard argument. A failed guard can reach the catchall
without reusing the earlier row's index identity.

The source rule reuses existing Core constructor evidence and CorePrep/STG
case lowering. It adds no Core form, runtime field or interface codec.
Stored equality proofs remain ordinary lifted fields. The 29 new observations
check these scopes and demands, including partial and cyclic fields. They
do not establish totality or general pattern support.

## Runtime equality and aligned zip

`sameNat` compares two independent runtime lengths. Its successful result
contains the ordinary `Data.Type.Equality.:~:` proof. `zipPacked` opens two
packages and matches `Refl` before it calls the ordinary equal-length `zipVec`.
The source workers are:

```haskell
sameNat :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)
sameNat Z Z = Just Refl
sameNat (S left) (S right) = case sameNat left right of
    Nothing -> Nothing
    Just Refl -> Just Refl
sameNat _ _ = Nothing

-- | Pair elements of ordinary vectors with one shared length index.
zipVec :: Vec a n -> Vec b n -> Vec (a, b) n
zipVec VNil VNil = VNil
zipVec (VCons x xs) (VCons y ys) = VCons (x, y) (zipVec xs ys)

-- | Compare stored lengths before aligning the vector payloads.
zipPacked :: PackedVec a -> PackedVec b -> Maybe (PackedVec (a, b))
zipPacked (PackVec n xs) (PackVec m ys) = case sameNat n m of
    Nothing -> Nothing
    Just Refl -> Just (PackVec n (zipVec xs ys))
```

The `Just` constructor alone does not establish equality in the branch.
Matching its `Refl` field introduces the local equality that aligns the two
vector types. That equality cannot escape its branch. The negative source
checks reject transport without `Refl`, transport in the `Nothing` branch,
and transport between unrelated stored indices. `Nothing` contains no
refutation proof. This result is not Agda's `Dec` datatype.

Equal-length success completes the comparison of both finite `Nat` spines
before it returns `Just`. The comparison inspects the left length first.
`Z` versus `S bottom` and `S bottom` versus `Z` return `Nothing` without
forcing the predecessor. Comparing a bottom length still fails, including
comparison of that length with itself. Equal cyclic lengths can diverge.
These workers do not establish totality or adopt Agda's proof erasure.
See the [pinned equality and vector sources](../../REFERENCES.md#runtime-equality-and-aligned-zip-next-source-probe).

The consumer reads each length independently through an IORef. It builds
vectors of 42 and 7, then prints these results:

| Runtime lengths | Output |
| --- | --- |
| `(0,0)` | `runtime equality zip: same Just (0,[])` |
| `(0,2)` | `runtime equality zip: different Nothing` |
| `(2,0)` | `runtime equality zip: different Nothing` |
| `(2,2)` | `runtime equality zip: same Just (2,[(42,7),(42,7)])` |
| `(2,3)` | `runtime equality zip: different Nothing` |
| `(3,2)` | `runtime equality zip: different Nothing` |
| `(3,3)` | `runtime equality zip: same Just (3,[(42,7),(42,7),(42,7)])` |

The filtered inputs are `[1,2,3]` and `[10,20,30]`. Each uses its own
runtime cutoff and keeps elements greater than that cutoff:

| Runtime cutoffs | Output |
| --- | --- |
| `(0,0)` | `runtime filtered zip: Just (3,[(1,10),(2,20),(3,30)])` |
| `(1,10)` | `runtime filtered zip: Just (2,[(2,20),(3,30)])` |
| `(1,0)` | `runtime filtered zip: Nothing` |
| `(3,30)` | `runtime filtered zip: Just (0,[])` |

A length mismatch ignores both vector payloads. After equal-length success,
result WHNF, package WHNF and the stored length can also ignore both vectors.
Observing the vector demands its left constructor before its right constructor.
A defined pair still leaves its two head values lazy. A returned tail package
leaves both vector tails lazy until the consumer observes them. Separate error
markers check each demand. These claims concern vector payloads, pair fields
and tails. They do not make the preceding length comparison lazy.

The complete consumer also checks indices read from IORefs inside local lets.
The checker normalizes assigned type and multiplicity metadata before it
validates an index Id. It checks that Id against its current lexical or
same-module binding and preserves its Name and Unique. Ordinary expected-result
matching selects immutable type arguments and retains the actual result type
for final subsumption. The bounded `($)` path infers its native-anchored RHS
once. It obtains a complete result from the expected type or a named first
operand's fixed declared result. It retains the original operator, checks its
representation choices and scope, and uses the ordinary argument and result
checks. The source expressions stay unchanged. General native meta-variable
assignment and general Quick Look over native types remain unsupported.

## Append with a computed result length

`Add` computes an erased sum index. `sumNat` computes an ordinary runtime
length `p` and stores an equality proof in `Sum n m`. `appendNative` returns
a vector at `Add n m`. `appendPacked` matches `Refl` to transport that vector
to `p`, then stores the actual runtime length with it:

```haskell
type family Add (n :: Nat) (m :: Nat) :: Nat where
    Add Z m = m
    Add (S n) m = S (Add n m)

-- | Store a runtime sum and evidence that it matches the erased addition.
data Sum (n :: Nat) (m :: Nat) where
    Sum :: foreach (p :: Nat) -> (p :~: Add n m) -> Sum n m

-- | Compute a runtime sum and its equality witness.
sumNat :: foreach (n :: Nat) -> foreach (m :: Nat) -> Sum n m
sumNat n m = case n of
    Z -> Sum m Refl
    S k -> case sumNat k m of
        Sum p Refl -> Sum (S p) Refl

-- | Append two vectors with an erased sum index.
appendVec :: Vec a n -> Vec a m -> Vec a (Add n m)
appendVec VNil ys = ys
appendVec (VCons x xs) ys = VCons x (appendVec xs ys)

-- | Append vectors indexed by two retained runtime lengths.
appendNative :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec a m -> Vec a (Add n m)
appendNative n m xs ys = appendVec xs ys

-- | Compute the sum length before packaging the appended vector.
appendPacked :: PackedVec a -> PackedVec a -> PackedVec a
appendPacked (PackVec n xs) (PackVec m ys) = case sumNat n m of
    Sum p Refl -> PackVec p (appendNative n m xs ys)
```

The proof is an ordinary lifted field. Matching `Sum p proof` alone does not
establish equality; matching `Refl` supplies the branch-local equality.
The recursive proof uses the `Add` equations and successor congruence.
The solver does not evaluate runtime indices. See the
[pinned addition, vector append and congruence sources](../../REFERENCES.md#computed-length-append-next-source-probe).

The consumer reads each length independently through an IORef. Its left
vector contains 42 and its right vector contains 7:

| Runtime lengths | Appended length and elements |
| --- | --- |
| `(0,0)` | `(0,[])` |
| `(0,2)` | `(2,[7,7])` |
| `(2,0)` | `(2,[42,42])` |
| `(2,2)` | `(4,[42,42,7,7])` |
| `(2,3)` | `(5,[42,42,7,7,7])` |
| `(3,2)` | `(5,[42,42,42,7,7])` |
| `(3,3)` | `(6,[42,42,42,7,7,7])` |

Filtering `[1,2,3]` and `[10,20,30]` with independent runtime cutoffs also
preserves the left input before the right input:

| Runtime cutoffs | Appended length and elements |
| --- | --- |
| `(0,0)` | `(6,[1,2,3,10,20,30])` |
| `(1,10)` | `(4,[2,3,20,30])` |
| `(1,0)` | `(5,[2,3,10,20,30])` |
| `(3,30)` | `(0,[])` |

A fully applied `sumNat` inspects the entire first Nat spine before returning
`Sum`. A partial application stays in function WHNF without forcing that
length. A finite first length can leave the right length lazy. Matching the
computed proof still ignores a bottom right length; observing the complete
result length can force it. An explicitly constructed `Sum` can also contain
a bottom length or proof, which separate observations demand independently.

`appendPacked` inspects both package constructors before computing the sum.
Its result package and stored length can ignore both vectors. Vector demand
checks distinguish the left constructor, its first element and tail, and the
right vector reached after the finite left spine. A finite left input can
precede a tested cyclic right input and produce a bounded prefix. A cyclic
first length can diverge during `sumNat`. These are separate demand checks,
not totality or general productivity claims. The proof remains an ordinary
lazy Haskell value; this example does not establish proof erasure.

## Reverse with the original stored length

`reverseAppendNative` reverses its first vector onto the accumulator and
returns a vector at `Add n m`. `addSucc` transports the recursive result
across the successor equation. `rightZero` transports the final result from
`Add n Z` to `n`. `reversePacked` stores that original runtime `n` and delays
the reversed vector:

```haskell
addSucc :: foreach (n :: Nat) -> foreach (m :: Nat) -> Add n (S m) :~: S (Add n m)
addSucc Z right = Refl
addSucc (S k) later = case addSucc k later of
    Refl -> Refl

-- | Prove the right identity of addition by runtime Nat induction.
rightZero :: foreach (n :: Nat) -> Add n Z :~: n
rightZero Z = Refl
rightZero (S k) = case rightZero k of
    Refl -> Refl

-- | Reverse the first vector onto the second vector.
reverseAppendNative :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec a m -> Vec a (Add n m)
reverseAppendNative Z right xs acc = case xs of
    VNil -> acc
reverseAppendNative (S k) later xs acc = case xs of
    VCons x rest -> case addSucc k later of
        Refl -> reverseAppendNative @a k (S later) rest (VCons x acc)

-- | Reverse a vector while preserving its retained length.
reverseNative :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n
reverseNative n xs = case rightZero n of
    Refl -> reverseAppendNative @a n Z xs VNil

-- | Keep the original stored index and delay the reversed vector.
reversePacked :: PackedVec a -> PackedVec a
reversePacked (PackVec n xs) = PackVec n (reverseNative n xs)
```

The worker signature checks the length relation. It does not specify element
order. The consumer checks that separately. IORef lengths 0, 2 and 3 produce
`(0,[])`, `(2,[42,42])` and `(3,[42,42,42])`. Filtering `[1,2,3]` with runtime
cutoffs 0, 1 and 3 before reversal produces `[3,2,1]`, `[3,2]` and `[]`.
Reverse-append preserves the accumulator after those reversed elements:
`(5,[3,2,1,7,7])`, `(3,[3,2,7])` and `(2,[7,7])`.

Both arithmetic proofs are ordinary lifted values. Matching `Refl` supplies
local equality; the solver does not evaluate a runtime Nat. Agda's indexed
fold, reverse and arithmetic lemmas provide the
[pinned prior art](../../REFERENCES.md#length-preserving-reverse-next-source-probe).
Agda changes the fold motive at each step. This worker instead uses the fixed
`Add` family with explicit proofs. M19 changes `addSucc` and
`reverseAppendNative` to constructor equations with a later retained binder.
The checker still binds both runtime arguments before constructor discrimination.

Package WHNF leaves the stored index and reversed vector lazy. Reading the
stored index does not evaluate the vector or its proofs. Demanding the
reversed vector first forces the complete finite Nat spine through
`rightZero`, then scans the vector spine. The explicit `VNil` pattern even
inspects an empty input vector. Elements remain lazy. The observations check
that the complete input spine precedes the first output head and that later
head values retain their separate errors.

The direct reverse-append worker has a distinct demand order. Its successor
branch inspects the input vector constructor before the predecessor proof.
That proof can force the remaining Nat spine before the recursive tail is
inspected. An empty first vector can return the accumulator without demanding
the accumulator's runtime length. Partial function applications and ignored proof values
remain lazy. Bottom and cyclic fields have separate bounded demand checks.
No cyclic reversal is claimed productive.

Each `addSucc` can traverse the remaining Nat spine again. Repeated proof
induction can therefore be quadratic. An accumulator does not establish
linear total runtime. These observations do not establish totality or proof
erasure, and the implementation adds no Core rule.

The consumer also uses `const (42 :: Int) (rightZero n)` under a native anchor.
The complete expression signature supplies the literal's checking type.
The classifier reads its syntax without elaborating it. The existing
expression-signature checker checks it through the same once-only argument
cache. Kind, scope, exact-domain and final subsumption checks remain in force.
This does not enable general native meta-variable assignment.

## Constructor equations with later retained arguments

The `addSucc` and `reverseAppendNative` equations above name their second
retained argument `right` in the zero row and `later` in the successor row.
The checker chooses one shared runtime Id for that argument across the rows.
It instantiates the later telescope with that Id before checking payload types
and results. It renames each row's resolved source references and stored
free-name sets. A let alias does not supply index equality.

Local functions can capture the later index. An ordinary local variable that
shadows its source spelling keeps its own identity. The private `forwardLater`
helper tests these scopes. Its telescope places an ordinary vector argument
before two later retained arguments. `keepWithLaterEvidence` also places an
ordinary equality dictionary between retained arguments. `leadingContext`
checks a `Show a` context before the first retained binder. Its dictionary
remains before both runtime indices. The checker preserves these value and
dictionary positions. Constructor evidence specializes later payload types
and results within the selected branch.

All source argument lambdas precede constructor discrimination. Partial
applications can remain in function WHNF with bottom or cyclic supplied
indices and bottom payloads. With all arguments supplied, these equations
inspect the first index before any selected payload. Tuple and package WHNF
can leave later indices and vectors lazy. Separate checks demand each stored
index or vector and require its own error marker.

Later retained arguments currently require bare variable patterns, fixed
closed domains and unrestricted multiplicity. Constructor equations with later
retained arguments reject Template Haskell quotations before source renaming.
The [pinned Agda telescope rules](../../REFERENCES.md#later-retained-arguments-in-constructor-equations)
provide scope prior art. Haskell demand checks remain necessary. This increment
adds no Core equality rule. Its 20 expected output lines follow the M18 output
below; both compiler stages pass all 20 additions.

## Separate-module runner

Run this from the repository root with matching compiler libraries.
The commands compile ordinary Haskell source to objects
and link an executable. They do not construct Core through the GHC API.

The runner uses one serial job with a 4 GiB limit. The consumer import path
contains only the copied interface. The producer source directory is absent
from that path. Keep the producer object for linking. Use a new work directory
for each compiler checkpoint.

```bash
repo_root=$PWD
/home/tritlo/Code/heavy-management/bin/capped --mem 4G --time 2m \
  bash -s -- "$repo_root" <<'RUN'
set -euo pipefail
repo_root=$1
compiler=${PACKED_VECTOR_GHC:-$repo_root/_build/stage1/bin/ghc}
work=${PACKED_VECTOR_WORK:-$repo_root/_build/verification/packed-vector-source}
example=$repo_root/docs/dependent-haskell/examples/packed-vector
mkdir -p "$work/producer" "$work/consumer"
cp "$example/PackedVecLibrary.hs" "$work/producer/PackedVecLibrary.hs"
cp "$example/Main.hs" "$work/consumer/Main.hs"
flags=(-O0 -fomit-interface-pragmas -fno-write-if-simplified-core \
       -dcore-lint -dstg-lint -fforce-recomp)
"$compiler" "${flags[@]}" -i -c "$work/producer/PackedVecLibrary.hs" \
  -odir "$work/producer" -hidir "$work/producer"
cp "$work/producer/PackedVecLibrary.hi" "$work/consumer/PackedVecLibrary.hi"
"$compiler" "${flags[@]}" -i -i"$work/consumer" -c "$work/consumer/Main.hs" \
  -odir "$work/consumer" -hidir "$work/consumer"
"$compiler" -O0 "$work/consumer/Main.o" "$work/producer/PackedVecLibrary.o" \
  -o "$work/packed-vector"
"$work/packed-vector"
RUN
```

The prototype requires `-O0 -fomit-interface-pragmas
-fno-write-if-simplified-core`. Core Lint and STG Lint must pass. A successful
source compile is a prerequisite for execution.

## Observed output

```text
runtime package: (0,[])
runtime package: (2,[42,42])
runtime package: (3,[42,42,42])
constructor with bottom fields: ok
repack with bottom fields: ok
repack with cyclic fields: ok
ignore bottom fields: 42
ignore cyclic fields: 42
partial length payload head: 42
cyclic length payload head: 42
demand payload before length: packed-bottom-payload
demand stored length: packed-bottom-length
ignore bottom package: packed-bottom-package
repack bottom package: packed-bottom-package
unused whole package: 42
runtime filter: (3,[1,2,3])
runtime filter: (2,[2,3])
runtime filter: (0,[])
filter empty ignores predicate: (0,[])
filter rejects bottom element: (0,[])
filter retains bottom element: ok
filter retained length: 1
filter demands retained element: packed-filter-bottom-element
filter payload before length: packed-bottom-payload
filter predicate before tail: packed-filter-bottom-predicate
filter scans partial tail: packed-bottom-length
filter demands package: packed-bottom-package
unused filtered package: 42
left section with bottom operator: ok
right section with bottom operator: ok
left section with bottom operand: ok
right section with bottom operand: ok
applied left section demands operator: packed-section-bottom-operator
applied right section demands operator: packed-section-bottom-operator
applied left section demands operand: packed-filter-bottom-element
applied right section demands operand: packed-filter-bottom-element
runtime bounded filter: (3,[1,2,3],3)
runtime bounded filter: (2,[2,3],2)
runtime bounded filter: (0,[],0)
bounded empty ignores predicate: (0,[],0)
bounded rejects bottom element: (0,[],0)
bounded retains bottom element: ok
bounded retained proof depth: 1
bounded retained length: 1
bounded demands retained element: packed-filter-bottom-element
bounded constructor with bottom proof: ok
bounded length ignores proof: 1
bounded vector ignores proof: (1,[42])
bounded demands proof: bounded-bottom-proof
bounded partial proof constructor: ok
bounded demands recursive proof: bounded-bottom-proof
bounded constructor with weakened proof: ok
bounded vector ignores weakened proof: 42
bounded demands weakened proof: bounded-bottom-proof
bounded vector ignores cyclic proof: 42
bounded predicate before tail: packed-filter-bottom-predicate
bounded scans partial tail: packed-bottom-length
unused bounded filter: 42
runtime nested patterns: (Just 1,3)
runtime nested patterns: (Just 2,2)
runtime nested patterns: (Nothing,0)
nested empty head: Nothing
nested partial tail head: Just 42
nested bottom element constructor: ok
nested demands element: packed-filter-bottom-element
nested payload before index: packed-bottom-payload
nested proof before fields: bounded-bottom-proof
nested partial proof: bounded-bottom-proof
wildcard proof repack: ok
wildcard proof ignores bottom: Just 42
wildcard proof ignores cyclic: Just 42
early catchall ignores payload: 42
reversed catchall demands payload: packed-bottom-payload
runtime guarded repack: (2,[42,42])
runtime guarded repack: (2,[42,42])
guard fallback bottom fields: ok
guard fallback cyclic fields: ok
nested head bottom package: packed-bottom-package
nested proof bottom package: bounded-bottom-package
wildcard repack bottom package: bounded-bottom-package
guard fallback bottom package: packed-bottom-package
runtime uncons: Nothing
runtime uncons: 42:(1,[42])
runtime uncons: 42:(2,[42,42])
runtime drain filtered: [1,2,3]
runtime drain filtered: [2,3]
runtime drain filtered: []
uncons zero ignores payload: Nothing
drain zero ignores payload: []
uncons index before payload: packed-bottom-length
uncons successor demands vector: packed-bottom-payload
uncons partial result: ok
uncons partial pair: ok
uncons partial tail package: ok
uncons demands head: packed-filter-bottom-element
uncons demands predecessor: packed-bottom-length
uncons demands tail payload: packed-bottom-payload
uncons cyclic predecessor result: ok
uncons cyclic predecessor tail package: ok
uncons cyclic predecessor head: 42
uncons cyclic bounded prefix: [42,42,42]
uncons bottom package: packed-bottom-package
runtime equality zip: same Just (0,[])
runtime equality zip: different Nothing
runtime equality zip: different Nothing
runtime equality zip: same Just (2,[(42,7),(42,7)])
runtime equality zip: different Nothing
runtime equality zip: different Nothing
runtime equality zip: same Just (3,[(42,7),(42,7),(42,7)])
runtime filtered zip: Just (3,[(1,10),(2,20),(3,30)])
runtime filtered zip: Just (2,[(2,20),(3,30)])
runtime filtered zip: Nothing
runtime filtered zip: Just (0,[])
equality zero successor ignores predecessor: Nothing
equality successor zero ignores predecessor: Nothing
equality cyclic successor zero: Nothing
equality demands left length: packed-bottom-length
equality demands right length: equality-right-length
equality same index still demands length: packed-bottom-length
equality demands left predecessor: packed-bottom-length
equality demands right predecessor: equality-right-length
zip mismatch ignores vectors: Nothing
zip reversed mismatch ignores vectors: Nothing
zip demands left package first: packed-bottom-package
zip demands right package: zip-right-package
zip equal result ignores vectors: ok
zip equal package ignores vectors: ok
zip length ignores vectors: 1
zip vector demands left tag: zip-left-vector
zip vector demands right tag: zip-right-vector
zip tuple ignores components: ok
zip demands left head: packed-filter-bottom-element
zip demands right head: zip-right-head
zip tail package ignores tails: ok
zip demands left tail: zip-left-vector
zip demands right tail: zip-right-vector
runtime append: (0,[])
runtime append: (2,[7,7])
runtime append: (2,[42,42])
runtime append: (4,[42,42,7,7])
runtime append: (5,[42,42,7,7,7])
runtime append: (5,[42,42,42,7,7])
runtime append: (6,[42,42,42,7,7,7])
runtime filtered append: (6,[1,2,3,10,20,30])
runtime filtered append: (4,[2,3,20,30])
runtime filtered append: (5,[2,3,10,20,30])
runtime filtered append: (0,[])
partial sum keeps bottom left: ok
partial sum keeps cyclic left: ok
sum zero ignores right bottom: ok
sum successors ignore right bottom: ok
sum ignores cyclic right: ok
sum demands left length: packed-bottom-length
sum demands left predecessor: packed-bottom-length
sum proof ignores right bottom: ok
sum length head ignores right bottom: S
sum length demands right bottom: equality-right-length
sum constructor ignores bottom fields: ok
sum stored length demand: packed-bottom-length
sum stored proof demand: append-bottom-proof
sum length ignores proof: 0
unused append: 42
append demands left package first: packed-bottom-package
append demands right package before lengths: append-right-package
append demands left length before vectors: packed-bottom-length
append scans first length: packed-bottom-length
append zero ignores right length and vector: ok
append zero demands right vector: zip-right-vector
append zero length demands right length: equality-right-length
append package ignores vectors: ok
append length ignores vectors: 2
append vector demands left tag: zip-left-vector
append first head ignores right vector: 42
append tail demands right vector: zip-right-vector
append package ignores left element: ok
append demands left element: packed-filter-bottom-element
append first head ignores left tail: 42
append demands left tail: zip-left-vector
append finite left ignores cyclic right: 42
append cyclic right bounded prefix: [42,42,42]
runtime equal tail patterns: (0,[])
runtime equal tail patterns: (1,[42])
equal patterns demand proof: append-bottom-proof
equal patterns ignore predecessors before proof: append-bottom-proof
equal patterns keep partial tail package: ok
equal patterns demand tail vector: packed-bottom-length
equal patterns demand tail length: packed-bottom-length
equal patterns demand first index: packed-bottom-length
equal patterns demand second index: equality-right-length
sum zero pattern ignores proof: 0
sum successor pattern ignores predecessor and proof: 1
sum pattern demands stored index: packed-bottom-length
index zero vector pattern: True
explicit zero vector pattern demands payload: packed-bottom-payload
index pattern before payload: packed-bottom-length
successor pattern ignores predecessor and vector: False
index pattern before later argument: packed-bottom-length
zero pattern then later argument: pattern-flag
vector pattern before later argument: packed-bottom-payload
zero pattern selects first row: 0
successor pattern selects element: 1
later argument mismatch reaches catchall: 42
index pattern before later view: packed-bottom-length
zero pattern then later view: pattern-view
index mismatch skips later view: 1
index guard ignores cyclic fields: 42
index guard failure reaches catchall: 99
index mismatch skips guard argument: 0
index pattern before guard: packed-bottom-length
runtime reverse: (0,[])
runtime reverse: (2,[42,42])
runtime reverse: (3,[42,42,42])
runtime filtered reverse: (3,[3,2,1])
runtime filtered reverse: (2,[3,2])
runtime filtered reverse: (0,[])
runtime reverse append: (5,[3,2,1,7,7])
runtime reverse append: (3,[3,2,7])
runtime reverse append: (2,[7,7])
runtime reverse arithmetic proofs: Refl
reverse constructor with bottom fields: ok
reverse constructor with cyclic fields: ok
reverse keeps stored length: 2
reverse keeps partial stored constructor: 42
reverse stored length demands index: packed-bottom-length
reverse index before payload: packed-bottom-length
reverse partial index before payload: packed-bottom-length
reverse zero inspects payload: packed-bottom-payload
reverse successor inspects payload: packed-bottom-payload
reverse complete spine before head: packed-bottom-payload
reverse lazy element result: ok
reverse demands element: packed-filter-bottom-element
reverse preserves last element: 7
reverse later element demand: packed-filter-bottom-element
reverse bottom package: packed-bottom-package
unused reverse package: 42
partial reverse ignores bottom index: ok
partial reverse ignores cyclic index: ok
partial reverse append ignores indices: ok
partial reverse append ignores payload: ok
empty reverse append inspects first vector: zip-left-vector
empty reverse append ignores right index: zip-right-vector
successor proof zero ignores right index: Refl
successor proof demands first index: packed-bottom-length
successor proof demands remaining index: packed-bottom-length
right zero proof demands index: packed-bottom-length
ignored reverse proof stays lazy: 42
reverse append payload before predecessor: packed-bottom-payload
reverse append proof before tail: packed-bottom-length
reverse append ignores right fields: 42
runtime later indices: (2,[7,7])|(3,[9,9,9])|42
runtime later indices: (3,[7,7,7])|(0,[])|42
runtime later evidence: ([],[9,9])
runtime later evidence: ([7,7],[9,9,9])
later partial ignores first index: ok
later partial ignores cyclic first index: ok
later partial ignores all supplied fields: ok
later tuple ignores later fields: ok
later left package ignores partial index: ok
later right package ignores cyclic first index: ok
later first index before payloads: packed-bottom-length
later left stored index demand: packed-bottom-length
later right stored index demand: equality-right-length
later left payload demand: zip-left-vector
later right payload demand: zip-right-vector
later ordinary shadowing: 42
later evidence partial ignores indices: ok
later evidence first index before payloads: packed-bottom-length
runtime leading context: ([],[7])
runtime leading context: ([42,42],[7])
```

These observations check this source subset. They do not establish a general
existential package implementation, optimizer support, or support for all
dependent constructor telescopes.


## Two constructor columns

M20 checks `sameNat` as four constructor equations. The successor/successor
row uses its recursive `Refl`. Returning `Just Refl` without that evidence
rejects. `Main.sameNatByCase` keeps the previous nested-case algorithm as a
reference. Sixteen independent IORef input pairs cover lengths 0 through 3.
Both implementations must agree with an independent result table. Demand
checks force successful evidence, preserve partial function WHNF, compare
left-before-right exceptions and leave mismatched successor predecessors lazy.
A cyclic successor can mismatch zero after one constructor. Equal cyclic
inputs have no termination claim.

`matrixPayload` places a vector argument between its two retained indices.
Four runtime cases check both returned vectors. Partial WHNF and bottom order
checks cover its four source argument lambdas. This exercises transport
before and after the second selected constructor column.

The initial dependent matrix requires complete unique constructor pairs and
bare subfields. Interior contexts, mixed constructor/variable rows and nested
constructor fields reject with source diagnostics. Other retained columns
remain bare variables. An unused `foreach` binder can become an ordinary
value arrow, so coverage probes keep both indices in `Maybe (n :~: m)`.
The next source target is a checked trailing variable fallback.


## Ordered final variable equation

M21 replaces the two explicit mismatch equations with a final variable row.
The checker gives this row no constructor equality. It checks its RHS at the
original `Maybe (n :~: m)` type. Its two source variable Names become the
shared whole-index Names before TC. The checked index VarPats stay unwrapped.
A fallback `Just Refl` rejects because the two original indices are independent.

`sameNatByMatrix` keeps the four-clause reference. The existing finite equality
and demand observations compare it with the case and fallback workers.
`fallbackPacked` places a payload before the second retained column and
repackages both vectors with their original indices in fallback. Four runtime
pairs check stored lengths and contents. `zeroOnly` has only `Z/Z` before
fallback. Four finite pairs and seven demand checks establish that an absent
first constructor can leave the second index untouched. They also check
partial function WHNF and package laziness at partial mismatch indices.

M21 admitted one final row with bare variables in both selected index columns.
Constructor pairs before it remain unique. A row with one constructor and one
variable still rejects. M22 extends the fallback below. No equality follows
from a let alias or a failed row.


## Anonymous final equation

M22 uses `sameNat _ _ = Nothing`. A selected fallback column can contain a
bare variable or wildcard. Anonymous columns get internal VarPats with the
existing whole-index Ids before TC. They add no source-visible name and no
constructor equality. The original source equation remains available for errors.

Both compiler stages pass 331 source observations; the original 316 remain
unchanged. `sameNatByVariables`, `sameNatByMatrix` and `sameNatByCase` retain
the named fallback, four-clause and nested-case references. `fallbackFirst`
and `fallbackSecond` check named-index package identity beside an anonymous
index. `zeroOnlyAnonymous` checks a both-wildcard fallback against `zeroOnly`.
Four runtime pairs and eleven demand observations check finite results, partial
function WHNF, lazy payload fields and selective index demand.

Unsigned local vector bindings still reject inferred retained types. The probe
passes inline payload expressions to the workers. Complete local payload
signatures that mention an ambient runtime index remain the next source task.
