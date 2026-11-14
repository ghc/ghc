# Packed vector source example

The bootstrap and self-hosted compilers compile and run this source example.
The consumer imports the library interface with the producer source absent
from its import path. All 81 observations below pass with Core Lint and STG
Lint enabled.

`PackedVecLibrary.hs` stores a runtime `Nat` and a `Vec` indexed by that same
field. `PackVec` has two ordinary runtime fields. It has no singleton argument.
`withPacked` opens their shared scope for a checked consumer. `repack` constructs
a new package with the same field identity. `ignorePacked` inspects the package
constructor and does not evaluate its fields.

The compiler checks the relation between the fields. It rejects `PackVec m xs`
when `xs :: Vec Int n` and `m` and `n` are independent runtime bindings. Opening
a package gives the consumer a fresh scoped index. Its vector cannot escape as
`Vec Int m` for an unrelated index. Both rejections are regression checks.

The source subset has one package constructor. Retained index fields require
bare variable patterns. Ordinary fields admit GHC patterns, including nested
vector/proof constructors and wildcards. The
length domain is the closed first-order datatype `Nat`. The element parameter
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
`VCons x xs` with GHC-05617. Wildcards and constructors in the retained length
field still reject with GHC-83605. No Core form or interface format changes.

General projections and constructor patterns in retained index fields remain
separate work. This increment does not establish totality or proof erasure.

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
```

These observations check this source subset. They do not establish a general
existential package implementation, optimizer support, or support for all
dependent constructor telescopes.
