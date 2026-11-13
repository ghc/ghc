# Packed vector source target

**TARGET: this example does not compile yet.** Retained constructor declaration
and source pattern checking remain pending. These files specify the next source
acceptance program. They have not been compiled or executed.

`PackedVecLibrary.hs` stores a runtime `Nat` and a `Vec` indexed by that same
field. `PackVec` has two ordinary runtime fields. It has no singleton argument.
`withPacked` opens their shared scope for a checked consumer. `repack` constructs
a new package with the same field identity. `ignorePacked` inspects the package
constructor and does not evaluate its fields.

The initial source subset has one constructor and one equation per eliminator.
The constructor patterns bind both fields with bare variable patterns. The
length domain is the closed first-order datatype `Nat`. The element parameter
`a` remains an erased universal. The stored fields have unrestricted
multiplicities. Strict fields, record fields, existential binders, constructor
contexts, nested field patterns, guards, and multiple constructor equations are
outside this first constructor-field increment. Existing Nat recursion and Vec
GADT patterns remain in the program.

`Main.hs` imports the library. It passes lengths 0, 2, and 3 through an IORef to
the same worker. It checks constructor weak-head normal form, field sharing after
repackaging, unused bottom and cyclic fields, demanded payloads, and a bottom
package. It reads one payload constructor at a partial or cyclic length. It does
not fully evaluate an infinite length or a cyclic bottom payload. Exception
checks catch only `ErrorCall` and require the named marker.

## Proposed separate-module runner

Run this from the repository root after source support and matching compiler
libraries are available. The commands compile ordinary Haskell source to objects
and link an executable. They do not construct Core through the GHC API.

The runner uses one serial job with a 4 GiB limit. The consumer import path
contains only the copied interface. The producer source directory is absent
from that path. Keep the producer object for linking. Use a new work directory
for each compiler checkpoint. This proposal has not been executed.

```bash
repo_root=$PWD
/home/tritlo/Code/heavy-management/bin/capped --mem 4G --time 2m \
  bash -s -- "$repo_root" <<'RUN'
set -euo pipefail
repo_root=$1
compiler=${PACKED_VECTOR_GHC:-$repo_root/_build/stage1/bin/ghc}
work=${PACKED_VECTOR_WORK:-$repo_root/_build/verification/packed-vector-source}
example=$repo_root/docs/dependent-haskell/examples/packed-vector
mkdir -p "$work/producer" "$work/consumer" "$work/hi"
cp "$example/PackedVecLibrary.hs" "$work/producer/PackedVecLibrary.hs"
cp "$example/Main.hs" "$work/consumer/Main.hs"
flags=(-O0 -fomit-interface-pragmas -fno-write-if-simplified-core \
       -dcore-lint -dstg-lint -fforce-recomp)
"$compiler" "${flags[@]}" -i -c "$work/producer/PackedVecLibrary.hs" \
  -odir "$work/producer" -hidir "$work/producer"
cp "$work/producer/PackedVecLibrary.hi" "$work/hi/PackedVecLibrary.hi"
"$compiler" "${flags[@]}" -i -i"$work/hi" -c "$work/consumer/Main.hs" \
  -odir "$work/consumer" -hidir "$work/consumer"
"$compiler" -O0 "$work/consumer/Main.o" "$work/producer/PackedVecLibrary.o" \
  -o "$work/packed-vector"
"$work/packed-vector"
RUN
```

The prototype requires `-O0 -fomit-interface-pragmas
-fno-write-if-simplified-core`. Core Lint and STG Lint must pass. A successful
source compile is a prerequisite for execution. A current rejection is not a
successful acceptance result.

## Required output

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
```

These observations are requirements. They are not verified results. They do not
establish a general existential package implementation, optimizer support, or
support for all dependent constructor telescopes.
