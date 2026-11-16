# Singleton comparison programs

These programs preserve the temporary Boolean probes checked on 2026-11-11.
They are comparison implementations for the overlapping singleton subset of
[the native Core plan](../PLAN.md). They do not implement native dependent Core.

- `Oracle.hs` defines its own Boolean singleton. It checks selected values,
  constructor refinement, shared indices, and an ignored undefined witness.
- `Reject.hs` must fail to type-check. It passes `False` where `Select True` is `Int`.
- `LibraryOracle.hs` uses upstream `Data.Singletons` with a small local Boolean
  adapter. It tests the foundational API only.
- `check.sh` compiles at `-O0` and `-O2` with `-dcore-lint`, compares exact output,
  and checks the expected rejection. It optionally checks the pinned upstream API.

## Run the standalone comparison

Run from the GHC repository root. Use Bash, `rg`, `diff`, and the built compiler.
The baseline compiler is `_build/stage1/bin/ghc`, version `10.1.20261109`, built
from `19ff36f2aede0ca2e554016ff22c71e73aa9803b`. The script accepts another compiler
as its first argument. It writes artifacts to a new temporary directory and prints
that directory's path. It does not download source or modify the compiler.

On the current WSL host, run compilation through the existing memory-cap tool:

```sh
mkdir -p _build/verification
export XDG_RUNTIME_DIR="/run/user/$(id -u)"
export DBUS_SESSION_BUS_ADDRESS="unix:path=$XDG_RUNTIME_DIR/bus"
HEAVY_PEAKS="$PWD/_build/verification/dependent-haskell-reference.peaks" \
  /home/tritlo/Code/heavy-management/bin/capped --mem 4G --time 5m \
  bash docs/dependent-haskell/reference/check.sh "$PWD/_build/stage1/bin/ghc"
```

The standalone program prints the following at both optimization levels:

```text
(1,False,True,True)
()
naive-bridge-forces-input
```

The second line checks ``keep @True undefined `seq` ()``. The final line records
that the ordinary-Boolean bridge forces its input. The probe catches the exception
from `undefined`; it does not establish termination for arbitrary programs.

## Run the pinned upstream API comparison

The inspected upstream file has Git blob ID
`c6bf5970d85d28ed6edb4d887f14cfa127a01be2`. This identifies the contents of
`singletons/src/Data/Singletons.hs`, not a repository commit. The recipe retrieves
only that file into a temporary directory. It does not vendor external source.
It additionally requires `gh`, Python 3, and Git.

```sh
upstream_dir=$(mktemp -d /tmp/ghc-singletons-source.XXXXXX)
mkdir -p "$upstream_dir/Data"
gh api repos/goldfirere/singletons/git/blobs/c6bf5970d85d28ed6edb4d887f14cfa127a01be2 \
  > "$upstream_dir/blob.json"
python3 - "$upstream_dir/blob.json" "$upstream_dir/Data/Singletons.hs" <<'PY'
import base64
import json
import pathlib
import sys

blob = json.loads(pathlib.Path(sys.argv[1]).read_text())
pathlib.Path(sys.argv[2]).write_bytes(base64.b64decode(blob["content"]))
PY
mkdir -p _build/verification
export XDG_RUNTIME_DIR="/run/user/$(id -u)"
export DBUS_SESSION_BUS_ADDRESS="unix:path=$XDG_RUNTIME_DIR/bus"
HEAVY_PEAKS="$PWD/_build/verification/dependent-haskell-reference.peaks" \
  /home/tritlo/Code/heavy-management/bin/capped --mem 4G --time 5m \
  bash docs/dependent-haskell/reference/check.sh \
    "$PWD/_build/stage1/bin/ghc" "$upstream_dir"
```

The script checks the source blob before compilation. The API program prints
`library-reification-is-strict` in place of the standalone program's final line.
Both programs preserve the distinction between an ignored singleton witness and
strict conversion of an ordinary runtime Boolean.

## Evidence boundary

The original temporary programs passed the two optimization levels with Core Lint.
The retained script also checks their exact output. This covers defined Boolean
values and the explicit `undefined` probes. It does not prove contextual
equivalence, native substitution, optimizer safety for a new Core form, or a
transparent encoding of unrestricted retained arguments.

The retained script passed both comparisons and the expected rejection on
2026-11-11 with the baseline compiler. The capped run exited with status 0 and
recorded a 154 MiB peak under its 4 GiB cap. Its artifacts were written to
`/tmp/ghc-dependent-reference.x60S8H`; that temporary directory can expire.
The upstream comparison used exact bytes matching the pinned blob. The original
temporary source copy differed only by an extra final newline.

Both comparisons and the expected rejection also passed on the modified compiler
at `c1694431a1`. The check used both optimization levels with Core Lint and exact
upstream bytes. It recorded a 155 MiB peak under a 4 GiB cap. This is regression
evidence for the singleton comparison, not native dependent execution.

The upstream comparison compiles one unmodified foundational module with a local
adapter. It does not build `singletons`, `singletons-th`, or `singletons-base` as
packages. The inspected package bounds exclude the baseline's `base-4.23.0.0`;
the generated support also targets an older Template Haskell API. See
[the bibliography](../REFERENCES.md#upstream-implementation-and-compatibility).
