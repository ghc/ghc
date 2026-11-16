# eps-hackage: EPS benchmark on a large Hackage package

Cabal-syntax depends only on boot packages, so its External Package State
(EPS) is small. This benchmark compiles **pandoc-3.14's library** (~220
modules) instead. With ~165 Hackage dependencies, its EPS holds the
interfaces of many external packages.

## Layout

| Path | What |
|---|---|
| `project/cabal.project` | The test project: pandoc, pinned index states, boot packages from the compiler, head.hackage |
| `project/vendor/` | Hackage releases with the minimal changes they need for GHC HEAD; see `vendor/CHANGES.md` |
| `tool/` | `eps-hackage` (system GHC): `deps` builds pandoc's dependencies with a given compiler, `flags` records the GHC arguments cabal uses for it (via `cabal repl --with-repl`), `summary` prints result tables |
| `runner/` | `eps-hackage-run` (built with the compiler under test): compiles pandoc with the GHC API and measures it in-process. With the cabal flag `abstract-eps`, `--store` installs an EPS store from `bench/eps-store` |
| `common/` | The `Result` type shared by both |
| `bench.sh` | Coordination only |

Each compiler gets its own cabal store and build directory under
`work/LABEL/`. All the builds report the same GHC version, but their libraries
differ.

## Usage

```sh
# once per compiler (takes a while: ~165 packages)
bench/eps-hackage/bench.sh setup head         ../ghc/_default+ipe
bench/eps-hackage/bench.sh setup abstract-eps _default+ipe abstract-eps

# compile pandoc's library: GHC's default EPS for both compilers, and the
# EPS stores of bench/eps-store for this tree
RUNS=3 bench/eps-hackage/bench.sh run head ghc-api
RUNS=3 bench/eps-hackage/bench.sh run abstract-eps ghc-api store:memory store:hi store:sqlite

# tables; the first result is the baseline
bench/eps-hackage/bench.sh summary bench/eps-hackage/results/head/*.result \
                                   bench/eps-hackage/results/abstract-eps/*.result
```

`run` takes `GHC_OPTS` (default `-j1`) and `RUNS`. With `HASHES=1` (and
`GHC_OPTS="-j1 -fobject-determinism"`) it records hashes of all `.hi`/`.o`
outputs. `summary` then checks that all runs of a label produce identical
files.

## What is measured

All runs use the same program, `eps-hackage-run`, a GHC API equivalent of
`ghc --make`, so HEAD and this tree are compared like for like:

* wall time of the `load`, mutator and GC CPU time, and bytes allocated;
* max residency (sampled at major GCs) and the peak memory in use by the RTS;
* max RSS (`VmHWM`). It includes memory outside the GHC heap, such as
  SQLite's page cache;
* live bytes after a major GC at the end, while the session (and the EPS) is
  still alive;
* for the low-memory stores: rehydrated declarations.

## Building Hackage with GHC HEAD

* **head.hackage.** `cabal.project` contains the repository and constraints of
  <https://ghc.gitlab.haskell.org/head.hackage/cabal.project>, minus its pins
  of boot packages. Boot packages are always the installed ones.
* **Pinned indices.** `index-state` pins Hackage and head.hackage. Run
  `cabal update` once if cabal complains that it doesn't know the index state.
* **Vendored fixes.** Packages that don't build with GHC HEAD and aren't
  patched by head.hackage are vendored with minimal changes
  (`vendor/CHANGES.md`): a missing `base` dependency, noncanonical
  `mappend`/`return` definitions (hard errors now), and `KindRepTYPE`.
