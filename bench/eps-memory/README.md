# EPS memory benchmark

Measures how much memory is reachable from GHC's `ExternalPackageState` (EPS)
after `ghc --make` has compiled a package (default: `Cabal-syntax` from
`../../cabal`), using a ghc-debug-instrumented GHC.

## Pieces

| File | What it does |
|---|---|
| `ghc/Main.hs` (`debugPauseAtEnd`) | With `GHC_DEBUG_PAUSE_AT_END=1`, after `--make` finishes: saves the EPS and the `HscEnv` as ghc-debug roots, does a major GC, prints `ghc-debug: live_bytes=N` and pauses forever. Only in ghc-debug builds (`GHC_DEBUG`, stage-2 `ghc`). |
| `freeze-compiler.sh LABEL [BUILD_ROOT]` | Copies `BUILD_ROOT/stage1/{bin,lib}` (default `_default+ipe`) to `compilers/LABEL`, with the commit, the uncommitted diff and the matching `GHC/Unit/External.hs`. The install is relocatable. |
| `eps-flags` (`app/Flags.hs`) | Runs `hie-bios flags` in a cradle under `work/LABEL/cradle` (symlink to the package, `hie.yaml`, `cabal.project.eps-bench` pinning the frozen compiler). Writes a GHC response file without output-dir flags and with the chosen `-O` level. The cabal checkout is not modified. |
| `eps-analyse` (`app/Analyse.hs`) | Connects to the paused GHC, takes the saved EPS closure, and for every EPS field (names parsed from the frozen `External.hs`) computes the reachable size, exclusive size, `ARR_WORDS` bytes and the top closure types. Writes `eps.md` and `eps.hsdat`. |
| `eps-compare` (`app/Compare.hs`) | Compares several `eps.hsdat` files; the first is the baseline. |
| `run-peak.sh LABEL` / `eps-peak` (`app/Peak.hs`) | Compiles once with `-ddump-timings` (phase markers) and `+RTS -l -hT -i0.25`, writing `results/LABEL/peak.eventlog`. `eps-peak analyse` finds the peak live bytes after a major GC, the GHC phase/module running then, and the `-hT` census nearest to it; `eps-peak compare A B` diffs two of them. |
| `run-bench.sh LABEL` | Builds the tools, computes flags, runs GHC once unpaused (`+RTS -t --machine-readable`) and once paused for the analysis, then kills GHC. Results go to `results/LABEL/`. |

The debugger tools are built with the system GHC (ghcup default) against the
`../../ghc-debug` checkout; `ghc-debug-client` has no RTS-version-specific code.

## Usage

```sh
# 1. build a ghc-debug compiler (ghc-debug is linked into the stage-2 ghc)
./hadrian/build -o_default+ipe --flavour=default+ipe --freeze1 -j
# 2. freeze it
bench/eps-memory/freeze-compiler.sh head
# 3. measure
bench/eps-memory/run-bench.sh head
# ... repeat 1-3 with the patched tree as e.g. `slim-eps`, then
cd bench/eps-memory
cabal run -v0 eps-compare -- results/head/eps.hsdat results/slim-eps/eps.hsdat > results/comparison.md
```

Environment knobs of `run-bench.sh`: `CABAL_DIR`, `PACKAGE`, `OPT` (default
`-O1`), `GHC_OPTS` (default `-j1`), `SKIP_RTS=1`.

## Caveats

* RTS `max_bytes_used` is the maximum over the (few, ~20 per build) major
  GCs and usually misses the real peak, which lasts a few seconds while the
  biggest module is compiled. Use `run-peak.sh` (a census every 0.25 s) or
  `+RTS -F1.2` (more major GCs) to measure the peak.

* "Reachable" sizes include structure shared with other fields, and with the
  rest of the session (e.g. `Name`s, `FastString`s), so fields do not add up.
  "Exclusive" is the EPS total minus what the other fields reach.
* `gcRoots` cannot be used after the debuggee paused itself (the RTS refuses:
  "rts_listThreads: called from a different OS thread than rts_pause"), so the
  analysis only starts from the saved objects (EPS and `HscEnv`).
* "big ARR_WORDS" are byte arrays of at least 16 KiB, typically whole `.hi`
  buffers, or large copied slices of them.
* Sizes are ghc-debug closure sizes in bytes. They do not include
  fragmentation of pinned blocks.
