# PLFH shared notebook

The PLFH author and the GHC compiler team use this file to exchange working proofs, compiler gaps and fixes. The [starting prompt](docs/dependent-haskell/plfh/PROMPT.md) gives the scope. PLFH sources belong under `docs/dependent-haskell/plfh/`.

Append a dated entry with the author, chapter and theorem. For a blocker, include the smallest source path, compiler commit, command, expected result and actual diagnostic with its source location. Record a Serokell card when the connection is known. The compiler team replies under that entry with a reproducer, fix or explanation. The PLFH author reruns the original example and records the outcome. Read new entries when resuming work and after each batch. Keep previous failures visible when a fix lands.

## 2026-11-24 · Compiler coordinator · Naturals and Induction

The first PLFH target is addition associativity with three arbitrary operands. HaskLib already contains the right-identity and successor lemmas. No PLFH theorem has been ported yet. Use the ordinary `plus` definition and record whether the fork accepts it in the statement of the new theorem. [Existing proofs](docs/dependent-haskell/hasklib/HaskLib/Size.hs)

On this host, the starting compiler was built from `db2586fdc2f391829174a5652d8744e9a9b761f8`. Job 2019 checked the existing HaskLib induction module with this compiler. Use the shared queue for PLFH checks. Its two GHC slots share a 16-core, 64 GB limit and give priority to other benchmarks. The [compiler handoff](docs/dependent-haskell/HANDOFF.md) records replacement compilers when old build products are retired.

```bash
cd /home/tritlo/Code/ghc
compiler="$PWD/_build/on-demand/r04-r05-r17-combined-584-20261123/source/_build/stage1/bin/ghc"
"$compiler" --info | rg 'Project Git commit id'
out=$(mktemp -d)
CAPPED_MEM=4G docs/dependent-haskell/tools/ghc-build-job plfh-baseline \
  "$compiler" --make -v0 -fforce-recomp -fno-code -O0 -package-env - -i \
  -idocs/dependent-haskell/hasklib -idocs/dependent-haskell/examples/open-evaluation \
  -outputdir "$out" docs/dependent-haskell/hasklib/HaskLib/Size.hs
```

The scheduler returns a job ID. Use `pueue log ID` to read the result. Add `-idocs/dependent-haskell/plfh` when checking a PLFH module. Record the compiler commit even if the executable path stays the same across builds.
