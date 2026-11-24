# PLFH shared notebook

The PLFH author and the GHC compiler team use this file to exchange working proofs, compiler gaps and fixes. The [starting prompt](docs/dependent-haskell/plfh/PROMPT.md) gives the scope. PLFH sources belong under `docs/dependent-haskell/plfh/`.

Append a dated entry with the author, chapter and theorem. For a blocker, include the smallest source path, compiler commit, command, expected result and actual diagnostic with its source location. Record a Serokell card when the connection is known. The compiler team replies under that entry with a reproducer, fix or explanation. The PLFH author reruns the original example and records the outcome. Read new entries when resuming work and after each batch. Keep previous failures visible when a fix lands.

## 2026-11-24 04:27 CET · Exponentiation and the De Bruijn value case

The [exponentiation fix](https://github.com/Tritlo/ghc/commit/1f873969e589e84b632e5d0304399ab4d2c997e3) checks ordinary `pow` from Naturals. Job 2420 checked `powPlus`, `powMul` and `powTimes` through compiled interfaces at both optimization levels. The proofs are in `_build/on-demand/r13-pow-open-eq-20261124/inputs/PowLaws.hs` on this host. Job 2433 passed all 27 patched modules in four configurations. Of 29 reports, 17 compiled at `-O0` and 16 at default `-O2`.

The [De Bruijn fix](https://github.com/Tritlo/ghc/commit/2e4fbb752a2fca6d9d4ac4b7901e2aa07b3c1534) checks the case-based value classification over the chapter's actual term constructors. This is one prerequisite of progress. Job 2441 checked the chapter proof and its source-hidden client at both optimization levels. The separate direct-equation example remains open. [Verification record](docs/dependent-haskell/CHECKPOINTS.md#exponentiation-and-keyed-trees).

## 2026-11-24 04:15 CET · Published compiler for the PLFH Induction exercises

The [subtraction fix and regression](https://github.com/Tritlo/ghc/commit/63e2f0974b174dc86bef79c3a84d1d76fea3dadf) are on `dependent-haskell`. The unchanged `R13_MonusOpenEq` compiles. Induction can prove `zeroMonus` and `monusPlusAssoc` with ordinary `monus` from Naturals. Its clause order still determines which operand is forced.

Two independent chapter patches apply to PLFH `85f90be`. Fetch `refs/yardsticks/plfh/monus-induction-85f90be` or `refs/yardsticks/plfh/unsplit-preservation-85f90be` from `/home/tritlo/Code/ghc` in the book checkout. Job 2405 tested them together. All 27 modules passed the interface audit in four fresh configurations. Of the 29 reproducers, 16 compiled at `-O0` and 15 at default `-O2`. The difference is the append rule in `R13_ShuntAccumOpenEq`. Job 2414 matched all 600 Showcase outputs at native `-O0`, default `-O2`, and in GHCi.

## 2026-11-24 03:46 CET · Published compiler for the PLFH preservation proof

The GHC fixes for this proof are on [Tritlo/ghc at `692eae0778`](https://github.com/Tritlo/ghc/commit/692eae077896a00384fecf15a3d5d92d828a6415) on `dependent-haskell`. This revision includes same-module substitution, fresh `-fno-code` interfaces, list countdown and accumulator equations, and the typed-splice fix.

On that compiler, job 2381 checked all 27 modules of the audited book revision in four configurations, native and no-code at both `-O0` and `-O2`. The proof uses the ordinary local `substTerm`. Its beta constructors bind their type variables explicitly. Both native runs exercised all eight reduction constructors. The compiler rejected a false beta target and an extra substitution argument.

For the book maintainer, please cite the public GHC revision above when updating chapter status. The book proof itself is a separate change to Tritlo/PLFH. The [compiler handoff](docs/dependent-haskell/HANDOFF.md) records verification scope and open compiler work.

The follow-up [`9cc4065fab`](https://github.com/Tritlo/ghc/commit/9cc4065fab4f2f106859b39daa9bdad23f00f0fd) fixes the nested retained case in Part 3's `subInvDist`. The compiler kept equality evidence used by the inner case. Job 2389 checked the original failure, runtime branch behavior and rejection of the wrong witness at both optimization levels.

Job 2394 checked 27 modules in four configurations. Of the 29 PLFH compiler reports, 15 compile at `-O0` and 14 at default `-O2`. The difference is the append proof in `R13_ShuntAccumOpenEq`.

The preservation chapter patch also applies to PLFH `85f90be`. From the book checkout, use `git fetch /home/tritlo/Code/ghc refs/yardsticks/plfh/unsplit-preservation-85f90be` and inspect `git show FETCH_HEAD`. The patch changes the literate chapters, their extracted Haskell, and the negative test for the beta target.

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
