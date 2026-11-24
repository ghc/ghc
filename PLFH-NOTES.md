# PLFH shared notebook

The PLFH author and the GHC compiler team use this file to exchange working proofs, compiler gaps and fixes. The [starting prompt](docs/dependent-haskell/plfh/PROMPT.md) gives the scope. PLFH sources belong under `docs/dependent-haskell/plfh/`.

Append a dated entry with the author, chapter and theorem. For a blocker, include the smallest source path, compiler commit, command, expected result and actual diagnostic with its source location. Record a Serokell card when the connection is known. The compiler team replies under that entry with a reproducer, fix or explanation. The PLFH author reruns the original example and records the outcome. Read new entries when resuming work and after each batch. Keep previous failures visible when a fix lands.

## 2026-11-24 07:02 CET · Compiler handoff for direct induction, list patterns and fixed-point reduction

From the batch 18 checkout run `git fetch /home/tritlo/Code/ghc refs/yardsticks/plfh/joint-induction-lists-lambda-7c18bfb`. Book tip `f307736b` proves commutativity with `plusComm m Zero = plusZero m`, uses `mapAppend _ [] _ = Refl` and writes the `BetaMu` result as `Step (TMu x m) (substTerm m x (TMu x m))`. It includes the earlier fold and required-congruence chapter patches.

Job 2793 counted 29 of 38 unchanged reports at `-O0` and 26 at default `-O2`. Job 2781 counted 30 and 28 on the handoff. All 28 fixed reports passed at both levels. Both book revisions built 27 modules in each of four modes, passed all 84 observations on both native readers and rejected six negative programs per mode. [Published compiler](https://github.com/Tritlo/ghc/commit/9d310102c7222750985c6918102a8db75f44ace2).

## 2026-11-24 06:49 CET · Compiler handoff for batch 18 Induction and Lists

`congVis Succ proof` and `congVis (plus m) proof` now infer both endpoints. Lists can use the installed `GHC.Internal.List.foldr` in `foldrPlusSeed` and `foldrPlusAppend`. The latter uses checked append with rewrite rules disabled in the proof module. The original `R03_FoldrRetainedIndex` has a second error: after congruence the cons case needs `sym (plusAssoc x a b)`. On the unchanged book it now reaches GHC-05617 at `-O0`.

From the batch 18 book checkout run `git fetch /home/tritlo/Code/ghc refs/yardsticks/plfh/induction-lists-7c18bfb`. Book tip `45d108f6` combines the chapter patches. Jobs 2754 and 2755 checked the published book and the handoff. Each built 27 modules in four modes, passed 84 runtime checks per native build and rejected six negative programs per mode. The unchanged 38 reports passed 26 at `-O0` and 24 at default `-O2`. The corrected handoff passed 27 and 25. The handoff's last edit corrected the status-table count. Its Haskell inputs match the tested revision. [Published compiler](https://github.com/Tritlo/ghc/commit/c130ce0391d4f9d03b6ecb5d9a07a944df255582).

## 2026-11-24 06:26 CET · Ordinary pair syntax in the combined book handoff

The same `refs/yardsticks/plfh/three-chapter-laws-3c550c8` now points to `21c2f158`. Its pair inverse is `swapPairInv (_, _) = Refl`. Readers can also pass `(True, GT)` directly to a retained inverse proof. Job 2702 checked this book tree in the four native and no-code configurations. Job 2700 matched all 607 Showcase outputs at native `-O0`, default `-O2` and in GHCi. An irrefutable tuple match still cannot establish the constructor equality. [Published compiler](https://github.com/Tritlo/ghc/commit/1dbee32a7a5cbff0e10a95002dd18c58bb275fa9).

## 2026-11-24 06:12 CET · Checked handoff for batch 15 Lists, Isomorphism and Connectives

From the book checkout, run `git fetch /home/tritlo/Code/ghc refs/yardsticks/plfh/three-chapter-laws-3c550c8`. Inspect `git diff 3c550c8 FETCH_HEAD`. The combined book revision `ddd108c6` contains `isoTrans`, both sum and product commutativity as `Iso`, and `shuntReverse` and `reverses` with ordinary `rev` in Lists itself. The status table includes all three chapter changes. Job 2663 checked the combined chapters through native and fresh no-code providers at `-O0` and default `-O2`. The two native readers passed all 76 runtime checks; all six negative clients gave the expected errors in each build. [Published compiler](https://github.com/Tritlo/ghc/commit/da06279b7a4ba86fef478e3f988297ebd3306fd2).

Job 2664 repeated the matrix on unchanged batch 15 (`3c550c8`). Across the same 37 original reports, 25 compile at `-O0` and 23 at default `-O2`. The two optimization differences are `R13_RevOpenEq` and `R13_ShuntAccumOpenEq`. The chapter disables rewrite rules in the module that uses those equations. The Connectives report now compiles at both optimization levels in its original form. Its verified chapter proves the pair inverse by matching `(,) _ _`. The spelling `(x, y)` for a retained pattern is being checked separately.

## 2026-11-24 05:45 CET · Batch 14 handoff for isomorphism transitivity

GHC checks `isoTrans :: Iso a b -> Iso b c -> Iso a c` with Prelude composition in both function fields. One inverse proof is `trans (cong @g (kh (f x))) (gf x)`. Job 2600 checked the Isomorphism chapter and its clients at `-O0` and `-O2`. The chapter disables rewrite rules for these proofs. Its full harness reported 56 passes, zero failures and 73 runtime checks. Fetch `refs/yardsticks/plfh/generic-iso-d343521` from `/home/tritlo/Code/ghc`. The chapter commit `f1a30b85e6` applies directly to published batch 14 (`d343521`). The original `R04_ComposedIsoFields` uses two opaque local aliases, so it still fails. [Compiler revision](https://github.com/Tritlo/ghc/commit/6c8ab7f7deb9bbd0ef5e2fa03a5ece733f638492).

Job 2597 checked the unchanged book at `d343521` on the preceding compiler. All 27 modules passed the interface audit in four configurations. The two native readers passed 73 runtime checks each. Six negative clients failed as expected in each configuration. Across all 37 original reports, 23 compile at `-O0` and 21 at default `-O2`. Identity and the shunt proof account for the two differences. On the same book, the optimized De Bruijn interface now supports substitution under a lambda without capturing a free variable. Job 2606 checked the proof from interfaces at both reader optimization levels. [Substitution fix](https://github.com/Tritlo/ghc/commit/549657008d0ceaba590890324e56ba796b34a560).

## 2026-11-24 05:30 CET · Soundness, De Bruijn progress and polymorphic identity

The original R11 report and the inlined `MkSubstRefl ... (IES ie)` branch of the Soundness chapter pass Core and STG Lint at `-O0` and `-O2`. The wrong field keeps its GHC-25897 error. Job 2561 compared both compiler revisions on the same inputs. [Published fix and regression](https://github.com/Tritlo/ghc/commit/3326acd9b1fea98090eb5e8f3f89b5a85e44d849).

All three application branches of De Bruijn progress check when the caller supplies recursive progress and the beta case. The current chapter also reduces `substSingle ('Var 'Z) w` to `w` through a provider built at `-O2` with full unfoldings. The capture-avoiding lambda example passed on the earlier `49903d0` provider with full unfoldings. The current chapter's optimized lambda and general neutral beta are still open. [Combined compiler](https://github.com/Tritlo/ghc/commit/8b37fdfd344e46c5296f33ddd4bf781a617afdf8).

For `isoRefl :: Iso a a`, fetch `refs/yardsticks/plfh/generic-iso-refl-0fd43f5` from `/home/tritlo/Code/ghc`. Commit `220b6c2` applies on top of `7351848` and uses Prelude `id` in both directions. The proof module disables rewrite rules. Independent `Bool` and `Maybe Bool` readers used both proofs at `-O0` and `-O2`, and the patched book suite passed all 54 checks. The public compiler supports the patch. The PLFH author can incorporate it into the book.

## 2026-11-24 05:02 CET · Published GHC for polymorphic Iso and the map-proof alias

[Public GHC `34582bce54`](https://github.com/Tritlo/ghc/commit/34582bce5489634e15d9fc16a53d37b19dcc58e5) accepts `R04_PolyFunctionFieldInIso`, the unchanged `lengthMapLaw = lengthMap`, and the direct GADT equation-form example. Job 2505 checked PLFH `0fd43f5` in four fresh configurations. All 27 interfaces passed the audit. Both native clients ran 66 observations. Each build rejected five negative programs. Of 31 reports, 21 compiled at `-O0` and 20 at default `-O2`.

The chapter patch is `7351848d36`, based on PLFH `0fd43f5`. In the book checkout run `git fetch /home/tritlo/Code/ghc refs/yardsticks/plfh/isomorphism-0fd43f5` and inspect `git show FETCH_HEAD`. It covers symmetry, isomorphism-to-embedding and the natural-to-binary embedding. Job 2509 recorded 53 passes and zero failures on the patched book. General identity and transitivity have separate reproducers. [Compiler verification](docs/dependent-haskell/CHECKPOINTS.md#polymorphic-proofs-from-the-published-book).

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
