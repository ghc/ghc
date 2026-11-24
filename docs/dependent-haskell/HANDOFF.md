# Dependent Haskell handoff

Start from the `dependent-haskell` branch of [Tritlo/ghc](https://github.com/Tritlo/ghc/tree/dependent-haskell).
Use this file to resume an interrupted build or review. Update the revision,
test results and next action whenever a checkpoint lands or a build stops.
The [roadmap](ROADMAP.md) tracks feature scope. The [Showcase](Showcase.hs)
and its [expected output](Showcase.stdout) are the executable examples.

## Design authority hierarchy

All compiler syntax, typechecking, promotion, and ergonomics work on this fork follows a strict four-tier specification order:

1. **GHC Proposal #378 (*Design for Dependent Types*)** whenever possible. Already-implemented fork features that diverge from `#378` take priority for alignment.
2. **Other GHC proposals** (`#281`, `#242`, `#370`, `#402`, `#425`, `#509`, `#765`, `#23`/`#282`, etc.) for design areas delegated or not covered by `#378`.
3. **Serokell Dependent Haskell roadmap** ([ghc.serokell.io/dh](https://ghc.serokell.io/dh)) for scope tracking and card dependencies.
4. **Agda** (and *Programming Language Foundations in Agda*) for inspiration where GHC proposals are silent.

## Current checkpoint

Wave 1 consolidated compiler `186670b0b9` passed full two-stage self-hosted qualification on 24 November 2026, while Wave 2 (`9a27c55688`), Wave 3, and the Proposal `#378` alignment audit are actively running. Revision `186670b0b9` (atop `69a36b132f`) integrates four verified compiler tracks and adds Showcase observations 624–627:

- **R16 simultaneous substitution (`c942ad30b5`)**: Accepts `-O2` FloatOut lifted constructor helpers (`exts1`) without relaxing source helper checks, and publishes open `T:subst` alongside `T:exts` and `T:weakenLookup`, checking all 20 constructors of PLFH `Bisimulation.simSubst` with ordinary functions.
- **R21 same-module indexed lookup (`75eea40500`)**: Stages `lookupEnv` for same-module mutual GADT variable rules in either declaration or clause order, enforcing exported constructor/owner visibility (`[GHC-70566]`), unused-table pruning across TH splices, and strict early-vs-final Core agreement (`R13_LookupEnvOpenEq.hs`).
- **R03 closed-family refinement & proof tracking (`432b300f8f`, `186670b0b9`)**: Refines closed type families from scoped nominal retained Givens while tracking nested checked-call and reduced proof-synonym rewrite-rule dependencies (`R03_ForEachGadtEqTypeFamily.hs`, `NativeOpenEitherFmap`).
- **R13 indexed context traversal & untyped scope closures (`eb74aedbe0`)**: Publishes checked equations for two-index GADT traversals (`plug :: UCtx m n -> UTerm m -> UTerm n`, including diagonal `CHole` siblings and binder-reordered `CAppR`) and untyped scope-indexed `substSingleU` both cross-module and same-module (`R13_PlugOpenEq.hs`, `R13_SubstSingleUOpenEq.hs`).

The roadmap has **27 of 50 cards complete**
(23 inherited, four completed on the fork), 18 partial and five unstarted.
[Roadmap and acceptance criteria](ROADMAP.md#where-this-fork-stands)

| Verified input | Result |
| --- | --- |
| Wave 1 normal two-stage compiler at `186670b0b9` (`_build/on-demand/wave1-two-stage`) | Both `stage0` and `stage1` report `186670b0b9`. All 15 qualification gates passed (`0`), including 55/55 registered tests and 627/627 Showcase observations at native `-O0`, default `-O2` and in GHCi. All 234/234 compatibility cases passed. The focused suite passed 137/138 (sole known upstream `MTrans` diagnostic in `deriving-via-compile`). |
| Published PLFH batch 37 at `8756aed` (with Wave 2 Lane 1 `03b7bb9a00`), job 3549 | 34/34 modules, `.hi` audit clean, 155/155 runtime checks (`Untyped.evalNormal` constructive normal-order evaluator checked, 21/25 chapters `Checked`), 10/10 negative controls, 38/38 fixed reproducers at `-O0` and `-O2`. |
| Published PLFH batch 35–36 at `8d868df` on `186670b0b9` (`stage1`) | 34/34 modules in all four modes (`native-O0`, `native-O2`, `readback-O0`, `readback-O2`), 10/10 negative programs per mode, and 40/50 standalone reports at `-O0` / 37/50 at `-O2` (promoting `R03_ForEachGadtEqTypeFamily`, `R13_LookupEnvOpenEq`, `R13_SubstSingleUOpenEq`, and `R13_PlugOpenEq`). |
| Retained baseline compiler at `844f309834` (`_build/current`), job 3311 | Kept intact until Wave 2 two-stage promotion completes so `PLFH` `check_plfh.sh`'s `wave2-preview` selector remains active for Batch 37 (`R04_IndexedForEachCtorField.hs`). |

The book author published batch 32 at
[`f3643cfa5b`](https://github.com/Tritlo/PLFH/tree/f3643cfa5bc257e44742fec98a4d5a875f41efd8).
It adopts both proofs and adds a tenth negative program. Its own test
runner reported 78 passes and zero failures (job 3305). Start new book
work from this published revision. The [PLFH checkpoint](plfh/README.md)
preserves the earlier combined manuscript and all three unfinished compiler
candidates. The [shared notebook](../../PLFH-NOTES.md) records the original
reports and the author's verification.
The earlier paths and queue IDs below are historical records.

### Build storage on the shared host

The 24 November retirement removed 201.11 GB of old experiment paths.
Including the retained compiler and the new recovery files, the top-level
`_build` fell from 201.62 GB to 11.00 GB. The current compiler and test
receipts use about 4.41 GB. The source and receipt archives use 5.88 GB.
About 0.72 GB remains in registered worktree areas and their Git object
providers. All 24 worktrees can report Git status. Eleven show local changes.

Use `_build/retirement-20261124/README.md` to recover a local input by
path. The archives omit old native binaries and interfaces. Use the source
revisions in the branch to rebuild them when needed. The published book
runner now selects `_build/current` automatically. It passed 78 checks
with the new default in job 3314. After the old trees were removed, job
3315 passed the registered checked-closure and vector-facade scripts and
ran GHCi with the retained compiler.


<details>
<summary>Membership, parallel reduction and bytecode results through batch 29</summary>

Compiler `0452d7d17c` checks both inverse laws for the ordinary Lists
functions `anyToWitness` and `witnessToAny`. It also checks PLFH's
`parRefl` under a lambda, where the body has one more variable in
scope. The two proofs are available together on batch 29. In the book
checkout, fetch the handoff with
`git fetch /home/tritlo/Code/ghc refs/yardsticks/plfh/membership-confluence-3b4892f`.

Job 3231 checked all 30 modules of unmodified batch 29 in four modes.
Both native builds matched 132 observations. Of the 42 standalone
reports, 36 compile at `-O0` and 33 at default `-O2`. The newly
accepted report is `R03_NestedDatatypeForEach`. On the book handoff
`ea2c1e773d`, job 3217 checked all 31 modules at both native optimization
levels and against their native interfaces. Each run rejected the nine
negative programs. Both native executables and default GHCi (job 3234)
matched all 134 observations. Job 3206 matched all 621 Showcase
observations in the three modes.
[Published book](https://github.com/Tritlo/PLFH/tree/3b4892fce5b2ca7f38f05d1023c212e14e0e35e2),
[membership induction](examples/open-evaluation/BookAnyWitnessProof.hs),
[parallel reduction](examples/open-evaluation/BookParallel.hs).

At `46abd6733e`, the second inverse law also accepts `encode proof`
directly. CorePrep retains the index of this local function call when
it names the operand for code generation. Job 3232 checked the original
composed program at `c4943a42d7`: 31 modules in each of four modes,
nine negative programs per mode, and 134 matching native observations.
Job 3241 checked that ignoring the result of a throwing encoder leaves
it unevaluated. To fetch the composed proof in a book checkout, use
`git fetch /home/tritlo/Code/ghc refs/yardsticks/plfh/composed-membership-confluence-3b4892f`.
[CorePrep Note](../../compiler/GHC/CoreToStg/Prep.hs),
[regression](../../testsuite/tests/dependent/should_run/NativeIndexedIsoOperand.hs).

At `0b2ad262df`, default GHCi runs the entire batch 27 book in
bytecode. All 123 observations match the native run. This includes
the **More** progress proof with the debugger enabled. A separate
debugger test stops inside substitution and inspects a local value.
Job 3162 also passed three registered compiler tests.

[Batch 28](https://github.com/Tritlo/PLFH/tree/67dc34b68d1dfcf4d379594986281831c20ae7fd)
adopts the multiplication fix and adds a report about abstract renamings.
Job 3179 checked all 29 modules at both native optimization levels
and rechecked every module against its dependencies' native interfaces.
Both executables matched all 129 runtime observations. All four runs
rejected the nine negative programs with their expected diagnostics.
The compiler accepted 35/42 original reports at `-O0` and 32/42 at
default `-O2`. The new renaming report received GHC-70565 at both levels.

Showcase revision `6a608b4f38` proves that decoding an existential
list witness preserves its position even when the next element has the
same value. The proof works for an arbitrary tail witness and predicate
proof. Job 3171 matched all 619 observations at native `-O0`, default
`-O2` and in GHCi on the decoder compiler at `5ff3e67e13`.
[Ordinary decoder](examples/open-evaluation/BookAnyWitness.hs),
[proof](examples/open-evaluation/BookAnyWitnessProof.hs).

Compiler `056871faa9` checks the list-case step in PLFH's **More** chapter.
The case binds the head and the tail. The step's result type calls the
ordinary `substDouble` function with those two values. Replacing one
with a different value or shifting a free variable by only one binder
fails at both native optimization levels. Job 3073 matched all 617
Showcase outputs at native `-O0`, default `-O2` and in GHCi.
The existential predicate witness example compiles its provider to
object code in the interactive run.
[Public compiler](https://github.com/Tritlo/ghc/commit/056871faa94d6fcb5907fe136d7f47b00d55a84e),
[replacement-order proof](examples/open-evaluation/BookBetaReduction.hs),
[R16 requirements](roadmap/R16-open-term-evaluator.md#typed-substitution-under-a-binder).

[Published PLFH batch 27](https://github.com/Tritlo/PLFH/tree/81f8c63afdb722766d5ea6a1163f7d20345105d9)
includes the complete progress proof for the 19 constructors of
**More**. It states all 27 reduction rules. In particular, matching
`Cons v vs` fixes both indices in the list-case result. On
`056871faa9`, job 3101 checked all 29 book modules at native `-O0`
and default `-O2`. Each module also compiled against its dependencies'
native interfaces at both levels. All four runs rejected eight negative
programs. The native runs matched 123 observations. The corrected
double-substitution report passed at both optimization levels. In total, 34 of the 41 original
reports passed at `-O0` and 31 at default `-O2`.

Compiler `744122325d` also accepts the book's original wildcard
multiplication report. The zero row ignores the other argument. Job
3109 raised the totals to 35/41 and 32/41 respectively. All other
reports, negative controls and runtime output matched job 3101. Job
3119 matched 618 Showcase observations at native `-O0`, default `-O2`
and in GHCi. The new observation evaluates `times 0 (error "forced wildcard")`
and checks the open equality `times 0 n :~: 0`.
[Book verification instructions](#build-and-verify).

[HaskLib.Nat.plusComm](hasklib/HaskLib/Nat.hs) proves that addition
commutes. Book patch `f332a78d0f` reuses it in Induction and Equality.
It preserves the author's progress proof and all seven negative controls.
Job 2996 matched the 113 book observations at native `-O0`, default
`-O2` and in GHCi. From a batch 24 book checkout, fetch it with
`git fetch /home/tritlo/Code/ghc refs/yardsticks/plfh/hasklib-plus-comm-2fd9b9a`.

</details>

## Next compiler work

1. **Wave 2 Carbon consolidation & two-stage promotion (`9a27c55688`)**: Complete Carbon review, Showcase observations 628–631, and two-stage promotion to `_build/current` for indexed GADT `foreach` constructor fields & vanilla-return `Sigma` operands (`R04_IndexedForEachCtorField.hs`), `let`/`where` composed function aliases (`R04_ComposedIsoFields.hs`), fresh `-fno-code` closure providers with `.hi` `sourceRuleHeadsField` persistence, and `-O2` Final-Core explicit-list verification (`R13_RevOpenEq.hs`) + matchability mismatch printing (`R16_AbstractRenamingInType.hs`).
2. **Wave 3 Carbon tracks (in flight)**:
   - Track A: R17 `%U` type-lambda capture (`R17_TypeLambdaCaptureUnmatchable.hs`) and R21/R13 same-module `plug` & `substSingle` staging (`R13_SameModulePlugOpenEq.hs`, `R21_SameModuleSubstSingleOpenEq.hs`).
   - Track B: R03 inline GADT constructor `foreach` arguments (`R03_InlineGadtCtorForEachArg.hs`), R03 retained-call untouchable polymorphism (`R03_UntouchableRetainedCallPoly.hs`), and R06/R13 term-to-`Type` large elimination (`R13_LargeElimTermToType.hs`) + `HaskLib` `foldr` / `Matchability(..)` exports, followed by Batch 37 follow-ups (`R04_ExistentialGadtRetainedArg.hs`, `R05_NestedFinForEachCoreLint.hs`).
3. **Wave 4 — GHC Proposal #378 Deep Alignment (prep in flight)**: Audit all differences between GHC Proposal `#378` and the fork's current syntax/semantics/pragmas, prioritizing already-implemented fork features that diverge from `#378`, followed by remaining `#378` features, delegated GHC proposals, Serokell roadmap cards, and Agda-inspired ergonomics.

[Batch 37 status ledger and open reports](https://github.com/Tritlo/PLFH/blob/8756aed/content/Status.md)

See the [candidate revisions and examples](plfh/README.md#resume-unfinished-work).

<details>
<summary>Earlier PLFH verification through the 599-example candidate</summary>

### The 590 compiler

Both stages built from `1a02a866e7`. A caller can prove equality of two
schema transformers before supplying any rows. A closed validation schema
checks with `NoUndecidableInstances`. All 590 Showcase observations
matched at native `-O0`, default `-O2` and in GHCi.
[Source and verification](CHECKPOINTS.md#compare-schema-transformers-before-supplying-the-rows).

Job 2080 passed all 20 selected registrations, four upgrade checks,
map-fusion and package checks, and all 234 compatibility cases. The
focused suite passed 137 of 138. The remaining failure was the known
`MTrans` diagnostic. The verification script is in
`_build/on-demand/r17-schema-transformer-post-588-20261124`.

HaskLib exports the Peano datatype as `HaskLib.Nat.Nat`. Import
`Nat(..)` for constructor patterns and `type (+)` for infix proofs.
Job 2295 checked the installed package at `-O0` and `-O2`.
`plusZero 3` and `(2 + 3 :: Nat) :~: 5` both checked. HaskLib also
exports `rewrite` to use an equality in a proof body. Job 2307 checked
all 590 Showcase observations at both optimization levels and in GHCi
with the library at `54d74a0067`.

### PLFH results before publication

Read [PLFH-NOTES.md](../../PLFH-NOTES.md) on resuming work and after each
compiler batch. Reply with the compiler revision and the result of running
the PLFH author's example.

The book's current baseline is the 590 compiler at `1a02a866e7`.
Job 2300 checked PLFH revision `1ffcd4df52` and reproduced its 27
compiler reports. Job 2309 migrated a copy of those 26 modules to
`Nat` and `rewrite`, checked them at both optimization levels and ran
an ordinary client. All three negative controls reported GHC-83865.
Job 2236 confirmed that the 23 reports in the older book revision
`d0e4fa73df` also reproduced on 590.

Job 2209 built both compiler stages at `984647892b`. All 24 selected
registrations and all 234 compatibility tests passed. The focused suite
had its known `MTrans` failure. A typed splice in the 594-example Showcase caused
GHC to demand type-level equations for three ordinary runtime functions.
Job 2341 verified the renamer fix at `8c1a412703`. The original 594
observations then passed at native `-O0`, default `-O2` and in GHCi.
Job 2338 checked the new typed quote through a separately compiled client
and rejected the wrong splice result. The old quote test still expected
this newly supported use to fail.

Job 2356 builds the combined compiler at `923571c2c8` from
`refs/spikes/plfh-nat-scope-599`. It checks 599 Showcase observations
and 38 registrations. This run supersedes the queued 597-example build.
The inputs are in `_build/on-demand/plfh-nat-preservation-normal-20261124`.
Its component passed all six targeted registrations in jobs 2347 and
2354. A client read the countdown and function-domain proofs through
compiled interfaces. Job 2358 accepted 13 of the 27 reduced examples
from book revision `69354d2` at both levels. Three other chapters used
the same implicit substitution binder as Lambda. The combined compiler
reported those uses in DeBruijn, More and Untyped. Job 2364 compiled all
27 modules of the audited book revision `89c8292` at `-O0` and `-O2`.
Of its 28 reduced examples, 13 compiled and 15 still failed at each level.

The Nat edition of the book is `69354d2`. Job 2319 passed all 31
book checks on the published compiler. The book author audited the
exported constructor types in `89c8292`. Job 2340 passed its 27 modules,
four negative controls, interface audit and runtime check on 590.
The audited edition withholds the named functional reduction relation
until a normal compiler can check the intended beta rules.
On component `09bc6546ca`,
job 2310 found that the named Lambda chapter's beta constructors
implicitly bound a fresh `substTerm`. A caller could choose the wrong
target of a reduction. The book patch moves the producer to a separate
module and qualifies it in the three constructor results. Job 2336
checked full substitution and one-step preservation for this relation.
An ordinary client exercised all eight reductions at both optimization
levels. Job 2337 also checked the proofs with `-fno-code` against native
producer interfaces. Recover the book patch with
`git format-patch -1 --stdout refs/yardsticks/plfh/preservation-nat`.

Job 2335 checked the compiler-side name resolution on the unchanged
Lambda chapter. The candidate rejects its same-module call with
GHC-68947 before a caller can choose a different function. The qualified
book patch compiles. Using the original same-module producer directly
and building every producer with `-fno-code` are separate follow-ups.
The private no-code compiler in job 2359 checked the 28 modules from
the preservation patch using fresh no-code producer interfaces.

Jobs 2196 and 2213 reproduced the first 21 PLFH examples on the
published compiler at both optimization levels. They include two compiler
panics. Five examples pass on the components in job 2209. They cover
congruence under partial addition, `withEquality` inside `trans`, direct
substitution in the parity proof, an inline dependent sum and the
polymorphic `lengthReverseLaw = lengthReverse` alias. Job 2206 accepted
the unchanged alias and rejected the wrong length at `lengthReverse`.
The [PLFH replay](../../PLFH-NOTES.md#compiler-reply--inference-and-the-revised-21-examples)
lists the original results. Roadmap card counts stay the same.

The next two component fixes are in `270196132d` at
`refs/spikes/plfh-source-callbacks-596`. Job 2219 checked HaskLib's
Boolean size comparison and its reflexivity proof from an ordinary client.
Jobs 2216 and 2218 checked a same-module callback stored with its proof
and the optimizer regression in the earlier candidate. Job 2227 combined
the compiler modules for both changes. At both optimization levels an
ordinary client read the two results through compiled interfaces. The
596-example source awaits a full build.

PLFH subsequently added a failed Core Lint check for the Bisimulation
value proof and a rejected GADT-indexed environment lookup. Job 2217
reported 21 checked modules, three rejected negative controls and 23
compiler reproducers on the published compiler. The [edition and proof
scope](../../PLFH-NOTES.md#compiler-reply--edition-chapter-coverage-and-the-isosize-correction)
explains which chapters and theorems those figures cover.

The combined component at `d033d0132d` includes the fix for the
Bisimulation value proof and support for structural recursion with
additional ordinary arguments. Job 2235 tested the 23 reports against
fresh interfaces at `-O0` and `-O2`. All eight fixed examples passed.
The fifteen open examples matched their recorded diagnostics. Job 2236
replayed all 23 failures on normal 590. Job 2239 checked the general
`simValue` declaration from the book itself and ran a separately compiled
client. The [PLFH results](../../PLFH-NOTES.md#compiler-reply--the-bisimulation-chapters-general-value-proof)
include the scope of the comparison. The private source and receipts
are in `_build/on-demand/plfh-bisimulation-carry-post596-20261124`.

</details>

| Area | Verified result and next step |
| --- | --- |
| R04 | Job 2025 compiled an ordinary client against interfaces alone. It copied flags when two stored lengths compared equal and returned `Nothing` for different lengths at `-O0` and `-O2`. Jobs 2002, 2015 and 2021 cover the record update, equality and selector operations used by this example. |
| R17 | Jobs 1992 and 1993 verified exact family forwarding. Jobs 2012 and 2020 checked the source spelling of visible type arguments, including a nested type splice. The two ordinary control diagnostics matched the baseline byte for byte. |
| R16 proof validity | Independent count expressions can produce `Zero` and bottom under stock fusion. Both shared counts produced `Zero`. The private Specialise repair leaves that distinction observable. In job 1957, rebuilding HaskLib with private `4c4cc16244` recorded the `map` dependency on the composed proof. The optimized client then reported GHC-83605 at the call to `lengthMapZip`. The same diagnostic occurred with a producer that simply constructs one cell. The old installed interface still reached Core Lint. The consumer-side policy remains open. |
| R23 matchability | The private prototype `99bfde4ec4` passed the original transformer test and 32 Template Haskell reification cases. An old installed provider still needs rebuilding. The published compiler retains the existing matchability policy. |

For R16, job 1995 compared the same HaskLib source built with two
compilers. Its original assertion missed a GHC-83605 at the closed
`Refl` pattern. The [16-case comparison](roadmap/R16-open-term-evaluator.md#proof-dependencies-in-the-rebuilt-hasklib-interface)
records the diagnostics and runtime results. Job 1996 verified the
separate constructor comparison at `7f56212d49`. The four disjoint
local and imported rules allowed the proof. All four matching rules
still blocked it. Job 1999 passed the extended maintained regression.

Job 2080 uses the current compiler as its build seed. The former 560
and 562 build products have been retired. Their Git revisions and test
receipts remain available. The exact 7.81 GB inventory is in
`_build/on-demand/retirement-560-562-for-plfh-20261124`.

The November 24 cleanup retired 826 completed test executables and freed
7,733,612,544 bytes. Inputs, objects and results stayed in place. The file
inventory is in `_build/on-demand/retirement-completed-test-executables-20261124`.

The GHC queue has four slots. Local benchmark jobs take priority and freeze
the GHC slice. The remote Colab benchmark uses the
`colab-inkling-graded-*` label in the `build` group and runs alongside GHC.
Jobs 1947 and 1949 were exploratory allocation measurements.
Both pilot harnesses are retired. Their prompts, draft code and measurements
are archived for reference under `_build/reference-only`. Active performance
work needs independently written benchmarks. OpenCode reviews stopped on 24 November 2026. The coordinator and the
three compiler owners handle implementation, review and testing.

The previous 577 compiler was `893d2ebc06`. Its qualified records are
in `r04-r16-r17-normal-after-574-20261123/published-qualification.tsv`.
The 581 compiler, the omitted-field component and the first display
component now require rebuilding for a binary replay. Their Git sources,
logs and results remain available. The exact three generated trees are in
`retirement-four-finished-warm-trees-20261123/sizes.tsv`.
The 585 qualification reuses the previous 584 build tree.
On 24 November, retirement of the completed R16 operand-inference and
R17 diagnostic build trees recovered 7.83 GB. The directory list and
source revisions are in
`_build/on-demand/retirement-r16-r17-post-585-20261124`.

## Previous checkpoints

### The 578 compiler

Compiler `1efc4bcaf2` passed all 578 Showcase observations in native
`-O0`, default `-O2` and GHCi. A spliced declaration can capture the
error type in a row schema. HaskLib can infer both element types of
`zip xs ys` in the composed length proof. The installed proof passed
at both optimization levels with rewrite rules disabled. Job 1969 also
ran the current out-of-line HaskLib proof at default `-O2`. It returned
`Refl` while the independent counts differed in demand.
[Examples and qualification](CHECKPOINTS.md#a-spliced-row-schema-and-inferred-zip-arguments).

Job 1900 built both compiler stages. All 13 selected registrations and
234 compatibility cases passed. The focused suite passed 137 of 138.
Four tests timed out during the benchmark suspension. Job 1950 reran
those four against the same compiler, and all passed. The remaining
`MTrans` diagnostic matched the earlier compiler. Job 1914 passed all
four incremental checks. Across a real library upgrade the unchanged
control rebuilt because `GHC.Essentials` changed. With a fixed library
database it reused its interface. Both cases reused all artifacts on
the next invocation.

Run `accept-publication.py` in
`_build/on-demand/r04-r17-combined-quoted-columns-20261123` to check
the original and replay receipts. The original job exits with status 1.
The verifier checks both sets of evidence, including the original optimized
proof failure. Job 1966 qualified the rewritten HaskLib proof and tested
all 578 Showcase observations at native `-O0`, default `-O2` and GHCi.
Job 1965 passed the independently written worker regression in all four
combinations of native or typecheck-only compilation and fixed or
upgraded libraries. The [compiled count and witness example](roadmap/R16-open-term-evaluator.md#the-optimized-client-of-the-composed-proof)
records that failure on both 577 and 578.

Updated 23 November 2026. The development branch is
`dependent-haskell`. Use the fork command below to find its latest commit.

The 574-observation compiler is `c11b29a399`. Job 1853 built both
stages. All nine selected registrations passed. The 574 Showcase
observations matched at `-O0`, default `-O2` and in GHCi. The installed
HaskLib and first-class-families checks passed. Native and typecheck-only
upgrades from 571 recovered the missing equation for an unchanged length
worker. A second invocation reused every output. All 234 compatibility
cases passed. The focused suite passed 137 of 138 with the existing
`MTrans` diagnostic in `deriving-via-compile`. Job 1858 also passed
the maintained `--retained-worker` check with the narrower imports.
[Runtime replacement and upgrade evidence](CHECKPOINTS.md#replace-a-vector-after-comparing-runtime-lengths).

An ordinary caller can replace a vector when a runtime comparison
proves that the two lengths agree. A local signature can also use an
inferred capture kind in a type lambda. A reader built from 572
consumed the resulting row from a stripped interface at both
optimization levels.

The four GHC build slots share a 16-core, 64,000,000,000-byte slice and
yield to the benchmark queues. Use the
[queue recovery procedure](CHECKPOINTS.md#restore-the-shared-wsl-build-queue)
after a distribution restart.

Job 1756 qualified the private 572 compiler. Job 1818 found its stale
length interface before publication. Jobs 1843 and 1848 verified the
targeted repair. The 571 release at `203245e124` remains the baseline
for the incremental check. Its 571 Showcase observations matched in
three modes, all 234 compatibility cases passed, and the focused suite
retained its existing `MTrans` failure.

The preceding R04 compiler is `aba28b367f`. A dependent pair can select
its payload type with an associated family. Reading the tag leaves
the payload lazy. An ordinary client that only typechecks a provider
interface gets the same missing-class error as one that builds an
object. Job 1701 built both stages. All 569 Showcase observations
matched in three modes. All 234 compatibility cases passed. The
focused suite passed 137 of 138 with the existing `MTrans` failure.
The original driver exited 1 because its package fixture was missing.
Job 1721 verified the fixture and passed the package checks on the same
compiler. Use `r04-normal-569-20261123/post-review/verified.pass` for the
combined results. [Release evidence](CHECKPOINTS.md#associated-families-as-dependent-pair-payloads).

The preceding R17 compiler is `c4d702c7f4`. A record field can use a type
lambda that captures the record's error type. Family instance equations
can use a lambda under an arrow. A row reader can capture indices from
explicitly kinded erased `forall` binders. The field, instance and ranked
examples work with the provider source hidden.

Job 1670 built both compiler stages. All 568 Showcase observations
matched at native `-O0`, default `-O2` and in GHCi. The prefix upgrade
driver checked the wrong comparison pin before invoking either compiler.
Job 1698 passed the upgrade and cross-version checks on the same binaries
with the original 566 reader. Job 1694 passed the extended selector
recompilation regression on the normal compiler. All 234 compatibility
cases and 137 of 138 focused cases passed. The existing `MTrans`
diagnostic matched its baseline. Use `review/qualified.pass` for the
combined results. The original job exited 1.
[Release evidence](CHECKPOINTS.md#type-lambdas-in-record-fields-and-ranked-row-readers).

The preceding R13 compiler is `687b465aad`. HaskLib exports `lengthTakeSize`,
a proof that taking a prefix of a list gives the smaller of the bound
and the list length. Its producer is ordinary Haskell. Job 1650 built
both compiler stages. All 567 Showcase observations matched at native
`-O0`, default `-O2` and in GHCi. All 234 compatibility cases and 137 of 138
focused cases passed. The existing `MTrans` diagnostic matched its
baseline. The previous reader needs rewrite
rules disabled to check the direct zero-prefix equation through a new
provider interface. [Qualification and reader checks](CHECKPOINTS.md#size-bounded-list-prefixes-in-hasklib).

The preceding R18/R19 compiler is `b6ddbb4ce8`. It qualifies R18 local
type-level definitions, including scope through intervening aliases and
shadowed kind binders. R19 now supports a case in a closed family
equation, including a decreasing recursive call. The two-stage build
passed all 566 Showcase observations in three modes, all 234
compatibility cases and 136 of 137 focused cases. The remaining
failure is the existing `MTrans` diagnostic. The initial source-case
driver exited 1. Corrected job 1649 verified all 42 O0/O2 source-case
statuses on the same binary. [Release evidence](CHECKPOINTS.md#lexical-type-lets-and-cases-in-closed-family-equations).

The preceding compiler is `049ac2c031`. It accepts
`Map ((+) 1) '[2, 3, 4]` and can use a previous type-level case result
as a wildcard branch's default. Job 1608 built both stages. All 564
Showcase outputs matched at native `-O0`, default `-O2` and in GHCi.
The initial 17 regressions, arithmetic interface checks and the
migrated `first-class-families` package passed. All 234 compatibility
registrations passed. The focused run passed 136 of 137, with the
existing `MTrans` failure unchanged.
[Release checks](CHECKPOINTS.md#built-in-arithmetic-callbacks-and-type-let-case-results).

The preceding R18/R23 compiler is `f030369687`. It supports the standalone
`UnsaturatedTypeFamilies` extension and reports constructor-pattern
errors in type lets at the written constructor. Job 1582 built both
stages. All 562 Showcase lines matched at native `-O0`, default
`-O2` and in GHCi. The 17 targeted registrations and the migrated
`first-class-families` package passed. Job 1607 passed all 234
compatibility cases and 136 of 137 focused tests. The existing
`deriving-via-compile` failure matched the 561 diagnostic.
[Release checks](CHECKPOINTS.md#standalone-unsaturated-type-families).

The preceding R18 implementation is `9daa85cf02`. A type-level `let`
can bind the fields of a named product. The Showcase adds two dimensions
while leaving a third unknown. Normal job 1572 built both stages and
matched 561 outputs at native `-O0`, default `-O2` and in GHCi.
All 234 compatibility cases passed. Of 131 focused tests, 130 passed.
`deriving-via-compile` still reports the same errors as 560.
[Product-let verification](CHECKPOINTS.md#constructor-patterns-in-type-level-lets).

The preceding compiler implementation is `5d6ac951f6`. An inferred
type synonym can take both a data constructor and a type family.
All 560 Showcase outputs matched at native `-O0`, default `-O2`
and in GHCi. The migrated `first-class-families` package compiled
with the bare `Pure1 'Endo` expression. Across the normal suite and
two focused replays, 127 of 128 focused registrations and all 234
compatibility registrations passed. The sole open failure is
`deriving-via-compile`, already present in 556 and 558.
[Detailed results](CHECKPOINTS.md#inferred-matchability-in-type-synonyms).

The preceding runtime-message implementation is `b963ccfaad`. Both
normal stages built at `3dd34e5327`. Its 558-line Showcase matched
at `-O0`, default `-O2` and in GHCi. All 234 compatibility cases and
125 of 126 focused registrations passed. The remaining upstream
`deriving-via-compile` case gives the same errors on public 556.
Stock 9.14.1 accepts its transformer kind synonym. Job 1531 checked
the explicit matchability migration on both fork compilers.
[Full results](CHECKPOINTS.md#runtime-payload-selection-through-an-associated-family).

The preceding character-predicate implementation is `1b5c5a9676`.
A type synonym can use a checked character predicate in the same
declaration group. An importing module can prove its result from the
interface. Jobs 1454 and 1455 qualified normal two-stage build
`54c4c62287`. All 556 Showcase lines matched at native `-O0`,
default `-O2` and in GHCi. All 111 focused and 234 compatibility
registrations passed.
[Character-predicate checks](CHECKPOINTS.md#checked-character-predicates-in-the-defining-module).

The preceding Unicode checkpoint is `16de21824a`. Its compiler and
libraries matched normal two-stage build `1659c10b23`. The historical
compiler can be rebuilt from `refs/archive/dependent-haskell/2026-11-23/r03-r15-unicode-555`.
The installed title-case, upper-case and lower-case functions all
publish their checked equations. Native `-O0`, default `-O2` and GHCi
matched the 555-line Showcase. All 109 focused registrations and all
234 compatibility cases passed across normal job 1398 and the two
corrected diagnostics in job 1451.
[Unicode release checks](CHECKPOINTS.md#installed-unicode-case-mappings).

The preceding 554-line release is `ce1c89c534`. It added imported
function identity and qualified the staged wildcard diagnostic.
[Callback release checks](CHECKPOINTS.md#imported-function-identity-in-retained-indices).

The previous R41 release is `61fd13f735`. Keep its normal compiler
at `71e5215115` while the callback interface tests use the earlier reader.
[R41 verification](CHECKPOINTS.md#ordered-gadt-signatures-and-required-type-arguments).

The preceding 552 compiler at `f42ebe4659` matches normal build
`00115859a5` in `r19-r41-next-20261122`. Keep it while an active
interface comparison uses it.

| Next check | Source and verification |
| --- | --- |
| Tree induction and ordered `type data` | Job 1110 built `c5b5acbffc` in `r41-type-data-554-normal-20261122` using a private copy of the 552 tree. Job 1115 passed the 554-line Showcase in three modes, 48 registered tests, the installed HaskLib client and the combined interface. Jobs 1100 and 1107 had already passed the source-distribution and downstream tree clients on the compiler component. |
| Legacy CUSK defaults | Job 1112 built GHC and its boot libraries from clean sources at `c27193021b` in `r27-legacy-transition-normal-20261122`. Job 1121 passed the 34 diagnostic inputs, three-package comparisons, cross-version interface links and five registered tests. The fork keeps existing Haskell98 and Haskell2010 defaults and the opt-in warning. The transition experiment is complete. Its temporary installation was retired after archiving its source. |
| Declaration groups and diagnostics | The R21 candidate at `87fcbcef7d` checks the tree proof with both workers in the same group and the tree datatype imported. The R16 diagnostic at `e0e63bec5d` names the enclosing call and highlights its full argument. Job 1151 passed the source-hidden mapper regression. |

The tree-proof release also needs a cross-module rewrite check. Jobs
1161 and 1166 accepted an imported tree witness when a client rule changed
the measured fork count. Job 1164 reproduced the same behavior on the
published list proof. With the conflicting rewrite disabled, job 1171
obtained matching counts. See the [R16 evidence](roadmap/R16-open-term-evaluator.md#an-imported-proof-and-a-client-rewrite-rule).

Job 1181 reproduced both rewrite counterexamples on the exact 554
compiler. Keep that checkpoint private. Job 1184 built both stages
from clean sources at `1bef98f413` in `r03-r16-r21-r41-consolidated-20261122`.
Job 1187 passed the downstream list and tree rule checks. All 555
Showcase lines matched at `-O0` and in GHCi. At default `-O2` the
compiler rejected three uses of the installed filter proof. The mixed
interface check passed. Of 53 registered tests, 52 passed. The new
GADT test rejected a terminal existential. Cabal installed HaskLib,
then the `list-laws` test failed to compile at proofs that use `map`
and `(++)` with the ordinary optimization rules. The separate installed
tree client did not run in that job. The per-gate logs are in
`qualification/tries`. See the [filter demand comparison](roadmap/R16-open-term-evaluator.md#optimized-clients-of-the-filter-proof).

Job 1251 used the installed archive to run the tree and list clients
independently. At default `-O2`, `nodesMap`, `lengthZip`, `lengthZipWith`
and `lengthReverse` passed. The three list proofs involving `map` or
`(++)` reported GHC-83605. All six list laws passed with client
rewriting disabled. Results are in
`r16-installed-proof-consumers-20261123/local-callbacks/results.tsv`.

The successor checkout is `r16-r41-proof-upgrade-normal-20261123/source`.
Job 1227 passed both normal compiler stages at `6f12b4e3b3`. This
compiler has the terminal existential and declaration
reification fixes from `39dccda474`, the boxed equality fix and
`NativeOpenProofRules`. Job 1207 passed the source-hidden proof and
forwarding cases. Job 1222 passed the three incremental builds for an
ordinary home proof, including a proof that uses installed definitions.
An old installed *proof* still needs a source rebuild. Job 1229 passed all 55 registered tests, the list and tree rule checks,
three upgrades from normal 554, mixed interfaces and the Showcase at
`-O0` and in GHCi. The default `-O2` Showcase and HaskLib list tests
still failed at imported proof uses. Inferred proof bindings have separate checks. See the
[ordinary proof upgrade](roadmap/R16-open-term-evaluator.md#upgrading-a-proof-whose-type-hides-the-equality).

The earlier R41 comparison at `acd48e3f3f` passed 66 registrations
before the new flag policy. Its generated compiler tree was retired
on 23 November. To repeat the comparison, restore
`refs/archive/dependent-haskell/2026-11-23/r41-only-normal`.
The build recipe and results are in `r41-only-normal-553-20261123`.
The final release source and tests are archived under
`refs/archive/dependent-haskell/2026-11-23/r41-verified`.
Jobs 1286 and 1319 provide the focused results on the final compiler.
The former has one failed diagnostic expectation. The latter checks
the same source examples with both flag settings. Job 1322 passed all
four updated compatibility registrations together.

The next combined source is `4d10fdc35f` in
`r03-r16-proof-attribution-intake-20261123/source`. It includes the
single interface feature for a named callback as a value or in an
application. It also includes inferred proof bindings at `4a119b07e1`.
The 32 component cases passed at `-O0` and `-O2`. Job 1267 built
both normal stages. Job 1269 passed all 59 registrations, proof upgrades,
mixed interfaces, the HaskLib archive and its installed tree client.
The Showcase failed Core Lint in `ConstructorCallClient` at `-O0`
and in GHCi. CorePrep substituted the global `trueConstructor` for a
local callback. Lint could no longer reconstruct its case equality.
Default `-O2` still rejected the filter proofs. The HaskLib list test
still rejected the `map` and `(++)` proofs. Keep this compiler private.
The R03 correction at `808230436c` checks the named call or its
certified source equation after CorePrep. Job 1300 matched all 17 lines
from the original callback client at native `-O0`, default `-O2` and
in GHCi. Job 1303 passed the new `NativeStrictNamedCase` regression.

The separate R16 prototype in `r16-length-observation-20261123` records
the `size (map f xs)` observation on the installed `lengthMap` proof.
Job 1279 installed HaskLib. Its client compiler crashed, so this
prototype stays private.

The keyed-tree series ends at `3f82769e48` in
`r21-two-parameter-tree-20261123/source`. Job 1295 passed both
declaration orders, both legal splice boundaries and the negative
controls. Job 1299 matched the 556-line Showcase at native `-O0`
and in GHCi on this component. The keyed-tree regression also passed
at `-O2`. The combined checkout at `fc2186e9cb` in
`r03-r21-r41-integration-20261123/source` includes the tree series,
the R03 CorePrep correction and the R41 scoping fixtures. Job 1320
built both stages. Job 1321 passed the 556-line Showcase at native
`-O0` and in GHCi, the 17 callback observations, the installed tree
client, rule and proof upgrades, constructor matrices and interface readers.
Of 99 registrations, 98 passed. The merge had restored an obsolete
stderr file for `GadtEmptyRequiredOff`. Job 1343 passed the unchanged
test on the same compiler after removing that file. The fix is archived
as `r03-r21-r41-verified-tests-556`. Default `-O2` still rejects the
filter proof in the Showcase and the `map` and append proofs in the
HaskLib package test. Keep this combined compiler private.

The earlier R03 candidate at `b2cda4061d` failed specialization even
though ordinary runtime clients passed. The published fix lets a
client give `spaceDecision 'x'` the type `Decision Char.isSpace (Space 'x')`.
[Substitution example](roadmap/R03-dependent-products.md#specializing-an-imported-call).

Before integration, normal job 1574 built the separate flag
candidate at `4215377781`. Jobs 1576 and 1578 checked its real enum,
flag order, the old interface reader, changes to the provider flag, and
the new extension hint. With the standalone flag alone, a module can
use partial type functions. It does not also gain `foreach` or `some`.
The ordinary `MTrans` default is a separate open decision.

Jobs 1595 and 1601 identified another R23 limit. User-defined closed
family wrappers passed the higher-order arithmetic checks. With ordinary
type synonyms in their place, public 561 failed Core Lint on a partial
built-in family. Job 1605 tested private compiler component `3422b091ba`.
Direct addition, comparison and string prefixes passed at `-O0` and
`-O2`. An ordinary client read the new interface. The old reader
rejected a partial built-in with GHC-47808 and accepted a provider
that used only saturated built-ins. Normal job 1608 repeated those checks.
[Reproducer](roadmap/R23-unsaturated-type-families.md#passing-built-in-arithmetic-to-map).

Job 1335 used installed HaskLib on `4d10fdc35f` to check the
[append counterexample](roadmap/R16-open-term-evaluator.md#append-fusion-and-an-undefined-right-list).
The private guard rejected the proof at default `-O2`. The public R41
compiler still accepts it. Job 1338 separately verified that GHCi
shows constructor argument order from the published R41 interfaces.
This supersedes stashed job 1273.

On the private R16 compiler, job 1375 verified that the recursive
`lengthAppend` proof passes the same right operand at each use.
Jobs 1392 and 1393 checked that a source-hidden client can recover
that position at `492fe1285a`. Job 1423 also recovered the defining
name of the count after checking its recursive equations in final Core.
Stock fusion can introduce stricter intermediate folds. The guarded
compiler still checks the active rules at the optimized proof use.

Jobs 1468 and 1471 observed `Zero` from the mapped count and bottom
from the plain count on stock GHC and the public compiler.
Job 1473 reproduced the difference with two separate callers.
The published `lengthMap` witness still returned `Refl`. Job 1477
checked the runnable example in all three optimization modes.
[Source, controls and proof boundary](roadmap/R16-open-term-evaluator.md#map-length-and-sharing-under-stock-fusion).

The private R16 package review is `ae28f52fc6`. Job 1512 identifies
the old defining module when a client calls a re-exported proof. A pair
of byte-identical old interfaces bounds what the reader can recover.
The decision about enforcing optimized proof-use checks is pending.
[Package controls](roadmap/R16-open-term-evaluator.md#what-an-old-package-interface-can-tell-the-reader).

The 560 compiler includes the R04 coverage correction. Incomplete
patterns on a source sum now show the source alias, such as
`Packed Bool`, and the missing pattern. Job 1508 compared the new
warning with 558. The maintained regression passed again on 560.

The R23 compatibility decision for old, unannotated transformer kind
synonyms is pending. Private `6048ccc04b` accepts the untouched
`MTrans` program and preserves ordinary function uses. Its
reify-and-splice test fails with three out-of-scope inferred variables.
An old provider needs a rebuild to expose the new inferred indices.
The public compiler still requires the explicit `%M` migration. See
`r23-mtrans-compatibility-20261123/DESIGN.md` for the comparison and
`2026-11-23/r23-legacy-kind-experiment` for the archived source.

The local archive refs `r19-r41-normal-552`, `r16-hasklib-tree-553`
and `r27-legacy-transition-normal` are under
`refs/archive/dependent-haskell/2026-11-22/`. The `normal-sources/`
refs preserve original private compiler and test revisions. The 555
source is under `refs/archive/dependent-haskell/2026-11-23/r03-r16-r21-r41-normal-555`.

| Revision | Verified result |
| --- | --- |
| `5fe06625c3` | Clean two-stage build. Both stages matched the 543-line Showcase at `-O0` and `-O2`. Stage 2 GHCi and all 29 selected registered tests passed. The 542 and 543 compilers read each other's imported rule interfaces. |
| `c88606fe92` | Same compiler with the proof over the two runtime lists. Both stages passed the updated zipper tests and Showcase. The proof uses `size (zip xs ys) :~: Minimum (size xs) (size ys)`. |
| `6d442d71b8` | Both normal compiler stages matched 543 native outputs at `-O0` and `-O2`. Stage 2 GHCi matched the same output with breakpoints enabled. An ordinary recursive `minSize` works in the same module as the zip proof or in a library. |
| `dcaa2a244a` | All 30 selected normal-way tests passed on that compiler, including the single-file proof, imported proof, rule controls and no-code map regression. Both compiler-upgrade checks passed. |
| `c24ffa9ca0` and `f84b71ab53` | Clean two-stage compiler and separate 545-line fixtures. All four native Showcase runs, stage 2 GHCi, 30 registered normal tests and the upgrade checks passed. The compiler preserves demand on strict stored witnesses and tracks absent callbacks during full laziness. |
| `a7f42b75db` | HaskLib packages four runtime-list laws. Cabal built and installed its source distribution outside the checkout. Its package test and a separate client passed. |
| `5292956a82` | HaskLib also exports right-zero and right-successor lemmas for addition. A downstream module combines them with `lengthAppend` to prove the length of a list with one element appended. |
| `8e57520a14` | HaskLib's `lengthReverse` proves the length law for a runtime list. Its package test and separate clients passed. The public theorem mentions the installed Prelude function. Only HaskLib imports its accumulator worker. |
| `8d756d44da` and `193e3f35d0` | Both normal compiler stages matched 546 native outputs at `-O0` and `-O2`. Stage 2 GHCi, all 31 selected normal tests, the package consumers and the selective upgrade checks passed. The public source at `999f0ed489` has the same Git tree as the tested fixture. |
| `2d6368dd57` and `3ecc8d6fc9` | Both stages matched 546 native outputs at `-O0` and `-O2`. Stage 2 GHCi and all 31 selected normal tests passed. Installed clients used `zip` and `zipWith` proofs at ordinary `-O2`. A scoped equality with a runtime callback result rewrote the argument of `minSize`. |
| `6765da7957` | HaskLib exports `trans`, `congSucc` and `congFirst`. A single-import client uses them to prove the append-singleton and zip-after-map laws. Job `692` passed the final package and demand checks with the normal `2d6368dd57` compiler at both optimization levels. |
| `1011998c82` | The Showcase calls the direct and composed proofs on the same lists from IO. It prints `([False],Refl,Refl)`. Job `704` matched all 546 lines at native `-O0`, native `-O2` with stripped and ordinary interfaces, and GHCi. |
| `a395eea233` and `f73e659381` | Both stages built and matched 546 native Showcase lines at `-O0` and `-O2`. Stage 2 GHCi, installed HaskLib and the selective-upgrade checks passed. The official normal-way selection passed 39 of 40 tests. Unchanged `T15874` failed with the same GHC-45219 as on the preceding released compiler. |
| `d41294266d` and `a7088dd69f` | R19 composes type-level cases across two modules at a runtime enum index. The compiler sources match the normal two-stage build at `4e0c9b393a`. Job `828` passed `NativeDependentCoreRun` and the 547-line Showcase at stage 2 in native `-O0`, native `-O2` and GHCi. |
| `4f2fc1146f` and `229cecd250` | A client passes `HaskLib.size` as a retained callback. A scoped equality lets `safeHead` infer the tail index of a vector of length one. The installed-package clients and 548-line Showcase passed. Of 41 registered regressions, only the known `T15874` failed. |
| `4e169241d2` and `098ca0ae97` | Independent modules compose type-level cases on an unknown `Maybe Bool`. The `Just` field stays lazy. Both stages built, the 549-line Showcase passed, and all ten original term-capture diagnostic tests passed. The 158 upstream test outcomes match the previous release. |
| `2b64ffc406` and `930910d25b` | Both normal stages built. The 549-line Showcase, 23 focused regressions and interface upgrade checks passed. The full renamer run restored eight upstream goldens. R36 passed all 16 pun-warning regressions. R35 passed 25 namespace registrations, including the source-hidden regression in `e0fa805055`. |
| `5833e38cbd` and `ad644c290f` | The same polymorphic `Endo` supports an ordinary lambda and a promoted callback with omitted matchability. Lifted data family instances and fixed boxed unlifted data constructors accept ordered arguments. All 30 selected registrations and all three 550-line Showcase modes passed. |
| `fd19c8fe5f` and `6272edb866` | Both stages built. A field can index the result of a type-level case across modules. Boxed data with unknown levity and fixed-representation newtypes accept ordered constructor arguments. The opt-in CUSK warning passed its package checks. All 36 selected registrations and all three 551-line Showcase runs passed. |
| `00115859a5` and `f42ebe4659` | Nested cases compose across modules at unknown indices when their decisions agree. Representation-polymorphic ordered newtypes and bare higher-rank constructor aliases work. All 40 registrations and all three 552-line Showcase runs passed. |

[Equality continuations and kind inference](CHECKPOINTS.md#equality-continuations-and-structural-kind-inference), [HaskLib pipeline and parametricity](CHECKPOINTS.md#compose-installed-list-proofs), [Ordinary minimum](CHECKPOINTS.md#an-ordinary-minimum-in-the-zip-length-proof)
and [previous release](CHECKPOINTS.md#direct-zip-proof-and-retained-callback-rules).

## Current R16 result

**R16 open evaluation.** The [single-file example](examples/open-evaluation/ZipMinimum.hs)
defines `size`, `minSize` and the proof together. The result type is
`size (zip xs ys) :~: minSize (size xs) (size ys)`. The
[importing example](examples/open-evaluation/PreludeZipRuntime.hs) reads
the equations from the provider interface. An earlier home provider
rebuilds once. Rebuild an older installed provider to obtain its equations.

Job `499` built both normal compiler stages at `6d442d71b8`. The
registered tests use `dcaa2a244a`, which adds a signature to the
interactive test caller. The compiler sources are identical at both pins.
Job `503` completed qualification. Its results are in
`_build/on-demand/r16-normal-6d442d71b8-20261122/verification`.
The `source` link in that area points to the actual checkout at
`_build/on-demand/r16-normal-20261122/source`. Rebuild this compiler
from the checkout to repeat job `503`. The 543 and 544 comparison
compilers at `5fe06625c3` and `c24ffa9ca0` still run. After removal
of their intermediate build products, job `785` verified both compiler
hash manifests, compiled and ran native programs at `-O0` and `-O2`,
and passed the 543-to-current minimum-equation upgrade and warm rebuild.
R16 remains open for ordinary recursion beyond the checked case shapes.
The proofs for Prelude's `zip` and `zipWith` now compile at standard
`-O2`. The compiler verifies the installed fusion rules and the order
in which both functions inspect their lists.
[Open evaluator card](roadmap/R16-open-term-evaluator.md#prove-an-equation-for-preludezipwith).

The 546-line Showcase uses HaskLib's direct proof of the length of
`zipWith combine (map pre xs) ys`. The proof involving `map` still
disables rewrite rules in its own module. A downstream client derives
the same equation from `lengthZipWith` and `lengthMap`. Only that
derivation requires the whole left spine.
[Library guide](README.md#reuse-a-proof-with-hasklib) and
[equality controls](roadmap/R11-homogeneous-equality.md#rewrite-a-size-in-a-downstream-proof).

The inductive steps of `lengthMap`, `lengthAppend`, `plusZero` and
`plusSucc` now use `congSucc`. Job `792` ran the package client and
compared nine demand observations before and after this change at
`-O0` and `-O2`. Job `794` installed the source archive offline and
repeated all nine checks from a client with only the installed package.

Job `760` checks `oneCellHead` with the same 546-line output at native
`-O0`, native `-O2` and in GHCi. It uses `gcastWith` to return a
`Maybe Char` under the equality supplied by the runtime length test.
HaskLib now exports that operation as `withEquality` at `3f22060189`.
The installed clients in job `801` used it at kind `Type` and at kind
`Size`, forced the witness and rejected an unrelated equality.
Job `804` checked the new spelling in all 546 Showcase outputs at both
native optimization levels and in GHCi.

The standalone [filter example](examples/open-evaluation/ParametricFilter.hs)
states a free theorem at two element types. For Prelude's filter, the
client can import a proof by induction. For an arbitrary erased
polymorphic function, a bare `Refl` fails at the proof expression.
[Hypotheses and controls](REFERENCES.md#filter-naturality-in-the-presence-of-seq).

The earlier R19 selector designs are recoverable under
`refs/archive/dependent-haskell/2026-11-22/`. The published R19
implementation compares the generated case equations directly. Its private
compiler passed both known and unknown scrutinees at 1,610 constructors.
Jobs 802 and 815 built and tested that implementation in a normal compiler.

| Ref | Current evidence |
| --- | --- |
| `r19-eight-constructor-selector` (`568979c39e`) | Separate providers agree on a case over an unknown enum with up to eight nullary constructors. The wider recursive alternative in `r19-selector-depth-comparison` (`732d9c5bb8`) exceeds the default reduction depth for the last of 1,610 constructors. |

## Compiler builds submitted on 22 November

Check Pueue for each job before resuming it. The experiment directories
below are under `_build/on-demand/`.

| Job | Directory | What to verify |
| --- | --- | --- |
| 783, 806 | `r03-named-measured-zip-normal-20261122` | Both stages built at `0eb0718f35`. The dedicated named-callback, lexical-callback and rewrite-rule tests passed, as did the source-hidden HaskLib clients. |
| 798, 809, 816, 817 | `r03-named-safehead-normal-20261122` | Both stages built at `a43a310c08` and matched all 547 lines of fixture `cde69193d8` at `-O0` and `-O2`. Stage 2 GHCi and the installed HaskLib clients passed. The registered normal suite passed 39 of 41. The Core Lint repair in `4f2fc1146f` restores the stored-function test. |
| 802, 815 | `r19-closure-normal-20261122` | Both stages built at `4e0c9b393a`. Four registered tests passed. A stripped-interface client used the two case results at an unknown index. Changing a row rebuilt the consumer and rejected its old proof. |

The generated compiler trees for `0eb0718f35` and `a43a310c08`
were removed after qualification of `4f2fc1146f`. Each experiment
directory retains its Git source bundle, recipe and test results.
Rebuild the old compiler only if an experiment needs to run it.

The previous 548-line compiler is in `r03-r19-r36-normal-4f2fc1146f-20261122`.
Job 855 rebuilt both stages there. Jobs 864, 865, 868 and 871 checked
the Showcase, selected tests, installed HaskLib and upstream compatibility.
The build reuses the physical checkout under
`r03-r19-r36-normal-91fd7365bf-20261122/source`. Verify its actual
revision against the `compiler.source` file in the newer directory.
[Results and test counts](CHECKPOINTS.md#a-named-measurement-and-an-inferred-vector-length).

**R36 pun warnings.** Job 881 qualified the independent normal
compiler at `a20088cba1`. All 15 warning controls passed. The rename
suite recorded 479 expected passes, three expected failures and the
same 16 unexpected failures as the preceding compiler. Jobs 887 and
912 verified those controls and the 548-line Showcase on cumulative
source `575e35e0d6`.
The splice-scope check in job 889 then exposed a missing binding warning across declaration
groups. Job 913 also distinguished a fresh implicit type binder from
an annotation that would reuse an earlier term binder. The corrected
cumulative source is `58bcf66601`. The verified 575e comparator is in
`r36-combined-top-groups-20261122/preserved-575e`. Jobs 923 and
936 qualified the correction. The combined compiler passed the
complete R36 and R35 acceptance checks.
[Namespace results](CHECKPOINTS.md#namespace-management-and-ordered-gadt-results).

**R13 ordinary typechecking.** With only `RequiredTypeArguments`
enabled, a name in a type can resolve to a term. The released compiler
reports missing source equations in several of these cases. The repair
`faad9b1e9a` sends the captured name through the ordinary checker
when dependent types are off. Job 899 passed all ten original
`T23740a`–`T23740j` tests. Compiled clients used an imported proof
and HaskLib at both optimization levels. Four direct function calls in
types still checked with the extension enabled. This repair shares the
R19 two-stage build below.

**R19 cases on constructors with fields.** At `96efc76a77`, a
source-hidden client composes two cases on `Maybe Bool` at an unknown
index. Job 893 checked the unused `Just` field, a changed row, a
different index, old readers and reuse after rebuilding. The 549-line
Showcase also passed in the private compiler in job 902. Job 904
built both stages for R19 and R13 at `4e169241d2`. Rebuild that historical
compiler from `r19-ignored-fields-next-20261122/normal` if needed.
Job 927 passed the four maintained cases, interface compatibility and
the 549-line Showcase at native `-O0`, native `-O2` and in GHCi.
Job 928 passed the ten unchanged diagnostic tests. Of 158 upstream
dependent test registrations, 143 passed. All 15 failure outputs
matched the previous release.

**R41 nested GADT quantifiers.** Compiler `9884c35ff0` reads
fieldless ordered constructors and preserves all 24 one-field orders.
Job 903 built both normal stages. Job 924 passed 33 registered GADT
tests and the 548-line Showcase. Its verified installation is preserved
before the next build reuses the physical checkout.

Job 909 found a further failure for result indices such as `T a a`.
The private repair at `b13259766e` introduces the result equalities
at pattern entry and brings written constraints into scope at their
declared positions. Jobs 925 and 926 passed. The next aggregate
source is `2b64ffc406`. Job 932 built both stages in
`r41-r36-aggregate-normal-20261122`. The tests in job 951 passed
their release criteria. Read the final `qualification/qualification.pass`
and `qualification/rename-comparison-verified.tsv`. The raw job exited
1 because its report comparator assumed a system temporary directory.
The final comparison checked the same test results with the correct
temporary path. R41 still needs the remaining source-grammar checks.
[R41 ordering tests](roadmap/R41-nested-quantification-in-gadts.md#which-prefix-orders-work)
and [R19 scope](roadmap/R19-type-level-cases.md).

**R27 CUSK migration study.** The comparison source `aea0428256` in
`r27-cusk-impact-20261122/source` applies the three compiler lines
from [GHC MR !1602](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1602).
Jobs 905 and 911 compared four legacy declarations on stock and fork
GHC. In the 317-registration comparison, the proposed defaults added
one expected-output mismatch for an explicit deprecation warning.
The 19 pre-existing failures had the same diagnostics. Job 914
supplies Make's companion tools for the definitive comparison.
Jobs 919 and 922 built `binary` and `deepseq` with their original
Haskell2010 package settings. Both clients ran on both compilers
and linked the expected archives. The full comparison also exposed
an earlier fork failure in the positive upstream test `T19092`.
Stock GHC 9.14.1 accepts the same source. Job 918 records the
Core Lint diagnostic for the incorrect coercion.
Job 941 compared a three-package Haskell2010 release chain on stock
GHC 9.14.1. Adding 14 standalone signatures restored all three builds
with `-XNoCUSKs`. The 84 printed kinds in the six edited modules and
all three clients agreed with the original builds. The fork rejects
the unchanged first dependency at `Pure` under both configurations.
On the fork with the matchability changes, all 215 printed kinds in the
23 interfaces and all five clients agree across the CUSK migration. Job
1041 verified the warning and its five copyable suggestions on the first package.
[R27 scope](roadmap/R27-deprecate-cusks.md).

The main checkout has local Git recovery refs for these source
revisions under `refs/archive/dependent-haskell/2026-11-22/`:
`r36-module-and-pattern-scopes`, `r41-ordered-prefix-normal`,
`r19-r13-ignored-fields-normal`, `r27-upstream-migration-comparator`
and `r36-r41-r19-r13-549-normal`.
The compiler sources remain available there after generated builds expire.

The [release-readiness assessment](ROADMAP.md#release-readiness) records
R18, R35, R36 and R41 as complete on the fork. R27 keeps the legacy CUSK
defaults and an opt-in migration warning.

## Resume after a restart

1. Compare `git status -sb` and `git rev-parse HEAD` with the fork's
   `refs/heads/dependent-haskell`. The fork URL appears at the top of this file.
2. Read the current checkpoint, then restore the book and the candidate for
   the card you intend to work on. All required candidate sources are in the
   branch. The old experiment directories can be regenerated.
3. On the shared WSL host, inspect `env -u XDG_RUNTIME_DIR pueue status` and
   the [queue setup](CHECKPOINTS.md#restore-the-shared-wsl-build-queue).
   Use `tools/ghc-build-job` for builds. The shared GHC slice has a 16-core
   and 64 GB limit and yields to local benchmarks. Choose new assignments
   when work resumes.

## Build and verify

Use [check-plfh-revision](tools/check-plfh-revision) to test a published book
revision. Give it the local book repository, Git revision, compiler,
library directory and a new results directory. It reads the book and
HaskLib from Git. It keeps the diagnostic logs and runtime output and
removes the temporary build products.

```sh
docs/dependent-haskell/tools/ghc-build-job plfh-qualification \
    docs/dependent-haskell/tools/check-plfh-revision \
    /home/tritlo/Code/PLFH "$book_revision" "$compiler" "$libdir" "$results"
```

Check `verified.pass`, `modules.tsv`, `negative.tsv` and `reports.tsv`.
The runner supplies `-fomit-interface-pragmas -fno-write-if-simplified-core`
and checks the original reports once per optimization level. A report in
`fixed/` must compile at both levels. By default the checker builds
fresh no-code providers. That check reaches the open substitution
publication gap in the DeBruijn chapter. To check against native interfaces,
pass `--native-interfaces` immediately after `check-plfh-revision`.
This recompiles each book module using only the native `.hi` files of
its dependencies. The checker audits the resulting interfaces and runs
the negative clients against the same native interfaces. The
`readback-O0` and `readback-O2` directories contain the results.

The retained WSL build is under `_build/current/source`. Run these
commands from the GHC repository root. Hadrian puts its bootstrap-built
compiler in `stage0` and its self-hosted compiler in `stage1`.

```sh
stage="$PWD/_build/current/source/_build/stage1"
compiler="$stage/bin/ghc"
libdir="$stage/lib"
queue="$PWD/docs/dependent-haskell/tools/ghc-build-job"
check="$PWD/docs/dependent-haskell/check-showcase.sh"
"$compiler" --info | grep 'Project Git commit id'
"$queue" showcase-ghci bash "$check" "$compiler" "$libdir" --ghci
"$queue" showcase-o2 bash "$check" "$compiler" "$libdir" --default-o2
```

For the standalone PLFH runner, set `PLFH_GHC="$compiler"` and
`PLFH_GHC_LIBDIR="$libdir"` in its environment. The published runner from
`1e7fe2e934` selects the verified compiler here automatically. Older book
revisions need the environment variables because they name retired paths. For the combined book
use the `--native-interfaces` command above with the restored book ref.

A clean checkout can regenerate the compiler. Initialize the submodules,
including the fork of `libraries/binary`. Use GHC 9.14.1, Cabal 3.18.1.0,
Alex 3.5.4.0 and Happy 2.1.7 to reproduce the host toolchain.
From that checkout, run the following commands. On WSL, submit the build
through the shared queue.

```sh
git submodule update --init --recursive
./boot
./configure GHC="$(command -v ghc)"
docs/dependent-haskell/tools/ghc-build-job build-dependent-haskell \
  hadrian/build -j4 --flavour=quick --docs=none
```

On another host, run the same Hadrian command directly. A fresh build puts
the self-hosted compiler and its libraries under that checkout's
`_build/stage1`.

The runner checks native output, source-hidden clients and Core/STG Lint.
The interactive flag also checks GHCi output. Verify the installed
`GHC.Internal.List` interfaces and the registered tests as separate checks.
Record the number of tests run, failed or skipped. A compiler revision,
a built executable and a passing Showcase answer different questions.

After qualification, update this handoff and [the checkpoint record](CHECKPOINTS.md).
Keep the Git revisions, the commands and any outstanding failure. Release
superseded build products after their final consumer finishes. The main
`_build` cleanup threshold is 200 GB. After R41, retirement removed
700,309,504 bytes of superseded relinks and guide caches and
4,672,086,016 bytes of archived comparison-build intermediates. Job 1341
checked native linking and GHCi on both preserved installations.
See `retirement-r41-release-20261123` for paths and receipts.
After the 554-line release, the completed R27 package experiment
released 2,784,550,912 allocated bytes across 3,045 native products
and linker maps. Its sources, patches and comparison results remain.
See `retirement-r27-products-20261123/retired.tsv` for the file list.
The superseded `9202568aae` compiler released 892,313,600 allocated bytes
in 5,873 loose compilation objects. Job 1516 ran native programs with both
preserved stages and GHCi with stage 2. The bootstrap driver's interactive
mode matched the untrimmed 556 control. See
`retirement-554-checked-callbacks-20261123/retired.tsv`.
After 561, four superseded 543, 544, 546 and 554 build trees released
7,674,286,080 allocated bytes. Their exact source revisions are in Git.
Their build recipes and test results remain. The R18 and standalone R23
compilers ran their reader checks while those trees were absent.
See `retirement-legacy-normal-products-20261123/retired.tsv`.
After 562, job 1614 checked the 560, 561, standalone R23 and 562
compilers and the R18 wildcard component with the 555, 556 and 558
build trees removed. All five linked and ran a native program and
evaluated the same expression in GHCi. Cleanup recovered
11,673,653,248 bytes. The source revisions and build recipes remain.
See `retirement-555-556-558-20261123/retired.tsv`.
Job 1665 tested seven native and interactive compilers and three package
clients while four older compiler trees were absent. Removing the trees
recovered 15,574,077,440 bytes. Eight historical run names referred to
these four trees. Their source revisions, recipes and test logs remain.
See `retirement-four-archived-compilers-20261123/retired.tsv`.
Job 1711 tested six native and interactive compilers and three package
clients with the 561 and standalone R23 build trees removed. Cleanup
recovered 7,806,869,504 bytes. The archived source pins and recipes remain.
Replaying 562's original warm build requires rebuilding its standalone R23
donor first. The live reader tests use the existing 562 compiler directly.
See `retirement-561-standalone-20261123/retired.tsv`.
Job 1741 tested seven retained compilers and three package clients with
seven pre-560 compiler trees removed. The 571 compatibility and focused
suites then ran with those trees absent. Cleanup recovered
27,235,835,904 bytes. Rebuilding an original warm donor may be
necessary to rerun an older qualification. The pins, sources and recipes
remain. See `retirement-seven-pre560-20261123/retired.tsv`.
[Retention rules](CHECKPOINTS.md#clean-up-after-each-experiment).
