# Dependent Haskell handoff

Start from the `dependent-haskell` branch of [Tritlo/ghc](https://github.com/Tritlo/ghc/tree/dependent-haskell).
Use this file to resume an interrupted build or review. Update the revision,
test results and next action whenever a checkpoint lands or a build stops.
The [roadmap](ROADMAP.md) tracks feature scope. The [Showcase](Showcase.hs)
and its [expected output](Showcase.stdout) are the executable examples.

## Current checkpoint

Compiler `893d2ebc06` and Showcase `09b855305e` passed the 577-observation
checkpoint. A heterogeneous row now takes its two vector lengths from
one retained record. HaskLib also exports `lengthMapZip`; its proof uses
`trans`, `lengthMap` and `lengthZip`.

Job 1881 built the bootstrap and self-hosted compilers and passed all
ten selected registrations. Both warm upgrade tests, both package tests
and all 234 compatibility cases passed. The focused suite passed 137
of 138. The remaining failure is the existing `MTrans` diagnostic in
`deriving-via-compile`. Job 1893 ran all 577 observations at native
`-O0`, default `-O2` and in GHCi after the local row signature gained
an explicit `forall len`. The installed list-law client ran at both
optimization levels with rewrite rules disabled.
[Code and qualification](CHECKPOINTS.md#two-vector-fields-at-one-stored-length).

For a restart, verify `published-qualification.tsv` in
`_build/on-demand/r04-r16-r17-normal-after-574-20261123` with
`python3 accept-publication.py` from that directory. Job 1881's original
exit code is 1. Its three Showcase runs found the scoping error. The
receipt includes the corrected Showcase and the original test suites.

## Next compiler work

The published source above has 577 Showcase observations. Two later
compiler revisions have separate qualification directories.

- **578**, `1efc4bcaf2`, in
  `_build/on-demand/r04-r17-combined-quoted-columns-20261123`.
  Job 1900 built both stages. All 13 selected registrations, 578
  Showcase outputs in three modes and 234 compatibility cases passed.
  The matching interpreter accepted the spliced local type variable
  that the earlier compiler rejected. Read `cohorts/gates.tsv` for
  the focused suite. Two original incremental checks failed because
  `GHC.Essentials` changed. Job 1914 passed all four corrected comparisons.
  With one library database, only the old length worker and its new client
  rebuilt. Across the actual library upgrade, the unchanged control module
  also rebuilt once. Both native and typecheck-only upgrades reused all
  artifacts on the next invocation. The receipts are in
  `r17-essentials-warm-upgrade-578-20261123/results`. Also check the
  optimized proof control below before publication.
- **581**, `156cd84633`, in
  `_build/on-demand/r04-r19-following-quoted-578-20261123`.
  This source combines named dependent record fields, wildcard patterns
  in quoted type cases, and inline lambdas in visible type arguments.
  Job 1937 depends on the exact Showcase test in 1934. Check
  `qualification/gates.tsv`, `cohorts/gates.tsv` and `run-all.exit`.
  The unstarted 580 job was superseded.

| Area | Component evidence and remaining work |
| --- | --- |
| R04 named records | Jobs 1901 and 1907 passed native, typecheck-only and Template Haskell checks. An ordinary client zips both generated fields and rejects a field from a different record. A warm typecheck-only run reused the provider interfaces. A retained witness variable, field puns and record wildcards passed in job 1909. The separate omitted-payload candidate is `2c36fdea26` in job 1915. Its source requires the length field. It compiled and the first runtime demand checks passed. The warning-as-error check expected a different rendering of the flag. Job 1944 replays that check and job 1946 checks the missing-witness diagnostic after it passes. |
| R17 and R19 quotations | Job 1902 checked the old and new encoders. Shared payloads had identical bytes and decoded in both directions. The old reader rejected the new `LamT` tag. Jobs 1897 and 1904 qualified wildcard patterns in quoted type synonyms. In job 1905 an ordinary client read a spliced row of named and missing sensors. |
| R17 visible type arguments | Compiler `0076e5167b` passed the source-hidden comparison in job 1912. Named, annotated, closed, captured and spliced callbacks returned the same row. Job 1925 compares an enclosing type variable in a spliced visible argument on the separate compilers. Both separate compilers rejected the exact combined capture in job 1925. The combined registration is `NativeTypeLambdaTypeAppCapture`. |
| R16 list proofs | HaskLib built from its source distribution with `zip xs ys` in the composed proof. The installed law ran with rewrite rules disabled at both optimization levels. Job 1911 found different outer results for the independently optimized mapped count and minimum on stock GHC with a `seq`-sensitive producer. On 577, job 1916 observed `Zero/Zero/Refl` at `-O0` and at `-O2` with rewrite rules disabled. At default `-O2`, specialization exposed a fused `build` operand that Core Lint rejected. Job 1945 compares ordinary counts and optimized proof callers on 577 and 578. See `r16-zip-map-demand-20261123/locals/published/O2/compile.log` and `optimized/results.tsv` in the same experiment. Job 1940 tests a private rule-dependency check for signed primitive proofs. |
| R23 matchability | The private prototype `99bfde4ec4` passed the original transformer test and 32 Template Haskell reification cases. An old installed provider still needs rebuilding. The published compiler retains the existing matchability policy. |

The GHC queue has two slots. Other benchmark jobs take priority and
freeze the GHC slice. Use Pueue task labels and the live cgroup state
when resuming. Pueue's `switch` command exchanges the task contents
at two IDs. Job 1914 is the warm comparison, and 1937 is the 581 build.

Job 1948 tests a change to the Specialise pass on the published 577 compiler.
It applies the in-scope substitution to an operand that determines a result
type. It also checks an ordinary caller and a vacuous retained binder.
See `r05-specialise-retained-zip-20261123` under `_build/on-demand`.
A successful Core Lint check only establishes that the operand is well typed.
The independent demand comparison and the R16 proof check are still required.

Job 1947 compares the runtime allocation of `zipRows` with an ordinary
recursive zip that has the same two constructor fields. Each implementation
runs in a separate process. Both must return the expected checksum and pass
the prefix-demand checks. See `opencode-go-20261123/ziprows-pilot` under
`_build/on-demand` for the driver and results. Keep compiler allocation,
runtime allocation and regression review independent of the feature builds.
Job 1949 measures whole-compiler allocation on 32, 128 and 256 shared
bindings. The ordinary and retained inputs use the same arithmetic worker.
The two performance jobs use the existing GHC queue.

The generated compiler trees for checkpoints 549, 564, 566, 567, 568,
569, the first quotation compiler, its superseded row compiler, and
the completed R23 prototype have been retired. Sources, recipes and
test receipts remain. See the `retired.tsv` manifests in
`retirement-completed-r19-r23-20261123`,
`retirement-r17-before-578-20261123`,
`retirement-normal-564-566-20261123` and
`retirement-normal-567-569-20261123`.

## Previous checkpoints

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

The two GHC build slots share a 16-core, 64,000,000,000-byte slice and
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

1. Check `git status -sb` and `git ls-remote fork refs/heads/dependent-haskell`.
   Compare both revisions with the checkpoint table. Fetch the fork branch
   if this checkout predates the release.
2. Check `env -u XDG_RUNTIME_DIR pueue status` and inspect the named job and
   its checkout. On 22 November, Pueue recorded signal 15 (exit 143) for
   job `382`. Its shell wrote a zero exit status during shutdown. The
   missing `build.pass` and the unfinished Hadrian log confirmed the
   interruption. Check Pueue, both compiler revisions and the success
   record before reusing a result.
   The earlier WSL restart erased `/tmp` while Git and Pueue retained
   records for its old paths. Keep active builds under `_build/on-demand/`.
3. Restore the [shared WSL build queue](CHECKPOINTS.md#restore-the-shared-wsl-build-queue)
   before submitting work. Use at most two subagents. The coordinator
   reviews and integrates their results. Check the live assignments and
   queue before resuming a roadmap card. The group has two slots. All GHC jobs share
   16 logical CPUs, 64,000,000,000 bytes of memory and zero swap.
   Higher-priority jobs pause the queue and freeze the GHC slice.

## Build and verify

The 574-line Showcase uses the normal two-stage compiler at
`c11b29a399` in `r04-r17-r21-normal-after-572-20261123`.
On this host the bootstrap tools are GHC 9.14.1, Cabal 3.18.1.0, Happy 2.1.7
and Alex 3.5.4.0. A fresh checkout needs the Git submodules at their recorded
revisions. This build copied the preceding build tree, ran `./boot`
and `./configure`, and reused the preceding Hadrian executable with
`-j4 --flavour=quick --docs=none`. Reuse the configured checkout and Hadrian
binary if a build stops partway. Keep each compiler with its own libraries.

Hadrian puts the stage 1 compiler in `_build/stage0/bin/ghc` and the
self-hosted stage 2 compiler in `_build/stage1/bin/ghc`. Both use the
target libraries in `_build/stage1/lib`. For example, from the main checkout:

```sh
stable="$PWD/_build/on-demand/r04-r17-r21-normal-after-572-20261123/source/_build/stage1"
queue="$PWD/docs/dependent-haskell/tools/ghc-build-job"
check="$PWD/docs/dependent-haskell/check-showcase.sh"
"$queue" showcase-stage2-ghci bash "$check" "$stable/bin/ghc" "$stable/lib" --ghci
"$queue" showcase-stage2-o2 bash "$check" "$stable/bin/ghc" "$stable/lib" --default-o2
```

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
