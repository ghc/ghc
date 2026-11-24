# Historical verification records

These records describe individual compiler checkpoints. Each result applies
to the implementation and tests named in its section. For the published
compiler and the current queue, use the [program roadmap](ROADMAP.md#where-this-fork-stands).
The [implementation log](PLAN.md) gives the specifications for the numbered
increments.

## Wave 1: simultaneous substitution, same-module lookup, context plug, and closed-family refinement

Compiler `186670b0b9` (atop `69a36b132f`) consolidates four Wave 1 lanes:
R16 accepts `-O2` FloatOut constructor helpers (`exts1`) and publishes open
`T:subst` so all 20 constructors of PLFH `Bisimulation.simSubst` check with
ordinary functions; R21 stages same-module `lookupEnv` with `.hi` export
visibility (`[GHC-70566]`) and early-vs-final Core agreement; R03 refines
closed families from nominal retained Givens while tracking nested checked-call
and reduced proof-synonym rule dependencies; and R13 publishes two-index GADT
traversals (`plug`) and untyped `substSingleU` open equations.

Both self-hosted stages built at `186670b0b9` in `_build/on-demand/wave1-two-stage`.
All 15 qualification gates passed (`0`), including 55/55 registered tests and
all 627 Showcase observations at native `-O0`, default `-O2`, and in GHCi.
All 234 compatibility registrations passed, and 137/138 focused tests passed
(keeping only the known upstream `deriving-via-compile` `MTrans` diagnostic).
On PLFH `8d868df`, all 34 book modules passed in all four modes (`native-O0`,
`native-O2`, `readback-O0`, `readback-O2`) with 10/10 negative rejections per mode.

## Direct induction on the second argument

Compiler `9d310102c7` checks both `plusComm m Zero = plusZero m` and
`mapAppend _ [] _ = Refl`. The earlier arguments can include an ordinary
pattern or a retained function. The original match checks the patterns
from left to right. Job 2763 checked a failing earlier pattern, a guard
that falls through, lazy constructor fields and a false index.
The false index still fails at `Refl`.

PLFH Lambda also uses the ordinary constructor spelling in its fixed-point
reduction. Job 2763 checked the chapter and its separate readers.
Job 2793 used unchanged batch 18. Of 38 original reports, 29 compile at
`-O0` and 26 at default `-O2`. The original `R03_FunctionColumnSplit`
still needs the rewrite rules for append disabled at `-O2`.
Job 2781 ran the combined chapter changes. All 28 reports in its fixed
directory compiled in both modes. The native readers each passed 84 checks.

## Required congruence and sum over append

PLFH Induction can write `congVis Succ (plusAssoc m n p)` in the
successor case. It can also pass the partially applied `plus m` in
the same argument position. The following equality proof determines
the endpoints. Jobs 2722 and 2723 checked the same 38 original reports
on PLFH batch 18 before and after compiler `35916907b9`.
Only `R23_MatchabilityPolyCong` changed from rejection to acceptance.
The totals rose from 25 to 26 at `-O0` and from 23 to 24 at default `-O2`.

Compiler `c130ce0391` checks that summing two appended lists gives
the sum of their separate folds. The proof calls the ordinary library
fold. It uses the checked equations for append with rewrite rules
disabled in the proof module. Job 2730 checked the corrected theorem
with native and fresh no-code interfaces. The unchanged book report
uses associativity in the opposite direction. With rules disabled, it
now reaches that error in the cons case. A false open endpoint in a
separate reader still fails with GHC-83865.

Job 2740 matched all 609 Showcase output lines in GHCi and with native
code at `-O0` and default `-O2`.
[Congruence proof](examples/open-evaluation/BookCongruence.hs)
and [fold proof](examples/open-evaluation/BookFoldProof.hs).

## Reversal and commutativity in batch 15

The Lists proof shows that `shunt xs ys` equals `rev xs ++ ys` by
induction on the input list. The same module defines the ordinary
recursive `rev` and proves its accumulator law. Jobs 2632 and 2639
checked that combination and the maintained regression. A changing rule
on the imported append function still rejects the proof. The proof
module disables rewrite rules when it needs the Prelude append equations.
An installed HaskLib client also needs that setting to open the witness
from `appendAssoc` at `-O2`. Jobs 2651 and 2654 checked the package,
the default rejection and the successful reader.

The Connectives chapter packages the forward and reverse swaps of an
`Either` or a pair with their inverse laws. Job 2646 checked both laws
and their runtime behavior on lazy payloads. Job 2661 checked the
registered regression. Extending the early reservation to arbitrary tuple
functions broke valid projection and reconstruction examples. The
compiler restricts early reservation to the checked permutation.
[Compiler and regression](https://github.com/Tritlo/ghc/commit/da06279b7a4ba86fef478e3f988297ebd3306fd2).

The [605th through 607th examples](Showcase.hs) exercise these laws.
Job 2674 matched all 607 expected lines at native `-O0`, default `-O2`
and in GHCi. The reversal witness is opened inside its proof module.
The sums and products use different input types to expose their field
order. [PLFA provenance](REFERENCES.md#plfh-arithmetic-dependent-patterns-and-callback-inference).

Job 2700 replayed the 607 observations using ordinary boxed pair
patterns and expressions. The inverse proof can now use
`swapPairInv (_, _) = Refl` and the reader can pass `(True, GT)` directly
to that proof. Demanding the proof forces the outer pair and leaves
both fields lazy. With irrefutable tuple matching enabled, that pattern
cannot establish the constructor equality. Job 2696 checked that
boundary in single- and two-column matches. Job 2702 checked the
updated combined book tree `21c2f158` in all four configurations.

Jobs 2663 and 2664 used [the PLFH checker](tools/check-plfh-revision)
on the combined chapter patch and on unchanged batch 15. Both built
27 modules and audited all interfaces in four configurations. Both native
readers passed 76 runtime checks. Each configuration rejected six negative
programs. On unchanged batch 15, 25 of 37 original reports compile at
`-O0` and 23 at default `-O2`. Against the same input, job 2619 recorded
23 and 22 before the reversal and commutativity fixes.

## Composing isomorphisms in the published book

The [composition example](examples/open-evaluation/BookIsoComposition.hs)
stores `h . f` in the forward field and `g . k` in the inverse field.
In the proof, `kh (f x)` cancels one inverse pair. Applying `cong @g`
preserves the surrounding function and `trans` composes the result with
`gf x`. Job 2601 evaluated the composed Boolean-to-bit isomorphism and
its inverse in the 604-observation Showcase at native `-O0`, default
`-O2` and in GHCi. Job 2594 also checked a re-exported composition
function, a false proof, a strict worker, an ignored bottom callback
and an active rewrite rule.

Job 2603 checked the unchanged [batch 14 book](https://github.com/Tritlo/PLFH/tree/d343521eb54eb18aca970b302adb3583f98f3111)
on the composition compiler. Job 2597 ran the same inputs on the
preceding compiler. The four book builds and all 74 original report
outcomes agreed. At `-O0`, 23 of 37 reports compiled. At default `-O2`,
21 compiled. The six negative programs failed with their expected
diagnostics in each build. Both native readers ran 73 observations.
The 27 interfaces passed the audit in all four builds. The unchanged
`R04_ComposedIsoFields` uses opaque local aliases for the compositions and
still fails. The book patch uses Prelude `(.)` directly in the two fields.
Job 2600 passed all 56 checks on the patched book.
[PLFA provenance](REFERENCES.md#plfh-arithmetic-dependent-patterns-and-callback-inference).

## Nested Soundness patterns and identity isomorphisms

The [R11 fix](https://github.com/Tritlo/ghc/commit/3326acd9b1fea98090eb5e8f3f89b5a85e44d849)
keeps the original cast when projecting equality evidence from a nested
GADT pattern. Job 2561 compiled the Soundness chapter with its environment
pattern inlined at both optimization levels. An intentionally wrong field
retained its GHC-25897 diagnostic. Job 2573 rebuilt the 27 modules in
published PLFH `0fd43f5` in four configurations. Of 31 original reports,
22 compiled at `-O0` and 21 at default `-O2`. R11 was the only report
whose result changed since job 2505. The two native readers passed 66
observations each. Each of the four builds rejected five negative clients.

The [603rd Showcase observation](examples/open-evaluation/BookIsoRefl.hs)
instantiates the polymorphic identity isomorphism at `Maybe Bool` and
opens its two inverse proofs. Job 2559 matched all 603 observations at
native `-O0`, default `-O2` and in GHCi on compiler `8b37fdfd34`.
Job 2574 repeated the checks with the R11 fix and its updated GADT fixture.
The proof module disables rewrite rules. Job 2551 ran the Isomorphism
chapter patch and recorded 54 passes. A changed identity rule, a strict
identity and a false round trip remained separate negative tests.

On the published De Bruijn chapter a reader can prove
`substSingle ('Var 'Z) w :~: w` from the provider's full optimized
interface. Job 2556 also tested capture avoidance under a lambda in
the earlier `49903d0` chapter. With compiler `549657008d`, job 2589
also checked the capture-avoiding shift through the current chapter's
full optimized interface, at both reader optimization levels. The reader
rejects the endpoint that would capture the variable. The evaluator
recognizes the compiler-generated weakening of the callback's argument
multiplicity. A stripped or fresh no-code provider omits
the unfoldings required for that proof. General substitution for a
neutral function body remains open.

## Polymorphic proofs from the published book

Job 2505 checked [PLFH `0fd43f5`](https://github.com/Tritlo/PLFH/tree/0fd43f5353c4c3ae7670bac668ff2abe8f3c67ea)
with [compiler `34582bce54`](https://github.com/Tritlo/ghc/commit/34582bce5489634e15d9fc16a53d37b19dcc58e5).
In four fresh builds, all 27 modules passed the interface audit. Both
native executables passed 66 observations. Each build rejected all five
negative programs with the expected diagnostics. Fresh no-code builds
emitted no object or bytecode files. Of 31 compiler reports, 21 compiled
at `-O0` and 20 at default `-O2`.

- Jobs 2457 and 2475 isolated the map proof alias. GHC inferred both
  callback endpoint types but compared the dependent function types
  before reading the assignments. Reading them fixes
  `lengthMapLaw = lengthMap`. A wrong length still fails with GHC-83865.
  Job 2479 checked that forcing an alias of a bottom-valued proof still
  raises the original exception.
- Job 2471 checked the distinction between a record label that also
  names an independent type index and a selector applied in a field or
  result type. The latter retains term lookup.
- Jobs 2454 and 2482 checked an unrelated retained proof beside ordinary
  subtraction. Native and no-code clients can use the equation when GHC
  verifies the original case order and result. A strict producer, local
  rewrite rule or loaded plugin suppresses the no-code equation.
- Jobs 2487 and 2488 checked polymorphic `Iso` fields through compiled
  interfaces. A reader reverses the isomorphism by exchanging both
  functions and proofs. Keeping the old proof order fails. Job 2503
  checked the binary-natural embedding and isomorphism-to-embedding
  proof. Job 2509 ran the full book harness on that chapter patch and
  reported 53 passes and zero failures.
  [Agda and GHC provenance](REFERENCES.md#plfh-arithmetic-dependent-patterns-and-callback-inference).

Job 2510 ran the same [isomorphism definition](examples/open-evaluation/BookIsomorphism.hs)
in the registered interface test and the Showcase. Both native optimization
levels and GHCi matched all 602 observations. The reader converted a Boolean
to a bit and evaluated both proofs. The regression still rejected swapped
proofs. [Published example](https://github.com/Tritlo/ghc/commit/f6aff88d4040111518f1d148aaf0be20c01f009d).

## Exponentiation and keyed trees

The [ordinary exponentiation fix](https://github.com/Tritlo/ghc/commit/1f873969e589e84b632e5d0304399ab4d2c997e3)
checks three PLFH laws through compiled Naturals and Induction interfaces.
For example, the book can state the following law about its own functions.

```haskell
powPlus :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
        -> pow m (plus n p) :~: times (pow m n) (pow m p)
```

Job 2420 checked all three laws at both optimization levels. A worker that
changed its carried base published no equation. At runtime, raising an
undefined base to zero returned one. Job 2433 checked 27 book modules with
the preservation and subtraction patches in four configurations. Of 29 original
reports, 17 compiled at `-O0` and 16 at default `-O2`.

The [R21 fix](https://github.com/Tritlo/ghc/commit/3c380131aec0954c2a2cae17abeea46fb0ab40ca)
checks `nodes (mapValues f t) :~: nodes t` for a tree with independent key
and value types. Jobs 2436–2438 tested four declaration and splice
arrangements at both optimization levels and an independent proof through
interfaces. Crossed key types and a wrong branch count still failed.
The combined compiler passed the PLFH subtraction and exponentiation checks.

Job 2441 checked the [De Bruijn value-classification change](https://github.com/Tritlo/ghc/commit/2e4fbb752a2fca6d9d4ac4b7901e2aa07b3c1534)
on the exponentiation build. An ordinary client checked the chapter's value
classifier at both optimization levels and observed that inspecting a lambda
leaves its body unevaluated. The separate equation-form example remains open.

## Truncated subtraction in the Induction chapter

The [published compiler](https://github.com/Tritlo/ghc/commit/63e2f0974b174dc86bef79c3a84d1d76fea3dadf)
checks the following PLFH exercises with the ordinary `monus` from Naturals.

```haskell
zeroMonus :: foreach (n :: Nat) -> monus 'Zero n :~: 'Zero
zeroMonus Zero     = Refl
zeroMonus (Succ _) = Refl

monusPlusAssoc :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
               -> monus (monus m n) p :~: monus m (plus n p)
```

Job 2403 checked the original reproducer and a client that could read only
compiled interfaces. Both optimization levels accepted successor cancellation
and the two book proofs. They rejected two false or unjustified equalities.
Runtime tests checked that subtraction inspects the second operand first
and leaves unused predecessor fields lazy.

Job 2405 built PLFH revision `85f90be` with the Induction and preservation
patches together. All 27 modules passed the interface audit in native and
no-code builds at both optimization levels. Sixteen of 29 PLFH reproducers
compiled at `-O0` and fifteen at default `-O2`. The difference was the
append rule in `R13_ShuntAccumOpenEq`. The source layout also matters:
placing subtraction next to the proofs in `BookCalculations` lost its
exported equation. A separate Naturals provider retains it. Job 2414 matched all 600 lines
of the Showcase at native `-O0`, default `-O2`, and in GHCi.

## One-step preservation with same-module substitution

The [published compiler revision](https://github.com/Tritlo/ghc/commit/692eae077896a00384fecf15a3d5d92d828a6415)
checks these declarations from the PLFH Lambda and Properties chapters.

```haskell
-- Constructor of Step
BetaLam :: forall v x n. Value v
        -> Step ('App ('Lam x n) v) (substTerm n x v)

preserve :: foreach (m :: Term)
         -> HasType 'CEmpty m a -> Step m n -> HasType 'CEmpty n a
```

The result of beta reduction is the term computed by the book's ordinary
`substTerm` function in the same module. A caller supplies evidence that the
closed source term has type `a` and takes a step. The proof returns a typing
derivation for the target at the same `a`. The fixed-point rule also uses
substitution. Its replacement can itself be a fixed-point term.

Job 2381 built all 27 modules from PLFH proof revision `3251159` in four
fresh directories. At `-O0` and default `-O2`, native and no-code providers
passed the interface audit. A source-hidden client read each provider.
The native clients exercised all eight reduction constructors. The no-code runs
produced no object or bytecode files. The compiler rejected a false beta
target (GHC-83865) and an extra substitution argument (GHC-95781) in all
four runs. This covers one-step preservation for the book's closed,
simply typed language.

Job 2384 used the same compiler components. All 599 lines in
[the Showcase](Showcase.stdout) matched at native `-O0`, default `-O2`,
and in GHCi. Jobs 2376 and 2377 also tested a fresh no-code interface
that exports both a structural equation and a list-accumulator equation.
The [handoff](HANDOFF.md#current-checkpoint) records the separate two-stage build.

### Nested equality evidence and the PLFH reproducer corpus

The [fix at `9cc4065fab`](https://github.com/Tritlo/ghc/commit/9cc4065fab4f2f106859b39daa9bdad23f00f0fd)
keeps equality evidence that an inner dependent case uses. On the earlier
revision, Core Lint reported a variable out of scope in the Denotational
chapter's nested `subInvDist`. Job 2389 checked the proof at both optimization
levels and ran a client through compiled interfaces. The client exercised
six impossible-branch witnesses and the successful factorization. The compiler
rejected the wrong witness. Job 2396 verified the registered regression.

Job 2394 repeated the four book checks on this revision with the preservation
patch on PLFH `7c38276`. All 27 modules passed each interface audit. Of 29
standalone PLFH reports, 15 compiled at `-O0` and 14 at default `-O2`.
The difference was `R13_ShuntAccumOpenEq`, which uses `(++)` in its theorem.
Default `-O2` reported GHC-83605. Truncated subtraction, exponentiation,
and reverse remained open on this revision at both optimization levels.

## Compare schema transformers before supplying the rows

```haskell
forwardSchemaTransformer :: forall (e :: Type). Proxy e
                         -> Map (Prefixes ('Just e))
                            :~: Map (\cells -> Prefixes ('Just e) cells)
forwardSchemaTransformer _ = Refl
```

A caller can use the equality while the column groups are still unknown.
The compiler compares the named and lambda callbacks at their function
kind. A second example defines `type Validate err = \cell -> CheckCell err cell`.
The closed family has no recursive call. The declaration checks with
`NoUndecidableInstances`. [Schema transformer](examples/open-evaluation/TypeLambdaSchema.hs)
and [closed validation family](examples/open-evaluation/CellValidation.hs).

Both compiler stages built from `1a02a866e7`. Job 2080 checked all
590 Showcase observations at native `-O0`, default `-O2` and in GHCi.
The 20 selected registrations, four upgrade checks, map-fusion checks and
both package checks passed. Both compatibility and focused results are
recorded by `verify-publication.py` in
`_build/on-demand/r17-schema-transformer-post-588-20261124`.
The R17 roadmap card remains partial.

## Copy one field between records after comparing their lengths

```haskell
copyFlagsAtSameLength :: NamedColumns -> NamedColumns -> Maybe NamedColumns
copyFlagsAtSameLength p q =
    case sameNat (storedLength p) (storedLength q) of
        Just Refl -> Just (p { storedLength = storedLength q, namedFlags = namedFlags q })
        Nothing -> Nothing
```

The caller keeps the letters from `p` and copies the flags from `q`.
The equality from `sameNat` justifies keeping the letters when assigning
the second record's length. The ordinary importing module needs only
`GADTs`. Equal lengths produced `Just [('x',False)]` after reading the
result. Unequal lengths produced `Nothing`.
[Ordinary record client](examples/packed-vector/NamedSiblingColumnsClient.hs)
and [dependent sums](roadmap/R04-dependent-sums.md#named-fields-at-one-stored-length).

A second example proves `Map (Prefixes ('Just e)) rows :~:
Map (\cells -> Prefixes ('Just e) cells) rows` for an unknown list of
column groups. The proof uses the equation of the forwarding lambda.
R16 also distinguishes a rule on `Nothing` from one on `Just True`
when checking a source equation.
[Schema proof](examples/open-evaluation/TypeLambdaSchema.hs),
[lambda conversion](roadmap/R17-type-level-lambdas.md) and
[constructor tests](roadmap/R16-open-term-evaluator.md#constructor-clashes-with-different-field-counts).

The two-stage compiler was built from `d15cbd41fe`. Job 2027 checked
588 observations in native `-O0`, default `-O2` and GHCi, 20 selected
registrations, four upgrade checks, HaskLib, first-class-families and
234 compatibility cases. Job 2125 ran the published fusion regression
on the same compiler. See the [current handoff](HANDOFF.md) for the
focused-suite result and the publication check.

## Change the length of a dependent record

An ordinary importing module can clear both vectors of a nonempty
record and set their shared length to zero.

```haskell
clearColumns :: NamedColumns -> NamedColumns
clearColumns p = p { storedLength = Z, namedLetters = VNil, namedFlags = VNil }
```

The result has length zero and contains no pairs. Evaluating the new
record inspects the old record's outer constructor and leaves its vector
fields unevaluated. If an update changes the witness and omits a dependent
field, GHC-83605 names that field. Giving a nonempty vector at `Z`
produces GHC-83865 at the replacement expression.
[Dependent record](roadmap/R04-dependent-sums.md#named-fields-at-one-stored-length)
and [runnable example](Showcase.hs).

Both stages built from `db2586fdc2`. All 585 Showcase observations matched
at native `-O0`, default `-O2` and in GHCi. The focused suite passed
137 of 138, including the repaired Core test for localised function and
selector identities. The remaining failure was the existing `MTrans`
diagnostic. All 18 selected registrations and 234 compatibility cases
passed. The package and compatibility checks used fresh output directories
after the original run encountered output from the 584 compiler.

## A spliced row schema and inferred zip arguments

The declaration quotation for `CheckedColumns` captures the error type
in a type lambda. Splicing it produces the same column types as writing
the lambda in the client signature. The separate `labelledColumn`
example captures a type variable bound by its enclosing function.
[Quoted column definitions](examples/open-evaluation/TypeLambdaQuote.hs).

An installed HaskLib client can use this proof with unknown lists.

```haskell
lengthMapZip f xs ys = case lengthMap f (zip xs ys) of
    Refl -> lengthZip xs ys
```

The declarations of the two lists determine the two erased type
arguments of `zip`. An empty left list leaves the right list unevaluated.
An undefined left list raises its exception, even if the right list
is empty. The callback stays unevaluated in those checks. Job 1889
installed HaskLib from its source distribution and ran the client with
rewrite rules disabled at `-O0` and `-O2`.
[Proof and its optimization conditions](roadmap/R16-open-term-evaluator.md#map-over-the-result-of-zip).

## Two vector fields at one stored length

A `Columns` constructor stores one length and two vectors. Their
element types differ. Both projections use the stored length as their
index. An ordinary client can therefore apply `zipVec` directly.

```haskell
readPairs :: Columns -> [(Char, Bool)]
readPairs p = toList (zipVec (letters p) (flags p))
```

Selecting the Boolean vector from another record fails with an index
mismatch. Job 1863 upgraded a provider which already had an older
equation marker. Native builds and typecheck-only builds recovered the
new projection equation and reused their outputs on the next run.
[Ordinary caller](examples/packed-vector/SiblingColumnsClient.hs).

R17 job 1880 checked an inline type lambda that builds a row with
those same two vector types. A separately compiled client used a typed
eliminator at both optimization levels. R16 job 1876 installed HaskLib
and ran the composed proof for the length of a map over zipped lists
with rewrite rules disabled.
[Row example](examples/packed-vector/SiblingColumnRow.hs) and
[composed list law](roadmap/R16-open-term-evaluator.md#map-over-the-result-of-zip).

Normal job 1881 built the bootstrap and self-hosted compilers at
`893d2ebc06`. All ten selected registrations passed. The ordinary
replacement, historical reader, warm upgrades, field visibility,
first-class-families, HaskLib and installed list-law checks passed.
The Showcase's original row signature used the name of a length
already in scope. Job 1893 ran the example with `forall len` on the
same compiler. All 577 observations matched at native `-O0`, default
`-O2` and in GHCi. [Runnable example](Showcase.hs).

Job 1896 isolated the scoping rule in a small client. With `n` bound
by `readIORef`, the unquantified signature mentioning `n` failed.
A fresh name worked. Both `forall n` and `forall len` worked.
All 234 compatibility registrations passed. The focused suite passed
137 of 138. Its only failure reproduced the existing transformer kind
synonym diagnostic in `deriving-via-compile`.

## Replace a vector after comparing runtime lengths

The 574-observation compiler at `c11b29a399` accepts an ordinary client
that uses a runtime equality test to replace a vector. The target length
and source vector may come from separately read packages.

```haskell
replaceCells :: CharVector -> CharVector -> Maybe CharVector
replaceCells target source = case sameNat (lengthOf target) (lengthOf source) of
    Nothing -> Nothing
    Just Refl -> Just (CharVector (lengthOf target) (cellsOf source))
```

The provider defines the datatype and its projections in one module.
An unequal length returned `Nothing` without forcing either payload.
When the lengths matched, the result contained the source vector.
Without the equality test, GHC-83865 identified the source projection.
[Showcase client](examples/packed-vector/RetainedCharClient.hs).

Job 1853 built the bootstrap and self-hosted compilers. Upgrading a 571
build recompiled the ordinary length worker and checked the newly added
consumer. This worked for native code and `-fno-code -fwrite-interface`.
The unrelated module retained its files. A second invocation recompiled
nothing, and the file hashes and timestamps matched in both modes.
All nine selected registrations passed. The R17 older reader, ordinary
replacement, field visibility, first-class-families and installed HaskLib
checks also passed. All 574 Showcase observations matched at native
`-O0`, default `-O2` and in GHCi.

All 234 compatibility cases passed (231 normal and three GHCi). The
focused suite passed 137 of 138. Its one failure was the known
`deriving-via-compile` transformer-kind diagnostic. Job 1858 ran the
maintained `--retained-worker` test against the 571 and 574 normal
compilers. It also checked the stale interface in isolation.
[Two-compiler regression](examples/closed-evaluation/check-readback-upgrade.sh).

## A vector projection indexed by an ordinary length function

The R04/R21 candidate `3d95b04b27` checks the following two definitions
in the same module. The package datatype comes from another module.

```haskell
localLength :: CharVector -> Nat
localLength (CharVector n _) = n

localCells :: foreach (r :: CharVector) -> Vec Char (localLength r)
localCells (CharVector _ xs) = xs
```

In the Showcase, reading the length leaves the vector unevaluated.
Reading the vector from a package with two undefined fields reports
the error in the vector field. The combined observation is
`(1,"r",True)`. The test also checks the length of a known package
with `Refl`. Job 1752 checked that the 571 reader could use the new
equation from the stripped interface. It rejected a result at the
successor index with GHC-05617.
[Definitions](examples/packed-vector/RetainedCharLocal.hs),
[ordinary consumer](examples/packed-vector/RetainedCharClient.hs)
and [declaration order](roadmap/R21-dependency-analysis.md#stage-a-retained-witness-projection).

An ordinary client compares two runtime lengths with `sameNat`.
In the branch that matches `Just Refl`, it can substitute one package's
vector for the other's. Job 1763 compiled this client against the
component under test with the provider source hidden and interface
pragmas omitted. Native runs at `-O0` and `-O2` returned `Just "b"`
for equal lengths and `Nothing` for unequal lengths with undefined
payloads. GHC-83865 points to `localCells source` when the client
omits the equality check.
[Client and expected diagnostic](roadmap/R04-dependent-sums.md#replace-a-vector-after-checking-its-runtime-length).

Job 1756 built both compiler stages. All 18 qualification groups passed,
including all 572 Showcase observations at `-O0`, default `-O2` and
GHCi. The normal compiler passed all 24 selected registrations and all
234 compatibility cases. The focused suite retained its existing
`MTrans` failure (137/138). Job 1837 passed the replacement check on
the same compiler. This checkpoint stayed private after job 1818 found
that upgrading an unchanged length worker from 571 required a manual
rebuild. The fix must pass on the next combined compiler.

## Local row callbacks and composed dependent pair payloads

The Showcase uses a local lambda to map error handling over a row.
It also stores an optional associated payload in a dependent pair.
A combined client can apply both features in one function:

```haskell
checkPackets :: forall (e :: Type). Proxy e -> Optional -> Fields '[Either e Optional]
checkPackets _ packet = check (Field packet End)
  where
    check :: forall ys. Fields ys -> Fields (Map (\cell -> Either e cell) ys)
    check End = End
    check (Field x xs) = Field (Right x) (check xs)
```

Here `Optional` is `some (tag :: Bool) | Maybe (FirstBody [Int] tag)`.
On normal compiler `203245e124`, an ordinary module called
`checkPackets` with a flag from `readIORef`. Matching `Right (Pair tag _)`
returned `True` with an undefined payload. The same match returned
`False` for `Nothing` with the opposite flag. Both runs used stripped
provider interfaces and passed Core and STG Lint at `-O0` and `-O2`.
Job 1748 also checked the inferred binding
`checked = checkPackets (Proxy :: Proxy String)` in a module whose
overlapping `First [Int]` instance requires `MissingFormat Int`.
Its associated equations match the earlier instance. GHC-39999 points
to `checked`, even with deferred type errors. An instance that omits
the equations is rejected earlier with GHC-90647. The 569 compiler
rejects the bridge interface at its import with GHC-47808. Its header
requires both scoped lambdas and composed sum domains.
[Row mapper](examples/open-evaluation/TypeLambda.hs),
[payload definitions](examples/packed-vector/ComposedPayloadSum.hs)
and [ordinary payload client](examples/packed-vector/ComposedPayloadSumClient.hs).

The combined two-stage build passed all 16 qualification groups.
All 571 Showcase observations matched at native `-O0`, default
`-O2` and in GHCi. All 22 selected registrations and all 234
compatibility cases passed. The focused suite passed 137 of 138.
The complete `MTrans` diagnostic from `deriving-via-compile` matched
its baseline. The seven polykinds cases recorded under the 566
checkpoint remain outside these suites.

Job 1726 exited 126 before the build script started. Job 1727 used Bash
but stopped when `ghc-pkg` could not read the `array` registration
during parallel package updates. Job 1739 configured the affected
packages serially, completed the same checkout and passed the original
qualification. The receipt is `r04-r17-normal-571-20261123/qualified.pass`.

The new writer also reuses the free names it has already collected from
local interface declarations. In two matched runs with 1,000 generated
Haskell2010 data declarations and 1,000 value definitions, this saved
16.31 MB (0.53%) of compilation allocation on the preceding R04 writer.
The 100-declaration control saved 1.68 MB. Both wrote byte-identical
ordinary interfaces. These are typecheck-only allocation measurements.
Jobs 1722 and 1723 record the measurements and reader checks.

## Associated families as dependent pair payloads

```haskell
instance PayloadFormat Bool where
    type Body Bool False = Int
    type Body Bool True  = Char
    type Packet Bool = some (tag :: Bool) | Body Bool tag
```

On compiler `aba28b367f`, an ordinary client pattern-matches a runtime
tag and uses the payload as an `Int` or a `Char`. Another client obtains
the required class from a GADT pattern. Both can read the stored tag
with an undefined payload. The Showcase prints
`((True,"x"),True,((False,"43"),False))`.
[Provider](examples/packed-vector/AssociatedPayloadSum.hs)
and [ordinary client](examples/packed-vector/AssociatedPayloadSumClient.hs).

The 567 and 568 readers reject a provider, an inferred binding or a
re-export whose interface requires `scoped-sum-domain-v1`. They accept
an unrelated sum. They can open a re-export of an unrelated `Bool` from
a marked provider. A client that uses that binding still has to load
its marked defining module. The current compiler links that client and
prints `True`. With `-fno-code -fwrite-interface`, a client needs only
the provider's interface. It accepts the valid program. A more specific
instance with an unsatisfied premise produces GHC-39999 at the written
signature or inferred binding. Jobs 1703 and 1716 cover the component
and normal compiler respectively.
[Formation and reader rules](roadmap/R04-dependent-sums.md#use-an-associated-family-for-the-payload).

Job 1701 built both stages. All 569 Showcase observations matched at
native `-O0`, default `-O2` and in GHCi. All 21 selected registrations
and all 234 compatibility cases passed. The focused suite passed 137 of
138. The `deriving-via-compile` diagnostic for `MTrans` matched its
baseline. The seven polykinds regressions from the 566 checkpoint remain
outside these runs. The installed HaskLib clients and the pinned 566
prefix-upgrade checks passed.

The original driver exited 1. Its `first-class-families` check lacked
the package source fixture and stopped before invoking GHC. Job 1721
restored all 23 fixture files from the preceding qualified build,
verified their checksums, then built the package, ran its tests and ran
all three clients on the same compiler. Job 1716 checked the separate
typecheck-only path. The combined receipt is
`r04-normal-569-20261123/post-review/verified.pass`.

## Type lambdas in record fields and ranked row readers

```haskell
data CheckedRow e (xs :: [Type]) = CheckedRow
  { checkedCells :: Fields (Map (\a -> Either e a) xs) }
```

On normal compiler `c4d702c7f4`, the Showcase reads a successful
Boolean and a missing count from the same row. The result is
`(Right True,Left "missing count")`. GHC also checks lambdas under
arrows in open and associated family instances, and under checked
erased `forall` binders in a synonym or signature.
[Row and record definitions](examples/open-evaluation/TypeLambda.hs)
and [instance example](examples/open-evaluation/SchemaColumns.hs).

Job 1670 built both compiler stages. All 568 observations matched
at native `-O0`, default `-O2` and in GHCi. All 20 selected registrations
passed. Ordinary clients on 568, 567 and 566 read stripped interfaces,
linked and ran at both optimization levels. Template Haskell on all
three readers reified the record and could splice the family instances
with `UnsaturatedTypeFamilies`. On 568 it also spliced the reified field
selector. Versions 566 and 567 reported GHC-55017 at that splice.

The prefix-upgrade check stopped before compilation because the driver
compared the pinned 566 reader to the 567 build donor. Job 1698 ran the
upgrade and reader matrix with the intended 566 and 568 binaries. All
ten outcomes matched the preceding qualification. Job 1694 ran the
amended `NativeTypeLambda` test on the 568 compiler. Changing the field
callback recompiled its ordinary client and reported GHC-25897 at the
selector. [Regression](../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh).

The compatibility suite passed 234 of 234 registrations, including
three GHCi cases. The focused suite passed 137 of 138. The complete
`deriving-via-compile` diagnostic for `MTrans` matched its previous
failure. The seven older polykinds regressions documented under the
566 checkpoint remain outside these suites. Job 1670 exited 1 for the
comparison-pin error. The combined record for jobs 1670, 1694 and 1698
is in `r17-normal-568-20261123/review/qualified.pass`.

## Size-bounded list prefixes in HaskLib

Compiler `687b465aad` checks the three source equations of the ordinary
list function `takeSize`. The proof in a separate module recurses on the
smaller bound and list tail. An installed client can import it from
`HaskLib`. [Proof](hasklib/HaskLib/List.hs) and [producer](hasklib/HaskLib/List/Prefix.hs).

```haskell
lengthTakeSize :: forall a. foreach (n :: Size) -> foreach (xs :: [a])
               -> size (takeSize n xs) :~: minSize n (size xs)
```

Job 1650 built both stages. All 567 Showcase observations matched at
native `-O0`, default `-O2` and in GHCi. All 18 selected registrations
passed. Cabal built and tested the source distribution offline. Two
independent clients used the installed package. The demand checks cover
a zero bound with an undefined list and a finite prefix of an infinite
list. Even for an empty list the producer must inspect the bound first.
[Package checks](hasklib-test/Main.hs).

All 234 compatibility cases passed (231 normal test registrations and
three GHCi registrations). The focused suite passed 137 of 138. The
remaining `deriving-via-compile` diagnostic for `MTrans` matched the
saved baseline byte for byte. The seven polykinds cases listed under
the preceding checkpoint remain outside these suites.

With a 567 provider and reader, a separate optimized client proved the
zero-prefix equation and rejected the false successor result at `Refl`
(GHC-83865). The 566 reader reported GHC-83605 for both direct claims at
default `-O2`. With rewrite rules disabled it accepted the true claim
and rejected the false one. A 567 reader of a 566 provider reported
GHC-70565 because the older interface lacked the checked equations.
Upgrading recompiled the unchanged provider once and preserved the
unrelated interface. The next warm build reused both.

## Lexical type lets and cases in closed family equations

Normal compiler `b6ddbb4ce8` preserves the result of inserting an
intermediate type-level name, including when a local binding shadows
the kind of another parameter. A source-hidden client can reuse that
parameter at its original kind. A closed family can also use an earlier
case result in a later branch. The client selected widths `2`, `3` and
`7`. The two unknown-input proofs failed at `Refl`. A 564 reader could
use the width family through the new provider interface.
[R18 release check](roadmap/R18-type-level-lets.md#release-check) and
[R19 case example](roadmap/R19-type-level-cases.md#a-case-in-a-closed-type-family-equation).

Job 1627 built both stages and matched all 566 Showcase observations
at native `-O0`, default `-O2` and in GHCi. All 17 initial registrations
passed. These included `NativeTypeLet`, `NativeTypeLetQuote` and
`TypeLetPrint`. The package, three source-hidden clients and arithmetic
interfaces passed. All 234 compatibility registrations passed. Of 137
focused tests, 136 passed. The unchanged `MTrans` diagnostic accounted
for the raw focused driver exit of 1.

The original aggregate job also exited 1. Three Haskell2010 negative
fixtures needed `KindSignatures` to reach the intended equality check.
The first targeted replay counted additional helper diagnostics as
source errors. Job 1649 checked the four original source locations
and passed all 42 source-case checks at `-O0` and `-O2`. Both replays
used the sealed 566 compiler. Job 1644 separately compiled the exact
User's Guide example and checked both results from an ordinary client.

`Down` is a decreasing recursive case that compiles without
`UndecidableInstances`. Some deeper cases still fail the size check
on a generated helper even when the written equations pass. R19
remains partial. R18 meets the upstream scope for local definitions
with ordered, nonrecursive bindings on this fork.
[Compiler implementation and tests](roadmap/R19-type-level-cases.md#a-case-in-a-closed-type-family-equation).

## Built-in arithmetic callbacks and type-let case results

The normal compiler at `049ac2c031` accepts
`Map ((+) 1) '[2, 3, 4] :~: '[3, 4, 5]` with `Refl`.
Job 1608 checked addition, comparison and string prefixes at `-O0`
and `-O2`. Ordinary clients imported both direct and synonym-wrapped
applications without the provider source. A 562 reader rejected a
partial built-in with GHC-47808. Both compilers could read a provider
that used only saturated built-ins. The arithmetic example in the
User's Guide also compiled under Haskell2010 with its written pragmas.
[Arithmetic example](roadmap/R23-unsaturated-type-families.md#passing-built-in-arithmetic-to-map)
and [registered test](../../testsuite/tests/dependent/should_run/NativeBuiltinTypeFunctions.hs).

A wildcard branch in a type-level `let` can now return a width
computed by an earlier case. An explicit override can determine the
result even when the input to the earlier case is unknown. The
ordinary client proves the override and rejects a guessed default.
The same registration checks simple type-level abbreviations in six
source contexts with an abstract parameter.
[Provider and client](roadmap/R18-type-level-lets.md#using-a-previous-case-result-as-a-default).

Job 1608 built both compiler stages. The 564-line Showcase matched at
native `-O0`, default `-O2` and in GHCi. All 17 initial registrations
passed. The migrated `first-class-families` package passed its suite
and its three clients.

The same job then passed all 234 compatibility registrations, including
three GHCi runs. Of 137 focused tests, 136 passed. Stock GHC accepts
the remaining `deriving-via-compile` case. The fork produced the same
`MTrans` diagnostic as 562. The focused driver exited with status 1.
[Transformer kind](roadmap/R23-unsaturated-type-families.md#a-transformer-kind-synonym).

A separate stock-GHC comparison confirmed seven known matchability
failures outside both selections. [Seven unchanged polykinds tests](roadmap/R23-unsaturated-type-families.md#unchanged-source-on-stock-ghc-and-the-fork).

## Standalone unsaturated type families

A module can enable `UnsaturatedTypeFamilies` to pass a type family
to a higher-order type synonym. An ordinary Haskell2010 client imports
the inferred synonym `Apply` and the provider's `FamilyResult`.
It uses `Apply Maybe Char` and `FamilyResult` in the same signature.
The Showcase evaluates the call to `(Just 'q',[False,True])`.
[Provider](examples/open-evaluation/StandaloneCallback.hs) and
[client](examples/open-evaluation/StandaloneCallbackClient.hs).

Job 1582 built both stages from `f030369687`. All 562 Showcase lines
matched at native `-O0`, default `-O2` and in GHCi. All 17 focused
registrations passed. They include the R18 constructor diagnostic
checks for the written name, its source span and the number of fields.
The same compiler built the migrated `first-class-families` library,
passed its test suite and checked three clients with the provider
source hidden.

Template Haskell reported the standalone flag through both interpreters.
Changing the flag recompiled the provider. A build with the same flags
reused its interface. The previous reader could read the new provider.
The standalone flag implies `TypeFamilies`. The umbrella retains its
existing behavior, including local generalisation in Haskell2010
modules. [Flag and interface tests](../../testsuite/tests/dependent/should_run/NativeStandaloneUnsaturation.sh)
and [R23 scope](roadmap/R23-unsaturated-type-families.md#release-scope-against-proposal-242).

Job 1607 used the same compiler. All 234 compatibility registrations
passed, with 231 in the normal way and three in GHCi. Of 137 distinct
focused registrations, 136 passed. The upstream `deriving-via-compile`
test still failed on the fork. Its `MTrans` diagnostic matched 561
byte for byte. Stock GHC 9.14.1 accepts that source.
[Transformer kind and migration](roadmap/R23-unsaturated-type-families.md#a-transformer-kind-synonym).

## Constructor patterns in type-level lets

A source type can unpack a regular, single-constructor product in a
`let` binding. For example, `ProductDimensionSum` selects two fields
of `Dimensions` and adds them. Applying it to
`'Dimensions 2 3 UnknownDepth` gives `5` while the final field remains
unknown. An ordinary client can use the equality from the interface.
Every written field counts toward the constructor arity, including
wildcards. The checked implementation supports flat named-constructor
patterns. Type quotations reject them at the constructor because
Template Haskell's `LetT` cannot retain the constructor check.
[Source](examples/open-evaluation/TypeLet.hs) and
[R18 scope and provenance](roadmap/R18-type-level-lets.md#a-named-product-binding).

Job 1572 built both stages from `9daa85cf02`. All 561 Showcase
lines matched at native `-O0`, default `-O2` and in GHCi. Removing
the new dimension-sum line reproduces the previous golden output.
The focused run passed 130 of 131 registrations, including
`NativeTypeLet`, `NativeTypeLetQuote`, `NativeSynonymReify` and
`TypeLetPrint`. The last test checks exact printing and parsing.
The unchanged `deriving-via-compile` failure matched the previous
compiler byte for byte. All 234 compatibility registrations passed.
The 34 specialization cases, package identity tests, GHC API
producer-change check and guide build passed. The migrated
`first-class-families` package built all 17 modules, passed its
test suite and ran its three source-hidden clients.

## Inferred matchability in type synonyms

With `ExperimentalDependentTypes`, the inferred synonym
`type Apply f a = f a` accepts both `Apply Maybe Bool` and
`Apply Identity Bool` when `Identity` is a type family. A separate
ordinary client can use the exported synonym with `Maybe`. A package
can also pass the promoted constructor `'Endo` to `Pure1` without
explicit matchability arguments.
[Source and ordinary client](examples/open-evaluation/LaterMatchabilityClient.hs)
and [package migration](roadmap/R23-unsaturated-type-families.md#one-constructor-for-term-and-promoted-callbacks).

Job 1533 built both normal stages from `5d6ac951f6`. Job 1534
matched all 560 Showcase lines at native `-O0`, default `-O2` and in
GHCi. It compiled all 17 modules of the migrated
`first-class-families-0.8.2.0`, passed its test suite and ran three
interface-only clients. Their results were `True`, `True` and `Refl`.
The 34 specialization checks, package identity checks, GHC API
producer-change check and guide build passed.

The first compatibility run passed 233 of 234 registrations.
`DependentArrowMatchability` still expected an inferred synonym to reject
an abstract family with the extension enabled. Job 1568 ran the corrected
registration on the same compiler. It proved the result with the extension
on and checked the original rejection with the extension off. Together
the runs cover all 234 registrations. Of 127 focused registrations, 126
passed. The full `deriving-via-compile` failure matched 558 byte for byte.
The additional `NativeSynonymReify` registration passed in job 1561,
bringing the focused total to 127 of 128.

A plain Template Haskell copy of an inferred synonym can lose binder
specificity. Copying its kind with `reifyType` and emitting a standalone
kind signature preserves it. Jobs 1554 and 1558 compared both forms on
the normal compiler. The exact copy accepted a constructor and a family
and rejected explicit application of the inferred index.
[Registered regression](../../testsuite/tests/dependent/should_run/NativeSynonymReify.sh).

## Runtime payload selection through an associated family

The ordinary client in this example reads a Boolean from an `IORef`
and constructs `Pair flag (make flag)`. The `True` payload contains
text and the `False` payload contains an integer. The client can also
read the tag when the payload is `error "unused message payload"`.
The package type comes from an associated family. One call uses an
instance in scope. Another gets its dictionary by matching `Permit`.
[Provider](examples/packed-vector/RuntimeMessage.hs) and
[ordinary client](examples/packed-vector/RuntimeMessageClient.hs).

The same compiler checks a local function used by an associated type
equation in either source order. It derives `Show` for the result datatype
with the regular instances in scope. A separate client uses the exposed
equation and the derived instance.
[Example](examples/open-evaluation/LocalDerivedAssociated.hs) and
[deriving checks](roadmap/R21-dependency-analysis.md#deriving-and-associated-equations).

Job 1493 built both compiler stages at `3dd34e5327`. Its compiler,
libraries, tests and Showcase match implementation `b963ccfaad`.
Job 1494 passed the 558-line Showcase at native `-O0`, default `-O2`
and in GHCi. It also passed all 234 compatibility registrations, 34
specialization checks, both package identity checks, the GHC API
producer-change check and the guide build.

The original focus list included a fixture, `NativePackageOperand.hs`,
under its own name as well as under its registered test,
`NativeRetainedPackage`. Job 1526 ran all 126 distinct registrations.
125 passed, including the retained-package tests. The unchanged upstream
`deriving-via-compile` registration failed at the type synonym `MTrans`.
Job 1531 checked that source on stock 9.14.1, public 556 and
normal 558. Stock passed. Both fork compilers reported byte-identical
errors. Both compiled the explicit matchability migration.
[R23 reproducer](roadmap/R23-unsaturated-type-families.md#a-transformer-kind-synonym).

## Checked character predicates in the defining module

Implementation `1b5c5a9676` accepts `type Flag (c :: Char) = flag c`
in the declaration group that defines the ordinary function `flag`.
Either source order works. A separate module can import the synonym
and prove `Flag 'x' :~: 'True` with `Refl`. The Showcase checks two
literal branches and the default. It prints `[True,True,False]`.
[Predicate](examples/open-evaluation/CharacterCase.hs),
[exported synonym](examples/open-evaluation/CharacterExport.hs) and
[declaration order](roadmap/R21-dependency-analysis.md#checked-character-equations-in-their-defining-module).

Job 1454 built both normal stages from `54c4c62287`. Job 1455
matched all 556 Showcase lines at native `-O0`, default `-O2` and in
GHCi. It passed 111 focused registrations and 234 compatibility
registrations (231 native and three GHCi). The specialization and package
identity checks passed. The separate GHC API example recovered nominal
equality evidence from ordinary interfaces. Changing its producer changed
the result seen by the unchanged client.

## Installed Unicode case mappings

The 555-line release is `16de21824a`. A type synonym can name
`Data.Char.toTitle c` or `Data.Char.toLower c` while `c` is unknown.
The installed worker supplies checked literal equations and an identity
default. A client with only interfaces can prove the specialized result.
For U+01C6, title case is U+01C5 and upper case is U+01C4.
[Exported synonyms](examples/open-evaluation/UnicodeExport.hs) and
[Showcase](Showcase.hs).

Job 1397 rebuilt Hadrian and both normal compiler stages at
`1659c10b23`. It checked the installed interfaces of the three workers
and their three public forwarding bindings. Job 1398 passed the
555-line Showcase at native `-O0`, default `-O2` and in GHCi,
34 specialization cases and both package-identity checks.

The initial runs passed 108 of 109 focused registrations and 232
of 234 compatibility cases. The private interface setup had recorded
a loader error for `NativeOpenRetryQuote`. The normal compiler
reports GHC-70565 at the executable call inside the quotation.
`NativeClosedLazyParcelKind` now uses the executable `isSpace` to
check a rejected `Parcel` operand. Its old `toTitle` call reaches the
ordinary kind checker through the new equations. Job 1451 passed both
corrected registrations on the same normal compiler. Together the
runs cover all 109 focused and 234 compatibility registrations.

Job 1384 changed one worker equation and invalidated an unchanged
interface-only proof. Job 1390 changed only the object of an
executable wrapper and invalidated its staged call. These component
checks cover both dependency paths.

## Imported function identity in retained indices

The 554-line release is `ce1c89c534`. An index can refer to a
qualified imported function by its defining name, including its package.
The Showcase uses this to classify runtime characters. Its ordinary
client prints `[True,False,True]` for a space, an `x` and a tab.
[Source](examples/open-evaluation/CharacterDecision.hs) and
[specialization behavior](roadmap/R03-dependent-products.md#specializing-an-imported-call).

Job 1371 built both normal stages at `9202568aae`. Job 1377 compared
all 554 outputs at native `-O0`, default `-O2` and in GHCi. The 95
focused registrations passed. The wider compatibility run passed all
231 normal cases and all three GHCi cases. It reported zero missing
libraries, framework errors or unexpected results.

The same job checked 34 specialization outcomes at `-O0` and `-O2`,
the older interface readers, and two installed packages with the same
module and function names. Job 1381 ran the two maintained callback
registrations on the normal compiler. The nominal callback test also
passed with each interpreter capability disabled in the test driver.
The public compiler differs from the normal build only in comments
in `GHC.Tc.Gen.HsType`. Its final fixtures match `10f627f3a7`.

## Ordered GADT signatures and required type arguments

Merge `61fd13f735` contains the compiler and libraries from the
two-stage build of `71e5215115` (job 1285). Job 1286 matched all 553
Showcase outputs in three modes. It passed the constructor order and
refinement matrices, interface readers and flag checks. Of the 84 focused
registrations, 83 passed on the first run. Job 1319 passed the final
`NativeDependentCoreRun` with the same compiler. Its ten scoping examples
now run both with the implied extension and with an explicit override.
The older implicit-quantification ABI check uses the override. The two
additional TH printer registrations passed on the normal compiler. The
552 compiler rejected the same quoted constructor with GHC-65904.

The compatibility run selected another 234 registrations, including
seven interpreter tests. The first pass recorded 221 successes.
Four printer tests passed with their build helpers present. Job 1322
passed all four migrated fixtures. Explicit type binders preserve the
meaning of `id`, `pick` and an expression annotation. The strict field
projection still rejects with either scoping rule. The five inherited
failures are `NativeFamilyIndexEvidence`, `NativeIrredIndexEvidence`,
`NativeRetainedPromotedIface`, `NativeTypeCaseQuote` and
`NativeTypeNumeralWrong`. Each matched the prior compiler. Three separate
duplicate-root framework reports did not correspond to test executions.

Job 1349 replaced the obsolete expected failure for `NativeTypeCaseQuote`
with a proof that the spliced case selects `Int`. A `Bool` result
still fails at `Refl`. Job 1353 passed the revised expectations for
`NativeIrredIndexEvidence`, `NativeRetainedPromotedIface` and
`NativeTypeNumeralWrong`. The controls preserve rejection of distinct
values, malformed interfaces and the wrong successor addition.
`NativeFamilyIndexEvidence` has a separate compiler fix.

The User's Guide resolved all six new links and the extension implication.
It has 28 warnings and three errors, compared with 29 warnings and the
same three errors on the published comparison. The corrected heading
removed one warning. Qualification receipts are under
`r41-required-normal-553-20261123`, `r41-th-printers-20261123` and
`r41-required-compatibility-20261123` in `_build/on-demand`.
[Card and example](roadmap/R41-nested-quantification-in-gadts.md#release-check).

## Nested case decisions and representation-polymorphic newtypes

Job 1087 built both stages of `00115859a5` from clean sources. Merge
`f42ebe4659` contains the tested compiler, tests and Showcase. Job 1101
matched all 552 outputs at native `-O0`, native `-O2` and in GHCi.
All 40 selected normal-way registrations passed.

A provider takes a runtime `Maybe Bool` and returns a `Char`, `Int`
or `Bool`. An independently compiled consumer writes the same nested
type-level case in a different clause order. For three values read
from IO the ordinary client printed `["n","42","False"]`. Cases that
inspect different fields first still reject at an unknown index.
[Provider](examples/open-evaluation/NativeCase.hs),
[consumer](examples/open-evaluation/NativeRetainedTH.hs)
and [registered controls](../../testsuite/tests/dependent/should_run/NativeTypeCaseOrderedScope.sh).

One interface contained the ordered case proof and a representation-
polymorphic ordered newtype. Each earlier component reader loaded
its own control and rejected the combined interface. The normal
client printed `(True,42)`. The new compiler also passed the
higher-rank alias checks and three separate `first-class-families`
package builds. See `r19-r41-next-20261122/qualification` for
outputs, linker maps and hashes.

A cleanup of the earlier 547 compiler removed 14,242 intermediate
files and freed 2,326,372,352 allocated bytes. Job 1126 checked the
installed compiler hashes and used both stages to build and run a
native program.

## Field-dependent case results, ordered newtypes and CUSK migrations

Jobs 1035 and 1041 built and tested `fd19c8fe5f`. Merge `6272edb866`
contains the same compiler, examples and tests. The Stage 2 Showcase
matched all 551 lines at native `-O0`, native `-O2` and in GHCi. All
36 selected normal-way registrations passed.

Two modules give a retained `Maybe Bool` separate case expressions.
In the `Just x` branch the first returns `Proxy x` and the second
accepts it. They compose for input read from IO, even if the field is
bottom. A mixed interface contained the new case proof and an ordered
newtype. Both earlier component readers reported the unsupported
features. The integrated client printed `(42,True,(False,Refl))`.
[Showcase](Showcase.hs) and [R19 case](roadmap/R19-type-level-cases.md).

The integrated compiler also passed the stripped-interface newtype,
unknown-levity and Template Haskell checks. In the migrated
`first-class-families` package, `-Wcusks` issued 15 diagnostics.
Five contained source edits. Applying all five and disabling CUSKs
built the package and ran its importing client. The default build
was quiet. The edition change remains a private experiment.
[R41 tests](roadmap/R41-nested-quantification-in-gadts.md#boxed-results-and-fixed-representation-newtypes)
and [R27 package study](roadmap/R27-deprecate-cusks.md#the-release-chain-on-the-normal-fork).

## Promoted callbacks and ordered data family constructors

Normal job 988 built both compiler stages at `5833e38cbd`. The
compiler in merge `ad644c290f` matches that source. Job 1006 passed
all 30 named normal-way registrations. The 550 Showcase lines
matched at native `-O0`, native `-O2` and in stage 2 GHCi.

Two pairs of stripped-interface clients ran at both optimization
levels. They used ordered constructors in a lifted data family, a
boxed unlifted datatype and a boxed unlifted data family. The
previous reader at `2b64ffc406` loaded those interfaces. The tests
checked strict and lazy fields, a result equality, an out-of-scope
dictionary and an invalid family result.

The migrated `first-class-families-0.8.2.0` package passed its own
test and three clients that had access only to the compiled library.
The promoted client printed `True`, the runtime lambda printed `True`,
and the documented `FoldrDefault_` equality printed `Refl`. The
compiler's private matchability test matrix also passed on this
normal build at `-O0` and `-O2`.
[R23 migration and remaining annotation](roadmap/R23-unsaturated-type-families.md#one-constructor-for-term-and-promoted-callbacks)
and [R41 constructor coverage](roadmap/R41-nested-quantification-in-gadts.md#gadt-result-refinements).

The build reused the completed R36 experiment's physical tree. A
separate cleanup removed 14,242 intermediate files from the earlier
`2d6368dd57` build. It released 2,325,647,360 allocated bytes.
Job 1022 compiled and ran a program with both retained compiler stages.

## Namespace management and ordered GADT results

Job 932 built stages 1 and 2 from `2b64ffc406`. The compiler sources
in the integration commit `930910d25b` match that revision. The two
new local completions are R36 and R35. Serokell still marks 23 cards
complete upstream.

| Check | Result |
| --- | --- |
| Showcase | All 549 lines matched at native `-O0`, native `-O2` and in stage 2 GHCi. |
| GADTs, Core, interfaces and related regressions | All 23 selected normal-way registrations passed. The mixed interface loaded in the new compiler. Two older compilers each reported the incompatible features before decoding. The new client printed `(True,(False,Refl))`. |
| Full renamer directory | 508 registrations. The 499 executed cases gave 488 expected passes, three expected failures and eight unexpected failures inherited from the baseline. All eight diagnostic bodies matched. Eight original `T23740` goldens recovered. All 16 R36 tests passed. |
| Namespace management | Job 957 passed all 25 registered normal-way tests on the same compiler. The new regression checked separate compilation, required arguments, distinct Template Haskell annotations and the exact ambiguous-use diagnostic. |

Job 951's last postprocessing step compared unified diffs. It expected
the test scratch directory under `/tmp`. This run used a temporary
directory under the build tree. The final check normalized the file
headers and timestamps and compared every remaining line, including
the compiler's source positions. Keep the original exit code and the
verified comparison under `r41-r36-aggregate-normal-20261122/qualification`.
The older R36 warning build and ten superseded overlays released
5,308,047,360 allocated bytes. The current compiler and comparison
manifests remain in the active build tree.
[R35 controls](roadmap/R35-namespace-management.md),
[R36 scope](roadmap/R36-pun-warnings.md) and
[R41 orderings](roadmap/R41-nested-quantification-in-gadts.md#which-prefix-orders-work).

## Local build capacity

On the shared WSL host, schedule GHC builds and compiler verification
in the `ghc` group on the usual Pueue daemon. The group has four slots
across all worktrees. Its jobs share sixteen logical CPUs and a memory
limit of 64,000,000,000 bytes in `ghcbuild.slice`. For four concurrent
Hadrian builds, use up to four workers per build. A private compiler link
or a separate test job also uses a slot. Keep the combined job reservations
within the slice limit. Four 16 GiB reservations exceed it.

Runnable work in other groups has priority. The local priority watcher
pauses the GHC queue and freezes its running processes until that work
finishes. A frozen job keeps its allocated memory. After a daemon or
host restart, verify the slice limits and priority watcher before
resuming builds.

The testsuite timeout continues to count elapsed time while the slice is
frozen. The first run on `bb45f2d668` reported 25 passes and two 300-second
timeouts across a priority pause. The affected tests were
`NativeIndexedFin` and `NativeImplicitExistential`. Keep the first summary.
Rerun the timed-out tests on the same compiler and allow for the pause in
the wall-time limit. Record the rerun separately.

## Restore the shared WSL build queue

Use `/home/tritlo/Code/ghc`. Keep source changes in Git and put current
builds and receipts under `_build/on-demand/live-recovery/`. The WSL restart
erased `/tmp`. A Git worktree registration or a saved Pueue task can survive
after its checkout or process has gone.

1. Run `git status -sb`, `git worktree list --porcelain`, and
   `git ls-remote fork refs/heads/dependent-haskell`. Compare the revisions
   with the [handoff](HANDOFF.md). Run `env -u XDG_RUNTIME_DIR pueue status` to check
   the shared daemon. Inspect the specific job and its checkout before
   retrying it. Keep the queue paused while restoring resource limits.
2. Set the user bus variables in the shell that controls systemd. If
   `/run/user/$(id -u)/bus` is missing, start the user manager from a WSL
   terminal with `sudo systemctl start user@$(id -u).service`.

   ```sh
   export XDG_RUNTIME_DIR=/run/user/$(id -u)
   export DBUS_SESSION_BUS_ADDRESS=unix:path=$XDG_RUNTIME_DIR/bus
   ```

3. If the shared daemon is down, start it on its existing socket.
   Pueue clients on this host use `env -u XDG_RUNTIME_DIR pueue`.
   This preserves the queue shared with local-model jobs.

   ```sh
   systemd-run --user --unit=shared-pueued --property=Restart=on-failure \
       /usr/bin/env -u XDG_RUNTIME_DIR "$HOME/.nix-profile/bin/pueued"
   ```

4. Restore the GHC slice and its priority watcher. Run these commands from
   the repository root. Start the watcher only if its service is inactive.
   Its state marker lives in the user runtime directory. Inspect any queued
   GHC commands from the previous boot before starting the group.

   ```sh
   systemctl --user set-property ghcbuild.slice \
       CPUQuota=1600% AllowedCPUs=0-15 MemoryMax=64000000000 MemorySwapMax=0
   systemctl --user start ghcbuild.slice
   systemd-run --user --unit=ghc-build-priority --property=Restart=on-failure \
       --setenv="PATH=$PATH" --setenv="XDG_RUNTIME_DIR=$XDG_RUNTIME_DIR" \
       --setenv="DBUS_SESSION_BUS_ADDRESS=$DBUS_SESSION_BUS_ADDRESS" \
       /usr/bin/python3 "$PWD/docs/dependent-haskell/tools/ghc-build-priority.py"
   systemctl --user show ghcbuild.slice -p CPUQuotaPerSecUSec -p MemoryMax -p MemorySwapMax
   env -u XDG_RUNTIME_DIR pueue parallel 4 --group ghc
   env -u XDG_RUNTIME_DIR pueue group
   ```

   The expected values are `16s`, `64000000000` and `0` respectively.
   The `ghc` group has four slots. A runnable local benchmark takes priority. The watcher
   pauses GHC admissions and freezes the GHC slice until that work finishes.
   When another job has priority, leave GHC paused. Use
   `env -u XDG_RUNTIME_DIR pueue start --group ghc` when ready to resume.
5. Submit the build with [ghc-build-job](tools/ghc-build-job). It runs each
   job in the shared slice and records peak memory with
   `~/Code/heavy-management/bin/capped-record`. Inspect the job result and
   the compiler revision in its receipt.

## Clean up after each experiment

Use Git to recover finished experiments. Before cleanup, commit the source
changes and any fixture needed to repeat the result. Record the revision,
build and test commands, observed result, and relevant failure diagnostic.

Run one-off builds and test links in temporary directories. Keep an
experiment directory only while a running or queued job uses it. When the
last job finishes, remove the directory, including its compiler, objects,
generated interfaces, executables, link archives and raw logs. Check for
shared inputs first. A thin archive refers to the original objects, so keep
those objects until its last consumer has finished linking.

The local retention timer checks allocated bytes under the main `_build`
every 15 minutes. At 200 GB (200,000,000,000 bytes), it collects all eligible
products it found under `_build/verification`. It finishes that list even if
usage falls below 200 GB during the pass. It recognizes completed native API
test drivers and uses the existing expiry rule for old native test programs.
It also removes a testsuite's `tmp/ghctest-*` directory when the test names
and timestamps match a successful summary and JUnit report. It keeps both
reports and checks for live, queued and linked users before removal.
The timer can also remove the compiled `ghc-config` helper in a completed
test run. It checks the reports, the helper's source and the Makefile rule.
The next run rebuilds the helper from that source.
For a normal two-stage build, the timer can reclaim the RTS C objects and
dependency files after both compilers pass the same Showcase at the recorded
Git revision. The normal build script can regenerate those files.
When several experiments use each other's compilers, check their libraries,
linker response files and thin archives together. Keep the inputs of any
running or queued job. Record the configured compiler revision as well as
the checkout revision when the checkout has advanced. A sparse historical
checkout restores its tracked tests with `git sparse-checkout disable`.

For other products, keep the completion log outside `_build`. After the last
consumer finishes, record the source revision and build command.

```sh
ghc-build-retention --build-root "$PWD/_build" retire "$product" \
  --source "$revision" --completed-evidence "$completion_log" --rebuild "$rebuild_command"
```

Run `ghc-build-retention --build-root "$PWD/_build" withdraw "$product"`
before using that product again. The collector checks active and queued work,
reader references, and file identities before removal. Local receipts are in
`~/.local/state/ghc-build-retention/`.

An interface-upgrade test can need an older compiler. Record which test
uses it, its revision, and the matching libraries or objects that the test
reads. Release those build products when the last consumer finishes.
Rebuild the pinned revision when that comparison is needed again.
[Append upgrade test](examples/open-evaluation/check-append-upgrade.sh)
and [reverse upgrade test](examples/open-evaluation/check-reverse-upgrade.sh).

At consolidation, identify the compiler and package database used by the
next work. Check their library paths and the inputs of running and queued
jobs. Retire superseded Hadrian stages and loose compiler outputs together.
Two finished experiments can still point at each other's files through a
linker response file, package configuration or symlink. Record their
source revisions and rebuild commands, then remove both sets of outputs
after the last live user finishes. The 200 GB threshold is a backup for
this cleanup.

## Compose type-level cases on an unknown `Maybe`

Normal compiler `4e169241d2` and merge `098ca0ae97` have the same
compiler, tests and 549-line Showcase. The provider chooses `Char`
for `Nothing` and `Int` for `Just _`. An independent consumer writes
the rows in the opposite order. An ordinary module composes the two
at an unknown index. The Showcase reads `Nothing` and then
`Just bottomB` from an IORef and prints `("n","73")`.

Job 904 built both stages. Job 927 checked four registered tests,
native `-O0` and `-O2`, and GHCi. The source-hidden client rejected a
different index or changed result. An older reader could still use
the new providers at known indices. It rejected an interface that
stored the new coercion. The new compiler replayed that proof under
Core Lint after substituting `Just False`.

The same compiler restored the ordinary term-capture diagnostic when
dependent types are off. Job 928 passed all ten original tests from
`T23740a` through `T23740j`. The separate unchanged upstream suite
matched the previous compiler on all 158 registrations, including
all 15 failure reports.
[Provider](examples/open-evaluation/NativeCase.hs),
[consumer](examples/open-evaluation/NativeRetainedTH.hs)
and [registered test](../../testsuite/tests/dependent/should_run/NativeTypeCaseRetainedTh.sh).

## A named measurement and an inferred vector length

The compiler at `4f2fc1146f` and merge `229cecd250` have identical
compiler sources, tests and Showcase. A client can pass `HaskLib.size`
to a proof as a retained callback. The same function measures a list
read from IO and occurs in the result type. A separately compiled
client rejects a different measurement at `Refl`. Core Lint checks
the function's source equations when it uses them to form the index.
An ordinary constructor field can still store a function by identity.

The Showcase also uses a proof from `sameNat count (S Z)` to call
`safeHead (replicateNative count 'x')` without explicit type
arguments. An equality to zero cannot satisfy the call. HaskLib's
source distribution built offline, and its list laws passed. Clients
that could only see the installed interfaces passed at `-O0` and
`-O2`. Jobs 855 and 865 verified the build and package.

Jobs 864 and 871 matched all 548 Showcase outputs in GHCi, native
`-O0` and native `-O2`. Native `-O2` passed with both stripped and
ordinary interfaces. Ten R19/R36 registrations passed. In the separate
41-test selection, 40 passed. The sole failure, `T15874`, produced
the same diagnostic as the preceding compiler. Both the raw failing
test and its diagnostic are retained in the job 868 results. The
unchanged upstream compatibility comparison below also ran on this
compiler. All 158 outcomes and all 15 failure reports matched the
preceding comparison.
[Named callback](roadmap/R03-dependent-products.md#pass-a-named-function-with-checked-equations),
[scoped successor](roadmap/R11-homogeneous-equality.md#infer-the-checked-successor-tail)
and [Showcase](Showcase.hs).

## Compose independent type-level cases

The published R19 compiler compares the equations of two generated
cases on the same enumeration. Each constructor must occur once and
select the same named result type in both cases. The order of independent
rows may differ. The client can then pass the selected value between
modules while its index is unknown. The ordinary function
`renderIndependent :: Bool -> String` in the Showcase calls
`NativeRetainedTH.renderCase flag (NativeCase.retainedPick flag)`.
The Showcase applies it to two Booleans read from IO and prints
`("7","f")`.

The normal two-stage build at `4e0c9b393a` passed the four registered
R19 checks. An older compiler could use the new providers at known
indices. It rejected an interface that stored the new coercion.
Changing one result in one provider invalidated the consumer's proof.
Job 828 then checked the public fixture at `a7088dd69f`, including
the separate Core API regression and the 547-line Showcase.
[R19 test and scope](roadmap/R19-type-level-cases.md#a-case-in-a-value-signature)
and [Showcase source](Showcase.hs).

## Compatibility of the named callback and successor inference changes

The preceding normal compiler at `a395eea233` and the combined normal
compiler at `a43a310c08` ran the same 158 upstream test registrations.
The sources, driver registrations and expected files came from the
`dependent/should_compile`, `should_fail` and `should_run` directories
at upstream commit `19ff36f2aede`. Each compiler ran 157 registrations
in the normal way and `T14729kind` in GHCi. Both passed 143 of 158.
All 15 failures had the same actual diagnostic or output difference
on the two compilers.

| Upstream expectation | Tests that still fail it |
| --- | --- |
| The program compiles | `GADTSingletons`, `LopezJuan`, `Rae31`, `RaeBlogPost`, `RaeJobTalk`, `T12088sg2`, `T12088sg3`, `T12442`, `T13910`, `T13938` |
| Rejection has the recorded diagnostic | `RAE_T32a`, `T13135`, `T14066d` |
| Compiled type declarations print as recorded | `T15743`, `T15743e` |

For example, `T13938` rejects the unannotated kind synonym `~>`
at a declaration that requires a matchable arrow. `T15743` compiles
and prints different names for quantified kind variables. The selected
release regression `T15874` lives in the separate polykinds directory.
[Upstream dependent tests](https://github.com/ghc/ghc/tree/19ff36f2aede0ca2e554016ff22c71e73aa9803b/testsuite/tests/dependent)
and [R23 compatibility](roadmap/R23-unsaturated-type-families.md).

## Equality continuations and structural kind inference

The original GHC test `T13822` returns `Maybe` at a kind computed by
an injective closed type family. The unannotated family can now infer
the matchability of its result arrow. An older home provider rebuilds
once and exposes its new kind to dependent clients. A subsequent
build reuses its interface. An older installed provider needs rebuilding.
Clean two-stage job `712` checked both the ordinary program and the
retained caller at `9f33902199`. [Source and compatibility conditions](roadmap/R23-unsaturated-type-families.md).

The type checker also accepts `gcastWith proof Refl` under a retained
binder. It can use the supplied argument and the expected result type
to instantiate the equality eliminator before checking its continuation.
The continuation still receives only its declared equality. A proof
about a second, independent list fails at `Refl` with GHC-05617.
[Maintained source test](../../testsuite/tests/dependent/should_run/NativeRetainedFamilyGiven.hs)
and [type inference and Agda references](REFERENCES.md#checking-a-retained-equality-continuation).

Job `733` built both combined compiler stages at `a395eea233`.
Job `738` checked fixtures at `f73e659381`. All four native Showcase
runs and stage 2 GHCi matched 546 lines. HaskLib's unpacked source
distribution, interface-only clients, nominal equality controls and
provider-upgrade tests passed. Of 40 registered normal-way tests,
39 passed. The upstream test `T15874` failed at its data-family
constructor. Job `750` reproduced the same GHC-45219 diagnostic with
the preceding normal compiler, the R23-only development compiler and
the combined normal compiler. The return-kind synonym fixes its
invisible forall to U while the data family requires M.
[Original unannotated source](../../testsuite/tests/polykinds/T15874.hs).

## Optimized zippers and scoped callback equalities

HaskLib commit `6765da7957` composes the two list proofs with base's
`trans`. The `congFirst` lemma rewrites the first argument of a
binary size function. The client supplies `minSize` and the fixed
right size. The same lemma works with `plus`. Job `692` installed the
consolidated source distribution offline. Clients at `-O0` and
`-O2` passed with only the public interface. The compiler rejected
both missing premises and a false conclusion. The demand comparison
confirmed the original proof order when the lists contain distinct
unevaluated errors. [Proof and rationale](README.md#compose-equality-proofs).

Job `668` built both normal compiler stages at `2d6368dd57`. Jobs
`674` and `682` checked fixture `3ecc8d6fc9`. Both stages matched
all 546 Showcase lines at `-O0` and `-O2`. Stage 2 GHCi, the
interface-only clients and all 31 selected normal-way tests passed.
The compiler accepted the installed zipper fusion rules and reported
GHC-83605 for an additional active rule that changed the builder.
The dormant rule and selective compiler-upgrade checks passed.
[Zipper rules](roadmap/R16-open-term-evaluator.md#prove-an-equation-for-preludezipwith).

Jobs `672` and `675` checked a source distribution, its package test
and separate consumers. An ordinary `-O2` caller observed that an
empty left list leaves the right list untouched. A left cons demands
the right spine. The zip proofs leave the callbacks and elements
unevaluated. A dependent client used a nominal witness
`size xs :~: measure xs` to rewrite the result of an imported proof.
Using representational evidence failed with GHC-05617.
[Scoped family controls](../../testsuite/tests/dependent/should_run/NativeRetainedFamilyGiven.hs).

## Compose installed list proofs

HaskLib's `lengthZipMap` proves the shorter-length law for
`zipWith combine (map pre xs) ys` by induction on the two lists.
A consumer can prove the same equation with `lengthZipWith` and
`lengthMap`. Matching the second proof rewrites an argument to
`minSize`. Omitting either equality leaves the final `Refl` unsolved.
Both callbacks and list elements can be undefined. The proofs differ
in their demand on the left spine when the right list ends first.
Job `652` checked this difference at `-O0` and `-O2` using the installed
package with the normal self-hosted compiler. [Worked example](README.md#reuse-a-proof-with-hasklib).

Job `589` built both stages from a clean checkout at `3fceb0b35d`.
This compiler panicked when checking a closed-family equality at a
retained result variable. Job `633` rebuilt both stages at `8d756d44da`
after the solver stopped inferring a family equation from such a result.
Job `643` checked both stages against fixture `193e3f35d0`. Four native
runs and stage 2 GHCi matched the 546-line Showcase. Stage 2 passed
all 31 selected normal-way tests. The package suite, source-hidden
clients at both optimization levels and compiler-upgrade checks passed.
The source at `999f0ed489` has the same Git tree as the tested fixture.
[Closed-family regression](../../testsuite/tests/dependent/should_run/NativeRetainedFamilyGiven.hs).

Job `653` checked the standalone filter example against the normal
self-hosted compiler. It accepts the concrete proof and rejects a bare
`Refl` for the general law. The example includes a runtime `seq`
counterexample at both optimization levels.
[Statement and hypotheses](REFERENCES.md#filter-naturality-in-the-presence-of-seq).

## Runtime callbacks and HaskLib

Job `515` built compiler `c24ffa9ca0` from a clean checkout. Job `551`
checked the 545-line Showcase at `f84b71ab53` on both stages at `-O0`
and `-O2`, and in stage 2 GHCi. All 30 selected normal-way tests passed.
The R04 checks cover a bang on the stored witness with a lazy payload.
The R05 checks cover an absent callback at `-O2` and an unrelated
binding that the optimizer can still float. The zipWith length law
accepts a callback and both lists from IO.
[Runtime-list proofs](examples/open-evaluation/PreludeZipRuntime.hs).

HaskLib exposes these proofs through `HaskLib` and `HaskLib.List`.
Job `566` built and installed the source distribution offline in an
isolated Cabal store. Its `list-laws` suite passed. A separate dependent
module named the reexported functions in its signature. An ordinary
client linked against the installed package at `-O0` and `-O2` and
used all four laws. For the package and compiler paths, see the
[handoff](HANDOFF.md) and [library guide](README.md#reuse-a-proof-with-hasklib).

Job `569` ran the Showcase through HaskLib's public API on both normal
compilers and in GHCi. At `5292956a82`, `HaskLib.Size` exports
`plusZero` and `plusSucc`. Job `576` installed the updated source
distribution, ran its package test and ran the downstream append-singleton
proof at `-O0` and `-O2`. The client uses those two lemmas and
`lengthAppend` without repeating their induction. Job `577` loaded
the new public module and matched all 545 lines in stage 2 native
code and GHCi.
[Arithmetic lemmas](hasklib/HaskLib/Size.hs) and [consumer proof](hasklib-test/Client.hs).

Job `581` installed the source distribution with `lengthReverse` and
ran its package test. A separate module used the theorem with only
`base` and `hasklib` exposed at `-O0` and `-O2`. Forcing the proof
evaluated the list spine and left the elements unevaluated. Job `595`
ran the 545-line Showcase with the public theorem on an IO-supplied list
at both native optimization levels and in stage 2 GHCi.
[Reverse proof](hasklib/HaskLib/List.hs).

## An ordinary minimum in the zip length proof

The [single-file example](examples/open-evaluation/ZipMinimum.hs) uses
ordinary recursive definitions of `size` and `minSize`. It proves
`size (zip xs ys) :~: minSize (size xs) (size ys)` by matching the two
input lists. The [separate-module version](examples/open-evaluation/PreludeZipRuntime.hs)
uses the same equations from the provider interface. The empty-left case
returns `Refl` without inspecting the right list. Adding an extra
`Succ` to the recursive minimum changes its checked equation and GHC
rejects the unchanged induction step with GHC-05617.

The first clean two-stage build used `f984b0369f`. All four native
Showcase runs matched the 543 expected lines. GHCi exposed debugger
breakpoints in the Core inspected for the imported minimum. The checker
now ignores those ticks during inspection. The no-code map regression
also exposed an unnecessary dependency on the minimum's Core equations.
The zipper proof now owns that type synonym.

The independent runtime check covers 81 pairs of finite lengths. The
rule checks include a declaration after a splice and a consumer with
only the provider interface. Changing the compiled function with a rule
invalidates its original equation. The [compiler upgrade check](examples/open-evaluation/check-minimum-upgrade.sh)
rebuilds the affected home provider once and reuses it on the next build.

Job `499` rebuilt both compiler stages at `6d442d71b8`. Job `503`
matched all 543 Showcase lines at `-O0` and `-O2` on both stages and
in stage 2 GHCi with debugger breakpoints enabled. It also checked both
installed list interfaces and the two-compiler provider upgrade.
All 30 selected tests passed in the normal way, including the existing
no-code map check and the new local minimum and GHCi test. The test
fixture at `dcaa2a244a` gives the interactive caller a concrete
signature. Its compiler sources match `6d442d71b8`.

## Direct zip proof and retained callback rules

Both normal compiler stages at `5fe06625c3` matched the 543-line
Showcase at `-O0` and default `-O2`. Stage 2 GHCi and all 29 selected
normal-way tests passed in job `445`. The 542 and 543 compilers wrote
byte-identical optimized interfaces for the three tested rule providers.
Both read the other's interfaces and fired the imported-head and uncast
callback rules. The 543 reader also fired the callback rule through
a nominal cast. Both returned `(19,23,1,0)`.
[Rule tests](examples/qualified-pi/check-retained-rules.sh).

The integrated tree at `c88606fe92` has the same compiler. Its
Showcase reads two lists from IO and proves the length law on them.
Both stages passed the updated zipper regression, both native
optimization levels and the interface-only clients. Stage 2 GHCi passed
in job `457`. The proof uses ordinary `size` and `Prelude.zip`. At
this checkpoint `Minimum` is a closed family. The complete example
also compiles together in one Haskell module at both optimization levels.
[Direct proof](examples/open-evaluation/PreludeZipRuntime.hs) and
[Agda reference](REFERENCES.md#the-length-of-zip-and-demand-in-a-case-tree).

## Ordered zippers and nominal callback casts

Both compilers reported revision `3a4e5c237a`. Both matched the 542 lines
of the `54d1d5c54a` Showcase at `-O0` and default `-O2`, with Core and
STG Lint, HIE files and interface-only clients. Self-hosted GHCi matched
the same output. Both stages found the `zip` and `zipWith` checked equations
and fusion rules in the static and dynamic installed interfaces. Both
stages passed the zipper proofs and their demand and active-rule controls.

Job `397` passed 28 of the 29 selected registered tests. The remaining
test still expected the old pattern diagnostic for a variable in a
constructor matrix. The checker accepts a final variable row and names the missing constructor
pair when the rows are incomplete. The fixture at `89d450a893` checks
the missing pair `(Z, S)` and the current restrictions on each column.
It retains the three rejected programs, error code and source-span check.
Job `409` passed that test on the same compiler. These runs cover the
selected normal way. The earlier v80/v79 compatibility run is recorded
under [erased calibration status](#erased-calibration-status).

The ordinary zipper proofs use the equations exported by the installed
`GHC.Internal.List`. The client can prove the cons equation and both
empty-list equations, then prove the length law by induction. With an
unknown left spine and an empty right list, reduction stays stuck.
[Source proofs](examples/open-evaluation/PreludeZipProof.hs) and
[demand and rule controls](../../testsuite/tests/dependent/should_run/NativeOpenPreludeZipWith.sh).

The optimizer can push a retained function cast through an application
when nominal equalities change both ends of a lexical callback. The
result proof keeps the original callback and both equalities. A
representational newtype cast alone cannot justify the substitution.
The Core API regression failed on the baseline at `70ded7e706` with
`push a mixed callback cast forward did not push explicit retained congruence`.
Relinking the same fixture with the changed optimizer object produced
`native-core-api-ok`. [Core API test](../../testsuite/tests/ghc-api/NativeDependentCore.hs).

The following selection covers the changed compiler paths and their
existing regression tests. From a normal configured checkout, set
`hadrian` to its Hadrian executable and run this command through the
[shared queue](#local-build-capacity).

```sh
tests='NativeDependentCore NativeDependentCoreRun NativeLiftedCallback
NativeRetainedFunctionRules NativeDepFunCoWire NativeSimplifier NativeFunctionIndex
NativeIndexedGadtFields NativeIndexedFin NativeIndexedConsProof NativeConstrainedFamily
NativeConstrainedFamilyRules NativeDataFamilyRetainedDomain NativeGadtRetainedSum
NativeNestedPackagePat NativeRetainedPackage NativeImplicitExistential NativeOpenInstalledMap
NativeOpenInstalledAppend NativeOpenInstalledReverse NativeOpenConcreteGuardedList NativeOpenTheseFmap
NativeOpenPreludeZipWith NativeOpenZipRows NativeHomeRuleRecomp NativeTypeLet
NativeTypeLetQuote NativeTypeCase NativeTypeCaseRetainedTh'
docs/dependent-haskell/tools/ghc-build-job dependent-regressions \
    "$hadrian" -j5 --flavour=quick --docs=none test --test-compiler=stage2 \
    --only="$(echo "$tests" | tr '\n' ' ')" --test-speed=fast --test-way=normal \
    --skip-perf --config=config.timeout=900
```

## Erased calibration status

Both compiler stages at `6dd45e0d08` matched the 538-line Showcase at
`-O0` and `-O2`. Self-hosted GHCi matched the same output. All 28
selected registered tests passed. Both stages passed the nested type-let
quotation, exact-printing and source-context checks. The signed and
unsigned machine-word interface checks passed, including bidirectional
reads between this compiler and the previous v80 compiler at `e1c029d2a7`.

An ordinary client constructs a value of type `SomeSample a` from a
runtime flag. The constructor result fixes its hidden index to
`'Just calibrated`. The client opens the package and returns
`[Just 37,Nothing,Nothing]` without forcing the rejected sample's payload.
Other checks cover predecessor inference under a known equality, strict
cases on a curried callback, and the written kinds in type-case diagnostics.
[Provider](examples/open-evaluation/DirectIndexedList.hs),
[client](examples/open-evaluation/DirectIndexedListClient.hs)
and [callback](examples/open-evaluation/CompareAt.hs).

## Indexed volumes and unsigned word patterns

Both compiler stages at `e1c029d2a7` matched the 537-line Showcase at
`-O0` and `-O2`. Self-hosted GHCi matched it in bytecode. All 28
selected registered tests passed. Both stages also passed the type-let
quotation, exact-printing and source-context checks.

An ordinary client matches the first layer of a volume at a successor
layer count. The nested vector pattern is exhaustive and preserves
the other two dimensions. With an unknown layer count, omitting the
empty vector still warns. A second client applies a retained comparator
at both arguments when a family exposes the second arrow.
[Volume reader](examples/packed-vector/IndexedMatrixClient.hs)
and [callback example](examples/open-evaluation/CompareAt.hs).

Closed type families can match an unsigned machine-word literal or a
`Word#` field binder. The Showcase checks zero and the target maximum.
An unknown word stays stuck. At both optimization levels the previous
v80 reader accepted ordinary and signed-only interfaces and rejected
the unsigned feature at the module import with GHC-47808. The first
compatibility run used a regular expression for a marker containing a
literal plus sign. The corrected literal comparison passed on the same
compiler and interfaces. The global interface version stays at v80.
[Word family](examples/open-evaluation/PrimitiveWordFields.hs)
and [source and interface checks](examples/open-evaluation/check-word-prim-family.sh).

## Runtime tiles and curried comparisons

Compiler `a6460bb784` passed the 534-line Showcase with its matching
libraries. Stage 1 and self-hosted stage 2 ran native code at `-O0`
and `-O2`. Stage 2 also ran the complete source in GHCi bytecode.
All 27 selected registered tests passed. The testsuite reported no
framework failures or warnings.

An ordinary client packs and opens a tile whose shape records two
runtime dimensions. The zero-row case accepts an undefined column
count and undefined cells. A second client observes a runtime
comparator at both arguments. An equality proof can mention the
same call without evaluating the comparator or its operands.
[Tile client](examples/packed-vector/IndexedMatrixClient.hs),
[comparison](examples/open-evaluation/CompareAt.hs)
and [callback and demand checks](../../testsuite/tests/dependent/should_run/NativeLiftedCallback.sh).

The preceding compiler passed the native Showcase but rejected a
valid mapping proof in bytecode. GHCi adds debugger breakpoints to
the constructor result and the callback call. The compiler now
checks the equations through these ticks. A strict instance still
rejects a proof that would discard demand on its label.
[Instance and control](../../testsuite/tests/dependent/should_run/NativeOpenTheseFmap.sh).

Both stages checked nested tuple lets with provider sources hidden,
type quotations, exact printing, and source diagnostics. A bad
nested pattern points to the inner pattern and prints the written
let in context. The primary kind error can still name an internal
projection. The v79 and v80 readers rejected incompatible quantified
GADT interfaces in both directions. The home-module control rebuilt
the affected provider and reused it on its next build.
[Let patterns](roadmap/R18-type-level-lets.md)
and [interface upgrade](examples/r11-heq-pair/check-quantified-gadt-upgrade.sh).

## Four roadmap cards and the Core occurs check

The normal compiler at `bb45f2d668` integrates indexed vector patterns
(R04), a derived mapping method (R12), recursion on either list (R13),
and local kind signatures in type-level lets (R18). Both compiler stages
matched all 525 Showcase lines at `-O0` and `-O2`. The self-hosted GHCi
run also matched. All 27 selected tests passed on the same self-hosted
compiler. The first run passed 25 and timed out on two during a priority
pause. The two timed-out tests passed on their own rerun. The twelve
family-branch upgrade cases and both mapping upgrades passed. Exact
printing reproduced the signed type-let fixture byte for byte.

The previous normal compiler at `63cd0fb120` classified two recursive
substitutions as unifiable. One referred to the type variable through a
retained callback and its operand. The other reached it through an earlier
substitution. After the repair, the direct and indirect cases report
`MARInfinite`, and an independent substitution remains unifiable.
[Core controls](../../testsuite/tests/ghc-api/NativeDependentCore.hs)
and [unifier](../../compiler/GHC/Core/Unify.hs).

A separate Core API probe exposed missing occurs-check cases for an
experimental dictionary binder and a class-selector application.
Unifying with these forms can raise a non-exhaustive-pattern exception.
This remains an open R05/R12 issue in this checkpoint.

## R11 retained vector equality

At `48bb9fa5bd`, both normal Hadrian stages produced the 519 expected
Showcase lines at `-O0` and `-O2`. The self-hosted GHCi run matched
them too. The 17 selected normal tests passed. Each stage also ran the
reachable and impossible vector equality examples through the native
pipeline. The warning-disabled case passed Core and STG Lint.

On `5f70c2bdfc` and both normal stages of `2aacad1cf1`, matching
`HRefl` between a successor-length vector and `VNil` reached Core Lint
with a mixed-kind coercion binder. The same error occurred on a reachable
match with an unknown length. The source regression is
[NativeRetainedPackage](../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh).

Rebuilding the two solver modules from `2e8659f1bc` against the
`2aacad1cf1` compiler restored the source diagnostic. Both orientations
of the impossible match failed with GHC-40564 at `HRefl`. Suppressing
the warning let the same module pass Core and STG Lint at `-O0` and
`-O2`. At both optimization levels a client ran the reachable cases
through a stripped provider interface. A proof for a different vector
failed with GHC-83865. The full retained-package test also passed.

The ordinary pattern-synonym test `T11711` passed Core and STG Lint
before and after the repair. Five interleaved `-fno-code` runs of
`T9872d` allocated a median of 388,286,296 bytes before the repair
and 388,285,656 bytes after it.
[Design and source example](roadmap/R11-homogeneous-equality.md#an-equality-with-the-empty-vector).

## Shared R11 and R16 rebuild

The combined compiler at `9a1ce603f2` exposed a warm R12 upgrade
failure. Hadrian invoked GHC on `GHC.Internal.Data.Either`, and GHC
reused an interface that had passed the older ground-instance check.
The `Functor (Either e)` dictionary omitted its checked `fmap`
method. Both `6bfd1279a5` and `9a1ce603f2` accepted the source proof
when given the earlier, freshly built library. Both reported GHC-70565
at the same open callback when given the reused library.

The fix at `18fbc8a25b` checks eligible polymorphic dictionary providers
once. The warm provider then supplies the same evidence as a fresh
provider. The source-hidden proof compiles. An ordinary saturated
receiver and an instance with a dictionary premise keep their old
interface bytes. Use the [two-compiler upgrade test](examples/open-evaluation/check-polymorphic-method-upgrade.sh)
to repeat the warm and cold builds.

The integrated build at `63cd0fb120` repaired the installed
`GHC.Internal.Data.Either` interface in that same build directory.
Both normal stages matched all 522 Showcase lines at `-O0` and `-O2`.
The self-hosted GHCi run matched too. The self-hosted testsuite passed
all 22 selected tests. Both stages passed the twelve warm family upgrade
cases and the installed-provider and polymorphic-method upgrade checks.

The v69 comparison compiler at `ae563bb342` matched all 517 Showcase
lines at native `-O0` and `-O2` with both normal Hadrian stages.
Self-hosted GHCi also matched. All 12 selected tests passed, including
retained constructor contexts, existential instances, family applications,
reflection and home-module rule changes.

The first combined v79 build at `83f2aea915` started with an empty
build directory and stock GHC 9.14.1. Both stages built with the same
libraries. Importing an old provider failed at the import. Linking its
object directly failed on its reference to the old equality selector.
This also happened when the provider exported only `Bool -> Int`.
With source available, `--make` rebuilt all three modules and the
executable ran. A second build reused all three objects. An ordinary
client of the rebuilt GADT provider printed `(41,42)` at both optimization
levels. The old compiler also rejected the new interface.

The optimized Showcase exposed a separate R16 failure in this build.
An ordinary client imported two closed results computed by a rules-off
provider. It received GHC-83605 on both uses because its installed
Prelude had an applicable rule for the provider's internal `foldr`.
The provider now marks these results `OPAQUE`. The checker recognizes
their closed source signatures. A client that tries to import a generic
fold law still has to meet the rule check.
[Proof checks](roadmap/R16-open-term-evaluator.md) and
[worker representation](roadmap/R11-homogeneous-equality.md).

Both normal compiler stages at `5f70c2bdfc` matched the 517-line
Showcase at `-O0` and `-O2`. The self-hosted compiler also matched in
GHCi. All 12 consolidation tests passed. The three focused R16 tests
passed, including the installed fold and transitive home-rule changes.
The checked-method test builds a dynamic object for its separately
compiled callback when the in-process interpreter needs one.
The same fixture passed with the external interpreter.

The v69-to-v79 upgrade rejected an old alias or family interface at
import. After rebuilding the provider, the client reported GHC-83605
at the conflicting proof. Rules-off and ordinary home modules rebuilt
once and reused their new outputs. With the final self-hosted compiler,
ordinary `-O0` and `-O2` clients both observed `(41,42,43,44)` from
the retained `Whole` and `Move` examples compiled at `-O0`.
Compiling those providers at `-O2` passed desugaring lint and reached
the existing optimizer restriction GHC-59107.
[Installed provider upgrade](examples/open-evaluation/check-source-proof-provider-upgrade.sh)
and [home cache upgrade](examples/open-evaluation/check-home-rule-closure-upgrade.sh).

## Ordered type let source checkpoint

At compiler revision `fd41efddef`, a type can use a nonempty group of
simple let bindings. The first binding can capture an enclosing name
before a later binding shadows it. In the optional-header example,
`width = 1 + n` is in scope in `withHeader = Vec a width`.
An ordinary client can import the result from the provider's interface.
[Type definitions](examples/open-evaluation/TypeLet.hs) and
[scope and proposal history](roadmap/R18-type-level-lets.md#ordered-binding-groups).

Both normal compiler stages produced all 511 expected Showcase lines at
`-O0` and `-O2`. The self-hosted compiler also matched the output in
GHCi. The first-stage source and quotation checks passed. The
self-hosted testsuite returned seven expected passes and no unexpected
results across the seven selected tests. Five exact-print fixtures
passed. The ordinary pretty-printer round-tripped a grouped let with a
case inside its right-hand side. A forward reference in a closed synonym
reported GHC-76037 at the offending variable. In a signature with
implicit quantification the same free name can bind an outer parameter.
[Source and interface tests](../../testsuite/tests/dependent/should_run/NativeTypeLet.sh)
and [printing fixture](../../testsuite/tests/ghc-api/exactprint/TypeLetPrint.hs).

## Combined matrix and retained constructor checkpoint

At `dc31b96c26`, normal quick Hadrian builds of Stage 1 and Stage 2
passed. Both built and ran all 510 Showcase rows at native `-O0` and
`-O2`. Stage 2 also matched those rows in source GHCi. The combined
testsuite run had 24 expected passes and zero unexpected outcomes.

* **R03, R04 and R41.** A nested constructor call can supply a runtime
  result index. A vector package can carry different element types
  without forcing its witness. Quoting a GADT preserves its ordered
  binders and fields. The wire test included an earlier compiler reader.
* **R10.** A closed family can match a signed `Int#`. The source-hidden
  client passed with both compilers and both optimization levels on the
  64-bit host. The API probe checked raw literals and target-width conversion.
* **R15 and R17.** Closed evaluation recovered both dimensions of a square
  and checked labels stored after the cells of a matrix. The registered
  tests cover invalid dimensions and imported lambda closures.
* **R36.** Registered positive and negative cases cover pun warnings at
  unqualified uses, implicit binders and explicit disambiguation.

The normal first-stage compilers at `e5b6168b83` and `dc31b96c26`
reported the same errors for nested family applications at depths 32,
128 and 512. At depth 512, compiler allocation changed from 665,688,176
to 665,765,448 bytes. The 480-call ordinary Quick Look program compiled
on both and changed from 42,072,608 to 42,213,248 bytes. These figures
are single runs of the same sources with `-O0 -fno-code`.

Two experiments remain available for separate decisions.

* **R11.** Worker ABI v79 at `04c39c5989` passed source-hidden worker and
  interface checks. An earlier normal build at `b9a090f1dd` matched 481
  Showcase rows with each compiler, passed 71 registered tests, and
  matched all 37 native API outputs. A v69 provider can retain the old
  selector in native code even when its interface ABI hash matches.
  Adopting v79 requires a coordinated native package rebuild.
* **R16.** The family-axiom prototype is saved at `68c252d93d`. The
  proof-identifier port is `376a415fcf`, and the selected-method
  prototype is `22ee2d86dd`. An unmarked older interface can carry a
  normalized result with no record of the original method. The
  selected-method prototype also rejects an installed rewrite rule in
  the optimized Showcase. The next check must cover both cases.

## Family and package checkpoints

At compiler revision `d002ca9da1`, a closed family can match the
`Int#` field of promoted `I#`. Clients with the extension disabled
proved reduction at a concrete field through the hidden provider
interface. Named, wildcard, and written field binders all passed. The
64-bit machine literal wrapped at its target width. An unknown `Int`
application stayed stuck, and an ordinary `forall` over an `Int#` index
still gave GHC-83865. A matched Core API check also rejected raw
runtime-index readback.

The normal Hadrian `quick` build completed both compiler stages. Both
matched all 501 Showcase outputs at native `-O0` with stripped
interfaces and at native `-O2` with default interfaces. The self-hosted
compiler also matched in source GHCi. Both compilers ran the focused
check at `-O0` and `-O2` with Core and STG Lint.
[Showcase checks](check-showcase.sh),
[focused interface check](examples/open-evaluation/check-int-prim-family.sh),
and [R10 scope](roadmap/R10-unboxed-promotion.md#machine-integer-family-patterns).

At compiler revision `2a1003b253`, a client proves the contents of
`Table.withoutRowLabels (Table.sample 4)` from a compiled producer.
The outer cell vector supplies its own length. A later vector of row
labels must have that length. The checks cover empty axes, a wrong
inner width and inconsistent trailing labels at `-O0` and `-O2` in
both interpreter modes. A square whose two dimensions share one hidden
index still reports GHC-70565 at the call.
[Table example](roadmap/R15-closed-term-evaluator.md#read-the-stored-cells-of-a-rectangular-table).

The same compiler reports GHC-59431 for explicit type and term
binders with the same name in one match under `-Wpun-bindings`.
All 502 Showcase outputs matched at native `-O0`, native `-O2`, and
in source GHCi. All 29 selected registered tests and the three linked
GHC API drivers passed.
[Local warning examples](roadmap/R36-pun-warnings.md).

At compiler revision `5ecac2c539`, an ordinary client reads
`Just 42` and `Left "missing"` from rows whose schemas were defined
in different modules. Each open family equation can pass a capturing
lambda to `Map`. A conflicting instance reports GHC-34447, the public
family name and the location of the local declaration.
[Instance and client checks](../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh).

All 501 Showcase outputs matched at native `-O0`, native `-O2` and
in source GHCi. The 17 selected registered tests and the linked GHC
API checks passed. Local and imported lambda signatures round-tripped
in both interpreter modes. A module with ordinary equations and prior
retained-sum equations produced byte-identical interfaces and objects
at both optimization levels when compiled at the same output path.
A separate `-fno-code` sample of 400 ordinary open family equations
allocated 684,612,648 bytes, up from 684,114,056 at the R36 checkpoint.
All three runs with each compiler gave the same allocation.

At compiler revision `711b6e0e56`, `-Wpun-bindings` and
`-Werror=pun-bindings` identify type and term definitions with the
same spelling in one top-level declaration group. The diagnostic is
GHC-59431. Default flags and `-Wall` accept the same declarations.
A plain provider produced byte-identical objects and interfaces at
`-O0` and `-O2`. Both compilers could read and link the other's
output. All 500 Showcase outputs matched at native `-O0`, native
`-O2` and in source GHCi. All 17 selected registered tests and the
linked GHC API checks passed.
[Source fixture and warning sites](../../testsuite/tests/rename/should_compile/ToplevelPunBindings.hs).

At compiler revision `da6124e9a1`, a closed call reads the stored
`Vec rows (Vec cols a)` from an ordinary table producer. Row and
column labels establish the dimensions before the evaluator checks
the cells. A source-hidden client proved all six cells of a two-by-three
table and both zero-dimension cases. The evaluator rejected a wrong
outer count and a short second row.
[Table tests](../../testsuite/tests/dependent/should_run/NativeClosedWitnessedBatch.sh).

The row function `withErrors` accepts a different type in each field.
Its signature passes `\a -> Either e a` to `Map`. An ordinary
interface-only caller can read the first output with an undefined
second field. Template Haskell reified and spliced local and imported
signatures. All 500 Showcase outputs matched at native `-O0`,
native `-O2` and in source GHCi. The 16 selected registered tests
passed. The linked GHC API checks matched 37 dependent-program and
18 open-evaluation outputs. The constructor and coercion checks passed.
[Row source](examples/open-evaluation/TypeLambda.hs) and
[reification tests](../../testsuite/tests/dependent/should_run/NativeTypeLambdaReify.hs).

At compiler revision `6a32ffff94`, `lookupColumnWhen` takes a visible
retained Boolean and an invisible runtime type witness. An ordinary
client read a matching element and skipped a column with an undefined
witness. Reading one field of a two-witness result left the other
witness unevaluated. These checks passed at `-O0` and `-O2` with
only the provider interfaces and objects available.
[Column example](examples/qualified-pi/RuntimeTypeColumn.hs).

A type synonym or type-level let can have an unparenthesized lambda
on its right-hand side. Exact printing preserves parentheses, comments
and the ASCII or Unicode arrow. The ASCII pretty-printer tests also
round-trip. The extension diagnostic points at the backslash.
[Lambda tests](../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh)
and [printer fixtures](../../testsuite/tests/printer/RetainedPrint.hs).

All 498 Showcase outputs matched at native `-O0`, native `-O2` and
in source GHCi. All 15 selected registered tests passed. The linked
GHC API checks matched 37 dependent-program and 18 open-evaluation
outputs. The constructor and coercion checks also passed.

At compiler revision `8c4942f617`, a type-level `let` can bind a
callback that captures an outer type. An inner `let` can shadow that
type name. Applying the callback still uses the outer type. In an
ordinary module with `ImpredicativeTypes`, Quick Look can infer the
erased witness `forall a. a -> a` in `\flag -> id (prepare flag)`.
[Local type callback](examples/open-evaluation/TypeLambda.hs) and
[erased witness](roadmap/R22-existential-quantification.md).

All 497 Showcase outputs matched at native `-O0`, native `-O2` and
in source GHCi. All 15 selected registered tests passed. The linked
GHC API tests matched 37 dependent-program and 18 open-evaluation
outputs. The constructor and coercion checks also passed.

At compiler revision `f1fb064fa5`, Template Haskell can quote and
splice a type-level let. Its right-hand side sees the outer binder.
In the same compiler, a GADT constructor can take an ordinary field,
a required type argument and a dictionary for that type, in that
order. An ordinary client recovers the type with `typeRep`.
[Type let](roadmap/R18-type-level-lets.md#type-quotations-and-splices)
and [constructor round trip](../../testsuite/tests/gadt/TerminalGadtContextTh.hs).

All 495 Showcase outputs matched at native O0 and O2 and in source
GHCi. The type-let and constructor round trips passed in both
interpreter modes. Thirteen registered tests passed. The linked
GHC API tests matched all 37 dependent-program and 18 open-evaluation
outputs. The constructor and coercion checks in `NativeDependentCore`
also passed. The build reused the verified target libraries. Its
`ghc-boot` library reports the donor Git revision in `--info`.

At compiler revision `5e857a0d67`, the ordered GADT constructor
`InferredPair :: Int -> forall {a} b. Show b => (a, b) -> InferredPair`
accepts `InferredPair 61 @Bool ('x', True)`. The visible type argument
selects `b`. Reification and splicing preserve both binder flags.
[Ordered-binder tests](../../testsuite/tests/gadt/GadtOrderedTelescopeTh.hs).

An ordinary instance can use `(Collects c, Show (Elem c))` as its
context when the provider marks `Elem` as a constrained family.
A missing domain in a ground instance reports GHC-39999 at the
instance context, including with deferred type errors.
[Instance premises](roadmap/R24-constrained-type-families.md#family-applications-in-a-signature-constraint).

The combined compiler passed 11 registered tests. All 493 Showcase
outputs matched at native `-O0`, native `-O2`, and in GHCi. The split
example uses ordinary `2` and `cut + 1`. The opted-in package example
uses `1` as its type index.
[Vector split](examples/packed-vector/CertifiedSplitClient.hs) and
[one-cell package](examples/packed-vector/PackedProjection.hs).

At compiler revision `bffa69e282`, the fork reads the selected
`Prelude.fmap` method from the installed `Functor Maybe` dictionary.
In the `Just` case the callback and payload stay abstract. A second
mapping can use the first call's result. Both source proofs use
`Refl`. The runtime program observes the outer constructor without
forcing the callback or payload.
[Proofs and runtime example](examples/open-evaluation/PreludeMappingProof.hs).

The source tests reject a different callback, a different operand, a
locally supplied dictionary, a changed rule, and an opaque worker.
Matching a proof of equality between two mapped results still cannot
establish equality between the inputs. A test instance that forces the
callback result also rejects the lazy `Just` equation.

All 491 Showcase outputs matched at native O0 and O2 and in
source-loaded GHCi. Ten distinct registered tests passed. The native
GHC API executable matched all 37 outputs. The Core checks also cover
curried and nested callbacks, a blocked case, a runtime callback head,
wrong kinds, and the shared reduction budget.
[Core tests](../../testsuite/tests/ghc-api/OpenTermWhnf.hs).

The earlier clean build at `28055fe292` compiled both Hadrian stages
and passed 461 Showcase outputs at each stage and 36 registered tests.
The linker printed the expected archive through `lib/../lib`. Checking
the file identity confirmed the link, and the original executable then
matched all 37 GHC API outputs.

At compiler revision `e3c9d53c0b`, the ordinary `MappingWorker`
module exports a ground instance whose mapping method selects a named
worker. The proof module checks the `Nothing` and `Just x` equations
for an open callback. A second instance exchanges two constructor tags.
GHC rejects a proof that claims it preserves the first tag. A locally
supplied dictionary rejects the `Maybe` proof with GHC-83605.

A source upgrade from `61785e1925` rebuilt the worker and its instance.
When the worker already carried its equations, GHC rebuilt the old
instance to record the method choice. Native and typecheck-only
builds reused the warm interfaces. The unrelated test interface stayed
byte-identical. An ordinary client using the previous compiler read the
new interface and linked its provider at O0 and O2.
All 489 Showcase outputs matched in source-loaded GHCi and at native
O0 and O2. The compiled GHC API program matched all 37 expected lines.
Nine registered tests passed, including the negative method-selection
and constructor-tag proofs.
[Proofs](examples/open-evaluation/MappingProof.hs)
and [review and Core observations](roadmap/R05-dependent-core.md#open-observation-of-the-installed-maybe-method).

Compiler `61785e1925` accepts a required type argument after a value
field in a prefix GADT constructor. An ordinary client supplies
`TypedNote 79 (type Bool) True`. Pattern matching binds the type at
the same position. The client also uses the Quick Look package reader
and the retained family results from R04. For R24, a newtype-derived
associated equation can use a representation constraint supplied by
the owning class's superclass. A constraint available only in the
deriving context still fails the domain check.

All 488 Showcase rows matched at native O0, native O2 and in
source-loaded GHCi. All 47 selected registered tests passed. The
separate indexed-list diagnostic from `fd52c23d14` also passed.
All 37 GHC API results matched. This Stage 2 compiler used a rebuild
of 77 compiler modules and three boot modules against matching
libraries. A clean two-stage build that also includes quoted type lets
is queued at `cfdb42ce78`.
[Required type arguments](roadmap/R41-nested-quantification-in-gadts.md),
[derived equations](roadmap/R24-constrained-type-families.md),
and [package readers](roadmap/R22-existential-quantification.md).

At revision `e5f162cbec`, an ordinary client passes a three-way `case`
directly to a vector reader. Only the selected vector is evaluated.
With `ImpredicativeTypes`, the client can also write `toList (id xs)`
when `xs` carries a hidden length. GHC opens the package around the
reader call. A constructor can require `Ord a` before a field and expose
`a ~ Int` after it. A view pattern on the field can use the first
constraint. The branch body can use both.

The 484 Showcase outputs matched at native O0, O1, O2 and in source-loaded
GHCi. The compiler passed 38 constructor, existential and impredicativity
tests, the retained-family domain test and all 37 GHC API observations.
The previous compiler rejects a new leading-and-terminal context layout
when it imports the interface.
[Ordinary vector client](examples/erased-existentials/RankedPackageClient.hs)
and [constructor example](examples/open-evaluation/LabelledValue.hs).

At compiler revision `95182f6deb`, a client can pass a conditional
directly to an ordinary vector reader. The operands can be two named
packages or two polymorphic producers such as `filterVec`. GHC checks
the conditional once. It then opens the selected package for the call.
The runtime example returns `([6],[7])` and leaves the unused branch
and the rest of the selected vector unevaluated. A custom `ifThenElse`
retains its declared argument and result types. A type family that
reduces to a package domain still forwards the package intact.
[Ordinary client](examples/erased-existentials/RankedPackageClient.hs)
and [source regression](../../testsuite/tests/dependent/should_run/implicit-existential-source/QuickLookForward.hs).

The same compiler accepts `StoredCheck :: !Int -> forall a. Eq a => a
-> StoredCheck a`. The wrapper takes arguments in source order.
Matching makes the dictionary available after its position in the
pattern. The ordinary client returns `((67,True),(71,False))`.
The constructor build checked 16 registrations and five historical
interface layouts. The application typechecker change preserved its
compiler package ABI. [Constructor](examples/open-evaluation/LabelledValue.hs),
[client](examples/open-evaluation/LabelledValueClient.hs), and
[proposal #402 review](https://github.com/ghc-proposals/ghc-proposals/pull/402#discussion_r575067219).

All 480 Showcase rows matched at native O0, O1 and O2 and in source-loaded
GHCi. The source-hidden conditional matrix passed at O0 and O2 with
`ImpredicativeTypes` both enabled and disabled. The registered existential
test passed. The GHC API run matched all 37 expected lines.


At compiler revision `3b5e52d9ad`, an ordinary module can choose the
types of two successive GADT fields and call an inferred polymorphic
function that uses erased packages. An interface-only client reads
two differently typed lists from `LabelledPair` and folds each with
`totalJobs`. At O2 GHC inlines both calls. The program returns
`((2,4.0),(7,0.0))`. The second call leaves an undefined element and
an unused cost function unevaluated. A separate `INLINABLE` version lets
an importing module specialize the function at `Integer` and `String`.
[Constructor client](examples/open-evaluation/LabelledValueClient.hs)
and [recursive job costs](examples/qualified-pi/RecursiveJobs.hs).

Core Lint accepts a constructor application and pattern whose second
equality uses the first. It rejects a proof with the wrong kind, role
or scope. This tests the representation needed for homogeneous worker
arguments. The installed `HRefl` worker still uses its original type.
At the same checkpoint GHC can read older optimized proofs of both
`CharToNat` and `NatToChar`. An ordinary source-hidden client that
uses the numeric, symbol and character deductions returns
`(2,3,5,7,11)`. [Core Lint regression](../../testsuite/tests/plugins/CoercionForall.hs)
and [literal proof regression](../../testsuite/tests/ghc-api/AppendSymbolDeductionIface.hs).

The merged release includes the rectangular-table example from
`e0e3dae8a9`. All 477 rows matched at native O0, O1, O2, and in
source-loaded GHCi. The 16 selected compiler tests passed. The GHC
API run matched its 37 expected lines.


At compiler revision `00c3df07d4`, a local producer's signature can
mention a closed family with a standalone kind signature before GHC
checks the imported family equation that uses the producer. The
stripped-interface client returned `True` at O0 and O2. A genuine cycle
through the kind signature still reports GHC-70566.
[Signed family example](examples/open-evaluation/FamilySignature.hs).

The same compiler reconstructs both prefix and suffix deductions for
`AppendSymbol` from old optimized interfaces when the premise identifies
one rule. A fresh producer records the direction explicitly. An older
reader can link the fresh object with interface pragmas ignored. Both
readers retain the ordinary type mismatch for a wrong result index on
the supported paths. Six registered cases and all 474 optimized Showcase
rows passed. [Source test](../../testsuite/tests/typecheck/should_run/AppendSymbolDeductionReadback.hs)
and [proof selection check](../../testsuite/tests/ghc-api/AppendSymbolDeductionIface.hs).


At compiler revision `e59256cf58`, a prefix GADT accepts one specified
`forall` between two value fields. An ordinary client of a stripped
interface chooses `@Bool` or `@Char` after the first field and observes
`((53,True),(59,'q'))`. The 474 Showcase rows match at native O0, O1
and O2, and in source-loaded GHCi. The GHC testsuite passed 56 distinct
registered cases. The GHC API run matched all 37 output rows. Four
older constructor layouts retain the same interface bytes. An older
reader diagnoses the new field order when it opens the interface.
[Constructor example](examples/open-evaluation/LabelledValueClient.hs)
and [scope and quotation](roadmap/R41-nested-quantification-in-gadts.md).

A second check at the same revision consumes both equalities that GHC
projects from a matchable application. Each of two compilers reads the
other compiler's interface. At O0 and O2 the program returns
`(47,53)`. At O2 both readers inline the producer's definition. The
Core audit finds zero mixed-kind evidence binders in this program,
down from two at `1a08e7d43d`. The installed `HRefl` worker still
has one such binder. [Core regression](../../testsuite/tests/plugins/HomogeneousSelectorGivens.hs).


At `892ba587fb`, an ordinary module passed a runtime Boolean to both
`frameRow` and `frameToList`. With two cells it obtained `":xy"` or
`"xy"`. The header-free branch ignored an undefined header. Separate
type equalities establish the result length for every `n`. Stripped
providers and clients ran at `-O0` and `-O2` with Core and STG lint.
Both compiler versions read the interfaces for the old prefix spelling
and the new infix spelling.
[Type-let test](../../testsuite/tests/dependent/should_run/NativeTypeLet.sh)
and [width example](roadmap/R18-type-level-lets.md#a-vector-with-an-optional-header).

The previous compiler also panicked on a prefix RHS `f a b` when the
kind of `f` was inferred. Resolving the kind of each checked capture
before substitution preserves the function kind at its application.
See Note [The Purely Kinded Type Invariant (PKTI)] in
[GHC.Tc.Gen.HsType](../../compiler/GHC/Tc/Gen/HsType.hs).

At `020023a416`, an ordinary keyed row used the key vector to determine
the length of the value vector. A dependent client checked the whole row
through stripped interfaces. At `9aeba5a435`, an ordinary caller also used
a scoped class method in a vector filter at `-O1` and `-O2`. GHC still
specialised the independent equality count in the same module. Each
component compiler passed 16 registered tests and all 463 Showcase rows
at native `-O0`, `-O1`, `-O2` and in GHCi.
[Keyed rows](roadmap/R15-closed-term-evaluator.md#recover-a-hidden-length-from-the-vector)
and [ordinary class client](../../testsuite/tests/dependent/should_run/promoted-class/ScopedSpecialise.hs).

The native Core API driver linked directly to the archive at
`8a431a16cc` and matched all 37 expected lines with empty standard error.
The runtime type operand change at `e029a412f3` passed the three registered
witness and diagnostic tests. Its 437 Showcase rows matched at native
`-O0`, `-O1`, `-O2` and in GHCi. A source-hidden Safe client passed
`(type Bool)` at an expected `TypeRep`. The negative test reports a
missing `Typeable a` at the written type operand.
The follow-up diagnostic names `Type.Reflection.TypeRep` when the caller
expects a local type with the same name. A bare operand with no expected
type requests a signature. All four registered tests and the 437-row
Showcase passed in the same modes.

At `4910df774c`, a regular Hadrian Stage 2 rebuilt the stock Base, List
and Foldable modules with Core and STG Lint. The installed list interface
exported checked equations for `span`. All 432 expected Showcase rows
matched at native `-O0`, `-O1`, `-O2` and in GHCi. The registered
selection had 15 passes. Its platform predicate skipped the native Core
API driver. The later `04ba4bc839` compiler passed its eight registered
tests and all 434 Showcase rows in the same four modes.

The combined compiler at `8a431a16cc` includes the public family domains
and the open evaluator for `span`. The build recompiled 52 affected
modules and boot interfaces against the regular compiler, then linked
the 888-member archive. Eleven registered tests passed. At `9eb0b0b0e2`, the same compiler matched all 437
Showcase rows at native `-O0`, `-O1`, `-O2` and in GHCi. The ordinary
append client and open arrow kind checks passed at `-O0` and `-O2`.
[Append check](examples/packed-vector/check-append-length.sh) and
[span proof](examples/open-evaluation/PreludeSpanProof.hs).

At `b4e8ac6ced`, the numeric zip and the two matchable projections
passed together. All 431 Showcase rows matched in native code at
`-O0`, `-O1` and `-O2` and in source-loaded GHCi. Nine registered
tests passed. The GHC API reader accepted valid evidence for either
projection. It rejected a wrong argument, a representational premise
and a missing premise for each direction. A module that also stored
a retained character requested both interface features.

On the projection change alone, 98 ordinary GADT tests produced the
same 96 passes and two expected failures as the predecessor. A source
stress test cancelled 16 nested applications. Calling the full proof
rule to compute one endpoint hit a 12-second time limit. Reading the
checked component completed in 0.30 s at `-O1` with Core Lint. At 96
applications the predecessor allocated 1.05 GB and the revised compiler
allocated 2.25 GB. These figures count compiler allocations on this
test. [Rule and limits](roadmap/R23-unsaturated-type-families.md#preserve-a-matchable-head-through-inlining)
and [nested source test](../../testsuite/tests/dependent/should_run/NativeNestedMatchableProjection.hs).

The regular Hadrian self-host at `74731c9f19` completed with the stock
libraries and all 429 Showcase rows in the four modes. The source change
at `0609d6bf6b` passed 430 rows in those modes. An ordinary `[Int]`
client obtained `[11,22]` from two written lists. Source-hidden tests
checked sharing and an undefined later element under ordinary and strict
defaults at `-O0`, `-O1` and `-O2`. Four registered tests passed.

At `16b14c445e`, Hadrian built a regular Stage 2 compiler and the stock
libraries. This compiler rebuilt `GHC.Internal.Base` and stripped
`GHC.Internal.Data.Foldable` with Core and STG Lint. The installed list
instance supplied the method for the `Prelude.foldr` append and map
proofs. An ordinary client also used character and string literals as
retained arguments. All 428 Showcase rows matched in native code at
`-O0`, `-O1` and `-O2`, and in source-loaded GHCi. Nine registered
tests passed.

The merge at `74731c9f19` added the current family scope and matchability
changes. Three compiler modules differed from the self-host. We rebuilt
them and linked them with its other objects and stock packages. All 429
rows matched in the same four modes. Ten registered tests passed. At
`-O0` and `-O2`, the older nullary-family providers rebuilt once and
remained current on the next run. An unrelated module stayed current.
The previous character reader rejected a retained character unfolding
at its interface header and linked the provider's stripped interface.
[Fold proofs](examples/open-evaluation/PreludeFoldrProof.hs) and
[ordinary character client](examples/qualified-pi/RetainedCharacterClient.hs).

At `057f0d628c`, a written character or a list of characters supplied a
retained argument at its DataKinds index. All 422 Showcase outputs
matched in stripped native code at `-O0`, source-loaded GHCi, and native
code at `-O1` and `-O2`. Nineteen registered tests, the 37-row native
Core test and the evidence API checks passed.

The preceding 421-output compiler rejected a provider that exported a
retained character in an unfolding. It reported GHC-47808 at the
interface header. The same reader used a stripped retained object and
inlined ordinary character code. The new reader also inlined the
retained literal. [Character client](examples/qualified-pi/RetainedCharacterClient.hs)
and [two-reader test](../../testsuite/tests/dependent/should_run/NativeRetainedChar.sh).

At `d1db028e3c`, the ordinary two-vector client inferred its result
package. Its output was `([7],[2,4])`. Reading the first cell on
either side left the other input and the selected tail unevaluated.
Removing a required `Show a` constraint reported GHC-39999 at the
producer argument. Identifying different hidden lengths still failed.

All 421 Showcase outputs matched in stripped native code at `-O0`,
source-loaded GHCi, and native code at `-O1` and `-O2`. Eighteen
registered tests passed. The erased-existential test also passed with
the added missing-constraint control. The ordinary impredicative suite
recorded 21 expected passes and one expected failure.
[Two-vector client](examples/erased-existentials/RankedPackageClient.hs)
and [R22 inference](roadmap/R22-existential-quantification.md).

The combined compiler at `0869c02fcf` passed all 420 Showcase outputs
in stripped native code at `-O0`, source-loaded GHCi and native code
at `-O1` and `-O2`. Eighteen registered tests, the 37-row Core test
and the evidence API checks passed.

A separate ordinary client used only the provider interfaces at compile
time. It read `Just (Token 2)` from a column of its own `Token` type.
A type mismatch and a negative offset both returned `Nothing`. The
test passed at `-O2` with Core and STG Lint and stripped interfaces.
[Runtime type and bounded position](examples/qualified-pi/RuntimeTypeColumn.hs).

The written-list and callback changes at `3b90104b36` passed all 417
Showcase rows in stripped native code at `-O0`, source-loaded GHCi, and
native code at `-O1` and `-O2`. Fourteen registered tests passed. The
37-row native Core test and the evidence API checks passed.

An ordinary client compiled at `-O2` with only the interfaces for
`zipWithExact` and `toList`. Core and STG Lint checked the unboxed
equality argument after CorePrep named both list spines. Taking the
first pair left the second element of each list unevaluated. A second
client used ordinary composition to sum a length-checked result.
[Written lists and runtime equality](roadmap/R03-dependent-products.md#zip-two-lists-at-a-common-length).

At `cd293f731f`, all 416 Showcase outputs matched in stripped native
code at `-O0`, source-loaded GHCi, and native code at `-O1` and `-O2`.
Fourteen registered tests, the 37-row native Core test and the evidence
API checks passed.

The reviewed matrix checker accepts `zipWithExact` on ordinary lists.
A runtime length comparison lets independent inputs use the typed zip.
For the same input twice, the ordinary caller gets its first pair with
an undefined tail. A missing cons/cons clause reports GHC-83605. A
cons clause that returns `VNil` reports GHC-83865 at `VNil` and lists
the source binders. The input dictionary has no written binder.

The ordinary GHC pattern-match suite ran on the public cursor compiler
with these five checker modules. It recorded 159 expected passes and
six expected failures, with no unexpected results. The 37-row native
Core API regression, evidence API checks and four registered dependent
tests passed on the same compiler.
[Checked zip](examples/qualified-pi/AlignedList.hs) and
[coverage and demand](roadmap/R03-dependent-products.md#zip-two-lists-at-a-common-length).

The join-point integration at `418cd6f6bb` also passed all 414 rows in
the same four modes. Fourteen registered tests passed on that revision.
The recorded join fixture runs inside `NativeTypeCase`. The 37-row Core
API check and its evidence checks passed with the new join-result rules.

At `973445d496`, the linear `TwoPositions` reader matches both stored
positions. Each position has the length of its own list as its bound.
GHC infers separate element types for the two calls to `sourceLength`
in the constructor declaration. The ordinary client also runs two
vector filters through a rank-2 callback at an erased length.

All 414 Showcase rows matched at native `-O0`, `-O1`, `-O2` and in GHCi.
Thirteen registered tests, the 37-row Core API test and the evidence
API test passed. A separate clean two-stage Hadrian build of the preceding
`ee2fdead20` revision passed all 410 rows at native `-O0` and in GHCi.
[Linear reader](examples/qualified-pi/ListCursor.hs) and
[ranked callback](examples/erased-existentials/RankedPackageClient.hs).

At `274ea2ece0`, an ordinary client reuses a bounded list position after
a comparison proves that two runtime lists have equal lengths. The
three observations are `[Just ('b',20),Nothing,Nothing]` for a valid
pair, unequal lengths and a negative offset. A written empty case and
an omitted empty equation both pass under a chain of length equalities.
An unrelated equality reports GHC-83605 at the first written equation.

The Stage 2 compiler matched 412 Showcase rows in source GHCi and at
native `-O0`, `-O1` and `-O2`. Seven registered tests passed, including
the separately compiled list client and higher-rank existential tests.
The direct linear cursor preserves demand on its selected field. A
conditional producer can infer one hidden callback type for both arms.
[Checked lists](examples/qualified-pi/ListCursor.hs) and
[conditional producer](examples/erased-existentials/RankedPackageClient.hs).

At `1610131f91`, `withIndex` consumes both a bounded position and a
callback linearly. A discarded or duplicated callback reports GHC-18872
at its binder in the written equation. A repeated retained constructor
with a predecessor shares its package case. An ordinary client also
opens a package with a higher-rank reader and formats its contents with
a `Show` superclass. An unused runtime tag stays unevaluated.

The Stage 2 compiler matched all 410 Showcase rows at stripped `-O0`,
default `-O1`, default `-O2` and in source-loaded GHCi. It passed 15
registered integration tests, the complete 37-row Core API regression
and the evidence API checks. The separate formatter and its located
errors passed at `-O0` and `-O2`. The previous reader ran the ordinary
rule and rejected the linear rule at the interface header. The linear
continuation predecessor passed 44 focused registered tests before the
higher-rank package integration.
[Linear callback](examples/qualified-pi/ListCursor.hs)
and [interface regression](../../testsuite/tests/dependent/should_run/NativeImplicitExistential.sh).

At `f3cc8c7849` the same two list-index equations accept a linear
position or an unrestricted one. An ordinary client checks a runtime
offset, packages the list and position, then opens the package and calls
its linear reader. The direct and packaged clients both select from a
cyclic list. A negative offset leaves an undefined list unevaluated.
An unsupported repeated linear package pattern reports GHC-47619
at the source pattern.

The combined Stage 2 compiler matched all 407 Showcase rows at stripped
`-O0`, default `-O1`, default `-O2`, and in GHCi. Eleven registered tests
passed on the final family-domain integration. Its predecessor passed
44 registered tests and the full 37-row Core API and evidence checks.
[Linear reader](examples/qualified-pi/CursorClient.hs),
[bounded lookup](examples/qualified-pi/ParameterizedIndexLibrary.hs)
and [linear package regression](../../testsuite/tests/dependent/should_run/NativeLinearRetainedPackage.sh).

At `189b97bf90`, an ordinary module writes a family domain below a
required type argument. The same `renderKnown` works at `[Bool]` and
`FieldKey 'True`. A missing domain still fails at the signature. The
Stage 2 compiler matched 404 Showcase rows at stripped `-O0`, default
`-O1`, default `-O2` and in GHCi. Seven focused registered tests passed.
The scope test also checked a required kind followed by a type of that kind.
[R24 examples](roadmap/R24-constrained-type-families.md#required-type-arguments).

At `455a53f43e`, a polymorphic callback can use its class constraint
to form an associated family. The ordinary client formats a field read
at a runtime flag. All 403 Showcase rows matched at stripped `-O0`,
default `-O1`, default `-O2` and in GHCi. Five focused registered tests
passed. At `e577ab4962`, two registered formation tests also checked
that a missing premise names the callback type that can bind it.
[Provider](examples/open-evaluation/ConstrainedElements.hs)
and [diagnostic checks](../../testsuite/tests/dependent/should_run/NativeRetainedFamilyDomain.sh).

At `00b4822dd4`, a family domain can use the preceding retained
argument. A dictionary from a GADT pattern justifies the element type
for that same flag. The combined Stage 2 compiler passed the 402-row
Showcase at stripped `-O0`, default `-O1`, default `-O2` and in GHCi.
Five focused registered tests passed, including wrong-flag and
out-of-scope evidence checks.
[Runtime example](examples/open-evaluation/ConstrainedElementsClient.hs)
and [scope checks](../../testsuite/tests/dependent/should_run/NativeRetainedFamilyDomain.sh).

At `5af708d0cc`, the ordinary client checks the class domain of a
fully applied associated family from an imported interface. The same
interface supplies a retained unfolding and a rewrite rule for linear
existential packages. The compiler inlines the unfolding and fires
the rule twice. Changing `Elem [Bool]` to `Elem [Int]` reports the
missing `Allowed Int` even with deferred term errors. The previous
reader accepts an ordinary interface and rejects the new family header.

The combined Stage 2 compiler matched all 401 Showcase observations at
stripped `-O0`, default `-O1`, default `-O2` and in GHCi. Nine selected
registered tests passed. The 399-observation predecessor also completed
a normal two-stage Hadrian build.
[Domain checks](../../testsuite/tests/dependent/should_run/NativeConstrainedFamily.sh)
and [mixed interface regression](../../testsuite/tests/dependent/should_run/NativeConstrainedFamilyRules.sh).

At `3c90ebc198`, an ordinary client uses a case to select the input
of a multiplicity-polymorphic arrow. A linear call consumes its argument
once. An unrestricted call returns the first field of a pair and leaves
the second field unevaluated. The Showcase prints `(7,(19,'q'),29)`.
[Provider and linearity checks](../../testsuite/tests/dependent/should_run/NativeTypeCase.sh).

This revision also enforces matchability on variable-headed applications
in instance heads and family patterns. A written unmatchable arrow gives
GHC-83865 under either setting of the dependent extension, including
with deferred term errors. The combined Stage 2 compiler matched all
398 Showcase observations in native `-O0`, native `-O2`, and source-loaded
GHCi. Nine registered tests passed in the normal way.
[Instance pattern regression](../../testsuite/tests/dependent/should_run/NativeUnsaturatedFamily.sh)
and [proposal #242](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#application-in-pattern-position).

At `2df3d0020e`, a local `INLINE` function can replicate a captured
element at two runtime lengths. An ordinary module opens an erased
package with a strict tuple pattern and passes its hidden state to its
linear reader. The previous compiler reads ordinary, object-only and
unrestricted-rule interfaces. It rejects an exported linear opening
at the interface header. The current reader fires the rule twice.

The predecessor with both R23 callback changes passed all 36 selected
tests. After integrating R19 arrow domains, seven tests covering the
changed features passed again. The final Stage 2 compiler matched 397
Showcase lines at stripped `-O0`, default `-O1`, default `-O2`, and
in GHCi. The Core API matched its 37-line output and the evidence API
checks passed. The 390-line revision at `300e845ea2` also passed a
normal two-stage Hadrian build.
[Ordinary linear reader](examples/erased-existentials/RankedPackageClient.hs),
[local unfolding](examples/qualified-pi/IndexBetaClient.hs),
and [interface rule regression](../../testsuite/tests/dependent/should_run/NativeImplicitExistential.sh).

At `f90de7ce0c`, a family equation can apply its callback when the
declared kind computes its matchability with `Flip m`. The ordinary
client reads a stripped interface and returns `(Just False,[True])`.
The two-equation bounded list index also passes in the same compiler.
[Callback check](../../testsuite/tests/dependent/should_run/NativeLaterMatchability.sh)
and [bounded index check](../../testsuite/tests/dependent/should_run/NativeIndexedFin.sh).

At `87107252ab`, a higher-order family can take another family with
a required type binder. The remaining binders must be unmatchable.
The client observes `(3,LT)` after applying the family at two kinds.
A matchable receiver, an extra equation parameter, and a proof equating
two distinct family heads still fail. [Provider](examples/open-evaluation/InvisibleFamily.hs)
and [source-hidden check](../../testsuite/tests/dependent/should_run/NativeUnsaturatedInvisible.sh).

At `8c11606405`, a type synonym can contain a case in the argument
of an ordinary arrow. The Showcase client returns `(7,120)`. A second
client combines this case with `RequiredPair` through two stripped
interfaces. It returns `(3,1)` at `-O0` and `-O2`. Passing `False` to
the branch that expects `(Int, Ordering)` reports GHC-83865 at that
argument. The provider enables `UndecidableInstances` for the nested
type family in this combined example. The Stage 2 compiler matched
all 395 Showcase lines at stripped `-O0`, default `-O2`, and in
source-loaded GHCi. [Arrow-domain regression](../../testsuite/tests/dependent/should_run/NativeTypeCase.sh).

At `4e969e6059`, the two-equation list index compiles from source.
The ordinary client checks a signed offset and looks up the resulting
position. Partial application to the empty list returns a function. Forcing
its result with an undefined position raises the exception from the position.

The Stage 2 compiler matched 392 Showcase lines at stripped `-O0`,
default `-O1` and `-O2`, and in GHCi. All 35 focused tests passed
across the main run and the corrected list-index test. The Core API
driver matched its 37-line golden output. The evidence API checks passed.
The list-index regression checks an inhabited branch, an unresolved
family, a wildcard, a lazy pattern, and a missing successor pattern.
[Source and coverage](roadmap/R03-dependent-products.md#omit-an-impossible-list-index-equation)
and [ordinary client checks](../../testsuite/tests/dependent/should_run/NativeIndexedFin.sh).

At `d35002f260`, `Recipe Map` retains the inferred matchability argument
of `Map`. A client reads the stripped provider interface, constructs a
recipe, and obtains `(Just True,[False])`. A shorter receiver specializes
the omitted matchability. A receiver with the same number of binders
but an incompatible kind fails with GHC-83865. An explicit family
equation binder can use the mapper at both matchabilities.

The Stage 2 compiler with the public bytecode and package changes
matched all 391 Showcase lines at stripped `-O0`, default `-O2`, and
in GHCi. The source-hidden recipe and ordinary list-index checks cover
the new family argument and the existing runtime index.
[Source and negative checks](../../testsuite/tests/dependent/should_run/NativeLaterMatchability.sh)
and [list-index checks](../../testsuite/tests/dependent/should_run/NativeIndexedFin.sh).

At `706677658b`, source-loaded GHCi checks the guarded list proof with
bytecode for the producer. The debugger's breakpoint ticks remain in
executable Core. Adding strictness to the skipped head prevents the
client from using the equation. A separate regression checks that a
strict local tuple pattern opens an erased package with the same demand
as an ordinary tuple pattern. An outer equality `ma ~ U` also permits
a fresh ordinary lambda inside a retained variable equation.

The Stage 2 compiler matched all 390 Showcase lines at stripped `-O0`,
default `-O1` and `-O2`, and in GHCi. All 35 focused tests passed.
The Core API run matched its 37-line golden. The bytecode test also
checked that the ordinary producer had no object file.
[Bytecode regression](../../testsuite/tests/dependent/should_run/NativeOpenConcreteGuardedListGhci.sh),
[local packages](roadmap/R22-existential-quantification.md#a-nonrecursive-local-package-pattern),
and [matchability regression](../../testsuite/tests/dependent/should_run/NativeRetainedMatchability.sh).

At `1c7f70b3ef`, the list index accepts `Fin (sourceLength xs)`.
An unsigned local caller can pass its list to both `mkFinGeneric` and
`indexGeneric`. The corresponding inputs on the preceding compiler needed
a signature, even when the caller supplied `@Char`. The earlier
`GenericCount` family keeps its arity and forwards to the ordinary
function. Both public workers now name that function directly.

All 390 Showcase lines matched stripped `-O0`, default `-O1` and
`-O2`, and GHCi. All 33 focused tests passed, and the Core API run
matched its 37-line golden. The updated source-hidden finite-index test
also passed at all three native optimization levels. Its negative cases
check GHC-83865 at `FZ` for an empty list, GHC-83605 at a linear
operand, and GHC-25897 at an operand with a rigid incompatible type.
[Ordinary caller](examples/qualified-pi/FinitePositionClient.hs)
and [registered checks](../../testsuite/tests/dependent/should_run/NativeIndexedFin.sh).

At `f17c3f41b8`, an ordinary client can use
`let (value, use) = package in use value` when the package hides the
type of `value`. The original let binding shares the producer.
Returning the hidden value or passing it to a consumer from a
different package fails at the written field. A separate check uses
guarded list equations at `Bool` and at a user-defined element type.
The GHCi Showcase loads the concrete list producer as object code.
A bytecode-only producer currently omits those equations.
[Lexical package checks](../../testsuite/tests/dependent/should_run/implicit-existential-source/LexicalLetMain.hs)
and [concrete list proof](roadmap/R16-open-term-evaluator.md).

A source wildcard in a retained pattern no longer prints an internal
binder in the relevant bindings. A named `_first` is still reported.
Given `Width p ~ Width q`, a retained constructor that requires
`Proxy p` rejects `Proxy q` with GHC-25897. A genuine recursive
family axiom still reaches the reduction-depth limit. In one matched
allocation run, changing only the rewriter object measured −2.38% on
`T9872a`, +0.24% on `T9872d`, and +0.10% on `T24984`.
[Retained diagnostic](../../testsuite/tests/dependent/should_fail/NativeFamilyIndexEvidence.stderr)
and [ordinary regression](../../testsuite/tests/typecheck/should_fail/WantedFamilyRewriteCycle.hs).

The 389-line Showcase matched stripped native `-O0`, default `-O1`
and `-O2`, and GHCi. All 33 focused tests passed, including the
three new concrete-list and matchability checks. The Core API run
matched 37 lines. After merging the ordinary skolem printer from
`b11b85c2b7`, all four Showcase modes and 11 diagnostic and package
regressions passed again. A separate normal two-stage build at
`d177823714` matched 383 lines in the same modes. Its source-hidden
split, finite lookup, ordinary `INLINE`, and ordinary `INLINABLE`
tests passed with the corrected split diagnostic span.

At `7128a29dc8`, one runtime cut splits both Boolean flags and characters.
The proofs track the original contents of each vector. Using the same
element type for both inputs still rejects exchanged proofs at each
written argument. The source-hidden client matched its 18-line golden
across three optimization levels.

The compiler at `45054a2d9a` also checks that the installed `takeWhile`
and `dropWhile` recover a finite list when their results are appended.
An erased package selector leaves the unused alternative lazy.
An imported rewrite rule matches two separate required type arguments.
All 28 focused regressions passed. The Core API executable matched its
37-line golden, and the interface evidence checks passed. That compiler
rebuilt the installed list equations and matched the 387-line Showcase
at stripped `-O0`, default `-O1` and `-O2`, and in source-loaded GHCi.
[Mixed vector inputs](examples/packed-vector/CertifiedSplitClient.hs),
[prefix and suffix](roadmap/R16-open-term-evaluator.md),
and [package selection](roadmap/R22-existential-quantification.md).

At `afcdaf34da`, the paired split needs no visible type applications.
After `cutRemainder` returns `Pair suffix Refl`, GHC checks each
`splitAtValue cut input` against its field type. It recovers the suffix
from the lengths of the returned vectors. A separate test hides an index
under a noninjective family. GHC reports GHC-83865 at the call when
the caller omits that index.
[Paired split](examples/packed-vector/CertifiedSplit.hs) and
[ambiguity control](../../testsuite/tests/dependent/should_fail/NativeRetainedSumInference.hs).

The same Stage 2 compiler matched all 383 Showcase lines at stripped
`-O0`, default `-O1` and `-O2`, and in source-loaded GHCi.
The Core API executable matched its 37-line golden and passed the
interface evidence checks. All 26 focused regressions passed across
the suite and a rerun. The first run caught an obsolete expected span
in the paired split test. GHC now selects the inner application itself.
The negative still checks the displayed types and the location of each
exchanged argument.

At `6b2c1a6029`, a source-hidden caller can split two different vectors
at one runtime cut. Each proof reconstructs its own vector. Exchanging
the fields produces GHC-83865 at each wrong argument and prints the two
written `SplitAtValue` types. Ignoring the bounds equality produces
GHC-83865 at the constructor and shows both widths. Reading the first
proof leaves the second vector unevaluated. A separate kind-order test
checks a constructor whose second inferred operand depends on the first.

The compiler checks the installed definition of `(.)` for the finite map
composition proof. The installed `filter` and `(++)` support a distribution
proof with one Boolean decision per head of the finite left list. A
runtime caller constructs those decisions for `Prelude.not`.
[Paired client](examples/packed-vector/CertifiedSplitClient.hs),
[map composition](examples/open-evaluation/PreludeMapCompositionProof.hs),
and [filter distribution](examples/open-evaluation/PreludeFilterAppendProof.hs).

The Stage 2 compiler built against the normal `89aa40ec56` baseline
matched all 380 Showcase lines at native `-O0`, `-O1`, `-O2`, and in
source-loaded GHCi. All 23 focused tests passed. The complete
`NativeDependentCoreRun` executable matched its 37-line golden, and
`NativeIndexAbstraction` passed both evidence assertions. The existing
inner-forall negative now reports GHC-25897 at the body of the function.
Its type signature is legal. A separate two-stage Hadrian build passed
on the preceding 378-line paired-split revision, including all four
Showcase modes and the source-hidden split and finite-position tests.

At `b5faf22c3e`, the rule for `keepRequired tag (type Bool) m x`
infers the types of `m` and `x`. A client compiled from the producer's
interface fires the rule at `-O2` and leaves both indices lazy. A type
splice in the prefix runs once. The signed rule and an ordinary control
produce identical interface and object files before and after the change.
Six focused regressions passed, and 378 Showcase lines matched at stripped
`-O0`, default `-O2` and in source-loaded GHCi. The full Core API test
checks both endpoints of the branch equality for the scoped call `f b`.
The same interface bytes are rejected when the reader gives `f` a linear
arrow.
[Source rule](examples/qualified-pi/VisibleRuleArguments.hs) and
[interface proof](../../testsuite/tests/ghc-api/NativeDependentCore.hs).

At `cd14d1268d`, an ordinary reader gets 17, 23 and 29 from a named
erased package, a producer call, and the same call forwarded through
`id`. A required erased type can also follow a retained argument.
The source client calls `keepRequired tag (type Bool) m x` without
forcing either runtime index. An extra value argument now reports five
visible arguments supplied and four declared.

The Stage 2 compiler matched all 378 Showcase lines at stripped `-O0`,
default `-O2`, and in source-loaded GHCi. At `c1372d1ebf`, all twelve
focused integration tests passed. The subsequent diagnostic change
matched five existing arity diagnostics against the preceding compiler.
The ordinary required-argument control also matched GHC 9.14.1.
[Retained argument checks](examples/qualified-pi/check-visible-rule-arguments.sh)
and [package reader](examples/erased-existentials/RankedPackageClient.hs).

At `89aa40ec56`, the 376-line Showcase includes a split written as
two equations, an ordinary `INLINE` wrapper, and finite map composition.
The split's recursive call supplies the equality that its cons branch
needs. Removing that premise fails at the written `Refl`. A separate
ordinary client compiled against a hidden producer reproduced a Core
panic on the previous compiler. Reinstating the vanilla constructor
case in `mapNativeIndex` let the same packed-vector client compile and
run all 562 output lines. Its producer interface was identical in both
runs. The complete `NativeDependentCoreRun` executable matched its
37-line golden, including module recompilation. The interpreter-enabled
Stage 2 compiler matched all 376 Showcase lines at native `-O0`, `-O1`,
`-O2`, and in source-loaded GHCi. All 19 focused regressions passed.
The map proof at this revision uses a one-equation family for the
composed callback.
[Split](examples/packed-vector/CertifiedSplit.hs),
[map proof](examples/open-evaluation/PreludeMapCompositionProof.hs),
and [packed-vector client](examples/packed-vector/Main.hs).

Both stages of a normal Hadrian build passed at `49b6526f52`.
All 373 Showcase lines matched at native `-O0`, `-O1`, `-O2`, and
in source-loaded GHCi. The same compiler passed the direct split, signed
position, and ordinary `INLINE` regressions.

At `f2cbaced47`, an ordinary client can pass the result of a declared
producer directly to a polymorphic reader. The lazy reader can ignore
the producer's input. A retained rewrite rule also accepts an explicit
type argument after an ordinary value argument. The self-hosted
compiler matched all 373 Showcase lines at stripped `-O0`, default
`-O2`, and in source-loaded GHCi. All eight focused tests passed.
[Package client](../../testsuite/tests/dependent/should_run/implicit-existential-source/MixedPackageMain.hs)
and [rewrite rule client](examples/qualified-pi/VisibleRuleClient.hs).

At `a5bba656b8`, an ordinary client can pass `(2 :: Nat)` to a
retained length parameter. A second client passes a named erased package
directly to a polymorphic reader. Its argument type determines which
witness to open. The self-hosted compiler matched 372 Showcase lines
at stripped `-O0`, default `-O2`, and in source-loaded GHCi. All eight
focused tests passed, including rejection of a different vector at the
same length in the checked split.
[Literal client](../../testsuite/tests/dependent/should_compile/NativeLiteralClient.hs)
and [mixed-package reader](examples/erased-existentials/ErasedPackage.hs).

At `68207d8a32`, the Stage 2 compiler combined R23 callback
matchability, written GADT constructor operands and checked-split
inference. The 371-line Showcase matched at stripped `-O0`, default
`-O1`, default `-O2`, and in source-loaded GHCi. Ten registered
integration tests passed. Fourteen ordinary GHC tests gave 26 expected
passes across their selected ways on both the combined compiler and
the preceding signed-position compiler. The old reader accepted the
vector operand interface and rejected the general constructor operand
header. The ordinary client inlined the multiple-index constructor
function from its interface and checked its lazy fields.
[Finite positions](../../testsuite/tests/dependent/should_run/NativeIndexedFin.sh),
[checked split](../../testsuite/tests/dependent/should_run/NativeCertifiedSplit.sh)
and [negative index diagnostic](../../testsuite/tests/dependent/should_fail/NativeIndexOperandFamily.stderr).

The preceding signed-position source at `75e123a31c` also passed a normal
two-stage Hadrian update in the existing build tree. Its compiler ran
all 368 Showcase lines at the same optimization levels and in GHCi.
The separately compiled signed lookup and certified split passed.

At `0ae49102b4`, the unsigned datatype `data Box f = Box (f Bool)`
works alongside the signed finite-position lookup and the certified vector
split. The self-hosted compiler matched all 370 Showcase lines at stripped
`-O0`, default `-O2`, and in source-loaded GHCi. Four tests covered
the callback, finite positions, a cyclic nested family, and the diagnostic
for independent inputs. Its parent `32242d14fb` passed 15 focused tests.
[Callback example](examples/open-evaluation/LaterMatchabilityClient.hs)
and [finite-position example](examples/qualified-pi/FinitePositionClient.hs).

At `75e123a31c`, the Stage 2 relink produced all 368 Showcase lines at
stripped `-O0`, default `-O1`, default `-O2`, and in source-loaded GHCi.
The separately compiled finite-position client tested signed offsets from
IO, a second vector of the same length, and undefined vector fields.
Replacing the actual predecessor with an independent one failed at
`Refl`. An older compiler read an ordinary GADT interface from the
new writer and rejected a provider that requires the new zero-case
evidence. The preceding `a51c497d96` passed an incremental two-stage
Hadrian build, the 366-line Showcase in the same modes, and the
source-hidden certified split.
[Finite-position test](../../testsuite/tests/dependent/should_run/NativeIndexedFin.sh)
and [ordinary client](examples/qualified-pi/FinitePositionClient.hs).

At `c9e62d3e34`, an unsigned Haskell 98 datatype accepts an imported
open type family as its parameter. A client compiled from stripped
interfaces constructs both a `Maybe` box and an open-family box. The
self-hosted compiler matched all 366 Showcase lines at stripped `-O0`,
default `-O2` and in source-loaded GHCi. Nine focused tests passed.
The closed-call append test used the external interpreter to load its
dynamic objects. The arity and explicit matchability controls still
reject invalid applications.
[Example](examples/open-evaluation/LaterMatchabilityClient.hs) and
[R23 inference conditions](roadmap/R23-unsaturated-type-families.md).

At `508cd799f6`, an ordinary caller can again write `expectMarker label
marker $ evaluate proof` when the proof mentions the length bound by a
constructor pattern. Before the fix, the checker rejected the application
with GHC-83865. The packed-vector client produced the same 562 lines at
`-O2` as the unoptimized reference. The self-hosted compiler matched the
364-line Showcase at stripped `-O0`, default `-O2` and in GHCi. The
ordinary rank-n `($)` and nested vector append controls also passed.
[Packed-vector client](examples/packed-vector/Main.hs) and
[Quick Look proposal review](https://github.com/ghc-proposals/ghc-proposals/pull/274#issuecomment-533664634).

At `bcf4d0c389`, the self-hosted relink matched all 366 Showcase lines
at stripped `-O0`, default `-O1`, default `-O2`, and in source-loaded
GHCi. The source-hidden recursive proof and its demand checks passed at all
three optimization levels. The test rejects omission of the induction
premise at the written `Refl`. The retained-cast, indexed-append and
GHCi provider-reload regressions passed on this compiler. A comparison of
14 ordinary GHC tests across their selected ways produced 26 expected
passes on each of the public and combined compilers. Both used the same
installed libraries.
[Split proof and independent bounds](roadmap/R04-dependent-sums.md#prove-a-split-preserves-the-original-vector)
and [registered client](../../testsuite/tests/dependent/should_run/NativeCertifiedSplit.sh).

The fieldful constructor checks also exercise old and current interface
readers. The current reader accepts and inlines an older nullary
provider. An earlier reader rejects an interface that requires support
for a new constructor operand. Matching a constructor preserves the
actual fields through optimization and GHCi reload.
[Constructor test](../../testsuite/tests/dependent/should_run/NativeIndexedConsProof.sh).

At `f8d7a68fb1`, a source-hidden client demands empty-append proofs
for two vectors at one length. It checks literal cuts of zero, one and
two, two cuts from IO, and an undefined cut. Using the first vector twice fails at the
second proof. The earlier compiler failed Core Lint on this client
at `-O1`. The revised compiler preserves the constructor proof through
occurrence analysis, float-out, and CorePrep.

The self-hosted relink matched all 364 Showcase lines at stripped `-O0`,
default `-O1`, default `-O2`, and in source-loaded GHCi. The ordinary
client passed at all three optimization levels with Core and STG Lint.
The GHCi provider-edit test also passed. The updated fork revision adds
the independently tested literal-cut case to the retained cast regression.
Its compiler differs from the tested relink only in comments.
[Ordinary client](examples/packed-vector/OrdinaryOpenVectorAppend.hs),
[proof provider](examples/packed-vector/OpenVectorAppend.hs)
and [registered test](../../testsuite/tests/dependent/should_run/NativeOpenIndexedAppend.sh).

At `ecfe7ca89f`, matching the runtime cut supplies the nominal equality
needed to pass the same vector at the branch's width. The source-hidden
ordinary client returned `(True,True)` on two undefined vectors. A wrong
width failed with GHC-05617. The bootstrap relink matched all 363 Showcase
lines at default `-O2`. The self-hosted relink matched them at stripped
`-O0`, default `-O2` and in source-loaded GHCi. Both relinks used pinned
compiler objects. In one GHCi process, making append strict removed the
cons equation. GHCi reported GHC-70565 at the recursive proof term.
Restoring the lawful provider made the proof available again.
[Showcase](Showcase.hs), [argument test](../../testsuite/tests/dependent/should_run/NativeRetainedCast.sh)
and [reload test](../../testsuite/tests/dependent/should_run/NativeOpenIndexedAppendGhci.sh).

The 362-line public checkpoint includes an ordinary caller that uses
the stored suffix in both a pattern signature and a nested vector append.
At `f47fadd22e` the bootstrap relink passed stripped `-O0`, stripped
`-O2`, and default `-O2`. A self-hosted relink ran all 362 lines from
source in GHCi. At `82fdb32532` the alias scheduling change passed the
same golden at `-O0`, default `-O2`, and in source-loaded GHCi.
[Split caller](examples/packed-vector/NamedSplitClient.hs)
and [R21 alias checks](roadmap/R21-dependency-analysis.md#local-synonyms-in-a-retained-field-worker).

A clean Hadrian build of `c24dcec367` reproduced all 360 lines with
both compiler stages at stripped `-O0` and default `-O1` and `-O2`.
The self-hosted compiler also ran the source-loaded GHCi golden. The
dependent test directory had 313 passes and the same 17 failures as the
354-line baseline. The focused normal run had 33 passes and one failure.
Its API test expected GHC to reject an alias with the valid proof
`Add 'Z m :~: m`. The updated test at `50c06eab01` executes that case
and requires GHC-83865 for a successor contradiction. The complete
updated API test passed on the clean compiler.
[API regression](../../testsuite/tests/ghc-api/NativeDependentCoreRun.hs).

At `c24dcec367`, the fork includes both vector append equations, the
optimized cast fix and inferred matchability for higher-order callbacks.
The bootstrap and self-hosted compiler relinks each reproduced all 360
Showcase lines at stripped `-O0` and default `-O1` and `-O2`.
Self-hosted GHCi reproduced the same output from source. A live breakpoint
stopped in `appendVec` and resumed. The recursive vector program returned
`[False,True]` with breakpoints both enabled and disabled.

The relinks rebuilt `GHC.Core.Opt.Simplify.Iteration`, `GHC.Tc.Gen.HsType`
and `GHC.Tc.Gen.SourceTable` for the two compiler stages. The self-hosted
relink used the remaining compiler objects from the fingerprinted
`c94baacebe` build. [Vector equations](examples/packed-vector/OpenVectorAppend.hs)
and [registered debugger controls](../../testsuite/tests/dependent/should_run/NativeOpenIndexedAppendGhci.sh).

The bootstrap and self-hosted compiler relinks at `91341c4172` each
ran all 358 Showcase observations at stripped `-O0` and default
`-O1` and `-O2`. Self-hosted source-loaded GHCi matched the same
golden. Both compilers passed the source matchability diagnostics
and the singletons migration checks against stock GHC 9.14.1.

The preceding 354-line two-stage build at `90f02cc9f5` passed all
six native runs and 29 focused normal tests. The dependent directory
recorded 312 passes and the same 17 failures as the 351-line build.
GHCi rejected the open vector append proof when it inserted a
breakpoint around the returned field. The corrected verifier checks
the field below that breakpoint. The registered debugger test stops
at the original source line and resumes. Strict and coverage-counted
branches still withhold the equation.
[Debugger regression](../../testsuite/tests/dependent/should_run/NativeOpenIndexedAppendGhci.sh)
and [source matchability tests](../../testsuite/tests/dependent/diagnostics/DependentArrowMatchability.sh).

At `fa8b71fb1c`, all 358 Showcase lines matched at `-O0`, default
`-O1`, and default `-O2`. This run used the normal bootstrap compiler
at `c94baacebe` with one rebuilt simplifier module. Before that change
the full Showcase passed `-O0` and failed Core Lint at both optimized
levels. The simplifier now keeps the nominal evidence for the cast
from an opened package's payload type to `Vec a n`.
`NativeRetainedCast`, `NativeOpenIndexedAppend`, `NativeGadtRetainedSum`,
and `NativeRetainedPackage` passed on the corrected compiler.
The bootstrap executable does not support interactive use. Self-hosted
GHCi and the broader suites require the normal build of this revision.
[Simplifier](../../compiler/GHC/Core/Opt/Simplify/Iteration.hs),
[cast controls](../../testsuite/tests/dependent/should_run/NativeRetainedCast.sh)
and [Showcase](Showcase.hs).

At `2d1fd63cc6`, a relink of the 352-line bootstrap compiler checked
the equality returned by an indexed empty-vector match inside an opened
package. A nominal cast identifies the payload as a vector. All 352 lines
matched at `-O0`. The default `-O1` run stopped in Core Lint after a
simplifier pass lost the nominal evidence on a retained argument. This run
did not reach `-O2`. In a separate ordinary client with a vector argument,
the 352 bootstrap checked transport at unknown length and printed
`(Just [],Nothing)`. Asking to match the empty proof at a successor length
still gave GHC-40564.
[Empty-branch proof](Showcase.hs)
and [transport regression](../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh).

The four-module stage1 library relink at `789ee08833` passed
`NativeDependentCore`, `NativeDependentCoreRun`, and `RetainedQuantification`.
The first two matched their existing output. The third reported the two
kind errors on the occurrences of `b` in `Select b`. The API fixture
checks an opaque `Int` proof through desugaring. An independent ordinary
client used the imported proof with a bottom `Int` and printed 42
at `-O0` and `-O2`. The fresh constructor-field checks reject recursive
retained constructor telescopes and preserve the ordinary opaque function
binder.
[Source and API cases](../../testsuite/tests/ghc-api/NativeDependentCoreRun.hs)
and [diagnostic golden](../../testsuite/tests/typecheck/should_fail/RetainedQuantification.stderr).

The bootstrap compiler relink at `fa6b3b44ca` reproduced all 356
Showcase lines at stripped `-O0` and default `-O1` and `-O2`.
`NativeLocalOpen` and `NativeTypeCase` passed. The source-hidden shadow
test selected the local worker. The worker that needs its own unfinished
constructor failed with GHC-68947 at both optimization levels.
[Local field worker](examples/open-evaluation/LocalAliasField.hs) and
[scope checks](../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

Both compiler stages at `91382c33c4` reproduced all 351 Showcase lines
at the same three optimization levels. Self-hosted GHCi matched the
golden. All 27 focused normal tests and the separate GHCi inference
test passed. Of 327 dependent normal cases, 310 passed and 17 failed.
`T14880`, `T14880-2`, and `T15825` matched their unchanged diagnostics.
The other 17 failures kept their previous categories.

At `90f02cc9f5`, the bootstrap compiler relink reproduced all 354
Showcase lines at stripped `-O0` and default `-O1` and `-O2`. The
indexed vector append and installed list reverse tests passed, including
their rule and source-hidden interface controls. These two paths share
the check of equations against final Core.
[Vector append test](../../testsuite/tests/dependent/should_run/NativeOpenIndexedAppend.sh)
and [reverse test](../../testsuite/tests/dependent/should_run/NativeOpenInstalledReverse.sh).

Both compiler stages at `f1749eccba` reproduced all 348 Showcase lines
at stripped `-O0` and default `-O1` and `-O2`. Self-hosted GHCi matched
the same output. The external proof plugin accepted the checked candidate
and rejected the bad candidate. The typed splice and its single finalizer
passed. Of 28 focused tests, 27 ran and passed in the normal way. One
requires GHCi. The dependent directory ran 327 normal cases across 334
tests. It recorded 307 passes and the same 20 failures, by name and
failure category, as the 340-line compiler.

At `bcc07e37a9`, all 350 Showcase lines matched at `-O0`, default
`-O1`, and default `-O2`. The compiler relink used the five changed
modules and the normal 347 build. All six registered tests passed.
The ordinary interface-only client used the proof for a runtime vector.
Obtaining `Refl` left an undefined vector unevaluated. A proof about
another vector of the same length failed at `Refl` with GHC-83865.
An independent rule fired at both optimization levels when enabled
and stayed disabled when the proof module's flag was used.
[Proof](examples/packed-vector/OpenVectorAppend.hs)
and [source regression](../../testsuite/tests/dependent/should_run/NativeOpenIndexedAppend.sh).

At `6ac33d43bf`, a checked vector split takes an independent runtime
cut. The success case supplies the proof needed to rejoin the vectors.
The compiler relink matched all 349 Showcase lines at `-O0`, default
`-O1`, and default `-O2`. The four registered tests for splitting,
package use, and application inference passed. The ordinary client
checked empty, interior, exact, and oversized cuts. The check also
returned its bounds decision with an undefined vector.
[Split regression](../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh)
and [inference diagnostics](../../testsuite/tests/dependent/should_fail/NativeSplitResult.hs).

Both compiler stages at `7278fc1e52` matched the 347-line Showcase in
all three native modes. Self-hosted GHCi also matched. The focused
suite had 74 passes, one missing-library skip, and three unexpected
failures. `NativeDependentCore` exposed a recursive constructor field
accepted by the wider function binder domain. `NativeDependentCoreRun`
and `RetainedQuantification` still expected rejection at an opaque
`Int` binder. The dependent directory recorded 309 passes and 17
unexpected failures. Polykinds recorded 227 passes, one expected
failure, and 15 unexpected failures.

At `7278fc1e52`, an ordinary client can pass a named callback to
`withPair` and infer its `Char` result. The solver compares the callback
bodies under one shared runtime binder of type `IO Int`. Inferring a
result that mentions that binder fails at the callback. The combined
compiler relink matched all 347 Showcase lines at `-O0`, default `-O1`,
and default `-O2`. Both source regressions and the ten focused kind and
matchability diagnostics passed.
[Source test](../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh)
and [quantifier display](roadmap/R23-unsaturated-type-families.md).

The full build of `4d5d6e467f` matched all 341 Showcase lines at both
compiler stages and all three optimization levels. Self-hosted GHCi
also matched. The focused tests recorded 72 passes and one missing-library
skip. The dependent directory recorded 306 passes and 20 unexpected
failures. Polykinds recorded 224 passes, one expected failure, and
18 unexpected failures. Both directories had the same failure names and
categories as the 337-line compiler.

At `7d04760583`, all 347 Showcase lines matched with the combined compiler
relink at `-O0`, default `-O1`, and default `-O2`. The registered vector
program also ran with Core and STG Lint. The proof at the expression
`Refl` failed with GHC-83865 when the two appended elements were swapped.
A separately compiled ordinary route function checks a result family
indexed by a user-defined stop type.
[Examples](Showcase.hs) and [indexed route](../../testsuite/tests/dependent/should_run/NativeClosedIndexedRoute.hs).

At `c753344c5b`, a compiler relink with the opaque-domain, beta-reduction,
and signature changes matched all 345 Showcase lines at `-O0`, default
`-O1`, and default `-O2`. Core and STG Lint passed.
`NativeRetainedPackage` checked direct vector binders, forwarding,
distinct vector identities and a named callback in an ordinary consumer.
A non-injective family in the callback signature still produced an
ambiguity error. `NativeFunctionIndex` matched its existing golden.
[Domain check](../../compiler/GHC/Core/Type.hs),
[source regression](../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh)
and [callback type](examples/packed-vector/DependentPair.hs).

At `a35653b580`, the public 341-line compiler relink matched all 344
Showcase lines at `-O0`, default `-O1`, and default `-O2`, with Core and
STG Lint. The registered source tests for the comparison and the split
matched their goldens. The source-hidden filter regression rejected all
nine incorrect evidence uses at their expected locations. The concrete
contents proof also passed the indexed-vector regression on self-hosted
337 at `-O0` and `-O1`. Swapping the result elements failed at `Refl`
with GHC-83865.
[Indexed-vector proof](../../testsuite/tests/dependent/should_run/NativeClosedIndexedVec.hs)
and [filter evidence](../../testsuite/tests/dependent/should_run/NativeOpenFilterGivenControls.hs).

The full build of `410405da2b` reproduced the 337-line Showcase with
both bootstrap and self-hosted GHC at all three optimization levels.
Self-hosted GHCi matched. The focused run had 68 passes and one
missing-library skip. The dependent directory recorded 303 passes and
20 unexpected failures. Polykinds recorded 224 passes, one expected
failure, and 18 unexpected failures.

At `4d5d6e467f`, we relinked six compiler modules against the 337-line
bootstrap compiler and used the 336 target libraries. All 341 Showcase
lines matched at `-O0`, default `-O1` and default `-O2` with Core and
STG Lint. The source regressions cover the captured cutoff, a closed
proof field under optimization, selection of either vector partition,
and partial family diagnostics after source expansion. Two diagnostic
goldens check the span after synonym expansion and a splice.
[Runtime predicate and demand](roadmap/R16-open-term-evaluator.md#capture-a-runtime-cutoff-in-the-predicate)
and [partition client](examples/packed-vector/ListVectorClient.hs).
At `3f51ce4e50`, bootstrap and self-hosted GHC reproduced all 340
Showcase lines at stripped `-O0` and default `-O1` and `-O2`.
Self-hosted GHCi matched. All 21 focused tests passed. Of 324 runnable
normal-way cases in the dependent directory, 304 passed. The other 20
had the same names and failure categories as at `a6fa7ededf`.

An optimized client used a type-applied rule on a runtime `Maybe` index
and a rule declared outside the retained function's module. The checked
type argument ran its Template Haskell finalizer once. A separate module
proved that the installed `Prelude.reverse` preserves the length of a
finite list. The append and reverse upgrade checks verified that a second
build reused each refreshed interface. An ordinary client also introduced
a nested existential package and opened it to return 17.
[Rules](roadmap/R05-dependent-core.md),
[reverse](roadmap/R16-open-term-evaluator.md) and
[packages](roadmap/R22-existential-quantification.md).

At `410405da2b`, the released 336-line compiler with the four changed
modules relinked reproduced all 337 Showcase lines at `-O0`, default
`-O1`, and default `-O2` with Core and STG Lint. The registered
`NativeRetainedPackage` test used stripped producer interfaces. It checked
the ordinary regroup operation, the demand on the elements, and both
incorrect length claims. The kind-error golden also matched the user-facing
matchability syntax. This run used the existing 336 target libraries.
[Source and regressions](../../testsuite/tests/dependent/should_run/NativeRetainedPackage.sh)
and [nested sum typing](roadmap/R04-dependent-sums.md#partition-at-the-original-length).

At `a6fa7ededf`, bootstrap and self-hosted GHC reproduced all 336
Showcase lines at stripped `-O0` and default `-O1` and `-O2`.
Self-hosted GHCi matched. All 19 focused normal-way tests passed. The
dependent directory ran 323 cases. It recorded 303 expected passes and
the same 20 unexpected failures, with the same failure categories, as
the 331-line checkpoint. The typed splice and retained-rule interface
checks passed. The compiler upgrade rechecked eligible append providers
at `-O0` and `-O1` and then reused their refreshed interfaces.

This checkpoint adds a computed length paired with a vector, recursive
family forwarding, and proofs of append's length law and map's
distributivity over append. A GADT field uses a runtime Boolean to
select its result type. The Showcase branches before calling the field
with the corresponding constructor.
[Append proof](roadmap/R16-open-term-evaluator.md#prove-that-append-adds-the-list-lengths),
[dependent pairs](roadmap/R04-dependent-sums.md) and
[GADT fields](roadmap/R19-type-level-cases.md).

At `5171866a01`, bootstrap and self-hosted GHC reproduced the 331-line
Showcase at stripped `-O0` and default `-O1` and `-O2`. Self-hosted
GHCi matched the output. All 16 focused normal-way tests passed. The
dependent directory ran 321 cases. It recorded 301 expected passes
and the same 20 unexpected failures as the 330-line checkpoint.
The retained-rule interface checks, typed splice, provider upgrade,
orphan-rule header and two-way filter upgrade also passed.

The compiler infers both binders of a rule from a retained signature.
Different branches can choose different callback witnesses by visible
type application. An unsigned recursive family can forward either a
constructor or a partial family. GHC also checks compound rank-n field
kinds and ordered type cases under retained GADT field binders.
[Rule inference](roadmap/R05-dependent-core.md),
[existential witnesses](roadmap/R22-existential-quantification.md) and
[family inference](roadmap/R23-unsaturated-type-families.md).

At `38c0f996f5`, the bootstrap and self-hosted compilers reproduced
the 330-line Showcase at stripped `-O0` and default `-O1` and `-O2`.
Self-hosted GHCi reproduced it too. All 15 focused normal-way tests passed.
Of the 321 runnable cases in the dependent directory, 301 passed and
20 failed unexpectedly. Each failing test and its failure category
matched the 321-line checkpoint.

An optimized client fired three retained rules from a separate interface.
The Boolean payload match used the same runtime index in its type and
its call. The preceding compiler rejected a closed retained rule at
each of the three interface compression levels. The class receiver and
field cases also passed their separate-client checks. A compiled typed
splice ran the nested vector matches and checked which fields they forced.
[Rule firing and interface checks](examples/qualified-pi/check-retained-rules.sh),
[class receiver](roadmap/R23-unsaturated-type-families.md) and
[type cases](roadmap/R19-type-level-cases.md).

At `65eebfca2d`, both compiler stages reproduced the 321-line Showcase
at `-O0`, default `-O1` and default `-O2`. The self-hosted GHCi matched
the same output. All 12 focused tests passed. In the dependent directory,
299 tests passed and 20 failed unexpectedly. The failing names and
categories match the 311-line checkpoint. A separately compiled typed
splice ran the nested vector patterns and preserved their demand.

The first integration run exposed three regressions in ordinary uses of
`id`. A saturated call now keeps its ordinary evaluator, including the
expected result kind of a nested `fromIntegral`. Passing `id` as a
callback still requires its checked equation. The existential regression
also checks the emitted orphan-rule header. The retained-provider rebuild,
bidirectional filter upgrade, and three interface compression levels passed.
[R16](roadmap/R16-open-term-evaluator.md),
[R19](roadmap/R19-type-level-cases.md) and
[imported rule checks](examples/erased-existentials/check-imported-rule-interfaces.sh).

At compiler source `c70b3be78f`, all 17 focused tests passed. The
dependent directory recorded 295 passes and 20 unexpected failures.
The failing names and categories match the 309-line checkpoint.
Both stages compiled the revised shared `size` proof and reproduced the
full 311-line Showcase at `-O0`, default `-O1` and default `-O2`.
The self-hosted compiler also reproduced it in GHCi.
[Shared size proof](examples/open-evaluation/PreludeMapLengthProof.hs),
[scoped signature](examples/open-evaluation/NativeCase.hs)
and [interface upgrade](examples/open-evaluation/check-filter-scope-upgrade.sh).

At `4bb020f702`, both stages reproduce the 305-line Showcase at `-O0`.
The self-hosted compiler also reproduces it in GHCi and at default
`-O1` and `-O2`. Two ordinary stream folds eliminate the intermediate
list. On a compiler upgrade a provider with a forwarded family and a
package rule referring to a private helper recompiles once and stays
up to date on the next build. All 61 runnable focused tests passed.
The dependent directory recorded 294 passes and 20 unexpected failures.
Polykinds recorded 224 passes, one expected failure and 18 unexpected
failures. The names and categories of all unexpected failures match
`b92cf1d999`.
[Stream source](examples/erased-existentials/StreamFold.hs)
and [interface checks](examples/erased-existentials/check-rule-interfaces.sh).

At `b92cf1d999`, the 304-line Showcase covers inference of earlier invisible
arguments. The previous compiler rejects the new family source with
GHC-91028. Both readers compile and run the ordinary client from the new
optimized interface. All 61 runnable focused tests passed. The dependent
directory had 294 passes and 20 unexpected failures. Polykinds had 224 passes,
one expected failure and 18 unexpected failures. The unexpected failures in
both directories match `7185bd64f8` by name and category. All 21 expected
passes in the impredicative suite passed. The linear suite had 106 passes and
the same seven unexpected failures as at `7185bd64f8`.
[Family source and client](roadmap/R23-unsaturated-type-families.md#inferring-a-kind-after-supplying-a-visible-argument).

At `7185bd64f8`, the 303-line Showcase adds `(Right 7,False)` and uses the direct
callback case. Both compiler stages reproduce the full output at
`-O0`. The self-hosted compiler also reproduces it at default `-O1`,
default `-O2` and in GHCi. The family upgrade rebuilds the provider
once and reuses it on the next build. The old and new compilers both
compile the ordinary client from the new interface. The R22 source
test checks witness escapes, linear use and the original GADT pattern.
[Showcase](Showcase.hs) and [family source and upgrade checks](roadmap/R23-unsaturated-type-families.md#compatibility-with-existing-haskell).

At the 302-line checkpoint (`b8cb91ff6f`), all 61 focused tests that ran
passed. The dependent directory recorded 294 passes and 20 unexpected
failures. Polykinds recorded 224 passes, one expected failure and 18
unexpected failures. Each unexpected failure had the same name and
failure category as at the 301-line checkpoint.

The 302-line Showcase adds `(4,3,6)` from the package rules. Both
compiler stages reproduce every line at `-O0`, default `-O1` and
default `-O2`. The self-hosted compiler also reproduces it in GHCi.
The Core matcher rejects substitutions that would capture an opened
witness or payload. The preceding compiler can still use older-style
rules from a new interface at each of the three compression levels.
[Showcase](Showcase.hs) and [interface check](examples/erased-existentials/check-rule-interfaces.sh).

At `73bffa1a15`, the 301-line Showcase includes a constructor match at an open family
index and a packaged lambda whose hidden witness is polymorphic.
Both compiler stages reproduce the expected output at `-O0`. The
self-hosted compiler also reproduces it in GHCi and at default `-O2`.
All nine focused tests passed. The dependent directory recorded 294
passes and 20 unexpected failures. Its failure summary matches the
preceding 299-line checkpoint. The split-interface and family-upgrade
checks passed. [Showcase source](Showcase.hs) and [run commands](README.md#run-the-showcase).

At `4b59f93deb`, both compiler stages reproduced the 299-line Showcase at `-O0`,
default `-O1` and default `-O2`. The self-hosted compiler also checks
stripped optimized interfaces and GHCi. Both this compiler and the
previous release, `7c873b5f46`, compile the ordinary client from the
new optimized interfaces. It prints `(Just (Just True),Just True)`.
The previous release rejects the interpreter's source.
[Showcase](Showcase.hs) and [commands](README.md#run-the-showcase).

At `55c20e158c`, all 61 focused tests that ran passed. In the dependent
directory, 294 passed and 20 failed. The unchanged `KindEqualities2`
now compiles and prints `Nothing` at `-O0` and `-O2`. In polykinds,
224 passed, one failed as expected, and 18 failed unexpectedly. In
indexed-types, 420 passed, four failed as expected, and 15 failed
unexpectedly. The remaining failures in all three directories have the
same names and failure categories as at `7c873b5f46`.

## Local integration checkpoints

These local refs preserve work that has separate acceptance checks. To
inspect one on the development machine, use `git show` with the full
ref or commit. The [roadmap](ROADMAP.md#where-this-fork-stands) identifies
which behavior is available in the published compiler.

At `bbfc594ab3`, the installed filter and the scoped signature cases
run together. Both stages reproduced the 309-line Showcase at `-O0`,
default `-O1`, and default `-O2`. The self-hosted compiler also
reproduced it in GHCi. All 16 focused tests passed. The dependent
directory recorded 294 passes and 20 unexpected failures.
[Interface upgrade](examples/open-evaluation/check-filter-scope-upgrade.sh).

At `1928e531f9`, a top-level signature can repeat the same type-level case
in its argument and result. A checked mapper can appear partially applied
with a named callback when its object is present or when only its interface
is available. A staged family can take its arrow kind from the caller.
Both compiler stages reproduced all 308 Showcase lines at `-O0`, default
`-O1`, and default `-O2`. The self-hosted compiler also reproduced them in
GHCi. All 15 focused tests passed. The dependent directory recorded 294
passes and the same 20 unexpected failures by name and category as
`4bb020f702`.
[Source case](examples/open-evaluation/NativeCase.hs),
[mapper](examples/open-evaluation/OpenMapSource.hs)
and [staged result](examples/open-evaluation/InterpretedKindUseSite.hs).

| Local ref under `refs/archive/dependent-haskell/` | Commit | What was checked |
| --- | --- | --- |
| `r04-abstract-projections` | `2fb04af78d` | A source-free client uses both general pair projections. The native Showcase runs 267 lines. Old witness workers rebuild once and then reuse their artifacts. |
| `r05-reviewed` | `2d95e2d36c` | Selected retained modules use the normal optimizer with stripped interfaces. The compiler and dependent tests from this stack were replayed onto the shared branch. |
| `r19-ordered-cases` | `9b967b37df` | Nested patterns, ordered demand, ordinary type constructor heads, reordered aliases, TH reification and exact printing have focused tests. |
| `r22-expressions` | `949e49d863` | Core Lint checks existential formation, introduction and elimination. A fresh reader loads the interface. Seven bytecode runtime and STG checks cover demand and erasure. |
| `r04-r23-reviewed` | `e6f21bdb98` | The combined bootstrap overlay runs 267 Showcase lines at both optimization levels. T7368 and T7368a match the ordinary kind-error goldens. An old abstract-pair worker rebuilds, then its outputs are reused. |
| `r19-checked-gadt` | `643e7e3641` | Ordered type patterns can contain alias-introduced constructors and a GADT index reduced by a completed closed family. The native case, source-free client, reification and exact-print checks pass on the private compiler. |
| `r22-source-inference` | `97994ac34b` | A provider exports an erased existential result. A separately compiled ordinary client demands the result. A TH splice recovers from the unsupported reification diagnostic. |
| `r22-source-pack` | `7f0214fe15` | A provider constructs and opens an existential package in source. Its ordinary interface-only client prints `True`. The private compiler passes the Core/STG erasure, demand, name resolution, quotation and scope checks with its marker package overlay. |

## Numeric indices

The packed-vector example accepts these signatures on the original GADT.

```haskell
three :: Vec Bool 3
three = sourceVector @Bool [True, False, True]

allTrue :: Vec Bool 3
allTrue = replicateNative 3 True

oneMore :: foreach (n :: Nat) -> Vec Bool (n + 1)
oneMore n = replicateNative (n + 1) True

appendOne :: Vec a n -> a -> Vec a (n + 1)
appendOne xs x = appendVec xs (VCons x VNil)
```

The library registers its Peano interpretation with
`GHC.TypeNats.Experimental.FromNatural`. Its `Num Nat` instance supplies
runtime numbers. The type operator and the `Num` addition method both use
the checked `plus` function. The [runnable example](Showcase.hs) reads
`2 :: Nat` from an `IORef` and appends three elements to that runtime length.
`replicateAt` takes a required type argument, so `replicateAt 3 True`
constructs a vector at exactly that length. For a runtime length `n`,
`replicateNative n True` constructs a vector indexed by that value.

For the retained literal in `allTrue`, the compiler checks the library's
`fromInteger` equations. Zero produces `Z`. A positive number produces one
`S` around the conversion of its predecessor. The typechecker can then use
the same constructors for the runtime argument and the vector's index.

The length itself can be a computation. Here the caller can get both
lengths from IO, and the result type records their sum.

```haskell
fillSum :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Bool (n + m)
fillSum n m = replicateNative (n + m) True
```

Here the body uses the ordinary overloaded `Prelude.(+)`. The vector
library defines `(+) = plus` in its `Num Nat` instance. Its interface
records that method definition. The typechecker checks that this call
uses the same dictionary before relying on the equations of `plus`.
An abstract `Num Nat` dictionary supplied by a caller cannot establish
those equations. The [Showcase](Showcase.hs) also passes an undefined
sum to a function that ignores its length.

Multiplication counts the cells in a rectangular vector. Enable
`NoStarIsType` to use `*` as a type operator. Here each row has
`cols` cells, so appending it to the remaining rows gives exactly
`cols + (remainingRows * cols)` cells.

```haskell
flattenRows :: Vec (Vec a cols) rows -> Vec a (rows * cols)
flattenRows VNil = VNil
flattenRows (VCons row rest) = appendVec row (flattenRows rest)
```

The library defines `times (S n) m = plus m (times n m)` and
`times Z m = Z`. GHC checks the successor branch of `flattenRows`
from those equations, even when both dimensions come from IO. An
ordinary expression can use the same multiplication, as in
`replicateNative (rows * cols) 'x'`.
An index type and a retained operand can combine the operators. For example,
this vector has a cell for the prefix and one for each cell of the rectangle:

```haskell
prefixed :: foreach (rows :: Nat) -> foreach (cols :: Nat) -> Vec Bool (1 + rows * cols)
prefixed rows cols = replicateNative (1 + rows * cols) True
```

At two rows and three columns the length is seven. Writing
`(1 + rows) * cols` gives nine. The first cell of `prefixed` is available
even if evaluating the product would raise an exception.
[Nested arithmetic](PLAN.md#m89-nested-computations-in-retained-operands).

You can name the product with an ordinary lazy `let`. The declared result
still uses the formula. The first cell remains available if a dimension
is undefined.

```haskell
namedPrefix :: foreach (rows :: Nat) -> foreach (cols :: Nat) -> Vec Char (1 + rows * cols)
namedPrefix rows cols =
    let cells = rows * cols
    in replicateNative (1 + cells) 'x'
```

GHC checks which `Num Nat` dictionary supplies `(*)` in the written RHS.
When its multiplication method is the checked `times` function, the body
can use `cells` at the length `rows * cols`. Two uses of `cells` share the
product. A caller can also write `let cells = times rows 3` and use it
at the length `rows * 3`. GHC checks the literal conversion as well.

An explicit `Num Nat =>` constraint can supply another multiplication.
The local arithmetic still works with that dictionary. Returning a vector
at the canonical product length then requires independent evidence.
[Dictionary selection in a let](PLAN.md#m91-dictionary-evidence-for-a-local-index-definition).

Local signatures and multiple bindings also work. GHC accepts this group
in either declaration order. The signature on the result still uses
`1 + rows * cols`.

```haskell
let total, cells :: Nat
    total = 1 + cells
    cells = rows * cols
in replicateNative total 'x'
```

The erased tail is inferred when a caller uses `safeHead` in the same
expression. The example also works with a fixed ordinary result:

```haskell
localHead :: foreach (rows :: Nat) -> foreach (cols :: Nat) -> Char
localHead rows cols = safeHead (replicateNative total 'x')
  where
    total, cells :: Nat
    total = 1 + cells
    cells = rows * cols
```

The caller need not know either runtime dimension. GHC checks the actual
vector type before accepting the call. A signature that takes a `Num Nat`
dictionary can still use a successor written as `S cells`. The caller's
addition method can give `1 + cells` a different result.
[Local inference and its evidence](PLAN.md#m94-infer-a-nonempty-tail-through-local-length-definitions).

The order of addition also affects evaluation. With an undefined `cells`,
`1 + cells` already has a successor constructor. Asking for the first
constructor of `cells + 1` evaluates `cells` and raises the exception.
GHC rejects a body that returns the second length when its signature
requires the first. [Diagnostic](../../testsuite/tests/dependent/should_fail/NativeTypeNumeralWrong.stderr).

## Reading ordinary lists at an indexed boundary

`fromListExact n xs` returns `Just` a vector of length `n` if the list
has exactly that many cells. A short or long list gives `Nothing`. This
lets an ordinary module use an API such as `zipVec`, which requires the
same length on both inputs.

```haskell
ordinaryZipExact :: Nat -> [a] -> [b] -> Maybe [(a, b)]
ordinaryZipExact n xs ys = do
    left <- fromListExact n xs
    right <- fromListExact n ys
    pure (toList (zipVec left right))
```

`n` can come from IO. The checks run when the result is demanded. GHC
checks at compile time that both successful calls use the same index.
If the caller accepts any length, `packList xs` packages the list's own
length with its vector. For example, `headPacked (packList ('p' : undefined))`
returns `Just 'p'` without inspecting the tail. To zip two such packages,
`zipPacked` compares their stored lengths and uses equality evidence in
the matching branch.
[Ordinary client](examples/packed-vector/OrdinaryVecClient.hs)
and [list conversion](examples/packed-vector/PackedVecLibrary.hs).

A partial constructor can prefix a successful checked row:

```haskell
prefixRowExact :: forall a. foreach (n :: Nat) -> a -> [a] -> Maybe (Vec a (1 + n))
prefixRowExact n x xs = VCons x <$> fromListExact n xs
```

The checked input determines the callback's tail index. The prefix `fmap`
form also works. With a runtime `n` of `2`, prefixing `'x'` to `"ab"`
gives `Just "xab"` after `toList`. A short input `"a"` gives `Nothing`.
GHC still checks the full callback type and its field multiplicities.
This bounded inference rule does not infer arbitrary callbacks.
[Partial constructor callbacks](PLAN.md#m96-infer-retained-indices-for-partial-constructor-callbacks).

## What happens on empty input?

```haskell
safeHead :: Vec a (1 + n) -> a
safeHead (VCons x _) = x
```

Addition inspects its first argument. Thus `1 + n` reduces to `S n`
even when the tail length comes from IO. The following call checks directly.
The vector produces its first cell even if evaluating `n` would raise an
exception.

```haskell
firstCopy :: foreach (n :: Nat) -> a -> a
firstCopy n x = safeHead (replicateNative (1 + n) x)
```

A list cell also determines its element type and proves the vector is
nonempty. The tail can come from IO or remain unevaluated.

```haskell
firstCell :: a -> [a] -> a
firstCell x xs = safeHead (sourceVector (x : xs))
```

GHC rejects `safeHead (sourceVector @Int [])` at the call site.

```text
Couldn't match type ‘Z’ with ‘S n0’
  Expected: Vec Int ((1 :: Nat) + n0)
    Actual: Vec Int (sourceLength '[])
```

For a list `xs` read from an `IORef`, GHC knows the vector's length is
`sourceLength xs` but cannot tell whether it is zero. A caller can handle
both possible values. The `VCons` pattern supplies the successor equality
that `safeHead` requires.

```haskell
case sourceVector xs of
    VNil          -> pure Nothing
    v@(VCons _ _) -> pure (Just (safeHead v))
```

The same distinction applies when an equation ignores the length.
In `keep _ xs = xs` at type `foreach (n :: Nat) -> Vec a n -> Vec a n`,
the input and output share the caller’s length. The wildcard leaves it
unnamed. An unused length can remain unevaluated under the default lazy
pattern semantics. [Retained wildcard patterns](PLAN.md#m86-wildcard-patterns-for-retained-arguments-and-fields).

A vector with length `n + 1` also has a head for each finite `n`.
Since addition recurses on the left, its first constructor stays unknown
until GHC has evidence about `n`. The library's `addSucc` proves
`n + 1` is a successor. The caller can pass zero to that proof using
the same retained numeric syntax.

```haskell
headRight :: foreach (n :: Nat) -> Vec a (n + 1) -> a
headRight n xs = case addSucc n 0 of
    Refl -> safeHead xs
```

Matching on the proof evaluates `addSucc` by induction on `n`. Matching
on `VCons` directly inspects just the outer cell.

The [rejection test](../../testsuite/tests/dependent/should_fail/NativeSourceLengthEmpty.hs)
covers empty, unknown and unreduced lengths. The [Showcase](Showcase.hs) runs
the two successful branches. If the input is already known to be
`True : xs`, the cons equation gives `S (sourceLength xs)`, which suffices
even if evaluating `xs` would diverge.

## Native Core implementation

`TermVarTy` refers to the index of an existing runtime identifier in a supported
closed algebraic domain, including Bool and user-defined Nat. It can also refer
to an eligible local function at its complete fixed U/Many type.
`DepFunTy` binds that identifier in a dependent result type. Type recovery and
Core Lint agree on retained application. The implementation includes scope,
capture avoidance, supported index substitution, alpha equality, and diagnostic
printing. It does not add a singleton argument or evaluate index variables.

`TermAppTy` also records scoped opaque calls in Core. Its head is an ordinary
local function between independently closed scalar domains. This form supports
forward congruence. It supplies no injectivity or function evaluation rule.
Source `case f n` now exposes this representation. See the
[M43 checkpoint](#m43-strict-opaque-call-refinement-from-source).

Boolean alternatives can bind checked nominal equality evidence. The strict case
justifies each proof. Proof-bearing cases must cover both Boolean values, and
each proof stays inside its branch. The existing result annotation scope remains.

The `NativeDependentCore` GHC API regression checks constructor and shared-variable
applications, substitution composition, binder cloning, invalid indices, scope
errors, and boundary gates. It also checks `Select True = Int`,
`Select False = Bool`, shared-index composition, branch casts, and forged proofs.

The restricted frontend produces native Core for retained functions. The
ordinary optimizer and native unfolding serialization remain gated. Program
compilation has an explicit checked entry for native signatures and ordinary
exported types. Signature validation is recorded below.
There is no performance measurement for the changed type traversals.

## Native execution

`hscCompileCoreExpr` now has a restricted native path. It skips the ordinary
simplifier. It runs `simpleOptNativeExpr` between mandatory Core Lint checks,
then checks Core again after CorePrep. CorePrep preserves native
types, clones proof types with their Boolean indices, and supports retained eta
binders. CoreToStg removes virtual Boolean proofs and casts. STG Lint checks the
runtime argument representations before bytecode generation. The normal pipeline
keeps its rejection gates. JavaScript execution remains unsupported.

`NativeDependentCoreRun` compiles each `choose` and shared `keep`/`choose` worker
once. An ordinary helper supplies both Boolean inputs through an IORef. The
results are `42` for `True` and `False` for `False`. The shared path uses an
explicit retained eta expansion. It does not add a singleton argument.

The laziness observations have separate types:

- `keepSelect bottom` reaches function WHNF. This test does not construct a
  non-bottom value of type `Select bottom`.
- A fully applied indexed `keep` returns `42` with a bottom Boolean. Its value has
  type `(b ~ b) => Int`. Ordinary reflexivity supplies the dictionary without
  evaluating `b`.
- A lazy local index can refer to a global Boolean thunk. CorePrep must preserve
  the local binding when its RHS cannot replace an index.
- `choose bottom` raises the named Boolean-bottom exception.

Prepared Core and STG inspection confirmed one Boolean argument for `choose` and
Boolean/value arguments for `keep`. The `keep` closure returns the value without
scrutinizing the Boolean. Index-only dependencies do not add runtime captures.
The branch proofs disappear from STG, and the strict Boolean cases remain.

Compiler source commit `5a636d9274` passed the final default Hadrian rebuild on
2026-11-12. The command used `-j6 --flavour=default --docs=none`. Hadrian reported
7m31s. The memory scope recorded a peak of 4,658 MiB under a 16 GiB cap with no
swap. This was an incremental rebuild.

The registered test run selected `NativeDependentCore NativeDependentCoreRun`
with `--test-way=normal`. Both tests passed. Twenty other ways were skipped, and
there were no unexpected failures or framework warnings. The test scope peaked
at 2,476 MiB under an 8 GiB cap. Both tests also passed against the bootstrap API
library; runtime execution used the matching target libraries and an external
interpreter.

Ordinary smoke checks passed package registration, `-O2 -dcore-lint`, Template
Haskell, threaded execution, GHCi, and Haddock startup. The standalone singleton
comparison and exact pinned `Data.Singletons` source passed at `-O0` and `-O2`
with Core Lint. The expected mismatch was rejected.

Hadrian reported a nonfatal configuration comparison between `lib` and `lib/.`.
It continued with its in-tree arguments. Package registration reported missing
documentation because documentation generation was disabled.

The full testsuite and documentation build have not run. This work establishes
bytecode execution for the restricted Core examples. It does not establish
ordinary optimizer preservation or the later source implementation. That
increment kept machine-code compilation behind the program gate. All commits
are local.

## Restricted simplification

The native entry first uses GHC's deep free-variable annotations to remove
unused lifted, unrestricted local bindings. References in surviving types,
coercions or stable metadata keep their bindings. The pass preserves all
other nodes and infers no new occurrence counts. See the
[M53 contract](CORE.md#native-dead-local-bindings).

The native SimpleOpt entry substitutes supported Boolean beta arguments in
terms and types together. Exact Boolean cases replace their virtual proof with
checked nominal reflexivity. Variable cases remain strict. Unsupported Boolean
producers keep their application redex. General inlining and occurrence analysis
remain disabled. The mode creates no rules, Core unfoldings, or join points.

The Core API checks now cover constructor and variable beta reduction, capture
avoidance, literal proof elimination, opaque indices with constructor unfoldings,
and retained binding metadata. Both bootstrap API and runtime checks passed.
The runtime checks include a local bottom index used only in erased annotations
and an unsupported producer application. Both reach function WHNF. Prepared
Core and STG retain the necessary scope without a runtime use of that index in
the function body. The dump run matched all 13 expected output lines.

Compiler source commit `0bbc32d396` passed the default Hadrian rebuild with
`-j6 --flavour=default --docs=none`. Hadrian reported 21m51s. The memory scope
recorded a peak of 7,726 MiB under a 16 GiB cap with no swap. This was an
incremental rebuild of both compiler stages and their dependencies.

The registered normal-way run passed both tests. Twenty other ways were skipped.
There were no unexpected failures or framework warnings. Its peak was 2,241 MiB
under an 8 GiB cap. The bootstrap API and runtime checks peaked at 907 MiB and
918 MiB. Ordinary `-O2 -dcore-lint`, Template Haskell, threaded execution, GHCi,
package registration, and Haddock startup passed. Both singleton comparisons
passed at `-O0` and `-O2` with Core Lint, including the expected rejection.

Unsupported compiler operations now report their phase with `Sorry`. The native
JavaScript entry uses this diagnostic instead of a compiler panic. The API checks
verify the exception class and phase. No JavaScript execution was attempted.

This increment does not enable the ordinary optimizer or establish the full M6
acceptance check. The full testsuite, documentation build, and performance checks
have not run.

The first source step uses one explicit retained variable pattern. Existing
pattern checking and desugaring preserve its `Unique`; `MatchGroupTc` needs no
new field for that subset. See [the frontend plan](CORE.md#frontend-identity-plan)
and [Agda provenance](REFERENCES.md#agda-provenance).

## Experimental source syntax

The local `ExperimentalDependentTypes` flag enables `foreach` in types. The AST
uses `HsForEachTy`, separate from erased `HsForAllTy`. The flag is disabled by
default and implies no other extensions. With the flag disabled, `foreach`
remains an ordinary identifier. The initial syntax increment rejected all retained
signatures. The checking increment below accepts a restricted subset.

The printers, exact printer, signature traversals, HIE traversal, and Haddock
visitors handle the new syntax. Haddock counts retained binders as runtime
arguments. The source regressions check the diagnostic and flag-disabled
compatibility. The existing API regression also checks flag parsing, the
distinct AST node, a printing round trip, brace rejection, and visible `forall`.
The printer regression checks ASCII pretty/exact round trips and Unicode exact
printing. The suite already records a Unicode pretty-printing limitation.

Compiler source commit `03c4343351` passed the final default Hadrian rebuild
with `-j6 --flavour=default --docs=none`. Hadrian reported 8m07s. The memory scope
recorded a peak of 6,659 MiB under a 16 GiB cap with no swap. Both compiler stages
and their dependencies built. The preceding complete rebuild took 26m56s and
exposed missing AST cases. The final rebuild contains those fixes and has no
new exhaustiveness warnings.

All five registered normal-way regressions passed: `NativeDependentCore`,
`NativeDependentCoreRun`, `RetainedKeywordOff`, `RetainedQuantification`, and
`RetainedPrint`. Four passed in the first run. The diagnostic passed after its
golden file matched the harness's no-caret format. The first run skipped 34
other ways. The scopes peaked at 2,320 MiB and 856 MiB under 8 GiB caps. Neither
run had framework failures or warnings. The runtime regression retained its 13
expected output lines, including the non-strict `keep` cases.

The bootstrap API check passed and peaked at 963 MiB under a 4 GiB cap. Separate
printer checks passed. Haddock generated HTML and source links for
`RetainedKeywordOff`; each `foreach` token kept identifier styling. That check
peaked at 181 MiB under a 4 GiB cap. Fourmolu parsing and Cabal-gild checks passed.
The compiler and Haddock files retain their existing formatting.

## Restricted source checking

The verified Boolean checkpoint accepts one explicit `foreach (b :: Bool)` binder at the start of a
complete monomorphic value signature. A dependent function has one equation with
a bare retained variable pattern, or one unguarded equation for each Boolean
constructor. Its result has lifted kind, and its outer match wrapper
must be identity. Calls accept Boolean constructors and named local Boolean Ids.
The first dependent result example is `Select b -> Select b`.

The renamer allocates one value `Name` and adds a scoped type-lookup alias. The
checker registers its fixed Bool/Many Id. For a variable equation, pattern
checking reuses the existing pattern Name. The runtime lambda and its result
type have the same Unique.
Zonking preserves the fixed index prototype. Looking up its runtime metadata
would force GHC's recursively tied Id environment. Runtime occurrences still
use ordinary zonking.

Validity preserves index scope through nested constraints and applies ordinary
rank rules. The first subset permits only reflexive Boolean equalities that
mention the index. Constraint instantiation creates their checked boxed
reflexivity evidence at the call site. This keeps the evidence inside the
runtime lambda. Other index-dependent predicates need a solver evidence scope.
Unsigned inferred aliases, Quick Look, index-bearing metavariable assignments,
and optimisation pragmas remain restricted. Dedicated rejection checks use
located GHC-83605 diagnostics. Ordinary payload mismatches use GHC-83865.

The existing runtime regression now parses, renames, checks, and desugars real
source. It retains module evidence bindings around an ordinary `entry :: IO String`
and runs that closed expression through native lowering. One source `keep`
receives both runtime Boolean values through an IORef and returns `42` for each.
The fully applied indexed call with a bottom Boolean also returns `42`.
The source family example returns `42` for `Select True` and `False` for
`Select False`. A partial `keep bottom` reaches function WHNF.

Initial source `keep` validation at `0781706b2e` passed six source observations
and twelve Core observations against the bootstrap API and rebuilt compiler.
All 27 source rejection cases checked the
diagnostic code, message, and source span. The opaque-index matching checks also
pass. The bootstrap API and runtime checks each used a 4 GiB cap and peaked at
913 MiB and 918 MiB. Core Lint and STG Lint remain mandatory in native lowering.

That default both-stage rebuild passed in 10 minutes 30 seconds with
`-j6 --flavour=default --docs=none`. It peaked at 6,055 MiB under a verified
16 GiB cap with swap disabled. All five registered normal-way regressions passed:
`NativeDependentCore`, `NativeDependentCoreRun`, `RetainedKeywordOff`,
`RetainedQuantification`, and `RetainedPrint`. The run skipped 34 other ways and
had no unexpected failures or framework warnings. It peaked at 2,403 MiB under
an 8 GiB cap. Fourmolu and Cabal-gild checks passed. Compiler files retain their
existing formatting.

Local commits separate opaque matching (`723a588ed5`), native checking support
(`eb29ee0a5d`), and source checking with regressions (`0781706b2e`). Reproduction
scripts, logs, and memory records are in the ignored `_build/verification/`
directory. The final runs used `build.sh` and `native-source-syntax-tests.sh`.

The constructor increment adds source `choose` and a constructor-based observer.
Each equation checks later argument patterns and its RHS against types with the
constructor substituted for the retained index. Explicit argument casts and
inverse result casts preserve the outer types. Generated typed `AsPat` nodes
retain one hidden runtime Id across both equations. The existing constructor
dictionary field binds each nominal proof inside its Core alternative.

Keep coercion patterns even for reflexive casts. The `Ignore b = Int` regression
checks that desugaring preserves the original indexed argument annotation.
Family normalisation keeps a runtime index opaque. The solver keeps its unknown
constructor equality potentially soluble, so PMC does not discard a live branch.
These rules add no singleton, equality axiom, or general runtime-index rewriting.

Validation at `dfe9637939` passes twelve source and twelve Core observations,
plus 35 located rejection cases. One source worker defines
`again b = keep b (choose b)` under the same unknown Boolean. Constructor
consumers return `42` and `False` for the
two inputs. A bang on the selected payload exposes `choose bottom`'s named
exception. A separate constrained-payload WHNF control leaves its Boolean
unforced. The bootstrap API and runtime checks peaked at 920 MiB and 915 MiB
under 4 GiB caps. The final registered run also verifies the named `again` worker.

The constructor increment's default both-stage rebuild passed in 14m36s with
`-j6 --flavour=default --docs=none`. Its peak was 7,007 MiB under a verified
16 GiB cap with swap disabled. All five registered normal-way regressions passed;
34 other ways were skipped. There were no unexpected failures or framework
warnings. The test scope peaked at 2,490 MiB under an 8 GiB cap with swap disabled.
Fourmolu parsing, fixture formatting, Cabal-gild, and local documentation link
checks passed. Compiler files retain their existing formatting.

Reproduction records use `native-source-choose-complete-build` and
`native-source-choose-registered` in `_build/verification/`. The registered run
uses `native-source-syntax-tests.sh` with the self-hosted stage1 compiler.

Dependent source `case` expressions remain unfinished. General runtime-index
inference, the ordinary dependent optimizer, and interfaces remain open. The full
testsuite, documentation build, and performance
checks have not run. See the
[frontend plan](CORE.md#frontend-identity-plan) and
[Agda provenance](REFERENCES.md#agda-provenance).

## Ordinary executable integration

The restricted module driver now accepts ordinary top-level types with local
retained workers. Use `-O0 -fomit-interface-pragmas -fno-write-if-simplified-core`.
It applies native SimpleOpt to each RHS and keeps top-level binding identities.
Core Lint runs around simplification, tidying, and program CorePrep. CorePrep
disables speculation and optional arity analysis, then skips its final ordinary
occurrence-analysis cleanup. CoreToStg erases the checked virtual proofs and
casts. STG Lint is mandatory before the Cmm backend.

`NativeDependentCoreRun` creates a fresh compiler context and a self-contained
`Main (main)` source file. It invokes normal module compilation and linking,
checks that an object file exists, and runs the resulting binary. Its six
observations cover both shared `choose`/`keep` results, partial and fully applied
non-strict `keep bottom`, the constrained-payload WHNF control, and strict
`choose bottom`. Eight negative cases require a phase-specific `Sorry`; a panic
or a generic failed load does not pass. These cover optimized options, interface
unfoldings, serialized Core, late cost centres, `INLINE`, `INLINABLE`, native
top-level signatures, and native-containing class-default methods. The bootstrap
API runtime scope passed and peaked at 921 MiB under a 4 GiB cap.

Compiler source commit `300eefffd1` passed the default both-stage Hadrian build
on 2026-11-12 with six workers and `--docs=none`. This incremental build took
9m41s and peaked at 5,330 MiB under a 16 GiB cap. The self-hosted stage1 CLI
then compiled, linked, and ran the same source. Its six observations passed;
the scope peaked at 115 MiB under 4 GiB. Final STG retains the Boolean argument
to `choose`, removes the virtual branch proofs and casts, and represents `keep`
as two arguments followed by the unchanged payload, without a case on the index.

All five registered normal-way tests passed: `NativeDependentCore`,
`NativeDependentCoreRun`, `RetainedKeywordOff`, `RetainedQuantification`, and
`RetainedPrint`. The run had 39 cases, with 34 skipped, no unexpected results,
and no framework warnings. It peaked at 2,517 MiB under an 8 GiB cap. Swap was
disabled for each capped run. The runtime fixture now covers 30 observations,
35 located source diagnostics, and the eight program rejection cases.

The ignored verification scripts are `build.sh`, `native-program-runtime.sh`,
`native-program-cli.sh`, and `native-source-syntax-tests.sh` under
`_build/verification/`. The executable checks use the native code generator.
The full testsuite, documentation build, and performance checks have not run.

That increment kept native top-level signatures gated. The signature increment
below removes that gate; native unfoldings remain unsupported.
Profiling, plugins, non-object module backends, and optimized module options
remain unsupported. Native-containing `INLINE`, `INLINABLE`, and class-default
unfoldings reject before desugaring can invoke ordinary optimization. Omitting
interface pragmas alone does not prevent those calls.

## Native interface signatures

The signature codec uses local interface Id binders and names, then reconstructs
fresh runtime identities on import. It has binary round-trip checks at all three
compression levels. A separate executable fixture imports a producer `.hi` in a
fresh one-shot compiler context and links its `.o`. Its consumer composes imported
`choose` and `keep` with one runtime Boolean and repeats the six program
observations above. It also checks concrete True/False applications, which need
the imported family equations. Native expression unfoldings remain gated.

The Boolean signature checkpoint pinned the fork to `10.1.20261112.1`. Its header version was
`1001202611121`, distinct from the previous `100120261109`. Matching libraries
must be rebuilt. The preserved old reader accepts its old-format control file
and rejects the new native interface with both `CheckHiWay` and `IgnoreHiWay`.
The new reader also rejects the old file. These are actual header reads,
including a native producer interface written by the rebuilt CLI.

Compiler sources at `b3b0a835c8` passed the default both-stage version rebuild
on 2026-11-12 in 29m56s, with a peak of 9,625 MiB under 16 GiB. A final
incremental pass included the late diagnostic detail and Note edits in both
stages; it passed in 7m54s and peaked at 4,700 MiB. Both runs used six workers,
`--docs=none`, and disabled swap.

The bootstrap API checks passed at all three compression levels, including
fresh binder identity, repeated references, tidied nested names, constructor and
caller substitution, malformed scopes and annotations, diagnostic-only forms,
and unknown tags. This scope peaked at 972 MiB under 4 GiB. The runtime scope
peaked at 913 MiB under 4 GiB. It covers 36 value/laziness observations plus two
concrete cross-module applications, 35 located source diagnostics, and eight
phase-specific program rejections.

Separate CLI processes compiled the producer and consumer, linked their objects,
and passed the same cross-module observations. `--show-iface` printed retained
signatures without unfoldings. This scope, including the version checks, peaked
at 93 MiB under 4 GiB. The ignored scripts are `native-iface-api.sh`,
`native-program-runtime.sh`, and `native-iface-cli.sh` in `_build/verification/`.

All five registered normal-way tests passed after the fixture-only correction
at `18fafdda4f`: 39 cases, 34 skipped, no unexpected results, and no framework
warnings. The run peaked at 2,277 MiB under 8 GiB. The initial run exposed an
inconsistent bytecode/executable-link request in the negative fixture. The fix
uses `NoLink` and retains the expected non-object phase rejection.

The ordinary compatibility smoke passed package checks, an optimized threaded
Template Haskell program using containers/text/bytestring, GHCi evaluation, and
Haddock startup. Its peak was 147 MiB under 4 GiB. Missing-documentation package
warnings are expected with `--docs=none`. Swap was disabled in each capped run.

The full testsuite, documentation build, performance checks, and native unfoldings
remain open. Signature fingerprint checks are recorded below. See the scoped
representation in [Core](CORE.md#native-interface-signatures) and the
[Agda provenance](REFERENCES.md#agda-provenance).

## Signature fingerprints and recompilation

The existing `NativeDependentCoreRun` regression now checks incremental consumer
compilation. It reads actual declaration and module ABI hashes from producer
interfaces. Rebuilding the same producer with fresh runtime Ids preserves these
hashes. Changing only `choose True` from `42` to `43` also preserves them when
unfoldings are omitted. In both cases an unchanged consumer skips code generation;
fixed earlier object and interface timestamps make that check independent of
filesystem timestamp resolution.

The producer then changes `Select True` from `Int` to `Bool` and updates its
payloads. The `choose` and `keep` signature text stays unchanged. Their declaration
hashes and the module ABI change. The `keepIndependent` hash stays unchanged.
The consumer imports only `choose` and `keep`, with no explicit `Select` use.
Recompiling without `-fforce-recomp` rejects its stale `Int` use with located
`GHC-83865`, including the `Int`, `Bool`, and `choose` context. Fresh one-shot
contexts read only producer interfaces; producer sources stay outside the import
path. These checks pass through the bootstrap API, at 917 MiB under 4 GiB with
swap disabled.

All five registered normal-way tests also pass: 39 cases, 34 skipped, no
unexpected results, and no framework warnings. Hadrian reported 1m11s; the
scope peaked at 2,558 MiB under 8 GiB with swap disabled. The fixture remains
inside the existing regression; the compiler sources did not change. The
ignored scripts are `native-recompilation-runtime.sh` and
`native-source-choose-registered.py` in `_build/verification/`. Their logs and
peak records use the `native-recompilation` prefix. Fourmolu, Cabal-gild parsing,
whitespace checks, and local documentation link checks pass.

The test Note records these controls and links the existing GHC fingerprinting
Note. The [Agda import-validation comparison](REFERENCES.md#agda-provenance)
records the shared dependency check and GHC's distinct declaration ABI policy.
Normal module unfolding generation remains gated. The expression-table and
declaration-reconstruction increment below supplies the checked template path.
See [the remaining M8 work](PLAN.md#m8-support-interfaces-and-validate-the-compiler-change).

## Native expression templates

At `bb998056d5`, expression tables reconstruct fresh Boolean branch proofs from
the scrutinee and constructor. Imported native templates run mandatory Core Lint
and match their declared types inside deferred RHS reconstruction. The unfolding
builder preserves runtime index bindings, native proofs, and native stable
metadata. It leaves unused unstable templates deferred and preserves the wire
cache. Native rule/dictionary payloads reject before ordinary occurrence analysis.

The regression covers all three compression levels, variable and literal proof
cases, ordinary Boolean cases, DEFAULT coverage, index-only lets, fresh proof
identity, scope, malformed annotations, both signature-mismatch directions,
named DEFAULT/literal binders, native rule/dictionary gates, and lazy metadata.
It imports actual `IfaceId` declarations and forces their templates with optional
Core Lint disabled. These are binary-table and declaration checks; they do not
establish real native unfolding files or cross-module inlining.

Both-stage default builds pass with six workers, `--docs=none`, and swap disabled.
The first pass took 9m44s and peaked at 5,417 MiB under 16 GiB. The final alignment
at `bb998056d5` took 8m28s and peaked at 4,946 MiB. The target API run peaked at
832 MiB under 4 GiB. The aligned bootstrap API run peaked at 987 MiB under 4 GiB.

All five registered normal-way tests pass: 39 cases, 34 skipped, no unexpected
results, and no framework warnings. Hadrian reported 1m09s; the scope peaked at
2,493 MiB under 8 GiB. The ordinary optimized/threaded TH, package, GHCi, and
Haddock smoke also passes, at 145 MiB under 4 GiB. Swap was disabled in each scope.
The full testsuite, documentation build, and performance measurements have not run.

Ignored logs and peak records use the `native-expr` prefix under
`_build/verification/`. Scripts include `build.sh`, `native-expr-self-api.sh`,
`native-iface-api.sh`, `native-source-choose-registered.py`, and
`native-iface-smoke.sh`. Compiler formatting checks are nonmutating parses;
the fixture passes canonical Fourmolu. Cabal-gild, diff, and local link checks pass.
See [the representation and checks](CORE.md#native-expression-templates).

A statically linked header-only reader and native Boolean control interface are
preserved in `_build/verification/interface-bool-v1/`. The reader accepts the
`.1` control with both `CheckHiWay` and `IgnoreHiWay`. Its preservation scope
peaked at 848 MiB under 4 GiB. `SHA256SUMS` records both artifacts. Keep this
binary for the next format's rejection check; do not rebuild it after the bump.
Rejection of the future format has not been tested yet.

## Core naturals and vectors

Closed first-order algebraic domains now work as native Core indices. The domain
check follows actual TyCon and DataCon structure. It accepts recursive and
mutually recursive domains. It rejects parameters, existentials, constraints,
newtypes, unsupported fields, and constructor wrappers.
The user-defined `Nat` has no wired compiler identity. Constructor indices use
the ordinary promoted constructors from DataKinds.

The existing API fixture checks Nat kinding, saturated constructor indices,
substitution, capture avoidance, closed `Add` reduction, and field-scoped branch
proofs. A native `S` alternative binds its runtime predecessor before its virtual
proof. Core Lint checks both. Backend lowering keeps the predecessor and removes
only the proof. Opaque indices remain opaque even when their unfoldings contain
known constructors. Apartness does not assume that every Nat is finite.

The runtime fixture loads ordinary source definitions of `Nat`, `Vec`, `Add`,
and a GADT append helper. It constructs checked native Core for recursive
`replicate`, `head`, and a retained append wrapper. One worker reads unknown
lengths from IORefs and uses the same runtime Ids across producers and consumers.
Lengths two and three produce `[42,42,7,7,7]` with observed length five.

The same fixture checks empty inputs and unused bottom elements. A replication
at `S bottomNat` produces a first element without forcing its predecessor.
Forcing the remaining spine raises the named Nat exception. A cyclic Nat permits
a finite vector prefix. `head bottomNat (VCons 42 bottomVec)` returns `42`.
Fully applying append to bottom indices and vectors raises the vector exception,
which checks that append does not force its retained lengths first.

These Core programs pass through the checked bytecode entry. The source fixture
below separately validates source elaboration, machine code, and Nat interface
imports through the bootstrap API, registered self-hosted tests, and separate CLI
processes. The checker and signature reconstruction use the structural domain rule.
At this Core checkpoint, CorePrep rejected retained constructor-tree arguments
with fields. Naming `S k` as an opaque local Id needs checked index transport.
The current implementation is described under constructor-argument transport.

The implementation checkpoint is `df623104fd`. Both-stage default builds pass
with six workers and `--docs=none`. The final incremental alignment took
16m07s and peaked at 7,344 MiB under 16 GiB. Bootstrap
execution passed at 966 MiB under 4 GiB. Swap was disabled in both scopes.

All five registered normal-way tests pass: 39 cases, 34 skipped, no unexpected
results, and no framework warnings. Hadrian reported 1m35s. The scope peaked at
2,557 MiB under 8 GiB, with swap disabled. This includes the final Nat substitution
and field-capture checks and the full runtime fixture.

The ordinary optimized/threaded TH, package, GHCi, and Haddock smoke also passes
at 145 MiB under 4 GiB, with swap disabled. Compiler sources pass nonmutating
Fourmolu parses. Both fixtures pass canonical Fourmolu. Cabal-gild, diff, and
local document link checks pass. The repository merge-request template requires
the focused regressions. The full testsuite, documentation build, and performance
measurements have not run.

Logs and peak records use the `native-nat` prefix in `_build/verification/`.

## Source naturals and vectors

The [M9 target](PLAN.md#m9-runtime-natural-numbers-and-indexed-vectors) is a
user-defined Peano Nat with `replicate`, `append`, and `head` for indexed vectors.
Runtime lengths become native indices directly. `replicate` inspects a length;
`append` can use an ordinary GADT helper without forcing its retained lengths.
The [pinned Agda definitions](REFERENCES.md#agda-provenance) supply the typing
and recursion comparison. Generic algebraic Core support is implemented.
Source checkpoint `714689a2fa` checks and executes these definitions:

```haskell
replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
replicateNative Z x = VNil
replicateNative (S k) x = VCons x (replicateNative k x)

headNative :: forall a. foreach (n :: Nat) -> Vec a (S n) -> a
headNative n (VCons x _) = x

appendNative :: forall a. foreach (n :: Nat) (m :: Nat)
             -> Vec a n -> Vec a m -> Vec a (Add n m)
appendNative n m xs ys = appendVec xs ys
```

`appendVec` is the ordinary polymorphic GADT helper in the M9 target.
An IORef supplies runtime lengths 0, 2, and 3. The source regression checks
replication, four append pairs, and head at both `Int` and `Bool`. It also checks
partial and cyclic Nat values, bottom tails, and unused bottom indices.
Wrong lengths, unsupported field patterns, and an ambiguous erased payload
produce located diagnostics. No singleton conversion or wired Nat is used.

A producer owns the actual Nat/Vec declarations and exports the retained
functions. A fresh one-shot consumer reads only its interface and object.
The linked executable repeats the runtime length and laziness observations.
The bootstrap API, registered self-hosted fixture, and separate CLI processes
pass these checks. The CLI writes the producer interface, imports it, and links
the object in separate processes. A preserved Boolean-only reader accepts its
old control and rejects the new interface with both way settings. The new reader
rejects the old format before type decoding. Its saved binary and control file
retain their original hashes.

The audit identifies four preservation requirements: field binders before proofs
that mention them, scoped GADT index instantiation, retained telescopes, and
CorePrep transport when naming constructor arguments. M9 records source-level
and cross-module acceptance checks, including partial and cyclic Nat values.
Direct constructor-tree arguments now use checked CorePrep transport.
Constructor equations currently
split one leading retained index. Variable equations can bind several retained
indices. Source `case` refinement and broader inference remain separate work.
Producers and consumers must use the same named index. Its defining expression
does not establish an index equality.

On 2026-11-13, the full both-stage build passed in 15m26s, peaking at 6,616 MiB
under 16 GiB. The final diagnostic synchronization passed in 8m32s at 5,244 MiB.
Both used six workers, `--docs=none`, and no swap. The five registered normal-way
tests passed across the initial run and focused rerun. The latter corrected the
domain-error span and the expected rigid-variable diagnostic, without compiler
changes. The test scopes peaked at 2,546 MiB and 1,704 MiB under 8 GiB.
Separate CLI execution checks ten observations and both version-mismatch
directions. Ordinary optimized code, Template Haskell, GHCi, package-database,
and Haddock startup smoke checks also pass. Repository contribution guidance
requires regressions; this change extends the existing API fixtures and golden.
The guide's exact Nat/Vec example also compiles with Core and STG Lint.

## Constructor-argument transport

The implementation checkpoint is `a2f6553bdf`.

CorePrep accepts direct `S n` and nested `S (S n)` retained arguments. It names
constructor fields recursively and composes checked nominal equalities. It
transports later indexed payloads and the result across the supplied telescope.
The proof cases preserve lazy fields. CoreToStg erases the virtual evidence.
No singleton witness, unfolding equality, or universal coercion supplies it.
See [CORE.md](CORE.md) and [the provenance](REFERENCES.md).

The source checks cover `replicateNative (S n) x`,
`headNative n (replicateNative (S n) x)`, and two-index append with constructed
arguments. Partial and cyclic fields preserve finite prefixes. A head consumer
does not force the unused predecessor. Forcing a bottom function still raises.

On 2026-11-13, both compiler stages built in 8m58s, peaking at 5,600 MiB under
16 GiB. The five registered normal-way tests passed in 1m10s at 2,439 MiB under
8 GiB: 39 cases, 34 skipped, no unexpected results or framework warnings.
Bootstrap Core/API and runtime checks passed. A separate CLI producer and
interface-only consumer passed 19 observations. Both interface-format rejection
directions passed. The exact guide example and ordinary optimized TH, GHCi,
package, and Haddock smoke passed. All scopes disabled swap. The CLI, guide,
and ordinary smoke scope peaked at 162 MiB under 4 GiB.

At this checkpoint, dependent-function congruence and dependent erased binder
kinds were gated. The `lazy`/`noinline`/`nospec` stripping path can bypass preprocessing
and still reject a constructed retained argument. Known partial-function heads
now pass PMC as described below. Source `case` refinement and native unfolding
files remain open.
These checks do not establish a general preservation theorem. The full
testsuite, documentation build, and performance measurements have not run.

Logs use the `native-alias` prefix in `_build/verification/`. Compiler sources
pass nonmutating Fourmolu parses. Both existing fixtures pass canonical
Fourmolu. Cabal-gild, diff, and local document link checks pass. The repository
merge-request template requires the focused regressions.

## PMC and retained function heads

The implementation checkpoint is `0335c399f3`.

Strict wildcard matching on a partial retained function now passes the source
pipeline. With a local runtime index `n`, `case appendNative @Int n of !_ -> ...`
does not force the unused index or the function body. The tests cover IORef
inputs zero, two, and three, bottom and cyclic indices, and a canonical mixed
ordinary/retained telescope. An empty match on a function type reports located
GHC-62161 under `-Werror=incomplete-patterns`.

PMC preserves a known function head without rewriting its domain and codomain.
The immutable matcher can bind a whole retained type to a template variable
under its usual scope and occurs checks. Unsupported comparisons produce
`MaybeApart MARRetainedFunction`. Alpha-equivalent retained types without free
TyCoVars can compare reflexively. At this checkpoint, Pi congruence and general
matching under a retained binder were open. See [the Agda WHNF provenance](REFERENCES.md) and Notes
[Coverage checking retained functions] and [Matching retained function types].

The new source checks preserve possible function inhabitation. They do not
assume a function expression is evaluated. At this checkpoint, constructed partial
applications such as `appendNative (S n)` reported the CorePrep result-congruence restriction.
The former PMC and immutable-matcher panics no longer obscure that boundary.

The both-stage build passed in 9m21s, peaking at 6,804 MiB under 16 GiB. All five
registered normal-way tests passed in 1m31s at 2,593 MiB under 8 GiB, with no
unexpected results or framework warnings. Bootstrap matching and source runtime
checks passed. The finalized runtime scope peaked at 972 MiB under 4 GiB.
A separate CLI producer and interface-only consumer passed 21 observations.
CLI checks also confirmed the located empty-case diagnostic and the named
constructed partial-result restriction, with neither reported as a panic.
Both format-rejection directions, the guide example, and ordinary compiler
smoke passed. The final CLI and smoke scope peaked at 165 MiB under 4 GiB.
All scopes disabled swap. Compiler sources passed nonmutating Fourmolu parses,
both existing fixtures passed canonical Fourmolu, and cabal-gild passed.
Logs use the `native-pmc` prefix in `_build/verification/`.

## Source case refinement

At `3621781dc4`, expected-type-directed source `case` expressions reuse the native
constructor proofs. For example:

```haskell
replicateCaseNative :: forall a. foreach (n :: Nat) -> a -> Vec a n
replicateCaseNative n x = case n of
  Z -> VNil
  S k -> VCons x (replicateCaseNative k x)
```

The expected result supplies the motive. Referenced locals such as
`xs :: Vec a n` become fresh arguments of the generated retained pattern
function. This refines their types together with the result. Constructor
substitution also refines direct uses of the original index. Resolved Names
preserve field shadowing. The elaboration adds no Core form, singleton argument,
interface codec, or unfolding equality. See Note [Source retained case refinement]
and [the Agda provenance](REFERENCES.md).

The rule requires a named local scrutinee, complete unguarded constructor
branches, bare fields, unrestricted captures, and a lifted checked result.
Parentheses are accepted.
Unsupported clause shapes and quotations report located GHC-83605. Incorrect
vector lengths use the existing type mismatch. Ordinary cases keep their
existing checker. General inference, guards, defaults, and nested patterns remain
open. This checkpoint preceded fixed-domain retained congruence.

The bootstrap Core and runtime checks pass. The new source matrix checks
recursive vectors, captured Boolean payloads, direct index substitution, field
shadowing, partial and cyclic inputs, and function-result strictness. It checks
six located rejection cases. The standalone executable has nine observations;
the separate producer and interface-only consumer have 24. The successful
runtime scope peaked at 973 MiB under 4 GiB, with swap disabled.

The both-stage build passed in 12m56s at 6,934 MiB under 16 GiB. All five
registered normal-way regressions passed: 39 cases, 34 skipped, no unexpected
results or framework warnings. The test command took 8m13s, including a fresh
bootstrap prerequisite rebuild, and peaked at 5,731 MiB under 8 GiB. The test
driver itself took eight seconds. Both scopes disabled swap.

Separate CLI processes passed the 24-observation interface-only consumer.
Four additional machine-code observations check function-result strictness and
payload laziness. An unsupported constructor field reports GHC-83605 at its
source location. At this checkpoint, the previous empty-case diagnostic,
constructed partial-result restriction, and both interface format-rejection
directions passed.
The updated guide example compiles with Core and STG Lint. Ordinary optimized
Template Haskell, package, GHCi, and Haddock smoke checks pass. The guide and
ordinary smoke scope peaked at 149 MiB under 4 GiB, with swap disabled.

When checking exceptions from `IO String`, force the result inside `try`.
`observeInt x = pure (show x)` can return a lazy string before it forces `x`.
The CLI strictness harness now checks that evaluation inside the protected
action. Core and final STG dumps also preserve payload inspection inside the
returned lambda. Compiler sources passed nonmutating Fourmolu parses, both
existing fixtures passed canonical Fourmolu, and cabal-gild passed.
Logs use `native-case` in `_build/verification/`. Generated bootstrap artifacts
are retained with hashes in `native-case-bootstrap-work/` under that directory.
The signature format is unchanged.

## Fixed-domain retained congruence

On 2026-11-13, `DepFunCo` adds checked coercion transport under an unchanged
closed algebraic domain and `Many` runtime binder. The binder scopes over the
whole proof, including intermediate types absent from the final endpoints.
Each endpoint independently canonicalizes a vacuous retained binder to an
ordinary arrow. See [the Core rule](CORE.md#retained-congruence-and-nominal-domain-conversion)
and [the Agda and D/DC provenance](REFERENCES.md#agda-provenance).

A complete signature can now check a definition with one source pattern and
a remaining retained function result:

```haskell
appendAfterSuccessor :: foreach (n :: Nat) (m :: Nat)
                     -> Vec Int (S n) -> Vec Int m -> Vec Int (Add (S n) m)
appendAfterSuccessor n = appendNative @Int (S n)
```

Later applications preserve the actual runtime indices and indexed payloads.
Unsigned let aliases still report located GHC-83605 and require a complete
signature. This increment does not enable retained inference.

Bootstrap kernel and runtime checks pass. The kernel covers roles, endpoints,
scope, capture avoidance, one-sided endpoint dependence, proof-only dependence,
and malformed binders or result kinds. Runtime checks apply saved Core partial
functions to IORef inputs zero, two, and three. They check function WHNF,
bottom and cyclic indices, and delayed payload evaluation. The source definition
above and its later applications pass. The bootstrap interface-only consumer
now checks 26 observations. The logs are `native-pico-api-v2.log` and
`native-pico-runtime-v2.log` in `_build/verification/`.

The both-stage build passed in 17m14s at 7,442 MiB under 16 GiB. All five
registered normal-way regressions passed: 39 cases, 34 skipped, no unexpected
results or framework warnings. The test command took 1m36s, including helper
builds, at 2,531 MiB under 8 GiB. The test driver took eight seconds.

Separate CLI processes passed the 26-observation interface-only consumer,
eight constructed partial/application observations, and nine standalone
observations. Four additional observations check function-result strictness and
payload laziness. Located GHC-83605, the empty-case diagnostic, both interface
format-rejection directions, the exact guide example, and ordinary optimized
Template Haskell, package, GHCi, and Haddock smoke checks passed. The serial
CLI, guide, and smoke scopes peaked at 146 MiB under 4 GiB. All scopes disabled
swap. Compiler sources passed nonmutating Fourmolu parses. Both fixtures passed
canonical Fourmolu, and cabal-gild passed.

Logs use `native-pico` in `_build/verification/`. Bootstrap-generated files and
their hashes are retained in `native-pico-bootstrap-work/` there.
The signature format is unchanged. Native coercion
serialization in unfolding bodies, structural Pi matching and ambient-evidence normalization,
heterogeneous domains, changed multiplicities, and the ordinary optimizer remain
open. Non-reflexive eta-cast handling also remains gated. Value casts preserve
Haskell forcing behavior; no function eta law is assumed.

## Family-axiom normalization

The next increment reduces family axioms under a retained binder with a fixed
closed algebraic domain and `Many` multiplicity. `mkDepFunRedn` preserves scoped
coercion evidence and separately canonicalizes each endpoint. The Core lifting
context permits shared valid same-domain index alpha-renaming. The solver keeps
global family reduction, ordinary meta-variable reading, roles, depth limits,
and locations. Its binder-local mode suppresses plugins, inert evidence,
family-cache reads and writes, and coercion zapping. Ordinary erased-forall
binder-kind handling is unchanged. Family axioms reduce at nominal and
representational roles; phantom family heads remain unreduced. Ordinary forall
binder kinds normalize at nominal role. See [the contract](CORE.md#family-axiom-normalization-under-retained-binders)
and [the Agda sources](REFERENCES.md#agda-provenance).

New source checks target complete signatures with no source argument patterns:

```haskell
leftUnit :: foreach (n :: Nat) -> Vec Int (Add Z n) -> Vec Int n
leftUnit = nativeId

leftUnitAppend :: foreach (n :: Nat) (m :: Nat)
               -> Vec Int (Add Z n) -> Vec Int m -> Vec Int (Add n m)
leftUnitAppend = appendNative @Int
```

Here `nativeId n xs = xs` has an explicit retained identity signature. A further
case reduces `ConstantNat n = Int`, so `constantAlias = ordinaryNat` checks
against a signature that becomes an ordinary `Nat -> Int` arrow. These checks
do not enable unsigned retained aliases or general structural unification.
Runtime indices stay opaque, including bottom and cyclic values.

The equality canonicalizer rechecks reflexivity after both endpoints have been
rewritten. Rewriting returns to `can_eq_nc` without repeating `zonkEqTypes`.
Same-Id runtime indices and `tcEqType`-equal retained functions use the existing
reflexivity rule. The rewriting evidence already relates the original and
normalized endpoints. This step adds no structural Pi decomposition or
meta-variable assignment. See Note [Reflexivity after retained normalization].

The initial bootstrap compiler build took 2m10s. Bootstrap API v3 and runtime v3 pass.
The runtime matrix checks 17 dedicated source observations and 38 imported
observations: the previous 26 plus 12 normalization observations. It also checks
the opaque right-unit mismatch at GHC-83865 and reduction-depth error GHC-40404.
The kernel checks roles and binder freshening. The successful logs are
`native-pinorm-api-v3.log` and `native-pinorm-runtime-v3.log` in
`_build/verification/`.

The both-stage build passed in 11m11s at 6,188 MiB under 16 GiB. All five
registered normal-way regressions passed: 39 cases, 34 skipped, no unexpected
results or framework warnings. The test command took 1m33s, including helper
builds, at 2,390 MiB under 8 GiB. The test driver took eight seconds.

Separate CLI processes passed the 38-observation interface-only consumer,
eight constructed partial/application observations, and nine standalone
observations. Four additional observations check function-result strictness
and payload laziness. The right-unit GHC-83865 and depth-limit GHC-40404 point
to their actual RHS lines. The ordinary type-error control, empty-case error,
located GHC-83605, and both interface format-rejection directions passed.
All three Nat/Vec guide blocks compiled with Core and STG Lint. Ordinary
optimized Template Haskell, package, GHCi, and Haddock smoke checks passed.
The serial CLI, guide, and smoke scopes peaked at 147 MiB under 4 GiB.
All scopes disabled swap. Compiler sources passed nonmutating Fourmolu parses.
Both existing fixtures passed canonical Fourmolu, and cabal-gild passed.

Logs use `native-pinorm` in `_build/verification/`. Bootstrap-generated files
and their hashes are retained in `native-pinorm-bootstrap-work/` there.
Full normalization with ambient Given or plugin evidence remains future work.
The signature format is unchanged.

## Arithmetic equality proofs

At `89c5ce92bf`, the source subset proves an arithmetic property of an unknown runtime
Nat and uses it to transport a vector. Enable `TypeOperators` and import
`Data.Type.Equality ((:~:)(Refl))`. Use the existing `Nat`, `Vec`, `Add`, and
`appendNative` definitions:

```haskell
rightZero :: foreach (n :: Nat) -> Add n Z :~: n
rightZero Z = Refl
rightZero (S k) = case rightZero k of Refl -> Refl

rightUnit :: foreach (n :: Nat) -> Vec Int (Add n Z) -> Vec Int n
rightUnit n xs = case rightZero n of Refl -> xs

appendRightUnit :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
appendRightUnit n xs = rightUnit n (appendNative @Int n Z xs VNil)
```

`Add Z n` reduces directly. `Add n Z` at an unknown `n` needs the induction
proof. The successor branch uses the recursive equality under `Refl`; the
checker combines it with successor congruence and the Add equation. An ordinary
`congruentSuccessor Refl = Refl` helper can express the same step. This follows
[Agda's right-identity proof and transport](REFERENCES.md#arithmetic-equality-proofs)
and the existing Haskell equality datatype. The native function receives the
actual Nat, with no singleton wrapper.

The constructor checker now selects immutable type arguments from the expected
result before ordinary subsumption proves the full equality. This permits the
direct recursive `Refl` spelling. The fallback applies to supported data
constructor heads. It does not assign retained syntax to a meta-variable.
See [the checking and evaluation contract](CORE.md#arithmetic-equality-proofs).

Matching `Refl` forces the proof. The recursive proof can force the complete Nat
spine. An ignored proof remains lazy; a bottom proof establishes no equality.
The transport therefore has runtime proof work. GHC does not check termination.
The fixture checks finite unknown lengths 0, 2, and 3, ignored bottom and cyclic
proofs, lazy partial applications, proof-before-payload failure, and rejection
when a supplied proof is not matched or its equality leaves its branch.

The focused bootstrap compiler check passes direct and congruence proofs,
vector transport, and a user-defined equality GADT with Core and STG Lint.
Unproved successor `Refl` and `n :~: S n` fail with GHC-83865 at the actual RHS.
The both-stage build passed in 7m58s at 5,191 MiB under 16 GiB. All five
registered normal-way regressions passed: 39 cases, 34 skipped, no unexpected
results or framework warnings. The command took 1m11s at 2,419 MiB under 8 GiB.
The test driver took 8.5 seconds. The bootstrap API and expanded runtime fixture
pass together at 980 MiB under 4 GiB.

The runtime fixture checks 17 dedicated proof observations, five exception-order
cases, and four GHC-83865 rejection controls. The interface-only consumer now
has 55 observations: the previous 38 plus the same 17 proof observations.
Separate CLI processes pass all 55, five proof exception-order observations,
the user-defined equality GADT, and four proof negatives at their actual RHS
lines. The earlier eight partial/application observations, nine standalone
observations, four source-case strictness observations, ordinary diagnostics,
and both interface format-rejection directions still pass.

All five literal Nat/Vec guide blocks compile together with Core and STG Lint.
Ordinary optimized Template Haskell, package, GHCi, and Haddock smoke checks pass.
The final serial CLI, guide, and smoke scope peaked at 157 MiB under 4 GiB.
All build and verification scopes disabled swap. Both existing fixtures pass
canonical Fourmolu. The compiler source passes a nonmutating Fourmolu parse,
and cabal-gild passes.

At this checkpoint, generic `forall a. a -> ...` argument inference rejected a
whole type that contained a retained index. The next increment below accepts
named argument spines. General retained inference and ambient-evidence
normalization under Pi remain open.

Successful logs use `native-proof-api`, `native-proof-runtime`,
`native-proof-registered`, `native-proof-cli`, `native-proof-extra-cli`,
`native-proof-runtime-cli`, `native-proof-guide`, and `native-proof-smoke` in
`_build/verification/`. The final CLI scope log is
`native-proof-cli-after-import-fix.log`. The first wrapper stopped at an import
collision in its standalone control; the corrected control hides the producer's
proof names. Bootstrap-generated files and their hashes are retained in
`native-proof-bootstrap-work/`. The signature format is unchanged.

## Generic retained consumers

At `c90959323a`, ordinary polymorphic functions can consume named values whose types contain
runtime indices. The inferred erased type argument can be a whole proof type,
a vector type, or a retained dependent function type. With the preceding Nat
and proof definitions, these examples use ordinary signatures:

```haskell
force :: a -> IO String
force x = x `seq` pure "forced"

ignoreAny :: a -> Int
ignoreAny _ = 42

forceProof :: Nat -> IO String
forceProof n = force (id (rightZero n))

forceBoxedProof :: Nat -> IO String
forceBoxedProof n = force (Just (rightZero n))

ignoreProofValue :: Nat -> Int
ignoreProofValue n = ignoreAny (rightZero n)

forceRetainedFunction :: Nat -> IO String
forceRetainedFunction n = force (id (appendNative @Int n))
```

`forceProof` forces the recursive proof and can force the complete Nat spine.
`forceBoxedProof` forces only `Just`, so its proof stays lazy at bottom and
cyclic indices. `ignoreProofValue` returns 42 without forcing its proof.
`forceRetainedFunction` observes the partial function's closure and keeps its
index lazy. Inference selects type arguments; it does not evaluate these values.

The checker extends the existing typed-argument cache. A known native Id in a
named argument spine enables immutable inference for the call. Other named
arguments can determine remaining binders, including `const proof Nothing`.
The expected result can determine binders first. Lambdas, literals, and other
checking-dependent forms keep their expected-type checks. Complete candidates,
kind and runtime-index scope checks, exact cached-type agreement, and ordinary
subsumption remain required. Source classification does not expand splices.
See [the contract](CORE.md#generic-retained-argument-inference) and
[the pinned Agda application and meta-context sources](REFERENCES.md#generic-retained-argument-inference).

The bootstrap fixture adds 34 generic observations after the previous 55,
for 89 imported observations. These cover ordinary consumers, linear proof
consumption, and retained-function values. Generic proof forcing also has
bottom and partial-index exception controls. Reusing a linear proof is rejected
with GHC-18872. The local linear helper has an explicit unrestricted let binding;
the same annotation is needed by the ordinary local-capture control.

The both-stage build passed in 7m30s at 4,573 MiB under 16 GiB. All five
registered normal-way regressions passed: 39 cases, 34 skipped, no unexpected
results or framework warnings. The command took 1m11s at 2,565 MiB under 8 GiB.
The test driver took 8.4 seconds. Bootstrap API and runtime v3 pass at 987 MiB
under 4 GiB.

Separate CLI processes pass all 89 imported observations, seven proof failure
observations, the generic equality GADT, four located proof rejections, and the
located GHC-18872 linear duplication rejection. Earlier partial/application,
standalone, source-case, and interface-version checks still pass. Eight
additional invalid applications produce located GHC-83865, GHC-91028, or
GHC-83605 without a panic. This covers the tested forms; native type-map support
remains future work.

Ordinary GADT/lambda, rank-n, and literal controls pass with the experimental
extension enabled. A higher-kinded consumer accepts a wrapped native proof.
A Template Haskell splice in a native vector argument expands exactly once.
All six literal Nat/Vec and generic-consumer guide blocks compile together with
Core and STG Lint. Optimized Template Haskell, package, GHCi, and Haddock smoke
checks pass. The final CLI/control/smoke scope peaked at 166 MiB under 4 GiB.
Build and verification scopes disabled swap. Compiler Fourmolu parsing, both
existing fixtures' canonical Fourmolu checks, and cabal-gild pass.

Successful logs use `native-generic` in `_build/verification/`. In particular,
`native-generic-cli.log` records 89 imported observations and
`native-generic-cli-after-path-fix.log` records the final CLI/control/smoke run.
Its first wrapper stopped at a missing copied standalone-control source; no
compiler change was needed. `native-generic-guide-final.log` checks the six
current literal blocks. Bootstrap artifacts and SHA256SUMS are retained in
`native-generic-bootstrap-work/`. The ordinary linear comparison and its
explicit unrestricted-binding controls are retained in `native-generic-linear/`.

Native meta-variable assignment, Quick Look, and general
checking-dependent argument inference remain open. The signature format is unchanged.

## Ordinary class consumers

Native map checkpoint: `1b37399cd5`. Class consumer checkpoint: `17740f7135`.

Ordinary class dictionaries can refer to types that contain a runtime index.
The existing `Show (a :~: b)` instance can render `rightZero n`. A normal vector
instance can render a retained vector:

```haskell
instance Show a => Show (Vec a n) where
  showsPrec _ VNil = showString "VNil"
  showsPrec d (VCons x xs) = showParen (d > 10)
    (showString "VCons " . showsPrec 11 x . showChar ' ' . showsPrec 11 xs)

showProof :: foreach (n :: Nat) -> String
showProof n = show (rightZero n)

showVector :: foreach (n :: Nat) -> Vec Int n -> String
showVector n xs = show xs

ignoreShow :: Show a => a -> Int
ignoreShow _ = 42

ignoreProof :: foreach (n :: Nat) -> Int
ignoreProof n = ignoreShow (rightZero n)
```

Rendering the proof forces it. Ignoring the proof remains lazy at bottom and
cyclic indices. Dictionary selection inspects types. A `Show` dictionary does
not supply equality evidence for vector transport. An equality-constrained
consumer can transport a vector after an explicit `Refl` case. It fails without
that evidence.

Immutable application matching now separates the leading constraints from the
ordinary body. It retains the substituted constraints for GHC's normal evidence
generation. An application-local implication binds native dictionary evidence
below the runtime index lambdas. It preserves enclosing Givens and the current
type-checking level. Native residual constraints remain in scope for solving or
a located error. The type maps now support free runtime indices and retained
binders with scoped alpha-equivalence. Ordinary family and optimizer gates remain.

The implementation passes the source/runtime matrix and the Core API checks. The
class block adds 42 observations after the previous 89. It checks repeated,
distinct, and mixed dictionary keys, inferred native results, visible type
applications, later quantifiers, local lets, and equality-Given transport.
Missing instances produce located GHC-39999. Equality constraints without proof
produce GHC-18872; direct failed transport produces GHC-83865.

The full build passes both compiler stages and matching libraries. Hadrian
reported 13 minutes 14 seconds. Peak memory was 6,953 MiB under a 16 GiB cap.
The five registered normal-way regressions pass: 39 cases, 34 skipped, five
expected passes, and no unexpected results or framework warnings. The driver
took 9.0 seconds; the command also rebuilt test tools and took 1 minute 38 seconds.
Its peak was 2,556 MiB under an 8 GiB cap.

Separate CLI processes reproduce all 131 imported observations. They also pass
the earlier proof, partial-application, standalone, and source-case controls;
three new proof exceptions; four located class/equality rejections; both interface
version-rejection directions; and ordinary bidirectional, rank-n, higher-kinded,
and one-expansion Template Haskell controls. Fourteen further observations use
Nat-indexed class parameters and retained function types with distinct free
indices, including ignored bottom and cyclic closures. All seven literal native
guide blocks pass Core and STG Lint. Optimized package/TH, GHCi, and Haddock smoke
checks pass. The final CLI scope peaked at 223 MiB under a 4 GiB cap. Swap was
disabled in all heavy scopes. The full GHC testsuite was not run.

Logs and scripts use the prefix `_build/verification/native-class-`. The final
wrapper is `native-class-final-checks.sh`. The bootstrap runtime and API logs are
`native-class-runtime-v5.log` and `native-class-api-v2.log`. The archived bootstrap
outputs and 27 file checksums, including CLI-generated STG dumps, are in
`native-class-bootstrap-work/`. The registered run's 24 files are saved separately
in `native-class-registered-work/`. The API
fixture localizes embedded module binders through ordinary Core substitution;
Core Lint remains enabled. Earlier prototype logs retain the reproduced map
panic, fixture export-flag failure, and corrected diagnostic expectation.

See [the contract](CORE.md#retained-class-consumers),
[instance-context provenance](REFERENCES.md#retained-class-consumers), and
[native type-map provenance](REFERENCES.md#native-type-map-equality).
Source-written class contexts with runtime binders, general inference, and
Quick Look remain open. This increment adds no interface format or equality axiom.

## Source retained constraints

A signature can put an ordinary class or equality context after its runtime
binder. The function receives its dictionary after that index. The body can use
the incoming evidence through normal GHC checking:

```haskell
renderWith :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String
renderWith n xs = show xs

transportGiven :: foreach (n :: Nat) -> (Add n Z ~ n)
               => Vec Int (Add n Z) -> Vec Int n
transportGiven n xs = xs

rightUnitViaContext :: foreach (n :: Nat) -> Vec Int (Add n Z) -> Vec Int n
rightUnitViaContext n xs = case rightZero n of
  Refl -> transportGiven n xs
```

These definitions reuse the Nat, Vec, Add, and proof definitions above. Enable
`FlexibleContexts` for the class signature. An equality context supplies evidence;
it does not cause GHC to evaluate `n` or prove the arithmetic identity. The caller
above supplies that evidence by checking `rightZero n` against `Refl`.

The equation checker keeps suffix patterns, guards, and local bindings under the
incoming dictionaries. Constructor equations keep their full argument lambdas
before the constructor case. Each branch transports its dictionaries with the
existing checked constructor proof. With lazy patterns, an ignored index stays
lazy. Inspecting the index or rendering a bottom vector can still force it.

Native quantified and irreducible predicates remain gated. A split with remaining
value patterns requires a nonforcing prefix and excludes `Strict`. A trailing
context preserves the earlier prefix pattern shapes. Inner retained lambdas,
general native inference, and ordinary optimization remain separate work.
The next checkpoint adds
[incoming evidence to explicit native cases](#source-case-evidence).

The implementation checkpoint is `670bcef166`. The shared fixture adds 46
observations after the previous 131. All 177 pass through interface-only module
consumers. The new 46 also pass source execution. The fixture checks no-instance
Givens, tuple-derived evidence, guards, views, local bindings, trailing contexts, and two retained
indices. Bottom and cyclic indices remain lazy in the tested partial applications.
Missing instances, missing equality proofs, unsupported predicate families,
quantified predicates, and strict prefixes produce located errors.

Both compiler stages and matching libraries build. The final build took 8 minutes
47 seconds and peaked at 4,956 MiB under a 16 GiB cap. All five registered normal-way
regressions pass: 39 cases, 34 skipped, and five expected passes. The driver reports
no unexpected results, framework failures, or framework warnings. It took 9.4
seconds; the command rebuilt test tools and took 1 minute 39 seconds. Its peak
was 2,430 MiB under an 8 GiB cap. Hadrian's setup
comparison warns about `lib` versus `lib/.`; both resolve to the same directory.

Separate CLI checks reproduce the 177 rows, earlier proof and linear controls,
both interface-version rejection directions, and the ordinary bidirectional and
single-expansion TH controls. The scoped CLI checks add two exception-order
observations and seven located rejection controls. All eight literal native guide
blocks and the optimized package/TH, GHCi, and Haddock startup smoke checks pass.
Missing-documentation warnings are expected with `--docs=none`. The full GHC
testsuite was not run.

Artifacts use `_build/verification/native-context-`. The final build log is
`native-context-full-build-v2.log`. Runtime and registered logs are
`native-context-runtime-v8.log` and `native-context-registered.log`. CLI results
are in `native-context-final-checks.log` and `native-context-scoped-cli-v2.log`.
The bootstrap archive `native-context-bootstrap-v8-work/` has 27 file checksums.
A final comment-only cleanup removes a local Haddock warning. Its targeted
`-haddock -fno-code` check passes without changing compiler artifacts.

See [the contract](CORE.md#source-retained-constraints) and the
[pinned Agda telescope sources](REFERENCES.md#source-retained-constraints).

## Source case evidence

An explicit dependent case can use the incoming class and equality evidence:

```haskell
renderCaseWith :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String
renderCaseWith n xs = case n of
  Z -> show xs
  S k -> show xs

transportCaseGiven :: foreach (n :: Nat) -> (Add n Z ~ n)
                   => Vec Int (Add n Z) -> Vec Int n
transportCaseGiven n xs = case n of
  Z -> xs
  S k -> xs
```

These definitions reuse the earlier Nat, Vec, Add, and Show definitions. The case
helper receives the original index, dependent evidence, and captured values.
Each branch transports the evidence with its checked constructor proof. The
ordinary solver can then derive superclass and tuple evidence. A global instance
is not needed when the caller supplies a Given. Equality transport still needs
an equality proof.

The checker records actual lexical evidence parameters, including primitive
equality CoVars. Static forms exclude enclosing evidence. Arrow heads restore
their enclosing scope. Nested typed splices restore the bracket-entry scope with
the outer constraint accumulator. This adds no native quotation support.

An explicit case must inspect its index before it can return a branch function.
It need not force an unused constructor field or captured vector. The shared
fixture appends 19 runtime observations to the previous 177. It also checks
no-instance declarations, tuple evidence, local bindings, nested shadowing,
boxed equality, primitive GADT equality, and arrow evidence scope. An incoming
`OnlyGiven (S k)` does not supply `OnlyGiven k`.

The constructor pattern limits remain unchanged: use complete, unguarded
alternatives with bare field variables. General motive inference, inner retained
lambdas, native coercion unfolding serialization, and ordinary optimization remain
separate work. The Core forms, dictionary ABI, and interface format stay unchanged.

The implementation checkpoint is `fe7329d1b6`. Both compiler stages and matching
libraries build. The full build took 11 minutes 45 seconds and peaked at 6,550 MiB
under a 16 GiB cap. All five registered normal-way tests pass: 39 cases, 34 skips,
five expected passes, and no unexpected results or framework failures or warnings.
The driver took 9.5 seconds. The command took 1 minute 37 seconds and peaked at
2,616 MiB under an 8 GiB cap. The bootstrap runtime and API kernel also pass.

Separate CLI compilation reproduces all 196 imported observations with Core and
STG Lint. The no-instance case and outer-Given arrow compile. Eight context
rejections have the expected codes and source locations. Six strictness controls,
both interface-version rejection directions, earlier proof and linear controls,
ordinary bidirectional typing, and single-expansion TH checks pass. All nine
literal native guide blocks compile. Ordinary nested typed splices, a static
pointer, optimized package/TH execution, GHCi, and Haddock startup also pass.
The CLI scope peaked at 269 MiB under a 4 GiB cap. All scopes used no swap.
The full GHC testsuite was not run.

Artifacts use `_build/verification/native-case-evidence-`. The build log is
`native-case-evidence-full-build.log`. The runtime log is
`native-case-evidence-runtime-v5.log`; earlier failures were in new arrow-control
fixtures and are retained in `native-case-runtime-v2.log` through `v4.log`.
The registered and final CLI logs end in `registered.log` and `final-cli.log`.
The bootstrap archive is `native-case-evidence-bootstrap-work/`.
Compatibility scripts use `native-case-compat-` and separate artifact copies.
Fixtures pass Fourmolu. Compiler files parse without rewriting; `Splice.hs`
requires CPP preprocessing in both interpreter branches. Its unchanged source
reproduces the raw Fourmolu parser failure. The Cabal template passes cabal-gild.
Missing-documentation warnings are expected with `--docs=none`.

See [the contract](CORE.md#explicit-case-evidence),
[the task list](PLAN.md#source-case-evidence-follow-up), and
[the pinned Agda implementation](REFERENCES.md#source-case-evidence).

## Checked retained lambdas

A complete retained signature now checks an anonymous lambda:

```haskell
lambdaIdentity :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n
lambdaIdentity = \n xs -> xs
```

This uses the earlier Nat and Vec declarations. The runtime Id for `n` also
instantiates the later argument and result types. Named equations and lambdas
share the retained match checker. The rule preserves outer erased type patterns,
nested lambdas, later retained indices, and class and equality contexts.

Preserve the source value arity. A one-pattern `\case` can inspect the index
before its branch returns a function. A two-pattern `\cases` waits for both
arguments before matching. Unused indices, fields, and payloads stay lazy.
Lambdas retain their ambient usage. A local retained lambda can capture a linear
value and consume it once. Duplicated linear proof use remains an error.

An empty-domain lambda-case binds its index explicitly before an empty case
inspects it. The result type refers to the same Id. This fixes an unbound-index
Core Lint error that the empty match group exposed. Empty source cases need no
branch refinement and stay in their original context. Demanding either form at
a bottom index still raises that bottom exception.

The shared fixture appends 30 observations to the previous 196. It also checks
four located lambda rejections and two empty-domain bottom exceptions. The
bootstrap runtime and API kernel pass. Separate probes check explicit outer type
patterns, linear captures, and the existing inference and type-position limits.

The implementation checkpoint is `a0c493d5f9`. Both compiler stages and matching
libraries build in 8 minutes 26 seconds. The scope peaks at 4,833 MiB under a
16 GiB cap. All five registered normal-way tests pass: 39 cases, 34 skips,
five expected passes, and no unexpected results or framework failures or warnings.
The driver takes 9.6 seconds. The command takes 1 minute 13 seconds and peaks at
2,492 MiB under an 8 GiB cap. The final bootstrap scope peaks at 1,000 MiB
under a 4 GiB cap.

Separate CLI processes reproduce all 226 imported observations with Core and
STG Lint. Six new lambda rejections have the expected codes and source locations.
Empty lambdas with trailing class and equality contexts compile. Four strictness
controls include ordinary empty-case behavior. STG keeps arity one for `\case`
and arity two for `\cases`. The ignored-index worker has no case.

Earlier proof, linear, source-case, interface-version, and ordinary typing checks
pass. All ten literal native guide blocks compile. Single-expansion TH, nested
typed splices, static pointers, optimized package execution, GHCi, and Haddock
startup pass. The initial CLI scope peaks at 210 MiB under a 4 GiB cap.
A copied TH fixture used its old counter path. The corrected remaining checks
pass in a separate scope with a 167 MiB peak. All scopes use no swap.
The full GHC testsuite was not run.

Artifacts use `_build/verification/native-checked-lambda-`. The final bootstrap
logs are `runtime-v4.log`, `api.log`, and `bootstrap-v3.log`. Build and registered
logs end in `full-build.log` and `registered.log`. CLI evidence spans
`final-cli.log` and `final-remainder.log`; the latter records the corrected TH
fixture and remaining checks. The bootstrap archive has 27 checksummed files,
including three STG dumps. The source manifest covers 11 compiler files and both
API fixtures. Fourmolu, compiler-file parsing, cabal-gild, and diff checks pass.

This checks an expected native type. It adds no new Core form, singleton ABI, or
interface format. Retained syntax in nested type positions, general native
inference, structural Pi unification, and ordinary optimization remain open.
See [the contract](CORE.md#checked-source-lambdas),
[the tasks](PLAN.md#checked-retained-lambdas), and
[Agda's lambda and absurd-lambda rules](REFERENCES.md#checked-retained-lambdas).

## Higher-order retained closures

A retained function can be an ordinary closure argument in a complete signature:

```haskell
applyNative :: forall a.
               (foreach (m :: Nat) -> Vec a m -> Vec a m)
            -> foreach (n :: Nat) -> Vec a n -> Vec a n
applyNative f n xs = f n xs
```

This uses the earlier Nat and Vec declarations. The caller can pass an explicitly
specialized named worker or a lambda checked against the supplied retained type.
The argument `f` is a function value. It is not a runtime index. An inner
function type can also capture an enclosing runtime index. Each nested binder
scopes only its own codomain. Both ordinary-leading and retained-leading
signatures follow the same arrow traversal.

The ambiguity check opens retained binders in argument and result positions in
one fresh runtime context. The actual signature stays retained. Ordinary helpers
can also receive a known retained function type through immutable instantiation.
This checks `id` around a lambda and a genuine polymorphic bottom without native
meta assignment or eta expansion. Erased inner foralls and structural Pi
unification remain gated. Native templates can still require explicit type
applications. Retained syntax under type constructors, kinds, and other source
positions remains separate work.

The fixture appends 29 observations to the previous 226. These cover polymorphic
calls, ordinary-leading functions, checked lambdas, value-changing vector
closures, captured indices, and erased/runtime shadowing. Unused bottom functions
and bottom vectors stay lazy. Demanding the signed bottom function raises its
named exception. Wrong lengths, escaping indices, unsupported domains, unsigned
aliases, and partial signatures have located diagnostics. Constructor clauses
and explicit case transport a captured closure through its fixed-domain Pi.
The transport uses the checked branch proof and existing `DepFunCo` lifting.
It preserves the function value with a cast. Changed domains and dependent
erased foralls remain gated.

The implementation is in `64044341e8` and `94d7d8f2f3`. The final both-stage
build passed in 7m35s at 4,651 MiB under 16 GiB. All five registered normal-way
regressions passed: 39 cases, 34 skipped, no unexpected results or framework
warnings. The command took 1m11s at 2,560 MiB under 8 GiB. The driver took
9.725 seconds. The repository requires regression coverage for compiler changes;
this change extends the existing runtime fixture.

The bootstrap runtime passes 255 imported rows and 124 source-context rows.
The API kernel and standalone known-result controls pass. Separate self-hosted
CLI processes reproduce the 255 rows and check 13 further closure observations.
These include ignored and returned cast bottom functions. Six new located
rejections, previous lambda/context/proof/linear controls, 177 earlier
compatibility rows, and both interface-version rejection directions pass.

All 11 literal guide blocks pass Core and STG Lint. Ordinary typing,
single-expansion Template Haskell, 14 native function/class observations,
optimized package/GHCi/Haddock smoke, nested typed splices, and static pointers
pass. The final CLI scope peaked at 291 MiB under 4 GiB. All heavy jobs were
serial and disabled swap. The full GHC testsuite was not run.

STG shows ordinary three-argument closure passing in `applyAt` and `applyFirst`.
`ignoreClosure` has no case. `bottomClosure` remains a bottom thunk.
`returnCapturedCase` has arity two and returns the original closure after its
index case. The Core forms, dictionary ABI, and interface version are unchanged.
General native inference, Quick Look, structural Pi unification, and ordinary
optimization remain gated.

Artifacts use `_build/verification/native-higher-order-`. The final logs are
`full-build-v2.log`, `registered-v2.log`, and `final-cli-v2.log`. The
`validation.txt` record includes the earlier prototype failure, source hashes,
artifact hashes, and the older compatibility snapshot's stale STG hashes.

See [the contract](CORE.md#explicit-higher-order-closure-arguments),
[the tasks](PLAN.md#explicit-higher-order-arguments-follow-up), and
[Agda provenance](REFERENCES.md#explicit-higher-order-arguments).

## Expected-type instantiation of retained workers

A complete expected retained function type can supply a known worker's erased
type arguments. This uses the Nat and Vec types and `applyNative` from above:

```haskell
polyIdentity :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n
polyIdentity n xs = xs

identityInt :: foreach (k :: Nat) -> Vec Int k -> Vec Int k
identityInt = polyIdentity

applyIdentityInt :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
applyIdentityInt n xs = applyNative @Int polyIdentity n xs
```

The expected type determines `a = Int`. The worker receives an ordinary type
application. Its retained runtime binder stays intact. Nested telescopes,
ordinary-leading aliases, and contexts follow the same checking rule. Explicit
type applications remain available.

The new immutable matcher is opt-in. It pairs equal closed-domain `Many`
binders, seeds their scope with input free variables, and rejects solutions
that capture an inner index. Default matching and general unification retain
their existing native gates. Require a known head without concreteness
requirements, NoQL, all erased arguments, kind/scope agreement, exact result
equality, and ordinary final subsumption. The compiler does not guess a runtime
index or eta-expand the worker.

The bootstrap API and runtime pass. The runtime fixture appends 22 rows to the
previous 255. These cover implicit worker arguments, polymorphic and monomorphic
aliases, ordinary-leading and applied aliases, nested telescopes, and laziness.
The source checks reject inconsistent payloads, inner-index capture, domain
mismatch, and an unsigned alias. A signed bottom worker remains bottom when
demanded. The API regression also checks a shared-pointer binder/free-index
case that failed before the matching scope included both input types.

Implementation `dae037074c` passes the full both-stage build in 9m22s at
5,872 MiB under 16 GiB. All five registered normal-way regressions pass:
39 cases, 34 skipped, no unexpected results or framework warnings. The command
took 1m36s at 2,613 MiB under 8 GiB. The driver took 10.529 seconds. The
repository requires regression coverage; this change extends both existing
native Core API fixtures.

The bootstrap API and runtime pass together at 1,098 MiB under 4 GiB. The
runtime checks 277 imported rows and 146 source-context rows. Separate
self-hosted processes reproduce the 277 rows. Seven further worker observations
check bottom-function behavior and runtime class-context dispatch. Four new
located rejection controls, the previous 13 closure controls, earlier
lambda/context/proof/linear checks, 177 compatibility rows, and both interface
rejection directions pass.

All 12 literal guide blocks pass Core and STG Lint. Ordinary typing,
single-expansion Template Haskell, 14 native function/class observations,
optimized package/GHCi/Haddock smoke, nested typed splices, and static pointers
pass. The CLI scope peaked at 302 MiB under 4 GiB. All heavy jobs were serial
and disabled swap. The full GHC testsuite was not run.

CorePrep shows actual `@Int` applications for captured, constrained, and
ordinary-leading aliases. STG preserves direct worker aliases. The generic
bottom worker and its specialization remain thunks. Ordinary partial
applications can retain CorePrep's existing arity wrappers. The Core forms,
dictionary ABI, and interface version are unchanged. General native meta
assignment, unification, and ordinary optimization remain gated.

Artifacts use `_build/verification/native-worker-instantiation-`. Final logs
are `full-build.log`, `registered.log`, `final-cli.log`, `api-v3.log`, and
`runtime-v2.log`. The source manifest covers 18 compiler files and two fixtures.
The bootstrap and isolated compatibility archives contain 27 files each.
`validation.txt` records the failing freshness regression and its verified fix.
See
[the contract](CORE.md#explicit-higher-order-closure-arguments),
[the tasks](PLAN.md#expected-type-instantiation-of-retained-workers), and
[Agda provenance](REFERENCES.md#expected-type-instantiation-of-retained-workers).

## Verified baseline

The default Hadrian build passed with six workers and `--docs=none`. The build
produced `_build/stage1/bin/ghc`, version `10.1.20261109`. Hadrian reported
30 minutes 48 seconds. The memory scope recorded a peak of 10,577 MiB under a
16 GiB cap with swap disabled.

The boot compiler was GHC 9.14.1. The Cabal version was 3.18.1.0. Nix supplied
Autoconf 2.72, Automake 1.18.1, Happy 2.1.7, and Alex 3.5.4.0.

Focused smoke checks passed. They covered package registration, optimized
compilation with Core Lint, Template Haskell, threaded execution, GHCi, and
Haddock startup. Documentation generation and the full testsuite were not run.
Package registration emitted missing-documentation warnings because documentation
was disabled.

The local build commands used these steps:

```sh
export CC=/usr/bin/gcc CXX=/usr/bin/g++ LD=/usr/bin/ld
./boot
./configure GHC=/home/tritlo/.ghcup/bin/ghc-9.14.1
cabal --project-file="$PWD/hadrian/cabal.project" build \
  --with-compiler=/home/tritlo/.ghcup/bin/ghc-9.14.1 \
  --disable-documentation --disable-profiling --disable-library-profiling \
  -j6 exe:hadrian
cabal --project-file="$PWD/hadrian/cabal.project" exec \
  --with-compiler=/home/tritlo/.ghcup/bin/ghc-9.14.1 \
  --disable-documentation --disable-profiling --disable-library-profiling \
  hadrian -- --directory "$PWD" -j6 --flavour=default --docs=none
```

Use an absolute path for `--project-file`. A relative path resolved the package
locations incorrectly. Supply the required tools on `PATH`. On this WSL2 host,
run builds and checks through `/home/tritlo/Code/heavy-management`. Read its
current scripts before reuse. Preserve the queue's concurrency settings.

Local build scripts, logs, and memory records are in `_build/verification/`.
They are ignored build artifacts. The results above describe the pinned baseline;
they do not describe later source revisions.

## Verified singleton translation

The unmodified upstream `Data.Singletons` module compiled against the baseline
with a small `SBool` and `SingKind Bool` adapter. Its source blob was
`c6bf5970d85d28ed6edb4d887f14cfa127a01be2`.

The reference program passed at `-O0` and `-O2`, both with `-dcore-lint`. It checked:

- `Select True = Int` and `Select False = Bool`.
- Branch refinement through `STrue` and `SFalse`.
- Shared index use in `keepS sb (chooseS sb)`.
- Runtime input through `withSomeSing @Bool`.
- Non-strict behavior when `keepS` ignores its witness.
- Rejection of `keepS STrue False`.

The reification check confirmed this distinction:

```haskell
keepS @True undefined `seq` ()                 -- Terminates.
withSomeSing @Bool undefined (\_ -> ())        -- Diverges.
```

These results establish the reference encoding. They do not establish a frontend
implementation or full-package compatibility. HEAD has `base-4.23.0.0`. The
inspected `singletons` package excludes that version in its bounds.
`singletons-base` also targets the GHC 9.14 Template Haskell API.

The temporary evidence is at `/tmp/ghc-retained-bool-plan.DJqnmf/`:
`Oracle.hs`, `Reject.hs`, `LibraryOracle.hs`, `check.log`, and `library-check.log`.
The temporary directory can expire. The maintained comparison sources and
reproduction recipe are now in [reference/](reference/README.md).

## Retained constructor field checkpoint

Specification: `cae8e34694`. Core implementation: `151bd9d6ad`.
This checkpoint adds constructor field binders and sequential Core pattern
instantiation. The new API checks construction, retained consumption,
repackaging, fresh identities, nested Pi capture avoidance, payload identity and
result nonescape. It rejects flat instantiated-field APIs and interface
declarations that would discard the binders. It excludes even vacuous retained
constructors from native index domains and rejects unsupported promotion.

The API fixture uses an indexed equality-dictionary function as its payload.
It does not execute a stored Vec package. Source constructor signatures remain
gated by located GHC-43582. The next steps are source declaration/pattern
checking, coherent field identities during desugaring, and interface field-binder
encoding, followed by real Nat/Vec execution
and field-laziness controls. See [M10](PLAN.md#m10-retained-constructor-field-telescopes),
[the contract](CORE.md#retained-constructor-fields), and
[the pinned provenance](REFERENCES.md#retained-constructor-field-telescopes).

Validation passed on 2026-11-13:

- Both compiler stages and libraries: 17m17s, 6,800 MiB peak under 16 GiB.
- Final bootstrap Core API: passed, 984 MiB under 4 GiB. The initial API run
  passed before the extra guards. The new flat-API regression then failed
  against that library and passed after the guards were rebuilt.
- Bootstrap runtime: passed the unchanged stdout golden, including the new
  located source rejection, module execution and fingerprint/recompilation
  checks. Peak: 992 MiB under 4 GiB. Existing observations remain 277 imported
  and 146 source-context rows; these are compatibility checks for this change.
- Five registered normal-way tests: 39 cases, 34 skips, five expected passes,
  and no unexpected results or framework failures/warnings. Driver time:
  11.018s; command time: 1m43s. Peak: 2,562 MiB under 8 GiB.
- Separate CLI compatibility controls: 277 main imported observations, 177
  older compatibility observations, 13 closure controls, seven worker controls,
  existing lambda/context/proof/linear checks, both interface-version rejection
  directions, 12 literal guides, ordinary typing and single-expansion TH,
  14 native function/class observations, package/GHCi/Haddock smoke checks,
  nested typed splices and static pointers all passed. Peak: 306 MiB under 4 GiB.
- The final constructor gate initially failed because its imported interface
  was outside the one-shot compilation directory. The combined CLI wrapper
  exited there after its earlier controls passed. Copying the interface into
  that directory fixed the harness. Rerunning only the gate passed with
  GHC-43582 at `NativePackedVecGate.hs:5:14`. No compiler fix was needed.
- Fourmolu formatting of both fixtures, nonmutating parsing of the five changed
  compiler modules, cabal-gild and `git diff --check` passed. The full GHC
  testsuite was not run. Existing Cabal/Haddock warnings and Hadrian's libdir
  normalization assertion remain outside this change.

Artifacts use `_build/verification/native-constructor-fields-`. Main logs are
`full-build.log`, `api-v2.log`, `runtime.log`, `registered.log`,
`final-cli.log` and `source-gate-v2.log`. Source hashes cover 24 compiler files
and two fixtures. Harness hashes cover 32 files. Current archive manifests cover
27 bootstrap, 27 compatibility, 30 worker, 34 closure, 32 lambda and four source
gate files. Current hashes, the immutable interface reader, and both previous
worker-instantiation archives verify. No serialized declaration format changed.
Source, interfaces and runtime package execution remain pending in M10.

## Retained constructor interface checkpoint

Implementation: `6a0bfbf0fd`. Schema and pinned Agda provenance:
`65611cfdb8`. Constructor declarations now serialize aligned optional field
names. The reader allocates fresh runtime Ids in order. It keeps field scope
out of sibling constructors and rejects unsupported promotion. The field types
remain the only encoding of domains and multiplicities.

Recursive imports expose metadata before field-domain checks. Validate Core
constructor telescopes on worker/user type demand. Allocate named Id skeletons
with literal Many; Id construction forces multiplicity. Inspect metadata before
a domain constructor's full signature. This avoids reentering a validation thunk.
See the [interface provenance](REFERENCES.md#constructor-field-interface-provenance).

The fork is `10.1.20261113.2`, with interface header `1001202611132`.
The preserved version-1 reader still accepts its original interface. Both
readers reject the other version with CheckHiWay and IgnoreHiWay.

Validation uses `_build/verification/native-constructor-iface-`:

- Both compiler stages and libraries passed. The full rebuild took 27m38s and
  peaked at 9,600 MiB under 16 GiB. A later Note correction triggered another
  dependency check. Its first scope reached the actual 10m limit, exit 124;
  continuation passed in 3m44s, with 3,995 MiB peak. No compiler fix followed.
- The final bootstrap API and runtime fixtures passed under 4 GiB, with a
  combined peak of 1,154 MiB. The API checks Normal/MaximumCompression declaration
  round trips, repeated imports, vacuous binders, parameters, name tidying,
  shared payload Ids, construction/elimination/repackaging Lint, malformed scope,
  promotion and bounded self/mutual imported-domain rejection. Runtime stdout
  remains unchanged. These package payloads are equality-dictionary functions.
- All five selected registered tests passed across the initial run and a focused
  rerun. The first run had 39 cases, 34 skips and four passes. The API fixture
  missed an import after warning cleanup; adding it made the one-test rerun pass.
  Neither run had framework failures or warnings. Peaks were 1,922/1,776 MiB
  under 8 GiB. An earlier API setup fix initializes its package environment.
- Separate CLI controls passed: 277 current and 177 older imported observations,
  closure/worker/proof/class/lambda controls, 12 literal guides, ordinary typing
  and TH, package/GHCi/Haddock smoke checks, typed splices and static pointers.
  Version assertions needed the new header number; the copied guide harness
  also needed its 12 input files. The remainder alone was rerun and passed.
  Maximum peak across these scopes was 1,057 MiB under 4 GiB.
- Source constructor syntax still rejects with located GHC-43582. The full GHC
  suite was not run. Fourmolu fixture formatting, nonmutating compiler parsing,
  cabal-gild and whitespace checks passed. Existing Cabal/Haddock notices and
  Hadrian's libdir-normalization assertion remain outside this change.

The successful logs are `build-v4.log`, `build-v6.log`, `bootstrap-final.log`,
`registered-recheck.log` and `cli-remainder.log`; earlier CLI results are in
`cli-final-v2.log`. Source hashes cover 28 compiler files, two fixtures and
VERSION_DATE. Harness hashes cover 42 files. Archive manifests cover 27 bootstrap,
27 older compatibility, 30 worker, 34 closure, 32 lambda and four source-gate files.
The previous interface reader and earlier archives remain immutable.

This checkpoint tests binary declaration codecs through the API. It does not
execute a stored Vec package or establish full on-disk package compilation.
The next gate is the [standalone source example](examples/packed-vector/README.md).
Constructor/pattern checking, roles, PMC, desugaring and runtime argument counting
remain open in M10. Native unfoldings, optimization and profiling remain gated.

## Retained constructor source checkpoint

Implementation: `8e90e0386b`. The [packed-vector example](examples/packed-vector/README.md)
now compiles from Haskell source through both compiler stages. It declares its
own Nat, Vec and PackedVec. A separate consumer imports the producer's interface
and links its object. The producer source is absent from the consumer import path.
The program constructs, opens and repackages stored vectors without singleton
reification or a GHC API producer.

Constructor checking preserves each runtime binder through renaming, kind
checking, generalization and zonking. Field substitution remains lazy until the
parent datatype knot is complete. Role checking, source patterns, PMC and
desugaring preserve ordered field scope. Runtime argument counters handle retained
Pi binders. Automatic Typeable generation skips the unsupported promoted
constructor and retains the ordinary parent. The existing immutable application
matcher reads resolved expected-type metas before matching polymorphic payloads.
See the [Core contract](CORE.md#source-constructor-checking).

Validation on 2026-11-13:

- Both compiler stages and matching libraries build in 26m35s. Peak memory was
  6,458 MiB under the 16 GiB limit, with swap disabled.
- Five focused registered regressions pass. The run has 39 cases, 34 skips,
  no unexpected results, and no framework failures or warnings. Its scope peaked
  at 2,715 MiB under 8 GiB. The repository MR template requires regression coverage.
  The existing runtime fixture now reads the two example files directly.
- All 15 source observations pass in the standalone bootstrap and self-hosted
  runs and in the runtime fixtures. The Core API and bootstrap runtime regression
  pass. The final source/runtime scope peaked at 1,058 MiB under 4 GiB.
- Twelve located negative cases pass: StrictData, type data, existentials,
  contexts, refined universals, promotion, constructor field patterns, lazy
  patterns, let patterns, index mismatch, index escape and shared equations.
  They require GHC-83605, GHC-83865 or GHC-94726 as applicable. Panics and timeouts
  are failures. The standalone negative scope peaked at 37 MiB under 4 GiB.
- CorePrep and STG show a constructor with two runtime arguments and two lazy
  fields. Repacking passes both original fields. `ignorePacked` inspects only
  the package constructor. The consumer retains its IORef read.
- Fourmolu parses all 19 changed compiler files and checks the runtime fixture.
  Its upstream parser needs the fork-only extension pragma removed to parse the
  example files. The fork compiles the unchanged examples. Cabal-gild ran on an
  ignored copy of the generated Cabal file. Whitespace checks pass.

Artifacts use `_build/verification/native-source-package-`. The main commands
are `build.sh`, `native-source-package-registered.sh`,
`native-source-package-selfhost-run.sh`, `native-source-package-api.sh` and
`native-source-package-runtime.sh`, with the resource limits above. Successful
logs are `build-v1.log`, `registered-v1.log`, `selfhost-run.log`, `api-v1.log`,
`bootstrap-runtime-v2.log` and `negatives-v1.log`. Source, documentation, harness
and executable-artifact manifests accompany them. The old interface readers and
archives remain unchanged. This increment does not change the interface schema.

The initial source package has one constructor, bare variable fields and one
equation per eliminator. Complete signatures and the prototype's O0/interface
flags remain required. Native unfolding optimization, profiling, records and
general projections remain separate work. The full GHC suite was not run.
The following checkpoint implements the filter with a length computed at runtime.

## Source vector filter checkpoint

Implementation: `c9aa3545c0`. The [source example](examples/packed-vector/README.md#filter-with-a-computed-result-length)
now filters `[1,2,3]` with cutoffs read through an IORef. It returns lengths 3,
2 and 0, with the corresponding vectors. Each retained element constructs
`PackVec (S n) (VCons x ys)`. Storing `n` instead rejects with GHC-05617 at the
constructor application. The compiler checks agreement between the output
fields. Element selection and order have separate runtime checks.

Direct package `case` patterns use GHC's existing multiplicity constraints.
Their fresh multiplicity variable can resolve to `Many`; linear and rigid
multiplicities retain ordinary diagnostics. Native simplification also expands
the two wired-in section helpers through their existing compulsory templates.
This prevents the representation-polymorphic temporary for `(> cutoff)` from
reaching code generation. Partial sections keep their lambda and remain lazy.
Both changes have GHC Notes. The [references](REFERENCES.md#source-vector-filter)
record the Agda filter structure and the GHC-specific lowering rules.

Validation on 2026-11-14:

- Both stages compile and run the separate-module source example with Core and
  STG Lint. The consumer imports only the producer interface and links its object.
  All 36 observations pass: 15 package, 13 filter and 8 section checks.
- The resumed self-hosted compiler target and its dependencies build in 4m12s.
  This is an incremental build after the requested pause, not a clean build.
  Peak memory was 4,718 MiB under 16 GiB, with swap disabled.
- All five focused registered tests pass: 39 cases, 34 skips, no unexpected
  results, and no framework failures or warnings. The driver took 20.624s; the
  complete job took 3m24s and peaked at 3,904 MiB under 8 GiB.
- The bootstrap Core API and runtime fixture pass. The initial combined job
  passed the source executable and API, then stopped on a diagnostic-text
  expectation: API printing retains the tick in `'S`, while CLI printing omits
  it. The fixture now checks the constructor expression, code and location.
  The corrected runtime run peaked at 1,045 MiB under 4 GiB.
- Sixteen source rejections pass. The four additions cover linear clause and
  case patterns (GHC-18872), rigid multiplicity (GHC-25897), and wrong filter
  length (GHC-05617). The standalone negative run peaked at 38 MiB under 4 GiB.
- CorePrep/STG retain the two runtime package fields and the filter recursion.
  The temporary section helpers are absent. The immutable previous-format
  reader's three hashes still match. No interface schema change was needed.
- Fourmolu parses the two compiler files, checks the fixture, and checks the
  examples with their existing leading-comma style. Its upstream parser needs
  only the fork extension pragma omitted; the fork compiles the actual sources.
  Cabal-gild ran on an ignored copy of the generated Cabal file. Whitespace
  checks pass. The repository MR template requires regression coverage.

Artifacts use `_build/verification/native-filter-source-`. Successful logs are
`selfhost-build-v2.log`, `selfhost-run-v1.log`, `api-v1.log`,
`bootstrap-runtime-v2.log`, `registered-v1.log` and `negatives-v2.log`.
`build-input.SHA256SUMS`, `source.SHA256SUMS`, `docs.SHA256SUMS` and
`harness.SHA256SUMS` identify the inputs. The source work directories contain
their own output manifests. The first failed expectation remains in
`bootstrap-runtime-v1.log`.

This filter scans a finite input spine before it returns a package. It leaves
retained elements lazy when the predicate permits it. It does not prove the
output-length bound or predicate correctness. The following M12 checkpoint adds
a source `LE` proof for the bound. Broader package
patterns, general projections, native optimization and proof erasure remain
separate work. The full GHC suite was not run.

## Source filter bound proof checkpoint

Implementation: `7eae40be91`; prerequisite frontend fix: `4ad47fdeca`.
The [source example](examples/packed-vector/README.md#filter-with-a-checked-length-bound)
now returns a runtime length, a vector at that length and an `LE` proof that
the output length is at most the input bound. It uses ordinary GADTs and the
existing retained constructor fields. No new Core form or interface format is
required. The retained branch constructs `S len`, `VCons x ys` and `LES proof`.
The rejected branch weakens only the proof's upper bound. The constructors and
weakening function follow the [pinned Agda definitions](REFERENCES.md#source-filter-bound-proof).

The source observer exposed a frontend gap in `evaluate proof >> pure ()`.
Dictionary selectors put their method foralls after the class constraint.
Native instantiation now matches across that invisible prefix and retains an
immutable plan in binder order. Existing IALL and `instCall` emit each group's
type and evidence applications in the original order. The usage cache, runtime
scope checks and final subsumption checks remain in place. Native types do not
enter mutable type metas. See
Note [Immutable instantiation of nested contexts] in `GHC.Tc.Gen.App`.

Validation on 2026-11-14:

- Both compiler stages compile and run the producer and interface-only consumer
  with Core and STG Lint. All 58 observations pass: the previous 36 plus 22 bound
  and proof-demand checks. Cutoffs 0, 1 and 3 produce vector lengths and proof
  depths 3, 2 and 0. No GHC API producer adds these source declarations.
- Nineteen package/bound rejection checks pass in the runtime fixtures. The
  three new source mutations separately break the vector length, proof lower
  index and proof upper index. They reject with located GHC-05617, GHC-05617
  and GHC-25897 diagnostics. The standalone harness pins the same results.
- The bootstrap compiler build took 51.86s and peaked at 2,040 MiB. The
  self-hosted compiler target and its dependencies built in 13m33s and peaked
  at 6,276 MiB. Both jobs used 16 GiB caps with swap disabled.
- The bootstrap runtime passes at 1,053 MiB under 4 GiB. The self-hosted source,
  Core API and pinned negative checks pass together at 1,118 MiB under 4 GiB.
- All five focused registered tests pass: 39 cases, 34 skips, no unexpected
  results, and no framework failures or warnings. The driver took 11.864s;
  the complete job took 1m56s and peaked at 2,923 MiB under 8 GiB.
- CorePrep/STG show three runtime constructor fields. Proof observations remain
  separate from package and vector demand. The immutable previous-format reader
  remains unchanged. Fourmolu parsing/checks, an ignored Cabal-gild copy and
  whitespace checks pass. The repository MR template requires regression coverage.

Artifacts use `_build/verification/native-filter-bound-`. Successful logs are
`bootstrap-build-v1.log`, `bootstrap-run-v1.log`, `bootstrap-runtime-v1.log`,
`selfhost-build-v1.log`, `selfhost-run-v2.log`, `api-v1.log`, `negatives-v2.log`
and `registered-v1.log`. The initial `selfhost-run-v1.log` records the scoped
IO error before the frontend fix. Input, source, documentation, harness and
output manifests identify this checkpoint. The Agda snapshots are in
`prior-art/` under the same prefix.

The proof is an ordinary lazy runtime field. Package WHNF and vector observation
can ignore an undefined proof. A defined proof constructor can contain an
undefined recursive proof. A bound interpretation requires terminating indices
and a terminating proof. This does not establish totality, predicate selection
correctness or general proof erasure. Native optimization, broader package
patterns and general projections remain separate work. The full GHC suite was
not run.

## Nested source package pattern checkpoint

Implementation: `7a98362197`; source target and provenance: `0b87e9fe16`.
The [source library](examples/packed-vector/README.md#nested-package-patterns)
now checks complete consumers with nested vector and proof patterns:

```haskell
headPacked :: PackedVec a -> Maybe a
headPacked (PackVec n VNil) = Nothing
headPacked (PackVec m (VCons x xs)) = Just x
```

Retained fields remain bare source variables. Ordinary fields use `tcConArg`,
so nested GADT evidence scopes over later fields and the RHS. Each package
equation compiles to its own case on the shared scrutinee. Existing
`MatchResult` failure continuations preserve source clause order. The compiler
does not merge different index Ids through let aliases. PMC uses each checked
outer pattern type and retains sequential speculative field instantiation.
Ordinary constructor-family compilation keeps its existing path. Core and
interface formats are unchanged. See Note [Retained package equation scope]
in `GHC.HsToCore.Match.Constructor` and the
[pinned Agda sources](REFERENCES.md#source-package-patterns).

Validation on 2026-11-14:

- Both compiler stages compile and run separate producer and interface-only
  consumer modules with Core and STG Lint. All 81 observations pass: the
  previous 58 plus 23 nested-pattern, wildcard and clause-order observations.
  Runtime cutoffs produce heads `Just 1`, `Just 2`, `Nothing` and proof depths
  3, 2, 0. The source snapshots match the committed examples exactly.
- The runtime fixtures check 20 package/bound rejections. The previous shared
  equation restriction is replaced by positive guarded clauses. New checks
  reject a nested index escape with located GHC-05617 and a retained-field
  wildcard with GHC-83605. A standalone harness pins those results and the
  retained-field constructor rejection. No GHC API producer adds the examples.
- The successful bootstrap compiler build took 22.14s and peaked at 1,876 MiB.
  The self-hosted compiler target and dependencies built in 17m26s and peaked
  at 4,844 MiB. Both jobs used serial 16 GiB caps with swap disabled.
- The bootstrap source run peaked at 96 MiB under 4 GiB. The self-hosted
  source, Core API, bootstrap runtime and pinned negative checks pass together
  at 1,203 MiB under 4 GiB.
- All five focused registered tests pass: 39 cases, 34 skips, no unexpected
  results, and no framework failures or warnings. The driver took 25.560s.
  The full job took 3m41s and peaked at 3,029 MiB under 8 GiB.
- CorePrep/STG preserve distinct `n` and `m` fields in separate cases on the
  shared scrutinee. Proof/wildcard and catchall observations preserve the
  checked field demand. The previous-format reader's three hashes are
  unchanged. Fourmolu checks, compiler parsing, an ignored Cabal-gild copy and
  whitespace checks pass. The Default MR template requires regression coverage.

Artifacts use `_build/verification/native-package-pattern-`. Successful logs
are `bootstrap-build-v2.log`, `bootstrap-run-v1.log`, `selfhost-build-v1.log`,
`selfhost-run-v1.log`, `api-v1.log`, `bootstrap-runtime-v1.log`,
`negatives-v2.log` and `registered-v1.log`. Input, source, documentation,
harness and output manifests identify this checkpoint. The saved Agda
`Compile.hs` has Git blob `962a80bbaee38601c392a2f9bfd2af8ea0f972d1`.
The initial build log records a missing `hsPatType` import, fixed before v2.
The first probe needed its imported interface copied to the one-shot hidir;
the corrected baseline probes are in `probes-v2/`. These are separate from
the passing source execution and final negative checks.

Retained index constructor patterns, lazy package patterns, pattern bindings,
linear package elimination and general projections remain gated. Native
optimization, proof erasure and totality remain separate work. The full GHC
suite was not run. The source uncons target is verified below.

## Source vector uncons checkpoint

Implementation: `ed5b0f56e4`; source target and provenance: `5a297d202e`.
The Core Tidy adaptation was specified at `296eb26a01` after the full source
observer exposed the failure. The
[source library](examples/packed-vector/README.md#uncons-with-a-runtime-predecessor)
now defines `unconsPacked` and `toListPacked`. In the successor branch,
`PackVec k rest` stores the actual predecessor obtained from `S k`.
The checked case rule transports the captured vector. Ordinary GADT evidence
relates its tail to `k`. No singleton or inferred runtime predecessor is added.

The full observer's `S bottomLength` argument passed typechecking but failed
Core Lint after Tidy globalised `bottomLength`. Before tidying a native RHS,
the compiler now creates lazy local aliases for its free top-level indices.
Existing Core substitution replaces term, type and coercion occurrences
together. The alias RHSs retain the original top-level references. CorePrep's
existing native alias guard preserves the local index. Ordinary RHSs, the
index grammar and interface format keep their existing paths. See
Note [Local indices across Core Tidy] in `GHC.Iface.Tidy` and the
[pinned Agda sources](REFERENCES.md#source-vector-uncons).

Validation on 2026-11-14:

- Both compiler stages compile separate producer and interface-only consumer
  modules with Core and STG Lint. All 102 observations pass: the previous 81
  plus 21 uncons and draining observations. IORef lengths 0, 2 and 3 give
  `Nothing`, a tail of length 1, and a tail of length 2. Filtered inputs drain
  in their original element order. Result, pair, tail package, predecessor,
  head and payload demand are observed separately. A bounded cyclic prefix
  produces `[42,42,42]`.
- The Core API and bootstrap runtime controls pass. The runtime fixture
  checks 22 package/bound rejections. A separate harness pins three located
  GHC-25897 errors for the original length, successor length and an independent
  length used to package the tail. Some relevant-binding details still show
  generated capture names; the primary errors identify source locations.
- The bootstrap compiler target built in 28.72s and peaked at 2,027 MiB.
  The self-hosted target and dependencies built in 16m39s and peaked at
  4,698 MiB. Both jobs used serial 16 GiB caps with swap disabled.
- The bootstrap source run peaked at 99 MiB under 4 GiB. Core API and runtime
  checks peaked at 1,129 MiB under 4 GiB. The final self-hosted source and
  pinned rejection checks peaked at 108 MiB under 4 GiB.
- All five focused registered tests pass: 39 cases, 34 skips, no unexpected
  results, and no framework failures or warnings. The driver took 26.071s
  and the job peaked at 1,735 MiB under 8 GiB. The initial direct invocation
  omitted Hadrian's Make/tool environment. Its printer failure is preserved;
  v2 supplies that environment and runs the same five tests.
- The 22 compiler input hashes and the previous-format reader's three hashes
  remain unchanged. Both source snapshots match the example files. Fourmolu
  checks, compiler parsing, an ignored Cabal-gild copy and whitespace checks
  pass. The Default MR template requires regression coverage.

Artifacts use `_build/verification/native-uncons-source-`. Successful logs
are `bootstrap-build-v1.log`, `bootstrap-run-v2.log`, `selfhost-build-v1.log`,
`selfhost-run-v1.log`, `api-v1.log`, `bootstrap-runtime-v1.log`,
`negatives-v2.log` and `registered-v2.log`. `bootstrap-run-v1.log` preserves
the original Tidy Core Lint failure. Input, source, documentation, harness
and output manifests identify this checkpoint. The saved Agda vector source
has Git blob `8c03fb8a83e6cec1fa7b8e1f64bb7e606c531cb8`.

The source programs run at `-O0` with the existing native interface gates.
Native optimization, general projections, retained index patterns, proof
erasure and totality remain separate work. The full GHC suite was not run.
Next, probe runtime equality evidence and use it to zip two source packages.

## Source runtime equality and aligned zip checkpoint

Source and fixture commit `f3350988c3`, with frontend commit `78460ce4f4`,
passes all 136 source observations in both compiler stages. The consumer
imports only the producer interface, then links its object file. Core and
STG Lint run in both stages. The bootstrap runtime fixture and registered
runtime test pass all 27 package/bound rejection checks. Four standalone
equality-scope errors have pinned GHC-83865 codes and primary source tokens
in both stages. All five focused registered regressions pass: 39 cases,
34 skipped, five expected passes, no framework warnings or failures.

`sameNat` compares two independently supplied runtime lengths. Its successful
result contains ordinary `:~:` evidence. `zipPacked` matches `Refl` before
zipping their vectors. The source observations cover independent IORef
inputs, runtime-filtered vectors, mismatch laziness, package order, partial
lengths, vector tags, pair components and tails. Equal-length success
compares both finite Nat spines. `Nothing` carries no negative proof.
See the [source guide](examples/packed-vector/README.md#runtime-equality-and-aligned-zip).

The consumer required three bounded frontend changes. Index resolution
normalizes assigned Id metadata and validates current bindings. Result
matching can select a complete index while an ordinary payload type remains
unresolved. Fully applied wired-in `($)` can use a complete expected or
independently declared result and one cached right operand. It preserves the
original operator, concrete representation checks and final subsumption.
Higher-rank callbacks keep their expected-checking path. These changes add
no Core rule or interface format. The fork version remains
`10.1.20261113.2`.

The final bootstrap compiler build took 46.36 seconds and peaked at
1,852 MiB under 16 GiB. The self-hosted compiler target and dependencies
took 9m08s and peaked at 5,274 MiB under 16 GiB. Bootstrap source execution
peaked at 106 MiB under 4 GiB. Bootstrap runtime and standalone rejection
checks peaked at 999 MiB under 4 GiB. Self-hosted source and standalone
rejection checks peaked at 117 MiB under 4 GiB. The registered normal-way
run took 12.620 seconds and peaked at 1,647 MiB under 8 GiB. All final
commands passed with zero recorded swap.

Artifacts use `_build/verification/native-equality-zip-`. Final compiler
inputs are in `build-input-v5.SHA256SUMS`. Successful source logs are
`bootstrap-run-v5.log` and `selfhost-run-v2.log`. The updated runtime log is
`bootstrap-runtime-v2.log`; standalone diagnostics are `negatives-v3/` and
`negatives-v4/`. The registered command and environment are in
`registered-argv.json` and `registered-env-v1.json`. Source, documentation,
harness and output manifests identify the checkpoint. Earlier compiler,
source and harness failures remain saved. The previous-format reader,
executable and interface fixture retain their original three hashes.

An exploratory imported-global probe reached an unlocated late-pipeline
gate. It was excluded from the located rejection counts. M16 resolves the
declaration classification and checks the actual index restriction below. Native optimization,
general index expressions, totality and proof erasure remain separate work.
The full GHC suite was not run. Next, probe source append with a computed
runtime length and ordinary equality evidence. The
[M16 checkpoint](#source-append-with-a-runtime-sum-checkpoint) records that extension.

## Source append with a runtime sum checkpoint

Source and fixture `fba00f8f0c`, with compiler changes `491c5fb0c9`, pass all
180 source observations in both compiler stages. `sumNat` returns a runtime
length `p` and ordinary evidence `p :~: Add n m`. `appendPacked` matches
`Refl` before repackaging the appended vector at that length. Independent
IORef inputs and runtime filters preserve the left elements before the right
elements. The [source guide](examples/packed-vector/README.md#append-with-a-computed-result-length)
shows the implementation and demand observations.

The runtime fixture checks 34 package/bound rejections in bootstrap and
registered runs. Seven standalone sum/append errors pin GHC-05617 or GHC-83865
and the primary source token in both stages. A separate declaration-only
producer and interface-only consumer pass three observations: inferred
constructor parameters, partial VTA, and an unused retained field. The
self-hosted declaration checks also preserve the optimization, unfolding,
Core-serialization and profiling gates. Those option failures are separate
from the located source-error counts.

Constructor result matching now follows validated field metadata through a
fully applied real constructor's current signature. It chooses only outer
erased parameters. It keeps the actual instantiated telescope and normal
proof checking. An unused retained binder can have a canonical ordinary Many
arrow. Matching checks that arrow's closed domain. Module classification
also includes local retained worker types before implicit bindings are
generated. This gives declaration-only modules the existing checked native
pipeline. These changes add no Core rule or interface schema.

The final incremental bootstrap compiler build took 22.96 seconds and peaked
at 1,627 MiB under 16 GiB. The self-hosted compiler target and dependencies
took 9m10s and peaked at 4,510 MiB under 16 GiB. Final bootstrap source,
runtime and standalone diagnostic checks peaked at 1,022 MiB under 4 GiB.
Self-hosted source, diagnostics and capability checks peaked at 120 MiB
under 4 GiB. All five registered tests passed: 39 cases, 34 skipped, no
framework warnings or failures. The driver took 12.064 seconds and peaked
at 1,659 MiB under 8 GiB. Final commands recorded zero swap.

The earlier imported-global probe has a vacuous Boolean binder. GHC
canonicalizes that binder to an ordinary arrow, so its imported `otherwise`
argument is valid. It now compiles. A separate genuinely dependent stored
Boolean still rejects `otherwise` with GHC-83605 at that token. These two
scope controls passed at 49 MiB under 4 GiB.

Artifacts use `_build/verification/native-append-sum-`. Final compiler inputs
are in `build-input-v3.SHA256SUMS`. Passing source logs are
`bootstrap-run-v2.log` and `selfhost-run-v1.log`; the runtime log is
`bootstrap-runtime-v1.log`. Diagnostic directories `negatives-v2/` and
`negatives-v3/` retain sources, commands, messages and primary tokens.
`gates-v1/` records declaration capability checks. Registered command and
environment files are retained. Source, documentation, harness and output
manifests identify the checkpoint. The original declaration/application
failures and a failed bootstrap build remain saved. M15's 31 harness and
337 output hashes still match. The protected previous-format reader retains
its original three hashes.

The sum witness scans the entire first length before it returns a constructor.
A finite first length can leave the right length and vector payloads lazy.
The proof is an ordinary lifted field. Totality, proof erasure, native
optimization and general index expressions remain separate work. The full
GHC suite was not run. Next, probe constructor patterns on retained package
fields in [M17](PLAN.md#m17-constructor-patterns-on-retained-package-fields).


## Retained field constructor pattern checkpoint

Compiler implementation: `ab985a150e`; import cleanup: `f423a0ce97`.
Source and runtime fixture: `e54802bcfd`. Both compiler stages pass 209 source
observations with Core Lint and STG Lint. The separate consumer uses only the
producer interface and object. The bootstrap and registered runtime fixtures
pass 40 package/bound rejections. Seven new standalone pattern errors and the
seven existing sum/append errors have pinned codes and primary source tokens
in both stages. All five focused registered regressions pass.

The public source worker now reads:

```haskell
unconsPacked :: PackedVec a -> Maybe (a, PackedVec a)
unconsPacked (PackVec Z _) = Nothing
unconsPacked (PackVec (S k) (VCons x rest)) = Just (x, PackVec k rest)
```

The second row refines the vector using the actual stored length and packages
its tail with the actual predecessor `k`. The zero row leaves the vector lazy.
A separate `PackVec Z VNil` check confirms that an explicit vector pattern
forces that vector. The 29 new observations also cover two retained fields,
ordinary equality proof demand, partial predecessors, other arguments, views,
guards and fallback rows. A failed pattern can try another row. A demanded
bottom cannot.

`tcRetainedCon` is shared by retained function, case and package pattern
checking. A fresh whole-field Id sits in an outer generated as-pattern.
The inner constructor alternative binds the existing nominal index equality
using the actual source subfield Ids. Later field types receive that transport
in telescope order. Checked coercion patterns retain their original outer
types. Ordinary pattern checking keeps GADT evidence in scope. Existing
per-equation match compilation preserves source row order. No Core rule,
interface schema or backend lowering rule changes in this increment.
See [the pinned Agda LHS and RHS sources](REFERENCES.md#retained-constructor-fields-in-source-patterns).

Retained fields currently admit bare variables or one constructor layer with
bare variable subfields. Nested retained constructors, wildcards and source
as-patterns remain gated. This restriction does not apply to ordinary vector
or proof fields. The proof fields remain ordinary lazy Haskell fields. These
runtime observations do not prove totality, preservation or general proof
erasure. The full GHC suite was not run.

Artifacts use `_build/verification/native-index-pattern-`. The final compiler
input manifest is `build-input-v6.SHA256SUMS` with 26 paths. Exact Agda source
snapshots are in `prior-art/`. The initial rejection and three failed
bootstrap builds remain saved. `bootstrap-run-v4.log` and
`selfhost-run-v1.log` record the final source output. The final runtime log is
`bootstrap-runtime-v2.log`. Pattern diagnostics use `negatives-v4/` and
`negatives-v5/`; sum diagnostics use `legacy-negatives-v3/` and
`legacy-negatives-v4/`. Source, documentation, harness and output manifests
identify the checkpoint. M16's 26 harness and 326 output hashes still match.
The protected previous-format reader retains its original three hashes.

The self-hosted compiler target and dependencies took 10m40s and peaked at
5,640 MiB under 16 GiB. That build reported the redundant imports. The final
cleanup build rebuilt both stages in 1m54s and peaked at 3,883 MiB under
16 GiB, with no source warnings. Final source, bootstrap runtime and both
stages' diagnostic checks peaked at 1,030 MiB under 4 GiB. All five registered
tests passed: 39 cases, 34 skipped, no framework warnings or failures. The
driver took 12.844 seconds and peaked at 1,622 MiB under 8 GiB. The resource
wrapper disables swap. [M18 source reverse](PLAN.md#m18-source-reverse-with-the-original-stored-length)
is verified below.


## Source reverse with a preserved length checkpoint

Compiler implementation: `12bfbef0b3`. Source and runtime fixture:
`9fefa1a496`. Both compiler stages pass 249 source observations with Core Lint
and STG Lint. The bootstrap and registered runtime fixtures pass 47
package/bound rejections. Seven standalone reverse errors have pinned codes
and primary source tokens in both stages. The source consumer uses the
producer's interface and object with the producer source absent from its
import path. All five registered regressions pass: 39 cases, 34 normal-way
skips and five expected passes, with no framework warnings or failures.

`reverseAppendNative` reverses one vector onto an accumulator and returns
`Vec a (Add n m)`. The ordinary equality proofs `addSucc` and `rightZero`
follow Agda's addition lemmas. Matching `Refl` permits the accumulator step
and the final result at `n`. `reversePacked` stores the original runtime `n`
with the delayed vector. The native worker's signature relates input and
output lengths. It does not specify reversal order. Separate finite observations
check distinct elements selected by runtime predicates and an independently
sized accumulator.

The wrapper can return package WHNF without evaluating either field. Reading
the stored length does not demand the reversed payload. Demanding that payload
first completes the right-zero proof over the Nat spine, then scans the vector
spine. Elements remain lazy. The reverse-append worker has its own demand order:
it inspects the current vector constructor before its recursive successor proof.
The observations distinguish those demands and partial applications. Repeated
proof induction can traverse the remaining Nat spine at each step. This is not
a linear-time or totality claim. These proofs remain ordinary lifted values.

The consumer exposed `const (42 :: Int) (rightZero n)` under nested inference.
The signed literal was not eligible for immutable argument inference. Skipping
it left the first parameter unknown; ordinary fallback could not instantiate
the native proof type. Complete signed expressions now enter the existing
infer-once cache. A read-only syntax guard excludes wildcards, outer quantified
variables, written inner quantifiers, retained binders, contexts and type
splices. Normal signature checking and all final argument/result checks remain.
A signature supplies a checking type, not an equality proof. Both signed
argument positions and a signed function pass the controls in both compiler
stages. A wrong annotation still produces a located GHC-83865 error. See
[the exact source precedent](REFERENCES.md#signed-expression-arguments-in-native-inference).

The first proof used constructor equations with two retained arguments.
At M18, that form rejected with the misleading result-wrapper diagnostic.
The M18 source used the explicit-case form, which binds both runtime
indices before discrimination. M19 below now supports those equations. [M19](PLAN.md#m19-constructor-equations-with-later-retained-arguments)
records the required shared-identity extension and its source-error obligations.
No Core rule or interface schema changed in M18. Optimization, arbitrary index
expressions, general inference, totality and proof erasure remain separate work.
The full GHC suite was not run.

Artifacts use `_build/verification/native-reverse-source-`. Final compiler
inputs are in `build-input-v1.SHA256SUMS`, with 26 paths. Initial clause and
consumer failures remain saved. Five exact prior-art snapshots are in
`prior-art/`. The source, documentation, harness, outputs and per-directory
files have separate SHA-256 manifests. M17's 42 harness hashes, 499 output
hashes and the original three reader hashes remain unchanged.

The bootstrap compiler build took 25.73 s and peaked at 1898 MiB under 16 GiB.
The self-host build took 9m30s and peaked at 5172 MiB under 16 GiB. The final
bootstrap checks peaked at 1054 MiB under 4 GiB; the self-host source, reverse
errors and annotation controls peaked at 145 MiB under 4 GiB. The registered
run took 12.255137 s and peaked at 1718 MiB under 8 GiB. The resource wrapper
sets `MemorySwapMax=0`. Initial failures and final logs remain saved.


## Constructor equations with later retained indices checkpoint

Compiler: `fdf23acaea`, with local-comment cleanup at `a367bd0a7a`.
Source and runtime fixture: `dc44d9d25c`, with the missing-proof rejection at
`6f7d5f7e5f`. Both compiler stages pass 269 source observations. The bootstrap
and registered runtime fixtures pass 55 package/bound rejections. Eight new
errors have pinned GHC-83605/GHC-83865 codes and primary source tokens in both
stages; the previous seven reverse errors also pass. All five registered
regressions pass: 39 cases, 34 skips and five expected passes, with no framework
warnings or failures.

The existing `addSucc` and `reverseAppendNative` now use constructor equations.
Their later argument has different source names in each row. The checker
renames those resolved Names to one shared runtime identity per position,
including local captures and stored free-name sets. It preserves source spelling,
spans and original equations for diagnostic context. Shared indices remain
outermost lambda variables. Ordinary payloads still receive the existing
constructor transport, and evidence has separate specialised Givens.

Twenty new source observations cover multiple later indices, ordinary values
between retained positions, local captures, shadowing, partial applications
and demand order. A primitive equality context between retained binders checks
that the branch can use its transported evidence. A leading `Show` context
checks ordinary class dictionaries before the retained telescope. This required
the signature selector, renamer and kind checker to continue through qualified
bodies with their existing constraint routines. [References](REFERENCES.md#later-retained-arguments-in-constructor-equations)
pin the Agda telescope rules and ordinary GHC context routines.

Later retained patterns still require bare variables. Both quotation forms
reject at the quotation expression before resolved Names can change. Wrong
result indices still reject, and an error in the second row prints that row's
original variable names. Unsupported ordinary patterns point at that pattern.
No Core rule or interface schema changed. The full GHC suite was not run.

Artifacts use `_build/verification/native-later-index-`. Final selected compiler
inputs are in `build-input-v5.SHA256SUMS`, with 27 paths. All five historical
input versions have exact archived bytes and verified hashes. The initial failed
build and the qualified-body source failure remain saved. Six exact prior-art
snapshots cover Agda telescopes, clause compilation, the next equality example,
and GHC's ordinary qualified-signature routines.

The initial self-host build took 12m08s and peaked at 5002 MiB under 16 GiB.
It reported the new local Haddock warning and pre-existing warnings in App,
Match/Constructor and Main/Passes. The final cleanup rebuilt both compiler
stages in 1m33s, at 2653 MiB under 16 GiB, without source warnings. Final
bootstrap source/API/runtime checks peaked at 1048 MiB under 4 GiB. Self-hosted
source and both diagnostic groups peaked at 147 MiB under 4 GiB. The registered
run took 13.905708 s and peaked at 1726 MiB under 8 GiB. The wrapper sets
`MemorySwapMax=0`.

M18's 31 harness and 372 output hashes, M17's 42 harness and 499 output hashes,
and the original reader's three hashes remain unchanged. The final source,
docs, harness, output and directory manifests are frozen separately.
[M20](PLAN.md#m20-constructor-equations-over-two-runtime-indices) below continues
with a complete two-index constructor matrix for `sameNat`.


## M20: source equations over two runtime indices

Both compiler stages and all five registered regressions pass.
`PackedVecLibrary.sameNat` now has four source equations. Each row refines
both runtime indices. The `S/S` row calls the same worker on its actual
predecessors. It must match the recursive `Refl` before returning `Just Refl`.
The source result stays `Maybe (n :~: m)`. `zipPacked` consumes that evidence
before calling the equal-length vector zipper.

The checker keeps shared whole-argument Ids and each row's actual field Ids.
It lifts the first constructor proof through a type. It lifts the second
proof through that result, then composes the coercions. Each proof sits on
its own inner `ConPat`, beneath the shared whole-argument `AsPat`. Payloads
keep their original outer types. Each RHS casts back beneath both proofs.
The existing one-column path remains. Desugaring compiles proof-bearing rows
separately. All source argument lambdas precede discrimination. This change adds no Core or
interface forms or equality axioms.

Both compiler stages pass 301 observations through separate producer
and importer compilations with Core and STG lint. The previous 269 lines stay
in order. Sixteen IORef comparisons check lengths 0 through 3 against a private
nested-case worker and an independent expected table. Ten checks compare
partial WHNF, bottom order, lazy mismatches and bounded cyclic mismatch.
Four payload matrix cases place an ordinary vector argument before the second
retained column. Two more checks cover partial WHNF and index-before-payload
demand. The API/runtime fixture passes 63 package/bound rejections.

The initial matrix requires two constructor columns, complete unique pairs,
one unguarded RHS per row and bare constructor subfields. Other retained
positions keep M19's bare-variable rule. Contexts within this matrix telescope,
mixed constructor/variable rows and deeper constructor fields reject. This
restriction applies to dependent checking. `mkDepFunTy` turns an unused retained
binder into an ordinary unrestricted arrow and keeps the runtime argument.
The coverage probes use `Maybe (n :~: m)` even when every RHS is `Nothing`.

The eight new standalone probes check incomplete and duplicate pairs, a wrong
domain, a nested field, both false zero/successor proofs, a missing recursive
proof and an interior context. The existing eight later-index and seven
reverse probes also pass in both stages. The old mixed `LaterConstructor`
probe now reports the matrix pattern error at its later variable.

The pinned [Agda equality example and split-tree compiler](REFERENCES.md#two-index-constructor-equations)
supply the program and compilation precedents. `Nothing` carries no refutation.
Equal cyclic inputs have no termination claim. The next source probe is an
ordered trailing variable fallback. It must preserve proof scope and the
same demand order before this pattern restriction can be relaxed.


Validation uses `_build/verification/native-index-matrix-*`. Final compiler
bytes are in `build-input-v3.SHA256SUMS` with 27 selected inputs. Three input
archives preserve both failed compiler attempts and the final implementation.
The initial compiler probes found a pretty-printing `(<>)` collision and a
list/`NonEmpty` mismatch. Both are fixed. Direct `evaluate` and `seq` reference
probes reported GHC-91028. The final reference checks use a generic WHNF helper
with fixed `IO ()` result. A global `ImpredicativeTypes` probe hit the existing
index-expression gate. It is not part of the accepted source flags.
The first coverage probe returned `Int` and used ordinary checking. The
corrected dependent result rejects at the first equation's group location.
All failed probes remain in the evidence. The full GHC suite was not run.

The final bootstrap compiler rebuild took 1m27s and peaked at 1823 MiB under
16 GiB. The self-hosted build took 12m49s and peaked at 5182 MiB under 16 GiB.
It reports pre-existing warnings in `App.hs`, `Constructor.hs` and `Passes.hs`.
The final bootstrap source run peaked at 146 MiB under 4 GiB. Bootstrap
matrix/API/runtime checks peaked at 993 MiB under 4 GiB. Self-hosted source
and all located errors peaked at 144 MiB under 4 GiB. The registered run took
12.425778 s and peaked at 1723 MiB under 8 GiB. The wrapper disables swap.

M19's 40 harness and 596 output hashes, M18's 31 harness and 372 output hashes,
M17's 42 harness and 499 output hashes, and the original reader's three hashes
remain unchanged. M19's 34 source and five document hashes also match commit
`7e2d2a6224`. Final M20 source, docs, harness, output and directory manifests
are frozen separately.


## M21: an ordered variable fallback from source

Both compiler stages and all five registered regressions pass.
The library now uses the ordinary ordered fallback form:

```haskell
sameNat :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)
sameNat Z Z = Just Refl
sameNat (S left) (S right) = case sameNat left right of
    Nothing -> Nothing
    Just Refl -> Just Refl
sameNat left right = Nothing
```

The final equation has no constructor proof. The checker renames both source
variable Names to the shared whole-index Names before TC. It checks the RHS
against the original result type and keeps its index VarPats unwrapped.
It uses reflexive transports, without calling the constructor-only lifting
helper. Constructor rows keep their actual fields and M20's two proofs.
GHC's existing ordered `PgCon` and `PgAny` blocks put fallback execution
outside failed row proofs. The backend and Core/interface forms stay the same.
Note `[Retained variable fallback]` records these invariants. The
[Agda catchall and GHC failure-continuation sources](REFERENCES.md#ordered-variable-fallback-equations)
record their exact blobs and ranges.

Both stages pass 316 observations through separate producer/importer
compilations with Core and STG lint. The first 301 lines remain unchanged.
The existing 16 finite equality pairs and ten demand checks now also compare
with a private four-clause worker. The nested-case reference remains.
Four new runtime package pairs repackage vectors under the actual fallback
indices. Four `zeroOnly` pairs check a worker with only `Z/Z` before fallback.
Seven new demand checks cover an untouched second index when the first
constructor is absent, the opposite demand, first-before-second errors,
partial function WHNF and lazy packages at mismatched partial indices.

The API/runtime fixture passes 70 package/bound rejections. Seven new
standalone errors reject an earlier fallback, a repeated fallback, wildcard
patterns, a forged `Refl`, a wrong-index payload and both quotation forms.
Their exact source positions are pinned. The previous 23 located errors
also pass in both stages. The earlier/repeated fallback error points to the
first misplaced variable pattern. A fallback has no equality between its
original indices. It cannot inherit evidence from an earlier failed row.

The initial extension accepts only one final equation with bare variables in
both selected index columns. Constructor pairs before it must remain unique.
Without it, constructor-pair coverage must remain complete. A row with one
constructor and one variable, an anonymous wildcard, guards and interior
contexts remain unsupported. Other retained columns retain their existing
bare-variable rule. Quotations reject before source Name renaming.
The next source task is an anonymous final fallback such as `sameNat _ _`.


M21 evidence uses `_build/verification/native-index-fallback-*`. The final
`build-input-v2.SHA256SUMS` has 28 selected inputs. It includes the original
`compiler/ghc.cabal.in` template. Compiler changes are `c3a068a7fe`; source and
runtime-fixture changes are `571ae3d690`. The implementation plan and pinned
Agda/GHC provenance were committed first in `80ba85869f`.

Compiler and source checks passed on the first attempt. A formatter check
used cabal-gild's positional in-place mode and changed the Cabal template's
formatting. The original bytes were restored from HEAD. The diff and formatted
bytes remain as evidence. The corrected command writes a separate output.
Both compiler targets then passed a verification build in 54.83 s, with no
compiler modules recompiled. Fresh producer/importer source executions in both
stages and the fresh API fixture pass against the restored template. The final
source and Cabal input bytes are pinned separately from the initial 27 inputs.

The bootstrap compiler build took 3m06s and peaked at 2710 MiB under 16 GiB.
The self-hosted build took 12m15s and peaked at 5009 MiB under 16 GiB. Existing
warnings remain in `App.hs`, `Constructor.hs` and `Passes.hs`. The verification
build peaked at 742 MiB under 16 GiB. Initial bootstrap source execution peaked
at 144 MiB under 4 GiB; source rejections and API/runtime checks peaked at
995 MiB. Self-hosted source and located errors peaked at 145 MiB under 4 GiB.
The final restored-state source/API checks peaked at 1111 MiB under 4 GiB.
The wrapper disables swap. The full GHC suite was not run.

M20's 42 harness and 514 output hashes, M19's 40 harness and 596 output hashes,
M18's 31 harness and 372 output hashes, M17's 42 harness and 499 output hashes,
and the original reader's three hashes remain unchanged. M20 and M19 source
and document hashes match commits `74cf40fd57` and `7e2d2a6224`. Final M21
source, docs, harness, output and directory manifests are frozen separately.


The final registered run took 13.666040 s and peaked at 1667 MiB under 8 GiB:
five tests, 39 cases, 34 skips and five passes. The earlier run also passed
in 13.129000 s at 1708 MiB. The complete compiler, source and registered checks
pass with the restored Cabal template. No Cabal file change remains.


## M22: anonymous terminal fallback from source

Both compiler stages and all five registered regressions pass. The library
now ends `sameNat` with `sameNat _ _ = Nothing`. Each selected fallback column
accepts a bare variable or wildcard. The checker replaces a selected wildcard
with a VarPat carrying its existing shared whole-index Name before TC. This
binds the actual index Id for later payload and result types. It introduces no
source-visible name, fresh index Id or equality proof. The fallback retains
reflexive transports and the original result type. Constructor rows keep their
actual fields and proofs. Core, lowering and interface forms stay the same.
Note `[Retained variable fallback]` records these rules. The
[Agda anonymous-context and GHC wildcard sources](REFERENCES.md#anonymous-terminal-fallback-equations)
are pinned by blob hash and exact source ranges.

Both stages pass 331 observations through separate producer/importer source
compilations with Core and STG lint. The original 316 outputs remain unchanged.
The existing 16 finite equality pairs and ten demand checks also compare with
a named fallback worker. Four new runtime pairs check `first _` and `_ second`
package workers against independent stored-length/content expectations.
A `_ _` zero-only worker also compares with its named reference. Eleven new
demand observations check a missing first constructor, a cyclic first index,
first-before-second failure, partial function WHNF and lazy package fields.
Forcing the selected payload still raises its original error.

The API/runtime fixture passes 76 package/bound rejections. Each compiler stage
also passes 42 standalone located errors: the previous 23 later-index, reverse
and matrix cases, plus 19 fallback cases. New controls reject forged Refl and
wrong-index payloads for all three wildcard combinations. They check original
source equation text, early or repeated fallbacks, both anonymous quotation
forms, an unsupported third-index wildcard and an unbound source name.
A wildcard supplies no user-visible binding. Mixed constructor/wildcard rows,
guards, contexts and other pattern extensions remain outside this probe.

The first source run used unsigned local vector bindings. The existing inferred
retained-type gate rejected both at their source Names. The final probe uses
inline payload expressions. Complete ambient-index local signatures were also
unsupported at M22. M23 below implements that source-scope rule. The initial
failure remains in the artifacts.

Evidence uses `_build/verification/native-index-wildcard-*`. The 28 selected
compiler inputs are archived in `inputs-v1` with `build-input-v1.SHA256SUMS`.
The plan/provenance commit is `df9600f757`; frontend `7b7586bfe0`; source and
existing runtime fixture `2435b1a7df`. The bootstrap build took 40.90 s and
peaked at 1836 MiB under 16 GiB. The self-hosted build took 7m59s and peaked
at 4333 MiB under 16 GiB. No GHC source warnings were emitted; Cabal's existing `${pkgroot}` prefix
warning remains. Bootstrap source
execution peaked at 151 MiB under 4 GiB; located errors and API/runtime checks
peaked at 996 MiB. Self-hosted source and located errors peaked at 147 MiB
under 4 GiB. The registered run took 12.427260 s and peaked at 1722 MiB under
8 GiB: five tests, 39 cases, 34 skips and five passes. The wrapper disables
swap. The full GHC suite was not run.

M21's 26 manifest bytes, 42 harness hashes and 600 output hashes remain
unchanged. Its 35 source hashes and five document hashes match `cdd782d28e`.
M20, M19, M18 and M17 harness/output evidence and the original reader's three
hashes remain intact. M22 source, docs, harness and outputs are frozen
separately.

## M23: local signatures at runtime indices

Both compiler stages and all five registered regressions pass. A complete
local value signature can refer to an eligible
already-bound runtime index under `ExperimentalDependentTypes`:

```haskell
n <- readIORef cell
let xs :: Vec Int n
    xs = replicateNative @Int n 7
```

The signature uses the actual runtime `n`. The renamer adds a signature-local
lookup alias before implicit forall collection. The checker validates the
existing unrestricted local Id and its closed index domain. It reads assigned
metadata without assigning a native metavariable, inventing equality or
evaluating `n`. Validity applies the same metadata normalization to the lexical
binding. Notes `[Local retained signature aliases]` and
`[Local runtime indices in signatures]` state these rules.

An erased type variable or explicit `forall` binder takes precedence. Under
the experimental flag, a free `m` in `depth :: LE n m -> Int` captures a local
runtime `m`. Use `forall n m.` to preserve erased polymorphic intent. The
example's proof observer now states that intent. Named wildcards keep their
ordinary meaning; a runtime `_n` requires `NoNamedWildCards`. This extension
does not infer unsigned local native bindings or capture globals and members
of the same recursive group.

Both source clients pass 343 observations, preserving the previous 331.
Twelve additions check finite IORef indices, repeated payload sharing, runtime
shadowing, erased-variable precedence, `_n`, partial and cyclic indices,
payload demand and deferred IO effects. The API/runtime fixture passes 87
package/bound rejections. Standalone probes pass 53 located errors, including
eleven new signature cases. Four ordinary-Haskell controls pass in both stages,
including RequiredTypeArguments capture of an erased variable. Source
compilation uses separate producer and importer modules with Core and STG Lint.

Evidence uses `_build/verification/native-local-signature-*`. Compiler inputs
are archived in `inputs-v1` and `inputs-v2`; the latter records the lexical
validity fix. The specification is `b84462b0bc`, frontend `d45eac2a85`, validity
fix `da16ec16e6`, source/API fixture `58fe0a1e10` and source formatting
`e4766d99ac`. Initial source failures remain recorded. No Core, backend or
interface extension was needed.

The final bootstrap rebuild took 21.90s and peaked at 1774 MiB. The self-hosted
build took 12m53s and peaked at 5200 MiB. Both used 16 GiB caps. Final bootstrap
source execution peaked at 149 MiB under 4 GiB. Its API/control run peaked at
1018 MiB. Self-hosted source, errors and both-stage ordinary controls peaked
at 231 MiB under 4 GiB. The registered run took 12.63s and peaked at 1725 MiB
under 8 GiB: five passes, 34 skipped ways and no unexpected results. Swap was
disabled for all jobs. Existing GHC and Cabal warnings remain. The full GHC
suite was not run. Source, docs, harness and outputs are frozen separately;
the M22 evidence and earlier runtime/interface records remain unchanged.

At M23, the saved direct `xs <- action` client failed when
`action :: IO (Vec Int n)`. M24 below implements that source path.

## M24: ordinary IO binds with native payloads

Both compiler stages and all five registered regressions pass. A saved action
preserves its native payload type through ordinary do:

```haskell
n <- readIORef cell
let action :: IO (Vec Int n)
    action = pure (replicateNative @Int n 7)
xs <- action
pure (toList xs)
```

The checker fixes bind's input type as `Vec Int n` before it checks the
generated continuation. It exposes existing expected-type assignments and
keeps native choices in an immutable substitution. Missing ordinary parameters
use GHC's standard fresh TauTvs. Inline `pure` can keep its ordinary head meta
until final result subsumption resolves it to IO. A separate joint match must
still satisfy every cached argument. Known native result choices also anchor
checking. Kind-first matching determines representation parameters for error
without a native meta assignment or special error rewrite.

Notes `[Available expected types in native applications]`,
`[Ordinary parameters in native applications]` and
`[Ordinary heads in native argument matching]` record these decisions.
[Pinned Agda lambda, implicit-argument and application sources](REFERENCES.md#direct-do-bind-of-a-saved-native-action)
supply the checking and scope precedents. GHC retains its own meta, kind,
scope, concreteness, dictionary-order and usage safeguards. The do translation,
Core forms, lowering and interface version remain unchanged.

Both source clients pass 360 observations, preserving the previous
343. Four runtime IORef lengths compare saved and inline do/bind forms and
two inferred local computations. Per-call counters check effects. Thirteen
further observations check deferred effects, ignored bottom fields, original
errors, partial and cyclic indices and shared payloads. The API fixture passes
94 package/bound rejections. Standalone probes pass 60 located errors and four
ordinary-Haskell controls. New errors cover independent indices, both wrong
monads, conflicting cached inputs, native-result inference, unsigned native
bindings and linear capture.

Evidence uses `_build/verification/native-action-bind-*`. Four compiler input
archives retain each implementation increment. The first self-hosted build
was interrupted by a VM restart. Its log is retained. The resumed build passed;
the final build includes the later expected-result fixes. The final bootstrap
build took 20.17s and peaked at 2049 MiB under 16 GiB. The final self-hosted
build took 6m22s and peaked at 5060 MiB under 16 GiB. Bootstrap source and
bottom probes peaked at 189 MiB under 4 GiB. Its rejection/API run peaked at
1027 MiB. Self-hosted source and controls peaked at 195 MiB under 4 GiB. The
registered run took 12.57s and peaked at 1844 MiB under 8 GiB: five passes and
34 skipped ways. No unexpected results occurred. Swap was disabled for all
jobs. Existing GHC/Cabal warnings remain; the full GHC suite was not run.
Source, docs, harness and outputs are frozen separately. M23 and earlier
evidence remain unchanged. At M24, native-result inference and unsigned native
bindings remained gated. [M25](#m25-scoped-native-source-inference) admits the
scoped source forms below.

## M25: scoped native source inference

Both compiler stages and all five registered regressions pass. Local values
and callback results can infer native types at an existing runtime index:

```haskell
n <- readIORef cell
let vector = replicateNative @Int n 7
    action = modifyIORef' effects (+ 1) >> pure vector
    compute = do
      xs <- action
      pure xs
xs <- compute
pure (PackVec n xs)
```

The existing single nonrecursive FunBind path infers its RHS before allocating
the Id. M25 admits the resulting monotype for a local NoGen value with one
zero-argument equation. The validity checker checks free runtime indices
against their actual lexical bindings. It also checks its existing deep
free-variable list, since the structural walk skips variable kinds and
coercions. Bound indices and erased CoVars are excluded from that list's
runtime checks. No native type is assigned to a meta-variable.

After an action fixes a callback input, the existing match checker infers the
body through an InferResult. Its result is checked after the callback context
closes. A callback-local or inner local runtime Id cannot escape in an
ordinary arrow result. Explicit retained arrows and packages keep their own
checking paths. Generic argument matching can unify ordinary head metas for
`effect >> pure vector`, then repeats exact whole-argument matching. Both heads
must contain no native syntax or outer templates. Final subsumption remains.

Notes `[Scoped inferred native values]`, `[Scoped native callback results]`,
`[Ordinary action heads in native inference]` and
`[Validity of retained binders]` record the rules.
[Pinned Agda let, lambda and contextual-meta sources](REFERENCES.md#scoped-native-source-inference)
supply scope and checking precedents. GHC retains its own laziness, usage,
FRR, promotion, generalization, kind, concreteness and evidence checks.
The callback admission starts with one ordinary Many argument, a variable
pattern and one unguarded equation. Recursive groups, pattern bindings,
partial signatures and inferred retained function telescopes remain gated.

Both source clients pass 377 observations, keeping the previous 360 unchanged.
The 17 additions cover four IORef inputs, repeated action effects, bottom
indices and payloads, original errors, partial/cyclic prefixes, shared vectors
and the original shared IORef payload. Both stages pass 69 located rejection
cases and four ordinary controls. Two M24 rejection forms are now positive;
the other 58 remain. Eleven new cases cover scope, independent indices,
generalization, GADT escape and linear capture. The API/runtime fixture passes
103 package/bound rejection cases. The five registered tests pass with 34
skipped ways, no unexpected results and no framework warnings.

Evidence uses `_build/verification/native-source-inference-*`. Three compiler
input archives each contain 31 selected paths. The final self-hosted build
took 11m28s and peaked at 5761 MiB under 16 GiB. Final both-stage source/API
checks peaked at 1099 MiB under 4 GiB. The registered run took 14.07s and
peaked at 1734 MiB under 8 GiB. Swap was disabled. Full GHC tests were not run.
Earlier evidence and the original reader remain preserved. Initial source
failures, the rejected Hadrian target and a corrected probe-generator error
are retained. The deep metadata check is justified by the structural audit;
no source-level exploit is claimed. Core and the interface version are unchanged.

The next failure is `pure ((42 :: Int), xs)` inside the inferred callback.
Equivalent `(,)` and ordinary polymorphic helper applications compile.
[M26](PLAN.md#m26-boxed-tuple-source-inference) completes tuple syntax inference.

## M26: inferred boxed tuples

Both compiler stages accept native fields in complete boxed tuple syntax:

```haskell
n <- readIORef cell
let action = pure (replicateNative @Int n 7)
    compute = action >>= \xs -> pure ((42 :: Int), xs)
pair <- compute
pure (PackVec n (snd pair))
```

For an `Infer` result under `ExperimentalDependentTypes`, a read-only native
anchor permits expansion to the ordinary tuple data constructor. The existing
application checker caches fields and chooses native types immutably. Ordinary
fields can use fresh ordinary parameters. The tuple-size check remains.
The expansion retains the written tuple context and field locations.
Known expected types retain component checking. Tuple sections and unboxed
tuples retain their existing paths. Core and the interface version are unchanged.

Note `[Inference of native tuple fields]` in `GHC.Tc.Gen.Expr` owns the rule.
The argument classifier refers to that Note.
[Agda product, Sigma and constructor references](REFERENCES.md#boxed-tuple-inference-prior-art)
record the precedent. An ordinary tuple does not bind a new runtime index.
Returning an inner index with its vector still requires an explicit package.

Both source clients pass 395 observations. The previous 377 remain unchanged.
The 18 additions compare syntax, constructor and helper forms at IORef inputs
0 through 3. They check effects, lazy tuple fields, original errors, partial
and cyclic prefixes, and sharing of the vector and original IORef payload.
Both stages pass 79 located rejection cases and four ordinary controls.
Ten new rejections cover index escape, independent indices, linear usage,
generalization, field mismatch, GADT escape and the unsupported native section.
Ten tuple compile controls cover nested tuples, unsigned literals, linear
lets, known expected types, ordinary higher-rank fields, sections and unboxed
tuples. The API/runtime fixture passes 113 package/bound rejections.
All five registered regressions pass, with 34 skipped ways and no framework
warnings or unexpected results. Full GHC tests were not run.

Evidence uses `_build/verification/native-boxed-tuple-*`. Three archives contain
32 selected compiler inputs each. The self-hosted build took 6m16s and peaked
at 7210 MiB under 16 GiB. Final source/API checks peaked at 1047 MiB under
4 GiB. Registered tests took 13.10s and peaked at 1907 MiB under 8 GiB.
Swap was disabled. Failed setup runs and the original M25 evidence remain.

The next baseline compiles an ordinary unit case returning the tuple.
It also compiles `((,) xs) (42 :: Int)`. Applying `(xs,)` still reports
GHC-83865. These are compile/Lint controls, not runtime validation.
[M27](PLAN.md#m27-native-tuple-sections) targets that separate section path.

## M27: native tuple sections

Both compiler stages accept inferred and expected boxed tuple sections:

```haskell
n <- readIORef cell
let action = pure (replicateNative @Int n 7)
    compute = action >>= \xs ->
        let section = (xs,)
         in pure (section (42 :: Int))
pair <- compute
pure (PackVec n (fst pair))
```

The checker retains `ExplicitTuple` and its existing desugaring. It infers an
anchored native present field once with its exact type and checks its lifted
kind. Inference keeps ordinary missing-field metas. A known native function
type supplies the tuple components and hole multiplicities. Whole-function
subsumption checks the reconstructed section against that expected type, so
input and result casts stay connected. Projection crosses only ordinary
visible arrows. Other expected shapes retain their existing path.
The owning `[Inference of native tuple fields]` Note records these rules.
Core and the interface version are unchanged.

Both source clients pass 417 observations, preserving the previous 395 outputs.
The 22 additions compare sections and constructor applications at IORef lengths
0 through 3. They check effects, repeated applications, hole order, ignored and
demanded bottom fields, partial/cyclic prefixes, and sharing of a named captured
vector and its original IORef payload. Existing desugaring places present
expressions inside the missing-field lambdas. It adds no sharing let; repeated
applications need not share a newly evaluated present expression.

Both stages pass 88 located rejections and four ordinary runtime controls.
The old section rejection is now positive; the other 78 M26 cases remain.
Ten new cases check scope, independent indices, mismatched holes/fields,
linear usage, generalization, lifted kinds and the existing native forall gate.
Ten section compile controls cover inferred and expected sections, multiple
holes, nested storage, linear captures, all ordinary hole multiplicities and
ordinary higher-rank checking. The API/runtime fixture passes 122 package/bound
rejections. All five registered tests pass, with 34 skipped ways and no
unexpected results or framework warnings. The full GHC suite was not run.

Evidence uses `_build/verification/native-tuple-section-*`. Two compiler input
archives contain 32 selected paths each. The self-hosted build took 7m00s and
peaked at 4674 MiB under 16 GiB. Bootstrap source/API checks peaked at 1063 MiB
under 4 GiB; the remaining both-stage checks peaked at 242 MiB under 4 GiB.
Registered tests took 13.68s and peaked at 1917 MiB under 8 GiB. Swap was
disabled. Failed discovery probes and all earlier frozen evidence remain.
The [pinned prior art](REFERENCES.md#native-tuple-sections-and-application-spines)
distinguishes Agda's typing precedent from GHC's demand and sharing behavior.

The next compile/Lint baseline stores a section in a pair. Naming `fst pair`
before applying it compiles; direct `(fst pair) (42 :: Int)` reports GHC-83865
in both stages. This is the separate generic overapplication path in
[M28](PLAN.md#m28-native-generic-result-overapplication), not section creation.

## M28: applying native generic results

Both compiler stages accept direct application of a projected native function:

```haskell
n <- readIORef cell
let vector = replicateNative @Int n 7
    action = pure ((vector,), True)
    compute = action >>= \stored -> pure (fst stored (42 :: Int))
pair <- compute
pure (PackVec n (fst pair))
```

`fst` declares one argument domain. Its result becomes a function after the
checker infers the stored pair. The generic route now uses that nonempty
declared prefix under the existing NoQL, native-anchor, VTA, kind, scope and
concreteness guards. Prefix matching starts with an empty substitution because
the final expected result follows the extra arguments. Exact saturation keeps
its existing result seed. Native-index-kind instantiation stays exactly
saturated. The original application pipeline checks extra arguments, evidence,
multiplicities and the final result. Core and the interface version are unchanged.
`[Generic retained argument inference]` owns the rule; the cache Note refers to it.

Both source clients pass 439 observations, preserving the previous 417 outputs.
The 22 additions compare direct/named `fst`, `id`, `const` and an Eq-constrained
projection at runtime IORef lengths 0 through 3. Counters check one action per
call. Demand checks distinguish unused and applied bottom functions/pairs,
ignored and demanded vector fields, the constrained tag, partial/cyclic
prefixes and sharing of captured vectors and original IORefs.

Both stages pass 99 located rejection cases and four ordinary runtime controls.
The new matrix has nine compile controls and eleven rejections. It covers
multiple extra arguments, cache position two, nested contexts, known final
types, linear captures and expected higher-rank extra domains. Wrong arguments,
results, joint prefix choices, scopes, dictionaries and linear usage reject.
A separate failed-match trace counts one `prefixProbe` application elaboration
in each stage. The API/runtime fixture passes 133 source rejection cases.
All five registered regressions pass, with 34 skipped ways and no unexpected
results or framework warnings. The full GHC suite was not run.

Evidence uses `_build/verification/native-generic-overapp-*`. Two input archives
contain 32 selected compiler paths each. The self-hosted build took 6m21s and
peaked at 6386 MiB under 16 GiB. Bootstrap source/API checks peaked at 1069 MiB
under 4 GiB; final both-stage checks peaked at 350 MiB under 4 GiB. Registered
tests took 12.71s and peaked at 1872 MiB under 8 GiB. Swap was disabled.
Earlier frozen evidence, the initial VTA probe and trace-parser failures remain.
[Pinned Agda and upstream GHC sources](REFERENCES.md#generic-result-overapplication-prior-art)
record the application-spine precedent and the limits of TH counter evidence.

At M28, the next compile/Lint baseline omitted or supplied a leading `@Bool`.
The omitted form worked; partial VTA reported located GHC-83865 in both stages.
Complete ordinary VTA worked. [M29](PLAN.md#m29-native-generic-inference-after-leading-vta)
will attempt prefix inference after GHC checks the written type arguments.


## M29: native inference after leading visible type applications

A selector can now infer a remaining generic type after leading VTA:

```haskell
selectVTA :: forall tag value. tag -> (value, Bool) -> value
selectVTA _ (value, _) = value
-- vector has the exact type Vec Int n at an existing runtime index n.
result = selectVTA @Bool True ((vector,), False) (42 :: Int)
```

ITYARG checks the written `@Bool`. Before IALL allocates the remaining
variables, the native matcher uses the substituted signature and declared
argument prefix. Immutable choices pass through IALL in telescope order.
Checked type arguments and dictionaries keep their order. Only the NoQL
`tcApp` route enables this inference, before any value argument and with no
remaining type arguments. Public callers that cannot consume an argument
cache leave it disabled. No Core form or interface format changes.

The checker retains inferred argument caches even when matching fails.
Value-cache keys count values. Diagnostic positions also count written VTA;
generated record-dot type arguments do not shift them. The owning Note is
`[Native inference after visible type applications]` in `GHC.Tc.Gen.App`.
[Pinned Agda and GHC sources](REFERENCES.md#native-inference-after-leading-visible-type-applications)
record the application, substitution and generated-argument precedents.

Both compiler stages pass 461 source observations, preserving the earlier
439 lines. The 22 additions compare omitted, partial, named and two-leading
VTA calls at runtime IORef lengths 0 through 3. They check action effects,
index/payload/tag/function demand, partial/cyclic indices and captured-vector
and original-IORef sharing. Both stages pass 112 located rejection cases and
four ordinary runtime controls. The new matrix has ten positive cases and
fourteen negative cases. It covers contexts, linear usage, higher-rank extra
arguments, kinds, scopes, dictionaries, generated type arguments and the
Quick Look gate. Interleaved VTA remains outside this increment. Wrong tag
and extra-value errors retain second/fourth argument contexts. Deep
annotation errors retain their nested expression contexts. Failed matching
elaborates the traced `prefixProbe` application once per stage.

The API fixture passes 146 source rejection cases. All five registered
regressions pass, with 34 skipped ways and no unexpected results or framework
warnings. The full GHC suite was not run. Evidence uses
`_build/verification/native-leading-vta-*`. Three archives each retain 32
selected compiler paths. The first self-host build was stopped to correct
one Note sentence about generated type arguments. The corrected bootstrap
and self-host builds took 15.94s and 5m08s; their serial job peaked at
7303 MiB under 16 GiB. Final both-stage source/API checks peaked at 1217 MiB
under 4 GiB. Registered tests took 13.76s and peaked at 1877 MiB under 8 GiB.
Swap was disabled. Frozen M28 and earlier evidence remains unchanged.

The separate native-index-kind route still runs only in its existing prepass.
Both stages compile `observe vector` and `observe @Int @Z VNil`, but reject
`observe @Int vector` when the remaining erased variable has kind `Nat`.
That compile/Lint baseline reports GHC-83865 at `localVector n`, with a
second-argument context. [M30](PLAN.md#m30-native-index-kind-inference-after-leading-vta)
will reuse this route after leading VTA under its exact-saturation guards.


## M30: native-index inference after leading visible type applications

The exact-saturated native-index route now runs after leading VTA:

```haskell
observe :: forall a (index :: Nat). Vec a index -> Int
observe _ = 42
-- vector has type Vec Int n at the existing runtime Nat n.
result = observe @Int vector
```

ITYARG first checks `@Int`. The existing immutable matcher infers the
remaining Nat index from the vector type. IALL consumes the choices in the
original telescope and dictionary order. NoQL, extension, head/concreteness,
kind, exact-domain and lexical scope guards remain. Index-kind inference
requires exact saturation; generic result overapplication keeps its separate
route. Successful and failed matches retain their argument caches.

The owning Note `[Native inference after visible type applications]` records
both routes. The cached-inference Note refers to it. The Quick Look argument
context now adds the written-type-argument offset, as final argument checking
already does. `observe @Int xs` reports its retained-type rejection at `xs`,
with a second-argument context. Generated type arguments do not add an offset.
This diagnostic adjustment does not change matching or instantiation.
[Pinned Agda and GHC sources](REFERENCES.md#native-index-kind-inference-after-leading-vta)
record the existing application and inference rules. Core representation and
the interface version are unchanged.

Both compiler stages pass 479 source observations; the previous 461 outputs
remain unchanged. The 18 additions compare omitted, partial and complete
ordinary VTA, including two independent runtime indices and nested contexts.
They check action counts, ignored bottom index/vector values, original vector,
index and element errors, bounded partial/cyclic prefixes, left/right demand,
and sharing of named vectors and original IORefs. Inferring an erased index
does not force its runtime value.

Both stages pass 126 located rejection cases and four ordinary runtime
controls. The new matrix has ten positive and fourteen negative cases. It
covers remaining ordinary binders, an incoming index dictionary without an
instance, linear use, contexts, mismatches, kinds, escapes, interleaved VTA and
Quick Look. The failed-match trace elaborates `prefixProbe` once per stage.
The API fixture passes 160 source rejection cases. All five registered
regressions pass, with 34 skipped ways and no unexpected results or framework
warnings. The full GHC suite was not run.

Evidence uses `_build/verification/native-index-vta-*`. Three archives each
retain 32 selected compiler paths. The final bootstrap and self-host builds
took 16.97s and 6m52s; their serial job peaked at 4583 MiB under 16 GiB.
Final source/API checks peaked at 1229 MiB under 4 GiB. Registered tests took
13.24s and peaked at 1812 MiB under 8 GiB. Swap was disabled. The failed API
assertion remains: the API printer uses promoted `'Z` while the CLI prints
`Z`. The corrected API assertion checks the stable third-argument context;
the CLI still checks the closed-index fragment and exact source tokens.
Frozen M29 and earlier evidence remains unchanged.

The next strict compile/Lint baseline passes a named selection of a native
function from a combined vector/payload tuple. Its direct application rejects.
A separate payload also rejects, including an ordinary `Int -> Int` payload:
the index route only infers domains that mention an index binder, then requires
complete choices. Both stages agree on two accepted and four rejected forms.
[M31](PLAN.md#m31-joint-native-index-argument-inference-and-result-overapplication)
will infer the index and payload together from the declared prefix. A later
capability is safe ambient equality beneath a fixed-domain retained Pi;
optimizer support requires its own substitution and scope audit.

## M31: joint index and payload inference

The declared argument prefix can now determine both a native index and an
independent payload type. The selected payload can receive further arguments:

```haskell
selectIndex :: forall a (index :: Nat) value. Vec a index -> value -> value
selectIndex _ value = value
-- witness :: Vec Int n at an existing runtime Nat n.
ordinary = selectIndex @Int witness ((+1) :: Int -> Int) 42
retained = selectIndex @Int witness (replicateNative @Int) m 7
```

The retained result in the second call uses the independent runtime index `m`.
The first inference phase matches index-bearing arguments. The second phase
infers eligible independent domains whose parameters remain unknown. This
order preserves expected-type checking for `filterBounded (const False) xs`:
the vector determines the predicate's element type. The first implementation
inferred that predicate too early and rejected the existing source program.
Commit `1728237cc8` corrects the order.

Only the nonempty declared prefix supplies inference choices. Extra arguments
use the existing application checks. Overapplication starts with an empty
substitution because the final expected result follows those extra arguments.
New independent-domain and extra-argument inference requires a head without
concreteness requirements. Concrete heads retain exact index-only inference.
Complete immutable index choices, NoQL, kind, exact cached-domain, lexical
scope, usage and final subsumption checks remain. Native mutable fills remain
forbidden. No Core form or interface version changes.
The [pinned references](REFERENCES.md#joint-native-index-prefix-inference-and-result-overapplication)
distinguish Agda's application/substitution precedents from this GHC inference
policy. The owning Note is `[Cached retained argument inference]`.

Both stages pass 503 source observations. The previous 479 outputs remain
unchanged. The 24 additions compare named and direct selections, omitted and
leading VTA, ordinary and retained function payloads, independent IORef indices,
contexts, effects, demand and sharing. Both stages pass 139 located rejection
cases and four ordinary runtime controls. The new matrix has fourteen positive
and fourteen negative cases. One former M30 overapplication rejection now
compiles. All six forms in the previous composition baseline compile.
Failed matches in each inference phase elaborate `prefixProbe` once per stage.
The API fixture passes 173 rejection cases. All five registered regressions
pass, with 34 skipped ways and no unexpected results or framework warnings.
The full GHC suite was not run.

Evidence uses `_build/verification/native-joint-prefix-*`. Canonical archives
v2, v3 and v4 retain 32 selected compiler paths each. The v4 bytes match v3;
the final bootstrap and self-host builds took 21.12s and 6m39s. The self-host
build peaked at 4386 MiB under 16 GiB. Source/API checks peaked at 1336 MiB
under 4 GiB. Registered tests took 14.19s and peaked at 1822 MiB under 8 GiB.
Swap was disabled. The partial v1 archive, the initial predicate regression,
and a failed wrapper path remain as evidence. The initial formatter command
changed the Cabal template in place; the original template was restored before
the canonical archives. Frozen M30 and earlier evidence remains unchanged.

The next six-case baseline isolates outer equality beneath a retained Pi.
Ordinary function transport, identical retained functions, pointwise transport
under `a ~ b`, and family-axiom normalization compile in both stages. Returning
the whole retained function under `a ~ b` rejects with GHC-83865 at `f`.
The corresponding no-proof control also rejects. See the
[M32 plan](PLAN.md#m32-outer-equality-evidence-under-a-retained-pi).


## M32: outer equality evidence under retained functions

Whole dependent functions now reuse an existing `a ~ b` Given beneath a
retained binder. The direct definition preserves the runtime function:

```haskell
transportElement :: forall a b. a ~ b
                 => (foreach (n :: Nat) -> Vec a n)
                 -> (foreach (n :: Nat) -> Vec b n)
transportElement f = f
```

The pointwise control `transportElement f n = f n` remains useful for demand
comparison. Direct transport of a bottom function is bottom at function WHNF.
The pointwise form returns a lambda until application. No eta equality or Core
form was added. A separate helper eliminates an explicit `Refl` proof with a
case expression. It preserves proof-before-function demand.

Evidence can also mention an outer runtime index, as in `a ~ Vec Int k`, while
the returned function binds an independent `n`. The source caller writes
`transportOuterIndex @(Vec Int k) k original`. Complete visible type arguments
now use the lexical capture rules from complete local signatures. They retain
the existing Name and Unique. Erased variables take precedence. Native meta
assignment remains restricted. Captured partial types, globals, unsupported
index domains, linear indices and quotations remain outside this capture path.

The rewrite environment records every active retained Id. A Given's predicate,
canonical RHS and evidence must have no deep free occurrence of those Ids.
The check includes variable kinds and coercion metadata. It rejects coercion
holes. Existing flavour, role and dependency rules still apply. Ordinary
rewriting avoids these extra scans. The native Given checker accepts opaque
runtime indices without assignment. It disables family cycle breaking because
restoration later writes a metavariable. See Notes `[Rewriting retained
dependent functions]`, `[Rigid retained indices]` and `[Runtime indices in
visible type arguments]`.

Both stages pass 532 observations. The previous 503 outputs
remain an exact prefix. New checks cover IORef-fed comparisons, proof scope,
bottom and cyclic indices, function identity, named-result sharing, original
IORef payloads and outer captured-vector identity. Separate calls do not imply
sharing of newly generated vectors. Each compile/Lint matrix passes
39 cases: twenty-two positive and seventeen located rejections. The API fixture
passes 190 package rejections, with 272 rejection checks in total. Five old
cases now report a concrete constraint mismatch with GHC-05617 instead of
GHC-25897. Their source declarations and location checks are unchanged.
Each stage passes all 156 located standalone rejections, including the 139
retained M31 programs. Four ordinary runtime controls still print `42`.
Both failure-phase traces report one `prefixProbe` elaboration. All five
registered regressions pass; 34 cases are skipped. There are no unexpected
failures or framework warnings.

The final 37-file compiler input archive is `native-pi-given-inputs-v5`.
The bootstrap build took 25.24 seconds and peaked at 1811 MiB under 16 GiB.
The final self-host build took 6m29s and peaked at 4334 MiB under 16 GiB.
The API fixture peaked at 1015 MiB under 4 GiB. The registered run peaked at
1930 MiB under 8 GiB. Swap was disabled. The full GHC suite was not run.
The `native-pi-given-*.SHA256SUMS` manifests bind the source, documentation,
harness and outputs. Earlier milestone evidence remains frozen.

Agda's pinned Pi comparison and context extension motivate the scope rule.
Its explicit equality rewrite explains the proof-elimination comparison.
GHC's Given, role and cycle-breaker rules determine the FC implementation.
See [the provenance and implementation references](REFERENCES.md#outer-given-evidence-beneath-a-retained-pi)
and [the runtime source example](examples/packed-vector/README.md#outer-equality-evidence-under-retained-functions).


Review also pins same-name runtime shadowing and generated wildcards. A type
splice can introduce `_` after the parsed completeness scan. Checking the
actual `HsWildCardTy` under a captured VTA rejects it before meta allocation.
The ordinary spliced-wildcard control still compiles. The final matrix records
these cases with the target external interpreter.

The next six-case baseline accepts two controls. Three retained equality
operands reject at the source type-position gate with GHC-43582. A return
without proof rejects with GHC-91028. See the
[M33 plan](PLAN.md#m33-retained-function-types-as-equality-operands).

## M33: retained function types as equality operands

Checked source type arguments now admit retained Pi types. This includes
whole-Pi operands of `~` and `:~:`. The implementation reuses the retained
telescope checkers. It preserves index scope, closed domains, `Many` usage,
lifted bodies and complete visible arguments. It does not special-case
equality names. Type synonyms use the same checked formation. Native meta
assignment remains blocked, including through aliases.

```haskell
exposePi :: forall value.
            value ~ (foreach (n :: Nat) -> Vec Int n)
         => value -> (foreach (n :: Nat) -> Vec Int n)
exposePi f = f
```

The direct example can match an irreducible Given. Rewriting below another
retained binder needs the additional immutable Given check. It recursively
checks the Pi RHS and scopes the body reduction with `mkDepFunRedn`.
Occurs checks and the M32 scope filter remain active. Wanted decomposition,
native family cycle breaking, structural Pi unification and native meta
assignment remain gated.

The source example adds 30 observations. IORef-fed inputs compare direct,
pointwise and explicit-proof calls. The checks cover bottom-function WHNF,
proof-before-function demand, ignored bottom/cyclic indices, productive
prefixes and function/vector sharing. `liftExposedPi` rewrites a whole Pi
equality beneath another retained binder without eta expansion.

| Validation | Final result |
| --- | --- |
| Source execution | 562 observations in each compiler stage; previous 532 outputs are an exact prefix |
| New strict source matrix | 42 cases per stage: 21 compile controls and 21 located rejections |
| Retained checks | 156 located rejections, 104 compile controls, four ordinary runtime controls and two failed-match cache traces per stage |
| API/runtime fixture | 210 package / 291 total source rejections, 22 quiet compile controls and the 562-observation embedded oracle |
| Registered regressions | Five expected passes, 34 skipped cases, no unexpected results or framework failures/warnings |
| Next source baseline | One ordinary qualified-arrow pass and five located failures in each stage |

Source formation is in `6963da76a7` and `826f47d880`. The immutable Given
check is in `050d928510`. Runtime examples are in `4acfc34dc8`; the existing
API fixture is updated in `33a0755a1f`. The final compiler archive contains
37 selected files at `c8fa644189` in `native-pi-operand-inputs-v5`.
All five input archives match their recorded Git blobs.

The final bootstrap build took 26.85 seconds; the final self-hosted build
took about 1m13s. Their combined job peaked at 2840 MiB under 16 GiB.
The earlier functional self-host build took 7m39s and peaked at 4292 MiB.
The final CLI scope peaked at 357 MiB under 4 GiB. The API fixture peaked
at 1023 MiB under 4 GiB. Registered tests took 14.18 seconds and peaked at
1902 MiB under 8 GiB. Swap was disabled. The full GHC suite was not run.
Nonmutating Fourmolu parsing and cabal-gild validation pass.

The `native-pi-operand-*.SHA256SUMS` manifests bind source, documentation,
harness and output evidence. They preserve failed attempts as well as final
results. M32 and earlier evidence remains frozen. See
[the source example](examples/packed-vector/README.md#m33-retained-function-types-as-equality-operands),
[the Core rules](CORE.md#family-axiom-normalization-under-retained-binders)
and [the provenance](REFERENCES.md#retained-function-types-as-equality-operands).

The next baseline rejects four qualified-Pi equality signatures with
GHC-91510 at `forall`. The missing-proof body rejects with GHC-91028.
The trace reaches source validity before Given rewriting. M34 must address
that boundary first. See [the next plan](PLAN.md#m34-qualified-retained-function-types).

The [paper ledger](papers/README.md) now records five conference papers and
selected thesis chapters, with eight distinct PDF versions. Their typing,
role and erasure results guide the implementation. They do not establish
preservation, termination or a runtime correspondence theorem for this fork.

## M34: qualified retained function transport

This section records the frozen M34 checkpoint. It left witness construction
and proof-demand checks open. M35 below completes those checks. Both compiler
stages built with the
selected 37 compiler inputs from `9cbab37151`. Later commits add source
controls and documentation without changing those compiler inputs.

A producer can export `QPi = foreach (n :: Nat) -> Eq Int => Vec Int n` and
transport a value through an incoming `value ~ QPi` assumption. The producer
uses `ImpredicativeTypes` for this equality operand. A separate consumer with
`NoImpredicativeTypes` specializes the helper from its complete expected arrow.
Run the [source example](examples/qualified-pi/README.md) to inspect this path.

A known monomorphic variable needs no type instantiation. The application
checker uses `NoQL` for that restricted case. It preserves final subsumption
and returns an existing type through the ordinary inference result when
needed. Deep free-variable checks exclude existing Quick Look instantiation
variables. Applied, polymorphic and leading-context heads retain their guards.
This does not add general native Quick Look or infer new native function types.

The immutable native Given check now traverses ordinary dictionary arrows.
It also expands native synonyms before the non-tau constructor guard. The
expanded predicate and result retain the existing occurs, role, scope and
multiplicity checks. The mutable native-meta guard remains active. There is
no new Core constructor, interface schema or function eta expansion.

| Check | Result |
| --- | --- |
| Compiler stages | Bootstrap and self-hosted builds pass |
| Previous packed-vector executable | 562 observations per stage, unchanged |
| Qualified retained executable | 22 observations per stage through separate interfaces |
| Focused source cases | 26 per stage: 13 accepted, 13 rejected or currently unsupported |
| Located diagnostic envelopes | 14 in those rejection cases; the inferred alias has two |
| Previous Pi operand matrix | 42 per stage: 21 accepted and 21 rejected |
| Previous standalone rejections | All 177 per stage retained, including those 21 |
| Other retained controls | 104 compile positives, four ordinary runtime controls and two cache traces per stage |
| API source inventory | 221 package and 302 total rejection programs; 35 quiet positives |
| API golden | 37 lines, unchanged |
| Focused registered regressions | Five passes across 39 cases, with 34 skips |
| Changed registered fixture | Retested after strengthening: one pass across 11 cases, with 10 skips |
| Full GHC suite | Not run |

API inventory counts come from the fixture structure and successful runs.
They are not printed runtime counters. The final bootstrap API run and the
self-hosted registered fixture pass the strengthened occurrence controls.
All focused runs have zero unexpected or framework failures and warnings.

The runtime example covers finite independent indices, ignored and demanded
bottom, cyclic prefixes, partial application, payload sharing and two function
`StableName` comparisons at `-O0`. Six observations use `withDict` and a
single-method `Demand` class. They distinguish vector shape from method demand.
They do not inject a bottom global `Eq Int` dictionary.

The two isolated predicate-occurrence controls keep the vector element type
unchanged. Both reject at the final `f`. The synonym trace records
`cteInsolubleOccurs` and keeps an `IrredCt`; the successful beneath-binder trace
keeps an `EqCt`. This checks the recursive predicate boundary separately from
an unrelated element-type mismatch.

At the M34 checkpoint, `withQ` checked an incoming equality witness. Separate
`Refl` and bottom-proof callers at `QPi :~: QPi` rejected with GHC-91028. Proof
demand and proof order remained unverified for that caller path. The original tuple-under-Pi source
shape compiled, but the M34 runtime example used an ordinary vector argument
there. M35 below completes these acceptance checks.

The [paper ledger](papers/README.md) now records seven works and nine downloaded
PDF versions, including the corrected author copy of the Quick Look paper.
It records authors, publication versions, hashes and selected reading ranges.
The [provenance section](REFERENCES.md#qualified-retained-function-types)
separates the Agda context rules, the paper algorithm and this GHC adaptation.
No paper theorem is claimed for this implementation.

Evidence is isolated under `_build/verification/native-qualified-pi-*`.
Earlier failed probes and superseded input archives remain available.
The M33 manifests and committed source/documentation versions are preserved.
The compiler build used the serial 16 GiB cap and peaked at 4892 MiB.
The final API and affected regression used the serial 4 GiB cap and peaked
at 1123 MiB. Both scopes disabled swap and exited successfully.


## M35: retained functions in algebraic containers

Both compiler stages build from the 37 selected compiler inputs at
`d09a21e379`. Later commits update the existing API fixture and documentation.
M35 completes the M34 witness-caller, proof-demand and tuple runtime checks.
The [public example](examples/qualified-pi/README.md) passes 48 observations
through separate interfaces. Its first 22 outputs preserve the M34 oracle.

Exact expected matching now traverses saturated algebraic containers of kind
`Type`. Each argument keeps its incoming Pi scope. A Pi in one tuple field
does not admit an independent qualified or erased-forall sibling. The existing
immutable matcher still requires complete choices, kind agreement, scope,
roles and final subsumption. General native Quick Look and mutable native-meta
assignment remain unsupported.

A single ordinary constructor prefix now uses the ordinary pattern checker.
The expected first argument must be an actual `DepFunTy` before retained
constructor coverage applies. This admits newtype patterns and leading GADT
proof patterns without inserting function eta expansion. Multiple ordinary
constructor rows remain unsupported.

Native simplification also expands compulsory newtype constructor templates.
These constructors have no runtime binding. Keeping their Ids as runtime calls
caused the saved LFInfo panic. The narrow exception retains the activation,
substitution and beta checks. General compulsory unfolding remains disabled.
See Notes [Checked retained lambdas] in `GHC.Tc.Gen.Match` and
[Restricted dependent simplification] in `GHC.Core.SimpleOpt`.

| Check | Result |
| --- | --- |
| Compiler stages | Bootstrap and self-hosted builds pass |
| Qualified/container executable | 48 observations per stage; 22 earlier outputs unchanged |
| Packed-vector executable | All 562 earlier observations per stage unchanged |
| Container source matrix | 26 per stage: 17 accepted, nine rejected or currently unsupported |
| Located diagnostic envelopes | 11 across those nine source rejections |
| Earlier qualified-Pi matrix | All 26 per stage unchanged: 13 accepted and 13 rejected |
| API source inventory | 229 package and 309 total rejection programs; 53 quiet positives |
| API golden | 37 lines, unchanged |
| Focused registered regressions | Five passes across 39 cases, with 34 skips |
| Full GHC suite | Not run |

The runtime checks distinguish ignored proofs, partial application and demand
at the returned function's WHNF. A demanded proof fails before the bottom
function or index. The original tuple-under-Pi result now runs at independent
runtime indices. Newtype and tuple transport preserve the tested function
identity. Named and lambda data patterns demand the constructor but can leave
its function field unused. Newtype patterns can ignore a bottom box. A leading
`Refl` pattern transports vector contents and preserves the tested identity.
These observations apply at `-O0`; they are not a general sharing theorem.

Two former restrictions become positive controls. `undefined` accepts a
complete `Maybe (foreach ...)` expected type through immutable matching.
`transportRefl Refl f = f` checks through the corrected ordinary pattern route.
The missing, unrelated, escaping and representational evidence controls still
reject. The source inventory comes from these fixture changes and successful
runs; it is not a printed runtime counter. The quiet total includes the existing
standalone splice control, which earlier reported subtotals omitted.
At M35 the API provider compiled as object code without linking.
M87 adds [interpreted module loading](README.md#try-a-retained-function-in-ghci).

At M35, fully applied `withQ (error ...) f` rejects at `f` with GHC-91028 when
its result does not determine the erased parameter. The complete expected
`QPi -> QPi` arrow makes the partial helper work. M36 below selects the missing
parameter from an independently known native argument and cache that check.
The [plan](PLAN.md#m36-native-worker-arguments-after-full-application) keeps the
native-meta and Quick Look guards explicit.

The [references](REFERENCES.md#exact-matching-through-algebraic-containers)
record Agda's scoped argument and pattern checking. Ordinary GHC Notes supply
the newtype representation and demand rules. The paper ledger remains seven
works with nine verified PDF versions. No paper theorem is claimed here.

Evidence uses the fresh `native-pi-container-*` prefix. Failed source attempts
and all three compiler archives remain available. The M34 prefix and its
historical source and documentation remain frozen. The self-hosted compiler
build peaked at 4363 MiB under 16 GiB. The final checks peaked at 2020 MiB under
4 GiB. Both scopes disabled swap. All focused registered checks have zero
unexpected failures, framework failures and framework warnings. The earlier
177 standalone rejection corpus was not rerun in full for M35.

## M36: native worker arguments

Full witness and equality-Given calls now compile from source with
`NoImpredicativeTypes`:

```haskell
via :: QPi -> QPi
via f = withQ Refl f

viaBottom :: QPi -> QPi
viaBottom f = withQ (error "bottom proof") f
```

The final `QPi` result does not determine the erased `value` parameter.
The compiler infers the independently known `f :: QPi` once and selects
`value := QPi`. It then checks the proof against the complete witness type.
Direct `withQ Refl f n` and `exposeQ f n` also check the extra runtime argument.
The separate-module [public example](examples/qualified-pi/README.md) exercises
these calls at four lengths read through IORefs.

The new mode uses the existing `NoQL` argument cache. It infers only original
native-free tau domains on a known head without concreteness requirements.
The original body or context must contain native syntax. Match kinds before
types with one restricted immutable substitution. Require every invisible
choice, its substituted kind, exact cached domains and current runtime scope.
Keep cached expressions and usage on failure. The trace confirms one
elaboration of `prefixProbe` when conflicting arguments force fallback.
The ordered plan preserves evidence groups before the first visible arrow
and after leading VTA. Final subsumption still runs. This adds no Core or
interface constructor and does not assign native types to mutable metas.

| Check | Result in each compiler stage |
| --- | --- |
| Full and partial source calls | Both forms compile; five full applications in the initial baseline reported GHC-91028 |
| Worker source matrix | 52 clients: 27 compile, 25 reject or remain unsupported; 35 located diagnostic envelopes |
| Closed-proof control | Separate `qRefl = Refl` with ImpredicativeTypes reports GHC-83605 |
| Qualified/container/worker executable | 57 observations; all 48 earlier output lines preserved |
| Previous packed-vector executable | All 562 output lines unchanged |
| Previous container source matrix | 18 compile and eight reject; only the full bottom-proof call changes admission |
| Previous qualified-Pi source matrix | All 26 outcomes, codes and locations unchanged |
| API source inventory | 232 package and 312 total rejection programs; 61 quiet positives |
| API execution | Bootstrap and self-hosted registered execution preserve the 37-line golden |
| Focused registered tests | Five expected passes across 39 cases, 34 skips, zero unexpected failures or framework warnings |

The added runtime observations distinguish ignored proof and function bottoms,
proof-before-function demand, `Refl` exposing a bottom function, ignored-proof
function identity and lazy partial application. These results apply to the
checked programs at `-O0`. They do not establish optimizer-wide equivalence.

The source matrix includes a named proof obtained from a `ProofBox`, scoped
runtime indices, repeated arguments, expected-result choices, nested contexts
and leading VTA. A direct `ProofQ` parameter or qualified `@QPi` without
`ImpredicativeTypes` reports the separate formation error GHC-91510. An erased
forall after a visible arrow remains outside the collected telescope. Four
applications with `ImpredicativeTypes`, including explicit VTA and a known
monomorphic worker, form successfully but report GHC-83605 in Quick Look.
The [M37 plan](PLAN.md#m37-applications-with-impredicativetypes) starts from
those measured controls. General native Quick Look and mutable native-meta
assignment remain gated.

The [provenance](REFERENCES.md#native-worker-argument-inference) pins four full
Agda source files and the previous GHC checker. Agda normally checks arguments
in telescope order and retains blocked checks with their context. Its source
also records an abandoned postponement experiment. The GHC change uses the
existing cache; it does not transfer Agda's mutable-meta or eta policies.
The paper ledger remains seven works and nine verified PDF versions.

Evidence uses `native-worker-arguments-*`. The selected compiler source is
`c2f86121b675ab36c491043b4bd623e05e1a99b7`, with 37 archived inputs. Both
compiler stages build. The self-hosted build takes 7m07s and peaks at 4326 MiB
under 16 GiB. Fourteen serial validation phases and two reused matrix checks
pass. The validation scope peaks at 2209 MiB under 4 GiB. Both scopes disable
swap. Failed and superseded source attempts remain available. The M35 prefix
and older evidence remain frozen. The full GHC suite and the full earlier
177-program standalone rejection corpus were not run for this checkpoint.

## M37: fixed native applications with ImpredicativeTypes

Direct source applications now work when explicit type arguments or a complete
expected result fix the native choices. The four preserved M36 application
failures compile in both stages. The public example contains this call in a
separate module with `ImpredicativeTypes`:

```haskell
groundDirect :: foreach (n :: Nat) -> QPi -> Vec Int n
groundDirect n f = withQ @QPi Refl f n
```

Its caller reads `n` from an `IORef`. It passes lengths zero through three and
checks the resulting vector contents. Other direct calls use a named closed
`qRefl = Refl`, a monomorphic proof worker, an intervening `Eq` dictionary and
an equality-Given worker. The extra checks cover inferred kind arguments,
phantom native type arguments, bottom proofs and function identity.
The [source runner](examples/qualified-pi/README.md) compiles each module through
separate interfaces with Core and STG Lint at `-O0`.

The checker prepares only the leading type, pragma and wrapper arguments.
It uses the existing IALL/ITYARG loop and checks each type argument once.
It selects ordinary checking only after it excludes remaining erased choices
and free QL variables in the signature, expected type and saved metadata.
A failed selection retains the prefix and its original QL variables.
A complete expected result can also choose `Refl` or `error` arguments
immutably. The actual result type remains available for final subsumption.
A nominal Given can discharge an equality; representational evidence cannot.
The change adds no Core form, equality axiom or eta expansion.

| Check | Result in each stage |
| --- | --- |
| New source matrix | 64 clients: 42 compile, 22 source errors, 28 located diagnostic envelopes |
| Predefined source oracles | All 31 pass; the remaining 33 were exploratory observations |
| Original 61 source clients | 39 compile; three additional prefix-metadata controls also compile |
| Failure-cache ownership | `prefixProbe` elaborates once |
| Raw and public executable | All 76 lines match the authored oracle; the previous 57-line prefix is unchanged |
| Previous packed-vector executable | All 562 lines unchanged |
| API source inventory | 233 package and 312 total rejection programs; 76 quiet positives |
| API execution | Bootstrap API and self-hosted registered execution preserve the 37-line golden |
| Focused registered tests | Five expected passes across 39 cases, 34 skips, zero unexpected failures or framework warnings |

The unused phantom-kind trace creates unresolved inferred variables that do
not occur in the residual `Int` type. They remain in the saved type arguments.
The checker retains `DoQL`, and later monomorphisation visits those same
variables. In the filled-kind control, ordinary VTA checking assigns `Type`
to the inferred kind before the checker selects `NoQL`.
The [provenance](REFERENCES.md#applications-with-fixed-native-type-choices)
pins the Quick Look rules, corrected paper pages and Agda application checker.
The paper ledger remains seven works and nine verified PDF versions.

Six exploratory forms still report GHC-83605. They cover a signed witness,
a signed application head, broader ambient inference and a polymorphic
applied head. A qualified written operand without `ImpredicativeTypes` still
reports GHC-91510. General native Quick Look, native instantiation across erased
foralls after visible arguments and mutable native-meta assignment remain
outside this checkpoint.
The [M38 plan](PLAN.md#m38-native-choices-from-known-arguments-with-impredicativetypes)
starts by measuring native choices supplied by known arguments.

Evidence uses `native-ground-applications-*`. The selected compiler source is
`89c98cf7b4a23b048f8ffd77170537f27a434224`, with 37 archived inputs. Both stages
build. The self-hosted build takes 8m03s and peaks at 4512 MiB under 16 GiB.
Ten selected validation phases pass. Two API/registered attempts failed after
a newly positive fixture reused a module name. A unique name fixes that
fixture; only those two checks were repeated. Both attempted source versions
and failed results remain available. Validation scopes peak at 1331 MiB under
4 GiB, with swap disabled. The M36 evidence and its older preservation chain
remain frozen. The full GHC suite, the full earlier 177-program standalone
corpus and the older qualified-Pi standalone matrix were not rerun.

## M39: checked beta exposure in Core

The native index bridge now contracts a bounded immediate lambda application.
Identity applied to a runtime Nat variable keeps that actual variable.
Successor produces the corresponding constructor index. Constant zero can
ignore a bottom-valued variable. The argument must normalize to an eligible
variable or nullary constructor. Opaque functions and constructed beta arguments
remain gated. M39 changed Core conversion. Source lambda arguments remained
unsupported at that checkpoint; M40 adds the checked source subset below.

`normaliseNativeIndexExpr` returns the runtime normal form and index together.
CorePrep consumes that pair before naming. A variable result stays that same
Id. Constructor results use the existing nominal alias transport. The original
expression still needs Lint. The owning Note and
[contract](CORE.md#checked-beta-exposure) specify these obligations.
[Agda's pinned substitution code](REFERENCES.md#core-contract-audit-and-bounded-beta-exposure)
is the primary implementation precedent. The D/DC beta rules supply a separate
typing obligation. Neither proves this implementation's Haskell runtime rules.

Both compiler stages build. The 13 selected phases pass with 11 launches and
two reused bootstrap API checks. Both stages preserve the 41-client matrix
(27 compile, 14 located errors) and prior 64-client matrix (43 compile,
21 located errors). All 47 predefined controls pass. Raw and public
programs preserve 94 observations per stage. All 562 packed-vector observations
remain unchanged. Five registered tests pass across 39 cases with 34 skips
and no unexpected failures or framework warnings.

Quiet additions to the existing fixtures check normalization, exact types,
reused-Unique capture avoidance, wrong domains, multiplicity, scope, and opaque
unfoldings. Binary proof expressions and imported unfoldings pass all three
compression modes. Direct CorePrep checks run before SimpleOpt. They include
`keep` returning 42 with a bottom index, plus constructor-field demand controls.
No new tracked test or golden output was added.

Two unit attempts failed on old identity-lambda rejection expectations.
Those cases are now positives; genuine opaque producers retain rejection
coverage. The passing third unit attempt and original CoreRun success are
reused with matching fixture and compiler hashes. Both failures remain saved.
The source-program corpus remains 243 package/322 total rejection programs
and 95 quiet positives; these counts exclude the new raw beta checks.

Evidence uses `native-index-beta-*`. Compiler source
`707e681f284061f18b3e2bca590b27d153f2146a` has an original 37-input archive.
The independent audit found that this inherited list omitted both changed Core
files. The post-build `dependent-haskell-M39-complete-inputs-v1` supplement
records their exact committed/current bytes. It covers 39 selected inputs and
53 source paths. It does not claim an exact 39-file pre-build snapshot. The
pre-build Core hash and later caller hashes match. The original 37-file reuse
identity remains explicitly qualified; no source mismatch was found.
Bootstrap build: 1m56s, peak 2008 MiB. Self-hosted build: 10m58s, peak 4502 MiB.
Both use a 16 GiB cap. Final validation peaks at 1196 MiB under 4 GiB.
Swap is disabled. M38 and the older evidence remain frozen. The full GHC
suite and older standalone corpora were not rerun. ROADMAP C1/C2 remain open.

## M40: checked source lambda indices

The retained argument checker now admits an immediate single-variable,
unguarded lambda application. The lambda has no local bindings. Its argument
must normalize to an eligible local variable or nullary constructor. Its body
must remain in the supported index grammar. Nested applications and lexical
shadowing preserve the actual binder Name and Unique. Constructor fields use
their own declared domains. The original argument still passes ordinary
checking once.

For example, the source client now checks:

```haskell
betaSuccessor :: foreach (n :: Nat) -> Int -> Vec Int (S n)
betaSuccessor n x = replicateNative @Int ((\k -> S k) n) x
```

`Strict` rejects this new form with located GHC-83605. Expression signatures,
non-variable patterns, opaque producers, lets, cases, residual lambdas and
non-atomic beta arguments remain gated. No Core form, equality rule or general evaluator was
added. An unsigned captured local alias can still have flexible multiplicity
at this boundary. M39 and M40 both reject that case. A complete local `Nat`
signature supplies `Many`; the signed alias works with and without beta.

Both stages pass the 41-client source matrix: 16 compile and 25 reject with
located diagnostics. Each stage separately checks six actual zonked lambda
Ids before desugaring. All six have `Nat` domain, `Many` multiplicity and the
renamed source identity, including under `LinearTypes`. The inherited 41- and
64-client matrices preserve their outcomes, codes and source tokens. Their
47 predefined controls and argument-cache checks pass.

All 17 final phases pass, with 12 launches and five phase reuses.
Raw and public programs pass 106 observations in each stage, with the original
94 lines unchanged. The runner compiles `IndexBetaClient` in a separate
directory. Its 12 observations cover finite inputs, shadowing, ignored and
demanded bottoms, partial successor fields, cyclic prefixes and demand at
function WHNF. Both stages also preserve all 562 packed-vector observations. Five
registered tests pass across 39 cases with 34 skips and no unexpected failures
or framework warnings. The API goldens remain one and 37 lines. The existing
source-program corpus remains 243 package/322 total rejection programs and
95 quiet positives; these counts exclude the new source matrix and raw checks.
No tracked test or golden output was added.

Selected compiler: `5853b765f3319ed994bb088f4ce257a61bbeafc5`. The fixture
diagnostic update is in `366f94c32f5bea38c1a45916e0667aecaa52ac97`. The complete
archive contains 39 compiler inputs. The source manifest covers 54 paths. All
three changed compiler paths are included. Snapshot v2 captured the 39 inputs
before the successful bootstrap build. Snapshot v3 captured the same 39 inputs
before the self-hosted build. The M39 post-build correction and older evidence
remain preserved. Evidence uses `native-source-beta-*`. Baseline and matrix
interface-setup failures, the first bootstrap AST-pattern failure, the measured
unsigned-alias oracle correction and the stale API diagnostic assertion remain
saved.

The bootstrap build took 1m11s and peaked at 1818 MiB. The self-hosted build
took 17m18s and peaked at 5221 MiB. Both used a 16 GiB cap. Final validation
peaked at 1317 MiB under 4 GiB, with no swap. The full GHC suite and older
standalone corpora were not rerun. M41 checks variable-alias propagation in
native Core below. General transformations and the other
[roadmap gates](ROADMAP.md#remaining-work-by-roadmap-area) remain open.

See the [source contract](CORE.md#bounded-source-beta-applications),
[M40 plan](PLAN.md#m40-bounded-source-lambda-index-applications) and
[pinned provenance](REFERENCES.md#bounded-source-beta-applications).

## M41: checked native variable aliases

The restricted native simplifier now removes `let alias = actual in body`
when both Ids are eligible local Many indices in the same closed domain.
The input RHS must be a literal variable. The current substitution must also
map its target to an eligible variable. Existing traversal substitutes the
same actual Id through terms, types and coercions, with capture avoidance.
An outer `let actual = opaqueProducer x` stays lazy and shared. The rule
does not unfold its target or enable general inlining. Recursive bindings,
top-level bindings, excluded metadata and constructor targets keep their
existing behavior. Source alias defaulting remains unchanged.

The old-M40 baseline compiles the new fixture, then fails exactly at
`native literal variable alias disappears`. This establishes the missing
transformation. The earlier baseline attempt failed on import ambiguity
GHC-87543 for `setIdExported`. That setup failure and its correction remain
saved. It is not structural baseline evidence.

The unit fixture checks 17 valid cases: six transformation, identity and
capture cases, plus 11 fallback cases. Four malformed inputs fail Core Lint
before simplification. Five direct runtime kernels check finite replication,
demanded bottom, a finite cyclic prefix, ignored bottom and bottom-function
WHNF. The source-program corpus remains 243 package/322 total rejection
programs and 95 quiet positives. These counts exclude the new raw checks.
The one-line and 37-line goldens remain unchanged. No tracked test was added.

All nine focused phases pass, with seven launches and two bootstrap API
reuses. Each reuse checks its own fixture and complete compiler input hashes.
Both stages pass 106 raw and public observations, with the original 94 lines
unchanged, and all 562 packed-vector observations. Five registered tests pass
across 39 cases with 34 skips. There are no unexpected failures or framework
warnings. The 41-client source matrix, inherited 41- and 64-client matrices,
and pre-desugaring Tc metadata probe were not rerun. The full GHC suite and
older standalone corpora were not rerun.

Selected compiler: `25f7652dce2488c73115963a4396cf74a066c31c`. Fixture source:
`ddd23db09cd2270c95d36879694ac0e1378868a1`. The pre-build snapshot covers
39 compiler inputs and 54 source paths. Its exact compiler delta is
`compiler/GHC/Core/SimpleOpt.hs`. The bootstrap build took 25.76s and peaked
at 1826 MiB. The self-hosted build took 10m33s and peaked at 4302 MiB.
Both used a 16 GiB cap. The API scope peaked at 1219 MiB, and final validation
peaked at 1203 MiB, under 4 GiB. Swap is zero.

Evidence uses `native-variable-alias-*`. The prepared freeze preserves the
canonical M40 envelope, its 47 manifests and the older chain. It also covers
all 34 files in the external function-index research cache, including the
substitution-closure comparison. Existing cache manifests remain unchanged.
Opaque function-call indices are the next C1 design task. They remain
unimplemented. Admission needs function-head substitution and composition,
range free-variable tracking, and CorePrep/Tidy protections that currently
accept only scalar indices. Native SimpleOpt already keeps ordinary function
beta as a lazy let when `so_inline = const False`. This behavior does not
settle those remaining obligations. General C1/C2 gates remain open.

See the [M41 plan](PLAN.md#m41-checked-native-variable-aliases) and
[function-index comparison](REFERENCES.md#opaque-runtime-function-indices-c1-comparison-and-first-kernel).

## M42: scoped opaque function-call indices

Core now represents an opaque call as `TermAppTy f arg`.
The actual local head has an ordinary Many arrow between independently closed
scalar domains. The argument can be a scoped symbolic type of the input domain.
Forward `TermAppCo` congruence uses nominal argument evidence.
Equal call results never imply equal inputs. A constant-function control checks
this restriction. No unfolding or function evaluation rule is added.

Strict Core cases can now inspect an actual call. They bind and consume checked
constructor or DEFAULT evidence. CorePrep preserves proof endpoints when it
names constructor arguments. Literal function aliases use the checked alias
rule. Unsupported producers remain lazy. General lazy call naming stays gated.
Runtime checks cover Nat-to-Nat and Bool-to-Nat calls, ignored and demanded
bottom arguments, bottom heads, lazy predecessor fields and finite cyclic
prefixes. Type-only dependencies return 42 without forcing the head or index.

The existing fixtures add 19 quiet runtime invocations, including five direct
SimpleOpt-to-CorePrep checks. Unit controls cover raw head validity, exact
domains, roles, alpha equality, capture, composition and stored substitution
ranges. Call arguments normalize at Nominal, Representational and Phantom.
The source-program corpus remains 243 package and 322 total rejection programs,
with 95 quiet positives. These counts exclude the new Core controls.
No tracked test or golden line was added. The goldens remain one and 37 lines.

All nine final phases pass: seven launches and two bootstrap API reuses.
Each reuse verifies its own fixture and all 77 selected compiler input hashes.
Both stages preserve 106 raw/public and 562 packed-vector observations.
Five registered regressions pass across 39 cases with 34 skips.
There are no unexpected failures or framework warnings.
The source 41-client, known 41-client and prior 64-client matrices were not
rerun. The Tc metadata probe and full GHC suite were not rerun.

Selected compiler: `0447f8a7a199675423cd7f5fafa94567afa40114`.
Fixture source: `428aa3795341e8628e258976adc6df434c17bda4`.
Snapshot v7 covers 77 compiler inputs and 92 source paths. All 50 compiler
delta paths are included. All seven snapshots retain their own Git identities.
Bootstrap took 6m56s and peaked at 2899 MiB. Self-hosting took 20m58s and
peaked at 5957 MiB. Both used 16 GiB caps. API checks peaked at 1223 MiB.
Final checks peaked at 1214 MiB. Both validation scopes used 4 GiB. Swap is zero.
Setup failures remain recorded. A separate measured failure exposed acceptance
of constraint arrows. The corrected checker requires `FTF_T_T`.

Evidence uses `native-neutral-calls-*`. The prepared freeze retains the
canonical M41 envelope, its 21 manifest hashes and the older evidence chain.
Pinned Agda neutral applications and scoped substitution supply operational
prior art. D/DC and System DR supply separate equality precedents.
These references do not prove this implementation.

At M42, source call formation and interface encoding remain gated. Function-domain
retained binders, general evaluation and ordinary optimization remain open.
The next step is a strict source case that refines the whole call while
preserving unrelated inputs and source demand. See the
[Core contract](CORE.md#opaque-native-call-indices),
[M42 plan](PLAN.md#m42-scoped-opaque-function-call-indices) and
[M43 source plan](PLAN.md#m43-strict-opaque-call-refinement-from-source).

## M43: strict opaque-call refinement from source

The [source example](examples/qualified-pi/OpaqueCallClient.hs) accepts an
ordinary function and an unknown runtime index. In `case f n`, each branch can
construct a value of type `Vec Int (f n)`. Neither the input index nor a different
call is refined. The mixed-domain example uses `f :: Bool -> Nat` and a Boolean
read from an IORef. No singleton argument or function evaluation rule is added.

Type checking records the actual local head and argument. A typed
`NativeCaseAlias` emits one strict DEFAULT case around the checked call.
Its proof connects the computed result to the whole `f n` index. The existing
retained constructor matcher checks each branch. Captured consumers receive
explicit nominal transport. The call is checked once and executed once per
case. Wildcard and variable cases without dependent refinement stay lazy.

Twenty-six quiet source controls pass in the bootstrap API and the self-hosted
registered fixture. They cover captures, function-valued case results, partial
and cyclic fields, bottom order, and six `Strict` controls. The one-line and
37-line goldens are unchanged. Both compilers separately compile, link and run
the public example's nine observations. Fourteen negative clients reject with
located diagnostics: six use GHC-83865 and eight use GHC-83605. Coverage and
debug options produce six errors each, so the clients produce 24 error envelopes.
The arity client rejects after branch rewriting produces `'Z n`; it does not
isolate the direct call-arity diagnostic.

A separate fifteen-line program preserves the public nine-line prefix and
adds six controls. Both stages accept a local function with a complete
signature. They preserve opaque-head identity through `seq` and leave an unused
bottom index unevaluated. Ordinary `toList ys` obtains its known call index
through the existing immutable argument-instantiation route. General mutable
native-meta assignment is unchanged.

The `Strict` controls exposed a binder-identity error in `seq` desugaring.
Reusing an index as the case binder let capture-avoiding substitution change
a type argument while a captured vector kept the outer index. The corrected
path uses an unused case binder when the continuation contains native types.
It preserves the operand's demand and evaluation order.

Both compiler stages build. The nine final phases pass: two validated API
reuses and seven launches. Both stages preserve the old 106 raw/public and
562 packed-vector observations. Five registered regressions pass across
39 cases with 34 skips and no unexpected results or framework warnings.
The source 41-client, known 41-client and prior 64-client matrices, Tc metadata
probe and full GHC suite were not rerun.

Selected compiler and source: `edbe394d03628de2900fff828b4137d72e5cc94c`.
Snapshot v5 covers 82 compiler inputs and 98 source paths. All five snapshots
retain their own Git identities. The final bootstrap build took 21.67s and
peaked at 1825 MiB. Self-hosting took 20m11s and peaked at 5791 MiB.
Both builds used 16 GiB caps. API checks peaked at 1243 MiB and final checks
at 1185 MiB under 4 GiB caps. Swap is zero.

Evidence uses `native-source-neutral-calls-*`. It preserves the frozen M42
envelope and its 45 manifest hashes. Earlier import, dispatch, `seq`, finder
and supplemental source errors retain their actual outcomes. Pinned Agda
with-abstraction and application checking supply operational prior art.
They do not prove preservation for this implementation.

At M43, source case arguments were bare eligible variables, modulo parentheses.
Function-valued retained binders, lazy `keep (f n)`, new call-bearing interface
forms, general evaluation and ordinary optimization remain gated. C1 and C2
remain open. M44 below exposes the existing Core constructor-argument bridge.

## M44: checked constructor arguments in source calls

The [constructor-call example](examples/qualified-pi/ConstructorCallClient.hs)
accepts `case f Z`, `case f (S n)` and nested constructor arguments.
Branches can construct or consume `Vec Int (f (S n))` at that exact call.
The input n and other calls remain independent. Constant functions need no
injectivity assumption.

The dispatcher first checks the original expression and its usage. It builds
a temporary constructor spine from the actual checked Ids and DataCons.
It uses `dataConWrapId`, as desugaring does. Existing native domain checks
reject constructors with wrappers. Parentheses, WpHole and checked source
expansions are transparent. Argument expression signatures, casts, nonidentity
wrappers, type/evidence applications, lambdas and residual opaque producers
get no new admission.
The original checked expression stays in `NativeCaseAlias` and runs once.

Annotation matching compares actual variable and promoted constructor Names,
exact arity and recursive fields. It keeps `f n`, `f (S n)`, `f (S m)` and
shadowed heads distinct. The transport scope includes every free dependency
of the call. Existing CorePrep recursively names constructor arguments and
composes their proofs. This increment changes no Core node, equality rule,
backend lowering or interface format. Fields keep their tested laziness.

Both compiler stages pass the new 17-line program and 17 negative clients.
Eleven clients report GHC-83865 and six report GHC-83605. Their 37 located
envelopes contain 13 and 24 diagnostics, respectively. A separate semantic
review checks the four added negative controls. The bootstrap API and the
self-hosted registered fixture also pass 17 quiet source observations.
Keep these separate from M43's 26 source controls and M42's 19 Core invocations.
The one-line and 37-line goldens remain unchanged. No tracked test is added.

Four legacy checks preserve the nine-line M43 public program and its separate
15-line supplement in both stages. All nine final phases pass, with two API
reuses and seven launches. Both stages preserve 106 raw/public and 562 packed
observations. Five registered tests pass across 39 cases with 34 skips and no
unexpected results or framework warnings. The source 41-client, known
41-client, prior 64-client matrices, Tc metadata probe and full GHC suite
were not rerun.

Selected compiler: `f76bba39b8bc61ea3e7ee48338086b1949fdce60`.
Selected source: `30fb3cca1eb2027b4ff490892ce69ec17ac57190`.
Snapshot v2 records 82 compiler inputs and 99 source paths. The compiler delta
changes only `Tc/Gen/Expr.hs` and `Tc/Gen/Match.hs`. Bootstrap took 33.59s and
peaked at 1827 MiB. Self-hosting took 7m25s and peaked at 4335 MiB.
Both builds used 16 GiB caps with zero swap. API checks peaked at 1239 MiB,
legacy checks at 221 MiB, and final checks at 1201 MiB under 4 GiB caps.
The final checks recorded zero swap.

Evidence uses `native-source-call-constructors-*`. The old-compiler baseline,
API1 source-preparation failure using imported `bottomNat`, and the earlier
13-client CLI attempt remain preserved. Freeze and independent audit must pass before publication. Pinned Agda argument checking
and context abstraction supply prior art, not a Haskell preservation theorem.
See the [Core contract](CORE.md#checked-constructor-arguments-in-source-calls),
[plan](PLAN.md#m44-checked-constructor-arguments-in-source-calls) and
[primary readings](REFERENCES.md#checked-constructor-call-arguments-prior-art).

At M44, signed immediate lambda arguments remained separate work. M45 below
implements that bounded extension. General function-domain binders, lazy
retained calls, optimizer support and call-index wire forms remain gated.

## M45: signed beta arguments in source calls

The [signed-call example](examples/qualified-pi/BetaCallClient.hs) accepts an
immediate argument such as `case f (((\x -> S x) :: Nat -> Nat) n) of ...`.
Branches can use the canonical annotation `Vec Int (f (S n))`. Signed identity,
constant and nested scalar bodies preserve actual Id scope and field demand.
The outer opaque function still has independently closed scalar domains A/B.
The signed lambda itself must be an endofunction A -> A.

The classifier requires the surviving checked `ExprWithTySig`. Its signature
has one plain unrestricted arrow and the same bare, optionally parenthesized
TyCon Name on both sides. It excludes aliases, applied domains, forall,
contexts and wildcards. The actual LamSingle has one bare VarPat, one
unguarded RHS and no local bindings. The during-TC zonker reads existing
assignments. Actual Id and scaled MatchGroup multiplicities must be Many;
binder, argument and result types must agree. Flexi is not filled or defaulted.
A scoped NameEnv preserves the actual zonked binder's Name and Unique.

The temporary Core lambda is classification data. The existing scalar beta
bridge validates its atomic operand and constructor body. A local variable or
nullary constructor can be an atomic operand; a constructor with fields cannot.
The original checked scrutinee remains in the strict case carrier and executes
once. Source checking and usage collection do not run again. Under Strict,
this new classification returns unsupported; ordinary nondependent cases retain
their existing checking and demand. No general evaluator or Core rule is added.

Both stages pass 15 public runtime observations and reject eight CLI clients.
Five clients emit both GHC-83865 and GHC-83605. One client emits only GHC-83605
from the existing local-signature scope guard. Two emit only GHC-83865 after
valid refinement, testing wrong and shadowed calls under active branch evidence.
These eight clients produce 15 located diagnostic envelopes per stage: nine
GHC-83865 and six GHC-83605. They are not two disjoint client groups.

The bootstrap API and registered fixture pass 15 quiet source observations,
separate from the inherited M44 source 17, M43 source 26 and M42 Core 19.
The one-line and 37-line goldens stay unchanged. Four supplementary runtime
observations per stage check ordinary Strict behavior and same-spelling binder
scope, two of each. Each of two designated trace markers occurs once per stage.
Those markers give limited lookup evidence, not a universal elaboration count.
Six legacy checks preserve public programs of 9, 15 and 17 observations in
both stages. The final nine phases pass with two API reuses and seven launches.
Both stages preserve raw/public 106 and packed 562 observations. Registered
results are five passes across 39 cases, with 34 skips and no unexpected results
or framework warnings. No new registered test or golden file is added.

Selected compiler: `44e91a7618fe55c3713c85641b4a2d88d4536f13`.
Selected source: `0c1bb75a6ecaef5ecc62cf84450968ad21de67ca`.
Snapshot1 records 82 compiler inputs and 100 source paths. The compiler changes
only `Tc/Gen/Expr.hs`. Bootstrap2 takes 9.67s and peaks at 1594 MiB; self-hosting
takes 7m24s and peaks at 4229 MiB. Builds use 16 GiB caps. API1 peaks at
1247 MiB, legacy checks at 258 MiB and final checks at 1203 MiB under 4 GiB
caps. Final swap is zero. Bootstrap1's wrong-target setup attempt remains
preserved. It is not a compiler failure. The old M44 compiler's source baseline
rejects with 20 errors, ten of each code above.

Evidence uses `native-source-call-beta-*` and preserves M44. Freeze and
independent audit must pass before publication. No full GHC suite, historical
source41/known41/prior64 matrix or Tc metadata probe is claimed as rerun.
See the [Core contract](CORE.md#signed-beta-arguments-in-source-calls),
[plan](PLAN.md#m45-signed-beta-arguments-in-source-calls) and
[pinned checking precedent](REFERENCES.md#signed-source-call-beta-prior-art).
C1 and C2 remain open. M45 left function-domain retained binders for
[M46](PLAN.md#m46-head-only-function-valued-retained-binders-compiler-validation-complete) below.
Call-bearing exports stay gated until a separate interface increment.

## M46: head-only function-valued retained binders

The [function-binder client](examples/qualified-pi/FunctionBinderClient.hs)
uses complete local signatures such as:

```haskell
idCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat)
       -> Vec Int (f n) -> Vec Int (f n)
idCall f n xs = xs
```

The retained worker requires a complete local value signature and one equation
with bare variables in retained columns. Each actual function operand is an
already-bound unrestricted local function variable of the exact checked type.
The operand does not need its own written local signature. Its enclosing
signature or existing assigned type can supply that type. A signed local alias
can hold a lambda, an ordinary function or a lazy producer. Passing a lambda,
global name or applied producer directly gets no new rule. The function has a
closed ordinary Many arrow A -> B. A and B are independently closed scalar
types. The Bool -> Nat client checks different input and result domains.

In types and coercions, function occurrences use TermAppTy/TermAppCo head slots.
Scalar TermVarTy, index reification and constructor-field eligibility do not
change. Exact local Var substitution masks bound heads and avoids capture.
CorePrep substitutes directly without a strict scalar alias case. DepFunCo
keeps its domain fixed; body congruence adds no function equality or inverse
call equality. Call-bearing exports and wire forms remain gated.

The working client defines caller = idCall f n before case f n. Existing
whole-call refinement can transport that instantiated caller. The separate
direct in-branch probes2/3 reject in both stages: two GHC-83865 payload mismatches
at lines 33/37 compare Vec Int Z or Vec Int (S k) with Vec Int (f n). These probes
have no runtime result. The M47 section below records later direct checking.

Selected snapshot6 has compiler/source
`8fd22b97c74571c7d5508607f4f16e6611a84435`, with 83 compiler inputs and 102
source paths. Reduction.hs is the additional archived compiler path. Snapshot6
restores the original unsupported scalar-domain wording. It uses the new
function wording only when isPiTy holds. No golden changed; two quiet scalar
assertion fragments were restored.

| Evidence | Measured result |
| --- | --- |
| Old M45 baseline2 | Same public source bytes reject with seven GHC-83605 diagnostics |
| Snapshot5 bootstrap5/API2 | Build passes in 35.32s; Unit/CoreRun pass in 5.72s/17.14s with unchanged 1/37 goldens |
| Snapshot5 source clients, both stages | Public14 passes; ten negative clients reject, with nine GHC-83605 and one GHC-83865 |
| Snapshot5 constructor supplement, both stages | All eleven controls pass |
| Snapshot5 legacy8 | Passes |
| Snapshot5 final1 | First eight phases pass, including raw/public106 and packed562 in both stages; registered tests give four passes and one scalar diagnostic-text mismatch |
| Selected bootstrap6 | Passes in 2m48s |
| Selected API3 | Unit/CoreRun pass in 5.698s/17.910s; goldens remain 1/37 |
| Selected self-host2 | Passes in 11m03s, peak 4966MiB under 16GiB, swap0 |
| Selected CLI bootstrap4/self-host2 | Each passes public14 and rejects ten clients; ten error envelopes each, nine GHC-83605 and one GHC-83865 |
| Constructor supplements3 | All eleven controls pass in each stage |
| Direct probes3 | Each stage rejects two payloads at lines 33/37; review-v3 confirms four GHC-83865 errors total, no panic or runtime |
| Selected legacy2 | All eight phases pass |
| Selected final2 | All nine phases pass: two API3 reuses and seven launches; both stages pass raw/public106 and packed562 |
| Selected registered run | Five expected passes, 39 cases and 34 skips; zero unexpected/framework failures or warnings; 23.631763s |
| Final2 resource bound | Peak 1213MiB under 4GiB, swap0 |
| Independent audit/publication | Outcomes are recorded separately in the canonical M46 audit/publication files |

CoreRun adds ten quiet positive observations and four expected errors. Public14
checks IORef-fed finite inputs, ignored bottom functions/indices/payloads,
original demanded exceptions, lazy producer aliases, partial/cyclic prefixes
and a mixed-domain head. The unit controls cover alpha/FVs, head-only
substitution, fixed-domain coercions, scoped matching and ordinary
multiplicity-family normalization at Nominal, Representational and Phantom.
These are bounded observations, not a full laziness or preservation theorem.

The Phantom control found an ordinary FunCo role defect. The fix weakens only
its stored multiplicity proof to funRole's required role. Preserve bootstrap1/2
import failures, the later passing attempts, API1's role failure and stale
assertions, and the historical CLI1 catalog. Keep snapshot5's registered
wording mismatch. Snapshot6 passes all selected compiler/runtime validation,
including legacy2 and final2. Evidence uses native-function-binders-*; the M45
prefix stays frozen. Source41, known41, prior64, Tc metadata and the full GHC
suite were not rerun. The later canonical audit is recorded in
`dependent-haskell-M46-final-audit-v1.json`; publication has its separate
`dependent-haskell-M46-publication-v1.json` record. This section reports compiler
validation, not predicted outcomes or counters for those records.
See the [Core contract](CORE.md#function-valued-retained-binders),
[plan](PLAN.md#m46-head-only-function-valued-retained-binders-compiler-validation-complete) and
[primary comparison](REFERENCES.md#head-only-function-valued-retained-binders-prior-art).

## M47: remembered equality for direct call application

M47 remembers the relation between a checked call `f n` and its fresh motive
index. Focused compiler/runtime validation is complete. A source branch can then check `idCall f n payload`
directly. Existing result, capture, Given or annotation information still
activates whole-call refinement. M47 leaves annotation-free activation to the
M48 step below.

The motive takes `q_motive :: i ~# f n` as its first ordinary evidence argument
after `i`. Existing constructor transport derives `q_branch :: K ~# f n`.
Ordinary argument/result checking emits a Wanted equality. The existing solver
uses the exact or symmetric Given. Its normal dependency tracking retains the
needed evidence. The Given covers the complete row, including `where` bindings.
Ordinary application and solver paths are unchanged. App, environment,
quotation and splice paths retain their M46 behavior.

Selected snapshot5 has compiler `3504f1be5342c9f3fbe12e2bf8dbe3bbc008e239`
and source `f7f0e92e4a775d407189faa79ae0cc608eee7749`, with 83 compiler inputs
and 102 source paths. At that selected source snapshot, the net change from
M46 has three tracked paths. The compiler change adds 22 lines in Match;
it adds no solver or backend rule.

| Evidence | Measured result |
| --- | --- |
| Given-only bootstrap3 | Passes in 2m01s |
| Selected self-host2 | Passes in 10m20s, peak 5196MiB under 16GiB, swap0 |
| Selected API4 | Unit/CoreRun pass in 5.364124714s/16.739944110s; goldens remain exactly 1/37 |
| API4 post-build comparison | Both 238-entry input sets remain unchanged; no API5 rerun |
| Selected quiet controls | Eleven values, three expected runtime markers and three located GHC-05617 source rejections pass; older groups pass too |
| Both-stage selected CLI | Baseline3, public17 and annotated-where3 pass in each stage with Core/STG Lint |
| Raw scope run2 | All 36 pure assertions pass; these are utility facts, not a new admission policy |
| Legacy checks | All eight phases pass |
| Constructor supplements | All eleven positive controls pass in each stage |
| Selected final9 | All nine phases pass: two API4 reuses and seven launched phases |
| Inherited final controls | Raw/public106 and packed562 pass in both stages |
| Registered run | 39 cases, 34 skips and five expected passes; zero unexpected/framework failures or warnings |

Public17 preserves the old fourteen-line prefix and adds three direct inputs.
The raw control is pure host API evidence, not TcM or a native-meta/source-proof
test. The selected fixture pins all three measured GHC-05617 rejections.
Earlier variants and expected-diagnostic assertion failures remain in
`native-call-application-refinement-*` artifacts; they do not broaden acceptance.

Compiler/runtime validation is complete for this bounded increment. The full
GHC suite was not run. Artifact audit and publication remain pending.
M47 adds no Core node, codec, inverse call rule, function equality or evaluator. Existing
native-meta, Quick Look, scope and usage gates remain. General optimization
and exported call codecs stay gated. C1 and C2 remain open.
See the [contract](CORE.md#remembered-equality-for-direct-call-application),
[plan](PLAN.md#m47-remembered-equality-for-direct-call-application) and
[Agda comparison](REFERENCES.md#remembering-an-abstracted-call-prior-art).

## M48: known-worker case activation

M48 lets selected source cases activate retained refinement without a payload
annotation. Focused compiler/runtime validation is complete. Existing result, capture, Given and annotation
triggers keep priority. Only when all are absent does Match inspect an already
checked lexical worker and its leading retained arguments.

The discovery requires complete, unique, unguarded constructor rows with bare
field variables and no quotations. It reads the worker's leading `DepFunTy`
telescope. Each supplied retained argument must be an eligible bare local Var
at the exact domain. `applyTypeToArgs` substitutes those actual Ids. The whole
residual must be tau and contain the exact free call being matched.
These are trigger-discovery rules, not a general App restriction.

Discovery does not infer a type, allocate a meta, collect usage or execute a
splice. Unsupported metadata silently leaves the ordinary case path in place.
Normal checking still checks the actual application. M47's ordinary motive
Given supplies the branch relation, including `where` bindings. Complete native
worker signatures remain required. No App, Core, evidence, solver or backend
rule changes.

Snapshot2 selects compiler `439fbf6396` and source `8480d83c98`, with 83
compiler inputs and 102 source paths. The public source removes payload
annotations. Fresh/freshwhere baselines reject with GHC-83865/GHC-83605
and have no runtime result.

| Evidence | Measured result |
| --- | --- |
| Bootstrap1 | Passes in 25.56s |
| Self-host1 | Passes in 6m14s, peak 5412MiB under 16GiB, swap0 |
| Selected API1 | Unit/CoreRun pass in 5.616214630s/16.836089039s, with exact 1/37 goldens |
| API1 post-build comparison | Own input maps are unchanged; no repeat needed |
| Fresh and freshwhere CLI | Each emits 0/2/3 in both stages without payload annotations |
| Ordinary/duplicate/quote CLI | Both stages keep 42/7/42, 42 plus GHC-53633, and 42/7 respectively |
| Wrong-head/index clients | Each rejects with a located GHC-83865 in both stages; compile-only, no runtime |
| Simplified public example | All seventeen unchanged expected outputs pass in both stages |
| Legacy and constructor controls | Legacy8 passes; all eleven positive constructor controls pass in each stage |
| Selected final9 | All nine phases pass: two own API1 reuses and seven launched phases |
| Inherited final controls | Raw/public106 and packed562 pass in both stages |
| Registered run | 39 cases, 34 skips and five expected passes; zero unexpected/framework failures or warnings |

The ten new quiet values cover direct and where calls at inputs 0/2/3.
They also check 42 from an ignored undefined payload, 42/7 from guards and 42
from the tested incomplete case. The duplicate control stays in its separate
CLI to avoid an expected warning in the quiet fixture. The two independent negative clients add no fixture assertions.
Raw scope36 remains inherited M47 evidence; it was not rerun for M48.

All selected M48 compiler/runtime validation, freeze and independent audit pass.
Publication is complete at `3e6f9aa35fba64e9276433e490351d426bac0973`.
The canonical audit SHA256 is
`953f58351b6ed84eaaccf0a93e957fe7fc7e291ac7bfafef0161cb3516d16aa8`.
It records 65 manifests and 35457 integrity checks. These focused checks do not
cover the full GHC suite or complete dependent Haskell.
C1 and C2 remain open. See the
[contract](CORE.md#known-worker-case-activation),
[plan](PLAN.md#m48-known-worker-case-activation) and
[prior-art comparison](REFERENCES.md#known-worker-case-activation-prior-art).


## M49: constructed-worker case activation

M49 extends the M48 metadata query to scalar constructor trees. A local worker
can expose the exact call `f (S n)` or `f (S (S n))` without a branch payload
annotation. This local complete-signature group uses the existing bridge:

```haskell
observeConstructedCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> [Int]
observeConstructedCall f n = case f (S n) of
    Z -> toList (idCall f (S n) VNil)
    S k -> toList (idCall f (S n) (VCons 42 (replicateNative @Int k 42)))
```

The query accepts resolved `HsVar`, `HsApp` and `HsPar` constructor trees.
It requires a real monomorphic constructor at the expected scalar domain.
It checks each child against its declared field domain and validates the
reconstructed metadata with `exprToNativeIndex`. `applyTypeToArgs` opens the
known leading `DepFunTy` binders. Function operands remain eligible bare local
Vars. Complete rows, quotation boundaries and whole-tau residual checks keep
their M48 behavior. The actual source application is still checked once.

The change affects Match only. App, Core, evidence, solver, codecs and backend
rules do not change. M47's ordinary motive equality and row Given supply the
same branch evidence. A constant function can ignore a bottom predecessor;
constructor-tree discovery neither executes the call nor makes it injective.

Snapshot1 selects compiler `a90757e6d9adfd4e7500672a715bf939c65371bb`
and source `4ef6ba241ed13dc195b60d1b5fcbfa0bcbd5681c`. It records 83 compiler
inputs and 102 source paths.

| Evidence | Measured result |
| --- | --- |
| Bootstrap1 | Passes in 25.35s |
| Self-host1 | Passes in 5m48s, peak 5816MiB under 16GiB, swap0 |
| Selected API1 | Unit/CoreRun pass in 5.679607153s/17.552677284s; exact 1/37 goldens and eight new quiet values |
| API1 post-build comparison | Both 304-entry input maps remain unchanged; each contains zero Stage1 paths |
| Constructed source CLI | Both stages emit 1/3/4/0 |
| Nested and demand CLI | Both stages emit 2/4/5/0 and 0/0 respectively |
| Ordinary source CLI | Both stages preserve 42/7/42 |
| Wrong constructed/bare keys | Both modules reject with located GHC-05617 in both stages; compile-only, no runtime |
| Public example | Both stages emit all 21 expected lines: the unchanged old17 prefix and independent 1/3/4/0 appendix |
| Legacy8 and constructor11 | Legacy8 passes; all eleven positive constructor controls pass in each stage |
| Final9 and registered run | Two own API1 reuses and seven launches pass; both-stage raw/public106 and packed562 pass; registered results are 39 cases, 34 skips and five expected passes, with zero unexpected/framework failures or warnings |

The wrong-constructed payload is at line25 column83. It requires `Z ~ f (S m)`
under the unrelated Given `Z ~ f (S n)`. The wrong-bare payload is at line25
column79 and requires `Z ~ f n` under `Z ~ f (S n)`. Relevant bindings can still
print a generated `co :: call_motive ~ f (S n)`. The measured source locations
and contexts are retained; M49 does not implement the proposed display fix.

The eight quiet values and standalone CLI programs have separate scopes.
The inherited 243 package/322 total rejection programs and 95 quiet source
controls keep their existing denominator. M48 duplicate and quotation checks,
and the transitive M47 utility36 evidence, are inherited rather than rerun.
No source41, known41, prior64, TC-metadata or full-suite rerun is claimed.

M49 is published at `7029ed2a7d78e0b6ddc44ac3fa7c6b4e0b9cb7d5`. Its canonical
audit SHA256 is `ba310681ec8effaf4c6a84bc4e4dfe26ca902ee0545a89cd0aefe5f069cf25a7`.
It records 55 manifests and 35230 integrity checks. Multi-constructor package
patterns were its next bounded source candidate and are implemented below. R22,
C1 and C2 remain open. See [the contract](CORE.md#constructed-worker-case-activation),
[plan](PLAN.md#m49-constructed-worker-case-activation) and
[prior art](REFERENCES.md#constructed-worker-case-activation-prior-art).


## M50: multi-constructor retained package patterns

A datatype can now combine a retained-field constructor with other constructors.
The public `NativeMultiPackage` producer and `MultiPackageClient` consumer use
ordinary source declarations and actual interfaces:

```haskell
data Availability a where
    Unavailable :: Availability a
    Available :: foreach (n :: Nat) -> Vec a n -> Availability a

toPacked :: Availability a -> Maybe (PackedVec a)
toPacked Unavailable = Nothing
toPacked (Available n xs) = Just (PackVec n xs)
```

Each constructor keeps its own field telescope. Each equation keeps its actual
retained field Ids. Pattern checking removes the parent-constructor count check
and keeps its existing constructor shape, scope, result-wrapper and Many checks.
Desugaring detects retained fields in any constructor group. It builds a case
for each row and keeps the DEFAULT failure continuation. A failed nested field
pattern or guard can then try the next row outside the previous row's scope.
Distinct outer constructors do not overlap. Source order within a constructor
group is preserved. The fields retain their tested laziness.

The compiler delta is three files: `Tc.Gen.Pat`, `HsToCore.Match.Constructor`
and the package-pattern diagnostic in `Tc.Errors.Ppr`. It adds no Core node,
solver rule, field representation, interface schema or backend rule. Native
constructors retain the existing existential, equality-specification, context,
prefix-pattern, multiplicity, wrapper, promotion, lazy-pattern and let-pattern
restrictions. An ordinary GADT sibling can use its ordinary equality evidence.
This is not a first-class Sigma or R22 implementation.

Snapshot3 records compiler `7e47932abc629face112eecf0d083fb2f2efaca3`, source
`905554e64226d1d26041dc7ba417ca006ae016ce`, 83 compiler inputs and 104 source
paths. The two new public modules account for the source-path increase.
The selected archive was captured after bootstrap1 and before self-hosting;
its compiler bytes match the selected Git revision. Metadata1's inherited
pre-build description and metadata2's earlier fixture remain historical.

| Evidence | Measured result |
| --- | --- |
| Builds | Bootstrap1 passes in 35.81s; self-host1 passes in 6m16s, peak 5134MiB under 16GiB, swap0 |
| Selected API2 | Unit/CoreRun pass in 5.716686885s/17.922187794s; exact 1/37 goldens and eight new quiet observations |
| API2 post-build comparison | Both actual 296-entry input maps are unchanged and contain zero Stage1 paths |
| Public source | Both stages pass the exact seventeen-line producer/consumer oracle, with IORef lengths0/2/3, both constructor orders, row failure and demand markers |
| Fixed-Nat family control | Both stages pass three observations; this source form already passes the M49 baseline |
| Mixed sibling source | Both stages pass seventeen observations, including ordinary fields and an `IntOnly` GADT sibling |
| Distinct retained telescopes | Both stages pass seventeen observations; `TwoVec` stores two independent indices beside `OneVec` |
| Lazy/linear clients | Both stages reject the compile-only clients with located GHC-83605/GHC-18872; no main-is, link or runtime phase |
| Legacy8 | All eight selected phases pass |
| Constructor11 | Selected attempt2 passes eleven observations in each stage |
| Final9 | Two own API2 reuses and seven launches pass; raw/public106 and packed562 match in both stages |
| Registered tests | Five total tests,39 cases,34 skips and five expected passes; zero unexpected/framework failures or warnings |

The M49 compiler first compiles PackedVecLibrary and then rejects the original
multi-constructor producer with located GHC-83605. No consumer or runtime phase
runs in that baseline. Its Sigma control already passes three lines. The first
M50 ignored-source run passes17 under metadata2. It stays historical; selected
public17 covers the same operations without a redundant ignored-source run in
both stages.

API1 passed the unit fixture but failed the new group because extracted entry
Core did not link its generated DataCon workers. The fixture now uses the
existing `runModuleCase` object-module runner. API2 checks six finite package
observations and both absent alternatives. These are genuine object-module
observations, not a claim that the new declarations pass entry-only bytecode
extraction. Existing groups and golden output remain unchanged.

The new eight observations remain separate from the inherited 243 package/
322 total rejection programs and95 quiet source controls. CLI programs overlap
and are not added into one count. Utility36, duplicate and quotation evidence
is inherited; source41, known41, prior64, TC metadata, the old177 standalone
controls and the full GHC suite were not rerun. M50 is published at
`d918069c802b84490eec9dc175b77bfc25545c85`. Its canonical audit SHA256 is
`85d0f9827b9fd444049a40ff11cc24bfe3b9a1eb62988c50df258fe9470fbb15`.
The independent audit passes 35874 integrity checks over 69 manifests.
R22, C1 and C2 remain open.

Constructor attempt1 failed before any target because its runner looked up the
older M48 output manifest. The corrected runner uses the direct M49 parent.
Selected attempt2 passes eleven observations in both stages. Final9 passes
with two own API2 reuses and seven launches. Both stages preserve the exact
raw/public106 and packed562 oracles. The registered run retains five tests,
39 cases,34 skips and five expected passes, with no unexpected results or
framework failures or warnings. No new test file or golden line is added.

The maintained [Showcase.hs](Showcase.hs) demonstrates the completed source
features. It is a separate documentation change after the M50 source snapshot.
See [the contract](CORE.md#multi-constructor-retained-package-patterns),
[plan](PLAN.md#m50-multi-constructor-retained-package-patterns) and
[prior art](REFERENCES.md#multi-constructor-retained-packages-prior-art).


## M51: function-valued retained package fields

The checkpoint is independently audited and published at
[b92d3f1595](https://github.com/tritlo/ghc/commit/b92d3f1595fa9a2341f683dfd4f0c7ed66b906a6).
This later status update changes documentation only. The maintained
[Showcase.hs](Showcase.hs) includes the `functionFields` group.

[FunctionPackageLibrary.hs](examples/qualified-pi/FunctionPackageLibrary.hs)
stores the function and input used by a later field type:

```haskell
data FunctionPack a where
    FunctionPack :: foreach (f :: Nat -> Nat)
                  -> foreach (n :: Nat)
                  -> Vec a (f n) -> FunctionPack a
```

The parameter `a` remains an ordinary erased parameter. The constructor stores
three lazy runtime fields. Matching exposes fresh actual `f` and `n` Ids in
the payload type. Repacking preserves those identities. Type checking does
not execute `f` or `n`. A runtime observer can execute them.

The source checker, DataCon validator and deferred interface importer admit
the existing closed function-domain policy for a named field. The interface
type codec stores a lexical call head and its argument with tag12. A separate
`IfaceFreeTermAppTy` keeps diagnostic free heads out of the wire format. The
fork is now `10.1.20261115.1`, header `1001202611151`. Both new readers accept
their actual interfaces. Old/new reader combinations reject the version
mismatch. The preserved old reader still reads its actual old interface.

Both compiler stages pass the 24-line public producer/consumer program and a
14-line mixed-constructor probe. The latter combines `Nat -> Nat` and
`Bool -> Nat` fields with a plain alternative. These programs check IORef
inputs, generic Char payloads, repacking, guard fallback, ignored bottoms and
distinct demanded errors. Both compilers also pass the maintained showcase
with all 51 lines and twelve actual source modules. Core and STG Lint run.

Each stage rejects four invalid client bodies: deriving input equality from
equal call results (GHC-05617), changing the head or input (GHC-83865), and
returning the payload at an unrelated index (GHC-25897). Two separate controls
retain GHC-83605 for a wildcard function field and a wrong function domain.
Both dependencies compile before these compile-only controls run. No client
link or runtime failure counts as a type rejection.

Existing API goldens remain one and37 lines. The CoreRun fixture also executes
24 quiet object-module observations. Legacy8, constructor11 per stage and all
nine final phases pass. Two final phases reuse exact compatible API results;
seven launch. Raw106, public106 and packed562 pass in each stage. Registered
checks report39 cases,34 skips and5 passes with no unexpected failures. These
counts overlap; they are not an aggregate observation count. Bootstrap takes
5m17s. Self-hosting takes20m12s and peaks at7708MiB under16GiB with no swap.

Only a bare variable can bind a stored function field. Functions are not
scalar indices. Generic exported function-valued Pi signatures, native
coercion encoding, native unfoldings and general optimization remain gated.
The work does not establish totality or close R22, C1 or C2. See the
[contract](CORE.md#function-valued-retained-package-fields),
[plan](PLAN.md#m51-function-valued-retained-package-fields), and
[prior art](REFERENCES.md#function-valued-retained-package-fields-prior-art).


The canonical local record is
`_build/verification/dependent-haskell-M51-final-audit-v1.json`, SHA256
`4cc77414d28e947f26301bc59168d9306671f04144a09c9fadc1535d033a7fa2`.
It pins72 manifests. The frozen cohort has110 source paths, nine documents,
45 harness files and1509 outputs. Independent auditor-v3 passes55914 checks
and146121 overlapping manifest-entry checks. These are not distinct artifact
or observation totals. The original auditor-v2 report remains failed because
its historical baseline schema omitted the successful third reader step. An
external repair record, exact diff and manifest preserve that history without
changing the frozen checkpoint or compiler/source inputs.


## M52: exported function-valued retained value signatures

[FunctionSignatureLibrary.hs](examples/qualified-pi/FunctionSignatureLibrary.hs)
exports the worker that was previously limited to complete local signatures:

```haskell
idCall :: forall a. foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n) -> Vec a (f n)
idCall f n xs = xs
```

[FunctionSignatureClient.hs](examples/qualified-pi/FunctionSignatureClient.hs)
opens `FunctionPack`, then calls `idCall @a f n xs` through the actual imported
Id type. The function actual is still a local variable with the exact closed
Many domain. Exporting the worker adds no function equality, injectivity or
function evaluation rule.

Only the complete ordinary named-value signature path grants this permission.
Class, instance, foreign, expression, partial, visible-type-argument and
hs-boot contexts retain their restrictions. `FunSigCtxt` alone is insufficient:
instance signatures and specialization pragmas also use that context. The
generic signature entry point keeps the function-domain permission disabled.
Some unsupported forms reject earlier during renaming. The scalar retained
signature paths retain their existing policy.

The interface reuses `IfaceDepFunTy` and `IfaceTermAppTy`. It reconstructs each
actual binder before its body. Bare `IfaceTermVar` remains scalar-only. The
version stays10.1.20261115.1, with header1001202611151. Raw interface display
and typed import are separate checks. Both reader matrices pass raw
old/new combinations at the same header, while the old executable rejects the
used new retained Id type with `Invalid retained binder in interface:`.
Typed old-reader checks use compatible current standard libraries. They do
not claim preservation of the full old toolchain. The first typed attempt
failed package lookup before reaching the native check; that attempt remains
in the evidence.

Both stages pass19 finite public observations through separate object modules.
Independent IORef inputs0/2/3 select identity, successor and constant heads.
The checks cover generic Char payloads, the original vector constructor's
`StableName`, partial application, ignored and demanded payloads, and partial
or cyclic inputs. The partial-WHNF controls already supply the function actual;
they do not pass a whole function-retained Pi value to generic `evaluate`.
The API runs the same19 observations quietly, with unchanged1/37 goldens.
The maintained showcase passes53 lines from thirteen actual source modules.
These overlapping programs are not an aggregate observation count.

Wrong-head and wrong-input clients reject at `xs` with GHC-83865 in both
stages. The bootstrap context run has an independent review of eight actual
rejections and two scalar controls. Its two original message mismatches remain
recorded; they are not compiler failures. A fresh self-hosted context run
passes. A separate old-form SPECIALISE control rejects in both stages; it adds
a ninth negative context and does not change the original eight-case review.
Omitted-VTA and local-callee-alias probes compile in both stages; no runtime
result is claimed for those optional probes.

Synthetic malformed ordinary-domain interface knots raise `<<loop>>` for
three function and three scalar variants. The scalar comparison also loops
through the unchanged scalar check. No old executable ran these synthetic
knots. These outcomes expose an unsupported malformed-interface validation
gap and are not successful defensive rejections. These raw shapes are outside
the supported source contract. M52 does not repair malformed recursive
interface types.

The first registered run had four passes and one stderr mismatch from the
new consumer's partial-list warning. The client and embedded copy now use an
explicit nonempty/empty case. The 19 observed values and 1/37 goldens did not
change. The build-reuse record confirms that both 84-file compiler archives
are identical; the source observation fix requires no compiler rebuild.

The final snapshot2 cohort passes both stages. It preserves the 19 public
results, the 53 Showcase lines and the 1/37 API goldens. Registered validation
has 39 cases, 34 skips and five expected passes, with no unexpected results
or framework failures or warnings. The independent artifact audit passes
6,247 checks over 1,772 unique pinned paths. The first audit attempt required
the same keys in both hash maps. The corrected auditor checks that each
original input hash stays unchanged. It also checks that each added file is
in the phase directory and matches the sealed file hash.
Both attempts remain recorded.

The selected compiler is `3d4994c68ba42b8fb1bd43d3f780780384edd0da`. The measured
source head is `d8c605b6c63dd8f68920bcd83103fcd2ca389009`. Its snapshot records 84
compiler inputs and 113 source paths. Final document bytes have their own
revision. Native coercion encoding, native unfoldings and general optimization
remain gated. The evidence namespace is frozen. The audited revision
`00b128403ce1e31b15bd960b89690f1d6e99fc1f` is published on the fork.
The canonical record is `dependent-haskell-M52-final-audit-v1.json`, SHA256
`6af17c2536a0aba1f41d3ec413a246c4120157ddef94b2d44d0fcda92b5614da`.
C1, C2 and R22 remain open. See the
[contract](CORE.md#exported-function-valued-retained-value-signatures),
[plan](PLAN.md#m52-exported-function-valued-retained-value-signatures), and
[prior art](REFERENCES.md#exported-function-valued-retained-signatures-prior-art).

## M53: native dead local bindings

A checked expression with an unused ordinary `Int` let around an opaque
`Nat` binding now loses the `Int` let. The `Nat` binding stays because a
surviving erased type argument refers to it. The new native entry reuses
GHC's deep FV annotation tree. It preserves all remaining nodes and infers
no occurrence counts. Unlifted, `One`, token and protected metadata cases
keep their bindings. This increment changes the compiler pass, not source
syntax or the interface format.

Both compiler builds pass from compiler
`d740b247a8bae21bbf741e8b24fdbf1c54f871f4` and source
`2225c8f09c52b74556d34e87d19afa2ff4f5b176`, with 85 compiler inputs and 114
source paths. The API controls pass with unchanged 1/37-line goldens. Both
stages preserve the 19-result separate-module program and the 53-line
Showcase from 13 source modules. The registered selfhosted run has 39 cases,
34 skips and five expected passes, with no unexpected results or framework
failures or warnings.

A direct API probe checks an index referenced only by a retained stable
unfolding before SimpleOpt cleans metadata. Its third attempt passes. The
first omitted two imports. The second used a comparison helper that does
not compare lambdas. Both failures and the original mixed cohort result
remain recorded. The compiler and tracked source did not change for those
probe repairs.

The independent artifact audit passes 2,359 checks over 599 pinned paths.
The evidence namespace is frozen. The audited revision
`a50950d55c50625cc4fb962bb1d35045944299f3` is published on the fork.
The canonical record is `dependent-haskell-M53-final-audit-v1.json`, SHA256
`ece09f9d7fb15fb96766138e324ac22201076efdc8d57536c6dbea5f3d3ed8aa`.
General occurrence analysis,
native unfolding export and `-O1` stay gated. C1, C2, R22 and malformed
recursive interface validation remain open. See the
[contract](CORE.md#native-dead-local-bindings),
[checklist](PLAN.md#m53-native-dead-local-binding-analysis), and
[prior art](REFERENCES.md#native-dead-local-binding-analysis-prior-art).

## M54: diagnostics for detected recursive interface-domain failures

A generic retained Pi in an ordinary imported field could force its own
unfinished domain information. The runtime raised `NonTermination` for the
six measured function-shaped and scalar-shaped knots. M54 catches that
exception at the four native interface classifier sites and reports a normal
interface error. It keeps the predicates and their order. It does not return
an unchecked Type or retry an active decoder.

Nine negative declaration graphs require both diagnostic sentences in each
compression mode: 18 negative cases. Nine valid declaration and forcing-order
cases give 18 positive cases. They cover first-order recursive domains,
nullary synonyms and an ordinary field containing a nonvacuous Boolean Pi.
The six earlier retained-field rejection graphs remain and now use both modes.
A raw exception or timeout does not satisfy a negative check.

Both compiler builds and both API fixtures pass. Their 334 actual inputs per
API phase are unchanged after self-hosting. Both stages pass the 19-result
separate-module source program and the 53-line Showcase from 13 modules.
The registered self-hosted run has 39 cases, 34 skips and five expected passes.
It has no unexpected results or framework warnings or failures.
This increment adds no source runtime feature or interface format.
The runtime does not guarantee detection of every loop. Shape-first loading,
C1, C2 and R22 remain open. See the
[contract](CORE.md#detected-recursive-interface-domains),
[plan](PLAN.md#m54-diagnostics-for-detected-recursive-interface-domain-failures),
and [prior art](REFERENCES.md#detected-recursive-interface-domains-prior-art).

The first bootstrap build passed with a redundant-import warning. The final
bootstrap build removes that import and passes without warnings. The first
API run remains historical evidence. The final fixture also forces domain
classification for a dependent function hidden in a field synonym. Raw field
free variables alone do not demand that synonym's RHS. The final API run,
both source stages and the registered run use this final fixture.
The independent artifact audit passes 2,733 checks over 735 pinned paths.
The evidence namespace is frozen. The audited revision
`91d582bb945cd0f37c4dede7ee168e24c2ba4d81` is published on the fork.
The canonical record is `dependent-haskell-M54-final-audit-v1.json`, SHA256
`6843fd43d979a85acecfa2b5f1cd74cca0eec0427c8fda94ce01833aaa341073`.

## M55: native interface domain shapes

Imported data and nullary synonyms now carry an independent domain-shape view.
The reader checks it before it decodes recursive fields. Generic retained Pi
import checks raw multiplicity and domain before allocating the binder, because
that allocator expands aliases. Valid data cycles and alias/data back-edges
remain supported. Alias-only cycles and known unsupported shapes report a
normal interface error. The original typed checks still run after a positive
precheck. The interface encoding and Core type forms stay unchanged.

Unclassified forms use the typed path with M54's detected-loop boundary.
They include applied synonyms, casts, constraint arrows and raw variable/call
indices. This preserves existing scope diagnostics. A vacuous raw Pi can become
an eligible ordinary function, so its function shape remains unclassified.
Parent and binder headers must already exist. There is no general termination
or preservation proof. This increment adds no source feature or Showcase label.

Validation selects `b0b876db25e087b286022931df03223abd499059`, with 90 compiler
inputs and 119 source paths. Both stages build. The API goldens remain 1/37
lines; both phases retain all 346 actual inputs after self-hosting. Both stages
pass the 19-result hidden-source interface client and the 53-line Showcase from
13 modules. Registered self-hosted checks have 39 cases, 34 skips and five
expected passes, with no unexpected results or framework warnings or failures.
The API covers 28 generic rejection cases, 12 named-field rejection cases,
32 valid graph scenarios and five direct cache/fallback controls.

The initial two API attempts preserve a scoped-index diagnostic regression.
The third preserves an alias-expansion loop. A temporary traced copy isolated
the allocator boundary under an OS timeout. The final checks pass after the
raw precheck moved before allocation. Build warnings in unchanged modules
remain in the logs. No full GHC suite, old executable or broad historical
matrix was replayed. C1, C2, R22 and the full program goal stay open.

The owning Notes are [Native interface domain shapes] in GHC.Core.TyCon and
[Checking native domain shapes] in GHC.IfaceToCore. See the
[contract](CORE.md#native-interface-domain-shapes) and
[provenance](REFERENCES.md#native-interface-domain-shapes-provenance).
The independent static audit passes 3,789 checks over 1,094 pinned paths,
including complete frozen-namespace coverage. Audited checkpoint
`b5695d28a0b882eaa2da378ff91535702ad0344f` is published on the fork.
All `native-interface-domain-shapes-*` files and directories are permanently
frozen. The canonical record is `dependent-haskell-M55-final-audit-v1.json`,
SHA256 `67c0af1483bffda59b9fc577ba30dfbcb78da11a34fcec6839a0d96b39b30877`.

## M56: closed parameterized index domains

Implementation checks pass at source and compiler commit
`3131ebc644c580632b8ba6f8776d3dab512c95f3`. M56 is independently audited and published on
`dependent-haskell` in the fork. The full program remains active.

Ordinary datatype parameters now pass through native index formation, constructor
conversion, substitution, branch evidence, Lint and CorePrep. The representation
keeps existing Core types, coercions and interface tags. DataKinds supplies
promoted constructors and their ordinary type arguments. Runtime arguments stay
ordinary values. No singleton translation is added.

The new [library](examples/qualified-pi/ParameterizedIndexLibrary.hs) and
[client](examples/qualified-pi/ParameterizedIndexClient.hs) use runtime lists,
optional natural numbers and Boolean trees. A list determines a vector length.
A tree determines a nested payload with `Int` and `Bool` leaves. The examples
use explicit closed families for these type computations. They do not promote
arbitrary term functions.

Validation uses the selected 91 compiler inputs and 122 source paths. It records:

- Bootstrap build: 43.06 seconds. Self-hosted build: 904.05 seconds. Both use
  `-j6`, a 16 GiB cap and no swap. The self-hosted peak is 6236 MiB.
- Both stages: 14 parameterized source observations, three intended rejection
  controls, 19 prior public programs, and the 59-line Showcase from 14 modules.
- API: unchanged one-line and 37-line goldens. Each API phase preserves 354 actual
  input hashes. The existing object fixture retains 19 quiet observations.
- Both stages: a separate constructed-list/optional-result backend probe passes
  two assertions. It emits one line per run.
- Registered tests: five tests, 39 cases, 34 skips and five expected passes. There
  are no unexpected results, missing libraries, or framework problems.

The self-hosted build changes package configuration. The three bootstrap source
runs were refreshed against that configuration. The final nine-phase record
aggregates actual results without replay. Earlier compiler errors, harness errors
and the registered wrapper's missing-configuration preflight remain recorded.
Fourmolu parses 17 changed or probe Haskell files without rewriting them.
`cabal-gild` reports the generated GHC Cabal file as not formatted. Its bytes and
the tracked Cabal inputs remain unchanged.

The domain must be saturated, boxed and first order. Its static arguments must
be closed monotypes of kind `Type`. Constructor universals are instantiated
before fields are checked. An active recursive declaration must retain the same
nominal type arguments. Growing or permuting recursion, GADTs, constructor
contexts, newtypes and actual function fields remain outside this domain.
Runtime bottoms and cycles remain possible. Native unfoldings, general
optimization and the full C1/C2 contracts remain open.

The audited documentation commit is `0d9c1a1a0b935c8caed53bf44b31591362b2f45a`.
The static audit passes 7,138 checks across 1,710 pinned paths. The canonical
record is `_build/verification/dependent-haskell-M56-final-audit-v1.json`,
SHA256 `f9af3265afcfea79878ecc87886d7cfcb915a0e417df21a081695ea092269d66`. It preserves 92 cumulative manifests.
All `_build/verification/native-parameterized-domains-*` files and directories
are permanently frozen. This publication update changes only README and PLAN.
C1 remains the next program priority.


## M57: nominal retained domain conversion

The selected compiler is `0c5699265f14826a0294d7a2b0ca9be2fefd0cbe`.
The fixture source is `cd304ad0d1af2f4f7a5502de3601fe0aaf080111`.
Both builds and the focused controls pass. M57 is independently audited.
The audited documentation commit is `2351b533905fd9a237403f073e5aa619abec1614`.

`DepFunCo` now has optional nominal domain evidence. Its body proof binds one
actual argument in the source domain. Its right endpoint binds a fresh target
argument and casts it backward in the dependent result. Explicit congruence,
symmetry, transitivity and role weakening support checked application.
An ordinary family equation relates `Token (F Bool)` and `Token Bool`.
The controls retain an opaque call in a later payload type.

Runtime casts retain the nominal proof inside `SubCo`. Expression substitution,
native SimpleOpt composition and CorePrep preserve that proof. Beta exposure
substitutes embedded proofs and accepts independent closed scalar input and
result domains. The controls check proof-only dependencies, capture avoidance,
two distinct conversions, later payloads and genuinely representational-only
rejections. A function cast remains a cast until application. No eta expansion
is added.

Seven quiet bytecode observations check values, demanded and ignored bottoms,
partial application and function WHNF. One separate control injects converted
`keep` into an actual `Main.entry`. It requires native forms after SimpleOpt,
then generates assembly, assembles and links an object, and executes it.
It returns `42` without forcing its bottom token. These observations are
separate from the nineteen inherited object observations and the 1/37 goldens.

The final bootstrap build took 24.54s and peaked at 1844 MiB. The self-hosted
build took 15m25s and peaked at 7276 MiB. Both used the unchanged six-job build
under a 16 GiB cap. Checks used 4 GiB with zero swap. Both stages pass public19,
parameterized14 with three rejections, and Showcase59 from fourteen modules.
Registered results are five expected passes, 39 cases and 34 skips. There are
no unexpected or framework failures, warnings, or missing libraries.

Evidence uses `native-domain-conversion-*`. The final source snapshot covers
92 compiler inputs and 125 source paths. The final API replay pins 45,837
logical target-library files in each phase. Artifact revalidation checks
46,199 unique inputs and both recorded compiler binaries. Earlier bootstrap,
fixture and Showcase-harness failures remain in the evidence. Fourmolu parses
all current Haskell edits. Cabal-gild still reports the existing formatting
failure in generated `compiler/ghc.cabal`; no Cabal input changed.

The Showcase stays at 59 source observations. The source checker does not yet
construct general domain congruence. Closed scalar source conversion is the
next experiment. Changed function domains, opaque Pi projections, native
coercion serialization, general optimization and the full GHC suite remain
outside this validation. C1, C2 and the full program goal stay open.

The canonical M57 record is
`_build/verification/dependent-haskell-M57-final-audit-v1.json`, SHA256
`27bddd7057cb0b6f26f220a461935bb2aed943ca7a804abbb4a2f398a9b60107`.
It preserves 96 cumulative manifests. The independent audit passes 101,598
static checks across 47,184 pinned paths. This audit executes no compiler or
test target. The complete `native-domain-conversion-*` evidence namespace is
permanently frozen. The later publication commit changes only README and PLAN.


## Source nominal domain conversion checkpoint

M58 checks nominal retained-function conversion directly from Haskell source.
The selected compiler is `c3bb19372c96753cd6ccca915894fc46202c3abb`. The phase source is `b1a97f28c22b34e7e66ff2249a191f8d63e5e0b4`.
`ParameterizedIndexLibrary` defines `SourceToken` with a family application and
`TargetToken` with its nominal normal form. `adaptTokenVector f original = original`
checks without a singleton translation or function eta expansion.
`applyTokenVector` converts an actual argument before result instantiation.
`specializedTokenVector` infers the ordinary numeric payload during body comparison.

The body solve uses the existing forall solver's private-scope pattern. Runtime
Ids keep a separate lexical solver scope. Unresolved body Wanteds do not escape.
The original whole equality remains when the body cannot be solved. Native meta
checks still reject assignments that contain a runtime index, including proof
metadata. Nominal cast evidence survives wrappers and final zonking even for a
phantom datatype parameter. Wrapper composition keeps its existing order.

Domain reduction uses the existing solver reduction-depth limit. The recursive
changed-domain control now reports a located GHC-40404 error at depth 11 with
`-freduction-depth=10`. It previously timed out after eight seconds. An identical
recursive domain needs only reflexivity and remains accepted. These controls pass
in both compiler stages. The existing API fixture records both cases.

Both compiler stages build under 16 GiB, no swap and unchanged `-j6`. The focused
Core API goldens pass 1/37. In both stages, public source checks pass 19 observations,
parameterized clients pass 14 plus three rejections, and the Showcase passes 68
observations across 14 actual modules. The new standalone source controls pass
11 runtime observations and two located rejections per stage. The payload-inference
controls pass three runtime observations and one index-escape rejection per stage.
These controls overlap with the Showcase; do not add them as distinct feature totals.
The five registered tests pass across 39 cases, with 34 skips and no unexpected or
framework outcomes.

The three original probe sources now compile. These are compile-only observations.
The old compiler rejected the function conversion and panicked on the explicit
argument conversion. The actual type-checker trace now shows the ordinary payload
meta assigned `Int` at the deeper body-comparison level. It records an empty local
residual and a completed nominal retained-function proof. The trace uses
`-fno-zap-coercions`. Runtime controls check ignored indices, demanded indices,
function WHNF, head-before-index order and reverse conversion.

Snapshot 7 pins 95 selected compiler leaves and 128 phase-source paths. Each build
keeps its own producer metadata. All final checks use the selected phase source.
Final documentation has its own commit and manifest. The nine-phase cohort only
aggregates actual executions. The first registered wrapper lacked its argument
file and launched no tests. Its separate retry supplies the selected registered
result. The six successful source phases retain their original execution records. The API records pin the full target libdir, and a
separate artifact check validates both compiler hashes and the original input maps.
Older builds, fixture failures and parser failures remain in the evidence tree.

The ten changed compiler modules pass parsing, including both branches at the
existing statement-level CPP boundary. Fourmolu cannot parse the experimental
source extension in either the parent or current examples. Both compilers check
those examples with Core and STG Lint instead. The generated Cabal formatting
check retains its existing failure; no Cabal input changed.

Selected evidence is under `_build/verification/native-source-domain-conversion-*`.
The independent static audit passes 103,851 checks across 47,634 pinned paths.
The audited documentation commit is `a03302da20`. The selected inputs remain the
95 compiler leaves and 128 phase-source paths recorded above. The final partitions
contain nine documents, 163 harness files and 1,325 output files. The cumulative
manifest count is 100. The entire `native-source-domain-conversion-*` namespace is
permanently frozen. The external canonical record is
`_build/verification/dependent-haskell-M58-final-audit-v1.json`, with SHA256
`ab9e2ce9ac0b60c86d66caf55ac6dc9eb715352fc9c600bb3b634c30f5df0270`.
This publication commit changes only README and PLAN status.
The existing seven-work, nine-PDF reading ledger is unchanged. D/DC supplies the
nominal congruence rule. Agda supplies the domain-before-body comparison precedent.
Neither supplies a preservation or sharing theorem for this combined implementation.

Next, compare bodies beneath fixed function-valued retained domains with `MRefl`.
Typed projection from opaque nominal Pi equality needs a separate checked rule.
Open domains, general dependent inference, native coercion serialization and normal
optimization remain open. M58 completes none of C1, C2 or R22.

## Opaque retained-function cast lowering

M61 checks a whole cast between two dependent functions while its payload types
and nominal equality proof remain abstract. Existing CorePrep and CoreToStg
support the measured path. It adds no coercion constructor or eta expansion.
The probe exposed an escaping let alias during partial application. Native
function beta reduction now substitutes an eligible bare function variable
through terms, types and coercions together. Other arguments retain their redex.

Compiler `f0b45ad34b` and fixture `b03cfae43b` pass both builds and the selected
cohort at source checkpoint `c45b0b7bf0`. The existing fixture checks three
abstract preparation shapes, nine quiet bytecode observations, and a separate
abstract `NOINLINE` kernel in actual NCG. Its CorePrep dump retains `Sub cv`
between distinct abstract payloads. The object executable checks an opaque keep
caller returning 42 and preserves the earlier scalar control's separate 42.
Partial-WHNF and vector/index-demand checks are bytecode observations.

API goldens stay 1/37. Both stages pass public19, parameterized14 with three
rejections, and Showcase84 through fourteen modules. Registered tests report
five expected passes with zero unexpected failures or framework warnings.
The first self-host attempt's package-registration failure remains recorded;
the unchanged-input retry passes. The independent audit passes 218,015 checks
over 46,963 pinned paths. All 491 `native-opaque-whole-cast-*` files are frozen.
The canonical record is `dependent-haskell-M61-final-audit-v1.json`; its digest
and source identities are in [the M61 plan](PLAN.md#m61-opaque-whole-function-cast-lowering).
This closes no general
Core, inference or optimizer obligation. Opaque proof projection remains open.

## Complete retained-function type arguments

M62 permits a complete written worker type in an explicit type argument.
The checked [example library](examples/qualified-pi/FunctionSignatureLibrary.hs)
includes this ordinary lifted consumer:

```haskell
forceExplicitCall ::
    (foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)) -> IO ()
forceExplicitCall worker =
    seq @(foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n))
        worker (pure ())
```

`selectCall` uses `id` with the same explicit type, then permits runtime
application. `forceNestedCall` selects `Maybe` of that type and forces only
the outer constructor. The source controls also check list and tuple fields,
ordinary kind annotations, captured runtime indices and required type
arguments. A hole outside an embedded complete type still makes the whole
argument partial. The type checker rejects it after T2T translation.

Both rebuilt stages pass eighteen focused cases and fifteen compatibility
cases. The selected API goldens remain 1/37. Public19, parameterized14 with
three rejections, Showcase88/14 modules and five registered tests pass.
The existing fixture and Showcase contain the new observations. No test file
was added. The independent evidence audit passes 210,784 checks over 47,471
pinned paths and freezes 1,314 evidence files. Its canonical record and source
identities are in [the M62 plan](PLAN.md#m62-complete-retained-function-type-arguments).

This uses the existing GHC completeness and type-application paths. It adds
no Core constructor or singleton layer. NoImpredicativeTypes direct `seq`
passes; the separate outer Quick Look controls still reject. General
inference, matchability and generic index domains remain open. See
[the M62 plan](PLAN.md#m62-complete-retained-function-type-arguments) and
[the R03 card](roadmap/R03-dependent-products.md).

## Historical roadmap checkpoints

The table records implementation history and proposed local work.
M75–M88 include examples motivated by R15. Their computation rules
compile checked clauses to type families under R13.
M98's two-list zip is a proposed extension of that mechanism. The
evaluator work has priority.

| Order | Owner and contribution | State | Gate and next result |
| --- | --- | --- | --- |
| 1 | R03: fixed function-domain body comparison; exercises R05 evidence | Complete bounded increment: M59 | Both stages and independent audit pass. The larger R03/R05 features remain open. |
| 2 | R03: safe indexing over a retained `[Bool]` | Complete bounded increment: M60 | Both stages and the evidence audit pass. The next compiler task is the R05 opaque-cast probe. |
| 3 | R05: application through opaque whole-function evidence | Complete bounded increment: M61 | Both builds, selected cohort and independent audit pass. Typed opaque proof projection remains separate. |
| 4 | R03: explicit complete retained-function type arguments | Complete bounded increment: M62 | Both builds, nested source arguments, worker demand, the selected cohort and independent audit pass. Next: the R23 foundation. |
| 5 | R23: matchability foundation; also advances R05 | M63/M64 foundation integrated and published | Explicit arrow and forall metadata pass Core, source, interface, TH and reflection checks. The generic native client and source family unsaturation remain open. |
| 6 | R05: function values in closed algebraic indices; enables R03 source work | Complete bounded increment: M65 | Both rebuilt stages pass source/interface controls and Showcase100/16. Seven registered tests pass. Generic `[a]`, higher-order endpoints and changed dependent domains remain open. |
| 7 | R05: bounded family views in native frontend inference | Complete bounded increment: M66 | Both builds, exact source diagnostics, finite controls, Showcase100/16 and seven registered tests pass. Other pure normalization paths remain separate. |
| 8 | R05: structural generic domains and dependent-domain transport; enables R03 | Complete bounded increment: M67 | Both builds, selected API/source controls, Showcase116/16 and seven registered tests pass. Generic elements include arbitrary lifted and runtime-index-bearing values. Old readers reject the new typed signature cleanly. R05 and general source domains remain open. See M67 in PLAN.md. |
| 9 | R03: ordinary callers of imported retained APIs; preserves R05 evidence | Complete bounded increment: M68 | Both rebuilt stages pass source/interface checks, scope and multiplicity controls, Showcase118/17 and seven registered tests. Published M67 readers accept the interfaces. Explicit syntax, Expr inference modes and native pipeline policies retain their scope. |
| 10 | R03: safe indexing over a generic retained list | Complete bounded increment: M69 | Reuses Fin and GenericCount with scoped list refinement. Both rebuilt stages pass the reviewed primary/focused source collections, API1/37, Showcase124/17 and seven registered tests. Guarded list-declaration advice preserves the original runtime-vector callback. Case-based ordinary clients pass. Measured literal-list/fmap and NoEDT native let-list inference retain their limits; named-list fmap is unmeasured. No new Core judgment or wire field. R03, R05 and R23 remain open. |
| 11 | R03: higher-order calls with native list arguments | Complete bounded increment: M70 | List fields supply an anchor to the existing argument cache. The literal fmap call passes with and without the dependent source extension. Both stages pass the 125-line Showcase, and seven selected registered tests pass. |
| 12 | R05: invisible retained function types and coercions; enables R03 | Complete bounded increment: M71 | Both stages compile. The API, versioned interface and object tests cover visibility and demand. |
| 13 | R03: source definitions and omitted invisible runtime operands | Complete bounded increment: M72 | The imported call passes the IO-sourced Nat from a later vector declaration. Native object tests cover ignored bottom, name shadowing and located missing and conflicting operands. |
| 14 | R03: explicit invisible runtime operands | Complete bounded increment: M73 | Written `@{n}` works with or without erased `@Int`. The interface-only client preserves demand. A wrong written index reports the later argument where the two indices disagree. |
| 15 | R03: consecutive invisible runtime operands | Complete bounded increment: M74 | The interface-only vector append infers two lengths and checks their sum in the result type. Constructor inference respects term scope and records import usage. |
| 16 | R13: finite tables of source equations | M75 finite constructor tables | Check the complete table once and export its equations. A body change invalidates an unchanged client proof. |
| 17 | R13: constructor fields in a source equation | [M76](PLAN.md#m76-constructor-fields-in-a-source-equation) | The checked `previous` clause returns the length of the tail. A `VCons` branch can therefore return its tail directly from `dropFirst`. |
| 18 | R13: structural recursion on a list spine | [M77](PLAN.md#m77-structural-recursion-on-a-list-spine) | A checked recursive call on the tail computes the Peano result index of `boolVector`. A call on the rebuilt list has no checked equation. |
| 19 | R13: parametric list length | [M78](PLAN.md#m78-one-list-length-function-for-every-lifted-element-type) | One checked family proves the cons equation for an abstract element type and tail. The named length function indexes a vector built from a list of `Int` read at runtime. |
| 20 | R13: a second ordinary argument | [M79](PLAN.md#m79-addition-from-an-ordinary-two-argument-function) | The runtime addition function computes the length index of vector append. The second length can stay unknown. |
| 21 | R07 and R13: numerals and infix addition | [M80](PLAN.md#m80-numerals-and-infix-addition-in-vector-types) | Use `Vec a 3` and `Vec a (n + 1)` with the original Peano-indexed GADT. |
| 22 | R03 and R13: a computed retained argument | [M81](PLAN.md#m81-a-computed-retained-length) | Pass the ordinary call `plus n m` directly as a vector length. Keep its index through Core Lint and share its value as a lazy STG thunk. |
| 23 | R12 and R03: a concrete class method | [M82](PLAN.md#m82-overloaded-addition-in-a-retained-argument) | Pass the overloaded `n + m` as a retained length when the selected instance's addition method is the checked `plus`. |
| 24 | R07, R12 and R03: term numerals as retained lengths | [M83](PLAN.md#m83-ordinary-numerals-in-retained-arguments) | Use a checked `fromInteger` for `replicateNative 3 x` and `replicateNative (n + 1) x`. |
| 25 | R03: infer a runtime index from a nested call | [M84](PLAN.md#m84-infer-a-nonempty-vectors-tail-at-a-nested-application) | Match the checked successor equations in `safeHead (replicateNative (1 + n) x)` and preserve the unevaluated tail. |
| 26 | R03: infer parameters from constructor fields | [M85](PLAN.md#m85-infer-the-parameters-of-a-retained-constructor-argument) | Read the fields of a saturated constructor in a retained argument to select erased parameters. |
| 27 | R03 and R04: wildcard argument and field patterns | [M86](PLAN.md#m86-wildcard-patterns-for-retained-arguments-and-fields) | Ignore a runtime field while preserving its identity in later types. |
| 28 | R05: interactive bytecode | [M87](PLAN.md#m87-persistent-dependent-definitions-in-ghci) | Store a retained function and call it from a subsequent GHCi command. |
| 29 | R13: composition of source equations | [M88](PLAN.md#m88-compose-checked-source-equations) | Check multiplication in terms of addition and use the product as the length of a flattened rectangle. |
| 30 | R03 and R05: nested retained operands | [M89](PLAN.md#m89-nested-computations-in-retained-operands) | Pass `1 + rows * cols` directly to a vector constructor and preserve demand on the product. |
| 31 | R03 and R05: equality for lazy local definitions | [M90](PLAN.md#m90-equality-for-lazy-local-definitions) | Give a named checked product its defining index and share its runtime value. |
| 32 | R03 and R05: dictionary evidence for local definitions | [M91](PLAN.md#m91-dictionary-evidence-for-a-local-index-definition) | Use the selected multiplication and literal conversion to check a local vector length. |
| 33 | R03 and R05: signed and grouped local lengths | [M92](PLAN.md#m92-signed-and-grouped-local-lengths) | Check signed runtime lengths in dependency order and compose their equalities in the body. |
| 34 | R05: retained types in editor metadata | [M93](PLAN.md#m93-retained-types-in-hie-files) | Write and read retained binders, runtime indices and opaque applications in HIE files. |
| 35 | R03: inferred nonempty local lengths | [M94](PLAN.md#m94-infer-a-nonempty-tail-through-local-length-definitions) | Infer the tail of a vector through checked let and where definitions, then check the call against its actual argument. |
| 36 | R03 and R04: ordinary lists at an indexed boundary | [M95](PLAN.md#m95-check-ordinary-list-inputs-at-an-indexed-boundary) | Check a requested length or package the list's own length. Match ordinary list patterns in retained equations. |
| 37 | R03: partial constructor callbacks | [M96](PLAN.md#m96-infer-retained-indices-for-partial-constructor-callbacks) | Use a checked input's runtime index in `VCons x <$> fromListExact n xs`. Preserve constructor multiplicities and check the full callback type. General callback inference remains open. |
| 38 | R05: simplify a case of a known constructor | [M97](PLAN.md#m97-simplify-a-case-of-an-ordinary-constructor-in-dependent-core) | Remove a measured allocation and dispatch while keeping dependencies in types, lazy field sharing and strict-field demand. |
| 39 | R13: the length of ordinary zip | [M98](PLAN.md#m98-the-length-of-zip-over-two-ordinary-lists) | Check equations that inspect two list spines and recurse on both tails. Preserve the empty-first equation with an unknown second input. |
| 40 | R15: evaluate an imported closed call | [M99, verified for literal Natural lists](PLAN.md#m99-evaluate-an-imported-closed-call) | The source call computes the vector length. The unchanged client observes an ABI-preserving implementation change. |
| 41 | R16: evaluate with a rigid argument | [M100, verified for polymorphic identities](PLAN.md#m100-reduce-an-ordinary-identity-at-a-rigid-type-variable) | Both compilers accept the upstream signature. Changing a worker behind an imported alias invalidates the unchanged consumer. |
| 42 | R15: closed character calls | [M101](PLAN.md#m101-evaluate-closed-character-calls-from-installed-libraries) | The ordinary `Data.Char.toUpper` and `Data.Char.isDigit` check equality witnesses. Both interpreters track the producer code they loaded. |
| 43 | R16: open projections | [M102](PLAN.md#m102-reduce-open-projections) | Prelude's `const` and an independent second projection return the selected rigid type variable. A strict second projection identifies the operand blocking evaluation. |
| 44 | R15: overloaded closed calls | [M103](PLAN.md#m103-infer-a-closed-overloaded-call-from-its-argument) | `Prelude.sum '[2, 3, 5]` computes the index 10 using ordinary dictionaries. The client records the selected instance even if the interpreter calls its method implementation directly. |
| 45 | R16: retry after local inference | [M104](PLAN.md#m104-retry-a-type-call-after-inference-determines-its-input) | A pattern fixes the input. GHC checks the computed result before generalization. An ordinary client reads the concrete signature from the interface. |
| 46 | R15: lists as results | [M105](PLAN.md#m105-return-a-promoted-list-from-a-closed-call) | Prelude's ordinary `reverse` checks a list equality. The checked scalar element kind determines how to read the result, including for an empty list. |
| 47 | R15: nested closed calls | [M106](PLAN.md#m106-check-a-closed-call-inside-another-closed-call) | A reversed list supplies the argument to an ordinary `sum`. The client records dependencies on both producers. |
| 48 | R16: observe a character case | [M107](PLAN.md#m107-observe-a-character-case-with-rigid-result-operands) | An imported worker selects between two rigid types. The API regression records and checks the selected branches against its typed Core. |
| 49 | R15: use computed operands | [M108](PLAN.md#m108-accept-a-computed-value-at-each-supported-result-kind) | The result kind determines the next operand's representation, including an arbitrary-precision natural and lists of Boolean or character values. |
| 50 | R16: export a character case | [M109](PLAN.md#m109-carry-an-open-call-in-an-exported-equality) | A client solves an exported worker equality at a literal from two interfaces. A visible conflicting rewrite rule prevents that use. |
| 51 | R15: accept source literals | [M110](PLAN.md#m110-check-ground-source-operands-at-their-declared-kinds) | The argument kind selects how to check the literal. A mixed list fails at the element with the wrong kind. |
| 52 | R15: accept the upstream spelling | [M111](PLAN.md#m111-use-the-closed-evaluator-with-the-roadmaps-source-spelling) | The existing name lookup rules admit an unqualified imported length function on `[1,2,3]`. |
| 53 | R15/R16: compose a family result with an ordinary call | [M112](PLAN.md#m112-pass-a-checked-family-result-to-the-closed-evaluator) | The solver supplies the inner character and the interpreter runs the outer function. Each records its own dependency. |
| 54 | R15: check ordinary functions with several arguments | [M113](PLAN.md#m113-check-fully-applied-ordinary-functions) | `gcd 18 24` computes six; later operands determine the kind of earlier empty lists. |
| 55 | R16: project a rigid type from a boxed pair | [M114](PLAN.md#m114-project-a-type-from-a-known-pair) | The imported `fst` and `snd` select original type variables. A demanded rigid pair still blocks. |
| 56 | R23: diagnose arrow matchability after family reduction | [M115](PLAN.md#m115-distinguish-matchability-errors-from-missing-arguments) | GHC reports conflicting matchabilities at `Maybe` and retains the hint for a genuinely missing argument. Ordinary program compatibility remains open. |
| 57 | R15: return and use a boxed pair | [M116](PLAN.md#m116-return-a-boxed-pair-from-a-closed-call) | `quotRem 17 5` returns `'(3,2)` during type checking. Nested pairs retain their field kinds. |
| 58 | R15: return an optional value or a sum | [M118](PLAN.md#m118-check-maybe-and-either-results) | `uncons` returns a promoted optional pair. Another closed call can consume either alternative of a parser's result. |
| 59 | R16: export checked projections of unknown pairs | [M119](PLAN.md#m119-export-checked-projections-of-unknown-pairs) | An ordinary client checks both fields from the exported equality with only the interfaces available. |
| 60 | R15: check finite lists of compound results | [M120](PLAN.md#m120-check-lists-of-compound-closed-results) | The list of suffixes from `tails` supplies the operand to ordinary `concat`. |
| 61 | R16: certify a pair constructor case | [M121](PLAN.md#m121-export-a-checked-pair-constructor-case) | An installed `swap` equation retains the identity of both fields. Its result can feed either projection. |
| 62 | R15: check closed constructor arguments together | [M122](PLAN.md#m122-check-closed-constructor-arguments-together) | `catMaybes` accepts a literal list of optional values. Another argument can determine a missing constructor kind before evaluation. |
| 63 | R16: prove a projection through a swap | [M123](PLAN.md#m123-prove-strict-pair-composition-after-substitution) | An ordinary client combines three imports at a rigid pair index. GHC checks nominal evidence even when substitution first exposes the composition. |
| 64 | R15: public enumerations in closed calls | [M124](PLAN.md#m124-reify-public-enumeration-constructors-in-closed-calls) | An ordinary producer returns a user-defined constructor that becomes the input to another ordinary function. |
| 65 | R16: compose strict field selections | [M125](PLAN.md#m125-follow-strict-field-projections-through-pair-reconstructions) | A nominal proof relates two field paths from a shared scrutinee, including duplication. |
| 66 | R21/R16: schedule an independent local definition | [M126](PLAN.md#m126-schedule-local-definitions-for-open-evaluation) | The same source module uses an ordinary selection function on rigid type arguments in either declaration order. |
| 67 | R15: reify finite recursive datatypes | [M127](PLAN.md#m127-return-finite-recursive-datatypes-from-closed-calls) | A function returns an ordinary binary tree that another function traverses at type checking. |
| 68 | R21/R16: observe a local helper chain | [M128](PLAN.md#m128-follow-local-value-dependencies-in-source-types) | Changing a transitive helper changes the exported type. A rule on an ignored helper leaves the proof valid. |
| 69 | R15: reify fixed-index GADT constructors | [M129](PLAN.md#m129-reify-constructors-at-known-indices) | A partial constructor import covers every value at one result index. A second ordinary function consumes the result. |
| 70 | R15: recover a constructor-local vector index | [M130](PLAN.md#m130-recover-constructor-local-vector-indices) | The same ordinary polymorphic `safeHead` runs on a vector and computes a closed index from its first cell. |
| 71 | R21/R16: observe recursive local value groups | [M131](PLAN.md#m131-observe-recursive-local-value-groups) | GHC can infer a mutually recursive group and use its terminating selection in a type. A demanded loop reaches the open reduction limit. |
| 72 | R15: apply an ordinary consumer to a written existential | [M132](PLAN.md#m132-consume-a-written-existential-package) | The kind checker supplies a vector's erased length from a written singleton and vector. |
| 73 | R16: select a symbolic field by constructor | [M133](PLAN.md#m133-select-symbolic-fields-by-constructor) | A case selects different field positions in two alternatives using only the producer interface. |
| 74 | R21: interleave local type and value checking | [M134](PLAN.md#m134-order-mixed-type-and-value-dependencies) | A type can use a local function whose signature depends on another local type-level call. |
| 75 | R15: read back a witnessed existential | [M135](PLAN.md#m135-reify-a-package-at-its-witnessed-index) | An ordinary function chooses a vector length and a dependent client checks the returned contents. |
| 76 | R15: preserve demand across ordinary calls | [M136](PLAN.md#m136-preserve-demand-in-nested-ordinary-calls) | An ordinary consumer reads the first field of a private intermediate. |
| 77 | R16: return a constructor with symbolic fields | [M137](PLAN.md#m137-read-back-open-constructor-results) | A producer constructs a user datatype or boxed pair of unknown types. An ordinary client checks the exported result. |
| 78 | R21: install derived instances and defaults | [M138](PLAN.md#m138-check-derived-instances-and-defaults-before-local-functions) | A dependent type and its ordinary consumer use the same instance and default environments. |
| 79 | R15: infer the target of a closed conversion | [M139](PLAN.md#m139-use-the-expected-kind-of-a-closed-call) | The installed Prelude length and conversion compute the index of a three-cell vector. |
| 80 | R15: infer nested results from a declared domain | [M140](PLAN.md#m140-check-a-nested-conversion) | The target of a nested conversion can come from a sibling literal in either position. |
| 81 | R23: check omitted matchability in dependent arguments | [M141](PLAN.md#m141-infer-arrow-matchability-in-a-dependent-argument) | An ordinary type declaration uses `D (Type -> Type) Maybe`. Both explicit mismatches still fail. |
| 82 | R16: export an open equation for an ordinary pair case | [M142](PLAN.md#m142-export-a-stuck-pair-swap-from-an-ordinary-producer) | A separate ordinary consumer reduces an imported swap when it supplies the pair. |
| 83 | R15: share inferred kinds across ordinary calls | [M143](roadmap/R15-closed-term-evaluator.md#m143-sharing-the-kinds-of-computed-operands) | Two computed operands specialize each other in either order. |
| 84 | R16: keep a sum projection in an exported type | [M144](roadmap/R16-open-term-evaluator.md#m144-deferring-a-projection-over-an-ordinary-sum) | An ordinary consumer supplies either constructor and recovers the selected field. |
| 85 | R15: infer a computed argument through a checked case | [M145](PLAN.md#m145-infer-a-computed-operand-through-a-checked-case) | The known result fixes part of the kind of a computed pair. |
| 86 | R16: defer the constructor tests in a nested case | [M146](PLAN.md#m146-export-an-ordinary-nested-constructor-case) | A downstream client supplies the constructors and recovers the selected field or fallback. |
| 87 | R21: schedule an explicit class instance in a dependent module | [M147](PLAN.md#m147-check-a-local-instance-head-before-its-methods) | An ordinary client runs a method whose type annotation needs a later checked producer. |
| 88 | R16: construct results of an open case | [M148](roadmap/R16-open-term-evaluator.md#m148-constructor-results-from-a-checked-case) | An ordinary client proves an optional head from two interfaces with a rigid tail. |
| 89 | R15: project a computed pair inside an ordinary call | [M149](roadmap/R15-closed-term-evaluator.md#m149-demand-only-the-selected-field-of-a-computed-pair) | A closed test for oddness leaves the unused pair field unevaluated. |
| 90 | R21: schedule family instances with local producers | [M150](roadmap/R21-dependency-analysis.md#m150-constructors-and-selectors-of-family-instances) | An ordinary client uses a method, a selector, and derived instances from the dependent module. |
| 91 | R16: retain a helper under a result constructor | [M151](roadmap/R16-open-term-evaluator.md#m151-checked-helper-calls-inside-constructor-results) | An ordinary client checks an optional head through a private helper. |
| 92 | R21: check a local producer needed by an instance equation | [M152](roadmap/R21-dependency-analysis.md#m152-independent-local-producers-for-type-instances) | An independent identity supplies the result type of a local family equation. |
| 93 | R15: keep an unevaluated pair field in an interface | [M153](roadmap/R15-closed-term-evaluator.md#m153-export-a-direct-projection-of-a-partial-pair) | A direct projection of the copied input checks through two stripped interfaces. |
| 94 | R16: check the body of a private constructor helper | [M154](roadmap/R16-open-term-evaluator.md#m154-a-helper-can-construct-its-result) | A case result can call a private helper that wraps its input in a public lazy constructor. |
| 95 | R16: follow private helper chains | [M155](roadmap/R16-open-term-evaluator.md#m155-reachable-helper-chains) | An ordinary client proves a constructor result through three private helpers. A changed leaf or a cycle invalidates the proof. |
| 96 | R21: schedule local functions before imported family equations | [M156](roadmap/R21-dependency-analysis.md#m156-independent-producers-for-imported-type-families) | An ordinary client can use all three equations. The test observes the change in dictionary selection that would make unrestricted reordering incorrect. |
| 97 | R16: select a branch of a private case | [M158](roadmap/R16-open-term-evaluator.md#m158-a-private-helper-can-match-a-constructor) | A client proves which element of a nonempty spine the helper selects. The remaining tail can stay unknown. |
| 98 | R16: reduce a structurally recursive constructor result | [M159](roadmap/R16-open-term-evaluator.md#m159-structural-recursion-under-a-result-constructor) | The first output cell and its recursive tail agree with the worker's equation for every tail. |
| 99 | R21: check independent imported function types | [M160](roadmap/R21-dependency-analysis.md#m160-independent-imported-function-types) | An ordinary imported projection determines a family result. Changing the projection's body invalidates the method that consumes it. |
| 100 | R16: preserve an argument through a recursive call | [M162](roadmap/R16-open-term-evaluator.md#m162-structural-recursion-with-an-unchanged-argument) | A client checks one output cell and its recursive tail with the tag still unknown. |
| 101 | R21: use ordinary constructors in an imported family equation | [M163](roadmap/R21-dependency-analysis.md#m163-ordinary-data-in-an-independent-producer) | A local function uses the installed `Prelude.maybe` on `Just x`. A hidden family inside an imported `Maybe` type still prevents premature checking. |
| 102 | R21: infer an independent local producer | [M164](PLAN.md#m164-infer-a-local-function-before-an-imported-family-equation) | The ordinary client reads the family result from the interface when the producer has no signature. |
| 103 | R15: pass an imported function to a closed call | [M165](PLAN.md#m165-pass-an-imported-function-to-a-closed-call) | `map not` reduces at a known list. Changing only a callback object invalidates the unchanged client proof. |
| 104 | R23: pass a type family to another family | [M166](PLAN.md#m166-pass-an-unapplied-type-family) | `Each Flip` accepts an unapplied user family. Its equations can reduce when a client supplies an argument. |
| 105 | R16: supply a constructor as a function | [M167](PLAN.md#m167-pass-a-constructor-to-an-ordinary-helper-during-open-evaluation) | `uncurry` can build a constructor from two rigid fields. |
| 106 | R16: preserve a callback while mapping over an unknown list | [M168](PLAN.md#m168-check-the-list-equation-of-a-higher-order-source-function) | A rule-free mapper exports the cons equation for an arbitrary callback and tail. |
| 107 | R13 and R16: instantiate the callback with a checked source function | [M169](PLAN.md#m169-carry-a-checked-named-callback-across-an-interface) | A traffic-light callback determines the head of the result through interfaces alone. |
| 108 | R16: use a callback from an installed ordinary module | [M170](roadmap/R16-open-term-evaluator.md#m170-use-the-installed-boolean-callback) | A client checks the head using `Prelude.not` and the rules visible at the proof. |
| 109 | R21: order local datatypes, a term, and an imported family equation | [M171](roadmap/R21-dependency-analysis.md#mixed-type-and-term-declarations) | A field whose type is `Payload Double` accepts `True`. Adding a cyclic dependency in another constructor's field rejects the equation. |
| 110 | R23: pass a family with an invisible argument | [M172](roadmap/R23-unsaturated-type-families.md#passing-a-family-with-an-invisible-argument) | A receiver that expects the polymorphic family can apply it at `Bool`. An ordinary client checks the resulting equality. |
| 111 | R16: use the ordinary Prelude mapper across an interface | [M173](roadmap/R16-open-term-evaluator.md#m173-check-the-installed-prelude-mapper-before-publishing-its-equations) | An ordinary client proves the empty and cons equations with rewriting disabled. A changed optimized producer still compiles without issuing the evidence. |

M64 now carries explicit forall metadata in the development source. The
[Core contract](CORE.md#forall-matchability) records scope, nominal proofs,
runtime U and the conservative expression and constructor-evidence limits.
The selected builds and checks pass, including Showcase95 with 14 home
modules in both stages. The standard selection passes seven tests with
54 skipped ways. R23 remains open for the generic native client and family source
unsaturation.

M65 connects stored function values to canonical opaque call indices. A
saturated family can expose the stored function without losing its forward
application proof. Later package fields retain the actual function identity.
See [the Core contract](CORE.md#function-values-in-algebraic-indices) and
[the validation record](PLAN.md#m65-validation). This increment does not
complete R05, R03 or R23.

M67 adds generic `[a]` constructor refinement and nominal transport through
later dependent domains. M68 measures direct retained API use without the
dependent extension. At baseline, three ordinary consumers reject because
their erased index cannot be inferred. M68 uses imported type contracts to
enable the existing application checker. Explicit dependent
syntax and the restricted native pipeline keep their current policies.
Both rebuilt stages and the selected tests pass that bounded application change.
M69 followed with generic safe indexing. The local subset and
dependent-base tasks below retain their own scope.

The user resumed GHC execution on **2026-11-16 at 17:53 CET**. Run heavy checks
serially under the configured memory cap. This execution policy does not add
feature dependencies.


## Historical work packages

These names predate the roadmap-aligned work queue. Keep them as references for
the recorded increments. S0–S3 map to R03/R05; C1/C2 to R05; Q1 to R03; Q2 to
R04/R22; E1/E2 to R15/R16; P1 to R13; P2 to the remaining promotion domains;
L1 to R20; and I1 to R01. The current queue above owns the next work.

- [x] **S0: Retain a runtime index.** Native Bool functions, checked case
  refinement, and `choose`/`keep` execution. `keep` does not force its index.
  Evidence: CORE.md and the existing Core/API runtime fixtures.
- [x] **S1: Use dependent data from source.** Nat-indexed vectors, retained
  packages, append/reverse, dynamic-length equality, and bounded filtering.
  Evidence: the public packed-vector example and its 562 observations.
- [x] **S2: Cross module and IO boundaries.** Separate interfaces, retained
  closures, runtime IO indices, scoped local types, and measured lazy fields.
  Evidence: the public examples and the M23–M35 checkpoints in PLAN.md.
  M52 also exports complete ordinary function-retained signatures through the
  existing interface type forms. Both stages pass the19-line imported worker
  program and53-line showcase. This does not complete the general product or
  transformation tasks.
- [x] **S3: Instantiate known native workers.** Expected types, visible type
  arguments, and declared monomorphic arguments select immutable erased choices.
  ImpredicativeTypes controls pass at M38. General mutable native instantiation
  stays gated. Evidence: `KnownArgumentClient.hs`, M36–M38, and the two source
  matrices above. M52 calls an imported `idCall @a f n xs` with actual stored
  function/input fields. Exact local function operands and ordinary one-time
  argument/usage checks remain required. No new inference rule is added.
- [ ] **C1: Reconcile the Core contract with the research.** Depends on the
  measured S0–S3 baseline. Map each native form and coercion to the relevant
  rules in the 2017 specification, 2019 roles paper, and 2021 graded paper.
  State the differences. Specify substitution, conversion, scope, runtime usage,
  and erasure before extending the representation. Close this task with a
  reviewable contract and focused transformation checks, not a claim that the
  papers prove our implementation. Advances `R05`.
  M39 records the correspondence limits and adds checked Core beta exposure.
  The [contract](CORE.md#checked-beta-exposure) and existing API fixtures cover
  this bounded reduction. M40 exposes the same subset from source with checked
  scope, multiplicity and Strict-mode boundaries. Its
  [source contract](CORE.md#bounded-source-beta-applications) preserves ordinary
  usage checking. The full C1 obligation remains open.
  M54 reports detected recursive interface-domain failures through the normal
  interface diagnostic path. It preserves valid first-order recursive domains
  and ordinary fields containing valid retained Pi types. This is not a
  shape-first loader, guaranteed loop detection or a termination proof.
  M55 adds derived data and synonym views before field decoding and generic
  binder allocation. Known unsupported graphs fail before typed recursion.
  Valid data cycles remain supported. Unclassified normalization keeps the
  typed fallback. Missing headers and general termination remain open.
  C1 remains open.
  M42 adds scoped opaque call indices with independently closed scalar input
  and result domains. It checks forward nominal congruence, conservative
  noninjectivity, function-head substitution and actual strict-call lowering.
  M43 adds source formation and exact whole-call refinement for `case f n`.
  Checked alias evidence preserves call demand, captures and lazy fields.
  Both stages pass the focused source, runtime and negative controls.
  M44 admits checked constructor-tree arguments in these source cases. It
  preserves exact whole-call identity and reuses the existing Core bridge.
  Both stages pass source, demand and located negative controls. General
  computation and a preservation theorem remain open.
  M45 implements an explicitly signed immediate-lambda step. It requires
  actual assigned Many metadata and excludes Strict from that new path.
- [ ] **C2: Complete native Core transformations.** Depends on C1. Add the
  missing transformations incrementally, including normal simplification and
  unfolding handling. Preserve Core Lint, type preservation, runtime demand,
  sharing, and diagnostics at each step. Keep unsupported passes gated until
  checked. Run broader GHC compatibility checks before closing this task.
  Advances `R05`; it is also a release gate for later frontend work.
  M41 adds one checked local variable-alias substitution. Direct API checks
  cover scope, exact types, type-only dependencies and demand. Both stages
  preserve the 106 raw/public and 562 vector observations. Ordinary inlining,
  occurrence analysis and general unfolding handling remain gated.
  M53 adds a restricted dead-local analysis from GHC's deep FV annotations.
  It retains type-only dependencies and removes eligible unused lifted Many
  locals. It preserves remaining nodes and metadata and adds no occurrence
  counts. Both stages pass source19 and Showcase53. General occurrence
  analysis, native unfolding export and the full optimizer remain gated.
  M42 extends the checked alias and traversal rules to eligible function heads.
  Direct CorePrep checks consume call proofs and preserve argument demand.
  Top-level Tidy protection is implemented and reviewed. It has no separate
  isolated execution probe.
  M43 adds a typed source carrier and preserves native index identities through
  `seq` desugaring. Its `Strict` and opaque-head demand controls pass both stages.
  M44 extends the source classifier and structural annotation matcher. It adds
  no Core or backend transformation. Ordinary optimization remains gated.
  M45 adds assigned-Many signed source beta using the existing scalar bridge.
  It preserves the checked runtime expression and ordinary Strict fallback.
  M46 implements complete local keepCall/idCall signatures with a separate
  binder policy and exact local variable function operands. Scalar TermVarTy,
  constructor fields and exported call codecs keep their gates. Bootstrap
  snapshot6 builds, source/API catalogs, constructor supplements, legacy8 and
  final9 pass. Canonical audit/publication records are separate. Direct
  in-case probes2/3 reject
  payloads with GHC-83865 in both stages. M47 remembers the motive-to-call
  equality as an ordinary evidence parameter. Existing constructor transport
  and Wanted/Given solving support direct calls, including row `where`
  bindings. Bootstrap, self-host, API and selected both-stage source/runtime
  controls pass. Final9 passes its two API4 reuses and seven launches, including
  both-stage raw/public106 and packed562. The registered run gives five passes,
  39 cases and 34 skips, with no unexpected/framework failures or warnings. No custom App cast, solver rule,
  evaluator or codec is added. M48 adds a metadata-only activation policy for
  known lexical workers when earlier triggers are absent. Bootstrap fresh and
  freshwhere clients run without payload annotations. Snapshot2 API1 passes
  ten new quiet values with exact 1/37 goldens. Ordinary, quote and duplicate
  controls retain their measured behavior; independent wrong-head/index clients
  reject with located errors. Self-hosting, selected both-stage source controls
  and public17 pass. Legacy8, both-stage constructor11 and all nine final
  phases pass. Both stages preserve raw/public106 and packed562; registered
  results are 39 cases, 34 skips and five expected passes, with no unexpected/
  framework failures or warnings. Artifact audit and publication remain separate. This changes no Core, evidence,
  solver or backend rule. M48 is published at `3e6f9aa35f`, with canonical
  audit `953f58351b6ed84eaaccf0a93e957fe7fc7e291ac7bfafef0161cb3516d16aa8`.
  M49 extends the metadata query to resolved scalar constructor trees, using
  expected field domains and the existing scalar converter. Function operands
  stay bare local Vars. The selected source matrix and public21 pass both stages;
  legacy8, constructor11 and final9 pass. Both-stage raw/public106 and packed562
  pass. The registered run has 39 cases, 34 skips and five expected passes,
  with zero unexpected/framework failures or warnings. Artifact checks remain
  separate. M49 is published at `7029ed2a7d`, canonical audit
  `ba310681ec8effaf4c6a84bc4e4dfe26ca902ee0545a89cd0aefe5f069cf25a7`.
  M50 changes source package pattern checking and row desugaring, with no new
  Core, solver, codec or backend rule. C1 and C2 remain open.
- [ ] **Q1: Complete retained products.** Depends on C1; C2 gates general
  release. Broaden the documented static subset and supported domains. Extend application and definition checking beyond the leading invisible
  binders supported by M72–M74. Reuse inherited
  visibility and scoping machinery. State when annotations are required. Never
  infer a new retained quantifier merely because a definition could have one.
  Advances `R03` and `R02`.
- [ ] **Q2: Add first-class dependent sums.** Depends on C1 and a specified
  first-class existential design (`R22`). Reuse retained fields for storage,
  but specify first-class formation, introduction, elimination, scope, and
  erasure separately. Existing `PackVec` is the execution baseline. Keep erased
  `exists` and retained `some` distinct. Advances `R04` and `R02`.
  M50 admits the bounded multi-constructor package slice. Both stages pass
  public17, mixed17 and twin17 through actual interfaces; lazy and linear
  clients retain located compile-only rejections. Constructor-local scopes
  and row failure reuse existing field metadata and case lowering. The Sigma3
  control already passes the parent baseline. This does not implement general
  first-class sums, universes or dependent projections and closes none of R22,
  C1 or C2. Legacy/final evidence and artifact checks remain separate.
  M51 adds closed function-valued retained fields with generic call-indexed
  payloads. Source24 and mixed14 pass both stages through actual object
  interfaces. Matching retains actual function/input scope and lazy fields.
  Four logical body errors and two policy guards pass per stage. The maintained
  `docs/dependent-haskell/Showcase.hs` now passes both stages with51 lines and
  twelve actual source modules. M52 admits complete ordinary exported function
  Pi signatures. Public19 and the updated53-line showcase pass both stages.
  The source permission remains separate from class, instance, foreign,
  partial and hs-boot contexts. Native coercion encoding, unfolding handling
  and general optimization remain separate gates. The malformed ordinary-
  domain interface-knot gap remains open. C1, C2 and R22 stay unchecked.
- [ ] **E1: Specify and implement closed evaluation.** Requires an explicit
  compile-time execution boundary. M99 can use existing typed Core for a
  closed call that elaborates to a literal. A call remaining in Core needs
  a representation and checked evidence under C1.
  Select the smallest closed pure expression subset. Specify constraint,
  divergence, caching, staging, and error behavior. A closed computation such
  as list length must work in an actual type without a parallel handwritten
  type family. A runtime executable alone does not close this task.
  M77 and M78 use R13 source equations to compute Peano lengths across
  an interface. M99 executes an imported call during source elaboration
  and tracks the producer object in the client interface.
- [ ] **E2: Specify and implement open evaluation.** Depends on C1. Select and
  document normalization rules for expressions with rigid variables. Include
  stuck terms and capture avoidance. A lambda application to a rigid variable
  must normalize while an opaque function application remains well scoped.
  Closed evaluation alone does not close this task. Advances `R16`.
- [ ] **P1: Reuse term definitions in types.** Depends on E1, E2, and the
  selected requirements from `R17`–`R19`, `R21`, and `R23`. Integrate lambda,
  let, and case syntax with the Core rules. Interleave term/type declaration
  dependencies. Track matchability for unsaturated families. Demonstrate one
  source function used at both levels across a module interface. Advances
  `R13`; completing it does not finish all of `R06`.
- [ ] **P2: Complete promotion domains.** Coordinate `R07`, `R10`–`R12`,
  `R14`, and `R24`. Account for homogeneous equality, unboxed representations,
  overloaded literals, dictionaries, and data families. Reuse the completed
  Char/Natural work. Close each domain with its own boundary and evidence.
- [ ] **L1: Finish language preparation.** Coordinate accepted upstream work
  on `R21`, `R23`, `R27`, `R34`–`R36`, and `R41`, plus `R22` and `R24`.
  Reuse inherited parser, renamer, namespace, and binder implementations.
  Treat known issues on completed cards as individual follow-ups. This work can
  proceed beside Core and evaluation work.
- [ ] **I1: Integrate the full program.** Depends on the required work above.
  Check ordinary Haskell compatibility, normal optimization, interfaces,
  recompilation, tools, backends, errors, documentation, and compiler resources.
  Establish a broad GHC regression baseline. State the remaining research and
  implementation restrictions before a production-readiness claim.

M56 completes a bounded part of C1: closed parameterized first-order domains.
Ordinary type arguments now remain consistent across constructor indices,
substitution, branch evidence, Lint, lowering and separate interfaces. Both
stages check runtime `[Bool]`, `Maybe Nat` and `Tree Bool` source examples.
Closed families compute vector lengths and heterogeneous tree payloads.
Uniform recursive declarations qualify. General promotion, arbitrary term
computation in types, native unfoldings and normal optimization remain open.
This increment does not complete C1, C2 or R22.

M57 adds nominal domain conversion to retained congruence. Its body proof uses
one source-domain binder. Its right endpoint substitutes a backward-converted
target argument. Checked application uses that same actual argument. Scope,
substitution, role weakening, symmetry and distinct-domain composition pass
the Core controls. Seven bytecode observations and one native object control
check runtime behavior. The object control preserves native forms through
SimpleOpt and ignores a bottom token. This increment adds no source observation.
C1, C2 and R22 remain open.

M58 uses that nominal rule from source. A private body solve preserves ordinary
payload inference and keeps runtime indices local. Source wrappers retain the
nominal proof through zonking. Both compilers run the 68-observation Showcase,
including nine new conversion and demand observations. Three located rejection
controls prevent unrelated domains, mismatched payloads and local-index escape.
The older parameterized-domain rejection controls remain in force.
This is progress within R05/C1. It completes none of C1, C2 or R22.
M59 addresses fixed function-domain body comparison. Opaque Pi instantiation
and domain projection remain separate obligations. Representational Pi equality
alone cannot supply nominal domain equality.

## Vector split and package checkpoints

The vector split now exercises dependent products, dependent sums, Core,
and open evaluation together. The empty and cons equations for ordinary
append pass through separate compilation. The split returns a
reconstruction proof for an arbitrary cut within the vector.
[Split evidence](roadmap/R04-dependent-sums.md#prove-a-split-preserves-the-original-vector)
and [append equations](roadmap/R16-open-term-evaluator.md#prove-that-empty-vector-append-preserves-its-right-operand).

1. **R03 and R05.** The expected constructor result can now identify
   two original vectors and both element types after one bounds check.
   The expected vector domains of the returned pieces also determine the
   suffix. The paired implementation needs no visible type application.
   An index that appears only under a noninjective type family remains
   ambiguous. If a local Given equates two applications of an open
   family, GHC can use it without equating the arguments. A retained
   constructor that requires the arguments to agree reports the mismatch
   at that constructor's field.
   [Checked input inference](roadmap/R03-dependent-products.md)
   and [independent inputs](../../testsuite/tests/dependent/should_fail/NativeIrredIndexEvidence.hs).
2. **R05.** Package templates can now close over the inferred types and
   dictionaries of a recursive group. An ordinary client can inline a
   selected worker while its peers remain in scope. An inferred monomorphic group can use a
   direct alias to export a checked worker unfolding. The interface keeps
   the loop breaker, and older readers can call the object code.
   [Retained unfolding checks](roadmap/R05-dependent-core.md#export-retained-unfoldings-at-ordinary-signatures).

### Related checks

1. **R05.** A rule can match a retained head from another module.
   The same checked type application supplies the domains of `m`
   and `x` in `keepReading @Bool m x`. An ordinary value can precede
   the type argument. A retained prefix can also precede a specified
   erased binder and a second retained argument, as in
   `keepTagged tag @Bool m x`. The corresponding required binder
   takes `(type Bool)`. A rewrite rule can infer the types of the later
   runtime arguments from that prefix.
   [Rule and interface support](roadmap/R05-dependent-core.md).
2. **R23 and R24.** A recursive family can forward a constructor
   or a partial family across stripped interfaces. When a signature
   contains one eligible written application, GHC locates its arity
   error there and suggests the extension. In a complete signature,
   the body can determine a nested callback arrow's matchability. An
   unconstrained callback arrow still takes its ordinary default.
   R24 checks the parent class when a separate client writes a fully applied
   associated family in a signature, constructor field, or visible type argument.
   [Domain example](roadmap/R24-constrained-type-families.md),
   [inferred callback arrows](roadmap/R23-unsaturated-type-families.md#inferring-a-callback-arrow-in-a-closed-family),
   [later argument](roadmap/R23-unsaturated-type-families.md#choosing-a-prefixs-matchability-from-a-later-argument)
   and [singletons migration](roadmap/R23-unsaturated-type-families.md#arrow-patterns-in-family-equations).
3. **R22 and R04.** An ordinary module can put an explicit existential
   package inside an implicit one and open both in a pattern. A direct call
   can infer several of a reader's erased parameters from a named mixed
   package, a call with a declared mixed result, or a polymorphic call
   that forwards a supplied package type directly to its result. An
   independent higher-rank callback can be another argument to that
   call. A selector can use several operands with the same closed
   package type. A binder kind can determine an earlier reader parameter. The
   list-to-vector producer carries a computed witness. A reader can open
   the result of a `case` on three packages. With `ImpredicativeTypes`,
   `toList (id xs)` also opens the forwarded package around the reader.
   [Mixed package readers](roadmap/R22-existential-quantification.md#a-package-with-nested-dot-binders),
   [Retained list pairs](roadmap/R04-dependent-sums.md),
   [Guarded witness](roadmap/R22-existential-quantification.md#inferring-a-higher-rank-witness),
   [local package pattern](roadmap/R22-existential-quantification.md#a-nonrecursive-local-package-pattern),
   and [nested patterns](roadmap/R22-existential-quantification.md#constructor-cases-on-the-payload).
4. **R19.** A branch on an ordinary Boolean can pass that same value to
   a retained field. Matching on an expression such as `not b` in
   the field selects the result type using the function in scope.
   Template Haskell can quote and reify the retained binder in a signature
   or a prefix GADT constructor. A type quotation preserves a case on an
   enclosing retained binder as `CaseT`.
   [Retained scrutinees](roadmap/R19-type-level-cases.md#compute-a-retained-scrutinee)
   and [constructor quotations](roadmap/R19-type-level-cases.md#quote-and-reify-retained-gadt-constructors).
5. **R12 and R16.** An ordinary client can prove that `Prelude.reverse`
   preserves length. A proof using `Prelude.foldr` establishes the append
   law for an arbitrary step and seed. It uses the method selected by the
   installed list dictionary. A code-generating producer can record that
   choice even when its interface omits optimizer pragmas. The installed
   `Functor Maybe` dictionary supplies both `Prelude.fmap` equations.
   A second call can consume the first call's result with both callbacks
   still abstract. A local dictionary leaves the method choice open.
   [Installed method proof](examples/open-evaluation/PreludeMappingProof.hs)
   and [named worker proof](examples/open-evaluation/MappingProof.hs).
   [Public fold proof](examples/open-evaluation/PreludeFoldrProof.hs).
   [Append and map proofs](roadmap/R16-open-term-evaluator.md#prove-that-map-distributes-over-append).

6. **R21.** A retained field's worker can use a saturated local synonym
   such as `WorkerArgument Bool` in its signature. A worker in a later
   splice group gives GHC-76037 at the function name. A worker that needs
   the unfinished constructor still gives GHC-68947.
   [Declaration groups and aliases](roadmap/R21-dependency-analysis.md#local-synonyms-in-a-retained-field-worker).

## Showcase history through 502 outputs

The 502-output Showcase proves a rectangular table's contents after the
producer discards its row labels. The evaluator recovers the row count
from the outer vector. Column labels still establish the width, even
for an empty table. [Table example and limits](roadmap/R15-closed-term-evaluator.md#read-the-stored-cells-of-a-rectangular-table).

A library can declare an open row schema. Another module can supply an
instance whose type lambda captures its error type. A client can use
both the optional schema and the error schema from compiled interfaces.
The two instances appear in the 501-output Showcase.
[Schema library](examples/open-evaluation/TypeLambda.hs) and
[separate instance](examples/open-evaluation/SchemaColumns.hs).

Proposal #765 adds pun warnings. With `-Wpun-bindings`, `data T = T`
reports both declaration locations. The warning also covers explicit
type and term binders with the same name in one function or lambda
match. A binder can still shadow a name from an enclosing scope.
Implicit local type binders and use-site pun warnings remain open.
[Accepted proposal and review](https://github.com/ghc-proposals/ghc-proposals/pull/765)
and [binding examples](roadmap/R36-pun-warnings.md).

A separately compiled producer can return a rectangular table whose
type hides its dimensions. The client can prove every stored cell at
compile time, including the shape of empty tables. A recursive function
over a heterogeneous row can now put its type lambda directly in the
value signature. The 500-output Showcase includes both programs.
[Table proof](roadmap/R15-closed-term-evaluator.md#read-the-stored-cells-of-a-rectangular-table)
and [row function](roadmap/R17-type-level-lambdas.md#mapping-a-row-in-a-value-signature).

A typed column reader can accept a visible Boolean followed by an
invisible type witness. The skip branch returns `Nothing` even when
both the witness and the column are undefined. A type-level let can
now use the ordinary lambda spelling `let wrap = \a -> Either e a`.
Both examples are part of the 498-output Showcase.
[Column reader](examples/qualified-pi/RuntimeTypeColumn.hs),
[type-level lambda](roadmap/R17-type-level-lambdas.md) and
[verification](CHECKPOINTS.md#family-and-package-checkpoints).

A type-level `let` can name a callback and pass it to `Map` after
an inner `let` shadows a captured type name. An ordinary client with
`ImpredicativeTypes` can return an existentially packaged callback
without naming its polymorphic witness. All 497 Showcase outputs
matched at native `-O0`, native `-O2` and in source GHCi.
[Scoped row](examples/open-evaluation/TypeLambda.hs) and
[verification](CHECKPOINTS.md#family-and-package-checkpoints).

Type-level lets can now appear in Template Haskell type quotations.
A GADT constructor can also place a `Typeable` constraint after a
required type argument that follows an ordinary field. The 495-output
checkpoint includes both forms. [Verification](CHECKPOINTS.md#family-and-package-checkpoints).

A closed type family can pass `\a -> Either e a` to `Map`. Its
equation binds `e` in the pattern `'Just e`. A client can use the
result through the producer's compiled interface. All 493 Showcase
outputs at this revision agree at native O0 and O2 and in GHCi.
[Closed family example](examples/open-evaluation/TypeLambda.hs)
and [remaining work by area](ROADMAP.md#remaining-work-by-roadmap-area).

An ordinary client can supply `@Bool` and `@Char` between the value
arguments of one constructor. Another client can inline a recursive
function that hides its job payload type and uses an inferred
`Num` dictionary. Both compile against the provider's interface.
[Ordered fields](examples/open-evaluation/LabelledValueClient.hs),
[recursive jobs](examples/qualified-pi/RecursiveJobs.hs), and
[combined verification](CHECKPOINTS.md#family-and-package-checkpoints).

The roadmap's bounded list index works from source. Its type is
`foreach (xs :: [a]) -> Fin (sourceLength xs) -> a`, where
`sourceLength` is the ordinary list function. In a separately compiled
ordinary module, GHC infers the local list argument in
`checked xs n = fmap (indexGeneric xs) (mkFinGeneric xs n)`.
`checked "abc" 1` returns `Just 'b'`. A negative position returns
`Nothing` before inspecting the list. A written `FZ` at the empty list
fails at compile time. A finite position can select from a cyclic list.
[Implementation](examples/qualified-pi/ParameterizedIndexLibrary.hs)
and [ordinary caller](examples/qualified-pi/FinitePositionClient.hs).

The bounded list index now has the two cons equations in Serokell's
example. The empty list gives a position of type `Fin 'Z`. GHC checks
that this type has no constructor and inserts an empty case on the
position. [Equations and coverage](roadmap/R03-dependent-products.md#omit-an-impossible-list-index-equation).

The same two list equations also accept a linear position. A library
stores the list together with its checked `Fin` in a dependent pair.
It can hide that pair behind a linear reader. An ordinary client opens
the package and passes the position to the reader once. A direct call to
`fmap (indexGeneric xs)` infers an unrestricted callback.
[Linear cursor](examples/qualified-pi/ListCursor.hs)
and [ordinary client](examples/qualified-pi/CursorClient.hs).

A bounded lookup can consume both its position and a callback linearly.
Each cons equation calls or forwards the callback once. Grouped ordinary
patterns retain their original demand under `Strict`. A repeated constructor
in a linear package can also bind a predecessor used by a later field.
[Lookup with a continuation](examples/qualified-pi/ListCursor.hs)
and [pattern evidence](roadmap/R04-dependent-sums.md#repeated-retained-constructors-in-linear-packages).

Two lists of different element types can share a checked position.
An ordinary client asks for both elements at a signed offset. The
provider checks the offset on the first list and compares the list
lengths. The equality from that comparison justifies the second
lookup. A length mismatch returns `Nothing`.
[Paired lookup and coverage](roadmap/R03-dependent-products.md#use-one-position-on-two-lists-of-equal-length).

A library can also store one position for each of two independent lists.
Its linear reader matches both positions directly. The lists can have
different lengths and element types. A finite offset works on a cyclic
list. An ordinary vector client can pass its filtered result to a
rank-2 callback and infer the hidden length of the second filter.
[Two positions](examples/qualified-pi/ListCursor.hs) and
[ordinary filter client](examples/erased-existentials/RankedPackageClient.hs).

An ordinary caller can write `zipWithExact [x, y] [u, v] (,)`. GHC
checks both lengths at compile time and infers both element types.
The same caller can write `toList (zipWithExact [1, 2] [10, 20] (+))`
at type `[Int]` and obtain `[11,22]`. The proof uses the two list
constructors on each side. The elements keep their ordinary numeric
meaning and stay lazy until the result demands them.
[Written list example](roadmap/R03-dependent-products.md#zip-two-lists-at-a-common-length).

An ordinary module can also give a vector append the result length
`Length (2 N.+ 1)`. Appending two copies of `'a'` and one `'b'` yields
`"aab"`. GHC rejects a result annotation of length four and a call to
`headVec` on a vector of length zero.
[Ordinary append client](examples/packed-vector/AppendLengthClient.hs).

The open evaluator checks equations about the installed `Prelude.span`.
When the first element fails the predicate, its suffix is the entire
input, even with an unknown tail. A proof by induction can rejoin the
two projected lists when the caller supplies the predicate decisions.
The proof module disables rewrite rules when it uses these equations.
[Source proof](examples/open-evaluation/PreludeSpanProof.hs) and
[R16 scope](roadmap/R16-open-term-evaluator.md).

A column can store a `TypeRep a` beside values of hidden element type
`a`. A caller supplies the type it wants and a signed offset. A type
comparison refines the element type. A bounded position permits the
lookup. A type mismatch leaves the offset and values unevaluated.
[Runtime type and position](examples/qualified-pi/RuntimeTypeColumn.hs).

A producer can zip two ordinary lists into a vector whose length is
the length of the first input. Its signature requires equal input
lengths. For two independent inputs, a runtime comparison supplies
that equality. Passing the same list twice supplies it by reflexivity
and allows the first result cell to be read without inspecting the tail.
[Source and coverage](roadmap/R03-dependent-products.md#zip-two-lists-at-a-common-length).

An ordinary client can filter two vectors independently, pass them to
a higher-rank callback, and infer the package returned by the callback.
It can read the first result from either vector while the other input is
undefined. The second filter can change the two lengths independently.
[Two-vector example](examples/erased-existentials/RankedPackageClient.hs)
and [inference and escape checks](roadmap/R22-existential-quantification.md).

An ordinary client can request `Show (Rest c)` alongside `Collection c e`.
The collection constraint supplies the domain of its associated type.
A library can export the same pair of constraints as a synonym.
[Showing the rest of a collection](roadmap/R24-constrained-type-families.md#family-applications-in-a-signature-constraint).

A type synonym can use a `case` to choose the argument type of an
ordinary arrow. For example, one branch can take a pair and another
a `Char`. An ordinary client sees the selected argument type through
the provider's interface. A case in the domain also works under a
multiplicity annotation. One identity can be called at `One` and
`Many`. The ordinary linearity checks still apply to each call.
[R19 example and limits](roadmap/R19-type-level-cases.md).

An application pattern `f x` in a type family equation or class instance
requires a matchable `f`. GHC checks the declared arrow modifier, even
when another pattern determines `f` and `x`. [R23 pattern checks](roadmap/R23-unsaturated-type-families.md).

An opted-in library can declare an associated type whose use requires
the parent class. An ordinary client can prove `Elem [Bool] :~: Bool`
when `Allowed Bool` supplies the instance. Writing `Elem [Int] :~: Int`
in the same client reports the missing `Allowed Int` at compile time,
even though the family equation could reduce to `Int`. A GADT pattern
can also supply the class dictionary locally. The Showcase opens a field
with evidence for its runtime flag. An ordinary client supplies a polymorphic
formatter that works for any field with that evidence.
[Ordinary client](examples/open-evaluation/ConstrainedElementsClient.hs)
and [R24 scope](roadmap/R24-constrained-type-families.md).

An opted-in provider can pass the type family `Map` itself to a GADT.
The GADT uses it once with the matchable constructor `Maybe` and once
with an unmatchable type family. An ordinary client uses both results
through the provider's interface. An explicit equation binder can also
carry the polymorphic kind into the right-hand side of a type family.
A declaration can compute its callback's matchability with a family such
as `Flip m`. A higher-order family can also take an unmatchable family
whose first argument is a required type. The ordinary client invokes it
at two kinds and obtains `(3,LT)`.
[R23 examples and limits](roadmap/R23-unsaturated-type-families.md#passing-a-polymorphic-mapper-at-its-full-kind).

An ordinary module can open an erased state and its linear reader in a
strict tuple binding. A local `INLINE` function can use a retained
call with a captured element. The Showcase reads both an encoded Boolean
and an encoded integer, and replicates a captured element at two runtime
lengths. [Release and interface checks](CHECKPOINTS.md#family-and-package-checkpoints).

An ordinary module can check a signed offset once against a vector.
On success, `mkFinVec` returns a position of type `Fin n`. The caller
can use that position on any vector of length `n`. A negative offset
returns `Nothing` without inspecting the vector. A match on `FS p` in
a retained function identifies the actual predecessor `p`. Its proof
leaves that predecessor unevaluated. The same function accepts written
`FZ` and `FS p` as retained arguments. A constructor result can also
determine multiple erased parameters.
[Written constructors](roadmap/R03-dependent-products.md#write-indexed-constructors-in-retained-applications)
and [signed lookup and predecessor proof](roadmap/R03-dependent-products.md#retain-a-bounded-position-and-its-actual-predecessor).

An ordinary caller can split a vector at a runtime cut and obtain a
proof that appending the two pieces gives back its original input.
`splitAtValue` returns two retained vectors and a homogeneous equality.
For an independently read cut and total, `checkedSplitAtValue` returns
`Nothing` if the cut exceeds the total. On success, checking the suffix
length leaves the vector unevaluated. Opening the contents proof takes
the recursive path down the input spine. The expected result and the
length equality now select the original vector at the constructor call.
The entire proof fits in the zero and successor equations. The successor
branch matches the vector and uses the recursive equality directly.
[Split theorem](examples/packed-vector/CertifiedSplit.hs) and
[ordinary caller](examples/packed-vector/CertifiedSplitClient.hs).

One bounds check can also split two different vectors. Each proof names
its original input. The vectors can contain different element types.
Both calls are written as `splitAtValue cut xs` or
`splitAtValue cut ys`. The expected types of the pieces determine the
common suffix. Inspecting the first proof leaves the second vector
unevaluated. Exchanging the proofs reports the two written `SplitAtValue`
types at the exchanged arguments.

The Showcase includes ordinary clients of the `INLINE` and `INLINABLE`
finite zero views. It also includes finite-list proofs for the installed
Prelude definitions of `map`, `(.)`, `filter`, `takeWhile`, `dropWhile`,
and `(++)`. One proof recovers the original finite list by appending its
prefix and suffix. Another ordinary client opens an erased package in a
local tuple binding. It can use both fields while the witness stays local.
The source-hidden
split regression passes at `-O0`, `-O1`, and `-O2`. Removing the induction
premise fails at the final `Refl`. A normal two-stage Hadrian build also
passed on the 378-line paired-split revision.
[Verification scope](CHECKPOINTS.md#family-and-package-checkpoints).

A retained function can name an entire indexed vector as its argument.
`foreach (xs :: Vec a n) -> xs :~: xs` returns `Refl` even for an
undefined vector. Using two different vector binders in the same equality
fails at `Refl`. A type synonym can also name a dependent consumer. An
ordinary module uses the exported synonym to pass an indexed payload.
A match on `VNil` now refines the value `xs` and its length. The
branch can return a heterogeneous equality between `xs` and `VNil`.
Matching `VCons x rest` refines the input to its actual head and tail.
The match forces the outer constructor. The fields stay lazy until the
caller inspects them.
[Dependent products](roadmap/R03-dependent-products.md) and
[named consumer](examples/packed-vector/DependentPair.hs).

`splitAtVec` takes a runtime cut `n` and a vector of length `Add n m`.
It returns two vectors of lengths `n` and `m`. An expected pair of
vectors determines the suffix length. So does the result of `dropVec`.
Use `takeVec @m` when only the prefix occurs in the result.

For a cut read independently of the vector, `splitAtVecChecked cut total xs`
returns `Nothing` when the cut exceeds the length. A successful split
packages a suffix length, the two vectors, and a proof of
`Add cut suffix :~: total`. An ordinary caller can match on `Refl` and
rejoin the vectors at the original length. Checking the bound only inspects
the lengths. [Checked split](examples/packed-vector/PackedVecLibrary.hs)
and [ordinary caller](examples/packed-vector/ListVectorClient.hs).

The Showcase also
checks the contents of a concrete split. Appending the two parts of
`[True, False]` gives that same vector. Swapping the elements makes the
equality proof fail at `Refl`. The ordinary polymorphic `appendVec` can itself appear in a type.
GHC infers the input lengths of two written vectors, reduces their sum,
and checks the exact output elements. An ordinary client can also use
`appendEmptyAt xs` to prove that appending an empty vector preserves
its runtime input. The proof does not inspect `xs`. The recursive
equation also applies to a runtime head, left tail, and right vector.
The successor branch of `splitAtValue` uses this equation and the
recursive proof to reconstruct an arbitrary input after splitting.
[Split and prior art](REFERENCES.md#split-a-vector-at-a-retained-length) and
[contents proof](../../testsuite/tests/dependent/should_run/NativeClosedIndexedVec.hs).

A case on an ordinary comparator can return a witness for `p x` in all
three `Ordering` branches. A separately compiled ordinary module uses
the witness to choose an `Int`, `Bool`, or `Char` payload. It leaves the
other two payloads unevaluated. [Provider](examples/open-evaluation/RuntimeOrdering.hs)
and [ordinary client](examples/open-evaluation/OrderingPayload.hs).

An ordinary client can partition a vector with a runtime predicate and
return the two parts appended at the original input length. The producer
returns two nested `some` packages and proves that their lengths add up to
the input length. Returning just the kept vector at that length fails at
the returned expression. A caller can also choose either partition at
runtime and pass its length and vector to the same consumer. [Partition proof](examples/packed-vector/ConservedPartition.hs),
[ordinary caller](examples/packed-vector/ListVectorClient.hs) and
[verification](CHECKPOINTS.md#family-and-package-checkpoints).

A function can read a Boolean from an `IORef` and use it as the index
of an ordered type case in a value signature. The sample renders the
two results as `"7"` and `"'f'"`. A call that ignores its arguments
still returns 42 with an undefined index and payload. An ordinary client
can also open a hidden vector inside a pair without forcing the other
field. In a GADT field example, each branch passes the original runtime
Boolean to the field. Its result has type `Int` at `True` and `Char` at
`False`.
[Showcase](Showcase.hs) and [verification](CHECKPOINTS.md#family-and-package-checkpoints).

A rewrite rule can omit the types of its Boolean index and dependent
payload. GHC infers both from the function's retained signature. An
ordinary client can use the optimized rule. The rule fires even when the index comes from
an `IORef`. A call whose result ignores the index can return 19 with an
undefined Boolean. GHC checks type cases in ordinary and rank-n
constructor fields. The same higher-order class method can map over
values indexed by `Maybe` or by a partially applied type family.
[Rewrite rules](examples/qualified-pi/RetainedRules.hs),
[field cases](examples/open-evaluation/NativeNestedCase.hs) and
[class method client](examples/open-evaluation/IndexedMapClient.hs).

The module that defines the Peano list `size` can keep dependent syntax
off. Its `-fwrite-source-equations` option publishes the checked
recursive equations, including when compiling without object code.
The same source function runs at runtime. Separate ordinary proof
modules use its interface to reason about installed `map` and `filter`.
[Size definition](examples/open-evaluation/PreludeMapLengthSize.hs)
and [ordinary proof module](examples/open-evaluation/PreludeMapLengthProof.hs).

An ordinary caller can package a list's computed length with the vector
indexed by that length. A separate proof module uses the checked
equations for Prelude `(++)` to prove that append adds lengths and that
`map` distributes over append for finite lists. Its tests also check
that the right list and the elements remain lazy where promised.
[List-to-vector example](roadmap/R04-dependent-sums.md)
and [append proofs](roadmap/R16-open-term-evaluator.md#prove-that-map-distributes-over-append).

GHC checks that filtering a finite list cannot increase its `size`.
The list and the cutoff captured by its predicate can both come from
`IO`. A case on the predicate result supplies the evidence for the
checked `Prelude.filter` equation at that element. The proof takes an
explicit predicate argument that GHC erases. A decider for a different
predicate fails to typecheck. Observing the complete proof requires a
defined decision at each element.
[Filter proof](examples/open-evaluation/PreludeFilterLengthProof.hs),
[runtime cutoff](examples/open-evaluation/PreludeFilterCutoff.hs) and
[verification](CHECKPOINTS.md#family-and-package-checkpoints).

A function can repeat the same type-level case in its argument and result.
For `caseIdentity`, the result type is `Int` at `True` and `Bool` at
`False`. The body simply returns its argument even while the index is
unknown. An ordinary client reads the signature through the provider
interface and obtains `(42,False)`. A second example checks that an outer
index under a type family stays distinct from a shadowing pattern binder.
A staged interpreter can use the arrow kind supplied by its caller to
produce `(Just 3,Right 3)`.
[Value signature](examples/open-evaluation/NativeCase.hs),
[scoped indices](examples/open-evaluation/NativeNestedCase.hs) and
[staged interpreter](examples/open-evaluation/InterpretedKindUseSite.hs).

An ordinary module proves that `Prelude.map` preserves the length of
any finite list spine for any callback. The proof counts constructors
with the same Haskell `size` function that the program calls at runtime.
The empty case is immediate. The cons case uses the installed map equation
and the induction hypothesis for the tail. A claim that mapping adds one
element fails at the empty case. Omitting the induction hypothesis leaves
the tail equality unsolved. The runtime example counts two elements
without evaluating the callback.
[Proof](examples/open-evaluation/PreludeMapLengthProof.hs)
and [assumptions and checks](roadmap/R16-open-term-evaluator.md#prove-that-map-preserves-length).

A GADT field can package three vectors with two runtime lengths. Two
vectors share the outer length and can be zipped directly. The third
uses the inner length. The sample reads lengths 2 and 1 and the first
aligned pair `('h','s')` without evaluating either remaining tail or
the third vector. Giving either length the wrong vector is a type error
in the client that constructs the package.
[Source](examples/packed-vector/StoredVector.hs)
and [constructor and interface checks](roadmap/R04-dependent-sums.md#sums-in-constructor-fields).

A vector partition can store its two result lengths in one erased package.
The producer writes `Rows yes no`. An ordinary function equation can
inspect the passing vector with `firstPassing (Rows (VCons x _) _) = Just x`.
The match leaves both unused tails lazy. With `ImpredicativeTypes`, a
client can use `map rowLists` over partitions with different hidden lengths.
Quick Look can infer a complete erased package as a type argument. It
still checks runtime dependencies under the package binder.
[Partition equations](examples/erased-existentials/ImplicitVector.hs)
and [ordinary list client](examples/erased-existentials/PartitionChoices.hs).

A function with a proof of `a :~: b` can change a filtered vector's
element type from `a` to `b` under the same hidden length. A type-level
case can also use an equality for an earlier constructor index when
checking a later argument whose kind refers to that index. When a family
cannot reduce, the case still waits for more information.
[Type-level case](examples/open-evaluation/NativeNestedCase.hs).

An existential witness can itself be polymorphic. A producer can write
`(id, atTwoTypes)` for `exists t. (t, t -> (Int, Bool))`. The second field
determines that the first has type `forall a. a -> a`. A consumer can write
`observePair (f, k) = k f`. If it tries to return `f` at an arbitrary type,
the error identifies both the use and the pattern that opened the package.
A function can also hide the witness shared by all its calls. The consumer
can write `case builder @Int n of Callback f call -> call f` and use `f` at
both `Int` and `Bool` inside the case.
[Callback example](examples/erased-existentials/RankedPackage.hs).

A library can export optimizer rules that construct or open an erased
package. For example, a rule for `readHidden (x, k) = k x` with
`x :: Int` fires for an integer package. A package that holds a `Bool`
keeps its own callback. Both the provider and a client compiled from
its interface fire all four example rules at default `-O1` and `-O2`.
[Rules and provider](examples/erased-existentials/LexicalPackageRules.hs).

A stream can hide its state in an ordinary tuple with its step function.
The same consumer reads a list-backed stream and an integer stream that
skips even values. A one-element prefix leaves the rest of the input list
unevaluated. [Stream example](examples/erased-existentials/ErasedStream.hs).

A kind family can select a matchable arrow for `Maybe` and an unmatchable
arrow for a type family. Both results apply to `Int`. Before this change,
the fork rejected the second application. An ordinary client now prints
`(Just 7,[7])` from the producer interface. [Computed arrow kinds](examples/open-evaluation/InvisibleFamily.hs).

An interpreter can define arrow kinds before a declaration splice and
apply the interpreted type constructor after it. Interpreting two
applications of `Maybe` gives `Just (Just True)`. A class can also give
one associated type default, `type Callback a = a -> Maybe a`. An
ordinary client uses the default as both the kind of the promoted
constructor `'Just` and the type of a runtime callback.
[Interpreter](examples/open-evaluation/InterpretedKind.hs) and
[callback client](examples/open-evaluation/LegacyKindArgument.hs).

A second interpreter can apply `Either` to two argument types and can
also describe the type of `(&&)`. The choice of arrow in a family's
result can differ from the choice in its kind. The client prints
`(Right 7,False)` and leaves the second Boolean unevaluated. The
original `GADTSingletons` program compiles with a matchable arrow on
`(~>)` and a separate matchable `Promote` equation. Its bare equation
continues to handle ordinary function types.
[Interpreter and compatibility boundary](roadmap/R23-unsaturated-type-families.md#compatibility-with-existing-haskell).

The list interface of a stream with hidden state supports two ordinary
`foldr` consumers. With a separately compiled provider, both consume the
producer's builder at `-O1` and `-O2`. The first-element fold returns
`Just True` for a stream with an undefined tail. The integer fold reads
the odd digits in order and gives 531. The focused checks also run the
same source at `-O0`.
[Source folds](examples/erased-existentials/StreamFold.hs) and
[Core and recompilation checks](roadmap/R05-dependent-core.md#a-list-fold-over-a-hidden-stream-state).

A family argument can keep its last invisible binder while GHC infers
the earlier ones. For `RunVisible @Bool (ReadAt 'True)`, GHC infers
the result kind `Bool` and leaves the input type for `RunVisible`.
The same family produces `Int` when the receiver requests kind `Type`.
[Source and ordinary client](roadmap/R23-unsaturated-type-families.md#inferring-a-kind-after-supplying-a-visible-argument).

An ordinary client can prove two equations about the installed
`Prelude.filter Prelude.not`. A `False` head survives. A `True` head
is skipped. Both proofs leave the list tail unknown. The client reads
only the producer interface and disables rewrite rules in the module
that proves the equations. Keeping rewrite rules enabled reports
GHC-83605 at the proof. A runtime check reads the kept head with an
undefined tail. The Showcase checks the two proofs and this
demand behavior.
[Proof source](examples/open-evaluation/PreludeFilterProof.hs),
[filter alias](examples/open-evaluation/PreludeFilterSource.hs)
and [Showcase](Showcase.hs).

The upgrade tests cover a change to a family, a packed projection from
a home or installed module, and combinations of dependent projections
and ordered type cases. They check a first rebuild, reuse on a second
build, and rejection of stale evidence. The old and new compilers use
the same target libraries for each comparison. At three interface
compression levels the old reader rejects new constructor evidence at
the header. The new reader checks the coercion's role and scope.
For a provider that exports both an ordered case and a list filter, either
older compiler rebuilds that provider exactly once. A provider that needs only
the check its compiler already performed stays up to date.
[R05 interface checks](roadmap/R05-dependent-core.md#default-optimized-interfaces)
and [two-way filter and case upgrade](examples/open-evaluation/check-filter-scope-upgrade.sh).
