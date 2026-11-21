# R03: Dependent products

Upstream: [Dependent products](https://ghc.serokell.io/dh#dependent-products).
Local status: partial implementation. The source subset is restricted.
Use the [work queue](../ROADMAP.md#current-work-queue) for priorities.

## Goal and dependencies

A retained function's result type can refer to its runtime argument. Support
source formation, checking, application and conversion with explicit scope,
visibility and retention rules. The proposed visible spelling is
`foreach (n :: Nat) -> Vec a n`.

The upstream graph relates R03 to R05 and the completed visible-forall card
R39. Each local increment needs the relevant Core rule. It need not wait for
all Core transformations. Normal optimization remains a broader release gate.

## Supported subset and evidence

A type synonym can name a dependent function over a lifted parameter.
The ordinary caller can use `Consumer a p r` in a signature and supply a
witness and its indexed payload. The Showcase instantiates the witness
type with a vector whose length comes from IO. An abstract family such as
`Hidden a` still leaves `a` ambiguous when the rest of the signature
cannot determine it. The result of a named callback passed to `withPair` can determine the
result of the call when the two witness domains agree nominally. A result
that would contain the callback's local witness fails at that callback.
[Consumer](../examples/packed-vector/DependentPair.hs) and
[formation rule](../REFERENCES.md#naming-a-dependent-function-type).

For `splitAtVec n xs`, the result type `(Vec a n, Vec a m)` supplies
the erased suffix `m`. Both lengths can come from IO. GHC still checks
the cut against the expected first length and the input against
`Vec a (Add n m)`. A suffix hidden under a non-injective family remains
ambiguous. [Split example and prior art](../REFERENCES.md#split-a-vector-at-a-retained-length).

A constructor equation for the cut can pass the original input vector
at its refined width. The zero branch can use width `m` and the successor
branch can use `'S (Add k m)`. Both branches preserve the vector's
identity in the result and can leave the entire vector unevaluated.
An ordinary caller can also pass `S (S Z)` directly and inspect that
identity witness while the input vector is undefined.
[Showcase](../Showcase.hs), [ordinary caller](../../../testsuite/tests/dependent/should_run/NativeRetainedCastDomainClient.hs)
and [derivation](../REFERENCES.md#passing-the-split-input-at-a-refined-width).

A retained length can be a checked source call such as `plus n m`.
Its two inputs can come from IO. The result index remains the same
through compilation, and the runtime sum stays lazy.
[Computed retained length](../PLAN.md#m81-a-computed-retained-length).

A retained Boolean witness can also use `Prelude.not x` on a head
from an IO-supplied list. The witness provides the branch equality
needed to prove the filter size bound. An ordinary client can open the
packaged bound. [Runtime filter proof](R16-open-term-evaluator.md#prove-that-filter-never-increases-length).

An imported higher-order contract can take a decider with a retained
head and a result indexed by the chosen predicate. An ordinary module
can choose that decider at runtime. [Runtime filter decider](R16-open-term-evaluator.md#supply-a-runtime-filter-decider).

A package can retain a local predicate that captures an IO-supplied
cutoff. The proof index keeps that function's identity. [Captured cutoff](R16-open-term-evaluator.md#capture-a-runtime-cutoff-in-the-predicate).

- Runtime Bool and Peano Nat indices, checked equations and bounded lambdas.
- A retained binder can have a lifted type parameter as its domain.
  The `withPair` callback can name that argument as the index of its
  payload. A function-valued witness can pass through this callback
  without being evaluated. [Example](../examples/packed-vector/DependentPair.hs).
- Opaque binders at saturated lifted data types, including indexed GADTs.
  `foreach (xs :: Vec a n) -> xs :~: xs` can return `Refl` without inspecting
  `xs`. An ordinary client can use that proof through an interface.
  Structural retained matching still requires a supported constructor schema.
  See [Agda's dependent function types](https://agda.readthedocs.io/en/v2.8.0/language/function-types.html)
  and the `NativeRetainedPackage` source regression.
- A checked `case` on a named retained vector can refine its identity in a
  `VNil` branch. The branch uses GHC's ordinary length equality to cast the
  promoted constructor to the scrutinee kind. The case still demands a
  vector of known length zero. The initial source rule covers nullary
  constructors with one closed index refinement. See the `NativeRetainedPackage`
  source regression.
- Closed parameterized domains such as `[Bool]`, `Maybe Nat` and `Tree Bool`.
- Retained workers and function fields through separate interfaces.
- Complete value signatures can interleave a visible retained binder and an
  invisible erased forall. A single equation can bind the later erased argument
  with `@a` and continue with more retained arguments. A binder kind can refer
  to an earlier retained index. The separate `NativeAlternatingBinders` client
  checks written type applications, runtime inputs, and lazy ignored indices.
  The immediate retained operand can determine a complete erased prefix
  from its declared type. A strict prefix in a flat equation remains a
  separate case.
- Nominal scalar-domain conversion with scoped source body solving at M58.
- Fixed function-domain body comparison and payload selection at M59.
  Both stages pass the selected source, runtime and rejection controls.
  Its independent audit passes.
- Closed-list safe indexing at M60. Both stages pass bounds, lazy-field and
  finite cyclic-input checks through separately compiled interfaces.
- Complete retained-function type arguments at M62. Both stages pass nested
  container formation, worker demand, ambient indices and located partial-type
  rejection. Required type arguments also check holes combined after renaming.
  Its independent audit passes. The M62 Showcase has 88 output lines.
- Generic structural lists and runtime-index-bearing element domains at M67.
- Consecutive hidden indices in an imported append function at M74. Both
  vectors determine the result index. Non-injective family arguments require
  another determining argument or an explicit operand.
- Imported retained application contracts in ordinary modules at M68. Both
  rebuilt stages pass paired clients, scoped callback dictionaries and located
  rejections. The Showcase has 118 output lines through seventeen modules.
- Generic safe indexing at M69. Fin (GenericCount xs) selects ordinary,
  function-valued and runtime-index-bearing elements. Both stages pass reviewed
  bounds, demand and scope controls through interfaces. The original PackedVec
  callback keeps its literal list. Case-based ordinary clients pass. The NoEDT
  native let-list and literal `fmap toList (lookupGeneric [vector] 0)` probes
  reject. The named-list fmap variant is unmeasured.

The published M60 checkpoint appends five observations to M59's 79-line prefix
and passes all 84 with both compilers. Its separate evidence audit passes.
See [the current evidence](../ROADMAP.md#where-this-fork-stands) and
[the M59 specification](../PLAN.md#m59-bodies-beneath-fixed-function-domains).
M56's domain representation and M57's conversion rules belong primarily to R05.
M58 and M59 expose those rules through source checking.

## Next increments

- [x] Finish M59's final audit and prepare publication. Preserve the existing
      compiler and fixture identities when documentation changes.
- [x] Measure closed-list safe indexing:
      `indexClosed :: foreach (xs :: [Bool]) -> Fin (Count xs) -> Bool`.
      Use an ordinary `Fin` GADT and the existing saturated `Count` family.
      Check negative and out-of-range runtime requests, distinct list names,
      empty-case demand, lazy fields and finite access to cyclic input.
      The integrated library passes nineteen runtime observations with both
      stages. Both agree on five rejections and accept equal-count reuse.
      Fin proves a bound. It does not identify list values.
- [x] Finish M60's separate evidence audit and prepare publication.
- [x] Admit a complete retained-function type in the existing lifted
      consumer's explicit type-argument position. Preserve complete-type,
      kind, scope and native-meta checks. Check full worker WHNF separately
      from direct `seq` representation polymorphism and outer Quick Look.
- [x] Measure generic safe indexing at M69:
      `forall a. foreach (xs :: [a]) -> Fin (GenericCount xs) -> a`.
      Reuse Fin from ParameterizedIndexLibrary and GenericCount from
      PackedVecLibrary. Both stages pass function-valued and runtime-index-bearing
      elements, signed bounds, lazy fields and finite cyclic access. Keep the
      family saturated. Guarded declaration advice supports the original
      callback list literal. No new Core judgment or native meta fill is added.
- [x] Form an invisible retained source signature and check a named definition.
      Infer one retained operand from later declared domains. Check IORef input,
      lazy ignored operands and import through an object interface.
      See [M72](../PLAN.md#m72-invisible-retained-binders-in-source).
- [x] Add explicit invisible runtime operands. Check `f @Int @{n} xs`, a
      constructor operand, and a located error for a mismatched later vector.
      See [M73](../PLAN.md#m73-explicit-invisible-runtime-arguments).
- [x] Infer consecutive invisible operands in telescope order. Run vector
      append with both lengths omitted or with an explicit leading length.
      Recover in-scope constructor trees from declared indices.
      See [M74](../PLAN.md#m74-successive-invisible-retained-arguments).
- [x] Infer the erased tail index when `safeHead` consumes the result of
      `replicateNative (1 + n) x` or `sourceVector @a (x : xs)`. The first
      element is available without evaluating the tail.
      See [M84](../PLAN.md#m84-infer-a-nonempty-vectors-tail-at-a-nested-application).
- [x] Infer erased parameters from the fields of a saturated retained
      constructor argument, including a list whose element type refers to a
      runtime index. See [M85](../PLAN.md#m85-infer-the-parameters-of-a-retained-constructor-argument).
- [x] Bind an anonymous retained argument in an equation or checked lambda.
      Keep distinct runtime arguments independent. See [M86](../PLAN.md#m86-wildcard-patterns-for-retained-arguments-and-fields).
- [x] Check nested named and overloaded calls at their operand domains.
      [M89](../PLAN.md#m89-nested-computations-in-retained-operands) uses
      `1 + rows * cols` directly as a vector length.
- [x] Use the defining index of a single lazy local binding in a checked
      result when the RHS names a certified computation.
      See [M90](../PLAN.md#m90-equality-for-lazy-local-definitions).
- [x] Check a local product, including numerals and nested calls, using the
      evidence for the ordinary RHS. See [M91](../PLAN.md#m91-dictionary-evidence-for-a-local-index-definition).
- [x] Carry a checked length through a signed value and several acyclic
      let bindings in dependency order. Preserve ordinary signature checking
      and generalisation. See [M92](../PLAN.md#m92-signed-and-grouped-local-lengths).
- [x] Infer the tail of a nonempty vector from grouped local length aliases.
      A checked RHS can suggest the erased argument. The ordinary solver
      must prove that the actual vector has that type. A runtime equality
      test can supply the evidence in its success branch.
      See [M94](../PLAN.md#m94-infer-a-nonempty-tail-through-local-length-definitions).
- [x] Use ordinary list or GADT patterns after a retained constructor.
      M95's checked list adapter returns a vector indexed by a caller's
      runtime length. An ordinary client can combine two checked results
      at that same index. See [M95](../PLAN.md#m95-check-ordinary-list-inputs-at-an-indexed-boundary).
- [x] Infer a partial constructor callback's erased index from the checked
      input in `VCons x <$> fromListExact n xs` and prefix `fmap` calls.
      Keep constructor multiplicities and full callback subsumption.
      See [M96](../PLAN.md#m96-infer-retained-indices-for-partial-constructor-callbacks).
      General callback inference remains open.
- [ ] Extend omission beyond the measured domains and application forms.
      Keep retention independent of visibility and inference.
- [ ] Specify runtime `Type` operands and their universe representation.
      Visible binders in value signatures accept explicit `TypeRep` operands.
      Type operands that request evidence remain open.
- [x] Finish [M68 ordinary callers](../PLAN.md#m68-ordinary-callers-of-retained-apis).
      Imported retained contracts must preserve inference and evidence scope
      without requiring dependent source syntax. Keep the application change
      separate from container and dependent-case inference.

Running safe indexing after M59 is a scheduling choice. Closed `[Bool]` does
not require generic domains, new evaluation or bounds-check elimination.
Generic `[a]` needs the matchability invariant described in
[R23](R23-unsaturated-type-families.md). It does not require the whole R23
surface feature or ordinary function promotion. Runtime-dependent later domains,
such as `[Vec Int n]`, use M67's scoped metadata substitution and nominal
telescope transport. M69 measures that contract with the finite-position client.
M72 implements leading invisible source definitions and one exact
declaration-based omitted operand. The
[research references](../REFERENCES.md#invisible-retained-binders-next-candidate)
record the choice of delimiter, representation and telescope-order checks.

A retained argument can precede an erased `forall` and another retained
argument. In `keepTagged tag @Bool m x`, the type of `x` uses both
`tag` and `m`. The ordinary interface-only client reads bottom for both
indices and returns 47. At `-O2`, the imported R05 rule fires.
[Provider](../examples/qualified-pi/VisibleRuleArguments.hs),
[ordinary client](../examples/qualified-pi/VisibleRuleClient.hs) and
[checks](../examples/qualified-pi/check-visible-rule-arguments.sh).

A required `forall a ->` can also follow a retained argument.
`keepRequired tag (type Bool) m x` checks the type in that source
position. Its kind can refer to a preceding runtime index. A partial
application before the required argument keeps the required binder.
Flat equations split at that binder when the prefix has no strict
value patterns. An explicit curried equation can express a strict prefix.
The R05 rule for `keepRequired` infers its later dependent binders
from the prefix ending at `(type Bool)`.

## Completion boundary

Keep R03 open until the supported domain judgment is stable under substitution
and the full retained-quantifier contract is implemented. Check annotations,
visibility, scope, interfaces, diagnostics, runtime demand and ordinary programs.
A closed-list example or another application-inference case cannot close R03.

## Research

- [Design for Dependent Types, #378](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html):
  quantifiers, known result types and runtime-name identity.
- [The `singletons` repository](https://github.com/goldfirere/singletons):
  singleton and defunctionalization comparisons. The reviewed source is commit
  `8c9221c164832bfd17878af94d50d8ade706932d`. Arbitrary-closure reification
  uses trusted casts; it does not establish native function equality.
- [Agda v2.8.0](https://github.com/agda/agda/tree/3d04bacca842729f9c0869b9287256321b5f450f):
  selected telescope, substitution and pattern-refinement code. Transport later entries together.
  Preserve Haskell's separate laziness and evidence rules.

The dated research records are in
`_build/verification/native-roadmap-research-2026-11-16/`:
`syntax-safe-index-v1/`, `syntax-agda-telescope-v1/`, and
`syntax-singletons-functions-v1/`. Their provenance files state what was read.
The cache is ignored. The task boundaries above and the reference ledger must
remain usable without it.

## Runtime Type operands

Proposal #378 gives `foreach (a :: Type) -> a -> a` as an example.
Its `ex5` can pass the retained type to an erased argument and ignore
the runtime representation. The proposal leaves pattern matching on
`Type` open. Its review asks what a program gains from carrying a
runtime representation back across the phase boundary.
[Design sketch, §1.4.9](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html#phase-distinction)
and [review discussion](https://github.com/ghc-proposals/ghc-proposals/pull/378#issuecomment-765416198).

A visible binder in a complete value signature gives the caller an
ordinary `TypeRep` argument. For example:

```haskell
preserve :: foreach (a :: Type) -> a -> a
preserve _ x = x

preserve (typeRep @Bool) True
```

The signature checker binds the source `a` to an erased type variable and
uses the canonical `TypeRep a` as the operand type. The function equation
can bind a value witness and name the erased parameter separately with
`(rep :: TypeRep witness)`. Use `eqTypeRep` and `HRefl` for a runtime
comparison. A failed comparison can leave the payload unevaluated.

The [source-hidden regression](../../../testsuite/tests/dependent/should_run/NativeRetainedTypeWitness.sh)
compiles a Safe provider and a Safe ordinary client at `-O0` and `-O2`.
The client supplies a witness for its own type. It checks agreement
between two witnesses and gets its payload back with a bottom witness.
The interface stores the elaborated type
`forall {a}. TypeRep a -> a -> a`. The original `foreach` spelling is
absent from the interface. GHCi displays the elaborated type.

The [column example](../examples/qualified-pi/RuntimeTypeColumn.hs)
combines this binder with a bounded lookup. The column stores a
representation with its values. On a successful `eqTypeRep` comparison,
`HRefl` identifies the requested element type. `mkFinGeneric` then
checks the signed offset against those values. A type mismatch leaves
both the offset and the values unevaluated. A negative offset at the
correct type leaves the values unevaluated.

The ordinary reflection API implements the same equality and demand
semantics with the following types.

```haskell
data Request where
    Request :: TypeRep (a :: Type) -> a -> Request

keep :: TypeRep a -> a -> a
keep _ x = x

same :: TypeRep a -> TypeRep b -> a -> Maybe b
same a b x = case eqTypeRep a b of
    Just HRefl -> Just x
    Nothing    -> Nothing
```

An IORef supplied a Boolean request and then an integer request. The
test selected the Boolean branch and negated its payload. It returned
the integer unchanged. A mismatched comparison returned `Nothing`.
`keep` returned `True` with an undefined representation. Stock GHC
9.14.1 and the self-hosted fork both gave these results at `-O0` and
`-O2`. Agda's standard universe example defines a datatype of codes
and an interpretation function. Haskell's indexed `TypeRep` also
supports types declared in other modules.
[Agda universe example](https://agda.readthedocs.io/en/stable/language/mutual-recursion.html#interleaved-mutual-blocks)
and [reflection paper, §§5.2–5.6](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/dynamic.pdf#page=6).

An ordinary caller can pass `TypeRep a` to a checked lookup that returns
`Maybe a`. After `eqTypeRep` yields `HRefl`, the provider can use
`mkFinGeneric` and `indexGeneric` on an existential list at that type.
Public Stage 2 at `96fddcca11` checked the provider with the dependent
extension disabled and the native indexing provider available through
interfaces. The ordinary caller defined its own element type. At `-O0`
and `-O2`, a type mismatch left the payload and offset lazy.

`SomeTypeRep` can hide a kind other than `Type`. For lifted value types,
compare `typeRepKind rep` with `typeRep @Type` and match `HRefl` before using
`withTypeable`. This makes a `Typeable a` constraint available to a
checked continuation. A required erased `forall a ->` still needs a
`Typeable a` constraint to construct the representation of an abstract `a`.

An opted-in caller can write `preserve (type Bool) True`. This fork
checks the operand against the expected `TypeRep` type, including when
that type comes from an ordinary imported function. A concrete operand
obtains its dictionary through GHC's `Typeable` solver. An abstract
`(type a)` needs evidence for `Typeable a` at the call site. The same
spelling at a required `forall` binder stays erased. An expression
with no expected witness type cannot request reflection this way.
Runtime type patterns still need a syntax and an evidence scope.
Agda's hierarchy distinguishes a
universe's level from the datatypes of codes that programs can inspect.
[Agda universe levels](https://agda.readthedocs.io/en/v2.8.0/language/universe-levels.html)
and [Agda type codes](https://agda.readthedocs.io/en/v2.8.0/language/mutual-recursion.html#forward-declaration).

- [x] Give a visible source binder an erased type variable and a value
  whose representation is `TypeRep a`. Check a provider and a source-hidden
  ordinary caller. Keep the ordinary type and arrow in the interface.
- [x] Use the type variable in erased positions. Pass the value in
  retained positions. Check that `preserve` ignores a bottom
  representation in compiled code and GHCi.
- [ ] Preserve source telescope information for implicit construction of
  the runtime representation across modules.
- [x] Construct concrete and abstract type operands when the caller
  expects `TypeRep`. Reject an abstract type with no `Typeable` evidence.
- [ ] Check a runtime type test only inside the branch that receives
  nominal equality evidence. Specifying source patterns for this test
  remains a separate design decision.

## Infer the original input index after a length match

In `checkedSplitAtValue`, the expected `SplitAtWidth` result names `v`.
The constructor call infers its last erased argument from that result.
The `Refl` match proves the width equality needed to check the inferred
argument's kind. The source-hidden checked split runs at `-O0`, `-O1`,
and `-O2`.

Two Boolean-indexed inputs can have the same width index and different
values. The negative regression checks that their mismatch reports a
type error at the field argument. The solver records the holes of the
source evidence when it uses a Given to fill an equality hole. This
keeps diagnostics finite when another index equality remains unsolved.
[Checked constructor](../examples/packed-vector/CertifiedSplit.hs)
and [negative regression](../../../testsuite/tests/dependent/should_fail/NativeIrredIndexEvidence.hs).

A constructor can hide the argument of an open family. For example,
`Hide :: Proxy p -> Hidden (Width p)` records `p` in its field.
When the caller knows `Width p ~ Width q`, a `Proxy q` still cannot
replace the required `Proxy p`. GHC now reports GHC-25897 at the field.
Previously a Wanted for `q ~ p` rewrote the argument of `Width`, and the
Given sent the solver back to the same family application. The same cycle
occurred under `Maybe` in ordinary Haskell.

The solver tracks where it used a local family equality on the current
rewrite path. A sibling occurrence can still use the same equality.
Recursive family axioms retain their ordinary reduction-depth diagnostic.
[Retained constructor](../../../testsuite/tests/dependent/should_fail/NativeFamilyIndexEvidence.hs)
and [ordinary equalities](../../../testsuite/tests/typecheck/should_fail/WantedFamilyRewriteCycle.hs).

## Retain a bounded position and its actual predecessor

The two constructors of `Fin n` have different telescopes. `FZ` has no
runtime fields. Its result still fixes the bound to a successor.
`FS` stores a position at the predecessor bound. `viewFin` uses the
ordinary GADT equality in each alternative to prove which value matched.

An ordinary client reads position two from IO, opens the successor
view, matches its proof, and returns position one. The zero view has
no predecessor. A bottom position fails when matched. The successor
view can return its proof with a bottom predecessor. Supplying another
predecessor at the same bound fails at `Refl`.
[Haskell source](../examples/qualified-pi/FinitePosition.hs),
[ordinary client](../examples/qualified-pi/FinitePositionClient.hs)
and [Agda provenance](../REFERENCES.md#recover-the-predecessor-of-a-bounded-position).

`mkFinVec` checks a signed position against the vector spine. A negative
position leaves the vector unevaluated. A valid position needs only
the spine through that cell. In the successor view, the ordinary client
opens the proof and uses the predecessor to index the vector tail.
The bound also lets `indexVec` use the checked position with another
vector of the same length.

## Write indexed constructors in retained applications

`constructedViews p` passes `FZ` and `FS p` directly to `viewFin`.
The expected bound fixes their hidden parameter. An ordinary client
opens the successor view to retrieve its input. The test also constructs
`Mk2 :: b -> a -> Both (Maybe a) [b]`. The expected result fixes
both erased arguments even though the constructor quantifies them in
the opposite order. A stored unfolding preserves the field order and
leaves both fields lazy.
[Finite-position view](../examples/qualified-pi/FinitePosition.hs),
[multiple result indices](../../../testsuite/tests/dependent/should_run/NativeGeneralGadtOperandProvider.hs)
and [Agda and GHC provenance](../REFERENCES.md#recover-the-predecessor-of-a-bounded-position).

The application checks the fields after matching the result. For
`Hide :: Proxy p -> Hidden (Width p)`, the expected `Hidden (Width p)`
can supply the constructor argument syntactically. Two independent
values `x` and `y` at `Proxy p` still give distinct indices. Returning
the application built from `y` when the result requires `x` fails at
the source expression. The old vector operand keeps its original
interface header. Pass the previous compiler as the third argument of
the regression to check both headers.
[Source diagnostic](../../../testsuite/tests/dependent/should_fail/NativeIndexOperandFamily.stderr)
and [cross-module checks](../../../testsuite/tests/dependent/should_run/NativeIndexedFin.sh).

### Infer two retained indices from a constructor result

An ordinary caller can split two vectors at one runtime cut. A successful
bounds check identifies the common length with the sum of the two piece
lengths. The expected result determines the two element types, the cut,
the suffix, and the two original vectors. Each field fixes the suffix
through the vector domain of its inner sum. The constructor and both
calls to `splitAtValue` need no written type application. The single-vector
bounds check uses the same inference rule.

An ordinary caller splits Boolean flags and characters at one cut. Each
reconstruction proof identifies its own input. For two Boolean vectors,
exchanging the proofs still fails at the written arguments. The caller
can open the first proof while the second vector is undefined.
An excessive cut returns `Nothing`.

GHC selects an omitted index only when it occupies a unique direct position
in the result of the same data constructor type. It collects all choices
before emitting kind equalities. It then checks them in binder order so a
later kind can mention an earlier choice. The type checker still checks
both fields against the instantiated constructor signature. Exchanging
two vectors of equal length fails at the first exchanged field.
[Source](../examples/packed-vector/CertifiedSplit.hs),
[ordinary client](../examples/packed-vector/CertifiedSplitClient.hs),
and [dependent binder regression](../../../testsuite/tests/dependent/should_compile/NativeDependentResultBinders.hs).

## Index an ordinary list using its source length

`indexGeneric :: forall a. foreach (xs :: [a]) -> Fin (sourceLength xs) -> a`
uses the same ordinary function that counts a list at runtime. The existing
`GenericCount` family forwards to this definition. An ordinary module can
write the following local function without a signature.

```haskell
checked xs n = fmap (indexGeneric xs) (mkFinGeneric xs n)
```

The negative offset case returns `Nothing` before inspecting the list.
A finite offset into a cyclic list can succeed. The API computes only
the prefix it needs. A type checker can also use a statically constructed
`Fin` when its bound matches the list.
[Ordinary caller](../examples/qualified-pi/FinitePositionClient.hs)
and [Agda and Serokell examples](../REFERENCES.md#index-an-ordinary-list-at-its-source-length).

### Omit an impossible list-index equation

The generic list index needs only the two cons equations.

```haskell
indexGeneric :: forall (a :: Type). foreach (xs :: [a]) -> Fin (sourceLength xs) -> a
indexGeneric (value : _) FZ = value
indexGeneric (_ : rest) (FS smaller) = indexGeneric rest smaller
```

At `[]`, the checked equation for `sourceLength` gives `Fin 'Z`.
Neither `FZ` nor `FS` can construct that type. The written constructor
patterns on the position tell GHC which argument to inspect in the
omitted equation. A wildcard or a lazy pattern alone supplies no split.
An inhabited or unresolved argument still needs a written equation.
Omitting the successor equation still produces the ordinary incomplete
pattern diagnostic. The empty-list function waits for its position.
A bottom position raises its own exception.
[Implementation](../examples/qualified-pi/ParameterizedIndexLibrary.hs),
[ordinary caller](../examples/qualified-pi/FinitePositionClient.hs)
and [Agda and Haskell precedents](../REFERENCES.md#omit-an-impossible-list-index-equation).

### Consume a bounded position at its inferred multiplicity

`indexGeneric` quantifies the multiplicity of its position. The same
two equations support an unrestricted callback to `fmap` and a linear
reader for a stored cursor. The element type stays the first visible
type argument. The caller can still write `indexGeneric @Char`.

A cursor stores a runtime list and a position indexed by that list's
source length. An ordinary module can open an erased package containing
the cursor and its linear reader. Both lookup paths return the selected
element on a cyclic list and leave the suffix unevaluated on a partial
list. A callback restricted to `%1` still fails at ordinary `fmap`.
A position checked against another list still requires a proof that
their lengths agree.
[Cursor](../examples/qualified-pi/ListCursor.hs),
[ordinary client](../examples/qualified-pi/CursorClient.hs)
and [prior art](../REFERENCES.md#consume-a-list-position-in-a-linear-function).

The element of the list can itself depend on a runtime input. In
`indexGeneric [v] pos`, the declaration of `v :: Vec Int n` fixes
the element type, including the local length `n`. The caller can still
infer the multiplicity of the position. A fully applied ordinary arrow
supplies that multiplicity variable. Its use remains subject to GHC's
linearity check. The Core API exercises this example inside `withPacked`.
[Runtime-indexed client](../../../testsuite/tests/ghc-api/NativeDependentCoreRun.hs).

### Consume a checked position and a continuation

`withIndex xs pos visit` consumes two linear arguments. The cons equations
match on the position and bind the continuation. An ordinary client
checks an offset before supplying both. A negative offset leaves the
list unevaluated. Partial application to the list and position waits for
the continuation before inspecting either input.

A strict function can match an additional flag in the same equations.
A written `~_` keeps an unused fallback lazy in the selected row. A
callback used twice or discarded reports GHC-18872 at its binder in the
source equation. The check covers ordinary suffix arguments. Later
retained indices still use the separate dependent-pattern rules.
[Source function](../examples/qualified-pi/ListCursor.hs),
[ordinary caller](../examples/qualified-pi/CursorClient.hs)
and [strict pattern control](../../../testsuite/tests/dependent/should_run/NativeLinearIndexColumns.hs).

### Use one position on two lists of equal length

`indexAtLength` takes a list, a position of type `Fin n`, and a constraint
`n ~ sourceLength xs`. The two cons clauses cover this function. In the
empty branch the constraint gives `n ~ Z`. The same equality justifies
a written empty case on the position. An independent equality about a
different index leaves the empty-list equation missing (GHC-83605).

`lookupAligned` checks an offset against the first list, then compares the
lengths of both lists. A successful comparison supplies the equality for
the second lookup. Equal lengths require traversing both finite spines.
A negative offset returns before inspecting either list.
[Library](../examples/qualified-pi/ListCursor.hs) and
[ordinary client](../examples/qualified-pi/CursorClient.hs).

### Zip two lists at a common length

```haskell
zipWithExact :: forall a b c. foreach (xs :: [a]) -> foreach (ys :: [b])
             -> (sourceLength xs ~ sourceLength ys) => (a -> b -> c) -> Vec c (sourceLength xs)
zipWithExact [] [] _ = VNil
zipWithExact (x : xs) (y : ys) f = VCons (f x y) (zipWithExact xs ys f)
```

At two cons cells the signature gives equality of the tail lengths.
At a cons and an empty list it would equate a successor with zero.
GHC checks each omitted pair against the declared equalities and the
enclosing nominal Givens. The warning checker tests that the remaining
counterexamples satisfy their specialised equality dictionaries.

For independent inputs, `checkedZipWith` first compares the two finite
spines with `sameNat`. Unequal lengths return `Nothing` before the
callback runs. An ordinary caller can use `fmap ((.) sum toList)` on
the checked result in an unsigned binding. The known vector length
determines the callback's input while its result type is inferred.
For two occurrences of the same input, an ordinary
caller can use `zipWithExact xs xs` directly. The first output cell
then leaves the tail of `xs` unevaluated. A cons equation that returns
`VNil` reports GHC-83865 at its result. Omitting the possible cons/cons
equation reports GHC-83605 with the two constructors.
[Producer](../examples/qualified-pi/AlignedList.hs),
[ordinary caller](../examples/qualified-pi/CursorClient.hs), and
[callback inference](../REFERENCES.md#runtime-equality-and-aligned-zip-next-source-probe).

An ordinary caller can also write both list spines. The declared types
of the elements determine the two element types. The equations for
`sourceLength` reduce at compile time.

```haskell
zipTwoPairs :: a -> a -> b -> b -> [(a, b)]
zipTwoPairs x y u v = toList (zipWithExact [x, y] [u, v] (,))
```

Changing the second list to `[u]` reports GHC-18872 at `zipWithExact`.
With `[x, u]` as the first list, GHC-25897 points to the element `u`.
With `OverloadedLists`, GHC checks the selected `fromListN` implementation
as a function call. The ordinary list constructors do not justify equations
for an arbitrary `IsList` instance.

An ordinary caller can write `toList (zipWithExact [1,2] [10,20] (+)) :: [Int]`.
The checked equations reduce the length of each written spine for
arbitrary element values. GHC binds those expressions lazily and uses
each binding in the index and in the runtime list. A later bottom remains
unforced when the caller needs only the first result. The same scope can
return a vector of closed length. A result that still refers to a local
element is rejected at the source, and two local elements do not acquire
an equality from their list lengths. This rule applies to leading written
list operands when checked family axioms remove their element identities
from the remaining type and constraints.
[Haskell 2010 Report, let expressions](https://haskell.foundation/haskell-2010-revised-report/expressions.html#let-expressions),
[GHC proposal #378, dependent application](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html#dependent-application-and-the-static-subset),
and [Agda's length-indexed zip](https://agda.github.io/agda-stdlib/v2.3/Data.Vec.Base.html).
