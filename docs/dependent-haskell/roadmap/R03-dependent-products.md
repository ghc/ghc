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
- Closed parameterized domains such as `[Bool]`, `Maybe Nat` and `Tree Bool`.
- Retained workers and function fields through separate interfaces.
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
      Existing `TypeRep` and required erased `forall` leave a bridge to define.
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

The 354-line compiler rejects `preserve :: foreach (a :: Type) -> a -> a`
with GHC-83605 at `(a :: Type)`. The existing algebraic binder
implementation cannot supply a type representation for this argument.
There is a working reference implementation with the following types.

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

- [ ] Give the source binder an erased type variable and a value whose
  representation is `TypeRep a`. Preserve the source telescope in
  interfaces. Check a provider and a source-hidden ordinary caller.
- [ ] Use the type variable in erased positions. Pass the value in
  retained positions. Check that `preserve` ignores a bottom
  representation in compiled code and GHCi.
- [ ] For a concrete type operand, construct the representation from
  GHC's existing `Typeable` evidence. For an abstract operand, track
  where the caller obtains the evidence. An existential input must
  also establish that its witness has kind `Type`.
- [ ] Check a runtime type test only inside the branch that receives
  nominal equality evidence. Specifying source patterns for this test
  remains a separate design decision.
