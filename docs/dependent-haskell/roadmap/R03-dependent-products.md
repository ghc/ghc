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

- Runtime Bool and Peano Nat indices, checked equations and bounded lambdas.
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
- Imported retained application contracts in ordinary modules at M68. Both
  rebuilt stages pass paired clients, scoped callback dictionaries and located
  rejections. The Showcase has 118 output lines through seventeen modules.

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
- [ ] After the R05 generic-domain work, measure
      `forall a. foreach (xs :: [a]) -> Fin (GenericCount xs) -> a`.
      Reuse Fin from ParameterizedIndexLibrary and GenericCount from
      PackedVecLibrary. Include function-valued and runtime-index-bearing
      elements, signed bounds, lazy fields and finite cyclic access. Keep the
      family saturated. Static review selects library composition first.
      Source acceptance remains unmeasured.
- [ ] Specify invisible retained binders and explicit runtime overrides.
      Keep retention independent of visibility. Do not infer a new `foreach`.
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
such as `[Vec Int n]`, need an additional telescope contract.

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
