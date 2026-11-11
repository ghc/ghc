# Native dependent Core implementation plan

Implement dependent types in Haskell through a native extension to Core. Base the
work on Richard Eisenberg's historical `nokinds-pi` branch. Port its useful changes
to current GHC and complete the missing rules. The first working target is a
retained Boolean argument whose value determines the result type.

Last updated: 2026-11-11. Compiler source baseline:
`19ff36f2aede0ca2e554016ff22c71e73aa9803b`.

See [current status](README.md) and [primary references](REFERENCES.md).

## Direction and boundaries

The implementation extends Core. It does not require singleton elaboration or
explicit Boolean reification at ordinary runtime call sites. Keep the validated
singleton translation as a comparison implementation for the overlapping subset.
It supplies expected results, but it does not prove the native extension.

The historical starting point is
[`nokinds-pi` at `b64ef80f8002727422c05092f429c38d0527ed0b`](https://github.com/goldfirere/ghc/tree/b64ef80f8002727422c05092f429c38d0527ed0b),
dated 2014-08-19. It introduces `PiTy`, independent binder attributes, and a sketch
of dependent application checking. It is unfinished. Its type-computation utility
still discards runtime arguments, and its case checker uses one fixed result type.
Updating this branch requires semantic work as well as API changes.

Use the later D/DC, roles, and graded-usage research to evaluate the port. Their
formal results do not automatically apply to the historical branch or this GHC
implementation. Keep existing FC coercion checks. Do not add trusted universal
coercions to make incomplete dependent rules pass.

Start with visible, retained, unrestricted Boolean binders. Results have fixed
lifted runtime representations. Source index expressions initially use Boolean
constructors and named runtime Boolean variables. A producer can supply the value
of such a variable. The type checker does not evaluate that producer or equate
separate variables because their definitions look equal.

The internal index representation must remain valid under compiler substitution.
A source restriction to variables and constructors is insufficient: inlining can
replace a variable with an arbitrary expression. Either represent the substituted
expression safely or preserve its binding and restrict the relevant transformation.
Settle this rule before enabling dependent optimization.

Defer general term evaluation in types, automatic promotion, retained linear
binders, dependent runtime representations, and general dependent inference.
Keep arithmetic and SMT integration separate from this Core work.

## First source target

These examples describe intended behavior. The `foreach` spelling follows the
design proposal. It is not syntax implemented by this checkout.

```haskell
type family Select (b :: Bool) where
  Select True = Int
  Select False = Bool

choose :: foreach (b :: Bool) -> Select b
choose True = 1
choose False = False

keep :: foreach (b :: Bool) -> Select b -> Select b
keep b x = x

again :: foreach (b :: Bool) -> Select b
again b = keep b (choose b)
```

The native target must also accept an existing runtime Boolean as an index.
Multiple dependent calls must refer to the same binding. Branches can consume the
selected value after checked Boolean refinement. A locally bound index cannot
escape its scope without a specified dependent result or existential package.
Passing a runtime Boolean must not insert a strict singleton conversion.

## Task sequence

### M0: completed preparation

- [x] Update and build the current GHC baseline.
- [x] Inspect the proposals, roadmap, and implementation research.
- [x] Validate a singleton comparison implementation on the baseline compiler.
- [x] Record the native Core direction and initial tasks.

### M1: audit and pin the historical port

- [x] Retain the historical branch and exact commit as a local reference.
- [x] Inventory the relevant commits. Separate work already incorporated into GHC
      from retained-dependent changes. Map old modules to current equivalents.
- [x] Record incomplete rules and unavailable validation. Preserve commit provenance
      when porting changes. Keep current buildable GHC as the development base.
- [x] Retain the temporary singleton comparison program in a reproducible location.

Gate: a port inventory identifies reusable code, required adaptations, and new
semantic work. We have inspected the historical source but have not built it.
See [the port inventory](PORT.md) and [comparison programs](reference/README.md).

### M2: specify the restricted Core extension

- [ ] Define retained Boolean binders, term indices, lambda, application, and case
      typing. State scope, substitution, erasure, and non-strictness rules.
- [ ] Choose how runtime terms occur in types. Compare historical promotion with
      a checked index-expression representation. Require substitution closure.
- [ ] Keep dependence, visibility, retention, and multiplicity distinct. Prefer a
      dedicated retained-dependent form that preserves existing `FunTy` and
      `ForAllTy` fast paths, unless the audit gives a concrete reason to change them.
- [ ] Define nominal equality, constructor discrimination, and checked reductions.
      State which index comparisons remain opaque.
- [ ] Specify a dependent motive or explicit branch-equality mechanism for cases.
      The existing case annotation lies outside the case binder's scope.
- [ ] Explain how the rules fit FC, roles, multiplicities, and fixed runtime
      representations. Record the remaining formal proof obligations.

Gate: written rules and positive and negative Core examples. Resolve representation
choices before implementation. Cover bottom, partial application, and `seq`.
The first increment follows [the restricted Core specification](CORE.md).

### M3: port binders and implement dependent lambda and application

- [ ] Add the retained binder and index representation to current type code.
- [ ] Update free variables, capture avoidance, term/type substitution, equality,
      kinding, pretty-printing, and relevant traversals together.
- [ ] Update `exprType`, function-result calculation, and Core Lint consistently.
      Runtime dependent application substitutes its argument into the result.
- [ ] Check binder renaming, aliases, shadowing, beta reduction, and scoped lets.
- [ ] Keep ordinary GHC programs building while the feature remains gated.

Gate: hand-built Core for identity and retained application passes Lint. Lint and
type computation agree before and after supported substitutions. No frontend
syntax is required for this milestone.

### M4: implement dependent Boolean elimination and evidence

- [ ] Implement the M2 case motive or branch-equality design.
- [ ] Instantiate branch results under checked `True` and `False` refinements.
- [ ] Update case scope in substitution, free variables, and Lint.
- [ ] Preserve nominal index equality. Reject invalid casts and escaping binders.
- [ ] Check that bottom cannot supply constructor refinement or unchecked equality.

Gate: native Core for `choose`, `keep`, and shared-index composition passes Lint.
Mismatched branches and forged refinements fail. Evidence follows checked rules;
it does not rely on `unsafeCoerce` or plugin axioms.

### M5: execute through the existing backend

- [ ] Carry the restricted extension through the minimum mandatory Core pipeline.
- [ ] Update CorePrep and CoreToStg for the specified erasure of new index and proof
      annotations. The retained Boolean remains one ordinary runtime argument.
- [ ] Reject unsupported forms explicitly. Do not silently pass them to the backend
      or enable transformations that violate their invariants.
- [ ] Inspect Core and STG for duplicate witnesses and inserted forcing.

Gate: native examples execute with the specified values and termination behavior
through a restricted pipeline. This does not establish optimizer safety. Even an
unoptimized build runs mandatory transformations that need an audit.

### M6: preserve dependencies through optimization

- [ ] Audit simplification, beta/eta transformations, inlining, unfoldings, and
      substitution into types and coercions.
- [ ] Audit case simplification, case-of-case, join points, occurrence analysis, and
      let floating. A binder used only in a type still has a scope dependency.
- [ ] Audit demand analysis, worker/wrapper, specialization, and arity changes.
      Preserve laziness and the fixed runtime representation restriction.
- [ ] Enable transformations in bounded steps. Run Core Lint after each relevant
      pass and compare optimized and unoptimized behavior.

Gate: the Boolean examples pass at `-O0` and `-O2` with Core Lint. Record which
transformations were exercised and which remain gated. A passing executable does
not establish safety for untested forms.

### M7: expose the restricted source language

- [ ] Add an experimental opt-in flag, syntax, AST representation, and diagnostics.
- [ ] Extend renaming and checking for explicit retained Boolean signatures.
      Track one runtime binder when it occurs in both terms and types.
- [ ] Elaborate constructor calls, retained variables, and dependent Boolean
      patterns directly into checked native Core.
- [ ] Accept ordinary runtime Booleans under shared bindings. Reject index escape
      and unsupported expressions with precise diagnostics.
- [ ] Update source printers and extension documentation.

Gate: the source target and a runtime-unknown Boolean example compile and execute
directly. Their implementation uses no singleton ABI or reification step.

### M8: support interfaces and validate the compiler change

- [ ] Encode retained binders, indices, and evidence in interface types, unfoldings,
      fingerprints, and recompilation dependencies.
- [ ] Check cross-module calls, imported unfoldings, and interface round trips.
- [ ] Add focused compiler regression cases. Reuse existing testsuite conventions
      and golden tests where appropriate.
- [ ] Run relevant type-checker, simplifier, interface, and code-generation tests.
      Rebuild and bootstrap the compiler with the feature disabled.
- [ ] Record exact revisions, commands, results, and proof boundaries.

Gate: the experiment works across modules, ordinary programs remain compatible,
and the modified compiler builds itself. General dependent Haskell remains further
work.

## Acceptance checks

| ID | Required result |
| --- | --- |
| B01 | `choose True` returns an `Int`; `choose False` returns a `Bool`. |
| B02 | A runtime-unknown Boolean selects and consumes the correct result. |
| B03 | `keep b (choose b)` reuses one runtime binding and one index. |
| B04 | Boolean branches supply checked nominal refinement. |
| B05 | Mismatched arguments and branch result types are rejected. |
| B06 | Separate opaque Boolean bindings do not become equal from equal-looking definitions. |
| B07 | Aliases and shadowing preserve the specified binder identity and scope. |
| B08 | An index cannot escape a local or existential binding. |
| B09 | An ignored bottom Boolean stays non-strict, including partial application and `seq`. |
| B10 | Inspecting a bottom Boolean diverges. Index knowledge does not replace its runtime value. |
| B11 | Substitution and beta reduction preserve types without capture. |
| B12 | Case transformation and let floating preserve dependency scope. |
| B13 | Invalid coercions and representational substitutions cannot forge nominal index equality. |
| B14 | Unoptimized and optimized compilation pass Core Lint and agree on defined results and tested termination behavior. |
| B15 | Interfaces round-trip and cross-module execution passes; feature-disabled programs remain compatible. |

Distinguish Core construction, frontend behavior, optimization, runtime execution,
and module integration. The existing singleton probe supplies comparison evidence
for some values and laziness cases only. No native Core implementation or native
acceptance check has passed yet.

## Further work

After M8, review broader retained kinds, nested and higher-order dependence,
source inference, arbitrary index expressions, roles involving dependent
functions, linear retained binders, and dependent runtime representations.
Use separate tasks for each extension. SMT reasoning and proof-certificate
checking remain separate work within the longer-term program.
