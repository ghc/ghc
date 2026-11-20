# R05: Dependent Core language

Upstream: [Dependent Core language](https://ghc.serokell.io/dh#dependent-core-language).
Local status: restricted native implementation. General transformations remain
open. Use the [work queue](../ROADMAP.md#current-work-queue) for priorities.

## Goal and dependencies

Provide a coherent typed Core representation for dependent types. Define
substitution, conversion, scoped evidence, roles, runtime usage and erasure.
Preserve the compiler transformations and ordinary GHC behavior that depend on
those rules. The upstream graph gives R05 no outgoing feature edge. That does
not remove its research or implementation obligations.

The user selected a native Core extension based on Eisenberg's historical
`nokinds-pi` work. The current representation is specified in
[CORE.md](../CORE.md). Keep `singletons` as a comparison implementation.

## Supported subset and evidence

- `TermVarTy`, `TermAppTy`, `DepFunTy` and scoped native coercions.
- Closed algebraic domains, retained field telescopes, fixed function-valued
  fields and opaque function heads. A head is not a general term expression.
- Scoped constructor refinement, Core Lint, interfaces and checked lowering.
- Bounded beta exposure, alias substitution and dead-local removal.
- M56 closed parameterized domains and M57 nominal scalar-domain conversion.
  M58 source checking uses those Core rules. M59 extends frontend comparison
  beneath unchanged function domains using the existing congruence rule.

The [published M62 checkpoint](../ROADMAP.md#where-this-fork-stands) passes its
selected checks with both compiler stages and its independent artifact audit.
M63/M64 add ordinary-arrow and forall matchability. Their selected source,
Core, reflection and compatibility checks pass in both stages. The foundation
is published. The full R23 scope remains open. M65 adds closed function-value indices.
M66 bounds the selected native family views and preserves diagnostic context.
M67 adds structural generic admission, opaque parameter fields and nominal
later-domain transport. Both builds, the selected API/source controls,
Showcase116/16 and seven registered tests pass. Generic elements include
`Vec Int n` for an earlier runtime index. Source roots retain their structural
or fixed native-call-head policy. See [the checkpoint](../PLAN.md#m67-generic-domains-source-and-self-hosted-validation).
Restricted pipelines and Lint checks do not establish general preservation.
M68 makes the existing application contract available to ordinary importing
modules. Both stages check native dictionary scope and reject escaping or
linear actuals. The owner is R03. This increment adds no Core equality rule.

## Occurrence analysis

The ordinary occurrence analyser can remove a runtime binding that occurs
only in a type. In the source probe, `b = opaque source` supplies the
index of a vector in a type application. Removing the binding makes
Core Lint report `Invalid runtime index: b :: Bool`. A second example
uses an earlier lambda binder only in a later binder's type. The ordinary
pass marks the earlier binder dead.

A private compiler counts those uses and visits the types of lambda
binders from the innermost to the outermost. Its standalone occurrence
pass then preserves the two bindings. Core and STG Lint pass. An
ordinary interface client observes that `choose` forces its Boolean and
`keep` can return the vector without forcing it. For modules with algebraic indices such as `Bool` and `Nat`, the compiler
can now run one Gentle simplifier iteration at `-O0`. Occurrence analysis
keeps an index that occurs only in a let signature, a recursive binding,
a case result or a later alternative field. Preserving its scope keeps
the original lazy demand. A client can call a function with an
exceptional index and obtain `20` from the unused branch.

An opaque function head can also occur in an index. A default case for
that call binds nominal evidence. Preserve the binder during case
preparation and merging. A separately compiled client observes that
`const True` leaves its exceptional argument unevaluated. A function
that demands the same argument still raises the exception.
The `transportElement` example changes the element type of its result
vector while preserving its runtime length. The `adaptTokenVector`
example converts its domain with a nominal proof between two accepted
index types. In both directions the ordinary client can return a
constant vector without evaluating the token. A worker that examines
the token still evaluates it. Local rewrite rules select the restricted
pipeline. General `-O1` remains open.
[Function field and ordinary client](../../../testsuite/tests/dependent/should_run/NativeFunctionIndex.sh).
[Source controls](../../../testsuite/tests/dependent/should_run/NativeSimplifierProvider.hs)
and [interface client](../../../testsuite/tests/dependent/should_run/NativeSimplifierClient.hs).

## Retained operands in the native optimizer

Both compiler stages produce all 266 Showcase output lines at `-O0` and
`-O1` with Core and STG Lint. Self-hosted GHCi matches at `-O0`. The runner applies `-fomit-interface-pragmas` and
`-fno-write-if-simplified-core` after `-O1`. The retained argument of a
call keeps the constructor tree that the next argument's type mentions.
A case on `f n` keeps that expression in scope for its branch equality.
The regression compares nested calls, cyclic inputs and exceptions at both
optimization levels. [Function index test](../../../testsuite/tests/dependent/should_run/NativeFunctionIndex.sh).

A list used as a retained argument desugars to its constructors. In
the same optimized module, an ordinary list literal desugars to `build`.
A floated opaque witness receives a lazy local alias during Core Tidy.
When CorePrep lowers `seq#` to a case, it renames the index in the case
binder's type. These steps preserve the result type and the observed
demand in the same example. [Showcase](../Showcase.hs) and
[decision provenance](../REFERENCES.md#retained-operands-under-optimization).

### Default optimized interfaces

The bootstrap compiler at `879625a6d8` reproduces all 271 Showcase lines at
default `-O1`. Run the [Showcase check](../check-showcase.sh) with
`--default-o1` to include optimized unfoldings. Core Tidy keeps the original
scrutinee when a branch's payload type mentions it. When the simplifier
inlines the case twice, each copy gets its own field and proof binders.
The [interface client](../../../testsuite/tests/dependent/should_run/NativeDefaultO1Interface.hs)
uses a vector witness and an IO witness in the same expression.

The compiler at `63ebc62180` adds a header for retained constructor proofs.
At all three compression levels, the ordinary and dependent-coercion sample
interfaces have the same bytes as before. An older reader rejects a new
projection at its header. The new reader runs the same source-free client
with either provider. An interface that carries both proof forms selects the
constructor header in either declaration order. In six runs the client
returned `(9,'i')` without forcing either witness.

At `3b7108a97b`, both the bootstrap compiler and the self-hosted compiler
produce all 272 Showcase lines with default optimized interfaces. The
[ordinary simplifier client](../../../testsuite/tests/dependent/should_run/NativeSimplifier.sh)
imports its provider with only an interface and object and compares its
output to the stripped-interface run. The same test still checks rejection
of unsupported optimizer options.

The private compiler at `e070cb70cc` also exports a generated specialization
rule and a written inline pragma on a retained function. A source-free
client inlines a case on the result of a callback. It distinguishes the
strict `id` callback from lazy `const True`. The same client fires the
numeric specialization rule `SPEC numericCall @Int` for lengths two and
zero, producing `[42,42]` and `[]`. An older compiler that supports the
rule alone rejects a mixed interface at the header. The reader also
rejects repeated field names in imported branch evidence. The 272-line
Showcase passes with default
`-O1` on that private bootstrap compiler. A class method can require its
unfolding when an instance omits its own definition. The private compiler
at `55c7fb4606` checks that the unfolding belongs to the declared default
method and has the selector’s type. A separately compiled instance
inherits the default at `-O0` with interface pragmas enabled or ignored.
A retained-sum result also works at default `-O1`. Each set of required
interface features has a header that an earlier reader can check.

The [vector example](../examples/packed-vector/DefaultVector.hs) defines
`filled :: foreach (n :: Nat) -> Vec a n` in terms of the class’s element.
An instance for `Char` supplies only `defaultElement`. An ordinary client
gets `"xxx"` from `toList (filled @Char 3)`. In that client,
`safeHead (filled @Char 0)` fails with GHC-83865. Its actual length is
`Z` and the head function requires `S n0`.

### Optimizing erased packages

Restricted SimpleOpt eliminates an adjacent pack and unpack when the
witness stays in scope. The ordinary optimizer can then process the
result. For a surviving operation, a GHC API check runs the retained
simplifier, call arity, exitification, inward and outward floating, demand
analysis, CPR, worker/wrapper and late demand analysis.
Worker/wrapper produces an unboxed pair worker in this program. The native
output matches the `-O0` program when the caller ignores a package or
demands its payload. The [Core API regression](../../../testsuite/tests/ghc-api/NativeExistentialExpr.hs)
also checks the scope of both operations after worker/wrapper. A recursive
join exits through a strict case on the hidden payload. Exitification lifts
the exit and abstracts the type witness before its payload.

The normal retained `-O1` schedule also runs on definitions that construct
or open an erased package. The vector filter and partition use the default
interface settings. A separately compiled ordinary client reads their
results without forcing the unused tail. The previous compiler can also
compile that client against the new interfaces.
[Filter and partition](../examples/erased-existentials/ImplicitVector.hs).

The specialiser keeps dictionaries that mention the hidden witness inside
its scope. Expression keys in CSE include the package schema, supplied
witness and payload. The divergence check follows the body of an opening.
The source regression opens a package with a local `Show` dictionary and
renders both a `Bool` and an `Int` through it.
[Source regression](../../../testsuite/tests/dependent/should_run/NativeExistentialSource.sh).

A local source rule can refer to functions with erased package types.
The example provider and a separately compiled client both fire
`"open/hide" forall x. open (hide x) = x` at default `-O1`. The reader
checks the rule head, the erased types in the rule and Core Lint.
The interface records a capability for this rule. The previous compiler
rejects that capability at the header. Orphan rules and rules over a
retained function still report GHC-51063. The lexical package checks
below cover rules that construct or open a package directly.
[Package rule](../examples/erased-existentials/PackageRules.hs)
and [source regression](../../../testsuite/tests/dependent/should_run/NativeSimplifier.sh).

Forward congruence under an existential
binder can run through the normal pipeline when its scoped payload
coercion is supported. Core Lint checks the role and the two endpoints.
`transportFiltered Refl xs = xs` preserves the hidden length while changing
the element type under a given equality. GHC-59107 identifies a binding
with other unsupported Core forms.

### Standard `-O2` object code

Liberate-case traverses a package and places the opened witness and value
at the current binding level. It can unroll a recursive call after examining
an outer variable. A case on the value just opened inside the recursion
does not trigger the same unrolling. Constructor specialization substitutes
the witness in the field type before it enters the body. It keeps a case
that binds index equality evidence so that the proof remains in scope.

The native source test specializes a recursive loop over `Maybe` and runs
the generated code. The vector library also checks a constructor field that
becomes live during specialization. The Showcase uses ordinary stream
consumers with integer state and list state. An ignored list tail stays lazy.
[Optimizer regression](../../../testsuite/tests/dependent/should_run/NativeSimplifier.sh),
[Core scope regression](../../../testsuite/tests/ghc-api/NativeExistentialExpr.hs)
and [stream example](../examples/erased-existentials/ErasedStream.hs).

### Lexical package operations in user rules

An optimizer rule can construct or open a package on either side.
`readHidden (x, k) = k x` matches a package at the witness type
`Int`. A call with a `Bool` witness keeps its own callback.
`increment (case p of (x, k) -> k x)` matches the opening even when
the caller uses different names for the two fields.

The provider and a client compiled from its interface each fire all
four rules at default `-O1` and `-O2`. The `-O0` comparison checks
the same results, including a callback that ignores an undefined
value. Readers that only support the earlier `open (hide x) = x`
rule still use it. When they import the four new rules, GHC-47808
identifies the incompatible interface. The two-compiler check covers
all three interface compression levels with the same target libraries.
[Four rules](../examples/erased-existentials/LexicalPackageRules.hs),
[ordinary client](../../../testsuite/tests/dependent/should_run/NativePackageRuleClient.hs)
and [interface check](../examples/erased-existentials/check-rule-interfaces.sh).

## Next increments

- [x] Specify and run an opaque whole-function cast probe. Use genuinely
      abstract payload types and a scoped coercion variable. Check type
      recovery, Lint, CorePrep and object execution with full and partial
      applications. Measure bottom demand and ignored arguments.
      Prepare abstract evidence before supplying reflexivity at the call site.
      The native-object check needs a separate abstract `NOINLINE` binding.
      Inspect that binding after simplification, tidying and actual CorePrep.
      The bounded specification is [M61](../PLAN.md#m61-opaque-whole-function-cast-lowering).
      Scratch checks pass after function-head beta substitution is corrected.
      The actual NCG dump retains the abstract cast. The self-hosted build and
      selected regression cohort pass. The independent audit passes.
      Partial-WHNF and demand checks use bytecode. NCG checks a fully applied
      keep caller and the preserved scalar-domain caller.
- [x] Compile retained module and prompt declarations to interpreted bytecode.
      Keep typed statement results across GHCi commands. See [M87](../PLAN.md#m87-persistent-dependent-definitions-in-ghci).
- [x] Preserve nested certified calls and constructor fields through Core
      Lint and erasure. Give each nontrivial operand a lazy STG binding.
      See [M89](../PLAN.md#m89-nested-computations-in-retained-operands).
- [x] Discharge a local index equality through retained application and
      reflexivity. The value argument remains lazy.
      See [M90](../PLAN.md#m90-equality-for-lazy-local-definitions).
- [x] Check proof use against committed solver evidence before lowering
      an overloaded local definition. See [M91](../PLAN.md#m91-dictionary-evidence-for-a-local-index-definition).
- [x] Compose selected local equalities through constructors and source
      families. Keep a shared runtime binding for each named length.
      See [M92](../PLAN.md#m92-signed-and-grouped-local-lengths).
- [x] Write visible and invisible retained binders, runtime variables and
      opaque applications to HIE files. Recover the typed public binder
      for a local definition. See [M93](../PLAN.md#m93-retained-types-in-hie-files).
- [ ] Specify an opaque Pi instantiation rule if a typed transformation needs
      one. M61's measured whole-cast lowering needs no new primitive. Add a proof
      operation only when a measured or calculus obligation requires it.
- [ ] Integrate the [R23 matchability foundation](R23-unsaturated-type-families.md)
      with native types and coercions. Preserve forward congruence and restrict
      inverse projection to the permitted function kinds.
- [x] Implement M67 generic domain metadata for `[a]`. Update binder types,
      occurrence types, multiplicities, substitution and zonking together.
      Constructor fields at arbitrary `a` need an opaque-field rule. Include
      Int, functions, arbitrary lifted types and `a = Vec Int n`. The last
      case also needs later-domain transport through the earlier runtime n.
      Separate structural containers, opaque values and call permission.
      The bounded source, interface and self-hosted checks pass. General R05
      transformations and broader source domains remain open.
- [x] Admit fixed function leaves in closed algebraic indices. Preserve
      canonical call endpoints, forward family proofs and later package fields.
      M65 passes both builds, separate-module demand controls, Showcase100/16
      and seven registered tests. Generic `[a]` and higher-order endpoints
      remain open. See [the contract](../CORE.md#function-values-in-algebraic-indices).
- [x] Bound the inherited family views in native shape and argument inference.
      Use the solver's reduction-depth policy. Preserve the inverse nominal
      proof for shape matching and the original cached argument usage.
      The bounded contract is [M66](../PLAN.md#m66-bounded-family-views-in-native-frontend-inference).
      Both stages, exact diagnostic spans/contexts, finite controls,
      Showcase100/16 and the seven selected registered tests pass.
- [ ] Specify transport through later runtime-dependent telescope entries.
      Freshen binders and transport remaining fields, targets and evidence
      together. Preserve the distinction between lexical scope and active
      retained-body comparison scope. M67 supplies nominal transport for its
      generic retained-domain contract. General expression and proof
      abstraction remain separate obligations.
- [x] Preserve retained-binder visibility in Core, proof operations and
      interfaces. Check a representational visibility cast through native
      object execution. See [M71](../PLAN.md#m71-visibility-of-retained-binders-in-core).
- [x] Extend one ordinary Core transformation with a measured benefit.
      [M97](../PLAN.md#m97-simplify-a-case-of-an-ordinary-constructor-in-dependent-core)
      removes an adjacent allocation and constructor case. The indexed
      `Just` and two-field examples save 16 and 24 bytes per call.
      Keep a case needed by a type-only dependency or by strict-field
      evaluation. The normal optimization pipeline remains open.
- [ ] Complete native unfolding and coercion-interface handling. Broaden
      optimizer and ordinary GHC compatibility checks before release.

Do not derive nominal domain equality from arbitrary representational Pi
equality. `DepFunCo` can have the role of its body proof; its domain evidence
is nominal. `TermAppCo` has a separate nominal restriction. Ordinary `InstCo`
assumes erased quantification and needs a rule review before reuse here.

## Completion boundary

The historical C1 package is the representation and calculus contract.
C2 is the transformation and compatibility work. Both belong to R05 and remain
open. A paper correspondence is not an implementation proof. A passing probe
closes only its stated behavior. Complete scope, metadata, roles, interfaces,
normal optimization, demand and compatibility obligations before closing R05.

For generic domains, preserve ordinary instantiation through constructor
refinement. Metadata substitution and acyclic zonking come first. Opaque
parameter fields must stay opaque after their type becomes known. Nominal
later-domain transport must retain earlier runtime indices. A preparatory
runtime-independent gate does not complete this generic target. See the
[updated research](../REFERENCES.md#generic-domains-after-matchability-and-function-values).

## Research

- [A Specification for Dependently-Typed Haskell (2017)](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf):
  explicit equality, substitution and erasure with nontermination.
- [A Role for Dependent Types in Haskell (2019)](https://richarde.dev/papers/2019/dep-roles/dep-roles.pdf):
  nominal and representational dependent equality.
- [A Graded Dependent Type System with a Usage-Aware Semantics (2021)](https://richarde.dev/papers/2021/grad/grad-extended.pdf):
  usage and dependent typing. Its assumptions do not combine automatically with
  the other calculi or prove this implementation.
- [System FC as implemented in GHC](https://gitlab.haskell.org/ghc/ghc/-/blob/master/docs/core-spec/core-spec.pdf):
  compare native rules with the current compiler contract.

See [the reference ledger](../REFERENCES.md) and [paper records](../papers/README.md)
for reading scope. The dated ignored research cache has
`core/opaque-pi-elimination-v1.md`, `core/generic-domain-obligations-v1.md`,
`core/generic-matchability-v1.md`, and the Agda telescope review. These are
research records; proposed controls remain unexecuted.

The historical opaque-cast preparation is in
`_build/verification/native-opaque-pi-whole-cast-probe-preparation-v2/`.
M61 completed nine quiet API checks for bytecode and demand, plus separate
object-path checks. Its object-path notes distinguish actual backend preparation
from a preview call. Runtime closure with reflexivity alone cannot establish
that opaque evidence survived an earlier typed phase.
