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
rejects that capability at the header. Rules over unsupported retained
functions still report GHC-51063. The lexical package checks
below cover rules that construct or open a package directly.
[Package rule](../examples/erased-existentials/PackageRules.hs)
and [source regression](../../../testsuite/tests/dependent/should_run/NativeSimplifier.sh).

The provider can declare `forall xs. keep False xs = xs` and
`forall (b :: Bool). keep b (choose b) = choose b`. An ordinary client
imports both rules from the interface. Both fire at default `-O1` and
`-O2`. Reading a runtime Boolean gives 7 or 0. A separate call returns
19 with an undefined index. The same observations hold at `-O0`.
A closed rule can have ordinary types throughout its payload. Its head
still has a retained argument, so the interface records that requirement.
[Provider](../examples/qualified-pi/RetainedRules.hs),
[ordinary client](../examples/qualified-pi/RetainedRulesClient.hs)
and [rule firing checks](../examples/qualified-pi/check-retained-rules.sh).

A later pattern variable can name the earlier runtime index in its type.
In `forall (b :: Bool) (xs :: Vec Int (Length b)). keep b xs = xs`,
the matcher first matches the value of `b`. It then checks `xs` at that
same index. The client can use the rule while its input Boolean is unknown.
The matcher rejects two distinct indices and never inverts `Length`.
[Vector regression](../../../testsuite/tests/dependent/should_run/NativeSimplifier.sh).

The Boolean and its payload can both omit their annotations. In
`forall b x. readKept b x = readChoice x`, the declared arguments of
`readKept` give `b :: Bool` and then `x :: Choice b`. The compiler
checks every occurrence of each variable at this type. The index must
already be in scope when the compiler assigns the payload's type.
The same rule works for two indices and for a polymorphic `Maybe a`
domain. In `forall m x. keepReading @Bool m x = x`, the checked
`@Bool` gives `m :: Maybe Bool` and `x :: Reading m`. GHC reuses
the checked prefix when it checks the call. The prefix can mention
a type variable from a signed rule binder or an earlier runtime index.
Value arguments can also precede the written type argument.
In `keepAfter ignored @Bool m x`, the rule checks `ignored` at `Bool`
and infers `x :: Reading m`. The caller can pass an undefined value
for `ignored`. A test puts a value between two visible type arguments
and uses a noninjective family for the payload type.
In `keepTagged tag @Bool m x`, both `tag` and `m` are retained.
The checked prefix gives the types of the later index and payload.
The list-indexed regression also checks an equality proof after the
list argument. The optimized rule forces the proof and leaves the
two indices lazy. A rule can also use a required type operand written
with the `type` herald. In `keepRequired tag (type Bool) m x`, the
ordinary required-argument check supplies the types of `m` and `x`.
Without the herald, the later rule binders still need signatures.
In `keepTwoRequired tag (type Bool) m (type Char) n x`, each written
type operand gives the domain for the retained argument after it.
The source-hidden client fires the rule at `Bool` and `Char`. A call
with `Char` and `Char` keeps the original function. Both calls leave
all three runtime indices lazy.
An ordinary caller can write `keepReading @_ m x` when the type of
`m` determines the wildcard. The call remains lazy in `m`.
[Source rule](../examples/qualified-pi/VisibleRuleArguments.hs),
[client](../examples/qualified-pi/VisibleRuleClient.hs)
and [checks](../examples/qualified-pi/check-visible-rule-arguments.sh).

A fresh value pattern in a retained telescope checks its ordinary arrow
against `%U`, even when an erased `forall` precedes that pattern. The
same check applies to constructor equations. The solver can infer `%U`
from the pattern. An existing `%M` function can pass through the result
at its full type. An enclosing Given such as `ma ~ U` can justify an arrow
cast in a variable or constructor equation. GHC lifts that cast through
preceding runtime binders without moving their pattern matches. Evidence
from a pattern is in scope inside its row, after the type of that pattern's
arrow has been checked. Proposal #242 specifies unmatchable term-level
function arrows. Its review discusses how term-level functions used in
types should keep their arrow meaning.
[Proposal #242](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#term-level-functions),
[review discussion](https://github.com/ghc-proposals/ghc-proposals/pull/242#issuecomment-502487934),
and [Given and demand checks](../../../testsuite/tests/dependent/should_run/NativeRetainedMatchability.sh).

The two `forall` groups and the lexical scope of explicit type
applications follow the existing GHC RULES syntax. The discussion on
proposal #480 considered how disabling implicit quantification should
affect rule signatures. This experiment uses the current binder rules.
[GHC User's Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rewrite_rules.html)
and [proposal #480 review](https://github.com/ghc-proposals/ghc-proposals/pull/480#issuecomment-1010366717).

A module can declare a rule for a retained function from another module.
In `keep True (tagged n) = tagged n`, the rule module defines `tagged`.
GHC uses it as the rule's local anchor. A rule for `keep False p = p`
has no local anchor and follows ordinary orphan import visibility.
The interface identifies the imported retained head even when every type
in the rule payload is ordinary. An older reader rejects that requirement
before loading the rule. [Source](../examples/qualified-pi/ImportedRetainedRules.hs)
and [separate readers](../examples/qualified-pi/check-imported-retained-rules.sh).

Agda registers an equality proof with `REWRITE` and can use it when
reducing a neutral type. GHC's `RULES` typechecks both sides and gives
the programmer responsibility for preserving their meaning. R05 follows
the GHC contract. The retained index must agree on both sides during
Core checking. A rule firing changes the optimized program and supplies
no equality evidence to a Haskell proof. [Agda rewrite rules](https://agda.readthedocs.io/en/v2.8.0/language/rewriting.html#general-shape-of-rewrite-rules)
and [GHC rule semantics](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rewrite_rules.html#semantics).

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

A module can supply a true orphan rule over an imported function. The
producer and the interface reader check the rule against the imported
head's type. A rule-only import such as `import OrphanRules ()` brings
the rule into scope through GHC's ordinary orphan dependencies. This also
works through a module that imports the orphan. Removing both imports
keeps the rule out of scope. The package matcher still checks the erased
witness at the call. The interface records the orphan rule capability
so that a reader without that capability rejects it at the header.

### A list fold over a hidden stream state

A library defines `toList :: Stream a -> [a]` and exports a rule that
opens the stream and calls a private fold. An ordinary client can fold
the resulting list with Prelude's `foldr`. The local `toList/build`
rule exposes the builder to GHC's existing `fold/build` rule. The rule
matcher gets `a` from the type argument to `toList` before it compares
the existential types. It cannot learn a type through a hidden witness.
Both the integer fold and the polymorphic first-element fold use these
two rules. Skipped states yield the digits 1, 3 and 5 in order. The
first-element fold leaves an undefined tail unevaluated.

The client needs only the provider's interface to compile. The interface
must include a declaration for the private step-fold function. Older
providers could record its name as an unbound local. The list-builder rule has the local head `toList`. A provider can also
write a rule directly for `foldr k z (mapToList f s)`. The local name `mapToList`
anchors the rule. Both the writer and reader check that the anchor still
occurs in its left-hand side. The rule requires the anchored-rule interface
capability. A true orphan with an imported head keeps its original format.
The preceding reader still applies those orphan rules at each interface
compression level. It rejects the anchored rule at the header.
[Two-reader check](../examples/erased-existentials/check-imported-rule-interfaces.sh),
[producer](../examples/erased-existentials/ErasedStream.hs),
[ordinary client](../examples/erased-existentials/StreamFold.hs)
and [rule-firing test](../../../testsuite/tests/dependent/should_run/NativeSimplifier.sh).

The [integer package rule](../examples/erased-existentials/OrphanPackageRules.hs)
is a separate example. Importing its module brings the specialization of
`PackageRules.open` into scope. A Boolean package still uses the original
function. The test compiles the same integer calls with and without the
import and checks which rules fire.

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

## Export retained unfoldings at ordinary signatures

A top-level ordinary signature can use `INLINE` or `INLINABLE` with a
checked retained template. In `inlineIncrement`, the local call
simplifies to an ordinary addition. In `zeroView`, the retained call
remains in the stable template. An ordinary client sees its source tag
before it reads the template and selects the retained optimizer. The
tag travels in the in-memory module details and the interface file.

`zeroViewInlinable` keeps GHC's size guidance. Demand and cast
worker/wrapper preserve the tag. The retained specializer can use
marked imports. An ordinary wrapper compiled with ignored interface
pragmas can introduce a marked call after pipeline selection. The
ordinary optimizer leaves that call intact. A residual local template
can use the same checked tag. Restricted SimpleOpt substitutes its lexical
captures into the stable template. Both float passes keep a marked binding
in scope when moving it would require abstraction. A local template inside
an exported unfolding is checked in its enclosing interface scope.
An unsupported template, such as one with an erased package operation,
reports GHC-92256 at the binding.
[Simplified wrapper](../examples/qualified-pi/IndexBetaClient.hs),
[finite view producer](../examples/qualified-pi/FinitePosition.hs),
[ordinary client](../examples/qualified-pi/FinitePositionClient.hs),
[INLINE regression](../../../testsuite/tests/dependent/should_run/NativeOrdinaryInline.sh),
and [INLINABLE regression](../../../testsuite/tests/dependent/should_run/NativeOrdinaryInlinable.sh).

## Higher-rank wrappers around ordinary and retained arrows

A local helper can accept a named retained consumer with matching
higher-rank arguments. For `run reflectMaybe`, shallow subsumption infers
the callback's type argument from its nominal `Maybe` domain. The cast
checks the whole retained function and preserves demand on the callback
and its runtime index.
[Ordinary client](../examples/open-evaluation/ConstrainedElementsClient.hs).

With `DeepSubsumption`, an ordinary arrow can also
return a wrapper that supplies a dictionary lambda for the retained suffix.
Check the instantiated domain first. Then solve the complete retained
function equality and use its coercion for the suffix. With
`DeepSubsumption`, a visible retained arrow with the same domain on both
sides can also introduce a dictionary immediately below its binder. The
wrapper applies the worker to the bound runtime value and uses that same
value in its result type. The implicit and explicit wrappers preserve lazy
runtime indices. Independent indices still require equality evidence.
[Alternating binder regression](../../../testsuite/tests/dependent/should_run/NativeAlternatingBinders.sh),
[proposal #511](https://ghc-proposals.readthedocs.io/en/latest/proposals/0511-deep-subsumption.html),
and [discussion of dictionary arguments and evaluation](https://github.com/ghc-proposals/ghc-proposals/pull/287#issuecomment-543733379).

### Retained arguments in join points

An optimized client can inline a producer of a GADT and share a case
alternative using a join point. The join takes the runtime index before
the field whose type depends on it. For example, its type can be
`foreach (b :: Bool) -> Value b -> Stored -> String`. The result
`String` has the same type at both jumps. A result that still mentions
`b` after the join consumes its arguments cannot leave this scope.
The simplifier preserves the index when changing the join's result type.
[Source-hidden regression](../../../testsuite/tests/dependent/should_run/NativeRetainedJoin.hs).
