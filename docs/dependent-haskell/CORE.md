# Restricted dependent Core

This document specifies the restricted internal implementation. It is based on the
`nokinds-pi` binder and application work recorded in [PORT.md](PORT.md).
The dependent typing decisions use [Agda as the primary reference](REFERENCES.md#agda-provenance).
Runtime evaluation must obey Haskell's non-strict rules. This document does not
specify the full dependent-Haskell language.

## Representation

The internal `Type` datatype has two new constructors:

```text
TermVarTy b       -- The algebraic index of a runtime identifier.
DepFunTy b result -- A retained dependent function.
```

`b` is a local non-coercion `Id`. Its type is a supported closed algebraic domain.
Its multiplicity is `Many`.
It keeps its identity as a runtime variable. It is not a `TyVar` or a `CoVar`.
The term binder and every reference to its index use the same `Unique`.

`TermVarTy b` has kind `varType b`. Constructor indices use ordinary promoted
constructors, including `True`, `False`, `Z`, and `S n`. `promoteDataCon` supplies
these types through GHC's DataKinds machinery. Nat has no wired compiler identity.
`TermVarTy` adds the link to a runtime variable.
The result of a retained function has kind `Type`.
Dependent runtime representations are outside this subset.

Canonicalize a dependent function whose binder does not occur in its result to
an ordinary unrestricted arrow over its domain. This preserves one representation for a
vacuous retained binder. Lambda typing detects dependence from the result type.
No retainedness flag is added to every ordinary identifier.

`isNativeIndexType` validates a closed graph of lifted, first-order data domains.
Its worklist admits recursive and mutually recursive constructor fields. Exclude
parameters, existential variables, constraints, datatype contexts, newtypes,
type data, unboxed fields, and constructor wrappers. Original and representation
fields must have the same domain. Field multiplicities may be `One` or `Many`.
This policy bounds the Core increment. It is not an Agda language restriction.
The source checker and signature reader now use this structural policy. Nat
source execution and signature imports pass the M9 regression checks.

## Scope and equality

Check `DepFunTy b result` under a context extended by the runtime binder `b`.
Check every `TermVarTy` occurrence against its binding-site identifier and type.
Free-variable collection includes the runtime index and its type dependencies.
It removes `b` under the dependent function binder. Index occurrences are erased
dependencies. They are not additional runtime uses for linearity analysis.

Compare runtime index variables by scoped identity. Compare dependent functions
modulo alpha-renaming of their binders. Do not inspect identifier unfoldings to
equate separately bound runtime producers.

Existing type variables, coercion variables, and their substitutions keep their
current meanings. The new forms must use separate mapper and folder callbacks.

## Lambda and application

If a runtime lambda's body type mentions a supported runtime binder, its type is
the canonical retained dependent function. Otherwise its type is an ordinary arrow.

For application of `DepFunTy b result`, first check the runtime argument against
the binder's domain. Then substitute its index for `b` in the result. The Core
grammar accepts a local index Id or a saturated tree of constructor workers.
Convert the fields recursively and check each field domain. Map workers to their
ordinary promoted constructors. A variable stays opaque even if its unfolding
is a known constructor. No index conversion evaluates a value.

CorePrep names a retained constructor argument with a checked case alias:
`case S k of t { DEFAULT [cv] -> ... }`. Core Lint derives
`cv :: TermVarTy t ~# 'S (TermVarTy k)` from the exact scrutinee. It requires
fresh local binders.
No unfolding or universal coercion supplies evidence. CorePrep emits this rule
only for a constructor in WHNF. It does not force a variable field. The Core
case remains strict by its normal semantics.

Nested fields require composed transport. CorePrep names inner constructors
first and composes their proofs with constructor congruence. It follows the
complete supplied telescope, casts later payloads back to their expected types,
and casts the final result to its original type. SimpleOpt and CorePrep preserve
the proof scope. CoreToStg erases the virtual proof explicitly.

Transport through a remaining dependent function type is gated because Pi
congruence is not implemented. Eta expansion must not hide a bottom function.
Erased binder kinds that depend on a named index are also gated. Alias proof
expressions have no interface codec. See Notes [Native index alias evidence]
and [Retained constructor arguments in CorePrep] and the provenance in
[REFERENCES.md](REFERENCES.md).

The `lazy`/`noinline`/`nospec` stripping path can bypass this preprocessing and
still reject a constructed retained argument. This is a conservative lowering
gap. Strict source matching on a partially applied retained function can also
reach an unsupported Pi constraint in PMC normalization and panic before
CorePrep. Neither boundary is resolved by the constructor-alias rule.

Type computation and Core Lint must perform the same instantiation. A wrong
argument type, unsupported expression, or escaping index must fail explicitly.
Do not silently discard the runtime argument or allocate an unrelated index.

This implements the dependent application rule described in
[Agda's function types](REFERENCES.md#agda-provenance). The Core argument remains
a runtime expression. Its index uses its existing identity in the result type.

## Laziness and eta

The [Haskell evaluation rules](REFERENCES.md#haskell-evaluation-provenance) apply
to retained functions. `keep` must not inspect its unused Boolean argument.
`choose` must inspect its Boolean before it selects a branch. Erasing the branch
proof must preserve that case. Type computation must not evaluate an index.

Haskell's `seq` distinguishes a bottom function from a lambda that returns bottom.
Do not infer a general eta law from the dependent function type. Native eta
expansion must preserve the existing arity and evaluation conditions. It must
also substitute the fresh runtime binder into later argument types. The current
execution test expands the known `keep` lambda. It does not validate eta expansion
of an arbitrary bottom function. Native eta reduction remains disabled in the
scoped SimpleOpt increment.

## Substitution

Type and coercion substitution must account for the existing `IdSubstEnv` when
types can contain runtime indices. An empty type/coercion environment alone does
not justify skipping substitution.

Entering a dependent binder removes its outer substitution and renames it to
avoid capture. Extend the expression environment with the renamed runtime binder
when required. Every occurrence in the result follows that renaming.

Substitution of a supported variable or constructor tree into an index produces
the corresponding index. Substitution of any other expression must fail explicitly
in the initial experiment. It must not leave a stale variable behind.

`composeTCvSubst` composes type and coercion environments. It also composes
supported algebraic index mappings in the expression environment. It preserves
apply-once substitution and the second environment's priority on shared domains.
It does not compose arbitrary Core expressions. Such expressions cannot replace
native indices in this increment.

This restricted grammar is not closed under general Core substitution. The
experiment therefore cannot enter the ordinary optimizer. Backend preparation
must preserve lazy index binding boundaries. The optimizer will also need this
restriction or an internal expression-index form for arbitrary runtime producers.
The source frontend supports the restricted Boolean functions described below.

## Evidence and dependent cases

The existing `Case` result annotation remains outside its case binder's scope.
The implementation preserves this rule.

Boolean elimination uses checked branch-scoped nominal evidence. It keeps the
existing `Case` syntax and result scope:

```text
Case (Var b) case_bndr result
  [ Alt (DataAlt False) [coFalse :: TermVarTy b ~# 'False] rhsFalse
  , Alt (DataAlt True)  [coTrue  :: TermVarTy b ~# 'True]  rhsTrue
  ]
```

The displayed proof types annotate the Core binders. They are not source syntax.
Each proof is a virtual coercion binder. It is not a Boolean constructor field.
The Boolean constructor workers keep their nullary types.

A successor alternative has a real field before its virtual proof:

```text
Alt (DataAlt S) [k :: Nat, cv :: TermVarTy n ~# 'S (TermVarTy k)] rhs
```

Put `k` in scope before checking `cv`. Check its type and multiplicity against
the ordinary constructor worker. Require fresh field identities and a fresh
proof identity. A field that captures `n` could change the expected equality.
The worker still takes only its actual field. An ordinary `S k` alternative
without a virtual proof keeps its ordinary typing rule.

Check the scrutinee first. Derive its index with the same variable/constructor
rule as retained application. Do not derive evidence from an unrelated variable,
the case binder, an unfolding, or an unsupported producer expression.

A proof-bearing Boolean alternative must bind exactly one fresh local `CoVar`.
Its type must be the homogeneous nominal primitive equality from the scrutinee's
index to that exact promoted constructor. Check its type before adding it to
scope. Check its right-hand side with the proof in scope. Keep the proof outside
all other alternatives and the result annotation. Ordinary Boolean alternatives
with no binders keep their existing rule. `DEFAULT` cannot bind a proof.

Require exhaustive alternatives whenever an algebraic alternative binds a proof.
All constructors of its domain or a binder-free `DEFAULT` must cover the input.
Existing Core Lint does not check this property for ordinary algebraic cases.
It is necessary here: an unchecked single impossible alternative could supply a
contradictory proof if later code treated that alternative as unconditional.

Strict constructor discrimination supplies the equation. A diverging scrutinee
reaches no branch. An impossible branch can have contradictory local evidence,
but that evidence cannot justify a result outside the strict case. A branch can
transport its result through the nominal proof and an existing family equation.
No `UnivCo` or plugin axiom supplies the refinement.

This is a new Core typing rule. Its primary design reference is
[Agda's constructor refinement](REFERENCES.md#agda-provenance). The implementation
records that refinement with existing FC coercions. The
[pi-forall implementation](REFERENCES.md#pi-forall) supplies a smaller comparison.
Neither the D/DC proof nor the current Ott formalism establishes preservation
for this GHC rule.

The ordinary optimizer remains gated. Program compilation uses the checked
module entry below. The ordinary simplifier
reconstructs constructor applications from alternative binders. It cannot treat
this proof as a worker field. After checked preparation, CoreToStg removes only
the trailing virtual proof and keeps the actual fields. It erases casts and
preserves strict case discrimination
and ordinary coercion argument positions.
The boundary scanner must detect virtual algebraic proofs even when their types
contain only constructor indices.

Non-reflexive coercions over retained dependent functions require their own
congruence rule. Existing `mkPiCo` must not turn a retained dependent binder into
an ordinary function coercion. Reject unsupported coercion construction.

## Lowering audit for M5

The source audit found these requirements for the restricted bytecode entry.

| Function | Restricted lowering rule |
| --- | --- |
| `CoreToStg.Prep.cpCloneBndr` | Substitute proof types when only the Id environment changes. Cloning `b` must update every `TermVarTy b`. |
| `Core.Opt.Arity.typeOneShots` | Count the retained Boolean as one runtime argument. Otherwise `typeArity` returns zero and Lint rejects positive arity metadata. |
| `Core.Opt.Arity.mkEtaWW` | Allocate a fresh Boolean eta binder and substitute that exact Id into the codomain. Reject non-reflexive dependent congruence until its rule exists. |
| `CoreToStg.vars_alt` | Remove the checked virtual Boolean proof from the constructor payload. The current `filterStgBinders` retains CoVars. |
| `Stg.Lint.lintStgAppReps` | Check native argument representations. The current `splitFunTys` stops at `DepFunTy` and skips those checks. |
| `StgToCmm.Closure.getTyDescription` | Specify supported native profiling descriptions before enabling profiling. |

CorePrep still performs cloning, eta expansion, and saturation at `-O0`.
Disabling the ordinary optimizer does not remove these obligations. Do not change
`splitFunTy` to return a codomain whose retained binder has disappeared. Use an
explicit representation query where the caller needs only argument layout.

Preserve native typing through CorePrep. Check Core again after preparation.
Then erase computational proof uses at CoreToStg while retaining valid layout
metadata. `typePrimRep` obtains representations from kinds. A `Select b :: Type`
result therefore has a fixed lifted-pointer representation. This does not
justify replacing its index with a constructor or inserting an `Any` cast.
Validate every runtime argument's representation separately.

Do not erase ordinary CoVar lambdas or application positions. GHC's
`Note [Coercion tokens]` in `CoreToStg.hs` explains why those positions preserve
`seq` behavior. STG unarisation removes zero-width constructor fields but retains
void function arguments. The new virtual alternative proof is a separate case.
Check that erased index dependencies do not add runtime uses or captured values.

Start with a closed API harness for Core Lint, CorePrep, Core Lint again,
CoreToStg, and STG Lint. Cover both constructor applications, shared runtime
indices, partial application, eta expansion, and proof-binder cloning. Keep the
entry gates until this path passes. Then execute and check strict `choose bottom`
and non-strict `keep bottom` behavior.

`hscCompileCoreExpr` detects native forms and skips ordinary simplification for
these expressions. It must run Core Lint before and after CorePrep. It must run
STG Lint before bytecode generation. CorePrep must disable speculative evaluation
and optional argument arity analysis for this entry. Program CorePrep requires
an explicit checked opt-in. JavaScript execution remains unsupported. This entry
does not enable the ordinary optimizer or interfaces.
The boundary scanner must inspect synonym definitions and original arguments.
CorePrep must keep a lazy local Boolean binding if its RHS cannot replace an
index. A global Boolean producer is one such RHS.

The interpreter's `evalString` executes an `IO String` observation without a host
`unsafeCoerce`. The test must compile the native function before it receives its
Boolean input. An ordinary IO action supplies the input at execution time.
Specializing a closed constructor application does not meet this requirement.

Check `keep` at a bottom index without asserting an index equation. A partial
application of `keepSelect` has function representation even when its index is
bottom. A fully applied indexed identity can also use a value that has a type
at every index. Label these observations separately. Do not cast a non-bottom
`Int` to `Select bottom` to construct the test.

## Program lowering for M8

Compile one native module with ordinary top-level types and local retained
workers. Use `-O0 -fomit-interface-pragmas -fno-write-if-simplified-core`.
The module driver selects `simpleOptNativeProgram` before `core2core`. This mode
simplifies each RHS independently. It preserves top-level Ids, binding groups,
and rules. Force Core Lint before and after simplification and module tidying.
This entry reuses the Agda-derived application and constructor rules above.
It adds no index evaluation or new proof rule.

Program CorePrep requires an explicit opt-in, disabled speculation, no optional
arity analysis, and a mandatory CorePrep Lint configuration. Check the actual
program before and after preparation. Skip `deFloatTop`'s ordinary occurrence
analysis because it can delete a binding used by an index. The expression entry
does not use that cleanup. After CoreToStg erases proofs and casts, force STG Lint
and use the existing Cmm backend. Check the linked binary directly.

Reject profiling, late cost centres, plugins, non-object backends, optimized
module options, and native top-level signatures in this first path. Interface
types and native unfoldings remain separate work. Also reject native-containing
`INLINE`, `INLINABLE`, and default-method unfolding construction in `makeCorePair`.
These builders run ordinary optimization before the module driver sees the
program. Omitting interface pragmas does not prevent that construction.

The executable regression uses a fresh compiler context, an ordinary
`Main (main)` source module, normal module compilation and linking, and direct
binary execution. It verifies one object file and six runtime observations.
No Core expression is extracted for this phase. A source-to-executable check
does not establish cross-module native types or ordinary optimizer safety.

## Native interface signatures

`IfaceDepFunTy` stores an `IfaceIdBndr` and a body. `IfaceTermVar` stores an
`IfLclName` in the local Id namespace. The binder's domain and multiplicity are
outside its scope. `bindIfaceId` allocates a fresh Id and extends the environment
for the body. The decoder checks each binder and occurrence against its supported
algebraic domain and `Many` multiplicity,
and requires result kind `Type`. `Constraint` has a lifted representation but
does not satisfy this result rule. Core Lint, validity checking, and lambda type
construction use the same exact kind check.

Tidy types before conversion. Distinct nested binders must have distinct local
spellings. Binary sharing compares this named syntax; it does not compare raw
Uniques or establish alpha equivalence. `IfaceFreeTermVar` preserves raw free Ids
for diagnostics and cannot be serialized or imported. The ordinary printer-only
interface substitution still excludes binders. It does not become a general
capture-avoiding substitution in this increment.

Tags 10 and 11 encode the retained function and runtime index. Tag 9 remains
the ordinary type literal. Unknown tags fail with an IO error. The configured
fork version is `10.1.20261113.1`, with interface header `1001202611131`.
The existing whole-file version check runs before type decoding, including for
`IgnoreHiWay`. Rebuild matching libraries when moving from the old format.
Nat signatures now import and execute through the bootstrap API and rebuilt
CLI. The preserved Boolean-only reader accepts its old control and rejects the
new file with both way checks. The new reader rejects the old format. Normal module unfolding
generation remains gated. The expression-table path
below reconstructs virtual alternative proofs separately.

The regression uses the real binary tables at all three compression levels.
The executable fixture compiles a producer, then imports only its `.hi` in a
fresh one-shot context. The producer source is outside the consumer import path.
The consumer also checks concrete True/False applications. Those applications
require the imported family equations to reduce to Int and Bool. It compiles
the consumer separately and links both objects. The bootstrap API, self-hosted
registered tests, and separate CLI processes pass these checks. Actual old/new
header reads reject both incompatible directions. The signature fingerprint
check also requires stable hashes after fresh Id allocation and an omitted-body
edit. A family equation change must alter the retained declaration hashes and
recheck an unchanged consumer that does not name the family. The bootstrap
check and all five registered normal-way tests pass. The Agda provenance is recorded in
[the references](REFERENCES.md#agda-provenance).

## Native expression templates

The expression codec reuses `IfaceAlt` names. A wired Boolean alternative has
no runtime fields. One name represents its virtual CoVar. The reader allocates
a fresh CoVar with nominal type `index(scrutinee) ~# promotedConstructor` and
extends only that branch. It does not read proof endpoints from the file.
Ordinary binder-free alternatives keep their existing interpretation.

The reader rejects unsupported scrutinees, malformed proof counts, named
DEFAULT/literal binders, and incomplete proof-bearing coverage. It checks
branch-local and case-binder escape before recovering the result type.
`coreAltType` would otherwise panic on an escaping binder. These are checks for
the new forms, not a verifier for all malformed legacy interface expressions.

Each native imported unfolding runs mandatory Core Lint inside its existing
`forkM` thunk. This includes nested unfoldings and declarations whose retained
signature has an ordinary but incorrect RHS. After Lint, compare the recovered
type with the declared type through `eqType`. Report named interface failures.
Native rule and dictionary unfolding payloads remain explicitly unsupported.

`mkCoreUnfolding` preserves native templates instead of running ordinary
occurrence analysis. The decision stays inside `uf_tmpl`. Inspecting `uf_src`
does not force the RHS. Preserve the serialized cache. An own-RHS scan skips
bound metadata for deferred validation. A separate construction scan includes
the stable templates, rules, and dictionary metadata that occurrence analysis
would traverse. It leaves unused unstable unfolding templates deferred.

The API regression checks all compression levels, actual declaration
reconstruction, types, scope, fresh proof identities, cache preservation, and
laziness with optional Core Lint disabled. The normal module writer still omits
unfoldings. Low-level file writers can bypass that module gate. Actual native
expression files, a new format distinction, and cross-module template use remain
separate work. See the two GHC Notes in `GHC.IfaceToCore` and
`GHC.Core.Unfold.Make`, and [the Agda comparison](REFERENCES.md#agda-provenance).

## Scoped native SimpleOpt

This increment passed the Core API and bytecode regressions at `0bbc32d396`.
It adds `simpleOptNativeExpr` as a separate entry. It does not enable the full
simplifier or the ordinary program optimization pipeline.

The native mode reuses the SimpleOpt traversal with restricted rewrites. It
skips occurrence analysis, general inlining, compulsory unfolding, binding
dropping, eta reduction, and cast pushing through lambdas. It also disables
generic known-constructor case elimination.
It creates no rules, Core unfoldings, or join points. Unfolding construction
would invoke ordinary occurrence analysis again. Existing join points remain.

Dependent beta reduction can substitute an accepted local Boolean variable or
an exact Boolean constructor. An unsupported producer keeps its application
redex. A virtual-proof case can reduce only when its scrutinee is an exact
syntactic Boolean constructor. The substituted proof must be nominal primitive
equality whose endpoints are that same promoted constructor. Replace that proof
with checked reflexivity before substituting into the selected branch.

Keep variable cases and lazy source lets. Do not inspect unfoldings to decide an
opaque index. These rules implement dependent substitution and checked case
refinement from [the design references](REFERENCES.md#agda-provenance). They do
not add arbitrary expression indices, dependent congruence, or general proof
erasure. Run Core Lint before and after this entry. Retest the shared runtime
index and bottom observations before claiming optimizer preservation.

## Frontend identity plan

The checker now supports structural domains, outer erased quantification,
retained telescopes, and bare constructor fields. The bootstrap API executes
source Nat/Vec functions through bytecode and separate-module machine code.
The rebuilt CLI and registered self-hosted fixture pass the same module checks.
Refinement in source `case` expressions remains open. Retained signatures bind
ordinary runtime values. The Boolean checkpoint below uses signatures such as
`foreach (b :: Bool) -> Select b`. No singleton conversion is required.

The Boolean checkpoint preserves one runtime identity through these stages:

1. Parse a distinct retained telescope in `Parser/Lexer.x`, `Parser.y`, and
   `Language/Haskell/Syntax/Type.hs`. Reuse located binder syntax and exact source
   annotations. Do not encode the syntax in strings, plugins, or erased foralls.
2. Rename the retained binder as a term name that is available in its result
   type. Allocate one `VarName`. Add a scoped type-lookup alias to that same
   `Name` in `LocalRdrEnv`. An inner `forall` must shadow this alias through the
   existing local environment. Do not create a second Name with the same Unique.
   Do not enable general term-name lookup in types. Check each binder annotation
   as a value's domain type before introducing that binder.
   In `Tc/Gen/HsType.hs`, check the `Bool` domain and lifted result. Keep a scoped
   retained-index environment in `TcTyMode`. Permit `TermVarTy` only for a
   registered Boolean Id. Preserve ordinary `ATcId` rejection for other names.
   Apply term staging checks or reject retained staging constructs explicitly.
3. Start with one equation and a bare retained variable pattern. Construct its
   Boolean Id from the pattern's renamed `Name`. Substitute that Id into the
   codomain before decomposing the remaining argument types. `tcPatBndr` keeps
   that `Unique`. `selectMatchVar` reuses the checked variable Id, and `wrapBind`
   adds no alias when the Ids agree. Reuse `MatchGroupTc` unchanged for this
   subset. Require an identity result wrapper. Constructor patterns and multiple
   equations use the shared scrutinee and branch-index rule below.
   RequiredTypeArguments instead skolemises type variables.
4. In `Tc/Gen/App.hs`, check a retained argument as a value and instantiate the
   result with its accepted index. Keep the value application. Do not use
   `tcVDQ`, which translates an expression into a type argument.
5. Check Boolean constructor equations with one shared hidden Bool/Many Id.
   Generated typed `AsPat` nodes make `selectMatchVar` preserve this identity.
   Keep `MatchGroupTc` unchanged. Specialise both later argument types and the
   result type for each constructor, as Agda's `splitCon` does. Check later bare
   variable patterns and the RHS against these types. Use coercion patterns to
   transport the original arguments. Apply the inverse result coercion to the
   RHS. Keep the original types in `MatchGroupTc`.
6. Store each fresh nominal Boolean proof in `ConPatTc.cpt_dicts`. Constructor
   desugaring places these binders in the strict Core alternatives. Build
   transport with existing FC lifting over a fresh immutable type template.
   This template adds no witness or axiom. Do not request general solver
   rewriting of a runtime index. Preserve opaque native equalities as potentially
   soluble irreducibles, so PMC does not treat the proof as a contradiction.
   Require one unguarded equation for each Boolean constructor. Later arguments
   initially require bare variable patterns and `Many` multiplicity.

Native zonking and the required reflexive unification paths now support this
source subset. Unsupported source constructs must produce located errors before
other internal guards. Keep binder-domain, unsupported-index, coverage,
and branch-result errors at their respective source locations.
Enable the first rule only at the root of a complete explicit value signature.
Keep the generic `HsForEachTy` rejection for unsupported source positions.
This prevents native types from reaching synonym, data, class, and role checks
before those checks support them. Reject partial signatures and embedded retained
quantification before constructing native types. Start execution with local
declarations, as specified below.
`zonkMatchGroup` checks group annotations outside the pattern environment. For
the first subset, preserve the prototype Id's fixed `Bool` type, `Many`
multiplicity, and `Unique`. Do not consult the recursively tied runtime Id
environment for an index occurrence in a type. GHC's existing
`Note [The ZonkEnv]` explains this knot. Runtime expression occurrences still
use the ordinary Id lookup.

`kindGeneralizeSome` also traverses the complete signature. Its candidate
collector must treat a runtime index as rigid. It must not generalize that Id.
Validity checking must check scope, domains, multiplicities, result kinds, and
ordinary rank rules inside the retained body. Equal native types can use the
existing reflexive unification path. This does not permit a metavariable to
capture an untracked runtime index. Solver rewriting and native mismatch paths
still need conservative handling or precise source rejection. Reject inferred
types that contain native forms before ordinary binding generalization or
normalization. An unsigned alias can obtain such a type without assigning a
type metavariable. Keep complete signatures on their checking path.

A constraint in the result can require a nonidentity match wrapper. For example,
`foreach (b :: Bool) -> ((b ~ b) => Int) -> ((b ~ b) => Int)` falls outside the
first identity-wrapper subset. A fully applied laziness observation can instead
use `foreach (b :: Bool) -> ((b ~ b) => Int) -> Int`. Its body instantiates the
argument's constraint. It does not require a constrained result wrapper.
The observation uses checked rigid-index handling and scoped reflexivity.
Constraint instantiation builds a boxed `(b ~ b)` dictionary directly at its
call site, with the existing `evDictApp` and nominal reflexivity constructors.
A Wanted dictionary could receive a solver evidence binding outside the runtime
lambda. The direct expression preserves its scope. Other index-dependent
predicates remain gated until the solver has a runtime-index evidence scope.
Opaque reflexive rewriting and same-Id equality remain supported in the solver.
Do not inspect Boolean unfoldings.
Pure family matching also preserves opaque indices. The same Id matches itself.
Different Ids can still hold the same Boolean. A constructor and an opaque Id
can also coincide. These comparisons are `MaybeApart MARRuntimeIndex`, rather
than `SurelyApart`. A closed family must not skip an earlier constructor clause
because an index is unknown. Repeated template-variable targets preserve this
uncertainty. Ordinary family template variables can still bind indices through
the immutable matching substitution. This does not assign a Tc metavariable.
Ordinary constructor instantiation must also preserve this scope. The M9 route
matches an expected indexed result against outer immutable type variables.
It checks kinds, the exact instantiated result, and the current runtime Ids.
It emits explicit type applications. After an explicit source type application,
it can match the remaining erased indices before allocating metavariables.
An ordinary consumer can instead infer its index-bearing arguments once and
match their types. The cache retains checked expressions and usage records.
Payload arguments still use expected-type checking. These routes require
`NoQL`; argument-directed matching also requires the experimental flag.
The mutable unifier's index escape checks remain unchanged.

Ambiguity checking opens each retained binder with one shared opaque runtime Id.
It compares ordinary arrows under those Ids and retains the outer erased
quantifiers. The check discards its wrapper and evidence. It does not require
a retained-function coercion. Noninjective erased payload families still fail
the ordinary ambiguity check. See Note [Ambiguity checking retained signatures]
in `GHC.Tc.Validity` and the Agda provenance references.

Keep `foreach` an ordinary identifier when the experimental flag is disabled.
Its proposed syntax can also parse as an ordinary type-variable application.
Do not reserve that spelling to produce a missing-flag error.
Use typed, located `TcRnMessage` diagnostics for unsupported retained forms.
Reuse ordinary diagnostics only when their meanings match. An unsupported `Int`
domain is not a kind mismatch. A retained argument is not an erased required type
argument. Ordinary inferred functions remain valid without a retained signature.
Error-context formatting can itself invoke zonking. The native zonkers above
support this subset. Reject other forms before unsupported traversals.

Start execution with local retained declarations inside an ordinary
`entry :: IO String`. Parse, rename, typecheck, and desugar real source. Pass its
closed Core right-hand side to the checked native bytecode entry. Retain its
module-level evidence bindings around that expression. This exercises
the frontend without enabling native interfaces or the ordinary module pipeline.
General computed-scrutinee abstraction remains separate work. Agda's
[with-abstraction reference](REFERENCES.md#agda-provenance) explains its effect on
the goal and dependent context.

## Initial validation boundary

Check hand-built Core for retained identity and application. Cover alpha-renaming,
capture avoidance, variable and constructor substitution, non-Boolean indices,
wrong applications, and escaped variables. Compare Lint results with type recovery.

The native forms print in diagnostics. The API regression checks dependent
Boolean cases, `Select True = Int`, `Select False = Bool`, and composition of
`choose` and `keep` at one shared runtime index. It also checks malformed proof
types, incomplete proof-bearing cases, scope errors, and supported substitution
through branch proofs. These checks construct and typecheck Core. They do not
execute the dependent functions.

The separate runtime regression compiles each native kernel once. The same
compiled function receives both Boolean values through an ordinary IO action.
It checks `choose`, shared `keep`/`choose`, partial `keepSelect` at a bottom index,
fully applied dictionary-index `keep` at a bottom index, and strict `choose` on
bottom. The prepared Core and STG retain one Boolean argument. The index-only
type dependencies add no captured runtime values. Virtual branch proofs disappear
before bytecode generation.

The runtime regression checks real source `keep` and constructor-based `choose`.
It observes both runtime Boolean inputs, shared `keep`/`choose` composition, a
fully applied indexed bottom, both concrete `Select` results, and a partial
`keep bottom` at function WHNF. A payload-only bang exposes strict `choose`.
Refinement in source `case` expressions remains open. Interface serialization
and the ordinary optimizer remain gated. The restricted program entry above
supports local workers with ordinary exported types. Existing ordinary GHC
programs must continue to build.
