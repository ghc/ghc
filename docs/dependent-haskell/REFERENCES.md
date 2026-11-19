# Dependent Haskell references

The original survey was checked on 2026-11-11. The Agda and Haskell evaluation
references were checked on 2026-11-12. The GHC source baseline is
[`19ff36f2aede0ca2e554016ff22c71e73aa9803b`](https://github.com/ghc/ghc/commit/19ff36f2aede0ca2e554016ff22c71e73aa9803b).
Later changes to proposal status, package bounds, branches, or documentation need
verification. A link to a mutable branch identifies the inspected source, not a
permanent compatibility guarantee.
The Pi normalization sources below were checked on 2026-11-13.
The first seven research works were fetched and read on 2026-11-14. The
roadmap review extends the collection on 2026-11-16 to sixteen works and
twenty-two PDF versions. See the
[paper reading ledger](papers/README.md) for page-specific findings and the
[version manifest](papers/manifest.json) for PDF hashes and reading
scopes. This includes selected thesis chapters, not the full thesis.

See [the project overview](README.md) for the objective. See [the implementation
plan](PLAN.md) for scope, decisions, and acceptance criteria. The first experiment
extends Core from Eisenberg's historical branch. The singleton translation remains
a comparison implementation. External solvers remain later work.

## References by roadmap feature

The [program roadmap](ROADMAP.md) owns the work queue and the upstream task
index. The 2026-11-16 review adds feature pages that connect research to the
next implementation obligations:

| Feature | Research to use for the next specification |
| --- | --- |
| [R03: dependent products](roadmap/R03-dependent-products.md#research) | Proposal #378, pinned `singletons` source, and Agda telescope substitution and refinement. Separate source checking from runtime demand. |
| [R05: dependent Core](roadmap/R05-dependent-core.md#research) | The D/DC specification, dependent roles, GraD and current Core. Review opaque Pi elimination and substitution of generic domain metadata. |
| [R23: unsaturated type families](roadmap/R23-unsaturated-type-families.md#prior-art-and-current-boundary) | Proposal #242, the higher-order type-level paper and the pinned 2024 implementation. Read the native integration correction with the draft. |

These pages record the selected implementation sources, open questions and
unexecuted controls. The paper ledger records additional versions and selective
readings. It preserves the older version records and their reading scopes.
The dated ignored research cache retains the
larger link inventory and exact retrieval records. Keep the tracked summaries
and primary links usable without that cache.

## Reading order

For M59, use the same-domain congruence and scoped comparison references in
[the fixed-domain contract](CORE.md#fixed-function-domain-body-comparison).
The implementation uses the existing `DepFunCo` with `MRefl` at an unchanged
function domain. It does not derive function equality. The private body solve
and separate known-value selection keep their existing scope and native-meta
limits. See Note `[Solving retained function equalities]` in
`GHC.Tc.Solver.Equality` and the M59 source controls.

The later [R23 review](roadmap/R23-unsaturated-type-families.md#research-records)
records the prototype's invisible-forall constructor shortcut and current
required-forall visibility bridge. The [R05 probe plan](roadmap/R05-dependent-core.md)
separates abstract CorePrep checks from bytecode and native-object evidence.
Those prospective controls remain unexecuted and do not enlarge M59's scope.

1. Read [the Agda provenance](#agda-provenance) and
   [the Haskell evaluation constraints](#haskell-evaluation-provenance).
   Separate the dependent typing rules from runtime evaluation rules.
2. Read proposal #378 and the current RequiredTypeArguments documentation. Separate
   dependence, visibility, and erasure before interpreting the other proposals.
3. Inspect the pinned `nokinds-pi` branch and its incomplete application and case
   handling. Compare it with current type computation, substitution, and Core Lint.
4. Read the D/DC specification and artifact, the roles paper, and GraD. Identify
   which rules the native extension needs and which proof assumptions it must meet.
5. Read pi-forall's Boolean checking rules as a comparison implementation.
6. Read the singletons papers, SHE's pi-type translation, and the upstream API as
   comparison implementations. The TH generator remains relevant to promotion.
7. Read the solver and proof-certificate references only when the language needs
   additional arithmetic or theory reasoning. The first Boolean experiment does not.

## Agda provenance

Agda supplies the primary dependent-language reference for the spike. The links
below use the Agda 2.8.0.2 manual. The decisions describe this GHC implementation;
the manual does not prove those decisions correct in System FC.

- [Function types](https://agda.readthedocs.io/en/v2.8.0.2/language/function-types.html)
  gives the dependent application rule: the result of `f a` has type
  `B[x := a]`. The native implementation substitutes the argument's index into
  the codomain. A variable index uses the same runtime `Id`. It does not allocate
  a second witness. An opaque Boolean stays opaque until constructor inspection.
- [Function definitions](https://agda.readthedocs.io/en/v2.8.0.2/language/function-definitions.html)
  describes constructor-pattern refinement and case trees. Agda also checks
  coverage and termination. The GHC rule records a constructor refinement as a
  branch-local nominal `CoVar`. Its explicit coercions transport the branch
  result to the fixed outer result type. Exhaustive proof-bearing Boolean cases
  are required. These checks do not establish termination. Optional totality
  checking is now a separate local task in the roadmap.
- [With-abstraction equality](https://agda.readthedocs.io/en/v2.8.0.2/language/with-abstraction.html#with-abstraction-equality)
  explains how `with ... in eq` retains an equation for an inspected expression.
  It motivates keeping explicit refinement evidence until checked lowering.
  Agda's more general abstraction also changes dependent context types.
  That elaboration remains outside the current variable/constructor index subset.
- [Run-time irrelevance](https://agda.readthedocs.io/en/v2.8.0.2/language/runtime-irrelevance.html)
  separates erased information from values that drive computation. The native
  Boolean remains relevant because `choose` inspects it. Index occurrences and
  virtual branch proofs add no runtime witness. Erasure must preserve the strict
  Boolean case. Agda's treatment of erased higher-order arguments also shows why
  erasure needs a calling-convention specification.

Constructor checking also follows the pinned Agda v2.8.0 implementation.
Its [`checkPiTelescope` and `checkTelescope'`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L263-L290)
check each binding before checking the remainder in the extended context.
The source checker follows that order for retained binder lists and successive
`foreach` groups. Ordinary outer `forall` binders retain GHC's erased scope.
The native matcher follows both ordinary arrows and dependent binders because
Core canonicalises an unused retained binder to an ordinary arrow. The pinned
`Term.hs` blob is `02991c9fa0085315cc854d2833a9080af14a7740`.

Agda compares Pi codomains under one shared extended context.
[`equalFun`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L746-L754)
calls [`compareDom`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L827-L849),
which checks the domains and extends that context before comparing the bodies.
The pinned `Conversion.hs` blob is `69423e2c11b6f35bc5eee2d502cf2830efbc005a`.
The GHC ambiguity check opens each retained binder with one shared opaque runtime
Id. It compares temporary ordinary arrows under those Ids and retains outer
erased quantifiers. It discards the wrapper and evidence. This checking-only
projection is a GHC adaptation. It does not extend FC coercions or disable
ordinary ambiguity checks. A noninjective payload family remains ambiguous.

Expected-result instantiation uses a separate GHC adaptation. It matches an
ordinary constructor or helper result against a checked expected type. It binds
only that function's outer immutable type variables. It checks each type argument's
kind, the resulting type, and every runtime Id's current local scope. It emits
explicit type applications. It does not assign a metavariable to a runtime index.
An erased-index consumer can need an index that is absent from its result.
For that case, infer its index-bearing arguments once and match their types.
Cache the checked expressions and usage records. Check payload arguments against
their expected types. This path requires the experimental flag and `NoQL`.
It does not replay source checking when matching falls back to ordinary
instantiation. The mutable unifier's runtime-index escape guard remains active.
The native application Note states the restrictions. Agda's pattern-context
substitution below supplies the context discipline; it does not establish the
correctness of GHC's matcher or its FC evidence.

[`LHSResult`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L594-L620)
records the pattern-context substitution and specialised body type.
[`splitCon`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1503-L1532)
applies the constructor substitution to the later argument telescope, target,
and remaining pattern equations.
[`checkRHS`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Def.hs#L825-L852)
checks the body against that target. Thus refinement must update argument types
as well as result types. The native frontend uses explicit coercion patterns
and inverse result casts to connect these specialised types to the outer types.

Agda's [coverage rule](https://agda.readthedocs.io/en/v2.8.0/language/coverage-checking.html#single-match-on-a-non-indexed-datatype)
requires each constructor or a catchall. The first GHC constructor subset requires
one unguarded equation for each Boolean constructor. General catchalls remain
separate work. Agda's [dependent case combinator](https://agda.readthedocs.io/en/v2.8.0/language/with-abstraction.html#alternatives-to-with-abstraction)
needs an explicit motive and does not automatically refine existing arguments.
Constructor equations therefore precede dependent Haskell `case` expressions.
This rule does not transfer Agda's totality assumptions to lazy Haskell.

Module integration reuses these application, constructor, and erasure rules.
It keeps the relevant Boolean as one ordinary runtime argument and removes
only checked proof binders. Preserving lazy index bindings is a Haskell
requirement, described below. The first module path's restrictions on profiling,
plugins, interface bodies, and unfolding builders are GHC implementation limits.
They are not restrictions from Agda's source language. Core and STG Lint plus
direct binary observations supply bounded implementation evidence. They do not
establish a compiler-preservation theorem.

The pinned Agda backend supplies a concrete comparison.
[`substArg`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Compiler/ToTreeless.hs#L589-L594)
translates usable arguments and replaces unusable ones with `TErased`.
MAlonzo's [`intros`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Compiler/MAlonzo/Compiler.hs#L797-L817)
extends one variable context. Its
[`TVar`, `TLam`, `TLet`, and `TCase` rules](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Compiler/MAlonzo/Compiler.hs#L914-L931)
use that context for ordinary Haskell variables, bindings, and case inspection.
These rules introduce no singleton witness. Agda also prunes unused arguments
and inserts backend coercions. Those mechanisms do not prove native FC scope
or coercion preservation.

Agda's own
[`etaContractErased` comment](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Compiler/ToTreeless.hs#L507-L547)
uses total, effect-free semantics to justify erased-lambda eta contraction.
That assumption does not cover a relevant lazy Haskell Boolean. Keep the
separate Haskell `seq` and bottom observations below.

The signature interface representation also has a direct Agda comparison.
Agda's [internal syntax](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/Internal.hs#L204-L245)
represents variables with de Bruijn indices and dependent functions with `Pi`
and `Abs`. Its [serializer](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Serialise/Instances/Internal.hs#L97-L133)
keeps the variable index, domain, body, and distinction between `Abs` and
`NoAbs`. This preserves the binding structure on disk. GHC instead uses its
existing lexical interface names and local Id environment. Import allocates a
fresh Id for each retained binder and resolves its body occurrences there.
No raw Id Unique enters the new wire format. Tidy nested binders before
conversion so their local spellings remain distinct.

Agda's [interface version policy](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Serialise.hs#L9-L14)
requires a version change when serialization changes. The fork uses GHC's
existing whole-file version check with the explicit configured snapshot
`10.1.20261112.1`. Unknown type tags also fail explicitly. Neither the Agda
codec nor its version check proves that arbitrary decoded terms are well scoped.
GHC's new decoder checks its Bool/Many binders and index lookups separately.

Agda's [import validation](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Interaction/Imports.hs#L840-L891)
compares stored imported-module hashes with their current hashes. A mismatch
requires rechecking. This supports checking dependent signature changes through
the existing interface dependency mechanism. GHC uses finer declaration hashes;
see `Note [Fingerprinting IfaceDecls]` in `GHC.Iface.Recomp`. The regression
changes a closed-family equation beneath retained function signatures, then
requires a consumer recheck without `-fforce-recomp`. Fresh binder identities
must preserve these hashes. With unfoldings omitted, a body-only edit must also
preserve the signature ABI. That last rule follows GHC's ABI policy; Agda's
module-hash check does not establish it.

Boolean expression reconstruction also uses Agda as a comparison.
[`splitCon`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1464-L1532)
unifies constructor indices and applies the resulting substitution to the
context and target. Agda's [case trees](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/CompiledClause.hs#L35-L74)
and [serializer](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Serialise/Instances/Internal.hs#L480-L513)
retain constructor structure, arities, scoped branches, and bodies. They do not
store an FC-style virtual equality binder. GHC therefore reconstructs that
binder's nominal type from the actual Boolean scrutinee and constructor. It
does not read trusted proof endpoints from the file. The proof remains local
to the branch reached by strict discrimination. Agda's totality and definitional
equality do not justify changing Haskell's bottom or `seq` behavior.

The unfolding implementation also follows GHC's own lazy import contracts.
`Note [Lazily checking Unfoldings]` and `Note [Tying the 'CoreUnfolding' knot]`
in `GHC.IfaceToCore` require deferred templates and preserved cache fields.
Native validation runs in each existing `forkM` thunk. It checks Core and the
declared signature. A separate template scan follows the stable metadata that
ordinary occurrence analysis would traverse, while leaving unused unstable
templates deferred. This is a GHC adaptation, not an Agda serialization rule.

The next Nat/Vec slice follows pinned Agda library definitions.
The standard library's [vector constructors](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda#L36-L38)
index the datatype by Nat. Its [replicate](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda#L246-L248)
takes a length value and returns `Vec A n` by structural recursion. Its
[append](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda#L108-L110)
recurses through vectors, and [head](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda#L53-L54)
matches a nonempty vector. GHC's retained append wrapper can use an ordinary
erased-index GADT helper. A GADT predecessor type variable is not a runtime Nat
argument. Agda's implicit argument syntax does not itself imply erasure.

Agda's [Nat and addition](https://github.com/agda/agda/blob/v2.8.0/src/data/lib/prim/Agda/Builtin/Nat.agda#L8-L22)
use zero/successor recursion. Agda marks them as builtins. The GHC target uses an
ordinary user-defined Peano datatype and a closed `Add` family. The sources
support the typing and recursion design; they do not establish Haskell runtime
behavior. Check partial and cyclic Nat values separately. In particular, a
successor does not force its predecessor, and finite prefixes of a cyclic
replication must remain observable. Keep opaque runtime indices conservative in
equality and apartness. No Agda build was run for this comparison.

The first algebraic Core increment uses this constructor/refinement precedent.
`TermVarTy` takes its kind from the same runtime Id's domain. `S k` becomes the
ordinary promoted application to that field's index. A native alternative scopes
its actual fields before one trailing nominal proof. The closed, first-order,
wrapper-free domain policy is a GHC implementation limit. It makes the source
constructor fields match the worker fields. It is not a restriction copied from
Agda. The Core Notes name this policy and the fresh-field scope rule.

Agda 2.8.0 distinguishes a known function head from full normalization.
[`slowReduceTerm`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Reduce.hs#L563-L586)
returns a `Pi` head unchanged. The
[fast reducer](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Reduce/Fast.hs#L962-L966)
also treats function types as WHNF.
[`ifPiB`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Telescope.hs#L650-L668)
can inspect that head immediately. In contrast, the
[full normalizer](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Reduce.hs#L1270-L1283)
normalizes the domain and codomain.

The same normalizer enters a dependent body through
[`Normalise (Abs a)`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Reduce.hs#L1299-L1301).
[`underAbstraction_`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Monad/Context.hs#L389-L413)
adds a dummy-domain context to maintain de Bruijn scope. It does not recheck the
body's type. Substitution under an
[`Abs` lifts the substitution](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Substitute.hs#L1058-L1061);
[`piApply`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Substitute.hs#L599-L602)
instantiates the codomain with the actual argument.
At tag `v2.8.0`, the `Reduce.hs` file blob is
`63fafb0b1ab734b6902ba5eb99b40af43de833cc`; the `Substitute.hs` file blob is
`1ef41f6d2a0223c8328984903b60dabc2a05a783`.

Full normalization preserves neutral heads and normalizes their arguments.
The compiled clause matcher
[returns `NoReduction` when discrimination is stuck](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/CompiledClause/Match.hs#L180-L194).
This supports scoped reduction under an opaque variable. GHC still uses its own
family apartness rules. It must not infer a constructor from an Id unfolding.
The separate
[`instantiateFull'` routine invokes `etaOnce`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Reduce.hs#L1484-L1498).
Do not copy that routine's eta behavior into Haskell normalization.

The GHC increment normalizes global family axioms under a fixed closed-domain,
`Many` retained binder. `mkDepFunRedn` supplies scoped FC evidence and separately
canonicalizes each endpoint. The Core lifting context permits a shared valid
same-domain variable alpha-renaming. The solver's `re_native_binder` mode excludes
plugins, inert evidence, family-cache reads and writes, and coercion zapping.
It keeps roles, reduction depth, source locations, and ordinary meta-variable
reading. This is family-axiom normalization, not full normalization with ambient
Given evidence or structural retained unification. It changes no ordinary
`ForAllTy` binder-kind path. Agda supplies the scope precedent; it supplies no
preservation proof for these FC rules or Haskell bottom and cyclic values.
Family axioms reduce at nominal and representational roles. Existing phantom
normalization leaves family heads unreduced. Ordinary forall binder kinds still
normalize at nominal role. The equality canonicalizer rechecks rigid-index and
retained-type reflexivity after rewriting both sides, without structural
decomposition or meta-variable assignment. See Note
[Reflexivity after retained normalization] in `GHC.Tc.Solver.Equality`.

The PMC increment uses this distinction. A known retained function head does
not need component rewriting to classify the scrutinee as a function. An
ordinary arrow that contains a retained type has the same outer-shape property.
The rule preserves that head before the ordinary solver would require rewriting
under retained binders. GHC's immutable matcher also returns a conservative `MaybeApart`
for unsupported retained-function comparisons when it filters COMPLETE sets.
This is a GHC implementation policy. It does not claim Agda uses this matcher.
Whole-type template substitution retains the existing escape and occurs checks.

Agda's head-reduction rule does not prove Haskell inhabitation or evaluation
behavior. A Haskell lambda can inhabit a function type whose result has no
constructors. A function expression can still be bottom. Coverage checking must
retain possible inhabitation, and a strict wildcard must retain its forcing.
The coverage-checking rule adds no dependent congruence or Tc metavariable assignment.

Agda v2.8.0 compares
[Pi domains before their codomains](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L747-L754).
[`compareDom`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L827-L849)
checks modalities and the domain, then extends one shared context for the body.
This is type equality. The separate
[function-term equality rule](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L437-L442)
uses extensional comparison and supplies no Haskell eta law.

D/DC's An-PiCong rule supplies the explicit coercion precedent. See Figure 8,
page 31:16 of
[A Specification for Dependent Types in Haskell](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf#page=16)
and the
[pinned Ott rule](https://github.com/sweirich/corespec/blob/ee2d477fb26f3d0155be438a93a2c1427511cb24/spec/ett.ott#L1304-L1311).
The artifact includes later eta work; this citation names the Pi congruence rule.
Equal domains specialize its domain-cast substitution to identity.

`DepFunCo` uses that specialization: one shared unrestricted runtime binder,
one unchanged supported closed domain, and a checked body coercion. Both result
endpoints have kind `Type`. The coercion inherits its body's FC role, as ordinary
function congruence does. Agda supplies the shared-context precedent; the role
rule is the GHC adaptation. Keep the runtime binder scoped through the proof,
including intermediate types that no longer occur in the final endpoints.
Each endpoint separately canonicalizes an unused binder to an ordinary arrow.
See Note [Retained dependent function coercions] in `GHC.Core.TyCo.Rep`.

Preserve a cast on the function value. Replacing it with a lambda can hide a
bottom function in Haskell. The
[PiSnd](https://github.com/sweirich/corespec/blob/ee2d477fb26f3d0155be438a93a2c1427511cb24/spec/ett.ott#L1340-L1345)
and [Push](https://github.com/sweirich/corespec/blob/ee2d477fb26f3d0155be438a93a2c1427511cb24/spec/ett.ott#L1517-L1522)
rules also identify the obligation to instantiate an application with its actual
index. The fixed-domain increment adds no erased-forall projection or `InstCo`
rule. Changed domains, multiplicities, and native unfolding serialization remain
separate work. Diagnostic interface conversion must print the new proof before
the binary writer rejects unsupported serialization.

Agda's [dependent case combinator](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/with-abstraction.lagda.rst#L710-L741)
checks a pattern function against an explicit motive and applies it to the
scrutinee. The standard library calls it
[`case_returning_of_`](https://github.com/agda/agda-stdlib/blob/v2.3/src/Function/Base.agda#L110-L116).
The manual also states that this combinator does not refine other arguments
automatically. Agda's
[with-abstraction context generalization](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/with-abstraction.lagda.rst#L910-L936)
supplies the separate precedent for abstracting dependent context arguments.
Its compiler checks an
[extended lambda against its expected type](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L831-L866),
then checks an auxiliary function and applies the captured context.
[`splitCon`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1464-L1532)
applies constructor substitution to the remaining telescope and target.

The source `case` increment adapts these rules to the existing retained
constructor checker. The checked result type supplies the motive. Each
referenced local whose type depends on the scrutinee becomes a fresh later
argument. Constructor substitution refines direct uses of the original index.
Fresh resolved Names preserve field shadowing. Template Haskell quotations are
gated because source substitution must preserve quoted Names. See Note
[Source retained case refinement] in `GHC.Tc.Gen.Match`.

Complete unguarded constructor clauses preserve strict inspection of the
Haskell scrutinee. The generated application supplies every generalized
argument. Bottom, partial constructors, cyclic values, and function-valued
results require separate Haskell runtime checks. The Agda sources establish
the typing precedent. They do not prove these evaluation properties.

Agda's pinned [local-definition reference](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/let-and-where.lagda.rst#L57-L68)
states that checking substitutes away let definitions; its internal syntax has no
let construct. The file blob is `eef72940b74f0efa87c55ba9378789d26d7df402`.
This precedent does not supply GHC's alias-transport rule. Naming
`f (S k)` as `let t = S k in f t` changes the recovered result index. GHC must
preserve lazy sharing and check transport between these indices. The current
CorePrep increment uses a checked case-local equality with the exact scrutinee.
It composes equalities for nested constructor fields and transports later
payloads as well as the result. It emits cases only for constructor WHNF.
This rule is a GHC design choice. It is not copied from Agda's let elimination.
The tests check lazy and cyclic Haskell values separately. They do not establish
a general preservation theorem. Ambient-evidence normalization under retained
binders, changed-domain congruence, and alias-expression serialization stay gated.

[Koronkevich, Rakow, Ahmed, and Bowman, *ANF preserves dependent types up to
extensional equality*](https://doi.org/10.1017/S0956796822000090), JFP 32, e12
(2022), studies this naming problem. Section 4.1 retains the identity of the
bound computation in continuation typing. In the attempted translation in
section 7.4, the branch result after an explicit coercion fails to match
the computation that its continuation expects. Section 7.6
also discusses commuting conversions and GHC's monadic form. Its calculus uses
equality reflection and call-by-value evaluation. Its proof does not establish
correctness for lazy Haskell or System FC. Use it to guide a checked transport
rule, with separate scope, erasure, and laziness checks in GHC.

The frontend must preserve these identities through parsing, checking, and
desugaring. RequiredTypeArguments provides erased type arguments. It cannot
implement the retained rule by renaming `foreach` to `forall`. See the
[frontend identity plan](CORE.md#frontend-identity-plan).

The first source increment uses the proposed `foreach` spelling from
[GHC proposal #378](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst).
It stores retained quantification in `HsForEachTy`. Existing `HsForAllTy`
telescopes continue to describe erased quantification. The local
`ExperimentalDependentTypes` flag limits this incomplete source experiment.
This flag is not a name specified by that proposal. It implies no other
extensions. In `foreach (b :: Bool)`, `Bool` is the domain type of a value
binder. It is not the kind of an erased type variable. Parsing this annotation
does not require the implementation to imply `KindSignatures`.

The first source implementation follows the dependent application rule above.
It reuses GHC's local name binding and adds a scoped type-lookup alias for the
same value `Name`. Its final index zonker follows the existing
`Note [The ZonkEnv]`: a type must not force the recursively tied runtime Id
environment. Each index already has fixed Bool/Many metadata.

The fully applied laziness observation uses `(b ~ b)` as a payload constraint.
GHC's existing `solveEqualityDict` supplies the boxing construction through
`evDictApp`. This implementation constructs checked nominal reflexivity at the
call site to preserve runtime-binder scope. It does not add a trusted equality
axiom. Other index-dependent predicates need a solver evidence scope and remain
unsupported. These GHC adaptations are implementation choices; the Agda manual
does not establish their preservation properties.

Closed-family matching reuses GHC's existing three-way apartness result in
`GHC.Core.Unify`, described by `Note [Specification of unification]`.
An opaque Boolean can coincide with a constructor or another opaque index.
The experiment records that uncertainty as `MARRuntimeIndex`. It does not
declare the indices apart or select a later family clause. This preserves the
distinction between an unavailable refinement and a disproved constructor case.

### Arithmetic equality proofs

This increment uses an ordinary equality proof value. See
[the Core contract](CORE.md#arithmetic-equality-proofs) and the
[singletons comparison](#singletons-and-frontend-elaboration).
It adds no Nat wrapper or singleton conversion.

Agda stdlib v2.3 proves
[`+-identityʳ` by induction](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Nat/Properties.agda#L551-L556):
the zero branch uses `refl`; the successor branch uses `cong suc` on the recursive
proof. The file blob is `3198ad7f4d783a46d30cc70751a198294d350892`.
Its equality core defines
[`cong`](https://github.com/agda/agda-stdlib/blob/v2.3/src/Relation/Binary/PropositionalEquality/Core.agda#L54-L55)
and [`subst`](https://github.com/agda/agda-stdlib/blob/v2.3/src/Relation/Binary/PropositionalEquality/Core.agda#L82-L83)
by matching `refl`. That file blob is
`2db2dd64e1a8bbb9bda6410930e3bd15366719d7`.
Agda v2.8.0's
[builtin addition](https://github.com/agda/agda/blob/v2.8.0/src/data/lib/prim/Agda/Builtin/Nat.agda#L18-L20)
recurses on the first argument; its file blob is
`2b4801f18bb487e2d999304729bf38ac14ca8400`.
The [builtin equality datatype](https://github.com/agda/agda/blob/v2.8.0/src/data/lib/prim/Agda/Builtin/Equality.agda#L6-L9)
has file blob `22d08dc45b843ed4a3bb9410d38a7508383699c2`.
The right-identity lemma supplies propositional evidence. It does not add a
family reduction for neutral `n + 0`.

Agda's
[`checkConstructorApplication`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L1030-L1071)
reduces the expected target, checks its datatype, and instantiates the constructor
with the datatype parameter prefix. It then checks the remaining arguments and
compares the actual result with the target. The file blob is
`82ba94ac864d19b947d4c92b9af252b0fe81347a`.
Our bounded path instead selects immutable candidates per result argument,
then runs full ordinary subsumption. It accepts DataCon heads and, since M15,
known ordinary Ids with a plain result and no Quick Look path. Existing kind
and scope guards remain. Heads with concreteness requirements keep ordinary instantiation.
Candidate selection does not establish index equality. These are different
algorithms. Our path does not special-case `Refl` or add a general index solver.

Singletons commit `8c9221c164832bfd17878af94d50d8ade706932d` defines
[`Decision`, `SDecide`, and `%~`](https://github.com/goldfirere/singletons/blob/8c9221c164832bfd17878af94d50d8ade706932d/singletons/src/Data/Singletons/Decide.hs#L52-L80).
The method returns `Decision (a :~: b)` from singleton inputs. Reuse the
proof-value convention, not that input ABI. The file blob is
`d99fd54b97727d39417dd0650772f4827e35c444`.
GHC baseline `19ff36f2aede0ca2e554016ff22c71e73aa9803b` already provides
[`Refl`, `castWith`, and `gcastWith`](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/libraries/ghc-internal/src/GHC/Internal/Data/Type/Equality.hs#L61-L88).
Its equality guarantee requires a terminating value. The file blob is
`9b5c8adddc2ed4a48fd844ceb1645aef4e63baae`.

In Haskell, matching `Refl` forces the proof before its local CoVar equality
becomes available. The recursive right-identity proof can force the complete Nat
spine. Bottom can raise; cyclic input can diverge. An ignored proof thunk can
remain lazy. A bottom expression with an equality type does not establish that
equality. This increment adopts neither Agda's totality assumptions nor its
function eta laws.

The explicit congruence proof and `rightUnit` control compile with the prior
checker. The rebuilt stage0 compiler accepts the direct recursive `Refl` proof,
vector transport, and a user-defined equality GADT with Core and STG Lint.
Unproved successor `Refl` and `n :~: S n` fail at their RHS expressions.
The self-hosted regression and separate CLI checks also pass proof transport,
failure order, lazy ignored proofs, and the located rejection controls.

### Generic retained argument inference

This increment lets an ordinary polymorphic consumer accept a type that contains
a runtime index. For example, `force :: forall a. a -> IO String` can accept
`rightZero n`. See [the Core contract](CORE.md#generic-retained-argument-inference).
The erased argument `a` has kind `Type`. The earlier native-index-kind cache
does not select it.

Agda v2.8.0 inserts metas for hidden arguments in
[`implicitNamedArgs`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Implicit.hs#L70-L101).
Its [`newMetaArg`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Implicit.hs#L114-L124)
requests an occurs check. The file blob is
`55a03200d2300486c91a95798fc8e0c3c9c675cf`.
The implicit form is `{A : Set} -> A -> ...`; `(A : Set)` is explicit.

[`checkArgumentsE'`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
checks an argument against its Pi domain. It records the checked elimination and
substitutes the checked term into the codomain.
[`addCheckedArgs`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L833-L844)
retains checked arguments on success and postponement.
[`checkHeadApplication`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L449-L465)
also checks the actual application result against the expected type. The file
blob is `82ba94ac864d19b947d4c92b9af252b0fe81347a`.
[`checkNamedArg` and `inferExpr`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L1535-L1587)
connect argument checking to expression inference. Their file blob is
`02991c9fa0085315cc854d2833a9080af14a7740`.

Agda's metas can depend on their creation context.
[`newValueMeta`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/MetaVars.hs#L282-L321)
abstracts over the current telescope. Assignment
[checks allowed variables](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/MetaVars.hs#L1005-L1045)
and [renames the solution into the meta context](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/MetaVars.hs#L1272-L1313).
The file blob is `13f52cb7f2a08ac55f62e326564043470e62f6f6`.
These are scoped mutable assignments. They do not justify native assignments to
GHC's ordinary type metas. Agda can reduce terms during conversion. This
experiment keeps runtime indices opaque and does not evaluate a proof to infer
its type.

Our bounded algorithm uses immutable type applications. It requires NoQL, a
saturated ordinary tau signature, no visible type arguments, and an ordinary
head without concreteness requirements. A read-only classifier identifies named
argument spines and known Ids whose types contain native syntax. It looks through
applications, type applications, parentheses, pragmas, and already expanded
expressions. It does not expand syntax or execute a splice. A known native Id
permits caching other named arguments, including bare constructors such as
`True` or `Nothing`. Top-level lambdas, literals, cases, and lets retain
expected-type checking. A nested named application checks its own arguments by
the ordinary application rules.

The expected result seeds the immutable substitution first. The cache infers
only eligible domains that mention undetermined outer binders. Every binder must
have a candidate with the correct kind and runtime scope. Cached actual types
must match the substituted formal types exactly. The generic route applies its
substitution only when an actual type argument contains native syntax. It emits
each saved usage once and retains argument and final-result subsumption. The
earlier index-kind route remains unchanged.

See `Note [Generic retained argument inference]`,
`Note [Cached retained argument inference]`, and `Note [Quick Look overview]` in
[`GHC.Tc.Gen.App`](../../compiler/GHC/Tc/Gen/App.hs).
Quick Look uses mutable instantiation variables. This bounded NoQL route does
not implement that algorithm or Agda's meta solver. Bootstrap, self-hosted, and
separate CLI checks pass as of 2026-11-13. Controls also cover ordinary GADT,
rank-n, and higher-kinded applications, linear usage rejection, and one-time
Template Haskell expansion.

### Retained class consumers

Use Agda v2.8.0 as the primary comparison for instance arguments. An instance
argument is a contextual term. GHC passes an ordinary class dictionary as
evidence to a method. The mechanisms share a scope obligation. Their search and
evidence rules differ.

| Source | File blob | Relevance |
| --- | --- | --- |
| [Instance manual, lines 24–40 and 147–172](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/instance-arguments.lagda.rst#L24-L40) | `69c52629a737a8670fe11286a705111b7ba99e9d` | `{{...}}` syntax; a record argument follows hidden type arguments |
| [Implicit insertion, lines 81–101](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Implicit.hs#L81-L101) | `55a03200d2300486c91a95798fc8e0c3c9c675cf` | Insert arguments in telescope order and substitute into the remaining type |
| [Application checking, lines 266–279](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L266-L279) | `82ba94ac864d19b947d4c92b9af252b0fe81347a` | Postpone instance search during application checking |
| [Instance postponement, lines 1018–1020](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/InstanceArguments.hs#L1018-L1020) | `d2c8d6d1fd74b1fd8771ab1525fe872c861d22dd` | Restore instance search after the application action |
| [Instance metas, lines 227–253](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/MetaVars.hs#L227-L253) | `13f52cb7f2a08ac55f62e326564043470e62f6f6` | Abstract over the current telescope; apply the meta to current context arguments |
| [Constraint closures, lines 124–145](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Monad/Constraints.hs#L124-L145) | `3a589abda7f6544215bf4a4edfbd7a3fb38c65cc` | Save and restore the constraint context |

The instance-meta path explicitly uses `DontRunMetaOccursCheck`. Do not infer its
policy from ordinary-meta checks. This spike adopts neither Agda's assignment
algorithm nor its instance-search algorithm.

Immutable GHC matching inspects the ordinary body after the constraint prefix.
It returns the substituted original body with that prefix. Ordinary `IALL`,
`instantiateSigmaQL`, and `instCall` then generate dictionary arguments. An
application-local implication owns native evidence bindings. It preserves
enclosing Givens and the current `TcLevel`. Native residual predicates cannot
become an inferred context outside that scope. See Notes [Constrained immutable
instantiation], [Retained application evidence], and [Retained residual constraints].

The static-expression path in `GHC.Tc.Gen.Expr` supplies the GHC precedent for
`buildImplicationFor` and expression-local `WpLet`. Its `StaticFormSkol` removes
enclosing Givens. The retained path uses its own origin and preserves them.
The earlier Note [Reflexive retained equality evidence] records why outer
evidence bindings were insufficient for runtime indices.

### Native type-map equality

Agda v2.8.0's
[`Var` and `Pi` representation](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/Internal.hs#L221-L264)
uses de Bruijn indices and explicit binder bodies. The file blob is
`a6e0938ed369b1f0f5f0b8aa1ff3baeb9aad033b`.
Its [syntactic equality](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Substitute.hs#L1446-L1458)
compares variable indices. Its
[`Abs` equality](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Substitute.hs#L1524-L1533)
ignores binder names and weakens bodies when needed. The `Substitute.hs` blob is
pinned above. The pinned
[`compareDom`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L827-L849)
compares domains before extending the context for codomains. The native type-map
keys reuse GHC's `eqDeBruijnVar`, keep native key forms distinct, and compare the
domain outside the extended body scope. This adds no index evaluation or family
equations. See Note [Native type-map keys] in `GHC.Core.Map.Type`.

The generic-consumer checkpoint `c90959323a` did not need native map support. Eight stage1
adversarial controls include plain and successor mismatches, class constraints,
let bindings, and repeated generic arguments. They report
located GHC-83865, GHC-91028, or GHC-83605 errors without a panic. These results
do not establish rejection behavior for all invalid programs. No type-map change
was needed for those generic inference cases. The class-consumer prototype then
reached dictionary-key comparison and reproduced `eqDeBruijnType: unsupported
runtime index`. Its log is retained as
`_build/verification/native-class-prototype-map-panic.log`. The native map
implementation handles singleton equality and trie lookup, update, mapping,
filtering, and folding with the same scope rules.

### Closed-list safe indexing

Agda v2.8.0's [finite-position example](https://agda.readthedocs.io/en/v2.8.0/language/instance-arguments.html#constructor-instances)
defines `Fin` with zero and successor constructors. Its `mkFin` requires
evidence of a bound. The reviewed source is
[`instance-arguments.lagda.rst`, lines 353–377](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/instance-arguments.lagda.rst#L353-L377),
blob `69c52629a737a8670fe11286a705111b7ba99e9d`. The versioned manual was
rechecked on 2026-11-16. This is the datatype precedent for M60.

M60 uses an ordinary Haskell GADT with the existing saturated `Count` family.
`mkFinClosed` validates signed runtime input and returns `Maybe (Fin (Count xs))`.
It does not use Agda's instance search to discharge that runtime bound.
`indexClosed` consumes the witness after retained list refinement. Equal-count
evidence permits reuse; it cannot establish equality of list values. The
explicit constructor applications follow the existing visible-type-application
design. The measured implicit `FZ` variant rejects its unsolved bound.
See Note [Closed-list positions] in `ParameterizedIndexLibrary`.

The candidate, comparisons and integrated verification reports are under
`_build/verification/native-closed-list-indexing-*`. Integrated version 2 passes
nineteen runtime observations with both stages through copied interfaces.
Five source rejections and accepted equal-count reuse keep the boundaries
explicit. Neither these checks nor Agda's example establish totality, optimizer,
generic-domain or bounds-check-elimination results for this implementation.

### Source retained constraints

Agda v2.8.0 gives a direct example of an instance argument after a visible Nat
binder. Its
[`mkFin`](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/instance-arguments.lagda.rst#L357-L377)
has type `∀ {n} (m : Nat) → {{suc m - n ≡ 0}} → Fin n`.
Later equations omit the instance pattern. The manual file blob is
`69c52629a737a8670fe11286a705111b7ba99e9d`.
The [lambda manual](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/lambda-abstraction.lagda.rst#L85-L93)
also places `{{monoid-A : Monoid A}}` after the visible binder `A`.
Its file blob is `5162584cecf76e69206da056f4135d892d7ab41a`.

[`insertHiddenLambdas`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L730-L740)
constructs a lambda and checks the remainder under `addContext`.
[`tryInsertHiddenLambda`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L1303-L1338)
handles both ordinary hidden binders and instance binders.
The `Rules/Term.hs` blob is `02991c9fa0085315cc854d2833a9080af14a7740`.
For equations,
[`insertImplicitPatternsT`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS/Implicit.hs#L88-L104)
inserts omitted patterns and continues under the current abstraction.
The `Rules/LHS/Implicit.hs` blob is
`f47fcb713d3b37af67fa11c3e8293afc73be8d84`.

The GHC prototype keeps source class and equality contexts after a runtime binder
in that binder's scope. A variable prefix uses a generated nested lambda.
Ordinary checking then introduces evidence inside the retained binders.
Constructor equations bind dictionaries before the constructor case.
Checked branch proofs transport dependent dictionary types into each branch.
These choices preserve telescope order and evidence scope. They reuse source
`=>` contexts and ordinary GHC evidence rules. Agda supplies scope prior art;
it does not establish GHC's dictionary ABI, FC coercions, or Haskell eta laws.
Checking a source context introduces Givens. Application-side instance search
supplies arguments. Neither operation evaluates a runtime index to obtain a
singleton witness.

### Source case evidence

Agda v2.8.0 generalizes dependent instance arguments with the context of a
with-expression. This supplies scope and substitution prior art for an explicit
GHC case that captures incoming dictionaries.

- [`withFunctionType`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/With.hs#L101-L173)
  partitions the original telescope and abstracts the scrutinee from the trailing
  telescope and result. It does not select arguments by source-name occurrence.
  File blob: `fb84d352f3c6b2e786a0c095c74c5bee15e533cf`.
- [`splitTelescope`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Telescope.hs#L238-L258)
  permutes complete domains. Domain
  [substitution](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Substitute.hs#L1072-L1077)
  preserves argument metadata, and
  [`telePiPath`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Telescope/Path.hs#L42-L59)
  reconstructs Pi binders with the original domains. An instance binder keeps its
  instance annotation. File blobs, in link order:
  `4631c0ea381c47e1fe717f2bbf26b94497f5eb4e`,
  `1ef41f6d2a0223c8328984903b60dabc2a05a783`, and
  `0c94c01971a1e140c7e873ca9ef3206cb72a3620`.
- [`splitCon`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1503-L1531)
  specializes the whole later telescope and target with the constructor
  substitution. This includes dependent instance types.
  File blob: `3779ffa6e418bca99e30c071e189106f82ecbe1b`.
- [`getContextCands`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/InstanceArguments.hs#L115-L143)
  obtains instance-marked variables from the current context. This connects the
  preserved instance annotation to instance lookup.
  File blob: `d2c8d6d1fd74b1fd8771ab1525fe872c861d22dd`.
- The [helper substitution](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Def.hs#L1106-L1117)
  uses all original context arguments. The
  [helper application](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Def.hs#L1201-L1215)
  supplies those arguments and checks the generated type without residual
  constraints. File blob: `0532811dabeb3a7e6dd3f08cfa18c08472072fe1`.
- The [instance-annotated with-expression](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/with-abstraction.lagda.rst#L344-L358)
  gives a source example. File blob:
  `ff0888af84f3101351c501534de087c180dc7afd`.

GHC must pass its actual lexical evidence parameters into the helper. A dictionary
can be needed without a resolved source `Name` referring to it. Transport the
original class dictionary; let GHC derive superclass evidence in the branch.
Agda supplies context scope and specialization precedent. Its normalization and
eta contraction do not establish Haskell evaluation laws or FC evidence rules.

### Checked retained lambdas

Agda v2.8.0 checks an unannotated lambda with the supplied Pi type. In
[`useTargetType`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L560-L585),
the checker uses the expected domain, introduces the source binder with
`lambdaAddContext`, and checks the body against the scoped codomain.
[`checkPostponedLambda`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L678-L710)
repeats this rule for the remaining binders. An
[unannotated binder](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L1474-L1479)
has an omitted domain. The expected telescope supplies that domain.
[`checkNamedArg`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L1535-L1553)
uses expected-type checking in ordinary argument position.

[`checkExtendedLambda`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L831-L866)
creates an auxiliary function with the expected type and passes its clauses to
`checkFunDef'`. It then applies that function to its captured context. This is
direct prior art for sharing GHC's retained function-equation checker with its
constructor lambda checker. These links use `Rules/Term.hs` file blob
`02991c9fa0085315cc854d2833a9080af14a7740`.

The manual's dependent
[`mySnd` pattern lambda](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/lambda-abstraction.lagda.rst#L137-L141)
gives a source example. Its
[internal representation](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/lambda-abstraction.lagda.rst#L171-L184)
uses fresh auxiliary function definitions. File blob:
`5162584cecf76e69206da056f4135d892d7ab41a`.
The hidden-lambda and instance-binder rules above supply context-scope precedent.

Agda's [`checkAbsurdLambda`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L744-L766)
requires an expected Pi type and checks that its domain is empty.
[`makeAbsurdLambda`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L768-L810)
keeps a one-binder telescope. Its absurd pattern refers to that binder, and its
result uses the scoped codomain. These use the same `Rules/Term.hs` blob above.
[`ensureEmptyType`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Empty.hs#L71-L98)
adds the proposed argument to the context and accepts a split with zero clauses.
File blob: `0b2195c68a0010060901d49c8d4b1147617f4129`.
The [manual](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/lambda-abstraction.lagda.rst#L205-L234)
uses `λ ()` for an absurd lambda. GHC uses an explicit runtime binder followed
by an empty case. The binder must remain in scope even without branch bodies.

Checking a source-written lambda needs no eta transformation. Agda's
[function eta equality](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/lambda-abstraction.lagda.rst#L69-L78)
does not apply to Haskell's bottom and `seq`. GHC must preserve each lambda form's
source arity and its existing usage rules. General native lambda inference is
not established by this expected-type rule.

### Explicit higher-order arguments

Agda v2.8.0's
[`checkNamedArg`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L1535-L1553)
checks a value argument against its supplied domain. The
[`checkPostponedLambda`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L678-L710)
rule introduces the Pi binder before checking the body against its codomain.
These rules also apply when the argument is a function. The lambda rule does
not require that the Pi be the outer type of the enclosing definition.
Both links use file blob `02991c9fa0085315cc854d2833a9080af14a7740`.

The Pi telescope and shared-context conversion references under
[Agda provenance](#agda-provenance) supply binder-scope precedent. GHC retains
its own ambiguity check. Its argument/result projection opens nested binders
in one fresh runtime context for that diagnostic check only. It does not change
the actual signature or establish structural Pi congruence. Haskell's `seq`
still requires the source arity and non-strict closure-argument behavior below.
GHC can use a complete expected result to choose immutable type arguments before
checking an argument. The same expected-argument rule then checks a contained
lambda. This is separate from solving a mutable native meta-variable. A runtime
Pi is a function value; an erased inner forall remains a separate polymorphic
type-argument boundary.

Constructor transport through a captured retained closure uses the same
fixed-domain `DepFunCo` rule described under [Agda provenance](#agda-provenance).
The branch proof changes occurrences of the outer index in the body. The inner
domain and binder scope stay fixed. FC lifting constructs the proof; a function
cast preserves Haskell's bottom-function behavior.

### Expected-type instantiation of retained workers

Agda v2.8.0 inserts omitted hidden arguments in
[`implicitNamedArgs`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Implicit.hs#L81-L101).
It creates an argument and substitutes it into the remaining codomain.
Blob: `55a03200d2300486c91a95798fc8e0c3c9c675cf`.
The application checker uses the expected result's hiding when
[inserting trailing implicit arguments](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L587-L602).
It then [compares the actual result with the expected type](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L453-L465).
Blob: `82ba94ac864d19b947d4c92b9af252b0fe81347a`.

Agda's [Pi comparison](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L827-L849)
checks domains and modalities before comparing codomains under one shared
context variable. Blob: `69423e2c11b6f35bc5eee2d502cf2830efbc005a`.
The GHC worker matcher follows this scope rule for equal closed `Many` domains.
It pairs runtime binders and uses the existing bound-variable escape check.
Only the worker's outer erased parameters can receive an immutable substitution.
The application checker requires complete kind/scope-valid arguments and exact
result equality before it emits type applications and performs final subsumption.

Agda's [meta creation](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/MetaVars.hs#L295-L321)
and [solution abstraction](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/MetaVars.hs#L1281-L1340)
retain the meta's permitted context. Blob:
`13f52cb7f2a08ac55f62e326564043470e62f6f6`.
This increment does not adopt those mutable metas. It adds a separate opt-in
matching entry point. Existing unification and apartness entry points retain
their native gates. Haskell function WHNF still forbids an eta-expansion shortcut.

### Retained constructor field telescopes

Decision: represent a stored dependent pair as an ordinary algebraic datatype
whose constructor has a retained field telescope. Reuse `DepFunTy` and
`TermVarTy`. Store field binders separately from erased DataCon existentials.
Agda v2.8.0 supplies these precedents:

- [`Builtin/Sigma.agda`, lines 7–11](https://github.com/agda/agda/blob/v2.8.0/src/data/lib/prim/Agda/Builtin/Sigma.agda#L7-L11)
  defines an ordinary record with `fst : A` and `snd : B fst`. Its constructor
  stores the two fields. File blob: `d68aead018bb834aec9a547a7907576005c28dcf`.
- [`Rules/Record.hs`, lines 119–151](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Record.hs#L119-L151)
  checks field types, extracts their telescope, and builds the constructor type
  with `telePi_`. Lines 237–246 install the constructor and its field arity.
  Lines 653–668 substitute projections of earlier fields into later projection
  types. File blob: `86300a28d259cbb07833f547cb6533c6cce0dd35`.
- [`Rules/Data.hs`, lines 362–375](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Data.hs#L362-L375)
  checks constructor signatures and their datatype result. Lines 1825–1847
  traverse dependent constructor binders under `underAbstraction`.
  File blob: `349a3c508d8a77d8f86b41decedec6de9a4d7afc`.
- [`Rules/Application.hs`, lines 1051–1071](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L1051-L1071)
  instantiates constructor parameters, checks the remaining argument telescope,
  and compares the result with the expected type.
  File blob: `82ba94ac864d19b947d4c92b9af252b0fe81347a`.
- [`Rules/LHS.hs`, lines 1358–1389](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1358-L1389)
  obtains the instantiated constructor telescope and source field names.
  Lines 1503–1531 substitute the constructor through the later context and
  target. File blob: `3779ffa6e418bca99e30c071e189106f82ecbe1b`.

Agda uses de Bruijn scope. It does not supply GHC's Unique-freshening algorithm.
The GHC implementation substitutes actual pattern Ids sequentially and extends
its in-scope set. The first package subset has lazy lifted fields and an
index-independent result. General projections exceed the current native index
grammar. Agda record eta laws do not justify eta conversion in lazy Haskell.
These sources guide the rules; they do not prove preservation for this fork.

The historical
[`nokinds-pi` DataCon.lhs, lines 644–655](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/basicTypes/DataCon.lhs#L644-L655)
counts runtime Ids among `dcExTyCoVars` and builds Pi representation types.
Its [user-type builder, lines 911–916](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/basicTypes/DataCon.lhs#L911-L916)
filters those Ids out. This handling is incomplete. Current GHC existential
machinery assumes erased TyVars/CoVars. Separate retained field metadata keeps
that invariant and preserves ordinary constructor callers.

An ordinary existential `forall n. Nat -> Vec a n -> PackedVec a` does not
relate its Nat field to erased `n`. Type abstractions can name that erased
witness but cannot establish the missing equality. A continuation consumer can
use existing retained functions; it does not provide a stored package. The new
metadata addresses that representation gap. Promotion and interface declaration
encoding require explicit follow-up work before source constructor support.

### Constructor field interface provenance

Agda v2.8.0 preserves a dependent field telescope in its module interface:

- [`Syntax/Internal.hs`, lines 254–264](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/Internal.hs#L254-L264)
  represents each telescope domain followed by an abstraction over the rest.
  Lines 221–229 use integer variable indices; lines 302–308 define abstractions.
  Blob: `a6e0938ed369b1f0f5f0b8aa1ff3baeb9aad033b`.
- [`Serialise/Instances/Internal.hs`, lines 27–54](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Serialise/Instances/Internal.hs#L27-L54)
  encodes and decodes `EmptyTel` and `ExtendTel`. Lines 97–133 preserve
  abstractions, variable indices and Pi types. Lines 226–238 and 422–447 encode
  declaration types and datatype, record and constructor metadata.
  Blob: `d4df2b9480b9721f14b09bdc11980a927f6175e5`.
- [`Serialise/Instances.hs`, lines 28–34](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Serialise/Instances.hs#L28-L34)
  serializes the complete module `Interface`.
  Blob: `cd61f3d60040047ccbb9abfefbd84c2bde07bbcc`.
- [`Serialise/Base.hs`, lines 254–265](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Serialise/Base.hs#L254-L265)
  defines the decoder as `StateT St IO`. It does not establish semantic scope
  validation or fresh local-name rebinding.
  Blob: `7a6397261097c4cbab36c3f6a4dcfc7d5d429cec`.

GHC uses local names and fresh runtime Ids. The codec therefore adds an optional
name for each field and reconstructs scope sequentially. Field types already
encode domains and multiplicities; duplicating these in binder metadata could
introduce conflicting encodings. Reject duplicate names, unsupported constructor
shapes and runtime references outside the earlier fields. Optional local names
introduce no global name dependency. The serialized field schema requires a
header-version change.

GHC's recursive TyCon import knot requires a separate validation-order decision.
Expose metadata before forcing field domains, then validate field types lazily
and check the Core telescope on worker/user type demand. This prevents invalid
self or mutual native-domain declarations from reentering their own checks.
Agda's structural serializer does not supply this GHC-specific lazy knot rule.
The sources establish prior art, not a proof for this implementation.

## Haskell evaluation provenance

The [Haskell 2010 Report, section 6.2](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html)
specifies non-strict application and `seq`. It distinguishes a bottom function
from a lambda that returns bottom. Therefore `f` and `\x -> f x` are not
interchangeable for arbitrary Haskell `f`.

Retained application must not force an index to construct a type witness.
[Agda's lambda abstraction reference](https://agda.readthedocs.io/en/v2.8.0.2/language/lambda-abstraction.html)
describes function eta equality. Haskell's `seq` prevents that general equality.
Eta expansion needs the existing arity and evaluation conditions. Eta reduction
needs its own justification. The scoped native SimpleOpt increment disables eta
reduction. Its dependent beta and case rules do not establish an eta law.
See [the evaluation constraints](CORE.md#laziness-and-eta).

## Existing DataKinds machinery

The [datatype promotion guide at the source baseline](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/docs/users_guide/exts/data_kinds.rst)
describes promotion of data constructors to type constructors. GHC's
`trueDataCon` and `falseDataCon` already describe the ordinary Boolean
constructors. `promoteDataCon` supplies their existing promoted type constructors.
`tcTyVar` in `GHC.Tc.Gen.HsType` uses this machinery for DataKinds.

The spike reuses that promotion. `TermVarTy` adds a runtime-variable index, and
`DepFunTy` binds that runtime variable in a result type. DataKinds alone supplies
neither rule. Boolean-specific recognition is a restriction of this spike.
General datatype support remains pending. The `Types.hs-boot` signatures expose
existing wired-in values to low-level compiler modules through source imports.
They do not define new Boolean constructors.

## Roadmap and proposal status

- [Serokell's Dependent Haskell roadmap](https://ghc.serokell.io/dh), dated
  2026-11-12, was reviewed again on 2026-11-15. This replaces the earlier
  2026-09-03 page snapshot. [ROADMAP.md](ROADMAP.md) records all 50 feature cards,
  all 67 graph edges, and a separate checklist for this fork. Edges mean
  prerequisites or subtasks; two are explicit loose ends of completed visible
  forall. The map separates retained quantification, universal promotion, and
  language cleanup. It gives no completion date or implementation guarantee.
- [GHC proposals repository](https://github.com/ghc-proposals/ghc-proposals) is the
  source for language decisions. Acceptance of a design does not establish that
  this checkout implements it. The table records both kinds of evidence.

| Proposal | Status at the snapshot | Relevance and implementation boundary |
| --- | --- | --- |
| [#378: Design for Dependent Types](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst) | Accepted 2021-05-26. The design-policy proposal carries an implemented marker. | Defines the independent dependence, visibility, and erasure choices. `forall` erases its argument; proposed `foreach` retains it. The marker does not mean that full Dependent Haskell exists. |
| [#281: Visible forall](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst) | Accepted and implemented, with remaining restrictions. | Supplies required, erased type arguments. Current `forall a ->` does not supply a retained runtime index. Visible forall in GADT constructors does not remove every constructor-type restriction. |
| [#425: Invisible binders in type declarations](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0425-decl-invis-binders.rst) | Accepted and implemented. | Supplies declaration `@` binders. Reuse this work rather than adding another declaration-binding mechanism. |
| [#448: Modern scoped type variables](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst) | Accepted; implementation remains partial. | Organizes type-variable scoping and abstraction. Check the current TypeAbstractions restrictions for each use. |
| [#402: GADT syntax](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0402-gadt-syntax.rst) | Accepted; the full proposed syntax is incomplete in this checkout. | Relaxes the constructor-type grammar. Current GADT constructor signatures still impose a prenex forall/context structure. |
| [#242: Unsaturated type families](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0242-unsaturated-type-families.rst) | Accepted as experimental; absent from this checkout. | Tracks matchability for higher-order type functions. Its prototype and older syntax need adaptation. Ordinary saturated Boolean families do not require it. |
| [#765: Pun warnings](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0765-pun-warnings.rst) | Accepted 2026-09-02; no implementation found in this checkout. | Prepares namespace changes. It needs scope-aware reasoning, including imports, shadowing, and implicit quantification. A name-collision scan is insufficient. |
| [#473: First-class existentials](https://github.com/ghc-proposals/ghc-proposals/pull/473) | Open; needs revision. | Discusses existential types and significant Core consequences. This is not an approved dependency of the Boolean experiment. |
| [#509: Type functions](https://github.com/ghc-proposals/ghc-proposals/pull/509) | Open; not approved. | Explores a broader type-function design. Do not treat its syntax or semantics as settled. |
| [#102: Dependent-type syntax](https://github.com/ghc-proposals/ghc-proposals/pull/102) | Closed without merge in 2019. | Preserves historical syntax discussion. It is not evidence of an implemented `DependentTypes` extension. |
| [#370: Modifiers](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0370-modifiers.rst) | Implemented; experimental since GHC 10.0. | Provides extensible syntax. Only the current recognized modifiers have semantics; it does not implement retained quantification. |
| [#475: List and tuple syntax](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0475-tuple-syntax.rst) | Implemented. | `NoListTuplePuns` prepares term/type syntax convergence. Reuse the existing parser and renamer behavior. |
| [#581: Namespace-specified imports](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0581-namespace-specified-imports.rst) | Implemented. | Gives explicit type/data namespace control in imports and exports. This avoids duplicating namespace preparation. |
| [#364: Unify Natural and Nat](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0364-unify-natural.rst) | Implemented. | Removes one term/type distinction. It does not add general evaluation of terms in types. |
| [#387: Char kind](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0387-char-kind.rst) | Implemented. | Extends the promoted literal domain. It is prior promotion work, not retained quantification. |
| [#54: Standalone kind signatures](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0054-kind-signatures.rst) | Implemented. | Gives explicit declaration kinds. The old `0036-kind-signatures.rst` path redirects to this file. Use standalone kind signatures rather than relying on CUSK inference when the plan requires an explicit kind. |

The current documentation records the implemented boundaries:

- [RequiredTypeArguments](../users_guide/exts/required_type_arguments.rst) explicitly
  excludes value-dependent return types. Required arguments remain erased.
- [TypeAbstractions](../users_guide/exts/type_abstractions.rst) describes partial
  implementation, checking restrictions, and declaration binders.
- [Standalone kind signatures and CUSKs](../users_guide/exts/poly_kinds.rst) describes
  declaration inference. CUSKs are a distinct existing mechanism; this bibliography
  does not claim that the standalone-kind-signature proposal removed them.
- [Modifiers](../users_guide/exts/modifiers.rst) records recognized modifier syntax.

## Singletons and frontend elaboration

### Papers

- Richard A. Eisenberg and Stephanie Weirich,
  [Dependently Typed Programming with Singletons](https://richarde.dev/papers/2012/singletons/paper.pdf),
  Haskell 2012. The linked author PDF identifies itself as a submission draft.
  It explains how singleton GADTs connect promoted indices with runtime witnesses.
  It also describes generated boilerplate and the limits of Haskell's phase
  separation. Use it to specify the singleton comparison implementation.
- Richard A. Eisenberg and Jan Stolarek,
  [Promoting Functions to Type Families in Haskell](https://richarde.dev/papers/2014/promotion/promotion.pdf),
  Haskell 2014. The linked PDF is the final submission draft. It explains promotion,
  defunctionalization, and encodings of lambda, case, and let expressions. The
  correctness result concerns the formal translation. The reduction result assumes
  correct lambda lifting. It does not prove this GHC integration or every TH path.

### Upstream implementation and compatibility

The [singletons repository](https://github.com/goldfirere/singletons) separates its
foundational API, TH generator, and generated base-library support.

- [`singletons/src/Data/Singletons.hs`](https://github.com/goldfirere/singletons/blob/master/singletons/src/Data/Singletons.hs)
  defines `Sing`, `SingI`, `SingKind`, `SomeSing`, `toSing`, `fromSing`, and
  `withSomeSing`. `SingKind` was around line 299 and `withSomeSing` around line 925
  in the inspected source. The verified source **blob** is
  [`c6bf5970d85d28ed6edb4d887f14cfa127a01be2`](https://api.github.com/repos/goldfirere/singletons/git/blobs/c6bf5970d85d28ed6edb4d887f14cfa127a01be2).
  This hash identifies one file's contents, not a repository commit.
- [`singletons-th/src/Data/Singletons/TH/Single.hs`](https://github.com/goldfirere/singletons/blob/master/singletons-th/src/Data/Singletons/TH/Single.hs)
  implements singling. Start with `transform` around line 104, pattern translation
  around line 719, and lazy-pattern handling around line 737. Inspect these rules
  before generating witness patterns or changing demand behavior.
- [`singletons-base/src/Data/Bool/Singletons.hs`](https://github.com/goldfirere/singletons/blob/master/singletons-base/src/Data/Bool/Singletons.hs)
  supplies the upstream Boolean support. Prefer its conventions when specifying
  `SBool` and Boolean singleton operations.
- [`singletons/singletons.cabal`](https://github.com/goldfirere/singletons/blob/master/singletons/singletons.cabal)
  was version 3.0.4 with `base >= 4.9 && < 4.23`.
- [`singletons-base/singletons-base.cabal`](https://github.com/goldfirere/singletons/blob/master/singletons-base/singletons-base.cabal)
  was version 3.5.1 with `base >= 4.22 && < 4.23` and
  `template-haskell >= 2.24 && < 2.25`. It listed GHC 9.14.1 as tested.

This GHC checkout has `base-4.23.0.0` and `template-haskell-2.25.0.0`. Those versions
fall outside the inspected package bounds. The local probe compiled the unmodified
foundational `Data.Singletons` module with a small `SBool`/`SingKind Bool` adapter.
It did not build the full packages or exercise their TH generator. It establishes
that narrow API result only.

An ordinary `Bool` reification boundary needs a separate strictness specification.
Forcing `toSing` to expose a witness can force an input that a retained binder would
leave unevaluated. In particular, strict reification cannot transparently implement
a lazy `keep bottom` that returns a function. The native implementation must
preserve that non-strict behavior without requiring singleton reification.

### Earlier preprocessors

- Conor McBride's
  [Strathclyde Haskell Enhancement, SHE](https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/)
  is a historical GHC preprocessor with pi types and data-indexed types. Its
  singleton-based translation is direct prior art for frontend syntax that targets
  existing Haskell. The author describes it as an experimental implementation.
  It supplies no compatibility guarantee for this GHC checkout.
  The [pi-type manual](https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/pi.html)
  gives the exact translation from `pi (n :: Nat).` to `forall n.` plus a singleton
  argument. This documents the singleton frontend approach.
- Adam Gundry's [Inch implementation](https://github.com/adamgundry/inch) extends a
  Haskell subset with integer constraints and emits Haskell after erasure. Its
  README records incomplete module, class, and inference support. The repository's
  last push was in 2015 when inspected. Use it for elaboration and constraint-design
  examples, rather than as a current compiler component.
- Gundry's [2013 thesis and errata](https://adam.gundry.co.uk/pub/thesis/) explain
  dependent products, contextual inference, and an FC-based evidence language.
  Follow the published errata for the Chapter 4 pattern-unification algorithm.
  The thesis page links both the Inch and type-inference implementations.

## Native dependent Core: designs and implementations

### Pico, Bake, and historical GHC work

- Richard A. Eisenberg,
  [Dependent Types in Haskell: Theory and Practice](https://repository.brynmawr.edu/compsci_pubs/70/),
  PhD thesis, 2016. It defines the Pico intermediate calculus and Bake elaboration
  algorithm. Its implemented GHC contribution was the portion released in GHC 8.0.
  It does not establish that retained dependent functions were fully implemented.
- The [thesis repository](https://github.com/goldfirere/thesis) preserves the text,
  examples, Ott sources, and experimental code. Its
  [`hs/Pico/Rules.hs`](https://github.com/goldfirere/thesis/blob/master/hs/Pico/Rules.hs)
  ends with `ctx_ = undefined`. Other Pico utilities are incomplete. These files
  are not a completed executable implementation of the thesis calculus.
- The historical
  [`nokinds-pi` branch at `b64ef80f8002727422c05092f429c38d0527ed0b`](https://github.com/goldfirere/ghc/tree/b64ef80f8002727422c05092f429c38d0527ed0b)
  ends on 2014-08-19. Its
  [`TyCoRep.lhs`](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/types/TyCoRep.lhs#L172)
  introduces `PiTy` and separate dependence, visibility, and relevance flags.
  Its [`CoreLint.lhs`](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/coreSyn/CoreLint.lhs#L528)
  attempts to promote a relevant argument and substitute it into a dependent result.
  However, [`CoreUtils.lhs`](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/coreSyn/CoreUtils.lhs#L157)
  still discards runtime arguments during result-type computation. Its case checker
  also uses one fixed result type. The inspected branch has unfinished work. Its
  small dependent tests exercise promotion and kind polymorphism. We did not build
  it. It is the historical port baseline, not a completed dependent compiler.
- [Richard Eisenberg's `rae/dependent` repository](https://gitlab.haskell.org/rae/dependent)
  identifies itself as examples of Dependent Haskell. It is not a GHC implementation.
  Searches of current source and relevant available branch references found no
  verified retained-quantifier implementation suitable for this checkout. This
  bounded search does not rule out other unpublished or differently named work.

### pi-forall

- Stephanie Weirich's [pi-forall implementation](https://github.com/sweirich/pi-forall/tree/2023)
  accompanies her dependent-type implementation lectures. It provides progressive
  versions for equality, irrelevant arguments, and datatypes. Its documented 2023
  toolchain includes GHC 9.2.7. We did not rebuild it with this checkout.
- [`full/src/TypeCheck.hs`](https://github.com/sweirich/pi-forall/blob/2023/full/src/TypeCheck.hs#L186)
  checks Boolean elimination against an expected type under separate `True` and
  `False` refinements. This is a concrete reference for checking dependent branches.
- [`full/src/Equal.hs`](https://github.com/sweirich/pi-forall/blob/2023/full/src/Equal.hs#L211)
  implements refinements through local definitions and reduction. Its tutorial
  unifier is not GHC's evidence-producing constraint solver. The implementation
  has no FC coercion, optimizer, interface, or code-generation integration.
- [Implementing Dependent Types in pi-forall](https://arxiv.org/abs/2207.02129),
  2022 lecture notes, explain the code. Use the matching repository branch when
  following a particular lecture edition.

### D and DC: equality, erasure, and nontermination

- Weirich, Voizard, de Amorim and Eisenberg,
  [A Specification for Dependent Types in Haskell](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf),
  ICFP 2017, supplies an implicit dependent calculus D and an explicitly annotated
  calculus DC. D shares term/type syntax and supports relevant and irrelevant
  dependent quantification. DC has explicit coercions and decidable checking.
  The design admits
  nontermination and `Type : Type`; it does not claim logical consistency or
  normalization. Its safety results control equality assumptions rather than
  treating an arbitrary diverging term as usable equality evidence. Primitive
  Haskell datatypes and general Core case expressions still need integration.
- [The `corespec` artifact](https://github.com/sweirich/corespec) contains Ott
  definitions and Coq proofs. Use its
  [`icfp17` tag](https://github.com/sweirich/corespec/tree/icfp17) for the original
  paper. The default branch later adds eta-equivalence. The mechanization covers
  the research calculus and its stated assumptions, not the GHC implementation.
  D progress excludes coercion assumptions. DC progress permits only irrelevant
  term assumptions. Preservation does not require consistent assumptions
  (author PDF pages 12–13 and 20). These theorem premises do not authorize an
  arbitrary GHC Given context. Section 6.3 also preserves coercion abstraction
  during erasure (page 22). See [the reading and lowering comparison](papers/README.md#d-and-dc-icfp-2017).

### Roles and graded usage

- Weirich, Choudhury, Voizard, and Eisenberg,
  [A Role for Dependent Types in Haskell](https://arxiv.org/abs/1905.13706),
  ICFP 2019. System DR adds nominal and representational equality to the implicit
  dependent calculus. Newtype unfolding depends on role. Case discrimination
  requires nominal information. The work mechanizes safety, but leaves an explicit,
  decidably checked annotated calculus as future work. Its restricted case form
  does not directly implement all GHC datatype matching.
- The [`corespec` roles branch](https://github.com/sweirich/corespec/tree/9660a4b7ae5d6b3096fd4db2fe8565bc5a1bf65d)
  preserves related definitions and proofs. Some annotated syntax is work in
  progress. Do not infer that every definition on this branch has the paper's
  proved guarantees or constitutes a completed GHC implementation.
- Choudhury, Eades, Eisenberg, and Weirich,
  [A Graded Dependent Type System with a Usage-Aware Semantics](https://richarde.dev/papers/2021/grad/grad.pdf),
  POPL 2021. GraD uses semiring grades for erased, linear, and unrestricted usage.
  It includes dependent functions and elimination with dependent motives.
  Its heap semantics is call by name, not a proof of GHC's call-by-need runtime.
  Resource and noninterference results require the stated semiring conditions.
- The [graded-haskell artifact](https://github.com/sweirich/graded-haskell), especially
  [`GraD/README.md`](https://github.com/sweirich/graded-haskell/blob/master/GraD/README.md),
  [`GraD/ARTIFACT.md`](https://github.com/sweirich/graded-haskell/blob/master/GraD/ARTIFACT.md),
  and [`GraD/src/beta.v`](https://github.com/sweirich/graded-haskell/blob/master/GraD/src/beta.v),
  records the mechanization boundary. GraD's Coq development assumes an equality
  interface, including consistency and injectivity properties. It mechanizes
  selected syntactic safety results, not every heap/resource theorem in the paper.
  The repository also contains later calculi; their results need separate attribution.
  We inspected the sources but did not rebuild these proofs.

### Current System FC comparison points

- [The official Core specification](../core-spec/README) and
  [its explanatory text](../core-spec/core-spec.mng) describe current FC concepts.
  The Ott files generate the written specification; they are not a machine-checked
  correctness proof of GHC. Some details lag the implementation, including
  multiplicities and forall visibility. Check source before adopting a rule.
- [`GHC.Core.TyCo.Rep`](../../compiler/GHC/Core/TyCo/Rep.hs) separates `Type` from
  runtime expressions. [`GHC.Core.Utils`](../../compiler/GHC/Core/Utils.hs) computes
  ordinary runtime application results without substituting the argument into a
  type. These are native-extension boundaries.
- [`GHC.Core.Subst`](../../compiler/GHC/Core/Subst.hs),
  [`GHC.Core.TyCo.Subst`](../../compiler/GHC/Core/TyCo/Subst.hs), and
  [`GHC.Core.FVs`](../../compiler/GHC/Core/FVs.hs) encode substitution and scope.
  In particular, the current case result type lies outside the case binder's scope.
- [`GHC.Core.Lint`](../../compiler/GHC/Core/Lint.hs) checks FC evidence and branch
  result types. A native extension must provide sound motives and branch equality
  evidence, then preserve them through optimization.
- [`GHC.Iface.Type`](../../compiler/GHC/Iface/Type.hs),
  [`GHC.CoreToIface`](../../compiler/GHC/CoreToIface.hs),
  [`GHC.IfaceToCore`](../../compiler/GHC/IfaceToCore.hs), and
  [`GHC.CoreToStg`](../../compiler/GHC/CoreToStg.hs) identify serialization and erasure
  obligations. A calculus prototype does not discharge these compiler obligations.

## Source vector filter

Agda's standard library v2.3 supplies the program structure for the source
filter. [`Data.Vec.filter`](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec.agda#L43-L45)
returns `Vec≤ A n`. Its result stores the output length and a vector indexed by
that length. It also stores an irrelevant proof that the output length is at
most the input length. See the
[`Vec≤` fields](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Bounded/Base.agda#L42-L46)
and its
[cons operation](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Bounded/Base.agda#L106-L110).

The Haskell example follows the length and vector fields. `PackVec n xs` stores
one runtime `Nat` and a `Vec a n`. A retained element constructs `S n` and
`VCons x xs`. A rejected element returns the recursive package. This increment
uses an ordinary Boolean predicate. It does not implement Agda's `Decidable P`
predicate or the bound proof. The type checks the relation between the output
fields. It does not prove that the predicate selected the correct elements.

Haskell demand behavior needs separate checks. The current filter checks the
predicate before the recursive call. It opens each recursive package before
returning its own package. It therefore scans a finite input spine before it
returns a package constructor. It can leave each element and the stored length
unevaluated. It does not produce a package from an infinite input. Agda's
termination rules do not establish these Haskell properties.

Source `case` also uses GHC's existing multiplicity check. Its fresh multiplicity
variable must admit the unrestricted package pattern. `tcSubMult Many pat_mult`
uses the same rule as ordinary `checkManyPattern`. Linear and rigid multiplicities
remain subject to that check. See
[Note [Checking retained package patterns]](../../compiler/GHC/Tc/Gen/Pat.hs).

The runtime predicate `(> cutoff)` also exercises GHC's existing section
translation. See
[Note [Left and right sections]](../../compiler/GHC/Rename/Expr.hs) and
[the wired-in helper templates](../../compiler/GHC/Builtin/WiredIn/Ids.hs).
These representation-polymorphic helpers have no runtime definitions. The
native path must expand their compulsory unfoldings before code generation.
It uses the existing substitution rules and preserves the remaining lambda.
The source checks keep undefined operators and operands lazy at section WHNF.
They then demand each section application. These checks use ordinary section
syntax with `PostfixOperators` disabled. See
[Note [Restricted dependent simplification]](../../compiler/GHC/Core/SimpleOpt.hs).

### Source filter bound proof

The M12 source probe follows Agda standard library v2.3. Its
[`_≤_` constructors](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Nat/Base.agda#L56-L58)
correspond to `LEZ` and `LES`. The Haskell `weakenRight` function follows
[`m≤n⇒m≤1+n`](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Nat/Properties.agda#L297-L299)
by structural recursion on the proof. These sources were checked on 2026-11-14.

The retained branch constructs `S len`, `VCons x ys` and `LES proof` together.
The rejected branch keeps `len` and `ys`, then weakens only the upper bound.
Agda's filter instead uses
[`≤-cast`](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Bounded/Base.agda#L113-L114)
with `n≤1+n`. Direct structural weakening gives the same bound transformation
without a general transitivity helper in the Haskell example.

Agda marks the stored bound proof irrelevant. The Haskell `LE` field is an
ordinary lazy runtime field. Separate observations must check vector demand,
proof WHNF and recursive proof demand. A terminating proof over terminating
indices gives the bound interpretation. A bottom or cyclic proof does not
establish that interpretation. General proof erasure remains separate work.

The source observer also uses ordinary `evaluate proof >> pure ()`. GHC's
dictionary selector has the type
`forall m. Monad m => forall a b. m a -> m b -> m b`.
See [Note [Dictionary selectors]](../../compiler/GHC/Types/Id/Make.hs).
Its later `a` can denote `LE len limit` at a scoped runtime `len`. Native
instantiation must match across the invisible prefix while preserving the
original order of type and evidence applications. It must not flatten the
constraint into a different Core application order. The existing IALL and
`instCall` pipeline supplies that order. See
[Note [Immutable instantiation of nested contexts]](../../compiler/GHC/Tc/Gen/App.hs).
This is a GHC frontend requirement. It adds no new Core proof rule or mutable
type-meta assignment for the runtime index.

## Source package patterns

Decision: allow ordinary nested patterns in non-retained package fields.
Retained fields accept bare variables or one closed constructor layer with
bare variable subfields. Compile package equations in
source order with a separate case for each equation. Each case keeps that
equation's actual runtime index Ids. Reuse GHC's `MatchResult` failure
continuations and checked outer field pattern types. This avoids aliases
between different index Ids. No new Core form or interface format is required.

Agda v2.8.0 supplies these precedents:

- [`LHS.hs`, lines 1358–1389](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1358-L1389)
  instantiates the constructor telescope and adopts source field names.
  Lines 1427–1446 describe index unification. Lines 1503–1532 substitute the
  constructor through the later context and target. File blob:
  `3779ffa6e418bca99e30c071e189106f82ecbe1b`.
- [`Compile.hs`, lines 53–80](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/CompiledClause/Compile.hs#L53-L80)
  builds a case tree from source clauses and coverage information. Lines
  179–208 select the first non-lazy match in the first clause. Lines 314–343
  preserve catchall priority during expansion. File blob:
  `962a80bbaee38601c392a2f9bfd2af8ea0f972d1`.
- [The function-definition manual, lines 50–64](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/function-definitions.lagda.rst#L50-L64)
  describes source clauses and coverage/termination checks. Lines 207–245
  explain how case-tree choices affect definitional equations. File blob:
  `3c7e86cd7e6b88d67f35e8882455ee28f7da6d4d`.

Separate per-equation cases are a Haskell implementation choice. Agda does not
supply GHC's Unique handling or justify changing Haskell field demand. Nested
vector and proof patterns must force only the matched fields. An earlier
catchall must prevent later patterns from forcing a payload. The Haskell `LE`
field remains an ordinary lazy field. General retained-index patterns and
projections remain separate work. See Note [Retained package equation scope]
in `GHC.HsToCore.Match.Constructor`.

### Retained constructor fields in source patterns

M17 extends the M13 field checker with `PackVec Z _` and
`PackVec (S k) (VCons x rest)`. It shares the closed constructor resolver
with retained function and case checking. It keeps a fresh whole-field Id,
the actual source subfield Ids, and the existing nominal constructor proof.
Later field types receive the accumulated transport in telescope order.
Checked coercion patterns preserve their original outer types. The existing
match compiler keeps each row's failure continuation. Core Lint and lowering
already accept the scoped proof and an ordinary DEFAULT alternative.

The Agda v2.8.0 sources above supply the telescope precedent. More precisely,
[`LHSResult`, lines 594–620](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L594-L620)
records the checked context, patterns and specialized RHS type. Constructor
index unification is at
[lines 1427–1450](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1427-L1450).
The `splitCon` substitution through the later telescope, equations and target
is at
[lines 1503–1532](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1503-L1532).
The exact file blob is `3779ffa6e418bca99e30c071e189106f82ecbe1b`.

[`Def.hs`, lines 712–749](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Def.hs#L712-L749)
consumes that result and scopes the pattern bindings.
[Lines 825–852](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Def.hs#L825-L852)
check an expression RHS against its specialized type. File blob:
`0532811dabeb3a7e6dd3f08cfa18c08472072fe1`.
Exact snapshots of these two files and `Compile.hs` are saved under
`_build/verification/native-index-pattern-prior-art/`. Their Git blob hashes
match these pinned upstream files.

The fresh whole-field Id, generated as-pattern and primitive equality binder
are GHC adaptations. Agda's case-tree checks do not establish Haskell demand
behavior. The
[Haskell 2010 Report, sections 3.13 and 3.17.2](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html)
defines ordered alternatives, field matching and bottom. Moving a retained
pattern into an RHS case could evaluate a later argument, view or guard first.
The source observations check those orders, proof demand, partial fields and
ordinary row failure separately. These runtime observations do not prove
preservation or totality for this fork.

## Source vector uncons

Decision: inspect the stored runtime length before repackaging a vector tail.
In the `S k` branch, use that actual `k` as the tail package's retained field.
The checked case rule transports the captured vector to `Vec a (S k)`.
The nested ordinary GADT match relates its tail index to `k`. Do not reconstruct
a runtime value from an erased GADT index. No new Core rule is proposed.

Agda stdlib v2.3
[`Data.Vec.Base.uncons`, lines 285–286](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda#L285-L286)
returns `A × Vec A n` from `Vec A (suc n)`. The
[constructors, lines 36–38](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda#L36-L38),
and [head/tail, lines 53–57](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda#L53-L57)
give the same payload relation. File blob:
`8c03fb8a83e6cec1fa7b8e1f64bb7e606c531cb8`.
Agda's index is implicit. This Haskell adaptation must obtain its predecessor
from the actual stored `Nat`. Agda's `length` implementation is not a runtime
index-reconstruction rule for GHC.

The stdlib's
[`case_returning_of_`, lines 110–116](https://github.com/agda/agda-stdlib/blob/v2.3/src/Function/Base.agda#L110-L116)
makes the motive explicit. File blob:
`1f8f3f0b18ee0f90f1475226cba59b31c35696e9`.
The Agda v2.8.0
[with-abstraction manual, lines 728–741](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/with-abstraction.lagda.rst#L728-L741)
explains why a dependent case alone need not refine an existing argument.
Lines 910–936 describe context generalization and checking the auxiliary type.
File blob: `ff0888af84f3101351c501534de087c180dc7afd`.
These sources motivate the captured-context transport. They do not supply
Haskell's evaluation rules or prove preservation for this fork.

The full observer also exposed a GHC adaptation at Core Tidy. `tidyTopPair`
converts top-level LocalIds to GlobalIds. A retained constructor tree can
mention such an Id before this conversion. Native indices still require local
Ids afterward. Create lazy local aliases before tidying native RHSs and
substitute expressions, types and coercions together. Keep alias RHSs outside
that substitution. Reuse GHC's deterministic free-variable traversal,
`Core.Subst.substExpr`, and the CorePrep native alias guard. This is a Haskell
representation requirement, not a rule taken from Agda. See
Note [Local indices across Core Tidy] in `GHC.Iface.Tidy`.

The Haskell zero branch must ignore its vector. The successor branch must
inspect the vector constructor. Returning the result, pair and tail package
must leave predecessor, head and tail fields lazy. A terminating finite input
supports the functional interpretation. Partial and cyclic checks establish
only their stated evaluation observations.

## Runtime equality and aligned zip: next source probe

Status: both compiler stages pass all 136 source observations with Core and
STG Lint. The bootstrap and registered runtime fixtures pass 27 package/bound
rejections. All five focused registered regressions pass.
Compare two stored runtime lengths. On equality, return ordinary Haskell
`Data.Type.Equality` evidence and use `Refl` to align two vector indices.
Return `Maybe`, so the mismatch result carries no refutation proof.

- Agda stdlib v2.3
  [`Data.Nat.Properties`, lines 90–109](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Nat/Properties.agda#L90-L109),
  derives propositional equality from natural-number comparison. Successor
  injectivity and successor congruence support the recursive proof. File blob:
  `3198ad7f4d783a46d30cc70751a198294d350892`. Its `_≟_` uses Boolean comparison
  with proof generation. A structural Haskell comparison returning `Maybe`
  would use a different algorithm and result representation.
- [`Relation.Nullary.Decidable.Core`, lines 42–61](https://github.com/agda/agda-stdlib/blob/v2.3/src/Relation/Nullary/Decidable/Core.agda#L42-L61),
  defines `Dec` with a Boolean decision and reflected evidence. The `no` case
  carries negative evidence. A Haskell `Nothing` does not supply that evidence.
  File blob: `47b9e55b55bbff72ab21c21e144b2fc3d8f9c823`.
- [`Data.Vec.Base`, lines 128–143](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda#L128-L143),
  defines `zipWith` and `zip` for two vectors with the same index. The proposed
  runtime comparison would establish that shared index for independent Haskell
  packages. File blob: `8c03fb8a83e6cec1fa7b8e1f64bb7e606c531cb8`.
- [`Relation.Binary.PropositionalEquality.Core`, lines 54–55](https://github.com/agda/agda-stdlib/blob/v2.3/src/Relation/Binary/PropositionalEquality/Core.agda#L54-L55),
  defines `cong` by matching `refl`. This supports the successor proof shape;
  it does not justify an additional GHC equality axiom. File blob:
  `2db2dd64e1a8bbb9bda6410930e3bd15366719d7`.

Use nested source cases within the current retained telescope subset. Probe
the existing equality-Given path before adding a rule. A mismatch must leave
vector payloads lazy. A partial index or equality proof can diverge when the
consumer demands it. Agda's totality checks do not apply to these Haskell terms.

The full consumer exposes GHC-specific application boundaries. The native
resolver must read assigned Id type and multiplicity metadata with the
existing within-TC `zonkId`. Immutable instantiation must check current lexical
and same-module bindings. These operations preserve binding identity and do
not assign native types to mutable variables.

An ordinary polymorphic result can have a known index and an unresolved
ordinary payload type. Reuse the existing result-candidate matcher to choose
the index. Keep the actual instantiated type and let final subsumption check
all equalities. Expected checking can also supply an ordinary function arrow
after the written arguments. This follows expected-type instantiation; the
candidate itself supplies no equality proof.

GHC already gives wired-in dollar applications special Quick Look handling
in `GHC.Tc.Gen.App`. Its source signature in
`libraries/ghc-internal/src/GHC/Internal/Base.hs` quantifies over both runtime
representations. The bounded native route keeps that signature and Id. It
requires a complete expected or independently declared result, immutable
argument choices and concrete runtime representation choices. It infers the right operand once and retains
the final argument and result checks. General Quick Look still cannot assign
native syntax to an instantiation variable. These are GHC elaboration choices;
Agda's comparison or proof erasure does not justify changing them.

A discarded `do` statement can have expected type `IO alpha`. GHC's ordinary
`(>>)` does not determine that result type before checking the statement.
The bounded dollar route can read a named first operand's declared result
when that result is independent of its outer invisible binders. It does not
elaborate that operand or expand a splice. The original expected type still
passes through final checking. The inspected dollar-definition file has Git
blob `703a03ad64c5fe1a6f17c94c583bfd699abceb11`.

Decide eligibility before right-operand inference. Higher-rank callback
domains need their existing expected-checking path. If immutable matching
fails after inference, retain the elaborated operand and its usage in one
NoQL fallback. Do not elaborate that operand again.

## Computed-length append: next source probe

Status: both compiler stages pass 180 source observations. The runtime
fixtures pass 34 package/bound rejections. Seven standalone errors have
pinned codes and source tokens in both stages. Declaration-only producer,
partial VTA and unused-field controls pass through a separate interface.
All five focused registered regressions pass. Compute a runtime result `p`
with evidence `p :~: Add n m`.
Match that evidence before packaging an appended vector. `Sum` follows
the existing retained-field design.

- [Agda v2.8.0 builtin Nat, lines 18–20](https://github.com/agda/agda/blob/v2.8.0/src/data/lib/prim/Agda/Builtin/Nat.agda#L18-L20),
  defines addition by recursion on its first operand. File blob:
  `2b4801f18bb487e2d999304729bf38ac14ca8400`.
- [Agda stdlib v2.3 vector append](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda),
  gives the result length as the sum of the input lengths. File blob:
  `8c03fb8a83e6cec1fa7b8e1f64bb7e606c531cb8`.
- [Equality congruence, lines 54–55](https://github.com/agda/agda-stdlib/blob/v2.3/src/Relation/Binary/PropositionalEquality/Core.agda#L54-L55),
  transports equality through a function by matching `refl`. Successor
  congruence supplies the recursive sum proof. File blob:
  `2db2dd64e1a8bbb9bda6410930e3bd15366719d7`.

These definitions support the proof shape. The Haskell source checks its
constructor and proof through the current compiler. The recursive witness
forces the entire first Nat spine before
returning `Sum`. For a finite first length, its result can store the second
length without forcing it. Validate these demands separately from vector
demand. Agda's totality and erasure checks do not apply to this Haskell code.

Agda's
[`checkConstructorApplication`, lines 1030–1071](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L1030-L1071),
uses a fully applied expected datatype to select constructor parameters,
checks the remaining arguments, then compares the actual result. File blob:
`82ba94ac864d19b947d4c92b9af252b0fe81347a`. The bounded GHC route follows that
order with immutable erased arguments. It projects only a fully applied
real constructor with validated retained fields. It traverses the current
signature, including earlier VTA substitutions and canonical unused Many
arrows. It keeps the actual telescope and normal field/proof checks.
It does not generalize result projection through retained function arguments.

A declaration-only GHC module needs the native option check before its
implicit constructor worker is generated. Module classification now includes
local retained worker types before simplification, tidying and code generation.
This carries the existing constructor rules through GHC's driver. It adds no
equality rule, interface schema or runtime field.

## Later solver and proof-certificate work

These references concern additional equality reasoning. None is required for the
first Boolean experiment. Solver results cannot replace the retained-binder,
elaboration, substitution, or laziness design.

### Thoralf and GHC plugin interfaces

- [The Thoralf paper](https://richarde.dev/papers/2018/thoralf/thoralf.pdf), 2018,
  explains extensible SMT-backed theory reasoning for Haskell type checking.
  Compare each theory's translation with its Haskell semantics before reusing it.
- [The original Thoralf implementation at `fa4e403b25bfcaf9e8d5142ab40d3c54b33a3630`](https://github.com/bgamari/the-thoralf-plugin/tree/fa4e403b25bfcaf9e8d5142ab40d3c54b33a3630)
  is the inspected code baseline. Its
  [plugin entry](https://github.com/bgamari/the-thoralf-plugin/blob/fa4e403b25bfcaf9e8d5142ab40d3c54b33a3630/src/ThoralfPlugin/Plugin.hs),
  [solver integration](https://github.com/bgamari/the-thoralf-plugin/blob/fa4e403b25bfcaf9e8d5142ab40d3c54b33a3630/src/ThoralfPlugin/ThoralfPlugin.hs),
  [conversion](https://github.com/bgamari/the-thoralf-plugin/blob/fa4e403b25bfcaf9e8d5142ab40d3c54b33a3630/src/ThoralfPlugin/Convert.hs),
  [theory interface](https://github.com/bgamari/the-thoralf-plugin/blob/fa4e403b25bfcaf9e8d5142ab40d3c54b33a3630/src/ThoralfPlugin/Encode/TheoryEncoding.hs),
  [natural-number encoding](https://github.com/bgamari/the-thoralf-plugin/blob/fa4e403b25bfcaf9e8d5142ab40d3c54b33a3630/src/ThoralfPlugin/Encode/Nat.hs),
  and [finite-map encoding](https://github.com/bgamari/the-thoralf-plugin/blob/fa4e403b25bfcaf9e8d5142ab40d3c54b33a3630/src/ThoralfPlugin/Encode/FiniteMap.hs)
  are the useful starting points. The original plugin emits trusted nominal
  `mkUnivCo` evidence with `PluginProv`. Current GHC also requires dependency
  coercions. Core Lint assumes that plugin-supplied equalities are valid.
  The [current plugin documentation](../users_guide/extending_ghc.rst) describes
  the evidence interface and its dependency requirements.
- [Thoralf issue #21](https://github.com/bgamari/the-thoralf-plugin/issues/21)
  records multiplication work. The inspected encoding does not implement
  multiplication. [PR #25](https://github.com/bgamari/the-thoralf-plugin/pull/25)
  records compatibility work; it does not establish support for this checkout.
  No configured solver timeout was found in the inspected original code.
  Natural subtraction encoded through integers needs an underflow audit. We did
  not reproduce an underflow bug.
- [BlockScope's plugins-for-blobs](https://github.com/BlockScope/plugins-for-blobs)
  preserves related plugin implementations and examples. Treat it as comparative
  implementation material. Verify compiler support and theory semantics separately.
- [ghc-tcplugin-api](https://github.com/sheaf/ghc-tcplugin-api) provides an interface
  for type-checker plugins. Read its
  [changelog](https://github.com/sheaf/ghc-tcplugin-api/blob/main/changelog.md) when
  selecting compiler versions. An API adapter does not prove the soundness of a
  plugin's theory encoding or evidence construction.
- [ghc-typelits-natnormalise](https://github.com/clash-lang/ghc-typelits-natnormalise)
  is established prior art for specialized natural-number normalization in GHC.
  Compare this smaller theory-specific approach before adopting a general SMT
  service. Compatibility with this HEAD was not established here.

### Proof reconstruction and checking

- [lean-smt](https://github.com/ufmg-smite/lean-smt) is prior art for connecting SMT
  reasoning with Lean proof reconstruction. Its existence does not establish a
  verified translation from GHC constraints or a working GHC integration.
- [SMTCoq](https://github.com/smtcoq/smtcoq) is prior art for checking external solver
  evidence with a proof assistant. Check the supported solver, theory, certificate,
  and proof-assistant versions before selecting it.
- [Current cvc5 proof documentation](https://cvc5.github.io/docs/latest/proofs/proofs.html)
  describes current proof production and formats, including CPC and Alethe.
  The versioned
  [cvc5 1.3.4 LFSC documentation](https://cvc5.github.io/docs/cvc5-1.3.4/proofs/output_lfsc.html)
  preserves the older LFSC route. Do not combine instructions from these versions
  without checking the actual solver. Some proof configurations permit trusted
  steps; requesting proof output alone is not a complete checking guarantee.

A certificate-checking plugin would still need a sound constraint translation,
certificate checker, and GHC evidence bridge. Core would continue to trust that
bridge unless the result were reconstructed through existing checked coercion
rules. No end-to-end checked solver integration was implemented or validated in
this research.

## Length-preserving reverse: next source probe

Status: both compiler stages pass 249 source observations and seven standalone
reverse errors with pinned codes and source tokens. The runtime fixtures pass
47 package/bound rejections. Signed argument controls and all five registered
regressions pass.
The worker retains the original runtime `n` and returns `Vec a n`. It uses
the existing `Add` family and ordinary equality proofs.

Agda stdlib v2.3 supplies:

- [`Vec.Base`, lines 197–209](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda#L197-L209):
  the indexed `FoldlOp` and `foldl`. Recursion changes the motive from `B` to
  `B` composed with successor.
- [`Vec.Base`, lines 325–333](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda#L325-L333):
  `reverse` through that fold, followed by reverse-append. File blob for both
  ranges: `8c03fb8a83e6cec1fa7b8e1f64bb7e606c531cb8`.
- [`Nat.Properties`, lines 540–542](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Nat/Properties.agda#L540-L542):
  `+-suc`, which proves `m + suc n` equal to `suc (m + n)` by induction on `m`.
- [`Nat.Properties`, lines 554–556](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Nat/Properties.agda#L554-L556):
  right identity for addition, also by induction and successor congruence.
  File blob for both proof ranges:
  `3198ad7f4d783a46d30cc70751a198294d350892`.

The Haskell worker has result `Vec a (Add n m)`. In a successor
step, an explicit `Add k (S m) :~: S (Add k m)` proof permits the recursive
call with a larger accumulator. A final `Add n Z :~: n` proof gives the
length-preserving worker. The package wrapper stores the original `n`.
The reverse wrapper needs no runtime sum package or new Core equality rule.
Agda's changing fold motive avoids these explicit arithmetic casts. The Haskell adaptation
uses the fixed `Add` result instead.

Erased GADT indices cannot supply runtime Nat values. Ordinary GADTs can
still express length-preserving reverse, for example through a recursive
snoc operation. Arithmetic proofs could instead induct on a visible vector
spine. Runtime witnesses are needed by this Nat-induction algorithm;
they are not an inherent requirement of length preservation.

The wrapper can return package WHNF without evaluating either field.
Demanding the reversed vector first demands the complete finite Nat spine
through the right-identity proof, then the vector spine. Elements remain lazy.
Even an empty vector is inspected by its explicit `VNil` pattern. Partial or
cyclic indices can prevent vector observation from returning. Ordinary proof
matching forces proof values. Repeated proof induction can traverse the
remaining index at each accumulator step. An accumulator does not establish
linear total runtime. Totality, proof erasure and reversal correctness remain
separate claims. The signature relates lengths; element order needs its own
checks or specification.


## Signed expression arguments in native inference

The complete source consumer exposed `const (42 :: Int) (rightZero n)` inside
another generic application. Inference of `const` had no expected result.
The native proof supplied its second parameter, but the classifier skipped the
signed first argument. The remaining incomplete substitution led to ordinary
instantiation and GHC-83865 at the proof. The earlier failure is saved under
`_build/verification/native-reverse-source-bootstrap-run-v1.log`.

GHC already treats a signature as an inferable application head. In the pinned
upstream base, [`Head.hs`, lines 435–440](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/Tc/Gen/Head.hs#L435-L440)
recognizes `ExprWithTySig`.
[Lines 511–524](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/Tc/Gen/Head.hs#L511-L524)
check the user signature and value, then return the declared type. File blob:
`8cfe20304d8e54d1a32791f06711034f82a3709f`.

Agda uses a different surface form. Stdlib v2.3
[`Function.Base`, lines 173–177](https://github.com/agda/agda-stdlib/blob/v2.3/src/Function/Base.agda#L173-L177)
offers `_∋_ : (A : Set a) → A → A` for subexpression annotations. Its explicit
first argument supplies the checking type for the following value. File blob:
`1f8f3f0b18ee0f90f1475226cba59b31c35696e9`.
Agda v2.8.0
[`Application.hs`, lines 773–808](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L773-L808)
checks an argument against its Pi domain and instantiates the codomain.
[Lines 261–279](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L261-L279)
return the application's resulting type during inference. File blob:
`82ba94ac864d19b947d4c92b9af252b0fe81347a`.
This supports using a declared type for checking. Agda's contextual meta-solving
does not justify changing this fork's immutable native substitution rules.

The classifier now admits complete signed expressions with no outer quantified
variables. It rejects written inner quantifiers, retained binders, contexts and
type splices, including those in kinds and multiplicities. It performs no
elaboration. Normal `tcInferRho` checks and caches the signed value once.
Existing matching, kind, scope, exact-domain and final subsumption checks remain.
Type synonyms can hide polymorphism; this is a syntactic gate. Unsigned literals
and lambdas keep their expected-checking route. The annotation supplies no index
equality proof. Both signed argument positions and a signed function value pass
the controls in both compiler stages. An incorrect annotation still fails
with a located GHC-83865 error.

Five exact primary-source snapshots are saved in
`_build/verification/native-reverse-source-prior-art/`. They cover vector reverse,
addition proofs, Agda's annotation helper and application checker, and GHC's
signature checker. Their Git blob hashes match the pinned files above.


## Later retained arguments in constructor equations

Status: both compiler stages pass 269 source observations and eight new located
errors. The runtime fixtures pass 55 package/bound rejections. The previous
seven reverse errors and all five registered regressions pass. The source reverse proof uses
`addSucc Z m` and `addSucc (S k) m`. The checker must consume the later retained
binder before it checks the specialised RHS. Reuse the existing native index
and nominal constructor proof rules.

Agda v2.8.0 [`LHS.hs`, lines 1289–1297](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1289-L1297)
separates the earlier telescope, split argument and later dependent telescope.
[Lines 1427–1450](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1427-L1450)
describe the constructor substitution and its lifting over later arguments.
[Lines 1503–1532](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1503-L1532)
apply that substitution to `delta2`, remaining patterns, target and equations.
[Lines 594–620](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L594-L620)
retain the checked telescope, specialised body type and pattern substitution.
File blob: `3779ffa6e418bca99e30c071e189106f82ecbe1b`.

[`Def.hs`, lines 712–749](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Def.hs#L712-L749)
consume that result and scope inherited/as-pattern bindings.
[Lines 825–852](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Def.hs#L825-L852)
check the RHS against its specialised target. File blob:
`0532811dabeb3a7e6dd3f08cfa18c08472072fe1`.
The exact files are already saved in M17's frozen prior-art directory.

These sources justify consistent telescope specialization. They do not supply
GHC's shared runtime Id representation or Haskell demand semantics. M19 keeps
all source argument lambdas before constructor discrimination. It renames
resolved source references and stored free-name sets consistently, keeps
original source equations for diagnostics, and excludes quotations before that
rewrite. Each shared retained argument keeps the same closed domain and
remains an outermost variable pattern. Ordinary payload transport remains
inside the constructor proof scope. No alias supplies index equality.


The evidence-order probe exposed a separate signature-walker boundary at a
qualified body. The implementation now uses the ordinary context routines,
then recurses through the body with the retained signature walker.
At upstream GHC base `19ff36f2aede0ca2e554016ff22c71e73aa9803b`,
[`Rename/HsType.hs`, lines 600–607](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/Rename/HsType.hs#L600-L607)
uses `rnTyKiContext` before renaming the qualified body. File blob:
`01d0688febd8cee4493a176fce13ed80e08fa72f`.
[`Tc/Gen/HsType.hs`, lines 1192–1211](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/Tc/Gen/HsType.hs#L1192-L1211)
uses `tc_hs_context` and `tcMkPhiTy` for a qualified value type. File blob:
`cacb81f68ffcc9061370399171d10eba664754f4`.
M19 retains those constraint checks and evidence arrows. Its retained body has
kind `Type`. Later retained binders do not scope over preceding constraints.
The syntax selector must also traverse qualified bodies so a leading class
context selects the retained checker. Exact GHC and Agda files are saved under
`_build/verification/native-later-index-prior-art/`.


## Two-index constructor equations

Status: M20 implemented. Both compiler stages and all five registered
regressions pass. Agda stdlib v2.3
[`Decidability.agda`, lines 67–73](https://github.com/agda/agda-stdlib/blob/v2.3/doc/README/Design/Decidability.agda#L67-L73)
defines `_≟₀_` with four Nat constructor pairs. Its successor/successor clause
recursively checks the predecessors and lifts successful equality with
`cong suc`. File blob: `c6f086424f11ddd18ce7bb8f91526dda5472f300`.
This is the program precedent for four source `sameNat` equations.
Agda returns `Dec (m ≡ n)` with a refutation on failure. Haskell `Nothing`
contains no refutation. [Lines 75–89](https://github.com/agda/agda-stdlib/blob/v2.3/doc/README/Design/Decidability.agda#L75-L89)
describe a separate decision/proof implementation. The Haskell worker
still inspects the recursive result before returning successful `Just`.

The production stdlib [`Nat.Properties`, lines 93–109](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Nat/Properties.agda#L93-L109)
uses Boolean comparison and proof generation. It is a different algorithm.
That file's pinned blob is `3198ad7f4d783a46d30cc70751a198294d350892`.

Agda v2.8.0
[`CompiledClause/Compile.hs`, lines 53–91](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/CompiledClause/Compile.hs#L53-L91)
obtains a coverage split tree for checked clauses.
[Lines 126–162](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/CompiledClause/Compile.hs#L126-L162)
compile its branches recursively. File blob:
`962a80bbaee38601c392a2f9bfd2af8ea0f972d1`.
The LHS substitution ranges above supply successive scoped refinements through
later patterns and the RHS target. These sources do not establish Haskell
field identity, row demand, proof erasure or totality. The GHC probe must check
those boundaries separately. Both actual runtime lambdas must precede matching.


M20 uses shared runtime Ids and one nominal constructor proof per selected
index. Its transports compose in telescope order. GHC Notes
`[Retained constructor matrices]` and `[Retained constructor equation scope]`
state the frontend and desugaring invariants. No Core equality rule or
interface schema changed. The local six-file Agda/GHC snapshot is copied from
M19 into `_build/verification/native-index-matrix-prior-art/`.

For meaningful negative probes, both indices must occur free in their types.
[`mkDepFunTy`](../../compiler/GHC/Core/TyCo/Rep.hs) keeps a `DepFunTy` only when
its binder occurs free in the body. Note `[Restricted retained dependent types]`
states this invariant. Otherwise the binder becomes an ordinary unrestricted
value arrow. This inherited simplification keeps the runtime argument.


## Ordered variable fallback equations

Status: M21 implemented. Both compiler stages, the located-error checks and
all five registered regressions pass. Agda v2.8.0
[`CompiledClause/Compile.hs`, lines 126–172](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/CompiledClause/Compile.hs#L126-L172)
keeps a catchall branch and terminal `Done`/`Fail` clauses.
[Lines 179–244](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/CompiledClause/Compile.hs#L179-L244)
select a split from left to right and treat `VarP` as nonsplitting.
[Lines 314–343](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/CompiledClause/Compile.hs#L314-L343)
preserve or expand catchalls in relation to following constructor clauses.
[Lines 381–396](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/CompiledClause/Compile.hs#L381-L396)
substitute an expanded constructor into the RHS. File blob:
`962a80bbaee38601c392a2f9bfd2af8ea0f972d1`.

Agda's [`LHS.hs`, lines 282–310](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L282-L310)
treats an all-variable problem as solved. Its constructor substitution remains
as pinned above at lines 1427–1451. File blob:
`3779ffa6e418bca99e30c071e189106f82ecbe1b`.
These sources support terminal variable rows. They do not prove Haskell
laziness or the scope of GHC's virtual constructor CoVars.

GHC's ordinary [`Match.hs`, lines 176–209](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/HsToCore/Match.hs#L176-L209)
keeps ordered constructor and variable blocks. Its
[variable matcher, lines 243–246](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/HsToCore/Match.hs#L243-L246)
drops a column without demanding it.
[Lines 412–420](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/HsToCore/Match.hs#L412-L420)
tidy variable and AsPat bindings through `bindMatchId`.
The file is unchanged from base `19ff36f2aede0ca2e554016ff22c71e73aa9803b`;
its blob is `f6c63c37c5cfae10d11a9dde22829ae25adf3106`.

[`Utils.hs`, lines 230–240](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/HsToCore/Utils.hs#L230-L240)
combines the next block with a failed match.
[Lines 253–262](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/HsToCore/Utils.hs#L253-L262)
omit a redundant same-Id alias.
[Lines 906–913](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/HsToCore/Utils.hs#L906-L913)
bind the shared failure thunk outside the preceding constructor case. Its
unchanged blob is `c3ae336c9b2a0f9f55a0fc879905480414be544a`.

M21 reuses those paths. Before TC, rename a fallback's two source variable
Names to the shared whole-index Names. Keep its checked VarPats unwrapped and
check its RHS without constructor proofs. A fresh alias would not establish
native index identity. M20's separate constructor rows keep their actual
fields and proofs. This change adds no backend or Core rule.


## Anonymous terminal fallback equations

Status: verified M22, frontend `7b7586bfe0`, source/fixture `2435b1a7df`.
Both compiler stages pass variables and wildcards in the two selected columns
of the one terminal fallback. Constructor prefixes remain unchanged.

Agda v2.8.0 [`LHS.hs`, lines 291–306](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L291-L306)
treats `A.VarP` and `A.WildP` as solved patterns.
[Lines 468–481](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L468-L481)
retain an internal `VarP` and record the source wildcard as `PatOWild`.
[Lines 555–573](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L555-L573)
keep anonymous telescope slots and generate names outside source scope.
File blob: `3779ffa6e418bca99e30c071e189106f82ecbe1b`.
These rules support anonymous internal bindings. They do not select GHC Uniques
or establish Haskell demand semantics.

At GHC base `19ff36f2aede0ca2e554016ff22c71e73aa9803b`,
[`Tc/Gen/Pat.hs`, lines 641–646](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/Tc/Gen/Pat.hs#L641-L646)
checks a variable binder and extends the Id environment.
[Lines 684–688](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/Tc/Gen/Pat.hs#L684-L688)
check wildcard multiplicity without binding an Id. File blob:
`81efc26e865fcb090315091724f34b3aa4f25a06`.
[`HsToCore/Utils.hs`, lines 132–146](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/HsToCore/Utils.hs#L132-L146)
selects the variable pattern's Id; its generic path allocates a fresh Id.
File blob: `c3ae336c9b2a0f9f55a0fc879905480414be544a`.
[`HsToCore/Match.hs`, lines 406–413](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/HsToCore/Match.hs#L406-L413)
tidies a wildcard with an identity wrapper and a variable with `bindMatchId`.
File blob: `f6c63c37c5cfae10d11a9dde22829ae25adf3106`.

The GHC adaptation replaces each selected wildcard with the existing
shared index `VarPat` before TC. It adds no source-visible name, fresh index Id
or alias. Both typed index patterns remain outermost and unwrapped. The fallback
has zero constructor proofs and reflexive transports. Check its incoming result
type without evidence from failed rows. Preserve source lambdas and ordered
failure continuations. Keep the original Match through `tcMatchWithSource` for
errors. Reject quotations even when neither column has a source variable Name.
Constructor/wildcard rows and other pattern extensions remain outside M22.
No Core form or equality rule changes. Both stages pass 331 source
observations and 42 standalone located errors. The API/runtime fixture passes
76 package/bound rejections. All five registered regressions pass.


## Checked local payload signatures

Status: both M23 stages pass 343 source observations, 53 standalone located
errors and four ordinary controls. The API/runtime fixture passes 87
package/bound rejections. The previous 331 source outputs remain unchanged.
All five registered regressions pass. Frontend: `d45eac2a85`; lexical validity metadata normalization:
`da16ec16e6`; source/API: `58fe0a1e10`; formatting: `e4766d99ac`.
Complete local signatures can capture eligible indices already in lexical
scope. Unsigned native inference remains outside this target.

Agda v2.8.0 [`Rules/Term.hs`, lines 1662–1672](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L1662-L1672)
checks a let binding's declared type in the current context. It checks the RHS
against that type before extending the context with the checked value.
File blob: `02991c9fa0085315cc854d2833a9080af14a7740`.
[`Monad/Context.hs`, lines 434–438](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Monad/Context.hs#L434-L438)
stores the value and type with `makeOpen` in the lexical let environment.
File blob: `98e50ce60319dcb47a542ac7c04b4b3e491a64e9`.
The full tagged sources and their blob checks are retained with M22 evidence.

Agda's context discipline supplies scope prior art. Its transparent lets and
totality rules do not justify unfolding Haskell values or equating aliases.
The GHC adaptation captures the actual already-bound local runtime
Name for a complete local value signature under `ExperimentalDependentTypes`.
An in-scope erased type variable takes precedence. Otherwise, consider only
unqualified free type-variable spellings in the local term reader environment.
Install a signature-local lookup alias before implicit forall collection. The
actual Name remains in the value namespace and a free signature dependency.
Explicit and nested type binders keep normal scope. No local term candidate
means ordinary type-variable handling continues.

GHC's `rnLocalValBindsAndThen` brings the whole recursive group's Names into
scope before signature renaming. The `LocalBindCtxt` group set must therefore
exclude same-group capture. `TypeSigCtx` alone is insufficient. Never recover
an outer term binder hidden by a newer same-spelling binder. Globals, future
bindings and unavailable group binders do not supply ambient runtime indices.
TC must find the selected Name's actual local Id, read its assigned metadata
without changing its Unique, and validate its closed domain and Many
multiplicity. Reject CoVars and nonlocal Ids. Do not create a prototype index
or recover from invalid capture by inventing an erased forall.

Use complete renamed signatures, with no named or anonymous holes. Top-level,
class, declaration, expression and pattern signatures keep their existing
behavior. Unsigned native inference and native meta assignment remain unsupported.
Unavailable or invalid captured indices need located source diagnostics;
wrong-index payloads retain ordinary type errors. Flag-off and erased-variable
controls must keep ordinary Haskell behavior. Signature capture must not force
an index, payload or runtime IO action. Validity zonks the raw lexical binding
before comparing its metadata with the captured occurrence. This preserves its
Unique and checks that both domains agree. M23 adds no Core, backend or
interface extension. Both compiler stages are verified.

Free type-variable spellings can change meaning under the experimental flag.
With runtime `m` in scope, `depth :: LE n m -> Int` captures `m`. Use explicit
`forall n m.` to retain erased polymorphic intent. Explicit type binders win.
Upstream GHC already selects term-variable capture for RequiredTypeArguments:
[`Rename/HsType.hs`, lines 2005–2012](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/Rename/HsType.hs#L2005-L2012).
File blob: `01d0688febd8cee4493a176fce13ed80e08fa72f`.
The full baseline file is copied into `native-local-signature-prior-art`.
Preserve that flag's existing behavior independently of M23's local extension.


## Direct do bind of a saved native action

Status: M24 verified in both stages. Each passes 360 source observations, 60
standalone located errors and four ordinary controls. The API/runtime fixture
passes 94 package/bound rejections. All five registered regressions pass. A
saved action retains its actual native payload index through ordinary do bind.

Agda v2.8.0 [`Syntax/DoNotation.hs`, lines 79–100](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/DoNotation.hs#L79-L100)
translates local lets and distinguishes simple binders from matching patterns.
[Lines 131–134](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/DoNotation.hs#L131-L134)
build the ordinary bind application with a lambda for a simple binder.
[Lines 114–123](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/DoNotation.hs#L114-L123)
use an extended lambda for pattern matching. File blob:
`9694ea38208a3179044428ef6c76600149c59e53`.
The full tagged file is saved in `native-local-signature-prior-art`.
The [manual translation table, lines 132–155](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/syntactic-sugar.lagda.rst#L132-L155)
shows ordinary bind, pattern bind and let expansion.
[Lines 270–284](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/syntactic-sugar.lagda.rst#L270-L284)
use checked indexed results in do notation. Manual file blob:
`a0ccea26ab40c6ca30d54f304eee6828f5d12e88`.
These are syntax and context precedents. They do not establish Haskell IO
sequencing, bottom demand, eta laws or GHC's immutable matching algorithm.

Agda v2.8.0 [`Rules/Term.hs`, lines 560–585](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L560-L585)
checks a lambda domain, extends the context with `lambdaAddContext`, and
checks the body against the scoped codomain.
[Lines 678–710](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L678-L710)
continue this process for remaining binders. File blob:
`02991c9fa0085315cc854d2833a9080af14a7740`.
This supplies the precedent for checking the generated continuation after its
native input domain is fixed.

[`Implicit.hs`, lines 81–101](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Implicit.hs#L81-L101)
inserts omitted arguments in telescope order and substitutes each into the
remaining codomain. [Lines 114–124](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Implicit.hs#L114-L124)
create unification metas with an occurs check. File blob:
`55a03200d2300486c91a95798fc8e0c3c9c675cf`.
[`MetaVars.hs`, lines 282–321](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/MetaVars.hs#L282-L321)
records the current telescope and applies a new meta to its context arguments.
File blob: `13f52cb7f2a08ac55f62e326564043470e62f6f6`.
[`Rules/Application.hs`, lines 774–808](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
checks a Pi argument and substitutes the checked argument into the codomain.
File blob: `82ba94ac864d19b947d4c92b9af252b0fe81347a`.
The five exact tagged Haskell sources are copied into
`native-action-bind-prior-art` and checked by blob and SHA256.

Agda uses contextual mutable metas. M24 instead preserves native choices in
an immutable substitution and uses GHC's existing `newMetaTyVarX` only for
ordinary unresolved parameters. Agda's inference rules do not justify native
meta assignment, Haskell strictness, evidence movement or index equality.

At fork commit `da16ec16e6`, `Tc.Gen.Expr` at line 582 and `Tc.Gen.Do` at line 101 expand the
ordinary bind into an operator application and continuation. In `Tc.Gen.App`,
`tcRetainedArgInstEnabled` zonks cached actual types at line 1872, but
`result_subst` uses the raw expected type at lines 1903–1908. Nine isolated
source probes at fork commit `0e979288f8` fail with GHC-83865. The
`native-action-bind-baseline-v1/SavedChecked/Main.dump-tc-trace` assigns the
result and monad metas before the native action bind. The cached action keeps
its exact runtime index, but immutable matching reads the raw expected metas.
Normalize existing assignments through `readExpType_maybe` and the within-TC
zonker. An empty infer cell must remain empty. A genuinely unknown ordinary
continuation result also needs a fresh ordinary parameter while the native
input stays fixed. GHC's `newMetaTyVarX` supplies existing telescope-order
freshening with substituted kinds. No native type may enter those metas.
The normalization increment `d0e9085d79` fixes four saved-action probes. The
ordinary-parameter increment `e64655aa18` also fixes both local inferred
computations. Inline `pure` still fails: its trace infers `f (Vec Int n)` with
an ordinary head meta before the enclosing bind checks an `IO` result. A
separate exact joint argument match can preserve that actual head. Final
ordinary result subsumption must still resolve it or reject the program.
Reject fresh binders whose substituted kinds are native domains.
The full source probe also exposes missing expected-result anchors for
polymorphic bottom payloads. A successfully matched native result choice can
anchor generic instantiation even when no argument's uninstantiated type has
native syntax. Existing `needed_tvs` then leaves determined domains for normal
expected checking. For `error`, match the result kind before the result type
with the same outer-binder predicate. GHC's existing dollar helper uses this
two-step immutable match. Core.Unify's Note `[tcMatchTy vs tcMatchTyKi]`
explains why the type-only matcher can leave a runtime-representation binder
undetermined. Keep the original HasCallStack theta and concreteness gates.
Preserve the existing
native-meta and general Quick Look gates, immutable kind/scope/concreteness
checks, once-only usage, local evidence and final subsumption. M24 must support
the direct source bind, without a singleton or package conversion workaround.

## Scoped native source inference

M25 is verified in both compiler stages with 377 source observations,
69 standalone rejection cases, four ordinary controls and 103 API/runtime
package/bound rejections. All five registered regressions pass. M24 preserved
two source failures: a local unsigned native action reached the inferred-type
gate; a callback that returned its native payload required an ordinary result
meta to contain native syntax. M25 uses inference cells and exact immutable
choices before that allocation.

Agda v2.8.0 [Term.hs, lines 1662–1672](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L1662-L1672)
checks a let type and value in the lexical context before adding the binding.
An inserted annotation uses `checkDontExpandLast`. The file blob is
`02991c9fa0085315cc854d2833a9080af14a7740`.
[Context.hs, lines 434–438](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Monad/Context.hs#L434-L438)
stores the type and value with `makeOpen`. Its file blob is
`98e50ce60319dcb47a542ac7c04b4b3e491a64e9`.
This supplies lexical-scope prior art. GHC already infers a single
nonrecursive FunBind RHS before allocating its Id. M25 admits that exact type
only for a local NoGen value with one zero-argument equation and a monotype.
Use the existing validity checker to validate free runtime indices against
their actual lexical bindings. Keep generalization and other binding shapes
at their existing gates. A Haskell let remains lazy and opaque as an index.

Agda [Term.hs, lines 685–710](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L685-L710)
checks the argument domain, adds it to the lambda context and checks the body
under that context. GHC's `tcMatches` already accepts a known pattern type
and a result inference cell. Reuse it for a one-argument ordinary callback
after a native action fixes its input. Check the synthesized result after
the callback context has closed. A free callback or inner local runtime Id
cannot appear in an ordinary arrow result. Report the callback body span.
An explicit retained arrow or package needs its own source checking path.

Agda [MetaVars.hs, lines 282–321](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/MetaVars.hs#L282-L321)
captures the current telescope when it creates a contextual meta. File blob:
`13f52cb7f2a08ac55f62e326564043470e62f6f6`.
GHC's adaptation does not assign native syntax to a meta. Preserve the existing
inference-cell level, promotion, branch-joining and GADT escape checks.
Determine the native payload before allocating an ordinary callback-result
parameter. Final ordinary subsumption must still resolve an inferred monad
head. Keep usage collection and typed-argument caching once per argument.

The first M25 source run exposes `effect >> pure vector`: the first action
fixes IO, while the second infers an ordinary head meta. The pure immutable
matcher rejects those heads in both complete candidates. Use GHC's existing
`unifyType` only on head types without native syntax or outer templates, and
require a head meta. Repeat exact whole-argument matching afterwards. Keep
native payload choices immutable. This uses ordinary GHC unification; Agda's
contextual metas do not justify native assignment. Preserve the failed run
and the source programs while validating this increment.

The validity audit at `fecdc2a991` finds that the structural `check_type` walk
skips `TyVarTy` kinds and `CastTy` coercions. Reuse `checkValidType`'s existing
deep free-variable list for lexical runtime-index validation before its
structural walk. `TyCo.FVs` follows kind/coercion metadata and excludes bound
retained indices. Exclude CoVars explicitly. This establishes the required
scope check across metadata; no source-level exploit is claimed.

## Boxed tuple inference prior art

M26 is verified in both compiler stages with 395 source observations,
79 located rejection cases and 113 API package/bound rejections. All five
registered regressions pass. The M25 baseline compiled constructor/helper
forms but rejected tuple syntax with GHC-83865.
At `6cca33589e`, `Tc.Gen.Expr` lines 451–465 first converts the expected type
to a Type and matches the tuple TyCon, creating component metas. Its boxed
`tcInferTupArgs` branch at lines 915–920 also checks fields against fresh
metas. Those whole-field metas cannot contain a native vector type. The
ordinary constructor application route already preserves exact native choices.

Agda stdlib v2.3 [Data.Product.Base, lines 68–74](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Product/Base.agda#L68-L74)
defines the ordinary product using a constant Sigma family. File blob:
`02f7e5269732fea8275074a03e1c94b3397fc60b`.
Agda v2.8.0 [Builtin.Sigma, lines 7–11](https://github.com/agda/agda/blob/v2.8.0/src/data/lib/prim/Agda/Builtin/Sigma.agda#L7-L11)
defines its two stored fields and constructor. File blob:
`d68aead018bb834aec9a547a7907576005c28dcf`.
[Rules.Application, lines 774–808](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
supplies the existing argument-checking and codomain-substitution precedent;
its file blob is `82ba94ac864d19b947d4c92b9af252b0fe81347a`.
Exact source files, Git blob checks and retrieval metadata are recorded with
the M25 and M26 prior-art artifacts. M26 rechecks all three Git blob hashes.

M26 reuses GHC's constructor application inference. `mkExpandedExpr` in
`GHC.Rename.Utils`, `splitHsApps` in `GHC.Tc.Gen.Head`, and
`[Typechecking by expansion: overview]` in `GHC.Tc.Gen.Expand` supply the
source-context precedent. At M25 commit `e0e3e893bd`, the helper and split
start at lines 830 and 295. Generated head/application spans preserve field
locations and the original tuple context. The rule has one owner in
`[Inference of native tuple fields]`; the classifier refers to it.

The M26 baseline compiled an ordinary unit case returning the tuple and the
equivalent full constructor application with Lint. An applied native
tuple section rejected with GHC-83865. M27 now verifies the separate section
path, including runtime behavior. The original compile/Lint failures remain.

A GHC tuple with component `Vec Int outerN` still refers to that outer index.
The tuple does not bind a new index in its second field type. A callback-local
Nat cannot escape by returning an ordinary tuple. Use the explicit dependent
package for that relation. Preserve Haskell lazy fields and usage separately;
Agda's product and constructor rules do not establish those properties.

## Native tuple sections and application spines

M27 rechecks the three Agda blobs above and `Rules.Term` blob
`02991c9fa0085315cc854d2833a9080af14a7740`.
[Term.hs, lines 685–710](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L685-L710)
checks a lambda against its expected domain and codomain under the extended
context. This supports checking a section against known component types.
An ordinary product still does not bind a runtime index. Agda's rules do not
establish Haskell field demand, sharing or linear usage.

GHC's own section implementation supplies those operational rules. At
`6224c625d7abf5c777e447faa0ece2b4e8833672`, `GHC.HsToCore.Expr` lines 393–408
fold missing fields into lambdas in source order. Present expressions remain
inside their body. There is no new sharing let. M27 keeps this AST and
desugaring. `GHC.Tc.Utils.Unify` lines 1443–1495 retain the whole-function
wrapper; projection must not discard a domain/result cast or a forall/context.
`testsuite/tests/linear/should_compile/LinearConstructors.hs` supplies the
existing four combinations of One/Many hole multiplicities. Pinned source
files, Git blob checks and decisions are in the M27 prior-art artifacts.

The M28 baseline isolated a different application-spine boundary. A named
`fst pair` projection and later application compiled; the direct flattened
application rejected in both M27 stages. M28 now verifies the direct form.
The pinned Agda `Rules.Application`
argument-checking and codomain-substitution rule supports consuming a spine
in stages. The GHC adaptation must infer the declared argument prefix before
checking extra arguments. A final expected result after those arguments is
not the intermediate result used to choose `fst`'s erased parameter.

## Generic result overapplication prior art

M28 rechecks the pinned Agda `Rules.Application` and `Rules.Term` blobs above.
`Application.hs` lines 774–808 check the argument against the current Pi domain,
then continue with `absApp b u`. This supplies the precedent for consuming
the application spine in stages. The GHC adaptation chooses erased parameters
from a declared argument prefix, then checks extra arguments at the selected
function type. It does not assign native syntax to an ordinary meta.

Upstream GHC commit `19ff36f2aede0ca2e554016ff22c71e73aa9803b` supplies the existing
pipeline in `GHC.Tc.Gen.App`. See `[Application chains and heads]`,
`[tcApp: typechecking applications]` and `tcApp` lines 399–448. Instantiation,
result checking and argument checking retain their original sequence.
M28 keeps the full application spine and original evidence pipeline. Its
owning `[Generic retained argument inference]` Note records the empty prefix
substitution and generic-only saturation rule. The earlier cache Note refers
to that owner. Source snapshots and Git blob checks are in the M28 prior-art
archive, including the M27 checker used for the failing baseline.

The cache check uses `tcApp:inferAppHead` trace blocks to count one elaboration
of a named prefix application after joint prefix matching fails. A TH `runIO`
counter alone would not establish that property. In the pinned M27 source,
`GHC.Rename.Splice` lines 489–498 execute untyped splices during renaming.
`GHC.Tc.Module` checks errors before zonking and finalizers, so typed splices
and finalizers cannot measure this failing TC path. Preserve the normal
trace's tentative messages separately from emitted error diagnostics.
Linear positive/negative controls check usage independently.

## Native inference after leading visible type applications

M29's compiler implementation is `37963035139963313ab0c16bbe06fbf3870c4d9a`.
Both stages pass 461 source observations and 24 focused application cases:
ten positive and fourteen negative. They preserve 112 located rejections and
four ordinary runtime controls. The API fixture passes 146 rejection cases;
all five registered regressions pass.
The original M29 baseline rejected partial native `@Bool` in both M28 stages,
while omitted VTA and complete ordinary VTA compiled. Keep that baseline
separate from the bootstrap implementation results.

Agda v2.8.0 [`Rules/Application.hs`, lines 774–808](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
checks an argument against the current Pi domain, records the checked argument,
and continues with the substituted codomain `absApp b u`.
[Lines 449–465](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L449-L465)
retain the final application-result check. File blob:
`82ba94ac864d19b947d4c92b9af252b0fe81347a`.
[`Rules/Term.hs`, lines 685–710](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L685-L710)
checks a lambda under the domain and codomain supplied by its current expected
Pi. File blob: `02991c9fa0085315cc854d2833a9080af14a7740`.
Both exact tagged files are already pinned in the frozen M28 prior-art archive.
They support staged application checking and telescope substitution. They do
not supply GHC's erased VTA syntax, immutable inference policy or usage rules.

GHC's existing application pipeline is pinned at
`dad8b99f6f8b50b514e2e1ef149aa66e90351456`.
In `GHC.Tc.Gen.App`, lines 882–890 implement IALL's invisible-binder and
class-dictionary instantiation. Lines 979–984 implement ITYARG through `tcVTA`
and retain the checked type argument. File blob:
`19670a6fe282f97fedc8ce80c8bfc8cff997e07f`.
M29 keeps that pipeline. After leading type arguments, native inference uses
the substituted signature before IALL allocates the remaining variables.
It keeps the original head and checked arguments, then passes immutable
choices through IALL in the original type-argument and evidence order.

The owning Note is `[Native inference after visible type applications]` in
`GHC.Tc.Gen.App`. Only `tcApp` enables the cache-consuming route. Require NoQL,
no consumed value arguments, no remaining type applications and the existing
generic inference guards. The native-index-kind prepass remains separate.
Keep inferred caches on failed matching. Record an attempt so that later IALL
steps cannot elaborate the same arguments again. Public `tcInstFun` callers
that cannot consume the cache leave this route disabled. Preserve native-meta,
kind, lexical scope, concreteness, exact-domain and final subsumption checks.

Cache keys count value arguments. Diagnostic positions also count written
type arguments. Generated type arguments advance neither source diagnostic
positions nor value-cache keys. Emit each cached usage once at the actual
argument multiplicity. A generated type argument is still an application-spine
argument; it can enable the post-type-application cache route.

Record-dot expansion supplies the concrete generated-argument precedent.
At the same pinned GHC commit, `GHC.Rename.Expr` lines 419–425 expand
`HsGetField` while retaining its original expression. Lines 2883–2904 contain
`mkGetField` and `mkGet`. The latter builds `getField @field record` with
`genAppType` and `wrapGenSpan`. File blob:
`b086a0356ce48938a7ecc6d6ad2086b5b341c350`.
This generated label must not shift the user's value-argument diagnostic.
The separate M29 archive is `native-leading-vta-prior-art-v1`. Preserve the
frozen M28 source and evidence. M29 adds no Core form or equality rule.


## Native-index-kind inference after leading VTA

Both M29 stages compile an erased Nat-indexed `observe` without VTA and with
complete ordinary `@Int @Z`. Partial `@Int` rejects with GHC-83865 at
`localVector n`, with a second-argument context. The baseline sources,
commands, emitted diagnostics and tokens are in
`native-leading-vta-probes-{bootstrap,selfhost}-next-v1`. These are compile/Lint
checks. They make no runtime claim about the rejected form.

M30's compiler implementation is `459add4e28`, with diagnostic correction
`651ddfe0f7`. Both stages pass 479 source observations, 126 located rejection
cases and four ordinary controls. The new matrix has 24 cases. The API fixture
passes 160 rejection checks. All five registered regressions pass. M30 reuses
the existing native-index-kind matcher after leading ITYARG. For example,
the source target is:

```haskell
observe :: forall a (index :: Nat). Vec a index -> Int
observe _ = 42

use :: foreach (n :: Nat) -> Vec Int n -> Int
use n xs = observe @Int xs
```

ITYARG checks `@Int` and substitutes `a`. The remaining erased parameter
`index :: Nat` can then match the runtime index `n` in the actual type of `xs`.
That match supplies an immutable type-argument choice. It does not evaluate
`n`, pass a singleton, or put native syntax into an ordinary type meta.

Agda v2.8.0 [`Rules/Application.hs`, lines 774–808](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
checks the current Pi argument and continues with the substituted codomain
`absApp b u`. [Lines 619–643](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L619-L643)
insert implicit arguments before checking the next supplied argument.
The exact tagged file in the M29 archive has Git blob
`82ba94ac864d19b947d4c92b9af252b0fe81347a`.
These rules support staged telescope substitution. Agda's contextual metas
and Pi forcing do not justify relaxing GHC's native-meta or scope restrictions.

The GHC implementation precedent is `GHC.Tc.Gen.App` at
`51b8c1b41d010ecd1ba59f99efba7c542029b2db`, Git blob
`f34aa668c0d8a28d25878104c0f2028ea22cd3a0`. IALL starts at line 957;
ITYARG starts at line 1054. The removed guard was at line 1951.
Its native-index-kind prepass already performs immutable matching. Its M29
post-VTA route already returns choices through IALL in the original binder
and dictionary order. M30 removes the exclusion
`isNothing after_vta || null index_tvs`. The owning Note
`[Native inference after visible type applications]` now states both policies.
Native-index-kind inference requires exact saturation. Only the separate
generic route can use a nonempty declared argument prefix for overapplication.

Keep NoQL, the extension guard, the eligible original head, no consumed value
arguments and no remaining type applications. Keep all required immutable
choices, substituted-kind checks, exact argument types and lexical runtime-Id
scope checks. Preserve native-meta and concreteness guards. Retain successful
and failed-match caches, separate value/source counters, once-only usage and
final subsumption. Public callers that cannot consume the cache remain gated.
Calls without type arguments keep the existing prepass.

The diagnostic correction passes `pos + count written_type_arg acc` to
`quickLookArg`. Its integer argument supplies `addArgCtxt` in `quickLookArg1`.
It does not select matches, cache entries or instantiation choices. At the
pinned M29 source above, `tcValArgsWithCache` lines 638–649 already separate
value positions from source positions. Lines 3399–3401 use the Quick Look
position for argument context. The correction applies that existing source
convention before final argument checking too. For `observe @Int xs`, the
GHC-83605 context now identifies the second argument rather than the first.
The error location remains `xs`. Generated `ETypeArg` entries do not count.

M30's separate archive is `native-index-vta-prior-art-v1`. Preserve the frozen
M29 evidence. This change adds no Core representation or equality rule.


## Joint native-index and payload inference: next boundary

M30's strict compile/Lint baseline is
`native-index-vta-probes-{bootstrap,selfhost}-next-strict-v1`. Both stages
accept a named exact-saturated native-function selection from a combined
vector/payload tuple and a complete ordinary VTA control. Direct native
selection, its omitted-VTA counterpart, a separate native payload and a
separate ordinary function payload reject with located GHC-83865. The first
discovery expected that ordinary payload control to pass; the strict v2
dataset records the observed rejection. No runtime behavior is claimed for
rejected forms.

At M30, `tcRetainedArgInstEnabled` selects `index_tvs` as the domains to infer
for the index route. `tcRetainedArgChoices` requires complete immutable
choices on that route. An independently quantified payload can therefore
remain undetermined even when its actual argument is ordinary. M31 will
match these parameters together from the declared prefix, then use GHC's
existing application pipeline for extras. The pinned Agda application spine
and M28 empty-substitution rule for overapplication remain the precedents.
Do not infer missing native indices from extra-only anchors or assign native
syntax to mutable metas.


## Joint native-index prefix inference and result overapplication

M31's initial compiler implementation is `0265297eb3`. It exposed a regression
in the existing source program. Commit `1728237cc8` changes inference to two
phases. With that correction, bootstrap passes all 503 source observations
and 28 focused compile/Lint cases: fourteen positive and fourteen negative.
These include a genuine retained-function payload and its runtime application.
Both stages now pass 503 source observations, 139 located rejections and four
ordinary controls. Both failed-match cache traces elaborate `prefixProbe` once.
The API fixture passes 173 rejections. All five registered regressions pass.
The full GHC suite was not run.

The GHC baseline is `GHC.Tc.Gen.App` at
`b7286565696f21de8c87743b2171c2e58e3fb87c`, Git blob
`3550f031fc573d658fe1d9379ad8986f9aa92912`.
Its index route inferred domains containing `index_tvs`, then required complete
immutable choices. An independent payload parameter could remain unknown.
Its exact-saturation guard also excluded extra arguments on that route.
M31 reuses the same matcher, argument cache, IALL and final application checks.
It does not add a Core representation, equality rule or interface format.

Agda v2.8.0 [`Rules/Application.hs`, lines 774–808](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
checks the argument at the current Pi domain. It records that argument and
continues with the substituted codomain `absApp b u`.
[Lines 619–643](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L619-L643)
insert implicit arguments before checking the next supplied argument.
The pinned file blob is `82ba94ac864d19b947d4c92b9af252b0fe81347a`.
These are telescope-substitution and application-spine precedents. Agda uses
contextual metas. It does not implement the index-first policy below or
justify changes to GHC's native-meta, usage or strictness restrictions.

The owning Note `[Cached retained argument inference]` records the two phases.
First infer and match index-bearing domains. Then infer eligible independent
domains only for parameters that remain unknown. Use
`tcRetainedInferableArg` for those domains and skip already cached positions.
Independent domains whose parameters are fixed by the expected result or
the index arguments retain their ordinary expected-type checks.

For `filterBounded (const False) xs`, the vector determines the predicate's
element type. Inferring the predicate first can leave an ordinary meta as a
rigid actual type in the immutable match. The later vector cannot repair that
candidate. The two-phase policy first obtains the element type from `xs`,
then checks the predicate against its known domain. This is a bounded GHC
adaptation. It is not Agda's unification algorithm.

The owning Note `[Generic retained argument inference]` records extra arguments.
Match only the nonempty declared argument prefix. At exact saturation, keep
the existing expected-result seed. With extra arguments, start from an empty
substitution: the final result follows those arguments, so it cannot seed the
intermediate selection. Keep the original application spine. IALL and final
checking consume the extra arguments, dictionaries and written type arguments
in order. Do not infer missing native indices from extra-only arguments.

New independent-domain and extra-argument inference requires a head without
concreteness requirements. Concrete heads keep exact, index-only inference.
The index route still requires complete immutable choices. Preserve NoQL,
kind, exact cached-domain, lexical runtime-Id scope and native-meta guards.
Preserve once-only argument usage, failed-match caches and final subsumption.
Both leading-VTA and omitted-VTA calls use the existing application pipeline.
The separate source archive is `native-joint-prefix-prior-art-v1`.

## Outer Given evidence beneath a retained Pi

M32 implements existing Given evidence under a fixed-domain retained Pi.
Commit `a00972ede9` adds the scoped rewrite filter. Commit `9764958bf4`
permits checked native-index Givens to reach it without mutable cycle breaking.
Both stages pass the compile/Lint matrix with 39 cases: twenty-two positive and seventeen
negative. These include outer native-index Givens, family-cycle controls and
complete visible type arguments. Each source run passes 532 observations
with the previous 503 outputs preserved. The API fixture passes 190 package
rejections. Both stages and all five registered regressions pass. The full GHC suite
was not run.

The M31 six-case baseline accepted pointwise transport under `a ~ b` but
rejected returning the whole dependent function. This was an evidence
restriction in normalization. M32 needs no function eta equality or new Core
form. It preserves the fixed closed domain and `Many` binder.

Agda v2.8.0 [Pi comparison](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L746-L754)
uses [domain comparison and context extension](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L827-L849)
before comparing codomains. The `Conversion.hs` blob is
`69423e2c11b6f35bc5eee2d502cf2830efbc005a`.
[`defaultAddCtx`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Monad/Context.hs#L198-L202)
extends the context with `raiseS 1`.
[`updateContext`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Monad/Context.hs#L258-L265)
updates checkpoint substitutions. The `Context.hs` blob is
`98e50ce60319dcb47a542ac7c04b4b3e491a64e9`.
These rules preserve outer assumptions under a new binder. They motivate the
scope discipline. They do not prove GHC's flavour, role or coercion invariants.

Agda's [propositional equality rewrite](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/with-abstraction.lagda.rst#L594-L606)
uses with-abstraction and a `refl` match. The manual blob is
`ff0888af84f3101351c501534de087c180dc7afd`.
[`Builtin.Equality`](https://github.com/agda/agda/blob/v2.8.0/src/data/lib/prim/Agda/Builtin/Equality.agda#L5-L9)
has blob `22d08dc45b843ed4a3bb9410d38a7508383699c2`.
This is explicit proof elimination. GHC instead reuses an existing FC Given
through its inert rewrite machinery. Agda's extensional term comparison does
not establish Haskell eta or `seq` laws.

The GHC baseline is commit `8e58f91009a2e789571513c3207922d43333b5b6`.
Its `rewrite_tyvar2` in `GHC.Tc.Solver.Rewrite` blocked all inert rewrites
beneath a retained binder. M32 tracks active runtime Ids in the VarSet field
`re_native_binders`. `withNativeBinder` extends that set. Returning from the
body restores the previous environment.

A candidate must remain eligible under `eqCanRewriteFR`. Beneath a retained
binder, require Given evidence. Collect deep free variables from its predicate,
canonical RHS and coercion. Require them to be disjoint from every active
retained Id. Include variable kinds and CoVar predicates. Reject coercion
holes in the predicate, RHS and collected variable types. Preserve role
conversion and `recordRewriter`. An outer runtime index is allowed; an active
inner index cannot enter through the evidence or its metadata.

The first implementation admitted ordinary `a ~ b` but the outer-index case
still rejected. Its trace showed `a ~ Vec Int k` becoming
`CIrredCan{cteSkolemEscape}` in `checkTypeEqNoUnification`, before rewriting.
The correction sets `tefTyVar_allowRuntimeIndices = True` only for an existing
Given with a type-variable LHS and native syntax in its RHS. The checker
accepts an opaque `TermVarTy` only when `isNativeIndexId` validates its domain
and multiplicity. It preserves the ordinary occurs check and rejects
`DepFunTy`. It creates, promotes and assigns no runtime Id. Every Wanted,
unification, defaulting and concreteness check keeps this option disabled.
See Note `[Rigid retained indices]` in `GHC.Tc.Utils.Unify`.

On this Given path, `tef_fam_app = TEFA_Recurse` disables family cycle breaking.
`famAppBreaker BreakGiven` otherwise creates a cycle-breaker variable and
saves its family application. `restoreTyVarCycles` in `GHC.Tc.Solver.Monad`
later restores that RHS with `TcM.writeMetaTyVar`. A native RHS must not enter
this mutable route. Ordinary Givens keep their existing cycle breaking.
This rule comes from GHC's canonicalization and mutable-variable invariants,
not Agda's conversion algorithm.

The pinned GHC proof and downstream sources are:

- `compiler/GHC/Core/TyCo/FVs.hs`, lines 219–241: deep type and coercion free
  variables, including the variable metadata used by the filter.
- `compiler/GHC/Core/TyCo/Subst.hs`, `substNativeIndexBndr` at line 835, and
  `compiler/GHC/Core/Coercion.hs`, lines 1775–1780: binder substitution and
  lifting under the scoped runtime Id.
- `compiler/GHC/Core/TyCo/Rep.hs`, lines 272–294, and
  `compiler/GHC/Core/Reduction.hs`, lines 361–366: `DepFunCo` binds the complete
  body proof; `mkDepFunRedn` canonicalizes each endpoint independently.
- `compiler/GHC/Tc/Utils/Unify.hs`, lines 4198–4204, and
  `compiler/GHC/Tc/Solver/Monad.hs`, lines 2332–2335: Given cycle-breaker
  creation and later mutable restoration.

All paths and file blobs are recorded in
`native-pi-given-prior-art-v1/provenance.json`. These are baseline snapshots.
The M32 implementation commits describe the subsequent changes.
Keep plugins, inert-family rewrites, family-cache access, Wanted rewrites and
coercion zapping disabled beneath retained binders. Keep `rewrite_co` as
zonking only. Preserve ordinary proof scope and Haskell runtime demand.

The first full source run also exposed a call-site restriction. A complete
local signature can capture runtime `k`, but the existing VTA renamer rejects
`@(Vec Int k)`. Omitting the type argument requires a forbidden native meta
assignment. Commit `d69cedb848` shares the existing lexical alias and checked-Id mechanisms
with complete visible type arguments. Commit `5d6016da94` keeps the shared
failure helper result-polymorphic. Preserve the actual Name and Id.
Existing erased type variables take precedence. Captured partial types and
quotations reject. The typechecker retains `HM_VTA`, kind and validity checks,
and the ordinary ITYARG/IALL order. This is a GHC namespace adaptation of the
same context discipline cited above. It does not add Agda syntax or mutable
native inference. Note `[Runtime indices in visible type arguments]` in
`GHC.Rename.HsType` owns this rule. Type-argument diagnostics distinguish it
from the local-signature restriction.

The demand harness initially placed a dependent function inside `IO` through
`evaluate`. Its outer polymorphic consumer then required an unsupported native
polytype instantiation. The corrected check uses the existing `checkWhnf`
and returns `IO ()`. It still forces the function to WHNF before reporting an
expected error. It does not apply the function or weaken the demand assertion.


The review probes exposed a generated-wildcard bypass. The parsed type
`Vec $(pure WildCardT) k` has no anonymous wildcard until the splice runs.
The ordinary splice control compiles with the target external interpreter.
The native capture case also compiled before the correction. Commit
`eaf1df5478` rejects `HsWildCardTy` under `HM_VTA` with a nonempty
`mode_indices`, before wildcard or kind meta allocation. The same ordinary
splice still compiles. The source span identifies the splice that generated
the wildcard. The probe also checks that an inner runtime `k` cannot alias
an outer `k` with the same spelling.

This follows GHC's existing typechecking stages. Note `[Partial Type Splices]`
in `GHC.Rename.Splice` explains why expansion runs once during renaming.
Note `[Wildcards in visible type application]` in `GHC.Tc.Gen.HsType`
explains that `hswc_ext` contains named wildcards only. Anonymous wildcards
remain in the body. A check of that extension alone cannot enforce this rule.
The complete `GHC.Rename.Splice` snapshot and blob are recorded in the same
provenance file. The initial probe without `-fexternal-interpreter` stopped
at interpreter configuration. It is retained as a failed setup, not a located
type-system rejection.


## Retained function types as equality operands

M33 starts from three located GHC-43582 source gates and the no-proof
GHC-91028 rejection. Both M32 stages agree. These are observed source
boundaries, not evidence of a solver failure after Pi formation.

Agda v2.8.0 computes a dependent function's sort with
[piSort](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/sort-system.lagda.rst#L467-L502).
If the domain and each codomain inhabit `Set`, the Pi type inhabits `Set`.
[Builtin equality](https://github.com/agda/agda/blob/v2.8.0/src/data/lib/prim/Agda/Builtin/Equality.agda#L5-L9)
accepts operands at any universe. Equality between `A : Set` and a Pi type
uses the carrier `Set : Set₁`; the equality inhabits `Set₁`. This does not
require `Set : Set`. The
[Pi checker](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L106-L131)
checks the telescope, forms the scoped body and checks the resulting sort.

The stdlib v2.3
[subst](https://github.com/agda/agda-stdlib/blob/v2.3/src/Relation/Binary/PropositionalEquality/Core.agda#L82-L86)
transports a value by matching `refl`. This supports the intended proof
elimination. The separate
[extensionality assumption](https://github.com/agda/agda-stdlib/blob/v2.3/src/Axiom/Extensionality/Propositional.agda#L20-L53)
is not needed to transport along an existing equality between types.
These sources guide Pi formation and proof scope. They do not establish GHC's
inference, universe or runtime invariants.

GHC already gives `DepFunTy` kind `Type`. Its `isTauTy` classifies both
`DepFunTy` and erased `ForAllTy` as non-tau. Preserve that classification and
all native-meta restrictions. Reuse the mode-preserving retained checker
for checked source type arguments instead of adding special cases for `~`
or the library datatype `:~:`. Their arguments follow the same checked type
formation. Literal `foreach` kind syntax remains gated. A complete closed Pi
alias can be a kind through explicit specified kind instantiation. The
ordinary inferred kind-meta path still rejects it. This follows GHC's existing
Note `[Recursion through the kinds]` in `GHC.Tc.Gen.HsType`: kinds are types.
It does not add universe levels or general native kind inference. Preserve
ordinary erased-forall restrictions.

Nine Agda and nine GHC baseline files have verified Git blobs in
`native-pi-operand-prior-art-v1/provenance.json`. The GHC baseline is
`de3186ee88870533570fcc667e77197511d3ded3`. Source formation is implemented in `6963da76a7` and `826f47d880`. The first
compiler change alone still hit the renaming gate. Both stages must preserve
that failed observation.

After formation, direct Given and `Refl` examples compile. The direct Given
can match while it remains `CIrredCan{cteImpredicative}`. A separate example
under another retained binder fails. Commit `050d928510` recursively checks
the native Pi RHS on the immutable Given path and uses `mkDepFunRedn` to
scope the full body reduction. The trace then records `PuOK` and an inert
`EqCt`. A recursive `a ~ (foreach n -> Vec a n)` still records
`CIrredCan{cteInsolubleOccurs}`. It cannot justify transport. Native family
cycle breaking, Wanted decomposition and mutable native fills stay blocked.

Both compiler stages pass 562 observations, with the previous
532 as an exact prefix. The API fixture passes 210 package and 291 total
source rejections, plus 22 quiet positive cases. Bare `Refl` and an `error`
result at a native equality type hit ordinary meta guards. Their callers now
write complete type applications. The transitive-Given control uses
`AllowAmbiguousTypes`; its ordinary ambiguity check otherwise attempts a
native meta fill. The unrelated-proof and representational controls also
use that extension, then fail at the intended body expression.

Both stages pass 21 new located rejections, 21 new compile controls and the
156 retained located rejections. The API fixture and all five registered
regressions pass. These source results and bounded Haskell demand observations
do not prove preservation, totality or correspondence with Agda.

The next six-case baseline reaches an earlier source-validity restriction.
Four qualified-Pi equality examples reject with GHC-91510 at the signature's
`forall`. The no-proof body rejects with GHC-91028; the ordinary qualified-arrow
control compiles. The traces do not establish a Given-rewrite failure.
Agda's hidden/instance distinction and the D/DC/DR coercion quantifiers guide
separate questions. They do not identify ordinary Haskell class dictionaries
with erased equality evidence. See the
[roles paper reading](papers/README.md#roles-icfp-2019) and
[next source task](PLAN.md#m34-qualified-retained-function-types).

## Qualified retained function types

The M34 seven-case probe enables `ImpredicativeTypes` on the frozen M33
compiler. Both stages admit the signatures. Two ordinary controls compile.
Five native bodies reject with GHC-83605 at the Quick Look guard. The
`native-qualified-pi-*` evidence preserves source, commands, diagnostics and
both beneath-binder traces. The initial scope peaks at 179 MiB under 4 GiB.

Agda v2.8.0
[Pi checking](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L106-L130)
checks the body inside its telescope. Its
[domain checker](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L335-L365)
preserves instance metadata while checking later domains. Its
[argument insertion](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Implicit.hs#L81-L101)
distinguishes unification metas from instance metas and substitutes each
argument into the remaining type. These rules guide binder order and scope.
They do not equate Haskell class dictionaries with FC equality evidence.

GHC's existing application pipeline needs a separate checked-variable path.
A bare actual variable with no leading invisible quantifier or context needs
no type instantiation. Select `NoQL` with no arguments, known native syntax
and no existing Quick Look instantiation variables in the zonked types.
Read an available expected type without allocating an inference cell. A
case scrutinee can return its existing type through `InferResult`; its level,
promotion and runtime-representation checks remain active. Use the
native-aware deep free-variable traversal. The
Quick Look traversal itself rejects retained binders and indices. Preserve
final subsumption and native evidence capture. This does not add general
native Quick Look inference or establish a soundness theorem.

Eight Agda source records are pinned in
`native-qualified-pi-prior-art-v1/provenance.json`. Six full-file Git blobs
were checked locally; two records use tagged source ranges. Keep this
provenance separate from the frozen M33 evidence.

Serrano, Hage, Peyton Jones and Vytiniotis,
[A Quick Look at Impredicativity](https://simon.peytonjones.org/assets/pdfs/impredicativity.pdf),
ICFP 2020, supplies the application algorithm. The downloaded author copy
contains January 2026 corrections and 33 pages; the published citation has
29 pages. `h-var` retrieves a known type (Figure 3, page 7). `iresult` can
finish with empty arguments and substitution (Figure 4, page 8). Leading
foralls still instantiate. Final result equality remains required (Figure 7,
page 19). Section 5.8 warns that eta expansion changes lazy behavior (page 15).
See [the exact reading scope](papers/README.md#quick-look-icfp-2020).

The first end-to-end source probe also identifies a synonym boundary.
`checkTyConApp` normally rejects a non-tau synonym unless `rewriterView`
expands it. On the native Given path, expand synonyms with `coreView`, then
perform the same recursive check as for the written RHS. Preserve occurrence
checks inside predicates and results. All other paths keep the existing
synonym policy. See Notes [Rigid retained indices] and
[Forgetful synonyms in checkTyConApp] in `GHC.Tc.Utils.Unify`.

The current M34 checkpoint passes 26 source cases in both compiler stages:
13 compile and 13 reject or record unsupported inference forms. Synonym
controls distinguish an unused parameter from a real occurrence in a
predicate. Both stages pass the 562 previous source observations and 22
new runtime observations. The six dictionary observations use ordinary
`GHC.Exts.withDict` with a single-method class. The two function-identity
observations pass at `-O0` after forcing the compared values. These are
program observations, not a general sharing theorem.

At the frozen M34 checkpoint, the source helper with an incoming `:~:` proof
compiled. Its separate `Refl` and bottom-proof callers rejected with GHC-91028.
Proof demand was not established there. M35 extends exact expected matching
through ordinary algebraic containers; general native Quick Look and
native mutable-meta assignment remain separate work. See the
[source example](examples/qualified-pi/README.md) and
[M35 plan](PLAN.md#m35-exact-matching-through-algebraic-containers).

## Exact matching through algebraic containers

The M35 baseline preserves two witness-caller failures in each compiler stage.
The source signature is a complete `QPi -> QPi` arrow. Both `withQ Refl`
and `withQ (error ...)` report GHC-91028 at the helper and witness argument.
A known qualified copy compiles. These are source observations from the
unchanged M34 compiler, before the container eligibility change.

Agda v2.8.0 checks an available
[expected target](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L690-L767)
before the current argument only after rigidity, arity and dependency checks.
It strengthens an independent target and compares the types. Its
[argument checker](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
uses the current domain's hiding and modality, then substitutes into the
remaining telescope. Its
[conversion checker](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L827-L849)
compares Pi bodies in an extended context. These rules motivate expected-type
information with explicit scope checks. They do not certify GHC's algorithm.

The GHC adaptation extends only `isRetainedValueType` eligibility. An exactly
saturated algebraic container of kind `Type` can hold a supported retained
function. The check keeps the incoming `under_pi` flag for every argument.
A Pi in one argument does not put a sibling inside that Pi. A real enclosing
Pi remains in scope for every argument. Keep the ordinary tau fast path and
the existing erased-forall, class, family and nonlifted restrictions.
Saturation and exact `Type` kind are GHC restrictions, not Agda guarantees.

The immutable matcher already compares equal constructor heads and their
arguments. It retains complete substitutions, kind agreement, runtime scope,
Pi-local escape checks and final subsumption. No change to `isTauTy`, mutable
native metas, roles, Core, interfaces or eta rules follows from eligibility.
See Note [Known retained function instantiation] in `GHC.Tc.Gen.App` and
Note [Matching retained function types] in `GHC.Core.Unify`.

Four full-file Agda blobs are verified in
`native-pi-container-prior-art-v1/provenance.json`. The GHC excerpts record
the working-tree bytes read during implementation. They do not assign dirty
bytes to the earlier baseline commit.

Agda's [left-hand-side checker](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1019-L1051)
selects the current telescope domain before checking its pattern. Constructor
splitting checks that domain's data or record type. The GHC adaptation checks
whether the first expected argument is an actual `DepFunTy` before selecting
retained constructor coverage. A single ordinary constructor prefix uses the
existing pattern checker. Multiple ordinary constructor rows remain unsupported.
The verified Agda source blob is `3779ffa6e418bca99e30c071e189106f82ecbe1b`.
The source cache and reading ranges are in
`native-pi-container-prior-art-patterns-v1/provenance-v1.json`.

Ordinary GHC newtype patterns use a representational cast and a lazy binding.
Data patterns use an ordinary constructor match. See `matchNewtypeCon` in
`GHC.HsToCore.Match.Constructor` and `bindMatchId` in `GHC.HsToCore.Utils`.
Note [Newtype workers] in `GHC.Types.Id.Make` states that the constructor has
no runtime binding. Its compulsory unfolding becomes a cast. Its Id has
`DataConWrapId` details even when accessed through `dataConWorkId`.
The restricted native simplifier must expand this existing template. Preserve
its activation, substitution, beta and multiplicity checks. Keep general
compulsory unfolding disabled. See Note [Restricted dependent simplification]
in `GHC.Core.SimpleOpt`. The before and after source hashes, owning Notes and
measured panic are in `native-pi-container-prior-art-patterns-v2`.

Both compiler stages now pass 48 runtime observations and 26 source cases:
17 compile and nine reject at 11 located diagnostic envelopes. The runtime
checks cover `Refl`, ignored and demanded bottom proofs, tuple-under-Pi results,
function identity, and named and lambda patterns over data and newtypes.
At M35, the fully applied `withQ (error ...) f` case remains an inference boundary.
Its result does not determine the erased parameter. The partial helper has a
complete expected arrow and compiles. This does not admit native mutable metas.
A leading GADT `Refl` pattern now transports a retained function through its
nominal evidence. Its vector, identity, partial-application and proof-demand
observations pass in both stages. Two old expected rejections are retained as
positive controls: the leading GADT pattern and `undefined` at a complete
`Maybe (foreach ...)` result. The full existing API fixture and all five
focused registered tests pass. The full GHC suite was not run.

## Native worker argument inference

The M36 baseline agrees in both compiler stages. Two partial witness helpers
compile. Full `Refl`, full bottom proof, full Given transport and two direct
runtime-index applications reject with GHC-91028. Their final result does not
determine the erased `value` parameter. The known argument `f :: QPi` does.

Agda v2.8.0 normally checks explicit arguments in
[telescope order](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808).
It checks the current domain's hiding and modality, then substitutes into the
remaining type. A blocked check can retain its
[checked prefix and remaining spine](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L619-L641).
The [postponed check](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/MetaVars.hs#L607-L653)
stores its context and a real unblock condition. The application source
[warns against postponing unblocked arguments](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L779-L791).
That comment records an abandoned experiment. It is not an implemented rule
for scanning arbitrary later arguments first.

The GHC adaptation uses its existing argument cache. Infer an independently
known later argument once. Match its native-free formal domain immutably.
Require complete parameter choices, kind agreement, exact domains and current
runtime scope. Then check the earlier native-template proof normally. Match
kinds before types with the same restricted substitution. Keep the original
forall and evidence order through the existing instantiation plan. Preserve
failed inference caches. Skip callback inference and fresh parameter creation
for a native worker. General native Quick Look and mutable native-meta
assignment remain blocked. This changes checking order, not runtime order.
See Note [Native worker argument inference] in `GHC.Tc.Gen.App`.

Four full Agda source files and the frozen M35 GHC checker are verified in
`native-worker-arguments-prior-art-v1/provenance.json`. The reading records
source ranges, SHA256 and calculated Git blobs. Agda's context-abstracted
mutable metas and eta policies do not transfer to this implementation.
Both compiler stages pass the 52-client source matrix: 27 compile and 25
reject or remain unsupported at 35 located diagnostic envelopes. A separate
closed-proof control reports GHC-83605. The failure trace records one
elaboration of the cached argument. Consecutive forall/context groups before
the first visible arrow and leading VTA pass. Foralls after visible arrows
remain outside the collected telescope.

Both stages pass 57 runtime observations, including direct full applications
at four IORef-fed lengths. The ignored bottom proof and function stay lazy.
Demanding the result checks the proof before the function. The ignored-proof
worker preserves the tested function identity. The prior 562 observations,
the existing API fixture and all five focused registered tests pass. The full
GHC suite was not run.

Four `ImpredicativeTypes` application controls report GHC-83605. They include
explicit `@QPi`, an incoming proof and a monomorphic native head. Their types
form successfully, but applied Quick Look remains blocked. A direct `ProofQ`
parameter or qualified `@QPi` without `ImpredicativeTypes` instead reports
GHC-91510 during source formation. An ordinary `ProofBox` pattern can supply
the named incoming proof on the supported `NoQL` route. Keep these boundaries
separate. See the [next plan](PLAN.md#m37-applications-with-impredicativetypes).

## Applications with fixed native type choices

The M37 baseline has 61 clients in each compiler stage: 16 compile and 45
report located source errors. Its two support providers compile. The four
M36 applications and a closed `Refl` control still report GHC-83605.

The corrected author copy of
[A Quick Look at Impredicativity](https://simon.peytonjones.org/assets/pdfs/impredicativity.pdf)
separates choosing type arguments from checking value arguments. Figures 3–4
on printed pages 89:7–89:8 use `h-var` to retrieve a head type and `ityarg`
to substitute a supplied polytype. `iall` introduces a choice when an erased
argument is still unknown. Section 4.4 on page 89:10 explains explicit VTA.
Algorithmic Figures 7–8 on pages 89:19–89:20 retain argument checking and final
result comparison. Section 8 on pages 89:21–89:22 retains constraint entailment.
Section 5.8 on page 89:15 records the lazy semantic cost of eta expansion.
The paper's theorem does not cover runtime Id indices or the native FC forms.

Agda v2.8.0 retains the
[checked argument prefix](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L619-L641)
and [substitutes each checked argument](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
into the remaining telescope. Its
[early expected-target comparison](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L690-L767)
requires rigidity, sufficient arity and independence from consumed binders.
Its [final comparison](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L449-L495)
still applies when the earlier check has not established the result.
Agda's contextual mutable metas do not justify native GHC meta assignment.

The GHC adaptation reuses its IALL/ITYARG loop to check a leading
type-argument prefix once. It retains the prefix and original QL variables
when mode selection fails. Fixed explicit choices or an existing complete
immutable expected-result match can permit `NoQL`. Each inner application
must justify that mode independently. Preserve the native evidence scope,
constraint obligations, concrete representations, substituted kinds, runtime
scope, usage and final subsumption. Add no eta expansion or new equality rule.

The exact PDF, extraction, selected rendered figures, full Agda blobs and
frozen M36 checker are recorded in
`native-ground-applications-prior-art-v1/provenance.json`. The reading covers
the named paper sections and Agda ranges. It does not include the appendices,
the full Agda solver or a proof replay.

The selected compiler is `89c98cf7b4a23b048f8ffd77170537f27a434224`.
Its `Ground retained applications` Note owns this adaptation. Both compiler
stages agree on 64 source clients and pass 76 raw and public runtime
observations. The evidence prefix is `native-ground-applications-*`.

The prefix-metadata trace supplies a specific safeguard check. IALL creates
two unresolved inferred variables for an unused phantom kind. The residual
type is `Int`, but the saved type applications retain those same variables.
Mode selection keeps `DoQL`, and later monomorphisation visits those
identities. In the filled-kind control, ordinary VTA kind checking assigns
`Type` to the inferred kind before mode selection permits `NoQL`.
These observations support the bounded implementation. They do not extend
the paper's theorem to native FC or permit native mutable-meta assignment.

## Native choices from declared arguments

For `withQ Refl f`, the declared monomorphic type `f :: QPi` can select the
outer erased argument before the checker elaborates `Refl`. The corrected
[Quick Look author copy](https://simon.peytonjones.org/assets/pdfs/impredicativity.pdf)
uses later declared argument information before checking an earlier empty list
on PDF page 5. The `h-var` rules read the context on pages 7 and 19. Argument
advice precedes full checking on pages 11–15 and 19–22. Section 5.7's
precomputed guardedness is implemented; its guarded-first iteration is an
alternative. These rules do not cover native FC.

Agda v2.8.0's
[variable head rule](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L299-L325)
uses the current context and still checks modalities. Its
[domain checking](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
substitutes checked arguments into the telescope. Its
[blocked checking](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L924-L987)
retains the checked prefix. The rejected postponement comment at lines
779–791 is not an implemented later-first algorithm.

The GHC adaptation reads declared types without `tcInferId`, which emits
occurrence usage. A global lookup can load an interface. Complete immutable
choices enter the existing IALL plan before value checking. Original
forall/context groups retain their order. Normal argument checking records
scaled usage once and checks proof entailment. This is type advice, not
proof evidence or permission to assign native types to mutable variables.
`Note [Declared native argument choices]` in `GHC.Tc.Gen.App` owns the rule.

`native-known-arguments-prior-art-v1/provenance.json` records the corrected
PDF version, pinned Agda blobs, full GHC source snapshots and read ranges.
The reading did not replay a paper proof or inspect the full Agda solver.
The implementation and source validation are recorded separately.

## Whole-program roadmap review

The 2026-11-15 review read all roadmap card descriptions and dependency lists.
It extracted the full embedded graph. The source has 50 feature nodes and 67
edges. All 39 card-listed relations match graph edges. The other 28 edges point
to completed children. There are 37 remaining prerequisite/subtask relations
and two explicit possible improvements. Do not treat all edges as hard blockers.
The [program checklist](ROADMAP.md) separates the upstream snapshot from the
capabilities and remaining work of this fork.

The ignored cache `native-known-arguments-roadmap-research-v1` records source
URLs, fetch metadata, hashes, proposal revisions, reading scope, and the graph
extraction checks. The fetched HTML SHA256 is
`1d64aa659154eb9b1190c0cabe274e095d2d23ecc223bdd51aaa02649b54d8d8`.
Proposal #378 has Git blob `39a812b8154ee164e7de3ad9e27e21e012ebc000`.
The review read its design sketch, including erasure, lexical identity,
checked dependent matching, the static subset, and the typed intermediate
language. Its accepted design direction does not approve every later grammar
or supply a combined Core implementation proof.

[Type functions, PR #509](https://github.com/ghc-proposals/ghc-proposals/pull/509)
was open at head `f0b6af30a6e15545606195eb72c2981ea91aa326`.
Its full 309-line proposal was read. It specifies closed, fully instantiated
calls through loadable functions. A rigid argument can leave a call stuck.
This does not implement general open evaluation. The prototype performance
claims were not reproduced.

[First-class existentials, PR #473](https://github.com/ghc-proposals/ghc-proposals/pull/473)
was open at head `72de21d16aa0b6e1a85cd4ecc66c7dc729ee0d87`.
The retained-witness discussion, elimination and Core passages were read.
Its existential witness is erased. Retained dependent pairs are future work
in that proposal. The current retained-field packages do not discharge the
first-class `exists` or `some` roadmap tasks.

The [additional paper reading record](papers/README.md#roadmap-review-2026-11-15)
covers matchability, existential laziness, constrained families, and the
39-page extended graded paper. That extended version is distinct from the
32-page paper in the existing seven-work manifest. Appendix B explains why
its implicit calculus still needs an explicit encoding for an efficient Core
checker. Use the papers to state obligations; do not transfer their theorems
to this implementation without a correspondence argument.

The 2026-11-15 review encountered bot challenges on the open-evaluator
issue and homogeneous-equality wiki. A subsequent API read retrieved
the open-evaluator issue. See the [evaluator references](#executing-closed-calls-and-reducing-open-calls).

## Core contract audit and bounded beta exposure

The C1 audit is cached in `native-core-contract-research-v1`, with three exact
paper versions, 12 source snapshots, reading scope, and hashes. It maps the
current closed-domain forms, scoped coercions, virtual case evidence, and erasure
to the relevant obligations. It does not replay a proof for this implementation.

The 2017 D/DC author PDF gives An-Beta in Figure 8 on PDF page 16.
An-AppAbs and An-Push appear in Figure 10 on PDF page 19.
The endpoints must type check and the reduction must be an actual primitive
step. An-Push handles casted application with explicit evidence.
The 2019 role paper requires nominal equality for dependent arguments and does
not supply its annotated implementable counterpart. The extended 2021 GraD
paper separates type use from runtime usage and still requires an explicit
checker. These results motivate a checked beta bridge, not arbitrary producer
unfolding, equality between opaque names, or an eta rule.

Agda v2.8.0 supplies a closer implementation reference.
[`applyTermE`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Substitute.hs#L65-L98)
contracts a lambda application through `lazyAbsApp`. Its internal terms are
[beta normal](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/Internal.hs#L215-L229).
[`lookupS`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Substitute/Class.hs#L292-L307)
returns a stored substitution range directly. Lifting handles the range under
new binders. Our actual-Id representation instead preserves the range's outer
context by returning its normalized atom/index pair without another lookup.
[`piApplyM`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Telescope.hs#L705-L727)
obtains a Pi/path view before substituting in the codomain. These helpers assume
typing preconditions. They do not replace our original-expression Lint checks.

The five full source blobs, Git hashes, SHA256 hashes, and selected reading
ranges are cached in `native-index-beta-agda-research-v1`. Agda uses de Bruijn
indices and a different internal representation. Its substitution code does
not establish Haskell bottom, `seq`, sharing, or exception behavior. Runtime
checks must establish the tested behavior in our implementation.

The implemented first bridge accepts an immediate lambda over an eligible Many
index domain. Its argument must normalize to an atomic index. Its body
normalizes inside the existing variable/constructor grammar. Runtime argument
normalization, type recovery, index substitution, and CorePrep naming agree on
the same checked pair. Keep source elaboration separate until it preserves
typing, scope, usage, and failure behavior. See
[the Core contract](CORE.md#checked-beta-exposure) and the M39 spec in PLAN.md.

## Bounded source beta applications

M40 uses Agda v2.8.0's
[expected-domain argument checking](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
and [lambda context extension](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L685-L710)
as implementation precedents. The retained domain supplies the provisional
lambda binder's type. The source checker keeps the original argument and checks
it once. The Core bridge still validates its bounded grammar and domains.
These sources do not prove the GHC rule or Haskell runtime behavior.

At compiler commit `7129205`,
[`tcPatBndr`](../../compiler/GHC/Tc/Gen/Pat.hs) constructs the actual Id from the
renamed pattern Name. [`selectMatchVar`](../../compiler/GHC/HsToCore/Utils.hs)
reuses that VarPat Id. Ordinary lambda inference allocates a fresh multiplicity
in [`new_infer_arg_ty`](../../compiler/GHC/Tc/Utils/Unify.hs).
[`submult`](../../compiler/GHC/Core/Multiplicity.hs) accepts `One <= p` without
fixing `p` to `One`. Final
[`commitFlexi`](../../compiler/GHC/Tc/Zonk/Type.hs) defaults an unresolved
multiplicity to `Many`. This supports the bounded provisional-Many design.
Both compiler stages run an API probe over six actual zonked lambda binders with
`LinearTypes`. Their Nat domains, Many multiplicities, and renamed Names/Unique
values match before desugaring or SimpleOpt. The source and probe are in
`native-source-beta-metadata-v1`. The bootstrap early smoke and final
self-hosted phase record execution with matching compiler inputs.
Do not generalize these checks to signatures or other forms that can impose a
linear arrow.

Ambient aliases have a separate timing constraint. An unsigned captured let
can use `NoGen` in [`tcPolyBinds`](../../compiler/GHC/Tc/Gen/Bind.hs).
Its fresh multiplicity remains unresolved while the body is checked.
`zonkId` reads assignments; it does not default that variable. M39 and M40 both
reject the bare alias as a retained index. Both accept the signed bare alias;
M40 also accepts beta application to that signed alias. The five measured
controls and source excerpts are recorded in `native-source-beta-alias-controls-v1`
and `native-source-beta-alias-boundary-v1`. M40 does not change ambient defaulting.

[`decideBangHood`](../../compiler/GHC/HsToCore/Utils.hs) adds an outer bang to a
bare pattern under `Strict`. Reducing a constant lambda without that demand
could hide a bottom argument. M40 rejects its new immediate-lambda form under
`Strict`, at the original lambda span. Opaque producers remain unsupported.
The source snapshots, exact ranges, hashes and two verified Agda blobs are
cached in `native-source-beta-source-provenance-v1`. The M39 archive and its
Core-only evidence remain unchanged. No eta rule or general evaluator is added.

<a id="opaque-runtime-function-indices-unimplemented-c1-comparison"></a>

## Opaque runtime function indices: C1 comparison and first kernel

This comparison preceded M42. M42 selects a dedicated scoped call node for a
bounded Core kernel. The token/family alternative remains research. Keep scalar
index domains, permitted runtime function heads, and future domains of retained
function binders as separate policies. M43 adds the bounded source path below;
new wire forms remain gated. See the [Core contract](CORE.md#opaque-native-call-indices).

[Higher-Order Type-Level Programming in Haskell](https://simon.peytonjones.org/assets/pdfs/higher-order-type.pdf)
uses matchability to control equality decomposition. The read version is the
26-page ICFP 2019 author PDF. Pages 3–4 distinguish injectivity, generativity
and Given decomposition. Pages 6–7 explain why an ordinary term arrow supplies
no injectivity guarantee. Co_Left and Co_Right require matchability in Figure 5,
page 14; section 4.3 gives the counterexample on page 13. A constant runtime
function has equal results at `Z` and `S Z`. Projecting that result equality
must not prove `Z ~ S Z`. Ordinary `AppTy` permits such projection today under
its existing formation restrictions. Reusing it for opaque calls would remove
those restrictions without replacing their justification.

The pinned [proposal #378](https://api.github.com/repos/ghc-proposals/ghc-proposals/git/blobs/39a812b8154ee164e7de3ad9e27e21e012ebc000)
discusses matchability and keeps ordinary function definitions opaque in types.
[D/DC's 30-page author PDF](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf)
supplies forward An-AppCong in Figure 8, page 16.
Its An-PiSnd on page 17 concerns function types, not injectivity of runtime
functions. [System DR's 29-page conference PDF](https://richarde.dev/papers/2019/dep-roles/dep-roles.pdf)
requires nominal arguments for
unknown-function congruence on pages 8 and 11. Its E-Right rule requires a common
constructor CasePath on page 18. These rules do not justify decomposing an
arbitrary runtime call. The distinct ACM and extended versions remain separate.

Pinned [singletons Apply](https://github.com/goldfirere/singletons/blob/8c9221c164832bfd17878af94d50d8ade706932d/singletons/src/Data/Singletons.hs#L460-L526)
offers a second representation precedent. A saturated noninjective family
blocks result decomposition and keeps nominal argument roles. It still needs
a checked token for the actual runtime function Id. Its
[arbitrary-function `toSing`](https://github.com/goldfirere/singletons/blob/8c9221c164832bfd17878af94d50d8ade706932d/singletons/src/Data/Singletons.hs#L755-L776)
uses `unsafeCoerce`; it supplies no checked FC evidence for this extension.
A function-valued `TermVarTy` would expose an ordinary arrow kind and could
escape into matchable `AppTy`. A distinct checked token with a non-arrow kind
could avoid that problem. The family and token would still need scope,
substitution and runtime correspondence rules.

M42 uses a dedicated residual-call node for the first bounded kernel.
It forbids inverse decomposition and preserves actual function Id scope.
An Int-returning local test can avoid a function-domain retained binder.
Returning an index-dependent result cannot silently use an ordinary arrow.
Lazy naming of arbitrary calls also needs a separate rule; the existing strict
constructor-alias case cannot serve that purpose. Exact versions, pages, source
blobs, hashes and reading limits are in `native-next-core-function-indices-v1`.
This comparison establishes no evaluator or combined preservation theorem.

## Opaque native calls: representation, substitution and nominal congruence

Agda v2.8.0 supplies the operational precedent for a neutral head with eliminations:
[`Internal.hs`221–252](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/Internal.hs#L221-L252).
Its scoped substitution applies a variable's stored replacement and eliminations:
[`Substitute.hs`827–852](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Substitute.hs#L827-L852).
These references motivate head scope and substitution. They supply neither
inverse projection from an arbitrary call nor a proof of this GHC extension.
D/DC forward application congruence and System DR nominal arguments supply
separate equality precedents. Keep these roles distinct.

The local cache is `_build/verification/native-neutral-calls-research-v1/`.
Its `provenance.json` records access UTC, read ranges, URLs, hashes and reuse.
`Internal-v2.8.0.hs` has Git blob
`a6e0938ed369b1f0f5f0b8aa1ff3baeb9aad033b` and SHA256
`19bd9c6dd75caaa5b8aaaf2a90dda0caa0c03ceb758f4bcd437418c0f71250fc`.
`Substitute-v2.8.0.hs` has Git blob
`1ef41f6d2a0223c8328984903b60dabc2a05a783` and SHA256
`961b3e953d0eeff4370655d33b07cf81ce7837892eb263304173f0796dcd5800`.
These two files reuse earlier verified primary-source bytes. No new fetch or
Agda execution is claimed for that reuse. Full-file hashes do not imply
full-file reading. The older `piApply` and `Abs` pointers concern binder beta.
They are not the primary neutral-head reference for this increment.

M42 implements a bounded Core kernel. Its owning Note is
Note [Opaque native call indices] in `GHC.Core.TyCo.Rep`.
Compiler `0447f8a7a199675423cd7f5fafa94567afa40114` and fixture source
`428aa3795341e8628e258976adc6df434c17bda4` pass both-stage focused validation.
The v7 snapshot records all 77 compiler inputs and 92 source paths.
At that M42 checkpoint, source opaque-call formation and new wire encoding
remained gated. M43 implements the source subset described below. Its
[plan](PLAN.md#m43-strict-opaque-call-refinement-from-source-unimplemented)
retains the historical anchor. New wire encoding remains gated.


## Strict opaque-call refinement: prior art

Agda v2.8.0 supplies four selected source readings for M43:
[With.hs 101–173](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/With.hs#L101-L173)
partitions the context and constructs the abstracted helper type;
[the manual 710–741](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/with-abstraction.lagda.rst#L710-L741)
explains the dependent case combinator and its limits;
[manual 910–936](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/with-abstraction.lagda.rst#L910-L936)
requires checking the generalized type.
[LHS 1503–1532](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1503-L1532)
transports the remaining context, target and equations by constructor substitution.
[Application 774–808](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
checks an argument before substituting it into the codomain.

The fresh cache `native-source-neutral-calls-prior-art-v1/` records full bytes,
URLs, tag, Git blobs, SHA256, access/reuse UTC and the exact selected ranges.
With.hs was fetched and read on 2026-11-15; its independently computed blob
matches the earlier REFERENCES pointer. The original carrier Note used that
pointer before this local verification. The other three files reuse verified
cached bytes. Their identities are:

| File | Git blob | SHA256 |
| --- | --- | --- |
| With.hs | `fb84d352f3c6b2e786a0c095c74c5bee15e533cf` | `581b018beb066653327ab29b548e8fa77fdf84b6afec299d085b0bd795bb0d9a` |
| WithAbstraction | `ff0888af84f3101351c501534de087c180dc7afd` | `67921864b3aa8b1d139a543904440e7a2011cdbfdb47285e96f9735d05a9959e` |
| LHS.hs | `3779ffa6e418bca99e30c071e189106f82ecbe1b` | `263e2672f359cf2e7abe49e81f74ebf911aee20c6063b676f654100b9533c3f3` |
| Application.hs | `82ba94ac864d19b947d4c92b9af252b0fe81347a` | `3c843f26cde8bfc80f029eb42fba54e19d2ff06f94a9dbf6e4ef81e27dbc43c2` |

These readings support whole-call abstraction and checked scope transport.
They do not justify inverse call equality, Haskell eta conversion, or demand
changes. The strict DEFAULT alias, administrative lazy patterns under Strict,
and scalar/function-head identity guard for `seq` are explicit GHC adaptations.
The [Core contract](CORE.md#strict-opaque-call-refinement-from-source) records
source26, CLI9/14 and supplement15 as separate measured sets. All pass in both
stages. The supplement's corrected implicit observer uses immutable argument
matching; its first RuntimeRep source error is preserved as a probe mistake.
Four full-file hashes do not claim full-file reading. No Agda execution or
combined FC/usage/runtime preservation theorem is claimed.

## Checked constructor-call arguments: prior art

M44 reuses two selected Agda v2.8.0 readings.
[Application 774–808](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
checks an argument at the current Pi domain before substituting it into the
codomain. Its blob is `82ba94ac864d19b947d4c92b9af252b0fe81347a`.
[With 101–173](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/With.hs#L101-L173)
partitions the dependent context and constructs the helper type.
Its blob is `fb84d352f3c6b2e786a0c095c74c5bee15e533cf`.

The cache `native-source-call-constructors-prior-art-v1/` reuses their verified
full bytes. Its provenance records exact URLs, blobs, SHA256 and selected read
ranges. The six GHC source snapshots in that cache are historical M43 bytes at
`edbe394d03628de2900fff828b4137d72e5cc94c`. They are not the selected M44
implementation. Full-file hashes do not claim full-file reading.

These sources support checked scope and argument substitution. GHC separately
preserves constructor wrappers, actual Ids, Haskell sharing and field demand.
The structural source matcher and the use of `dataConWrapId` are GHC choices.
M44 adds no Agda eta conversion, function evaluation equation or inverse call
rule. Its selected compiler is `f76bba39b8bc61ea3e7ee48338086b1949fdce60`.
Both stages pass 17 source observations and 17 CLI observations. Seventeen
negative clients produce 37 diagnostic envelopes. The separate semantic review
covers the four added negative controls.
Freeze and independent audit must pass before publication. See
[the Core contract](CORE.md#checked-constructor-arguments-in-source-calls).

## Signed source call beta: prior art

M45 uses the selected Agda v2.8.0 readings in
[Term 468–486](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L468-L486)
and [542–587](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L542-L587),
blob `02991c9fa0085315cc854d2833a9080af14a7740`.
The header records the target. [Term 491–494](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L491-L494)
selects checking from its telescope. The later range preserves source NameIds
and checks the body in an extended context. Application 774–808, pinned above,
checks the current domain before substituting the checked argument.
The historical preparation cache is `native-source-call-constructors-next-beta-v1/`.

GHC's Note [Signed source call beta] in Tc.Gen.Expr requires a surviving
complete plain A -> A signature and actual checked assigned-Many metadata.
It preserves Name/Unique through NameEnv, leaves Flexi unfilled and keeps the
original runtime call. Agda modality handling does not justify GHC defaulting,
Haskell seq or Strict demand. Strict gets no new dependent-beta admission;
ordinary nondependent cases preserve their fallback behavior.

Selected compiler `44e91a7618fe55c3713c85641b4a2d88d4536f13` passes both stages:
public15, eight rejected clients and 15 located envelopes, with nine GHC-83865
and six GHC-83605. These envelope counts overlap at five clients. Fifteen quiet
API source observations, four supplementary demand/scope observations and the
limited 1/1 trace markers remain separate evidence. Legacy6 and final9 pass.
Freeze and independent audit must pass before publication. No new paper,
Agda execution or preservation theorem is claimed. The seven-work, nine-PDF
paper ledger is unchanged.

## Head-only function-valued retained binders: prior art

M46 implements a bounded local-signature subset. Focused compiler/runtime
validation passes; canonical audit/publication records remain separate.
Its type/coercion occurrences use only TermAppTy/TermAppCo head slots. It accepts
already-bound unrestricted local function operands of the exact checked
closed ordinary Many A -> B type. An operand does not need its own written
local signature. The complete local signature requirement governs retained
binder formation. Scalar TermVarTy and constructor fields keep their policies.
Inverse call decomposition, function equality, heterogeneous transport and
call-interface codecs remain outside this increment. See
[the concrete plan](PLAN.md#m46-head-only-function-valued-retained-binders).

Agda v2.8.0 [Conversion 746–761](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L746-L761)
and [827–850](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L827-L850),
blob `69423e2c11b6f35bc5eee2d502cf2830efbc005a`, compare domains before checking
codomains in one extended context. The Term/Application ranges above check
arguments uniformly at their expected domain. A domain can itself be a Pi;
these rules supply scope prior art, not GHC evidence or runtime guarantees.
Exact source bytes, Git blobs, selected excerpts and reading limits are in
`native-source-call-beta-next-function-pi-v1/`.

The D/DC author PDF's Figure 8, page 16 (printed 31:16), supplies An-PiCong
under a binder and forward An-AppCong. An-PiSnd on page 17 concerns Pi types,
not injectivity of runtime calls. The existing matchability comparison above
retains that distinction. This review rereads selected cached extracts only.
The canonical paper ledger stays seven works/nine PDFs. No Agda eta law,
Haskell function extensionality or combined preservation proof is adopted.

The implementation uses Note [Function-valued retained binders] in Core.Type
and Note [Function reduction multiplicity roles] in Core.Reduction. A new
Phantom control found an ordinary FunCo construction defect. The upstream
[FunTy normalizer at base 19ff36f, lines 1553–1558](https://gitlab.haskell.org/ghc/ghc/-/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/Core/FamInstEnv.hs#L1553-1558)
normalizes multiplicity at Nominal. The upstream
[Lint rule, lines 2432–2447](https://gitlab.haskell.org/ghc/ghc/-/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/Core/Lint.hs#L2432-2447)
requires a Phantom multiplicity child inside Phantom FunCo. A nonreflexive
ordinary multiplicity-family reduction reproduces the mismatch without a
retained binder. The fix keeps nominal normalization and weakens only the
stored proof. Exact bytes, excerpts and the observed failure are in
native-function-binders-phantom-review-v1/. This is source evidence and a
bounded correction, not a new paper result.

The unimplemented direct-case composition study reuses pinned
[With 101–173](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/With.hs#L101-L173),
[Application 774–808](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
and [LHS 1503–1532](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1503-L1532).
They check an argument before codomain substitution and transport the remaining
context by constructor substitution. D/DC Figure 8 supplies forward congruence;
Figure 10, author-PDF page 19, supplies An-Push. These are comparisons for
explicit scoped evidence after instantiation. They do not supply an automatic
whole-call rewrite in GHC or justify evidence escape. Selected excerpts, blobs,
hashes and reading limits are in native-function-binders-next-refinement-v1/.
The pre-instantiated caller is not evidence that direct source application
succeeds. The later probe2 gives two GHC-83865 payload mismatches in each stage
at lines 33/37 and has no runtime result. This measured baseline is separate
from the earlier static study. No new PDF, full-paper reading, Agda execution or combined
FC/usage/runtime theorem is claimed. The canonical ledger remains seven
works and nine PDFs.


The later scope review, native-function-binders-next-refinement-scope-v1/,
checks the existing free-call mask and proposed evidence order. Bound/reused
runtime Ids do not count as free call dependencies. A distinct motive Id needs
its own explicit i ~# f n parameter; the outer alias proof does not identify
i with r. Existing checked dictionary/cv transport can derive the row proof.
That study proposed a row-RHS descriptor and NoQL/whole-residual isTauTy gate.
M47's later Given-only ablation does not need them. The raw isTauTy observations
remain utility facts, not a new source-admission policy.
These are GHC scope/usage requirements. The pinned Agda and D/DC readings do
not establish them for this implementation. No new paper or proof replay is
added to the seven-work/nine-PDF ledger.

## Remembering an abstracted call: prior art

Agda v2.8.0 documents [with-abstraction equality and the inspect idiom](https://agda.readthedocs.io/en/v2.8.0/language/with-abstraction.html#with-abstraction-equality).
Generalizing an expression can lose its relation to the fresh helper argument.
The manual uses `with ... in eq` to retain equality evidence for later
occurrences. Its [manual source, lines 634–697](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/with-abstraction.lagda.rst#L634-L697),
blob `ff0888af84f3101351c501534de087c180dc7afd`, gives the doubled-value and
filter examples. The [implementation, With.hs 175–215](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/With.hs#L175-L215),
blob `fb84d352f3c6b2e786a0c095c74c5bee15e533cf`, counts two inspect arguments
and expands the value together with a reflexivity witness.

M47 uses that remembered-relation idea. Its motive retains `i ~# f n`, so a
newly instantiated worker can expose the original call again. Existing
constructor transport supplies a row Given. Ordinary exact or symmetric
irreducible-Given solving discharges the Wanted from argument/result checking,
including row `where` bindings. This needs neither inverse call reasoning nor
a new application-cast or solver rule.

The first ordinary evidence argument, its FC coercion-token ABI, strict alias,
Haskell demand, nontermination and evidence liveness are GHC obligations.
Agda's witness does not establish them or evaluate `f n` during GHC typechecking.
The discarded custom-App variant exposed dead-Given pruning because a manual
cast did not record a Wanted dependency. The selected ordinary evidence path
uses existing coercion-hole tracking. Note [Tracking needed EvIds] and
Note [Delete dead Given evidence bindings] in GHC.Tc.Solver.Solve explain that
requirement. The earlier diagnosis remains a historical variant record.

Exact source hashes, Git blobs and selected reading scope are in
`native-call-application-refinement-remember-equality-prior-art-v1/`.
The earlier D/DC forward-congruence and An-Push readings remain comparisons,
not an implementation theorem. Bootstrap, self-host, API and selected
both-stage CLI validation pass. Final9 and its registered run pass. Artifact
audit and publication remain separate; these are focused checks, not a full
GHC suite run. No Agda execution, proof replay,
new PDF or combined preservation result is added. The ledger stays seven
works and nine PDFs.

## Known-worker case activation: prior art

M48 reuses the pinned Agda v2.8.0
[Application 774–808](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
and [With 101–173](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/With.hs#L101-L173)
readings. They check an argument in context and generalize a helper's dependent
context. The [remembered-equality comparison](#remembering-an-abstracted-call-prior-art)
also remains relevant. These readings do not prescribe GHC's activation policy.

The M48 adaptation reads a checked lexical worker type before deciding whether
to use the existing case bridge. It substitutes known local actual Ids without
elaborating them or guessing a type. Complete rows, leading retained binders,
whole-tau residuals and silent fallback are bounded GHC discovery choices.
They are not new general application or solver rules. Ordinary checking and
the existing motive Given still supply evidence and usage checks.

Bootstrap fresh/freshwhere and selected API1 controls pass. The ten quiet
values retain exact 1/37 goldens. Self-hosting, selected both-stage source
controls and public17 pass. Legacy, constructor, final9 and registered checks
also pass. Artifact audit and publication remain separate; these focused checks
do not constitute a full GHC suite run.
This step adds no paper, PDF, full-file reading, Agda execution or preservation
claim. The canonical ledger remains seven works and nine PDFs. Earlier selected
source identities and reading limits remain in the existing Agda records.


## Constructed-worker case activation: prior art

M49 reuses the pinned Agda v2.8.0
[Application 774–808](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
and [With 101–173](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/With.hs#L101-L173)
readings. Application checks an argument and substitutes the codomain. With
constructs the helper's generalized context. Their file blobs remain
`82ba94ac864d19b947d4c92b9af252b0fe81347a` and
`fb84d352f3c6b2e786a0c095c74c5bee15e533cf` respectively. The existing
[remembered-equality comparison](#remembering-an-abstracted-call-prior-art)
explains why later worker instantiation can use the original call relation.

GHC's new query only reconstructs metadata from a known local worker and
resolved monomorphic constructor trees. Expected domains, the existing scalar
converter and exact call identity constrain this discovery. Normal checking
still handles values and usage; ordinary motive/row Given evidence handles
transport. Agda's contextual inference does not specify this bounded trigger,
Haskell demand, evidence liveness or FC lowering.

Both builds, selected API1, both-stage source/public21 and final9 checks pass.
Artifact checks are tracked in
[the current checkpoint](README.md#m49-constructed-worker-case-activation).
GHC-05617 points at each wrong payload, but relevant bindings can still show a
generated `call_motive` name. No new paper, PDF, full-file reading, Agda execution
or theorem claim is added. The ledger remains seven works and nine PDFs.


## Multi-constructor retained packages: prior art

Agda v2.8.0's [Data Types source59–76](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/data-types.lagda.rst#L59-L76)
gives each constructor a function type ending in its datatype. Its
[Vector example95–108](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/data-types.lagda.rst#L95-L108)
distinguishes shared parameters from locally bound constructor indices.
The file's Git blob is `7713c0755a94f784971a637059caaed4c616f04e`.
This supports constructor-local telescope scope, not GHC's runtime Id layout,
Many restriction or field erasure policy.

[Function Definitions source47–53](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/function-definitions.lagda.rst#L47-L53)
describes first-matching-clause selection. Git blob is
`3c7e86cd7e6b88d67f35e8882455ee28f7da6d4d`. M50 keeps order within each
constructor group and uses GHC's existing case failure continuations. Distinct
outer constructors do not overlap. This paragraph does not prove GHC's grouping
transformation or Haskell guard demand. The
[default Agda coverage/termination checks55–64](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/function-definitions.lagda.rst#L55-L64)
and its positivity discipline are not adopted as Haskell laws.

The new cache `native-multicon-package-prior-art-v1` records the actual fetched
paths, verified blobs and selected reads: Data Types1–191 and Function
Definitions1–64. The existing ICFP2017 author PDF was reread only at section9,
printed31:27/PDF27. It leaves primitive datatypes and pattern matching as future
work. Its D/DC proof does not establish GHC's primitive DataCon metadata,
interfaces, PMC or backend extension. See [the paper ledger](papers/README.md#d-and-dc-icfp-2017).
Seven works and nine cached PDFs remain unchanged. No proof replay or new
whole-paper reading is claimed.

The operational source evidence and remaining gates are in
[the M50 contract](CORE.md#multi-constructor-retained-package-patterns) and
[checkpoint](README.md#m50-multi-constructor-retained-package-patterns).


## Function-valued retained package fields prior art

M51 extends the existing field telescope to a stored function. Its later
payload type can refer to `f n` under the actual stored `f` and `n`.
The constructor uses an ordinary erased payload parameter. This uses the
existing opaque call rule; it adds no evaluator or call-injectivity rule.

Agda v2.8.0's [datatype constructors](https://agda.readthedocs.io/en/v2.8.0/language/data-types.html#general-form)
use dependent function telescopes. Its [record declarations](https://agda.readthedocs.io/en/v2.8.0/language/record-types.html#declaring-record-types)
allow later field types to depend on earlier fields. The builtin
[Sigma source](https://github.com/agda/agda/blob/v2.8.0/src/data/lib/prim/Agda/Builtin/Sigma.agda#L7-L11)
places the second field under the actual first field. A function can occupy
that first field. These are the formation and scope precedents. They do not
provide GHC's lazy-field semantics, Many restriction, FC roles or file codec.

The pinned v2.8.0 Git blobs are datatype manual
`7713c0755a94f784971a637059caaed4c616f04e`, record manual
`6abb10deffa8796ec2de469f37a096dc23ba13d5` and builtin Sigma
`d68aead018bb834aec9a547a7907576005c28dcf`.
The selected reading is datatype lines59–76 and95–108, record lines162–178,
and the full17-line Sigma module. Cached bytes and reading provenance are in
`_build/verification/native-function-package-next-research-v1/provenance.json`.
The rendered datatype and record manuals were checked again on2026-11-15.
The Sigma web fetch failed; its exact cached primary source was read instead.
No Agda execution, full-manual reading or termination/positivity transfer is claimed.

Eisenberg's [2016 thesis](https://richarde.dev/papers/2016/thesis/eisenberg-thesis.pdf),
printed54–55/PDF69–70, separates dependency from relevance. Here both stored
index fields are relevant: later types depend on them, and runtime code can
use their values. An erased existential variable is a different mechanism.
The selected thesis text was revisited; no full-thesis reading is claimed.

[System D/DC, ICFP2017 author version](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf),
PDF7–9 and14–16, supplies relevant telescopes and typed substitution as broader
prior art. Section9, printed31:27/PDF27, explicitly leaves primitive datatypes
and pattern matching to future work. It does not verify this DataCon/importer
extension. The seven-work/nine-PDF ledger is unchanged; no proof replay occurs.

For the wire boundary, follow GHC's existing separate free-variable diagnostic
forms. A diagnostic free function head must remain distinct from a scoped
local interface name. The new type tag requires a distinct fork version and
actual old/new reader checks. See [the M51 plan](PLAN.md#m51-function-valued-retained-package-fields).


M51's measured implementation is now described in the
[contract](CORE.md#function-valued-retained-package-fields) and
[checkpoint](README.md#m51-function-valued-retained-package-fields). The source
and mixed-domain object programs pass in both stages. The ordinary body
rejection for inverse call equality is distinct from the two explicit policy
guards. These operational results do not transfer Agda's totality or D/DC's
preservation result to this implementation.

## Exported function-valued retained signatures prior art

M52 extends complete ordinary value signatures from local bindings to exported
bindings. Application substitutes the actual function/input into the result.
The binder's scope must survive interface writing and fresh reconstruction.

Agda v2.8.0 [internal terms221–260](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/Internal.hs#L221-L260)
represent Pi domains/bodies and neutral variable application spines.
[Substitution827–850](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Substitute.hs#L827-L850)
substitutes variable heads and eliminations. [Abs substitution1053–1063](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Substitute.hs#L1053-L1063)
lifts the substitution under a binder. These exact cached primary ranges were
read again for M52. Git blobs are `a6e0938ed369b1f0f5f0b8aa1ff3baeb9aad033b`
and `1ef41f6d2a0223c8328984903b60dabc2a05a783`. The original read context and
source hashes are in `native-function-signature-next-research-v1/provenance.json`.
No fresh fetch, whole-module reading or Agda proof replay is inferred from
this cache reuse.

The existing 2017 author PDF of
[A Specification for Dependent Types in Haskell](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf),
PDF14–16, gives relevant Pi application and typed substitution. Its Pi
congruence permits broader domain transport. M52 preserves the existing fixed,
closed Many function domain and does not implement that broader rule. These
sources support the scope/substitution design. They do not specify GHC's wire
codec, lazy runtime ABI or restricted optimizer. See
[the implementation plan](PLAN.md#m52-exported-function-valued-retained-value-signatures).


The M52 source-permission check uses the complete named-value path, not the
general `FunSigCtxt` diagnostic label. Its measured separate producer/client
calls preserve actual imported function/input binders. The same interface
version and type tags encode the telescope. No runtime function equality or
inverse call rule follows from this change. See the
[contract](CORE.md#exported-function-valued-retained-value-signatures) and
[checkpoint](README.md#m52-exported-function-valued-retained-value-signatures).

The selected Agda v2.8.0 manual reading now also covers
[function domains and telescopes, lines20–55](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/function-types.lagda.rst#L20-L55).
Its Git blob is `f4cd92271648ed191d4daa565e7487a121846bfb`. It states that
application substitutes the actual argument in the codomain. The selected
[Pi formation code113–134](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L113-L134)
checks the body in the telescope context. The
[telescope code264–315](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L264-L315)
checks domains sequentially and extends the context before later domains and
the codomain. The Term.hs blob is `02991c9fa0085315cc854d2833a9080af14a7740`.
`native-exported-function-signature-primary-reading-v1/reading.json` records
these actual selected ranges, the fresh tagged manual fetch and reused source
copies. Only those ranges were read for this comparison.

These are scope and substitution precedents. They do not specify GHC's complete-
signature permission, local Many operands, same-version interface compatibility,
Haskell bottom behavior or malformed interface validation. The synthetic
ordinary-domain knot observations retain their unresolved `<<loop>>` outcome.
They are not Agda counterexamples or successful defensive-import tests. The
seven-work/nine-PDF reading ledger is unchanged. The selected reread of the
existing 2017 author PDF14–16 adds no paper or whole-paper reading claim.

### Native dead local binding analysis prior art

Agda v2.8.0 distinguishes complete free-variable queries from queries that
filter occurrences by relevance. Its
[`allFreeVars` definition](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Free.hs#L220-L268)
uses `IgnoreNot`. The Git blob is
`a45e117bb0ff9a840efe1ab978b5c6bc147aad4f`. The
[internal traversal](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Free/Lazy.hs#L515-L600)
collects variable heads and spines, Pi components and type sort annotations.
[`underBinder`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Free/Lazy.hs#L440-L462)
masks bound indices. The Lazy.hs blob is
`c0a19fb372712faccb45d939759276ee229ae9f5`.

The separate
[Treeless unused-argument pass](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Compiler/Treeless/Unused.hs#L22-L96)
works after translation to Treeless. Its blob is
`5782414c6f8642e0e4aa1c805662ae96b8580851`. Its representation omits native
GHC type annotations. It cannot replace scope analysis before GHC erasure.
Agda's
[erased eta discussion](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Compiler/ToTreeless.hs#L547-L588)
assumes total, effect-free terms. Those assumptions do not cover Haskell
bottoms or `seq`. The ToTreeless.hs blob is
`112b852e645f61614c37709bb8beb66e42c0b790`.

M53 uses GHC's existing `freeVars` annotation tree and
Note [The FVAnn invariant] in `GHC.Core.FVs`. That code includes deep Id
types, multiplicities, binder telescopes, case results, coercions and stable
metadata. Note [Controlling elimination of dead bindings in occurrence
analysis] in `GHC.Core.Opt.OccurAnal` describes the `oa_can_drop` policy and
bindings used only by Liquid Haskell refinement types. M53 tests membership
in the original body annotation. It preserves metadata on retained binders
and infers no new occurrence counts. It adds no FV walker or `ManyOccs`
tags. The general occurrence analyser stays gated.

The local reading bundle is
`native-occurrence-analysis-primary-reading-v1/reading.json`. It records ten
full source fetches with verified Git blobs and 22 selected reading ranges.
The selected reading covers 1,360 lines. It does not claim full-file reading,
a new paper, a proof replay or a preservation theorem for this GHC pass.
The seven-work/nine-PDF ledger stays unchanged.

### Detected recursive interface domains prior art

GHC's pinned [interface knot Note, lines170–208](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/IfaceToCore.hs#L170-L208)
makes the caller responsible for tying the environment knot. The
[lazy argument discussion, lines660–702](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/IfaceToCore.hs#L660-L702)
explains why loading constructor arguments eagerly can loop. The
[implicit-name invariant, lines1031–1068](https://github.com/ghc/ghc/blob/19ff36f2aede0ca2e554016ff22c71e73aa9803b/compiler/GHC/IfaceToCore.hs#L1031-L1068)
requires name lookup to avoid suspended type checks. The file's Git blob is
`a6997bcc145e4ea36b78e0cb144ece139c34dffd`.

Agda v2.8.0 registers
[`DataOrRecSig`, lines681–710](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Decl.hs#L681-L710)
and performs [post-mutual checks, lines265–280](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Decl.hs#L265-L280).
The Decl.hs blob is `0f1b76bbfe65bc8c1a93c4ad77a8539752196861`.
[Datatype checking, lines129–204](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Data.hs#L129-L204)
installs datatype information before adding constructors. The Data.hs blob is
`349a3c508d8a77d8f86b41decedec6de9a4d7afc`. These are staging precedents.
They do not prove progress for GHC's lazy loader. M54 uses them for diagnostic
containment. M55 below uses their declaration construction order.

The local [runtime definition, lines384–395](../../libraries/ghc-internal/src/GHC/Internal/Control/Exception/Base.hs#L384-L395)
defines `NonTermination` and its `<<loop>>` display. It explicitly gives no
guarantee that the runtime will detect an arbitrary loop. M54 catches this
specific exception at the existing four native interface classifier sites.
It changes diagnostic containment, not staging or the accepted domain rules.

The selected readings and verified source hashes are recorded in
`native-domain-loader-next-research-v1/reading.json`. Its bundle manifest
`SHA256SUMS` has SHA256
`804ddefdc5d64191fb5214f9a5acca7099a5c877e6805e65274ecd371cabc5ba`.
`native-domain-loader-review-v1/exception-boundary-addendum.md` records the
separate exception-boundary review. No new paper or whole-file reading is
claimed. The seven-work/nine-PDF ledger is unchanged.

### Native interface domain shapes: provenance

M55 follows the declaration-staging order in the
[GHC and Agda sources above](#detected-recursive-interface-domains-prior-art).
GHC's lazy interface Notes require deferred reference lookup and independent
implicit-name enumeration. Agda v2.8.0 installs datatype information before it
checks constructors. This is a construction-order precedent. It supplies no
termination theorem for this loader. A temporary empty constructor list is
not evidence that a native domain is checked and empty.

The fork stores a compact derived view on each reconstructed `TyCon`. This
keeps the view with the actual boot, real or reconstructed object. A global
Name cache could retain a view from a different reconstruction. The nullary
cache update follows `setTcTyConKind`. Synonym flags such as `synIsTau` and
`synIsFamFree` depend on the decoded RHS, so they cannot supply this raw view.
These ownership and rejection policies are choices in this fork.

The alias-cycle probe exposed an earlier boundary in
[`bindIfaceId`](../../compiler/GHC/IfaceToCore.hs),
[`mkLocalIdOrCoVar`](../../compiler/GHC/Types/Id.hs) and
[`isCoVarType`](../../compiler/GHC/Core/Predicate.hs). The allocator expands
synonyms before an Id-based precheck can run. M55 therefore checks the raw
domain and multiplicity first. It preserves the existing allocator and typed
checks. The selected snapshot includes the two unchanged allocator/predicate
dependencies. The trace records compile success, OS termination of the loop
and unchanged inputs. It is not a successful test run.

`native-shape-first-loader-design-v1` records the representation and header
addendum. `native-shape-first-loader-review-v1` records static reviews and their
limits. The initial review missed diagnostic priority for scoped indices.
Actual API failures led to the raw variable/call-index and constraint-arrow
fallbacks. `native-interface-domain-shapes-unit-trace-v1` records the allocator
boundary. All accepted domains still pass the original typed predicates.
The seven-work/nine-PDF ledger is unchanged. No new whole-paper reading or
proof replay is claimed.

## Closed datatype parameters and constructor specialization

The M56 reading uses selected official Agda v2.8.0 source ranges. In
[`Rules/Data.hs`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Data.hs#L249-L337),
constructor checking carries the datatype parameter telescope. The selected
return-type checks are at lines 1820–1881. In
[`Rules/LHS.hs`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1338-L1400),
constructor splitting uses the actual datatype parameters and specializes
the constructor type. Data.hs lines 120–129 make parameter erasure conditional
on Agda's erasure and equality settings. Do not transfer that convention or
Agda's evaluation properties to lazy Haskell.

The cached complete files were verified against Git blob IDs
`349a3c508d8a77d8f86b41decedec6de9a4d7afc` (Data) and
`3779ffa6e418bca99e30c071e189106f82ecbe1b` (LHS). The selected reading, exact
SHA256 values and scope are in `native-applied-domain-research-v1/`. This is
source-range reading. It adds no paper to the seven-work, nine-PDF ledger.

GHC already provides `dataConInstArgTys`, `dataConInstOrigArgTys` and nominal
`normaliseType`. `mkInferredPolyId` in `GHC.Tc.Gen.Bind` normalizes an inferred
type for selection and reconstructs the coercion through subsumption. M56
uses that pattern for cached argument views. It preserves the original cache
and usage. The D/DC, System DR and GraD limits in the paper ledger still
apply. No theorem or proof artifact was replayed for this extension.


## Nominal retained domain conversion

M57 follows the asymmetric Pi rules in the
[2017 D/DC author specification](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf).
An-PiCong is in Figure 8 on PDF page 16. An-Push is in Figure 10 on page 19.
The body proof uses the source binder. The target body substitutes the target
argument cast through inverse domain evidence. This supplies the conversion
and application precedent. It supplies no Haskell function eta law.

The selected Agda v2.8.0 comparison is
[Conversion.hs 746–761](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L746-L761)
and [827–849](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L827-L849).
Agda compares the domain and modalities before it checks bodies in one context.
The cached file has Git blob `69423e2c11b6f35bc5eee2d502cf2830efbc005a` and
SHA256 `1c4680f328a55518df9e7b23e360760f439b5d2d6a7d6fba8d1c539758d15a09`.
The D/DC author PDF has SHA256
`86c8496096a036aa6662ef9369de8ba668f977110759a744da7e723c5c2cd18e`.
Its 30 PDF pages differ from the published 29-page extent. Keep the versions
separate. Selected reading and scope are recorded in
`native-domain-conversion-research-v1/provenance.json`.

The implementation adds optional nominal domain evidence to `DepFunCo`.
`MRefl` preserves its existing fixed-domain specialization. Existing `CastTy`
records the converted index kind. Runtime casts preserve the nominal child
inside raw `SubCo`, including phantom datatype parameters. No arbitrary
representational proof becomes nominal evidence. The 2019 roles and 2021
usage limits in the paper ledger still apply. This extension has no combined
preservation theorem or replayed mechanization.

Review found three scope obligations: freshen the target binder against the
domain proof, preserve nominal evidence during value-cast rewriting, and
substitute proofs during beta exposure. Existing API controls now check these
cases. Runtime controls include seven bytecode observations and one separately
linked native executable. The source Showcase has no new observation yet.

Further C1 work remains unimplemented. D/DC An-PiFst and An-PiSnd on PDF page 17
supply the opaque projection and instantiation precedent. Start with nominal
opaque evidence; arbitrary representational Pi equality cannot provide nominal
domain equality. A phantom `Token` parameter gives a concrete counterexample.
Constant families can reduce two genuine Pis to ordinary representationally
related arrows over `Token Bool` and `Token Int`. Their domains still lack
nominal equality. Agda supplies context discipline, not FC proof projections.

Source review identifies earlier proof loss in `mkWpCastN` and `WpCompose`.
The current retained-Pi unifier accepts already-equal types. A future bounded
rule can normalize closed domains nominally and compare aligned bodies exactly.
It must retain nominal evidence before desugaring. This is a proposed source
step, not completed dependent subsumption. The seven-work, nine-PDF ledger is
unchanged. No new whole-paper reading or theorem replay is claimed.


## Complete retained-function type arguments

M62 reuses GHC's visible type-application entry point and anonymous-wildcard
syntax walk. The original `Tc.Gen.Sig` file at the M61 checkpoint has Git blob
`e646bd1b26ca74404542d1734b3ff1410243e070`. Its lines 299–344 inspect nested
type arguments, binder kinds, multiplicities and constraints. The new shared
predicate retains that rule. Named wildcard binders remain the renamer's list.

The selected [Visible Type Application paper](https://doi.org/10.1007/978-3-662-49498-1_10)
provides the explicit-choice and bidirectional-checking comparison. The earlier
[retained-type-argument discussion](#retained-function-types-as-equality-operands)
records Agda Pi formation and expected-type checking. Neither supplies a proof
for this fork's inference or runtime representation. This pass claims no new
paper reading. The version-specific reading scopes remain in the paper ledger.

The permission belongs to a complete explicit type argument. It is carried
through the existing checker so nested type-constructor operands use the same
formation rule. It does not grant permission in a kind, method signature,
SPECIALISE pragma, hs-boot declaration or other unsupported signature context.
Whole-argument completeness includes wildcards outside the retained telescope.
Do not infer permission from the wildcard mode or an outer syntax shape alone.
Existing lexical scope, staged-code checks and native-meta restrictions remain.

Current `GHC.Tc.Gen.App.tcVDQ` calls `expr_to_type`, then `tc_inst_forall_arg`.
A required argument can contain a nested `HsEmbTy` plus a separate expression
`HsHole`. The translation builds one `HsWC []` and changes the hole to
`HsWildCardTy`.
The renamer checked the nested embedded type, not this combined type. This
existing RequiredTypeArguments path is the concrete reason to check the whole
renamed argument in `tcHsTypeApp`. See Note [RequiredTypeArguments and the T2T
mapping] in that module and Note [Retained Pi type arguments] in
`GHC.Tc.Gen.HsType`.

Compiler `dc8d5635a8` and source `4c31ce0145` pass both-stage source controls.
All three T2T hole placements reach located completeness diagnostics. The
corrected eighteen-case oracle also checks kinds, scope, container demand and
ordinary wildcards. Direct explicit `seq` works in NoImpredicativeTypes;
separate outer Quick Look controls still reject. These are source and runtime
observations. They do not prove the new context permission sound in general.

## Opaque whole-function cast lowering

M61 first measures the existing whole-cast path. It uses the selected D/DC
An-PiSnd discussion on author-PDF page 17 and An-Push on page 19, the 2013 FC
instantiation discussion on PDF pages 6–8, and Pico's relevant/irrelevant
instantiation discussion on thesis PDF pages 127–129. The paper ledger retains
the exact versions and reading limits. No new whole-paper reading is claimed.

These rules distinguish evidence elimination from execution through a cast.
They do not require a new primitive for every application. The measured kernel
keeps opaque nominal evidence between distinct abstract payloads through typed
preparation. Only its caller closes both payloads to Int with reflexivity.
The actual NCG dump retains the abstract cast. Nine bytecode controls and two
object observations pass after native beta substitution is corrected. The
selected API and registered runs execute the controls from the existing fixture.
This does not implement general Pi projection or establish a preservation theorem.

The substitution correction follows the existing Agda context/substitution
precedent and this fork's scalar beta rule. Substitute an eligible bare function
argument through terms, types and coercions together. Ordinary let insertion can
make the alias escape in a partial result type. Keep Haskell's existing demand
and unsupported-argument restrictions. See Note [Restricted dependent
simplification] in `GHC.Core.SimpleOpt` and Note [Opaque retained function casts]
in the existing API fixture. The measured records use the new
`native-opaque-whole-cast-*` namespace. No singleton translation or eta law is added.

## Scoped source comparison

M58 constructs the existing M57 rule from source. It retains D/DC An-PiCong and
Agda's domain-before-body discipline from the preceding section. The new reuse is
GHC's `can_eq_nc_forall` control pattern: generate body Wanteds at a deeper level,
solve them synchronously in private state, track unifications, and retain used
Given evidence. A runtime Id cannot enter `ic_skols`. The retained counterpart
adds a lexical runtime-binder scope for rewriting and solver-plugin restrictions.
See Note [Solving retained function equalities] in `GHC.Tc.Solver.Equality`.

A complete forall-template translation would also need capture-safe abstraction
through cast and proof metadata. This increment uses a direct runtime scope instead.
Exact body equality remains a fast path. Ordinary payload inference is not restricted
to that path. The checked trace assigns its payload meta `Int` inside the deeper
body comparison. The escape control rejects a payload that contains its local index.

Source review found direct nominal-proof loss at `mkWpCastN` and at generic
`SubCo` reconstruction during zonking. Wrapper composition already retains the
children and their order. M58 preserves the nominal child before zonking and checks
scalar admission afterward. This corrects the earlier suspicion about composition.
No arbitrary representational coercion becomes nominal evidence.

Domain comparison reuses the solver family rewriter and its reduction-depth limit.
A family-axiom-only mode excludes inert evidence, plugins, family caches and zapping.
This preserves the closed-domain proof contract. Identical domains use reflexivity.
The pure Core normalizer did not enforce the source limit. The recursive-family
probe exposed that difference. See `native-source-domain-conversion-domain-depth-v1`
for the old timeout and `domain-depth-v4` for the selected bounded checks.

The versioned decision and primary-source hashes are in
`native-source-domain-conversion-research-v2`. The selected reading scope remains
seven works and nine PDFs. No new whole-paper reading, mechanization, Haskell eta
law, combined preservation theorem or sharing theorem is claimed.

## Ordinary-arrow matchability port

M63 implements the ordinary-arrow stage of the R23 foundation. The next stage
must add matchability to erased dependent quantifiers. The selected
[contract](PLAN.md#m63-ordinary-arrow-matchability) does not enable unrestricted
generic runtime domains between these stages.

The main sources are [proposal #242](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html),
its reviewed [prototype commit](https://gitlab.haskell.org/mmzk1526/ghc/-/commit/d65f4c6cc039385184c6d28ba5420a7a2b3ebc4f),
and Kiss, Field, Eisenbach and Peyton Jones,
[Higher-Order Type-Level Programming in Haskell](https://simon.peytonjones.org/assets/pdfs/higher-order-type.pdf),
DOI 10.1145/3341706. The later proposal assigns U to ordinary runtime arrows.
The 2019 paper's earlier term-arrow convention differs. Its Figure 5 permits
forward application congruence for either metadata value but restricts inverse
projection to M. Its substitution discussion explains why a proof created
before substitution needs the same typed distinction afterwards.

Current source review at `70f61c21e6` identifies the corresponding consumers:
`Tc.Solver.Equality.can_eq_app`, LRCo checking in `Core.Lint`,
`Core.Coercion.Opt.etaAppCo_maybe`, immutable unification in `Core.Unify`, and
injective-variable analysis in `Core.TyCo.FVs`. Current native TermAppTy is
already opaque in these operations. Adding a classifier with no stored
metadata would preserve the old behavior and would not establish R23.

The port must account for supplied full kinds in mkAlgTyCon, mkTcTyCon,
mkSynonymTyCon and mkFamilyTyCon. Updating mkTyConKind alone misses those
paths. The new specified invisible FUN parameter preserves ordinary visible
One binding. It still needs a concrete elaboration/defaulting policy. The
other three wired arrows retain the prototype's four-argument fixed-U layout.
The existing addendum supersedes contrary claims in the first ignored draft.

The prototype's forced invisible-M forall maker is not selected. A later
forall stage must preserve explicit invisible U, its outer metadata scope,
nominal evidence and modern required/invisible representational casts.
Agda supplies the separately recorded telescope/substitution comparison; its
eta and totality policies do not establish the FC projection rule. No new
paper version, whole-paper reading or preservation proof is claimed here.

## Function-valued fields in closed native indices

M65 uses the same proposal and 2019 paper as M63/M64. Re-read Figure 5 and
the surrounding discussion on PDF pages 13–14. Forward application preserves
both the head and argument proofs. Inverse projection needs M. The source
proposal still assigns U to ordinary runtime arrows. The paper's older term
arrow convention does not override that source contract.

The native bridge is an implementation choice. Canonical application of
`TermVarTy f` produces the existing `TermAppTy f a`. A family-head proof can
remain an ordinary AppCo. Preserve that proof when the family exposes f.
A direct TermAppCo alone would discard a nonreflexive head proof. Single and
batched makers, substitution, family reapplication and interface loading must
compute the same endpoints. These requirements follow from the current code
and its representations; the cited theorem does not cover the native Id forms.

The prior Agda neutral-application and substitution references remain relevant.
An opaque function field can occur in an index without evaluating its body.
Do not transfer Agda's normalization or eta policies to lazy Haskell. The source
client performs an explicit strict call when it needs to refine the result.
Function identity, call equality and function behavior remain distinct.

The selected Agda v2.8.0 implementation reading is pinned to the previously
recorded source bytes. In
[Internal.hs](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/Syntax/Internal.hs#L221),
a neutral variable retains ordered application and projection eliminations.
[Substitute.hs](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/TypeChecking/Substitute.hs#L70)
appends eliminations to neutral heads and selects a field of a known constructor
before applying its remaining arguments. Its substitution code replaces the
head and argument spine and lifts under binders. This is a scope comparison;
Agda does not carry the explicit FC head proof used by this fork.

The ignored `native-function-field-research-v1` records seven source files and
the selected reading ranges. Four implementation files were checked against
their existing Git blob and SHA256 pins. The other cached files have content
pins. The local page-14 render of the archived author PDF was inspected after
the web screenshot failed. No new PDF version, Agda execution or whole-source
audit is claimed.

The ignored `native-forall-matchability-next-client-research-v1` records the
initial source review at M64 development head. The selected M65 contract is in
[PLAN.md](PLAN.md#m65-function-valued-fields-in-closed-native-index-domains).
No M65 runtime, compatibility or preservation result is claimed by this reading.

## Dependent modules, library boundaries and optional totality

The user selected a dependent subset in opt-in modules and suggested a small
DependentBase library. The name is provisional. This direction narrows the
initial adoption target. It does not authorize discarding an imported type or
adding a stronger guarantee to ordinary Haskell code. The local tasks are in
[ROADMAP.md](ROADMAP.md#local-subset-library-and-assurance-work).

The primary library reference is
[linear-base at abbb74f45f7d](https://github.com/tweag/linear-base/tree/abbb74f45f7dc395436159b9c12fe26ed5c30259).
Its [guide](https://github.com/tweag/linear-base/blob/abbb74f45f7dc395436159b9c12fe26ed5c30259/docs/USER_GUIDE.md)
uses mixed imports from linear-base and base. Its typed bridges preserve
specific contracts. `Ur :: a -> Ur a` stores an unrestricted field; it cannot
wrap an arbitrary linearly bound capability. The current `forget` consumes its
function linearly. Public `withLinearIO` runs a computation with an unrestricted
result. The internal `toSystemIO` is not exported. `Unsafe.Linear` and selected
IO adapters use explicit unsafe equality or coercion and require a separate
trust argument. These operations do not certify termination.

The guide's blanket restriction on linear let/where bindings is stale. The
current local GHC guide permits local linear bindings with stated restrictions.
Current GHC source also preserves imported multiplicity through interface
loading and call checking. Disabling syntax does not erase that contract.
Use the same principle for dependent imports. Prefer a package with its actual
index, a runtime checking operation, or an operation that forgets an invariant.
Do not provide a general unchecked index assertion as a normal bridge.

A small library can reuse the existing Nat, Vec, Fin, equality and packed-vector
examples. It need not duplicate every Prelude class. Its first compatibility
operations should convert vectors to lists, package lists with their actual
length, check a requested length, and eliminate a package with both its index
and payload in scope. Conversion of an infinite or undefined input can diverge.
No new library implementation is claimed by this research.

For optional totality checking, the selected references are
[Idris2 v0.8.0](https://github.com/idris-lang/Idris2/blob/15a3e4e70843f7a34100f6470c04b791330788df/docs/source/tutorial/theorems.rst#L295)
and [Agda v2.8.0](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/doc/user-manual/language/termination-checking.lagda.rst#L92).
Start with coverage and structural descent over the complete recursive component.
An unverified callback or import remains unknown. Failure to prove termination
does not prove divergence. Record checked evidence and trusted assumptions
separately. Agda's safe mode also controls positivity, universes and imports;
a termination check alone does not supply that logical guarantee.

Vazou, Seidel, Jhala, Vytiniotis and Peyton-Jones's
[Refinement Types For Haskell (ICFP 2014)](https://simon.peytonjones.org/assets/pdfs/refinement-types-for-haskell.pdf)
provides Haskell-specific prior art. Its stratification separates potentially
divergent terms, weak-head values and finite values. Do not use a lazy binding's
postcondition as an unconditional logical assumption. A structural-recursion
argument must state its finite-input contract. Constructor matching alone does
not establish that contract.

The 14-page author PDF is archived in the ignored
`native-forall-totality-laziness-research-v1` directory. Read selected passages
in sections 2.2, 2.4, 2.5, 4.2, 5.1 and 5.2, plus Corollary 1. The paper ledger
records hashes and reading scope. No current LiquidHaskell implementation,
measurement or proof was replayed. This comparison adds no solver dependency
and grants no new conversion or totality rule to this fork.

The bounded GHC source review selects no checker implementation. A first
candidate can inspect the final zonked `tcg_binds` through
`typeCheckResultAction`. It can reuse coverage analysis at `pmcMatches` before
simplification. That interface returns `Nablas`, not a totality certificate.
Skipped checking, approximation and trusted COMPLETE assertions need explicit
treatment. Recompute the full call graph: `Tc.Gen.Bind.mkEdges` excludes signed
references for inference and can therefore omit a recursive cycle.

Start with direct structural recursion in singleton recursive components.
Unsupported syntax, mutual recursion, callbacks and unverified imports stay
unknown. Report the finite-input, observation and callee assumptions. A lazy
constructor can contain bottom; a cyclic Nat can have weak-head normal form
without a finite normal form. No advisory result may strengthen conversion or
relax native gates. The ignored `native-arrow-matchability-totality-followup-v1`
records nine current source files and ten archived inputs. It is a source
review and proposal, not an executed checker or termination proof.

Import maintenance needs more than a successful status flag. In pinned Agda,
[trust overrides](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/Termination/TermCheck.hs#L176)
can set the same successful termination status as a check. Its
[interface loader](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/Interaction/Imports.hs#L863)
checks relevant options and imported full hashes. Its
[full hash](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/TypeChecking/Monad/Base.hs#L1377)
combines the module source hash and imported full hashes.

Pinned Idris2 stores
[termination and coverage separately](https://github.com/idris-lang/Idris2/blob/15a3e4e70843f7a34100f6470c04b791330788df/src/Core/TTC.idr#L565).
Its [reuse check](https://github.com/idris-lang/Idris2/blob/15a3e4e70843f7a34100f6470c04b791330788df/src/Idris/ProcessIdr.idr#L325)
compares source freshness and imported interface hashes. Its
[body hashing policy](https://github.com/idris-lang/Idris2/blob/15a3e4e70843f7a34100f6470c04b791330788df/src/TTImp/ProcessDef.idr#L979)
depends on visibility and inline flags. This bounded reading does not establish
complete invalidation for every change that affects termination.

A future report must retain checked, trusted and unknown provenance. Record
checker policy, options, input assumptions and callee evidence. Recompute
affected call graphs and recursive components when those inputs change.
The ignored `native-forall-totality-interface-research-v1` archives six source
files and records the selected reading scope. This is source research. It
implements no checker and proves no invalidation theorem.

Casinghino, Sjoberg and Weirich's
[Combining Proofs and Programs in a Dependently Typed Language (POPL 2014)](https://www.seas.upenn.edu/~sweirich/papers/popl14-trellys.pdf)
is direct mixed-fragment prior art. The author PDF is now archived and hashed.
Read its introduction, relevant examples and technical sections on PDF pages
4 to 11. Its potentially nonterminating solver can supply logical evidence
after evaluation returns a suitable value. Corollary 9 proves closed logical
CBV termination. It does not prove normalization of arbitrary open terms.
Contradictory equality assumptions can type a looping logical expression.
The completed core proof omits several features implemented in Zombie.
The authors report Coq checking with functional extensionality. No proof was
replayed. Value restrictions, total inputs and imported assumptions need a
separate account for lazy Haskell. See the version-specific reading scope in
the paper manifest. Safe Haskell supplies a separate module-trust comparison;
it is not a totality checker.

The ignored `native-arrow-matchability-boundary-research-v1` directory records
source pins, selected line ranges and twelve verified Git blobs: eight
linear-base files and four Idris/Agda files. It also records local GHC source
hashes. No compiler, totality checker or library was run for that research.

## Observational equality and partial Haskell

The suggestion of observational equivalence opens a research task for R05.
Distinguish contextual equivalence of programs, explicit propositional equality,
and checker conversion. OTT develops the latter two with extensional equality
and transport. It does not decide arbitrary contextual equivalence of Haskell
programs. Pointwise equality also does not make arbitrary lifted Haskell
functions interchangeable: `seq` distinguishes bottom from a lambda with a
bottom body. See [Haskell 2010, section 6.2](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html).

Schmidt-Schauß, Sabel and Machkasova define contextual equivalence by
convergence in every context of a specified calculus. Their language
includes lazy evaluation and `seq`. This makes precise what a client
would need to replace one function everywhere. The boundary example
instead gives an equality for each named Boolean observation.
[Paper and reading record](papers/README.md#contextual-equivalence-in-a-call-by-need-calculus-lmcs-2015),
[Definition 2.2, page 6](https://lmcs.episciences.org/930/pdf#page=6) and [the fork's two checked observations](BOUNDARY-EVIDENCE.md#what-does-an-observation-establish).

The [paper ledger](papers/README.md) now records four selectively read PDFs:
Altenkirch, McBride and Swierstra (2007); Pujet and Tabareau (2022);
Sirman, Lennon-Bertrand and Krishnaswami (TYPES 2024, published 2025); and
Pujet, Leray and Tabareau (TOPLAS 2025). Their versions, hashes, page counts and
reading scopes are in [manifest.json](papers/manifest.json).

The 2022 paper identifies a conjecture dependency in the earlier normalization
argument. The Haskell NbE implementation is an experimental object-language
checker. Its `U : U` choice is deliberately inconsistent and undecidable.
The expanded CICobs article limits its formalized conversion proof to the
natural-number fragment. No formalization or prototype was rebuilt here.
None of these results certifies the native Core extension.

An OTT experiment would need a separate specification for proof computation,
partial runtime terms, open casts, erasure and Core roles. Extensional equality
would not remove the M premise for inverse application: a constant function
still maps distinct inputs to the same output. M63 therefore retains its
selected matchability rules. No OTT design switch is selected.

The ignored `native-arrow-matchability-observational-research-v1` directory
records the source review and a pinned four-file implementation archive.
`native-arrow-matchability-observational-papers-v1` contains four original PDFs
and per-page pypdf extractions. All eight PDF/text hashes verify. Automated
extraction does not expand the recorded reading scope.


## Bounded native frontend family views

M66 reuses the current nominal family-only rewrite in GHC.Tc.Solver.Rewrite.
The source contract is `rewriteFamilyAxioms`; `rewriteType` and `tcNormalise`
do not reduce family applications. The existing retained-function equality,
Typeable, pattern-synonym and strict-call paths provide local precedents.
No new dependent typing or equality rule is selected.

The ignored `native-bounded-family-research-v1` pins twenty source files.
It records the native Unify/App sites, proof directions and the import cycle.
The separate `native-bounded-family-diagnostics-research-v1` pins twelve files
at clean M65 commit `374d95b5e2994693c2898abd87b35d6bf3f91c97`.
Its corrected note matches that recorded HEAD. The prior prose and registry
remain archived. The selected source hashes agreed when read. The implementation
changes the documented native Unify/App call sites.

`getCtLocM` captures source and error context. Restore an original argument's
`addArgCtxt` before creating its location. `solverDepthError` later restores
that location. Preserve the cache position and its existing VTA offset.
`runRewrite` resets depth at the start of each view. Recursive family RHS
steps then increment the same location. This is a per-view finite limit,
not a global work bound. The explicit zero-depth flag retains its old policy.

M65 execution later records four runtime stack overflows despite a finite
depth flag. The corrected finite client passes five observations.
Both rebuilt M66 stages report located GHC-40404 for all five loop controls
and preserve those five finite observations. The full self-hosted build,
Showcase100/16 and seven registered tests pass. The VTA diagnostic counts its
written type argument: the third
value argument is the fourth displayed argument. This matches the existing
`tcValArgsWithCache` source convention. These results change no totality or
cross-language calculus claim.

## Generic domains after matchability and function values

The full R05 target includes structural `[a]` refinement with an opaque head
at `a`. Ordinary instantiation must preserve that meaning at Int, functions,
arbitrary lifted types and types such as `Vec Int n` with an earlier runtime
index. A runtime-independent preparatory gate does not define the final policy.

The ignored `native-generic-domain-metadata-research-v2` pins twenty-seven
current source files. `native-generic-domain-policy-research-v2` pins nineteen
source files, the prior reports and selected cached Agda/proposal passages.
They extend the dated generic-domain research after M64/M65. Their source
controls and implementation sequences remain unexecuted proposals.

Separate structural container formation, opaque value identity and call
permission. An element must not lose opaque support when its type is known.
Update retained binder and occurrence metadata through ordinary substitution
and both zonkers. Equality of runtime Ids alone can hide stale occurrence
types; inspect kind, type, deep free variables and Lint as well.
Then extend nominal transport through later dependent domains and opaque
element-kind casts before source and interface admission.

The selected Agda telescope-prefix substitution and proposal #378 neutral-value
passages provide comparisons. Their assumptions do not prove this design.
The full target, implementation order and remaining obligations belong to
[R05](roadmap/R05-dependent-core.md).

M67 records the full implementation contract in
[PLAN.md](PLAN.md#m67-structural-generic-domains-and-dependent-domain-transport).
The ignored `native-generic-domain-substitution-algorithm-v1` pins twelve
current source files at M66. It distinguishes source binder metadata from
stored target ranges and target in-scope representatives. Only source metadata
receives the substitution. Same-Unique binder updates use the in-scope entry.
Free-source preparation and composition need an explicit provenance contract;
membership alone does not establish fresh metadata. Proposed raw controls
remain unexecuted at specification time.

The ignored `native-generic-domain-zonker-research-v1` pins ten source files.
It refines the earlier prefix-map proposal: the existing metadata-only helpers
can reconstruct original Id prototypes without new environment state. This
requires acyclic earlier-index metadata, unchanged binding identities and
zonk-equivalent input prototypes. Final zonking is not idempotent. It must not
feed its own unfinished output back as input or consult the recursive IdInfo
map from a type occurrence. These are implementation obligations, not a new
domain-admission or termination result.

The M67 metadata implementation and executed controls are recorded in
[PLAN.md](PLAN.md#m67-metadata-foundation). The older proposed probe lists
retain their original unexecuted status. Actual receipts distinguish the raw
metadata failure on M66, corrected bootstrap controls, complete build and
self-hosted registered checks.

Three later source reviews refine the remaining implementation:

- `native-generic-free-id-callers-research-v1` pins eighteen source files.
  Actual forall-instantiation callers own a complete source sigma and target
  arguments. Preserve target entries when adding source free variables.
  Ordered retained constructor instantiation already installs earlier actual
  field ranges. A universal metadata refresh is unnecessary at these callers.
- `native-generic-structural-schema-research-v1` pins twenty-eight source files
  and eight historical inputs. Derive parameter origin before substitution.
  Keep opaque value identity, structural roots and strict M65 call endpoints
  distinct. Include generated dependent lambdas and exact interface field kinds.
- `native-generic-domain-transport-research-v1` pins sixteen source/context
  inputs. Existing GRefl and the asymmetric DepFunCo rule supply the proposed
  evidence orientation. A separate native lifting component retains runtime
  source provenance. Coordinate domain reduction, body prefix, source
  abstraction, casts and backend guards. No new equality primitive is proposed.

These reviews ran no compiler or executable control. Their proposed source
clients and transport oracles remain unexecuted. The comparisons use the
previously pinned Agda telescope rules and current FC kind-binder lifting.
They do not establish preservation or transfer a totality guarantee.

The subsequent implementation preparation keeps the same provenance:
`native-generic-structural-schema-implementation-preparation-v1` derives the
parameter-field rule from current DataCon telescopes. The source-scope patch is
`native-generic-domain-scope-preparation-v1`. The existing FC lifting context,
GRefl and asymmetric DepFunCo supply the basis for
`native-generic-domain-transport-patch-preparation-v1`. The backend correction
is `native-generic-domain-coreprep-correction-preparation-v1`. Each preparation
records its base source bytes and patch hash. Root integration adds source,
Lint, interface and backend changes to that group. These are implementation
choices, not a new calculus proof. See the validation section in PLAN.md.

The completed bounded implementation is recorded in
[PLAN.md](PLAN.md#m67-generic-domains-source-and-self-hosted-validation).
Keep the earlier unexecuted research controls separate from that evidence.
The final compiled source uses base f315ea64aa and patch SHA256
ac4bacd83c8d9dafdcbf5f5335cc88ccb311f29bdf52979fd29495f4fb17ac45.
Its source manifest covers 27,299 files. The final provenance receipt relates
the later status documents and comment corrections to those unchanged code
bytes. No calculus correspondence or totality theorem is established.

The implementation follows these existing mechanisms:

- Agda's previously pinned ordered telescope substitution and constructor
  refinement guide scope and prefix order. The source views use actual lexical
  constructor Givens. They preserve the incoming Id and cast its occurrences.
  See Note [Declared retained argument refinement] in GHC.Tc.Gen.App.
- Existing FC lifting, GRefl and asymmetric DepFunCo supply nominal transport.
  A separate native relation records source Id provenance. Its common body
  uses the left binder and its forward cast. The right endpoint uses the
  inverse domain conversion. See Notes [Lifting runtime binder domains] and
  [Scoped native index abstraction] in GHC.Core.Coercion. The D/DC and roles
  papers supply comparisons; they do not prove this implementation.
- Existing IALL, cached usage and final subsumption support immutable choices
  from declared domains and named callbacks. List inference can use an actual
  declared native element. These are local inference rules, not a claim of
  Agda completeness. See the owning Notes in Tc.Gen.App, Tc.Gen.Expr and
  Tc.Gen.HsType.
- GHC's wired-in coerceId already has a compulsory cast template and no runtime
  definition. The restricted simplifier must expand it in ordinary derived
  code too. The change reuses that template and adds no eta rule. See
  Note [Restricted dependent simplification] in GHC.Core.SimpleOpt.
- Existing family reduction supplies the selected finite-depth diagnostics.
  Closed retained domains use reflexivity during whole-Pi normalization.
  Unequal-domain comparison keeps bounded reduction. An unchanged family type
  can pass to an unconstrained polymorphic argument without normalization.
  The cached list-view negatives measure the current normalization strategy.
  See Note [Closed retained domains] in GHC.Core.FamInstEnv.

The ignored correction preparations retain base bytes and patch hashes.
They cover App refinement v2, pending-substitution Lint, multiplicity checking,
native list inference, named callbacks, compulsory coerce expansion, closed
domain relevance and written argument prefixes. The final source patch above
includes integration corrections. The final receipt pins executed reports,
outputs, interfaces, compiler binaries and the two comment-only source deltas.
The four invalid local Haddock markers become ordinary comments. Two Rep Notes
state the current domain rules. All bytes outside those comments are unchanged.

The next boundary review is
`native-generic-module-interoperability-research-v1`. It checks the existing
source clients and the previously pinned linear-base sources. Ordinary clients
already use wrapper APIs. Direct retained calls without the dependent extension
remain unmeasured. Its proposed call matrix and requested-length list adapter
are unexecuted candidates. Imported type contracts must not be silently erased.


## Ordinary callers of retained APIs: M68

The [GHC 10.0 alpha2 linear-types guide](https://downloads.haskell.org/ghc/10.0.1-alpha2/docs/users_guide/exts/linear_types.html)
separates declarations, contextual usage checking and display defaults. Disabling
LinearTypes can hide multiplicity polymorphism in printed types. A linear
callback requirement still belongs to the interface. This supplies a comparison
for syntax opt-in without a general rule that discards imported contracts.
The current local docs/users_guide/exts/linear_types.rst records this distinction.

The [linear-base user guide](https://github.com/tweag/linear-base/blob/master/docs/USER_GUIDE.md)
uses Ur, conversion helpers and scoped continuation APIs to compose linear and
ordinary code. Reuse the explicit API design when a caller needs an ordinary
result. Its temporary let/where limitation is stale relative to current GHC,
which documents inferred local multiplicities and annotations. Do not adopt
that old limitation as a compiler requirement. Both guides were read on
2026-11-17. The source-pinned reading is
native-generic-module-boundary-primary-reading-v1 under _build/verification.

Agda's existing application provenance remains the primary telescope comparison:
Rules/Application.hs lines 619-641 retains a checked prefix, and lines 774-808 substitute in
order at the previously pinned v2.8.0 source. This work changes application
availability for already typed native values. It introduces no Agda meta or
unification rule. See [the existing application record](#applications-with-fixed-native-type-choices).

The read-only pipeline review native-generic-module-boundary-pipeline-reading-v1
pins Driver/Main/Hsc.hs, Driver/Main/Passes.hs, Core/Dependent.hs and the ordinary
Core pipeline at d430f27102. Detection scans actual native types and bindings,
not the source flag. Restricted object options still apply to ordinary callers.
The existing withPacked takes a retained callback; it is not an erased rank-2
adapter. An erased adapter remains an unexecuted library proposal.

The source matrix preparation and inference audit are respectively
native-module-interoperability-source-matrix-preparation-v1 and
native-module-interoperability-inference-audit-v1. The executed stage0 baseline
is native-module-interoperability-baseline-stage0-v1. It distinguishes direct
retained calls from an ordinary consumer's erased-index inference. Source and
artifact hashes, conditional rejection controls and authored stdout oracles
remain in those records. See [the bounded plan](PLAN.md#m68-ordinary-callers-of-retained-apis).

The App-only implementation is pinned by
native-module-interoperability-app-preparation-v2. Its proposed App.hs SHA256
is 0db8b4f3c5ea1e8d41ad012aa53c51fd34d67aed8d700815f97d1dc0e936e461.
The bootstrap build v2 and source-bootstrap-v1 execute those bytes. The separate
source-bootstrap-diagnostic-review-v1 confirms the callback, multiplicity and
Quick Look diagnostics after their positives. These are application and scope
checks. They do not establish a preservation theorem for the extended Core.

The full M68 evidence is recorded in PLAN.md. The API printer adds a promotion
tick to One in the multiplicity diagnostic. Its test checks the same code,
source span and semantic fragments independently of that display choice.
The corrected fixture changes one literal. Both compiler binaries and the 9551
source-check runtime artifacts remain identical. Published M67 interface readers
accept the new interfaces. This is compile-only compatibility evidence.

The next R03 candidate uses existing Fin and GenericCount. The ignored
native-generic-safe-index-next-candidate-v1 report pins the current libraries
and the previously read Agda telescope and pattern code. It proposes generic
safe indexing, including function-valued and runtime-index-bearing elements.
It is a static source-composition review. No implementation, execution or new
compiler rule is claimed by that report.


## Generic safe list indexing: M69

The [Agda finite-position example](https://github.com/agda/agda/blob/v2.8.0/doc/user-manual/language/instance-arguments.lagda.rst#L353-L377)
provides the existing Fin datatype precedent. M60 already records its source
blob 69c52629a737a8670fe11286a705111b7ba99e9d. M69 reuses the same Haskell Fin
and the existing signed adapter's control flow. It adds no instance-search or
termination rule. A Fin bound does not record list identity.

The previously pinned Agda splitCon code in Rules/LHS.hs lines 1503-1531
substitutes the constructor into the later telescope and target. Domain
substitution in Substitute.hs lines 1072-1077 preserves domain metadata. The
M69 review rereads those cached ranges. GHC uses its existing nominal
constructor proofs and lifting. No Agda preservation or totality theorem
transfers to the Haskell implementation.

The source and prior-art pins are in
native-generic-safe-index-next-candidate-v1/readpins.json. The proposed source
copy and patch are in native-generic-safe-index-implementation-preparation-v1.
These records describe unexecuted preparation. See the
[bounded plan](PLAN.md#m69-generic-safe-list-indexing) for acceptance gates.

The first source run is native-generic-safe-index-source-both-v1. It stops at
the original callback's list literal. The App declaration planner already
selects complete immutable arguments from named values. The selected correction
extends that advice to a known native list field. It reuses the monomorphic
field policy in Note [Inference of native list elements] in GHC.Tc.Gen.Expr.
The existing Quick Look argument-advice and Agda current-context references
remain the precedent. They do not permit filling GHC metas with runtime Ids.


### M69 execution provenance and remaining inference limits

The original primary runtime-vector source is preserved. After the App advice
correction, source-selfhost-v1 accepts 22 clients and checks 102 finite
observations across both stages. Ten rejection events have a separate manual
review. focused-selfhost-v1 accepts sixteen clients, checks twenty observations
and retains sixteen reviewed rejections. Each count belongs to its collection;
the collections overlap. The source and focused review receipts use the same
native-generic-safe-index- prefix and diagnostic-review-v1 suffix.

selfhost-build-v4 pins the final compiler/example inputs. testdeps-v3 records
its matching test dependencies. regression-v3 keeps API goldens 1/37 and checks
the generic library quietly in both source modes. showcase-v1 matches 124 lines
through seventeen modules with both compilers. registered-v1 records seven
passing selected tests and 54 skipped ways. format-v1 records non-passing
read-only formatter observations separately. These receipt names all use the
native-generic-safe-index- prefix under _build/verification. They do not replace
the final exact-HEAD main build or a publication receipt.

Preserved source failures distinguish the supported path. A raw error operand
rejects with GHC-83605. A named field of type forall a. a rejects with GHC-83865.
Both are preserved failed probes. The NoEDT standalone inferred
let-list also rejects with GHC-83865. The literal-list composition `fmap toList (lookupGeneric [vector] 0)` rejects with
GHC-83865 because a fresh n0 cannot contain the actual runtime n. The named-list
fmap variant was not executed before the Showcase case edit and is unmeasured.
These probes do not establish a general fmap restriction. The final Showcase
uses a lookupGeneric case before toList. The maintained fixture uses the known
mkFinGeneric/indexGeneric retained-list contracts, then converts the selected
vector before constructing Just. The original direct literal-list callback
remains unchanged and passes. No compiler change is made for these expression
inference limits. See [M69](PLAN.md#m69-generic-safe-list-indexing).

## Invisible retained binders: next candidate

The pinned [proposal #378 blob](https://api.github.com/repos/ghc-proposals/ghc-proposals/git/blobs/39a812b8154ee164e7de3ad9e27e21e012ebc000)
separates dependence, visibility and erasure at lines 478-539. Its foreach-dot
binder is invisible and retained. Lines 813-835 require a signature and show
alternative explicit @ patterns. This is a design sketch, not this fork's
current grammar or a runtime-override implementation.

Agda v2.8.0 separates [implicit arguments](https://agda.readthedocs.io/en/v2.8.0/language/implicit-arguments.html)
from [instance search](https://agda.readthedocs.io/en/v2.8.0/language/instance-arguments.html#instance-resolution).
Its pinned [Implicit.hs:70-102](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/TypeChecking/Implicit.hs#L70-L102),
blob 55a03200d2300486c91a95798fc8e0c3c9c675cf, distinguishes their metas and
substitutes inserted arguments through the telescope.
[Application.hs:577-641](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/TypeChecking/Rules/Application.hs#L577-L641)
and [774-808](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808),
blob 82ba94ac864d19b947d4c92b9af252b0fe81347a, check hiding, expected Pi structure
and domain modalities. Both source links pin commit
3d04bacca842729f9c0869b9287256321b5f450f. These are scope and ordering precedents. They do not
justify assigning GHC native runtime Ids to mutable type metas.

The ignored native-invisible-retained-next-candidate-v1 report/readpins proposes
a checked hidden Nat operand selected from a later vector's actual declaration,
paired with an explicit runtime operand.
M71 adds persistent visibility to the Core type and interface node. It checks
that the visibility cast preserves demand in the object pipeline. M72 uses
that cast in source definitions and inserts one runtime operand from a later
declared argument type. M73 adds a written runtime operand.
See [M71](PLAN.md#m71-visibility-of-retained-binders-in-core),
[M72](PLAN.md#m72-invisible-retained-binders-in-source) and
[M73](PLAN.md#m73-explicit-invisible-runtime-arguments).

Agda v2.8.0 inserts implicit patterns in the expected telescope context before
it compares visibility and chooses binder names. See
[ProblemRest.hs:108–129](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/TypeChecking/Rules/LHS/ProblemRest.hs#L108-L129),
blob 24cb7b08975d4efafb8f8473def6eef5fb9045b3, and
[Implicit.hs:63–94](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/TypeChecking/Rules/LHS/Implicit.hs#L63-L94),
blob f47fcb713d3b37af67fa11c3e8293afc73be8d84. This supplies the expected-type
and telescope-order precedent. The GHC renamer uses the signature's actual
term Name. GHC's existing rules determine argument demand and usage.

### Explicit invisible runtime arguments

[Agda 2.8.0](https://agda.readthedocs.io/en/v2.8.0/language/implicit-arguments.html)
uses braces for written implicit terms. Its pinned
[application checker](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
checks the term at the binder domain and substitutes it in the remaining
type. The blob is 82ba94ac864d19b947d4c92b9af252b0fe81347a.
[Proposal #378](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html#switching-syntaxes)
discusses switches between term and type syntax. M73 uses `@{term}` for an
explicit invisible runtime argument. The prefix `@` is a syntax choice
in this fork. The braces take their contents through the ordinary term
namespace. The existing `@Type` form still selects an erased argument.

## Native list anchors in higher-order calls: M70

The M69 baseline rejects the literal lookup under fmap and accepts the
named list in EDT. M70 uses the list field to select the existing argument
cache. The original call and its argument checks supply the cached type and
usage. The classifier only reads a lifted monomorphic declaration. It
does not use the type as evidence for a lookup result.

[Agda v2.8.0 argument checking](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808)
records a checked argument with its modality and substitutes its term in
the remaining Pi. The pinned blob is 82ba94ac864d19b947d4c92b9af252b0fe81347a.
The implemented Agda rule checks the current argument. GHC's existing
cache and callback path determine its own checking order.

The authors' January 2026 [Quick Look paper](https://simon.peytonjones.org/assets/pdfs/impredicativity.pdf)
describes inspecting ordinary application arguments before final checking.
The cached PDF has SHA256
cd2702e52a609806597890bea8f89a7bfd2fe28e8f3d158a40bf3ba894336f25.
M70 selects the existing non-Quick-Look path for these native inputs.
See [the M70 record](PLAN.md#m70-higher-order-calls-with-native-list-arguments)
and Note [Native list anchors] in GHC.Tc.Gen.App.

## Successive invisible arguments

Agda v2.8.0 uses the next written argument's visibility to determine
which hidden arguments precede it. It substitutes a checked argument
before checking the suffix of the application. M74 follows this telescope
order. The Agda source is blob `82ba94ac864d19b947d4c92b9af252b0fe81347a`.
[Insertion in Application.hs, lines 605–641](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/TypeChecking/Rules/Application.hs#L605-L641)
and [substitution, lines 774–808](https://github.com/agda/agda/blob/3d04bacca842729f9c0869b9287256321b5f450f/src/full/Agda/TypeChecking/Rules/Application.hs#L774-L808).

This fork selects runtime operands by matching declared argument types.
A constructor tree in a declared index can supply the corresponding
runtime term. Each constructor must be in the caller's term scope. Each
leaf variable must identify an unrestricted lexical binding with the same
type. The existing Core index judgment checks the reconstructed tree.
GHC's built-in DataToTag instance checks constructor visibility and records
import usage. M74 records each constructor it uses when it inserts the
operand. See Note [DataToTag overview], especially DTW2, in
[GHC.Tc.Instance.Class](../../compiler/GHC/Tc/Instance/Class.hs) and
[the M74 tests](PLAN.md#m74-successive-invisible-retained-arguments).

## Closed evaluation of a source function

Agda accepts mechanically checked recursion and distinguishes runtime
evaluation from reduction during typechecking. M75 starts with finite
constructor tables. Its totality obligation concerns reduction on a closed
constructor input. Haskell runtime arguments can still be bottom.
[Agda 2.8.0 termination checking](https://agda.readthedocs.io/en/v2.8.0/language/termination-checking.html)

Eisenberg and Stolarek translate a term declaration to a type family in
`singletons`. The implementation promotes each clause and emits a closed
family with the resulting equations. This provides an implementation to
compare against when choosing how the GHC checker represents a verified
source equation. The pinned Promote.hs blob is
`232bea5b719f5eb6eda9926f197e63f9d0c9f30a`.
[Haskell 2014 paper](https://richarde.dev/papers/2014/promotion/promotion.pdf)
and [Promote.hs, lines 1010–1121](https://github.com/goldfirere/singletons/blob/8c9221c164832bfd17878af94d50d8ade706932d/singletons-th/src/Data/Singletons/TH/Promote.hs#L1010-L1121).

Proposal #378 gives the primary namespace precedence and describes a
fallback to the other namespace. The M74 interface-only client reaches
GHC-76037 for the value name in a type synonym. In the same experiment,
the ordinary runtime function and a separately declared family both work.
Local sources and observations are in
`_build/verification/native-closed-evaluation-baseline-v4`.
[Proposal #378, lexical scoping](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html#changes-to-support-dependent-types)

Agda's manual starts with Boolean negation. The checker validates clause
coverage and termination separately. GHC can use a simpler test for the
M75 grammar. Each constructor occurs exactly once on the left, and every
right-hand side names a constructor. There are no recursive calls to check.
The type checker can therefore reduce a closed input without running the
producer's object code. [Agda 2.8.0 function definitions](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#introduction)

Serokell's closed-evaluator card uses `length [1,2,3]` as its example and
discusses bytecode or native execution. M75 uses GHC's closed-family
reducer for verified constructor equations. Reaching the list example
requires constructor fields and recursion. The dependency-analysis card
also calls for checking types that depend on values in the same module.
[Closed term evaluator](https://ghc.serokell.io/dh#closed-term-evaluator)
and [dependency analysis](https://ghc.serokell.io/dh#dependency-analysis).

### Constructor fields and the next evaluation rule

Agda's one-argument coverage example permits one clause for each constructor.
Its case-tree rules explain why a nested or overlapping pattern can change
which source equations hold by reduction. M76 limits each field pattern to a
fresh variable or a wildcard. One row per outer constructor then covers all
constructor values. [Agda coverage checking](https://agda.readthedocs.io/en/v2.8.0/language/coverage-checking.html#single-match-on-a-non-indexed-datatype)
and [Agda case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees).

`singletons-th` scopes variables from a clause's pattern over its right-hand
side and promotes constructor arguments in order. It warns when promotion
drops a lazy or strict pattern. For instance, the Haskell binding
`always ~(S _) = True` also accepts `Z`. M76 excludes explicit lazy and
strict patterns. A lifted field matched by a wildcard must remain unevaluated
at runtime. [Clause translation](https://github.com/goldfirere/singletons/blob/8c9221c164832bfd17878af94d50d8ade706932d/singletons-th/src/Data/Singletons/TH/Promote.hs#L1344-L1381),
[pattern translation](https://github.com/goldfirere/singletons/blob/8c9221c164832bfd17878af94d50d8ade706932d/singletons-th/src/Data/Singletons/TH/Promote.hs#L1395-L1433)
and [Haskell pattern semantics](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-590003.17).

For the list-length experiment, Agda's primitive recursion rule permits a
self-call on a direct recursive field of the current pattern. The M77
Haskell check uses the identity of the bound tail in that call. A call on
the original input or on a reconstructed list has no such justification.
This gives a finite reduction argument for a finite input spine. Haskell
can still supply a partial or infinite spine at runtime.
[Agda 2.8.0 primitive recursion](https://agda.readthedocs.io/en/v2.8.0/language/termination-checking.html#primitive-recursion)

The generated equation for cons has one family call. Its argument is the
tail variable from the left-hand side. It is smaller than the cons pattern,
contains no family application and uses the variable once. These are the
corresponding GHC checks for calls in a type family equation. A Haskell
module can enable `UndecidableInstances`; M77 still checks each source call
before it emits an axiom. [GHC family decidability conditions](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html#decidability-of-type-synonym-instances).

GHC also checks family applications in the patterns on the left-hand side.
The list constructors carry an invisible element argument in those patterns.
M77 uses the same `isTyFamFree` predicate as `lint_family_branch` when checking
that argument. [GHC type instance declarations](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html#type-instance-declarations).

For a polymorphic list length, `singletons-th` supplies a second precedent.
Its generated family declares specified type parameters in source order.
For `forall a. [a] -> Nat`, the corresponding family takes the element
type and the list as its two Core arguments. Only the list argument is required
in surface syntax. The family header fixes the arity. Its kind alone does
not. [Pinned promotion implementation and arity note](https://github.com/goldfirere/singletons/blob/8c9221c164832bfd17878af94d50d8ade706932d/singletons-th/src/Data/Singletons/TH/Promote.hs#L1289-L1335)
and [GHC family declarations](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html#type-family-declarations).

Agda's standard library gives `fromList` a vector result indexed by the
length of its input list. The cons clause keeps the input head and calls
`fromList` on the tail. M78 uses that signature and those two clauses for
`sourceVector`. Its length function accepts the same abstract element
type as the list. [Agda standard library v2.3, `Data.Vec.Base.fromList`](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda#L308-L310).

Agda's primitive recursion section also gives the rule for addition in
M79. The first argument loses a successor at each call. The second
argument passes through unchanged. This lets the base equation return
the second argument even when it is abstract. [Agda 2.8.0 primitive recursion](https://agda.readthedocs.io/en/v2.8.0/language/termination-checking.html#primitive-recursion).

### Numerals and arithmetic operators

Agda's `FROMNAT` design gives an in-scope conversion the ordinary natural
literal as input. The expected result type can select an instance.
Agda also has a distinct registration for the natural datatype itself.
Neither requires the user's constructors to be named `Z` and `S`.
[Agda 2.8.0 literal overloading](https://agda.readthedocs.io/en/v2.8.0/language/literal-overloading.html#natural-numbers)
and [builtin naturals](https://agda.readthedocs.io/en/v2.8.0/language/built-ins.html#natural-numbers).

Agda's addition equations inspect the left operand. For an unknown `n`,
`1 + n` reduces to a successor. The vector example uses the same equations,
so `safeHead` needs no proof at that length. Agda also specializes arithmetic
on closed built-in naturals. The fork's vector library evaluates its own
Peano definition. [Agda 2.8.0, functions on natural numbers](https://agda.readthedocs.io/en/v2.8.0/language/built-ins.html#functions-on-natural-numbers).

GHC already has built-in type numerals at kind `Natural` and an arithmetic
operator in `GHC.TypeNats`. Promotion in `singletons-th` translates an
integer expression through its promoted `FromInteger`. The M80 design
uses an open family to select the type-level conversion. It leaves a term
literal's `Num` or rebound `fromInteger` interpretation with its ordinary
typechecker. [GHC type literals](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_literals.html),
[pinned `singletons-th` implementation](https://github.com/goldfirere/singletons/blob/8c9221c164832bfd17878af94d50d8ade706932d/singletons-th/src/Data/Singletons/TH/Promote.hs#L1490-L1498),
and [Serokell's literal-promotion card](https://ghc.serokell.io/dh#promote-literals).

The `fin` library supplies the Peano conversion itself. Its `FromGHC`
equations map zero to zero and a positive natural to one successor of
the preceding natural. They can leave an abstract input stuck. M80 uses
that definition for the vector library's particular index type.
[Pinned `Data.Type.Nat.FromGHC`](https://github.com/phadej/vec/blob/df95689f85798832d834a8035fa37bc0d4f66363/fin/src/Data/Type/Nat.hs#L410-L418).

Agda's vector replication accepts the length and builds one cell for
each successor. In the Haskell library, `replicateAt 3 x` uses a required
type argument. After the numeral reduces, ordinary type class instances
construct the empty vector or its next cell. A separate retained function
accepts a length from IO. [Agda vector replication](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Vec/Base.agda#L246-L248)
and [GHC required type arguments](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/required_type_arguments.html).

### Computed arguments and lazy naming

Agda checks an application by substituting the supplied argument into the
dependent result. For M81 the argument is the application of a checked
source function. Its generated family represents the same application in
the result type, even when its arguments come from IO.
[Agda 2.8.0, function application](https://agda.readthedocs.io/en/v2.8.0/language/function-types.html).

A fresh name for an argument changes the index available in the typing
context. Koronkevich et al. carry information about the bound computation
in continuation typing. Agda substitutes local definitions during type
checking. The Haskell implementation keeps the source call in the checked
Core application and introduces a shared thunk when translating to STG.
This choice preserves Haskell's demand on the call without requiring a
general equality rule for local definitions. The [paper ledger](papers/README.md#anf-and-dependent-types-jfp-2022)
records the PDF pages used for this comparison.
[Koronkevich et al., sections 4.1 and 7.4](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/anf-preserves-dependent-types-up-to-extensional-equality/73FC888A23E5E87BAE16B158ABE349C8)
and [Agda 2.8.0, local definitions](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-expressions).

### Instance methods used as indices

The first R12 example uses the ordinary `Num` selector in
`replicateNative (n + m) x`. Agda models an overloaded operation as a
field of the selected instance record. A local record can give that field
a different implementation. The `singletons-th` instance promoter
similarly uses the method declarations of the particular instance.
[Agda 2.8.0, instance arguments](https://agda.readthedocs.io/en/v2.8.0/language/instance-arguments.html#defining-type-classes)
and [pinned instance promotion in singletons-th](https://github.com/goldfirere/singletons/blob/8c9221c164832bfd17878af94d50d8ade706932d/singletons-th/src/Data/Singletons/TH/Promote.hs#L541-L565).

GHC's class-selector reduction selects a field of a particular dictionary.
M82 checks the generated dictionary constructor, its selector slot and
the checked method body. When the instance defines `(+) = plus`, the
dictionary's interface records the actual selector and target. In M81
the stripped dictionary declaration contained only `DFunId` with an empty
information list. The extra information has to survive
`-fomit-interface-pragmas` for an independently compiled client.
[GHC's evidence representation](../../compiler/GHC/Tc/Types/Evidence.hs),
[instance method checking](../../compiler/GHC/Tc/TyCl/Instance.hs),
and [interface exposure policy](../../compiler/GHC/Driver/Config/Tidy.hs).

For the client, an instance lookup supplies a candidate. A delayed check
inspects the evidence for the ordinary dictionary constraint at that
operator occurrence. It must lead to the certified dictionary before GHC
can accept the index. This lets the ordinary solver decide which dictionary
a lexical constraint or instance supplies. A certificate for addition says
nothing about `fromInteger`, a different method of the same class.
[Dictionary selection in retained arguments](../../compiler/GHC/Tc/Gen/App.hs)
and [the solver's evidence check](../../compiler/GHC/Tc/Solver/Solve.hs).

### A runtime literal in a retained argument

Agda elaborates an overloaded numeral with the selected `fromNat`
conversion. Its builtin natural numbers have a separate registration
for the zero and successor constructors. These designs share the
expression used for the value and its index.
[Agda literal overloading](https://agda.readthedocs.io/en/v2.8.0/language/literal-overloading.html#natural-numbers)
and [builtin natural numbers](https://agda.readthedocs.io/en/v2.8.0/language/built-ins.html#natural-numbers).

In GHC, the typechecked runtime literal carries its conversion in
`ol_witness`. The packed-vector library gives `FromNatural` and
`Num.fromInteger` separate definitions. A different instance could
return zero for every runtime literal and still register type-level
`3` as three successors. M83 checks the selected method's equations over `Integer` and records
the actual zero and successor constructors in the instance interface.
The caller checks its selected dictionary before constructing the same
tree in the runtime expression and in the index. GHC uses its reduction
depth setting to bound this constructor expansion. In `n + 1` the lazy
sum contains a constructed operand. The type of `safeHead` still uses
ordinary type reduction on the library's independent `FromNatural` instance.
[Overloaded literal checking](../../compiler/GHC/Tc/Gen/Head.hs),
[literal desugaring](../../compiler/GHC/HsToCore/Match/Literal.hs),
and [the two vector-library conversions](examples/packed-vector/PackedVecLibrary.hs).

### Comparing a computed length with a function argument

Agda 2.8.0's `compareAtom` reduces both inputs before inspecting their
heads. It keeps two literals in literal form. When one side has a different
form, it can expose the literal's constructors. The fork currently expands
a retained numeral to the constructors certified by its Haskell instance.
In M84, immutable argument matching first tries the declared domain. When
its length has a different head from the inferred native index, the fork
uses GHC's bounded nominal family rewrite to expose a constructor. The
ordinary argument checker supplies the equality evidence. This lets a
scoped runtime value instantiate the erased tail parameter of `safeHead`.
[Agda’s conversion checker](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L492-L521),
[its comparison of constructor arguments](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Conversion.hs#L602-L613),
and [the fork’s inference example](PLAN.md#m84-infer-a-nonempty-vectors-tail-at-a-nested-application).

### Inferring erased parameters from constructor fields

Agda's list constructor has fields of types `A` and `List A`. Its implicit
argument rules let the type of an explicit argument determine a hidden
parameter. M85 uses the corresponding information in GHC's data constructor
signature. When checking `sourceVector (x : xs)`, the declaration of either
field can determine the element type. The fork matches declarations before
ordinary instantiation because the choice can mention a retained runtime
binder. The final argument check still checks every field at its source
location. [Agda 2.8.0, parameterized datatypes](https://agda.readthedocs.io/en/v2.8.0/language/data-types.html#parametrized-datatypes),
[implicit arguments](https://agda.readthedocs.io/en/v2.8.0/language/implicit-arguments.html),
and [the fork's constructor inference tests](PLAN.md#m85-infer-the-parameters-of-a-retained-constructor-argument).

### Unnamed runtime arguments in dependent patterns

Agda 2.8.0 represents a solved wildcard with a variable and records that
the user wrote a wildcard. Its telescope can therefore mention a value the
right-hand side has no source name for. M86 uses this rule for functions
and dependent packages. Each fresh wildcard gets a distinct identity. A
constructor branch uses the same anonymous field in its constructor equality
and its checked pattern. [Agda’s left-hand side checker](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L468-L481)
and [the earlier two-column wildcard implementation](#anonymous-terminal-fallback-equations).

Demand follows Haskell’s pattern rules. Forcing a retained argument must
preserve its identity in the remaining argument and result types. GHC's
existing explicit `seq` rule handles an analogous capture when an
unrestricted runtime value occurs in a type. [GHC's strict pattern semantics](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/exts/strict.html#strict-by-default-pattern-bindings)
and [the fork’s desugaring of seq](../../compiler/GHC/HsToCore/Expr.hs).

### Persistent dependent definitions and interactive results

Agda's interactive type query checks an expression in the loaded top-level
context. Its computation command parses in that context and evaluates the
checked expression. GHCi adds a requirement specific to Haskell: an earlier
interactive binding can denote a lazy heap value, and shadowing must
preserve the identity of the old binding for closures that captured it.
[Agda 2.8.0, interactive commands](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Interaction/InteractionTop.hs#L493-L504)
and [GHC 9.14.1, shadowing at the prompt](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/ghci.html#shadowing-and-the-ghci1-module-name).

GHC already packages a statement's typed results into a list of lifted
values. M87 uses the existing compulsory unfolding for this boxing
operation. A stored function keeps its type and its closure. A declaration
batch uses the checked program-to-bytecode path.
[Statement result construction](../../compiler/GHC/Tc/Module.hs),
[compulsory unfolding](../../compiler/GHC/Core/SimpleOpt.hs),
and [interactive declaration compilation](../../compiler/GHC/Driver/Main/Interactive.hs).

For a stopped computation, GHCi already gives a fresh unknown type
variable to each polymorphic variable whose value it cannot recover.
Apply the same rule to a free runtime index in the result and local types:
give all occurrences of the same index one unknown of its declared kind.
Use a separate unknown for each independent index. This requires neither
evaluation of the original value nor a new field in saved bytecode.
[GHC 9.14.1, the debugger and polymorphism](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/ghci.html#breakpoints-and-inspecting-variables),
[GHCi’s runtime type variables](../../compiler/GHC/Runtime/Eval.hs),
and [breakpoint metadata generation](../../compiler/GHC/StgToByteCode.hs).

### Composition of source equations

Agda defines multiplication by adding one row to the product for the
remaining rows. The recursive call decreases the first operand. Addition
uses its own structural recursion. M88 takes both equations from the
library's checked source. A helper must already have an independent
certificate before its caller can use it.
[Agda 2.8.0, builtin natural operations](https://agda.readthedocs.io/en/v2.8.0/language/built-ins.html#functions-on-natural-numbers)
and [termination checking](https://agda.readthedocs.io/en/v2.8.0/language/termination-checking.html#structural-recursion).

In the generated equation for `times (S n) m`, one closed family
application occurs inside another. GHC's ordinary family decidability
check excludes this form. The source checker can justify the equation
because the self call uses the original constructor field and the graph
of calls between distinct functions is acyclic. A handwritten version
of the nested family equation was checked with `UndecidableInstances`.
[GHC's decidability conditions](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html#decidability-of-type-synonym-instances)
and [the checked producer](../../compiler/GHC/Tc/Gen/SourceTable.hs).

The vector library uses GHC's existing type-operator syntax.
`NoStarIsType` makes `*` available as an operator in a type, and the
library gives it the same fixity as term multiplication.
[GHC 9.14.1, the kind Type and StarIsType](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/exts/poly_kinds.html#the-kind-type).

### Nested calls in retained operands

Agda's rule for a dependent application substitutes its argument into
the codomain. Apply the same rule recursively to the inner and outer
calls. In the fork each checked source head supplies the closed family
that denotes its result. For an overloaded operator, check the dictionary
at that occurrence before using the target function's equations.
[Agda 2.8.0, function application](https://agda.readthedocs.io/en/v2.8.0/language/function-types.html)
and [GHC dictionary selection in retained arguments](../../compiler/GHC/Tc/Gen/App.hs).

Agda substitutes local definitions during type checking and documents
that its let expressions do not introduce sharing. Haskell has a separate
runtime obligation. The checked Core operand keeps its original calls.
STG introduces a lazy binding for each nontrivial operand inside the
computation that uses it. For example, the first cell at length
`1 + rows * cols` can be returned before the product is demanded.
[Agda 2.8.0, let expressions](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-expressions),
[the ANF paper ledger](papers/README.md#anf-and-dependent-types-jfp-2022),
and [the Core to STG translation](../../compiler/GHC/CoreToStg.hs).

### Equality for lazy local definitions

Agda allows a nonrecursive let definition to be replaced by its RHS during
type checking. The fork expresses this conversion using a retained function.
For a local `cells = times rows cols`, the function takes `cells` and equality
evidence between `cells` and the product. The caller supplies the product once.
Substitution makes the equality reflexive. Passing the value lazily preserves
sharing in the Haskell program. Agda documents that its own let substitution
does not introduce sharing.
[Agda 2.8.0, local definitions](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-expressions)
and [the GHC elaboration](../../compiler/GHC/Tc/Gen/Expr.hs).

Agda checks the RHS at its declared or inferred type before entering the
body. Its local context stores the checked term and its type under the let
name. Variable lookup consults that context. This is the source precedent
for associating the checked RHS with one lexical binder.
[Agda 2.8.0, checking a let binding](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Term.hs#L1529-L1543)
and [Agda's let environment](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Monad/Context.hs#L387-L399).
See also [variable lookup](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Monad/Context.hs#L521-L534).

Agda permits several local definitions with individual signatures. Each
definition is available in the later RHSs and in the body. In this fork,
Haskell name resolution determines the dependency order, including forward
references. Each checked equality can then justify the next index without
evaluating either binding at runtime.
[Agda 2.8.0, local signatures and scope](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-expressions).

For a Haskell class method, the selected evidence determines the runtime
function. A local constraint can supply a different dictionary even at a
ground type. The solver already checks the identity of a dictionary used
in a retained argument. Ordinary let bindings can also succeed without
using any index equality. An optional refinement must preserve that case.
[Dictionary selection](../../compiler/GHC/Tc/Solver/Solve.hs),
[source method certificates](../../compiler/GHC/Tc/Gen/SourceTable.hs),
and [the OutsideIn(X) reading record](papers/README.md#outsideinx-jfp-2011).

### Instance evidence in a local definition

Agda resolves an instance argument from the declarations and the current
context. Its example of list membership passes the caller's equality
instance to the element comparison. This motivates inspecting the evidence
selected for the written operation. Haskell's `Num Nat` constraint can
likewise supply multiplication at the call site.
[Agda 2.8.0, instance arguments](https://agda.readthedocs.io/en/v2.8.0/language/instance-arguments.html).

OutsideIn(X) solves outer simple constraints before solving a nested
implication. GHC can therefore inspect the ordinary RHS dictionary when
it checks the constraints arising from a let body. The optional Given,
its use in committed evidence, the rewrite-rule checks and lazy dependent
application are design choices in this fork.
[OutsideIn(X) reading record](papers/README.md#outsideinx-jfp-2011)
and [the GHC implication solver](../../compiler/GHC/Tc/Solver/Solve.hs).

### Editor context and HIE encoding

Agda's editor can show the type of each local identifier at a goal. For
this fork, an editor needs the public Haskell binder and its inferred
type at its source location, including when elaboration introduces a
different internal binder.
[Agda 2.8.0, commands in the context of a goal](https://agda.readthedocs.io/en/v2.8.0/tools/emacs-mode.html#commands-in-context-of-a-goal).

GHC already records typed source information in extended interface
files. M93 follows the existing HIE type table and both GHC readers.
The retained forms carry the same fields as the compiler's existing
interface types. The HIE version identifies its binary payload.
[GHC's extended-interface options](../users_guide/separate_compilation.rst),
[HIE types and binary representation](../../compiler/GHC/Iface/Ext/Types.hs)
and [GHC interface types](../../compiler/GHC/Iface/Type.hs).

### Inferring an omitted index from a local definition

Agda supports omitting arguments that can be recovered from how a function
is used. Its local definitions can be expanded when checking a type. These
two rules motivate trying the checked definition of a vector length while
choosing an omitted tail index.
[Agda 2.8.0, implicit arguments](https://agda.readthedocs.io/en/v2.8.0/language/implicit-arguments.html)
and [local definitions](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-expressions).

The fork distinguishes choosing the argument from proving that it fits.
The checker retains the expression, its inferred type and its usage, then
checks subsumption at the selected domain. A class method from a local
dictionary can invalidate a candidate. A constructor equality obtained
from a pattern can justify the call independently of that dictionary.
[Cached application checking](../../compiler/GHC/Tc/Gen/App.hs) and
[solver provenance](#equality-for-lazy-local-definitions).

### Checking a list at a requested length

Agda's `fromList` builds a vector indexed by the input's own length.
Its clauses translate the empty list and cons directly to vector
constructors. The fork already has this operation in `sourceVector`.
`packList` stores the length and vector as two lazy fields of an existing
dependent package. A consumer can inspect the vector without first
counting the input.
[Agda standard library v2.1, Data.Vec.Base.fromList](https://github.com/agda/agda-stdlib/blob/97bc55e47367c562b3032bf104f2a00b19716f88/src/Data/Vec/Base.agda#L308-L310).

Idris2's `toVect` takes a requested natural and a list. It checks the
two spines together and returns a vector only when they end together.
M95 uses this contract for `fromListExact`. In lazy Haskell, the
success branch requires the list spine through its end. A mismatch
can discard an arbitrarily long remaining tail. The elements stay lazy.
[Idris2 v0.8.0, Data.Vect.toVect](https://github.com/idris-lang/Idris2/blob/15a3e4e70843f7a34100f6470c04b791330788df/libs/base/Data/Vect.idr#L320-L325).

Haskell's `vector-sized` also offers a checked conversion and a scoped
consumer. Its `fromList` constructs an underlying vector, then
`toSized` compares the stored length to the requested size. M95 works
directly on list cells to support early rejection.
[vector-sized v1.6.1, list conversions](https://github.com/expipiplus1/vector-sized/blob/0b864ce8957cb5c7bfb228145a7ed5f8cf2e735b/src/Data/Vector/Generic/Sized.hs#L1733-L1761)
and [stored length check](https://github.com/expipiplus1/vector-sized/blob/0b864ce8957cb5c7bfb228145a7ed5f8cf2e735b/src/Data/Vector/Generic/Sized.hs#L1810-L1830).

Agda checks patterns in clauses against the function telescope. Here a
retained constructor supplies evidence that refines later ordinary
domains. GHC then uses its ordinary pattern checker and its existing
row-specific Core cases. This preserves Haskell's clause order, warnings
and demand on fields. The existing source-family certificate associates
a polymorphic source function with its family. Both applications must
carry the same erased type arguments.
[Agda 2.8.0, function definitions](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html)
and [implicit arguments](https://agda.readthedocs.io/en/v2.8.0/language/implicit-arguments.html).

### Partial constructor callbacks at a checked input

Agda uses the expected datatype parameters in a branch that handles
fully applied constructors.
It retains checked arguments and compares the actual application result
with its expected type. These steps motivate using an available callback
input before choosing a constructor's erased index. The reviewed Agda
v2.8.0 application source has file blob
`82ba94ac864d19b947d4c92b9af252b0fe81347a`.
[`checkConstructorApplication`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L1030-L1071),
[`addCheckedArgs`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L833-L844)
and [`checkHeadApplication`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Application.hs#L449-L465).

In the fork the partially applied constructor can get its missing index
from the input to the callback. GHC gives data constructor fields linear
arrows. Its application
subsumption treats a remaining linear field arrow as multiplicity
polymorphic. M96 preserves the original constructor head and field arrows
so that this existing check still runs. It selects immutable erased
arguments from callback inputs before output candidates. Selection does
not prove equality between conflicting indices. The ordinary final check
must use valid equality evidence or reject the callback.
[GHC's Note [Typechecking data constructors]](../../compiler/GHC/Tc/Gen/Head.hs),
[`tcSubTypeApp` and constructor subsumption](../../compiler/GHC/Tc/Utils/Unify.hs)
and [Note [Retained choices for partial constructors]](../../compiler/GHC/Tc/Gen/App.hs).

The higher-order call retains each checked expression and its usage in the
existing argument cache. The input `fromListExact n xs` determines the
callback index in `VCons x <$> fromListExact n xs`. The rule does not assign
native indices to mutable type variables or infer arbitrary callbacks.
Agda's contextual metavariables and evaluation rules do not establish
these Haskell scope, multiplicity or laziness properties.

### Reducing a known constructor case in native Core

Agda's compiled-clause matcher replaces a matched constructor argument
with its fields and applies the resulting substitution to the selected
clause. Its compiler distinguishes a runtime field from an erased
argument. In this fork the field can be a Haskell thunk whose type
mentions a runtime length. Substitution must preserve both its value
and the runtime binders used by its type.
[Agda compiled-clause matching at `7273757e`](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/TypeChecking/CompiledClause/Match.hs#L138-L164) and [Agda 2.8.0 run-time irrelevance](https://agda.readthedocs.io/en/v2.8.0/language/runtime-irrelevance.html).

The existing GHC simplifier binds the constructor's fields, preserving
sharing and any evaluation required to construct the value. For a strict
field, choosing the right branch alone can hide an exception. The
restricted path needs the original force in this case. Its dependency
check also includes free runtime variables in types and coercions.
[GHC's `knownCon`](../../compiler/GHC/Core/Opt/Simplify/Iteration.hs),
[Haskell 2010 pattern matching, section 3.17.2](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html),
and [Haskell evaluation provenance](#haskell-evaluation-provenance).

### The length of zip and demand in a case tree

A proposed length for zipping two ordinary lists tests the first spine,
then tests the second spine only if the first has a cell. Agda's `max`
example shows why a source equation need not reduce when its first
argument is unknown: the compiled tree must make its first split.
Its vector `zipWith` example permits fewer rows because its inputs
already have the same length. Ordinary lists need both mismatched
shapes. [Agda 2.8.0 case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees)
and [multiple-argument coverage](https://agda.readthedocs.io/en/v2.8.0/language/coverage-checking.html#matching-on-multiple-arguments).

GHC's `zip` and `zipWith` distinguish an empty first list from an
unknown first list. In particular, `zip [] undefined` returns `[]`;
evaluating `zip undefined []` raises the exception. The list library
documents the same demand issue for fusion. A type-level reduction
must preserve that choice of discriminator. The equation for an empty
second list can require a known cons in the first position. Neither
coverage nor structural recursion makes all Haskell spines finite.
[GHC.Internal.List, Note [Fusion for foldrN] and zip](../../libraries/ghc-internal/src/GHC/Internal/List.hs)
and [Agda 2.8.0 structural recursion](https://agda.readthedocs.io/en/v2.8.0/language/termination-checking.html#primitive-recursion).

## Executing closed calls and reducing open calls

The 2026-11-18 review returned to Serokell's examples. A closed call to
`length` can execute under the compiler's interpreter. An open call
to `id` must preserve a variable across reduction. The fork's earlier
translation of source clauses into closed families supplies useful equations
for R13. The two evaluator cards specify further mechanisms. [Closed term
evaluator](https://ghc.serokell.io/dh#closed-term-evaluator) and [open term
evaluator](https://ghc.serokell.io/dh#open-term-evaluator).

The open type-functions proposal (#509) describes qualified, loadable,
fully instantiated calls. Its predecessor implemented a `TYPEFUN` pragma
for closed families. At commit `7e5ecbd2`, that prototype calls
`hscCompileCoreExpr` on a term identifier, applies the resulting value
with `unsafeCoerce` and constructs a nominal universal coercion in the
rewriter. We reviewed the changes to lookup, loading and rewriting. We
did not build the historical compiler. The current GHC splice path gives
a precedent for linking and recording the actual dependencies. For R15
we can elaborate a completed call to a literal before it enters Core.
[Proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509),
[tracking issue #19634](https://gitlab.haskell.org/ghc/ghc/-/issues/19634),
[pinned prototype](https://gitlab.haskell.org/Haskell-mouse/ghc/-/commit/7e5ecbd2d0aca3ec94cbd3eae64c79e36857b096)
and [GHC's splice path](../../compiler/GHC/Tc/Gen/Splice.hs).

The description of open GHC issue #27413 asks how to compute weak-head
normal form in the presence of metavariable holes. It considers typed
Haskell syntax as an input representation. Agda's reducer records blockers
so the checker can retry when it learns more. A rigid variable can remain
unknown throughout checking. GHC must also account for lazy and strict
Haskell patterns and for the evidence that Core Lint can verify.
[GHC #27413](https://gitlab.haskell.org/ghc/ghc/-/issues/27413),
[Agda reduction](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/TypeChecking/Reduce.hs#L616)
and [Agda blockers](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/Syntax/Internal/Blockers.hs#L26).

Safe Agda checks imports transitively and restricts features that can
invalidate its guarantees, including unchecked termination assertions.
GHC has a different contract for compile-time execution. Its annotation
checker rejects running annotation code in a Safe Haskell module. R15
uses that boundary for executing an imported ordinary function. It also
marks a module that uses this path unsafe for Safe Haskell inference. A
function's Haskell type gives no termination proof. [Safe Agda](https://agda.readthedocs.io/en/v2.8.0/language/safe-agda.html),
[Safe Haskell](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/safe_haskell.html)
and [GHC's annotation checker](../../compiler/GHC/Tc/Gen/Annotation.hs).

For M100, the consumer has a fixed type variable and the producer has
a checked Core unfolding in its interface. This makes Core a usable input
for the first open source example. Agda's reducer distinguishes a variable
head from an expression waiting for more information. Its clause matcher
can suspend when it cannot inspect the next argument. The GHC evaluator
keeps a case on an unknown scrutinee suspended, including a case with one
default branch. GHC uses its own substitution for type arguments and
records the fingerprints of every imported definition used for reduction.
These checks preserve lexical identity and separate compilation.
[Agda's reducer](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/TypeChecking/Reduce.hs#L616),
[Agda clause matching](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/TypeChecking/CompiledClause/Match.hs#L84),
[GHC's open-term evaluator](../../compiler/GHC/Core/OpenWhnf.hs) and
[interface usage recording](../../compiler/GHC/Iface/Make.hs).

### Characters in closed calls

Proposal #509 gives equality witnesses for `Data.Char.toUpper 'x'` and
`Data.Char.isDigit 'x'`. It also gives an open call that could reduce
after the compiler learns the character. M101 implements the closed
examples. Agda exposes `primToUpper` and `primIsDigit` as character
primitives. Its manual specifies their behavior through the corresponding
Haskell `Data.Char` functions. GHC can call those ordinary functions
directly through the R15 interpreter. At the type boundary it can reuse
`isCharLitTy`, `mkCharLitTy` and the existing promoted Boolean constructors.
[Proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509)
and [Agda 2.8.0 character primitives](https://agda.readthedocs.io/en/v2.8.0/language/built-ins.html#characters).

### Instantiating an overloaded closed function

Proposal #509 uses `sum` as an example of solving constraints before
executing a function. In this checkout, its definition is a `Foldable`
class method with a `Num` constraint. The list instance delegates to
`GHC.Internal.List.sum`. An evaluator needs the selected implementations
of both classes. Agda's instance arguments also make the selected evidence
part of the elaborated application. For GHC, ordinary expression checking
and instance resolution can construct that application. Its Core must be
available to the interpreter before execution.
[Proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509),
[Foldable method and list instance](../../libraries/ghc-internal/src/GHC/Internal/Data/Foldable.hs),
[GHC's staged expression checker](../../compiler/GHC/Tc/Gen/Splice.hs)
and [Agda 2.8.0 instance arguments](https://agda.readthedocs.io/en/v2.8.0/language/instance-arguments.html).

### Strict and lazy open projections

Johann and Voigtländer give the polymorphic type of `seq` and show how a
strict first argument changes the behavior of a function that returns its
second argument. This matters directly to M102. The lazy and strict
versions of `second :: a -> b -> b` have identical signatures. The
strict version must evaluate its first argument to weak-head normal form
before it can return the second.
The evaluator follows the imported body and preserves the resulting Core
case. Agda's case-tree examples provide a related criterion for blocking
on an unknown input.
[Johann and Voigtländer, POPL 2004](https://janis-voigtlaender.eu/JV04.html),
[Agda 2.8.0 case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees)
and [R16 source checks](roadmap/R16-open-term-evaluator.md#selecting-between-two-type-variables).

### Retrying a check during inference

Agda stores a constraint with its blocker and its checking context. Its
constraint scheduler can wake a blocked constraint when a problem is
solved. For the GHC partial-signature experiment, the binding group owns
the inference variables. Its existing solver offers a place to retry
after solving ordinary constraints and before generalization. An inferred
literal can then be checked by the closed evaluator. A universally
quantified input in an exported signature has a different lifetime.
[Agda's constraint insertion](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/TypeChecking/Monad/Constraints.hs#L232-L259),
[Agda's wakeup](https://github.com/agda/agda/blob/7273757e5ee247d321270ef76f792c6156613b7c/src/full/Agda/TypeChecking/Monad/Constraints.hs#L300-L322)
and [GHC inference](../../compiler/GHC/Tc/Solver.hs).

### Reifying closed list results

Proposal #509 treats the promotability of a list as a property of its
element type. The `SingKind` class in `singletons` associates each supported
kind with a term representation and supplies conversions in both directions.
The compiler has the element kind when it elaborates a closed call, even
when the result list is empty. This gives it the information needed to
select a scalar conversion and reconstruct the promoted list. An ordinary
user `Show` instance may choose any text for that list, so its output
cannot establish the type index. The built-in dictionary for the element
type already supplies `showList`, including the escape rules for strings.
[The Show class and Char instance](../../libraries/ghc-internal/src/GHC/Internal/Show.hs),
[proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509)
and [the singletons 3.0.4 API](https://hackage-content.haskell.org/package/singletons-3.0.4/docs/Data-Singletons.html#t:SingKind).

### Checking a reduction across an interface

Weirich, Voizard, de Amorim and Eisenberg distinguish computational
reduction from congruence in the explicit calculus DC. Eisenberg's
historical Core Lint checks the kind of a promoted argument before
substituting it into a dependent result. The fork needs a check for both
endpoints when it adds a nominal proof for a selected case branch.
For GHC's built-in family rules, Core Lint consults the rule's proof
checker. Agda's case trees explain
why an unknown scrutinee can block reduction even with a default branch.
[DC specification](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf),
[historical Core Lint](https://github.com/goldfirere/ghc/blob/b64ef80f8002727422c05092f429c38d0527ed0b/compiler/coreSyn/CoreLint.lhs#L527-L544),
[GHC literal axioms](../../compiler/GHC/Builtin/WiredIn/TypeLits.hs)
and [Agda 2.8.0 case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees).

The compiler's `mkFailurePair` explains the unboxed-unit call seen in
the R16 character experiment. The desugarer can share a pattern-match
continuation as a local function. Observing its result must preserve
the evaluation of its unlifted argument.
[Desugarer and design note](../../compiler/GHC/HsToCore/Utils.hs)
and [R16 case experiment](roadmap/R16-open-term-evaluator.md#an-equality-that-crosses-a-module-boundary).

### Evaluation order for nested closed calls

Draft proposal #509 specifies argument normalization before execution of
the function. For R15 this makes an exception in the inner list visible
at the inner call, even when the outer Haskell function ignores its
input. The inner call has its own inferred kind. The fork uses that kind
to specialize the outer function. It checks those kinds across the chain before
executing the prepared calls. Agda's case trees address a distinct
question on R16, where a scrutinee can remain unknown.
[Proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509),
[GHC's bidirectional type checker](../../compiler/GHC/Tc/Gen/HsType.hs)
and [Agda 2.8.0 case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees).

### Character cases in imported Core

GHC's boxed `Char` has a strict primitive field. The desugared worker
first matches the wired-in constructor and then selects an alternative
on the primitive literal. GHCi can annotate the same expression with
a debugger breakpoint. The open observer needs the expression under
that annotation. The runtime debugger uses the original Core.
[GHC wired-in types](../../compiler/GHC/Builtin/WiredIn/Types.hs),
[breakpoint insertion](../../compiler/GHC/Driver/Config/HsToCore/Ticks.hs)
and [GHC's interface expression reader](../../compiler/GHC/IfaceToCore.hs).

### Passing a computed result to another ordinary function

The `SingKind` class in `singletons` selects demotion from the known kind.
R15 uses the same criterion after checking a nested call. Its input is a
promoted Core type. For a nullary promoted Boolean, GHC can recover the
originating data constructor through `isPromotedDataCon_maybe`. Numeric
and character literals use GHC's target-aware literal builders.
[Singletons 3.0.4](https://hackage-content.haskell.org/package/singletons-3.0.4/docs/Data-Singletons.html#t:SingKind),
[GHC type constructors](../../compiler/GHC/Core/TyCon.hs)
and [Core literal builders](../../compiler/GHC/Core/Make.hs).

### Evidence for the finite character fragment

A verified finite table could reuse GHC's closed-family axiom machinery.
GHC checks earlier incompatible equations before selecting the default.
The consumer already knows how to serialize and lint a family reduction.
The fork's source-table machinery links an ordinary definition to its
generated family. A design that uses it for characters must certify the
complete definition and its aliases and track the worker's declaration.
[Closed-family reduction](../../compiler/GHC/Core/FamInstEnv.hs),
[source-table construction](../../compiler/GHC/Tc/Gen/SourceTable.hs)
and [interface decoding](../../compiler/GHC/IfaceToCore.hs).

The [Unicode experiment](examples/certified-character-case/README.md)
derives the ordinary family from pre-optimization Core. The client can
reduce its exported equality with only the producer interfaces available.
[Structural check](../../compiler/GHC/Core/CharCase.hs)
and [exported signature](examples/certified-character-case/P3.hs).

### Literals as inputs to ordinary functions in types

Agda gives a natural literal its built-in natural-number type by default.
A program can select `fromNat` through instance arguments. GHC already
has a type-level natural literal and can use its kind to select the
ordinary `Integral Natural` instance for `Prelude.odd 5`. R15 checks
promoted lists and Boolean constructors through the same type checker as
other Haskell type syntax. For a bare promoted empty list, the function
domain can supply the missing element kind.
[Agda 2.8.0 literal overloading](https://agda.readthedocs.io/en/v2.8.0/language/literal-overloading.html#natural-numbers),
[GHC's type-level literals](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_literals.html)
and [GHC's source type checker](../../compiler/GHC/Tc/Gen/HsType.hs).

### Publishing the character worker's equality

Agda compiles ordered clauses to a case tree. A rigid scrutinee can
prevent the tree from selecting a branch. For a finite strict character
case with an identity default, a GHC closed family can express the same
condition through its apartness check. The generated equations must use
the worker's checked binding and preserve its literal alternatives. The
declaration's identity matters when a client reads the equation through
a second module's signature.
[Agda 2.8.0 case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees),
[GHC's closed-family axioms](../../compiler/GHC/Core/FamInstEnv.hs)
and [the exported equality in proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509).

The public `Data.Char` module re-exports `GHC.Internal.Unicode.toUpper`.
That definition names the imported `toSimpleUpperCase` worker. An exact
term alias can therefore publish a forwarding equation. The alias's
equation refers to the worker's family by name. The checked declaration
supplies its identity and tracks edits to the worker. In this fork,
Hadrian enables publication when its in-tree compiler builds those two
modules. A standalone Cabal build with this compiler can opt in by setting
`ghc-options: -XExperimentalDependentTypes` for `ghc-internal` in the
project file. That Cabal setting applies to the library component.
[Public export](../../libraries/base/src/Data/Char.hs),
[Unicode forwarding definition](../../libraries/ghc-internal/src/GHC/Internal/Unicode.hs),
[generated worker](../../libraries/ghc-internal/src/GHC/Internal/Unicode/Char/UnicodeData/SimpleUpperCaseMapping.hs)
and [Hadrian package settings](../../hadrian/src/Settings/Packages.hs).

For a mixed call such as `toLower (toUpper 'x')`, GHC's bounded
family-only rewriter uses checked nominal axioms to determine the
ground operand. Agda's case-tree account explains why an unknown
scrutinee can still leave nested applications blocked. GHC's closed
interpreter needs a concrete value at that boundary. Each side
records the dependency that supplied its result.
[Solver's family-only reduction](../../compiler/GHC/Tc/Solver.hs),
[source owner and evidence checks](../../compiler/GHC/Tc/Gen/SourceTable.hs)
and [Agda case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees).

### Saturated closed calls and open pair projections

The type-functions draft at `f0b6af30a6e1` requires full application,
instance resolution, and normalization of the operands before execution.
For R15, GHC matches the declared inputs under one substitution. A later
operand can fix the element kind of an earlier empty list. The fork checks
arguments in source order when it runs their prepared computations.
[Type-functions proposal #509](https://github.com/ghc-proposals/ghc-proposals/pull/509)
and [GHC's one-way type matching](../../compiler/GHC/Core/Unify.hs).

Agda's case trees establish when evaluation needs a constructor. For R16,
the actual Haskell definitions of `fst` and `snd` determine which pair
field the observer demands. It can also encounter a rigid variable of pair
kind. That variable supplies no information about either field. This
choice preserves the Haskell case's strictness on the outer pair.
[Agda 2.8.0 case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees)
and [GHC's boxed pair projections](../../libraries/ghc-internal/src/GHC/Internal/Data/Tuple.hs).

### A local term definition used by a type

Serokell's declaration analysis describes how GHC groups declarations and
which dependencies need a signature or a definition. R21 asks GHC to
extend this ordering across the term and type levels. An open call such
as `keep a` needs the definition of `keep`. The closed-call interpreter
also needs code it can load. Draft #509 expressly requires that the
function come from another module. [Serokell's declaration analysis](https://serokell.io/blog/dependency-analysis-haskell),
[R21](https://ghc.serokell.io/dh#dependency-analysis),
[type-functions draft](https://github.com/ghc-proposals/ghc-proposals/pull/509)
and [the fork's two-order source check](roadmap/R21-dependency-analysis.md).

### Returning a pair from a closed call

The type-functions draft lists a pair of promotable fields among its
supported types. It also specifies that the compiler force the result
before promoting it. In this fork, an ordinary typed Core case observes
the result once and uses its two field binders. Each leaf uses the
existing scalar or list representation. A client-defined method for
showing the whole pair would allow the client to change its type-level
value. The compiler therefore uses only the trusted leaf dictionaries.
[Supported results and evaluation order in draft #509](https://github.com/Haskell-mouse/ghc-proposals/blob/f0b6af30a6e15545606195eb72c2981ea91aa326/proposals/0000-type-functions.rst#L109-L142),
[GHC's promoted pair constructors](../../compiler/GHC/Builtin/WiredIn/Types.hs)
and [the interpreter check](roadmap/R15-closed-term-evaluator.md#returning-a-pair).

### Maybe and Either in the closed evaluator

Draft #509 includes `Maybe a` and `Either a b` when their parameters
are promotable. Agda's `Maybe` and its standard library sum distinguish
the constructor from its arguments. The nullary case of `Maybe` has
no payload. R15 uses the corresponding GHC data constructors for each
case alternative. It checks their instantiated field types and calls
`promoteDataCon` when decoding the selected alternative. Its tagged
string representation is local to the compiler and interpreter.
[Type-functions draft](https://github.com/Haskell-mouse/ghc-proposals/blob/f0b6af30a6e15545606195eb72c2981ea91aa326/proposals/0000-type-functions.rst#L122-L141),
[Agda 2.8.0 Maybe](https://github.com/agda/agda/blob/v2.8.0/src/data/lib/prim/Agda/Builtin/Maybe.agda),
[Agda standard library 2.3 sum](https://github.com/agda/agda-stdlib/blob/v2.3/src/Data/Sum/Base.agda#L25-L38)
and [GHC's result decoder](../../compiler/GHC/Tc/Gen/Splice.hs).

### Projections of an unknown pair in an exported equality

Agda's record documentation gives separate field types to the two pair
projections. Both projections take a pair variable. R16 uses the two
field parameters of GHC's boxed tuple and certifies only a case whose
body returns the chosen binder unchanged. The nominal equation can
remain in a signature until the caller supplies a pair constructor.
The compiler uses GHC's linker plan to preserve the existing behavior
of closed calls with executable operands and later rewrite rules.
[Agda 2.8.0 pair projections](https://agda.readthedocs.io/en/v2.8.0/language/record-types.html#example-the-pair-type-constructor),
[Serokell open evaluator](https://ghc.serokell.io/dh#open-term-evaluator),
[GHC's tuple definitions](../../libraries/ghc-internal/src/GHC/Internal/Data/Tuple.hs)
and [GHC's link dependency planner](../../compiler/GHC/Linker/Deps.hs).

### Running checkers at the dependent boundary

The [boundary experiment](BOUNDARY-EVIDENCE.md) uses an explicit witness
for a concrete obligation. Agda's decidability library connects the
result of a decision with its proof. The fork can use existing GHC
plugins to return ordinary Core evidence. Thoralf gives the contrasting
case in which an SMT translation is part of the trusted reasoning.
[Agda's decision and witness operations](https://agda.github.io/agda-stdlib/v2.3/Relation.Nullary.Decidable.Core.html),
[GHC's plugin evidence contract](../users_guide/extending_ghc.rst#typechecker-plugins)
and [Thoralf implementation and paper](#thoralf-and-ghc-plugin-interfaces).

QuickCheck tests generated cases. QuickSpec infers candidate equations
from observations. Both help find counterexamples and choose properties
for further checking. Liquid Haskell's refinement reflection relates
function definitions to verification conditions. Its treatment of lazy
Haskell explains why assumptions about which values terminate matter for
a universal claim. These are different guarantees a boundary library can
present to its caller.
[QuickCheck](https://hackage-content.haskell.org/package/QuickCheck),
[QuickSpec paper](https://smallbone.se/papers/quickspec.pdf),
[Refinement Reflection](https://arxiv.org/abs/1711.03842)
and [Refinement Types for Haskell](https://simon.peytonjones.org/assets/pdfs/refinement-types-for-haskell.pdf).

For runtime values, Dagand, Tabareau and Tanter give a constructive Coq
account of partial conversions between ordinary and indexed values.
This provides prior art for the failure branch of `fromListExact` and
for a boundary that makes the checked invariant visible in its result.
[Foundations of dependent interoperability, JFP 2018](https://doi.org/10.1017/S0956796818000011).

### Closed constructor arguments and composition of strict pair cases

Agda's standard library defines `concat` on lists of lists and
`catMaybes` on lists of optional values. R15 uses those two examples
with the corresponding ordinary Haskell functions. GHC's existing
promoted list, tuple, and data constructors check the source arguments.
The fork also needs to reconstruct a finite result at its checked kind
after the interpreter runs.
[Agda standard library 2.3, lists](https://agda.github.io/agda-stdlib/v2.3/Data.List.Base.html)
and [GHC's list and sum checks](../../testsuite/tests/dependent/should_run/NativeClosedCompoundLists.hs).

Agda represents a projection of a neutral term as an elimination. Its
record rules also permit eta equality for eligible records. The latter
rule cannot transfer to lazy boxed pairs in Haskell. Forcing an unknown
pair can diverge. Building a pair of its two unevaluated projections
always produces a constructor. R16 certifies each strict case from the
Haskell worker's typed Core. The separate composition experiment asks
which field two successive strict cases select.
[Agda 2.8.0 internal syntax](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/Internal.hs),
[Agda record eta](https://agda.readthedocs.io/en/v2.8.0/language/record-types.html#eta-expansion)
and [GHC’s pair definitions](../../libraries/ghc-internal/src/GHC/Internal/Data/Tuple.hs).

### A nominal law for two strict pair cases

System FC uses explicit coercions to witness equal types. The closed
type families calculus specifies when a branch can supply such a
coercion. For an unknown `p`, the constructor branch for `swap` cannot
match. The separate law that two strict cases select the same field is
a design choice in this fork. Its executable checks cover the branch
shapes, nominal roles, interface substitution, and source rule
dependencies. A metatheoretic account of this addition remains open.
[System F with type equality coercions, TLDI 2007](https://www.microsoft.com/en-us/research/publication/system-f-with-type-equality-coercions/),
[Closed Type Families with Overlapping Equations, POPL 2014](https://simon.peytonjones.org/assets/pdfs/closed-type-families.pdf)
and [local Core contract](CORE.md#nominal-evidence-for-a-composition-of-strict-pair-cases).

### Reifying enumerations and tracing pair fields

Agda's datatype rules introduce a finite set of named constructors,
including nullary constructors. R15 uses GHC's existing promoted
constructors for that role. It checks source visibility separately.
GHC already records implicit uses of constructors for built-in
`DataToTag` dictionaries. The closed evaluator reuses that mechanism
when a constructor is visible only through a re-export.
[Agda 2.8.0 simple datatypes](https://agda.readthedocs.io/en/v2.8.0/language/data-types.html#simple-datatypes),
[GHC DataKinds](../users_guide/exts/data_kinds.rst)
and [GHC's DataToTag instance solver](../../compiler/GHC/Tc/Instance/Class.hs).

Agda gives each field of a pair a projection and defines `swap` in
its standard library. The local R16 proof follows the selected
positions through strict Haskell constructor cases. A common inner
scrutinee can be opaque or bottom. This choice preserves the demand
of both expressions and limits their dependencies on ordinary source rules.
[Agda 2.8.0 pair projections](https://agda.readthedocs.io/en/v2.8.0/language/record-types.html#example-the-pair-type-constructor),
[Agda standard library 2.3 `Data.Product.Base`](https://agda.github.io/agda-stdlib/v2.3/Data.Product.Base.html)
and [the local Core rule](../../compiler/GHC/Builtin/CoercionRules.hs).

### Local source definitions and recursive constructor values

Serokell distinguishes a reference to a declaration's signature from a
dependency on its definition. Agda also distinguishes a forward type
declaration from its defining clauses. M126 needs the checked body to
determine which rigid argument a term selects. A complete signature
permits its independent definition to be checked first within a Haskell
declaration group. Top-level splices delimit those groups.
[Serokell's declaration analysis](https://serokell.io/blog/dependency-analysis-haskell),
[Agda's forward declarations](https://agda.readthedocs.io/en/v2.8.0/language/mutual-recursion.html#forward-declaration)
and [fork source check](roadmap/R21-dependency-analysis.md).

Agda's datatype examples include a binary tree with natural numbers at
the leaves. Eisenberg and Weirich use singleton constructors to connect
promoted and ordinary data. M127 uses the same constructor correspondence
for an ordinary Haskell tree. GHC supplies the checked field kinds and
constructor identities. User functions operate on the ordinary value.
[Agda's example datatypes](https://agda.readthedocs.io/en/v2.8.0/language/data-types.html#example-datatypes),
[Dependently Typed Programming with Singletons](https://richarde.dev/papers/2012/singletons/paper.pdf)
and [fork tree example](examples/closed-evaluation/FiniteTree.hs).

### Scheduling helpers and reifying an indexed result

The dependency graph for a type-level call includes the definition
of its head. Serokell distinguishes this edge from an ordinary reference
that needs only a type signature. The fork schedules a local chain in the
renamer's value-binding order. It infers unsigned functions with GHC's
existing typechecker. When an unqualified term name is used, the lookup
rules come from the accepted dependent-type design. Explicitly bound type
variables retain their scope.
[Serokell on dependency analysis](https://serokell.io/blog/dependency-analysis-haskell),
[proposal 378 on term variables in types](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html#term-variables-in-types)
and [fork scheduling examples](roadmap/R21-dependency-analysis.md#m128-following-a-local-dependency-chain).

Agda's vector has a shared element parameter and a length index that
differs by constructor. The Haskell example uses a Boolean index so that
the result arguments determine all constructor parameters. GHC already
separates the source type of a constructor from the representation type
of its worker. M129 uses that distinction when reifying a closed result.
Agda's vector cons has a predecessor index bound at the constructor;
recovering such an index is a further step in GHC.
[Agda indexed datatypes](https://agda.readthedocs.io/en/v2.8.0/language/data-types.html#indexed-datatypes),
[GHC constructor representation](../../compiler/GHC/Core/DataCon.hs)
and [source GADT](examples/closed-evaluation/IndexedMark.hs).

### Recovering a vector index and observing a recursive group

Agda's vector cons binds the predecessor locally. Its result has
successor length. M130 uses the same relationship for an ordinary
Haskell GADT after type erasure. The worker's Core case already has the
nominal equality from the result to the constructor's index. GHC's
coercion selector projects it and its lifting operation relates the
field types. The GADT's public constructor still determines which
DataKinds value the client receives.
[Agda indexed vector](https://agda.readthedocs.io/en/v2.8.0/language/data-types.html#indexed-datatypes),
[GHC coercions](../../compiler/GHC/Core/Coercion.hs)
and [the local coercion endpoints](CORE.md#nominal-evidence-for-constructor-local-indices).

The Haskell Report infers types for mutually dependent value bindings
as a group. Agda also groups mutual definitions and separately checks
termination. M131 uses GHC's own group, including its checked type
inference. Haskell's lazy evaluation can finish one observation of a
group containing a divergent binding. The open evaluator's step limit
handles calls that continue to demand it. Weirich, Voizard, de Amorim and
Eisenberg study equality and nontermination in their specification for
Dependent Haskell.
[Haskell Report, section 4.5.1](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html),
[Agda's mutual definitions](https://agda.readthedocs.io/en/v2.8.0/language/mutual-recursion.html),
[Agda's termination checker](https://agda.readthedocs.io/en/v2.8.0/language/termination-checking.html)
and [A Specification for Dependent Types in Haskell](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf).

### Packing an index, matching a constructor, and ordering declarations

Agda's dependent pair stores a first component and a second component
whose type can mention the first. The ordinary Haskell package used here
stores a singleton and a vector indexed by the same variable. DataKinds
records the variable's instantiation in a checked constructor application.
M132 uses that instantiation when it passes the package to an ordinary
consumer. M135 inspects a singleton returned by a producer and validates
the accompanying indexed payload before constructing a promoted value.
[Agda dependent products](https://agda.github.io/agda-stdlib/v2.3/Data.Product.Base.html),
[Eisenberg and Weirich on singletons](https://richarde.dev/papers/2012/singletons/paper.pdf)
and [the local package](examples/closed-evaluation/ExistentialVec.hs).

Agda's pattern clauses select an equation once the required constructor
is known. For M133 the Haskell case inspects its scrutinee. An ordinary
lazy field stays suspended until the selected branch demands it. GHC's
Core alternative gives both the constructor and the order of the field
binders. [Agda pattern matching](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html)
and [Core observer](../../compiler/GHC/Core/OpenWhnf.hs).

Serokell describes how GHC groups top-level declarations by dependency.
Agda's interleaved mutual blocks show a related reason to distinguish
introducing a datatype from checking a use of its constructors. M134 uses
Haskell's existing declaration groups. A local type can become ready
after GHC checks a function it mentions. The source needs no new block
syntax. [Serokell's dependency analysis](https://serokell.io/blog/dependency-analysis-haskell),
[Agda's interleaved declarations](https://agda.readthedocs.io/en/v2.8.0/language/mutual-recursion.html#interleaved-mutual-blocks)
and [GHC's declaration scheduler](../../compiler/GHC/Tc/Module.hs).

### Observing a composition and returning an open constructor

In Haskell a constructor pattern inspects the outer value. A field
matched by a wildcard can stay unevaluated. M136 passes an ordinary
nested application directly to its ordinary consumer so these rules
also determine demand during compilation. Agda's reflection API
distinguishes reducing a term to weak head normal form from normalizing
it. M137 makes the corresponding distinction in the open observer:
a case only needs the constructor tag, while returning a complete
promoted value requires observing its fields. The fork bounds that
second operation because ordinary Haskell fields can diverge.
[Haskell Report, section 3.17](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html),
[Agda reflection](https://agda.readthedocs.io/en/v2.8.0/language/reflection.html)
and [Serokell open evaluator](https://ghc.serokell.io/dh#open-term-evaluator).

Stock deriving generates instances from datatype declarations. Derived
contexts can depend on each other. Haskell module defaults determine
how eligible ambiguous constraints are resolved. M138 uses GHC's own
deriving and defaulting passes before checking the local function that
a type mentions. GHC's deriving strategies also support a local class
with a default method in this example.
[Haskell derived instances](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html),
[Haskell default declarations, section 4.3.4](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html)
and [GHC deriving strategies](../../docs/users_guide/exts/deriving_strategies.rst).

### Checking a closed call against a known result kind

Serokell's closed-evaluator example uses the length of a literal list
as a vector index. In the installed Prelude, `length` returns `Int`.
`fromIntegral` has a separate result parameter subject to `Num`.
M139 uses the `Nat` kind in the vector declaration to choose that
parameter. Agda also uses a declared type when inserting implicit
arguments. The Haskell Report specifies numeric defaulting for
unresolved overloading. M139 uses an already known type and runs
the ordinary GHC instance solver. An ambiguous result still produces
an error. [Serokell closed evaluator](https://ghc.serokell.io/dh#closed-term-evaluator),
[Agda implicit arguments](https://agda.readthedocs.io/en/v2.8.0/language/implicit-arguments.html),
[Haskell Report, section 4.3.4](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html),
[Foldable.length](../../libraries/ghc-internal/src/GHC/Internal/Data/Foldable.hs),
[fromIntegral](../../libraries/ghc-internal/src/GHC/Internal/Real.hs)
and [Nat](../../libraries/ghc-internal/src/GHC/Internal/TypeNats.hs).

### Inferring nested applications and exporting pair equations

Agda inserts implicit arguments during elaboration. A type or another
argument can determine them. M140 uses the same source of information
for the type parameters of a nested ordinary Haskell function. The
enclosing declaration, an expected result or a literal sibling can
determine the target of `fromIntegral`. Haskell's ordinary instance
solver must then discharge the constraints.
[Agda implicit arguments](https://agda.readthedocs.io/en/v2.8.0/language/implicit-arguments.html)
and [closed source checks](roadmap/R15-closed-term-evaluator.md#m140-checking-a-nested-conversion).

GHC proposal 242 distinguishes the matchability required for inverse
decomposition from the matchability of an ordinary function. It lets the
solver determine matchability before defaulting and distinguishes defaults
for type-level and term-level arrows. M141 recognizes one case in which
an argument supplies the kind of a later argument. That later argument
can determine the written arrow. If it leaves the choice open, the fork
keeps its previous U default for this source position. A regression
compares the result with explicit U using `Refl`.
[Proposal 242](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html)
and [checked declarations](roadmap/R23-unsaturated-type-families.md#m141-inferring-an-omitted-arrow-in-a-dependent-argument).

Agda compiles pattern matching to case trees. A clause can reduce when
the relevant constructor is known. In Haskell a pair pattern forces
the pair constructor, and either field can still be bottom. M142
exports a constructor equation from a strict pair case that returns
the selected lazy fields. The checker inspects the actual worker before
publishing the equation. [Agda case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees),
[Haskell pattern semantics, section 3.17](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html)
and [ordinary pair exporter](roadmap/R16-open-term-evaluator.md#m142-exporting-a-swap-from-an-ordinary-module).

GHC stores a fingerprint of the settings that affect each home module.
M142 uses that mechanism to recheck ordinary home source when upgrading
from a compiler that could not publish pair equations there. A binary
reader can still load the older interface. Reusing the old fingerprint
in an incremental build left the unchanged worker without its equation
even though the same source compiled in a fresh directory.
[Interface recompilation](../../compiler/GHC/Iface/Recomp.hs),
[fingerprints](../../compiler/GHC/Iface/Recomp/Flags.hs)
and [two-compiler reproduction](examples/open-evaluation/check-pair-upgrade.sh).

M143 shares a metavariable between the argument and result kinds of
ordinary calls in the same expression. This uses the type inferred for
one argument to instantiate an implicit parameter of another. Agda's
account of omitted arguments gives the corresponding elaboration
principle. Haskell still resolves each ordinary class constraint before
the call can run. [Agda implicit arguments](https://agda.readthedocs.io/en/v2.8.0/language/implicit-arguments.html)
and [computed operands in both orders](roadmap/R15-closed-term-evaluator.md#m143-sharing-the-kinds-of-computed-operands).

M144 uses the order of constructor inspection from the actual Haskell
case expression. Agda's case-tree example explains why a match may
remain stuck at a variable even when an equation looks applicable on
paper. Matching a Haskell sum also has to preserve which fields are
forced. The producer checks that each branch returns a lazy field.
[Agda case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees),
[Haskell 2010 pattern matching](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-610003.17)
and [sum projection controls](roadmap/R16-open-term-evaluator.md#m144-deferring-a-projection-over-an-ordinary-sum).

### Inferring a checked case and preserving its demand

Agda's implicit arguments can be determined by checking the other
arguments or the expected type. M145 uses this relationship for a
certified projection. Its result type fixes one component of an
ordinary pair builder even while the other component is unknown.
Whether the linker can load the projection changes how the compiler
executes the call. Both paths use the same argument and result kinds.
[Agda implicit arguments](https://agda.readthedocs.io/en/v2.8.0/language/implicit-arguments.html)
and [source and interface checks](roadmap/R15-closed-term-evaluator.md#m145-sharing-kinds-across-a-checked-family).

Agda's case trees split a selected variable and retain the choice of
which argument to inspect next. Haskell pattern matching distinguishes
a defined outer constructor from an undefined field. M146 inspects the
ordinary Core case tree and repeats its demanded constructor patterns
in each nominal equation. This permits a fallback to remain unevaluated
and a case on an unknown inner constructor to remain stuck.
[Agda case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees),
[Haskell 2010 pattern semantics, §3.17](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-610003.17)
and [the checked branch examples](roadmap/R16-open-term-evaluator.md#m146-nested-constructor-cases-with-a-fallback).

For R21, Agda distinguishes declaring a type from checking the clauses
that define its value. Its interleaved mutual blocks make this ordering
explicit. In GHC the corresponding implementation already splits instance
heads from method checking. M147 reuses those two passes when a local
ordinary producer is needed in a type. Its head supplies dictionary
resolution before the methods can refer to later types or values. GHC
stores the renamed associated equations separately from the methods,
so their actual type dependencies can be scheduled with the head.
[Agda interleaved mutual blocks](https://agda.readthedocs.io/en/v2.8.0/language/mutual-recursion.html#interleaved-mutual-blocks),
[GHC's instance passes](../../compiler/GHC/Tc/TyCl/Instance.hs)
and [scheduler and acceptance example](roadmap/R21-dependency-analysis.md#m147-separate-an-instance-head-from-its-methods).

### Constructor results, field demand and family instances

Agda's case trees explain when a function can choose a clause. Once the
required constructor is known, the right-hand side can build a value
from its bound variables. M148 checks the ordinary Core case and its
result constructors. Haskell's rules for a boxed pair pattern determine
M149's strictness. A constructor match inspects the pair, while the
variable pattern for an unused field requires no evaluation.
[Agda 2.8 case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees),
[Haskell 2010 pattern semantics, §3.17](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-610003.17)
and [closed evaluation controls](roadmap/R15-closed-term-evaluator.md#m149-demand-only-the-selected-field-of-a-computed-pair).

Agda permits a datatype declaration and its constructors to appear at
different points in a mutually dependent group. GHC's family instances
have additional requirements. Their constructors, selectors and derived
instances must be available when GHC checks ordinary term uses. M150
reuses GHC's existing instance pass and checks the field types before
the local producer. A method may refer to the producer later in the
group. [Agda interleaved mutual declarations](https://agda.readthedocs.io/en/v2.8.0/language/mutual-recursion.html#interleaved-mutual-blocks),
[GHC family instances](../users_guide/exts/type_families.rst)
and [the local source example](examples/open-evaluation/LocalDataFamily.hs).

### Private field families and checked helper calls

An Agda abstract function has a stable identity even where its body
cannot reduce. M153 uses the same distinction for a suspended pair
field. The representation is specific to GHC. An empty closed type
family has no reduction equations, and the outside of the pair follows
from checking the Haskell constructor worker. Haskell's lazy pair fields
explain why that check can describe the constructor without inspecting
the other field. [Agda 2.8 abstract definitions](https://agda.readthedocs.io/en/v2.8.0/language/abstract-definitions.html),
[GHC closed type families](../users_guide/exts/type_families.rst),
[Haskell 2010 pattern semantics, §3.17](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-610003.17)
and [R15's separate-module check](roadmap/R15-closed-term-evaluator.md#m153-export-a-direct-projection-of-a-partial-pair).

Agda also permits an opaque name in a type and controls which proofs
can unfold it. This distinction matters for the next R15 example.
GHC can run the projection of a computed field, but an exported
synonym can carry the projection into another module before a proof
uses it. The Haskell design must also record interpreted object
dependencies and applicable rewrite rules.
[Agda opaque definitions](https://agda.readthedocs.io/en/v2.8.0/language/opaque-definitions.html)
and [Gratzer et al., Controlling unfolding in type theory](https://www.cambridge.org/core/journals/mathematical-structures-in-computer-science/article/controlling-unfolding-in-type-theory/A2865261D18E29153AB7B0F2E89BA8C8).

GHC already uses distinct applications of an empty closed family
when two unknown types must remain distinct. Its pattern-match checker
can otherwise conclude that a reachable branch is inaccessible. For
R15 an unread field also has an owner and an observation context. A
single placeholder of the field's kind would lose that distinction.
See Note [The types Any and UnusedType], especially (Any6), in
[GHC.Builtin.WiredIn.Types](../../compiler/GHC/Builtin/WiredIn/Types.hs).

Agda's case trees preserve the order in which pattern matching inspects
inputs. M151 checks that order in GHC Core. A helper call in a result
constructor also needs its own equation. GHC retains the checked family
in the branch and verifies its owner and rewrite rules during reduction.
M154 reuses the constructor check for the helper's own body. A variable
pattern can return its input beneath a lazy constructor without forcing it.
[Agda case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees)
and [helper rules and recompilation](roadmap/R16-open-term-evaluator.md#m151-checked-helper-calls-inside-constructor-results).

Agda can place declarations before mutually dependent definitions. GHC
also has to decide which family equations were available while checking
an earlier value. M152 can delay all the equations of a fresh local
family whose declaration is outside the earlier dependency closure.
GHC's normal instance checker then checks the equations together.
[Agda interleaved mutual blocks](https://agda.readthedocs.io/en/v2.8.0/language/mutual-recursion.html#interleaved-mutual-blocks),
[GHC family instances](../users_guide/exts/type_families.rst) and
[the two local producers](examples/open-evaluation/IndependentFamily.hs).

Agda checks a recursive group's calls for a decrease that guarantees
termination. M155 admits private helper chains with an acyclic call
graph. GHC checks a callee before using its equation in a caller and
declines a recursive component. This establishes a finite chain of
checked equations for the supported constructor results.
[Agda 2.8 termination checking](https://agda.readthedocs.io/en/v2.8.0/language/termination-checking.html)
and [private helper chains](roadmap/R16-open-term-evaluator.md#m155-reachable-helper-chains).

### Imported family equations and private constructor cases

Agda allows declarations and bodies to appear at different points in an
interleaved mutual block. In GHC, bringing a type family instance into
scope can also change the dictionary selected for an ordinary call. M156
checks a group of signed local functions before the imported family
equations only when it can exclude those dependencies. The example runs
the same ordinary source with and without the family equation to show
the change in dictionary selection. [Agda interleaved mutual blocks](https://agda.readthedocs.io/en/v2.8.0/language/mutual-recursion.html#interleaved-mutual-blocks),
[Serokell on declaration dependencies](https://ghc.serokell.io/dh#dependency-analysis)
and [GHC source control](../../testsuite/tests/dependent/should_run/NativeImportedFamily.sh).

Agda's case trees distinguish a constructor known at the call site from
a variable that still blocks selection of a clause. M158 applies the
existing Core case checker to a private helper reachable from the
exported result. Haskell can construct the outer `Just` before the
helper inspects the spine. Its source clauses determine when the
contained element can be identified. [Agda case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees)
and [private helper example](examples/open-evaluation/SpineWorker.hs).

M159 uses a structural decrease that Agda also recognizes. In the
recursive clause, the next input is a proper subterm bound by the
constructor pattern. In Haskell, reaching a lazy output constructor
does not require the recursive call to return. The client can therefore
check one output cell while its tail is still unknown.
[Agda structural recursion](https://agda.readthedocs.io/en/v2.8.0/language/termination-checking.html#structural-recursion)
and [the ordinary recursive worker](examples/open-evaluation/SpineWorker.hs).

M160 separates dependencies needed to check a function's type from
those needed to compute with its body. Agda's interleaved blocks also
allow a function to use a declared signature before its clauses appear.
GHC must additionally account for dictionary selection and rewrite
rules. Changing only the body of an imported projection changes the
family equation in the example. [Agda mutual declarations](https://agda.readthedocs.io/en/v2.8.0/language/mutual-recursion.html#interleaved-mutual-blocks)
and [imported function test](../../testsuite/tests/dependent/should_run/NativeImportedFamily.sh).

M161 uses GHC's `isDeadEndAppSig` to examine a call after rewriting.
The number of supplied arguments matters. A function that cannot
return on full application can still return a function when partially applied.
GHC also simplifies the expression it sends to the interpreter and
selects its linkables before execution. The experiment varies the
actual linked implementation while preserving the source interface.
[Demand signatures](../../compiler/GHC/Types/Demand.hs),
[interactive Core compilation](../../compiler/GHC/Driver/Main/Passes.hs),
[linkable selection](../../compiler/GHC/Linker/Deps.hs) and
[field readback experiment](roadmap/R15-closed-term-evaluator.md#m161-observe-a-field-decision-from-the-loaded-executable).

Weirich, Voizard, de Amorim and Eisenberg give separate rules for
evaluation and equality evidence in the presence of nontermination.
This distinction also applies to the probe. The type checker can verify
which tagged result a particular executable returned. A new equality
between arbitrary applications would need its own justification.
[A Specification for Dependent Types in Haskell](https://doi.org/10.1145/3110275)
and [the boundary example](BOUNDARY-EVIDENCE.md#what-does-an-observation-establish).

Agda's list `replicate` recurses on the predecessor and passes the
element unchanged. M162 checks the same relationship between the two
arguments of `fill` in ordinary Core. The recursive tail can remain
unknown because the input cell determines the outer output constructor.
[Agda list operations](https://agda.github.io/agda-stdlib/master/Data.List.Base.html)
and [the checked spine equation](roadmap/R16-open-term-evaluator.md#m162-structural-recursion-with-an-unchanged-argument).

Agda's `maybe′` applies the selected function to the field of a
known `just`. M163 uses the ordinary Haskell `maybe` with its own
argument order. The scheduler checks the imported signature and the
actual constructor worker. The evaluator can then select the `Just`
branch without inspecting its field. [Agda Maybe eliminators](https://agda.github.io/agda-stdlib/master/Data.Maybe.Base.html)
and [the family producer](examples/open-evaluation/ImportedFamily.hs).

The Haskell Report uses dependency groups to infer and generalize
binding types. A reference to a binding with a signature can break a
typing cycle. M164 uses GHC's ordinary inference for a local producer.
It also checks the dependencies in the producer bodies before evaluating
a family equation. Those bodies can still call each other.
[Haskell 2010, sections 4.5.1–2](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-880004.5.1)
and [M164 source and interface checks](roadmap/R21-dependency-analysis.md#m164-inferred-independent-producer-types).

Agda's list `map` takes an ordinary function. Its cons equation applies
that function to the head and recurses on the tail. The same expression
serves two separate GHC checks. M165 runs the application on a known
list. R16 also needs to preserve the application in an exported equation
when the tail is unknown. Haskell's executable dependency tracking and
rewrite rules affect each check. [Agda standard library 2.3, Data.List.Base](https://agda.github.io/agda-stdlib/v2.3/Data.List.Base.html)
and [the two evaluator cards](ROADMAP.md#current-work-queue).

Agda's list `zip` passes the pair constructor to `zipWith`.
Its `unzipWith` passes the list cons constructor to `Product.zip`.
M167 uses the same source pattern with Haskell's `uncurry`. The GHC
evaluator needs the worker's instantiated field types to eta-expand a
partial application. It must also preserve the laziness and visibility
of the result. [Agda standard library 2.3, Data.List.Base](https://agda.github.io/agda-stdlib/v2.3/Data.List.Base.html)
and [GHC's open Core evaluator](../../compiler/GHC/Core/OpenWhnf.hs).

Farmer, Sculthorpe and Gill turn a GHC rewrite rule into a HERMIT
lemma and replay equational steps on Core. Their list example checks
map composition and treats an undefined spine as a separate case.
R16 can reuse those proof obligations when comparing the installed
fusion rule with its source definition. The visible rules for `build`
and `foldr` introduce additional obligations in this fork.
[HERMIT paper](https://doi.org/10.1145/2804302.2804303)
and [reading notes and checked scope](papers/README.md#checking-ghc-rewrite-equations-with-hermit-haskell-2015).
