# Dependent Haskell references

The original survey was checked on 2026-11-11. The Agda and Haskell evaluation
references were checked on 2026-11-12. The GHC source baseline is
[`19ff36f2aede0ca2e554016ff22c71e73aa9803b`](https://github.com/ghc/ghc/commit/19ff36f2aede0ca2e554016ff22c71e73aa9803b).
Later changes to proposal status, package bounds, branches, or documentation need
verification. A link to a mutable branch identifies the inspected source, not a
permanent compatibility guarantee.
The Pi normalization sources below were checked on 2026-11-13.

See [the project overview](README.md) for the objective. See [the implementation
plan](PLAN.md) for scope, decisions, and acceptance criteria. The first experiment
extends Core from Eisenberg's historical branch. The singleton translation remains
a comparison implementation. External solvers remain later work.

## Reading order

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
  are required. General termination checking is outside this Haskell spike.
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

[`LHSResult`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L548-L566)
records the pattern-context substitution and specialised body type.
[`splitCon`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/LHS.hs#L1389-L1414)
applies the constructor substitution to the later argument telescope, target,
and remaining pattern equations.
[`checkRHS`](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/TypeChecking/Rules/Def.hs#L747-L774)
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
bound computation in continuation typing. Section 7.4 shows why inserting
coercions alone does not establish the general preservation result. Section 7.6
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
then runs full ordinary subsumption. It applies only to DataCon heads with a
plain result and no Quick Look path. Existing kind and scope guards remain.
Constructors with concreteness requirements keep ordinary instantiation.
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

- [Serokell's Dependent Haskell roadmap](https://ghc.serokell.io/dh) was last updated
  on 2026-09-03 when inspected. It separates frontend work, promotion, and dependent
  Core. Retained dependent quantifiers require dependent Core in that roadmap.
  The roadmap gives dependencies, not an implementation guarantee or delivery date.
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

- [The D/DC specification paper](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf),
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
  Progress requires an appropriate consistent equality context; preservation can
  remain useful in contradictory contexts produced during optimization.

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
