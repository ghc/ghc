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
