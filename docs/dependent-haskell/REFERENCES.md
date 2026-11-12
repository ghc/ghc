# Dependent Haskell references

The original survey was checked on 2026-11-11. The Agda and Haskell evaluation
references were checked on 2026-11-12. The GHC source baseline is
[`19ff36f2aede0ca2e554016ff22c71e73aa9803b`](https://github.com/ghc/ghc/commit/19ff36f2aede0ca2e554016ff22c71e73aa9803b).
Later changes to proposal status, package bounds, branches, or documentation need
verification. A link to a mutable branch identifies the inspected source, not a
permanent compatibility guarantee.

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
