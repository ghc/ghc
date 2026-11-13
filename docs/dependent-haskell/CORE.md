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

## Fixed-domain retained congruence

`DepFunCo b co` binds one runtime index throughout its body proof. The binder
has an unchanged supported closed algebraic domain and `Many` multiplicity.
If `co :: s ~r t`, then:

```text
DepFunCo b co :: mkDepFunTy b s ~r mkDepFunTy b t
```

Both body endpoints must have kind `Type`. The coercion inherits the body's
FC role. Each endpoint independently becomes an ordinary arrow when its result
omits `b`. The binder also scopes over intermediate proof types. Keep it even
when both final endpoints omit the index. `mkDepFunCo` simplifies a syntactically
reflexive body; endpoint independence alone does not remove the proof binder.
Substitution, free variables, lifting, zonking, and tidying preserve that scope.

This rule specializes D/DC's An-PiCong to an unchanged domain. Agda supplies
the shared-context comparison precedent. See
[the pinned provenance](REFERENCES.md#agda-provenance) and Note
[Retained dependent function coercions] in `GHC.Core.TyCo.Rep`.
These sources do not prove preservation for the current GHC extension.

Transport a function value with a cast. Do not replace that cast with a lambda:
eta expansion can hide a bottom function in Haskell. General Pi matching,
ambient-evidence normalization under retained binders, changed domains or multiplicities,
dependent erased binder kinds, and the ordinary optimizer remain unsupported.
`ForAllCo` still binds only erased type and coercion variables. No new selector
or erased `InstCo` rule is added. Diagnostic interface conversion can print the
coercion; serialization in native unfolding bodies remains gated.

## Family-axiom normalization under retained binders

The Core normalizer recurses through `DepFunTy` under its unchanged supported
closed domain and `Many` binder. `substNativeIndexBndr` masks an outer Id
substitution and avoids capture. The lifting context can map an index to the
same valid same-domain variable on both endpoints. It must not infer an index
equation or evaluate the runtime variable.

Normalize the body with the existing family reducer. `mkDepFunRedn` wraps its
proof in `mkDepFunCo` and its result in `mkDepFunTy`. This preserves the proof
binder even when an intermediate type alone needs it. Each endpoint separately
canonicalizes a vacuous binder to an ordinary arrow. For example, `Add Z n`
reduces to `n`; `Add (S n) Z` reduces to `S (Add n Z)` while its inner family
application stays stuck. A constant family can remove the retained dependency.
Family axioms reduce at nominal and representational roles. At phantom role,
the existing `reduceTyFamApp_maybe` policy leaves family heads unreduced.
Ordinary forall binder kinds still normalize at nominal role.

The solver enables `re_native_binder` throughout the recursive body rewrite.
Global family axioms still reduce. Ordinary type meta-variables can be read and
zonked. This mode suppresses plugins, inert equality lookup for families and
type variables, family-cache reads and writes, and coercion zapping. These paths
could otherwise emit or retain evidence outside the runtime binder's scope.
Keep the current role, reduction depth, and diagnostic location. Nested retained
binders inherit the mode. No local index enters an outer inert set or cache.
The ordinary `ForAllTy` binder-kind paths remain unchanged.

After both equality endpoints have been rewritten, `can_eq_nc` checks same-Id
runtime indices and `tcEqType` equality of retained functions. Rewriting returns
to this function without repeating `zonkEqTypes`. This reflexivity check uses
the rewriting evidence that already relates the original endpoints to their
normalized types. It neither decomposes a retained function nor assigns a
meta-variable. See Note [Reflexivity after retained normalization] in
`GHC.Tc.Solver.Equality`.

This is genuine family reduction under the binder. It does not provide ambient
Given-based normalization, structural retained unification, retained inference,
or runtime index evaluation. Opaque indices remain stuck where a family needs
constructor discrimination, including bottom and cyclic runtime values.
See Notes [Normalising retained runtime indices] and
[Rewriting retained dependent functions], and
[the scoped Agda normalization sources](REFERENCES.md#agda-provenance).
Agda's conversion and eta rules do not add a Haskell function eta law.

## Arithmetic equality proofs

The source subset can return an ordinary `Data.Type.Equality.:~:` proof over a
runtime Nat. `rightZero :: foreach (n :: Nat) -> Add n Z :~: n` uses induction.
The zero branch returns `Refl`. The successor branch matches the recursive proof
and returns `Refl` under its local nominal equality. The solver combines that
equality with the existing Add axiom and successor congruence. The right-unit
family application at an opaque index remains stuck without a supplied proof.

`Refl` repeats an invisible index in its result. The two expected indices can
be equal only under a local Given. The exact immutable result matcher cannot
select the shared type argument in that case. Ordinary instantiation would
allocate a meta-variable that the retained-index fill check must reject.

The constructor fallback matches corresponding result arguments separately.
It selects the first candidate for each outer invisible binder. Every binder
must have a candidate. The complete substitution must satisfy the substituted
binder kinds and the existing local runtime-index scope check. No template
binder can remain free in the candidates. The fallback requires NoQL, an actual
data constructor, a saturated ordinary algebraic result of the same datatype,
and no pending type arguments, native template, or constraints. Constructors
with concreteness requirements keep ordinary instantiation.

The fallback emits ordinary type applications and preserves the actual
substituted constructor type. `finishAppWithCache` still runs `checkResultTy`
against the original expected type. Ordinary subsumption must prove every
remaining equality. Candidate selection supplies no evidence. Both a false
`n :~: S n` proof and an unproved right-unit `Refl` must fail at the expression.
See Note [Retained constructor instantiation candidates] in `GHC.Tc.Gen.App`.

The constructor change does not assign retained syntax to ordinary argument
meta-variables. Generic consumers use the separate immutable route below.

The proof is a lifted value. Matching `Refl` forces it and introduces an ordinary
GADT CoVar only within its alternative. Merely receiving a proof, or checking it
inside an unused local expression, supplies no equality to the enclosing body.
The recursive proof can traverse the whole Nat spine. Bottom raises an exception;
cyclic input can diverge. An ignored proof thunk can stay lazy. A partial
application of the transport function must keep its index lazy. These rules use
the existing GADT lowering and add no Core form or interface format change.

The [pinned arithmetic proof sources](REFERENCES.md#arithmetic-equality-proofs)
record Agda induction, congruence, and transport, expected-target constructor
checking, singletons proof values, and GHC's equality datatype. Agda selects
datatype parameters before checking the target. This fallback selects candidates
per result argument and keeps full ordinary subsumption. The algorithms differ.

## Generic retained argument inference

An ordinary erased argument can denote a whole type that contains a runtime
index. For example, `force :: forall a. a -> IO String` can consume `rightZero n`.
Here `a` has kind `Type`. The earlier cache selected only binders whose kinds
were supported native index domains. The generic route also handles whole proof
and vector types, and retained dependent functions passed through ordinary `id`.

Keep NoQL and a saturated ordinary tau body after any leading constraints.
Keep no native template, pending type arguments, or head concreteness requirements. Classify
named argument spines with read-only Id and constructor lookups. A known Id
whose type contains native syntax must occur in a spine. Look through named
applications, type applications, parentheses, pragmas, and existing expansions.
Never call `splitHsApps` or `tcExpand` during classification. They can execute
a Template Haskell splice. Pattern synonyms do not qualify as constructor heads.

After a native argument has been found, ordinary named arguments can determine
other binders. For example, `const (rightZero n) Nothing` determines its second
type argument as `Maybe alpha`, where `alpha` remains an ordinary meta-variable.
Seed the immutable substitution from the expected result first. Infer only
eligible argument spines whose formal domains contain undetermined binders.
Lambdas, literals, cases, lets, and do expressions keep expected-type checking.
A named application can check such forms within its own argument list.

Infer each selected argument once. Preserve its expression, type, and usage in
the existing cache, including failed-match paths. All outer binders must have
candidates. Check their substituted kinds, their runtime Id scope, and exact
agreement between cached actual types and substituted formal types. The generic
instantiation must contain native syntax. Emit ordinary type applications.
Final result and argument subsumption still run. Emit each saved usage once at
the argument's final multiplicity. Native meta-variable assignment stays gated.
See Note [Generic retained argument inference] in `GHC.Tc.Gen.App`.

Inference does not force the argument. `force (rightZero n)` forces the proof;
`force (Just (rightZero n))` forces only `Just`. Ignoring the proof stays lazy.
Forcing a partial retained `appendNative` through `id` keeps its index lazy.
The linear control gives the local helper an explicit unrestricted let binding,
`%'Many linearId = \x -> x`. Its proof argument still has multiplicity `One`.
Ordinary GHC also requires that binding annotation for the repeated local use
in this control. Duplicating the linear proof must produce GHC-18872.

This is a bounded application algorithm. General native meta-variable scope,
checking-dependent argument inference and Quick Look remain
separate work. The [pinned Agda application and meta-context sources](REFERENCES.md#generic-retained-argument-inference)
use context-scoped mutable metas. This route uses immutable matching and GHC's
existing usage cache. It does not adopt Agda's meta-assignment algorithm.

## Retained class consumers

An ordinary consumer can require a class dictionary for a type that contains a
runtime index. For example, `show (rightZero n)` requires
`Show (Add n Z :~: n)`. Match argument and result types in the ordinary body after
the constraint prefix. Return the substituted original body with the prefix.
Ordinary `IALL` and `instCall` must still generate every evidence application.
All newly constrained immutable routes require a known head with no concreteness
requirements. Constructor candidate matching remains unconstrained.

Capture actual application constraints with the extension enabled. A native
simple Wanted requires an expression-local implication and `WpLet`. Inspect the
generated predicates, including those exposed after visible type or earlier value
arguments. Keep the current `TcLevel` and enclosing Givens. The evidence bindings
must remain below each runtime index binder. Nested implications already own
their evidence. Applications without native simple Wanteds emit their original
constraints without a new wrapper.

Exclude native residual predicates from constraint generalization and defaulting.
Keep ordinary meta-variables in those predicates monomorphic. Preserve the
original Wanteds for solving and located diagnostics. An ordinary inferred result
such as `String` does not establish that its residual constraints are index-free.

Instance resolution inspects types. Each method keeps its ordinary runtime
behavior. `Show (a :~: b)` matches `Refl`, so rendering a proof can force it.
An ignoring consumer can remain lazy. A `Show` dictionary supplies no equality
between the indices. Transport still requires explicit equality evidence.
Source-written class contexts containing runtime binders remain outside this
class-consumer increment. The next section specifies their implementation.
See [the primary instance-context sources](REFERENCES.md#retained-class-consumers).

Native dictionary predicates also require native type-map keys. Keep `TermVarTy`
separate from `TyVarTy`. Bound runtime Ids compare by de Bruijn position; free
Ids compare by Unique. For `DepFunTy`, compare the domain in the outer context
and the body under the extended context. Keep retained functions separate from
ordinary arrows and erased foralls. Singleton equality and trie operations must
use the same rules. Preserve synonym, cast, and kind handling. This support does
not enable native family-cache reuse under a Pi binder or ordinary optimization.

## Source retained constraints

A source signature can put an ordinary class or equality context after a runtime
binder. For example, `foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String`
has the Core argument order `n`, dictionary, vector. Check the predicates with the
ordinary validity and constraint solver paths. Native quantified and irreducible
predicates remain gated, including components hidden by constraint-tuple synonyms.
Do not introduce a new Core form or equality axiom.

For a single equation with a variable prefix, split the equation at its first
context. Check the suffix as a generated lambda under the actual prefix binders.
Keep suffix patterns, guards, and local bindings together. An empty suffix still
needs this scope. When value patterns remain, require nonforcing prefix patterns
and exclude `Strict`. A trailing context keeps the old prefix shapes. These rules
preserve the order of source value lambdas and prefix matching.
Consume a known ordinary arrow without skolemising its result. Check its following
context at the next telescope step, including after the last source value pattern.
The trailing-context exception retains the old placement of dictionary lambdas
after any forcing prefix. It establishes no new law for partial dictionary
application in raw Core. General preservation of dictionary arity with forcing
prefixes remains separate work.

Constructor equations must put all value and dictionary lambdas before their
constructor case. Insert shared dictionary patterns at the signature positions.
Keep their original predicate types at the outer lambdas. In each alternative,
transport these dictionaries with the existing checked constructor proof. Check
the whole match with the specialised Givens. Bind the transported Givens and
solver evidence in `ConPatTc.cpt_binds`, within that proof scope. Local bindings
must have access to this evidence. Keep the existing constructor pattern limits.

An incoming Given must work without a global instance. A caller must supply the
dictionary through ordinary instance resolution or its own Givens. Equality
transport still requires equality evidence. Partial application must preserve the
runtime index's laziness. See the
[pinned Agda sources](REFERENCES.md#source-retained-constraints) for telescope and
hidden-argument scope. Haskell evaluation and GHC dictionary transport use the
existing local rules. This increment keeps the native interface format unchanged.

### Explicit case evidence

An explicit dependent case must generalize incoming evidence as well as captured
values. Record the actual lexical evidence parameters while checking their scope.
Select the parameters whose predicate depends on the scrutinee. Include primitive
equality CoVars. Substitute the fresh index into these predicates with the native
type substitution. A predicate has kind `Constraint`; it is not a retained
function result.

Put the shared evidence parameters after the fresh index and before the captured
values. Apply the generated helper to the original index, evidence, and values.
Supply every generated argument before the constructor case can run. Transport
each predicate with the checked branch proof. Let the ordinary solver derive
superclasses from the transported dictionaries. Do not search for replacement
instances or enumerate derived evidence. Static forms must exclude enclosing
evidence, as on the ordinary constraint solver path.
An arrow scope must save this evidence with its reader environment and constraint
accumulator. An escaped arrow head must restore that saved evidence. It must not
capture evidence from the `proc` pattern.
At a nested typed splice, restore the bracket-entry evidence with the outer
constraint accumulator. Quoted-body evidence must not enter an emitted splice.

Keep the existing complete, unguarded constructor pattern rule. An explicit case
must inspect its index before returning a branch lambda. The evidence transport
must not inspect a dictionary, a captured value, or an unused constructor field.
Check no-instance Givens, tuples, equality evidence, nested cases, local bindings,
and partial application. The implementation must preserve the current interface
format and dictionary ABI.

## Lambda and application

If a runtime lambda's body type mentions a supported runtime binder, its type is
the canonical retained dependent function. Otherwise its type is an ordinary arrow.

### Checked source lambdas

Check a source lambda against its complete expected retained type. Reuse the
function-equation rules for its telescope, contexts, and constructor patterns.
Use each source variable pattern's runtime Id when instantiating the remaining
types. Preserve outer erased binders and the lambda's ambient usage. Named
bindings retain their own usage scaling. Do not add unrestricted usage scaling
around an anonymous lambda.

Preserve the source pattern arity. A `\case` inspects its index when that
argument arrives, even when the selected branch returns a function. A `\cases`
with two patterns still waits for its second argument before matching. An
unapplied lambda remains a function value. Do not eta-expand either form.
Keep context splitting and constructor evidence transport in their existing
positions. Keep the current coverage, bare-field, multiplicity, and wrapper
restrictions. Ordinary lambdas keep their existing checking and inference path.

An empty lambda-case on an empty domain binds the runtime index before an empty
case inspects it. The result type uses that same Id. The empty case needs no
constructor proof. An empty source case keeps its original context because
there is no branch to refine. A demanded application to bottom still diverges.

This rule checks a supplied type. It does not infer a native function type or
enable new retained type positions. Retained quantification in nested type
positions and general checking-dependent argument inference remain separate work.
Agda's expected-Pi lambda rule and extended-lambda function checker supply the
checking precedent.
See [the pinned sources](REFERENCES.md#checked-retained-lambdas).

### Explicit higher-order closure arguments

A complete value signature can contain a retained function type in an ordinary
arrow argument or result. Traverse parentheses and both sides of ordinary
arrows. Keep the existing checks for retained domains, kinds, staging, and
other source type positions. Apply the complete-signature rule to the whole
signature, including wildcards outside the nested retained telescope.

Each retained binder scopes its own remaining telescope and codomain. An inner
binder must not enter an enclosing arrow's result. Its codomain can still refer
to an outer runtime index. The ordinary closure parameter has a function type.
It is not a runtime index and does not change the supported index domains.

Check a named function or source lambda against the supplied argument type.
Reuse retained application and the checked-lambda rule. Preserve source arity,
usage, and laziness. Ignoring a bottom closure must not evaluate it. Supplying a
bottom index must not evaluate that index when the selected function ignores it.
Do not infer a retained function argument type through a native meta assignment.

For signature ambiguity checking only, open retained binders in ordinary arrow
arguments as well as results. Use one fresh opaque runtime Id on both sides of
the self-check. Scope all fresh Ids over constraint capture and simplification.
Keep the actual signature retained. This projection supplies no coercion between
different retained functions. Preserve the structural-unification gate.
See [Agda's expected-argument and shared-context rules](REFERENCES.md#explicit-higher-order-arguments).

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

Transport through a remaining retained function uses `DepFunCo` under its
unchanged closed domain and `Many` binder. Keep the cast on the partial function
value. Eta expansion must not hide a bottom function.
Erased binder kinds that depend on a named index are still gated. Alias proof
expressions have no interface codec. See Notes [Native index alias evidence]
and [Retained constructor arguments in CorePrep] and the provenance in
[REFERENCES.md](REFERENCES.md).

The `lazy`/`noinline`/`nospec` stripping path can bypass this preprocessing and
still reject a constructed retained argument. This is a conservative lowering
gap. PMC now preserves known function heads before component normalization.
This supports strict wildcard matching on partial retained functions, including
an ordinary arrow with a retained codomain. It does not assume the function
expression is evaluated. Family-axiom normalization under Pi remains separate
from coverage classification and ambient-evidence rewriting. See Note
[Coverage checking retained functions].

The immutable matcher can bind a whole retained function type to a template
variable. It retains the usual occurs and escape checks. Unsupported retained
comparisons produce `MaybeApart MARRetainedFunction`. Alpha-equivalent retained
types without free TyCoVars can compare reflexively. This does not add structural
matching under Pi or assign Tc metavariables. See Note [Matching retained function types].

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

`mkPiCo` uses `mkDepFunCo` when its proof depends on a runtime binder.
The dependency check includes intermediate proof types. A proof independent
of that binder can use ordinary function congruence. Reject unsupported domains
and transport rules explicitly.

## Lowering audit for M5

The M5 Boolean source audit found these requirements for the restricted bytecode
entry. Later increments add algebraic domains and fixed-domain congruence.

| Function | Restricted lowering rule |
| --- | --- |
| `CoreToStg.Prep.cpCloneBndr` | Substitute proof types when only the Id environment changes. Cloning `b` must update every `TermVarTy b`. |
| `Core.Opt.Arity.typeOneShots` | Count the retained Boolean as one runtime argument. Otherwise `typeArity` returns zero and Lint rejects positive arity metadata. |
| `Core.Opt.Arity.mkEtaWW` | Allocate a fresh eta binder and substitute that exact Id into the codomain. Non-reflexive eta-cast handling remains gated; checked value transport uses `DepFunCo`. |
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
not add arbitrary expression indices, general Pi matching, or general proof
erasure. Run Core Lint before and after this entry. Retest the shared runtime
index and bottom observations before claiming optimizer preservation.

## Frontend identity plan

The checker now supports structural domains, outer erased quantification,
retained telescopes, and bare constructor fields. The bootstrap API executes
source Nat/Vec functions through bytecode and separate-module machine code.
The rebuilt CLI and registered self-hosted fixture pass the same module checks.
Expected-type-directed source `case` refinement reuses the retained constructor
rule. Retained signatures bind
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

With `ExperimentalDependentTypes`, a checked source `case n of ...` can reuse
this rule. Abstract the expected result over a fresh index. Generalize each
referenced local whose type depends on `n` as a later argument. For example,
`xs :: Vec a n` becomes an argument of type `Vec a j` under the fresh index `j`.
Check constructor clauses with the existing argument and result transports.
Apply the checked pattern function to `n` and the captured values. Use fresh
resolved Names and substitute each constructor for direct uses of `n` in its
branch. Preserve shadowed names and source locations. See Note
[Source retained case refinement] in `GHC.Tc.Gen.Match` and the
[Agda provenance](REFERENCES.md).

Complete unguarded clauses and a fully supplied generated application preserve
strict inspection of the scrutinee. A function-valued result must not defer that
inspection. Generalized payload arguments remain lazy. Require bare constructor
fields, unrestricted generalized locals, and a lifted checked result.
Gate quotations before source substitution.
General motive inference, guards, defaults, and nested constructor patterns
remain separate work. This elaboration adds no Core form, singleton argument,
unfolding equality, or trusted coercion.

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

A constraint in the result can require a nonidentity match wrapper. The first
identity-wrapper subset rejected
`foreach (b :: Bool) -> ((b ~ b) => Int) -> ((b ~ b) => Int)`.
The [source-context rule](#source-retained-constraints) now checks this result
under the actual runtime binder. The earlier fully applied laziness observation
uses `foreach (b :: Bool) -> ((b ~ b) => Int) -> Int`. Its body instantiates the
argument's constraint.
The observation uses checked rigid-index handling and scoped reflexivity.
Constraint instantiation builds a boxed `(b ~ b)` dictionary directly at its
call site, with the existing `evDictApp` and nominal reflexivity constructors.
A Wanted dictionary could receive a solver evidence binding outside the runtime
lambda. The direct expression preserves its scope. Other index-dependent
predicates were gated in the initial subset. The application and source-context
rules now give class and equality evidence its runtime-index scope.
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
General source `case` inference and the ordinary optimizer remain gated.
The restricted program entry above
supports local workers with ordinary exported types. Existing ordinary GHC
programs must continue to build.
