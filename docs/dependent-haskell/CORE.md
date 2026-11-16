# Restricted dependent Core

This document specifies the restricted internal implementation. It is based on the
`nokinds-pi` binder and application work recorded in [PORT.md](PORT.md).
The dependent typing decisions use [Agda as the primary reference](REFERENCES.md#agda-provenance).
Runtime evaluation must obey Haskell's non-strict rules. This document does not
specify the full dependent-Haskell language.

## Representation

The internal `Type` datatype has three new constructors:

```text
TermVarTy b       -- The algebraic index of a runtime identifier.
DepFunTy b result -- A retained dependent function.
TermAppTy f arg   -- A scoped opaque runtime call index.
```

`b` is a local non-coercion `Id` with multiplicity `Many`. In TermVarTy its
type is a supported closed algebraic domain. DepFunTy can also bind an eligible
opaque function head. See [function-valued retained binders](#function-valued-retained-binders).
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

`TermAppTy` has a separate head policy. It does not make a function-valued
`TermVarTy` valid. See [opaque native call indices](#opaque-native-call-indices).

## Retained constructor fields

The source package example stores a runtime index with its dependent payload:

```haskell
-- Compiles in the two-module source example.
data PackedVec a where
  PackVec :: foreach (n :: Nat) -> Vec a n -> PackedVec a
```

Use an ordinary algebraic datatype and the existing `DepFunTy` and `TermVarTy`.
Do not add a primitive Sigma type. Each retained field binds its actual runtime
Id over later field types. An optional binder list accompanies the original
argument types. The empty list preserves the ordinary DataCon representation.
`mkDataConWithRetainedArgs` validates the nonempty list when the worker or user
type is demanded. It exposes the metadata before this validation.
The worker and user types rebuild the telescope with `mkDepFunTy`.
Do not store runtime Ids in `dcExTyCoVars`.

The initial Core subset has lazy lifted fields, unrestricted retained binders,
closed native index domains, and no wrapper, existential, equality constraint,
dictionary, record selector, newtype, strict field, or unpacked field. A field
type can refer only to earlier retained fields. The constructor result cannot
contain a free runtime Id. The package itself does not become a native index
domain. Its Nat field remains an admissible index.

Instantiate pattern fields in order. Substitute each retained template Id with
the fresh actual field Id before instantiating later fields. Extend the
substitution scope after each field. Core Lint checks the fields as sequential
worker applications. It counts each retained Pi as one runtime field, requires
fresh local field Ids, and checks the case result outside the field scope.
Packing and unpacking must preserve the two ordinary runtime payload slots.
Fields remain lazy; constructor WHNF does not force the index or the vector.

Promotion needs a separate conversion from runtime field binders to promoted
binders. The initial builder rejects promotion instead of making an ill-scoped
promoted TyCon. Source checking must reject unsupported promotion with a located
GHC diagnostic before it can reach this internal gate. Interface declarations
encode optional field binders beside their argument types. Automatic Typeable
generation skips the promoted constructor and retains the parent datatype.

First expose scoped elimination through a retained consumer. General dependent
projections, such as a vector whose index is `fst package`, require a larger
index-expression grammar. Do not infer record eta laws from Agda. See the
[pinned Agda and historical provenance](REFERENCES.md#retained-constructor-field-telescopes).

### Constructor field interfaces

Store `ifConArgBinders :: [Maybe IfLclName]` beside `ifConArgTys`.
The empty list denotes an ordinary constructor. A nonempty list must align
with its fields. The existing field entry remains the only encoding of its
type and multiplicity. A name binds only in later fields. The writer tidies
each field type before extending the tidy environment with its binder.

The reader checks metadata shape, distinct names and unsupported declaration
features before decoding types. It allocates fresh runtime Ids in field order
and extends the local Id environment only for later fields. A named field uses
multiplicity Many. Check the encoded multiplicity, fixed domain, lifted field
kind and free-runtime scope in deferred decoding. Reject inherited, forward
and self references. Keep field scope out of the result and sibling constructors.
Report malformed native metadata through `failIfM` with the constructor name.
Require an ordinary lifted parent. Check that its cached kind matches its
binders and result kind. Universal and user binders must name the same distinct
type variables and declare equal kinds. Reject promotion during type lookup.

Expose constructor metadata before semantic domain validation. An imported
field can refer to its parent TyCon or another recursive declaration. Reject
retained metadata before inspecting a domain constructor's full signature.
Allocate a named field Id with literal Many; `mkLocalId` forces its multiplicity.
Defer its domain and encoded multiplicity checks together. Validate the Core
telescope when the worker or user type is demanded. This avoids reentering the
same recursive validation thunk. Preserve ordinary constructor laziness.

The field schema requires a new interface header version. Preserve an immutable
reader and interface for the previous format before rebuilding. Verify both
version-rejection directions. Source packages use this codec for separate-module
execution. Promotion and native unfolding optimization remain gated. See the
[Agda serialization provenance](REFERENCES.md#constructor-field-interface-provenance).

### Source constructor checking

Rename a retained suffix with runtime Names. Check its domain kinds during kind
inference. Inspect each native domain's constructor graph only in final checking.
An index datatype must be complete before the package's recursive declaration
group. Domains in that same group have local `TcTyCon` placeholders and are
rejected before graph inspection.
Keep explicit field metadata through generalization and zonking. Split the
telescope before substituting each field type. Keep this substitution lazy while
the parent datatype knot is incomplete. Validate the constructor result and its
erased universals after the parent exists.

Role inference and role checking traverse field scope in order. A runtime index
has a closed domain and cannot become a datatype parameter. Ordinary payload
parameters keep their inferred roles. Source patterns substitute the actual field
Id into later field types. Retained fields accept bare variables or a closed
constructor with bare variable subfields. Ordinary fields use GHC's pattern
checker and can contain nested GADT patterns. Desugar
each package equation into its own case on the shared scrutinee. Combine the
failure continuations in source order. Each equation keeps its actual field
Ids; no alias substitutes one runtime index for another. PMC uses checked
outer pattern types and the existing scope rule for speculative fields.
See Note [Retained package equation scope] in `GHC.HsToCore.Match.Constructor`.
See Notes [Source retained constructor declarations] in `GHC.Tc.TyCl` and
[Roles of retained constructor fields] in `GHC.Tc.TyCl.Utils`.

An ordinary source `case` starts with a fresh multiplicity variable. Constrain
it to admit an unrestricted package pattern with the existing `tcSubMult` rule.
Check each field at `Many`. A syntactic `isManyTy` test would reject the fresh
variable before constraint solving. Linear and rigid multiplicities retain
GHC's ordinary located diagnostics. See
Note [Checking retained package patterns] in `GHC.Tc.Gen.Pat`.

A retained constructor pattern binds a fresh Id for the whole stored field.
An outer generated as-pattern preserves that exact Id. Its inner constructor
alternative binds the existing nominal index equality. Use the actual source
subfield Ids in that equality. Transport each later field type through the
accumulated equalities in telescope order. A checked coercion pattern keeps
the field's original outer type. The continuation remains inside the ordinary
pattern checker, so nested GADT evidence scopes over later fields and the RHS.

For `PackVec (S k) (VCons x rest)`, the tail has the actual predecessor `k`.
No runtime predecessor is reconstructed from an erased vector index.
`PackVec Z _` leaves the vector lazy. `PackVec Z VNil` demands its constructor.
Ordinary failure continues to the next source row. A demanded bottom remains
bottom. Other arguments, views and guards keep their matching order. These
patterns add no Core rule, interface field or backend lowering rule. The
existing constructor equality and DEFAULT continuation suffice. Retained
wildcards, source as-patterns and nested retained constructor subfields remain
outside this initial subset.

The source filter uses these same constructor and case forms. It opens the
recursive result as `PackVec n ys`. Its retained branch stores `S n` with
`VCons x ys :: Vec a (S n)`. The result has one runtime length and one vector
field. No additional Core form or singleton witness is required. The filter
scans the finite input spine before returning its package. Element demand
depends on the predicate and the consumer. See
[the source example](examples/packed-vector/README.md#filter-with-a-computed-result-length).

`BoundedVec a limit` adds an ordinary proof field `LE len limit` after its
`Vec a len` field. Both field types use the same actual retained `len` Id.
The upper bound remains an erased parent parameter. The constructor keeps
three runtime fields. `LE` uses ordinary GADT checking and evidence. The proof
field remains lazy; it is not an erased coercion. See
[the bounded source filter](examples/packed-vector/README.md#filter-with-a-checked-length-bound).

## Scope and equality

Core Tidy turns same-module top-level LocalIds into GlobalIds. Before that
step, give each native RHS lazy local aliases for its free top-level index
Ids. Substitute the whole RHS, including types and coercions, then wrap the
aliases outside that substitution. Their RHSs retain the original top-level
references. This keeps all native index leaves local after tidying. Ordinary
RHSs keep their existing path. The existing CorePrep alias guard preserves
these local names. Top binder types and the native unfolding/interface gates
remain unchanged. See Note [Local indices across Core Tidy] in `GHC.Iface.Tidy`.

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
eta expansion can hide a bottom function in Haskell. Changed domains or
multiplicities, dependent erased binder kinds, and the ordinary optimizer
remain unsupported. Eligible ambient Givens can normalize retained bodies;
the later Given-normalization sections specify that restricted path.
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

The solver records every active retained Id in `re_native_binders` during body
rewriting. Global family axioms still reduce. Ordinary type meta-variables can
be read and zonked. Type variables can follow existing inert Givens whose
predicate, RHS and coercion have no deep free occurrence of an active Id.
Check variable kinds and CoVar metadata. Reject coercion holes in this metadata.
The evidence can mention an outer runtime index. Preserve ordinary flavour,
role and dependency-recording rules.

This mode suppresses plugins, inert equality lookup for families, Wanted
rewrites, family-cache reads and writes, and coercion zapping. These paths
could otherwise emit or retain evidence outside the runtime binder's scope.
Keep the current role, reduction depth, and diagnostic location. Nested retained
binders extend the active set; returning restores the previous set. No bound
index enters an outer inert set or cache.
The ordinary `ForAllTy` binder-kind paths remain unchanged.

Canonicalizing an existing Given does not assign a type metavariable.
`checkTypeEqNoUnification` accepts valid opaque runtime indices in a native RHS
only for a Given with a type-variable LHS. A checked retained Pi RHS recurses
through its body with the same flags. `mkDepFunRedn` scopes the complete body
reduction. Preserve ordinary occurs and coercion checks. A recursive
`a ~ (foreach n -> Vec a n)` remains irreducible. Disable family cycle
breaking because cycle-breaker restoration writes a metavariable. All Wanted,
unification and concreteness checks retain their native-syntax restrictions.
See Note [Rigid retained indices] in `GHC.Tc.Utils.Unify`.

After both equality endpoints have been rewritten, `can_eq_nc` checks same-Id
runtime indices and `tcEqType` equality of retained functions. Rewriting returns
to this function without repeating `zonkEqTypes`. This reflexivity check uses
the rewriting evidence that already relates the original endpoints to their
normalized types. It neither decomposes a retained function nor assigns a
meta-variable. See Note [Reflexivity after retained normalization] in
`GHC.Tc.Solver.Equality`.

This supports family reduction and eligible ambient Given normalization under
the binder. It does not provide structural retained unification or runtime
index evaluation. Opaque indices remain stuck where a family needs
constructor discrimination, including bottom and cyclic runtime values.
See Notes [Normalising retained runtime indices] and
[Rewriting retained dependent functions], and
[the scoped Agda normalization sources](REFERENCES.md#agda-provenance).
Agda's conversion and eta rules do not add a Haskell function eta law.

Checked retained Pi expressions can appear in ordinary type arguments and
equality operands. Renaming and typechecking share their existing retained
telescope workers. Preserve the caller's mode and index environment. Require
complete types, supported closed domains, `Many` binders and lifted bodies.
No equality constructor name receives a special case. Check the whole visible
type argument for named, anonymous and generated holes. A hole beside the Pi
is also incomplete. Inspect expanded type-splice types without revisiting
expressions or finalizers.

Type synonyms follow this checked formation. Native scanners inspect their
RHSs. Ordinary meta assignment stays blocked through an alias. Literal
`foreach` kind syntax remains gated. A complete closed alias can be a kind
with an explicit specified kind argument. Ordinary inferred kind-meta filling
with that alias still rejects. This does not add universe levels.
See Notes [Retained Pi type formation] in `GHC.Rename.HsType` and
[Retained Pi type arguments] in `GHC.Tc.Gen.HsType`.

Direct equality transport can match an irreducible Given. Transport beneath
another retained binder needs the eligible inert rewrite described above.
This distinction separates checked type formation from Given canonicalization.
Neither path adds structural Pi unification or function eta equality.
See [the pinned Agda and GHC sources](REFERENCES.md#retained-function-types-as-equality-operands).

The D/DC specification keeps coercion abstraction during erasure. GHC's
existing zero-width coercion tokens preserve this arity. Native alternative
lowering removes only the extra virtual proof binder. It retains the strict
case, real fields and ordinary coercion slots. This implementation audit is
not an erasure proof. See [the paper comparison](papers/README.md#d-and-dc-icfp-2017).

Complete visible type arguments can refer to an already-bound runtime index,
as in `f @(Vec Int k)`. The renamer reuses lexical aliases from local signatures.
The typechecker validates the actual Id and seeds the VTA checking mode with it.
Erased type variables take precedence. Global terms, unsupported or linear
values, captured partial types and quotations reject. Ordinary wildcard type
arguments keep their existing behavior. Check anonymous wildcard nodes after
type-splice expansion as well as before renaming. Reject them in a VTA mode
that contains captured indices, before allocating wildcard metas. No runtime value is evaluated or
assigned to a native meta-variable. See Note
[Runtime indices in visible type arguments] in `GHC.Rename.HsType`.

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

The result fallback matches corresponding result arguments separately.
It selects the first candidate for each outer invisible binder. Every binder
must have a candidate. The complete substitution must satisfy the substituted
binder kinds and the existing local runtime-index scope check. No template
binder can remain free in the candidates. The fallback requires NoQL, an actual
data constructor or known ordinary Id, a saturated ordinary algebraic result
of the same datatype, and no pending type arguments, native template, or
constraints. Heads with concreteness requirements keep ordinary instantiation.

The fallback emits ordinary type applications and preserves the actual
substituted function type. `finishAppWithCache` still runs `checkResultTy`
against the original expected type. Ordinary subsumption must prove every
remaining equality. Candidate selection supplies no evidence. Both a false
`n :~: S n` proof and an unproved right-unit `Refl` must fail at the expression.
See Note [Retained instantiation candidates] in `GHC.Tc.Gen.App`.

The fallback does not assign retained syntax to ordinary argument
meta-variables. Argument-directed consumers use the separate immutable route below.

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

A complete expression signature can also supply an argument's checking type.
For example, nested inference of `const (42 :: Int) (rightZero n)` has no
expected result to determine the first parameter. Classify the signed argument
without elaborating it, then infer and cache it once with the other arguments.
Reuse `isCompleteHsSig`. Require no outer quantified variables. Reject written
inner quantifiers, retained binders, contexts and type splices in the source
type, including its kinds and multiplicities. Stop traversal at Names and
embedded Core types. Type synonyms can hide polymorphism; normal signature
checking remains responsible for their actual types. This is a syntactic
eligibility test, not evidence of type equality. Keep the existing complete
substitution, kind, runtime scope, exact-domain and final subsumption checks.
Calls without a native anchor retain their ordinary path. See
[the signature and application precedent](REFERENCES.md#signed-expression-arguments-in-native-inference).

Dictionary selectors can place another invisible forall after a context.
For example, `(>>)` has `forall m. Monad m => forall a b. m a -> m b -> m b`.
For matching, collect only the invisible prefix before the first visible arrow.
Keep the existing native anchor, saturation, complete substitution, kind, scope,
concreteness and exact-domain checks. Preserve the original sigma and emit the
selected types through IALL one group at a time. `instCall` emits each group's
evidence in its original position. Keep the choices in binder order because
outer substitution can freshen inner binders. All choices must be consumed.
Constraint generation remains inside `tcRetainedAppEvidence`. Flat routes and
the usage cache remain unchanged. See
Note [Immutable instantiation of nested contexts] in `GHC.Tc.Gen.App`.

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

This lambda rule checks a supplied type. It does not infer a native function
type or enable new retained type positions. M33 adds checked Pi expressions
in type arguments as a separate rule. General checking-dependent argument
inference remains separate work.
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
A known named worker can specialize its outer erased parameters from a complete
expected retained function type. Keep explicit type applications as an option.
Use the scoped matching rule below instead of assigning a native meta-variable.

A known retained function result can also determine the outer type arguments of
an ordinary polymorphic helper. Use the existing immutable result match before
allocating meta-variables. Require a known head without concreteness requirements,
all type arguments, kind agreement, valid index scope, and exact result equality.
Accept runtime Pi types through function arrows. Keep erased inner foralls and
other polymorphic type arguments gated. This lets a polymorphic bottom or `id`
receive the already known retained function type without eta expansion.

For a native worker template, use an opt-in immutable matching entry point.
Require a known head without concreteness variables, NoQL, no pending type
applications, supported value types, and a complete expected result. Corresponding
retained binders must have equal closed domains, `Many`, and bodies of kind
`Type`. Seed the matching scope with free variables of both input types.
Compare their bodies under paired binder renaming. Register each shared
binder in the existing escape check. An outer erased parameter must not capture
an inner runtime index. Keep runtime indices rigid and preserve family rules.

Every outer erased parameter must have a kind-correct, scope-valid solution.
Require exact equality between the instantiated result and the expected type.
Preserve the original context and emit actual type applications before ordinary
argument checking and final subsumption. Do not evaluate indices, inspect
unfoldings, or eta-expand the worker. General unification and the default matcher
keep their existing retained-function gates.

A fully applied real constructor can project its result through validated
retained field metadata. Traverse the current instantiated signature in
field order. A retained entry consumes its native Pi, or its canonical
ordinary Many arrow when that binder is unused. An ordinary entry consumes
a visible arrow. Match only the outer erased parameters against the complete
expected result. Keep the actual telescope for argument and proof checking.
Partial VTA must use the current signature's substituted variables. This
route neither chooses a stored runtime index nor supplies equality evidence.
See Note [Expected retained worker instantiation] in `GHC.Tc.Gen.App`.

A constructor branch can transport a captured closure type through a retained
binder with an unchanged domain. Abstract the outer index only in the body.
Preserve the inner index scope. Existing FC lifting builds the fixed-domain
`DepFunCo` from the checked branch proof. Cast the function value without eta
expansion. Keep changed domains and dependent erased foralls gated.

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

The default immutable matcher can bind a whole retained function type to a template
variable. It retains the usual occurs and escape checks. Unsupported retained
comparisons produce `MaybeApart MARRetainedFunction`. Alpha-equivalent retained
types without free TyCoVars can compare reflexively. This does not add structural
matching under Pi or assign Tc metavariables. See Note [Matching retained function types].
The opt-in worker matcher additionally compares fixed-domain retained bodies
under paired runtime binders. It returns only an immutable substitution.

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

Substitution of an expression accepted by the checked index bridge produces
its corresponding index. This includes supported variable/constructor forms
and the bounded beta steps below. Reject other expressions explicitly.
Do not leave a stale variable behind.

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

Check the scrutinee first. Derive its index with the same checked bridge
as retained application. Do not derive evidence from an unrelated variable,
the case binder, an unfolding, or an unsupported producer expression.

A proof-bearing Boolean alternative must bind exactly one fresh local `CoVar`.
Its type must be the homogeneous nominal primitive equality from the scrutinee's
index to that exact promoted constructor. Check its type before adding it to
scope. Check its right-hand side with the proof in scope. Keep the proof outside
all other alternatives and the result annotation. Ordinary Boolean alternatives
with no binders keep their existing rule. `DEFAULT` supplies no constructor
refinement. It can carry the separate checked index-alias proof described in
the constructor-naming section.

Require exhaustive alternatives whenever an algebraic alternative binds a proof.
All constructors of its domain or a valid `DEFAULT` must cover the input.
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
skips occurrence analysis, general inlining, general compulsory unfolding, binding
dropping, eta reduction, and cast pushing through lambdas. It also disables
generic known-constructor case elimination.
It creates no rules, Core unfoldings, or join points. Unfolding construction
would invoke ordinary occurrence analysis again. Existing join points remain.

Dependent beta reduction can substitute an accepted local index variable or
an accepted constructor expression in the current index grammar. An unsupported
producer keeps its application redex. A virtual Boolean-proof case can reduce
only when its scrutinee is an exact
syntactic Boolean constructor. The substituted proof must be nominal primitive
equality whose endpoints are that same promoted constructor. Replace that proof
with checked reflexivity before substituting into the selected branch.

Keep variable cases and lazy source lets. Do not inspect unfoldings to decide an
opaque index. The current mode permits the existing compulsory templates for
wired tuple sections and native newtype wrappers. These exceptions expose
required wrappers; they do not authorize arbitrary unfolding. These rules
implement dependent substitution and checked case
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
This keeps unsupported native types out of synonym and class checks. Constructor
declarations use the separate field-telescope path above. Reject partial signatures and embedded retained
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

Normalize an index Id's assigned type and multiplicity metadata before these
checks. Preserve its Name and Unique. Validate immutable choices against the
current lexical or same-module binding. Imported GlobalIds, linear indices,
unresolved domains and stale bindings remain invalid. A known ordinary Id can
also supply an algebraic result candidate when its index is determined but
an ordinary payload type is unresolved. Keep the actual instantiated type.
Final subsumption solves ordinary payload equalities and verifies all index
equalities. This does not establish equality between independent runtime Ids.

A fully applied wired-in `($)` can use this immutable path with a complete
known result and a named native-anchored right operand. A fixed result can
come from the expected type or the first operand's declared ordinary result.
The checker reads that signature without elaborating the first operand.
It matches kinds before types, checks every type choice and original concrete
representation requirement, then instantiates the original operator. It uses
NoQL through final checking and emits the cached right operand's usage once.
Higher-rank callback domains retain expected checking. A failed candidate
keeps its already elaborated operand on one NoQL fallback path. General
native Quick Look and mutable native type assignments remain gated. See
Notes [Native index bindings during typechecking] and
[Immutable native dollar applications] in `GHC.Tc.Gen.App`.

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


## Constructor equations with later retained indices

The frontend consumes later retained binders in telescope order. Each position
gets one shared unrestricted runtime Id. The checker instantiates that binder's
codomain before reading the next argument. An argument plan distinguishes
ordinary payloads, evidence and retained indices.

Each equation must use bare variables at later retained positions. The checker
renames those resolved Names across patterns, RHSs, local bindings and stored
free-name sets. It preserves source spans and `WithUserRdr` spelling. It stops
at embedded Core types. Quotations reject at their expression spans before
renaming. Error contexts retain the original source equation.

Shared index patterns remain outermost variable patterns. Their closed domains
require reflexive constructor transport. A coercion pattern would allocate a
fresh outer lambda Id and lose runtime identity. Evidence keeps its separate
specialised Givens. Ordinary payloads and results keep nominal constructor
transport. All source argument and dictionary lambdas precede discrimination.

A qualified signature can contain another retained binder. The syntax selector,
renamer and kind checker continue through its body. Existing context routines
check its constraints in the current index scope. Later binders enter only the
body. Ordinary qualified bodies without retained syntax use the existing kind
checker. See [the exact provenance](REFERENCES.md#later-retained-arguments-in-constructor-equations).
No new Core rule or interface schema is required.

## Qualified retained function types

M34 permits a qualified body such as
`foreach (n :: Nat) -> Eq Int => Vec Int n` in the checked native type positions.
In an equality operand, `ImpredicativeTypes` admits this type but normally
enables Quick Look even for a bare variable. A known variable can need
conversion without type instantiation.

Select `NoQL` only for an actual `HsVar` with no arguments and no leading
`forall` or context. Require native syntax in its actual type or an available
expected type. Zonk these types and use the native-aware `tyCoVarsOfTypesList`
traversal to exclude free QL instantiation variables. Do not use the unsupported
QL traversal for this guard. In `Check` mode, retain final subsumption and
native evidence capture. In inference mode, return the variable's existing
type through `InferResult`. Read an available expected type without allocating
an inference cell. Preserve promotion, level, runtime-representation and branch
checks. Do not eta-expand the variable. Other applications keep their QL guards.
See Note
[Known native variables with ImpredicativeTypes] in `GHC.Tc.Gen.App`.

The immutable native Given RHS check can recurse through an invisible
dictionary arrow. Check its multiplicity, predicate and result with the same
flags. Preserve the complete `FunTy` reduction. This checks an existing Given
with a type-variable LHS; it neither solves a dictionary obligation nor assigns
a type metavariable. On this path, expand synonyms with `coreView` before the
`isTauTyCon` rejection. Recheck the expanded RHS so occurrences in predicates
and results remain visible. Other checks retain their ordinary synonym policy.

Keep the existing role, occurs, multiplicity and predicate checks. Preserve the
scoped Given rewrite filter beneath retained binders. Disable native family
cycle breaking because restoring a cycle breaker writes a metavariable.
Ordinary erased `forall` handling stays unchanged; the native Given RHS check
still rejects `ForAllTy`. Wanted and mutable native-meta checks keep their
restrictions. `Coercible` does not establish nominal equality. See Note
[Rigid retained indices] in `GHC.Tc.Utils.Unify`.

Dictionaries use the existing ordinary evidence arrows. This increment adds
no Core constructor or interface schema. Agda's telescope and instance-argument
rules guide context order and scope. They do not identify Haskell dictionaries
with FC equality evidence. Quick Look's `h-var` and `iresult` supply the
no-instantiation precedent (Figures 3–4, printed pages 89:7–89:8). Its algorithm
retains final result equality (Figure 7, page 89:19) and dictionary entailment
(section 8, pages 89:21–89:22). Section 5.8 records the semantic effect of eta
expansion (page 89:15). These are design precedents, not a soundness theorem for
our native types. See [the exact provenance](REFERENCES.md#qualified-retained-function-types).

Complete expected types also select ordinary parameters through saturated
lifted algebraic containers. A boxed equality witness can therefore contain a
qualified retained function type. Each container argument keeps the incoming
Pi scope. A Pi in one field does not admit a qualified sibling. Erased foralls,
newly non-tau class and family arguments, unsaturated heads and nonlifted
containers keep their gates. See Note [Known retained function instantiation]
in `GHC.Tc.Gen.App`.

## Native worker arguments

A full worker application can need information from a later argument:

```haskell
withQ :: forall value. (value :~: QPi) -> value -> QPi
withQ proof f = case proof of Refl -> f

via :: QPi -> QPi
via f = withQ Refl f
```

The final result does not determine `value`. Under `NoQL`, infer the known
`f` once and match its type against the original native-free domain `value`.
Select `value := QPi`, then check `Refl` against the complete witness type.
Apply this mode only to a known head without concreteness requirements. Its
original body or context must contain native syntax. Require an independently
known native argument or an immutable expected-result choice.

Match kinds before types with the same restricted immutable substitution.
Require every invisible parameter, exact cached domains, substituted kinds
and current runtime scope. Reject template escape. Do not infer callback
results or create fresh missing parameters in this mode. An overapplied call
uses only its declared ordinary prefix. It cannot seed choices from the final
result after the extra runtime arguments.

Keep inferred expressions and usage on failure. The ordinary fallback must
consume the same cache. The existing ordered plan handles consecutive forall
and context groups before the first visible arrow and after leading VTA.
Keep original evidence applications, argument subsumption and final result
checking. No mutable meta receives native syntax. Runtime argument order and
function eta rules stay unchanged. See Note [Native worker argument inference]
in `GHC.Tc.Gen.App` and [its provenance](REFERENCES.md#native-worker-argument-inference).

Qualified Pi type arguments still require `ImpredicativeTypes`. Its applied
Quick Look path is separate from this `NoQL` route. An erased forall after a
visible argument also remains outside the collected telescope.

## Fixed and declared choices with ImpredicativeTypes

The `DoQL` entry prepares the leading written type-argument prefix once.
It preserves the original head, residual signature, wrapper/evidence accumulator,
and any allocated QL variables. A failed mode choice resumes with that prefix.
It does not recheck the source or infer value arguments during mode selection.

Fixed native applications can select ordinary checking when no choice or free
QL variable remains in the residual, expected type, or checked prefix metadata.
The existing immutable expected-result matcher can also select complete choices
for a supported native proof type. Final checking keeps the actual substituted
result and nominal entailment. These routes have priority over argument advice.
See Note [Ground retained applications] in `GHC.Tc.Gen.App`.

Otherwise, a declared monomorphic argument can provide complete erased choices:

```haskell
knownDirect :: foreach (n :: Nat) -> QPi -> Vec Int n
knownDirect n f = withQ Refl f n
```

Read only bare variable types, with optional parentheses. Exclude constructors,
leading foralls/contexts, unsupported types, and free QL variables in deep
metadata. Global lookup can load an interface. The lookup does not call
`tcInferId`, elaborate an expression, or charge an occurrence use. A linear
value remains eligible; its eventual argument check records its actual use.

Match eligible domains and declared types with one restricted kind-first
substitution. Check every known pair that constrains a collected outer erased
binder. Require all choices, exact substituted domains, matching kinds, valid
runtime scope, no template escape, no free QL variables, and a native anchor.
An expected result can seed this matching only at ordinary saturation. Extra
arguments can consume the selected result later; their final expected type
cannot stand for the original worker's result. Keep the no-concreteness guard.
Signed/applied expressions supply no declaration advice in this mode.

Carry successful choices in `RetainedInstPlan`. Keep the original signature
and prefix so IALL consumes forall/context groups and emits evidence in their
original order. Skip eager argument inference when this plan is present.
Every argument then goes through ordinary scaled checking. Final nominal
entailment, result subsumption, usage, and failure-cache ownership remain.
A conflict retains the guarded fallback. An erased forall after a visible
arrow stays outside this collected telescope. No native metavariable assignment
or Core/interface change is added. See Note [Declared native argument choices]
in `GHC.Tc.Gen.App` and
[the primary-source record](REFERENCES.md#native-choices-from-declared-arguments).

## Core research correspondence and remaining obligations

The C1 audit compares this implementation with three distinct results. D/DC's
An-PiCong supplies a precedent for `DepFunCo` with an unchanged domain.
System DR supplies role-indexed equality obligations; dependent arguments require
nominal equality. GraD separates type dependency from runtime usage. None supplies
a ready-made theorem for the combined GHC implementation. The current rules
specialize these ideas to eligible closed domains, Many indices, explicit FC
coercions, and the existing checked backend.

A larger index language must preserve substitution, scope, result kind, role,
constructor consistency, and runtime erasure. An actual strict branch justifies
constructor evidence. A DEFAULT alias justifies identity with its scrutinee.
These rules need their own correspondence argument. General changed-domain
congruence, residual term expressions in types, optimizer substitution closure,
and normal unfolding support remain open. An open evaluator must not assume a
complete terminating equality procedure for arbitrary Haskell.

### Checked beta exposure

`normaliseNativeIndexExpr` returns a normalized runtime expression and its
checked index. It accepts an immediate `App (Lam b body) arg`. Require an
eligible local Many binder and the same supported closed domain for the
binder, argument, and body result. Normalize the argument first. Its result
must be an eligible variable or a nullary constructor worker. The body must
normalize within the existing variable/constructor grammar. Nested accepted
beta steps can occur in either operand. Other expression forms remain gated.

For scoped Nat variables, identity applied to `n` returns that actual `n`.
Successor applied to `n` returns `S n`. Constant zero applied to `n` returns
`Z`, even when the runtime binding of `n` is bottom. The walker does not inspect
unfoldings or evaluate runtime values. Its stored substitution ranges are
already normalized. Return each range directly. Do not substitute through it
again under an inner binder that can shadow one of its free Ids.

The atom restriction preserves the sharing of an argument used more than once.
The bridge rejects `S n` as a beta argument, even when the body ignores it.
The caller must still check the original expression with Core Lint. Reduction
must not hide an ill-typed or out-of-scope argument.

CorePrep consumes the normalized pair before naming. A normalized variable
remains the same actual Id. An alias case would force an unused bottom.
Normalized constructors use the existing scoped naming and nominal transport.
Type recovery, substitution, case evidence, interface validation, and
`mapNativeIndex` use the same bridge. This adds no coercion constructor or
residual expression form in `Type`.

See Note [Checked native index beta exposure] in `GHC.Core`, the M39 spec in
PLAN.md, and [the pinned Agda and paper references](REFERENCES.md#core-contract-audit-and-bounded-beta-exposure).
The source subset below uses this bridge. General promotion, ordinary optimizer
support, and a general open evaluator remain separate tasks.

### Bounded source beta applications

The source checker accepts an immediate single-variable lambda application in
a retained argument. For example, the public client defines:

```haskell
betaSuccessor :: foreach (n :: Nat) -> Int -> Vec Int (S n)
betaSuccessor n x = replicateNative @Int ((\k -> S k) n) x
```

The retained binder supplies the lambda's known domain. Resolution uses a
separate Name-to-Id environment. Each provisional Many binder uses the actual
renamed Name/Unique. Resolve the argument in the outer environment. Resolve the
body in the extended environment. Constructor worker arrows supply each field
domain. The Core bridge then validates saturation, domains, and atomic beta
arguments. It removes all provisional lambda binders from the resulting index.

Keep the original renamed argument for ordinary checking. `tcValArg` checks it
once and records usage. Resolution neither installs provisional Ids in the Tc
environment nor changes the source expression. At M40, both compiler stages
inspect six actual zonked lambda binders before desugaring. With `LinearTypes`, their
Nat domains, Many multiplicities, and renamed identities match.

Require one VarPat, one unguarded RHS, and no local bindings or signatures.
Reject this form under `Strict` with GHC-83605 at the lambda span. Strict can
make a variable pattern force an argument that a constant body ignores.
Bang, lazy, wildcard, constructor, and signature patterns remain unsupported.
Opaque named functions remain unsupported even when their definitions are
simple. This rule does not unfold them or add general term syntax in types.

Ambient indices must already meet the existing Many rule. An unsigned captured
local alias can still have an unresolved multiplicity while its body is checked.
M39 and M40 both reject that case. A complete local Nat signature supplies a
Many Id and permits the checked beta argument. Do not default or force the
ambient alias's multiplicity during resolution. This differs from final
zonking of the new bounded lambda after ordinary argument checking.

See Note [Retained argument application] in `GHC.Tc.Gen.App`,
[the source provenance](REFERENCES.md#bounded-source-beta-applications), and
`IndexBetaClient.hs` in the public qualified-function example.

### Checked native variable aliases

M41 removes `let alias = actual in body` in the restricted native simplifier.
The input RHS must be literally `Var actual`. Both Ids must be eligible local
Many indices. The current substitution must resolve the target to another
eligible variable of the same closed domain. Constructor and nonvariable
ranges retain the existing path. Joins, recursive and top-level bindings,
exported aliases, stable unfoldings, rules, inactive inline activation and
loop-breaker aliases remain excluded.

Store the same actual Id with `extendIdSubstWithClone`. This helper allocates
no Id. It maintains the in-scope set and the variable substitution. Clear a
stale alias entry in the inlining environment. Existing traversal substitutes
terms, binder types, Type arguments, coercions and native indices together.
Surviving binders use the existing capture-avoidance rules. The original Core
must pass Lint. `lintLetBody` rejects a local index that escapes in its result
type. The direct checks compare input and output types with alpha equality.

An outer `let actual = opaqueProducer x` remains lazy and shared. The rule
does not unfold that producer or inspect the target's unfolding. It creates no
constructor, equality evidence or runtime check. Ordinary optimization stays
disabled. Direct checks cover type-only dependencies, repeated Id identity,
reused-Unique capture, fallback cases and lazy demand before CorePrep.
The source alias multiplicity boundary is unchanged. See
Note [Restricted dependent simplification] in `GHC.Core.SimpleOpt` and the
[M41 plan](PLAN.md#m41-checked-native-variable-aliases).

## Opaque native call indices

`TermAppTy f arg` records an opaque runtime call. The actual head `f` must be
local, unrestricted and nonjoin. Its raw type and multiplicities must be closed.
Its type must expand to an ordinary `FTF_T_T` arrow `A %Many -> B`.
A and B are independently closed scalar native domains. They can differ.
The argument is any scoped, well-kinded `Type :: A`. The call has kind B.
Type formation can represent stuck family arguments and nested calls.

`TermAppCo f co` supplies forward nominal congruence for the same actual head.
Its child proof is nominal. Both child endpoints have kind A.
The call endpoints have kind B. `mkKindCo` returns reflexivity at B.
The smart constructor removes structurally reflexive children. It preserves
other proof dependencies. Representational use requires ordinary `SubCo`.
Equal call results supply no argument equality or function equality.
Different unresolved calls remain `MaybeApart`. Call arguments are not injective.

Type formation and runtime conversion have different domains.
`exprToNativeCaseIndex` admits a strict call with an eligible variable head.
Its argument uses the existing scalar variable, constructor and checked-beta
grammar. The bridge does not evaluate a head or inspect its unfolding.
`exprToNativeIndex` remains unchanged. General symbolic type arguments need
not have a runtime representation in this subset.
The strict case binds nominal evidence about its whole computed result.
CorePrep preserves that evidence when it names constructor arguments.
Forward call congruence composes the argument proof with the case proof.
The actual call preserves function-before-argument demand and lazy fields.

Head occurrences are free variables in types and coercions. Traversals preserve
them through alpha renaming, zonking, tidying and type-map keys.
Substitution accepts an eligible variable head with the same input and result
domains. Composition updates head ranges. Lookup uses each stored range once.
Capture avoidance freshens a bound head instead of capturing a free replacement.
The pipeline retains unsupported function bindings. It rejects unsupported
head substitution by a lambda, cast or arbitrary expression.
Core Tidy gives supported top-level heads lazy local aliases before globalising.
The M41 checked alias rule also removes eligible literal function-head aliases.
Its metadata exclusions and lazy producer behavior remain in force.

Function-domain retained Pi binders, new interface wire forms and general lazy
retained-call naming remain gated. M43 source formation is described below.
`mkLamType` and Lint reject a function head that escapes in a lambda result.
Diagnostic interface forms support printing. Binary writing and importing
reject them. Ordinary optimization and unfolding gates remain in force.
This kernel does not complete C1 or C2 and supplies no preservation theorem.

Both compiler stages build. The existing API fixtures pass at bootstrap and
in the self-hosted registered group. Nineteen quiet runtime invocations check
actual proof use, mixed Bool/Nat domains, bottom order and constructor naming.
Five use the direct SimpleOpt-to-CorePrep pipeline with input and output Lint.
Other controls check scope, capture, substitution ranges and normalization at
all three roles. The one-line and 37-line goldens remain unchanged.
Top-level Tidy protection is implemented and reviewed. It has no separate
isolated execution probe.

See Note [Opaque native call indices] in `GHC.Core.TyCo.Rep` and
Note [Opaque call arguments in CorePrep] in `GHC.CoreToStg.Prep`.
The [M42 plan](PLAN.md#m42-scoped-opaque-function-call-indices) records measured
results. The [M43 source contract](#strict-opaque-call-refinement-from-source)
extends this kernel through the checked source path below.


### Strict opaque-call refinement from source

M43 admits a lexical function head in a complete checked type context.
The head must be the actual local Many, nonjoin Id with the closed ordinary
`A %Many -> B` type required above. A complete local function signature can
supply this head. For example, `Vec Int (f n)` records the whole call.
The checker uses the registered term Names and checks the type argument at A.
It does not form matchable `AppTy` from a runtime function or unfold its body.

Checked type formation permits more arguments than runtime case recognition.
A type argument can be any scoped, well-kinded `Type :: A` admitted by the
checker. The source case path requires one checked `HsApp` with a bare local
function head, allowing parentheses. M43 required a variable argument. M44
also accepts saturated scalar constructor trees as described below.
A bare function is not a scalar index.

A complete constructor case on `f n` refines that exact whole call.
It does not refine n, another call `f m`, or a call with a different head.
The result type, captured values and incoming predicates are abstracted over
one scalar motive index. Ordinary nominal FC congruence lifts the alias proof.
Branch-local annotations at the exact call are specialized to the checked
constructor index. The checker resolves both Names before this replacement.
It rejects quotations before a rewrite could change quoted source.

The typed-only carrier is
`NativeCaseAlias scrutinee r q resultType body`. The scrutinee and result type
are outside the scopes of r and q. The body is under r, then q, with
`q :: r ~# f n`. The existing retained constructor matcher checks the motive
with a fresh motive index. Its application uses r. The final transport returns
its checked result at the original call index. There is no inverse call
decomposition or new trusted equality.

The dispatcher checks the original scrutinee and its usage once. It preserves
the original source case through `ExpandedThingTc`. Zonking keeps the carrier's
scope order. Desugaring emits one strict outer Core `Case` with one DEFAULT
alternative and q as its proof binder. This case stays outside returned
lambdas. The call evaluates the function first. The argument is forced only
when the function needs it. Constructor fields stay lazy.
Each source case supplies its own evaluation; this is not
memoization across separate cases.

Under `Strict`, generated capture and evidence patterns use administrative
`LazyPat` markers. Desugaring removes these markers instead of adding bangs.
Original constructor and field patterns keep their source demand.
The ordinary wildcard-only and variable-only case paths can ignore the call;
they do not acquire the strict alias. Supported constructor cases require
complete, unguarded rows and bare variable fields. Instrumentation that could
insert ticks around the call is rejected at the source location.

`seq` also preserves native identity. When its continuation contains native
syntax, desugaring does not reuse a scalar index or opaque function head as
the case binder. An unused binder keeps the actual free Id in the continuation.
The `NoMetadata` expression scan detects these dependencies without following
unfoldings or treating unrelated Id metadata as executable syntax.
See Note [Desugaring seq] in `GHC.HsToCore.Expr`.

An observer such as `toList ys`, with `ys :: Vec Int (f n)`, uses the existing
immutable index-kind application route. It infers and caches ys once, matches
the formal index to `TermAppTy f n`, then validates the kinds and current scope.
The original observer receives explicit type applications. Cached argument
subsumption still runs. The mutable native-index assignment ban is unchanged.

Both compiler stages pass the separate source corpus of 26 quiet runtime
checks. These run through the API fixture and the registered group. The M42
19 Core runtime invocations remain a separate set. The one-line unit golden
and 37-line runtime golden are unchanged. The nine final regression phases
reuse two passing API checks with their own input hashes and launch seven
checks: raw106, public106 and packed562 in both stages, plus five registered
tests. They do not repeat every historical source matrix.

The separate CLI corpus passes nine runtime observations and rejects 14 source
clients in both stages. Six clients report GHC-83865; eight report GHC-83605.
Their diagnostics contain 24 located envelopes. The arity client's `f n n`
annotation becomes `'Z n` during branch rewriting. Ordinary kind checking
reports GHC-83865. This does not isolate direct call-arity validation.
A supplementary program passes 15 observations in both stages: the same nine
public lines, signed local-function results at lengths 0/2/3, and three opaque
head `seq` demand controls. Its implicit `toList ys` succeeds. The earlier
supplement's GHC-83865 was a mistaken RuntimeRep application to `error`, not a
native-meta boundary. The corrected lifted error wrapper preserves demand.

Function-domain Pi binders, lazy retained application of `f n`, new wire
encoding, a general evaluator and ordinary optimizer support remain gated.
Type formation and checked case refinement do not remove those restrictions.
This increment does not complete C1/C2 or establish a preservation theorem.
See Note [Source opaque call indices] in `GHC.Tc.Gen.HsType`,
Note [Source opaque call refinement] in `GHC.Tc.Gen.Match`, and
Note [Checked opaque case alias] in `GHC.Hs.Expr`.
The [selected Agda reading](REFERENCES.md#strict-opaque-call-refinement-prior-art)
supports scoped abstraction and checking, with Haskell demand checked separately.

### Checked constructor arguments in source calls

M44 exposes the existing Core bridge through `case f Z`, `case f (S n)` and
nested constructor arguments. The head remains the actual eligible local
Many, nonjoin Id with independently closed scalar input and result domains.

`checkedNativeCallArgument` reads the already-checked HsVar, ConLikeTc and
HsApp nodes. It reconstructs a temporary spine with the actual Ids and
`dataConWrapId`. It never replaces a real wrapper with a worker. The existing
native domain predicate excludes constructors with wrappers and unsupported
fields. `exprToNativeIndex` checks saturation, domains and recursive fields.
The classifier does not execute the temporary spine or repeat source checking.

Parentheses, WrapExpr WpHole and the expression inside a checked source
expansion are transparent. On the M44 path, argument expression signatures,
nonidentity wrappers, casts, type/evidence applications, pattern synonyms,
lambdas and opaque producers get no new rule. M45's signed exception is below.
The original scrutinee remains in `NativeCaseAlias` and is desugared once.

The renamed annotation matcher compares its application spine with the checked
index. It requires identical variable and promoted DataCon Names, arities and
recursive fields. It ignores parentheses. It expands no general synonym and
infers no input from the function result. The transport substitution includes
all free dependencies of the complete call, including constructor fields.
Existing quotation, capture, evidence, scope and instrumentation checks remain.

CorePrep already names nested constructor arguments and composes child proofs
under `TermAppCo`. This source increment changes no Core or backend rule.
A constructor argument can reach WHNF without forcing its fields. Under Strict,
existing administrative capture protection remains; source patterns keep their
demand. M44 does not reject Strict as a whole.

Both stages pass 17 source observations, 17 negative clients and their 37 located
diagnostic envelopes. These counts are separate from the old source 26 and
Core 19 controls. The four legacy checks pass the public nine-line program and
15-line supplement in both stages. The final nine phases preserve raw/public
106 and packed 562 observations with the existing five registered tests.
Freeze and independent audit must pass before publication.

See Note [Checked source call arguments] in `GHC.Tc.Gen.Expr`,
Note [Source opaque call refinement] in `GHC.Tc.Gen.Match`, and
[the pinned readings](REFERENCES.md#checked-constructor-call-arguments-prior-art).
Ordinary optimization, lazy retained calls, function-domain retained binders
and call-index interface encoding remain gated.

### Signed beta arguments in source calls

M45 adds an immediate checked lambda application with a surviving explicit
ExprWithTySig. The complete source signature has one plain A -> A arrow and
the same bare, optionally parenthesized TyCon Name on both sides. Aliases,
applied domains, wildcard/forall/context signatures and nontrivial wrappers
remain unsupported. The outer opaque function retains its closed A -> B policy.

Require one actual LamSingle row, a bare VarPat, an unguarded RHS and empty
local bindings. The during-TC zonker reads existing assignments in the actual
binder and MatchGroup. Flexi remains unassigned; only existing Indirect chains
can be compressed. Both actual binder and scaled argument require Many.
Binder, argument and result domains agree. NameEnv maps each bound occurrence
to that zonked actual Id, with its original Name/Unique. External M44 Id/head
eligibility stays unchanged. No provisional Many Id or second usage check is
introduced.

A temporary Core.Lam feeds the existing scalar beta converter. Its normalized
operand is an eligible local variable or nullary constructor worker. Constructed
operands with fields remain unsupported. Supported nested scalar beta/constructor
bodies can yield the canonical index for exact whole-call annotation matching.
The runtime scrutinee is still the original checked expression. It is neither
replaced by the temporary Core nor evaluated during classification.

Under Strict the classifier returns Nothing. An ordinary nondependent case
keeps its original source demand through the usual fallback. A dependent case
can receive the existing located restriction. This is not general Strict beta,
lambda inference, function unfolding or inverse call equality.

Both stages pass public15 and eight negative clients, with 15 located envelopes
(nine GHC-83865 and six GHC-83605). Five clients have both codes; one is an early
local-signature scope error and two test mismatches after valid refinement.
The API's 15 quiet source observations are separate from source17/source26/Core19.
Strict2 and same-spelling scope2 supplement them. Two trace markers count 1/1;
this is not a universal elaboration count. Goldens remain 1/37. Legacy6 and
final9 pass, retaining raw/public106 and packed562 in both stages and the five
registered tests. Freeze and independent audit must pass before publication.

See Note [Signed source call beta] in GHC.Tc.Gen.Expr and
[the pinned readings](REFERENCES.md#signed-source-call-beta-prior-art).
M45 left function-domain retained binders for the M46 rule below. TermVarTy
remains scalar-only. Call-bearing interfaces stay gated.

### Function-valued retained binders

isNativeRetainedBinder accepts a scalar index Id or an existing eligible opaque
head Id. isNativeRetainedBinderType checks the domain without a fabricated Id.
A function head has a closed ordinary visible Many arrow A -> B. Both A and B
are independently supported scalar domains. The Id is local, unrestricted and
not a join point. Scalar predicates and TermVarTy remain scalar-only. M51
adds function-valued constructor fields under the separate policy below.

DepFunTy f body scopes f over calls in body. The body has kind Type. Within types and coercions, f occurs only in TermAppTy/TermAppCo head slots.
It is not a scalar index or a matchable type function. A vacuous retained binder still becomes an ordinary
arrow. Lambda type recovery uses this extended binder policy.

At a function-valued retained binder, application accepts only an eligible
local Var g at the exact domain of f. Its result is body[f := g], using the existing Id substitution. Lint checks
both the occurrence metadata and the actual scoped binding. Substitution masks
bound heads and freshens capturing binders. Free-variable, equality, type-map
and tidying traversal retain their existing runtime-binder scopes. Immutable
matching stays opt-in; outer templates cannot capture the bound function head.
No call-argument decomposition is added.

DepFunCo binds the same function head in its body proof and endpoints. Its
domain remains fixed and reflexive. Existing lifting and normalization can
abstract the body proof under that scope. They do not cast the function domain,
compare different runtime functions, or infer equality from equal call results.
CorePrep substitutes a supported Var directly. It does not form TermVarTy g or
add a strict scalar DEFAULT alias. A signed local lazy function alias can remain
shared. Unsupported non-variable substitutions keep the existing gate.

M46 first limited source permission to complete local value signatures. M52
also admits complete ordinary named value signatures, including exported
bindings. One equation uses bare variables in retained columns. Source actuals must be
already-bound unrestricted local function variables of the exact checked type.
An actual variable does not need its own written local signature. Bare function
indices, global or applied actuals, unsupported domains, staging and native
mutable-meta assignments keep their gates. M46 added no export or expression
codec. M52 reuses the existing interface type forms. The M46 public client
instantiates caller = idCall f n outside a case.
Direct in-branch probes2/3 reject two payloads with GHC-83865 in each stage
at lines 33/37. These M46 probes have no runtime result. The M47 section below
records later direct checking.

Note [Function-valued retained binders] in GHC.Core.Type owns the policy.
Note [Function reduction multiplicity roles] in GHC.Core.Reduction owns an
ordinary role correction found by the new tests. Multiplicity normalization
still supplies a nominal reduction. A Phantom FunCo requires Phantom stored
multiplicity evidence; mkFunRedn weakens only that proof with funRole. Its
normalized type and nominal/representational behavior do not change.

Snapshot5 API2 passes the function-binder and ordinary multiplicity-family
controls at N/R/P. Source14, ten negative clients, constructor11 and legacy8
also pass their separate checks. Final1 passes its first eight phases; one
registered test mismatches the existing scalar error text. Snapshot6 restores
that wording without changing a golden; function wording applies only to
isPiTy. Selected API3 and both-stage builds pass. CLI bootstrap4/self-host2
pass public14 and reject ten clients each (nine GHC-83605, one GHC-83865;
ten error envelopes per stage). Constructor supplements3 pass all eleven in
each stage. Direct probes3 have two source errors per stage and no runtime.
Legacy2 passes eight phases. Final2 passes nine phases, including both-stage
raw/public106 and packed562, with registered five passes/39 cases/34 skips and
no unexpected/framework failures or warnings. These compiler/runtime checks
are complete. Canonical audit/publication records are separate; component
checks do not establish combined preservation.

## Remembered equality for direct call application

After whole-call generalization, the generated motive takes
`q_motive :: i ~# f n`. This is the first ordinary evidence argument after its
index `i`. Keep `f n` on the RHS; do not generalize it to `i`. At the outer
strict alias `r`, supply `q :: r ~# f n`. The alias Id and motive Id are distinct.

Existing constructor transport derives `q_branch :: K ~# f n` from that
parameter and checked `cv :: i ~# K`. The existing `checkConstraints` scopes
the Given over the complete row, including its `where` bindings. A direct
application can expose `f n` after actual retained substitution. Ordinary
argument or result checking then emits a Wanted equality. The solver matches
the exact or symmetric irreducible Given. It does not decompose a call or
infer equality between independent inputs or function heads.

Normal coercion-hole filling records evidence use. The solver therefore keeps
the required branch binding during dead-Given pruning. Add no manual App cast
or lexical proof list. The selected implementation restores those experimental
paths to M46. It introduces no new whole-residual NoQL/isTau gate. Existing
application, native-meta, scope, usage and staging checks retain their rules.

Keep `q_motive` in the ordinary evidence-token ABI. Only `cv` occupies the
existing virtual constructor-proof slot. The strict alias and its demand rules
remain unchanged. M47 requires result, capture, Given or annotation information
to activate the retained case. M48 below adds a metadata-only trigger.

Given-only bootstrap, self-host2 and selected API4 pass. The quiet fixture
checks eleven values, three runtime exception markers and three located
GHC-05617 errors. The old 1/37 goldens and both API4 input sets are unchanged.
Selected baseline3/public17/where3 CLI checks pass in both stages with Core/STG
Lint. Raw scope run2 passes 36 pure utility assertions. Legacy8 and positive
constructor11 pass separately. Final9 passes two API4 reuses and seven launched
phases. Raw/public106 and packed562 pass in both stages. The registered run
has five expected passes, 39 cases and 34 skips, with zero unexpected/framework
failures or warnings. These focused checks do not cover the full GHC suite.
Artifact audit and publication remain separate.

M47 adds no Core/interface schema, codec, general optimizer or evaluator.
C1 and C2 remain open. Note [Source opaque call refinement] in GHC.Tc.Gen.Match
owns this evidence order. See [the plan](PLAN.md#m47-remembered-equality-for-direct-call-application)
and [the Agda comparison](REFERENCES.md#remembering-an-abstracted-call-prior-art).

## Known-worker case activation

M48 adds trigger discovery in Match. Existing result, capture, Given and
annotation triggers run first and keep their behavior. The new metadata rules
apply only when those triggers are absent. They are not a new general App gate.

First require complete, unique, unguarded constructor alternatives with bare
field variables. The metadata path does not activate through quotations.
Look up an already checked lexical worker. Read only its leading retained
`DepFunTy` telescope. Every consumed source actual must be an eligible bare
local Var of the exact binder domain. Parentheses do not change its identity.
Substitute actual Ids with `applyTypeToArgs`. Require `isTauTy` for the whole
residual and an exact free occurrence of the checked call. Bound/shadowed
identities keep their existing meaning.

Do not infer argument types, instantiate erased choices, assign metas, collect
usage or expand splices during discovery. Do not enter splice/quote subtrees
or opaque expression extensions. Unknown workers and unsupported forms decline
silently to the ordinary case path. Existing activation still keeps its own
diagnostics. Actual application checking and usage accounting remain ordinary.

Once triggered, reuse the M47 motive equality and constructor Given across the
complete row, including `where`. No new proof, App cast, solver rule, Core node
or backend step follows. Complete worker signatures are still required.
Note [Source opaque call refinement] in GHC.Tc.Gen.Match owns the policy.

Bootstrap1, fresh/freshwhere attempt2 and selected API1 pass. Ten new quiet
values pass with exact 1/37 goldens. Quote and ordinary controls keep their
outputs. The duplicate control stays in a separate warning-producing CLI.
Two independent wrong-head/index clients reject with located GHC-83865.
Self-hosting, selected both-stage source controls and public17 pass. API1's
input maps are unchanged after the build. Inherited M47 raw scope36 was not
rerun; Core, Type and Utils are unchanged. Legacy8 and both-stage positive
constructor11 pass. Final9 passes two own API1 reuses and seven launched phases,
including both-stage raw/public106 and packed562. The registered run has
39 cases, 34 skips and five expected passes, with zero unexpected/framework
failures or warnings. Artifact audit and publication remain separate. These
focused checks do not cover the full GHC suite. C1 and C2 stay open.
See [the plan](PLAN.md#m48-known-worker-case-activation) and
[the prior art](REFERENCES.md#known-worker-case-activation-prior-art).


## Constructed-worker case activation

M49 extends the metadata-only M48 trigger with scalar constructor trees.
`named_application` reads the known local worker's leading `DepFunTy` binders.
For a scalar binder, `scalar_argument` reconstructs only resolved `HsVar`,
`HsApp` and `HsPar` forms. A constructor must be a real monomorphic DataCon
without erased binders, equality specifications or constraints. Its result
must match the expected scalar domain. Each field is checked recursively
against the declared field domain. `exprToNativeIndex` validates the tree
before `applyTypeToArgs` substitutes it into the existing worker type.

Function operands retain the eligible bare-local-Var policy. The query does
not infer types, allocate choices, collect usage or run source expressions.
Complete constructor rows, whole-tau residuals, quotation boundaries and exact
free-call matching keep their existing roles. `f n`, `f (S n)` and `f (S m)`
remain distinct keys. No inverse-call or injectivity rule is added.

Actual source arguments are checked once by the existing path. M47's ordinary
motive equality and row Given still supply evidence. App, Core, solver, codecs
and backend rules do not change. Constant-head and nested-index controls check
that metadata discovery does not force a predecessor.

The selected source matrix and public21 pass both stages. Eight new quiet
values retain the old 1/37 goldens. Wrong-key payloads reject with GHC-05617 at
source locations; relevant bindings can still expose a generated `call_motive`
name. Final9 passes. Artifact status is recorded in the
[current checkpoint](README.md#m49-constructed-worker-case-activation).
Note [Source opaque call refinement] in GHC.Tc.Gen.Match owns the trigger.
C1 and C2 remain open. See [the plan](PLAN.md#m49-constructed-worker-case-activation)
and [prior art](REFERENCES.md#constructed-worker-case-activation-prior-art).


## Multi-constructor retained package patterns

M50 permits sibling constructors around a native retained field telescope.
`tcRetainedDataConPat` no longer requires a single constructor in the parent.
It still checks the selected native constructor's own universals, fields,
context, equality specification, wrapper and multiplicity. Existing scope and
pattern restrictions remain. An ordinary sibling keeps its ordinary GADT path.

`matchConFamily` selects the row path when any group has retained fields or
native virtual proof evidence. Testing only the first group misses an ordinary
constructor placed before a retained one. Each row now gets its own case on
the shared scrutinee. The row's actual retained field Ids scope its later
fields, nested patterns, guards and RHS. A singleton alternative keeps its
DEFAULT failure continuation in a multi-constructor family. Failure exits that
row's field/evidence scope before trying the next row. Groups for different
outer constructors are disjoint; order within a group is preserved.

No field representation, Core/FC rule, coercion token, interface schema or
runtime lowering rule changes. Core and STG Lint run on the source programs.
Selecting an outer constructor does not force its unused indices or payloads.
The selected demand checks do not imply totality or general lazy-pattern
admission. Native package patterns still reject lazy and let patterns and
require unrestricted multiplicity and the existing prefix/field subset.

Actual object-module imports cover both declaration orders, ordinary fields,
ordinary GADT sibling evidence and two distinct retained telescopes. Sigma3
already passes the parent baseline. This scope does not implement first-class
existentials, arbitrary universes or dependent record projections. See
[the checkpoint](README.md#m50-multi-constructor-retained-package-patterns) and
[primary sources](REFERENCES.md#multi-constructor-retained-packages-prior-art).


## Function-valued retained package fields

M51 permits a named field with the existing closed, visible Many `A -> B`
function type. Both domains must satisfy the existing scalar-domain check.
The stored function remains a local, unrestricted, non-join runtime Id. It can
head `TermAppTy`; it cannot occur as a bare scalar `TermVarTy`. This reuses
M46's call-head rule and adds no Core node or singleton witness.

`tcCheckRetainedConType` and deferred named-field import use
`isNativeRetainedBinderType`. DataCon validation and source pattern fields use
`isNativeRetainedBinder`. Existing universals, field order, scope, multiplicity,
lifted-field, lazy-field and wrapper restrictions remain. A function field
pattern must be a bare variable. Scalar constructor decomposition still uses
the scalar predicate. See Note [Retained constructor field telescopes] in
`GHC.Core.DataCon`.

Later fields use the actual preceding field Ids. Imported field domains remain
deferred. Allocate the lexical Id skeleton before forcing a domain. Extend
the environment only with earlier named fields. The whole-field free-variable
check rejects outer and sibling runtime fields. Two imports and two pattern
instantiations use fresh identities and alpha-equivalent worker types. The
generic three-field package occupies exactly three runtime slots.

`IfaceTermAppTy` uses type tag12, a lexical head name and the argument type.
The reader finds the actual local Id, checks its closed function policy and
the argument kind, then builds `TermAppTy`. `IfaceFreeTermAppTy` preserves an
actual diagnostic free Id and has no wire encoding. Printing and all interface
type traversals preserve that distinction. See Note [Opaque call indices in
interfaces] in `GHC.Iface.Type`. The fork version is10.1.20261115.1 and the
interface header is1001202611151. IgnoreHiWay skips the profile comparison;
it does not skip the version comparison.

Equal results do not imply equal inputs or heads. The four located logical
client controls check this boundary and index scope. The API fixture checks
both compression modes, malformed metadata, wrong kinds/domains/multiplicities,
free/forward/sibling heads and four bounded recursive-domain imports. Actual
object programs check lazy storage and matching with two function domains and
a plain sibling constructor. Both stages pass Core and STG Lint. These checks
do not provide a preservation theorem.

M51 left the generic `IfaceDepFunTy` function-domain reader gated. M52 admits
the bounded signature form described below. Native coercion encoding and
native unfoldings remain gated. Primitive constructor
field telescopes do not implement first-class dependent sums or projections.
See [the checkpoint](README.md#m51-function-valued-retained-package-fields)
and [primary sources](REFERENCES.md#function-valued-retained-package-fields-prior-art).


## Exported function-valued retained value signatures

M52 permits a retained function binder in a complete ordinary named value
signature. `tcUserTypeSig` grants the permission in its complete named branch;
`tcHsSigWcType` carries it to the existing retained signature walker. A
`FunSigCtxt` by itself grants no permission. The generic `tcHsSigType` entry,
instance and class contexts, specialization paths, and hs-boot checking keep
that permission disabled. Partial signatures retain the complete-signature
check before function domains are admitted.

Each function domain must be a closed ordinary visible Many `A -> B`. The
existing scalar checks validate `A` and `B` independently. Within a type or
coercion, the bound function remains a dedicated call head. It is never a bare
scalar `TermVarTy`. Application accepts an already-bound eligible local Var
of the exact function type, then substitutes its actual identity. A global
callee does not permit a global function operand. Ordinary argument checking
and usage checking still run once.

The interface writer and reader reuse `IfaceDepFunTy` and `IfaceTermAppTy`.
The reader checks `isNativeRetainedBinder` for the former, binds that actual
Id, then reads and kind-checks its body. It keeps `isNativeIndexId` for a bare
`IfaceTermVar`. Bound-head scope, alpha freshening and deep free variables
retain their existing rules. No tag, byte layout or version changes. The
fork remains10.1.20261115.1 with header1001202611151.

Raw display does not perform the typed binder check. Therefore an old reader
can display the new same-version signature while rejecting its used Id type
during typed import. A positive old-interface control is required before and
after the semantic negative. The typed compatibility experiment uses the
saved executable with pinned current standard libraries. The raw experiment
retains the copied settings and its original pre-build display check.

The source/object programs preserve actual function/input scope and vector
constructor identity. Ignoring the worker result leaves its fields lazy.
Demanding a vector may execute its original function or input producer. No
call inversion, computed function equality or mutable native-meta assignment
is added. Native coercion serialization, arbitrary unfoldings and ordinary
optimization remain unsupported.

Malformed recursive ordinary-domain interface types remain a known validation
gap: three function-shaped and three scalar-shaped synthetic knots raise
`<<loop>>`. The scalar comparison also loops through the unchanged scalar
check. No old executable ran these synthetic knots. These outcomes are not
successful checked rejections. Source-domain checks
exclude the tested unsupported function shapes. The observations do not
establish totality or a preservation theorem.

See Note [Exported function-valued retained signatures] in GHC.Tc.Gen.HsType,
[the checkpoint](README.md#m52-exported-function-valued-retained-value-signatures),
and [primary sources](REFERENCES.md#exported-function-valued-retained-signatures-prior-art).
