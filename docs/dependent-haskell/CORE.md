# Restricted dependent Core

This document specifies the restricted internal implementation. It is based on the
`nokinds-pi` binder and application work recorded in [PORT.md](PORT.md).
The dependent typing decisions use [Agda as the primary reference](REFERENCES.md#agda-provenance).
Runtime evaluation must obey Haskell's non-strict rules. This document does not
specify the full dependent-Haskell language.

## Representation

The internal `Type` datatype has three new constructors:

```text
TermVarTy b       -- The opaque value index of a runtime identifier.
DepFunTy vis b result -- A retained function with source visibility.
TermAppTy f arg   -- A scoped opaque runtime call index.
```

`b` is a local non-coercion, non-join `Id` with multiplicity `Many`.
`TermVarTy` and Core `DepFunTy` accept its complete lifted value type. This
includes an outer type variable, a function value, and `Vec Int n` for an
earlier runtime index `n`. Check its actual lexical binding and exact metadata.
The source `foreach` root keeps the separate structural-domain or eligible
native-call-head rule. See [generic structural domains](#generic-structural-domains-and-nominal-transport).
The runtime binder remains an `Id`, with the same `Unique` at each occurrence.

`TermVarTy b` has kind `varType b`. Constructor indices use ordinary promoted
constructors, including `True`, `False`, `Z`, and `S n`. `promoteDataCon` supplies
these types through GHC's DataKinds machinery. Nat has no wired compiler identity.
`TermVarTy` adds the link to a runtime variable.
The result of a retained function has kind `Type`.
Dependent runtime representations are outside this subset.

Canonicalize a visible retained function whose binder is absent from its
result to an ordinary unrestricted arrow. Keep an invisible retained binder
even when the result omits it. Both variants take one runtime value argument.
A Core lambda has the canonical visible type. A representational visibility
coercion gives the same closure an invisible type without adding a lambda.
See Note [Retained function visibility] in GHC.Core.TyCo.Rep.

`isNativeIndexType` validates a lifted structural datatype schema. Ordinary
parameters have kind `Type`. After instantiation, a field whose original type
is a universal parameter stays opaque. Thus `[Int]`, `[IO Int]` and
`[Vec Int n]` retain the same list schema. Fixed concrete fields keep structural
traversal or the separate eligible function-field rule. Track full datatype
applications through regular recursion. Reject changed recursive arguments,
existentials, constraints, datatype contexts, newtypes, type data, constructor
wrappers, and unsupported concrete fields. Original and representation field
types must agree. Field multiplicities may be `One` or `Many`.
Nat source execution and signature imports pass the M9 regression checks.

`TermAppTy` keeps its separate fixed native-call policy. Opaque function-value
identity does not grant call permission. See
[opaque native call indices](#opaque-native-call-indices).

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
remains a runtime field; it does not become an erased datatype parameter.
Ordinary parameters can occur in its domain and keep their inferred roles. Source patterns substitute the actual field
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

Check the result of `DepFunTy vis b result` under the runtime binder `b`.
Check every `TermVarTy` occurrence against its binding-site identifier and type.
Free-variable collection includes the runtime index and its type dependencies.
It removes `b` under the dependent function binder. Index occurrences are erased
dependencies. They are not additional runtime uses for linearity analysis.

Compare runtime index variables by scoped identity. Compare retained function
visibility and domains, then compare bodies modulo binder alpha-renaming.
Keep separately bound producers distinct.

Existing type variables, coercion variables, and their substitutions keep their
current meanings. The new forms must use separate mapper and folder callbacks.

## Retained congruence and nominal domain conversion

`DepFunCo visL visR b domain co` binds one unrestricted runtime index in its
body proof. `MRefl` uses the same lifted domain at both endpoints.
If `co :: s ~r t`, this specialization gives:

```text
DepFunCo visL visR b MRefl co
    :: mkDepFunTyWithVis visL b s ~r mkDepFunTyWithVis visR b t
```

Both body endpoints must have kind `Type`. The coercion inherits the body's
FC role. Nominal coercions require the same visibility at both endpoints.
A visible endpoint becomes an ordinary arrow when its result omits `b`.
The binder also scopes over intermediate proof types. A reflexive domain
and body reduce to reflexivity when both endpoint flags match.
Substitution, lifting, zonking, and tidying preserve scope and visibility.

`MCo delta` adds nominal evidence `delta : A ~N B` outside the binder's scope.
The binder has domain `A`. Both domains must be lifted value types. If either
is an eligible native-call function type, require identical complete types.
Other opaque domains can contain outer erased variables and earlier runtime
indices. The right endpoint binds a fresh `y : B` and substitutes `y |> sym delta` for
`b` in the right body. Freshening includes free variables in the domain proof.
Use `mkDepFunCoWithDomain` for this form. A reflexive domain proof specializes
to `MRefl`.

`CastTy` records the converted index kind. Its runtime Core expression uses
`Cast argument (SubCo delta)`. Preserve the nominal child during expression
substitution, native cast composition, and CorePrep substitution. Arbitrary
representational evidence cannot justify an index conversion. Beta exposure
substitutes the embedded proof as well as the operand. Stored substitution
ranges are used once. The existing beta-exposure grammar still requires
structural input and result domains.

Checked application converts the actual argument back to `A` and substitutes
that same converted index in the body proof. Explicit congruence, symmetry,
transitivity, and nominal-to-representational weakening use this rule. Opaque
coercion variables have no retained-Pi projection rule.

D/DC's An-PiCong and An-Push supply the asymmetric conversion and application
precedent. Agda supplies the domain and shared-context comparison precedent.
See [the pinned provenance](REFERENCES.md#agda-provenance) and Note
[Retained dependent function coercions] in `GHC.Core.TyCo.Rep`. These sources
do not prove preservation for the current GHC extension.

Transport a function value with a cast. Do not replace that cast with a lambda:
eta expansion can hide a bottom function in Haskell. Changes to eligible native-call
domains or multiplicities and the ordinary optimizer remain unsupported.
CorePrep still rejects application through an erased binder kind that depends
on a newly named runtime index. Eligible ambient Givens can normalize retained
bodies;
the later Given-normalization sections specify that restricted path.
`ForAllCo` still binds only erased type and coercion variables. No new selector
or erased `InstCo` rule is added. Diagnostic interface conversion can print the
coercion; serialization in native unfolding bodies remains gated.

Source checking can construct this nominal congruence for structural source
domains. Use reflexivity when their complete types already agree. Otherwise
use bounded nominal family reduction and require the reduced domains to agree. Compare the bodies under a fresh source
index, with the target index cast forward through the domain proof.

Solve ordinary body Wanteds in a private solver scope. Propagate its runtime
binder into rewriting. Block external solver evidence and family-cache paths
in this scope. Return a proof only when all body Wanteds are solved. Otherwise,
retain the original whole equality. Do not emit a residual implication with an
unbound runtime Id. Ordinary payload metavariables can be assigned; assignments
that contain runtime indices remain rejected. Zonk the completed proof and
expected types before comparing their endpoints. See Note [Solving retained
function equalities] in `GHC.Tc.Solver.Equality`.

A source scalar argument can also need a nominal domain cast. Keep its nominal
proof before the retained index substitution. Preserve this proof through value
wrappers and final zonking. Check scalar admission after zonking. A phantom
parameter can otherwise erase the nominal proof during representational
weakening. Keep ordinary representational evidence on its existing path.
See Note [Zonking nominal native index casts] in `GHC.Core.Coercion`.

## Family-axiom normalization under retained binders

The Core normalizer processes an open `DepFunTy` domain nominally before its
body. `liftCoSubstNativeBndrUsing` installs the source Id, target binder and
nominal domain proof in a separate runtime lifting relation. Keep the domain
proof outside its own binder. Do not infer a runtime equation or evaluate the
indexed value.

A closed domain uses nominal reflexivity during whole-Pi normalization. No
inherited substitution can affect it. Still freshen the binder, install its
relation and normalize the body. This preserves identical domains such as
`Token (Loop Bool)` without reducing `Loop Bool`. It makes no termination claim.
Explicit comparison of unequal domains still uses bounded family reduction.
See Note [Closed retained domains] in `GHC.Core.FamInstEnv`.

Normalize the body with the existing family reducer and construct
`mkDepFunCoWithVis` with the original visibility at each endpoint. Preserve
the proof binder when an intermediate type needs it. Each visible endpoint
separately canonicalizes a vacuous binder to an ordinary arrow. For example, `Add Z n`
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
complete types, supported structural source domains, `Many` binders and lifted bodies.
No equality constructor name receives a special case. Check the whole visible
type argument for named, anonymous and generated holes. A hole beside the Pi
is also incomplete. Inspect expanded type-splice types without revisiting
expressions or finalizers.

M62 carries whole-argument completeness through ordinary nested type checking.
A complete explicit value type can contain a supported retained function
binder. This includes container arguments and kind-annotated value types.
Fresh kind modes clear the permission. Other signature contexts keep their
existing gates. Signatures and explicit arguments share the anonymous-wildcard
syntax check in `GHC.Hs.Type`; each caller also checks named wildcards.
RequiredTypeArguments can combine an embedded complete type with an outside
hole after renaming. `tcHsTypeApp` must check that combined type. Complete type
formation adds no native meta assignment or function eta equality.

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

## Ordinary callers of retained APIs

Explicit dependent source syntax requires ExperimentalDependentTypes. An
imported native type retains its contract in a module without that extension.
M68 selects the existing application helpers from the source flag or an
available native type. Inspect the zonked head and expected types. Reuse the
declared argument anchors from tcRetainedInferableArg. These probes do not
check occurrences, record usage, instantiate a signature or expand source.

Pass this application capability through immutable index selection, post-VTA
caching, dollar applications and evidence capture. A known native callback
type also enables its existing checking path. Keep complete substitutions,
kind and concreteness checks, exact domains, runtime Id scope, IALL order,
cached usage and final subsumption. Preserve the original imported signature.
Never assign a native type to an ordinary mutable meta-variable.

Capture a native class Wanted below the runtime binder on which its predicate
depends. The source flag does not change this evidence obligation. Incomplete
immutable choices can retain DoQL. Reject actual zonked native types before
Quick Look traverses or assigns them, in either language mode. This check does
not add native Quick Look support.

The ordinary example exports Nat -> a -> [a] and [a] -> [a]. Its implementation
uses replicateNative and genericVector, then toList. These ordinary result
types come from written API signatures. They do not erase an imported indexed
contract. See Note [Imported native application contracts] in GHC.Tc.Gen.App
and [the M68 plan](PLAN.md#m68-ordinary-callers-of-retained-apis).

M69 extends declaration advice to a finite built-in list argument. A bare or
parenthesized variable field can supply its existing monomorphic native element
type. Require a lifted tau type and reuse the current lexical-scope and nominal
constructor-Given views. Propose mkListTy of that declaration as immutable erased
argument advice. Check every original element at the selected type afterward.
Do not infer an expression, collect usage, expand syntax or assign a native meta
to select the advice. Empty lists and lists without an eligible declaration keep
their existing paths. No Core judgment, proof primitive or wire field changes.
See Note [Declared retained-domain choices] in GHC.Tc.Gen.App and
[the M69 record](PLAN.md#m69-generic-safe-list-indexing). The M69 record
pins the passing case paths and the failed literal
`fmap toList (lookupGeneric [vector] 0)` probe. It makes no claim about the
unmeasured named-list variant. NoEDT standalone native let-list inference keeps
its source-flag boundary.

Container and dependent-case inference in GHC.Tc.Gen.Expr keep their existing
source-flag policy. Native Core detection still controls the restricted
pipeline, including for ordinary callers. General optimization, unfolding
serialization, general invisible-argument inference and totality checking remain open.

A checked `@{n}` expression records the result type after retained substitution.
Desugaring passes the checked expression in an ordinary Core value application.
The written operand remains a runtime value even when the function ignores it.
See Note [Explicit invisible retained arguments] in GHC.Tc.Gen.App and
[M73](PLAN.md#m73-explicit-invisible-runtime-arguments).

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
Ids compare by Unique. For `DepFunTy`, compare visibility and the domain in the
outer context, then the body under the extended context. Keep retained functions separate from
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
retained binders must have equal complete domains, `Many`, and bodies of kind
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
binder whose domain depends on an earlier index. Abstract that domain before
entering the binder, then preserve its scope through the body. Existing FC
lifting builds `DepFunCo` with nominal domain evidence. Cast the closure without
eta expansion. Preserve forall metadata outside its own binder. Input cast or
coercion evidence that depends on the abstracted index remains unsupported.

For signature ambiguity checking only, open visible retained binders in ordinary arrow
arguments as well as results. Use one fresh opaque runtime Id on both sides of
the self-check. Scope all fresh Ids over constraint capture and simplification.
Keep the actual signature retained. This projection supplies no coercion between
different retained functions. Preserve the structural-unification gate.
See [Agda's expected-argument and shared-context rules](REFERENCES.md#explicit-higher-order-arguments).

For Core application of `DepFunTy vis b result`, check the runtime argument against
the binder's domain. Then substitute its index for `b` in the result. The Core
grammar accepts a local index Id or a saturated tree of constructor workers.
Convert the fields recursively and check each field domain. Map workers to their
ordinary promoted constructors. A variable stays opaque even if its unfolding
is a known constructor. No index conversion evaluates a value.

A retained argument can also be a saturated call to a checked source function.
For `plus n m`, the runtime Id identifies the closed family for its equations.
Core Lint checks the actual head, arity and operand kinds. CorePrep preserves
the call because naming it with a fresh Core variable would change the index
of its dependent payload and result. Core-to-STG introduces a shared thunk
after type erasure. See Note [Source calls in retained arguments] in GHC.Core
and Note [Naming computed retained arguments at erasure] in GHC.CoreToStg.Prep.

For a retained class method, the typechecker can provisionally use the
source function named by a dictionary field in the producer interface.
Before the program reaches Core, the solver checks that the ordinary
dictionary constraint selects that exact instance. Both the runtime
expression and its index use the certified function. Core and STG use
the same call representation as for a direct
application of the function. See Note [Dictionary selection in retained
arguments] in GHC.Tc.Gen.App.

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

Transport through a remaining retained function uses `DepFunCo` with its
nominal domain relation and `Many` binder. CorePrep converts each later retained
argument to its prepared domain and composes its index proof in telescope
order. Keep the cast on the partial function value. Eta expansion must not hide a bottom function.
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

`IfaceDepFunTy` stores visibility, an `IfaceIdBndr` and a body.
`IfaceTermVar` stores an `IfLclName` in the local Id namespace. The binder's domain and multiplicity are
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

Tag 10 encodes a retained function with a visibility byte before the binder
and body. Tag 11 encodes a runtime index. Unknown tags fail with an IO error.
M71 uses interface header `100120261115165`. The M70 header ends in 64.
The whole-file version check runs before type decoding, including for
`IgnoreHiWay`. Rebuild matching libraries when moving between versions.
Both directions of the version mismatch pass their diagnostic checks.
Normal module unfolding generation and native coercion persistence remain
gated. The expression-table path below reconstructs virtual alternative
proofs separately.

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
skips general occurrence analysis, general inlining, general compulsory
unfolding, SimpleOpt binding dropping, eta reduction, and cast pushing through
lambdas. M53 adds a separate restricted dead-local pass before this traversal.
See [Native dead local bindings](#native-dead-local-bindings). It also disables
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

Keep cases on unknown variables and other lazy source lets. Do not inspect unfoldings to decide an
opaque index. The current mode permits the existing compulsory templates for
wired tuple sections and native newtype wrappers. These exceptions expose
required wrappers; they do not authorize arbitrary unfolding. These rules
implement dependent substitution and checked case
refinement from [the design references](REFERENCES.md#agda-provenance). They do
not add arbitrary expression indices, general Pi matching, or general proof
erasure. Run Core Lint before and after this entry. Retest the shared runtime
index and bottom observations before claiming optimizer preservation.

### Cases of known ordinary constructors

Desugaring a source `case Just xs of ...` can bind `ds = Just xs`
immediately before inspecting `ds`. When the worker has only lazy,
lifted fields, the native simplifier can select the alternative from
the constructor. The payload type may contain a runtime index.

Keep the original case if the selected RHS, result type or field
annotations need the eliminated binder. Bind each needed field lazily.
Two occurrences of a field share the same producer. A matched constructor
with a strict field can raise an exception even when the RHS ignores
the field. It stays on the original path, as do unknown scrutinees and
alternatives carrying native equality evidence.
[M97 measurements](PLAN.md#m97-simplify-a-case-of-an-ordinary-constructor-in-dependent-core)
and [design sources](REFERENCES.md#reducing-a-known-constructor-case-in-native-core).

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

Shared index patterns remain outermost variable patterns at their incoming
domains. A coercion pattern there would allocate a different lambda Id.
Check the constructor proof as a scoped Given. A later occurrence can use
`Cast (Var xs) (SubCo delta)` at its required domain. Keep the original Id type.
Store the resulting evidence below the constructor that binds its proof.
A matrix row stores combined evidence below its second constructor. Ordinary
payloads, dictionaries and results keep their nominal transports. All source
argument and dictionary lambdas precede discrimination.

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
eligible local Many structural binder and an argument of its exact domain.
The body result must have a supported structural domain. Normalize the argument first. Its result
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
M45 left function-domain retained binders for the M46 rule below. M65 later
admits function-value indices under the fixed-domain policy below.

### Function-valued retained binders

isNativeRetainedBinder accepts a structural source index Id or an eligible
native head Id. isNativeRetainedBinderType checks the domain without a
fabricated Id. Core opaque value identity has a separate rule.
A native function head has a closed ordinary visible U/Many arrow A -> B. Both A and B
are independently supported scalar domains. The Id is local, unrestricted and
not a join point. M51 adds function-valued constructor fields under the
separate policy below. M65 also admits these function Ids as value indices.

DepFunTy f body scopes f over calls and function-value indices in body.
The body has kind Type. A function-value index uses TermVarTy f at its complete
fixed type. It is not a scalar index or a matchable type function.
A vacuous retained binder still becomes an ordinary
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

Restricted simplification must use the same bare-variable substitution for
function beta reduction. Check the complete function type after substitution.
Substitute through terms, types and coercions together. Ordinary beta reduction
can create a lazy local alias whose name escapes through a partial result type.
Retain unsupported applications instead. See Note [Restricted dependent
simplification] in `GHC.Core.SimpleOpt`.

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
existing function-free algebraic checks validate `A` and `B` independently.
M65 permits the bound function as a value index as well as a call head.
Application accepts an already-bound eligible local Var
of the exact function type, then substitutes its actual identity. A global
callee does not permit a global function operand. Ordinary argument checking
and usage checking still run once.

The interface writer and reader reuse `IfaceDepFunTy` and `IfaceTermAppTy`.
The reader checks `isNativeRetainedBinder` for the former, binds that actual
Id, then reads and kind-checks its body. M65 uses the same retained union for
a bare `IfaceTermVar`. Bound-head scope, alpha freshening and deep free variables
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

### Ordinary-arrow matchability

M63 stores a type-valued matchability annotation in `FunTy`. `Matchability` has
constructors Matchable and Unmatchable. M and U name their promoted forms.
Ordinary runtime arrows use U. Constructor
kind arrows use M. Synonym and family declarations use U for their consumed
argument prefix and preserve the independently specified result kind.
Explicit metadata and outer dependencies survive substitution and traversal.

`FunCo` stores a proof for this annotation. Its role is nominal under nominal
or representational equality and phantom under phantom equality. Normalization
weakens the nominal proof explicitly when needed. Structural `SelFun` can
inspect the arrow type. Inverse application through `LRCo`, Given equality,
eta expansion and injective-variable analysis requires M at the consumed kind.
Forward `AppCo` works for M, U and unknown metadata. Binding a template variable
to a whole opaque application remains valid. Rigid opaque applications retain
uncertain apartness.

`FUN` has arguments `[ma,mult,rep1,rep2,arg,res]` and roles N,N,N,N,R,R.
Its specified invisible `ma` binder precedes the required multiplicity binder.
Ordinary `FUN One` still selects linear multiplicity. The other three wired
arrows retain their four arguments and fixed U/Many convention.

Native `TermAppTy` remains opaque and nominal. Eligible runtime heads require
U, Many and the existing scalar domains. A vacuous retained binder becomes a
U arrow. This increment does not widen native domains or add function equality.

Interfaces and HIE use the M63 version suffix. Public reflection `Fun` matches
only U/Many; `Fun'` exposes arbitrary metadata at Many. The compatibility
argument list remains `[arg,res]`. TH preserves explicit M/U synonyms and
retains metadata through generated declarations. Primitive Typeable bindings
remain in GHC.Internal.Types but identify GHC.Internal.Prim consistently.

Both builds and the selected API, source, reflection and regression checks
pass. The [M63 plan](PLAN.md#m63-ordinary-arrow-matchability) records their
scope. M63 leaves ForAllTy and ForAllCo at implicit M. The M64 port below
adds explicit forall metadata. Its selected builds and checks pass. Source
family unsaturation and general native domains remain open.

### Forall matchability

M64 stores `ForAllTy ma binder body`. Each annotation has kind `Matchability`.
It is outside the binder scope and can refer to earlier telescope binders.
Plural makers and split views retain `(ma, binder)` pairs. Reconstruction
preserves these pairs. Visibility does not select metadata. See Note
[Forall matchability metadata] in
[GHC.Core.TyCo.Rep](../../compiler/GHC/Core/TyCo/Rep.hs).

`ForAllCo tv visL visR ma_co kind_mco body` stores nominal metadata evidence
at every body role, including phantom. The metadata and binder-kind proofs
are outside the binder scope. Structural selection can return these stored
nominal children at any body role. Symmetry and reflexivity also preserve
those reductions. A residual opaque selector needs a nominal or
representational parent and actual forall endpoints. A phantom parent alone
cannot supply either nominal proof. Core Lint enforces this restriction.
The residual smart-maker precondition uses a debug assertion.

A vacuous known-U CoVar binder becomes an anonymous Many arrow. The maker
uses the domain and result sorts, including Constraint domains. A vacuous M
or unresolved CoVar forall remains quantified. Substitution and normalization
repeat this check. Structural coercion endpoints can therefore normalize to
different type constructors. Select stored evidence before checking the shape
of a residual opaque projection.

Source annotations follow the separator: `forall a b. @ma body` or
`forall a b -> @ma body`. Rename and check `ma` before the whole group enters
scope. Annotation-only names do not introduce implicit binders. An omitted
annotation defaults to U in a type and M in an independent kind. Checked
declaration kinds defer omissions. Their actual consumed prefix uses M for
data, data families and classes, and U for synonyms and type families.
Preserve the independent result kind. For the ordinary kind
`forall k -> k -> Type`, families with zero, one and two header arguments
have metadata M/M, U/M and U/U. Solve formation casts before selecting that
prefix. Constrain explicit annotations by equality before defaulting local
omissions. See Note [Checked declaration-kind matchability] in
[GHC.Tc.Gen.HsType](../../compiler/GHC/Tc/Gen/HsType.hs).

Runtime type and coercion lambdas use U. Constructor, pattern-synonym and
foreign prefixes also require U. Analysis can skolemise an M forall without
constructing a runtime lambda. Identity subsumption can reuse an existing
value at its complete M or variable-annotated type. For example, `pass f = f`
can retain an M-polymorphic argument. This does not cover arbitrary
application RHSs. Under a complete signature, only a bare variable under
parentheses takes the early reuse path. Other signed expressions keep the
scoped skolemisation path and can still fail its U obligation. Fresh lambdas
still require U. See Note [Checking existing values at non-U foralls] in
[GHC.Tc.Gen.Expr](../../compiler/GHC/Tc/Gen/Expr.hs).

H98 and GADT constructors share an ordered source telescope. It stores forall,
context and parenthesis nodes. Rename, check and quote each context before
later binders enter scope. Preserve context-only TH groups without adding an
empty forall. Source record and prenex restrictions remain. The typed DataCon
and interface forms still move fixed-U quantifiers before their contexts.
Constructor-local Givens cannot justify later metadata under this form.
Evidence-dependent annotations need typed telescope or evidence transport
work. See Note [Constructor telescope contexts] in
[GHC.Hs.Decls](../../compiler/GHC/Hs/Decls.hs).

TH adds `ForallMatchT Type Type`, `ForallMatchC Type Con` and
`ForallMatchE Type Exp`. Each wrapper annotates one immediate nonempty forall
group from outside its complete scope. Existing constructors keep their
arities. Quotation and reification retain selected annotations. An untyped
declaration-kind quotation leaves omitted main-prefix annotations contextual
until header checking. Independent kind defaults remain M. Malformed wrappers
and annotated empty groups receive located diagnostics. See Note
[Forall matchability wrappers] in
[GHC.Internal.TH.Syntax](../../libraries/ghc-internal/src/GHC/Internal/TH/Syntax.hs).

Typeable omits only certified M leading foralls of supported generative TyCon
kinds. It uses bounded family-axiom reduction for computed M, without local
Given or plugin evidence. It preserves the original kind. General polytype
TypeRep remains unsupported. FFI normalization uses the same bounded reduction
for computed U and retains its nominal proof in the normalization coercion.
It does not default unknown metadata or use later Givens. See the owning Notes
in [GHC.Tc.Instance.Typeable](../../compiler/GHC/Tc/Instance/Typeable.hs) and
[GHC.Tc.Gen.Foreign](../../compiler/GHC/Tc/Gen/Foreign.hs).

Interfaces and HIE persist metadata outside binder scope. Sharing keys, free
names, fingerprints and type tables include it. Declaration printers use the
authoritative full kind. The selected version configuration gives suffix
`164`: both versions append `64` after the configured patch level. Older
headers must fail before the changed forall payload is decoded. This does
not promise general cross-version compatibility.

The final selected builds and checks pass. Source controls run 32 commands;
TH controls run 18. Both stages pass HIE roundtrips. The six printer checks
retain source bytes where required and preserve ASTs. The maintained cohort
passes API goldens 1/37, both-stage public19, parameterized14 plus three
rejections, and Showcase95/14. Seven registered tests pass: 61 cases and
54 skipped ways. The bounded compiler review finds no acceptance defect.
These checks do not establish a preservation theorem or complete R23. Generic
native-domain transport and family source unsaturation remain separate work.

## Opaque whole-function cast lowering

An opaque nominal equality between two complete retained function types can
remain in a whole value cast. Application uses the target function type.
CorePrep need not project a body proof merely to retain this cast. CoreToStg
erases the cast in the checked lowering path. This does not supply a general
typed cast-pushing or opaque Pi-instantiation operation.

M61's abstract kernel has distinct payload types and a scoped nominal CoVar.
Direct CorePrep checks the cast alone and one or two retained applications.
Its separate object kernel stays abstract through simplification and tidying.
The dump from the actual NCG CorePrep pass retains `Sub cv` between the two
payload types. Structured checks precede that pass; dump inspection is textual.
Only the executable caller supplies `Int` and legitimate reflexivity.

Nine scratch bytecode controls check partial WHNF and ignored or demanded
operands. The NCG control returns 42 without forcing bottom function, scalar
index or vector arguments. It preserves the earlier scalar-domain control's
separate 42 result. Native function beta reduction needs the substitution rule
above to prevent an escaping partial-result alias. Both compiler stages are
rebuilt. The fresh API, both-stage source/Showcase and registered cohort pass.
The object control checks the fully applied keep caller. Partial-WHNF and
vector/index-demand observations belong to bytecode, not NCG.
See Note [Opaque retained function casts] in the existing API fixture.

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

## Native dead local bindings

M53 runs `occurAnalyseNativeExpr` before the existing native SimpleOpt
traversal. It reuses `GHC.Core.FVs.freeVars` and Note [The FVAnn invariant].
Each annotation contains deep local free variables. These include variables
in Id types, multiplicities, binder telescopes, case result types, coercions,
rules and stable unfoldings. A reference in a type records scope. It does
not require evaluation of the referenced value.

The pass removes a `NonRec` only if its binder is absent from the original
body annotation. The `oa_can_drop` policy also requires a lifted `Many`
local Id. It excludes CoVars, exported Ids, joins, stable unfoldings, rules,
inactive bindings and loop breakers. TyVars remain. The domain of a scalar
or function local does not itself prevent removal. A surviving reference
to that local in a type or metadata does prevent removal.

The pass reconstructs every other node with its original binders, types,
coercions, ticks and alternative positions. It preserves recursive groups
and their order while visiting their RHSs. It preserves binder metadata
and infers no occurrence counts. The existing SimpleOpt traversal can still
remove fragile metadata afterward. The direct metadata probe checks the new
pass before that later cleanup. The program entry still processes each RHS
separately and preserves top-level identities.

Original annotations can retain an outer binding after removal of an inner
binding. This pass does not compute a fixed point. It does not enable
ordinary occurrence analysis, binder swapping, joins, general inlining, eta
reduction, native unfolding export or `-O1`. Unlifted bindings remain because
they can be strict. The `One` and state-token exclusions are Core policy
checks. They do not establish source linearity acceptance.

The existing API fixture checks the dead ordinary let, type-only scalar and
function-head uses, later lambda types, a case-result-only dependency and
six protected-binding cases. It checks input/output Lint and exact types.
The direct stable-unfolding probe retains an outer Boolean index whose only
surviving reference is in an inner binding's template. Both compiler stages
preserve the 19-result separate-module program and 53-line Showcase.
These observations do not prove general optimizer preservation or totality.
C1, C2, R22 and malformed recursive interface validation remain open.

See Note [Native dead local bindings] in `GHC.Core.Opt.OccurAnal`, the
[plan](PLAN.md#m53-native-dead-local-binding-analysis), and the
[Agda and GHC prior art](REFERENCES.md#native-dead-local-binding-analysis-prior-art).

## Detected recursive interface domains

M54 evaluates each existing native interface eligibility result inside IO.
`evaluateIfaceNativeDomain` catches only `NonTermination` and calls `failIfM`:

```text
Cannot validate retained type in interface: <encoded name>
Validation encountered recursive type information.
```

The boundary covers generic retained binders, scalar indices, opaque call
heads and retained constructor fields. Existing predicates and guard order
remain unchanged. Validation stays at each reader boundary. The diagnostic
uses the encoded name. Printing the incomplete Type could force it again.
The reader returns no placeholder and does not retry the active decoder.
Other exceptions, including asynchronous exceptions, propagate unchanged.

The M54 fixture required these normal interface errors for nine declaration
graphs in both compression modes: 18 negative cases. Nine valid declaration
and forcing-order cases give 18 positive cases. They include self-recursive
and mutually recursive first-order domains, nullary synonyms, and an ordinary
field containing a valid nonvacuous Boolean Pi. The six earlier retained-field
rejection graphs also run in both modes. Raw exceptions and timeouts fail.

This reports detected failures. It does not stage the loader or guarantee
loop detection or termination. M55 below adds bounded raw shape prechecks.
C1, C2, R22 and general termination remain open.
It adds no source feature, Core rule or interface encoding. See Note
[Detected recursive native interface domains] in GHC.IfaceToCore and the
[primary sources](REFERENCES.md#detected-recursive-interface-domains-prior-art).

## Native interface domain shapes

M55 derives a lazy `NativeDomainShape` view from each imported data declaration
and nullary synonym. The view belongs to that reconstructed `TyCon`. Its
references use `forkM` and `tcIfaceTyCon` in the original interface environment.
Attaching the view rebuilds `tyConNullaryTy` so that `mkTyConTy` keeps the view.
Existing builders default to no view. These are internal GHC API additions.
The interface encoding and Core type forms stay unchanged.

The precheck reads this view before expanding synonyms or decoding fields.
It distinguishes scalar domains, closed function domains, ordinary field
multiplicities (`One` or `Many`) and unrestricted multiplicities (`Many`).
Data cycles are allowed. Each queued field must still pass. A separate active
synonym stack rejects alias-only cycles. Reset that stack at data edges, so
`type Alias = A; data A = MkA Alias` remains valid.

Generic Pi import checks the raw multiplicity and domain before `bindIfaceId`.
The allocator calls `mkLocalIdOrCoVar`, which expands aliases through
`isCoVarType`. A check after allocation reaches an alias-only cycle too late.
Named constructor fields also get a raw precheck before decoding. The Id-based
prechecks and all existing typed eligibility checks remain mandatory afterward.
An early failure uses the encoded name and this diagnostic:

```text
Unsupported retained domain shape.
```

An unknown shape uses the typed path with M54's detected-loop boundary.
Applied synonyms, casts, constraint arrows and raw variable/call-index forms
remain unclassified. Their decoders retain scope checks. A raw retained Pi is
not scalar. M65 retains its multiplicity, argument and result shapes because
a vacuous Pi can become an ordinary eligible arrow. Function endpoints use
the separate function-free policy before typed decoding.
No positive shape result validates a Type.

The existing API fixture checks 28 early generic rejection cases, 12 separate
named-field rejection cases and 32 valid graph scenarios across both compression
modes. Five direct controls check the cache, absent views and applied syntax.
The negative graph helper excludes the detected-loop fallback text. Valid
controls include mixed multiplicity aliases, alias/data cycles in both orders,
a vacuous Pi function alias and a genuinely applied forgetful synonym.

The trace isolated a nonallocating alias-expansion loop before Id allocation.
Its internal timeout did not interrupt that loop; the diagnostic probe used an
OS timeout. This is bounded loader staging. Parent kinds and constructor binder
headers must already exist. Unclassified normalization paths still use the
typed fallback. There is no general termination or preservation proof.
C1, C2 and R22 remain open. See Note [Native interface domain shapes] in
GHC.Core.TyCon and Note [Checking native domain shapes] in GHC.IfaceToCore.

## Closed parameterized runtime index domains

M56 instantiates ordinary datatype parameters before checking index fields.
The same rule applies to `[Bool]`, `Maybe Nat` and `Tree Bool`. The retained
value is the actual list, optional value or tree. Its static type arguments
do not add a runtime witness. See Note `[Algebraic runtime index domains]`.

| Operation | Restricted contract |
| --- | --- |
| Domain formation | Use saturated boxed vanilla data at closed monotype arguments of kind `Type`. Require ordinary parameter kinds and compatible original/representation fields. |
| Recursion | Track complete applications on the active path. Repeated `TyCon` arguments must satisfy `eqTypes`. Check sibling instances independently. Reject growing and permuting recurrence. |
| Constructor index | Preserve ordinary type arguments before runtime indices. Instantiate each runtime field domain with `dataConInstOrigArgTys`. |
| Substitution | Preserve actual runtime Ids and capture avoidance. Function-head replacement must preserve the complete nominal input and result domains. |
| Branch evidence | Relate the actual scrutinee index to the full promoted constructor application. Require fresh field Ids and one local nominal proof. Validate the instantiated parent in the source checker and Core Lint. |
| Lowering | Preserve erased type applications. Name only runtime fields. Use nominal reflexivity for static arguments and checked transport for fields. Keep strict cases and lazy field demand. |
| Interface import | Use existing type applications and constructor signatures. M55 leaves applied shapes unknown. The typed check validates the instantiated domain. Add no wire tag. |

Selected list patterns use existing constructor checking. Expose `[]` as the
resolved nil constructor. Expose an infix constructor as prefix syntax. Keep
the original source span and match context. Only selected retained columns
change. Nested list patterns remain outside the existing bare-field subset.

Static parameters and stored fields have different obligations. A monokinded
`Proxy (Bool -> Bool)` has no function field. `Maybe (Bool -> Bool)` stores
one and uses M65's separate function-leaf policy. No existential, equality specification,
constructor context, wrapper, newtype or retained constructor telescope is
admitted as an index domain. Haskell values may still be partial or cyclic.

`Count`, `OptionalPayload` and `TreePayload` are explicit type families.
Their equations use ordinary GHC nominal coercions. Before immutable cached
argument matching, normalize the actual type view at Nominal role. Preserve
the original cached expression, inferred type and usage. Final subsumption
constructs the family coercion and emits saved usage once. For a known
algebraic expected shape, reduce a native family head before allocating
fresh parameters. Return the inverse nominal reduction. Keep the existing
fallback for a stuck or different result shape. See Notes `[Family views in
native argument inference]` and `[Matching native family result shapes]`.

This follows GHC's existing normalization/subsumption pattern and Agda's
parameter specialization. D/DC leaves primitive datatype matching for later
work. System DR supports the nominal constructor boundary. GraD separates
type dependency from runtime usage. These comparisons do not supply a
preservation or sharing theorem for this implementation. C1, C2 and R22
remain open. There is no new term evaluator or general optimizer support.

## Fixed function-domain body comparison

M59 extends source equality checking beneath an unchanged function-valued
retained binder. Both binders must satisfy the existing opaque-head policy.
Their complete rewritten function types must agree. Use one fresh bare
function Id in both bodies. Keep `MRefl` as the domain evidence; a cast around
the function Id is not an eligible call head.

The shared solver keeps M58's nominal domain evidence for changed scalar
domains. That path substitutes the backward-converted actual into the right
body. Freshening includes the deep free variables of the domain proof.
Both paths use the private body solve, active comparison scope and checked
zonked endpoints. An ordinary payload meta can become `Int`. A local runtime
index must not escape into that meta. See Note `[Solving retained function
equalities]` in `GHC.Tc.Solver.Equality`.

Known-value selection separately recognizes complete retained functions with
eligible function binders. It preserves immutable matching, kind, scope and
usage checks. An explicit ordinary wildcard exercises body solving independently
of that earlier selection. The trace compiles and links this control; it is
not a runtime trace. Its final proofs are reflexive after zonking.

Both stages pass the selected source, demand and rejection controls. The
Showcase has 79 observations through fourteen source modules. This increment
adds no Core constructor, interface form, function equality or eta expansion.
Changed function domains, opaque Pi projection, general native metas and normal
optimization remain unsupported.

## Function values in algebraic indices

M65 permits an eligible local function as a leaf in a closed algebraic index.
For example, `Maybe (FUN @U 'Many Bool Bool)` can contain the actual function
Id. The function has a fixed ordinary visible U/Many arrow. Check its argument
and result with the previous function-free algebraic policy. This separate
policy rejects `data Loop = Loop (Loop -> Bool)` without restarting the same
eligibility query. Regular algebraic recursion retains its existing checks.

`TermVarTy f` records the function's lexical identity at its complete type.
`isNativeRetainedBinder` admits it. `isNativeIndexId` still requires an
algebraic root. Lint checks the actual scoped binding as well as its occurrence.
Reification and reconstruction preserve the same Id and its fixed domain.

The smart makers use one application representation:

```
mkAppTy (TermVarTy f) a = TermAppTy f a
```

Batched application, cast leftovers, substitution, family reduction and
interface import use the same rule. Lint rejects a raw ordinary `AppTy` with
this function head. An existing forward `AppCo` can contain a nonreflexive
head proof through a saturated family. Preserve that proof and its role.
`TermAppCo f arg_co` alone cannot replace it. M remains necessary for inverse
projection; U and unknown applications supply no inverse rule.

A strict source case can expose the same call through a saturated family.
The case checker uses the solver's bounded family rewrite for capture discovery
and transport. Compose the nominal family proof with the strict-call alias
proof. U and unknown application equalities remain unresolved when rewriting
cannot expose their heads. Their shape does not establish an inaccessible
GADT branch. Known-M shape contradictions retain their existing diagnostic.

The interface writer encodes direct `TermAppCo` with existing `IfaceAppCo`,
`IfaceReflCo` and `IfaceTermVar` forms. It preserves the argument proof.
The reader checks the actual function Id and canonicalizes application.
Raw Pi shape metadata retains multiplicity, argument and result views.
Check each function endpoint with a fresh function-free data traversal.
No wire tag, header version, coercion axiom or DepFunCo persistence is added.

Both rebuilt stages pass the selected separate-module source and demand
checks. The maintained API tests and seven registered regressions pass.
Showcase passes 100 observations through sixteen modules in both stages.
See [the M65 validation record](PLAN.md#m65-validation).
Open domains, higher-order endpoints, nonreflexive function-domain transport,
normal optimization and totality remain separate work. M66 adds the bounded
native argument and shape views described below.

See Note [Function fields in native index domains] in GHC.Core.Type,
Note [Native function-value application] in GHC.Core.TyCo.Make, and
Note [Function-value call congruence in interfaces] in GHC.CoreToIface.

## Bounded native frontend family views

M66 replaces the two pure native family views with the existing nominal
family-only solver rewrite. One `tcRewriteFamilyAxioms` wrapper exposes the
operation in TcM. A narrow Solver.hs-boot signature breaks the Unify import
cycle. The rewrite uses no ambient Given equality, plugin rewrite or cached
family evidence.

The native shape matcher returns the inverse reduction proof only when the
required algebraic TyCon is exposed. Other shapes keep the existing fallback.
Cached argument selection keeps the original expression, inferred type and
usage. Its initial, extra and final views all use the bounded rewrite.
Final subsumption still constructs the actual family proof.

Restore the original argument context before creating CtLoc. The depth error
uses its saved source span and context. Cache positions count value arguments;
the existing VTA offset gives the full diagnostic argument number. A written
type argument therefore counts in the displayed ordinal.

Each view starts at depth zero and increments depth through recursive family
RHS reduction. A finite configured limit produces GHC-40404. The explicit
zero-depth flag still disables that limit. This is no global work or termination
bound. Other pure normalization paths remain outside this change.

Both rebuilt stages pass five depth errors and five finite observations.
The exact argument-context checks, maintained API fixtures, seven registered
tests and unchanged Showcase100/16 pass. See
[the validation record](PLAN.md#final-m66-validation) and
Note [Bounded frontend family views] in GHC.Tc.Solver.

## Runtime Id metadata preparation

The M67 foundation substitutes a retained binder's source type and multiplicity
before freshening. Its domain is outside its own scope. Install the resulting
target Id before mapping the body, including when its Unique stays the same.
Check a cloned range against the substituted source domain. Return a stored
range or target in-scope Id once. A second substitution would change a range
such as `a := Maybe a` incorrectly. The deep mapper uses deep substitution
inside binder metadata.

Both zonkers reconstruct original Id metadata with their existing helpers.
Keep the Name and Unique. Do not inspect the recursive IdInfo environment.
Input prototypes must have equivalent zonked metadata and acyclic earlier-index
dependencies. Final zonking must not use its own unfinished output as new input.

These metadata operations alone supply no domain coercion or admission rule.
The current structural schema and proof transport are specified below.
See Note [Substituting runtime Id metadata] in GHC.Core.TyCo.Subst and
Note [Zonking runtime index metadata] in GHC.Tc.Zonk.TcType. The
[validation record](PLAN.md#m67-metadata-foundation) separates raw metadata
controls from the closed specialization checked with Lint.

## Generic structural domains and nominal transport

This section states the current M67 behavioral contract. See
[PLAN.md](PLAN.md#m67-generic-domains-source-and-self-hosted-validation) for
the bounded validation cohort and its remaining limits.

Keep three judgments separate. Source `foreach` roots use the structural schema
or fixed native-call-head policy. Core opaque value indices accept actual local,
non-CoVar, non-join `Many` Ids of arbitrary lifted value type. Native application
heads keep the closed visible U/Many function policy and independently closed,
function-free structural endpoints. Opaque identity supplies no function equality
or inverse U application rule.

Derive parameter-field origin before instantiating a constructor telescope.
A field that expands to an ordinary universal variable stays opaque after
instantiation. The cons head can therefore have type `Int`, `IO Int`, an
ineligible function type, or `Vec Int n`. Its tail still follows the list
schema. A fixed concrete field does not obtain this permission from its final
type. Check original and representation fields for exact agreement. Keep the
existing uniform-recursion and worker restrictions.

Substitution distinguishes source prototypes from completed target Ids and
stored ranges. Normal and deep mappers update source binder metadata before
freshening. Source free-variable augmentation preserves existing target scope
entries. Ordered constructor instantiation installs each actual runtime range
before later fields. Return target representatives and ranges once; do not
refresh them from the source substitution. Runtime metadata can mention earlier
indices, but cannot form a recursive metadata knot.

Lifting has a separate runtime map. For source binder `b`, lift its domain in
the earlier prefix to `delta : A ~N B`. Bind `bL : A` in the common body context
and relate the source occurrence to `GRefl N bL delta`. Its endpoint ranges
are `bL` and `bL |> delta`. `DepFunCo` binds the right endpoint at `B` and uses
the inverse domain cast there. Preserve deep proof free variables and the
original source metadata when freshening. Completed relations remain apply-once.
See Note [Lifting runtime binder domains] in `GHC.Core.Coercion`.

`abstractNativeIndices` abstracts domains before bodies and freshens their
binders. Forall matchability stays outside its own binder. Ordinary nominal FC
lifting transports the resulting template, including a supported polymorphic
payload. This is forward congruence; it does not reflect a polytype into a
runtime value. Input `CastTy` or `CoercionTy` evidence that depends on a selected
index remains unsupported. An eligible native function head cannot change its
complete fixed type. See Note [Scoped native index abstraction] in
`GHC.Core.Coercion`.

Closed retained domains stay unchanged during whole-Pi normalization. Open
domains retain nominal normalization and transport. If a closed `Element`
family reduces to `Int` in the body, argument normalization can retain
`xs |> [Element ~ Int]` while its binder keeps type `[Element]`. Keep that kind
cast. Do not retype the original Id. Unequal-domain checking still uses its
bounded reducer. Other pure normalization paths have no general bound.

These rules use existing casts, `GRefl`, `ForAllCo` and `DepFunCo`. They add no
Core constructor, wire tag, proof primitive, term evaluator or totality rule.
Preserve lazy values and bottom functions. Ordinary optimization, unrestricted
expression indices and general normalization guarantees remain outside this
contract.

## Nominal evidence for a composition of strict pair cases

Consider `fst (swap p) ~ snd p` for an unknown boxed pair. The checked
producer equations have a pair constructor on the left. Neither
constructor equation can reduce at this input. The solver uses a
built-in nominal rule to connect these endpoints, including when an
imported signature exposes the composition only after substitution.

For example, the outer projection in `fst (swap p)` selects the first
output of the exchange. Its defining branch puts the second input there.
The opposite projection on the right selects that same second input.
Forcing either result inspects the same shell and then the same field.
The proof does not require an equation between a pair and a fresh pair
of its projections. Such an equation would change strictness at bottom.

The two arguments of the `BoxedPairComposition` axiom identify the
endpoints with nominal reflexive coercions. The checker reads the
single closed branch of each family. It checks saturation, binder
identity, field order, kinds, and the common scrutinee. Core Lint uses
the same checker. The solver also records the three source owners so
a conflicting ordinary rewrite rule can invalidate the source judgment.
The interface records the built-in rule name and its arguments. The fork
changed the format suffix from `68` to `69`. Both readers rejected the
other format at the header. Changing only the rule name inside a new
proof produced a diagnostic naming the unknown rule.
[Rule and branch checker](../../compiler/GHC/Builtin/CoercionRules.hs),
[solver](../../compiler/GHC/Tc/Solver/Equality.hs)
and [Core Lint controls](../../testsuite/tests/dependent/should_run/NativeOpenPairCoercion.hs).

### Projections through several reconstructions

The compiler reads the exact output binders of each strict boxed-pair
case. They can come from the same input. Following the projected
position produces a sequence of input pairs and field positions. Two
paths are equal if they reach the same scrutinee and the same field
with the same kinds. Use the outermost common point that proves the
claim. This avoids using an equation for a shared inner computation.
A constructor case that forces an extra field cannot supply such an
equation.

The existing `BoxedPairComposition` rule still proves an exchange
followed by the opposite projection. The solver also emits that rule
for the reverse direction by symmetry. The `BoxedPairReconstruction`
rule checks the more general paths. Both use two nominal reflexive
premises. An old format-69 reader can read newly written exchange-only
interfaces. It reports the new rule by name when it needs a proof it
cannot check. Both behaviors were checked with the prior binary.
[Structural verifier](../../compiler/GHC/Builtin/CoercionRules.hs)
and [ordinary source cases](../../testsuite/tests/dependent/should_run/NativeOpenPairCoercionUsage.hs).

### Obtaining Core for a same-module call

The renamer records a qualified local term used in a type application.
For an independent, signed producer, the module typechecker solves its
binding before checking that application. The open evaluator reads the
binding's typed Core with the caller's unknown arguments. It records the
definitions it used for the existing source-rule checks. An exported
type synonym contains the result of the observation. A client can use
that synonym with only the interface.
[Declaration scheduler](../../compiler/GHC/Tc/Module.hs)
and [interface client](../../testsuite/tests/dependent/should_run/NativeLocalOpen.hs).

The scheduler follows references in selected term bodies, including
those needed only to typecheck ignored arguments. The observer records
the smaller set of bodies it actually reads. The late rewrite-rule check
uses the heads of those definitions.
[Local rule regression](../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

### GADT result indices during closed evaluation

For `Pair :: a -> a -> Mark a 'True`, the Core case binds an erased
equality that relates the scrutinee index to `'True`. The two values
are the fields to reify. GHC rules out `Off :: Mark a 'False` when it
plans the case. The interpreter returns a constructor tag and the fields.
The decoder applies the promoted constructor to its source arguments,
then checks the resulting kind. The ordinary constructor wrapper
supplies equality evidence when constructing an operand for a later call.
[Renderer and decoder](../../compiler/GHC/Tc/Gen/Splice.hs)
and [source checks](roadmap/R15-closed-term-evaluator.md#m129-constructors-at-a-known-gadt-index).

### Nominal evidence for constructor-local indices

At `Vec ('S ('S 'Z)) a`, a `VCons` alternative introduces a fresh
predecessor `m` and a nominal coercion from `'S ('S 'Z)` to `'S m`.
GHC projects through the promoted `'S` and reverses the coercion. Lifting
that equality through `Vec` relates the tail field's actual type
`Vec m a` to its planned type `Vec ('S 'Z) a`. GHC checks both
endpoints before using the representational cast on the field.

Planning records how to project each erased argument from the result
equalities. The renderer follows those paths using the actual branch
binders. Readback reuses the checked argument types. A constructor-local
argument hidden from the result needs another source of evidence.
[Constructor and renderer checks](../../compiler/GHC/Tc/Gen/Splice.hs)
and [ordinary vector producer](examples/closed-evaluation/IndexedVec.hs).

### Source owners of inferred recursive functions

For a group of inferred polymorphic functions, normal desugaring can bind
a tuple of implementations and select each export from it. To observe one
export, use the desugarer's single-export path with that export's original
wrapper, evidence and complete monomorphic binding group. This yields
the checked body directly. Normal module compilation still uses the
original export list.

The typechecked export maps each monomorphic binder to its source
function. Record that owner when weak-head evaluation follows the
binder's suspended expression. This keeps rewrite-rule checks precise
even when the two functions share the same inferred type.
[Declaration scheduler](../../compiler/GHC/Tc/Module.hs),
[weak-head evaluator](../../compiler/GHC/Core/OpenWhnf.hs)
and [sibling-rule test](../../testsuite/tests/dependent/should_run/NativeLocalOpenSibling.hs).

### Constructor cases with open fields

The observer recognizes a saturated ordinary boxed constructor worker.
It checks the type arguments, the representation and the types of the
lifted fields. A known constructor determines the Core alternative. The
alternative binds each field to its expression and lexical environment.
A case on an unknown field blocks if the branch demands it. A successful
source observation at this checkpoint returns one of the caller's rigid
type variables. The observer installs no coercion axiom.
[Core observer](../../compiler/GHC/Core/OpenWhnf.hs)
and [checked source application](../../compiler/GHC/Tc/Gen/OpenTypeEval.hs).

An observation can unfold an imported helper at more than one type. The
ordinary source-rule check uses the first known type argument at each
occurrence. An alias to a variable of the same type preserves its
pending type application. Returning to the cache for an unfolding still
records that occurrence. A demand with unknown specialization requires
checking every active rule for its owner.
[Interface examples](roadmap/R16-open-term-evaluator.md#eliminating-a-known-user-constructor).

### An existential index carried by a runtime witness

The package case binds a fresh index, its witness, and its payload.
For a vector, the encoder for the witness has the shape
`forall n. SN n -> ShowS`. The encoder for the payload has the shape
`forall n. Vec n a -> ShowS` at the known element type `a`. Each
recursive alternative uses its own bound predecessor variable and
checks the role and endpoints of the case's equality. Its recursive
field already has the type required by the next call. The interpreter
returns constructor tags and ordinary fields.

The decoder first reconstructs the witness and the index it establishes.
It checks the payload against that same index, including each base
case. It checks the reconstructed field kinds before applying the
promoted package constructor. The plan records the datatype and constructor
dependencies before evaluating the producer. A written package already
contains its erased arguments in the kind-checked promoted application.
[Planning, rendering and decoding](../../compiler/GHC/Tc/Gen/Splice.hs)
and [witnessed source results](roadmap/R15-closed-term-evaluator.md#m135-reading-a-runtime-existential-package).

### Composing closed ordinary calls

The planner keeps a checked ordinary application in Core when the next
operation is another ordinary Haskell function. It compiles the combined
expression and renders the observed result. For `first (make 7)`, the
callee's ordinary Core determines whether either field of the intermediate
parcel is demanded. A checked family still needs an argument in the type
language, so its input is a separate observation. Runtime exceptions
belong to the observing call. A static input-kind error belongs to the
offending operand.
[Source planning](../../compiler/GHC/Tc/Gen/HsType.hs)
and [interpreter boundary](../../compiler/GHC/Tc/Gen/Splice.hs).

Keep an interface dependency on each checked ordinary function before
simplification. Record the object dependencies that remain after linking.
An optimizer rule can replace the whole expression and remove the last
object reference. Changing the rule in just the provider interface must
then invalidate the client. [Rule replay](../../testsuite/tests/dependent/should_run/NativeClosedFusedRule.sh).

### Returning a constructor with symbolic fields

The ordinary producer's instantiated result arrow determines the result
kind. The observer saves each field and its lexical environment. To
construct a promoted result, it reads each field at its declared kind
and checks the result of the promoted application. A neutral variable
must refer to one of the supplied symbols. The weak-head reduction and
all field reads share one reduction budget. An unknown scrutinee
blocks a case even when other fields are known. The visible constructor
contributes its declaration fingerprint. Only demanded function bodies
contribute term rewrite-rule dependencies.
[Core readback](../../compiler/GHC/Core/OpenWhnf.hs)
and [source examples](examples/open-evaluation/OpenBuildSource.hs).

When the ordinary producer supplies a checked boxed pair equation, an
exported type can retain the family application at a neutral pair.
The closed-family axiom matches the actual pair constructor. A later
client that supplies the constructor uses that axiom for its nominal
equality. A stricter worker loses the equation. A rewrite rule that can
change the worker at the instantiated types prevents use of its axiom.
[Interface and rule checks](roadmap/R16-open-term-evaluator.md#m142-exporting-a-swap-from-an-ordinary-module).

For an ordinary sum, the producer checks every constructor before it
emits a closed family. Each branch returns the variable bound at the
selected field. Leaving the scrutinee unknown preserves the case as a
family application. Matching the promoted constructor later supplies
the nominal coercion. Adding demand on another field invalidates that
derivation. [Sum projection and its source controls](roadmap/R16-open-term-evaluator.md#m144-deferring-a-projection-over-an-ordinary-sum).

A nested case contributes the full sequence of demanded constructor
patterns to each branch. For `pickNested`, the outer constructor
exposes a field of type `Maybe a`. Only a known `Just` or `Nothing`
can select the result. A checked binary case may also return its second
argument. During closed evaluation from interfaces, rewrite the case on
the computed scrutinee and a fresh variable for the fallback. If the
family selects the variable, evaluate the fallback. If it selects a
closed result, that result is available immediately. GHC checks the
fallback's kind and dictionaries in both cases. The fresh variable
must never appear in a returned type. [Case tree](roadmap/R16-open-term-evaluator.md#m146-nested-constructor-cases-with-a-fallback)
and [closed operand checks](roadmap/R15-closed-term-evaluator.md#m145-sharing-kinds-across-a-checked-family).

A checked case can build a result constructor from scoped variables.
The checker follows the alternatives in the producer's desugared Core.
Each field must have the kind required by the constructor's worker.
The resulting nominal equation can keep the original case stuck on a
rigid input while a separate client supplies the missing constructor.
[Case result equations](roadmap/R16-open-term-evaluator.md#m148-constructor-results-from-a-checked-case).

When an outer ordinary call needs an interface-only pair projection,
the compiler can emit an ordinary Core case. The nominal equation
applied to a pair of distinct rigid variables determines the selected
field. Only the result of the outer call needs readback. A bare nominal
application still needs an operand that can be written to an interface.
[Closed pair projection](roadmap/R15-closed-term-evaluator.md#m149-demand-only-the-selected-field-of-a-computed-pair).
