# Restricted dependent Core

This document specifies the restricted internal implementation. It is based on the
`nokinds-pi` binder and application work recorded in [PORT.md](PORT.md).
The dependent typing decisions use [Agda as the primary reference](REFERENCES.md#agda-provenance).
Runtime evaluation must obey Haskell's non-strict rules. This document does not
specify the full dependent-Haskell language.

## Representation

The internal `Type` datatype has two new constructors:

```text
TermVarTy b       -- The Boolean index of a runtime identifier.
DepFunTy b result -- A retained dependent function.
```

`b` is a local non-coercion `Id`. Its type is `Bool`. Its multiplicity is `Many`.
It keeps its identity as a runtime variable. It is not a `TyVar` or a `CoVar`.
The term binder and every reference to its index use the same `Unique`.

`TermVarTy b` has kind `Bool`. Constructor indices use the existing promoted
`True` and `False` types. `promoteDataCon` supplies these types through GHC's
existing DataKinds machinery. `TermVarTy` adds the link to a runtime variable.
The result of a retained function has kind `Type`.
Dependent runtime representations are outside this subset.

Canonicalize a dependent function whose binder does not occur in its result to
an ordinary unrestricted Boolean arrow. This preserves one representation for a
vacuous retained binder. Lambda typing detects dependence from the result type.
No retainedness flag is added to every ordinary identifier.

## Scope and equality

Check `DepFunTy b result` under a context extended by the runtime binder `b`.
Check every `TermVarTy` occurrence against its binding-site identifier and type.
Free-variable collection includes the runtime index and its type dependencies.
It removes `b` under the dependent function binder. Index occurrences are erased
dependencies. They are not additional runtime uses for linearity analysis.

Compare runtime index variables by scoped identity. Compare dependent functions
modulo alpha-renaming of their binders. Do not inspect identifier unfoldings to
equate separately bound Boolean producers.

Existing type variables, coercion variables, and their substitutions keep their
current meanings. The new forms must use separate mapper and folder callbacks.

## Lambda and application

If a runtime lambda's body type mentions its Boolean binder, its type is the
canonical retained dependent function. Otherwise its type is an ordinary arrow.

For application of `DepFunTy b result`, first check the runtime argument against
`Bool`. Then substitute its index for `b` in the result. The initial accepted
arguments are a valid local Boolean identifier and the exact nullary Boolean
constructor workers. Constructor workers map to their corresponding promoted
constructors. A bare variable remains opaque even if its unfolding is a Boolean
constructor.

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

Substitution of a Boolean variable or constructor into an index produces the
corresponding index. Substitution of any other expression must fail explicitly
in the initial experiment. It must not leave a stale variable behind.

`composeTCvSubst` composes type and coercion environments. It also composes
supported Boolean index mappings in the expression environment. It preserves
apply-once substitution and the second environment's priority on shared domains.
It does not compose arbitrary Core expressions. Such expressions cannot replace
native indices in this increment.

This restricted grammar is not closed under general Core substitution. The
experiment therefore cannot enter the ordinary optimizer. Backend preparation
must preserve lazy index binding boundaries. The optimizer will also need this
restriction or an internal expression-index form for arbitrary Boolean producers.
Source frontend support remains pending.

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

Check the scrutinee first. Derive its index with the same variable/constructor
rule as retained application. Do not derive evidence from an unrelated variable,
the case binder, an unfolding, or an unsupported producer expression.

A proof-bearing Boolean alternative must bind exactly one fresh local `CoVar`.
Its type must be the homogeneous nominal primitive equality from the scrutinee's
index to that exact promoted constructor. Check its type before adding it to
scope. Check its right-hand side with the proof in scope. Keep the proof outside
all other alternatives and the result annotation. Ordinary Boolean alternatives
with no binders keep their existing rule. `DEFAULT` cannot bind a proof.

Require exhaustive alternatives whenever a Boolean alternative binds a proof.
Both exact Boolean constructors or a binder-free `DEFAULT` must cover the input.
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

The ordinary optimizer and program backend gates remain mandatory. The simplifier
reconstructs constructor applications from alternative binders. It cannot treat
this proof as a Boolean field. After checked preparation, CoreToStg removes the
virtual alternative proof and erases casts. It preserves strict case discrimination
and ordinary coercion argument positions.
The boundary scanner must detect virtual Boolean proofs even when their types
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
and optional argument arity analysis for this entry. The normal CorePrep entry
and the program entry keep their rejection gates. JavaScript execution remains
unsupported. This entry does not enable the ordinary optimizer or interfaces.
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

The source frontend is pending. The target signatures are explicit retained
Boolean telescopes such as `foreach (b :: Bool) -> Select b`. They must accept
ordinary runtime Boolean bindings. They do not require singleton conversion.

The implementation must preserve one runtime identity through these stages:

1. Parse a distinct retained telescope in `Parser/Lexer.x`, `Parser.y`, and
   `Language/Haskell/Syntax/Type.hs`. Reuse located binder syntax and exact source
   annotations. Do not encode the syntax in strings, plugins, or erased foralls.
2. Rename the retained binder as a term name that is available in its result
   type. In `Tc/Gen/HsType.hs`, check its `Bool` domain and lifted result. Permit
   `TermVarTy` only for the checked retained identifier in this scope.
3. Start with one equation and a bare retained variable pattern. Construct its
   Boolean Id from the pattern's renamed `Name`. Substitute that Id into the
   codomain before decomposing the remaining argument types. `tcPatBndr` keeps
   that `Unique`. `selectMatchVar` reuses the checked variable Id, and `wrapBind`
   adds no alias when the Ids agree. Reuse `MatchGroupTc` unchanged for this
   subset. Require an identity result wrapper. Constructor patterns and multiple
   equations need a separate design for the shared scrutinee and branch indices.
   RequiredTypeArguments instead skolemises type variables.
4. In `Tc/Gen/App.hs`, check a retained argument as a value and instantiate the
   result with its accepted index. Keep the value application. Do not use
   `tcVDQ`, which translates an expression into a type argument.
5. Check Boolean constructor clauses under scoped nominal evidence. The existing
   `ConPatTc.cpt_dicts` field can carry the proof. Constructor desugaring already
   places these binders in the Core alternative. Add the retained refinement
   before the ordinary Boolean no-evidence fast path. Check coverage and retain
   the fixed outer result scope.

Native zonking and relevant unification paths also need support. They currently
reject native forms. Unsupported source constructs must produce located errors
before those internal guards. Keep binder-domain, unsupported-index, coverage,
and branch-result errors at their respective source locations.
`zonkMatchGroup` checks group annotations outside the pattern environment. For
the first subset, preserve the prototype Id's fixed `Bool` type, `Many`
multiplicity, and `Unique` when that environment has no entry.

Keep `foreach` an ordinary identifier when the experimental flag is disabled.
Its proposed syntax can also parse as an ordinary type-variable application.
Do not reserve that spelling to produce a missing-flag error.
Use typed, located `TcRnMessage` diagnostics for unsupported retained forms.
Reuse ordinary diagnostics only when their meanings match. An unsupported `Int`
domain is not a kind mismatch. A retained argument is not an erased required type
argument. Ordinary inferred functions remain valid without a retained signature.
Error-context formatting can itself invoke zonking. Early rejection must precede
native type construction until the native zonkers are implemented.

Start execution with local retained declarations inside an ordinary
`entry :: IO String`. Parse, rename, typecheck, and desugar real source. Pass its
closed Core right-hand side to the checked native bytecode entry. This exercises
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

Interface serialization, source syntax, the ordinary optimizer, and the normal
program backend remain gated. The scoped SimpleOpt work and frontend plan above
have separate acceptance checks. Existing ordinary GHC programs must continue
to build.
