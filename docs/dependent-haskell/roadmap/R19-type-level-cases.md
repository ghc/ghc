# R19. Type-level case expressions

The roadmap calls for constructor matching inside a type expression.
Its prerequisites are dependent Core (R05) and open evaluation (R16).
[Upstream example](https://ghc.serokell.io/dh#type-level-case-expressions).

```haskell
type F b = case b of
  True  -> Int
  False -> Bool
```

With `ExperimentalDependentTypes`, a top-level synonym can select a
result by a constructor or bind its fields. A client with only the
compiled interface can prove `F 'True :~: Int`. For a rigid unknown
scrutinee, two branches with identical result types still leave the
case stuck. [Provider](../examples/open-evaluation/NativeCase.hs)
and [ordinary client](../../../testsuite/tests/dependent/should_run/NativeTypeCaseClient.hs).

A known constructor that has no matching branch also leaves the
case stuck. For `type Pick b = case b of True -> Int`, a client's
attempt to prove `Pick 'False :~: Int` fails at its `Refl`.

GHC first checks one equation per alternative. Each equation receives
the lexical captures and scrutinee. A flat case publishes these
equations in a private closed family. For a nested case, GHC compiles
the checked equations to private closed families that test one
constructor at a time. An error in a client can display the case using
the interface. The use of closed families follows the promotion
translation of Eisenberg and Stolarek. Agda's pattern lambdas provide
a related way to match on an intermediate expression. [Promotion paper, §4.5](https://richarde.dev/papers/2014/promotion/promotion.pdf)
and [Agda pattern lambdas](https://agda.readthedocs.io/en/stable/language/with-abstraction.html#pattern-lambdas).

Patterns can contain constructors, distinct variables, or wildcards.
A variable at the root binds the scrutinee in that row. A wildcard at
the root ignores it. The compiler tests earlier rows in source order.
For example, `case b of { True -> Int; _ -> Int }` stays stuck for an
unknown `b`. A first row `_ -> Int` returns `Int` immediately.
Case expressions inside arbitrary types remain open.

## Type constructors in case patterns

An unquoted head first resolves as a data constructor. If no data
constructor is in scope, GHC looks in the ordinary type namespace.
A promotion tick selects the data namespace. Thus both `Choose Bool
:~: Int` and `Choose Ordering :~: Char` hold for this synonym:

```haskell
type Choose a = case a of { Bool -> Int; Ordering -> Char }
```

In `data T = T`, bare `T` in a type case still selects the data
constructor. A type-only qualified import can select the type constructor
or class. Synonyms can expand to constructor patterns. A synonym that expands to a
variable cannot turn a constructor branch into a catch-all. The ordinary checks for
closed families reject applications of type families in patterns.
Term-level cases keep their existing name lookup. Type cases on promoted
GADT constructors still refine the scrutinee kind.
[Closed-family translation, §4.5](https://richarde.dev/papers/2014/promotion/promotion.pdf)
and [namespace and interface test](../../../testsuite/tests/dependent/should_run/NativeTypeCase.sh).

### Ordered nested patterns

The first surviving alternative determines the next test. If an earlier
test is stuck, the result is also stuck. For example, in the following
case, `Pick ('Just x)` is stuck when `x` is rigid:

```haskell
type Pick d = case d of
  Just (Just _) -> Int
  Just _        -> Int
  Nothing       -> Char
```

Agda's case tree for `max` illustrates this distinction between writing
a clause and making that clause a definitional equality. In GHC, flat
closed-family equations with the same result can be compatible. The
fork uses separate decision families so this compatibility cannot
skip an earlier test. [Agda case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees)
and [GHC family equation compatibility](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html#compatibility-and-apartness-of-type-family-equations).

At kind `Type`, GHC cannot enumerate the possible heads. A private
classifier with distinct Boolean results tests the written head.
Another family uses that result to select the next decision. A known
different head such as a list proceeds to a later alternative even
with an unknown element type. An unknown head or a stuck family
application blocks the test.

When an alias reorders fields, GHC follows the written order for
written tests. An alias can also add an inner constructor test.
Mixed patterns can use canonical field order when it visits the
surviving written tests once and in order. Other mixed shapes and
non-injective uses of erased pattern binders remain outside this slice.

At an indexed promoted kind, each constructor refines the index.
When ordinary unification fails, the compiler can try one nominal
reduction at each constructor result index using an imported or
previously checked closed-family axiom. This step can also establish
that a constructor is impossible at the scrutinee kind. An open or
unresolved index can still block an explicit constructor pattern.

An ordinary constructor can be tested at an open index. In
`OpenIndexPick`, `OpenPlain True` has the scrutinee kind for every
index. Its decision family tests that head before inspecting its field.
A different constructor can reach the fallback without solving its
index. An unknown head or field still blocks the fallback, even when
both rows return `Int`. The classifier takes the variables of the
scrutinee kind. An ordinary client can prove both outcomes using only
the provider interface.
[Agda case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees)
and [closed-family case translation, §4.5](https://richarde.dev/papers/2014/promotion/promotion.pdf).

A standalone kind signature can name an index under a noninjective family.
For example, a synonym with kind
`forall (b :: Bool) -> Maybe (Gate (Index b)) -> Type` can distinguish
`Just (Plain True)` from `Just (Indexed …)`. The caller supplies `b`
explicitly. An ordinary client can prove either result for a rigid `b`.
The synonym can also give both parameters their kinds at their binders.
GHC checks the lifted case family before the synonym when a later binder
kind uses an earlier parameter. Its constructor rows can then match at an
abstract index. The application supplies the enclosing `b` in `Index b`.
In `ScopedOpenIndex`, the first branch returns `Proxy b` for the outer
`b`. The next branch binds its own `b`. The fallback can refer to the
entire original scrutinee. An unknown constructor or field still blocks
the match. The change requires no injectivity assumption about `Index`.
Agda's with-abstraction also orders the arguments to a generated helper
according to the dependencies in their types.
[Source example](../examples/open-evaluation/NativeNestedCase.hs),
[interface proofs](../../../testsuite/tests/dependent/should_run/NativeTypeCaseClient.hs)
and [Agda's translation](https://agda.readthedocs.io/en/v2.8.0/language/with-abstraction.html#technical-details).

GHC also refreshes an older home interface for a lifted case with a
dependent capture. A provider with a lexical package rule might already
have its private helper. GHC retains that information when checking
the type-case upgrade. Changing the provider's ABI recompiles its client
through the ordinary dependency check.
[Mixed-provider upgrade test](../examples/open-evaluation/check-scoped-case-upgrade.sh).

In `IndexedFieldPick`, the constructor index `FieldIndex seed active`
reduces to `Bool`. The other index then unifies with `'True`, which
lets the nested pattern match `FieldOn`. The nominal coercion records
the refined index and keeps `seed` in scope for the other field.
An ordinary client checks the extracted field type through the
interface. A rigid leaf still blocks an earlier constructor test.
The reduced arguments must still fit the constructor kind. A reduction
that changes the kind of a later index needs explicit nominal transport.

A constructor can also fix an index that a closed family needs in
another index. In `RefinedBranchPick`, the outer constructor fixes
one tag to `True` and the inner constructor fixes a different tag to
`False`. Each tests its own tag and carries the same rigid seed.
The compiler collects the nominal equalities from family-free index
pairs at the current constructor. Under that substitution it tries
one reduction per family index and checks the whole result kind.
Each retry starts from the original indices. A recursive result
stays stuck. The first test of a rigid Boolean leaf still blocks the
same-result fallback, and either disjoint constructor can match.
[Agda indexed matching](https://agda.readthedocs.io/en/v2.8.0/language/coverage-checking.html#matching-on-indexed-datatypes).

The tests compile an ordinary client using only stripped interfaces.
They also check reification, exact printing and rejected rigid
equalities. On an upgrade, GHC rechecks a home module with an old
multirow type-case or type-lambda interface when the source is
available. A separately distributed provider must be rebuilt to
publish ordered equations. [Nested provider](../examples/open-evaluation/NativeNestedCase.hs)
and [interface test](../../../testsuite/tests/dependent/should_run/NativeTypeCase.sh).

## A case in a constructor field

An ordinary data constructor can choose a field type from an index
in its type. For example, the first two rows select `Int`, while
the disjoint `Nothing` row selects `Char`.

```haskell
data Box (d :: Maybe Bool) where
  Box :: forall (d :: Maybe Bool).
    (case d of { Just True -> Int; Just _ -> Int; Nothing -> Char }) -> Box d
```

An ordinary client can use the constructor at a known index or copy
the field at the same unknown index. Matching `Just b` with rigid
`b` still waits at the `True` test. A type-case pattern binder
shadows an enclosing constructor binder in its own row. The
constructor's field type keeps its helper private in the interface.
Each written field case has its own helper. To share a case between
fields, name the type synonym and use it from both fields.

The field rule applies to ordinary H98 and GADT arguments in their
declaration telescope. A field can also bind a written erased
`forall`. For example, the inner `b` in
`forall (b :: Bool). Proxy b -> (case b of …)` can shadow the data
parameter. Calling `f @'True Proxy` selects the branch for the inner
binder. The compiler keeps the quantifier in the field type.

A captured parameter can use several outer variables that the inner
forall shadows. For example, the kind of `a :: Tag k l` still uses
the outer `k` and `l` under `forall (k :: Type) (l :: Type)`. GHC saves
those arguments with type lets and restores their names inside the
copied kind. The inner binders keep their original scope. A stuck
noninjective family in a capture kind can still need a named helper
with a standalone kind signature.
[Provider](../examples/open-evaluation/NativeNestedCase.hs) and
[ordinary client](../../../testsuite/tests/dependent/should_run/NativeTypeCaseClient.hs).

In a GADT, a field-local `foreach` can supply the scrutinee. For example,
`One :: (foreach (b :: Bool) -> (case b of { True -> Int; False -> Char })) -> One`
keeps `b` as a runtime argument. A later field can also inspect a retained
argument in the constructor telescope. The helper copies the written
domain and keeps the original field scope. A field can also compute its
scrutinee with a checked function. In `NegatedCaseField`, the field type
tests `not b`. Its ordinary client reads `b` at runtime and gets a
character at `True` and an integer at `False`. An unqualified call uses
the function in scope in the declaration group. A rule that can change
that function blocks the proof. At an unknown index, the case still
waits for a matching row. The same check applies to a case in the
domain of an ordinary arrow in the field.
[Field source](../examples/open-evaluation/NativeNestedCase.hs).

### Quote and reify retained function types

`ForEachT` preserves the runtime binder, argument visibility and domain of
a retained function type. `IndexVarT` refers to that binder in the result.
The erased `ForallVisT` and `VarT` retain their meanings. The Showcase splices
a retained signature and calls the resulting function with a runtime Boolean.
The external-interpreter test also reifies a same-spelled erased and retained
binder from an interface, splices the type and calls the result. It checks the
type codec and rejects a runtime name used as an erased variable.
[Provider](../examples/open-evaluation/NativeRetainedTH.hs) and
[test](../../../testsuite/tests/dependent/should_run/NativeTypeCaseRetainedTh.sh).

The retained and erased binders follow the distinction in proposal #378.
Proposal #281 establishes visible erased quantification.
[Design #378](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html#quantifiers),
and [proposal #281](https://ghc-proposals.readthedocs.io/en/latest/proposals/0281-visible-forall.html).

An inline case in `[t| foreach (b :: Bool) -> case b of … |]` uses `CaseT`.
Its rows use the type-pattern syntax of closed family equations. A pattern
variable binds in the result of its own row. The scrutinee uses `IndexVarT`
for the actual retained binder. The case and its order survive the quotation.
The Showcase calls a function whose quoted signature has `()` between
the retained binder and its result case. It checks an `Int` result at
`True` and a `Char` result at `False`.
[Quoted signature](../examples/open-evaluation/NativeRetainedTH.hs).

A declaration splice can use the quoted type in `SigD`. The declarations
it returns can share a case helper within one group. A type splice can
supply the complete top-level value signature. Separate signature splices
in the same group can share a case on a retained binder when the capture
domains and ordered rows agree and their names have fixed identities.
The checker lifts the rows and preserves ordered reduction in both forms.
Each splice and its module finalizer run once in the ordinary order.
Use a named family to share a case across declaration groups or modules.
Nested type splices and raw cases lifted inside declaration quotes produce
a source diagnostic. Reification describes the checked signature and its
family applications.
[External-interpreter fixture](../../../testsuite/tests/dependent/should_run/NativeTypeCaseRetainedThRaw.hs),
[Template Haskell declaration groups](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/template_haskell.html#declaration-groups),
[proposal #378 scoping review](https://github.com/ghc-proposals/ghc-proposals/pull/378#discussion_r524560765),
and [Eisenberg and Stolarek, §4.5](https://richarde.dev/papers/2014/promotion/promotion.pdf).

### Quote and reify retained GADT constructors

`RetainedGadtC` carries the ordered fields and result of a prefix constructor.
A retained field binds its runtime name over later field types. An ordinary
field carries its source strictness and multiplicity. The bound name remains
in the declaration even when the constructor type simplifies it to an arrow.
Ordinary `GadtC`, `RecGadtC` and `ForallC` keep their representation.
The external-interpreter test reifies a declaration from a stripped interface
and splices a copy. It checks lazy fields, erased binders and the constructor
codec. The Showcase passes its runtime input and dependent payload to a
constructor declared in a quote.
[Provider](../examples/open-evaluation/NativeRetainedConTH.hs) and
[test](../../../testsuite/tests/dependent/should_run/NativeTypeCaseRetainedConTh.sh).

The current source checker rejects a retained name in the final result, a
retained telescope with record fields, and strictness annotations after the
first retained binder. Required erased forall in a constructor declaration
still receives the Template Haskell diagnostic. Proposal #402 separates
record fields and prefix arguments. Its review considers the namespace of a
constructor binder. Eisenberg's thesis describes ordered dependencies among
constructor arguments.
[Proposal #402](https://ghc-proposals.readthedocs.io/en/latest/proposals/0402-gadt-syntax.html),
[review #402](https://github.com/ghc-proposals/ghc-proposals/pull/402#issuecomment-801420999),
and [thesis §5.4.1](https://www.cis.upenn.edu/~sweirich/papers/eisenberg-thesis.pdf).

## A case in a value signature

A top-level function can write a case directly in its argument and result
types. Both cases below wait when `b` is unknown. The function body still
typechecks because the two occurrences share a generated family.

```haskell
caseIdentity :: forall (b :: Bool). Proxy b ->
  (case b of { True -> Int; False -> Bool }) ->
  (case b of { True -> Int; False -> Bool })
caseIdentity _ input = input
```

The renamer compares names, promotion marks, applications, wildcards
and row order in one signature. Extra parentheses leave the case
unchanged. Distinct scrutinees and reordered rows have separate helpers.
The ordinary client can use the function at `Int` and `Bool` from
its interface. Template Haskell can
reify the signature in its owner or a client. A stuck mismatch names the
source signature. An explicit nested erased `forall` can name its
own scrutinee. Each nested telescope starts a fresh scope, even when
two binders have the same spelling. A `foreach` binder can also supply
a retained constructor index with a written algebraic domain. A bare
function arrow copied from a runtime domain can change matchability
in an erased kind, so that case still requires a named helper.
[Source](../examples/open-evaluation/NativeCase.hs),
[client](../../../testsuite/tests/dependent/should_run/NativeTypeCaseClient.hs)
and [reification](../../../testsuite/tests/dependent/should_run/NativeTypeCaseReify.hs).

### Compute a retained scrutinee

A retained signature can match the result of an application tree.
This signature uses ordinary Prelude imports and the checked R16
equations for both calls to `not`.

```haskell
pick :: foreach (b :: Bool)
     -> (case not (not b) of { True -> Int; False -> Bool })
pick True = 42
pick False = False
```

A call can use two retained indices. The supplied order determines
which operands the source equations inspect. The private synonym
receives the original indices and preserves that order. A checked
equation may use the first constructor without inspecting the second
operand. Unknown inputs still block an earlier test. A visible rule
that can change a used source equation causes an error.
[R16 checked functions](R16-open-term-evaluator.md) and
[source and interface client](../../../testsuite/tests/dependent/should_run/NativeTypeCaseClient.hs).

Identical retained cases in a declaration group can share one generated
family when their captured domains are closed and their rows have no
local captures or pattern binders. The comparison uses each retained
operand's position in the captured telescope. It preserves the
application tree and row order. The lookup runs after the declaration
group's binders enter scope. Two import aliases can share a case when
they resolve to the same checked source family. Each alias still counts
as a use of its import. A shadowed variable keeps the identity of its
own runtime binder. A different checked definition with the same
spelling has a different identity.

Across modules, export a named case result to give both signatures the
same family. Infix calls, explicit kind applications and local callable
heads remain outside this lifting rule.

An old home module can still have separate private families for two
equivalent retained signatures. GHC rechecks the provider when an
exported retained signature refers to a local lifted case. The next
build reuses the refreshed interface. If sharing changes an exported
type, ordinary dependency tracking recompiles its clients. A client
that has only the old interface can still fail at an unknown index,
even when calls at known constructors work. Rebuild and reinstall an
older installed provider to publish its shared family. A client build
cannot rebuild the installed package.
[Two-compiler upgrade check](../examples/open-evaluation/check-retained-case-upgrade.sh).

## A case in a function arrow

A top-level synonym can select the input or result type of a function
arrow, including a linear or multiplicity-polymorphic arrow.

```haskell
type Input b = (case b of { True -> Int; False -> Char }) -> ()
type Result b = () -> (case b of { True -> Int; False -> Char })
type LinearInput b = (case b of { True -> Int; False -> Char }) %1 -> Int
type PolyInput b = forall (p :: Multiplicity).
  (case b of { True -> Proxy p; False -> Char }) %p -> Proxy p
```

Both sides share the arrow's lexical scope. The private family takes
the same captures as a case at the root of the synonym. The original
arrow retains its multiplicity, and `p` in the case and in `%p` refers
to the same binder. An ordinary client can use `Input 'True` as
`Int -> ()` and `Result 'True` as
`() -> Int` through a stripped interface. Choosing a domain type adds
no runtime demand on the argument.
Template Haskell reification preserves the arrow and the exact name of
the lowered case family. [Serokell roadmap](https://ghc.serokell.io/dh#type-level-case-expressions),
[LinearTypes proposal #111](https://ghc-proposals.readthedocs.io/en/latest/proposals/0111-linear-types.html),
[Modifiers review #370](https://github.com/ghc-proposals/ghc-proposals/pull/370),
[promotion paper, §4.5](https://richarde.dev/papers/2014/promotion/promotion.pdf),
[Agda pattern lambdas](https://agda.readthedocs.io/en/v2.8.0/language/with-abstraction.html#pattern-lambdas),
and [interface test](../../../testsuite/tests/dependent/should_run/NativeTypeCase.sh).

An invisible `forall` can bind variables used by the case. For example,
`forall (b :: Bool). Flag b -> (case b of { True -> Int; False -> Char })`
selects the result at each instantiation of the function type. The
private family takes `b` as a parameter. The application stays under
the original `forall`. Binder names must be distinct from the
surrounding captures.

In `forall k (tag :: k) (d :: Maybe Bool).`, the helper takes `k` before
`tag`. A case on `d` uses its written kind. If there is no dependency
cycle, the helper generalizes before the synonym uses it. An annotation
such as `(case d of …) :: TYPE r` gives the helper a result kind that
depends on the captured `r`. GHC asks for an annotation if inference
introduces a new variable in the result kind. A real cycle uses the
ordinary rules for recursive kinds.
[Quantified example](../examples/open-evaluation/NativeNestedCase.hs)
and [Agda function types](https://agda.readthedocs.io/en/v2.8.0/language/function-types.html).

## A case in a let body

A case branch can use a let-bound type or kind. For example,
`let k = PayloadKind a in case b of { False -> Maybe k; True -> [k] }`
checks with `a` still unknown. The branches and the original let refer
to one private synonym. The synonym takes the RHS's lexical captures,
including any variables named in their kinds. A case nested in the
scrutinee inherits the surrounding bindings. A branch pattern or
inner let can shadow a name in its own scope. The ordinary
interface-only client verifies both kinds of shadowing.
[Provider](../examples/open-evaluation/NativeCase.hs)
and [client](../../../testsuite/tests/dependent/should_run/NativeTypeCaseClient.hs).

The compiler checks the helper RHS once in each declaration-checking
pass. Each row substitutes its checked captures while sharing the RHS's
unresolved kind variables. For example, both branches of
`let m = 'Nothing in case b of …` can constrain `m` to `Maybe Bool`.
The compiler rejects incompatible uses and unresolved variables outside
the capture scope. Two separate lets can infer different kinds.

The supported RHSs use names, type applications, promoted lists and tuples,
kind annotations and type literals. A helper can infer the kind of a visible
capture. An independent kind parameter remains ambiguous.

In a chain of lets leading to a case, an inner RHS can refer to an
enclosing let. For example,
`let x = a in let x = Maybe x in case d of …` takes the second RHS's
`x` from the first let and uses the second `x` in the case body.
The compiler checks the helpers in dependency order and shares their
inferred kinds across rows. The first constructor test can still block
the fallback at a rigid index. RHS splices remain outside this slice.

The private synonym follows the promotion paper's lifting of local
bindings, and the rows follow its closed-family encoding of cases.
Agda checks `let` as a local abbreviation and uses pattern lambdas for
constructor cases inside expressions.
[Eisenberg and Stolarek, §§4.5 and 4.7](https://richarde.dev/papers/2014/promotion/promotion.pdf),
[Agda local definitions](https://agda.readthedocs.io/en/stable/language/let-and-where.html#let-expressions)
and [Agda pattern lambdas](https://agda.readthedocs.io/en/stable/language/with-abstraction.html#pattern-lambdas).


### Transport of dependent constructor indices

A nominal reduction of an earlier constructor argument can change a later
argument's kind. Lift the binder kind over the preceding argument coercions.
Cast the later index with that evidence before matching the constructor.
The source fixture refines a kind to `Bool`, then checks a value of that kind
and a `Proxy` indexed by the value. Its ordinary client proves both branches.
A stuck family still blocks the refinement.
[Constructor and type case](../examples/open-evaluation/NativeNestedCase.hs).
