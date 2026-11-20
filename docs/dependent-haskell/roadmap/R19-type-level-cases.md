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

Patterns have a constructor head. Fields can contain constructors,
distinct variables or wildcards. Catch-all alternatives and case
expressions inside arbitrary types remain open.

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
that a constructor is impossible at the scrutinee kind. Open or
unresolved index applications can still prevent compilation.

The tests compile an ordinary client using only stripped interfaces.
They also check reification, exact printing and rejected rigid
equalities. On an upgrade, GHC rechecks a home module with an old
multirow type-case or type-lambda interface when the source is
available. A separately distributed provider must be rebuilt to
publish ordered equations. [Nested provider](../examples/open-evaluation/NativeNestedCase.hs)
and [interface test](../../../testsuite/tests/dependent/should_run/NativeTypeCase.sh).

## A case as an arrow result

A top-level synonym can select the result of an ordinary arrow.

```haskell
type Result b = () -> (case b of { True -> Int; False -> Char })
```

The result shares the arrow's lexical scope. The private family takes
the same captures as a case at the root of the synonym. An ordinary
client can use `Result 'True` as `() -> Int` through a stripped interface.
Template Haskell reification preserves the arrow and the exact name of
the lowered case family. [Serokell roadmap](https://ghc.serokell.io/dh#type-level-case-expressions),
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
Splices and RHSs that refer to another enclosing type let remain outside
this slice.

The private synonym follows the promotion paper's lifting of local
bindings, and the rows follow its closed-family encoding of cases.
Agda checks `let` as a local abbreviation and uses pattern lambdas for
constructor cases inside expressions.
[Eisenberg and Stolarek, §§4.5 and 4.7](https://richarde.dev/papers/2014/promotion/promotion.pdf),
[Agda local definitions](https://agda.readthedocs.io/en/stable/language/let-and-where.html#let-expressions)
and [Agda pattern lambdas](https://agda.readthedocs.io/en/stable/language/with-abstraction.html#pattern-lambdas).
