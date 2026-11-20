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

GHC checks one equation of a private closed family per alternative.
Each equation receives the lexical captures and scrutinee. An error in
the client can display the checked alternatives using the interface.
The lifting follows the promotion of case expressions described by
Eisenberg and Stolarek. Agda's pattern lambdas provide a related way
to match on an intermediate expression. [Promotion paper, §4.5](https://richarde.dev/papers/2014/promotion/promotion.pdf)
and [Agda pattern lambdas](https://agda.readthedocs.io/en/stable/language/with-abstraction.html#pattern-lambdas).

The supported patterns have a constructor head and distinct variables or
wildcards for fields. General nested cases, catch-all alternatives and
case expressions inside arbitrary types remain open.

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

### Nested patterns at kind `Type`

The next source case selects `Int` for `Just Bool`, `Char` for
`Just Ordering`, and `()` for another known type. The compiler
cannot enumerate all types when it reaches the field of `Just`.
A closed family can instead classify the written heads:

```haskell
data Head = B | O | Other
type family Classify (a :: Type) :: Head where
  Classify Bool     = 'B
  Classify Ordering = 'O
  Classify a        = 'Other
```

The three results are distinct. GHC can choose `Other` for `[x]`
even when `x` is unknown. It must wait when the whole type is unknown
or a type family can still reduce to `Bool` or `Ordering`. A second
family with one equation per `Head` then selects the source result.
An ordinary client checked these properties through a stripped interface.
The unknown case stayed stuck even when every final branch returned
`Int`. Translating the nested source pattern to these families is open.
[Agda case trees](https://agda.readthedocs.io/en/v2.8.0/language/function-definitions.html#case-trees)
and [GHC family equation compatibility](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html#compatibility-and-apartness-of-type-family-equations).

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
