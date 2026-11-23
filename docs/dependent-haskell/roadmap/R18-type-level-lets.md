# R18. Type-level let-bindings

Serokell's example names the result of a type-level computation and
uses it twice. The card lists dependent Core (R05) and open evaluation
(R16) as prerequisites. [Upstream roadmap](https://ghc.serokell.io/dh#type-level-let-bindings).

```haskell
type F a = let b = G a in H b b
```

With `ExperimentalDependentTypes`, GHC checks the definition of `b`
in the enclosing scope. The two uses in `H b b` receive the same
checked type and share its kind metavariables. In `let b = b in …`,
the right-hand `b` refers to an outer binding. An unused definition
still has to kind-check. Agda uses the same scope rule for local
abbreviations and describes their meaning by substitution. [Agda
let-expressions](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-expressions).

In the example, `G Bool` reduces to `'True` and `H 'True 'True`
reduces to `Ordering`. An ordinary client proves `F Bool :~: Ordering`
from the provider's interface. The client needs no local `let` in
its own source. [Provider](../examples/open-evaluation/TypeLet.hs)
and [interface and scope checks](../../../testsuite/tests/dependent/should_run/NativeTypeLet.sh).

A retained vector result can use `let k = 1 + n in Vec a k` when
`n` comes from an `IORef`. At `n = 2`, the example returns three
characters. Taking the first character also succeeds with an exceptional
tail length. [Runtime example](../Showcase.hs).

A let RHS can also match a constructor. In
`let flag = case flag of { True -> Bool; False -> Char } in Either flag flag`,
the case inspects the outer Boolean. Both arguments to `Either` use the
locally bound result. The provider checks an unused case RHS, and an
ordinary interface client must know the constructor to prove which result
type was selected. The [R19 card](R19-type-level-cases.md#a-case-in-a-let-body)
describes the cases that can use a let-bound value in their branches. [Agda let scope](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-expressions)
and [Eisenberg and Stolarek on case lifting, §4.5](https://richarde.dev/papers/2014/promotion/promotion.pdf).

The source form supports simple variable bindings and nested lets.
A synonym or closed family equation can bind a type lambda on the RHS
and pass it to another family. A lambda in the body can capture the let
result. For example, `ScopedRow Char '[Bool]` gives
`'[(Maybe Char, Either Char Bool)]`. Its first callback captures `Char`
before an inner let binds the same name to `Maybe Char`. Its second
callback uses both types. An ordinary interface-only client constructs
a value of the resulting row type. General local patterns and cases remain open.
[Scoped row](../examples/open-evaluation/TypeLambda.hs)
and [client](../../../testsuite/tests/dependent/should_run/NativeTypeLambda.sh).

## Release check

Serokell defines the feature as naming a computed type and using it
again in the same type expression. The fork checks these source
properties before marking R18 complete.

- [x] Keep the parameter abstract in `let b = G a in Either b b`.
  Both uses of `b` must refer to the same type in a signature, field,
  GADT result, synonym, closed family equation and open instance.
  [Six source contexts](../../../testsuite/tests/dependent/should_run/NativeTypeLetContexts.hs).
- [x] Check an unused right-hand side and diagnose a forward reference.
  Resolve an earlier binding in the scope where it was defined after a
  later binding shadows its name. Use the exported result in an ordinary
  client with the provider source hidden. Reject an unfounded equality.
  [Scope and diagnostic checks](../../../testsuite/tests/dependent/should_run/NativeTypeLet.sh).
- [x] Preserve the result of inserting an intermediate alias and of
  renaming a local binder that shadows an enclosing kind. Jobs 1615
  and 1619 checked these on compiler components.
  [Intervening abbreviation](#an-intervening-abbreviation).
- [x] Normal compiler `b6ddbb4ce8` passed the source, quotation and
  exact-print registrations. Jobs 1627 and 1649 checked the combined
  compiler and both spellings of the dependent-kind binder. The 566-line
  Showcase matched in three modes. [Verification record](../CHECKPOINTS.md#lexical-type-lets-and-cases-in-closed-family-equations).

The fork also supports projection patterns for pairs and regular named
products. The latter have a separate quotation restriction. The
upstream dependency graph also tracks dependent Core and open
evaluation. Each has its own release criteria.
[Product patterns](#a-named-product-binding) and
[Serokell's card and dependencies](https://ghc.serokell.io/dh#type-level-let-bindings).

## Ordered binding groups

The fork accepts a nonempty group of simple bindings under
`ExperimentalDependentTypes`. Braces with semicolons and layout have the
same meaning. These two definitions give the same result.

```haskell
type Braced a = let { b = a; b = Maybe b } in b

type Layout a =
  let b = a
      b = Maybe b
  in b
```

Each right-hand side sees the enclosing scope and the preceding bindings.
The most recent binding of a name is visible in subsequent right-hand
sides and the body. Thus `Maybe b` uses the first `b`, and the body uses
the second. In `let { b = a; a = Bool } in b`,
the first right-hand side uses an enclosing `a` if one is in scope.
A signature with implicit quantification can bind that free `a` as a type
parameter. In a type synonym with no enclosing `a`, the reference is out of
scope. Each right-hand side must kind-check, including unused bindings and
bindings later shadowed.

For example, `type Bad = let { a = b; b = Bool } in Maybe a` reports
GHC-76037, “Not in scope: type variable ‘b’”, at the first use of `b`.
The normal compiler stages checked both forms of the group.
[Verification](../CHECKPOINTS.md#ordered-type-let-source-checkpoint).

Agda's manual specifies scope from earlier definitions to later ones. Its
compiler handles each simple binding without a signature separately and
hides only the new binder while resolving its right-hand side. For
repeated names, that exposes the preceding binding.
[Agda manual](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-expressions),
[Agda declaration processing](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/Concrete/Definitions.hs#L319-L341),
[Agda let scoping](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/Translation/ConcreteToAbstract.hs#L1609-L1625),
[Agda scope operations](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Syntax/Scope/Monad.hs#L519-L529)
and [Agda association-list operations](https://github.com/agda/agda/blob/v2.8.0/src/full/Agda/Utils/AssocList.hs#L46-L64).

Haskell 2010 term `let` makes the group mutually recursive. Every
declaration is in scope in every right-hand side. The open term-level
`RecursiveLet` proposal (#401) discusses sequential shadowing as an
alternative. [Haskell 2010 §3.12](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-440003.12)
and [RecursiveLet discussion](https://github.com/ghc-proposals/ghc-proposals/pull/401#issuecomment-791233011).

The `ExtendedLet` draft (#523) spells grouped type `let` without a `type`
keyword and forbids recursive synonyms. Its scope clause names the body.
The fork specifies the scope of the right-hand sides and repeated binders
using the Agda rule. In review, Simon Peyton Jones questioned the benefit
relative to the added complexity. Adam Gundry suggested considering
`let` in types separately from local type synonym declarations. Richard
Eisenberg closed the draft without acceptance.
[ExtendedLet draft](https://github.com/goldfirere/ghc-proposals/blob/f7861dfa72f9a92dee36486940dea6b2ba9b38bd/proposals/0000-extended-let.rst#L109-L136),
[Peyton Jones's review](https://github.com/ghc-proposals/ghc-proposals/pull/523#issuecomment-1346449731),
[Gundry's suggestion](https://github.com/ghc-proposals/ghc-proposals/pull/523#issuecomment-1350590569)
and [Eisenberg's decision](https://github.com/ghc-proposals/ghc-proposals/pull/523#issuecomment-1372209793).

## Local kind signatures

A single-name kind signature can immediately precede its binding in a
layout or braced group. For example, `let { width :: Nat; width = 1 + n }
in Vec a width` checks the right-hand side at kind `Nat`. The signature
and right-hand side see the same preceding bindings. In
`let { tag = Bool; tag :: tag; tag = 'True } in tag`, the signature
uses the first `tag` and the body uses the second. An unused binding must
still match its signature.

The fork uses the same checks as a kind ascription on the right-hand
side. Agda also permits optional signatures before local definitions in
source order. Haskell 2010 term `let` permits signatures as declarations
throughout a mutually recursive group. The closed `ExtendedLet` draft
specified binding equations for type `let`. The adjacent signature is a
fork design. [Agda](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-expressions),
[Haskell 2010 §3.12](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-440003.12),
[Haskell 2010 §4.4](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-780004.4),
and [ExtendedLet](https://github.com/ghc-proposals/ghc-proposals/pull/523/files).

### Sharing an arrow kind

A local name also shares any matchability inferred for its right-hand
side. Consider a synonym with two parameters and a standalone kind
signature.

```haskell
type Higher :: let k = Type -> Type in k -> k
type Higher f a = f a
```

The arrow for the synonym's second parameter must be unmatchable.
That fixes the shared `k` to `Type %U -> Type` in both positions.
An ordinary client gets GHC-83865 for `Higher Maybe Bool` because
`Maybe` has a matchable arrow. In the following signature the input
and result arrows are written separately. The input defaults to
matchable, so the same client compiles.

```haskell
type Higher :: (Type -> Type) -> Type -> Type
```

Jobs 1629 and 1630 compared the compiled provider interfaces and
compiled independent clients on 564. With two explicit `%U` arrows,
the expanded and let-bound forms reject the same client. With two
explicit `%M` arrows, both reject the two-parameter declaration.
The one-parameter definition `type Higher f = f` also works with the
original let-bound signature and `Maybe`. Its second arrow is in the
result of the synonym, beyond its declared arity.
[Proposal #242 on declaration kinds and defaults](https://ghc-proposals.readthedocs.io/en/latest/proposals/0242-unsaturated-type-families.html#inference)
and [matchability syntax and provenance](../REFERENCES.md#matchability-and-multiplicity-on-arrows).

## A boxed pair binding

```haskell
import qualified GHC.TypeNats as TypeNats

type DimensionSum (dimensions :: (TypeNats.Nat, TypeNats.Nat)) =
  let (rows, cols) = dimensions
  in rows TypeNats.+ cols
```

A field in a boxed pair pattern can be a name, a wildcard, or another
boxed pair pattern. Names in one pattern must be distinct. The right-hand
side sees the enclosing scope and the preceding bindings.
GHC checks it at a pair kind before extending the scope with the named
fields. An unused field need not reduce. A known promoted constructor
selects its fields. The Showcase prints `5` for `DimensionSum '(2, 3)`.
An ordinary client proves that equality from the provider interface. At
an unknown pair, the projections stay in the type. Rebuilding it from its
projections gives no equality proof with the original pair. [Provider](../examples/open-evaluation/TypeLet.hs)
and [Showcase](../Showcase.hs).

The fork uses closed families for the projections. Agda also translates
local record patterns to projections and separately gives its records an
eta rule. The fork's pair bindings use the existing type-family equality
rules. The base test `CatPairs` uses the same constructor equations for
pairs of types. Test `T14010` uses poly-kinded closed equations for those
projections. [Agda record lets](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-binding-record-patterns),
[Agda eta](https://agda.readthedocs.io/en/v2.8.0/language/record-types.html#eta-expansion),
[CatPairs](../../../libraries/base/tests/CatPairs.hs),
[T14010](../../../testsuite/tests/typecheck/should_compile/T14010.hs),
and [interface and scope checks](../../../testsuite/tests/dependent/should_run/NativeTypeLet.sh).

In `let ((width, _), height) = dims in …`, selecting `width` takes the
first field of the first pair. Selecting `height` takes the second field
of `dims`. Each nested pair must have a pair kind. An unused nested pair
need not reduce to a constructor. Agda also composes projections for
nested record patterns. [Agda record pattern translation](https://agda.github.io/agda/src/Agda.TypeChecking.RecordPatterns.html).

## A named product binding

`let Dimensions rows cols _ = dimensions in rows TypeNats.+ cols` binds
fields of a regular datatype with one constructor. GHC checks that the
resolved constructor is the sole constructor of its datatype, the pattern
supplies all fields, and the scrutinee has that datatype as its kind. The
checks also run for a pattern with only wildcards.

Each field can be a name or a wildcard. A generic application split and
the pair selectors expose the fields. A closed family compares the actual
head with the written constructor before returning a field. An unknown
head leaves the selection stuck. The Showcase adds `2` and `3` while
leaving `UnknownDepth` stuck. The new patterns are available in source
types. A quotation rejects them until Template Haskell can represent
the constructor check. [Agda record-pattern lets](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-binding-record-patterns),
[example](../examples/open-evaluation/TypeLet.hs) and
[source-hidden checks](../../../testsuite/tests/dependent/should_run/NativeTypeLet.sh).

## Type quotations and splices

A type quotation represents `let a = rhs in body` as `LetT a rhs body`.
The right-hand side sees the enclosing scope. Each quotation allocates a
fresh name for the binder. In the following type, the argument of
`Maybe` refers to the first binder. Both arguments of `Either` refer
to the second binder.

```haskell
[t| let a = Bool in let a = Maybe a in Either a a |]
```

Splicing the quotation checks the type at the splice site.
A local kind signature appears as `SigT` on the right-hand side of `LetT`.
A pair binding appears as a fresh temporary `LetT` followed by a `LetT`
for each projected field. A nested pair has a separate fresh temporary.
An ordinary client can import a value with that type and observe
`Right (Just True)`. Reification of a checked type can expand the
abbreviation. The TH constructor and `letT` smart constructor are
specific to this fork. Serokell gives the type-level let example.
The accepted design sketch in proposal #378 describes a separate use of
`let`. A term-level let-bound name can appear in a type as an opaque
variable. Eisenberg's review explains the aim of sharing term and type
syntax. [Term variables in types](https://ghc-proposals.readthedocs.io/en/latest/proposals/0378-dependent-type-design.html#term-variables-in-types)
and [review discussion](https://github.com/ghc-proposals/ghc-proposals/pull/378#discussion_r524560765).
[Quotation and splice test](../../../testsuite/tests/dependent/should_run/NativeTypeLetQuote.hs).

## A vector with an optional header

`Frame n b a` checks `width = 1 + n` at kind `Nat` before matching `b`.
The header branch returns a `Vec a width`. The other branch returns a
`Vec a n`. An ordinary client passes the same runtime flag to `frameRow` and
`frameToList`. It obtains `":xy"` or `"xy"` from the same two cells.
The second branch leaves the unused header unevaluated.
[Source](../examples/open-evaluation/TypeLet.hs) and
[ordinary client and width checks](../../../testsuite/tests/dependent/should_run/NativeTypeLet.sh).

The let RHS keeps its infix syntax for normal fixity resolution. A
backticked type variable can supply the operator, so the helper
captures the operator as well as variables from its two operands. A function
RHS can also mention a multiplicity parameter in its arrow.
[GHC's infix type syntax](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/infix_tycons.html).

## Using a previous case result as a default

A later case can refer to a type computed by an earlier binding.
Here the first `width` on the right refers to the Boolean parameter.
The `Nothing` branch uses the computed width. The `Just` pattern binds
its own `width` and returns it even if the Boolean parameter is unknown.

```haskell
type DimensionWithOverride (width :: Bool) (override :: Maybe TypeNats.Nat) =
  let width = case width of { True -> 3; False -> 2 }
      result = case override of { Nothing -> width; Just width -> width }
  in result
```

The value signature uses the wildcard fallback
`Just provided -> provided; _ -> width`. Component job 1606 checked
the result through an ordinary client with only the provider interface.
The client can prove that an explicit override wins. An attempt to
prove a particular default with the Boolean still unknown fails.
[Example](../examples/open-evaluation/TypeLet.hs) and
[interface and scope checks](../../../testsuite/tests/dependent/should_run/NativeTypeLet.sh).

The renamer passes the earlier result as an argument to the lifted
family for the later case. A pattern binder shadows that argument
inside its own branch. This preserves the source scope when GHC moves
the case to a top-level family. See Note [Lifting type cases]
in `GHC.Rename.Module`. Agda uses ordered scope for its local
definitions. Section 4.5 of Eisenberg and Stolarek describes lifting a
case and passing the in-scope bindings to its generated family.
[Agda local definitions](https://agda.readthedocs.io/en/v2.8.0/language/let-and-where.html#let-expressions)
and [Eisenberg and Stolarek](https://richarde.dev/papers/2014/promotion/promotion.pdf).

Job 1598 also checked `let b = G a in Either b b` with `a` still
abstract on the published 561 compiler. It compiled in a value
signature, a datatype field, a GADT result index, a type synonym,
a closed family equation and an open family instance.
[Six-context regression](../../../testsuite/tests/dependent/should_run/NativeTypeLetContexts.hs).

### An intervening abbreviation

Component job 1610 compared this synonym with the version obtained by
removing `saved = width` and using `width` in the `Nothing` branch.

```haskell
type Width (flag :: Bool) (override :: Maybe Nat) =
  let width = case flag of { True -> 3; False -> 2 }
      saved = width
      result = case override of { Nothing -> saved; Just n -> n }
  in result
```

Both spellings give `7` for a `'Just 7` override with `flag` still
abstract on normal compiler `b6ddbb4ce8`. Job 1615 also checked two
successive aliases and a branch binder that shadows an earlier name.
The preceding R18 component rejected the intervening alias with GHC-97233. The
renamer now follows references through each right-hand side in scope
order when it determines which values to pass to the lifted case.

Scope also matters for a kind. In `(k :: Type) (x :: k)`, a local
binding named `k` may change what a branch means by `Maybe k`.
The `k` in the previously declared kind of `x` must still refer to
the enclosing parameter. Job 1619 checked both the original spelling
and the version with a fresh name for the local binding. An ordinary
client could select the known result and GHC rejected a wrong equality.
[Source and client](../../../testsuite/tests/dependent/should_run/NativeTypeLet.sh).
