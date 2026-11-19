# R21. Dependency analysis

Serokell marks this card accepted. It requires the compiler to order
top-level declarations when a type depends on a term. R13 and R20 both
list R21 as a dependency. M126 extends the fork's declaration ordering
to qualified local calls handled by the open evaluator.
[Serokell card](https://ghc.serokell.io/dh#dependency-analysis),
[declaration analysis](https://serokell.io/blog/dependency-analysis-haskell),
and [declaration scheduling](../../../compiler/GHC/Tc/Module.hs).

## M126 source check

```haskell
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Example where
import Data.Kind (Type)

type Result :: Type -> Type
type Result a = Example.keep a

keep :: a -> a
keep x = x
```

The renamer records the resolved term Name and its source location.
When an independent producer with a complete signature belongs to the
same declaration group, the compiler checks it before the dependent
type, regardless of source order. The compiler solves and zonks the
producer before it gives the observer checked Core. The observer
returns the original rigid type argument or reports a demand it cannot
satisfy. The producer's ordinary forall binds a type parameter.
[Open evaluator](../../../compiler/GHC/Tc/Gen/OpenTypeEval.hs).

A later group can use a producer whose constraints the compiler solved
before an intervening top-level splice. The compiler reuses the checked
bindings and evidence on demand. It rejects pending typed splices in a
producer before zonking can execute them. A definition after a splice
stays out of scope before that splice. A direct cycle through the
producer's signature is reported at the type-level call.
[Local open evaluation example](../examples/open-evaluation/LocalOpen.hs)
and [source and interface regression](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

An ordinary client compiled with only the exported interface. Changing
the producer to select its second argument changed the exported synonym.
The unchanged client then failed where it supplied a Boolean for the
newly selected `Char` type.

## M128. Following a local dependency chain

The scheduler starts at the function mentioned in a type and follows
its local dependencies. It uses the renamer's existing value-binding
groups and their order. Ordinary inference can supply the types of
both the function and its helpers. A shared signature can cover a
selected function and an unrelated one. Each group keeps the part
that belongs to its own bindings.

For example, `type LastOf a b = lastOf a b` observes the ordinary
definition `lastOf x y = select y x`. When `select x _ = x`, the
result type is `b`. This unqualified use follows the usual term lookup
with `RequiredTypeArguments`. An explicit `forall a.` also lets `pick a`
refer to the local function. In an ordinary implicitly quantified
signature, `pick` can still bind a type variable. The regression supplies
`@Maybe` for that variable with visible type application. An explicitly
bound type variable takes precedence too. The fallback for an unqualified
ordinary function applies to a spine of named variables or pairs of
named variables. Closed constructor inputs retain the checked-equation
lookup and its diagnostics for incomplete clauses or invalid recursion.
[Source example](../examples/open-evaluation/LocalOpen.hs).

Scheduling a helper makes its checked body available. Its rewrite rules
affect the proof only if the observer uses that body. In the regression,
`select x (unused y)` returns `x`. A rule on `unused` is accepted.
Adding a rule that changes `select` to choose its second argument is
rejected at the rule. Changing the two-calls-deep helper's definition
also invalidates the unchanged ordinary client.
[Rule and recompilation checks](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

The scheduler can follow previously solved helpers across a top-level
splice. A pending typed splice in a reachable body still blocks early
observation. This slice reports a recursive term group and a cycle through
a type declaration separately. Mixed dependencies on same-group type
declarations, local instances, nonempty deriving, defaults and deferred
errors remain open.

## M131. Mutually recursive value bindings

GHC can typecheck a recursive group of ordinary functions before
observing a call. It keeps every member of the renamer's group together.
In `select x (loop y)`, the selector returns `x` without evaluating the
recursive argument. A call that does demand the loop spends the open
evaluator's reduction budget and reports GHC-70566 at the call. This
checks the particular observation. A declaration cycle in which a type
requires its own unfinished producer still fails before observation.
[Runnable selection](../examples/open-evaluation/LocalOpen.hs)
and [cycle and demand controls](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

Signed and inferred mutually recursive functions both work. For an
inferred group, the typechecker already records the monomorphic binder
that implements each polymorphic source function. The observer uses
that correspondence when it demands a recursive reference. A rule on
the other function therefore fails if its body is read and succeeds
if the argument containing that call is ignored.
[Inferred group with a source rule](../../../testsuite/tests/dependent/should_run/NativeLocalOpenSibling.hs).

## M134. Local type and value dependencies

A function used in a type can use constructors and type synonyms from the
same module. In the source example, `route` passes the constructor
`Second` to a helper. The helper's signature mentions the local datatype
`Route` and the local synonym `ResultOf`. The renamer has already put each
type declaration with its standalone kind signature, role annotations,
and mutually recursive peers. The scheduler moves the required groups
into scope before checking the helper and observing `route`.
[Local routing example](../examples/open-evaluation/LocalOpen.hs).

Sometimes a prerequisite type itself calls another local function.
For example, `type Final a = outer a`, `outer :: a -> Middle a`,
and `type Middle a = inner a` require two passes. First GHC checks
`inner`. With its definition available, GHC can check `Middle`, then
`outer`, then `Final`. The declarations can appear in reverse order.
If a helper's result synonym refers back to the consumer itself, GHC
reports the type dependency cycle at the original call. Local instances
and nonempty deriving clauses still require more declaration analysis.
[Acyclic and cyclic source checks](../../../testsuite/tests/dependent/should_run/NativeLocalOpen.sh).

## Closed calls

An ordinary qualified call on a known closed literal still requires an
imported definition. GHC-70565 reports that restriction at the call
head. The regression first compiles the producer without the type-level
call, then adds a closed call whose equality would hold if the compiler
used the stale object. The compiler rejects the call with the old
object present. R15 tracks interpreter execution separately.
[Type-functions draft](https://github.com/ghc-proposals/ghc-proposals/pull/509)
and [R15](R15-closed-term-evaluator.md).
