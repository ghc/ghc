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

The first slice schedules qualified calls and independent same-group
producers. A local dependency on another binding in that group,
nonempty deriving, local instances, defaults, and deferred errors
remain outside this scheduling path. Unqualified references remain
a follow-up.

## Closed calls

An ordinary qualified call on a known closed literal still requires an
imported definition. GHC-70565 reports that restriction at the call
head. The regression first compiles the producer without the type-level
call, then adds a closed call whose equality would hold if the compiler
used the stale object. The compiler rejects the call with the old
object present. R15 tracks interpreter execution separately.
[Type-functions draft](https://github.com/ghc-proposals/ghc-proposals/pull/509)
and [R15](R15-closed-term-evaluator.md).
