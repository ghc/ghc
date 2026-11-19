# R21. Dependency analysis

Serokell marks this card accepted. It requires the compiler to order
top-level declarations when a type depends on a term. R13 and R20 both
list R21 as a dependency. In this fork, the renamer already separates
some local producers of checked family equations. An ordinary function
used by the open evaluator still needs to come from another module.
[Serokell card](https://ghc.serokell.io/dh#dependency-analysis),
[declaration analysis](https://serokell.io/blog/dependency-analysis-haskell)
and [the current declaration split](../../../compiler/GHC/Rename/Module.hs).

## First source check

```haskell
{-# LANGUAGE ExperimentalDependentTypes, StandaloneKindSignatures #-}
module Example where
import Data.Kind (Type)

type Result :: Type -> Type
type Result a = Example.keep a

keep :: a -> a
keep x = x
```

At `c42ab625f6`, GHC-68947 points at `keep` in the synonym, even if
`keep` is moved above `Result`. The open evaluator needs the checked
definition to return the original `a`. Knowing the signature of `keep`
allows an ordinary call to type-check but cannot determine that result.
[Open evaluator](../../../compiler/GHC/Tc/Gen/OpenTypeEval.hs).

- [ ] Record the dependency on the resolved term definition in its
      declaration group. Accept both source orders for an independent
      producer with a complete signature.
- [ ] Give the observer the producer's checked Core. A body that demands
      the rigid input must still fail at the source call.
- [ ] Diagnose a cycle when the producer's own signature uses `Result`.
      Keep declarations after a top-level splice out of scope before it.
- [ ] Check an ordinary client from the two producer orders using their
      interfaces. Recheck the producer when its selected result changes.

Evaluating a closed call through the interpreter needs executable code.
Ordering declarations gives the type checker a body, but it does not
create the current module's object file. The type-functions draft requires
a separately loadable module for that operation. R15 tracks this staging
question separately. [Type-functions draft](https://github.com/ghc-proposals/ghc-proposals/pull/509)
and [R15](R15-closed-term-evaluator.md).
