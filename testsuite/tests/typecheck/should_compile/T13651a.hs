{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
module T13651 where

type family F r s = f | f -> r s

type instance F (Bar h (Foo r)) (Bar h (Foo s)) = Bar h (Bar r s)

data Bar s b
data Foo a

foo :: (F cr cu ~ Bar h (Bar r u),
        F cu cs ~ Bar (Foo h) (Bar u s))
    => Bar h (Bar r u) -> Bar (Foo h) (Bar u s) -> cu -> Foo (cr -> cs)
    -- A variant of T13651 which fixes 'cu'
    -- as well as the other type args
foo = undefined

{-
NB: In the constraints on the type of foo, we have:

  F cr cu ~ Bar h (Bar r u)                      (1)
    by assumption

  F cu cs ~ Bar (Foo h) (Bar u s)                (2)
    by assumption

  cu ~ Bar h (Foo u)                             (3)
    by injectivity in the second argument of F
    applied to (1)

  F (Bar h (Foo u)) cs ~ Bar (Foo h) (Bar u s)   (4)
    substituting (3) into (2)

  h ~ Foo h                                      (5)
    by injectivity in the first argument of F
    applied to (4)

As (5) fails the occurs check, the constraint on foo is unsatisfiable.

It will be hard to use foo except in counterfactual circumstances
(necessarily dead code), but rather than preventing its definition
altogether, a warning should suffice.
-}
