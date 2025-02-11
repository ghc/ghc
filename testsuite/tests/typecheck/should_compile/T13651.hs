{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, TypeOperators #-}
module T13651 where

type family F r s = f | f -> r s

type instance F (Bar h (Foo r)) (Bar h (Foo s)) = Bar h (Bar r s)

data Bar s b
data Foo a

foo :: (F cr cu ~ Bar h (Bar r u),
        F cu cs ~ Bar (Foo h) (Bar u s))
    => Bar h (Bar r u) -> Bar (Foo h) (Bar u s) -> Foo (cr -> cs)
foo = undefined

{-  Typechecking this program used to /just/ succeed in GHC 8.2,
    (see #14745 for why), but fails in the ambiguity check for `foo` in 8.4.

The ambiguity check gives:

[G] F cr cu ~ Bar h (Bar r u),
[G] F cu cs ~ Bar (Foo h) (Bar u s))
[W] d1 : F cr cu0 ~ Bar h (Bar r u)
[W] d2 : F cu0 cs ~ Bar (Foo h) (Bar u s)

Interacting with the local Given first gives cu~cu0, which
solves it easily.  Do that before interacting with the top
level instance.
-}
