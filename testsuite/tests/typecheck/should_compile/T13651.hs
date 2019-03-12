{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
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
    (see #14745 for why), but doesn't in 8.4.

[G]  F cr cu ~ Bar h (Bar r u),
     F cu cs ~ Bar (Foo h) (Bar u s))


[W] F cr cu0 ~ Bar h (Bar r u)
    --> (top-level fundeps)  cr ~ Bar h (Foo r)
                             cu0 ~ Bar h (Foo u)
        (local fundeps)      cu ~ cu0

[W] F cu0 cs ~ Bar (Foo h) (Bar u s)
    -->  (top-level fundeps)  cu0 ~ Bar (Foo h) (Foo u)
                              cs  ~ Bar (Foo h) (Foo s)
         (local fundeps)      cu0 ~ cu

[W] F cr (Bar (Foo h) (Fo u)) ~ Bar h (Bar r u)

-}
