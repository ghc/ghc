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
