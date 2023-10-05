
module T3449 (Foo, f1) where

import T3449A

class Foo a where
    f1 :: a
    f2 :: a

instance Foo Char where
    f1 = f2
    f2 = aChar

