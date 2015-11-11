module TransAssociated(A(..)) where

import Associated (A(..))

foo = MkA 5
baz = NoA

qux (MkA x) = x
qux NoA = 0
