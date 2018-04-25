module TransAssociated(A(..)) where

import Bundle (A(..))

foo = MkA 5
baz = NoA

qux (MkA x) = x
qux NoA = 0
