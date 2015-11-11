module Associated(A(..)) where

import AssociatedInternal (A(..))

foo = MkA 5
baz = NoA

qux (MkA x) = x
qux NoA = 0
