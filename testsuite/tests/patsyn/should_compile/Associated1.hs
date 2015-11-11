module Associated1(A(..)) where

import AssociatedInternal1 (A(..))

foo = MkA 5
baz = NoA

qux (MkA x) = x
qux NoA = 0
