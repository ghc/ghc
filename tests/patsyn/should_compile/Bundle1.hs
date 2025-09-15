module Associated1(A(..)) where

import BundleInternal1 (A(..))

foo = MkA 5
baz = NoA

qux (MkA x) = x
qux NoA = 0
