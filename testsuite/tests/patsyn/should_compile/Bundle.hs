module Bundle(A(..)) where

import BundleInternal (A(..))

foo = MkA 5
baz = NoA

qux (MkA x) = x
qux NoA = 0
