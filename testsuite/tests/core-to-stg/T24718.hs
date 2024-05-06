module T24718 where

import GHC.Exts ( Any )
import Unsafe.Coerce ( unsafeCoerce )

data T = MkT (Any -> Any)

g :: () -> ()
g x = x

f :: T
f = unsafeCoerce MkT g
