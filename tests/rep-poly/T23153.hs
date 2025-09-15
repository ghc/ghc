module T23153 where

import GHC.Exts

f :: forall r s (a :: TYPE (r s)). a -> ()
f = f

g h = f (h ())
