module T23153b where

import GHC.Exts

f :: forall r s (a :: TYPE (r s)). a -> a
f = f

g h = case f (h ()) of () -> ()
