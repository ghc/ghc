module M where

import GHC.Exts (TYPE)

f :: forall r (a :: TYPE r). () -> a
f x = f x
