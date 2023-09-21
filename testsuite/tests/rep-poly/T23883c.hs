module T23883c where

import GHC.Exts

setField :: forall r s (a :: TYPE (r s)). a -> ()
setField x _ = ()
