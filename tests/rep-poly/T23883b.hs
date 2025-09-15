module T23883b where

import GHC.Exts

setField :: forall a_rep (a :: TYPE a_rep). a -> ()
setField x _ = ()
