module T23883a where

import GHC.Exts

setField :: forall a_rep (a :: TYPE a_rep). a -> Int
setField x = undefined (\ _ -> x)
