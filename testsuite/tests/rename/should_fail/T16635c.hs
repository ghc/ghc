module T16635c where

import Data.Proxy (Proxy(..))
import GHC.Types (Type)

f :: forall a. ()
f = let g :: (Proxy a :: forall a . Type)
        g = Proxy
    in ()
