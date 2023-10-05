module KindVType where

-- test printing of "type" in output, not "kind"

import Data.Proxy

foo :: Proxy Maybe
foo = (Proxy :: Proxy Int)
