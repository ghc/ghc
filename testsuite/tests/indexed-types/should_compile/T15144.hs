{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module T15144 where

import Data.Coerce
import Data.Proxy

type family F x

f :: Coercible (F a) b => Proxy a -> F a -> b
f _ = coerce

-- In #15144, we inferred the less-general type
-- g :: Proxy a -> F a -> F a
g p x = f p x

h :: Coercible (F a) b => Proxy a -> F a -> b
h p x = g p x
