{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Bug where

import Data.Kind
import Data.Proxy

type Const a b = a

foo :: forall a b (c :: Const Type b). Proxy '[a, c]
foo = error "ruk"
