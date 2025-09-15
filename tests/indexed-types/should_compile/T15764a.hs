{-# Language PolyKinds #-}
{-# Language TypeFamilies #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language MultiParamTypeClasses #-}

module T15764a where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

class C6 (k :: Type) (a :: k) (b :: Proxy (a :: k)) where
 type T6 (proxy :: Proxy '(k, (b :: Proxy a)))
