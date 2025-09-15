{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
module T25148a where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce

data Dict :: Constraint -> Type where
  Dict :: c => Dict c

fact0 :: forall n a. Proxy a -> Dict (0 <= n)
fact0 _ = unsafeCoerce (Dict :: Dict (0 <= 0))

class Facts a where
  fact1 :: forall n. Proxy a -> Dict (0 <= n)
  fact1 _ = unsafeCoerce (Dict :: Dict (0 <= 0))
instance Facts ()

class Facts' a where
  fact' :: forall n. Proxy n -> Proxy a -> Dict (0 <= n)
  fact' _ _ = unsafeCoerce (Dict :: Dict (0 <= 0))
instance Facts' ()
