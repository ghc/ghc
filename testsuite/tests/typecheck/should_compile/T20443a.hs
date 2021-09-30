module T20443a where

import Data.Kind

data Proxy t where
  Proxy :: forall {k} (t :: k) . Proxy t

a :: () -> Proxy Int
-- a = Proxy @Type @Int -- This would, rightfully, not compile
a () = Proxy @Int

b :: Proxy Int -> ()
b (Proxy @Int) = ()       -- This should compile, but doesn't
