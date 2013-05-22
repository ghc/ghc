{-# LANGUAGE PolyKinds, ExplicitForAll #-}
module T7916 where


f :: forall (m :: k -> *) (a :: k). m a -> m a
f = id

-- g :: forall (m :: k -> *) (a :: k). m a -> m a
g x = f x
