{-# LANGUAGE PolyKinds, ExplicitForAll #-}
module T7916 where


f :: forall (m :: k -> *) (a :: k). m a -> m a
f = id

-- g :: forall (m :: k -> *) (a :: k). m a -> m a
g x = f x

data M f = M (f Int)

-- Test that g :: forall (m :: k -> *) (a :: k). m a -> m a
g1 = g :: [Int] -> [Int]
g2 = g :: M [] -> M []
