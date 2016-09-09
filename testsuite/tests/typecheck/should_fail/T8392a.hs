{-# LANGUAGE GADTs, AllowAmbiguousTypes #-}
module T8392a where

-- Should complain even with AllowAmbiguousTypes
--
-- But (Trac #12466) we now don't complain about
-- contradictory signatures
foo :: (Int ~ Bool) => a -> a
foo x = x
