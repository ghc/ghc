{-# LANGUAGE GADTs, AllowAmbiguousTypes #-}
module T8392a where

-- Should complain even with AllowAmbiguousTypes
--
-- But (#12466) we now don't complain about
-- contradictory signatures
-- Instead we get a redundant pattern-match warning,
-- in the post-typechecking pattern-match checks
foo :: (Int ~ Bool) => a -> a
foo x = x
