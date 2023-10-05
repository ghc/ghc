module OpaqueNoRebox2 where

{-# OPAQUE f #-}
f :: Int -> Int -> (Int, Int)
f x y = (x,y)

-- No W/W happens for f because it is OPAQUE, and by design its CPR
-- information is stripped, which is good!
--
-- If we hadn't stripped the CPR information, we would make a worker/wrapper
-- for g that would rebox the result of 'g':
--
--   $wg :: Bool -> Int -> (# Int, Int #)
--   $wg True  a = case f 2 a of (x, y) -> (# x, y #)
--   $wg False a = $wg True (a + 1)
--
--   g ds a = case $wg ds a of (# x, y#) -> (x, y)
--
-- as $wg was expecting that a worker for f that would be inlined.
--
-- See Note [The OPAQUE pragma and avoiding the reboxing of results]
g True  a = f 2 a
g False a = g True (a+1)
