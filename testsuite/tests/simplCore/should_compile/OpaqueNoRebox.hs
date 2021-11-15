module OpaqueNoRebox where

f :: (Int,Int) -> Int
f (x,y) = x + y
{-# OPAQUE f #-}

-- No W/W happens for f because it is OPAQUE, and by design its Boxity
-- information is stripped, which is good!
--
-- If we hadn't stripped the boxity information, we would make a worker
-- for g that would just rebox its arguments:
--
--   $wg :: Int# -> Int# -> Int
--   $wg ww ww1 =
--     let x = I# ww in
--     let y = I# ww1 in
--     let p = (x,y) in
--     case f (f p, f p) of { I# z -> ww +# z}
--
-- as $wg was expecting that a worker for f that would be inlined.
--
-- See Note [The OPAQUE pragma and avoiding the reboxing of arguments]
g :: (Int, Int) -> Int
g p = fst p + f (f p, f p)
