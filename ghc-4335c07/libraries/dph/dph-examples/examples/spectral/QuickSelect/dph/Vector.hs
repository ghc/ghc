{-# LANGUAGE BangPatterns #-}

module Vector (quickselect) where
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U

{-# NOINLINE quickselect #-}
quickselect
        :: U.Vector Double
        -> Int
        -> Double

quickselect xs k
 = let p                = xs U.! (U.length xs `div` 2)
       ls               = U.filter (<p) xs
   in  if       k < U.length ls
       then     quickselect ls k
       else
        let gs  = U.filter (>p) xs
            len = U.length xs - U.length gs
        in  if    k >= len
            then  quickselect gs (k - len)
            else  p

