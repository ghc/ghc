{-# OPTIONS_GHC -ddump-simpl #-}
module T3772 where

import T3772_A

foo :: Int -> ()
{-# NOINLINE foo #-}
foo n = apply n (id :: [Double] -> [Double])

-- = apply [Double] [Double] d1 d2 (id :: [Double] -> [Double]) =
-- deepSeq d2 (id (gen d1 n)) () = deepSeq d2 (gen d1 n) () = d2 |> co
-- (gen d1 n) () =


-- where d2 :: DeepSeq [Double]
--       d2 = df d3
-- 
--       d3 :: DeepSeq Double

--       d1 :: C [Double]
--       d1 = df' d3
