module T18328 where

f :: Int -> [a] -> [a] -> [a]
f x ys = let {-# NOINLINE j #-}
             j y = case x of
                     3  -> ((++) ys) . ((++) ys) . ((++) ys) . ((++) ys)
                     _  -> ((++) ys) . ((++) ys) . ((++) ys)

         in
         case x of
           1 -> j 2
           2 -> j 3
           3 -> j 4
           _ -> ((++) ys)
