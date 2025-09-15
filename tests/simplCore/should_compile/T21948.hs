module T21948 where

import GHC.Int( Int64 )

nf' :: (b -> ()) -> (a -> b) -> a -> (Int64 -> IO ())
nf' reduce f x = go
  where
    go n | n <= 0    = return ()
         | otherwise = let !y = f x
                       in reduce y `seq` go (n-1)
