{-# LANGUAGE MagicHash #-}
module T13413 where

import GHC.Exts

fillBlock2 :: (Int# -> Int# -> IO ())
           -> Int# -> Int# -> IO ()

fillBlock2 write x0 y0
 = fillBlock y0 x0
 where
   {-# INLINE fillBlock #-}
   fillBlock y ix
         | 1# <- y >=# y0
         = return ()
         | otherwise
         = do   write ix x0
                fillBlock (y +# 1#) ix

