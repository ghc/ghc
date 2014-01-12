
{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Prim
import GHC.Word
import Data.Bits

main :: IO ()
main = do f 5 6
          f maxBound 23
          f maxBound maxBound

f :: Word -> Word -> IO ()
f wx@(W# x) wy@(W# y)
    = do putStrLn "-----"
         putStrLn ("Doing " ++ show wx ++ " + " ++ show wy)
         case x `plusWord2#` y of
             (# h, l #) ->
                 do let wh = W# h
                        wl = W# l
                        r = shiftL (fromIntegral wh) (bitSize wh)
                          + fromIntegral wl
                    putStrLn ("High: " ++ show wh)
                    putStrLn ("Low: " ++ show wl)
                    putStrLn ("Result: " ++ show (r :: Integer))

