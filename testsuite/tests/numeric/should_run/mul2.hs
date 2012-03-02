
{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Prim
import GHC.Word
import Data.Bits

main :: IO ()
main = do f 5 6
          f 0xFD94E3B7FE36FB18 49
          f 0xFD94E3B7FE36FB18 0xFC1D8A3BFB29FC6A

f :: Word -> Word -> IO ()
f wx@(W# x) wy@(W# y)
    = do putStrLn "-----"
         putStrLn ("Doing " ++ show wx ++ " * " ++ show wy)
         case x `timesWord2#` y of
             (# h, l #) ->
                 do let wh = W# h
                        wl = W# l
                        r = shiftL (fromIntegral wh) (bitSize wh)
                          + fromIntegral wl
                    putStrLn ("High: " ++ show wh)
                    putStrLn ("Low: " ++ show wl)
                    putStrLn ("Result: " ++ show (r :: Integer))

