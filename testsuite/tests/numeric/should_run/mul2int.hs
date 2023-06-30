{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Exts
import Data.Bits

main :: IO ()
main = do g 5 6
          g (-5) 6
          g 0x7ECA71DBFF1B7D8C 49
          g (-0x7ECA71DBFF1B7D8C) 49
          g 0x7ECA71DBFF1B7D8C 0x7E0EC51DFD94FE35
          g 0x7ECA71DBFF1B7D8C (-0x7E0EC51DFD94FE35)


g :: Int -> Int -> IO ()
g wx@(I# x) wy@(I# y)
    = do putStrLn "-----"
         putStrLn ("Doing " ++ show wx ++ " * " ++ show wy)
         case x `timesInt2#` y of
             (# n, h, l #) ->
                 do let wh = I# h
                        wl = I# l
                        wlw = W# (int2Word# l)
                        wn = I# n
                        r | wn == 1   = shiftL (fromIntegral wh) (finiteBitSize wh)
                                      + fromIntegral wlw
                          | otherwise = fromIntegral wl

                    putStrLn ("High:      " ++ show wh)
                    putStrLn ("Low:       " ++ show wl)
                    putStrLn ("Needed:    " ++ show wn)
                    putStrLn ("Result:    " ++ show (r :: Integer))
                    putStrLn ("Should be: " ++ show (fromIntegral wx * fromIntegral wy :: Integer))


