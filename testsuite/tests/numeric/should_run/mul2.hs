
{-# LANGUAGE MagicHash, UnboxedTuples #-}
import GHC.Exts
import GHC.Int
import GHC.Word
import Data.Bits

main :: IO ()
main = do f 5 6
          f 0xFD94E3B7FE36FB18 49
          f 0xFD94E3B7FE36FB18 0xFC1D8A3BFB29FC6A
          g 5 6
          g 0xFD94E3B7FE36FB18 49
          g 0xFD94E3B7FE36FB18 0xFC1D8A3BFB29FC6A

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
{-# INLINE f #-}

g :: Int -> Int -> IO ()
g ix@(I# x) iy@(I# y)
    = do putStrLn "-----"
         putStrLn ("Doing " ++ show ix ++ " * " ++ show iy)
         case x `timesInt2#` y of
             (# needsHigh, h, l #) ->
                 do let ih = I# h
                        il = I# l
                        r = shiftL (fromIntegral ih) (bitSize ih)
                          .|. (fromIntegral @Word @Integer $ fromIntegral @Int @Word il)
                    putStrLn ("Needs high: " ++ show (isTrue# needsHigh))
                    putStrLn ("High: " ++ show ih)
                    putStrLn ("Low: " ++ show il)
                    putStrLn ("Result: " ++ show (r :: Integer))
{-# INLINE g #-}
