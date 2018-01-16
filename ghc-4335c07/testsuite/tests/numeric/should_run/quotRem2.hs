
{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Prim
import GHC.Word
import Control.Monad
import Data.Bits

main :: IO ()
main = do f 5 6 23
          f 0x80000000 0 0x80000001
          f 0xFC1D8A3BFB29FC6A 49 0xFD94E3B7FE36FB18

f :: Word -> Word -> Word -> IO ()
f wxHigh@(W# xHigh) wxLow@(W# xLow) wy@(W# y)
    = do when debugging $ putStrLn "-----"
         when debugging $ putStrLn ("Doing " ++ show (wxHigh, wxLow)
                                             ++ " `quotRem` " ++ show wy)
         let ix = (toInteger wxHigh `shiftL` bitSize wxHigh)
              .|. toInteger wxLow
             wanted = ix `quotRem` toInteger wy
         when debugging $ putStrLn ("Wanted: " ++ show wanted)
         case quotRemWord2# xHigh xLow y of
             (# q, r #) ->
                 do let wq = W# q
                        wr = W# r
                        got = (toInteger wq, toInteger wr)
                    when debugging $ putStrLn ("Got: " ++ show got)
                    if wanted == got then putStrLn "Worked"
                                     else putStrLn "Failed"

debugging :: Bool
debugging = False

