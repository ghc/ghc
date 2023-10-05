import Control.Monad
import Data.Word
import GHC.Float (word2Double, word2Float)
import Numeric (showHFloat)

t15926 :: IO ()
t15926 = do
  let sources = [ 2^63             -- exact
                , 2^63 - 1         -- round up
                , 2^63 - 2^9       -- round up
                , 2^63 - 2^9 - 1   -- round down
                , 2^63 - 2^10      -- exact
                , 2^64             -- exact
                , 2^64 - 1         -- round up
                , 2^64 - 2^10      -- round up
                , 2^64 - 2^10 - 1  -- round down
                , 2^64 - 2^11      -- exact
                ] :: [Integer]
  forM_ sources $ \i -> do
    putStrLn (showHFloat (fromInteger i :: Double) "")

t17231 :: IO ()
t17231 = do
  putStrLn (showHFloat (fromInteger $ 4141414141414141*4141414141414141 :: Double) "")

t17782 :: IO ()
t17782 = do
  let x = maxBound :: Word
  print (word2Double x == fromInteger (toInteger x))

toFloat :: IO ()
toFloat = do
  let sources = [ 0xFFFFFF8           -- round up
                , 0xFFFFFF7F          -- round down
                , 0xFFFFFF80          -- round up
                , 0xFFFFFF7FF         -- round down
                , 0xFFFFFF800         -- round up
                , 0xFFFFFE800000000   -- round down
                , 0xFFFFFF800000000   -- round up
                , 0xFFFFFF7FFFFFFFF   -- round down
                , 0xFFFFFF800000001   -- round up
                , 0xFFFFFE8000000000  -- round down
                , 0xFFFFFF8000000000  -- round up
                , 0xFFFFFF7FFFFFFFFF  -- round down
                , 0xFFFFFF8000000001  -- round up
                ] :: [Integer]
  forM_ sources $ \i -> do
    putStrLn (showHFloat (fromInteger i :: Float) "")
  let x = maxBound :: Word
  print (word2Float x == fromInteger (toInteger x))

main :: IO ()
main = do
  t15926
  t17231
  t17782
  toFloat
