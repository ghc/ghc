module For where

{-
import IOExts
import Addr
--y = putChar

count :: IORef Int -> IO Int
count ref = do
  x <- readIORef ref
  writeIORef ref (x+1)
  return x

createCounter :: IO Addr
createCounter = do
  ref <- newIORef 0
  mkCounter (count ref)

foreign import "sin" msin :: Double -> IO Double
-}

foreign export "putChar" putChar :: Char -> IO ()

--foreign export "createCounter" createCounter :: IO Addr
--foreign export dynamic mkCounter :: (IO Int) -> IO Addr
