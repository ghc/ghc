import Control.Exception
import Data.IORef

strictPrint :: Show a => a -> IO ()
{-# OPAQUE strictPrint #-}
strictPrint x = print $! x

f :: Show a => a -> IORef a -> IO ()
{-# OPAQUE f #-}
f x r = do
  x' <- evaluate $! x
  writeIORef r x'
  strictPrint x'

main :: IO ()
main = do
  r <- newIORef (42 :: Int)
  f (error "foo") r `catch` \(e :: SomeException) -> return ()
  n <- readIORef r
  print n
