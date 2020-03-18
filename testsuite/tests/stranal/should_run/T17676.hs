{-# LANGUAGE ScopedTypeVariables #-}
import Data.IORef
import Control.Exception
import Control.Monad

data Exc = Exc deriving Show

instance Exception Exc

-- Recursive instead of NOINLINE because of #17673
f :: Int -> Int -> IO ()
f 0 x = do
  let true = sum [0..4] == 10
  when true $ throwIO Exc
  x `seq` return ()
f n x = f (n-1) (x+1)

main = f 1 (error "expensive computation") `catch` \(_ :: Exc) -> return ()
