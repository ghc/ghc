import Foreign
import Control.Monad

-- check that all pointers returned by allocaBytes and mallocBytes are
-- 16-byte aligned
main = do
    sequence [ allocaBytes x $ return | x <- [1..500] ] >>= check 16
    (replicateM 500 (alloca $ return) :: IO [Ptr Align32])  >>= check 32
    (replicateM 500 (alloca $ return) :: IO [Ptr Align64])  >>= check 64
    (replicateM 500 (alloca $ return) :: IO [Ptr Align128]) >>= check 128
    (replicateM 500 (alloca $ return) :: IO [Ptr Align256]) >>= check 256
    -- mapM mallocBytes [1..500] >>= check 16

check :: Int -> [Ptr a] -> IO ()
check align xs = do
    let bad = [ p | p <- xs,  (p `minusPtr` nullPtr) .&. (align-1) /= 0 ]
    when (not $ null bad) $
      putStrLn ("FAIL: " ++ show align ++ " " ++ show bad)

data Align32 = Align32

instance Storable Align32 where
  sizeOf    _ = 32
  alignment _ = 32

data Align64 = Align64

instance Storable Align64 where
  sizeOf    _ = 64
  alignment _ = 64

data Align128 = Align128

instance Storable Align128 where
  sizeOf    _ = 128
  alignment _ = 128

data Align256 = Align256

instance Storable Align256 where
  sizeOf    _ = 256
  alignment _ = 256
