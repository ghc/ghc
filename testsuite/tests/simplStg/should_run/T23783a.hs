module T23783a where
import Debug.Trace
data T a = MkT (T a) (T a) !a !Int
wombat t x = MkT t t x 2

seqT :: Int -> T a -> ()
seqT 0 _ = ()
seqT n (MkT x y _ _) = seqT (n - 1) x `seq` seqT (n - 1) y `seq` ()
