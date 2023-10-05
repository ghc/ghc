-- Check that we can unpack a strict Maybe Int field.
import System.Exit

data T = MkT {-# UNPACK #-} !(Maybe Int)

xs = Nothing : [Just n | n <- [1..10]]

ts = map MkT xs

main = if xs == map (\(MkT m) -> m) ts
  then return ()
  else do
    putStrLn "Error in packing and unpacking!"
    exitFailure
