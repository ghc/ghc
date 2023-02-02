import Control.Monad.ST.Strict
import Control.Monad.Fix
import Data.STRef

foo :: ST s Int
foo = do
  ref <- newSTRef True
  mfix $ \res -> do
    x <- readSTRef ref
    if x
      then do
        writeSTRef ref False
        return $! (res + 5)
      else return 10

main :: IO ()
main = print $ runST foo
