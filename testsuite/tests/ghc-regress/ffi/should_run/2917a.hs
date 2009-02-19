import Foreign
import Control.Monad

-- check that all pointers returned by allocaBytes and mallocBytes are
-- 16-byte aligned
main = do
    xs <- sequence [ allocaBytes x $ return | x <- [1..500] ]
    ys <- mapM mallocBytes [1..500]
    when (not $ all ((==0) . (.&. 15) . (`minusPtr` nullPtr)) (xs ++ ys)) $ do
       print xs
       print ys
