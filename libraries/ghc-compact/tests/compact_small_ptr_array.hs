import GHC.Compact
import Data.Primitive.SmallArray

main :: IO ()
main = do
    arr <- newSmallArray 5 (Just 'a') >>= unsafeFreezeSmallArray
    arr' <- compact arr
    print $ getCompact arr'
