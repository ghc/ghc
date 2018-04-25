import qualified Data.ByteString.Lazy as P
import qualified Data.ByteString      as B
import qualified Data.List            as L
import Data.Word

main = do
    let s = [65..68] ++ [103,103]
    f s
    print (length s)

f s = do
    print "Should have the same structure:"
    print $ L.groupBy (/=) $        s

    flip mapM_ [1..16] $
        \i -> do putStr ((show i) ++ "\t"); print $ P.groupBy (/=) $ pack i s
{-# NOINLINE f #-}

chunk :: Int -> [a] -> [[a]]
chunk _    [] = []
chunk size xs = case L.splitAt size xs of (xs', xs'') -> xs' : chunk size xs''

pack :: Int -> [Word8] -> P.ByteString
pack n str = P.LPS $ L.map B.pack (chunk n str)
