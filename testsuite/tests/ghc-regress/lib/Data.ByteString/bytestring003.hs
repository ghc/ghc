{-# OPTIONS -cpp #-}

--
-- 'sums' benchmark from the great language shootout 
--

import System.IO
import qualified Data.ByteString as B
import Data.ByteString.Base (ByteString,unsafeTail,unsafeIndex)
import Data.Char    -- seems to help!

#define STRICT2(f) f a b | a `seq` b `seq` False = undefined

main = print . go 0 =<< B.getContents

STRICT2(go)
go i ps
    | B.null ps = i
    | x == 45   = neg 0 xs
    | otherwise = pos (parse x) xs
    where
        (x, xs) = (ps `unsafeIndex` 0, unsafeTail ps)

        STRICT2(neg)
        neg n qs | x == 10     = go (i-n) xs
                 | otherwise   = neg (parse x + (10 * n)) xs
                 where (x, xs) = (qs `unsafeIndex` 0, unsafeTail qs)

        STRICT2(pos)
        pos n qs | x == 10   = go (i+n) xs
                 | otherwise = pos (parse x + (10 * n)) xs
                 where (x, xs) = (qs `unsafeIndex` 0, unsafeTail qs)

parse w = fromIntegral (w - 48) :: Int
{-# INLINE parse #-}
