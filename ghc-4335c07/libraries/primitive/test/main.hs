{-# LANGUAGE MagicHash, UnboxedTuples #-}

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive
import Data.Primitive.Array
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Word
import GHC.Int
import GHC.IO
import GHC.Prim

-- Since we only have two test cases right now, I'm going to avoid the
-- issue of choosing a test framework for the moment. This also keeps the
-- package as a whole light on dependencies.

main :: IO ()
main = do
    testArray
    testByteArray

testArray :: IO ()
testArray = do
    arr <- newArray 1 'A'
    let unit =
            case writeArray arr 0 'B' of
                IO f ->
                    case f realWorld# of
                        (# _, _ #) -> ()
    c1 <- readArray arr 0
    return $! unit
    c2 <- readArray arr 0
    if c1 == 'A' && c2 == 'B'
        then return ()
        else error $ "Expected AB, got: " ++ show (c1, c2)

testByteArray :: IO ()
testByteArray = do
    let arr1 = mkByteArray ([0xde, 0xad, 0xbe, 0xef] :: [Word8])
        arr2 = mkByteArray ([0xde, 0xad, 0xbe, 0xef] :: [Word8])
        arr3 = mkByteArray ([0xde, 0xad, 0xbe, 0xee] :: [Word8])
    when (show arr1 /= "[0xde, 0xad, 0xbe, 0xef]") $
        fail $ "ByteArray Show incorrect: "++show arr1
    unless (arr1 > arr3) $
        fail $ "ByteArray Ord incorrect"
    unless (arr1 == arr2) $
        fail $ "ByteArray Eq incorrect"

mkByteArray :: Prim a => [a] -> ByteArray
mkByteArray xs = runST $ do
    marr <- newByteArray (length xs * sizeOf (head xs))
    sequence $ zipWith (writeByteArray marr) [0..] xs
    unsafeFreezeByteArray marr
