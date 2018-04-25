{-# LANGUAGE BangPatterns #-}

import qualified Data.HashTable as T
import System.Environment
import Control.Applicative
import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S

import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe   as S
import Foreign


type Table = T.HashTable S.ByteString Int

newTable :: IO Table
newTable = T.new (==) hash

update :: Table -> S.ByteString -> IO ()
update t w = do
    old <- T.lookup t w
    case old of
        Just c -> do T.update t w $! c + 1; return ()
        _      -> T.insert t w 1

main = do
    [name] <- getArgs
    t <- newTable
    S.words <$> S.readFile name >>= mapM_ (update t)
    xs <- sortBy (flip (comparing snd)) <$> T.toList t
    print (take 20 xs)

------------------------------------------------------------------------
-- a different Ord

hash :: S.ByteString -> Int32
hash ps = go 0 golden
  where
    len = S.length ps

    go :: Int -> Int32 -> Int32
    go !n !acc
             | n == len  = fromIntegral acc
             | otherwise = go (n+1)
                              ((fromIntegral (ps `S.unsafeIndex` n))
                                   * 0xdeadbeef + hashInt32 acc)

golden :: Int32
golden = 1013904242 -- = round ((sqrt 5 - 1) * 2^32) :: Int32

hashInt32 :: Int32 -> Int32
hashInt32 x = mulHi x golden + x

-- hi 32 bits of a x-bit * 32 bit -> 64-bit multiply
mulHi :: Int32 -> Int32 -> Int32
mulHi a b = fromIntegral (r `shiftR` 32)
   where r :: Int64
         r = fromIntegral a * fromIntegral b

------------------------------------------------------------------------

newtype OrdString = OrdString S.ByteString
     deriving Show

eq a@(S.PS p s l) b@(S.PS p' s' l')
         | l /= l'            = False    -- short cut on length
         | p == p' && s == s' = True     -- short cut for the same string
         | otherwise          = compare a b == EQ
  where
    compare (S.PS fp1 off1 len1) (S.PS fp2 off2 len2) = S.inlinePerformIO $
        withForeignPtr fp1 $ \p1 ->
            withForeignPtr fp2 $ \p2 ->
                cmp (p1 `plusPtr` off1)
                    (p2 `plusPtr` off2) 0 len1 len2

cmp :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int-> IO Ordering
cmp !p1 !p2 !n len1 len2
      | n == len1 = if n == len2 then return EQ else return LT
      | n == len2 = return GT
      | otherwise = do
          a <- peekByteOff p1 n :: IO Word8
          b <- peekByteOff p2 n
          case a `compare` b of
                EQ -> cmp p1 p2 (n+1) len1 len2
                LT -> return LT
                GT -> return GT
