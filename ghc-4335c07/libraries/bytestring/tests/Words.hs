{-# LANGUAGE BangPatterns #-}

import System.Environment
import Control.Applicative
import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S

import Data.ByteString.Internal
import Data.ByteString.Unsafe
import Foreign

main = do
    f <- head <$> getArgs
    x <- S.readFile f
    let t = foldl' count M.empty (S.words x) :: M.Map S.ByteString Int
    print . take 20 . sortBy (flip (comparing snd)) . M.toList $ t

count counts word = M.insertWith' (+) (word) 1 counts







------------------------------------------------------------------------
-- a different Ord

newtype OrdString = OrdString S.ByteString
    deriving (Eq, Show)

instance Ord OrdString where
    compare (OrdString p) (OrdString q) = compareBytes p q

compareBytes :: ByteString -> ByteString -> Ordering
compareBytes (PS fp1 off1 len1) (PS fp2 off2 len2)
    | len1 == 0  && len2 == 0                     = EQ  -- short cut for empty strings
    | fp1 == fp2 && off1 == off2 && len1 == len2  = EQ  -- short cut for the same string
--  | max len1 len2 > 1                           = inlinePerformIO $
    | otherwise                                   = inlinePerformIO $
    withForeignPtr fp1 $ \p1 ->
    withForeignPtr fp2 $ \p2 -> do
        i <- memcmp (p1 `plusPtr` off1) (p2 `plusPtr` off2) (fromIntegral $ min len1 len2)
        return $! case i `compare` 0 of
                    EQ  -> len1 `compare` len2
                    x   -> x


{-
    | otherwise                                   = inlinePerformIO $
    withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            cmp (p1 `plusPtr` off1)
                (p2 `plusPtr` off2) 0 len1 len2
-}

-- XXX todo.
cmp :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int-> IO Ordering
cmp p1 p2 n len1 len2
      | n == len1 = if n == len2 then return EQ else return LT
      | n == len2 = return GT
      | otherwise = do
          a <- peekByteOff p1 n :: IO Word8
          b <- peekByteOff p2 n
          case a `compare` b of
                EQ -> cmp p1 p2 (n+1) len1 len2
                LT -> return LT
                GT -> return GT
