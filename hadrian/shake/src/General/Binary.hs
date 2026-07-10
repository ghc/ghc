{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, Rank2Types #-}

module General.Binary(
    BinaryOp(..), binaryOpMap,
    binarySplit, binarySplit2, binarySplit3, unsafeBinarySplit,
    Builder(..), runBuilder, sizeBuilder,
    BinaryEx(..),
    Storable, putExStorable, getExStorable, putExStorableList, getExStorableList,
    putExList, getExList, putExN, getExN
    ) where

import Development.Shake.Classes
import Control.Monad
import Data.Binary
import Data.List.Extra
import Data.Tuple.Extra
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe as U
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Semigroup
import Prelude


---------------------------------------------------------------------
-- STORE TYPE

-- | An explicit and more efficient version of Binary
data BinaryOp v = BinaryOp
    {putOp :: v -> Builder
    ,getOp :: BS.ByteString -> v
    }

binaryOpMap :: BinaryEx a => (a -> BinaryOp b) -> BinaryOp (a, b)
binaryOpMap mp = BinaryOp
    {putOp = \(a, b) -> putExN (putEx a) <> putOp (mp a) b
    ,getOp = \bs -> let (bs1,bs2) = getExN bs; a = getEx bs1 in (a, getOp (mp a) bs2)
    }


binarySplit :: forall a . Storable a => BS.ByteString -> (a, BS.ByteString)
binarySplit bs | BS.length bs < sizeOf (undefined :: a) = error "Reading from ByteString, insufficient left"
               | otherwise = unsafeBinarySplit bs

binarySplit2 :: forall a b . (Storable a, Storable b) => BS.ByteString -> (a, b, BS.ByteString)
binarySplit2 bs | BS.length bs < sizeOf (undefined :: a) + sizeOf (undefined :: b) = error "Reading from ByteString, insufficient left"
                | (a,bs) <- unsafeBinarySplit bs, (b,bs) <- unsafeBinarySplit bs = (a,b,bs)

binarySplit3 :: forall a b c . (Storable a, Storable b, Storable c) => BS.ByteString -> (a, b, c, BS.ByteString)
binarySplit3 bs | BS.length bs < sizeOf (undefined :: a) + sizeOf (undefined :: b) + sizeOf (undefined :: c) = error "Reading from ByteString, insufficient left"
                | (a,bs) <- unsafeBinarySplit bs, (b,bs) <- unsafeBinarySplit bs, (c,bs) <- unsafeBinarySplit bs = (a,b,c,bs)


unsafeBinarySplit :: Storable a => BS.ByteString -> (a, BS.ByteString)
unsafeBinarySplit bs = (v, BS.unsafeDrop (sizeOf v) bs)
    where v = unsafePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> peek (castPtr ptr)


-- forM for zipWith
for2M_ as bs f = zipWithM_ f as bs

---------------------------------------------------------------------
-- BINARY SERIALISATION

-- We can't use the Data.ByteString builder as that doesn't track the size of the chunk.
data Builder = Builder {-# UNPACK #-} !Int (forall a . Ptr a -> Int -> IO ())

sizeBuilder :: Builder -> Int
sizeBuilder (Builder i _) = i

runBuilder :: Builder -> BS.ByteString
runBuilder (Builder i f) = unsafePerformIO $ BS.create i $ \ptr -> f ptr 0

instance Semigroup Builder where
    (Builder x1 x2) <> (Builder y1 y2) = Builder (x1+y1) $ \p i -> do x2 p i; y2 p $ i+x1

instance Monoid Builder where
    mempty = Builder 0 $ \_ _ -> pure ()
    mappend = (<>)

-- | Methods for Binary serialisation that go directly between strict ByteString values.
--   When the Database is read each key/value will be loaded as a separate ByteString,
--   and for certain types (e.g. file rules) this may remain the preferred format for storing keys.
--   Optimised for performance.
class BinaryEx a where
    putEx :: a -> Builder
    getEx :: BS.ByteString -> a

instance BinaryEx BS.ByteString where
    putEx x = Builder n $ \ptr i -> BS.unsafeUseAsCString x $ \bs -> copyBytes (ptr `plusPtr` i) (castPtr bs) (fromIntegral n)
        where n = BS.length x
    getEx = id

instance BinaryEx LBS.ByteString where
    putEx x = Builder (fromIntegral $ LBS.length x) $ \ptr i -> do
        let go _ [] = pure ()
            go i (x:xs) = do
                let n = BS.length x
                BS.unsafeUseAsCString x $ \bs -> copyBytes (ptr `plusPtr` i) (castPtr bs) (fromIntegral n)
                go (i+n) xs
        go i $ LBS.toChunks x
    getEx = LBS.fromChunks . pure

instance BinaryEx [BS.ByteString] where
    -- Format:
    -- n :: Word32 - number of strings
    -- ns :: [Word32]{n} - length of each string
    -- contents of each string concatenated (sum ns bytes)
    putEx xs = Builder (4 + (n * 4) + sum ns) $ \p i -> do
        pokeByteOff p i (fromIntegral n :: Word32)
        for2M_ [4+i,8+i..] ns $ \i x -> pokeByteOff p i (fromIntegral x :: Word32)
        p<- pure $ p `plusPtr` (i + 4 + (n * 4))
        for2M_ (scanl (+) 0 ns) xs $ \i x -> BS.unsafeUseAsCStringLen x $ \(bs, n) ->
            copyBytes (p `plusPtr` i) (castPtr bs) (fromIntegral n)
        where ns = map BS.length xs
              n = length ns

    getEx bs = unsafePerformIO $ BS.unsafeUseAsCString bs $ \p -> do
        n <- fromIntegral <$> (peekByteOff p 0 :: IO Word32)
        ns :: [Word32] <- forM [1..fromIntegral n] $ \i -> peekByteOff p (i * 4)
        pure $ snd $ mapAccumL (\bs i -> swap $ BS.splitAt (fromIntegral i) bs) (BS.drop (4 + (n * 4)) bs) ns

instance BinaryEx () where
    putEx () = mempty
    getEx _ = ()

instance BinaryEx String where
    putEx = putEx . UTF8.fromString
    getEx = UTF8.toString

instance BinaryEx (Maybe String) where
    putEx Nothing = mempty
    putEx (Just xs) = putEx $ UTF8.fromString $ '\0' : xs
    getEx = fmap snd . uncons . UTF8.toString

instance BinaryEx [String] where
    putEx = putEx . map UTF8.fromString
    getEx = map UTF8.toString . getEx

instance BinaryEx (String, [String]) where
    putEx (a,bs) = putEx $ a:bs
    getEx x = let a:bs = getEx x in (a,bs)

instance BinaryEx Bool where
    putEx False = Builder 1 $ \ptr i -> pokeByteOff ptr i (0 :: Word8)
    putEx True = mempty
    getEx = BS.null

instance BinaryEx Word8 where
    putEx = putExStorable
    getEx = getExStorable

instance BinaryEx Word16 where
    putEx = putExStorable
    getEx = getExStorable

instance BinaryEx Word32 where
    putEx = putExStorable
    getEx = getExStorable

instance BinaryEx Int where
    putEx = putExStorable
    getEx = getExStorable

instance BinaryEx Float where
    putEx = putExStorable
    getEx = getExStorable


putExStorable :: forall a . Storable a => a -> Builder
putExStorable x = Builder (sizeOf x) $ \p i -> pokeByteOff p i x

getExStorable :: forall a . Storable a => BS.ByteString -> a
getExStorable = \bs -> unsafePerformIO $ BS.unsafeUseAsCStringLen bs $ \(p, size) ->
        if size /= n then error "size mismatch" else peek (castPtr p)
    where n = sizeOf (undefined :: a)


putExStorableList :: forall a . Storable a => [a] -> Builder
putExStorableList xs = Builder (n * length xs) $ \ptr i ->
    for2M_ [i,i+n..] xs $ \i x -> pokeByteOff ptr i x
    where n = sizeOf (undefined :: a)

getExStorableList :: forall a . Storable a => BS.ByteString -> [a]
getExStorableList = \bs -> unsafePerformIO $ BS.unsafeUseAsCStringLen bs $ \(p, size) ->
    let (d,m) = size `divMod` n in
    if m /= 0 then error "size mismatch" else forM [0..d-1] $ \i -> peekElemOff (castPtr p) i
    where n = sizeOf (undefined :: a)


-- repeating:
--     Word32, length of BS
--     BS
putExList :: [Builder] -> Builder
putExList xs = Builder (sum $ map (\b -> sizeBuilder b + 4) xs) $ \p i -> do
    let go _ [] = pure ()
        go i (Builder n b:xs) = do
            pokeByteOff p i (fromIntegral n :: Word32)
            b p (i+4)
            go (i+4+n) xs
    go i xs

getExList :: BS.ByteString -> [BS.ByteString]
getExList bs
    | len == 0 = []
    | len >= 4
    , (n :: Word32, bs) <- unsafeBinarySplit bs
    , n <- fromIntegral n
    , (len - 4) >= n
    = BS.unsafeTake n bs : getExList (BS.unsafeDrop n bs)
    | otherwise = error "getList, corrupted binary"
    where len = BS.length bs

putExN :: Builder -> Builder
putExN (Builder n old) = Builder (n+4) $ \p i -> do
    pokeByteOff p i (fromIntegral n :: Word32)
    old p $ i+4

getExN :: BS.ByteString -> (BS.ByteString, BS.ByteString)
getExN bs
    | len >= 4
    , (n :: Word32, bs) <- unsafeBinarySplit bs
    , n <- fromIntegral n
    , (len - 4) >= n
    = (BS.unsafeTake n bs, BS.unsafeDrop n bs)
    | otherwise = error "getList, corrupted binary"
    where len = BS.length bs
