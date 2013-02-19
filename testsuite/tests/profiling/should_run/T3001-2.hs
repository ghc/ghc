
-- A second test for trac #3001, which segfaults when compiled by
-- GHC 6.10.1 and run with +RTS -hb. Most of the code is from the
-- binary 0.4.4 package.

{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts, MagicHash #-}

module Main (main) where

import Data.Monoid

import Data.ByteString.Internal (inlinePerformIO)

import qualified Data.ByteString      as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L

import GHC.Exts
import GHC.Word

import Control.Monad
import Foreign
import System.IO.Unsafe
import System.IO

import Data.Char    (chr,ord)

main :: IO ()
main = do
  encodeFile "test.bin" $ replicate 10000 'x'
  print =<< (decodeFile "test.bin" :: IO String)

class Binary t where
    put :: t -> Put
    get :: Get t

encodeFile :: Binary a => FilePath -> a -> IO ()
encodeFile f v = L.writeFile f $ runPut $ put v

decodeFile :: Binary a => FilePath -> IO a
decodeFile f = do
    s <- L.readFile f
    return $ runGet (do v <- get
                        m <- isEmpty
                        m `seq` return v) s

instance Binary Word8 where
    put     = putWord8
    get     = getWord8

instance Binary Word32 where
    put     = putWord32be
    get     = getWord32be

instance Binary Int32 where
    put i   = put (fromIntegral i :: Word32)
    get     = liftM fromIntegral (get :: Get Word32)

instance Binary Int where
    put i   = put (fromIntegral i :: Int32)
    get     = liftM fromIntegral (get :: Get Int32)

instance Binary Char where
    put a = put (ord a)
    get = do w <- get
             return $! chr w

instance Binary a => Binary [a] where
    put l  = put (length l) >> mapM_ put l
    get    = do n <- get
                replicateM n get

data PairS a = PairS a !Builder

sndS :: PairS a -> Builder
sndS (PairS _ b) = b

newtype PutM a = Put { unPut :: PairS a }

type Put = PutM ()

instance Functor PutM where
        fmap f m = Put $ let PairS a w = unPut m in PairS (f a) w

instance Monad PutM where
    return a = Put $ PairS a mempty

    m >>= k  = Put $
        let PairS a w  = unPut m
            PairS b w' = unPut (k a)
        in PairS b (w `mappend` w')

    m >> k  = Put $
        let PairS _ w  = unPut m
            PairS b w' = unPut k
        in PairS b (w `mappend` w')

tell :: Builder -> Put
tell b = Put $ PairS () b

runPut :: Put -> L.ByteString
runPut = toLazyByteString . sndS . unPut

putWord8            :: Word8 -> Put
putWord8            = tell . singletonB

putWord32be         :: Word32 -> Put
putWord32be         = tell . putWord32beB

-----

newtype Get a = Get { unGet :: S -> (a, S) }

data S = S {-# UNPACK #-} !S.ByteString  -- current chunk
           L.ByteString                  -- the rest of the input
           {-# UNPACK #-} !Int64         -- bytes read

runGet :: Get a -> L.ByteString -> a
runGet m str = case unGet m (initState str) of (a, _) -> a

isEmpty :: Get Bool
isEmpty = do
    S s ss _ <- getZ
    return (S.null s && L.null ss)

initState :: L.ByteString -> S
initState xs = mkState xs 0

getWord32be :: Get Word32
getWord32be = do
    s <- readN 4 id
    return $! (fromIntegral (s `S.index` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `S.index` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `S.index` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `S.index` 3) )

getWord8 :: Get Word8
getWord8 = getPtr (sizeOf (undefined :: Word8))

mkState :: L.ByteString -> Int64 -> S
mkState l = case l of
    L.Empty      -> S S.empty L.empty
    L.Chunk x xs -> S x xs

readN :: Int -> (S.ByteString -> a) -> Get a
readN n f = fmap f $ getBytes n

shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

getPtr :: Storable a => Int -> Get a
getPtr n = do
    (fp,o,_) <- readN n S.toForeignPtr
    return . S.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)

getBytes :: Int -> Get S.ByteString
getBytes n = do
    S s ss bytes <- getZ
    if n <= S.length s
        then do let (consume,rest) = S.splitAt n s
                putZ $! S rest ss (bytes + fromIntegral n)
                return $! consume
        else
              case L.splitAt (fromIntegral n) (s `joinZ` ss) of
                (consuming, rest) ->
                    do let now = S.concat . L.toChunks $ consuming
                       putZ $! mkState rest (bytes + fromIntegral n)
                       -- forces the next chunk before this one is returned
                       if (S.length now < n)
                         then
                            fail "too few bytes"
                         else
                            return now

joinZ :: S.ByteString -> L.ByteString -> L.ByteString
joinZ bb lb
    | S.null bb = lb
    | otherwise = L.Chunk bb lb

instance Monad Get where
    return a  = Get (\s -> (a, s))
    {-# INLINE return #-}

    m >>= k   = Get (\s -> let (a, s') = unGet m s
                           in unGet (k a) s')
    {-# INLINE (>>=) #-}

    fail      = error "failDesc"

getZ :: Get S
getZ   = Get (\s -> (s, s))

putZ :: S -> Get ()
putZ s = Get (\_ -> ((), s))


instance Functor Get where
    fmap f m = Get (\s -> case unGet m s of
                             (a, s') -> (f a, s'))

-----

singletonB :: Word8 -> Builder
singletonB = writeN 1 . flip poke

writeN :: Int -> (Ptr Word8 -> IO ()) -> Builder
writeN n f = ensureFree n `append` unsafeLiftIO (writeNBuffer n f)

unsafeLiftIO :: (Buffer -> IO Buffer) -> Builder
unsafeLiftIO f =  Builder $ \ k buf -> inlinePerformIO $ do
    buf' <- f buf
    return (k buf')

append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder (f . g)

writeNBuffer :: Int -> (Ptr Word8 -> IO ()) -> Buffer -> IO Buffer
writeNBuffer n f (Buffer fp o u l) = do
    withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    return (Buffer fp o (u+n) (l-n))

newtype Builder = Builder {
        -- Invariant (from Data.ByteString.Lazy):
        --      The lists include no null ByteStrings.
        runBuilder :: (Buffer -> [S.ByteString]) -> Buffer -> [S.ByteString]
    }

data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- used bytes
                     {-# UNPACK #-} !Int                -- length left

toLazyByteString :: Builder -> L.ByteString
toLazyByteString m = L.fromChunks $ unsafePerformIO $ do
    buf <- newBuffer defaultSize
    return (runBuilder (m `append` flush) (const []) buf)

ensureFree :: Int -> Builder
ensureFree n = n `seq` withSize $ \ l ->
    if n <= l then empty else
        flush `append` unsafeLiftIO (const (newBuffer (max n defaultSize)))

withSize :: (Int -> Builder) -> Builder
withSize f = Builder $ \ k buf@(Buffer _ _ _ l) ->
    runBuilder (f l) k buf

defaultSize :: Int
defaultSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

newBuffer :: Int -> IO Buffer
newBuffer size = do
    fp <- S.mallocByteString size
    return $! Buffer fp 0 0 size

putWord32beB :: Word32 -> Builder
putWord32beB w = writeN 4 $ \p -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)

shiftr_w32 :: Word32 -> Int -> Word32
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#`   i)

flush :: Builder
flush = Builder $ \ k buf@(Buffer p o u l) ->
    if u == 0
      then k buf
      else S.PS p o u : k (Buffer p (o+u) 0 l)

empty :: Builder
empty = Builder id

instance Monoid Builder where
    mempty  = empty
    mappend = append

