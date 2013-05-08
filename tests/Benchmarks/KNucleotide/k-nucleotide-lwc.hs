--
-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
--
-- contributed by Stephen Blackheath (with some bits taken from Don Stewart's
--     version), v1.2

import Text.Printf
import Data.ByteString.Internal
import qualified Data.ByteString.Char8 as S
import Control.Applicative
import Control.Monad
import LwConc.Substrate
import LwConc.RunQueue
import LwConc.MVar
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.ForeignPtr
import Data.Word
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.IORef
import GHC.Exts

initSched = do
  newSched
  n <- getNumCapabilities
  spawnScheds $ n-1
  where
    spawnScheds 0 = return ()
    spawnScheds n = do
      newCapability
      spawnScheds (n-1)


main = do
    initSched
    genome <- extract (S.pack ">TH")
    let actions = [
                do
                    a <- printFreqsBySize genome 1
                    b <- printFreqsBySize genome 2
                    return $ a ++ b
            ] ++ map (printFreqsSpecific genome) specificSeqs
    output <- concat <$> parallel actions
    forM_ output putStrLn

-- Drop in replacement for sequence
parallel :: [IO a] -> IO [a]
parallel actions = do
    vars <- forM actions $ \action -> do
        var <- newEmptyMVar
        forkIO $ do
            answer <- action
            putMVar var answer
        return var
    forM vars takeMVar

specificSeqs = map S.pack [
    "GGT","GGTA","GGTATT","GGTATTTTAATT","GGTATTTTAATTTATAGT"]

extract p = do
    s <- S.getContents
    let (_, rem)  = S.breakSubstring p s
    return $! S.map toUpper             -- array fusion!
            . S.filter    ((/=) '\n')
            . S.dropWhile ((/=) '\n') $ rem

printFreqsBySize :: S.ByteString -> Int -> IO [String]
printFreqsBySize genome keySize = do
        ht0 <- htNew keySize
        ht <- hashGenome genome keySize ht0
        l <- htToList ht
        htFree ht
        return $ map draw (sortBy sortRule l) ++ [""]
    where
        genomeLen = S.length genome
        draw :: (S.ByteString, Int) -> String
        draw (key, count) = printf "%s %.3f" (S.unpack key) pct
            where pct   = (100 * (fromIntegral count) / total) :: Double
                  total = fromIntegral (genomeLen - keySize + 1)

printFreqsSpecific :: S.ByteString -> S.ByteString -> IO [String]
printFreqsSpecific genome seq = do
    let keySize = S.length seq
        genomeLen = S.length genome
    ht0 <- htNew keySize
    ht <- hashGenome genome keySize ht0
    let (fp, offset, len) = toForeignPtr seq
    count <- withForeignPtr fp $ \p_ -> do
        htGet ht (p_ `plusPtr` offset)
    htFree ht
    return [show count ++ ('\t' : S.unpack seq)]

hashGenome :: S.ByteString
           -> Int
           -> Hashtable
           -> IO Hashtable
{-# INLINE hashGenome #-}
hashGenome genome keySize ht = do
    let (fp, offset, len) = toForeignPtr genome
    withForeignPtr fp $ \p_ -> do
        let p = p_ `plusPtr` offset
            loop ht idx = do
                let key = p `plusPtr` idx
                htInc ht key
            endIdx = len - keySize + 1
            foldMe i ht | i == endIdx = return ht
            foldMe i ht = loop ht i >>= foldMe (i+1)
        foldMe 0 ht

sortRule :: (S.ByteString, Int) -> (S.ByteString, Int) -> Ordering
sortRule (a1, b1) (a2, b2) =
    case compare b2 b1 of
        EQ -> compare a1 a2
        x  -> x

------ Hash table implementation ----------------------------------------------

-- Note: Hash tables are not generally used in functional languages, so there
-- are no available library implementations for Haskell.  This benchmark
-- requires a hash table.  This is why I have implemented the hash table here.

htNew :: Int -> IO Hashtable
htNew = htNew' (head primes)

htNew' :: Int -> Int -> IO Hashtable
htNew' slots ksz = do
    let ssz = spineSize ksz slots
    sp <- mallocBytes ssz
    memset sp 0 (fromIntegral ssz)
    return $ Hashtable {
            keySize   = ksz,
            noOfSlots = slots,
            spine     = sp
        }

primes = [ 1543,     3079,      6151,      12289,     24593,
           49157,    98317,     196613,    93241,     786433,
           1572869,  3145739,   6291469,   12582917,  25165843,
           50331653, 100663319, 201326611, 402653189, 805306457 ]

htFree :: Hashtable -> IO ()
htFree ht = do
    htTraverse ht $ \isSpine (Entry ePtr) -> when (not isSpine) $ free ePtr
    free (spine ht)

htGet :: Hashtable -> Ptr Word8 -> IO Int
htGet ht key = do
    hash <- calcHash ht key
    htPayload ht hash key >>= peek

htInc :: Hashtable -> Ptr Word8 -> IO Hashtable
{-# INLINE htInc #-}
htInc !ht !key = do
    hash <- calcHash ht key
    pPayload <- htPayload ht hash key
    value <- peek pPayload
    poke pPayload (value+1)
    if value == 0 && (hash .&. 0x7f) == 0
        then checkGrow ht
        else return ht

htPut_ :: Hashtable -> Ptr Word8 -> Int -> IO ()
{-# INLINE htPut_ #-}
htPut_ !ht !key !value = do
    hash <- calcHash ht key
    pPayload <- htPayload ht hash key
    poke pPayload value

checkGrow :: Hashtable -> IO Hashtable
checkGrow ht = do
        let pTotal = totalEntriesOf ht
            slots = noOfSlots ht
        total <- (0x200+) <$> peek pTotal
        poke pTotal total
        if total >= slots
            then do
                let newSlots = head $ dropWhile (<= slots*2) primes
                ht' <- htNew' newSlots (keySize ht)
                htTraverse ht $ \_ -> reinsert ht' -- re-insert all the elts
                htFree ht
                poke (totalEntriesOf ht') total -- copy the total entry count
                return ht'
            else return ht
    where
        reinsert :: Hashtable -> Entry -> IO ()
        reinsert ht entry = do
            value <- peek (payloadOf entry)
            htPut_ ht (keyOf entry) value

htToList :: Hashtable -> IO [(S.ByteString, Int)]
htToList ht =
    htMap (\entry -> do
        keyStr <- keyString ht (keyOf entry)
        payload <- peek (payloadOf entry)
        return (keyStr, payload)) ht

htMap :: (Entry -> IO a) -> Hashtable -> IO [a]
htMap f ht = mapM f =<< htEntries ht

keyString :: Hashtable -> Ptr Word8 -> IO S.ByteString
keyString ht key = S.pack . map w2c <$> peekArray (keySize ht) key

isEmptySlot :: Entry -> IO Bool
{-# INLINE isEmptySlot #-}
isEmptySlot entry = do
    ch <- peek $ keyOf entry
    return $ ch == 0

htEntries :: Hashtable -> IO [Entry]
htEntries ht = do
    es <- newIORef []
    htTraverse ht $ \_ entry -> modifyIORef es $ \l -> entry:l
    readIORef es

htTraverse :: Hashtable -> (Bool -> Entry -> IO ()) -> IO ()
htTraverse ht f = he 0
    where
        slots = noOfSlots ht
        he i | i == slots = return ()
        he i = do
            let entry = indexEntry ht i
            empty <- isEmptySlot entry
            if empty
                then he (i+1)
                else links True i entry
        links isSpine i entry = do
            next <- peek $ nextPtrOf entry
            f isSpine entry
            if next == nullPtr
                then he (i+1)
                else links False i (Entry next)

data Hashtable = Hashtable {
        keySize   :: Int,
        noOfSlots :: Int,
        spine     :: Ptr Word8
    }

wordSize :: Int
wordSize = max (sizeOf (nullPtr :: Ptr Word8)) (sizeOf (0 :: Int))

-- Round up to word size
roundUp :: Int -> Int
{-# INLINE roundUp #-}
roundUp !i = (i + wordSize - 1) .&. complement (wordSize - 1)

slotSize :: Int -> Int
{-# INLINE slotSize #-}
slotSize !ksz = roundUp ksz + wordSize * 2

spineSize :: Int -> Int -> Int
spineSize ksz slots = slots * slotSize ksz + wordSize

calcHash :: Hashtable -> Ptr Word8 -> IO Int
{-# INLINE calcHash #-}
calcHash !ht !key = (`mod` noOfSlots ht) <$> ch 0 0
    where
        ksz = keySize ht
        ch :: Int -> Int -> IO Int
        ch !i !acc | i == ksz = return acc
        ch !i !acc = do
            c <- peek (key `plusPtr` i)
            ch (i+1) (acc * 131 + fromIntegral (c::Word8))

newtype Entry = Entry (Ptr Word8)

-- Count of the total number of hash table entries
totalEntriesOf :: Hashtable -> Ptr Int
{-# INLINE totalEntriesOf #-}
totalEntriesOf ht = castPtr $ spine ht

indexEntry :: Hashtable -> Int -> Entry
{-# INLINE indexEntry #-}
indexEntry !ht !hash =
    let hOffset = wordSize + hash * slotSize (keySize ht)
    in  Entry $ spine ht `plusPtr` hOffset

entryMatches :: Hashtable -> Entry -> Ptr Word8 -> IO Bool
{-# INLINE entryMatches #-}
entryMatches !ht !entry !key = do
    let eKey = keyOf entry
    c <- memcmp key eKey (fromIntegral $ keySize ht)
    if c == 0
        then return True
        else do
            empty <- isEmptySlot entry
            if empty
                then do
                    memcpy eKey key (fromIntegral $ keySize ht)  -- ick
                    return True
                else
                    return False

nextPtrOf :: Entry -> Ptr (Ptr Word8)
{-# INLINE nextPtrOf #-}
nextPtrOf !(Entry ePtr) = castPtr $ ePtr

payloadOf :: Entry -> Ptr Int
{-# INLINE payloadOf #-}
payloadOf !(Entry ePtr) = castPtr $ ePtr `plusPtr` wordSize

keyOf :: Entry -> Ptr Word8
{-# INLINE keyOf #-}
keyOf !(Entry ePtr) = ePtr `plusPtr` (wordSize*2)

allocEntry :: Hashtable -> Ptr Word8 -> IO Entry
allocEntry !ht !key = do
    let esz = slotSize $ keySize ht
    ePtr <- mallocBytes esz
    memset ePtr 0 (fromIntegral esz)
    let entry = Entry ePtr
    memcpy (keyOf entry) key (fromIntegral $ keySize ht)
    return entry

htPayload :: Hashtable -> Int -> Ptr Word8 -> IO (Ptr Int)
{-# INLINE htPayload #-}
htPayload !ht !hash !key = do
        entry <- findEntry (indexEntry ht hash)
        return $ payloadOf entry
    where
        findEntry :: Entry -> IO Entry
        findEntry !entry = do
            match <- entryMatches ht entry key
            if match
                then
                    return entry
                else do
                    let pNext = nextPtrOf entry
                    next <- peek pNext
                    if next == nullPtr
                        then do
                            newEntry@(Entry ePtr) <- allocEntry ht key
                            poke pNext ePtr
                            return newEntry
                        else
                            findEntry (Entry next)


