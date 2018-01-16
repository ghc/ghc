{-
The Computer Language Benchmarks Game
http://benchmarksgame.alioth.debian.org/

contributed by Louis Wasserman
-}

import Control.Monad
import Foreign
import Data.ByteString.Internal
import System.IO

data Buf = Buf !Int !Int !(Ptr Word8) 

withBuf run = run . Buf 0 ini =<< mallocBytes ini
  where ini = 1024

newSize len sz
  | len <= sz  = sz
  | otherwise  = newSize len (2 * sz)

{-# INLINE putBuf #-}
putBuf pS lS (Buf lD szD pD) run
  | lD' > szD  = do
    let szD' = newSize lD' szD
    pD' <- reallocBytes pD szD'
    copyArray (pD' +* lD) pS lS
    run (Buf lD' szD' pD')
  | otherwise  = do
    copyArray (pD +* lD) pS lS
    run (Buf lD' szD pD)
  where lD' = lD + lS

findChar p n c zero one = do
    q <- memchr p c (fromIntegral (n :: Int))
    if q == nullPtr then zero else one $! q `minusPtr` p

clearBuf (Buf _ lB pB) = Buf 0 lB pB

main = allocaArray 82 $ \ line ->
  let go !buf = do
      !m <- hGetBuf stdin line 82
      if m == 0 then revcomp buf else do
        findChar line m (c2w '>') 
          (putBuf line m buf go)
          (\ end -> do
            putBuf line end buf revcomp
            putBuf (line +* end) (m - end) (clearBuf buf)
              go)
    in withBuf go

(+*) = advancePtr

{-# INLINE comps #-}
comps = Prelude.zipWith (\ a b -> (fromEnum a, c2w b)) "AaCcGgTtUuMmRrYyKkVvHhDdBb" 
  "TTGGCCAAAAKKYYRRMMBBDDHHVV"

ca :: Ptr Word8
ca = inlinePerformIO $ do
       !a <- mallocArray 200
       mapM_ (\ i -> pokeByteOff a (fromIntegral i) i ) [0..199::Word8]
       mapM_ (uncurry (pokeByteOff a)) comps
       return a

revcomp (Buf lBuf _ pBuf) = when (lBuf > 0) $ ca `seq`
  findChar pBuf lBuf (c2w '\n') undefined $ \ begin -> let
    begin' = begin + 1
    rc :: Ptr Word8 -> Ptr Word8 -> IO ()
    rc !i !j | i < j = do
      x <- peek i
      if x == c2w '\n' then let !i' = i +* 1 in rc1 j i' =<< peek i'
        else rc1 j i x
    rc i j = when (i == j) (poke i =<< comp =<< peek i)
    
    rc1 !j !i !xi = do
      y <- peek j
      if y == c2w '\n' then let !j' = j +* (-1) in rc2 i xi j' =<< peek j'
        else rc2 i xi j y
    
    comp = peekElemOff ca . fromIntegral
    
    rc2 !i !xi !j !xj = do
      poke j =<< comp xi
      poke i =<< comp xj
      rc (i +* 1) (j +* (-1))
    in do
      hPutBuf stdout pBuf begin'
      rc (pBuf +* begin') (pBuf +* (lBuf - 1))
      hPutBuf stdout (pBuf +* begin') (lBuf - begin - 1)
