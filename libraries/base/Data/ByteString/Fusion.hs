{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans #-}
-- |
-- Module      : Data.ByteString.Fusion
-- License     : BSD-style
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable
--
-- Functional array fusion for ByteStrings.
--
-- Originally based on code from the Data Parallel Haskell project, 
--      <http://www.cse.unsw.edu.au/~chak/project/dph>
--

-- #hide
module Data.ByteString.Fusion (

    -- * Fusion utilities
    loopU, loopL, fuseEFL,
    NoAcc(NoAcc), loopArr, loopAcc, loopSndAcc, unSP,
    mapEFL, filterEFL, foldEFL, foldEFL', scanEFL, mapAccumEFL, mapIndexEFL,

    -- ** Alternative Fusion stuff
    -- | This replaces 'loopU' with 'loopUp'
    -- and adds several further special cases of loops.
    loopUp, loopDown, loopNoAcc, loopMap, loopFilter,
    loopWrapper, sequenceLoops,
    doUpLoop, doDownLoop, doNoAccLoop, doMapLoop, doFilterLoop,

    -- | These are the special fusion cases for combining each loop form perfectly. 
    fuseAccAccEFL, fuseAccNoAccEFL, fuseNoAccAccEFL, fuseNoAccNoAccEFL,
    fuseMapAccEFL, fuseAccMapEFL, fuseMapNoAccEFL, fuseNoAccMapEFL,
    fuseMapMapEFL, fuseAccFilterEFL, fuseFilterAccEFL, fuseNoAccFilterEFL,
    fuseFilterNoAccEFL, fuseFilterFilterEFL, fuseMapFilterEFL, fuseFilterMapEFL,

    -- * Strict pairs and sums
    PairS(..), MaybeS(..)

  ) where

import Data.ByteString.Base

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable         (Storable(..))

import Data.Word                (Word8)
import System.IO.Unsafe         (unsafePerformIO)

-- -----------------------------------------------------------------------------
--
-- Useful macros, until we have bang patterns
--

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
#define STRICT5(f) f a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined

infixl 2 :*:

-- |Strict pair
data PairS a b = !a :*: !b deriving (Eq,Ord,Show)

-- |Strict Maybe
data MaybeS a = NothingS | JustS !a deriving (Eq,Ord,Show)

-- |Data type for accumulators which can be ignored. The rewrite rules rely on
-- the fact that no bottoms of this type are ever constructed; hence, we can
-- assume @(_ :: NoAcc) `seq` x = x@.
--
data NoAcc = NoAcc

-- |Type of loop functions
type AccEFL acc = acc -> Word8 -> (PairS acc (MaybeS Word8))
type NoAccEFL   =        Word8 ->             MaybeS Word8
type MapEFL     =        Word8 ->                    Word8
type FilterEFL  =        Word8 ->             Bool

infixr 9 `fuseEFL`

-- |Fuse to flat loop functions
fuseEFL :: AccEFL acc1 -> AccEFL acc2 -> AccEFL (PairS acc1 acc2)
fuseEFL f g (acc1 :*: acc2) e1 =
    case f acc1 e1 of
        acc1' :*: NothingS -> (acc1' :*: acc2) :*: NothingS
        acc1' :*: JustS e2 ->
            case g acc2 e2 of
                acc2' :*: res -> (acc1' :*: acc2') :*: res
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] fuseEFL #-}
#endif

-- | Special forms of loop arguments
--
-- * These are common special cases for the three function arguments of gen
--   and loop; we give them special names to make it easier to trigger RULES
--   applying in the special cases represented by these arguments.  The
--   "INLINE [1]" makes sure that these functions are only inlined in the last
--   two simplifier phases.
--
-- * In the case where the accumulator is not needed, it is better to always
--   explicitly return a value `()', rather than just copy the input to the
--   output, as the former gives GHC better local information.
-- 

-- | Element function expressing a mapping only
#if !defined(LOOPNOACC_FUSION)
mapEFL :: (Word8 -> Word8) -> AccEFL NoAcc
mapEFL f = \_ e -> (NoAcc :*: (JustS $ f e))
#else
mapEFL :: (Word8 -> Word8) -> NoAccEFL
mapEFL f = \e -> JustS (f e)
#endif
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] mapEFL #-}
#endif

-- | Element function implementing a filter function only
#if !defined(LOOPNOACC_FUSION)
filterEFL :: (Word8 -> Bool) -> AccEFL NoAcc
filterEFL p = \_ e -> if p e then (NoAcc :*: JustS e) else (NoAcc :*: NothingS)
#else
filterEFL :: (Word8 -> Bool) -> NoAccEFL
filterEFL p = \e -> if p e then JustS e else NothingS
#endif

#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] filterEFL #-}
#endif

-- |Element function expressing a reduction only
foldEFL :: (acc -> Word8 -> acc) -> AccEFL acc
foldEFL f = \a e -> (f a e :*: NothingS)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] foldEFL #-}
#endif

-- | A strict foldEFL.
foldEFL' :: (acc -> Word8 -> acc) -> AccEFL acc
foldEFL' f = \a e -> let a' = f a e in a' `seq` (a' :*: NothingS)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] foldEFL' #-}
#endif

-- | Element function expressing a prefix reduction only
--
scanEFL :: (Word8 -> Word8 -> Word8) -> AccEFL Word8
scanEFL f = \a e -> (f a e :*: JustS a)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] scanEFL #-}
#endif

-- | Element function implementing a map and fold
--
mapAccumEFL :: (acc -> Word8 -> (acc, Word8)) -> AccEFL acc
mapAccumEFL f = \a e -> case f a e of (a', e') -> (a' :*: JustS e')
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] mapAccumEFL #-}
#endif

-- | Element function implementing a map with index
--
mapIndexEFL :: (Int -> Word8 -> Word8) -> AccEFL Int
mapIndexEFL f = \i e -> let i' = i+1 in i' `seq` (i' :*: JustS (f i e))
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] mapIndexEFL #-}
#endif

-- | Projection functions that are fusion friendly (as in, we determine when
-- they are inlined)
loopArr :: (PairS acc arr) -> arr
loopArr (_ :*: arr) = arr
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopArr #-}
#endif

loopAcc :: (PairS acc arr) -> acc
loopAcc (acc :*: _) = acc
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopAcc #-}
#endif

loopSndAcc :: (PairS (PairS acc1 acc2) arr) -> (PairS acc2 arr)
loopSndAcc ((_ :*: acc) :*: arr) = (acc :*: arr)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopSndAcc #-}
#endif

unSP :: (PairS acc arr) -> (acc, arr)
unSP (acc :*: arr) = (acc, arr)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] unSP #-}
#endif

------------------------------------------------------------------------
--
-- Loop combinator and fusion rules for flat arrays
-- |Iteration over over ByteStrings

-- | Iteration over over ByteStrings
loopU :: AccEFL acc                 -- ^ mapping & folding, once per elem
      -> acc                        -- ^ initial acc value
      -> ByteString                 -- ^ input ByteString
      -> (PairS acc ByteString)

loopU f start (PS z s i) = unsafePerformIO $ withForeignPtr z $ \a -> do
    (ps, acc) <- createAndTrim' i $ \p -> do
      (acc' :*: i') <- go (a `plusPtr` s) p start
      return (0, i', acc')
    return (acc :*: ps)

  where
    go p ma = trans 0 0
        where
            STRICT3(trans)
            trans a_off ma_off acc
                | a_off >= i = return (acc :*: ma_off)
                | otherwise  = do
                    x <- peekByteOff p a_off
                    let (acc' :*: oe) = f acc x
                    ma_off' <- case oe of
                        NothingS -> return ma_off
                        JustS e  -> do pokeByteOff ma ma_off e
                                       return $ ma_off + 1
                    trans (a_off+1) ma_off' acc'

#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopU #-}
#endif

{-# RULES

"FPS loop/loop fusion!" forall em1 em2 start1 start2 arr.
  loopU em2 start2 (loopArr (loopU em1 start1 arr)) =
    loopSndAcc (loopU (em1 `fuseEFL` em2) (start1 :*: start2) arr)

  #-}

--
-- Functional list/array fusion for lazy ByteStrings.
--
loopL :: AccEFL acc          -- ^ mapping & folding, once per elem
      -> acc                 -- ^ initial acc value
      -> [ByteString]        -- ^ input ByteString
      -> PairS acc [ByteString]
loopL f = loop
  where loop s []     = (s :*: [])
        loop s (x:xs)
          | l == 0    = (s'' :*: ys)
          | otherwise = (s'' :*: y:ys)
          where (s'  :*: y@(PS _ _ l)) = loopU f s x -- avoid circular dep on P.null
                (s'' :*: ys)           = loop s' xs

#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopL #-}
#endif

{-# RULES

"FPS lazy loop/loop fusion!" forall em1 em2 start1 start2 arr.
  loopL em2 start2 (loopArr (loopL em1 start1 arr)) =
    loopSndAcc (loopL (em1 `fuseEFL` em2) (start1 :*: start2) arr)

  #-}


{-

Alternate experimental formulation of loopU which partitions it into
an allocating wrapper and an imperitive array-mutating loop.

The point in doing this split is that we might be able to fuse multiple
loops into a single wrapper. This would save reallocating another buffer.
It should also give better cache locality by reusing the buffer.

Note that this stuff needs ghc-6.5 from May 26 or later for the RULES to
really work reliably.

-}

loopUp :: AccEFL acc -> acc -> ByteString -> PairS acc ByteString
loopUp f a arr = loopWrapper (doUpLoop f a) arr
{-# INLINE loopUp #-}

loopDown :: AccEFL acc -> acc -> ByteString -> PairS acc ByteString
loopDown f a arr = loopWrapper (doDownLoop f a) arr
{-# INLINE loopDown #-}

loopNoAcc :: NoAccEFL -> ByteString -> PairS NoAcc ByteString
loopNoAcc f arr = loopWrapper (doNoAccLoop f NoAcc) arr
{-# INLINE loopNoAcc #-}

loopMap :: MapEFL -> ByteString -> PairS NoAcc ByteString
loopMap f arr = loopWrapper (doMapLoop f NoAcc) arr
{-# INLINE loopMap #-}

loopFilter :: FilterEFL -> ByteString -> PairS NoAcc ByteString
loopFilter f arr = loopWrapper (doFilterLoop f NoAcc) arr
{-# INLINE loopFilter #-}

-- The type of imperitive loops that fill in a destination array by
-- reading a source array. They may not fill in the whole of the dest
-- array if the loop is behaving as a filter, this is why we return
-- the length that was filled in. The loop may also accumulate some
-- value as it loops over the source array.
--
type ImperativeLoop acc =
    Ptr Word8          -- pointer to the start of the source byte array
 -> Ptr Word8          -- pointer to ther start of the destination byte array
 -> Int                -- length of the source byte array
 -> IO (PairS (PairS acc Int) Int) -- result and offset, length of dest that was filled

loopWrapper :: ImperativeLoop acc -> ByteString -> PairS acc ByteString
loopWrapper body (PS srcFPtr srcOffset srcLen) = unsafePerformIO $
    withForeignPtr srcFPtr $ \srcPtr -> do
    (ps, acc) <- createAndTrim' srcLen $ \destPtr -> do
        (acc :*: destOffset :*: destLen) <-
          body (srcPtr `plusPtr` srcOffset) destPtr srcLen
        return (destOffset, destLen, acc)
    return (acc :*: ps)

doUpLoop :: AccEFL acc -> acc -> ImperativeLoop acc
doUpLoop f acc0 src dest len = loop 0 0 acc0
  where STRICT3(loop)
        loop src_off dest_off acc
            | src_off >= len = return (acc :*: 0 :*: dest_off)
            | otherwise      = do
                x <- peekByteOff src src_off
                case f acc x of
                  (acc' :*: NothingS) -> loop (src_off+1) dest_off acc'
                  (acc' :*: JustS x') -> pokeByteOff dest dest_off x'
                                      >> loop (src_off+1) (dest_off+1) acc'

doDownLoop :: AccEFL acc -> acc -> ImperativeLoop acc
doDownLoop f acc0 src dest len = loop (len-1) (len-1) acc0
  where STRICT3(loop)
        loop src_off dest_off acc
            | src_off < 0 = return (acc :*: dest_off + 1 :*: len - (dest_off + 1))
            | otherwise   = do
                x <- peekByteOff src src_off
                case f acc x of
                  (acc' :*: NothingS) -> loop (src_off-1) dest_off acc'
                  (acc' :*: JustS x') -> pokeByteOff dest dest_off x'
                                      >> loop (src_off-1) (dest_off-1) acc'

doNoAccLoop :: NoAccEFL -> noAcc -> ImperativeLoop noAcc
doNoAccLoop f noAcc src dest len = loop 0 0
  where STRICT2(loop)
        loop src_off dest_off
            | src_off >= len = return (noAcc :*: 0 :*: dest_off)
            | otherwise      = do
                x <- peekByteOff src src_off
                case f x of
                  NothingS -> loop (src_off+1) dest_off
                  JustS x' -> pokeByteOff dest dest_off x'
                           >> loop (src_off+1) (dest_off+1)

doMapLoop :: MapEFL -> noAcc -> ImperativeLoop noAcc
doMapLoop f noAcc src dest len = loop 0
  where STRICT1(loop)
        loop n
            | n >= len = return (noAcc :*: 0 :*: len)
            | otherwise      = do
                x <- peekByteOff src n
                pokeByteOff dest n (f x)
                loop (n+1) -- offset always the same, only pass 1 arg

doFilterLoop :: FilterEFL -> noAcc -> ImperativeLoop noAcc
doFilterLoop f noAcc src dest len = loop 0 0
  where STRICT2(loop)
        loop src_off dest_off
            | src_off >= len = return (noAcc :*: 0 :*: dest_off)
            | otherwise      = do
                x <- peekByteOff src src_off
                if f x
                  then pokeByteOff dest dest_off x
                    >> loop (src_off+1) (dest_off+1)
                  else loop (src_off+1) dest_off

-- run two loops in sequence,
-- think of it as: loop1 >> loop2
sequenceLoops :: ImperativeLoop acc1
              -> ImperativeLoop acc2
              -> ImperativeLoop (PairS acc1 acc2)
sequenceLoops loop1 loop2 src dest len0 = do
  (acc1 :*: off1 :*: len1) <- loop1 src dest len0
  (acc2 :*: off2 :*: len2) <-
    let src'  = dest `plusPtr` off1
        dest' = src' -- note that we are using dest == src
                     -- for the second loop as we are
                     -- mutating the dest array in-place!
     in loop2 src' dest' len1
  return ((acc1  :*: acc2) :*: off1 + off2 :*: len2)

  -- TODO: prove that this is associative! (I think it is)
  -- since we can't be sure how the RULES will combine loops.

#if defined(__GLASGOW_HASKELL__)

{-# INLINE [1] doUpLoop             #-}
{-# INLINE [1] doDownLoop           #-}
{-# INLINE [1] doNoAccLoop          #-}
{-# INLINE [1] doMapLoop            #-}
{-# INLINE [1] doFilterLoop         #-}

{-# INLINE [1] loopWrapper          #-}
{-# INLINE [1] sequenceLoops        #-}

{-# INLINE [1] fuseAccAccEFL        #-}
{-# INLINE [1] fuseAccNoAccEFL      #-}
{-# INLINE [1] fuseNoAccAccEFL      #-}
{-# INLINE [1] fuseNoAccNoAccEFL    #-}
{-# INLINE [1] fuseMapAccEFL        #-}
{-# INLINE [1] fuseAccMapEFL        #-}
{-# INLINE [1] fuseMapNoAccEFL      #-}
{-# INLINE [1] fuseNoAccMapEFL      #-}
{-# INLINE [1] fuseMapMapEFL        #-}
{-# INLINE [1] fuseAccFilterEFL     #-}
{-# INLINE [1] fuseFilterAccEFL     #-}
{-# INLINE [1] fuseNoAccFilterEFL   #-}
{-# INLINE [1] fuseFilterNoAccEFL   #-}
{-# INLINE [1] fuseFilterFilterEFL  #-}
{-# INLINE [1] fuseMapFilterEFL     #-}
{-# INLINE [1] fuseFilterMapEFL     #-}

#endif

{-# RULES

"FPS loopArr/loopSndAcc" forall x.
  loopArr (loopSndAcc x) = loopArr x

"FPS seq/NoAcc" forall (u::NoAcc) e.
  u `seq` e = e

"FPS loop/loop wrapper elimination" forall loop1 loop2 arr.
  loopWrapper loop2 (loopArr (loopWrapper loop1 arr)) =
    loopSndAcc (loopWrapper (sequenceLoops loop1 loop2) arr)

--
-- n.b in the following, when reading n/m fusion, recall sequenceLoops
-- is monadic, so its really n >> m fusion (i.e. m.n), not n . m fusion.
--

"FPS up/up loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doUpLoop f1 acc1) (doUpLoop f2 acc2) =
    doUpLoop (f1 `fuseAccAccEFL` f2) (acc1 :*: acc2)

"FPS map/map loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doMapLoop f1 acc1) (doMapLoop f2 acc2) =
    doMapLoop (f1 `fuseMapMapEFL` f2) (acc1 :*: acc2)

"FPS filter/filter loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doFilterLoop f1 acc1) (doFilterLoop f2 acc2) =
    doFilterLoop (f1 `fuseFilterFilterEFL` f2) (acc1 :*: acc2)

"FPS map/filter loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doMapLoop f1 acc1) (doFilterLoop f2 acc2) =
    doNoAccLoop (f1 `fuseMapFilterEFL` f2) (acc1 :*: acc2)

"FPS filter/map loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doFilterLoop f1 acc1) (doMapLoop f2 acc2) =
    doNoAccLoop (f1 `fuseFilterMapEFL` f2) (acc1 :*: acc2)

"FPS map/up loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doMapLoop f1 acc1) (doUpLoop f2 acc2) =
    doUpLoop (f1 `fuseMapAccEFL` f2) (acc1 :*: acc2)

"FPS up/map loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doUpLoop f1 acc1) (doMapLoop f2 acc2) =
    doUpLoop (f1 `fuseAccMapEFL` f2) (acc1 :*: acc2)

"FPS filter/up loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doFilterLoop f1 acc1) (doUpLoop f2 acc2) =
    doUpLoop (f1 `fuseFilterAccEFL` f2) (acc1 :*: acc2)

"FPS up/filter loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doUpLoop f1 acc1) (doFilterLoop f2 acc2) =
    doUpLoop (f1 `fuseAccFilterEFL` f2) (acc1 :*: acc2)

"FPS down/down loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doDownLoop f1 acc1) (doDownLoop f2 acc2) =
    doDownLoop (f1 `fuseAccAccEFL` f2) (acc1 :*: acc2)

"FPS map/down fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doMapLoop f1 acc1) (doDownLoop f2 acc2) =
    doDownLoop (f1 `fuseMapAccEFL` f2) (acc1 :*: acc2)

"FPS down/map loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doDownLoop f1 acc1) (doMapLoop f2 acc2) =
    doDownLoop (f1 `fuseAccMapEFL` f2) (acc1 :*: acc2)

"FPS filter/down fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doFilterLoop f1 acc1) (doDownLoop f2 acc2) =
    doDownLoop (f1 `fuseFilterAccEFL` f2) (acc1 :*: acc2)

"FPS down/filter loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doDownLoop f1 acc1) (doFilterLoop f2 acc2) =
    doDownLoop (f1 `fuseAccFilterEFL` f2) (acc1 :*: acc2)

"FPS noAcc/noAcc loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doNoAccLoop f1 acc1) (doNoAccLoop f2 acc2) =
    doNoAccLoop (f1 `fuseNoAccNoAccEFL` f2) (acc1 :*: acc2)

"FPS noAcc/up loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doNoAccLoop f1 acc1) (doUpLoop f2 acc2) =
    doUpLoop (f1 `fuseNoAccAccEFL` f2) (acc1 :*: acc2)

"FPS up/noAcc loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doUpLoop f1 acc1) (doNoAccLoop f2 acc2) =
    doUpLoop (f1 `fuseAccNoAccEFL` f2) (acc1 :*: acc2)

"FPS map/noAcc loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doMapLoop f1 acc1) (doNoAccLoop f2 acc2) =
    doNoAccLoop (f1 `fuseMapNoAccEFL` f2) (acc1 :*: acc2)

"FPS noAcc/map loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doNoAccLoop f1 acc1) (doMapLoop f2 acc2) =
    doNoAccLoop (f1 `fuseNoAccMapEFL` f2) (acc1 :*: acc2)

"FPS filter/noAcc loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doFilterLoop f1 acc1) (doNoAccLoop f2 acc2) =
    doNoAccLoop (f1 `fuseFilterNoAccEFL` f2) (acc1 :*: acc2)

"FPS noAcc/filter loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doNoAccLoop f1 acc1) (doFilterLoop f2 acc2) =
    doNoAccLoop (f1 `fuseNoAccFilterEFL` f2) (acc1 :*: acc2)

"FPS noAcc/down loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doNoAccLoop f1 acc1) (doDownLoop f2 acc2) =
    doDownLoop (f1 `fuseNoAccAccEFL` f2) (acc1 :*: acc2)

"FPS down/noAcc loop fusion" forall f1 f2 acc1 acc2.
  sequenceLoops (doDownLoop f1 acc1) (doNoAccLoop f2 acc2) =
    doDownLoop (f1 `fuseAccNoAccEFL` f2) (acc1 :*: acc2)

  #-}

{-

up      = up loop
down    = down loop
map     = map special case
filter  = filter special case
noAcc   = noAcc undirectional loop (unused)

heirarchy:
  up     down
   ^     ^
    \   /
    noAcc
     ^ ^
    /   \
 map     filter

each is a special case of the things above

so we get rules that combine things on the same level
and rules that combine things on different levels
to get something on the higher level

so all the cases:
up/up         --> up     fuseAccAccEFL
down/down     --> down   fuseAccAccEFL
noAcc/noAcc   --> noAcc  fuseNoAccNoAccEFL

noAcc/up      --> up     fuseNoAccAccEFL
up/noAcc      --> up     fuseAccNoAccEFL
noAcc/down    --> down   fuseNoAccAccEFL
down/noAcc    --> down   fuseAccNoAccEFL

and if we do the map, filter special cases then it adds a load more:

map/map       --> map    fuseMapMapEFL
filter/filter --> filter fuseFilterFilterEFL

map/filter    --> noAcc  fuseMapFilterEFL
filter/map    --> noAcc  fuseFilterMapEFL

map/noAcc     --> noAcc  fuseMapNoAccEFL
noAcc/map     --> noAcc  fuseNoAccMapEFL

map/up        --> up     fuseMapAccEFL
up/map        --> up     fuseAccMapEFL

map/down      --> down   fuseMapAccEFL
down/map      --> down   fuseAccMapEFL

filter/noAcc  --> noAcc  fuseNoAccFilterEFL
noAcc/filter  --> noAcc  fuseFilterNoAccEFL

filter/up     --> up     fuseFilterAccEFL
up/filter     --> up     fuseAccFilterEFL

filter/down   --> down   fuseFilterAccEFL
down/filter   --> down   fuseAccFilterEFL
-}

fuseAccAccEFL :: AccEFL acc1 -> AccEFL acc2 -> AccEFL (PairS acc1 acc2)
fuseAccAccEFL f g (acc1 :*: acc2) e1 =
    case f acc1 e1 of
        acc1' :*: NothingS -> (acc1' :*: acc2) :*: NothingS
        acc1' :*: JustS e2 ->
            case g acc2 e2 of
                acc2' :*: res -> (acc1' :*: acc2') :*: res

fuseAccNoAccEFL :: AccEFL acc -> NoAccEFL -> AccEFL (PairS acc noAcc)
fuseAccNoAccEFL f g (acc :*: noAcc) e1 =
    case f acc e1 of
        acc' :*: NothingS -> (acc' :*: noAcc) :*: NothingS
        acc' :*: JustS e2 -> (acc' :*: noAcc) :*: g e2

fuseNoAccAccEFL :: NoAccEFL -> AccEFL acc -> AccEFL (PairS noAcc acc)
fuseNoAccAccEFL f g (noAcc :*: acc) e1 =
    case f e1 of
        NothingS -> (noAcc :*: acc) :*: NothingS
        JustS e2 ->
            case g acc e2 of
                acc' :*: res -> (noAcc :*: acc') :*: res

fuseNoAccNoAccEFL :: NoAccEFL -> NoAccEFL -> NoAccEFL
fuseNoAccNoAccEFL f g e1 =
    case f e1 of
        NothingS -> NothingS
        JustS e2 -> g e2

fuseMapAccEFL :: MapEFL -> AccEFL acc -> AccEFL (PairS noAcc acc)
fuseMapAccEFL f g (noAcc :*: acc) e1 =
    case g acc (f e1) of
        (acc' :*: res) -> (noAcc :*: acc') :*: res

fuseAccMapEFL :: AccEFL acc -> MapEFL -> AccEFL (PairS acc noAcc)
fuseAccMapEFL f g (acc :*: noAcc) e1 =
    case f acc e1 of
        (acc' :*: NothingS) -> (acc' :*: noAcc) :*: NothingS
        (acc' :*: JustS e2) -> (acc' :*: noAcc) :*: JustS (g e2)

fuseMapMapEFL :: MapEFL -> MapEFL -> MapEFL
fuseMapMapEFL   f g e1 = g (f e1)     -- n.b. perfect fusion

fuseMapNoAccEFL :: MapEFL -> NoAccEFL -> NoAccEFL
fuseMapNoAccEFL f g e1 = g (f e1)

fuseNoAccMapEFL :: NoAccEFL -> MapEFL -> NoAccEFL
fuseNoAccMapEFL f g e1 =
    case f e1 of
        NothingS -> NothingS
        JustS e2 -> JustS (g e2)

fuseAccFilterEFL :: AccEFL acc -> FilterEFL -> AccEFL (PairS acc noAcc)
fuseAccFilterEFL f g (acc :*: noAcc) e1 =
    case f acc e1 of
        acc' :*: NothingS -> (acc' :*: noAcc) :*: NothingS
        acc' :*: JustS e2 ->
            case g e2 of
                False -> (acc' :*: noAcc) :*: NothingS
                True  -> (acc' :*: noAcc) :*: JustS e2

fuseFilterAccEFL :: FilterEFL -> AccEFL acc -> AccEFL (PairS noAcc acc)
fuseFilterAccEFL f g (noAcc :*: acc) e1 =
    case f e1 of
        False -> (noAcc :*: acc) :*: NothingS
        True  ->
            case g acc e1 of
                acc' :*: res -> (noAcc :*: acc') :*: res

fuseNoAccFilterEFL :: NoAccEFL -> FilterEFL -> NoAccEFL
fuseNoAccFilterEFL f g e1 =
    case f e1 of
        NothingS -> NothingS
        JustS e2 ->
            case g e2 of
                False -> NothingS
                True  -> JustS e2

fuseFilterNoAccEFL :: FilterEFL -> NoAccEFL -> NoAccEFL
fuseFilterNoAccEFL f g e1 =
    case f e1 of
        False -> NothingS
        True  -> g e1

fuseFilterFilterEFL :: FilterEFL -> FilterEFL -> FilterEFL
fuseFilterFilterEFL f g e1 = f e1 && g e1

fuseMapFilterEFL :: MapEFL -> FilterEFL -> NoAccEFL
fuseMapFilterEFL f g e1 =
    case f e1 of
        e2 -> case g e2 of
            False -> NothingS
            True  -> JustS e2

fuseFilterMapEFL :: FilterEFL -> MapEFL -> NoAccEFL
fuseFilterMapEFL f g e1 =
    case f e1 of
        False -> NothingS
        True  -> JustS (g e1)

