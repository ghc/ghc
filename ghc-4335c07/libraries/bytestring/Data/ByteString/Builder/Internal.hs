{-# LANGUAGE ScopedTypeVariables, CPP, BangPatterns, RankNTypes #-}
#if __GLASGOW_HASKELL__ == 700
-- This is needed as a workaround for an old bug in GHC 7.0.1 (Trac #4498)
{-# LANGUAGE MonoPatBinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Unsafe #-}
#endif
{-# OPTIONS_HADDOCK hide #-}
-- | Copyright : (c) 2010 - 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : unstable, private
-- Portability : GHC
--
-- *Warning:* this module is internal. If you find that you need it then please
-- contact the maintainers and explain what you are trying to do and discuss
-- what you would need in the public API. It is important that you do this as
-- the module may not be exposed at all in future releases.
--
-- Core types and functions for the 'Builder' monoid and its generalization,
-- the 'Put' monad.
--
-- The design of the 'Builder' monoid is optimized such that
--
--   1. buffers of arbitrary size can be filled as efficiently as possible and
--
--   2. sequencing of 'Builder's is as cheap as possible.
--
-- We achieve (1) by completely handing over control over writing to the buffer
-- to the 'BuildStep' implementing the 'Builder'. This 'BuildStep' is just told
-- the start and the end of the buffer (represented as a 'BufferRange'). Then,
-- the 'BuildStep' can write to as big a prefix of this 'BufferRange' in any
-- way it desires. If the 'BuildStep' is done, the 'BufferRange' is full, or a
-- long sequence of bytes should be inserted directly, then the 'BuildStep'
-- signals this to its caller using a 'BuildSignal'.
--
-- We achieve (2) by requiring that every 'Builder' is implemented by a
-- 'BuildStep' that takes a continuation 'BuildStep', which it calls with the
-- updated 'BufferRange' after it is done. Therefore, only two pointers have
-- to be passed in a function call to implement concatenation of 'Builder's.
-- Moreover, many 'Builder's are completely inlined, which enables the compiler
-- to sequence them without a function call and with no boxing at all.
--
-- This design gives the implementation of a 'Builder' full access to the 'IO'
-- monad. Therefore, utmost care has to be taken to not overwrite anything
-- outside the given 'BufferRange's. Moreover, further care has to be taken to
-- ensure that 'Builder's and 'Put's are referentially transparent. See the
-- comments of the 'builder' and 'put' functions for further information.
-- Note that there are /no safety belts/ at all, when implementing a 'Builder'
-- using an 'IO' action: you are writing code that might enable the next
-- buffer-overflow attack on a Haskell server!
--
module Data.ByteString.Builder.Internal (
  -- * Buffer management
    Buffer(..)
  , BufferRange(..)
  , newBuffer
  , bufferSize
  , byteStringFromBuffer

  , ChunkIOStream(..)
  , buildStepToCIOS
  , ciosUnitToLazyByteString
  , ciosToLazyByteString

  -- * Build signals and steps
  , BuildSignal
  , BuildStep
  , finalBuildStep

  , done
  , bufferFull
  , insertChunk

  , fillWithBuildStep

  -- * The Builder monoid
  , Builder
  , builder
  , runBuilder
  , runBuilderWith

  -- ** Primitive combinators
  , empty
  , append
  , flush
  , ensureFree
  -- , sizedChunksInsert

  , byteStringCopy
  , byteStringInsert
  , byteStringThreshold

  , lazyByteStringCopy
  , lazyByteStringInsert
  , lazyByteStringThreshold

  , shortByteString

  , maximalCopySize
  , byteString
  , lazyByteString

  -- ** Execution
  , toLazyByteStringWith
  , AllocationStrategy
  , safeStrategy
  , untrimmedStrategy
  , customStrategy
  , L.smallChunkSize
  , L.defaultChunkSize
  , L.chunkOverhead

  -- * The Put monad
  , Put
  , put
  , runPut

  -- ** Execution
  , putToLazyByteString
  , putToLazyByteStringWith
  , hPut

  -- ** Conversion to and from Builders
  , putBuilder
  , fromPut

  -- -- ** Lifting IO actions
  -- , putLiftIO

) where

import           Control.Arrow (second)

#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup (Semigroup((<>)))
#endif
#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid
import           Control.Applicative (Applicative(..),(<$>))
#endif

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Short.Internal as Sh

#if __GLASGOW_HASKELL__ >= 611
import qualified GHC.IO.Buffer as IO (Buffer(..), newByteBuffer)
import           GHC.IO.Handle.Internals (wantWritableHandle, flushWriteBuffer)
import           GHC.IO.Handle.Types (Handle__, haByteBuffer, haBufferMode)
import           System.IO (hFlush, BufferMode(..))
import           Data.IORef
#else
import qualified Data.ByteString.Lazy as L
#endif
import           System.IO (Handle)

#if MIN_VERSION_base(4,4,0)
#if MIN_VERSION_base(4,7,0)
import           Foreign
#else
import           Foreign hiding (unsafeForeignPtrToPtr)
#endif
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import           System.IO.Unsafe (unsafeDupablePerformIO)
#else
import           Foreign
import           GHC.IO (unsafeDupablePerformIO)
#endif

------------------------------------------------------------------------------
-- Buffers
------------------------------------------------------------------------------
-- | A range of bytes in a buffer represented by the pointer to the first byte
-- of the range and the pointer to the first byte /after/ the range.
data BufferRange = BufferRange {-# UNPACK #-} !(Ptr Word8)  -- First byte of range
                               {-# UNPACK #-} !(Ptr Word8)  -- First byte /after/ range

-- | A 'Buffer' together with the 'BufferRange' of free bytes. The filled
-- space starts at offset 0 and ends at the first free byte.
data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !BufferRange


-- | Combined size of the filled and free space in the buffer.
{-# INLINE bufferSize #-}
bufferSize :: Buffer -> Int
bufferSize (Buffer fpbuf (BufferRange _ ope)) =
    ope `minusPtr` unsafeForeignPtrToPtr fpbuf

-- | Allocate a new buffer of the given size.
{-# INLINE newBuffer #-}
newBuffer :: Int -> IO Buffer
newBuffer size = do
    fpbuf <- S.mallocByteString size
    let pbuf = unsafeForeignPtrToPtr fpbuf
    return $! Buffer fpbuf (BufferRange pbuf (pbuf `plusPtr` size))

-- | Convert the filled part of a 'Buffer' to a strict 'S.ByteString'.
{-# INLINE byteStringFromBuffer #-}
byteStringFromBuffer :: Buffer -> S.ByteString
byteStringFromBuffer (Buffer fpbuf (BufferRange op _)) =
    S.PS fpbuf 0 (op `minusPtr` unsafeForeignPtrToPtr fpbuf)

-- | Prepend the filled part of a 'Buffer' to a lazy 'L.ByteString'
-- trimming it if necessary.
{-# INLINE trimmedChunkFromBuffer #-}
trimmedChunkFromBuffer :: AllocationStrategy -> Buffer
                       -> L.ByteString -> L.ByteString
trimmedChunkFromBuffer (AllocationStrategy _ _ trim) buf k
  | S.null bs                           = k
  | trim (S.length bs) (bufferSize buf) = L.Chunk (S.copy bs) k
  | otherwise                           = L.Chunk bs          k
  where
    bs = byteStringFromBuffer buf

------------------------------------------------------------------------------
-- Chunked IO Stream
------------------------------------------------------------------------------

-- | A stream of chunks that are constructed in the 'IO' monad.
--
-- This datatype serves as the common interface for the buffer-by-buffer
-- execution of a 'BuildStep' by 'buildStepToCIOS'. Typical users of this
-- interface are 'ciosToLazyByteString' or iteratee-style libraries like
-- @enumerator@.
data ChunkIOStream a =
       Finished Buffer a
       -- ^ The partially filled last buffer together with the result.
     | Yield1 S.ByteString (IO (ChunkIOStream a))
       -- ^ Yield a /non-empty/ strict 'S.ByteString'.

-- | A smart constructor for yielding one chunk that ignores the chunk if
-- it is empty.
{-# INLINE yield1 #-}
yield1 :: S.ByteString -> IO (ChunkIOStream a) -> IO (ChunkIOStream a)
yield1 bs cios | S.null bs = cios
               | otherwise = return $ Yield1 bs cios

-- | Convert a @'ChunkIOStream' ()@ to a lazy 'L.ByteString' using
-- 'unsafeDupablePerformIO'.
{-# INLINE ciosUnitToLazyByteString #-}
ciosUnitToLazyByteString :: AllocationStrategy
                         -> L.ByteString -> ChunkIOStream () -> L.ByteString
ciosUnitToLazyByteString strategy k = go
  where
    go (Finished buf _) = trimmedChunkFromBuffer strategy buf k
    go (Yield1 bs io)   = L.Chunk bs $ unsafeDupablePerformIO (go <$> io)

-- | Convert a 'ChunkIOStream' to a lazy tuple of the result and the written
-- 'L.ByteString' using 'unsafeDupablePerformIO'.
{-# INLINE ciosToLazyByteString #-}
ciosToLazyByteString :: AllocationStrategy
                     -> (a -> (b, L.ByteString))
                     -> ChunkIOStream a
                     -> (b, L.ByteString)
ciosToLazyByteString strategy k =
    go
  where
    go (Finished buf x) =
        second (trimmedChunkFromBuffer strategy buf) $ k x
    go (Yield1 bs io)   = second (L.Chunk bs) $ unsafeDupablePerformIO (go <$> io)

------------------------------------------------------------------------------
-- Build signals
------------------------------------------------------------------------------

-- | 'BuildStep's may be called *multiple times* and they must not rise an
-- async. exception.
type BuildStep a = BufferRange -> IO (BuildSignal a)

-- | 'BuildSignal's abstract signals to the caller of a 'BuildStep'. There are
-- three signals: 'done', 'bufferFull', or 'insertChunks signals
data BuildSignal a =
    Done {-# UNPACK #-} !(Ptr Word8) a
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
                     (BuildStep a)
  | InsertChunk
      {-# UNPACK #-} !(Ptr Word8)
                     S.ByteString
                     (BuildStep a)

-- | Signal that the current 'BuildStep' is done and has computed a value.
{-# INLINE done #-}
done :: Ptr Word8      -- ^ Next free byte in current 'BufferRange'
     -> a              -- ^ Computed value
     -> BuildSignal a
done = Done

-- | Signal that the current buffer is full.
{-# INLINE bufferFull #-}
bufferFull :: Int
           -- ^ Minimal size of next 'BufferRange'.
           -> Ptr Word8
           -- ^ Next free byte in current 'BufferRange'.
           -> BuildStep a
           -- ^ 'BuildStep' to run on the next 'BufferRange'. This 'BuildStep'
           -- may assume that it is called with a 'BufferRange' of at least the
           -- required minimal size; i.e., the caller of this 'BuildStep' must
           -- guarantee this.
           -> BuildSignal a
bufferFull = BufferFull


-- | Signal that a 'S.ByteString' chunk should be inserted directly.
{-# INLINE insertChunk #-}
insertChunk :: Ptr Word8
            -- ^ Next free byte in current 'BufferRange'
            -> S.ByteString
            -- ^ Chunk to insert.
            -> BuildStep a
            -- ^ 'BuildStep' to run on next 'BufferRange'
            -> BuildSignal a
insertChunk op bs = InsertChunk op bs


-- | Fill a 'BufferRange' using a 'BuildStep'.
{-# INLINE fillWithBuildStep #-}
fillWithBuildStep
    :: BuildStep a
    -- ^ Build step to use for filling the 'BufferRange'.
    -> (Ptr Word8 -> a -> IO b)
    -- ^ Handling the 'done' signal
    -> (Ptr Word8 -> Int -> BuildStep a -> IO b)
    -- ^ Handling the 'bufferFull' signal
    -> (Ptr Word8 -> S.ByteString -> BuildStep a -> IO b)
    -- ^ Handling the 'insertChunk' signal
    -> BufferRange
    -- ^ Buffer range to fill.
    -> IO b
    -- ^ Value computed while filling this 'BufferRange'.
fillWithBuildStep step fDone fFull fChunk !br = do
    signal <- step br
    case signal of
        Done op x                      -> fDone op x
        BufferFull minSize op nextStep -> fFull op minSize nextStep
        InsertChunk op bs nextStep     -> fChunk op bs nextStep


------------------------------------------------------------------------------
-- The 'Builder' monoid
------------------------------------------------------------------------------

-- | 'Builder's denote sequences of bytes.
-- They are 'Monoid's where
--   'mempty' is the zero-length sequence and
--   'mappend' is concatenation, which runs in /O(1)/.
newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)

-- | Construct a 'Builder'. In contrast to 'BuildStep's, 'Builder's are
-- referentially transparent.
{-# INLINE builder #-}
builder :: (forall r. BuildStep r -> BuildStep r)
        -- ^ A function that fills a 'BufferRange', calls the continuation with
        -- the updated 'BufferRange' once its done, and signals its caller how
        -- to proceed using 'done', 'bufferFull', or 'insertChunk'.
        --
        -- This function must be referentially transparent; i.e., calling it
        -- multiple times with equally sized 'BufferRange's must result in the
        -- same sequence of bytes being written. If you need mutable state,
        -- then you must allocate it anew upon each call of this function.
        -- Moroever, this function must call the continuation once its done.
        -- Otherwise, concatenation of 'Builder's does not work. Finally, this
        -- function must write to all bytes that it claims it has written.
        -- Otherwise, the resulting 'Builder' is not guaranteed to be
        -- referentially transparent and sensitive data might leak.
        -> Builder
builder = Builder

-- | The final build step that returns the 'done' signal.
finalBuildStep :: BuildStep ()
finalBuildStep !(BufferRange op _) = return $ Done op ()

-- | Run a 'Builder' with the 'finalBuildStep'.
{-# INLINE runBuilder #-}
runBuilder :: Builder      -- ^ 'Builder' to run
           -> BuildStep () -- ^ 'BuildStep' that writes the byte stream of this
                           -- 'Builder' and signals 'done' upon completion.
runBuilder b = runBuilderWith b finalBuildStep

-- | Run a 'Builder'.
{-# INLINE runBuilderWith #-}
runBuilderWith :: Builder      -- ^ 'Builder' to run
               -> BuildStep a -- ^ Continuation 'BuildStep'
               -> BuildStep a
runBuilderWith (Builder b) = b

-- | The 'Builder' denoting a zero-length sequence of bytes. This function is
-- only exported for use in rewriting rules. Use 'mempty' otherwise.
{-# INLINE[1] empty #-}
empty :: Builder
empty = Builder (\cont -> (\range -> cont range))
-- This eta expansion (hopefully) allows GHC to worker-wrapper the
-- 'BufferRange' in the 'empty' base case of loops (since
-- worker-wrapper requires (TODO: verify this) that all paths match
-- against the wrapped argument.

-- | Concatenate two 'Builder's. This function is only exported for use in rewriting
-- rules. Use 'mappend' otherwise.
{-# INLINE[1] append #-}
append :: Builder -> Builder -> Builder
append (Builder b1) (Builder b2) = Builder $ b1 . b2

#if MIN_VERSION_base(4,9,0)
instance Semigroup Builder where
  {-# INLINE (<>) #-}
  (<>) = append
#endif

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty = empty
  {-# INLINE mappend #-}
#if MIN_VERSION_base(4,9,0)
  mappend = (<>)
#else
  mappend = append
#endif
  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

-- | Flush the current buffer. This introduces a chunk boundary.
{-# INLINE flush #-}
flush :: Builder
flush = builder step
  where
    step k !(BufferRange op _) = return $ insertChunk op S.empty k


------------------------------------------------------------------------------
-- Put
------------------------------------------------------------------------------

-- | A 'Put' action denotes a computation of a value that writes a stream of
-- bytes as a side-effect. 'Put's are strict in their side-effect; i.e., the
-- stream of bytes will always be written before the computed value is
-- returned.
--
-- 'Put's are a generalization of 'Builder's. The typical use case is the
-- implementation of an encoding that might fail (e.g., an interface to the
-- 'zlib' compression library or the conversion from Base64 encoded data to
-- 8-bit data). For a 'Builder', the only way to handle and report such a
-- failure is ignore it or call 'error'.  In contrast, 'Put' actions are
-- expressive enough to allow reportng and handling such a failure in a pure
-- fashion.
--
-- @'Put' ()@ actions are isomorphic to 'Builder's. The functions 'putBuilder'
-- and 'fromPut' convert between these two types. Where possible, you should
-- use 'Builder's, as sequencing them is slightly cheaper than sequencing
-- 'Put's because they do not carry around a computed value.
newtype Put a = Put { unPut :: forall r. (a -> BuildStep r) -> BuildStep r }

-- | Construct a 'Put' action. In contrast to 'BuildStep's, 'Put's are
-- referentially transparent in the sense that sequencing the same 'Put'
-- multiple times yields every time the same value with the same side-effect.
{-# INLINE put #-}
put :: (forall r. (a -> BuildStep r) -> BuildStep r)
       -- ^ A function that fills a 'BufferRange', calls the continuation with
       -- the updated 'BufferRange' and its computed value once its done, and
       -- signals its caller how to proceed using 'done', 'bufferFull', or
       -- 'insertChunk' signals.
       --
    -- This function must be referentially transparent; i.e., calling it
    -- multiple times with equally sized 'BufferRange's must result in the
    -- same sequence of bytes being written and the same value being
    -- computed. If you need mutable state, then you must allocate it anew
    -- upon each call of this function. Moroever, this function must call
    -- the continuation once its done. Otherwise, monadic sequencing of
    -- 'Put's does not work. Finally, this function must write to all bytes
    -- that it claims it has written. Otherwise, the resulting 'Put' is
    -- not guaranteed to be referentially transparent and sensitive data
    -- might leak.
       -> Put a
put = Put

-- | Run a 'Put'.
{-# INLINE runPut #-}
runPut :: Put a       -- ^ Put to run
       -> BuildStep a -- ^ 'BuildStep' that first writes the byte stream of
                      -- this 'Put' and then yields the computed value using
                      -- the 'done' signal.
runPut (Put p) = p $ \x (BufferRange op _) -> return $ Done op x

instance Functor Put where
  fmap f p = Put $ \k -> unPut p (\x -> k (f x))
  {-# INLINE fmap #-}

-- | Synonym for '<*' from 'Applicative'; used in rewriting rules.
{-# INLINE[1] ap_l #-}
ap_l :: Put a -> Put b -> Put a
ap_l (Put a) (Put b) = Put $ \k -> a (\a' -> b (\_ -> k a'))

-- | Synonym for '*>' from 'Applicative' and '>>' from 'Monad'; used in
-- rewriting rules.
{-# INLINE[1] ap_r #-}
ap_r :: Put a -> Put b -> Put b
ap_r (Put a) (Put b) = Put $ \k -> a (\_ -> b k)

instance Applicative Put where
  {-# INLINE pure #-}
  pure x = Put $ \k -> k x
  {-# INLINE (<*>) #-}
  Put f <*> Put a = Put $ \k -> f (\f' -> a (\a' -> k (f' a')))
  {-# INLINE (<*) #-}
  (<*) = ap_l
  {-# INLINE (*>) #-}
  (*>) = ap_r

instance Monad Put where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  Put m >>= f = Put $ \k -> m (\m' -> unPut (f m') k)
  {-# INLINE (>>) #-}
  (>>) = (*>)

-- Conversion between Put and Builder
-------------------------------------

-- | Run a 'Builder' as a side-effect of a @'Put' ()@ action.
{-# INLINE[1] putBuilder #-}
putBuilder :: Builder -> Put ()
putBuilder (Builder b) = Put $ \k -> b (k ())

-- | Convert a @'Put' ()@ action to a 'Builder'.
{-# INLINE fromPut #-}
fromPut :: Put () -> Builder
fromPut (Put p) = Builder $ \k -> p (\_ -> k)

-- We rewrite consecutive uses of 'putBuilder' such that the append of the
-- involved 'Builder's is used. This can significantly improve performance,
-- when the bound-checks of the concatenated builders are fused.

-- ap_l rules
{-# RULES

"ap_l/putBuilder" forall b1 b2.
       ap_l (putBuilder b1) (putBuilder b2)
     = putBuilder (append b1 b2)

"ap_l/putBuilder/assoc_r" forall b1 b2 (p :: Put a).
       ap_l (putBuilder b1) (ap_l (putBuilder b2) p)
     = ap_l (putBuilder (append b1 b2)) p

"ap_l/putBuilder/assoc_l" forall (p :: Put a) b1 b2.
       ap_l (ap_l p (putBuilder b1)) (putBuilder b2)
     = ap_l p (putBuilder (append b1 b2))
 #-}

-- ap_r rules
{-# RULES

"ap_r/putBuilder" forall b1 b2.
       ap_r (putBuilder b1) (putBuilder b2)
     = putBuilder (append b1 b2)

"ap_r/putBuilder/assoc_r" forall b1 b2 (p :: Put a).
       ap_r (putBuilder b1) (ap_r (putBuilder b2) p)
     = ap_r (putBuilder (append b1 b2)) p

"ap_r/putBuilder/assoc_l" forall (p :: Put a) b1 b2.
       ap_r (ap_r p (putBuilder b1)) (putBuilder b2)
     = ap_r p (putBuilder (append b1 b2))

 #-}

-- combined ap_l/ap_r rules
{-# RULES

"ap_l/ap_r/putBuilder/assoc_r" forall b1 b2 (p :: Put a).
       ap_l (putBuilder b1) (ap_r (putBuilder b2) p)
     = ap_l (putBuilder (append b1 b2)) p

"ap_r/ap_l/putBuilder/assoc_r" forall b1 b2 (p :: Put a).
       ap_r (putBuilder b1) (ap_l (putBuilder b2) p)
     = ap_l (putBuilder (append b1 b2)) p

"ap_l/ap_r/putBuilder/assoc_l" forall (p :: Put a) b1 b2.
       ap_l (ap_r p (putBuilder b1)) (putBuilder b2)
     = ap_r p (putBuilder (append b1 b2))

"ap_r/ap_l/putBuilder/assoc_l" forall (p :: Put a) b1 b2.
       ap_r (ap_l p (putBuilder b1)) (putBuilder b2)
     = ap_r p (putBuilder (append b1 b2))

 #-}


-- Lifting IO actions
---------------------

{-
-- | Lift an 'IO' action to a 'Put' action.
{-# INLINE putLiftIO #-}
putLiftIO :: IO a -> Put a
putLiftIO io = put $ \k br -> io >>= (`k` br)
-}


------------------------------------------------------------------------------
-- Executing a Put directly on a buffered Handle
------------------------------------------------------------------------------

-- | Run a 'Put' action redirecting the produced output to a 'Handle'.
--
-- The output is buffered using the 'Handle's associated buffer. If this
-- buffer is too small to execute one step of the 'Put' action, then
-- it is replaced with a large enough buffer.
hPut :: forall a. Handle -> Put a -> IO a
#if __GLASGOW_HASKELL__ >= 611
hPut h p = do
    fillHandle 1 (runPut p)
  where
    fillHandle :: Int -> BuildStep a -> IO a
    fillHandle !minFree step = do
        next <- wantWritableHandle "hPut" h fillHandle_
        next
      where
        -- | We need to return an inner IO action that is executed outside
        -- the lock taken on the Handle for two reasons:
        --
        --   1. GHC.IO.Handle.Internals mentions in "Note [async]" that
        --      we should never do any side-effecting operations before
        --      an interuptible operation that may raise an async. exception
        --      as long as we are inside 'wantWritableHandle' and the like.
        --      We possibly run the interuptible 'flushWriteBuffer' right at
        --      the start of 'fillHandle', hence entering it a second time is
        --      not safe, as it could lead to a 'BuildStep' being run twice.
        --
        --      FIXME (SM): Adapt this function or at least its documentation,
        --      as it is OK to run a 'BuildStep' twice. We dropped this
        --      requirement in favor of being able to use
        --      'unsafeDupablePerformIO' and the speed improvement that it
        --      brings.
        --
        --   2. We use the 'S.hPut' function to also write to the handle.
        --      This function tries to take the same lock taken by
        --      'wantWritableHandle'. Therefore, we cannot call 'S.hPut'
        --      inside 'wantWritableHandle'.
        --
        fillHandle_ :: Handle__ -> IO (IO a)
        fillHandle_ h_ = do
            makeSpace  =<< readIORef refBuf
            fillBuffer =<< readIORef refBuf
          where
            refBuf        = haByteBuffer h_
            freeSpace buf = IO.bufSize buf - IO.bufR buf

            makeSpace buf
              | IO.bufSize buf < minFree = do
                  flushWriteBuffer h_
                  s <- IO.bufState <$> readIORef refBuf
                  IO.newByteBuffer minFree s >>= writeIORef refBuf

              | freeSpace buf < minFree = flushWriteBuffer h_
              | otherwise               =
#if __GLASGOW_HASKELL__ >= 613
                                          return ()
#else
                                          -- required for ghc-6.12
                                          flushWriteBuffer h_
#endif

            fillBuffer buf
              | freeSpace buf < minFree =
                  error $ unlines
                    [ "Data.ByteString.Builder.Internal.hPut: internal error."
                    , "  Not enough space after flush."
                    , "    required: " ++ show minFree
                    , "    free: "     ++ show (freeSpace buf)
                    ]
              | otherwise = do
                  let !br = BufferRange op (pBuf `plusPtr` IO.bufSize buf)
                  res <- fillWithBuildStep step doneH fullH insertChunkH br
                  touchForeignPtr fpBuf
                  return res
              where
                fpBuf = IO.bufRaw buf
                pBuf  = unsafeForeignPtrToPtr fpBuf
                op    = pBuf `plusPtr` IO.bufR buf

                {-# INLINE updateBufR #-}
                updateBufR op' = do
                    let !off' = op' `minusPtr` pBuf
                        !buf' = buf {IO.bufR = off'}
                    writeIORef refBuf buf'

                doneH op' x = do
                    updateBufR op'
                    -- We must flush if this Handle is set to NoBuffering.
                    -- If it is set to LineBuffering, be conservative and
                    -- flush anyway (we didn't check for newlines in the data).
                    -- Flushing must happen outside this 'wantWriteableHandle'
                    -- due to the possible async. exception.
                    case haBufferMode h_ of
                        BlockBuffering _      -> return $ return x
                        _line_or_no_buffering -> return $ hFlush h >> return x

                fullH op' minSize nextStep = do
                    updateBufR op'
                    return $ fillHandle minSize nextStep
                    -- 'fillHandle' will flush the buffer (provided there is
                    -- really less than 'minSize' space left) before executing
                    -- the 'nextStep'.

                insertChunkH op' bs nextStep = do
                    updateBufR op'
                    return $ do
                        S.hPut h bs
                        fillHandle 1 nextStep
#else
hPut h p =
    go =<< buildStepToCIOS strategy (runPut p)
  where
    strategy = untrimmedStrategy L.smallChunkSize L.defaultChunkSize

    go (Finished buf x) = S.hPut h (byteStringFromBuffer buf) >> return x
    go (Yield1 bs io)   = S.hPut h bs >> io >>= go
#endif

-- | Execute a 'Put' and return the computed result and the bytes
-- written during the computation as a lazy 'L.ByteString'.
--
-- This function is strict in the computed result and lazy in the writing of
-- the bytes. For example, given
--
-- @
--infinitePut = sequence_ (repeat (putBuilder (word8 1))) >> return 0
-- @
--
-- evaluating the expression
--
-- @
--fst $ putToLazyByteString infinitePut
-- @
--
-- does not terminate, while evaluating the expression
--
-- @
--L.head $ snd $ putToLazyByteString infinitePut
-- @
--
-- does terminate and yields the value @1 :: Word8@.
--
-- An illustrative example for these strictness properties is the
-- implementation of Base64 decoding (<http://en.wikipedia.org/wiki/Base64>).
--
-- @
--type DecodingState = ...
--
--decodeBase64 :: 'S.ByteString' -> DecodingState -> 'Put' (Maybe DecodingState)
--decodeBase64 = ...
-- @
--
-- The above function takes a strict 'S.ByteString' supposed to represent
-- Base64 encoded data and the current decoding state.
-- It writes the decoded bytes as the side-effect of the 'Put' and returns the
-- new decoding state, if the decoding of all data in the 'S.ByteString' was
-- successful. The checking if the strict 'S.ByteString' represents Base64
-- encoded data and the actual decoding are fused. This makes the common case,
-- where all data represents Base64 encoded data, more efficient. It also
-- implies that all data must be decoded before the final decoding
-- state can be returned. 'Put's are intended for implementing such fused
-- checking and decoding/encoding, which is reflected in their strictness
-- properties.
{-# NOINLINE putToLazyByteString #-}
putToLazyByteString
    :: Put a              -- ^ 'Put' to execute
    -> (a, L.ByteString)  -- ^ Result and lazy 'L.ByteString'
                          -- written as its side-effect
putToLazyByteString = putToLazyByteStringWith
    (safeStrategy L.smallChunkSize L.defaultChunkSize) (\x -> (x, L.Empty))


-- | Execute a 'Put' with a buffer-allocation strategy and a continuation. For
-- example, 'putToLazyByteString' is implemented as follows.
--
-- @
--putToLazyByteString = 'putToLazyByteStringWith'
--    ('safeStrategy' 'L.smallChunkSize' 'L.defaultChunkSize') (\x -> (x, L.empty))
-- @
--
{-# INLINE putToLazyByteStringWith #-}
putToLazyByteStringWith
    :: AllocationStrategy
       -- ^ Buffer allocation strategy to use
    -> (a -> (b, L.ByteString))
       -- ^ Continuation to use for computing the final result and the tail of
       -- its side-effect (the written bytes).
    -> Put a
       -- ^ 'Put' to execute
    -> (b, L.ByteString)
       -- ^ Resulting lazy 'L.ByteString'
putToLazyByteStringWith strategy k p =
    ciosToLazyByteString strategy k $ unsafeDupablePerformIO $
        buildStepToCIOS strategy (runPut p)



------------------------------------------------------------------------------
-- ByteString insertion / controlling chunk boundaries
------------------------------------------------------------------------------

-- Raw memory
-------------

-- | Ensure that there are at least 'n' free bytes for the following 'Builder'.
{-# INLINE ensureFree #-}
ensureFree :: Int -> Builder
ensureFree minFree =
    builder step
  where
    step k br@(BufferRange op ope)
      | ope `minusPtr` op < minFree = return $ bufferFull minFree op k
      | otherwise                   = k br

-- | Copy the bytes from a 'BufferRange' into the output stream.
wrappedBytesCopyStep :: BufferRange  -- ^ Input 'BufferRange'.
                     -> BuildStep a -> BuildStep a
wrappedBytesCopyStep !(BufferRange ip0 ipe) k =
    go ip0
  where
    go !ip !(BufferRange op ope)
      | inpRemaining <= outRemaining = do
          copyBytes op ip inpRemaining
          let !br' = BufferRange (op `plusPtr` inpRemaining) ope
          k br'
      | otherwise = do
          copyBytes op ip outRemaining
          let !ip' = ip `plusPtr` outRemaining
          return $ bufferFull 1 ope (go ip')
      where
        outRemaining = ope `minusPtr` op
        inpRemaining = ipe `minusPtr` ip


-- Strict ByteStrings
------------------------------------------------------------------------------


-- | Construct a 'Builder' that copies the strict 'S.ByteString's, if it is
-- smaller than the treshold, and inserts it directly otherwise.
--
-- For example, @byteStringThreshold 1024@ copies strict 'S.ByteString's whose size
-- is less or equal to 1kb, and inserts them directly otherwise. This implies
-- that the average chunk-size of the generated lazy 'L.ByteString' may be as
-- low as 513 bytes, as there could always be just a single byte between the
-- directly inserted 1025 byte, strict 'S.ByteString's.
--
{-# INLINE byteStringThreshold #-}
byteStringThreshold :: Int -> S.ByteString -> Builder
byteStringThreshold maxCopySize =
    \bs -> builder $ step bs
  where
    step !bs@(S.PS _ _ len) !k br@(BufferRange !op _)
      | len <= maxCopySize = byteStringCopyStep bs k br
      | otherwise          = return $ insertChunk op bs k

-- | Construct a 'Builder' that copies the strict 'S.ByteString'.
--
-- Use this function to create 'Builder's from smallish (@<= 4kb@)
-- 'S.ByteString's or if you need to guarantee that the 'S.ByteString' is not
-- shared with the chunks generated by the 'Builder'.
--
{-# INLINE byteStringCopy #-}
byteStringCopy :: S.ByteString -> Builder
byteStringCopy = \bs -> builder $ byteStringCopyStep bs

{-# INLINE byteStringCopyStep #-}
byteStringCopyStep :: S.ByteString -> BuildStep a -> BuildStep a
byteStringCopyStep (S.PS ifp ioff isize) !k0 br0@(BufferRange op ope)
    -- Ensure that the common case is not recursive and therefore yields
    -- better code.
    | op' <= ope = do copyBytes op ip isize
                      touchForeignPtr ifp
                      k0 (BufferRange op' ope)
    | otherwise  = do wrappedBytesCopyStep (BufferRange ip ipe) k br0
  where
    op'  = op `plusPtr` isize
    ip   = unsafeForeignPtrToPtr ifp `plusPtr` ioff
    ipe  = ip `plusPtr` isize
    k br = do touchForeignPtr ifp  -- input consumed: OK to release here
              k0 br

-- | Construct a 'Builder' that always inserts the strict 'S.ByteString'
-- directly as a chunk.
--
-- This implies flushing the output buffer, even if it contains just
-- a single byte. You should therefore use 'byteStringInsert' only for large
-- (@> 8kb@) 'S.ByteString's. Otherwise, the generated chunks are too
-- fragmented to be processed efficiently afterwards.
--
{-# INLINE byteStringInsert #-}
byteStringInsert :: S.ByteString -> Builder
byteStringInsert =
    \bs -> builder $ \k (BufferRange op _) -> return $ insertChunk op bs k

-- Short bytestrings
------------------------------------------------------------------------------

-- | Construct a 'Builder' that copies the 'SH.ShortByteString'.
--
{-# INLINE shortByteString #-}
shortByteString :: Sh.ShortByteString -> Builder
shortByteString = \sbs -> builder $ shortByteStringCopyStep sbs

-- | Copy the bytes from a 'SH.ShortByteString' into the output stream.
{-# INLINE shortByteStringCopyStep #-}
shortByteStringCopyStep :: Sh.ShortByteString  -- ^ Input 'SH.ShortByteString'.
                        -> BuildStep a -> BuildStep a
shortByteStringCopyStep !sbs k =
    go 0 (Sh.length sbs)
  where
    go !ip !ipe !(BufferRange op ope)
      | inpRemaining <= outRemaining = do
          Sh.copyToPtr sbs ip op inpRemaining
          let !br' = BufferRange (op `plusPtr` inpRemaining) ope
          k br'
      | otherwise = do
          Sh.copyToPtr sbs ip op outRemaining
          let !ip' = ip + outRemaining
          return $ bufferFull 1 ope (go ip' ipe)
      where
        outRemaining = ope `minusPtr` op
        inpRemaining = ipe - ip


-- Lazy bytestrings
------------------------------------------------------------------------------

-- | Construct a 'Builder' that uses the thresholding strategy of 'byteStringThreshold'
-- for each chunk of the lazy 'L.ByteString'.
--
{-# INLINE lazyByteStringThreshold #-}
lazyByteStringThreshold :: Int -> L.ByteString -> Builder
lazyByteStringThreshold maxCopySize =
    L.foldrChunks (\bs b -> byteStringThreshold maxCopySize bs `mappend` b) mempty
    -- TODO: We could do better here. Currently, Large, Small, Large, leads to
    -- an unnecessary copy of the 'Small' chunk.

-- | Construct a 'Builder' that copies the lazy 'L.ByteString'.
--
{-# INLINE lazyByteStringCopy #-}
lazyByteStringCopy :: L.ByteString -> Builder
lazyByteStringCopy =
    L.foldrChunks (\bs b -> byteStringCopy bs `mappend` b) mempty

-- | Construct a 'Builder' that inserts all chunks of the lazy 'L.ByteString'
-- directly.
--
{-# INLINE lazyByteStringInsert #-}
lazyByteStringInsert :: L.ByteString -> Builder
lazyByteStringInsert =
    L.foldrChunks (\bs b -> byteStringInsert bs `mappend` b) mempty

-- | Create a 'Builder' denoting the same sequence of bytes as a strict
-- 'S.ByteString'.
-- The 'Builder' inserts large 'S.ByteString's directly, but copies small ones
-- to ensure that the generated chunks are large on average.
--
{-# INLINE byteString #-}
byteString :: S.ByteString -> Builder
byteString = byteStringThreshold maximalCopySize

-- | Create a 'Builder' denoting the same sequence of bytes as a lazy
-- 'S.ByteString'.
-- The 'Builder' inserts large chunks of the lazy 'L.ByteString' directly,
-- but copies small ones to ensure that the generated chunks are large on
-- average.
--
{-# INLINE lazyByteString #-}
lazyByteString :: L.ByteString -> Builder
lazyByteString = lazyByteStringThreshold maximalCopySize
-- FIXME: also insert the small chunk for [large,small,large] directly.
-- Perhaps it makes even sense to concatenate the small chunks in
-- [large,small,small,small,large] and insert them directly afterwards to avoid
-- unnecessary buffer spilling. Hmm, but that uncontrollably increases latency
-- => no good!

-- | The maximal size of a 'S.ByteString' that is copied.
-- @2 * 'L.smallChunkSize'@ to guarantee that on average a chunk is of
-- 'L.smallChunkSize'.
maximalCopySize :: Int
maximalCopySize = 2 * L.smallChunkSize

------------------------------------------------------------------------------
-- Builder execution
------------------------------------------------------------------------------

-- | A buffer allocation strategy for executing 'Builder's.

-- The strategy
--
-- > 'AllocationStrategy' firstBufSize bufSize trim
--
-- states that the first buffer is of size @firstBufSize@, all following buffers
-- are of size @bufSize@, and a buffer of size @n@ filled with @k@ bytes should
-- be trimmed iff @trim k n@ is 'True'.
data AllocationStrategy = AllocationStrategy
         (Maybe (Buffer, Int) -> IO Buffer)
         {-# UNPACK #-} !Int
         (Int -> Int -> Bool)

-- | Create a custom allocation strategy. See the code for 'safeStrategy' and
-- 'untrimmedStrategy' for examples.
{-# INLINE customStrategy #-}
customStrategy
  :: (Maybe (Buffer, Int) -> IO Buffer)
     -- ^ Buffer allocation function. If 'Nothing' is given, then a new first
     -- buffer should be allocated. If @'Just' (oldBuf, minSize)@ is given,
     -- then a buffer with minimal size 'minSize' must be returned. The
     -- strategy may reuse the 'oldBuffer', if it can guarantee that this
     -- referentially transparent and 'oldBuffer' is large enough.
  -> Int
     -- ^ Default buffer size.
  -> (Int -> Int -> Bool)
     -- ^ A predicate @trim used allocated@ returning 'True', if the buffer
     -- should be trimmed before it is returned.
  -> AllocationStrategy
customStrategy = AllocationStrategy

-- | Sanitize a buffer size; i.e., make it at least the size of an 'Int'.
{-# INLINE sanitize #-}
sanitize :: Int -> Int
sanitize = max (sizeOf (undefined :: Int))

-- | Use this strategy for generating lazy 'L.ByteString's whose chunks are
-- discarded right after they are generated. For example, if you just generate
-- them to write them to a network socket.
{-# INLINE untrimmedStrategy #-}
untrimmedStrategy :: Int -- ^ Size of the first buffer
                  -> Int -- ^ Size of successive buffers
                  -> AllocationStrategy
                  -- ^ An allocation strategy that does not trim any of the
                  -- filled buffers before converting it to a chunk
untrimmedStrategy firstSize bufSize =
    AllocationStrategy nextBuffer (sanitize bufSize) (\_ _ -> False)
  where
    {-# INLINE nextBuffer #-}
    nextBuffer Nothing             = newBuffer $ sanitize firstSize
    nextBuffer (Just (_, minSize)) = newBuffer minSize


-- | Use this strategy for generating lazy 'L.ByteString's whose chunks are
-- likely to survive one garbage collection. This strategy trims buffers
-- that are filled less than half in order to avoid spilling too much memory.
{-# INLINE safeStrategy #-}
safeStrategy :: Int  -- ^ Size of first buffer
             -> Int  -- ^ Size of successive buffers
             -> AllocationStrategy
             -- ^ An allocation strategy that guarantees that at least half
             -- of the allocated memory is used for live data
safeStrategy firstSize bufSize =
    AllocationStrategy nextBuffer (sanitize bufSize) trim
  where
    trim used size                 = 2 * used < size
    {-# INLINE nextBuffer #-}
    nextBuffer Nothing             = newBuffer $ sanitize firstSize
    nextBuffer (Just (_, minSize)) = newBuffer minSize

-- | /Heavy inlining./ Execute a 'Builder' with custom execution parameters.
--
-- This function is inlined despite its heavy code-size to allow fusing with
-- the allocation strategy. For example, the default 'Builder' execution
-- function 'toLazyByteString' is defined as follows.
--
-- @
-- {-\# NOINLINE toLazyByteString \#-}
-- toLazyByteString =
--   toLazyByteStringWith ('safeStrategy' 'L.smallChunkSize' 'L.defaultChunkSize') L.empty
-- @
--
-- where @L.empty@ is the zero-length lazy 'L.ByteString'.
--
-- In most cases, the parameters used by 'toLazyByteString' give good
-- performance. A sub-performing case of 'toLazyByteString' is executing short
-- (<128 bytes) 'Builder's. In this case, the allocation overhead for the first
-- 4kb buffer and the trimming cost dominate the cost of executing the
-- 'Builder'. You can avoid this problem using
--
-- >toLazyByteStringWith (safeStrategy 128 smallChunkSize) L.empty
--
-- This reduces the allocation and trimming overhead, as all generated
-- 'L.ByteString's fit into the first buffer and there is no trimming
-- required, if more than 64 bytes and less than 128 bytes are written.
--
{-# INLINE toLazyByteStringWith #-}
toLazyByteStringWith
    :: AllocationStrategy
       -- ^ Buffer allocation strategy to use
    -> L.ByteString
       -- ^ Lazy 'L.ByteString' to use as the tail of the generated lazy
       -- 'L.ByteString'
    -> Builder
       -- ^ 'Builder' to execute
    -> L.ByteString
       -- ^ Resulting lazy 'L.ByteString'
toLazyByteStringWith strategy k b =
    ciosUnitToLazyByteString strategy k $ unsafeDupablePerformIO $
        buildStepToCIOS strategy (runBuilder b)

-- | Convert a 'BuildStep' to a 'ChunkIOStream' stream by executing it on
-- 'Buffer's allocated according to the given 'AllocationStrategy'.
{-# INLINE buildStepToCIOS #-}
buildStepToCIOS
    :: AllocationStrategy          -- ^ Buffer allocation strategy to use
    -> BuildStep a                 -- ^ 'BuildStep' to execute
    -> IO (ChunkIOStream a)
buildStepToCIOS !(AllocationStrategy nextBuffer bufSize trim) =
    \step -> nextBuffer Nothing >>= fill step
  where
    fill !step !buf@(Buffer fpbuf br@(BufferRange _ pe)) = do
        res <- fillWithBuildStep step doneH fullH insertChunkH br
        touchForeignPtr fpbuf
        return res
      where
        pbuf = unsafeForeignPtrToPtr fpbuf

        doneH op' x = return $
            Finished (Buffer fpbuf (BufferRange op' pe)) x

        fullH op' minSize nextStep =
            wrapChunk op' $ const $
                nextBuffer (Just (buf, max minSize bufSize)) >>= fill nextStep

        insertChunkH op' bs nextStep =
            wrapChunk op' $ \isEmpty -> yield1 bs $
                -- Checking for empty case avoids allocating 'n-1' empty
                -- buffers for 'n' insertChunkH right after each other.
                if isEmpty
                  then fill nextStep buf
                  else do buf' <- nextBuffer (Just (buf, bufSize))
                          fill nextStep buf'

        -- Wrap and yield a chunk, trimming it if necesary
        {-# INLINE wrapChunk #-}
        wrapChunk !op' mkCIOS
          | chunkSize == 0      = mkCIOS True
          | trim chunkSize size = do
              bs <- S.create chunkSize $ \pbuf' ->
                        copyBytes pbuf' pbuf chunkSize
              -- FIXME: We could reuse the trimmed buffer here.
              return $ Yield1 bs (mkCIOS False)
          | otherwise            =
              return $ Yield1 (S.PS fpbuf 0 chunkSize) (mkCIOS False)
          where
            chunkSize = op' `minusPtr` pbuf
            size      = pe  `minusPtr` pbuf
