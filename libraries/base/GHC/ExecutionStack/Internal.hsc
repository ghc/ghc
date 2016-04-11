-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ExecutionStack.Internal
-- Copyright   :  (c) The University of Glasgow 2013-2015
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Internals of the `GHC.ExecutionStack` module
--
-- @since 4.9.0.0
-----------------------------------------------------------------------------

#include "HsFFI.h"
#include "HsBaseConfig.h"
#include "rts/Libdw.h"

{-# LANGUAGE MultiWayIf #-}

module GHC.ExecutionStack.Internal (
  -- * Internal
    Location (..)
  , SrcLoc (..)
  , StackTrace
  , stackFrames
  , stackDepth
  , collectStackTrace
  , showStackFrames
  , invalidateDebugCache
  ) where

import Control.Monad (join)
import Data.Word
import Foreign.C.Types
import Foreign.C.String (peekCString, CString)
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr, FunPtr)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)

-- N.B. See includes/rts/Libdw.h for notes on stack representation.

-- | A location in the original program source.
data SrcLoc = SrcLoc { sourceFile   :: String
                     , sourceLine   :: Int
                     , sourceColumn :: Int
                     }

-- | Location information about an addresss from a backtrace.
data Location = Location { objectName   :: String
                         , functionName :: String
                         , srcLoc       :: Maybe SrcLoc
                         }

-- | A chunk of backtrace frames
data Chunk = Chunk { chunkFrames     :: !Word
                   , chunkNext       :: !(Ptr Chunk)
                   , chunkFirstFrame :: !(Ptr Addr)
                   }

-- | The state of the execution stack
newtype StackTrace = StackTrace (ForeignPtr StackTrace)

-- | An address
type Addr = Ptr ()

withSession :: (ForeignPtr Session -> IO a) -> IO (Maybe a)
withSession action = do
    ptr <- libdw_pool_take
    if | nullPtr == ptr -> return Nothing
       | otherwise      -> do
           fptr <- newForeignPtr libdw_pool_release ptr
           ret <- action fptr
           return $ Just ret

-- | How many stack frames in the given 'StackTrace'
stackDepth :: StackTrace -> Int
stackDepth (StackTrace fptr) =
    unsafePerformIO $ withForeignPtr fptr $ \ptr ->
        fromIntegral . asWord <$> (#peek Backtrace,n_frames) ptr
  where
    asWord = id :: Word -> Word

peekChunk :: Ptr Chunk -> IO Chunk
peekChunk ptr =
    Chunk <$> (#peek BacktraceChunk,n_frames) ptr
          <*> (#peek BacktraceChunk,next) ptr
          <*> pure (castPtr $ (#ptr BacktraceChunk,frames) ptr)

-- | Return a list of the chunks of a backtrace, from the outer-most to
-- inner-most chunk.
chunksList :: StackTrace -> IO [Chunk]
chunksList (StackTrace fptr) = withForeignPtr fptr $ \ptr ->
    go [] =<< (#peek Backtrace,last) ptr
  where
    go accum ptr
      | ptr == nullPtr = return accum
      | otherwise = do
            chunk <- peekChunk ptr
            go (chunk : accum) (chunkNext chunk)

-- | Unpack the given 'Location' in the Haskell representation
peekLocation :: Ptr Location -> IO Location
peekLocation ptr = do
    let peekCStringPtr :: CString -> IO String
        peekCStringPtr p
          | p /= nullPtr = peekCString $ castPtr p
          | otherwise    = return ""
    objFile <- peekCStringPtr =<< (#peek Location,object_file) ptr
    function <- peekCStringPtr =<< (#peek Location,function) ptr
    srcFile <- peekCStringPtr =<< (#peek Location,source_file) ptr
    lineNo <- (#peek Location,lineno) ptr :: IO Word32
    colNo <- (#peek Location,colno) ptr :: IO Word32
    let _srcLoc
          | null srcFile = Nothing
          | otherwise = Just $ SrcLoc { sourceFile = srcFile
                                      , sourceLine = fromIntegral lineNo
                                      , sourceColumn = fromIntegral colNo
                                      }
    return Location { objectName = objFile
                    , functionName = function
                    , srcLoc = _srcLoc
                    }

-- | The size in bytes of a 'locationSize'
locationSize :: Int
locationSize = (#const sizeof(Location))

-- | List the frames of a stack trace.
stackFrames :: StackTrace -> Maybe [Location]
stackFrames st@(StackTrace fptr) = unsafePerformIO $ withSession $ \sess -> do
    chunks <- chunksList st
    go sess (reverse chunks)
  where
    go :: ForeignPtr Session -> [Chunk] -> IO [Location]
    go _ [] = return []
    go sess (chunk : chunks) = do
        this <- iterChunk sess chunk
        rest <- unsafeInterleaveIO (go sess chunks)
        return (this ++ rest)

    {-
    Here we lazily lookup the location information associated with each address
    as this can be rather costly. This does mean, however, that if the set of
    loaded modules changes between the time that we capture the stack and the
    time we reach here, we may end up with nonsense (mostly likely merely
    unknown symbols). I think this is a reasonable price to pay, however, as
    module loading/unloading is a rather rare event.

    Morover, we stand to gain a great deal by lazy lookups as the stack frames
    may never even be requested, meaning the only effort wasted is the
    collection of the stack frames themselves.

    The only slightly tricky thing here is to ensure that the ForeignPtr
    stays alive until we reach the end.
    -}
    iterChunk :: ForeignPtr Session -> Chunk -> IO [Location]
    iterChunk sess chunk = iterFrames (chunkFrames chunk) (chunkFirstFrame chunk)
      where
        iterFrames :: Word -> Ptr Addr -> IO [Location]
        iterFrames 0 _ = return []
        iterFrames n frame = do
            pc <- peek frame :: IO Addr
            mframe <- lookupFrame pc
            rest <- unsafeInterleaveIO (iterFrames (n-1) frame')
            return $ maybe rest (:rest) mframe
          where
            frame' = frame `plusPtr` sizeOf (undefined :: Addr)

        lookupFrame :: Addr -> IO (Maybe Location)
        lookupFrame pc = withForeignPtr fptr $ const $ do
            allocaBytes locationSize $ \buf -> do
                ret <- withForeignPtr sess $ \sessPtr -> libdw_lookup_location sessPtr buf pc
                case ret of
                  0 -> Just <$> peekLocation buf
                  _ -> return Nothing

-- | A LibdwSession from the runtime system
data Session

foreign import ccall unsafe "libdwPoolTake"
    libdw_pool_take :: IO (Ptr Session)

foreign import ccall unsafe "&libdwPoolRelease"
    libdw_pool_release :: FunPtr (Ptr Session -> IO ())

foreign import ccall unsafe "libdwPoolClear"
    libdw_pool_clear :: IO ()

foreign import ccall unsafe "libdwLookupLocation"
    libdw_lookup_location :: Ptr Session -> Ptr Location -> Addr -> IO CInt

foreign import ccall unsafe "libdwGetBacktrace"
    libdw_get_backtrace :: Ptr Session -> IO (Ptr StackTrace)

foreign import ccall unsafe "&backtraceFree"
    backtrace_free :: FunPtr (Ptr StackTrace -> IO ())

-- | Get an execution stack.
collectStackTrace :: IO (Maybe StackTrace)
collectStackTrace = fmap join $ withSession $ \sess -> do
    st <- withForeignPtr sess libdw_get_backtrace
    if | st == nullPtr -> return Nothing
       | otherwise     -> Just . StackTrace <$> newForeignPtr backtrace_free st

-- | Free the cached debug data.
invalidateDebugCache :: IO ()
invalidateDebugCache = libdw_pool_clear

-- | Render a stacktrace as a string
showStackFrames :: [Location] -> ShowS
showStackFrames frames =
    showString "Stack trace:\n"
    . foldr (.) id (map showFrame frames)
  where
    showFrame loc =
      showString "    " . showLocation loc . showChar '\n'

-- | Render a 'Location' as a string
showLocation :: Location -> ShowS
showLocation loc =
        showString (functionName loc)
      . maybe id showSrcLoc (srcLoc loc)
      . showString " in "
      . showString (objectName loc)
  where
    showSrcLoc :: SrcLoc -> ShowS
    showSrcLoc sloc =
        showString " ("
      . showString (sourceFile sloc)
      . showString ":"
      . shows (sourceLine sloc)
      . showString "."
      . shows (sourceColumn sloc)
      . showString ")"
