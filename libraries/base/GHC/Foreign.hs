{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Foreign
-- Copyright   :  (c) The University of Glasgow, 2008-2011
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Foreign marshalling support for CStrings with configurable encodings
--
-----------------------------------------------------------------------------

module GHC.Foreign (
    -- * C strings with a configurable encoding

    -- conversion of C strings into Haskell strings
    --
    peekCString,
    peekCStringLen,

    -- conversion of Haskell strings into C strings
    --
    newCString,
    newCStringLen,

    -- conversion of Haskell strings into C strings using temporary storage
    --
    withCString,
    withCStringLen,
    withCStringsLen,

    charIsRepresentable,
  ) where

import Foreign.Marshal.Array
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import Data.Word

-- Imports for the locale-encoding version of marshallers

import Data.Tuple (fst)

import GHC.Show ( show )

import Foreign.Marshal.Alloc
import Foreign.ForeignPtr

import GHC.Debug
import GHC.List
import GHC.Num
import GHC.Base

import GHC.IO
import GHC.IO.Exception
import GHC.IO.Buffer
import GHC.IO.Encoding.Types


c_DEBUG_DUMP :: Bool
c_DEBUG_DUMP = False

putDebugMsg :: String -> IO ()
putDebugMsg | c_DEBUG_DUMP = debugLn
            | otherwise    = const (return ())


-- These definitions are identical to those in Foreign.C.String, but copied in here to avoid a cycle:
type CString    = Ptr CChar
type CStringLen = (Ptr CChar, Int)

-- exported functions
-- ------------------

-- | Marshal a NUL terminated C string into a Haskell string.
--
peekCString    :: TextEncoding -> CString -> IO String
peekCString enc cp = do
    sz <- lengthArray0 nUL cp
    peekEncodedCString enc (cp, sz * cCharSize)

-- | Marshal a C string with explicit length into a Haskell string.
--
peekCStringLen           :: TextEncoding -> CStringLen -> IO String
peekCStringLen = peekEncodedCString

-- | Marshal a Haskell string into a NUL terminated C string.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * new storage is allocated for the C string and must be
--   explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newCString :: TextEncoding -> String -> IO CString
newCString enc = liftM fst . newEncodedCString enc True

-- | Marshal a Haskell string into a C string (ie, character array) with
-- explicit length information.
--
-- * new storage is allocated for the C string and must be
--   explicitly freed using 'Foreign.Marshal.Alloc.free' or
--   'Foreign.Marshal.Alloc.finalizerFree'.
--
newCStringLen     :: TextEncoding -> String -> IO CStringLen
newCStringLen enc = newEncodedCString enc False

-- | Marshal a Haskell string into a NUL terminated C string using temporary
-- storage.
--
-- * the Haskell string may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withCString :: TextEncoding -> String -> (CString -> IO a) -> IO a
withCString enc s act = withEncodedCString enc True s $ \(cp, _sz) -> act cp

-- | Marshal a Haskell string into a C string (ie, character array)
-- in temporary storage, with explicit length information.
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withCStringLen         :: TextEncoding -> String -> (CStringLen -> IO a) -> IO a
withCStringLen enc = withEncodedCString enc False

-- | Marshal a list of Haskell strings into an array of NUL terminated C strings
-- using temporary storage.
--
-- * the Haskell strings may /not/ contain any NUL characters
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withCStringsLen :: TextEncoding
                -> [String]
                -> (Int -> Ptr CString -> IO a)
                -> IO a
withCStringsLen enc strs f = go [] strs
  where
  go cs (s:ss) = withCString enc s $ \c -> go (c:cs) ss
  go cs [] = withArrayLen (reverse cs) f

-- | Determines whether a character can be accurately encoded in a
-- 'Foreign.C.String.CString'.
--
-- Pretty much anyone who uses this function is in a state of sin because
-- whether or not a character is encodable will, in general, depend on the
-- context in which it occurs.
charIsRepresentable :: TextEncoding -> Char -> IO Bool
-- We force enc explicitly because `catch` is lazy in its
-- first argument. We would probably like to force c as well,
-- but unfortunately worker/wrapper produces very bad code for
-- that.
--
-- TODO If this function is performance-critical, it would probably
-- pay to use a single-character specialization of withCString. That
-- would allow worker/wrapper to actually eliminate Char boxes, and
-- would also get rid of the completely unnecessary cons allocation.
charIsRepresentable !enc c =
  withCString enc [c]
              (\cstr -> do str <- peekCString enc cstr
                           case str of
                             [ch] | ch == c -> pure True
                             _ -> pure False)
    `catch`
       \(_ :: IOException) -> pure False

-- auxiliary definitions
-- ----------------------

-- C's end of string character
nUL :: CChar
nUL  = 0

-- Size of a CChar in bytes
cCharSize :: Int
cCharSize = sizeOf (undefined :: CChar)


{-# INLINE peekEncodedCString #-}
peekEncodedCString :: TextEncoding -- ^ Encoding of CString
                   -> CStringLen
                   -> IO String    -- ^ String in Haskell terms
peekEncodedCString (TextEncoding { mkTextDecoder = mk_decoder }) (p, sz_bytes)
  = bracket mk_decoder close $ \decoder -> do
      let chunk_size = sz_bytes `max` 1 -- Decode buffer chunk size in characters: one iteration only for ASCII
      from0 <- fmap (\fp -> bufferAdd sz_bytes (emptyBuffer fp sz_bytes ReadBuffer)) $ newForeignPtr_ (castPtr p)
      to <- newCharBuffer chunk_size WriteBuffer

      let go !iteration from = do
            (why, from', to') <- encode decoder from to
            if isEmptyBuffer from'
             then
              -- No input remaining: @why@ will be InputUnderflow, but we don't care
              withBuffer to' $ peekArray (bufferElems to')
             else do
              -- Input remaining: what went wrong?
              putDebugMsg ("peekEncodedCString: " ++ show iteration ++ " " ++ show why)
              (from'', to'') <- case why of InvalidSequence -> recover decoder from' to' -- These conditions are equally bad because
                                            InputUnderflow  -> recover decoder from' to' -- they indicate malformed/truncated input
                                            OutputUnderflow -> return (from', to')       -- We will have more space next time round
              putDebugMsg ("peekEncodedCString: from " ++ summaryBuffer from ++ " " ++ summaryBuffer from' ++ " " ++ summaryBuffer from'')
              putDebugMsg ("peekEncodedCString: to " ++ summaryBuffer to ++ " " ++ summaryBuffer to' ++ " " ++ summaryBuffer to'')
              to_chars <- withBuffer to'' $ peekArray (bufferElems to'')
              fmap (to_chars++) $ go (iteration + 1) from''

      go (0 :: Int) from0

{-# INLINE withEncodedCString #-}
withEncodedCString :: TextEncoding         -- ^ Encoding of CString to create
                   -> Bool                 -- ^ Null-terminate?
                   -> String               -- ^ String to encode
                   -> (CStringLen -> IO a) -- ^ Worker that can safely use the allocated memory
                   -> IO a
withEncodedCString (TextEncoding { mkTextEncoder = mk_encoder }) null_terminate s act
  = bracket mk_encoder close $ \encoder -> withArrayLen s $ \sz p -> do
      from <- fmap (\fp -> bufferAdd sz (emptyBuffer fp sz ReadBuffer)) $ newForeignPtr_ p

      let go !iteration to_sz_bytes = do
           putDebugMsg ("withEncodedCString: " ++ show iteration)
           allocaBytes to_sz_bytes $ \to_p -> do
            -- See Note [Check *before* fill in withEncodedCString] about why
            -- this is subtle.
            mb_res <- tryFillBuffer encoder null_terminate from to_p to_sz_bytes
            case mb_res of
              Nothing  -> go (iteration + 1) (to_sz_bytes * 2)
              Just to_buf -> withCStringBuffer to_buf null_terminate act

      -- If the input string is ASCII, this value will ensure we only allocate once
      go (0 :: Int) (cCharSize * (sz + 1))

withCStringBuffer :: Buffer Word8 -> Bool -> (CStringLen -> IO r) -> IO r
withCStringBuffer to_buf null_terminate act = do
  let bytes = bufferElems to_buf
  withBuffer to_buf $ \to_ptr -> do
    when null_terminate $ pokeElemOff to_ptr (bufR to_buf) 0
    act (castPtr to_ptr, bytes) -- NB: the length information is specified as being in *bytes*

{-# INLINE newEncodedCString #-}
newEncodedCString :: TextEncoding  -- ^ Encoding of CString to create
                  -> Bool          -- ^ Null-terminate?
                  -> String        -- ^ String to encode
                  -> IO CStringLen
newEncodedCString (TextEncoding { mkTextEncoder = mk_encoder }) null_terminate s
  = bracket mk_encoder close $ \encoder -> withArrayLen s $ \sz p -> do
      from <- fmap (\fp -> bufferAdd sz (emptyBuffer fp sz ReadBuffer)) $ newForeignPtr_ p

      let go !iteration to_p to_sz_bytes = do
           putDebugMsg ("newEncodedCString: " ++ show iteration)
           mb_res <- tryFillBuffer encoder null_terminate from to_p to_sz_bytes
           case mb_res of
             Nothing  -> do
                 let to_sz_bytes' = to_sz_bytes * 2
                 to_p' <- reallocBytes to_p to_sz_bytes'
                 go (iteration + 1) to_p' to_sz_bytes'
             Just to_buf -> withCStringBuffer to_buf null_terminate return

      -- If the input string is ASCII, this value will ensure we only allocate once
      let to_sz_bytes = cCharSize * (sz + 1)
      to_p <- mallocBytes to_sz_bytes
      go (0 :: Int) to_p to_sz_bytes


tryFillBuffer :: TextEncoder dstate -> Bool -> Buffer Char -> Ptr Word8 -> Int
                    ->  IO (Maybe (Buffer Word8))
tryFillBuffer encoder null_terminate from0 to_p to_sz_bytes = do
    to_fp <- newForeignPtr_ to_p
    go (0 :: Int) (from0, emptyBuffer to_fp to_sz_bytes WriteBuffer)
  where
    go !iteration (from, to) = do
      (why, from', to') <- encode encoder from to
      putDebugMsg ("tryFillBufferAndCall: " ++ show iteration ++ " " ++ show why ++ " " ++ summaryBuffer from ++ " " ++ summaryBuffer from')
      if isEmptyBuffer from'
       then if null_terminate && bufferAvailable to' == 0
             then return Nothing -- We had enough for the string but not the terminator: ask the caller for more buffer
             else return (Just to')
       else case why of -- We didn't consume all of the input
              InputUnderflow  -> recover encoder from' to' >>= go (iteration + 1) -- These conditions are equally bad
              InvalidSequence -> recover encoder from' to' >>= go (iteration + 1) -- since the input was truncated/invalid
              OutputUnderflow -> return Nothing -- Oops, out of buffer during decoding: ask the caller for more
{-
Note [Check *before* fill in withEncodedCString]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very important that the size check and readjustment peformed by tryFillBuffer
happens before the continuation is called. The size check is the part which can
fail, the call to the continuation never fails and so the caller should respond
first to the size check failing and *then* call the continuation. Making this evident
to the compiler avoids historic space leaks.

In a previous interation of this code we had a pattern that, somewhat simplified,
looked like this:

go :: State -> (State -> IO a) -> IO a
go state action =
    case tryFillBufferAndCall state action of
        Left state' -> go state' action
        Right result -> result

`tryFillBufferAndCall` performed some checks, and then we either called action,
or we modified the state and tried again.
This went wrong because `action` can be a function closure containing a reference to
a lazy data structure. If we call action directly, without retaining any references
to action, that is fine. The data structure is consumed as it is produced and we operate
in constant space.

However the failure branch `go state' action` *does* capture a reference to action.
This went wrong because the reference to action in the failure branch only becomes
unreachable *after* action returns. This means we keep alive the function closure
for `action` until `action` returns. Which in turn keeps alive the *whole* lazy list
via `action` until the action has fully run.
This went wrong in #20107, where the continuation kept an entire lazy bytestring alive
rather than allowing it to be incrementaly consumed and collected.
-}

