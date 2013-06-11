{-# LANGUAGE ForeignFunctionInterface, NoImplicitPrelude, NondecreasingIndentation, RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module GHC.IO.Encoding.CodePage.API (
    mkCodePageEncoding
  ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad
import Data.Bits
import Data.Either
import Data.Word

import GHC.Base
import GHC.List
import GHC.IO.Buffer
import GHC.IO.Encoding.Failure
import GHC.IO.Encoding.Types
import GHC.IO.Encoding.UTF16
import GHC.Num
import GHC.Show
import GHC.Real
import GHC.Windows
import GHC.ForeignPtr (castForeignPtr)

import System.Posix.Internals


c_DEBUG_DUMP :: Bool
c_DEBUG_DUMP = False

debugIO :: String -> IO ()
debugIO s
 | c_DEBUG_DUMP = puts s
 | otherwise    = return ()


#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif


type LPCSTR = Ptr Word8


mAX_DEFAULTCHAR :: Int
mAX_DEFAULTCHAR = 2

mAX_LEADBYTES :: Int
mAX_LEADBYTES = 12

-- Don't really care about the contents of this, but we have to make sure the size is right
data CPINFO = CPINFO {
    maxCharSize :: UINT,
    defaultChar :: [BYTE], -- ^ Always of length mAX_DEFAULTCHAR
    leadByte    :: [BYTE]  -- ^ Always of length mAX_LEADBYTES
  }

instance Storable CPINFO where
    sizeOf    _ = sizeOf (undefined :: UINT) + (mAX_DEFAULTCHAR + mAX_LEADBYTES) * sizeOf (undefined :: BYTE)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
      ptr <- return $ castPtr ptr
      a <- peek ptr
      ptr <- return $ castPtr $ advancePtr ptr 1
      b <- peekArray mAX_DEFAULTCHAR ptr
      c <- peekArray mAX_LEADBYTES   (advancePtr ptr mAX_DEFAULTCHAR)
      return $ CPINFO a b c  
    poke ptr val = do
      ptr <- return $ castPtr ptr
      poke ptr (maxCharSize val)
      ptr <- return $ castPtr $ advancePtr ptr 1
      pokeArray' "CPINFO.defaultChar" mAX_DEFAULTCHAR ptr                              (defaultChar val)
      pokeArray' "CPINFO.leadByte"    mAX_LEADBYTES   (advancePtr ptr mAX_DEFAULTCHAR) (leadByte val) 

pokeArray' :: Storable a => String -> Int -> Ptr a -> [a] -> IO ()
pokeArray' msg sz ptr xs | length xs == sz = pokeArray ptr xs
                         | otherwise       = error $ msg ++ ": expected " ++ show sz ++ " elements in list but got " ++ show (length xs)


foreign import WINDOWS_CCONV unsafe "windows.h GetCPInfo"
    c_GetCPInfo :: UINT       -- ^ CodePage
                -> Ptr CPINFO -- ^ lpCPInfo
                -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h MultiByteToWideChar"
    c_MultiByteToWideChar :: UINT   -- ^ CodePage
                          -> DWORD  -- ^ dwFlags
                          -> LPCSTR -- ^ lpMultiByteStr
                          -> CInt   -- ^ cbMultiByte
                          -> LPWSTR -- ^ lpWideCharStr
                          -> CInt   -- ^ cchWideChar
                          -> IO CInt

foreign import WINDOWS_CCONV unsafe "windows.h WideCharToMultiByte"
    c_WideCharToMultiByte :: UINT   -- ^ CodePage
                          -> DWORD  -- ^ dwFlags
                          -> LPWSTR -- ^ lpWideCharStr
                          -> CInt   -- ^ cchWideChar
                          -> LPCSTR -- ^ lpMultiByteStr
                          -> CInt   -- ^ cbMultiByte
                          -> LPCSTR -- ^ lpDefaultChar
                          -> LPBOOL -- ^ lpUsedDefaultChar
                          -> IO CInt

foreign import WINDOWS_CCONV unsafe "windows.h IsDBCSLeadByteEx"
    c_IsDBCSLeadByteEx :: UINT    -- ^ CodePage
                       -> BYTE    -- ^ TestChar
                       -> IO BOOL


-- | Returns a slow but correct implementation of TextEncoding using the Win32 API.
--
-- This is useful for supporting DBCS text encoding on the console without having to statically link
-- in huge code tables into all of our executables, or just as a fallback mechanism if a new code page
-- is introduced that we don't know how to deal with ourselves yet.  
mkCodePageEncoding :: CodingFailureMode -> Word32 -> TextEncoding
mkCodePageEncoding cfm cp
  = TextEncoding { 
        textEncodingName = "CP" ++ show cp,
        mkTextDecoder = newCP (recoverDecode cfm) cpDecode cp,
        mkTextEncoder = newCP (recoverEncode cfm) cpEncode cp
      }

newCP :: (Buffer from -> Buffer to -> IO (Buffer from, Buffer to))
      -> (Word32 -> Int -> CodeBuffer from to)
      -> Word32
      -> IO (BufferCodec from to ())
newCP rec fn cp = do
  -- Fail early if the code page doesn't exist, to match the behaviour of the IConv TextEncoding
  max_char_size <- alloca $ \cpinfo_ptr -> do
    success <- c_GetCPInfo cp cpinfo_ptr 
    unless success $ throwGetLastError ("GetCPInfo " ++ show cp)
    fmap (fromIntegral . maxCharSize) $ peek cpinfo_ptr

  debugIO $ "GetCPInfo " ++ show cp ++ " = " ++ show max_char_size

  return $ BufferCodec {
    encode = fn cp max_char_size,
    recover = rec,
    close  = return (),
    -- Windows doesn't supply a way to save/restore the state and doesn't need one
    -- since it's a dumb string->string API rather than a clever streaming one.
    getState = return (),
    setState = const $ return ()
  }


utf16_native_encode' :: EncodeBuffer
utf16_native_decode' :: DecodeBuffer
#ifdef WORDS_BIGENDIAN
utf16_native_encode' = utf16be_encode
utf16_native_decode' = utf16be_decode
#else
utf16_native_encode' = utf16le_encode
utf16_native_decode' = utf16le_decode
#endif

saner :: CodeBuffer from to
      -> Buffer from -> Buffer to
      -> IO (CodingProgress, Int, Buffer from, Buffer to)
saner code ibuf obuf = do
  (why, ibuf', obuf') <- code ibuf obuf
  -- Weird but true: the UTF16 codes have a special case (see the "done" functions)
  -- whereby if they entirely consume the input instead of returning an input buffer
  -- that is empty because bufL has reached bufR, they return a buffer that is empty
  -- because bufL = bufR = 0.
  --
  -- This is really very odd and confusing for our code that expects the difference
  -- between the old and new input buffer bufLs to indicate the number of elements
  -- that were consumed!
  --
  -- We fix it by explicitly extracting an integer which is the # of things consumed, like so:
  if isEmptyBuffer ibuf'
   then return (InputUnderflow, bufferElems ibuf,       ibuf', obuf')
   else return (why,            bufL ibuf' - bufL ibuf, ibuf', obuf')

byteView :: Buffer CWchar -> Buffer Word8
byteView (Buffer {..}) = Buffer { bufState = bufState, bufRaw = castForeignPtr bufRaw, bufSize = bufSize * 2, bufL = bufL * 2, bufR = bufR * 2 }

cwcharView :: Buffer Word8 -> Buffer CWchar
cwcharView (Buffer {..}) = Buffer { bufState = bufState, bufRaw = castForeignPtr bufRaw, bufSize = half bufSize, bufL = half bufL, bufR = half bufR }
  where half x = case x `divMod` 2 of (y, 0) -> y
                                      _      -> error "cwcharView: utf16_(encode|decode) (wrote out|consumed) non multiple-of-2 number of bytes"

utf16_native_encode :: CodeBuffer Char CWchar
utf16_native_encode ibuf obuf = do
  (why, ibuf, obuf) <- utf16_native_encode' ibuf (byteView obuf)
  return (why, ibuf, cwcharView obuf)

utf16_native_decode :: CodeBuffer CWchar Char
utf16_native_decode ibuf obuf = do
  (why, ibuf, obuf) <- utf16_native_decode' (byteView ibuf) obuf
  return (why, cwcharView ibuf, obuf)

cpDecode :: Word32 -> Int -> DecodeBuffer
cpDecode cp max_char_size = \ibuf obuf -> do
#ifdef CHARBUF_UTF16
    let mbuf = obuf
#else
    -- FIXME: share the buffer between runs, even if the buffer is not the perfect size
    let sz =       (bufferElems ibuf * 2)     -- I guess in the worst case the input CP text consists of 1-byte sequences that map entirely to things outside the BMP and so require 2 UTF-16 chars
             `min` (bufferAvailable obuf * 2) -- In the best case, each pair of UTF-16 points becomes a single UTF-32 point
    mbuf <- newBuffer (2 * sz) sz WriteBuffer :: IO (Buffer CWchar)
#endif
    debugIO $ "cpDecode " ++ summaryBuffer ibuf ++ " " ++ summaryBuffer mbuf
    (why1, ibuf', mbuf') <- cpRecode try' is_valid_prefix max_char_size 1 0 1 ibuf mbuf
    debugIO $ "cpRecode (cpDecode) = " ++ show why1 ++ " " ++ summaryBuffer ibuf' ++ " " ++ summaryBuffer mbuf'
#ifdef CHARBUF_UTF16
    return (why1, ibuf', mbuf')
#else
    -- Convert as much UTF-16 as possible to UTF-32. Note that it's impossible for this to fail
    -- due to illegal characters since the output from Window's encoding function should be correct UTF-16.
    -- However, it's perfectly possible to run out of either output or input buffer.
    debugIO $ "utf16_native_decode " ++ summaryBuffer mbuf' ++ " " ++ summaryBuffer obuf
    (why2, target_utf16_count, mbuf', obuf) <- saner utf16_native_decode (mbuf' { bufState = ReadBuffer }) obuf
    debugIO $ "utf16_native_decode = " ++ show why2 ++ " " ++ summaryBuffer mbuf' ++ " " ++ summaryBuffer obuf
    case why2 of
      -- If we successfully translate all of the UTF-16 buffer, we need to know why we couldn't get any more
      -- UTF-16 out of the Windows API
      InputUnderflow | isEmptyBuffer mbuf' -> return (why1, ibuf', obuf)
                     | otherwise           -> error "cpDecode: impossible underflown UTF-16 buffer"
      -- InvalidSequence should be impossible since mbuf' is output from Windows.
      InvalidSequence -> error "InvalidSequence on output of Windows API"
      -- If we run out of space in obuf, we need to ask for more output buffer space, while also returning
      -- the characters we have managed to consume so far.
      OutputUnderflow -> do
        -- We have an interesting problem here similar to the cpEncode case where we have to figure out how much
        -- of the byte buffer was consumed to reach as far as the last UTF-16 character we actually decoded to UTF-32 OK.
        --
        -- The minimum number of bytes it could take is half the number of UTF-16 chars we got on the output, since
        -- one byte could theoretically generate two UTF-16 characters.
        -- The common case (ASCII text) is that every byte in the input maps to a single UTF-16 character.
        -- In the worst case max_char_size bytes map to each UTF-16 character.
        byte_count <- bSearch "cpDecode" (cpRecode try' is_valid_prefix max_char_size 1 0 1) ibuf mbuf target_utf16_count (target_utf16_count `div` 2) target_utf16_count (target_utf16_count * max_char_size)
        return (OutputUnderflow, bufferRemove byte_count ibuf, obuf)
#endif
  where
    is_valid_prefix = c_IsDBCSLeadByteEx cp
    try' iptr icnt optr ocnt
     -- MultiByteToWideChar does surprising things if you have ocnt == 0
     | ocnt == 0 = return (Left True)
     | otherwise = do
        err <- c_MultiByteToWideChar (fromIntegral cp) 8 -- MB_ERR_INVALID_CHARS == 8: Fail if an invalid input character is encountered
                                     iptr (fromIntegral icnt) optr (fromIntegral ocnt)
        debugIO $ "MultiByteToWideChar " ++ show cp ++ " 8 " ++ show iptr ++ " " ++ show icnt ++ " " ++ show optr ++ " " ++ show ocnt ++ "\n = " ++ show err
        case err of
          -- 0 indicates that we did not succeed
          0 -> do
            err <- getLastError
            case err of
                122  -> return (Left True)
                1113 -> return (Left False)
                _    -> failWith "MultiByteToWideChar" err
          wrote_chars -> return (Right (fromIntegral wrote_chars))

cpEncode :: Word32 -> Int -> EncodeBuffer
cpEncode cp _max_char_size = \ibuf obuf -> do
#ifdef CHARBUF_UTF16
    let mbuf' = ibuf
#else
    -- FIXME: share the buffer between runs, even though that means we can't size the buffer as we want.
    let sz =       (bufferElems ibuf * 2)     -- UTF-32 always uses 4 bytes. UTF-16 uses at most 4 bytes.
             `min` (bufferAvailable obuf * 2) -- In the best case, each pair of UTF-16 points fits into only 1 byte
    mbuf <- newBuffer (2 * sz) sz WriteBuffer
    
    -- Convert as much UTF-32 as possible to UTF-16. NB: this can't fail due to output underflow
    -- since we sized the output buffer correctly. However, it could fail due to an illegal character
    -- in the input if it encounters a lone surrogate. In this case, our recovery will be applied as normal.
    (why1, ibuf', mbuf') <- utf16_native_encode ibuf mbuf
#endif
    debugIO $ "\ncpEncode " ++ summaryBuffer mbuf' ++ " " ++ summaryBuffer obuf
    (why2, target_utf16_count, mbuf', obuf) <- saner (cpRecode try' is_valid_prefix 2 1 1 0) (mbuf' { bufState = ReadBuffer }) obuf
    debugIO $ "cpRecode (cpEncode) = " ++ show why2 ++ " " ++ summaryBuffer mbuf' ++ " " ++ summaryBuffer obuf
#ifdef CHARBUF_UTF16
    return (why2, mbuf', obuf)
#else
    case why2 of
      -- If we succesfully translate all of the UTF-16 buffer, we need to know why
      -- we weren't able to get any more UTF-16 out of the UTF-32 buffer
      InputUnderflow | isEmptyBuffer mbuf' -> return (why1, ibuf', obuf)
                     | otherwise           -> error "cpEncode: impossible underflown UTF-16 buffer"
      -- With OutputUnderflow/InvalidSequence we only care about the failings of the UTF-16->CP translation.
      -- Yes, InvalidSequence is possible even though mbuf' is guaranteed to be valid UTF-16, because
      -- the code page may not be able to represent the encoded Unicode codepoint.
      _ -> do
        -- Here is an interesting problem. If we have only managed to translate part of the mbuf'
        -- then we need to return an ibuf which has consumed exactly those bytes required to obtain
        -- that part of the mbuf'. To reconstruct this information, we binary search for the number of
        -- UTF-32 characters required to get the consumed count of UTF-16 characters:
        --
        -- When dealing with data from the BMP (the common case), consuming N UTF-16 characters will be the same as consuming N
        -- UTF-32 characters. We start our search there so that most binary searches will terminate in a single iteration. 
        -- Furthermore, the absolute minimum number of UTF-32 characters this can correspond to is 1/2 the UTF-16 byte count
        -- (this will be realised when the input data is entirely not in the BMP).
        utf32_count <- bSearch "cpEncode" utf16_native_encode ibuf mbuf target_utf16_count (target_utf16_count `div` 2) target_utf16_count target_utf16_count
        return (why2, bufferRemove utf32_count ibuf, obuf)
#endif
  where
    -- Single characters should be mappable to bytes. If they aren't supported by the CP then we have an invalid input sequence.
    is_valid_prefix _ = return False

    try' iptr icnt optr ocnt
     -- WideCharToMultiByte does surprising things if you call it with ocnt == 0
     | ocnt == 0 = return (Left True)
     | otherwise = alloca $ \defaulted_ptr -> do
      poke defaulted_ptr False
      err <- c_WideCharToMultiByte (fromIntegral cp) 0 -- NB: the WC_ERR_INVALID_CHARS flag is uselses: only has an effect with the UTF-8 code page
                                   iptr (fromIntegral icnt) optr (fromIntegral ocnt)
                                   nullPtr defaulted_ptr
      defaulted <- peek defaulted_ptr
      debugIO $ "WideCharToMultiByte " ++ show cp ++ " 0 " ++ show iptr ++ " " ++ show icnt ++ " " ++ show optr ++ " " ++ show ocnt ++ " NULL " ++ show defaulted_ptr ++ "\n = " ++ show err ++ ", " ++ show defaulted
      case err of
          -- 0 indicates that we did not succeed
          0 -> do
            err <- getLastError
            case err of
                122  -> return (Left True)
                1113 -> return (Left False)
                _    -> failWith "WideCharToMultiByte" err
          wrote_bytes | defaulted -> return (Left False)
                      | otherwise -> return (Right (fromIntegral wrote_bytes))

bSearch :: String
        -> CodeBuffer from to
        -> Buffer from -> Buffer to -- From buffer (crucial data source) and to buffer (temporary storage only). To buffer must be empty (L=R).
        -> Int               -- Target size of to buffer
        -> Int -> Int -> Int -- Binary search min, mid, max
        -> IO Int            -- Size of from buffer required to reach target size of to buffer
bSearch msg code ibuf mbuf target_to_elems = go
  where
    go mn md mx = do
      -- NB: this loop repeatedly reencodes on top of mbuf using a varying fraction of ibuf. It doesn't
      -- matter if we blast the contents of mbuf since we already consumed all of the contents we are going to use.
      (_why, ibuf, mbuf) <- code (ibuf { bufR = bufL ibuf + md }) mbuf
      debugIO $ "code (bSearch " ++ msg ++ ") " ++ show md ++ " = " ++ show _why ++ ", " ++ summaryBuffer ibuf ++ summaryBuffer mbuf
      -- The normal case is to get InputUnderflow here, which indicates that coding basically
      -- terminated normally.
      --
      -- However, InvalidSequence is also possible if we are being called from cpDecode if we
      -- have just been unlucky enough to set md so that ibuf straddles a byte boundary.
      -- In this case we have to be really careful, because we don't want to report that
      -- "md" elements is the right number when in actual fact we could have had md-1 input
      -- elements and still produced the same number of bufferElems in mbuf. 
      --
      -- In fact, we have to worry about this possibility even if we get InputUnderflow
      -- since that will report InputUnderflow rather than InvalidSequence if the buffer
      -- ends in a valid lead byte. So the expedient thing to do is simply to check if
      -- the input buffer was entirely consumed.
      --
      -- When called from cpDecode, OutputUnderflow is also possible.
      --
      -- Luckily if we have InvalidSequence/OutputUnderflow and we do not appear to have reached
      -- the target, what we should do is the same as normal because the fraction of ibuf that our
      -- first "code" coded succesfully must be invalid-sequence-free, and ibuf will always
      -- have been decoded as far as the first invalid sequence in it. 
      case bufferElems mbuf `compare` target_to_elems of
        -- Coding n "from" chars from the input yields exactly as many "to" chars
        -- as were consumed by the recode. All is peachy:
        EQ -> debugIO ("bSearch = " ++ show solution) >> return solution
          where solution = md - bufferElems ibuf
        -- If we encoded fewer "to" characters than the target number, try again with more "from" characters (and vice-versa)
        LT -> go' (md+1) mx
        GT -> go' mn (md-1)
    go' mn mx | mn <= mx  = go mn (mn + ((mx - mn) `div` 2)) mx
              | otherwise = error $ "bSearch(" ++ msg ++ "): search crossed! " ++ show (summaryBuffer ibuf, summaryBuffer mbuf, target_to_elems, mn, mx)

cpRecode :: forall from to. (Show from, Storable from)
         => (Ptr from -> Int -> Ptr to -> Int -> IO (Either Bool Int))
         -> (from -> IO Bool)
         -> Int -- ^ Maximum length of a complete translatable sequence in the input (e.g. 2 if the input is UTF-16, 1 if the input is a SBCS, 2 is the input is a DBCS). Must be at least 1.
         -> Int -- ^ Minimum number of output elements per complete translatable sequence in the input (almost certainly 1)
         -> Int -> Int
         -> CodeBuffer from to
cpRecode try' is_valid_prefix max_i_size min_o_size iscale oscale = go
  where
    go :: CodeBuffer from to
    go ibuf obuf | isEmptyBuffer ibuf                = return (InputUnderflow,  ibuf, obuf)
                 | bufferAvailable obuf < min_o_size = return (OutputUnderflow, ibuf, obuf)
                 | otherwise                         = try (bufferElems ibuf `min` ((max_i_size * bufferAvailable obuf) `div` min_o_size)) seek_smaller
      where
        done why = return (why, ibuf, obuf)

        seek_smaller n longer_was_valid
          -- In this case, we can't shrink any further via any method. Calling (try 0) wouldn't be right because that will always claim InputUnderflow...
          | n <= 1 = if longer_was_valid
                      -- try m (where m >= n) was valid but we overflowed the output buffer with even a single input element
                      then done OutputUnderflow
                      -- there was no initial valid sequence in the input, but it might just be a truncated buffer - we need to check
                      else do byte <- withBuffer ibuf $ \ptr -> peekElemOff ptr (bufL ibuf)
                              valid_prefix <- is_valid_prefix byte
                              done (if valid_prefix && bufferElems ibuf < max_i_size then InputUnderflow else InvalidSequence)
          -- If we're already looking at very small buffers, try every n down to 1, to ensure we spot as long a sequence as exists while avoiding trying 0.
          -- Doing it this way ensures that we spot a single initial sequence of length <= max_i_size if any such exists.
          | n < 2 * max_i_size = try (n - 1) (\pred_n pred_n_was_valid -> seek_smaller pred_n (longer_was_valid || pred_n_was_valid))
          -- Otherwise, try a binary chop to try to either get the prefix before the invalid input, or shrink the output down so it fits
          -- in the output buffer. After the chop, try to consume extra input elements to try to recover as much of the sequence as possible if we
          -- end up chopping a multi-element input sequence into two parts.
          --
          -- Note that since max_i_size >= 1:
          --  * (n `div` 2) >= 1, so we don't try 0
          --  * ((n `div` 2) + (max_i_size - 1)) < n, so we don't get into a loop where (seek_smaller n) calls post_divide (n `div` 2) calls (seek_smaller n)
          | let n' = n `div` 2 = try n' (post_divide n' longer_was_valid)

        post_divide _  _                n True  = seek_smaller n True
        post_divide n' longer_was_valid n False | n < n' + max_i_size - 1 = try (n + 1) (post_divide n' longer_was_valid) -- There's still a chance..
                                                | otherwise               = seek_smaller n' longer_was_valid              -- No amount of recovery could save us :(

        try n k_fail = withBuffer ibuf $ \iptr -> withBuffer obuf $ \optr -> do
          ei_err_wrote <- try' (iptr `plusPtr` (bufL ibuf `shiftL` iscale)) n
                               (optr `plusPtr` (bufR obuf `shiftL` oscale)) (bufferAvailable obuf)
          debugIO $ "try " ++ show n ++ " = " ++ show ei_err_wrote
          case ei_err_wrote of
            -- ERROR_INSUFFICIENT_BUFFER: A supplied buffer size was not large enough, or it was incorrectly set to NULL.
            Left True  -> k_fail n True
            -- ERROR_NO_UNICODE_TRANSLATION: Invalid Unicode was found in a string.
            Left False -> k_fail n False
            -- Must have interpreted all given bytes successfully
            -- We need to iterate until we have consumed the complete contents of the buffer
            Right wrote_elts -> go (bufferRemove n ibuf) (obuf { bufR = bufR obuf + wrote_elts })
