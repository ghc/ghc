{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , ExistentialQuantification
           , DeriveDataTypeable
  #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Handle.Types
-- Copyright   :  (c) The University of Glasgow, 1994-2009
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Basic types for the implementation of IO Handles.
--
-----------------------------------------------------------------------------

module GHC.IO.Handle.Types (
      Handle(..), Handle__(..), showHandle,
      checkHandleInvariants,
      BufferList(..),
      HandleType(..),
      isReadableHandleType, isWritableHandleType, isReadWriteHandleType,
      BufferMode(..),
      BufferCodec(..),
      NewlineMode(..), Newline(..), nativeNewline,
      universalNewlineMode, noNewlineTranslation, nativeNewlineMode
  ) where

#undef DEBUG

import GHC.Base
import GHC.MVar
import GHC.IO
import GHC.IO.Buffer
import GHC.IO.BufferedIO
import GHC.IO.Encoding.Types
import GHC.IORef
import Data.Maybe
import GHC.Show
import GHC.Read
import GHC.Word
import GHC.IO.Device
import Data.Typeable
#ifdef DEBUG
import Control.Monad
#endif

-- ---------------------------------------------------------------------------
-- Handle type

--  A Handle is represented by (a reference to) a record 
--  containing the state of the I/O port/device. We record
--  the following pieces of info:

--    * type (read,write,closed etc.)
--    * the underlying file descriptor
--    * buffering mode 
--    * buffer, and spare buffers
--    * user-friendly name (usually the
--      FilePath used when IO.openFile was called)

-- Note: when a Handle is garbage collected, we want to flush its buffer
-- and close the OS file handle, so as to free up a (precious) resource.

-- | Haskell defines operations to read and write characters from and to files,
-- represented by values of type @Handle@.  Each value of this type is a
-- /handle/: a record used by the Haskell run-time system to /manage/ I\/O
-- with file system objects.  A handle has at least the following properties:
-- 
--  * whether it manages input or output or both;
--
--  * whether it is /open/, /closed/ or /semi-closed/;
--
--  * whether the object is seekable;
--
--  * whether buffering is disabled, or enabled on a line or block basis;
--
--  * a buffer (whose length may be zero).
--
-- Most handles will also have a current I\/O position indicating where the next
-- input or output operation will occur.  A handle is /readable/ if it
-- manages only input or both input and output; likewise, it is /writable/ if
-- it manages only output or both input and output.  A handle is /open/ when
-- first allocated.
-- Once it is closed it can no longer be used for either input or output,
-- though an implementation cannot re-use its storage while references
-- remain to it.  Handles are in the 'Show' and 'Eq' classes.  The string
-- produced by showing a handle is system dependent; it should include
-- enough information to identify the handle for debugging.  A handle is
-- equal according to '==' only to itself; no attempt
-- is made to compare the internal state of different handles for equality.

data Handle 
  = FileHandle                          -- A normal handle to a file
        FilePath                        -- the file (used for error messages
                                        -- only)
        !(MVar Handle__)

  | DuplexHandle                        -- A handle to a read/write stream
        FilePath                        -- file for a FIFO, otherwise some
                                        --   descriptive string (used for error
                                        --   messages only)
        !(MVar Handle__)                -- The read side
        !(MVar Handle__)                -- The write side

  deriving Typeable

-- NOTES:
--    * A 'FileHandle' is seekable.  A 'DuplexHandle' may or may not be
--      seekable.

instance Eq Handle where
 (FileHandle _ h1)     == (FileHandle _ h2)     = h1 == h2
 (DuplexHandle _ h1 _) == (DuplexHandle _ h2 _) = h1 == h2
 _ == _ = False 

data Handle__
  = forall dev enc_state dec_state . (IODevice dev, BufferedIO dev, Typeable dev) =>
    Handle__ {
      haDevice      :: !dev,
      haType        :: HandleType,           -- type (read/write/append etc.)
      haByteBuffer  :: !(IORef (Buffer Word8)),
      haBufferMode  :: BufferMode,
      haLastDecode  :: !(IORef (dec_state, Buffer Word8)),
      haCharBuffer  :: !(IORef (Buffer CharBufElem)), -- the current buffer
      haBuffers     :: !(IORef (BufferList CharBufElem)),  -- spare buffers
      haEncoder     :: Maybe (TextEncoder enc_state),
      haDecoder     :: Maybe (TextDecoder dec_state),
      haCodec       :: Maybe TextEncoding,
      haInputNL     :: Newline,
      haOutputNL    :: Newline,
      haOtherSide   :: Maybe (MVar Handle__) -- ptr to the write side of a 
                                             -- duplex handle.
    }
    deriving Typeable

-- we keep a few spare buffers around in a handle to avoid allocating
-- a new one for each hPutStr.  These buffers are *guaranteed* to be the
-- same size as the main buffer.
data BufferList e
  = BufferListNil 
  | BufferListCons (RawBuffer e) (BufferList e)

--  Internally, we classify handles as being one
--  of the following:

data HandleType
 = ClosedHandle
 | SemiClosedHandle
 | ReadHandle
 | WriteHandle
 | AppendHandle
 | ReadWriteHandle

isReadableHandleType :: HandleType -> Bool
isReadableHandleType ReadHandle         = True
isReadableHandleType ReadWriteHandle    = True
isReadableHandleType _                  = False

isWritableHandleType :: HandleType -> Bool
isWritableHandleType AppendHandle    = True
isWritableHandleType WriteHandle     = True
isWritableHandleType ReadWriteHandle = True
isWritableHandleType _               = False

isReadWriteHandleType :: HandleType -> Bool
isReadWriteHandleType ReadWriteHandle{} = True
isReadWriteHandleType _                 = False

-- INVARIANTS on Handles:
--
--   * A handle *always* has a buffer, even if it is only 1 character long
--     (an unbuffered handle needs a 1 character buffer in order to support
--      hLookAhead and hIsEOF).
--   * In a read Handle, the byte buffer is always empty (we decode when reading)
--   * In a wriite Handle, the Char buffer is always empty (we encode when writing)
--
checkHandleInvariants :: Handle__ -> IO ()
#ifdef DEBUG
checkHandleInvariants h_ = do
 bbuf <- readIORef (haByteBuffer h_)
 checkBuffer bbuf
 cbuf <- readIORef (haCharBuffer h_)
 checkBuffer cbuf
 when (isWriteBuffer cbuf && not (isEmptyBuffer cbuf)) $
   error ("checkHandleInvariants: char write buffer non-empty: " ++
          summaryBuffer bbuf ++ ", " ++ summaryBuffer cbuf)
 when (isWriteBuffer bbuf /= isWriteBuffer cbuf) $
   error ("checkHandleInvariants: buffer modes differ: " ++
          summaryBuffer bbuf ++ ", " ++ summaryBuffer cbuf)

#else
checkHandleInvariants _ = return ()
#endif

-- ---------------------------------------------------------------------------
-- Buffering modes

-- | Three kinds of buffering are supported: line-buffering, 
-- block-buffering or no-buffering.  These modes have the following
-- effects. For output, items are written out, or /flushed/,
-- from the internal buffer according to the buffer mode:
--
--  * /line-buffering/: the entire output buffer is flushed
--    whenever a newline is output, the buffer overflows, 
--    a 'System.IO.hFlush' is issued, or the handle is closed.
--
--  * /block-buffering/: the entire buffer is written out whenever it
--    overflows, a 'System.IO.hFlush' is issued, or the handle is closed.
--
--  * /no-buffering/: output is written immediately, and never stored
--    in the buffer.
--
-- An implementation is free to flush the buffer more frequently,
-- but not less frequently, than specified above.
-- The output buffer is emptied as soon as it has been written out.
--
-- Similarly, input occurs according to the buffer mode for the handle:
--
--  * /line-buffering/: when the buffer for the handle is not empty,
--    the next item is obtained from the buffer; otherwise, when the
--    buffer is empty, characters up to and including the next newline
--    character are read into the buffer.  No characters are available
--    until the newline character is available or the buffer is full.
--
--  * /block-buffering/: when the buffer for the handle becomes empty,
--    the next block of data is read into the buffer.
--
--  * /no-buffering/: the next input item is read and returned.
--    The 'System.IO.hLookAhead' operation implies that even a no-buffered
--    handle may require a one-character buffer.
--
-- The default buffering mode when a handle is opened is
-- implementation-dependent and may depend on the file system object
-- which is attached to that handle.
-- For most implementations, physical files will normally be block-buffered 
-- and terminals will normally be line-buffered.

data BufferMode  
 = NoBuffering  -- ^ buffering is disabled if possible.
 | LineBuffering
                -- ^ line-buffering should be enabled if possible.
 | BlockBuffering (Maybe Int)
                -- ^ block-buffering should be enabled if possible.
                -- The size of the buffer is @n@ items if the argument
                -- is 'Just' @n@ and is otherwise implementation-dependent.
   deriving (Eq, Ord, Read, Show)

{-
[note Buffering Implementation]

Each Handle has two buffers: a byte buffer (haByteBuffer) and a Char
buffer (haCharBuffer).  

[note Buffered Reading]

For read Handles, bytes are read into the byte buffer, and immediately
decoded into the Char buffer (see
GHC.IO.Handle.Internals.readTextDevice).  The only way there might be
some data left in the byte buffer is if there is a partial multi-byte
character sequence that cannot be decoded into a full character.

Note that the buffering mode (haBufferMode) makes no difference when
reading data into a Handle.  When reading, we can always just read all
the data there is available without blocking, decode it into the Char
buffer, and then provide it immediately to the caller.

[note Buffered Writing]

Characters are written into the Char buffer by e.g. hPutStr.  At the
end of the operation, or when the char buffer is full, the buffer is
decoded to the byte buffer (see writeCharBuffer).  This is so that we
can detect encoding errors at the right point.

Hence, the Char buffer is always empty between Handle operations.

[note Buffer Sizing]

The char buffer is always a default size (dEFAULT_CHAR_BUFFER_SIZE).
The byte buffer size is chosen by the underlying device (via its
IODevice.newBuffer).  Hence the size of these buffers is not under
user control.

There are certain minimum sizes for these buffers imposed by the
library (but not checked):

 - we must be able to buffer at least one character, so that
   hLookAhead can work

 - the byte buffer must be able to store at least one encoded
   character in the current encoding (6 bytes?)

 - when reading, the char buffer must have room for two characters, so
   that we can spot the \r\n sequence.

How do we implement hSetBuffering?

For reading, we have never used the user-supplied buffer size, because
there's no point: we always pass all available data to the reader
immediately.  Buffering would imply waiting until a certain amount of
data is available, which has no advantages.  So hSetBuffering is
essentially a no-op for read handles, except that it turns on/off raw
mode for the underlying device if necessary.

For writing, the buffering mode is handled by the write operations
themselves (hPutChar and hPutStr).  Every write ends with
writeCharBuffer, which checks whether the buffer should be flushed
according to the current buffering mode.  Additionally, we look for
newlines and flush if the mode is LineBuffering.

[note Buffer Flushing]

** Flushing the Char buffer

We must be able to flush the Char buffer, in order to implement
hSetEncoding, and things like hGetBuf which want to read raw bytes.

Flushing the Char buffer on a write Handle is easy: it is always empty.

Flushing the Char buffer on a read Handle involves rewinding the byte
buffer to the point representing the next Char in the Char buffer.
This is done by

 - remembering the state of the byte buffer *before* the last decode

 - re-decoding the bytes that represent the chars already read from the
   Char buffer.  This gives us the point in the byte buffer that
   represents the *next* Char to be read.

In order for this to work, after readTextHandle we must NOT MODIFY THE
CONTENTS OF THE BYTE OR CHAR BUFFERS, except to remove characters from
the Char buffer.

** Flushing the byte buffer

The byte buffer can be flushed if the Char buffer has already been
flushed (see above).  For a read Handle, flushing the byte buffer
means seeking the device back by the number of bytes in the buffer,
and hence it is only possible on a seekable Handle.

-}

-- ---------------------------------------------------------------------------
-- Newline translation

-- | The representation of a newline in the external file or stream.
data Newline = LF    -- ^ '\n'
             | CRLF  -- ^ '\r\n'
             deriving (Eq, Ord, Read, Show)

-- | Specifies the translation, if any, of newline characters between
-- internal Strings and the external file or stream.  Haskell Strings
-- are assumed to represent newlines with the '\n' character; the
-- newline mode specifies how to translate '\n' on output, and what to
-- translate into '\n' on input.
data NewlineMode 
  = NewlineMode { inputNL :: Newline,
                    -- ^ the representation of newlines on input
                  outputNL :: Newline
                    -- ^ the representation of newlines on output
                 }
             deriving (Eq, Ord, Read, Show)

-- | The native newline representation for the current platform: 'LF'
-- on Unix systems, 'CRLF' on Windows.
nativeNewline :: Newline
#ifdef mingw32_HOST_OS
nativeNewline = CRLF
#else
nativeNewline = LF
#endif

-- | Map '\r\n' into '\n' on input, and '\n' to the native newline
-- represetnation on output.  This mode can be used on any platform, and
-- works with text files using any newline convention.  The downside is
-- that @readFile >>= writeFile@ might yield a different file.
-- 
-- > universalNewlineMode  = NewlineMode { inputNL  = CRLF, 
-- >                                       outputNL = nativeNewline }
--
universalNewlineMode :: NewlineMode
universalNewlineMode  = NewlineMode { inputNL  = CRLF, 
                                      outputNL = nativeNewline }

-- | Use the native newline representation on both input and output
-- 
-- > nativeNewlineMode  = NewlineMode { inputNL  = nativeNewline
-- >                                    outputNL = nativeNewline }
--
nativeNewlineMode    :: NewlineMode
nativeNewlineMode     = NewlineMode { inputNL  = nativeNewline, 
                                      outputNL = nativeNewline }

-- | Do no newline translation at all.
-- 
-- > noNewlineTranslation  = NewlineMode { inputNL  = LF, outputNL = LF }
--
noNewlineTranslation :: NewlineMode
noNewlineTranslation  = NewlineMode { inputNL  = LF, outputNL = LF }

-- ---------------------------------------------------------------------------
-- Show instance for Handles

-- handle types are 'show'n when printing error msgs, so
-- we provide a more user-friendly Show instance for it
-- than the derived one.

instance Show HandleType where
  showsPrec _ t =
    case t of
      ClosedHandle      -> showString "closed"
      SemiClosedHandle  -> showString "semi-closed"
      ReadHandle        -> showString "readable"
      WriteHandle       -> showString "writable"
      AppendHandle      -> showString "writable (append)"
      ReadWriteHandle   -> showString "read-writable"

instance Show Handle where 
  showsPrec _ (FileHandle   file _)   = showHandle file
  showsPrec _ (DuplexHandle file _ _) = showHandle file

showHandle :: FilePath -> String -> String
showHandle file = showString "{handle: " . showString file . showString "}"

