{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
{- | Copyright   : (c) 2010 Jasper Van der Jeugt
                   (c) 2010 - 2011 Simon Meier
License     : BSD3-style (see LICENSE)
Maintainer  : Simon Meier <iridcode@gmail.com>
Portability : GHC

'Builder's are used to efficiently construct sequences of bytes from
  smaller parts.
Typically,
  such a construction is part of the implementation of an /encoding/, i.e.,
  a function for converting Haskell values to sequences of bytes.
Examples of encodings are the generation of the sequence of bytes
  representing a HTML document to be sent in a HTTP response by a
  web application or the serialization of a Haskell value using
  a fixed binary format.

For an /efficient implementation of an encoding/,
  it is important that (a) little time is spent on converting
  the Haskell values to the resulting sequence of bytes /and/
  (b) that the representation of the resulting sequence
  is such that it can be consumed efficiently.
'Builder's support (a) by providing an /O(1)/ concatentation operation
  and efficient implementations of basic encodings for 'Char's, 'Int's,
  and other standard Haskell values.
They support (b) by providing their result as a lazy 'L.ByteString',
  which is internally just a linked list of pointers to /chunks/
  of consecutive raw memory.
Lazy 'L.ByteString's can be efficiently consumed by functions that
  write them to a file or send them over a network socket.
Note that each chunk boundary incurs expensive extra work (e.g., a system call)
  that must be amortized over the work spent on consuming the chunk body.
'Builder's therefore take special care to ensure that the
  average chunk size is large enough.
The precise meaning of large enough is application dependent.
The current implementation is tuned
  for an average chunk size between 4kb and 32kb,
  which should suit most applications.

As a simple example of an encoding implementation,
  we show how to efficiently convert the following representation of mixed-data
  tables to an UTF-8 encoded Comma-Separated-Values (CSV) table.

>data Cell = StringC String
>          | IntC Int
>          deriving( Eq, Ord, Show )
>
>type Row   = [Cell]
>type Table = [Row]

We use the following imports and abbreviate 'mappend' to simplify reading.

@
import qualified "Data.ByteString.Lazy"               as L
import           "Data.ByteString.Builder"
import           Data.Monoid
import           Data.Foldable                        ('foldMap')
import           Data.List                            ('intersperse')

infixr 4 \<\>
(\<\>) :: 'Monoid' m => m -> m -> m
(\<\>) = 'mappend'
@

CSV is a character-based representation of tables. For maximal modularity,
we could first render 'Table's as 'String's and then encode this 'String'
using some Unicode character encoding. However, this sacrifices performance
due to the intermediate 'String' representation being built and thrown away
right afterwards. We get rid of this intermediate 'String' representation by
fixing the character encoding to UTF-8 and using 'Builder's to convert
'Table's directly to UTF-8 encoded CSV tables represented as lazy
'L.ByteString's.

@
encodeUtf8CSV :: Table -> L.ByteString
encodeUtf8CSV = 'toLazyByteString' . renderTable

renderTable :: Table -> Builder
renderTable rs = 'mconcat' [renderRow r \<\> 'charUtf8' \'\\n\' | r <- rs]

renderRow :: Row -> Builder
renderRow []     = 'mempty'
renderRow (c:cs) =
    renderCell c \<\> mconcat [ charUtf8 \',\' \<\> renderCell c\' | c\' <- cs ]

renderCell :: Cell -> Builder
renderCell (StringC cs) = renderString cs
renderCell (IntC i)     = 'intDec' i

renderString :: String -> Builder
renderString cs = charUtf8 \'\"\' \<\> foldMap escape cs \<\> charUtf8 \'\"\'
  where
    escape \'\\\\\' = charUtf8 \'\\\\\' \<\> charUtf8 \'\\\\\'
    escape \'\\\"\' = charUtf8 \'\\\\\' \<\> charUtf8 \'\\\"\'
    escape c    = charUtf8 c
@

Note that the ASCII encoding is a subset of the UTF-8 encoding,
  which is why we can use the optimized function 'intDec' to
  encode an 'Int' as a decimal number with UTF-8 encoded digits.
Using 'intDec' is more efficient than @'stringUtf8' . 'show'@,
  as it avoids constructing an intermediate 'String'.
Avoiding this intermediate data structure significantly improves
  performance because encoding 'Cell's is the core operation
  for rendering CSV-tables.
See "Data.ByteString.Builder.Prim" for further
  information on how to improve the performance of 'renderString'.

We demonstrate our UTF-8 CSV encoding function on the following table.

@
strings :: [String]
strings =  [\"hello\", \"\\\"1\\\"\", \"&#955;-w&#246;rld\"]

table :: Table
table = [map StringC strings, map IntC [-3..3]]
@

The expression @encodeUtf8CSV table@ results in the following lazy
'L.ByteString'.

>Chunk "\"hello\",\"\\\"1\\\"\",\"\206\187-w\195\182rld\"\n-3,-2,-1,0,1,2,3\n" Empty

We can clearly see that we are converting to a /binary/ format. The \'&#955;\'
and \'&#246;\' characters, which have a Unicode codepoint above 127, are
expanded to their corresponding UTF-8 multi-byte representation.

We use the @criterion@ library (<http://hackage.haskell.org/package/criterion>)
  to benchmark the efficiency of our encoding function on the following table.

>import Criterion.Main     -- add this import to the ones above
>
>maxiTable :: Table
>maxiTable = take 1000 $ cycle table
>
>main :: IO ()
>main = defaultMain
>  [ bench "encodeUtf8CSV maxiTable (original)" $
>      whnf (L.length . encodeUtf8CSV) maxiTable
>  ]

On a Core2 Duo 2.20GHz on a 32-bit Linux,
  the above code takes 1ms to generate the 22'500 bytes long lazy 'L.ByteString'.
Looking again at the definitions above,
  we see that we took care to avoid intermediate data structures,
  as otherwise we would sacrifice performance.
For example,
  the following (arguably simpler) definition of 'renderRow' is about 20% slower.

>renderRow :: Row -> Builder
>renderRow  = mconcat . intersperse (charUtf8 ',') . map renderCell

Similarly, using /O(n)/ concatentations like '++' or the equivalent 'S.concat'
  operations on strict and lazy 'L.ByteString's should be avoided.
The following definition of 'renderString' is also about 20% slower.

>renderString :: String -> Builder
>renderString cs = charUtf8 $ "\"" ++ concatMap escape cs ++ "\""
>  where
>    escape '\\' = "\\"
>    escape '\"' = "\\\""
>    escape c    = return c

Apart from removing intermediate data-structures,
  encodings can be optimized further by fine-tuning their execution
  parameters using the functions in "Data.ByteString.Builder.Extra" and
  their \"inner loops\" using the functions in
  "Data.ByteString.Builder.Prim".
-}


module Data.ByteString.Builder
    (
      -- * The Builder type
      Builder

      -- * Executing Builders
      -- | Internally, 'Builder's are buffer-filling functions. They are
      -- executed by a /driver/ that provides them with an actual buffer to
      -- fill. Once called with a buffer, a 'Builder' fills it and returns a
      -- signal to the driver telling it that it is either done, has filled the
      -- current buffer, or wants to directly insert a reference to a chunk of
      -- memory. In the last two cases, the 'Builder' also returns a
      -- continutation 'Builder' that the driver can call to fill the next
      -- buffer. Here, we provide the two drivers that satisfy almost all use
      -- cases. See "Data.ByteString.Builder.Extra", for information
      -- about fine-tuning them.
    , toLazyByteString
    , hPutBuilder

      -- * Creating Builders

      -- ** Binary encodings
    , byteString
    , lazyByteString
    , shortByteString
    , int8
    , word8

      -- *** Big-endian
    , int16BE
    , int32BE
    , int64BE

    , word16BE
    , word32BE
    , word64BE

    , floatBE
    , doubleBE

      -- *** Little-endian
    , int16LE
    , int32LE
    , int64LE

    , word16LE
    , word32LE
    , word64LE

    , floatLE
    , doubleLE

    -- ** Character encodings
    -- | Conversion from 'Char' and 'String' into 'Builder's in various encodings.

    -- *** ASCII (Char7)
    -- | The ASCII encoding is a 7-bit encoding. The /Char7/ encoding implemented here
    -- works by truncating the Unicode codepoint to 7-bits, prefixing it
    -- with a leading 0, and encoding the resulting 8-bits as a single byte.
    -- For the codepoints 0-127 this corresponds the ASCII encoding.
    , char7
    , string7

    -- *** ISO/IEC 8859-1 (Char8)
    -- | The ISO/IEC 8859-1 encoding is an 8-bit encoding often known as Latin-1.
    -- The /Char8/ encoding implemented here works by truncating the Unicode codepoint
    -- to 8-bits and encoding them as a single byte. For the codepoints 0-255 this corresponds
    -- to the ISO/IEC 8859-1 encoding.
    , char8
    , string8

    -- *** UTF-8
    -- | The UTF-8 encoding can encode /all/ Unicode codepoints. We recommend
    -- using it always for encoding 'Char's and 'String's unless an application
    -- really requires another encoding.
    , charUtf8
    , stringUtf8

    , module Data.ByteString.Builder.ASCII

    ) where

import           Data.ByteString.Builder.Internal
import qualified Data.ByteString.Builder.Prim  as P
import qualified Data.ByteString.Lazy.Internal as L
import           Data.ByteString.Builder.ASCII

import           Data.String (IsString(..))
import           System.IO (Handle)
import           Foreign

-- HADDOCK only imports
import qualified Data.ByteString               as S (concat)
#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (Monoid(..))
#endif
import           Data.Foldable                      (foldMap)
import           Data.List                          (intersperse)


-- | Execute a 'Builder' and return the generated chunks as a lazy 'L.ByteString'.
-- The work is performed lazy, i.e., only when a chunk of the lazy 'L.ByteString'
-- is forced.
{-# NOINLINE toLazyByteString #-} -- ensure code is shared
toLazyByteString :: Builder -> L.ByteString
toLazyByteString = toLazyByteStringWith
    (safeStrategy L.smallChunkSize L.defaultChunkSize) L.Empty

{- Not yet stable enough.
   See note on 'hPut' in Data.ByteString.Builder.Internal
-}

-- | Output a 'Builder' to a 'Handle'.
-- The 'Builder' is executed directly on the buffer of the 'Handle'. If the
-- buffer is too small (or not present), then it is replaced with a large
-- enough buffer.
--
-- It is recommended that the 'Handle' is set to binary and
-- 'BlockBuffering' mode. See 'hSetBinaryMode' and 'hSetBuffering'.
--
-- This function is more efficient than @hPut . 'toLazyByteString'@ because in
-- many cases no buffer allocation has to be done. Moreover, the results of
-- several executions of short 'Builder's are concatenated in the 'Handle's
-- buffer, therefore avoiding unnecessary buffer flushes.
hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder h = hPut h . putBuilder


------------------------------------------------------------------------------
-- Binary encodings
------------------------------------------------------------------------------

-- | Encode a single signed byte as-is.
--
{-# INLINE int8 #-}
int8 :: Int8 -> Builder
int8 = P.primFixed P.int8

-- | Encode a single unsigned byte as-is.
--
{-# INLINE word8 #-}
word8 :: Word8 -> Builder
word8 = P.primFixed P.word8


------------------------------------------------------------------------------
-- Binary little-endian encodings
------------------------------------------------------------------------------

-- | Encode an 'Int16' in little endian format.
{-# INLINE int16LE #-}
int16LE :: Int16 -> Builder
int16LE = P.primFixed P.int16LE

-- | Encode an 'Int32' in little endian format.
{-# INLINE int32LE #-}
int32LE :: Int32 -> Builder
int32LE = P.primFixed P.int32LE

-- | Encode an 'Int64' in little endian format.
{-# INLINE int64LE #-}
int64LE :: Int64 -> Builder
int64LE = P.primFixed P.int64LE

-- | Encode a 'Word16' in little endian format.
{-# INLINE word16LE #-}
word16LE :: Word16 -> Builder
word16LE = P.primFixed P.word16LE

-- | Encode a 'Word32' in little endian format.
{-# INLINE word32LE #-}
word32LE :: Word32 -> Builder
word32LE = P.primFixed P.word32LE

-- | Encode a 'Word64' in little endian format.
{-# INLINE word64LE #-}
word64LE :: Word64 -> Builder
word64LE = P.primFixed P.word64LE

-- | Encode a 'Float' in little endian format.
{-# INLINE floatLE #-}
floatLE :: Float -> Builder
floatLE = P.primFixed P.floatLE

-- | Encode a 'Double' in little endian format.
{-# INLINE doubleLE #-}
doubleLE :: Double -> Builder
doubleLE = P.primFixed P.doubleLE


------------------------------------------------------------------------------
-- Binary big-endian encodings
------------------------------------------------------------------------------

-- | Encode an 'Int16' in big endian format.
{-# INLINE int16BE #-}
int16BE :: Int16 -> Builder
int16BE = P.primFixed P.int16BE

-- | Encode an 'Int32' in big endian format.
{-# INLINE int32BE #-}
int32BE :: Int32 -> Builder
int32BE = P.primFixed P.int32BE

-- | Encode an 'Int64' in big endian format.
{-# INLINE int64BE #-}
int64BE :: Int64 -> Builder
int64BE = P.primFixed P.int64BE

-- | Encode a 'Word16' in big endian format.
{-# INLINE word16BE #-}
word16BE :: Word16 -> Builder
word16BE = P.primFixed P.word16BE

-- | Encode a 'Word32' in big endian format.
{-# INLINE word32BE #-}
word32BE :: Word32 -> Builder
word32BE = P.primFixed P.word32BE

-- | Encode a 'Word64' in big endian format.
{-# INLINE word64BE #-}
word64BE :: Word64 -> Builder
word64BE = P.primFixed P.word64BE

-- | Encode a 'Float' in big endian format.
{-# INLINE floatBE #-}
floatBE :: Float -> Builder
floatBE = P.primFixed P.floatBE

-- | Encode a 'Double' in big endian format.
{-# INLINE doubleBE #-}
doubleBE :: Double -> Builder
doubleBE = P.primFixed P.doubleBE

------------------------------------------------------------------------------
-- ASCII encoding
------------------------------------------------------------------------------

-- | Char7 encode a 'Char'.
{-# INLINE char7 #-}
char7 :: Char -> Builder
char7 = P.primFixed P.char7

-- | Char7 encode a 'String'.
{-# INLINE string7 #-}
string7 :: String -> Builder
string7 = P.primMapListFixed P.char7

------------------------------------------------------------------------------
-- ISO/IEC 8859-1 encoding
------------------------------------------------------------------------------

-- | Char8 encode a 'Char'.
{-# INLINE char8 #-}
char8 :: Char -> Builder
char8 = P.primFixed P.char8

-- | Char8 encode a 'String'.
{-# INLINE string8 #-}
string8 :: String -> Builder
string8 = P.primMapListFixed P.char8

------------------------------------------------------------------------------
-- UTF-8 encoding
------------------------------------------------------------------------------

-- | UTF-8 encode a 'Char'.
{-# INLINE charUtf8 #-}
charUtf8 :: Char -> Builder
charUtf8 = P.primBounded P.charUtf8

-- | UTF-8 encode a 'String'.
{-# INLINE stringUtf8 #-}
stringUtf8 :: String -> Builder
stringUtf8 = P.primMapListBounded P.charUtf8

instance IsString Builder where
    fromString = stringUtf8
