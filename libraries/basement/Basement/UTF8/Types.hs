module Basement.UTF8.Types
    (
    -- * Stepper
      Step(..)
    , StepBack(..)
    , StepASCII(..)
    , StepDigit(..)
    , isValidStepASCII
    , isValidStepDigit
    -- * Unicode Errors
    , ValidationFailure(..)
    -- * UTF8 Encoded 'Char'
    , CharUTF8(..)
    -- * Case Conversion
    , CM (..)
    ) where

import           Basement.Compat.Base
import           Basement.Types.OffsetSize

-- | Step when walking a String
--
-- this is a return value composed of :
-- * the unicode code point read (Char) which need to be
--   between 0 and 0x10ffff (inclusive)
-- * The next offset to start reading the next unicode code point (or end)
data Step = Step {-# UNPACK #-} !Char {-# UNPACK #-} !(Offset Word8)

-- | Similar to Step but used when processing the string from the end.
--
-- The stepper is thus the previous character, and the offset of
-- the beginning of the previous character
data StepBack = StepBack {-# UNPACK #-} !Char {-# UNPACK #-} !(Offset Word8)

-- | Step when processing digits. the value is between 0 and 9 to be valid
newtype StepDigit = StepDigit Word8

-- | Step when processing ASCII character
newtype StepASCII = StepASCII { stepAsciiRawValue :: Word8 }

-- | Specialized tuple used for case mapping.
data CM = CM {-# UNPACK #-} !Char {-# UNPACK #-} !Char {-# UNPACK #-} !Char deriving (Eq)

-- | Represent an already encoded UTF8 Char where the the lowest 8 bits is the start of the
-- sequence. If this contains a multi bytes sequence then each higher 8 bits are filled with
-- the remaining sequence 8 bits per 8 bits.
--
-- For example:
-- 'A' => U+0041  => 41          => 0x00000041
-- '€  => U+20AC  => E2 82 AC    => 0x00AC82E2
-- '𐍈' => U+10348 => F0 90 8D 88 => 0x888D90F0
--
newtype CharUTF8 = CharUTF8 Word32

isValidStepASCII :: StepASCII -> Bool
isValidStepASCII (StepASCII w) = w < 0x80

isValidStepDigit :: StepDigit -> Bool
isValidStepDigit (StepDigit w) = w < 0xa

-- | Possible failure related to validating bytes of UTF8 sequences.
data ValidationFailure = InvalidHeader
                       | InvalidContinuation
                       | MissingByte
                       | BuildingFailure
                       deriving (Show,Eq,Typeable)

instance Exception ValidationFailure
