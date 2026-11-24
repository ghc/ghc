-- | Compatibility exports for code that uses the old name for natural counts.
module HaskLib.Size (Size, Nat (..), plus, plusZero, plusSucc, congSucc, congFirst) where

import HaskLib.Nat (Nat (..), congFirst, congSucc, plus, plusSucc, plusZero)
import PreludeMapLengthSize (Size)
