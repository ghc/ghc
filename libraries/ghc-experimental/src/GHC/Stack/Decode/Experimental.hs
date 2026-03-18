module GHC.Stack.Decode.Experimental (
    -- * High-level stack decoders
    decodeStackWithIpe,
    -- * Stack decoder helpers
    decodeStackWithFrameUnpack,
    -- * StackEntry
    StackEntry(..),
    -- * Pretty printing
    prettyStackFrameWithIpe,
    prettyStackEntry,
  ) where

import GHC.Internal.Stack.Decode
