module T20609d where

import Data.Word (Word8)

-- Declarations in this module used to be accepted by GHC
-- before `forall` became a keyword (#23719).

foreign import ccall unsafe "forall"
  forall :: IO Word8
