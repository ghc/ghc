-- !!! cc006 -- ccall with non-standard boxed arguments and results

module ShouldCompile where

import Foreign
import GHC.Base --CCall
import Foreign.ForeignPtr
-- Test returning results
type ForeignObj = ForeignPtr ()
a :: IO Int
a = _ccall_ a

b :: IO (StablePtr Int)
b = _ccall_ b

-- Test taking arguments

c :: ForeignObj -> IO Int
c x = _ccall_ c x

d :: StablePtr Int -> IO Int
d x = _ccall_ d x
