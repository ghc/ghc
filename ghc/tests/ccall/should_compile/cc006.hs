--!!! cc006 -- ccall with non-standard boxed arguments and results

module Test where

import GlaExts
import Foreign

-- Test returning results

a :: PrimIO Int
a = _ccall_ a

b :: PrimIO (StablePtr Int)
b = _ccall_ b

-- Test taking arguments

c :: ForeignObj -> PrimIO Int
c x = _ccall_ c x

d :: StablePtr Int -> PrimIO Int
d x = _ccall_ d x
