--!!! cc002 -- ccall with non-standard boxed arguments and results

module Test where

import PreludeGlaIO

-- Test returning results

a :: PrimIO _MallocPtr
a = _ccall_ a

b :: PrimIO _StablePtr
b = _ccall_ b

-- Test taking arguments

c :: _MallocPtr -> PrimIO Int
c x = _ccall_ c x

d :: _StablePtr -> PrimIO Int
d x = _ccall_ d x
