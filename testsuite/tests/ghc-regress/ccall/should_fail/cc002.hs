-- !!! cc002 -- ccall with non-standard boxed arguments and results

module Test where

import Foreign

-- Test returning results

a :: IO ForeignObj
a = _ccall_ a

b :: IO (StablePtr Double)
b = _ccall_ b

-- Test taking arguments

c :: ForeignObj -> IO Int
c x = _ccall_ c x

d :: StablePtr Int -> IO Int
d x = _ccall_ d x
