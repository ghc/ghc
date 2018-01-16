module ShouldCompile where

import Foreign

-- !!! test that a recursive newtype can be used as an argument or result
-- type of a foreign import. 

newtype T = T (Ptr T)
foreign import ccall foo :: T -> IO T
