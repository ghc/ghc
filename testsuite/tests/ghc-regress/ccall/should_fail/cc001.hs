-- !!! cc001 -- ccall with ambiguous argument
module Test where

f :: IO ()
f = _ccall_ foo (undefined ())
