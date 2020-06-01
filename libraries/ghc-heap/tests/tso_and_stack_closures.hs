{-# LANGUAGE ForeignFunctionInterface, MagicHash, CPP #-}

import Foreign
import Foreign.C.Types
import GHC.Exts.Heap
import GHC.Exts

import GHC.Word

#include "ghcconfig.h"
#include "rts/Constants.h"

foreign import ccall unsafe "create_tso.h create_tso"
    c_create_tso:: IO Word

-- Invent a type to bypass the type constraints of getClosureData.
-- Infact this will be a Word#, that is directly given to unpackClosure#
-- (which is a primop that expects a pointer to a closure).
data FoolStgTSO

main :: IO ()
main = do
    ptr <- c_create_tso
    let wPtr = unpackWord# ptr
    tso <- getClosureData ((unsafeCoerce# wPtr) :: FoolStgTSO)

    assertEqual (what_next tso) ThreadRunGHC
    assertEqual (why_blocked tso) NotBlocked
    assertEqual (saved_errno tso) 0

-- todo (sven): assert more?

    print $ "tso : "++ show tso

unpackWord# :: Word -> Word#
unpackWord# (W# w#) = w#

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual a b
  | a /= b = error (show a ++ " /= " ++ show b)
  | otherwise = return ()
