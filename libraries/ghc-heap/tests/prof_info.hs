{-# LANGUAGE ForeignFunctionInterface, MagicHash, CPP, BangPatterns #-}

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

-- We can make some assumptions about the - otherwise dynamic - properties of
-- StgTSO and StgStack, because a new, non-running TSO is created with
-- create_tso() (create_tso.c).create_tso
main :: IO ()
main = do
    tso <- createTSOClosure

-- TODO: remove print, add assertion
    print $ "tso : "++ show tso

createTSOClosure :: IO (GenClosure Box)
createTSOClosure = do
    ptr <- c_create_tso
    let wPtr = unpackWord# ptr
    getClosureData ((unsafeCoerce# wPtr) :: FoolStgTSO)

unpackWord# :: Word -> Word#
unpackWord# (W# w#) = w#
