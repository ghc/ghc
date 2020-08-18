{-# LANGUAGE ForeignFunctionInterface, MagicHash, BangPatterns #-}

import Foreign
import Foreign.C.Types
import GHC.Exts.Heap
import GHC.Exts

import GHC.Word

import TestUtils

foreign import ccall unsafe "create_tso.h create_tso"
    c_create_tso:: IO (Ptr ())

-- We can make some assumptions about the - otherwise dynamic - properties of
-- StgTSO and StgStack, because a new, non-running TSO is created with
-- create_tso() (create_tso.c).create_tso
main :: IO ()
main = do
    tso <- createTSOClosure
    assertEqual (what_next tso) ThreadRunGHC
    assertEqual (why_blocked tso) NotBlocked
    assertEqual (saved_errno tso) 0

    -- The newly created TSO should be on the end of the run queue.
    let !_linkBox = unsafe_link tso
    _linkClosure <- getBoxedClosureData _linkBox
    assertEqual (name _linkClosure) "END_TSO_QUEUE"
    assertEqual (getClosureType _linkClosure) CONSTR_NOCAF

    let !global_linkBox = unsafe_global_link tso
    globalLinkClosure <- getBoxedClosureData global_linkBox
    assertEqual (getClosureType globalLinkClosure) TSO

    let !stackBox = tsoStack tso
    stackClosure <- getBoxedClosureData stackBox
    assertEqual (getClosureType stackClosure) STACK

    let !stackPointerBox = unsafeStackPointer stackClosure
    stackPointerClosure <- getBoxedClosureData stackPointerBox
    assertEqual (getClosureType stackPointerClosure) RET_SMALL

    let !trecBox = unsafe_trec tso
    trecClosure <- getBoxedClosureData trecBox
    assertEqual (name trecClosure) "NO_TREC"

    let !blockedExceptionsBox = unsafe_blocked_exceptions tso
    blockedExceptionsClosure <- getBoxedClosureData blockedExceptionsBox
    assertEqual (name blockedExceptionsClosure) "END_TSO_QUEUE"

    let !bqBox = unsafe_bq tso
    bqClosure <- getBoxedClosureData bqBox
    assertEqual (name bqClosure) "END_TSO_QUEUE"

createTSOClosure :: IO (GenClosure Box)
createTSOClosure = do
    ptr <- c_create_tso
    createClosure ptr

getClosureType :: GenClosure b -> ClosureType
getClosureType = tipe . info
