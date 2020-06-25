{-# LANGUAGE MagicHash #-}

import Foreign.Ptr
import Foreign.Marshal.Array
import GHC.IORef
import Control.Concurrent
import GHC.Exts.Heap
import GHC.Exts


-- Invent a type to bypass the type constraints of getClosureData.
-- Infact this will be a Word#, that is directly given to unpackClosure#
-- (which is a primop that expects a pointer to a closure).
data FoolStgTSO

foreign import ccall safe "list_threads_and_misc_roots_c.h listThreadsAndMiscRoots"
    listThreadsAndMiscRoots_c :: IO ()

foreign import ccall safe "list_threads_and_misc_roots_c.h getTSOCount"
    getTSOCount_c :: IO Int

foreign import ccall safe "list_threads_and_misc_roots_c.h getTSOs"
    getTSOs_c :: IO (Ptr Word)

foreign import ccall safe "list_threads_and_misc_roots_c.h getMiscRootsCount"
    getMiscRootsCount_c :: IO Int

foreign import ccall safe "list_threads_and_misc_roots_c.h getMiscRoots"
    getMiscRoots_c :: IO (Ptr Word)

main :: IO ()
main = do
    listThreadsAndMiscRoots_c
    tsoCount <- getTSOCount_c
    print tsoCount
    tsos <- getTSOs_c
    tsoList <- peekArray tsoCount tsos
    tsoClosures <- sequence $ map createClosure tsoList
    print tsoClosures
    -- TODO: assert...

    miscRootsCount <- getMiscRootsCount_c
    print miscRootsCount
    miscRoots <- getMiscRoots_c
    miscRootsList <- peekArray miscRootsCount miscRoots
    heapClosures <- sequence $ map createClosure miscRootsList
    print heapClosures
    -- TODO: assert...

    return ()

createClosure :: Word -> IO (GenClosure Box)
createClosure tsoPtr = do
    let wPtr = unpackWord# tsoPtr
    getClosureData ((unsafeCoerce# wPtr) :: FoolStgTSO)

unpackWord# :: Word -> Word#
unpackWord# (W# w#) = w#
