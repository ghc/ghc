{-# LANGUAGE MagicHash #-}

import Foreign.Ptr
import Foreign.Marshal.Array
import GHC.IORef
import Control.Concurrent
import GHC.Exts.Heap
import GHC.Exts

import TestUtils

foreign import ccall safe "list_threads_and_misc_roots_c.h listThreadsAndMiscRoots"
    listThreadsAndMiscRoots_c :: IO ()

foreign import ccall safe "list_threads_and_misc_roots_c.h getTSOCount"
    getTSOCount_c :: IO Int

foreign import ccall safe "list_threads_and_misc_roots_c.h getTSOs"
    getTSOs_c :: IO (Ptr (Ptr ()))

foreign import ccall safe "list_threads_and_misc_roots_c.h getMiscRootsCount"
    getMiscRootsCount_c :: IO Int

foreign import ccall safe "list_threads_and_misc_roots_c.h getMiscRoots"
    getMiscRoots_c :: IO (Ptr (Ptr ()))

main :: IO ()
main = do
    listThreadsAndMiscRoots_c

    tsoCount <- getTSOCount_c
    tsos <- getTSOs_c
    tsoList <- peekArray tsoCount tsos
    tsoClosures <- mapM createClosure tsoList
    assertEqual tsoCount $ length tsoClosures
    mapM (assertEqual TSO) $ map (tipe . info) tsoClosures

    miscRootsCount <- getMiscRootsCount_c
    miscRoots <- getMiscRoots_c
    miscRootsList <- peekArray miscRootsCount miscRoots
    heapClosures <- mapM createClosure miscRootsList
    assertEqual miscRootsCount $ length heapClosures
    -- Regarding the type system, this always has to be True, but we want to
    -- force evaluation / de-serialization with a simple check.
    mapM assertIsClosureType $ map (tipe . info) heapClosures

    return ()

assertIsClosureType :: ClosureType -> IO ()
assertIsClosureType t
    | t `elem` enumerate = return ()
    | otherwise = error (show t ++ " not in  " ++ show enumerate)
    where
        enumerate :: [ClosureType]
        enumerate = [minBound..maxBound]
