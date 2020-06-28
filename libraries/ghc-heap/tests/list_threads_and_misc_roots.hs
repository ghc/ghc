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
data FoolClosure

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

createClosure :: Word -> IO (GenClosure Box)
createClosure tsoPtr = do
    let wPtr = unpackWord# tsoPtr
    getClosureData ((unsafeCoerce# wPtr) :: FoolClosure)

unpackWord# :: Word -> Word#
unpackWord# (W# w#) = w#

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual a b
    | a /= b = error (show a ++ " /= " ++ show b)
    | otherwise = return ()

assertIsClosureType :: ClosureType -> IO ()
assertIsClosureType t
    | t `elem` enumerate = return ()
    | otherwise = error (show t ++ " not in  " ++ show enumerate)
    where
        enumerate :: [ClosureType]
        enumerate = [minBound..maxBound]
