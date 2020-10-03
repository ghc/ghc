{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

import GHC.Prim (StackSnapshot#)
import GHC.Stack.CloneStack
import Control.Concurrent

foreign import ccall "printy" printStack:: StackSnapshot# -> IO ()

main :: IO ()
main = do
    mVarToBeBlockedOn <- newEmptyMVar
    threadId <- forkIO $ immediatelyBlocking mVarToBeBlockedOn

    stackSnapshot <- cloneThreadStack threadId
    let (StackSnapshot stack) = stackSnapshot
    printStack stack

immediatelyBlocking :: MVar Int -> IO ()
immediatelyBlocking mVarToBeBlockedOn = do
    takeMVar mVarToBeBlockedOn
    return ()
