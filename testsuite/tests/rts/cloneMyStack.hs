{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

import GHC.Prim (StackSnapshot#)
import GHC.Stack.CloneStack

foreign import ccall "printy" printStack:: StackSnapshot# -> IO ()

main :: IO ()
main = do
    stackSnapshot <- cloneMyStack
    let (StackSnapshot stack) = stackSnapshot
    printStack stack
