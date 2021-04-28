{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

import GHC.Prim (StackSnapshot#)
import GHC.Stack.CloneStack
import Foreign
import Foreign.C.Types (CUInt)

foreign import ccall "expectClosureTypes" expectClosureTypes:: StackSnapshot# -> Ptr CUInt -> Int -> IO ()

main :: IO ()
main = do
    stackSnapshot <- cloneMyStack

    let (StackSnapshot stack) = stackSnapshot
    let expectedClosureTypes = [ 30 -- RET_SMALL
                               , 30 -- RET_SMALL
                               , 34 -- CATCH_FRAME
                               ,36 -- STOP_FRAME
                               ]
    withArray expectedClosureTypes (\ptr -> expectClosureTypes stack ptr (length expectedClosureTypes))
