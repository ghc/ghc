{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
module Main(main) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C

data FnBlob

foreign import ccall "&free_fn_blob" free_fn_blob :: FunPtr (Ptr FnBlob -> IO ())

foreign import ccall safe "call_fn_blob" call_fn_blob :: Ptr FnBlob -> CDouble -> CDouble

type DoubleFn = CDouble -> CDouble

foreign import ccall unsafe "create_fn_blob" create_fn_blob :: FunPtr DoubleFn -> FunPtr (FunPtr DoubleFn -> IO ()) -> IO (Ptr FnBlob)

foreign import ccall unsafe "&freeHaskellFunctionPtr" free_fun_ptr :: FunPtr (FunPtr DoubleFn -> IO())

foreign import ccall "wrapper" wrapDoubleFn :: DoubleFn -> IO (FunPtr DoubleFn)

createFnBlob :: DoubleFn -> IO (ForeignPtr FnBlob)
createFnBlob dfn = do
  dfn_ptr <- wrapDoubleFn dfn
  ptr_fnblob <- create_fn_blob dfn_ptr free_fun_ptr
  newForeignPtr free_fn_blob ptr_fnblob

callFnBlob :: ForeignPtr FnBlob -> CDouble -> IO (CDouble)
callFnBlob fnblob d = withForeignPtr fnblob $ 
                        \ptrblob -> return $! call_fn_blob ptrblob d

main = do
  putStrLn "start"
  step 0
  putStrLn "done"

step n | n > 1000 = return ()
step n = do
  fnBlob <- createFnBlob (+ n)
  result <- callFnBlob fnBlob 0
  putStrLn $ "step " ++ show n ++ ": " ++ show result
  step (n + 1)
