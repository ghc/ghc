{-# LANGUAGE ForeignFunctionInterface, ExistentialQuantification #-}

import Data.List
import Control.Monad
import Control.Exception (evaluate)

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable

import System.IO
import System.Mem

import Unsafe.Coerce

foreign import ccall unsafe "setRootProfPtrs" c_setRootProfPtrs
  :: CInt -> Ptr (StablePtr a) -> Ptr CString -> IO ()

foreign import ccall "&g_rootProfileDebugLevel" g_rootProfileDebugLevel
  :: Ptr CInt

data Root = forall a. Root
  { rootDescr   :: String
  , rootClosure :: a
  }

setHeapRoots :: [Root] -> IO ()
setHeapRoots xs = do
  descs <- mapM (newCString . rootDescr) xs
  sps   <- forM xs $ \(Root _ a) ->
    newStablePtr =<< evaluate (unsafeCoerce a :: a)
  withArray descs $ \descs_arr ->
    withArray sps $ \sps_arr ->
      c_setRootProfPtrs (fromIntegral (length xs)) sps_arr descs_arr

main = do
  poke g_rootProfileDebugLevel 1
  hSetBuffering stdout NoBuffering
  putStrLn "= simple =" >> simple
  putStrLn "\n\n\n\n= sharing =" >> sharing
  poke g_rootProfileDebugLevel 0

simple = do
  let xs = [0..]; xs :: [Word]
  setHeapRoots [Root "xs" xs]
  forM_ [0..10] $ \i -> do
    evaluate (xs !! i)
    performMajorGC

sharing = do
  let x1 = 1 : x2
      x2 = 2 : x3
      x3 = 3 : x4
      x4 = 4 : x5
      x5 = 5 : x6
      x6 = 6 : x7
      x7 = []
      x7 :: [Word]

  setHeapRoots
    [ Root "1" x1
    , Root "2" x2
    , Root "3" x3
    , Root "4" x4
    , Root "5" x5
    , Root "6" x6
    , Root "[]" x7
    ]

  performMajorGC
