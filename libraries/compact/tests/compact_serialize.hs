module Main where

import Control.Exception
import Control.Monad
import System.Mem

import Data.IORef
import Data.ByteString (ByteString, packCStringLen)
import Foreign.Ptr
import Control.DeepSeq

import Data.Compact
import Data.Compact.Serialized

assertFail :: String -> IO ()
assertFail msg = throwIO $ AssertionFailed msg

assertEquals :: (Eq a, Show a) => a -> a -> IO ()
assertEquals expected actual =
  if expected == actual then return ()
  else assertFail $ "expected " ++ (show expected)
       ++ ", got " ++ (show actual)

serialize :: NFData a => a -> IO (SerializedCompact a, [ByteString])
serialize val = do
  cnf <- newCompact 4096 val

  bytestrref <- newIORef undefined
  scref <- newIORef undefined
  withSerializedCompact cnf $ \sc -> do
    writeIORef scref sc
    performMajorGC
    bytestrs <- forM (serializedCompactBlockList sc) $ \(ptr, size) -> do
      packCStringLen (castPtr ptr, fromIntegral size)
    writeIORef bytestrref bytestrs

  performMajorGC

  bytestrs <- readIORef bytestrref
  sc <- readIORef scref
  return (sc, bytestrs)

main = do
  let val = ("hello", 1, 42, 42, Just 42) ::
        (String, Int, Int, Integer, Maybe Int)

  (sc, bytestrs) <- serialize val
  performMajorGC

  mcnf <- importCompactByteStrings sc bytestrs
  case mcnf of
    Nothing -> assertFail "import failed"
    Just cnf -> assertEquals val (getCompact cnf)
