{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Data.Int
import System.IO

foreign import ccall "func64" func64 :: Int64 -> IO Int64
foreign import ccall "func32" func32 :: Int32 -> IO Int32
foreign import ccall "func16" func16 :: Int16 -> IO Int16
foreign import ccall "func8"  func8  :: Int8  -> IO Int8

main :: IO ()
main = do
    func64 (-2) >>= print >> hFlush stdout
    func32 (-2) >>= print >> hFlush stdout
    func16 (-2) >>= print >> hFlush stdout
    func8  (-2) >>= print >> hFlush stdout

