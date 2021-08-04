{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Attoparsec.ByteString
import RFC2616
import System.Environment
import System.IO
import qualified Data.ByteString.Char8 as B

refill :: Handle -> IO B.ByteString
refill h = B.hGet h (80*1024)

listy :: FilePath -> Handle -> IO ()
listy file h = do
  r <- parseWith (refill h) (many request) =<< refill h
  case r of
    Fail _ _ msg -> hPutStrLn stderr $ file ++ ": " ++ msg
    Done _ reqs  -> print (length reqs)

incrementy :: FilePath -> Handle -> IO ()
incrementy file h = go (0::Int) =<< refill h
 where
   go !n is = do
     r <- parseWith (refill h) request is
     case r of
       Fail _ _ msg -> hPutStrLn stderr $ file ++ ": " ++ msg
       Done bs _req
           | B.null bs -> do
              s <- refill h
              if B.null s
                then print (n+1)
                else go (n+1) s
           | otherwise -> go (n+1) bs

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg ->
    bracket (openFile arg ReadMode) hClose $
      -- listy arg
      incrementy arg
