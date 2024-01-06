module Main (main) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Exception (evaluate)
import GHC.Exts (noinline)

newtype Tricky = TrickyCon { unTrickyCon :: IO Tricky }

main :: IO ()
main = do
  ref <- newIORef False
  let
    tricky :: Tricky
    tricky = TrickyCon $ do
      putStrLn "tricky call"
      v <- readIORef ref
      case v of
        False -> writeIORef ref True >> evaluate (noinline tricky)
        True  -> putStrLn "this shouldn't be printed" >> pure tricky
  () <$ unTrickyCon tricky
