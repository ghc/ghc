{-# OPTIONS_GHC -O2 -ddump-cmm -ddump-stg-final #-}
import Debug.Trace
import Control.Monad
import GHC.Magic
import Data.Time
import GHC.IO.Unsafe

main :: IO ()
main = do
  putStrLn "evaluating a top level updating thunk 2 times:"
  replicateM_ 2 $ updatingThunk
  putStrLn "evaluating a top level reentrant thunk 2 times:"
  replicateM_ 2 $ reentrantThunk
  x <- show <$> getCurrentTime
  let
    updating_local = trace "updating local thunk" $ pure x
    reentrant_local = noupdate (trace "reentrant local thunk" (pure x))
  putStrLn "evaluating a top level updating thunk 2 times:"
  replicateM_ 2 $ updating_local
  putStrLn "evaluating a top level reentrant thunk 2 times:"
  replicateM_ 2 $ reentrant_local
  let y = bar "dougrulz"
  replicateM_ 2 $ print y







{-# noinline updatingThunk #-}
updatingThunk :: IO Int
updatingThunk = trace "updatingThunk" $ length . show <$> getCurrentTime

{-# noinline foo #-}
foo () = (trace "reentrantThunk" $ length . show <$> getCurrentTime)

{-# noinline reentrantThunk #-}
reentrantThunk :: IO Int
reentrantThunk  = noupdate foo ()


{-# noinline bar #-}
bar :: String -> Int
bar s = let
  x = noupdate (length s)
  in x
