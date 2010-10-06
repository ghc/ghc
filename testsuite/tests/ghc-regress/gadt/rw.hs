{-# LANGUAGE GADTs #-}

module Main where

import Data.IORef

data T a where
  Li:: Int -> T Int
  Lb:: Bool -> T Bool
  La:: a -> T a

writeInt:: T a -> IORef a -> IO ()
writeInt v ref = case v of
		   ~(Li x) -> writeIORef ref (1::Int)

readBool:: T a -> IORef a -> IO ()
readBool v ref = case v of
		   ~(Lb x) -> 
		       readIORef ref >>= (print . not)

tt::T a -> IO ()
tt v = case v of
         ~(Li x) ->  print "OK"

main = do
	tt (La undefined)
	ref <- newIORef undefined
	writeInt (La undefined) ref
	readBool (La undefined) ref
