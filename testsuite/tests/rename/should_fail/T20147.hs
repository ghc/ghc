{-# LANGUAGE NoNondecreasingIndentation #-}
module T20147 where

main = do
   print "doing things with X"
   withFile "test" $ \h -> do
   readFile h
