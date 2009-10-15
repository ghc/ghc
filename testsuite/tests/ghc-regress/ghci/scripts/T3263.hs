{-# OPTIONS_GHC -Wall #-}

-- Test Trac #3263

module T3263 where

foo :: IO ()
foo = do { getChar
         ; return () }
