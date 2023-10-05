
{-# LANGUAGE NoNondecreasingIndentation #-}

module ShouldCompile where

f :: IO ()
f = do if True then f else do
       f
       if True then f else do
       f
