{-# OPTIONS -fwarn-unused-binds #-}
module ShouldCompile where

-- !!! should produce warnings about unused identifiers
x :: [()]
x = [ () | y <- [] ]

z = do w <- getContents; return ()
