{-# OPTIONS -fwarn-unused-binds #-}
module ShouldCompile where

-- !!! should produce warnings about unused identifers
x :: [()]
x = [ () | y <- [] ]

z = do w <- getContents; return ()
