{-# OPTIONS -fwarn-unused-binds #-}
module ShouldCompile() where

-- should warn about unused f, even though f is used in itself
f = f
