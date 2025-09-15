{-# LANGUAGE ImplicitParams #-}

-- !!! Implicit Parameters

-- If the implicit param isn't recognized as a PredType, x and y
-- will be inferred to have two params instead of one.

module ShouldCompile where

x () = ?wibble

y () = x ()

same :: a -> a -> b
same x y = undefined

a () = same x id
b () = same y id
