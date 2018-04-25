{-# LANGUAGE ImplicitParams #-}

-- !!! Monotypes w/ Implicit Parameters

-- GHC 5.00 doesn't handle this:

--  Couldn't match `{?wibble :: Int}' against `()'
--      Expected type: {?wibble :: Int}
--      Inferred type: ()
--  In the first argument of `x', namely `()'
--  in the definition of function `y': x ()

module ShouldCompile where

x () = (?wibble :: Int)

y () = x ()
