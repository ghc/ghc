{-# LANGUAGE ImplicitParams #-}

-- !!! Implicit Parameters

-- GHC 5.00 doesn't handle this:

--  Could not deduce `?wibble :: t' from the context ()
--  Probable fix:
--      Add `?wibble :: t' to the banding(s) for {y}
--  Or add an instance declaration for `?wibble :: t'
--  arising from use of implicit parameter `?wibble' at tc122.hs:18
--  in the definition of function `y': wibble


module ShouldCompile where

x () = y
    where y = ?wibble
