-- !!! cc002 -- ccall with ambiguous result (should be defaulted to ())
module ShouldCompile where

a :: IO ()
a = do
 _ccall_ a
 return ()

	
