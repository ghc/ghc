--!!! cc003 -- ccall with unresolved polymorphism (should fail)
--!!! not anymore (as of 0.29, result type will default to ())
module Test where

import GlaExts

fubar :: PrimIO Int
fubar = _ccall_ f `seqPrimIO` _ccall_ b
		     --^ result type of f "lost" (never gets generalised)
