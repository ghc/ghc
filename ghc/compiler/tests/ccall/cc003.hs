--!!! cc003 -- ccall with unresolved polymorphism (should fail)
module Test where

import PreludeGlaST

fubar :: PrimIO Int
fubar = ccall f `seqPrimIO` ccall b
		     --^ result type of f "lost" (never gets generalised)
