{- The GHCerr module defines the code for the
   wired-in error functions, which have a special
   type in the compiler (with "open tyvars").
   
   We cannot define these functions in a module where they might be
   used (e.g., GHCbase), because the magical wired-in type will get
   confused with what the typechecker figures out.
-}
module GHCerr where

import GHCbase (error__)

---------------------------------------------------------------
-- HACK: Magic unfoldings not implemented for unboxed lists
--	 Need to define a "build" to avoid undefined symbol
-- in this module to avoid .hi proliferation.

build   = error "GHCbase.build"
augment = error "GHCbase.augment"
--{-# GENERATE_SPECS build a #-}
--build 		:: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
--build g 	= g (:) []


---------------------------------------------------------------
cannon_fodder_to_avoid_empty__versions__ = (1::Int)

-- Used for compiler-generated error message;
-- encoding saves bytes of string junk.

absentErr, parError :: a
irrefutPatError
 , noDefaultMethodError
 , noExplicitMethodError
 , nonExhaustiveGuardsError
 , patError
 , recConError
 , recUpdError :: String -> a

absentErr = error "Oops! The program has entered an `absent' argument!\n"
parError  = error "Oops! Entered GHCbase.parError (a GHC bug -- please report it!)\n"

noDefaultMethodError     s = error ("noDefaultMethodError:"++s)
noExplicitMethodError    s = error ("noExplicitMethodError:"++s)

irrefutPatError s	    = patError__ (untangle s "irrefutable pattern")
nonExhaustiveGuardsError s  = patError__ (untangle s "non-exhaustive guards")
patError s		    = patError__ (untangle s "pattern-matching")

patError__ = error__ (\ x -> _ccall_ PatErrorHdrHook x)

recConError s = error (untangle s "record constructor")
recUpdError s = error (untangle s "record update")

untangle coded in_str
  =  "In "     ++ in_str
  ++ (if null msg then "" else (": " ++ msg))
  ++ "; at "   ++ file
  ++ ", line " ++ line
  ++ "\n"
  where
    (file,line,msg)
      = case (span not_bar coded) of { (f, (_:rest)) ->
	case (span not_bar rest)  of { (l, (_:m)) ->
	(f,l,m) }}
    not_bar c = c /= '|'
