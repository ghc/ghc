module PreludeBuiltin (
	_runST,
	_trace,
	absent#,
	error,
	patError#,
	parError#,
	unpackPS#, unpackPS2#,
        unpackAppendPS#,
	unpackFoldrPS#
    ) where

import Cls
import Core
import IInt
import List		( (++), foldr, takeWhile )
import TyArray		( Array(..) )
import PreludeErrIO	( errorIO )
import PreludeGlaST	-- state transformer stuff
import PreludeDialogueIO ( appendChan# )
#ifndef __PARALLEL_HASKELL__
import PreludeGlaMisc	( deRefStablePtr )
#endif
import PS		( _PackedString, _unpackPS, _packCBytes )
import Stdio		( _FILE )
import Text
import TyComplex

---------------------------------------------------------------
{- OLD:
packCString# :: [Char] -> ByteArray#

packCString# str = packString# str -- ToDo: more satisfactorily
-}

---------------------------------------------------------------
-- ******** defns of `error' and `trace' using Glasgow IO *****
-- No specialised versions are required for these bottoming Ids

error  :: String -> a
error s = error__ ( \ x -> _ccall_ ErrorHdrHook x ) s

error__ :: (_FILE -> PrimIO ()) -> String -> a

error__ msg_hdr s
#ifdef __PARALLEL_HASKELL__
  = errorIO (msg_hdr sTDERR{-msg hdr-}		`seqPrimIO`
	     _ccall_ fflush sTDERR		`seqPrimIO`
	     appendChan# sTDERR s		`seqPrimIO`
	     _ccall_ fflush sTDERR		`seqPrimIO`
	     _ccall_ stg_exit (1::Int)
	    )
#else
  = errorIO (msg_hdr sTDERR{-msg hdr-}		`seqPrimIO`
	     _ccall_ fflush sTDERR		`seqPrimIO`
	     appendChan# sTDERR s		`seqPrimIO`
	     _ccall_ fflush sTDERR		`seqPrimIO`
	     _ccall_ getErrorHandler	        `thenPrimIO` \ errorHandler ->
	     if errorHandler == (-1::Int) 
	     then _ccall_ stg_exit (1::Int)
	     else
	       _casm_ ``%r = (StgStablePtr)(%0);'' errorHandler
						`thenPrimIO` \ osptr ->
	       _ccall_ decrementErrorCount      `thenPrimIO` \ () ->
	       deRefStablePtr osptr             `thenPrimIO` \ oact ->
	       oact
	    )
#endif {- !parallel -}
  where
    sTDERR = (``stderr'' :: _FILE)

absent# = error "Oops! The program has entered an `absent' argument!\n"

parError# = error "Oops! Entered parError# (a GHC bug -- please report it!)\n"

---------------------------------------------------------------
_runST m = case m (S# realWorld#) of
           (r,_) -> r

---------------------------------------------------------------
-- Used for compiler-generated error message;
-- encoding saves bytes of string junk.

patError# :: String -> a

patError# encoded_msg
  = error__ (\ x -> _ccall_ PatErrorHdrHook x) (expand (encoded_msg ++ "\n"))
  where
    expand [] = []
    expand ('%':next:rest)
      = let
	    decoded
	      = case next of
	      	  '%' -> "%"
		  'D' -> "No default method for \""
		  'E' -> "No explicit method for \""
		  'N' -> ": non-exhaustive guards"
		  'F' -> "incomplete pattern(s) to match in function \""
		  'L' -> "pattern-matching failed in lambda"
		  'C' -> "pattern-matching failed in case"
		  '~' -> ": pattern-match failed on an irrefutable pattern"
		  'l' -> "\", line "
		  _   -> error ("BAD call to builtin patError#:" ++ (next:rest))
        in
	decoded ++ expand rest

    expand (c:rest) = c : expand rest

---------------------------------------------------------------
-- ******** defn of `_trace' using Glasgow IO *******

{-# GENERATE_SPECS _trace a #-}
_trace :: String -> a -> a

_trace string expr
  = unsafePerformPrimIO (
	((_ccall_ PreTraceHook sTDERR{-msg-})::PrimIO ())	`seqPrimIO`
	appendChan# sTDERR string				`seqPrimIO`
	((_ccall_ PostTraceHook sTDERR{-msg-})::PrimIO ())	`seqPrimIO`
	returnPrimIO expr )
  where
    sTDERR = (``stderr'' :: _FILE)

--------------------------------------------------------------------------

unpackPS#	:: Addr#         -> [Char] -- calls injected by compiler
unpackPS2#	:: Addr# -> Int# -> [Char] -- calls injected by compiler
unpackAppendPS# :: Addr# -> [Char] -> [Char] -- ditto?
unpackFoldrPS# :: Addr# -> (Char -> a -> a) -> a -> a -- ditto?

unpackPS# addr -- calls injected by compiler
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = []
      | True		   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackAppendPS# addr rest
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = rest
      | True		   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackFoldrPS# addr f z 
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = z
      | True		   = C# ch `f` unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackPS2# addr len -- calls injected by compiler
  -- this one is for literal strings with NULs in them; rare.
  = _unpackPS (_packCBytes (I# len) (A# addr))
