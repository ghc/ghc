%
% (c) The GRASP/AQUA Project, Glasgow University, 1997
%
\section{String buffers}

Buffers for scanning string input stored in external arrays.

\begin{code}

{-# OPTIONS -fno-prune-tydecls #-}
-- Don't really understand this!
-- ERROR: Can't see the data constructor(s) for _ccall_/_casm_  argument; 
-- type: ForeignObj(try compiling with -fno-prune-tydecls ..)


module StringBuffer
       (
        StringBuffer,

	 -- creation
        hGetStringBuffer,  -- :: FilePath       -> IO StringBuffer
	freeStringBuffer,  -- :: StringBuffer   -> IO ()

         -- Lookup
	currentChar,      -- :: StringBuffer -> Char
	currentChar#,     -- :: StringBuffer -> Char#
	indexSBuffer,     -- :: StringBuffer -> Int -> Char
	indexSBuffer#,    -- :: StringBuffer -> Int# -> Char#
         -- relative lookup, i.e, currentChar = lookAhead 0
	lookAhead,        -- :: StringBuffer -> Int  -> Char
	lookAhead#,       -- :: StringBuffer -> Int# -> Char#
        
	 -- moving the end point of the current lexeme.
        setCurrentPos#,   -- :: StringBuffer -> Int# -> StringBuffer
	incLexeme,	  -- :: StringBuffer -> StringBuffer
	decLexeme,	  -- :: StringBuffer -> StringBuffer

         -- move the start and end lexeme pointer on by x units.        
        stepOn,           -- :: StringBuffer -> StringBuffer
        stepOnBy#,        -- :: StringBuffer -> Int# -> StringBuffer
        stepOnTo#,        -- :: StringBuffer -> Int# -> StringBuffer
        stepOnUntil,      -- :: (Char -> Bool) -> StringBuffer -> StringBuffer
        stepOverLexeme,   -- :: StringBuffer   -> StringBuffer
	scanNumLit,       -- :: Int -> StringBuffer -> (Int, StringBuffer)
        expandWhile,      -- :: (Char -> Bool) -> StringBuffer -> StringBuffer
        expandUntilMatch, -- :: StrinBuffer -> String -> StringBuffer
         -- at or beyond end of buffer?
        bufferExhausted,  -- :: StringBuffer -> Bool
        emptyLexeme,      -- :: StringBuffer -> Bool

	 -- matching
        prefixMatch,       -- :: StringBuffer -> String -> Bool
	untilEndOfString#, -- :: StringBuffer -> Int#
	untilEndOfChar#,   -- :: StringBuffer -> Int#
	untilChar#,        -- :: StringBuffer -> Char# -> Int#

         -- conversion
        lexemeToString,     -- :: StringBuffer -> String
        lexemeToByteArray,  -- :: StringBuffer -> _ByteArray Int
        lexemeToFastString, -- :: StringBuffer -> FastString
        lexemeToBuffer,     -- :: StringBuffer -> StringBuffer

        FastString,
	ByteArray
       ) where

#include "HsVersions.h"

import GlaExts
import Addr 		( Addr(..) )
import Foreign
import IOBase
import IOHandle
import ST
import STBase
import Char 		(isDigit)
import PackBase 
import PrimPacked
import FastString

\end{code} 

\begin{code}
data StringBuffer
 = StringBuffer
     Addr#
--     ForeignObj#  -- the data
     Int#         -- length
     Int#         -- lexeme start
     Int#         -- current pos
\end{code}

\begin{code}
instance Text StringBuffer where
	showsPrec _ s = showString ""
\end{code}

\begin{code}
hGetStringBuffer :: FilePath -> IO StringBuffer
hGetStringBuffer fname =
--    trace ("Renamer: opening " ++ fname) $
    openFile fname ReadMode >>= \ hndl ->
    hFileSize hndl          >>= \ len@(J# _ _ d#) ->
    let len_i = fromInteger len in
      -- Allocate an array for system call to store its bytes into.
      -- ToDo: make it robust
--    trace (show ((len_i::Int)+1)) $
    _casm_ `` %r=(char *)malloc(sizeof(char)*(int)%0); '' (len_i::Int)  >>= \ arr@(A# a#) ->
    if addr2Int# a# ==# 0# then
       failWith MkIOError(hndl,UserError,("hGetStringBuffer: Could not allocate "++show len_i ++ " bytes"))
    else

--   _casm_ `` %r=NULL; ''		                     >>= \ free_p ->
--    makeForeignObj arr free_p		                     >>= \ fo@(_ForeignObj fo#) ->
     readHandle hndl        >>= \ hndl_ ->
     writeHandle hndl hndl_ >>
     let ptr = _filePtr hndl_ in
     _ccall_ fread arr (1::Int) len_i ptr                     >>= \  (I# read#) ->
--     trace ("DEBUG: opened " ++ fname ++ show (I# read#)) $
     hClose hndl		     >>
     if read# ==# 0# then -- EOF or other error
        failWith MkIOError(hndl,UserError,"hGetStringBuffer: EOF reached or some other error")
     else
        -- Add a sentinel NUL
        _casm_ `` ((char *)%0)[(int)%1]=(char)0; '' arr (I# (read# -# 1#)) >>= \ () ->
        return (StringBuffer a# read# 0# 0#)

freeStringBuffer :: StringBuffer -> IO ()
freeStringBuffer (StringBuffer a# _ _ _) =
 _casm_ `` free((char *)%0); '' (A# a#)

unsafeWriteBuffer :: StringBuffer -> Int# -> Char# -> StringBuffer
unsafeWriteBuffer s@(StringBuffer a _ _ _) i# ch# =
 unsafePerformIO (
   _casm_ `` ((char *)%0)[(int)%1]=(char)%2; '' (A# a) (I# i#) (C# ch#) >>= \ () ->
   return s
 )

\end{code}

Lookup

\begin{code}
currentChar# :: StringBuffer -> Char#
currentChar# (StringBuffer fo# _ _ current#) = indexCharOffAddr# fo# current#

currentChar  :: StringBuffer -> Char
currentChar sb = case currentChar# sb of c -> C# c

indexSBuffer# :: StringBuffer -> Int# -> Char#
indexSBuffer# (StringBuffer fo# _ _ _) i# = indexCharOffAddr# fo# i#

indexSBuffer :: StringBuffer -> Int -> Char
indexSBuffer sb (I# i#) = case indexSBuffer# sb i# of c -> C# c

 -- relative lookup, i.e, currentChar = lookAhead 0
lookAhead# :: StringBuffer -> Int# -> Char#
lookAhead# (StringBuffer fo# _ _ c#) i# = indexCharOffAddr# fo# (c# +# i#)

lookAhead :: StringBuffer -> Int  -> Char
lookAhead sb (I# i#) = case lookAhead# sb i# of c -> C# c

\end{code}

 moving the start point of the current lexeme.

\begin{code}
 -- moving the end point of the current lexeme.
setCurrentPos# :: StringBuffer -> Int# -> StringBuffer
setCurrentPos# (StringBuffer fo l# s# c#) i# =
 StringBuffer fo l# s# (c# +# i#)

-- augmenting the current lexeme by one.
incLexeme :: StringBuffer -> StringBuffer
incLexeme (StringBuffer fo l# s# c#) = StringBuffer fo l# s# (c# +# 1#)

decLexeme :: StringBuffer -> StringBuffer
decLexeme (StringBuffer fo l# s# c#) = StringBuffer fo l# s# (c# -# 1#)

\end{code}

-- move the start and end point of the buffer on by
-- x units.        

\begin{code}
stepOn :: StringBuffer -> StringBuffer
stepOn (StringBuffer fo l# s# c#) = StringBuffer fo l# (s# +# 1#) (s# +# 1#) -- assume they're the same.

stepOnBy# :: StringBuffer -> Int# -> StringBuffer
stepOnBy# (StringBuffer fo# l# s# c#) i# = 
 case s# +# i# of
  new_s# -> StringBuffer fo# l# new_s# new_s#

-- jump to pos.
stepOnTo# :: StringBuffer -> Int# -> StringBuffer
stepOnTo# (StringBuffer fo l _ _) s# = StringBuffer fo l s# s#

stepOnUntil :: (Char -> Bool) -> StringBuffer -> StringBuffer
stepOnUntil pred (StringBuffer fo l# s# c#) =
 loop c#
  where
   loop c# = 
    case indexCharOffAddr# fo c# of
     ch# | pred (C# ch#) -> StringBuffer fo l# c# c#
	 | ch# `eqChar#` '\NUL'# && c# >=# l# -> StringBuffer fo l# l# l# -- EOB, return immediately.
         | otherwise     -> loop (c# +# 1#)

stepOverLexeme :: StringBuffer -> StringBuffer
stepOverLexeme (StringBuffer fo l s# c#) = StringBuffer fo l c# c#

expandWhile :: (Char -> Bool) -> StringBuffer -> StringBuffer
expandWhile pred (StringBuffer fo l# s# c#) =
 loop c#
  where
   loop c# = 
    case indexCharOffAddr# fo c# of
     ch# | pred (C# ch#) -> loop (c# +# 1#)
	 | ch# `eqChar#` '\NUL'# && c# >=# l# -> StringBuffer fo l# l# l# -- EOB, return immediately.
         | otherwise     -> StringBuffer fo l# s# c#


scanNumLit :: Int -> StringBuffer -> (Int,StringBuffer)
scanNumLit (I# acc#) (StringBuffer fo l# s# c#) =
 loop acc# c#
  where
   loop acc# c# = 
    case indexCharOffAddr# fo c# of
     ch# | isDigit (C# ch#) -> loop (acc# *# 10# +# (ord# ch# -# ord# '0'#)) (c# +# 1#)
	 | ch# `eqChar#` '\NUL'# && c# >=# l# -> (I# acc#, StringBuffer fo l# l# l#) -- EOB, return immediately.
         | otherwise        -> (I# acc#,StringBuffer fo l# s# c#)


expandUntilMatch :: StringBuffer -> String -> StringBuffer
expandUntilMatch (StringBuffer fo l# s# c#) str =
  loop c# str
  where
   loop c# [] = StringBuffer fo l# s# c#
   loop c# ((C# x#):xs) =
     if indexCharOffAddr# fo c# `eqChar#` x# then
	loop (c# +# 1#) xs
     else
	loop (c# +# 1#) str
\end{code}

\begin{code}
   -- at or beyond end of buffer?
bufferExhausted :: StringBuffer -> Bool
bufferExhausted (StringBuffer fo l# _ c#) = c# >=# l#

emptyLexeme :: StringBuffer -> Bool
emptyLexeme (StringBuffer fo l# s# c#) = s# ==# c#

 -- matching
prefixMatch :: StringBuffer -> String -> Maybe StringBuffer
prefixMatch (StringBuffer fo l# s# c#) str =
  loop c# str
  where
   loop c# [] = Just (StringBuffer fo l# s# c#)
   loop c# ((C# x#):xs) =
     if indexCharOffAddr# fo c# `eqChar#` x# then
	loop (c# +# 1#) xs
     else
        Nothing

untilEndOfString# :: StringBuffer -> StringBuffer
untilEndOfString# (StringBuffer fo l# s# c#) = 
 loop c# 
 where
  loop c# =
   case indexCharOffAddr# fo c# of
    '\"'# ->
       case indexCharOffAddr# fo (c# -# 1#) of
	'\\'# ->       
                  -- looks like an escaped something or other to me,
		  -- better count the number of "\\"s that are immediately
		  -- preceeding to decide if the " is escaped.
	      let
	       odd_slashes flg i# =
	        case indexCharOffAddr# fo i# of
	         '\\'# -> odd_slashes (not flg) (i# -# 1#)
	         _     -> flg
              in
	      if odd_slashes True (c# -# 2#) then
	          -- odd number, " is ecaped.
		  loop (c# +# 1#)
	      else  -- a real end of string delimiter after all.
		  StringBuffer fo l# s# c#
        _ -> StringBuffer fo l# s# c#
    '\NUL'# ->
	if c# >=# l# then -- hit sentinel, this doesn't look too good..
	   StringBuffer fo l# l# l#
	else
	   loop (c# +# 1#)
    _ -> loop (c# +# 1#)


untilEndOfChar# :: StringBuffer -> StringBuffer
untilEndOfChar# (StringBuffer fo l# s# c#) = 
 loop c# 
 where
  loop c# =
   case indexCharOffAddr# fo c# of
    '\''# ->
       case indexCharOffAddr# fo (c# -# 1#) of
	'\\'# ->
	   case indexCharOffAddr# fo (c# -# 2#) of 	
	     '\\'# -> -- end of char
		   StringBuffer fo l# s# c#
             _ -> loop (c# +# 1#) -- false alarm
        _ -> StringBuffer fo l# s# c#
    '\NUL'# ->
	if c# >=# l# then -- hit sentinel, this doesn't look too good..
	   StringBuffer fo l# l# l#
	else
	   loop (c# +# 1#)
    _ -> loop (c# +# 1#)

untilChar# :: StringBuffer -> Char# -> StringBuffer
untilChar# (StringBuffer fo l# s# c#) x# = 
 loop c# 
 where
  loop c# =
   if indexCharOffAddr# fo c# `eqChar#` x# then
      StringBuffer fo l# s# c#
   else
      loop (c# +# 1#)

         -- conversion
lexemeToString :: StringBuffer -> String
lexemeToString (StringBuffer fo _ start_pos# current#) = 
 if start_pos# ==# current# then
    ""
 else
    unpackCStringBA (copySubStr (A# fo) (I# start_pos#) (I# (current# -# start_pos#)))
    
lexemeToByteArray :: StringBuffer -> _ByteArray Int
lexemeToByteArray (StringBuffer fo _ start_pos# current#) = 
 if start_pos# ==# current# then
    error "lexemeToByteArray" 
 else
    copySubStr (A# fo) (I# start_pos#) (I# (current# -# start_pos#))

lexemeToFastString :: StringBuffer -> FastString
lexemeToFastString (StringBuffer fo l# start_pos# current#) =
 if start_pos# ==# current# then
    mkFastCharString2 (A# fo) (I# 0#)
 else
    mkFastSubString (A# fo) (I# start_pos#) (I# (current# -# start_pos#))

{-
 Create a StringBuffer from the current lexeme, and add a sentinel
 at the end. Know What You're Doing before taking this function
 into use..
-}
lexemeToBuffer :: StringBuffer -> StringBuffer
lexemeToBuffer (StringBuffer fo l# start_pos# current#) =
 if start_pos# ==# current# then
    StringBuffer fo 0# start_pos# current# -- an error, really. 
 else
    unsafeWriteBuffer (StringBuffer fo (current# -# start_pos#) start_pos# start_pos#)
		      (current# -# 1#)
		      '\NUL'#

\end{code}
