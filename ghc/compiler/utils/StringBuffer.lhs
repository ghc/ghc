%
% (c) The GRASP/AQUA Project, Glasgow University, 1997-1998
%
\section{String buffers}

Buffers for scanning string input stored in external arrays.

\begin{code}

{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module StringBuffer
       (
        StringBuffer,

	 -- creation/destruction
        hGetStringBuffer,     -- :: FilePath     -> IO StringBuffer
	stringToStringBuffer, -- :: String       -> IO StringBuffer
	freeStringBuffer,     -- :: StringBuffer -> IO ()

         -- Lookup
	currentChar,      -- :: StringBuffer -> Char
	currentChar#,     -- :: StringBuffer -> Char#
	indexSBuffer,     -- :: StringBuffer -> Int -> Char
	indexSBuffer#,    -- :: StringBuffer -> Int# -> Char#
         -- relative lookup, i.e, currentChar = lookAhead 0
	lookAhead,        -- :: StringBuffer -> Int  -> Char
	lookAhead#,       -- :: StringBuffer -> Int# -> Char#
        
	-- offsets
	currentIndex#,    -- :: StringBuffer -> Int#
	lexemeIndex,	  -- :: StringBuffer -> Int#

	 -- moving the end point of the current lexeme.
        addToCurrentPos,   -- :: StringBuffer -> Int# -> StringBuffer
	incCurrentPos,	  -- :: StringBuffer -> StringBuffer
	decCurrentPos,	  -- :: StringBuffer -> StringBuffer

         -- move the start and end lexeme pointer on by x units.        
        stepOn,           -- :: StringBuffer -> StringBuffer
        stepOnBy#,        -- :: StringBuffer -> Int# -> StringBuffer
        stepOnTo#,        -- :: StringBuffer -> Int# -> StringBuffer
        stepOnUntil,      -- :: (Char -> Bool) -> StringBuffer -> StringBuffer
	stepOnUntilChar#, -- :: StringBuffer -> Char# -> StringBuffer
        stepOverLexeme,   -- :: StringBuffer   -> StringBuffer
	scanNumLit,       -- :: Int -> StringBuffer -> (Int, StringBuffer)
	squeezeLexeme,	  -- :: StringBuffer -> Int# -> StringBuffer
	mergeLexemes,	  -- :: StringBuffer -> StringBuffer -> StringBuffer
        expandWhile,      -- :: (Char  -> Bool) -> StringBuffer -> StringBuffer
        expandWhile#,     -- :: (Char# -> Bool) -> StringBuffer -> StringBuffer
        expandUntilMatch, -- :: StrinBuffer -> String -> StringBuffer
         -- at or beyond end of buffer?
        bufferExhausted,  -- :: StringBuffer -> Bool
        emptyLexeme,      -- :: StringBuffer -> Bool

	 -- matching
        prefixMatch,       -- :: StringBuffer -> String -> Bool
	untilEndOfString#, -- :: StringBuffer -> Int#

         -- conversion
        lexemeToString,     -- :: StringBuffer -> String
        lexemeToFastString, -- :: StringBuffer -> FastString
       ) where

#include "HsVersions.h"


#if __GLASGOW_HASKELL__ < 502
import Panic		( panic )
#else
#if __GLASGOW_HASKELL__ < 503
import Ptr		( Ptr(..) )
#else
import GHC.Ptr		( Ptr(..) )
#endif
#endif

#if __GLASGOW_HASKELL__  < 501
import Char		( chr )
#elif __GLASGOW_HASKELL__ < 503
import PrelIO		( hGetcBuffered )
#else
import GHC.IO		( hGetcBuffered )
#endif

import PrimPacked
import FastString

import GLAEXTS

import Foreign

#if __GLASGOW_HASKELL__ >= 502
import CString ( newCString )
#endif

import IO		( openFile, isEOFError )
import EXCEPTION	( bracket )

#if __GLASGOW_HASKELL__ < 503
import PrelIOBase
import PrelHandle
#else
import GHC.IOBase
import GHC.Handle
#endif

import Char 		( isDigit )
\end{code} 

\begin{code}
data StringBuffer
 = StringBuffer
     Addr#
     Int#         -- length
     Int#         -- lexeme start
     Int#         -- current pos
\end{code}

\begin{code}
instance Show StringBuffer where
	showsPrec _ s = showString "<stringbuffer>"
\end{code}

\begin{code}
hGetStringBuffer :: FilePath -> IO StringBuffer
hGetStringBuffer fname = do
   (a, read) <- slurpFileExpandTabs fname 

	-- urk! slurpFile gives us a buffer that doesn't have room for
	-- the sentinel.  Assume it has a final newline for now, and overwrite
	-- that with the sentinel.  slurpFileExpandTabs (below) leaves room
	-- for the sentinel.
   let  (Ptr a#) = a;  
	(I# read#) = read;
	end# = read# -# 1#

   -- add sentinel '\NUL'
   writeCharOffPtr a (I# end#) '\0'

   return (StringBuffer a# end# 0# 0#)
\end{code}

-----------------------------------------------------------------------------
-- Turn a String into a StringBuffer

\begin{code}
stringToStringBuffer :: String -> IO StringBuffer
freeStringBuffer :: StringBuffer -> IO ()

#if __GLASGOW_HASKELL__ >= 502
stringToStringBuffer str = do
  let sz@(I# sz#) = length str
  Ptr a# <- newCString str
  return (StringBuffer a# sz# 0# 0#)

freeStringBuffer (StringBuffer a# _ _ _) = Foreign.free (Ptr a#)
#else
stringToStringBuffer = panic "stringToStringBuffer: not implemented"
freeStringBuffer sb  = return ()
#endif

\end{code}

-----------------------------------------------------------------------------
This very disturbing bit of code is used for expanding the tabs in a
file before we start parsing it.  Expanding the tabs early makes the
lexer a lot simpler: we only have to record the beginning of the line
in order to be able to calculate the column offset of the current
token.

We guess the size of the buffer required as 20% extra for
expanded tabs, and enlarge it if necessary.

\begin{code}
#if __GLASGOW_HASKELL__ < 501
getErrType :: IO Int
getErrType =  _ccall_ getErrType__
#endif

slurpFileExpandTabs :: FilePath -> IO (Ptr (),Int)
slurpFileExpandTabs fname = do
  bracket (openFile fname ReadMode) (hClose) 
   (\ handle ->
     do sz <- hFileSize handle
        if sz > toInteger (maxBound::Int) 
	  then ioError (userError "slurpFile: file too big")
          else do
      	    let sz_i = fromInteger sz
            if sz_i == 0
			-- empty file: just allocate a buffer containing '\0'
		then do chunk <- allocMem 1
			writeCharOffPtr chunk 0 '\0'
			return (chunk, 0)
		else do let sz_i' = (sz_i * 12) `div` 10 -- add 20% for tabs
	      	        chunk <- allocMem sz_i'
      	    		trySlurp handle sz_i' chunk
   )

trySlurp :: Handle -> Int -> Ptr () -> IO (Ptr (), Int)
trySlurp handle sz_i chunk =
#if __GLASGOW_HASKELL__ < 501
  wantReadableHandle "hGetChar" handle $ \ handle_ ->
  let fo = haFO__ handle_ in
#else
  wantReadableHandle "hGetChar" handle $ 
      \ handle_@Handle__{ haFD=fd, haBuffer=ref, haBufferMode=mode } ->
#endif
  let
	(I# chunk_sz) = sz_i

	tAB_SIZE = 8#

	slurpFile :: Int# -> Int# -> Ptr () -> Int# -> Int# -> IO (Ptr (), Int)
	slurpFile c off chunk chunk_sz max_off = slurp c off
	 where

	  slurp :: Int# -> Int# -> IO (Ptr (), Int)
	  slurp c off | off >=# max_off = do
		let new_sz = chunk_sz *# 2#
	     	chunk' <- reAllocMem chunk (I# new_sz)
	     	slurpFile c off chunk' new_sz (new_sz -# (tAB_SIZE +# 1#))
    	  slurp c off = do
#if __GLASGOW_HASKELL__ < 501
    		intc <- mayBlock fo (_ccall_ fileGetc fo)
    		if intc == ((-1)::Int)
     		  then do errtype <- getErrType
			  if errtype == (19{-ERR_EOF-} :: Int)
			    then return (chunk, I# off)
			    else constructErrorAndFail "slurpFile"
     		  else case chr intc of
#else
		buf <- readIORef ref
		ch <- (if not (bufferEmpty buf)
		      then hGetcBuffered fd ref buf
		      else do 
#if __GLASGOW_HASKELL__ >= 503
		              new_buf <- fillReadBuffer fd True False buf
#else
		              new_buf <- fillReadBuffer fd True buf
#endif
		              hGetcBuffered fd ref new_buf)
		    `catch` \e -> if isEOFError e
			then return '\xFFFF'
			else ioError e
		case ch of
			 '\xFFFF' -> return (chunk, I# off)
#endif
			 '\t' -> tabIt c off
			 ch   -> do  writeCharOffPtr chunk (I# off) ch
				     let c' | ch == '\n' = 0#
					    | otherwise  = c +# 1#
				     slurp c' (off +# 1#)

	  tabIt :: Int# -> Int# -> IO (Ptr (), Int)
	  -- can't run out of buffer in here, because we reserved an
	  -- extra tAB_SIZE bytes at the end earlier.
	  tabIt c off = do
		writeCharOffPtr chunk (I# off) ' '
		let c' = c +# 1#
		    off' = off +# 1#
		if c' `remInt#` tAB_SIZE ==# 0#
			then slurp c' off'
		    	else tabIt c' off'
  in do

	-- allow space for a full tab at the end of the buffer
	-- (that's what the max_off thing is for),
	-- and add 1 to allow room for the final sentinel \NUL at
	-- the end of the file.
  (chunk', rc) <- slurpFile 0# 0# chunk chunk_sz (chunk_sz -# (tAB_SIZE +# 1#))
  return (chunk', rc+1 {- room for sentinel -})


reAllocMem :: Ptr () -> Int -> IO (Ptr ())
reAllocMem ptr sz = do
   chunk <- c_realloc ptr sz
   if chunk == nullPtr 
      then fail "reAllocMem"
      else return chunk

allocMem :: Int -> IO (Ptr ())
allocMem sz = do
   chunk <- c_malloc sz
   if chunk == nullPtr 
#if __GLASGOW_HASKELL__ < 501
      then constructErrorAndFail "allocMem"
#else
      then ioException (IOError Nothing ResourceExhausted "malloc"
					"out of memory" Nothing)
#endif
      else return chunk

#if __GLASGOW_HASKELL__ <= 408
c_malloc sz = do A# a <- c_malloc' sz; return (Ptr a)
foreign import ccall "malloc" unsafe
  c_malloc' :: Int -> IO Addr

c_realloc (Ptr a) sz = do A# a <- c_realloc' (A# a) sz; return (Ptr a)
foreign import ccall "realloc" unsafe
  c_realloc' :: Addr -> Int -> IO Addr
#else
foreign import ccall "malloc" unsafe
  c_malloc :: Int -> IO (Ptr a)

foreign import ccall "realloc" unsafe
  c_realloc :: Ptr a -> Int -> IO (Ptr a)
#endif
\end{code}

Lookup

\begin{code}
currentChar  :: StringBuffer -> Char
currentChar sb = case currentChar# sb of c -> C# c

lookAhead :: StringBuffer -> Int  -> Char
lookAhead sb (I# i#) = case lookAhead# sb i# of c -> C# c

indexSBuffer :: StringBuffer -> Int -> Char
indexSBuffer sb (I# i#) = case indexSBuffer# sb i# of c -> C# c

currentChar# :: StringBuffer -> Char#
indexSBuffer# :: StringBuffer -> Int# -> Char#
lookAhead# :: StringBuffer -> Int# -> Char#
currentChar# (StringBuffer fo# _ _ current#) = indexCharOffAddr# fo# current#
indexSBuffer# (StringBuffer fo# _ _ _) i# = indexCharOffAddr# fo# i#

 -- relative lookup, i.e, currentChar = lookAhead 0
lookAhead# (StringBuffer fo# _ _ c#) i# = indexCharOffAddr# fo# (c# +# i#)

currentIndex# :: StringBuffer -> Int#
currentIndex# (StringBuffer fo# _ _ c#) = c#

lexemeIndex :: StringBuffer -> Int#
lexemeIndex (StringBuffer fo# _ c# _) = c#
\end{code}

 moving the start point of the current lexeme.

\begin{code}
 -- moving the end point of the current lexeme.
addToCurrentPos :: StringBuffer -> Int# -> StringBuffer
addToCurrentPos (StringBuffer fo l# s# c#) i# =
 StringBuffer fo l# s# (c# +# i#)

-- augmenting the current lexeme by one.
incCurrentPos :: StringBuffer -> StringBuffer
incCurrentPos (StringBuffer fo l# s# c#) = StringBuffer fo l# s# (c# +# 1#)

decCurrentPos :: StringBuffer -> StringBuffer
decCurrentPos (StringBuffer fo l# s# c#) = StringBuffer fo l# s# (c# -# 1#)

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

squeezeLexeme :: StringBuffer -> Int# -> StringBuffer
squeezeLexeme (StringBuffer fo l s# c#) i# = StringBuffer fo l (s# +# i#) c#

mergeLexemes :: StringBuffer -> StringBuffer -> StringBuffer
mergeLexemes (StringBuffer fo l s# _) (StringBuffer _ _ _ c#)
   = StringBuffer fo l s# c#

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

expandWhile# :: (Char# -> Bool) -> StringBuffer -> StringBuffer
expandWhile# pred (StringBuffer fo l# s# c#) =
 loop c#
  where
   loop c# = 
    case indexCharOffAddr# fo c# of
     ch# | pred ch# -> loop (c# +# 1#)
	 | ch# `eqChar#` '\NUL'# && c# >=# l# -> StringBuffer fo l# s# c# -- EOB, return immediately.
         | otherwise     -> StringBuffer fo l# s# c#

scanNumLit :: Integer -> StringBuffer -> (Integer,StringBuffer)
scanNumLit acc (StringBuffer fo l# s# c#) =
 loop acc c#
  where
   loop acc c# = 
    case indexCharOffAddr# fo c# of
     ch# | isDigit (C# ch#) -> loop (acc*10 + (toInteger (I# (ord# ch# -# ord# '0'#)))) (c# +# 1#)
	 | ch# `eqChar#` '\NUL'# && c# >=# l# -> (acc, StringBuffer fo l# s# c#) -- EOB, return immediately.
         | otherwise        -> (acc,StringBuffer fo l# s# c#)


expandUntilMatch :: StringBuffer -> String -> Maybe StringBuffer
expandUntilMatch (StringBuffer fo l# s# c#) str =
  loop c# str
  where
   loop c# [] = Just (StringBuffer fo l# s# c#)
   loop c# ((C# x#):xs) =
    case indexCharOffAddr# fo c# of
      ch# | ch# `eqChar#` '\NUL'# && c# >=# l# -> Nothing
	  | ch# `eqChar#` x# -> loop (c# +# 1#) xs
          | otherwise        -> loop (c# +# 1#) str
	
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
   loop c# ((C# x#):xs)
     | indexCharOffAddr# fo c# `eqChar#` x#
     = loop (c# +# 1#) xs
     | otherwise
     = Nothing

untilEndOfString# :: StringBuffer -> StringBuffer
untilEndOfString# (StringBuffer fo l# s# c#) = 
 loop c# 
 where
  getch# i# = indexCharOffAddr# fo i#

  loop c# =
   case getch# c# of
    '\"'# ->
      case getch# (c# -# 1#) of
	'\\'# ->       
                  -- looks like an escaped something or other to me,
		  -- better count the number of "\\"s that are immediately
		  -- preceeding to decide if the " is escaped.
	      let
	       odd_slashes flg i# =
	        case getch# i# of
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


stepOnUntilChar# :: StringBuffer -> Char# -> StringBuffer
stepOnUntilChar# (StringBuffer fo l# s# c#) x# = 
 loop c# 
 where
  loop c#
   | c# >=# l# || indexCharOffAddr# fo c# `eqChar#` x#
   = StringBuffer fo l# c# c#
   | otherwise
   = loop (c# +# 1#)

         -- conversion
lexemeToString :: StringBuffer -> String
lexemeToString (StringBuffer fo len# start_pos# current#) = 
 if start_pos# ==# current# then
    ""
 else
    unpackCStringBA
	(copySubStr fo (I# start_pos#) (I# (current# -# start_pos#)))
	(I# len#)
    
lexemeToFastString :: StringBuffer -> FastString
lexemeToFastString (StringBuffer fo l# start_pos# current#) =
 if start_pos# ==# current# then
    mkFastString ""
 else
    mkFastSubString fo (I# start_pos#) (I# (current# -# start_pos#))
\end{code}
