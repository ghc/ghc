%
% (c) The GRASP/AQUA Project, Glasgow University, 1994
%
\section[PrelReadTextIO]{Haskell 1.3 Text Input}

This module defines the standard set of input operations for reading
characters and strings from text files, using {\em handles}.

\begin{code}
module PreludeReadTextIO (
    hReady,
    hGetChar,
    getChar,
    hLookAhead,
    hGetContents,
    readFile13
  ) where

import Cls
import Core
import IChar
import IInt
import IList
import List		( (++) )
import Prel		( chr )
import Text
import TyArray
import TyComplex

import PreludeIOError
import PreludeMonadicIO
import PreludePrimIO
import PreludeGlaST
import PreludeStdIO
import PS

---------------------------------
infixr 1 `my_then`

my_then	:: IO a -> (a -> PrimIO b) -> PrimIO b
{-# INLINE my_then   #-}

my_then m k = m `thenPrimIO` \ r -> k' r
  where
    k' (Right x)  = k x
    k' (Left err) = error "my_then"
---------------------------------


hReady :: Handle -> IO Bool 
hReady handle = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
          failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _AppendHandle _ _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is not open for reading")
      _WriteHandle _ _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is not open for reading")
      other -> 
	  _ccall_ inputReady (_filePtr other)  	    `thenPrimIO` \ rc ->
	  putMVar handle (_markHandle htype)	    >>
          case rc of
            0 -> return False
            1 -> return True
            _ -> _constructError		    `thenPrimIO` \ ioError ->
		 failWith ioError

\end{code}

Computation $hReady hdl$ indicates whether at least
one item is available for input from handle {\em hdl}.

\begin{code}

hGetChar :: Handle -> IO Char
hGetChar handle = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
          failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _AppendHandle _ _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is not open for reading")
      _WriteHandle _ _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is not open for reading")
      other -> 
	  _ccall_ fileGetc (_filePtr other)  	    `thenPrimIO` \ intc ->
	  putMVar handle (_markHandle htype)	    >>
          if intc /= ``EOF'' then
              return (chr intc)
          else
              _constructError			    `thenPrimIO` \ ioError ->
	      failWith ioError

getChar :: IO Char
getChar = hGetChar stdin13

\end{code}

Computation $hGetChar hdl$ reads the next character from handle {\em
hdl}, blocking until a character is available.

$getChar$ reads the next character from $stdin$.

\begin{code}

hLookAhead :: Handle -> IO Char
hLookAhead handle = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
          failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _AppendHandle _ _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is not open for reading")
      _WriteHandle _ _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is not open for reading")
      other -> 
	  _ccall_ fileLookAhead (_filePtr other)    `thenPrimIO` \ intc ->
	  putMVar handle (_markHandle htype)	    >>
          if intc /= ``EOF'' then
              return (chr intc)
          else
              _constructError			    `thenPrimIO` \ ioError ->
	      failWith ioError

\end{code}

Computation $hLookahead hdl$ returns the next character from handle
{\em hdl} without removing it from the input buffer, blocking until a
character is available.

\begin{code}

hGetContents :: Handle -> IO String
hGetContents handle = 
    takeMVar handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  putMVar handle htype			    >>
          failWith ioError
      _ClosedHandle ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is closed")
      _AppendHandle _ _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is not open for reading")
      _WriteHandle _ _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is not open for reading")
      other -> 

	  {- 
             To avoid introducing an extra layer of buffering here,
             we provide three lazy read methods, based on character,
             line, and block buffering.
          -}

          _getBufferMode other			    `thenPrimIO` \ other ->
          case _bufferMode other of
            Just LineBuffering ->
		allocBuf Nothing		    >>= \ buf_info ->
	        putMVar handle (_SemiClosedHandle (_filePtr other) buf_info)
                                          	    >>
                unsafeInterleavePrimIO (lazyReadLine handle)
						    `thenPrimIO` \ contents ->
	        return contents

            Just (BlockBuffering size) ->
		allocBuf size			    >>= \ buf_info ->
	        putMVar handle (_SemiClosedHandle (_filePtr other) buf_info)
                                          	    >>
                unsafeInterleavePrimIO (lazyReadBlock handle)
						    `thenPrimIO` \ contents ->
	        return contents
            _ -> -- Nothing is treated pessimistically as NoBuffering
	        putMVar handle (_SemiClosedHandle (_filePtr other) (``NULL'', 0))
                                          	    >>
                unsafeInterleavePrimIO (lazyReadChar handle)
						    `thenPrimIO` \ contents ->
	        return contents
  where
    allocBuf :: (Maybe Int) -> IO (_Addr, Int)
    allocBuf msize =
	_ccall_ malloc size		    	    `thenPrimIO` \ buf ->
	if buf /= ``NULL'' then
	    return (buf, size)
	else
	    failWith (ResourceExhausted "not enough virtual memory")
      where
        size = 
	    case msize of
	      Just x -> x
	      Nothing -> ``BUFSIZ''

{-
   Note that someone may yank our handle out from under us, and then re-use
   the same FILE * for something else.  Therefore, we have to re-examine the
   handle every time through.
-}

lazyReadBlock :: Handle -> PrimIO String
lazyReadBlock handle =
    takeMVar handle				    `my_then` \ htype ->
    case htype of 
      -- There cannae be an _ErrorHandle here
      _ClosedHandle ->
	  putMVar handle htype			    `seqPrimIO`
	  returnPrimIO ""
      _SemiClosedHandle fp (buf, size) ->
	  _ccall_ readBlock buf fp size		    `thenPrimIO` \ bytes ->
          (if bytes <= 0 then returnStrictlyST _nilPS
           else _packCBytesST bytes buf)	    `thenStrictlyST` \ some ->
          if bytes < 0 then
	      putMVar handle (_SemiClosedHandle ``NULL'' (``NULL'', 0))
						    `seqPrimIO`
              _ccall_ free buf			    `thenPrimIO` \ () ->
              _ccall_ closeFile fp	            `seqPrimIO`
	      returnPrimIO (_unpackPS some)
	  else
	      putMVar handle htype		    `seqPrimIO`
              unsafeInterleavePrimIO (lazyReadBlock handle)
						    `thenPrimIO` \ more ->
	      returnPrimIO (_unpackPS some ++ more)

lazyReadLine :: Handle -> PrimIO String
lazyReadLine handle =
    takeMVar handle				    `my_then` \ htype ->
    case htype of 
      -- There cannae be an _ErrorHandle here
      _ClosedHandle ->
	  putMVar handle htype			    `seqPrimIO`
	  returnPrimIO ""
      _SemiClosedHandle fp (buf, size) ->
	  _ccall_ readLine buf fp size		    `thenPrimIO` \ bytes ->
          (if bytes <= 0 then returnStrictlyST _nilPS
           else _packCBytesST bytes buf)	    `thenStrictlyST` \ some ->
          if bytes < 0 then
	      putMVar handle (_SemiClosedHandle ``NULL'' (``NULL'', 0))
						    `seqPrimIO`
              _ccall_ free buf			    `thenPrimIO` \ () ->
              _ccall_ closeFile fp	            `seqPrimIO`
	      returnPrimIO (_unpackPS some)
	  else
	      putMVar handle htype		    `seqPrimIO`
              unsafeInterleavePrimIO (lazyReadLine handle)
						    `thenPrimIO` \ more ->
	      returnPrimIO (_unpackPS some ++ more)

lazyReadChar :: Handle -> PrimIO String
lazyReadChar handle =
    takeMVar handle				    `my_then` \ htype ->
    case htype of 
      -- There cannae be an _ErrorHandle here
      _ClosedHandle ->
	  putMVar handle htype			    `seqPrimIO`
	  returnPrimIO ""
      _SemiClosedHandle fp buf_info ->
	  _ccall_ readChar fp			    `thenPrimIO` \ char ->
          if char == ``EOF'' then
	      putMVar handle (_SemiClosedHandle ``NULL'' buf_info)
						    `seqPrimIO`
              _ccall_ closeFile fp	            `seqPrimIO`
	      returnPrimIO ""
	  else
	      putMVar handle htype		    `seqPrimIO`
              unsafeInterleavePrimIO (lazyReadChar handle)
						    `thenPrimIO` \ more ->
	      returnPrimIO (chr char : more)

\end{code}

Computation $hGetContents hdl$ returns the list of characters
corresponding to the unread portion of the channel or file managed by
{\em hdl}, which is made semi-closed.

\begin{code}

readFile13 :: FilePath -> IO String
readFile13 name = openFile name ReadMode >>= hGetContents

\end{code}

$readFile file$ returns the contents of {\em file} as a lazy string.
