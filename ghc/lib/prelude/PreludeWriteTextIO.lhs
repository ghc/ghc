%
% (c) The GRASP/AQUA Project, Glasgow University, 1994
%
\section[PrelWriteTextIO]{Haskell 1.3 Text Output}

This module defines the standard set of output operations for writing
characters and strings to text files, using 
$handles$

\begin{code}
module PreludeWriteTextIO (
    hPutChar,
    putChar,
    hPutStr,
    putStr,
    hPutText,
    putText,
    print13,
    writeFile13,
    appendFile13
  ) where

import Cls
import Core
import IChar
import IInt
import IList
import List		( splitAt, (++) )
import Prel		( ord, (.), otherwise )
import Text
import TyArray		-- instance _CCallable (_ByteArray a)
import TyComplex

import PreludeIOError
import PreludeMonadicIO
import PreludePrimIO
import PreludeGlaST
import PreludeStdIO
import PS

hPutChar :: Handle -> Char -> IO ()
hPutChar handle c =
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
      _ReadHandle _ _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is not open for writing")
      other -> 
	  _ccall_ filePutc (_filePtr other) (ord c) `thenPrimIO` \ rc ->
	  putMVar handle (_markHandle htype)	    >>
          if rc == 0 then
              return ()
          else
              _constructError			    `thenPrimIO` \ ioError ->
	      failWith ioError

putChar :: Char -> IO () 
putChar = hPutChar stdout13

\end{code}

Computation $hPutChar hdl c$ writes the character {\em c} to the file
or channel managed by {\em hdl}.  Characters may be buffered if
buffering is enabled for {\em hdl}.

\begin{code}
hPutStr :: Handle -> String -> IO ()
hPutStr handle str = 
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
      _ReadHandle _ _ _ ->
	  putMVar handle htype			    >>
	  failWith (IllegalOperation "handle is not open for writing")
      other -> 
          _getBufferMode other			    `thenPrimIO` \ other ->
          (case _bufferMode other of
            Just LineBuffering ->
		writeLines (_filePtr other) str
            Just (BlockBuffering (Just size)) ->
	        writeBlocks (_filePtr other) size str
            Just (BlockBuffering Nothing) ->
	        writeBlocks (_filePtr other) ``BUFSIZ'' str
            _ -> -- Nothing is treated pessimistically as NoBuffering
	        writeChars (_filePtr other) str
	  )    					    `thenPrimIO` \ success ->
	    putMVar handle (_markHandle other)	    `seqPrimIO`
          if success then
              return ()
          else
              _constructError			    `thenPrimIO` \ ioError ->
	      failWith ioError

  where

    writeBlocks :: _Addr -> Int -> String -> PrimIO Bool
    writeBlocks fp size "" = returnPrimIO True
    writeBlocks fp size s =
        let
	    (some, more) = splitAt size s
        in
	    _packBytesForCST some		    `thenPrimIO` 
              \ bytes@(_ByteArray (0, count) _) ->
	    _ccall_ writeFile bytes fp (count+1)    `thenPrimIO` \ rc ->
            if rc == 0 then
		writeBlocks fp size more
	    else
		returnPrimIO False

    writeLines :: _Addr -> String -> PrimIO Bool
    writeLines fp "" = returnPrimIO True
    writeLines fp s =
        let
	    (some, more) = breakLine s
        in
	    _packBytesForCST some		    `thenPrimIO` 
              \ bytes@(_ByteArray (0, count) _) ->
	    _ccall_ writeFile bytes fp (count+1)    `thenPrimIO` \ rc ->
            if rc == 0 then
		writeLines fp more
	    else
		returnPrimIO False
      where
        breakLine ""	= ("","")
        breakLine (x:xs)
	  | x == '\n'	= ([x],xs)
	  | otherwise	= let (ys,zs) = breakLine xs in (x:ys,zs)

    writeChars :: _Addr -> String -> PrimIO Bool
    writeChars fp "" = returnPrimIO True
    writeChars fp (c:cs) =
	_ccall_ filePutc fp (ord c)		    `thenPrimIO` \ rc ->
        if rc == 0 then
	    writeChars fp cs
	else
	    returnPrimIO False

putStr :: String -> IO () 
putStr = hPutStr stdout13

hPutText :: Text a => Handle -> a -> IO ()
hPutText hdl = hPutStr hdl . show

putText :: Text a => a -> IO () 
putText = hPutText stdout13

print13 :: Text a => a -> IO ()
print13 x = putText x >> putChar '\n'

\end{code}

Computation $hPutStr hdl s$ writes the string {\em s} to the file or
channel managed by {\em hdl}.

Computation $putStr s$ writes the string {\em s} to $stdout$.

Computation $hPutText hdl t$ writes the string representation of {\em
t} given by the $shows$ function to the file or channel managed by
{\em hdl}.

\begin{code}

writeFile13 :: FilePath -> String -> IO ()
writeFile13 name str =
 openFile name WriteMode >>= \hdl -> hPutStr hdl str >> hClose hdl

appendFile13 :: FilePath -> String -> IO ()
appendFile13 name str =
 openFile name AppendMode >>= \hdl -> hPutStr hdl str >> hClose hdl

\end{code}

$writeFile file s$ replaces the contents of {\em file} by the string
{\em s}. $appendFile file s$ appends string {\em s} to {\em file}.
