%
% (c) The GRASP/AQUA Project, Glasgow University, 1994
%
\section[PrelIO]{Top Level I/O}

This module collects and exports the more primitive Prelude modules. 

\begin{code}

module PreludeIO (
    (>>),
    (>>=),
    accumulate,
    appendFile13,
    either,
    fail,
    failWith,
    getChar,
    hClose,
    hFileSize,
    hFlush,
    hGetChar,
    hGetContents,
    hGetPosn,
    hIsBlockBuffered,
    hIsClosed,
    hIsEOF,
    hIsLineBuffered,
    hIsNotBuffered,
    hIsOpen,
    hIsReadable,
    hIsSeekable,
    hIsWritable,
    hLookAhead,
    hPutChar,
    hPutStr,
    hPutText,
    hReady,
    hSeek,
    hSetBuffering,
    hSetPosn,
    handle,
    interact13,
    isEOF,
    openFile,
    putChar,
    putStr,
    putText,
    print13,
    readFile13,
    return,
    sequence,
    stderr13,
    stdin13,
    stdout13,
    try,
    writeFile13,
    IOError13(..),
    Either(..),
    BufferMode(..),
    IOMode(..),
    SeekMode(..),
    Maybe(..),
    FilePath(..),
    Handle(..),
    HandlePosn(..),
    IO(..),
    _Handle,
    _MVar
 ) where

import Cls
import Core
import IChar
import IInt
import IList
import List		( (++) )
import PS
import Prel		( (.) )
import Text
import TyArray
import TyComplex

import PreludeGlaST
import PreludePrimIO	( _MVar )
import PreludeIOError
import PreludeMonadicIO
import PreludeStdIO
import PreludeReadTextIO
import PreludeWriteTextIO

\end{code}

The interact computation supports classical Landin-stream character
I/O, as in Haskell 1.2.

\begin{code}

interact13 :: (String -> String) -> IO ()
interact13 f = hGetContents stdin13 >>= (putStr . f)

\end{code}
