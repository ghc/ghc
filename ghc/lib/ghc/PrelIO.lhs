%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelIO]{Module @PrelIO@}

Input/output functions mandated by the standard Prelude.

\begin{code}
module PrelIO (
	IO, FilePath, IOError, 
	fail, userError, catch,
	putChar, putStr, putStrLn, print,
	getChar, getLine, getContents, interact,
	readFile, writeFile, appendFile, readIO, readLn
    ) where

import Prelude	()
import IO
import IOHandle
import IOBase
import PrelBase
import PrelRead

\end{code}

\begin{code}
putChar         :: Char -> IO ()
putChar c       =  hPutChar stdout c

putStr          :: String -> IO ()
putStr s        =  hPutStr stdout s

putStrLn        :: String -> IO ()
putStrLn s      =  do putStr s
                      putChar '\n'

print           :: Show a => a -> IO ()
print x         =  putStrLn (show x)

getChar         :: IO Char
getChar         =  hGetChar stdin

getLine         :: IO String
getLine         =  do c <- getChar
                      if c == '\n' then return "" else 
                         do s <- getLine
                            return (c:s)
            
getContents     :: IO String
getContents     =  hGetContents stdin

interact        ::  (String -> String) -> IO ()
interact f      =   do s <- getContents
                       putStr (f s)

readFile        :: FilePath -> IO String
readFile name	=  openFile name ReadMode >>= hGetContents

writeFile       :: FilePath -> String -> IO ()
writeFile name str
  = openFile name WriteMode >>= \hdl -> hPutStr hdl str >> hClose hdl

appendFile      :: FilePath -> String -> IO ()
appendFile name str
  = openFile name AppendMode >>= \hdl -> hPutStr hdl str >> hClose hdl

readIO          :: Read a => String -> IO a
  -- raises an exception instead of an error
readIO s        =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                        [x] -> return x
                        []  -> fail (userError "PreludeIO.readIO: no parse")
                        _   -> fail (userError 
                                      "PreludeIO.readIO: ambiguous parse")

readLn          :: Read a => IO a
readLn          =  do l <- getLine
                      r <- readIO l
                      return r
\end{code}
