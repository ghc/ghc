%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[Readline]{GNU Readline Library Bindings}

This module attempts to provide a better line based editing facility
for Haskell programmers by providing access to the GNU Readline
library.  Related to this are bindings for the GNU History library
which can be found in History (at some point in the future :-).

Original version by Darren Moffat
Heavily modified in 1999 by Sven Panne <Sven.Panne@informatik.uni-muenchen.de>

Notes:

   * This binding is still *very* incomplete...  Volunteers?

   * The GHC User's Guide section on Readline is not up-to-date anymore,
     the flags you need are: -syslib misc -syslib posix -lreadline -ltermcap
     (or -lncurses on some Linux systems)

\begin{code}
{-# OPTIONS -#include <readline/readline.h> -#include <readline/history.h> #-}

module Readline (
    rlInitialize,
    readline, addHistory,
	
    rlBindKey, rlAddDefun,
    RlCallbackFunction,

    rlGetLineBuffer, rlSetLineBuffer,
    rlGetPoint, rlSetPoint,
    rlGetEnd, rlSetEnd,
    rlGetMark, rlSetMark,
    rlSetDone,
    rlPendingInput,

    rlPrompt, rlTerminalName,
    rlGetReadlineName, rlSetReadlineName,

    rlInStream, rlOutStream
    ) where

import Addr(Addr)
import ByteArray(ByteArray)
import Char(ord, chr)
import CString(packString, unpackCStringIO)
import IO(Handle)
import IOExts(IORef, newIORef, readIORef, writeIORef, unsafePerformIO, freeHaskellFunctionPtr)
import Maybe(fromMaybe)
import Monad(when)
import Posix(intToFd, fdToHandle)
import System(getProgName)

-- SUP: Haskell has closures and I've got no clue about the return value,
--      so a better type for the callbacks is probably
--      Int {- Numeric Arg -} -> IO ()

type KeyCode = Char

type RlCallbackFunction = 
    (Int ->			-- Numeric Argument
     KeyCode ->			-- KeyCode of pressed Key
     IO Int)                    -- What's this?
\end{code}

%***************************************************************************
%*                                                                         *
\subsection[Readline-Functions]{Main Readline Functions}
%*                                                                         *
%***************************************************************************
\begin{code}

rlInitialize :: IO ()
rlInitialize = rlSetReadlineName =<< getProgName

foreign import "free"     unsafe free        :: Addr -> IO ()
foreign import "readline" unsafe readlineAux :: ByteArray Int -> IO Addr

readline :: String		-- Prompt String
	 -> IO (Maybe String)	-- Just returned line or Nothing if EOF
readline prompt =  do
   cstr <- readlineAux (packString prompt)
   if cstr == ``NULL''
      then return Nothing
      else do str <- unpackCStringIO cstr
              free cstr
              return (Just str)

foreign import "add_history" unsafe add_history :: ByteArray Int -> IO ()

addHistory :: String		-- String to enter in history
           -> IO ()
addHistory = add_history . packString


foreign export dynamic mkRlCallback :: (Int -> Int -> IO Int) -> IO Addr
foreign import "rl_bind_key" rl_bind_key :: Int -> Addr -> IO Int

rlBindKey :: KeyCode		    -- Key to Bind to
	  -> RlCallbackFunction	    -- Function to exec on execution
	  -> IO ()
rlBindKey key cback = do
   cbAddr <- mkRlCallback (\n k -> cback n (chr k))
   ok     <- rl_bind_key (ord key) cbAddr
   if ok /= 0 then wrongKeyCode else addCbackEntry key cbAddr

foreign import "rl_add_defun" unsafe rl_add_defun :: ByteArray Int -> Addr -> Int -> IO Int

rlAddDefun :: String ->			-- Function Name
	      RlCallbackFunction ->	-- Function to call
	      Maybe KeyCode ->		-- Key to bind to
	      IO ()
rlAddDefun name cback mbKey = do
   cbAddr <- mkRlCallback (\n k -> cback n (chr k))
   ok     <- rl_add_defun (packString name) cbAddr (maybe (-1) ord mbKey)
   when (ok /= 0) wrongKeyCode

-- Don't know how this should ever happen with KeyCode = Char
wrongKeyCode :: IO ()
wrongKeyCode = ioError (userError "Invalid ASCII Key Code, must be in range 0..255")

-- Global hacking for freeing callbacks

theCbackTable :: IORef [(KeyCode,Addr)]
theCbackTable = unsafePerformIO (newIORef [])

addCbackEntry :: KeyCode -> Addr -> IO ()
addCbackEntry key cbAddr = do
   cbackTable <- readIORef theCbackTable
   maybe (return ()) freeHaskellFunctionPtr (lookup key cbackTable)
   writeIORef theCbackTable
              ((key,cbAddr) : [ entry | entry@(k,_) <- cbackTable, k /= key ])

\end{code}


%***************************************************************************
%*                                                                         *
\subsection[Readline-Globals]{Global Readline Variables}
%*                                                                         *
%***************************************************************************

These are the global variables required by the readline lib. Need to
find a way of making these read/write from the Haskell side.  Should
they be in the IO Monad, should they be Mutable Variables?

\begin{code}

rlGetLineBuffer :: IO String
rlGetLineBuffer = unpackCStringIO =<< _casm_ ``%r = rl_line_buffer;''
				
rlSetLineBuffer :: String -> IO ()
rlSetLineBuffer str = _casm_ ``rl_line_buffer = %0;'' str
		
rlGetPoint :: IO Int
rlGetPoint = _casm_ ``%r = rl_point;''

rlSetPoint :: Int -> IO ()
rlSetPoint point = _casm_ ``rl_point = %0;'' point
	 
rlGetEnd :: IO Int
rlGetEnd = _casm_ ``%r = rl_end;''

rlSetEnd :: Int -> IO ()
rlSetEnd end = _casm_ ``rl_end = %0;'' end

rlGetMark :: IO Int
rlGetMark = _casm_ ``%r = rl_mark;''

rlSetMark :: Int -> IO ()
rlSetMark mark = _casm_ ``rl_mark = %0;'' mark

rlSetDone :: Bool -> IO ()
rlSetDone False = _casm_ ``rl_done = %0;'' (0::Int)
rlSetDone True  = _casm_ ``rl_done = %0;'' (1::Int)

rlPendingInput :: KeyCode -> IO ()
rlPendingInput key = _casm_ ``rl_pending_input = %0;'' key

rlPrompt :: IO String
rlPrompt = unpackCStringIO =<<  _casm_ ``%r = rl_readline_name;''

rlTerminalName :: IO String
rlTerminalName = unpackCStringIO =<< _casm_ ``%r = rl_terminal_name;''

rlGetReadlineName :: IO String
rlGetReadlineName = unpackCStringIO =<< _casm_ ``%r = rl_readline_name;''

rlSetReadlineName :: String -> IO ()
rlSetReadlineName str = _casm_ ``rl_readline_name = %0;'' str

rlInStream :: Handle
rlInStream  = unsafePerformIO (fdToHandle (intToFd ``fileno(rl_instream)''))

rlOutStream :: Handle
rlOutStream = unsafePerformIO (fdToHandle (intToFd ``fileno(rl_outstream)''))

\end{code}

A simple test:

main :: IO ()
main = do rlInitialize
          rlBindKey '\^X' (\nargc kc -> do print (nargc,kc); return 0)
          loop
   where loop = maybe (putStrLn "Qapla'!")
                      (\reply -> do unless (null reply) (addHistory reply)
                                    putStrLn (reply ++ "...   pItlh!")
                                    loop) =<< readline "nuqneH, ghunwI'? "
