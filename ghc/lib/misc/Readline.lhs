%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[Readline]{GNU Readline Library Bindings}

This module attempts to provide a better line based editing facility
for Haskell programmers by providing access to the GNU Readline
library.  Related to this are bindings for the GNU History library
which can be found in History.


\begin{code}
{-# OPTIONS -#include "cbits/ghcReadline.h" #-}

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

    rlPrompt, rlTerminalName, rlSetReadlineName, rlGetReadlineName

    ) where
import GlaExts

import CString	( unpackCStringIO )
import Foreign

import System

--#include <readline/readline.h>
     
type KeyCode = Int

type RlCallbackFunction = 
    (Int ->			-- Numeric Argument
     KeyCode ->			-- KeyCode of pressed Key
     IO Int)
\end{code}

%***************************************************************************
%*                                                                         *
\subsection[Readline-Functions]{Main Readline Functions}
%*                                                                         *
%***************************************************************************
\begin{code}

readline :: String ->			-- Prompt String
	    IO String			-- Returned line
readline prompt =  do
--ToDo: Get the "Live register in _casm_GC_ " bug fixed
--      this stops us passing the prompt string to readline directly :-(
--    litstr <- _casm_GC_ ``%r = readline(%0);'' prompt
    _casm_ ``rl_prompt_hack = (char*)realloc(rl_prompt_hack, %1);
	     strcpy (rl_prompt_hack,%0);'' 
		prompt	(length prompt)
    litstr <- _casm_GC_ ``%r = readline (rl_prompt_hack);''
    if (litstr == ``NULL'') 
     then fail (userError "Readline has read EOF")
     else do
	str <- unpackCStringIO litstr
	_ccall_ free litstr
	return str

addHistory :: String		-- String to enter in history
           -> IO ()
addHistory str = _ccall_ add_history str


rlBindKey :: KeyCode		    -- Key to Bind to
	  -> RlCallbackFunction	    -- Function to exec on execution
	  -> IO ()
rlBindKey key cback =
    if (0 > key) || (key > 255) then
	fail (userError "Invalid ASCII Key Code, must be in range 0.255")
    else  do
	addCbackEntry (key,cback)
	_casm_ `` rl_bind_key((KeyCode)%0,&genericRlCback); '' key

\end{code}

i.e. add the (KeyCode,RlCallbackFunction) key to the assoc. list and register
the generic callback for this KeyCode.

The entry point that $genericRlCback$ calls would then read the
global variables $current\_i$ and $current\_kc$ and do a lookup:

\begin{code}
rlAddDefun :: String ->			-- Function Name
	      RlCallbackFunction ->	-- Function to call
	      KeyCode ->		-- Key to bind to, or -1 for no bind
	      IO ()
rlAddDefun name cback key =
    if (0 > key) || (key > 255) then
	fail (userError "Invalid ASCII Key Code, must be in range 0..255")
    else do
	addCbackEntry (key, cback)
	_casm_ ``rl_add_defun (%0, &genericRlCback, (KeyCode)%1);'' name key

\end{code}


The C function $genericRlCallback$ puts the callback arguments into
global variables and enters the Haskell world through the
$haskellRlEntry$ function. Before exiting, the Haskell function will
deposit its result in the global varariable $rl\_return$.

In the Haskell action that is invoked via $enterStablePtr$, a match
between the Keycode in $current\_kc$ and the Haskell callback needs to
be made. To essentially keep the same assoc. list of (KeyCode,cback
function) as Readline does, we make use of yet another global variable
$cbackList$:

\begin{code}

createCbackList :: [(KeyCode,RlCallbackFunction)] -> PrimIO ()
createCbackList ls = 
#ifndef __PARALLEL_HASKELL__
    makeStablePtr ls  >>= \ stable_ls ->
    _casm_ `` cbackList=(StgStablePtr)%0; '' stable_ls
#else
    error "createCbackList: not available for Parallel Haskell"
#endif

getCbackList :: PrimIO [(KeyCode,RlCallbackFunction)]
getCbackList = 
#ifndef __PARALLEL_HASKELL__
    _casm_ `` %r=(StgStablePtr)cbackList; '' >>= \ stable_ls ->
    deRefStablePtr stable_ls
#else
    error "getCbackList: not available for Parallel Haskell"
#endif

setCbackList :: [(KeyCode,RlCallbackFunction)] -> PrimIO ()
setCbackList ls =
#ifndef __PARALLEL_HASKELL__
    _casm_ `` %r=(StgStablePtr)cbackList; '' >>= \ old_stable_ls ->   
    freeStablePtr old_stable_ls              >>
    createCbackList ls
#else
    error "setCbackList: not available for Parallel Haskell"
#endif

addCbackEntry :: (KeyCode,RlCallbackFunction) -> IO ()
addCbackEntry entry = do
    ls <- getCbackList
    setCbackList (entry:ls)
\end{code}

The above functions allows us to query and augment the assoc. list in
Haskell.

\begin{code}

invokeRlCback :: IO ()
invokeRlCback = do
    kc    <- _casm_ `` %r=(KeyCode)current_kc; ''
    narg  <- _casm_ `` %r=(int)current_narg; ''
    ls    <- getCbackList
    ret_val <- 
      (case (dropWhile (\ (key,_) -> kc/=key) ls) of
	 [] -> return (-1)
	 ((_,cback):_) -> cback narg kc
      )
    _casm_ `` rl_return=(int)%0; '' ret_val

\end{code}
 
Finally, we need to initialise this whole, ugly machinery:

\begin{code}
initRlCbacks :: PrimIO ()

initRlCbacks =
#ifndef __PARALLEL_HASKELL__
    createCbackList []             >>
    makeStablePtr (invokeRlCback)  >>= \ stable_f ->
    _casm_ `` haskellRlEntry=(StgStablePtr)%0; '' stable_f >>= \ () ->
    return ()
#else
    error "initRlCbacks: not available for Parallel Haskell"
#endif
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
rlGetLineBuffer = do
    litstr <- _casm_ ``%r = rl_line_buffer;''
    unpackCStringIO litstr
				
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
rlSetDone True  = _casm_ ``rl_done = %0;'' 1
rlSetDone False = _casm_ ``rl_done = %0;'' 0

rlPendingInput :: KeyCode -> IO ()
rlPendingInput key = primIOToIO (_casm_ ``rl_pending_input = %0;'' key)

rlPrompt :: IO String
rlPrompt = do
    litstr <- _casm_ ``%r = rl_readline_name;''
    unpackCStringIO litstr

rlTerminalName :: IO String
rlTerminalName = do
    litstr <- _casm_ ``%r = rl_terminal_name;''
    unpackCStringIO litstr


rlGetReadlineName :: IO String
rlGetReadlineName = do
    litstr <- _casm_ ``%r = rl_readline_name;''
    unpackCStringIO litstr

rlSetReadlineName :: String -> IO ()
rlSetReadlineName str = _casm_ ``rl_readline_name = %0;'' str
\end{code}

\begin{verbatim}
--
-- The following two were taken from PreludeStdIO stdin/stdout
--
rlInStream :: Handle
rlInStream = unsafePerformPrimIO (
    newMVar						>>= \ handle ->
    _ccall_ getLock (``rl_instream''::Addr) 0		>>= \ rc ->
    (case rc of
       0 -> putMVar handle ClosedHandle
       1 -> putMVar handle (ReadHandle ``rl_instream'' Nothing False)
       _ -> constructError				>>= \ ioError -> 
            putMVar handle (ErrorHandle ioError)
    )							>>
    returnPrimIO handle
  )


rlOutStream :: Handle
rlOutStream = unsafePerformPrimIO (
    newMVar						>>= \ handle ->
    _ccall_ getLock (``rl_outstream''::Addr) 1		>>= \ rc ->
    (case rc of
       0 -> putMVar handle ClosedHandle
       1 -> putMVar handle (WriteHandle ``rl_outstream'' Nothing False)
       _ -> constructError				>>= \ ioError -> 
            putMVar handle (ErrorHandle ioError)
    )							>>
    returnPrimIO handle
  )

\end{verbatim}
   

\begin{code}

-- rlStartupHook :: RlCallBackFunction -> IO ()	     

rlInitialize :: IO ()
rlInitialize = do
    pname <- getProgName
    rlSetReadlineName pname
    _casm_ ``rl_prompt_hack = (char*)malloc(1);''
    initRlCbacks

\end{code}
