%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[MainMonad]{I/O monad used in @Main@ module of the compiler}

\begin{code}
#include "HsVersions.h"

module MainMonad (
	MainIO(..),
#ifndef __GLASGOW_HASKELL__
	mainIOtoDialogue,
	appendFileMn,
#endif
	returnMn,
	thenMn,
	thenMn_,
--	foldlMn, INLINEd at its two (important) uses...
	readMn,
	writeMn,
	getArgsMn,
	getSplitUniqSupplyMn,
	exitMn,
#if __GLASGOW_HASKELL__ >= 23
	fopen, fclose, fwrite, _FILE(..),
#endif

	SplitUniqSupply
	IF_ATTACK_PRAGMAS(COMMA getArgsPrimIO)
	IF_ATTACK_PRAGMAS(COMMA appendFilePrimIO)
	IF_ATTACK_PRAGMAS(COMMA appendChanPrimIO)
	IF_ATTACK_PRAGMAS(COMMA readChanPrimIO)
	IF_ATTACK_PRAGMAS(COMMA mkSplitUniqSupply) -- profiling only, really
    ) where

#ifdef __GLASGOW_HASKELL__

# if __GLASGOW_HASKELL__ < 26
import PreludePrimIO
# endif
import PreludeGlaST

#endif

import SplitUniq
import Outputable
import Util

infixr 9 `thenMn`	-- right-associative, please
infixr 9 `thenMn_`
\end{code}

For Glasgow Haskell, we'll eventually be able to use the underlying
Glasgow I/O {\em directly}.  However, for now we do the business
through regular a @Dialogue@.

A value of type @MainIO a@ represents an I/O-performing computation
returning a value of type @a@.  It is a function from the whole list
of responses-to-the-rest-of-the-program, to a triple consisting of:
\begin{enumerate}
\item
the value of type @a@;
\item
a function which prefixes the requests for the computation to
the front of a supplied list of requests; using a function here
avoids an expensive append operation in @thenMn@;
\item
the depleted list of responses.
\end{enumerate}

\begin{code}
returnMn    :: a -> MainIO a
thenMn	    :: MainIO a -> (a -> MainIO b) -> MainIO b
thenMn_	    :: MainIO a -> MainIO b -> MainIO b
--foldlMn	    :: (a -> b -> MainIO a) -> a -> [b] -> MainIO a

readMn	    :: String{-channel-} -> MainIO String
writeMn	    :: String{-channel-} -> String -> MainIO ()
#ifndef __GLASGOW_HASKELL__
appendFileMn:: String{-filename-} -> String -> MainIO ()
#endif
getArgsMn   :: MainIO [String]
getSplitUniqSupplyMn :: Char -> MainIO SplitUniqSupply
exitMn	    :: Int -> MainIO ()

#ifdef __GLASGOW_HASKELL__
{-# INLINE returnMn #-}
{-# INLINE thenMn   #-}
{-# INLINE thenMn_  #-}
#endif

{- INLINEd at its uses
foldlMn f z []     = returnMn z
foldlMn f z (x:xs) = f z x	`thenMn` \ zz ->
		     foldlMn f zz xs
-}

exitMn val
  = -- trace ("exitMn:"++(show val)) (
    if val /= 0
    then error "Compilation had errors\n"
    else returnMn ()
    -- )

#ifdef __GLASGOW_HASKELL__

type MainIO a = PrimIO a

returnMn    = returnPrimIO
thenMn	    = thenPrimIO
thenMn_	    = seqPrimIO

readMn chan		    = readChanPrimIO chan
writeMn chan str	    = appendChanPrimIO chan str
getArgsMn		    = getArgsPrimIO

getSplitUniqSupplyMn char = mkSplitUniqSupply char
\end{code}

\begin{code}
#else {- ! __GLASGOW_HASKELL -}

type MainIO a = (a -> Dialogue) -> Dialogue

-- returnMn :: x -> MainIO x
returnMn x cont = cont x

-- thenMn :: MainIO a -> (a -> MainIO b) -> MainIO b
thenMn m k cont = m (\ a -> k a cont)

-- thenMn_ :: MainIO a -> MainIO b -> MainIO b
thenMn_ m k cont = m (\ _ -> k cont)
\end{code}

\begin{code}
mainIOtoDialogue :: MainIO () -> Dialogue

mainIOtoDialogue io = io (\ _ _ -> [])

readMn chan		= readChanIO chan
writeMn chan str	= appendChanIO chan str
appendFileMn fname str	= appendFileIO fname str
getArgsMn		= getArgsIO

getSplitUniqSupplyMn char = returnMn (mkSplitUniqSupply char)
\end{code}

\begin{code}
processRequestIO   :: Request -> MainIO Response
processRequestIO req cont ~(resp:resps) = req : cont resp resps

doneIO :: MainIO a
doneIO cont = \ _ -> []

data IoResult a = IoSucc a
                | IoFail IOError

type IOE a = MainIO (IoResult a)         

processRequestIOUnit :: Request -> IOE ()
processRequestIOUnit req =
        processRequestIO req                           `thenMn` \ resp -> 
        case resp of
          Success       -> returnMn (IoSucc ())
          Str str       -> error "funny Response, expected a Success"
          StrList strl  -> error "funny Response, expected a Success" 
          Failure ioerr -> returnMn (IoFail ioerr)

processRequestIOString :: Request -> IOE String
processRequestIOString req =
        processRequestIO req                           `thenMn` \ resp -> 
        case resp of
          Success       -> error "funny Response, expected a String"
          Str str       -> returnMn (IoSucc str)
          StrList strl  -> error "funny Response, expected a String" 
          Failure ioerr -> returnMn (IoFail ioerr)

processRequestIOStringList :: Request -> IOE [String]
processRequestIOStringList req =
        processRequestIO req                           `thenMn` \ resp -> 
        case resp of
          Success       -> error "funny Response, expected a [String]"
          Str str       -> error "funny Response, expected a [String]" 
          StrList strl  -> returnMn (IoSucc strl)
          Failure ioerr -> returnMn (IoFail ioerr)

readFileIOE     :: String ->           IOE String
writeFileIOE    :: String -> String -> IOE ()
appendFileIOE   :: String -> String -> IOE ()
deleteFileIOE   :: String ->           IOE ()
statusFileIOE   :: String ->           IOE String
readChanIOE     :: String ->           IOE String
appendChanIOE   :: String -> String -> IOE ()
statusChanIOE   :: String ->           IOE String
echoIOE         :: Bool   ->           IOE ()
getArgsIOE      ::                     IOE [String]
getEnvIOE       :: String ->           IOE String
setEnvIOE       :: String -> String -> IOE ()
sigActionIOE    :: Int    -> SigAct -> IOE ()

readFileIOE    file     = processRequestIOString     ( ReadFile file )
writeFileIOE   file str = processRequestIOUnit       ( WriteFile file str )
appendFileIOE  file str = processRequestIOUnit       ( AppendFile file str )
deleteFileIOE  file     = processRequestIOUnit       ( DeleteFile file )
statusFileIOE  file     = processRequestIOString     ( StatusFile file )
readChanIOE    chan     = processRequestIOString     ( ReadChan chan )
appendChanIOE  chan str = processRequestIOUnit       ( AppendChan chan str )
statusChanIOE  chan     = processRequestIOString     ( StatusChan chan )
echoIOE        bool     = processRequestIOUnit       ( Echo bool )
getArgsIOE              = processRequestIOStringList ( GetArgs )
getEnvIOE      var      = processRequestIOString     ( GetEnv var )
setEnvIOE      var obj  = processRequestIOUnit       ( SetEnv var obj )
sigActionIOE   sig act  = processRequestIOUnit       ( SigAction sig act )

handleErrIO :: IoResult a -> MainIO a 
handleErrIO (IoSucc a)     = returnMn a
handleErrIO (IoFail ioerr) = exitIO   ioerr

readFileIO      :: String ->           MainIO String
writeFileIO     :: String -> String -> MainIO ()
appendFileIO    :: String -> String -> MainIO ()
deleteFileIO    :: String ->           MainIO ()
statusFileIO    :: String ->           MainIO String
readChanIO      :: String ->           MainIO String
appendChanIO    :: String -> String -> MainIO ()
statusChanIO    :: String ->           MainIO String
echoIO          :: Bool   ->           MainIO ()
getArgsIO       ::                     MainIO [String]
getEnvIO        :: String ->           MainIO String
setEnvIO        :: String -> String -> MainIO ()
sigActionIO     :: Int    -> SigAct -> MainIO ()

readFileIO      file       = readFileIOE file           `thenMn` handleErrIO
writeFileIO     file str   = writeFileIOE file str      `thenMn` handleErrIO
appendFileIO    file str   = appendFileIOE file str     `thenMn` handleErrIO
deleteFileIO    file       = deleteFileIOE file         `thenMn` handleErrIO
statusFileIO    file       = statusFileIOE file         `thenMn` handleErrIO
readChanIO      chan       = readChanIOE chan           `thenMn` handleErrIO
appendChanIO    chan str   = appendChanIOE chan str     `thenMn` handleErrIO
statusChanIO    chan       = statusChanIOE chan         `thenMn` handleErrIO
echoIO          bool       = echoIOE bool               `thenMn` handleErrIO
getArgsIO                  = getArgsIOE                 `thenMn` handleErrIO
getEnvIO        var        = getEnvIOE var              `thenMn` handleErrIO
setEnvIO        var obj    = setEnvIOE var obj          `thenMn` handleErrIO
sigActionIO     sig act    = sigActionIOE sig act       `thenMn` handleErrIO

exitIO     :: IOError -> MainIO a

exitIO (ReadError s)   = error s
exitIO (WriteError s)  = error s
exitIO (SearchError s) = error s
exitIO (FormatError s) = error s
exitIO (OtherError s)  = error s
\end{code}

\begin{code}
#endif {- ! __GLASGOW_HASKELL -}
\end{code}
