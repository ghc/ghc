%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[MainMonad]{I/O monad used in @Main@ module of the compiler}

\begin{code}
#include "HsVersions.h"

module MainMonad (
	MainIO(..),
	returnMn,
	thenMn,
	thenMn_,
--	foldlMn, INLINEd at its two (important) uses...
	readMn,
	writeMn,
	getArgsMn,
	getSplitUniqSupplyMn,
	exitMn,
	fopen, fclose, fwrite, _FILE(..),

	UniqSupply
	IF_ATTACK_PRAGMAS(COMMA getArgsPrimIO)
	IF_ATTACK_PRAGMAS(COMMA appendFilePrimIO)
	IF_ATTACK_PRAGMAS(COMMA appendChanPrimIO)
	IF_ATTACK_PRAGMAS(COMMA readChanPrimIO)
	IF_ATTACK_PRAGMAS(COMMA mkSplitUniqSupply) -- profiling only, really
    ) where

#if __HASKELL1__ >= 3
import LibSystem
#endif

import PreludeGlaST

import Ubiq{-uitous-}

import UniqSupply	( mkSplitUniqSupply, UniqSupply )

infixr 9 `thenMn`	-- right-associative, please
infixr 9 `thenMn_`
\end{code}

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

#if __HASKELL1__ < 3
readMn	    :: String{-channel-} -> MainIO String
writeMn	    :: String{-channel-} -> String -> MainIO ()
#else
readMn	    :: Handle -> MainIO String
writeMn	    :: Handle -> String -> MainIO ()
#endif

getArgsMn   :: MainIO [String]
getSplitUniqSupplyMn
	    :: Char -> MainIO UniqSupply
exitMn	    :: Int -> MainIO ()

{-# INLINE returnMn #-}
{-# INLINE thenMn   #-}
{-# INLINE thenMn_  #-}

exitMn val
  = if val /= 0
    then error "Compilation had errors\n"
    else returnMn ()

#if __HASKELL1__ < 3

type MainIO a = PrimIO a

returnMn    = returnPrimIO
thenMn	    = thenPrimIO
thenMn_	    = seqPrimIO

readMn chan		    = readChanPrimIO chan
writeMn chan str	    = appendChanPrimIO chan str
getArgsMn		    = getArgsPrimIO

getSplitUniqSupplyMn char = mkSplitUniqSupply char

#else {- 1.3 -}

type MainIO a = IO a

returnMn    = return
thenMn	    = (>>=)
thenMn_	    = (>>)

readMn chan		    = hGetContents chan
writeMn chan str	    = hPutStr chan str
getArgsMn		    = getArgs

getSplitUniqSupplyMn char
  = mkSplitUniqSupply char `thenPrimIO` \ us ->
    return us

#endif {- 1.3 -}
\end{code}
