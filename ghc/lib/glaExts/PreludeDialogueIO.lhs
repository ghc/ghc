%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1994
%
\section{The @Dialogue@ interface}

\begin{code}
module PreludeDialogueIO (
	requestToPrimIO,    -- RTS uses this!

	processIORequest,   -- used in PreludeGlaIO
	appendChan#,	    -- used elsewhere in prelude
	unpackArgv,	    -- ditto
	unpackProgName	    -- ditto
    ) where

import PreludeGlaST	-- for _ST stuff
import PreludeGlaMisc	-- for stable pointers
import Cls
import Core
import IChar
import IInt
import IList
import IO		( stdout, stdin )
import List		( (++), reverse, foldr, foldl )
import PS		-- packed strings
import Prel		( chr, flip )
import Stdio		( fopen, fclose, fflush, _FILE )
import Text
import TyArray		( Array(..) )
import TyComplex
import TyIO
\end{code}

%************************************************************************
%*									*
\subsection[requestToIO]{Dialogue-to-IO}
%*									*
%************************************************************************

We would like to take existing Haskell programs, written with @main@
of type @Dialogue@, and run them on our system.	 To do this, our
system actually evaluates @mainPrimIO@ (rather than @main@ directly).
@main@ has type @Dialogue@ then @mainPrimIO@ [separate module] is defined
like this:
\begin{verbatim}
mainPrimIO :: PrimIO ()
mainPrimIO s = case (requestToPrimIO main s) of
	     ( (), s2) -> ( (), s2 )
\end{verbatim}

So, here's @requestToPrimIO@:
\begin{code}
requestToPrimIO :: Dialogue -> PrimIO ()

requestToPrimIO dialogue 
 = newVar (error "HELP! (Forgot to link with -fhaskell-1.3?)\n")
					`thenPrimIO` \ rsV ->
   unsafeInterleavePrimIO (readVar rsV)	`thenPrimIO` \ rs ->
   run (dialogue rs) rsV

run :: [Request] -> MutableVar _RealWorld [Response] -> PrimIO ()

run []	       v = returnPrimIO ()
run (req:reqs) v 
 = processIORequest req			`thenPrimIO` \ r ->
   newVar (error "GlasgowIO:run:synch")	`thenPrimIO` \ rsV ->
   unsafeInterleavePrimIO (readVar rsV)	`thenPrimIO` \ rs ->
   writeVar v (r:rs)			`seqPrimIO`
   run reqs rsV
\end{code}

%************************************************************************
%*									*
\subsection[processIORequest]{@processIORequest@}
%*									*
%************************************************************************

The guy that really does the business is @processIORequest@.  We make
this available to the intrepid user.

\begin{code}
processIORequest :: Request -> PrimIO Response

processIORequest (ReadFile name)
  = fopen name "r"	`thenPrimIO` \ file_star ->
    if (file_star == ``NULL'')
    then returnPrimIO (Failure (ReadError ("ReadFile: can't read: "++name)))
	-- ToDo: return SearchErrors when appropriate

    else readFile# file_star `thenPrimIO` \ str ->
	 returnPrimIO (Str str)

processIORequest (WriteFile name string)
  = fopen name "w"	`thenPrimIO` \ file_star ->
    if (file_star == ``NULL'')
    then returnPrimIO (Failure (WriteError ("WriteFile: open failed: "++name)))

    else writeFile# file_star string  `seqPrimIO`
	 fclose	     file_star	      `thenPrimIO` \ status ->
	 returnPrimIO (
	     if status == 0
	     then Success
	     else Failure (WriteError ("WriteFile: closed failed: "++name))
	 )

processIORequest (AppendFile name string)
  = fopen name "a+"{-don't create-} `thenPrimIO` \ file_star ->
    if (file_star == ``NULL'')
    then returnPrimIO (Failure (WriteError ("AppendFile: open failed: "++name)))

    else writeFile# file_star string `seqPrimIO`
	 fclose	    file_star	     `thenPrimIO` \ status ->
	 returnPrimIO (
	     if status == 0
	     then Success
	     else Failure (WriteError ("AppendFile: closed failed: "++name))
	 )

processIORequest (DeleteFile name)
  = _casm_ ``%r = (I_) unlink((char *) %0);'' name   `thenPrimIO` \ status ->
    returnPrimIO (
    if (status == (0::Int)) then
	Success
    else if ( (``errno''::Int) == (``ENOENT''::Int) ) then
	Failure (SearchError ("DeleteFile: no such file: "++name))
    else
	Failure (WriteError ("DeleteFile: could not delete: "++name))
    )

processIORequest (AppendChan chan str)
  = case chan of 
      "stdout" ->
    	appendChan# ``stdout'' str   	`seqPrimIO` 
    	fflush ``stdout''    	    	`thenPrimIO` \ status ->
	returnPrimIO (
	    if status == 0
	    then Success
	    else Failure (WriteError ("AppendChan: flush failed: " ++ chan))
	)
      "stderr" ->
    	appendChan# ``stderr'' str   	`seqPrimIO` 
    	fflush ``stderr''    	    	`thenPrimIO` \ status ->
	returnPrimIO (
	    if status == 0
	    then Success
	    else Failure (WriteError ("AppendChan: flush failed: " ++ chan))
	)
      _ -> error "AppendChan: not implemented except for \"stdout\" and \"stderr\"\n"

processIORequest (ReadChan chan)
  = case chan of
      "stdin" -> readChan# ``stdin'' `thenPrimIO` \ str ->
		 returnPrimIO (Str str)

      _ -> error "ReadChan: not implemented except for \"stdin\"\n"

processIORequest (Echo False) = returnPrimIO Success
processIORequest (Echo True)
  = {- REMOVED: Can't be bothered. WDP: 95/04
    appendChan# ``stderr'' "Glasgow Haskell doesn't support \"Echo\" requests properly (yet)\n"
    `seqPrimIO` -} returnPrimIO Success

processIORequest GetArgs
  = returnPrimIO (StrList (unpackArgv ``prog_argv'' (``prog_argc''::Int) ))

processIORequest GetProgName
  = returnPrimIO (Str (unpackProgName ``prog_argv''))

processIORequest (GetEnv name)
  = _casm_ ``%r = getenv((char *) %0);'' name `thenPrimIO` \ litstring ->
    returnPrimIO (
	if (eqAddr litstring ``NULL'') then
	    Failure (SearchError ("GetEnv:"++name))
	else
	    Str (_unpackPS (_packCString litstring)) -- cheaper than it looks
    )
  where
    eqAddr (A# a1) (A# a2) = eqAddr# a1 a2

#ifndef __PARALLEL_HASKELL__

processIORequest (SigAction n act)
  = (case act of
    SAIgnore -> _ccall_ stg_sig_ignore n (``NULL''::_Addr)
    SADefault -> _ccall_ stg_sig_default n (``NULL''::_Addr)
    SACatch dialogue -> 
                 let handler :: PrimIO ()
                     handler s = case (requestToPrimIO dialogue s) of
    	    	    	    	( (), s2@(S# _) ) -> ( (), s2 )
    	    	 in
    	    	    makeStablePtr handler    `thenPrimIO` \ sptr ->
    	    	    _ccall_ stg_sig_catch n sptr (``NULL''::_Addr))
    	    	    	    	    	     `thenPrimIO` \ osptr ->
    returnPrimIO (
    	if osptr >= 0 then Success
        else Failure (OtherError ("SigAction:" ++ show n)))

#endif {-!parallel-}

processIORequest _
  = error "DialogueToIO.processIORequest: unimplemented I/O request (please report)\n"
\end{code}

%************************************************************************
%*									*
\subsection[DialogueIO]{Access to all @Dialogues@ in the IO world}
%*									*
%************************************************************************

This is Andy Gill's stuff to make all of @Dialogue@-style IO readily
available in the monadic IO world.

%************************************************************************
%*									*
\subsection{Support bits for all of this}
%*									*
%************************************************************************

\begin{code}
-- like unpackCString ...

type CHAR_STAR_STAR	= _Addr	-- this is all a  HACK
type CHAR_STAR		= _Addr

unpackArgv	:: CHAR_STAR_STAR -> Int -> [String] -- argv[1 .. argc-1]
unpackProgName	:: CHAR_STAR_STAR	 -> String   -- argv[0]

unpackArgv argv argc = unpack 1
  where
    unpack :: Int -> [String]
    unpack n
      = if (n >= argc)
	then ([] :: [String])
	else case (indexAddrOffAddr argv n) of { item ->
	     _unpackPS (_packCString item) : unpack (n + 1)
	     }

unpackProgName argv
  = case (indexAddrOffAddr argv 0) of { prog ->
    de_slash [] (_unpackPS (_packCString prog)) }
  where
    -- re-start accumulating at every '/'
    de_slash :: String -> String -> String
    de_slash acc []	  = reverse acc
    de_slash acc ('/':xs) = de_slash []	     xs
    de_slash acc (x:xs)	  = de_slash (x:acc) xs
\end{code}

Read and append a string from/on a given @FILE *@ stream.  @appendChan#@
and @readChan#@ are well-behaved lazy functions; @writeFile#@ and
@readFile#@ (which ``know'' they are writing/reading disk files) are
much stricter.

\begin{code}
appendChan#, writeFile# :: _FILE -> String -> PrimIO Bool

appendChan# stream [] = returnPrimIO True

appendChan# stream (c : cs)
  = _ccall_ stg_putc c stream	`seqPrimIO` -- stg_putc expands to putc
    appendChan# stream cs		    -- (just does some casting stream)

-----------
writeFile# stream [] = returnPrimIO True

writeFile# stream (c1@(C# _) : c2@(C# _) : c3@(C# _) : c4@(C# _)
		 : c5@(C# _) : c6@(C# _) : c7@(C# _) : c8@(C# _)
		 : c9@(C# _) : c10@(C# _): c11@(C# _): c12@(C# _)
		 : c13@(C# _): c14@(C# _): c15@(C# _): c16@(C# _): cs)
 = _ccall_ stg_putc  c1 stream	`seqPrimIO`
   _ccall_ stg_putc  c2 stream	`seqPrimIO`
   _ccall_ stg_putc  c3 stream	`seqPrimIO`
   _ccall_ stg_putc  c4 stream	`seqPrimIO`
   _ccall_ stg_putc  c5 stream	`seqPrimIO`
   _ccall_ stg_putc  c6 stream	`seqPrimIO`
   _ccall_ stg_putc  c7 stream	`seqPrimIO`
   _ccall_ stg_putc  c8 stream	`seqPrimIO`
   _ccall_ stg_putc  c9 stream	`seqPrimIO`
   _ccall_ stg_putc c10 stream	`seqPrimIO`
   _ccall_ stg_putc c11 stream	`seqPrimIO`
   _ccall_ stg_putc c12 stream	`seqPrimIO`
   _ccall_ stg_putc c13 stream	`seqPrimIO`
   _ccall_ stg_putc c14 stream	`seqPrimIO`
   _ccall_ stg_putc c15 stream	`seqPrimIO`
   _ccall_ stg_putc c16 stream	`seqPrimIO`
   writeFile# stream cs

writeFile# stream (c : cs)
  = _ccall_ stg_putc c stream	`seqPrimIO`
    writeFile# stream cs
\end{code}

@readChan#@ lazily reads the rest of some stream.  Dodgy because two
uses of.

ToDo: return fclose status.

\begin{code}
readChan#, readFile# :: _FILE -> PrimIO String

readChan# stream
  = let
	read_rest
	  =  _ccall_ stg_getc{-macro-} stream `thenPrimIO` \ ch ->

	     if ch < 0 then -- SIGH: ch ==# ``EOF'' then
		returnPrimIO []
	     else
		unsafeInterleavePrimIO read_rest `thenPrimIO` \ rest ->
		returnPrimIO (chr ch : rest)
    in
    unsafeInterleavePrimIO read_rest `thenPrimIO` \ contents ->
    returnPrimIO contents

------------------
readFile# stream
  = let
	read_rest
	  =  newCharArray (0::Int, 1023){-malloc!?-} `thenStrictlyST` \ arr# ->
		-- ToDo: lift newCharArray out of the loop!

	     _ccall_ fread arr# (1::Int) (1024::Int) stream `thenPrimIO` \ num_read ->

	     cvt arr# 0 (num_read - 1)	`thenPrimIO` \ chars ->

	     if num_read < 1024 then
		fclose stream `seqPrimIO`
		returnPrimIO chars
	     else
		unsafeInterleavePrimIO read_rest `thenPrimIO` \ rest ->
		returnPrimIO (chars ++ rest)
    in
    unsafeInterleavePrimIO read_rest `thenPrimIO` \ contents ->
    returnPrimIO contents
  where
    cvt :: _MutableByteArray _RealWorld Int
	-> Int -> Int
	-> PrimIO [Char]

    cvt arr# idx last
      = if idx > last then
	   returnPrimIO []
        else
	   readCharArray arr# idx   `thenPrimIO` \ ch ->
	   cvt arr# (idx + 1) last  `thenPrimIO` \ rest ->
	   returnPrimIO (ch : rest)
\end{code}
