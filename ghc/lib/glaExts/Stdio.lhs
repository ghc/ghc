%
% (c) The AQUA Project, Glasgow University, 1993-1994
%
\section[Stdio]{Wrappers for C standard-IO library}

\begin{code}
module Stdio where

import Cls
import Core
import IInt
import IList
import List		( (++), foldr )
import PS		-- ( _PackedString )
import TyArray
import PreludeGlaST
import Text

data _FILE = _FILE Addr#
instance _CCallable   _FILE
instance _CReturnable _FILE

instance Eq _FILE where
    (_FILE a) == (_FILE b) = a `eqAddr#` b
    (_FILE a) /= (_FILE b) = if a `eqAddr#` b then False else True

type FILE_DESCRIPTOR = Int

fopen	:: String		-- as w/ C fopen, name
	-> String		-- type of open (as w/ C)
	-> PrimIO _FILE		-- FILE* returned; will be ``NULL''
				-- if things go wrong...

-- similarly...
freopen	:: String -> String -> _FILE -> PrimIO _FILE
fdopen	:: FILE_DESCRIPTOR -> String -> PrimIO _FILE

fopen name descr
  = _casm_ ``%r = (A_) fopen((char *) %0, (char *) %1);'' name descr

freopen name descr file
  = _casm_ ``%r = (A_) freopen((char *) %0, (char *) %1, (FILE *) %2);'' 
	name descr file

fdopen fd descr
  = _casm_ ``%r = (A_) fdopen((int) %0, (char *) %1);'' fd descr

---------------------------------------------------------------
fclose, fflush :: _FILE -> PrimIO Int

fclose file
  = _casm_ ``%r = fclose((FILE *) %0);'' file

fflush file
  = _casm_ ``%r = fflush((FILE *) %0);'' file

fread :: Int -> Int -> _FILE -> PrimIO (Int, _ByteArray Int)

fread size nitems file
  = let
	barr_end = size * nitems - 1
    in
    newCharArray (0::Int, barr_end){-malloc!?-} `thenStrictlyST` \ barr ->

    _ccall_ fread barr size nitems file `thenPrimIO` \ num_read ->

    unsafeFreezeByteArray barr	`thenStrictlyST` \ frozen ->

    returnPrimIO (num_read, frozen)

fwrite :: _ByteArray Int -> Int -> Int -> _FILE -> PrimIO Int

fwrite barr size nitems file
  = _ccall_ fwrite barr size nitems file `thenPrimIO` \ num_written ->
    returnPrimIO num_written

--fgetc	:: _FILE -> B Char
--fputc	:: Char -> _FILE -> B Char

-- ===============================================================
{- LATER

-- in Haskell, these are just synonyms for getc and putc

gets	:: B [Char]
fgets	:: C_FILE -> Int -> B [Char]
puts	:: [Char] -> B Bool	-- ??? ToDo: better error indicator
fputs	:: [Char] -> C_FILE -> B Bool

-- getw, putw omitted

feof	:: C_FILE -> B Int -- ToDo: Bool?
ferror	:: C_FILE -> B Int -- ToDo: something else?
fileno	:: C_FILE -> B Int
clearerr :: C_FILE -> B ()

popen	:: [Char] -> [Char] -> B C_FILE
pclose	:: C_FILE -> B Int -- exit status

tmpfile :: B C_FILE	-- B (Maybe C_FILE) ???
tmpnam	:: [Char] -> B [Char]
tempnam	:: [Char] -> [Char] -> B [Char]

lseek	:: C_FileDes -> C_off_t -> Int -> B C_off_t

ctermid	:: B [Char]
cuserid	:: B [Char]

-- nothing yet:
--  printf
--  fprintf
--  sprintf
--  scanf
--  fscanf
-}
\end{code}
