Glues lots of things together for ugen-generated
.hs files here

\begin{code}
module UgenUtil (
	-- stuff defined here
	module UgenUtil,
	Addr
    ) where

#include "HsVersions.h"

import GlaExts
import Name
import SrcLoc		( mkSrcLoc, noSrcLoc, SrcLoc )
import FastString	( FastString, mkFastCharString, mkFastCharString2 )
\end{code}

\begin{code}
type UgnM a
  = (FastString,Module,SrcLoc)	   -- file, module and src_loc carried down
  -> IO a

{-# INLINE returnUgn #-}
{-# INLINE thenUgn #-}

returnUgn x stuff = return x

thenUgn x y stuff
  = x stuff	>>= \ z ->
    y z stuff

initUgn :: UgnM a -> IO a
initUgn action = action (SLIT(""),mkSrcModule "",noSrcLoc)

ioToUgnM :: IO a -> UgnM a
ioToUgnM x stuff = x
\end{code}

\begin{code}
type ParseTree = Addr

type U_VOID_STAR = Addr
rdU_VOID_STAR ::  Addr -> UgnM U_VOID_STAR
rdU_VOID_STAR x = returnUgn x

type U_long = Int
rdU_long ::  Int -> UgnM U_long
rdU_long x = returnUgn x

type U_stringId = FastString
rdU_stringId :: Addr -> UgnM U_stringId
{-# INLINE rdU_stringId #-}
rdU_stringId s = returnUgn (mkFastCharString s)

type U_numId = Int -- ToDo: Int
rdU_numId :: Addr -> UgnM U_numId
rdU_numId i = rdU_stringId i `thenUgn` \ y -> returnUgn ((read (_UNPK_ y))::Int)

type U_hstring = FastString
rdU_hstring :: Addr -> UgnM U_hstring
rdU_hstring x
  = ioToUgnM (_ccall_ get_hstring_len   x)  `thenUgn` \ len ->
    ioToUgnM (_ccall_ get_hstring_bytes x)  `thenUgn` \ bytes ->
    returnUgn (mkFastCharString2 bytes len)
\end{code}

\begin{code}
setSrcFileUgn :: FastString -> UgnM a -> UgnM a
setSrcFileUgn file action stuff@(_,mod,loc) = action (file,mod,loc)

getSrcFileUgn :: UgnM FastString
getSrcFileUgn stuff@(file,mod,loc) = returnUgn file stuff

setSrcModUgn :: Module -> UgnM a -> UgnM a
setSrcModUgn mod action stuff@(file,_,loc) = action (file,mod,loc)

getSrcModUgn :: UgnM Module
getSrcModUgn stuff@(file,mod,loc) = returnUgn mod stuff

mkSrcLocUgn :: U_long -> (SrcLoc -> UgnM a) -> UgnM a
mkSrcLocUgn ln action (file,mod,_)
  = action loc (file,mod,loc)
  where
    loc = mkSrcLoc file ln

getSrcLocUgn :: UgnM SrcLoc
getSrcLocUgn stuff@(file,mod,loc) = returnUgn loc stuff
\end{code}
