Glues lots of things together for ugen-generated
.hs files here

\begin{code}
#include "HsVersions.h"

module UgenUtil (
	-- re-exported Prelude stuff
	returnPrimIO, thenPrimIO,

	-- stuff defined here
	UgenUtil..
    ) where

import PreludeGlaST

IMP_Ubiq()

import Name		( RdrName(..) )
import SrcLoc		( mkSrcLoc2, mkUnknownSrcLoc )
\end{code}

\begin{code}
type UgnM a
  = (FAST_STRING,Module,SrcLoc)	   -- file, module and src_loc carried down
  -> PrimIO a

{-# INLINE returnUgn #-}
{-# INLINE thenUgn #-}

returnUgn x stuff = returnPrimIO x

thenUgn x y stuff
  = x stuff	`thenPrimIO` \ z ->
    y z stuff

initUgn :: UgnM a -> IO a
initUgn action
  = action (SLIT(""),SLIT(""),mkUnknownSrcLoc) `thenPrimIO` \ result ->
    return result

ioToUgnM :: PrimIO a -> UgnM a
ioToUgnM x stuff = x
\end{code}

\begin{code}
type ParseTree = _Addr

type U_VOID_STAR = _Addr
rdU_VOID_STAR ::  _Addr -> UgnM U_VOID_STAR
rdU_VOID_STAR x = returnUgn x

type U_long = Int
rdU_long ::  Int -> UgnM U_long
rdU_long x = returnUgn x

type U_stringId = FAST_STRING
rdU_stringId :: _Addr -> UgnM U_stringId
{-# INLINE rdU_stringId #-}
rdU_stringId s = returnUgn (_packCString s)

type U_numId = Int -- ToDo: Int
rdU_numId :: _Addr -> UgnM U_numId
rdU_numId i = rdU_stringId i `thenUgn` \ y -> returnUgn ((read (_UNPK_ y))::Int)

type U_hstring = FAST_STRING
rdU_hstring :: _Addr -> UgnM U_hstring
rdU_hstring x
  = ioToUgnM (_ccall_ get_hstring_len   x)  `thenUgn` \ len ->
    ioToUgnM (_ccall_ get_hstring_bytes x)  `thenUgn` \ bytes ->
    returnUgn (_packCBytes len bytes)
\end{code}

\begin{code}
setSrcFileUgn :: FAST_STRING -> UgnM a -> UgnM a
setSrcFileUgn file action stuff@(_,mod,loc) = action (file,mod,loc)

getSrcFileUgn :: UgnM FAST_STRING
getSrcFileUgn stuff@(file,mod,loc) = returnUgn file stuff

setSrcModUgn :: Module -> UgnM a -> UgnM a
setSrcModUgn mod action stuff@(file,_,loc) = action (file,mod,loc)

getSrcModUgn :: UgnM Module
getSrcModUgn stuff@(file,mod,loc) = returnUgn mod stuff

mkSrcLocUgn :: U_long -> (SrcLoc -> UgnM a) -> UgnM a
mkSrcLocUgn ln action (file,mod,_)
  = action loc (file,mod,loc)
  where
    loc = mkSrcLoc2 file ln

getSrcLocUgn :: UgnM SrcLoc
getSrcLocUgn stuff@(file,mod,loc) = returnUgn loc stuff
\end{code}
