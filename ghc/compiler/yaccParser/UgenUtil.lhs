Glues lots of things together for ugen-generated
.hs files here

\begin{code}
#include "HsVersions.h"

module UgenUtil (
	-- re-exported Prelude stuff
	returnPrimIO, thenPrimIO,

	-- stuff defined here
	UgenUtil..,

	-- complete interface
	ProtoName
    ) where

#if __GLASGOW_HASKELL__ < 26
import PreludePrimIO
#else
import PreludeGlaST
#endif
import MainMonad

import ProtoName
import Outputable
import SrcLoc		( mkSrcLoc2 )
import Util
\end{code}

\begin{code}
type UgnM a
  = FAST_STRING		   -- source file name; carried down
  -> PrimIO a

{-# INLINE returnUgn #-}
{-# INLINE thenUgn #-}

returnUgn x mod = returnPrimIO x

thenUgn x y mod
  = x mod	`thenPrimIO` \ z ->
    y z mod

initUgn :: FAST_STRING -> UgnM a -> MainIO a
initUgn srcfile action
  = action srcfile

ioToUgnM :: PrimIO a -> UgnM a
ioToUgnM x mod = x
\end{code}

\begin{code}
type ParseTree = _Addr

type U_VOID_STAR = _Addr
rdU_VOID_STAR ::  _Addr -> UgnM U_VOID_STAR
rdU_VOID_STAR x = returnUgn x

type U_long = Int
rdU_long ::  Int -> UgnM U_long
rdU_long x = returnUgn x -- (A# x) = returnUgn (I# (addr2Int# x))

type U_unkId = ProtoName
rdU_unkId :: _Addr -> UgnM U_unkId
rdU_unkId x
  = rdU_stringId x `thenUgn` \ y ->
    returnUgn (Unk y)

type U_stringId = FAST_STRING
rdU_stringId :: _Addr -> UgnM U_stringId
rdU_stringId s
  = ioToUgnM (_ccall_ hash_index s) `thenUgn` \ (I# i) ->
    returnUgn (_packCString s) -- ToDo: use the i!

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
setSrcFileUgn :: FAST_STRING{-filename-} -> UgnM a -> UgnM a
setSrcFileUgn file action _ = action file

getSrcFileUgn :: UgnM FAST_STRING{-filename-}
getSrcFileUgn mod = returnUgn mod mod

mkSrcLocUgn :: U_long -> UgnM SrcLoc 
mkSrcLocUgn ln mod
  = returnUgn (mkSrcLoc2 mod ln) mod
\end{code}
