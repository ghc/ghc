Glues lots of things together for ugen-generated
.hs files here

\begin{code}
#include "HsVersions.h"

module UgenUtil (
	-- re-exported Prelude stuff
	returnPrimIO, thenPrimIO,

	-- stuff defined here
	EXP_MODULE(UgenUtil)
    ) where

IMP_Ubiq()

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
import PreludeGlaST
#else
import GlaExts
import Name
#endif

#if __GLASGOW_HASKELL__ == 201
# define ADDR	    GHCbase.Addr
# define PACK_STR   packCString
# define PACK_BYTES packCBytes
#elif __GLASGOW_HASKELL >= 202
# define ADDR       GHC.Addr
# define PACK_STR   mkFastCharString
# define PACK_BYTES mkFastCharString2
#else
# define ADDR	    _Addr
# define PACK_STR   mkFastCharString
# define PACK_BYTES mkFastCharString2
#endif

import RdrHsSyn		( RdrName(..) )
import SrcLoc		( mkSrcLoc, noSrcLoc, SrcLoc )
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
  = let
	do_it = action (SLIT(""),SLIT(""),noSrcLoc)
    in
#if __GLASGOW_HASKELL__ >= 200
    primIOToIO do_it
#else
    do_it	`thenPrimIO` \ result ->
    return result
#endif

ioToUgnM :: PrimIO a -> UgnM a
ioToUgnM x stuff = x
\end{code}

\begin{code}
type ParseTree = ADDR

type U_VOID_STAR = ADDR
rdU_VOID_STAR ::  ADDR -> UgnM U_VOID_STAR
rdU_VOID_STAR x = returnUgn x

type U_long = Int
rdU_long ::  Int -> UgnM U_long
rdU_long x = returnUgn x

type U_stringId = FAST_STRING
rdU_stringId :: ADDR -> UgnM U_stringId
{-# INLINE rdU_stringId #-}
rdU_stringId s = returnUgn (PACK_STR s)

type U_numId = Int -- ToDo: Int
rdU_numId :: ADDR -> UgnM U_numId
rdU_numId i = rdU_stringId i `thenUgn` \ y -> returnUgn ((read (_UNPK_ y))::Int)

type U_hstring = FAST_STRING
rdU_hstring :: ADDR -> UgnM U_hstring
rdU_hstring x
  = ioToUgnM (_ccall_ get_hstring_len   x)  `thenUgn` \ len ->
    ioToUgnM (_ccall_ get_hstring_bytes x)  `thenUgn` \ bytes ->
    returnUgn (PACK_BYTES bytes len)
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
    loc = mkSrcLoc file ln

getSrcLocUgn :: UgnM SrcLoc
getSrcLocUgn stuff@(file,mod,loc) = returnUgn loc stuff
\end{code}
