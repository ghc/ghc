{-% DrIFT (Automatic class derivations for Haskell) v1.1 %-}
%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\section[RdrName]{@RdrName@}

\begin{code}
module RdrName (
	RdrName,

	-- Construction
	mkRdrUnqual, mkRdrQual, mkRdrOrig, mkRdrUnqual,
	mkUnqual, mkQual, mkIfaceOrig, mkOrig,
	qualifyRdrName, unqualifyRdrName, mkRdrNameWkr,
	dummyRdrVarName, dummyRdrTcName,

	-- Destruction
	rdrNameModule, rdrNameOcc, setRdrNameOcc,
	isRdrDataCon, isRdrTyVar, isRdrTc, isQual, isUnqual, isOrig,

	-- Environment
	RdrNameEnv, 
	emptyRdrEnv, lookupRdrEnv, addListToRdrEnv, rdrEnvElts, 
	extendRdrEnv, rdrEnvToList, elemRdrEnv, foldRdrEnv, 

	-- Printing;	instance Outputable RdrName
	pprUnqualRdrName 
  ) where 

#include "HsVersions.h"

import OccName	( NameSpace, tcName,
		  OccName, UserFS, EncodedFS,
		  mkSysOccFS,
		  mkOccFS, mkVarOcc,
		  isDataOcc, isTvOcc, isTcOcc, mkWorkerOcc
		)
import Module   ( ModuleName,
		  mkSysModuleNameFS, mkModuleNameFS
		)
import FiniteMap
import Outputable
import Binary
import Util	( thenCmp )
\end{code}


%************************************************************************
%*									*
\subsection{The main data type}
%*									*
%************************************************************************

\begin{code}
data RdrName = RdrName Qual OccName
  {-! derive: Binary !-}

data Qual
  = Unqual

  | Qual ModuleName	-- A qualified name written by the user in source code
			-- The module isn't necessarily the module where
			-- the thing is defined; just the one from which it
			-- is imported

  | Orig ModuleName	-- This is an *original* name; the module is the place
			-- where the thing was defined
  {-! derive: Binary !-}

\end{code}


%************************************************************************
%*									*
\subsection{Simple functions}
%*									*
%************************************************************************

\begin{code}
rdrNameModule :: RdrName -> ModuleName
rdrNameModule (RdrName (Qual m) _) = m
rdrNameModule (RdrName (Orig m) _) = m

rdrNameOcc :: RdrName -> OccName
rdrNameOcc (RdrName _ occ) = occ

setRdrNameOcc :: RdrName -> OccName -> RdrName
setRdrNameOcc (RdrName q _) occ = RdrName q occ
\end{code}

\begin{code}
	-- These two are the basic constructors
mkRdrUnqual :: OccName -> RdrName
mkRdrUnqual occ = RdrName Unqual occ

mkRdrQual :: ModuleName -> OccName -> RdrName
mkRdrQual mod occ = RdrName (Qual mod) occ

mkRdrOrig :: ModuleName -> OccName -> RdrName
mkRdrOrig mod occ = RdrName (Orig mod) occ

mkIfaceOrig :: NameSpace -> (EncodedFS, EncodedFS) -> RdrName
mkIfaceOrig ns (m,n) = RdrName (Orig (mkSysModuleNameFS m)) (mkSysOccFS ns n)


	-- These two are used when parsing source files
	-- They do encode the module and occurrence names
mkUnqual :: NameSpace -> UserFS -> RdrName
mkUnqual sp n = RdrName Unqual (mkOccFS sp n)

mkQual :: NameSpace -> (UserFS, UserFS) -> RdrName
mkQual sp (m, n) = RdrName (Qual (mkModuleNameFS m)) (mkOccFS sp n)

mkOrig :: NameSpace -> ModuleName -> UserFS -> RdrName
mkOrig sp mod n = RdrName (Orig mod) (mkOccFS sp n)

qualifyRdrName :: ModuleName -> RdrName -> RdrName
	-- Sets the module name of a RdrName, even if it has one already
qualifyRdrName mod (RdrName _ occ) = RdrName (Qual mod) occ

unqualifyRdrName :: RdrName -> RdrName
unqualifyRdrName (RdrName _ occ) = RdrName Unqual occ

mkRdrNameWkr :: RdrName -> RdrName 	-- Worker-ify it
mkRdrNameWkr (RdrName qual occ) = RdrName qual (mkWorkerOcc occ)
\end{code}

\begin{code}
	-- This guy is used by the reader when HsSyn has a slot for
	-- an implicit name that's going to be filled in by
	-- the renamer.  We can't just put "error..." because
	-- we sometimes want to print out stuff after reading but
	-- before renaming
dummyRdrVarName = RdrName Unqual (mkVarOcc FSLIT("V-DUMMY"))
dummyRdrTcName  = RdrName Unqual (mkOccFS tcName FSLIT("TC-DUMMY"))
\end{code}


\begin{code}
isRdrDataCon (RdrName _ occ) = isDataOcc occ
isRdrTyVar   (RdrName _ occ) = isTvOcc occ
isRdrTc      (RdrName _ occ) = isTcOcc occ

isUnqual (RdrName Unqual _) = True
isUnqual other		    = False

isQual (RdrName (Qual _) _) = True
isQual _		    = False

isOrig (RdrName (Orig _)    _) = True
isOrig other		       = False
\end{code}


%************************************************************************
%*									*
\subsection{Instances}
%*									*
%************************************************************************

\begin{code}
instance Outputable RdrName where
    ppr (RdrName qual occ) = pp_qual qual <> ppr occ
			   where
			     pp_qual Unqual      = empty
			     pp_qual (Qual mod)  = ppr mod <> dot
			     pp_qual (Orig mod)  = ppr mod <> dot

pprUnqualRdrName (RdrName qual occ) = ppr occ

instance Eq RdrName where
    a == b = case (a `compare` b) of { EQ -> True;  _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False; _ -> True }

instance Ord RdrName where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <	 b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }

    compare (RdrName q1 o1) (RdrName q2 o2)
	= (o1  `compare` o2) `thenCmp` 
	  (q1  `cmpQual` q2) 

cmpQual Unqual	    Unqual      = EQ
cmpQual (Qual m1)   (Qual m2)   = m1 `compare` m2
cmpQual (Orig m1)   (Orig m2)   = m1 `compare` m2
cmpQual Unqual      _	        = LT
cmpQual (Qual _)    (Orig _)    = LT
cmpQual _	    _	        = GT
\end{code}



%************************************************************************
%*									*
\subsection{Environment}
%*									*
%************************************************************************

\begin{code}
type RdrNameEnv a = FiniteMap RdrName a

emptyRdrEnv  	:: RdrNameEnv a
lookupRdrEnv 	:: RdrNameEnv a -> RdrName -> Maybe a
addListToRdrEnv :: RdrNameEnv a -> [(RdrName,a)] -> RdrNameEnv a
extendRdrEnv	:: RdrNameEnv a -> RdrName -> a -> RdrNameEnv a
rdrEnvToList    :: RdrNameEnv a -> [(RdrName, a)]
rdrEnvElts	:: RdrNameEnv a -> [a]
elemRdrEnv	:: RdrName -> RdrNameEnv a -> Bool
foldRdrEnv	:: (RdrName -> a -> b -> b) -> b -> RdrNameEnv a -> b

emptyRdrEnv     = emptyFM
lookupRdrEnv    = lookupFM
addListToRdrEnv = addListToFM
rdrEnvElts	= eltsFM
extendRdrEnv    = addToFM
rdrEnvToList    = fmToList
elemRdrEnv      = elemFM
foldRdrEnv	= foldFM
\end{code}
\begin{code}
{-* Generated by DrIFT-v1.0 : Look, but Don't Touch. *-}
instance Binary RdrName where
    put_ bh (RdrName aa ab) = do
	    put_ bh aa
	    put_ bh ab
    get bh = do
	  aa <- get bh
	  ab <- get bh
	  return (RdrName aa ab)

instance Binary Qual where
    put_ bh Unqual = do
	    putByte bh 0
    put_ bh (Qual aa) = do
	    putByte bh 1
	    put_ bh aa
    put_ bh (Orig ab) = do
	    putByte bh 2
	    put_ bh ab
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do return Unqual
	      1 -> do aa <- get bh
		      return (Qual aa)
	      _ -> do ab <- get bh
		      return (Orig ab)

--  Imported from other files :-

\end{code}
