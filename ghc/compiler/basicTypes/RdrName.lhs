%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\section[RdrName]{@RdrName@}

\begin{code}
module RdrName (
	RdrName,

	-- Construction
	mkRdrUnqual, mkRdrQual,
	mkSrcUnqual, mkSrcQual, 
	mkSysUnqual, mkSysQual,
	mkPreludeQual, qualifyRdrName, mkRdrNameWkr,
	dummyRdrVarName, dummyRdrTcName,

	-- Destruction
	rdrNameModule, rdrNameOcc, setRdrNameOcc,
	isRdrDataCon, isRdrTyVar, isQual, isUnqual,

	-- Environment
	RdrNameEnv, 
	emptyRdrEnv, lookupRdrEnv, addListToRdrEnv, rdrEnvElts, 
	extendRdrEnv, rdrEnvToList,

	-- Printing;	instance Outputable RdrName
	pprUnqualRdrName 
  ) where 

#include "HsVersions.h"

import OccName	( NameSpace, tcName,
		  OccName,
		  mkSysOccFS,
		  mkSrcOccFS, mkSrcVarOcc,
		  isDataOcc, isTvOcc, mkWorkerOcc
		)
import Module   ( ModuleName, pprModuleName,
		  mkSysModuleFS, mkSrcModuleFS
		)
import FiniteMap
import Outputable
import Util	( thenCmp )
\end{code}


%************************************************************************
%*									*
\subsection{The main data type}
%*									*
%************************************************************************

\begin{code}
data RdrName = RdrName Qual OccName

data Qual = Unqual
	  | Qual ModuleName	-- The (encoded) module name
\end{code}


%************************************************************************
%*									*
\subsection{Simple functions}
%*									*
%************************************************************************

\begin{code}
rdrNameModule :: RdrName -> ModuleName
rdrNameModule (RdrName (Qual m) _) = m

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

	-- These two are used when parsing source files
	-- They do encode the module and occurrence names
mkSrcUnqual :: NameSpace -> FAST_STRING -> RdrName
mkSrcUnqual sp n = RdrName Unqual (mkSrcOccFS sp n)

mkSrcQual :: NameSpace -> FAST_STRING -> FAST_STRING -> RdrName
mkSrcQual sp m n = RdrName (Qual (mkSrcModuleFS m)) (mkSrcOccFS sp n)

	-- These two are used when parsing interface files
	-- They do not encode the module and occurrence name
mkSysUnqual :: NameSpace -> FAST_STRING -> RdrName
mkSysUnqual sp n = RdrName Unqual (mkSysOccFS sp n)

mkSysQual :: NameSpace -> (FAST_STRING, FAST_STRING) -> RdrName
mkSysQual sp (m,n) = RdrName (Qual (mkSysModuleFS m)) (mkSysOccFS sp n)

mkPreludeQual :: NameSpace -> ModuleName -> FAST_STRING -> RdrName
mkPreludeQual sp mod n = RdrName (Qual mod) (mkSrcOccFS sp n)

qualifyRdrName :: ModuleName -> RdrName -> RdrName
	-- Sets the module name of a RdrName, even if it has one already
qualifyRdrName mod (RdrName _ occ) = RdrName (Qual mod) occ

mkRdrNameWkr :: RdrName -> RdrName 	-- Worker-ify it
mkRdrNameWkr (RdrName qual occ) = RdrName qual (mkWorkerOcc occ)
\end{code}

\begin{code}
	-- This guy is used by the reader when HsSyn has a slot for
	-- an implicit name that's going to be filled in by
	-- the renamer.  We can't just put "error..." because
	-- we sometimes want to print out stuff after reading but
	-- before renaming
dummyRdrVarName = RdrName Unqual (mkSrcVarOcc SLIT("V-DUMMY"))
dummyRdrTcName  = RdrName Unqual (mkSrcOccFS tcName SLIT("TC-DUMMY"))
\end{code}


\begin{code}
isRdrDataCon (RdrName _ occ) = isDataOcc occ
isRdrTyVar   (RdrName _ occ) = isTvOcc occ

isUnqual (RdrName Unqual _) = True
isUnqual other		    = False

isQual rdr_name = not (isUnqual rdr_name)
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
			     pp_qual Unqual     = empty
			     pp_qual (Qual mod) = pprModuleName mod <> dot

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

cmpQual Unqual	  Unqual    = EQ
cmpQual Unqual    (Qual _)  = LT
cmpQual (Qual _)  Unqual    = GT
cmpQual (Qual m1) (Qual m2) = m1 `compare` m2
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

emptyRdrEnv  = emptyFM
lookupRdrEnv = lookupFM
addListToRdrEnv = addListToFM
rdrEnvElts	= eltsFM
extendRdrEnv    = addToFM
rdrEnvToList    = fmToList
\end{code}
