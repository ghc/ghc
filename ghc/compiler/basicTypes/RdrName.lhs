{-% DrIFT (Automatic class derivations for Haskell) v1.1 %-}
%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\section[RdrName]{@RdrName@}

\begin{code}
module RdrName (
	RdrName,

	-- Construction
	mkRdrUnqual, mkRdrQual, 
	mkUnqual, mkQual, mkOrig, mkIfaceOrig, 
	nameRdrName, getRdrName, 
	qualifyRdrName, unqualifyRdrName, mkRdrNameWkr,
	dummyRdrVarName, dummyRdrTcName,

	-- Destruction
	rdrNameModule, rdrNameOcc, setRdrNameSpace,
	isRdrDataCon, isRdrTyVar, isRdrTc, isQual, isUnqual, 
	isOrig, isExact, isExact_maybe,

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
		  mkSysOccFS, setOccNameSpace,
		  mkOccFS, mkVarOcc, occNameFlavour,
		  isDataOcc, isTvOcc, isTcOcc, mkWorkerOcc
		)
import Module   ( ModuleName,
		  mkSysModuleNameFS, mkModuleNameFS
		)
import Name	( Name, NamedThing(getName), nameModule, nameOccName )
import Module	( moduleName )
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
data RdrName 
  = Unqual OccName
	-- Used for ordinary, unqualified occurrences 

  | Qual ModuleName OccName
	-- A qualified name written by the user in 
	-- *source* code.  The module isn't necessarily 
	-- the module where the thing is defined; 
	-- just the one from which it is imported

  | Orig ModuleName OccName
	-- An original name; the module is the *defining* module.
	-- This is used when GHC generates code that will be fed
	-- into the renamer (e.g. from deriving clauses), but where
	-- we want to say "Use Prelude.map dammit".  
 
  | Exact Name
	-- We know exactly the Name. This is used 
	--  (a) when the parser parses built-in syntax like "[]" 
	--	and "(,)", but wants a RdrName from it
	--  (b) possibly, by the meta-programming stuff
\end{code}


%************************************************************************
%*									*
\subsection{Simple functions}
%*									*
%************************************************************************

\begin{code}
rdrNameModule :: RdrName -> ModuleName
rdrNameModule (Qual m _) = m
rdrNameModule (Orig m _) = m
rdrNameModule (Exact n)  = moduleName (nameModule n)
rdrNameModule (Unqual n) = pprPanic "rdrNameModule" (ppr n)

rdrNameOcc :: RdrName -> OccName
rdrNameOcc (Qual _ occ) = occ
rdrNameOcc (Unqual occ) = occ
rdrNameOcc (Orig _ occ) = occ
rdrNameOcc (Exact name) = nameOccName name

setRdrNameSpace :: RdrName -> NameSpace -> RdrName
-- This rather gruesome function is used mainly by the parser
-- When parsing		data T a = T | T1 Int
-- we parse the data constructors as *types* because of parser ambiguities,
-- so then we need to change the *type constr* to a *data constr*
--
-- The original-name case *can* occur when parsing
-- 		data [] a = [] | a : [a]
-- For the orig-name case we return an unqualified name.
setRdrNameSpace (Unqual occ) ns = Unqual (setOccNameSpace ns occ)
setRdrNameSpace (Qual m occ) ns = Qual m (setOccNameSpace ns occ)
setRdrNameSpace (Orig m occ) ns = Orig m (setOccNameSpace ns occ)
setRdrNameSpace (Exact n)    ns = Unqual (setOccNameSpace ns (nameOccName n))
\end{code}

\begin{code}
	-- These two are the basic constructors
mkRdrUnqual :: OccName -> RdrName
mkRdrUnqual occ = Unqual occ

mkRdrQual :: ModuleName -> OccName -> RdrName
mkRdrQual mod occ = Qual mod occ

mkOrig :: ModuleName -> OccName -> RdrName
mkOrig mod occ = Orig mod occ

mkIfaceOrig :: NameSpace -> EncodedFS -> EncodedFS -> RdrName
mkIfaceOrig ns m n = Orig (mkSysModuleNameFS m) (mkSysOccFS ns n)


	-- These two are used when parsing source files
	-- They do encode the module and occurrence names
mkUnqual :: NameSpace -> UserFS -> RdrName
mkUnqual sp n = Unqual (mkOccFS sp n)

mkQual :: NameSpace -> (UserFS, UserFS) -> RdrName
mkQual sp (m, n) = Qual (mkModuleNameFS m) (mkOccFS sp n)

getRdrName :: NamedThing thing => thing -> RdrName
getRdrName name = Exact (getName name)

nameRdrName :: Name -> RdrName
nameRdrName name = Exact name

qualifyRdrName :: ModuleName -> RdrName -> RdrName
	-- Sets the module name of a RdrName, even if it has one already
qualifyRdrName mod rn = Qual mod (rdrNameOcc rn)

unqualifyRdrName :: RdrName -> RdrName
unqualifyRdrName rdr_name = Unqual (rdrNameOcc rdr_name)

mkRdrNameWkr :: RdrName -> RdrName 	-- Worker-ify it
mkRdrNameWkr rdr_name = Qual (rdrNameModule rdr_name)
		 	     (mkWorkerOcc (rdrNameOcc rdr_name))

origFromName :: Name -> RdrName
origFromName n = Orig (moduleName (nameModule n)) (nameOccName n)
\end{code}

\begin{code}
	-- This guy is used by the reader when HsSyn has a slot for
	-- an implicit name that's going to be filled in by
	-- the renamer.  We can't just put "error..." because
	-- we sometimes want to print out stuff after reading but
	-- before renaming
dummyRdrVarName = Unqual (mkVarOcc FSLIT("V-DUMMY"))
dummyRdrTcName  = Unqual (mkOccFS tcName FSLIT("TC-DUMMY"))
\end{code}


\begin{code}
isRdrDataCon rn = isDataOcc (rdrNameOcc rn)
isRdrTyVar   rn = isTvOcc   (rdrNameOcc rn)
isRdrTc      rn = isTcOcc   (rdrNameOcc rn)

isUnqual (Unqual _) = True
isUnqual other	    = False

isQual (Qual _ _) = True
isQual _	  = False

isOrig (Orig _ _) = True
isOrig _	  = False

isExact (Exact _) = True
isExact other	= False

isExact_maybe (Exact n) = Just n
isExact_maybe other	= Nothing
\end{code}


%************************************************************************
%*									*
\subsection{Instances}
%*									*
%************************************************************************

\begin{code}
instance Outputable RdrName where
    ppr (Exact name)   = ppr name
    ppr (Unqual occ)   = ppr occ <+> ppr_name_space occ
    ppr (Qual mod occ) = ppr mod <> dot <> ppr occ <+> ppr_name_space occ
    ppr (Orig mod occ) = ppr mod <> dot <> ppr occ <+> ppr_name_space occ

ppr_name_space occ = ifPprDebug (parens (text (occNameFlavour occ)))

instance OutputableBndr RdrName where
    pprBndr _ n 
	| isTvOcc (rdrNameOcc n) = char '@' <+> ppr n
	| otherwise		 = ppr n

pprUnqualRdrName rdr_name = ppr (rdrNameOcc rdr_name)

instance Eq RdrName where
    a == b = case (a `compare` b) of { EQ -> True;  _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False; _ -> True }

instance Ord RdrName where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <	 b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }

	-- Unqual < Qual < Orig < Exact
    compare (Exact n1)    (Exact n2)  = n1 `compare` n2
    compare (Qual m1 o1) (Qual m2 o2) = (o1 `compare` o2) `thenCmp` (m1 `compare` m2) 
    compare (Orig m1 o1) (Orig m2 o2) = (o1 `compare` o2) `thenCmp` (m1 `compare` m2) 
    compare (Unqual o1)  (Unqual  o2) = o1 `compare` o2
 
	-- Convert Exact to Orig
    compare (Exact n1)    n2	      = origFromName n1 `compare` n2
    compare n1		  (Exact n2)  = n1 `compare` origFromName n2

    compare (Unqual _)   _ 	      = LT
    compare (Qual _ _)   (Orig _ _)   = LT
    compare _		 _	      = GT
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
instance Binary RdrName where
    put_ bh (Unqual aa) = do
	    putByte bh 0
	    put_ bh aa

    put_ bh (Qual aa ab) = do
	    putByte bh 1
	    put_ bh aa
	    put_ bh ab

    put_ bh (Orig aa ab) = do
	    putByte bh 2
	    put_ bh aa
	    put_ bh ab

    put_ bh (Exact n) = pprPanic "No Binary instance for RdrName.Exact" (ppr n)

    get bh = do
	  h <- getByte bh
	  case h of
	    0 -> do aa <- get bh
		    return (Unqual aa)
	    1 -> do aa <- get bh
		    ab <- get bh
		    return (Qual aa ab)
	    _ -> do aa <- get bh
		    ab <- get bh
		    return (Orig aa ab)
\end{code}
