%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnUtils]{Functions used by both renaming passes}

\begin{code}
#include "HsVersions.h"

module RnUtils (
	RnEnv(..), QualNames(..),
	UnqualNames(..), ScopeStack(..),
	emptyRnEnv, extendGlobalRnEnv, extendLocalRnEnv,
	lookupRnEnv, lookupTcRnEnv,

	unknownNameErr,
	badClassOpErr,
	qualNameErr,
	dupNamesErr,
	shadowedNameWarn,
	multipleOccWarn,

	-- ToDo: nuke/move? WDP 96/04/05
	GlobalNameMapper(..),  GlobalNameMappers(..)
    ) where

import Ubiq

import Bag		( Bag, emptyBag, snocBag, unionBags )
import ErrUtils		( addShortErrLocLine, addErrLoc )
import FiniteMap	( FiniteMap, emptyFM, isEmptyFM,
			  lookupFM, addListToFM, addToFM )
import Maybes		( maybeToBool )
import Name		( RdrName(..), isQual )
import Outputable	( pprNonOp, getLocalName )
import PprStyle		( PprStyle(..) )
import Pretty
import RnHsSyn		( RnName )
import Util		( assertPanic )

type GlobalNameMapper  = RnName -> Maybe Name
type GlobalNameMappers = (GlobalNameMapper, GlobalNameMapper)
\end{code}

*********************************************************
*							*
\subsection{RnEnv: renaming environment}
*							*
*********************************************************

Seperate FiniteMaps are kept for lookup up Qual names,
Unqual names and Local names.

\begin{code}
type RnEnv = ((QualNames, UnqualNames, QualNames, UnqualNames), ScopeStack)

type QualNames    = FiniteMap (FAST_STRING,Module) RnName
type UnqualNames  = FiniteMap FAST_STRING RnName
type ScopeStack   = FiniteMap FAST_STRING RnName

emptyRnEnv  	  :: RnEnv
extendGlobalRnEnv :: RnEnv -> [(RdrName,RnName)] -> [(RdrName,RnName)]
		  -> (RnEnv, Bag (RdrName, RnName, RnName))
extendLocalRnEnv  :: Bool -> RnEnv -> [RnName] -> (RnEnv, [RnName])
lookupRnEnv 	  :: RnEnv -> RdrName -> Maybe RnName
lookupTcRnEnv 	  :: RnEnv -> RdrName -> Maybe RnName
\end{code}

If the @RdrName@ is a @Qual@, @lookupValue@ looks it up in the global
value QualNames.  If it is @Unqual@, it looks it up first in the
ScopeStack, and if it isn't found there, then in the global
vaule Unqual Names.

@lookupTcRnEnv@ looks up tycons/classes in the alternative global
name space.

@extendGlobalRnEnv@ adds global names to the RnEnv. It takes seperate
value and tycon/class name lists. It returns any duplicate names
seperatle.

@extendRnEnv@ adds new local names to the ScopeStack in an RnEnv.
It optionally reports any shadowed names.

\begin{code}
emptyRnEnv
  = ((emptyFM, emptyFM, emptyFM, emptyFM), emptyFM)


extendGlobalRnEnv ((qual, unqual, tc_qual, tc_unqual), stack) val_list tc_list
  = ASSERT(isEmptyFM stack)
    (((qual', unqual', tc_qual, tc_unqual), stack), tc_dups `unionBags` dups)
  where
    (qual', unqual', dups)          = extend_global qual unqual val_list
    (tc_qual', tc_unqual', tc_dups) = extend_global tc_qual tc_unqual tc_list

    extend_global qual unqual rdr_list = (qual', unqual', dups)
      where
	(qual_list, unqual_list) = partition (isQual.fst) rdr_list
	qual_in   = map mk_qual qual_list
	unqual_in = map mk_unqual unqual_list
	mk_qual   (Qual m s, rn) = ((s,m), rn)
	mk_unqual (Unqual s, rn) = (s, rn)

	(qual', qual_dups)     = do_dups qual_in qual emptyBag (\ (s,m) -> Qual m s)
	(unqual', unqual_dups) = do_dups unqual_in unqual emptyBag Unqual

	dups = unqual_dups `unionBags` qual_dups

	do_dups [] fm dups to_rdr = (fm, dups)
	do_dups ((k,v):rest) fm dups to_rdr
          = case lookupFM fm k of
	      Nothing  -> do_dups rest (addToFM fm k v) dups to_rdr
	      Just cur -> do_dups rest fm (dups `snocBag` (to_rdr k, cur, v)) to_rdr


extendLocalRnEnv report_shadows (global, stack) new_local
  = ((global, new_stack), dups)
  where
    (new_stack, dups) = extend new_local stack

    extend names stack
      = if report_shadows then
	    do_shadows names stack []
	else
	    (addListToFM stack [ (getLocalName n, n) | n <- names], []) 

    do_shadows [] stack dups = (stack, dups)
    do_shadows (name:names) stack dups
      = do_shadows names (addToFM stack str name) ext_dups
      where
	str = getLocalName name
	ext_dups = if maybeToBool (lookupFM stack str)
		   then name:dups
		   else dups


lookupRnEnv ((qual, unqual, _, _), stack) rdr
  = case rdr of 
      Unqual str   -> lookup stack str (lookup unqual str Nothing)
      Qual mod str -> lookup qual (str,mod) Nothing
  where
    lookup fm thing do_on_fail
      = case lookupFM fm thing of
	    found@(Just name) -> found
	    Nothing   	      -> do_on_fail

lookupTcRnEnv ((_, _, tc_qual, tc_unqual), _) rdr
  = case rdr of 
      Unqual str   -> lookupFM tc_unqual str
      Qual mod str -> lookupFM tc_qual (str,mod)
\end{code}

*********************************************************
*							*
\subsection{Errors used in RnMonad}
*							*
*********************************************************

\begin{code}
unknownNameErr descriptor name locn
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "undefined ", ppStr descriptor, ppStr ": ", pprNonOp sty name] )

badClassOpErr clas op locn
  = addErrLoc locn "" ( \ sty ->
    ppBesides [ppChar '`', pprNonOp sty op, ppStr "' is not an operation of class `",
	      ppr sty clas, ppStr "'"] )

qualNameErr descriptor (name,locn)
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "invalid use of qualified ", ppStr descriptor, ppStr ": ", pprNonOp sty name ] )

dupNamesErr descriptor ((name1,locn1) : dup_things) sty
  = ppAboves (item1 : map dup_item dup_things)
  where
    item1
      = ppBesides [ ppr PprForUser locn1,
	    ppStr ": multiple declarations of a ", ppStr descriptor, ppStr ": ",
	    pprNonOp sty name1 ]

    dup_item (name, locn)
      = ppBesides [ ppr PprForUser locn,
	    ppStr ": here was another declaration of `", pprNonOp sty name, ppStr "'" ]

shadowedNameWarn locn shadow
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "more than one value with the same name (shadowing): ", ppr sty shadow] )

multipleOccWarn (name, occs) sty
  = ppBesides [ppStr "multiple names used to refer to `", ppr sty name, ppStr "': ",
	       ppInterleave ppComma (map (ppr sty) occs)]
\end{code}

