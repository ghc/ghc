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
	lookupRnEnv, lookupGlobalRnEnv, lookupTcRnEnv,

	lubExportFlag,

	qualNameErr,
	dupNamesErr
    ) where

IMP_Ubiq(){-uitous-}

import Bag		( Bag, emptyBag, snocBag, unionBags )
import CmdLineOpts	( opt_CompilingPrelude )
import ErrUtils		( addShortErrLocLine )
import FiniteMap	( FiniteMap, emptyFM, isEmptyFM,
			  lookupFM, addListToFM, addToFM )
import Maybes		( maybeToBool )
import Name		( RdrName(..), isQual, pprNonSym, getLocalName, ExportFlag(..) )
import PprStyle		( PprStyle(..) )
import Pretty
import RnHsSyn		( RnName )
import Util		( assertPanic )
\end{code}

*********************************************************
*							*
\subsection{RnEnv: renaming environment}
*							*
*********************************************************

Separate FiniteMaps are kept for lookup up Qual names,
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
lookupGlobalRnEnv :: RnEnv -> RdrName -> Maybe RnName
lookupTcRnEnv 	  :: RnEnv -> RdrName -> Maybe RnName
\end{code}

If the @RdrName@ is a @Qual@, @lookupValue@ looks it up in the global
value QualNames.  If it is @Unqual@, it looks it up first in the
ScopeStack, and if it isn't found there, then in the global
vaule Unqual Names.

@lookupTcRnEnv@ looks up tycons/classes in the alternative global
name space.

@extendGlobalRnEnv@ adds global names to the RnEnv. It takes separate
value and tycon/class name lists. It returns any duplicate names
seperately.

@extendRnEnv@ adds new local names to the ScopeStack in an RnEnv.
It optionally reports any shadowed names.

\begin{code}
emptyRnEnv
  = ((emptyFM, emptyFM, emptyFM, emptyFM), emptyFM)

extendGlobalRnEnv ((qual, unqual, tc_qual, tc_unqual), stack) val_list tc_list
  = ASSERT(isEmptyFM stack)
    (((qual', unqual', tc_qual', tc_unqual'), stack), tc_dups `unionBags` dups)
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
      Qual mod str -> lookup qual (str,mod)
			(if not opt_CompilingPrelude -- see below
			 then Nothing
			 else lookup unqual str Nothing)
  where
    lookup fm thing do_on_fail
      = case lookupFM fm thing of
	    found@(Just name) -> found
	    Nothing   	      -> do_on_fail

lookupGlobalRnEnv ((qual, unqual, _, _), _) rdr
  = case rdr of 
      Unqual str   -> lookupFM unqual str
      Qual mod str -> case (lookupFM qual (str,mod)) of
			Just xx -> Just xx
			Nothing -> if not opt_CompilingPrelude then
				      Nothing
				   else -- "[]" may have turned into "Prelude.[]" and
				        -- we are actually compiling "data [] a = ...";
					-- maybe the right thing is to get "Prelude.[]"
					-- into the "qual" table...
				      lookupFM unqual str

lookupTcRnEnv ((_, _, tc_qual, tc_unqual), _) rdr
  = case rdr of 
      Unqual str   -> lookupFM tc_unqual str
      Qual mod str -> case (lookupFM tc_qual (str,mod)) of -- as above
			Just xx -> Just xx
			Nothing -> if not opt_CompilingPrelude then
				      Nothing
				   else
				      lookupFM tc_unqual str
\end{code}

*********************************************************
*							*
\subsection{Export Flag Functions}
*							*
*********************************************************

\begin{code}
lubExportFlag ExportAll ExportAll = ExportAll
lubExportFlag ExportAll ExportAbs = ExportAll
lubExportFlag ExportAbs ExportAll = ExportAll
lubExportFlag ExportAbs ExportAbs = ExportAbs
\end{code}

*********************************************************
*							*
\subsection{Errors used *more than once* in the renamer}
*							*
*********************************************************

\begin{code}
qualNameErr descriptor (name,locn)
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "invalid use of qualified ", ppStr descriptor, ppStr ": ", pprNonSym sty name ] )

dupNamesErr descriptor ((name1,locn1) : dup_things) sty
  = ppAboves (item1 : map dup_item dup_things)
  where
    item1
      = addShortErrLocLine locn1 (\ sty ->
	ppBesides [ppStr "multiple declarations of a ", ppStr descriptor, ppStr " `", 
		   pprNonSym sty name1, ppStr "'" ]) sty

    dup_item (name, locn)
      = addShortErrLocLine locn (\ sty ->
	ppBesides [ppStr "here was another declaration of `",
		   pprNonSym sty name, ppStr "'" ]) sty
\end{code}

