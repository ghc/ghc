%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[MkIface]{Print an interface for a module}

\begin{code}
#include "HsVersions.h"

module MkIface (
	startIface, endIface,
	ifaceMain, ifaceInstances,
	ifaceDecls
    ) where

IMP_Ubiq(){-uitous-}
IMPORT_1_3(IO(Handle,hPutStr,openFile,hClose,IOMode(..)))

import HsSyn
import RdrHsSyn		( RdrName(..) )
import RnHsSyn		( SYN_IE(RenamedHsModule) )
import RnMonad

import TcInstUtil	( InstInfo(..) )

import CmdLineOpts
import Id		( idType, dataConRawArgTys, dataConFieldLabels, isDataCon,
			  getIdInfo, idWantsToBeINLINEd, wantIdSigInIface,
			  dataConStrictMarks, StrictnessMark(..), 
			  SYN_IE(IdSet), idSetToList, unionIdSets, unitIdSet, minusIdSet, 
			  isEmptyIdSet, elementOfIdSet, emptyIdSet, mkIdSet,
			  GenId{-instance NamedThing/Outputable-}
			)
import IdInfo		( StrictnessInfo, ArityInfo, Unfolding,
			  arityInfo, ppArityInfo, strictnessInfo, ppStrictnessInfo, 
			  getWorkerId_maybe, bottomIsGuaranteed 
			)
import CoreSyn		( SYN_IE(CoreExpr), SYN_IE(CoreBinding), GenCoreExpr, GenCoreBinding(..) )
import CoreUnfold	( calcUnfoldingGuidance, UnfoldingGuidance(..) )
import FreeVars		( addExprFVs )
import Name		( isLocallyDefined, isWiredInName, modAndOcc, getName, pprOccName,
			  OccName, occNameString, nameOccName, nameString, isExported, pprNonSym,
			  Name {-instance NamedThing-}, Provenance
			)
import TyCon		( TyCon(..){-instance NamedThing-}, NewOrData(..) )
import Class		( GenClass(..){-instance NamedThing-}, GenClassOp, classOpLocalType )
import FieldLabel	( FieldLabel{-instance NamedThing-} )
import Type		( mkSigmaTy, mkDictTy, getAppTyCon, splitForAllTy )
import TyVar		( GenTyVar {- instance Eq -} )
import Unique		( Unique {- instance Eq -} )

import PprEnv		-- not sure how much...
import PprStyle		( PprStyle(..) )
import PprType
import PprCore		( pprIfaceUnfolding )
import Pretty
import Unpretty		-- ditto


import Bag		( bagToList )
import Maybes		( catMaybes, maybeToBool )
import FiniteMap	( emptyFM, addToFM, lookupFM, fmToList, eltsFM, FiniteMap )
import UniqFM		( UniqFM, lookupUFM, listToUFM )
import Util		( sortLt, zipWithEqual, zipWith3Equal, mapAccumL,
			  assertPanic, panic{-ToDo:rm-}, pprTrace )

\end{code}

We have a function @startIface@ to open the output file and put
(something like) ``interface Foo'' in it.  It gives back a handle
for subsequent additions to the interface file.

We then have one-function-per-block-of-interface-stuff, e.g.,
@ifaceExportList@ produces the @__exports__@ section; it appends
to the handle provided by @startIface@.

\begin{code}
startIface  :: Module
	    -> IO (Maybe Handle) -- Nothing <=> don't do an interface

ifaceMain   :: Maybe Handle
	    -> InterfaceDetails
	    -> IO ()

ifaceInstances :: Maybe Handle -> Bag InstInfo -> IO ()

ifaceDecls :: Maybe Handle
	   -> RenamedHsModule
	   -> [Id]		-- Ids used at code-gen time; they have better pragma info!
	   -> [CoreBinding]	-- In dependency order, later depend on earlier
	   -> IO ()

endIface    :: Maybe Handle -> IO ()
\end{code}

\begin{code}
startIface mod
  = case opt_ProduceHi of
      Nothing -> return Nothing -- not producing any .hi file
      Just fn ->
	openFile fn WriteMode	>>= \ if_hdl ->
	hPutStr if_hdl ("{-# GHC_PRAGMA INTERFACE VERSION 20 #-}\n_interface_ "++ _UNPK_ mod ++ "\n") >>
	return (Just if_hdl)

endIface Nothing	= return ()
endIface (Just if_hdl)	= hPutStr if_hdl "\n" >> hClose if_hdl
\end{code}


\begin{code}
ifaceMain Nothing iface_stuff = return ()
ifaceMain (Just if_hdl)
	  (import_usages, ExportEnv avails fixities, instance_modules)
  =
    ifaceInstanceModules	if_hdl instance_modules		>>
    ifaceUsages			if_hdl import_usages		>>
    ifaceExports		if_hdl avails			>>
    ifaceFixities		if_hdl fixities			>>
    return ()

ifaceDecls Nothing rn_mod final_ids simplified = return ()
ifaceDecls (Just hdl) 
	   (HsModule _ _ _ _ _ decls _)
	   final_ids binds
  | null decls = return ()		 
	--  You could have a module with just (re-)exports/instances in it
  | otherwise
  = hPutStr hdl "_declarations_\n"	>>
    ifaceTCDecls hdl decls		>>
    ifaceBinds hdl final_ids binds	>>
    return ()
\end{code}

\begin{code}
ifaceUsages if_hdl import_usages
  = hPutStr if_hdl "_usages_\n"   >>
    hPutCol if_hdl upp_uses (sortLt lt_imp_vers import_usages)
  where
    upp_uses (m, mv, versions)
      = uppBesides [upp_module m, uppSP, uppInt mv, uppPStr SLIT(" :: "),
		    upp_import_versions (sort_versions versions), uppSemi]

	-- For imported versions we do print the version number
    upp_import_versions nvs
      = uppIntersperse uppSP [ uppCat [ppr_unqual_name n, uppInt v] | (n,v) <- nvs ]


ifaceInstanceModules if_hdl [] = return ()
ifaceInstanceModules if_hdl imods
  = hPutStr if_hdl "_instance_modules_\n" >>
    hPutStr if_hdl (uppShow 0 (uppCat (map uppPStr imods))) >>
    hPutStr if_hdl "\n"

ifaceExports if_hdl [] = return ()
ifaceExports if_hdl avails
  = hPutStr if_hdl "_exports_\n"			>>
    hPutCol if_hdl upp_avail (sortLt lt_avail avails)

ifaceFixities if_hdl [] = return ()
ifaceFixities if_hdl fixities 
  = hPutStr if_hdl "_fixities_\n"		>>
    hPutCol if_hdl upp_fixity fixities

ifaceTCDecls if_hdl decls
  =  hPutCol if_hdl ppr_decl tc_decls_for_iface
  where
    tc_decls_for_iface = sortLt lt_decl (filter for_iface decls)
    for_iface decl@(ClD _) = for_iface_name (hsDeclName decl)
    for_iface decl@(TyD _) = for_iface_name (hsDeclName decl)
    for_iface other_decl   = False

    for_iface_name name = isLocallyDefined name && 
			  not (isWiredInName name)

    lt_decl d1 d2 = hsDeclName d1 < hsDeclName d2
\end{code}			 

%************************************************************************
%*				 					*
\subsection{Instance declarations}
%*				 					*
%************************************************************************


\begin{code}			 
ifaceInstances Nothing{-no iface handle-} _ = return ()
				 
ifaceInstances (Just if_hdl) inst_infos
  | null togo_insts = return ()		 
  | otherwise 	    = hPutStr if_hdl "_instances_\n" >>
		      hPutCol if_hdl pp_inst (sortLt lt_inst togo_insts)
  where				 
    togo_insts	= filter is_togo_inst (bagToList inst_infos)
    is_togo_inst (InstInfo _ _ _ _ _ dfun_id _ _ _) = isLocallyDefined dfun_id
				 
    -------			 
    lt_inst (InstInfo _ _ _ _ _ dfun_id1 _ _ _)
	    (InstInfo _ _ _ _ _ dfun_id2 _ _ _)
      = getOccName dfun_id1 < getOccName dfun_id2
	-- The dfuns are assigned names df1, df2, etc, in order of original textual
	-- occurrence, and this makes as good a sort order as any

    -------			 
    pp_inst (InstInfo clas tvs ty theta _ dfun_id _ _ _)
      = let			 
	    forall_ty     = mkSigmaTy tvs theta (mkDictTy clas ty)
	    renumbered_ty = renumber_ty forall_ty
	in			 
	uppBesides [uppPStr SLIT("instance "), ppr_ty renumbered_ty, 
		    uppPStr SLIT(" = "), ppr_unqual_name dfun_id, uppSemi]
\end{code}


%************************************************************************
%*				 					*
\subsection{Printing values}
%*				 					*
%************************************************************************

\begin{code}
ifaceId :: (Id -> IdInfo)		-- This function "knows" the extra info added
					-- by the STG passes.  Sigh

	    -> IdSet			-- Set of Ids that are needed by earlier interface
					-- file emissions.  If the Id isn't in this set, and isn't
					-- exported, there's no need to emit anything
	    -> Id
	    -> CoreExpr			-- The Id's right hand side
	    -> Maybe (Pretty, IdSet)	-- The emitted stuff, plus a possibly-augmented set of needed Ids

ifaceId get_idinfo needed_ids id rhs
  | not (wantIdSigInIface (id `elementOfIdSet` needed_ids) 
			  opt_OmitInterfacePragmas
			  id)
  = Nothing 		-- Well, that was easy!

ifaceId get_idinfo needed_ids id rhs
  = Just (ppCat [sig_pretty, prag_pretty, ppSemi], new_needed_ids)
  where
    idinfo     = get_idinfo id
    ty_pretty  = pprType PprInterface (initNmbr (nmbrType (idType id)))
    sig_pretty = ppBesides [ppr PprInterface (getOccName id), ppPStr SLIT(" :: "), ty_pretty]

    prag_pretty | opt_OmitInterfacePragmas = ppNil
		| otherwise		   = ppCat [arity_pretty, strict_pretty, unfold_pretty]

    ------------  Arity  --------------
    arity_pretty  = ppArityInfo PprInterface (arityInfo idinfo)

    ------------  Strictness  --------------
    strict_info   = strictnessInfo idinfo
    maybe_worker  = getWorkerId_maybe strict_info
    strict_pretty = ppStrictnessInfo PprInterface strict_info

    ------------  Unfolding  --------------
    unfold_pretty | show_unfold = ppCat [ppStr "_U_", pprIfaceUnfolding rhs]
		  | otherwise   = ppNil

    show_unfold = not (maybeToBool maybe_worker) &&		-- Unfolding is implicit
		  not (bottomIsGuaranteed strict_info) &&	-- Ditto
		  case guidance of 				-- Small enough to show
			UnfoldNever -> False
			other       -> True 

    guidance    = calcUnfoldingGuidance (idWantsToBeINLINEd id) 
					opt_InterfaceUnfoldThreshold
					rhs

    
    ------------  Extra free Ids  --------------
    new_needed_ids = (needed_ids `minusIdSet` unitIdSet id)	`unionIdSets` 
		     extra_ids

    extra_ids | opt_OmitInterfacePragmas = emptyIdSet
	      | otherwise		 = worker_ids	`unionIdSets`
					   unfold_ids

    worker_ids = case maybe_worker of
			Just wkr -> unitIdSet wkr
			Nothing  -> emptyIdSet

    unfold_ids | show_unfold = free_vars
	       | otherwise   = emptyIdSet
			     where
			       (_,free_vars) = addExprFVs interesting emptyIdSet rhs
			       interesting bound id = not (id `elementOfIdSet` bound) &&
						      not (isDataCon id) &&
						      not (isWiredInName (getName id)) &&
						      isLocallyDefined id 
\end{code}

\begin{code}
ifaceBinds :: Handle
	   -> [Id]		-- Ids used at code-gen time; they have better pragma info!
	   -> [CoreBinding]	-- In dependency order, later depend on earlier
	   -> IO ()

ifaceBinds hdl final_ids binds
  = hPutStr hdl (uppShow 0 (prettyToUn (ppAboves pretties)))	>>
    hPutStr hdl "\n"
  where
    final_id_map  = listToUFM [(id,id) | id <- final_ids]
    get_idinfo id = case lookupUFM final_id_map id of
			Just id' -> getIdInfo id'
			Nothing  -> pprTrace "ifaceBinds not found:" (ppr PprDebug id) $
				    getIdInfo id

    pretties = go emptyIdSet (reverse binds)	-- Reverse so that later things will 
						-- provoke earlier ones to be emitted
    go needed [] = if not (isEmptyIdSet needed) then
			pprTrace "ifaceBinds: free vars:" 
				  (ppSep (map (ppr PprDebug) (idSetToList needed))) $
			[]
		   else
			[]

    go needed (NonRec id rhs : binds)
	= case ifaceId get_idinfo needed id rhs of
		Nothing		       -> go needed binds
		Just (pretty, needed') -> pretty : go needed' binds

	-- Recursive groups are a bit more of a pain.  We may only need one to
	-- start with, but it may call out the next one, and so on.  So we
	-- have to look for a fixed point.
    go needed (Rec pairs : binds)
	= pretties ++ go needed'' binds
	where
	  (needed', pretties) = go_rec needed pairs
	  needed'' = needed' `minusIdSet` mkIdSet (map fst pairs)
		-- Later ones may spuriously cause earlier ones to be "needed" again

    go_rec :: IdSet -> [(Id,CoreExpr)] -> (IdSet, [Pretty])
    go_rec needed pairs
	| null pretties = (needed, [])
	| otherwise	= (final_needed, more_pretties ++ pretties)
	where
	  reduced_pairs		 	= [pair | (pair,Nothing) <- pairs `zip` maybes]
	  pretties		 	= catMaybes maybes
	  (needed', maybes)	 	= mapAccumL do_one needed pairs
	  (final_needed, more_pretties) = go_rec needed' reduced_pairs

	  do_one needed (id,rhs) = case ifaceId get_idinfo needed id rhs of
					Nothing		       -> (needed,  Nothing)
					Just (pretty, needed') -> (needed', Just pretty)
\end{code}


%************************************************************************
%*				 					*
\subsection{Random small things}
%*				 					*
%************************************************************************
				 
\begin{code}
upp_avail NotAvailable    = uppNil
upp_avail (Avail name ns) = uppBesides [upp_module mod, uppSP, 
					upp_occname occ, uppSP, 
					upp_export ns]
			     where
				(mod,occ) = modAndOcc name

upp_export []    = uppNil
upp_export names = uppBesides [uppStr "(", 
			       uppIntersperse uppSP (map (upp_occname . getOccName) names), 
			       uppStr ")"]

upp_fixity (occ, Fixity prec dir, prov) = uppBesides [upp_dir dir, uppSP, 
						      uppInt prec, uppSP, 
					       	      upp_occname occ, uppSemi]
upp_dir InfixR = uppStr "infixr"				 
upp_dir InfixL = uppStr "infixl"				 
upp_dir InfixN = uppStr "infix"				 

ppr_unqual_name :: NamedThing a => a -> Unpretty		-- Just its occurrence name
ppr_unqual_name name = upp_occname (getOccName name)

ppr_name :: NamedThing a => a -> Unpretty		-- Its full name
ppr_name   n = uppPStr (nameString (getName n))

upp_occname :: OccName -> Unpretty
upp_occname occ = uppPStr (occNameString occ)

upp_module :: Module -> Unpretty
upp_module mod = uppPStr mod

uppSemid   x = uppBeside (prettyToUn (ppr PprInterface x)) uppSemi -- micro util

ppr_ty	  ty = prettyToUn (pprType PprInterface ty)
ppr_tyvar tv = prettyToUn (ppr PprInterface tv)
ppr_tyvar_bndr tv = prettyToUn (pprTyVarBndr PprInterface tv)

ppr_decl decl = prettyToUn (ppr PprInterface decl) `uppBeside` uppSemi

renumber_ty ty = initNmbr (nmbrType ty)
\end{code}


%************************************************************************
%*				 					*
\subsection{Comparisons
%*				 					*
%************************************************************************
				 

The various sorts above simply prevent unnecessary "wobbling" when
things change that don't have to.  We therefore compare lexically, not
by unique

\begin{code}
lt_avail :: AvailInfo -> AvailInfo -> Bool

NotAvailable `lt_avail` (Avail _ _)  = True
(Avail n1 _) `lt_avail` (Avail n2 _) = n1 `lt_name` n2
any	     `lt_avail` NotAvailable = False

lt_name :: Name -> Name -> Bool
n1 `lt_name` n2 = modAndOcc n1 < modAndOcc n2

lt_lexical :: NamedThing a => a -> a -> Bool
lt_lexical a1 a2 = getName a1 `lt_name` getName a2

lt_imp_vers :: ImportVersion a -> ImportVersion a -> Bool
lt_imp_vers (m1,_,_) (m2,_,_) = m1 < m2

sort_versions vs = sortLt lt_vers vs

lt_vers :: LocalVersion Name -> LocalVersion Name -> Bool
lt_vers (n1,v1) (n2,v2) = n1 `lt_name` n2
\end{code}


\begin{code}
hPutCol :: Handle 
	-> (a -> Unpretty)
	-> [a]
	-> IO ()
hPutCol hdl fmt xs = hPutStr hdl (uppShow 0 (uppAboves (map fmt xs))) >>
		     hPutStr hdl "\n"
\end{code}
