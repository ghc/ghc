%
% (c) The GRASP Project, Glasgow University, 1992-1996
%
\section[Rename]{Renaming and dependency analysis passes}

\begin{code}
#include "HsVersions.h"

module Rename ( renameModule ) where

import PreludeGlaST	( thenPrimIO, returnPrimIO, fixPrimIO, newVar, MutableVar(..) )

import Ubiq

import HsSyn
import RdrHsSyn		( RdrNameHsModule(..), RdrNameImportDecl(..) )
import RnHsSyn		( RnName, RenamedHsModule(..), isRnTyCon, isRnClass )

import RnMonad
import RnNames		( getGlobalNames, GlobalNameInfo(..) )
import RnSource		( rnSource )
import RnIfaces		( rnInterfaces, finalIfaceInfo, VersionInfo(..), ParsedIface )
import RnUtils		( extendGlobalRnEnv, emptyRnEnv, multipleOccWarn )
import MainMonad

import Bag		( isEmptyBag, unionBags, bagToList, listToBag )
import ErrUtils		( Error(..), Warning(..) )
import FiniteMap	( emptyFM, eltsFM )
import Name		( getOrigNameRdr, isLocallyDefined, Name, RdrName(..) )
import PrelInfo		( BuiltinNames(..), BuiltinKeys(..) )
import UniqFM		( emptyUFM, lookupUFM, addListToUFM_C, eltsUFM )
import UniqSupply	( splitUniqSupply )
import Util		( panic, assertPanic )

findHiFiles :: PrimIO (FiniteMap Module FAST_STRING)
findHiFiles = returnPrimIO emptyFM
\end{code}

\begin{code}
renameModule :: BuiltinNames
	     -> BuiltinKeys
	     -> UniqSupply
	     -> RdrNameHsModule

	     -> MainIO
		(
		 RenamedHsModule,  -- output, after renaming
		 [Module],	   -- imported modules; for profiling

	         VersionInfo,      -- version info; for usage
		 [Module],	   -- instance modules; for iface

		 Bag Error,
		 Bag Warning
		)
\end{code}

ToDo: May want to arrange to return old interface for this module!
ToDo: Return OrigName RnEnv to rename derivings etc with.
ToDo: Builtin names which must be read.
ToDo: Deal with instances (instance version, this module on instance list ???)

\begin{code}
renameModule b_names b_keys us
   	     input@(HsModule mod _ _ imports _ _ _ _ _ _ _ _ _ _)
  = findHiFiles			`thenPrimIO` \ hi_files ->
    newVar (emptyFM, hi_files)	`thenPrimIO` \ iface_var ->

    fixPrimIO ( \ ~(_, _, _, _, rec_occ_fm, rec_export_fn) ->
    let
	rec_occ_fn :: Name -> [RdrName]
	rec_occ_fn n = case lookupUFM rec_occ_fm n of
		         Nothing        -> []
		         Just (rn,occs) -> occs

	global_name_info = (b_names, b_keys, rec_export_fn, rec_occ_fn)
    in
    getGlobalNames iface_var global_name_info us1 input
		`thenPrimIO` \ (occ_env, imp_mods, imp_fixes, top_errs, top_warns) ->

    if not (isEmptyBag top_errs) then
	returnPrimIO (rn_panic, rn_panic, top_errs, top_warns, emptyUFM, rn_panic)
    else

    -- No top-level name errors so rename source ...
    case initRn True mod occ_env us2
		(rnSource imp_mods imp_fixes input) of {
	((rn_module, export_fn, src_occs), src_errs, src_warns) ->

    let
	occ_fm :: UniqFM (RnName, [RdrName])

	occ_list = [ (rn,(rn,[occ])) | (rn,occ) <- bagToList src_occs]
        occ_fm = addListToUFM_C insert_occ emptyUFM occ_list

	insert_occ (rn,olds) (rn',[new]) = (rn, insert new olds)

        insert new []         = [new]
        insert new xxs@(x:xs) = case cmp new x of LT_  -> new : xxs
			      			  EQ_  -> xxs
			      			  GT__ -> x : insert new xs

	occ_warns = map multipleOccWarn (filter multiple_occs (eltsUFM occ_fm))
	multiple_occs (rn, (o1:o2:_)) = True
	multiple_occs _               = False
    in
    returnPrimIO (rn_module, imp_mods,
		  top_errs  `unionBags` src_errs,
		  top_warns `unionBags` src_warns `unionBags` listToBag occ_warns,
		  occ_fm, export_fn)

    }) `thenPrimIO` \ (rn_module, imp_mods, errs_so_far, warns_so_far, occ_fm, _) ->

    if not (isEmptyBag errs_so_far) then
	returnMn (rn_panic, rn_panic, rn_panic, rn_panic,
		  errs_so_far, warns_so_far)
    else

    -- No errors renaming source so rename the interfaces ...
    let
        imports_used = [ rn | (rn,_) <- eltsUFM occ_fm, not (isLocallyDefined rn) ]
	(import_tcs, import_vals) = partition (\ rn -> isRnTyCon rn || isRnClass rn) imports_used

	(orig_env, orig_dups) = extendGlobalRnEnv emptyRnEnv (map pair_orig import_vals)
						             (map pair_orig import_tcs)
        pair_orig rn = (getOrigNameRdr rn, rn)

	-- ToDo: Do we need top-level names from this module in orig_env ???
    in
    ASSERT (isEmptyBag orig_dups)
    rnInterfaces iface_var orig_env us3 rn_module imports_used
		`thenPrimIO` \ (rn_module_with_imports,
				(implicit_val_fm, implicit_tc_fm),
				iface_errs, iface_warns) ->
    let
        all_imports_used = imports_used ++ eltsFM implicit_tc_fm ++ eltsFM implicit_val_fm
    in
    finalIfaceInfo iface_var all_imports_used imp_mods
		`thenPrimIO` \ (version_info, instance_mods) ->

    returnMn (rn_module_with_imports, imp_mods, 
	      version_info, instance_mods, 
	      errs_so_far  `unionBags` iface_errs,
	      warns_so_far `unionBags` iface_warns)

  where
    rn_panic = panic "renameModule: aborted with errors"

    (us1, us') = splitUniqSupply us
    (us2, us3) = splitUniqSupply us'
\end{code}
