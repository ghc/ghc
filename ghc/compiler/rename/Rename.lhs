%
% (c) The GRASP Project, Glasgow University, 1992-1996
%
\section[Rename]{Renaming and dependency analysis passes}

\begin{code}
#include "HsVersions.h"

module Rename ( renameModule ) where

import PreludeGlaST	( thenPrimIO, newVar, MutableVar(..) )

import Ubiq

import HsSyn
import RdrHsSyn		( RdrNameHsModule(..), RdrNameImportDecl(..) )
import RnHsSyn		( RnName, RenamedHsModule(..), isRnTyConOrClass, isRnWired )

--ToDo:rm: all for debugging only
import Maybes
import Name
import Outputable
import RnIfaces
import PprStyle
import Pretty
import FiniteMap
import Util (pprPanic, pprTrace)

import ParseUtils	( ParsedIface(..), RdrIfaceDecl(..), RdrIfaceInst(..) )
import RnMonad
import RnNames		( getGlobalNames, GlobalNameInfo(..) )
import RnSource		( rnSource )
import RnIfaces		( findHiFiles, rnIfaces, finalIfaceInfo, VersionInfo(..) )
import RnUtils		( RnEnv(..), extendGlobalRnEnv, emptyRnEnv, multipleOccWarn )

import Bag		( isEmptyBag, unionBags, unionManyBags, bagToList, listToBag )
import CmdLineOpts	( opt_HiDirList, opt_SysHiDirList )
import ErrUtils		( Error(..), Warning(..) )
import FiniteMap	( emptyFM, eltsFM, fmToList, lookupFM{-ToDo:rm-} )
import Maybes		( catMaybes )
import Name		( isLocallyDefined, mkBuiltinName, Name, RdrName(..) )
import PrelInfo		( BuiltinNames(..), BuiltinKeys(..) )
import PrelMods		( pRELUDE )
import UniqFM		( emptyUFM, lookupUFM, addListToUFM_C, eltsUFM )
import UniqSupply	( splitUniqSupply )
import Util		( panic, assertPanic )
\end{code}

\begin{code}
renameModule :: BuiltinNames
	     -> BuiltinKeys
	     -> UniqSupply
	     -> RdrNameHsModule

	     -> IO (RenamedHsModule, 	-- output, after renaming
		    RnEnv,		-- final env (for renaming derivings)
		    [Module],	   	-- imported modules; for profiling

	            VersionInfo,      	-- version info; for usage
		    [Module],	   	-- instance modules; for iface

		    Bag Error,
		    Bag Warning)
\end{code} 

ToDo: May want to arrange to return old interface for this module!
ToDo: Builtin names which must be read.
ToDo: Deal with instances (instance version, this module on instance list ???)

\begin{code}
renameModule b_names b_keys us
   	     input@(HsModule mod _ _ imports _ _ _ _ _ _ _ _ _ _)

  = --pprTrace "builtins:\n" (case b_names of { (builtin_ids, builtin_tcs) ->
    --			    ppAboves [ ppCat (map ppPStr (keysFM builtin_ids))
    --				     , ppCat (map ppPStr (keysFM builtin_tcs))
    --				     , ppCat (map ppPStr (keysFM b_keys))
    --				     ]}) $

    findHiFiles opt_HiDirList opt_SysHiDirList	    >>=	         \ hi_files ->
    newVar (emptyFM, hi_files){-init iface cache-}  `thenPrimIO` \ iface_cache ->

    fixIO ( \ ~(_, _, _, _, rec_occ_fm, rec_export_fn) ->
    let
	rec_occ_fn :: Name -> [RdrName]
	rec_occ_fn n = case lookupUFM rec_occ_fm n of
		         Nothing        -> []
		         Just (rn,occs) -> occs

	global_name_info = (b_names, b_keys, rec_export_fn, rec_occ_fn)
    in
    getGlobalNames iface_cache global_name_info us1 input >>=
	\ (occ_env, imp_mods, unqual_imps, imp_fixes, top_errs, top_warns) ->

    if not (isEmptyBag top_errs) then
	return (rn_panic, rn_panic, top_errs, top_warns, emptyUFM, rn_panic)
    else

    -- No top-level name errors so rename source ...
    case initRn True mod occ_env us2
		(rnSource imp_mods unqual_imps imp_fixes input) of {
	((rn_module, export_fn, src_occs), src_errs, src_warns) ->

    --pprTrace "renameModule:" (ppCat (map (ppr PprDebug . fst) (bagToList src_occs))) $

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
    return (rn_module, imp_mods,
	    top_errs  `unionBags` src_errs,
	    top_warns `unionBags` src_warns `unionBags` listToBag occ_warns,
	    occ_fm, export_fn)

    }) >>= \ (rn_module, imp_mods, errs_so_far, warns_so_far, occ_fm, _) ->

    if not (isEmptyBag errs_so_far) then
	return (rn_panic, rn_panic, rn_panic, rn_panic, rn_panic, errs_so_far, warns_so_far)
    else

    -- No errors renaming source so rename the interfaces ...
    let
	-- split up all names that occurred in the source; between
	-- those that are defined therein and those merely mentioned.
	-- We also divide by tycon/class and value names (as usual).

	occ_rns = [ rn | (rn,_) <- eltsUFM occ_fm ]
		-- all occurrence names, from this module and imported

	(defined_here, defined_elsewhere)
	  = partition isLocallyDefined occ_rns

	(_, imports_used)
          = partition isRnWired defined_elsewhere

	(def_tcs, def_vals) = partition isRnTyConOrClass defined_here
	(occ_tcs, occ_vals) = partition isRnTyConOrClass occ_rns
		-- the occ stuff includes *all* occurrences,
		-- including those for which we have definitions

	(orig_def_env, orig_def_dups)
	  = extendGlobalRnEnv emptyRnEnv (map pair_orig def_vals)
					 (map pair_orig def_tcs)
	(orig_occ_env, orig_occ_dups)
	  = extendGlobalRnEnv emptyRnEnv (map pair_orig occ_vals)
					 (map pair_orig occ_tcs)

        pair_orig rn = (origName rn, rn)

	-- we must ensure that the definitions of things in the BuiltinKey
	-- table which may be *required* by the typechecker etc are read.

	must_haves
	  = [ name_fn (mkBuiltinName u pRELUDE str) 
	    | (str, (u, name_fn)) <- fmToList b_keys,
	      str `notElem` [ SLIT("main"), SLIT("mainPrimIO")] ]
    in
    ASSERT (isEmptyBag orig_occ_dups)
    ASSERT (isEmptyBag orig_def_dups)

    rnIfaces iface_cache imp_mods us3 orig_def_env orig_occ_env
	     rn_module (must_haves ++ imports_used) >>=
	\ (rn_module_with_imports, final_env,
	   (implicit_val_fm, implicit_tc_fm),
	   (iface_errs, iface_warns)) ->
    let
        all_imports_used = imports_used ++ eltsFM implicit_tc_fm
					++ eltsFM implicit_val_fm
    in
    finalIfaceInfo iface_cache all_imports_used imp_mods >>=
	\ (version_info, instance_mods) ->

    return (rn_module_with_imports,
	    final_env,
	    imp_mods,
	    version_info,
	    instance_mods, 
	    errs_so_far  `unionBags` iface_errs,
	    warns_so_far `unionBags` iface_warns)
  where
    rn_panic = panic "renameModule: aborted with errors"

    (us1, us') = splitUniqSupply us
    (us2, us3) = splitUniqSupply us'
\end{code}

\begin{code}
pprPIface (ParsedIface m v mv lcm exm ims lfx ltdm lvdm lids ldp)
  = ppAboves [
	ppCat [ppPStr SLIT("interface"), ppPStr m, ppInt v,
	       case mv of { Nothing -> ppNil; Just n -> ppInt n }],

	ppPStr SLIT("__versions__"),
	ppAboves [ ppCat[ppPStr n, ppInt v] | (n,v) <- fmToList lcm ],

	ppPStr SLIT("__exports__"),
	ppAboves [ ppBesides[ppPStr n, ppSP, ppr PprDebug rn,
			     case ex of {ExportAll -> ppStr "(..)"; _ -> ppNil}]
		 | (n,(rn,ex)) <- fmToList exm ],

	pp_ims (bagToList ims),
	pp_fixities lfx,
	pp_decls ltdm lvdm,
	pp_insts (bagToList lids),
	pp_pragmas ldp
    ]
  where
    pp_ims [] = ppNil
    pp_ims ms = ppAbove (ppPStr SLIT("__instance_modules__"))
			(ppCat (map ppPStr ms))

    pp_fixities fx
      | isEmptyFM fx = ppNil
      | otherwise = ppAboves (ppPStr SLIT("__fixities__")
		   : [ ppr PprDebug fix | (n, fix) <- fmToList fx])

    pp_decls tds vds = ppAboves (ppPStr SLIT("__declarations__")
			      : [ pprRdrIfaceDecl d | (n, d) <- fmToList tds ++ fmToList vds])

    pp_insts [] = ppNil
    pp_insts is = ppAboves (ppPStr SLIT("__instances__")
			      : [ pprRdrInstDecl i | i <- is])

    pp_pragmas ps | isEmptyFM ps = ppNil
		  | otherwise = panic "Rename.pp_pragmas"

pprRdrIfaceDecl (TypeSig tc _ decl)
  = ppBesides [ppStr "tycon=", ppr PprDebug tc, ppStr "; ", ppr PprDebug decl]

pprRdrIfaceDecl (NewTypeSig tc dc _ decl)
  = ppBesides [ppStr "tycon=", ppr PprDebug tc, ppStr "; datacon=", ppr PprDebug dc,
	       ppStr "; ", ppr PprDebug decl]

pprRdrIfaceDecl (DataSig tc dcs dfs _ decl)
  = ppBesides [ppStr "tycon=", ppr PprDebug tc, ppStr "; datacons=", ppr PprDebug dcs,
	       ppStr "; fields=", ppr PprDebug dfs, ppStr "; ", ppr PprDebug decl]

pprRdrIfaceDecl (ClassSig c ops _ decl)
  = ppBesides [ppStr "class=", ppr PprDebug c, ppStr "; ops=", ppr PprDebug ops,
	       ppStr "; ", ppr PprDebug decl]

pprRdrIfaceDecl (ValSig f _ ty)
  = ppBesides [ppr PprDebug f, ppStr " :: ", ppr PprDebug ty]

pprRdrInstDecl (InstSig c t _ decl)
  = ppBesides [ppStr "class=", ppr PprDebug c, ppStr " type=", ppr PprDebug t, ppStr "; ",
		ppr PprDebug decl]
\end{code}
