%
% (c) The GRASP Project, Glasgow University, 1992-1996
%
\section[Rename]{Renaming and dependency analysis passes}

\begin{code}
#include "HsVersions.h"

module Rename ( renameModule ) where

import PreludeGlaST	( thenPrimIO )

IMP_Ubiq()
IMPORT_1_3(List(partition))

import HsSyn
import RdrHsSyn		( RdrNameHsModule(..), RdrNameImportDecl(..) )
import RnHsSyn		( RnName(..){-.. is for Ix hack only-}, SYN_IE(RenamedHsModule), isRnTyConOrClass, isRnWired )

--ToDo:rm: all for debugging only
--import Maybes
--import Name
--import Outputable
--import RnIfaces
--import PprStyle
--import Pretty
--import FiniteMap
--import Util (pprPanic, pprTrace)

import ParseUtils	( ParsedIface(..), RdrIfaceDecl(..), RdrIfaceInst(..),
			  UsagesMap(..), VersionsMap(..)
			)
import RnMonad
import RnNames		( getGlobalNames, SYN_IE(GlobalNameInfo) )
import RnSource		( rnSource )
import RnIfaces		( rnIfaces, initIfaceCache, IfaceCache )
import RnUtils		( SYN_IE(RnEnv), extendGlobalRnEnv, emptyRnEnv )

import Bag		( isEmptyBag, unionBags, unionManyBags, bagToList, listToBag )
import CmdLineOpts	( opt_HiMap, opt_NoImplicitPrelude )
import ErrUtils		( SYN_IE(Error), SYN_IE(Warning) )
import FiniteMap	( emptyFM, eltsFM, fmToList, addToFM, FiniteMap )
import Maybes		( catMaybes )
import Name		( isLocallyDefined, mkWiredInName, getLocalName, isLocalName,
			  origName,
			  Name, RdrName(..), ExportFlag(..)
			)
--import PprStyle		-- ToDo:rm
import PrelInfo		( builtinNameMaps, builtinKeysMap, SYN_IE(BuiltinNames), SYN_IE(BuiltinKeys) )
import Pretty
import Unique		( ixClassKey )
import UniqFM		( emptyUFM, lookupUFM, addListToUFM_C, eltsUFM )
import UniqSupply	( splitUniqSupply )
import Util		( panic, assertPanic{-, pprTrace ToDo:rm-} )
\end{code}

\begin{code}
renameModule :: UniqSupply
	     -> RdrNameHsModule

	     -> IO (RenamedHsModule, 	-- output, after renaming
		    RnEnv,		-- final env (for renaming derivings)
		    [Module],	   	-- imported modules; for profiling

		    (Name -> ExportFlag,	-- export info
		     ([(Name,ExportFlag)],
		      [(Name,ExportFlag)])),

		    (UsagesMap,
	            VersionsMap,      	-- version info; for usage
		    [Module]),	   	-- instance modules; for iface

		    Bag Error,
		    Bag Warning)
\end{code} 

ToDo: May want to arrange to return old interface for this module!
ToDo: Deal with instances (instance version, this module on instance list ???)

\begin{code}
renameModule us input@(HsModule modname _ _ imports _ _ _ _ _ _ _ _ _ _)

  = {-
    let
	pp_pair (n,m) = ppBesides [ppPStr m,ppChar '.',ppPStr n]
    in
    pprTrace "builtins:\n" (case builtinNameMaps of { (builtin_ids, builtin_tcs) ->
    			    ppAboves [ ppCat (map pp_pair (keysFM builtin_ids))
    				     , ppCat (map pp_pair (keysFM builtin_tcs))
    				     , ppCat (map pp_pair (keysFM builtinKeysMap))
    				     ]}) $
    -}
    -- _scc_ "rnGlobalNames"
    makeHiMap opt_HiMap	    >>=	         \ hi_files ->
--  pprTrace "HiMap:\n" (ppAboves [ ppCat [ppPStr m, ppStr p] | (m,p) <- fmToList hi_files])
    initIfaceCache modname hi_files  >>= \ iface_cache ->

    fixIO ( \ ~(_, _, _, _, rec_occ_fm, ~(rec_export_fn,_)) ->
    let
	rec_occ_fn :: Name -> [RdrName]
	rec_occ_fn n = case lookupUFM rec_occ_fm n of
		         Nothing        -> []
		         Just (rn,occs) -> occs

	global_name_info = (builtinNameMaps, builtinKeysMap, rec_export_fn, rec_occ_fn)
    in
    getGlobalNames iface_cache global_name_info us1 input >>=
	\ (occ_env, imp_mods, unqual_imps, imp_fixes, top_errs, top_warns) ->

    if not (isEmptyBag top_errs) then
	return (rn_panic, rn_panic, top_errs, top_warns, emptyUFM, rn_panic)
    else

    -- No top-level name errors so rename source ...
    -- _scc_ "rnSource"
    case initRn True modname occ_env us2
		(rnSource imp_mods unqual_imps imp_fixes input) of {
	((rn_module, export_fn, module_dotdots, src_occs), src_errs, src_warns) ->

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

	multiple_occs (rn, (o1:o2:_)) = getLocalName o1 /= SLIT("negate")
					-- the user is rarely responsible if
					-- "negate" is mentioned in multiple ways
	multiple_occs _               = False
    in
    return (rn_module, imp_mods, 
	    top_errs  `unionBags` src_errs,
	    top_warns `unionBags` src_warns `unionBags` listToBag occ_warns,
	    occ_fm, (export_fn, module_dotdots))

    }) >>= \ (rn_module, imp_mods, errs_so_far, warns_so_far, occ_fm, export_stuff) ->

    if not (isEmptyBag errs_so_far) then
	return (rn_panic, rn_panic, rn_panic, rn_panic, rn_panic, errs_so_far, warns_so_far)
    else

    -- No errors renaming source so rename the interfaces ...
    -- _scc_ "preRnIfaces"
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
	  = extendGlobalRnEnv emptyRnEnv (map pairify_rn def_vals)
					 (map pairify_rn def_tcs)
	(orig_occ_env, orig_occ_dups)
	  = extendGlobalRnEnv emptyRnEnv (map pairify_rn occ_vals)
					 (map pairify_rn occ_tcs)

	-- This stuff is pretty dodgy right now: I think original
	-- names and occurrence names may be getting entangled
	-- when they shouldn't be... WDP 96/06

        pairify_rn rn -- ToDo: move to Name?
	  = let
		name = getName rn
	    in
	    (if isLocalName name
	     then Unqual (getLocalName name)
	     else case (origName "pairify_rn" name) of { OrigName m n ->
		  Qual m n }
	     , rn)
    in
--  ASSERT (isEmptyBag orig_occ_dups)
--    (if (isEmptyBag orig_occ_dups) then \x->x
--     else pprTrace "orig_occ_dups:" (ppAboves [ ppCat [ppr PprDebug m, ppr PprDebug n, ppr PprDebug o] | (m,n,o) <- bagToList orig_occ_dups])) $
    ASSERT (isEmptyBag orig_def_dups)

    -- _scc_ "rnIfaces"
    rnIfaces iface_cache imp_mods us3 orig_def_env orig_occ_env
	     rn_module (initMustHaves ++ imports_used) >>=
	\ (rn_module_with_imports, final_env,
	   (implicit_val_fm, implicit_tc_fm),
	   usage_stuff,
	   (iface_errs, iface_warns)) ->

    return (rn_module_with_imports,
	    final_env,
	    imp_mods,
	    export_stuff,
	    usage_stuff,
	    errs_so_far  `unionBags` iface_errs,
	    warns_so_far `unionBags` iface_warns)
  where
    rn_panic = panic "renameModule: aborted with errors"

    (us1, us') = splitUniqSupply us
    (us2, us3) = splitUniqSupply us'

initMustHaves :: [RnName]
    -- things we *must* find declarations for, because the
    -- compiler may eventually make reference to them (e.g.,
    -- class Eq)
initMustHaves
  | opt_NoImplicitPrelude
  = [{-no Prelude.hi, no point looking-}]
  | otherwise
  = [ name_fn (mkWiredInName u orig ExportAll)
    | (orig@(OrigName mod str), (u, name_fn)) <- fmToList builtinKeysMap ]
\end{code}

\begin{code}
makeHiMap :: Maybe String -> IO (FiniteMap Module FilePath)

makeHiMap Nothing = error "Rename.makeHiMap:no .hi map given by the GHC driver (?)"
makeHiMap (Just f)
  = readFile f	>>= \ cts ->
    return (snag_mod emptyFM cts [])
  where
    -- we alternate between "snag"ging mod(ule names) and path(names),
    -- accumulating names (reversed) and the final resulting map
    -- as we move along.

    snag_mod map  []       []   = map
    snag_mod map  (' ':cs) rmod = snag_path map (_PK_ (reverse rmod)) cs []
    snag_mod map  (c:cs)   rmod = snag_mod  map cs (c:rmod)

    snag_path map mod []        rpath = addToFM map mod (reverse rpath)
    snag_path map mod ('\n':cs) rpath = snag_mod (addToFM map mod (reverse rpath)) cs []
    snag_path map mod (c:cs)    rpath = snag_path map mod cs (c:rpath)
\end{code}

Warning message used herein:
\begin{code}
multipleOccWarn (name, occs) sty
  = ppBesides [ppStr "warning:multiple names used to refer to `", ppr sty name, ppStr "': ",
	       ppInterleave ppComma (map (ppr sty) occs)]
\end{code}
