%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnNames]{Extracting imported and top-level names in scope}

\begin{code}
#include "HsVersions.h"

module RnNames (
	getGlobalNames,
	SYN_IE(GlobalNameInfo)
    ) where

import PreludeGlaST	( SYN_IE(MutableVar) )

IMP_Ubiq()

import HsSyn
import RdrHsSyn
import RnHsSyn

import RnMonad
import RnIfaces		( IfaceCache, cachedIface, cachedDecl, CachingResult(..) )
import RnUtils		( SYN_IE(RnEnv), emptyRnEnv, initRnEnv, extendGlobalRnEnv,
			  lubExportFlag, qualNameErr, dupNamesErr, pprRnEnv
			)
import ParseUtils	( ParsedIface(..), RdrIfaceDecl(..), ExportsMap(..), RdrIfaceInst )


import Bag		( emptyBag, unitBag, consBag, snocBag, unionBags,
			  unionManyBags, mapBag, foldBag, filterBag, listToBag, bagToList )
import CmdLineOpts	( opt_NoImplicitPrelude, opt_CompilingGhcInternals )
import ErrUtils		( SYN_IE(Error), SYN_IE(Warning), addErrLoc, addShortErrLocLine, addShortWarnLocLine )
import FiniteMap	( emptyFM, addToFM, addListToFM, lookupFM, fmToList, eltsFM, delListFromFM, keysFM{-ToDo:rm-}, FiniteMap )
import Id		( GenId )
import Maybes		( maybeToBool, catMaybes, MaybeErr(..) )
import Name		( RdrName(..), Name, isQual, mkTopLevName, mkWiredInName, origName,
			  nameOf, qualToOrigName, mkImportedName,
			  nameExportFlag, nameImportFlag,
			  getLocalName, getSrcLoc, getImpLocs,
			  moduleNamePair, pprNonSym,
			  isLexCon, isLexSpecialSym, ExportFlag(..), OrigName(..)
			)
import PrelInfo		( SYN_IE(BuiltinNames), SYN_IE(BuiltinKeys) )
import PrelMods		( pRELUDE, gHC_BUILTINS, modulesWithBuiltins )
import Pretty
import SrcLoc		( SrcLoc, mkBuiltinSrcLoc )
import TyCon		( tyConDataCons )
import UniqFM		( emptyUFM, addListToUFM_C, lookupUFM )
import UniqSupply	( splitUniqSupply )
import Util		( isIn, assoc, cmpPString, sortLt, removeDups,
			  equivClasses, panic, assertPanic, pprPanic{-ToDo:rm-}, pprTrace{-ToDo:rm-}
			)
import PprStyle --ToDo:rm 
\end{code}

\begin{code}
type GlobalNameInfo = (BuiltinNames,
		       BuiltinKeys,
		       Name -> ExportFlag,	-- export flag
		       Name -> [RdrName])	-- occurrence names
		       -- NB: both of the functions are in a *knot* and
		       -- must be tugged on oh-so-gently...

type RnM_Info s r = RnMonad GlobalNameInfo s r

getGlobalNames ::
	   IfaceCache		
	-> GlobalNameInfo	
	-> UniqSupply
	-> RdrNameHsModule
	-> IO (RnEnv,
	       [Module],		-- directly imported modules
	       Bag (Module,RnName),	-- unqualified imports from module
	       Bag RenamedFixityDecl,	-- imported fixity decls
	       Bag Error,
	       Bag Warning)

getGlobalNames iface_cache info us
	       (HsModule mod _ _ imports _ ty_decls _ cls_decls _ _ _ binds _ _)
  = let
	(us1, us2) = splitUniqSupply us
    in
    case initRn True mod emptyRnEnv us1 
		(setExtraRn info $
		 getSourceNames ty_decls cls_decls binds)
    of { ((src_vals, src_tcs), src_errs, src_warns) ->

    doImportDecls iface_cache info us2 imports	>>=
	\ (imp_vals, imp_tcs, imp_mods, unqual_imps, imp_fixes, imp_errs, imp_warns) ->

    let
        unqual_vals = map (\rn -> (Unqual (getLocalName rn), rn)) (bagToList src_vals)
        unqual_tcs  = map (\rn -> (Unqual (getLocalName rn), rn)) (bagToList src_tcs)

        (src_env, src_dups) = extendGlobalRnEnv initRnEnv unqual_vals unqual_tcs
	(all_env, imp_dups) = extendGlobalRnEnv src_env (bagToList imp_vals) (bagToList imp_tcs)

	-- remove dups of the same imported thing
	diff_imp_dups = filterBag diff_orig imp_dups
	diff_orig (_,rn1,rn2) = origName "diff_orig" rn1 /= origName "diff_orig" rn2

	all_dups = bagToList (src_dups `unionBags` diff_imp_dups)
	dup_errs = map dup_err (equivClasses cmp_rdr all_dups)
	cmp_rdr (rdr1,_,_) (rdr2,_,_) = cmp rdr1 rdr2
	dup_err ((rdr,rn1,rn2):rest) = globalDupNamesErr rdr (rn1:rn2: [rn|(_,_,rn)<-rest])

	all_errs  = src_errs  `unionBags` imp_errs `unionBags` listToBag dup_errs
	all_warns = src_warns `unionBags` imp_warns
    in
--    pprTrace "initRnEnv:" (pprRnEnv PprDebug initRnEnv) $
--    pprTrace "src_env:"   (pprRnEnv PprDebug src_env) $
--    pprTrace "all_env:"   (pprRnEnv PprDebug all_env) $
    return (all_env, imp_mods, unqual_imps, imp_fixes, all_errs, all_warns) }
\end{code}

*********************************************************
*							*
\subsection{Top-level source names}
*							*
*********************************************************

\begin{code}
getSourceNames ::			-- Collects global *binders* (not uses)
	   [RdrNameTyDecl]
	-> [RdrNameClassDecl]
	-> RdrNameHsBinds
	-> RnM_Info s (Bag RnName,	-- values
		       Bag RnName)	-- tycons/classes

getSourceNames ty_decls cls_decls binds
  = mapAndUnzip3Rn getTyDeclNames ty_decls	`thenRn` \ (tycon_s, constrs_s, fields_s) ->
    mapAndUnzipRn  getClassNames cls_decls	`thenRn` \ (cls_s, cls_ops_s) ->
    getTopBindsNames binds			`thenRn` \ bind_names ->
    returnRn (unionManyBags constrs_s `unionBags`
	      unionManyBags fields_s  `unionBags`
	      unionManyBags cls_ops_s `unionBags` bind_names,
	      listToBag tycon_s `unionBags` listToBag cls_s)

--------------
getTyDeclNames :: RdrNameTyDecl
	       -> RnM_Info s (RnName, Bag RnName, Bag RnName)	-- tycon, constrs and fields

getTyDeclNames (TyData _ tycon _ condecls _ _ src_loc)
  = --getExtraRn		`thenRn` \ ((b_val_names,b_tc_names),b_keys,rec_exp_fn,rec_occ_fn) ->
    --pprTrace "getTyDeclNames:" (ppr PprDebug tycon) $
    --pprTrace "getTDN1:" (ppAboves [ ppCat [ppPStr m, ppPStr n] | ((OrigName m n), _) <- fmToList b_tc_names]) $

    newGlobalName src_loc Nothing False{-not val-} tycon `thenRn` \ tycon_name ->
    getConFieldNames (Just (nameExportFlag tycon_name)) emptyBag emptyBag emptyFM
		     condecls 		`thenRn` \ (con_names, field_names) ->
    let
	rn_tycon   = RnData tycon_name con_names field_names
        rn_constrs = [ RnConstr name tycon_name | name <- con_names]
        rn_fields  = [ RnField name tycon_name | name <- field_names]
    in
    returnRn (rn_tycon, listToBag rn_constrs, listToBag rn_fields)

getTyDeclNames (TyNew _ tycon _ [NewConDecl con _ con_loc] _ _ src_loc)
  = newGlobalName src_loc Nothing False{-not val-} tycon	`thenRn` \ tycon_name ->
    newGlobalName con_loc (Just (nameExportFlag tycon_name)) True{-val-} con
					`thenRn` \ con_name ->
    returnRn (RnData tycon_name [con_name] [],
	      unitBag (RnConstr con_name tycon_name),
	      emptyBag)

getTyDeclNames (TySynonym tycon _ _ src_loc)
  = newGlobalName src_loc Nothing False{-not val-} tycon	`thenRn` \ tycon_name ->
    returnRn (RnSyn tycon_name, emptyBag, emptyBag)

----------------
getConFieldNames :: Maybe ExportFlag
		 -> Bag Name -> Bag Name
		 -> FiniteMap RdrName ()
		 -> [RdrNameConDecl]
		 -> RnM_Info s ([Name], [Name])

getConFieldNames exp constrs fields have []
  = returnRn (bagToList constrs, bagToList fields)

getConFieldNames exp constrs fields have (ConDecl con _ src_loc : rest)
  = newGlobalName src_loc exp True{-val-} con	`thenRn` \ con_name ->
    getConFieldNames exp (constrs `snocBag` con_name) fields have rest

getConFieldNames exp constrs fields have (ConOpDecl _ con _ src_loc : rest)
  = newGlobalName src_loc exp True{-val-} con	`thenRn` \ con_name ->
    getConFieldNames exp (constrs `snocBag` con_name) fields have rest

getConFieldNames exp constrs fields have (RecConDecl con fielddecls src_loc : rest)
  = mapRn (addErrRn . dupFieldErr con src_loc) dups	`thenRn_`
    newGlobalName src_loc exp True{-val-} con		`thenRn` \ con_name ->
    mapRn (newGlobalName src_loc exp True{-val-}) new_fields 	`thenRn` \ field_names ->
    let
	all_constrs = constrs `snocBag` con_name
	all_fields  = fields  `unionBags` listToBag field_names
    in
    getConFieldNames exp all_constrs all_fields new_have rest
  where
    (uniq_fields, dups) = removeDups cmp (concat (map fst fielddecls))
    new_fields = filter (not . maybeToBool . lookupFM have) uniq_fields
    new_have   = addListToFM have (zip new_fields (repeat ()))

-------------
getClassNames :: RdrNameClassDecl
	      -> RnM_Info s (RnName, Bag RnName)	-- class and class ops

getClassNames (ClassDecl _ cname _ sigs _ _ src_loc)
  = newGlobalName src_loc Nothing False{-notval-} cname	`thenRn` \ class_name ->
    getClassOpNames (Just (nameExportFlag class_name))
				  sigs	`thenRn` \ op_names ->
    returnRn (RnClass class_name op_names,
	      listToBag (map (\ n -> RnClassOp n class_name) op_names))

---------------
getClassOpNames :: Maybe ExportFlag
		-> [RdrNameSig]
		-> RnM_Info s [Name]

getClassOpNames exp [] = returnRn []

getClassOpNames exp (ClassOpSig op _ _ src_loc : sigs)
  = newGlobalName src_loc exp True{-val-} op `thenRn` \ op_name ->
    getClassOpNames exp sigs	 `thenRn` \ op_names ->
    returnRn (op_name : op_names)
getClassOpNames exp (_ : sigs)
  = getClassOpNames exp sigs
\end{code}

*********************************************************
*							*
\subsection{Bindings}
*							*
*********************************************************

\begin{code}
getTopBindsNames :: RdrNameHsBinds
		 -> RnM_Info s (Bag RnName)

getTopBindsNames binds = doBinds binds

doBinds EmptyBinds           = returnRn emptyBag
doBinds (SingleBind bind)    = doBind bind
doBinds (BindWith bind sigs) = doBind bind
doBinds (ThenBinds binds1 binds2)
  = andRn unionBags (doBinds binds1) (doBinds binds2)

doBind EmptyBind          = returnRn emptyBag
doBind (NonRecBind mbind) = doMBinds mbind
doBind (RecBind mbind)    = doMBinds mbind

doMBinds EmptyMonoBinds 			= returnRn emptyBag
doMBinds (PatMonoBind pat grhss_and_binds locn) = doPat locn pat
doMBinds (FunMonoBind p_name _ _ locn) 	        = doName locn p_name
doMBinds (AndMonoBinds mbinds1 mbinds2)
  = andRn unionBags (doMBinds mbinds1) (doMBinds mbinds2)

doPats locn pats
  = mapRn (doPat locn) pats	`thenRn` \ pats_s ->
    returnRn (unionManyBags pats_s)

doPat locn WildPatIn             = returnRn emptyBag
doPat locn (LitPatIn _) 	 = returnRn emptyBag
doPat locn (LazyPatIn pat)       = doPat locn pat
doPat locn (VarPatIn var) 	 = doName locn var
doPat locn (NegPatIn pat)	 = doPat locn pat
doPat locn (ParPatIn pat)	 = doPat locn pat
doPat locn (ListPatIn pats)      = doPats locn pats
doPat locn (TuplePatIn pats)     = doPats locn pats
doPat locn (ConPatIn name pats)  = doPats locn pats
doPat locn (ConOpPatIn p1 op p2)
  = andRn unionBags (doPat locn p1) (doPat locn p2)
doPat locn (AsPatIn as_name pat)
  = andRn unionBags (doName locn as_name) (doPat locn pat)
doPat locn (RecPatIn name fields)
  = mapRn (doField locn) fields `thenRn` \ fields_s ->
    returnRn (unionManyBags fields_s)

doField locn (_, pat, _) = doPat locn pat

doName locn rdr
  = newGlobalName locn Nothing True{-val-} rdr `thenRn` \ name ->
    returnRn (unitBag (RnName name))
\end{code}

*********************************************************
*							*
\subsection{Creating a new global name}
*							*
*********************************************************

\begin{code}
newGlobalName :: SrcLoc
	      -> Maybe ExportFlag
	      -> Bool{-True<=>value name,False<=>tycon/class-}
	      -> RdrName
	      -> RnM_Info s Name

newGlobalName locn maybe_exp is_val_name (Unqual name)
  = getExtraRn		`thenRn` \ ((b_val_names,b_tc_names),b_keys,rec_exp_fn,rec_occ_fn) ->
    getModuleRn  	`thenRn` \ mod ->
    rnGetUnique 	`thenRn` \ u ->
    let
	orig = OrigName mod name

	(uniq, is_toplev)
	  = case (lookupFM b_keys orig) of
	      Just (key,_) -> (key, True)
	      Nothing	   -> if not opt_CompilingGhcInternals then (u,True) else -- really here just to save gratuitous lookup
			      case (lookupFM (if is_val_name then b_val_names else b_tc_names) orig) of
			        Nothing -> (u, True)
				Just xx -> (uniqueOf xx, False{-builtin!-})

	exp = case maybe_exp of
	       Just flag -> flag
	       Nothing   -> rec_exp_fn n

	n = if is_toplev
	    then mkTopLevName  uniq orig locn exp (rec_occ_fn n) -- NB: two "n"s
	    else mkWiredInName uniq orig exp
    in
    returnRn n    

newGlobalName locn maybe_exp is_val_name rdr@(Qual mod name)
  | opt_CompilingGhcInternals
  -- we are actually defining something that compiler knows about (e.g., Bool)

  = getExtraRn		`thenRn` \ ((b_val_names,b_tc_names),b_keys,rec_exp_fn,rec_occ_fn) ->
    let
	orig = OrigName mod name

	(uniq, is_toplev)
	  = case (lookupFM b_keys orig) of
	      Just (key,_) -> (key, True)
	      Nothing	   -> case (lookupFM (if is_val_name then b_val_names else b_tc_names) orig) of
			        Nothing -> (pprPanic "newGlobalName:Qual:uniq:" (ppr PprDebug rdr), True)
				Just xx -> (uniqueOf xx, False{-builtin!-})

	exp = case maybe_exp of
	       Just flag -> flag
	       Nothing   -> rec_exp_fn n

	n = if is_toplev
	    then mkTopLevName  uniq orig locn exp (rec_occ_fn n) -- NB: two "n"s
	    else mkWiredInName uniq orig exp
    in
    returnRn n    

  | otherwise
  = addErrRn (qualNameErr "name in definition" (rdr, locn)) `thenRn_`
    returnRn (pprPanic "newGlobalName:Qual:" (ppr PprDebug rdr))
\end{code}

*********************************************************
*							*
\subsection{Imported names}
*							*
*********************************************************

\begin{code}
type ImportNameInfo
  = (GlobalNameInfo,
     FiniteMap OrigName RnName,		-- values imported so far
     FiniteMap OrigName RnName,		-- tycons/classes imported so far
     Name -> (ExportFlag, [SrcLoc]))	-- import flag and src locns;
					-- NB: this last field is in a knot
					-- and mustn't be tugged on!

type RnM_IInfo s r = RnMonad ImportNameInfo s r

------------------------------------------------------------------
doImportDecls ::
	   IfaceCache
	-> GlobalNameInfo		-- builtin and knot name info
	-> UniqSupply
	-> [RdrNameImportDecl]		-- import declarations
	-> IO (Bag (RdrName,RnName),	-- imported values in scope
	       Bag (RdrName,RnName),	-- imported tycons/classes in scope
	       [Module],		-- directly imported modules
	       Bag (Module,RnName),	-- unqualified import from module
	       Bag RenamedFixityDecl,	-- fixity info for imported names
	       Bag Error,
	       Bag Warning)

doImportDecls iface_cache g_info us src_imps
  = fixIO ( \ ~(_, _, _, _, _, _, rec_imp_stuff) ->
	let
	    rec_imp_fm = addListToUFM_C add_stuff emptyUFM (bagToList rec_imp_stuff)
	    add_stuff (imp1,locns1) (imp2,locns2) = (lubExportFlag imp1 imp2, locns1 `unionBags` locns2)

    	    rec_imp_fn :: Name -> (ExportFlag, [SrcLoc])
	    rec_imp_fn n = case lookupUFM rec_imp_fm n of
		             Nothing            -> panic "RnNames:rec_imp_fn"
		             Just (flag, locns) -> (flag, bagToList locns)

	    i_info = (g_info, emptyFM, emptyFM, rec_imp_fn)
	in
	-- cache the imported modules
	-- this ensures that all directly imported modules
	-- will have their original name iface in scope
	-- pprTrace "doImportDecls:" (ppCat (map ppPStr imp_mods)) $
	accumulate (map (cachedIface iface_cache False SLIT("doImportDecls")) imp_mods) >>

	-- process the imports
	doImports iface_cache i_info us all_imps

    ) >>= \ (vals, tcs, unquals, fixes, errs, warns, _) ->

    return (vals, tcs, imp_mods, unquals, fixes,
	    imp_errs `unionBags` errs,
	    imp_warns `unionBags` warns)
  where
    all_imps = implicit_prel  ++ src_imps
--  all_imps = implicit_qprel ++ the_imps

    explicit_prelude_imp
      = not (null [ () | (ImportDecl mod qual _ _ _) <- src_imps, mod == pRELUDE ])

    implicit_prel | opt_NoImplicitPrelude = []
		  | explicit_prelude_imp  = [ImportDecl pRELUDE True  Nothing Nothing prel_loc]
		  | otherwise		  = [ImportDecl pRELUDE False Nothing Nothing prel_loc]

    prel_loc = mkBuiltinSrcLoc

    (uniq_imps, imp_dups) = removeDups cmp_mod all_imps
    cmp_mod (ImportDecl m1 _ _ _ _) (ImportDecl m2 _ _ _ _) = cmpPString m1 m2

    qprel_imps = [ imp | imp@(ImportDecl mod True Nothing _ _) <- src_imps,
		 	 mod == pRELUDE ]

    qual_mods = [ (qual_name mod as_mod, imp) | imp@(ImportDecl mod True as_mod _ _) <- src_imps ]
    qual_name mod (Just as_mod) = as_mod
    qual_name mod Nothing       = mod

    (_, qual_dups) = removeDups cmp_qual qual_mods
    bad_qual_dups = filter (not . all_same_mod) qual_dups

    cmp_qual (q1,_) (q2,_) = cmpPString q1 q2
    all_same_mod ((q,ImportDecl mod _ _ _ _):rest)
      = all has_same_mod rest
      where
	has_same_mod (_,ImportDecl mod2 _ _ _ _) = mod == mod2

    imp_mods  = [ mod | ImportDecl mod _ _ _ _ <- uniq_imps ]

    imp_warns = listToBag (map dupImportWarn imp_dups)
    		`unionBags`
		listToBag (map qualPreludeImportWarn qprel_imps)

    imp_errs  = listToBag (map dupQualImportErr bad_qual_dups)

-----------------------
doImports :: IfaceCache
	  -> ImportNameInfo
	  -> UniqSupply
	  -> [RdrNameImportDecl]	-- import declarations
	  -> IO (Bag (RdrName,RnName),	-- imported values in scope
		 Bag (RdrName,RnName),	-- imported tycons/classes in scope
		 Bag (Module, RnName),	-- unqualified import from module
		 Bag RenamedFixityDecl,	-- fixity info for imported names
		 Bag Error,
		 Bag Warning,
		Bag (RnName,(ExportFlag,Bag SrcLoc))) -- import flags and src locs

doImports iface_cache i_info us []
  = return (emptyBag, emptyBag, emptyBag, emptyBag, emptyBag, emptyBag, emptyBag)

doImports iface_cache i_info@(g_info,done_vals,done_tcs,rec_imp_fn) us (imp:imps)
  = let
	(us1, us2) = splitUniqSupply us
    in
    doImport iface_cache i_info us1 imp
	>>= \ (vals1, tcs1, unquals1, fixes1, errs1, warns1, imps1) ->
    let
	ext_vals = foldl add_new_one done_vals (bagToList vals1)
	ext_tcs  = foldl add_new_one done_tcs  (bagToList tcs1) 
    in
    doImports iface_cache (g_info,ext_vals,ext_tcs,rec_imp_fn) us2 imps
	>>= \ (vals2, tcs2, unquals2, fixes2, errs2, warns2, imps2) ->
    return (vals1    `unionBags` vals2,
	    tcs1     `unionBags` tcs2,
	    unquals1 `unionBags` unquals2,
	    fixes1   `unionBags` fixes2,
	    errs1    `unionBags` errs2,
	    warns1   `unionBags` warns2,
	    imps1    `unionBags` imps2)
  where
    add_new_one :: FiniteMap OrigName RnName -- ones done so far
		-> (dont_care, RnName)
		-> FiniteMap OrigName RnName -- extended

    add_new_one fm (_, rn)
      = let
	    orig = origName "add_new_one" rn
	in
	case (lookupFM fm orig) of
	  Just  _ -> fm -- already there: no change
	  Nothing -> addToFM fm orig rn

----------------------
doImport :: IfaceCache
	 -> ImportNameInfo
	 -> UniqSupply
	 -> RdrNameImportDecl
	 -> IO (Bag (RdrName,RnName),			-- values
		Bag (RdrName,RnName),			-- tycons/classes
		Bag (Module,RnName),			-- unqual imports
		Bag RenamedFixityDecl,
                Bag Error,
		Bag Warning,
		Bag (RnName,(ExportFlag,Bag SrcLoc)))	-- import flags and src locs

doImport iface_cache info us (ImportDecl mod qual maybe_as maybe_spec src_loc)
  = --let
    --	(b_vals, b_tcs, maybe_spec')
    --	   = (emptyBag, emptyBag, maybe_spec)
    --in
    --pprTrace "doImport:" (ppPStr mod) $
    cachedIface iface_cache False SLIT("doImport") mod >>= \ maybe_iface ->
    return (maybe_iface, \ iface -> getOrigIEs iface maybe_spec)
	    >>= \ (maybe_iface, do_ies) ->

    case maybe_iface of
      Failed err ->
	return (emptyBag, emptyBag, emptyBag, emptyBag,
		unitBag err, emptyBag, emptyBag)
      Succeeded iface -> 
        let
	    (ies, chk_ies, get_errs) = do_ies iface
	in
	doOrigIEs iface_cache info mod src_loc us ies 
		>>= \ (ie_vals, ie_tcs, imp_flags, errs, warns) ->
	accumulate (map (checkOrigIE iface_cache) chk_ies)
		>>= \ chk_errs_warns ->
	let
	    fold_ies   = foldBag unionBags pair_occ emptyBag

	    final_vals = {-OLD:mapBag fst_occ b_vals `unionBags`-} fold_ies ie_vals
	    final_tcs  = {-OLD:mapBag fst_occ b_tcs  `unionBags`-} fold_ies ie_tcs
	    final_vals_list = bagToList final_vals
	in
	accumulate (map (getFixityDecl iface_cache . snd) final_vals_list)
			>>= \ fix_maybes_errs ->
	let
	    (chk_errs, chk_warns)  = unzip chk_errs_warns
	    (fix_maybes, fix_errs) = unzip fix_maybes_errs

	    unquals    = if qual{-ified import-}
			 then emptyBag
		         else mapBag pair_as (ie_vals `unionBags` ie_tcs)

	    final_fixes = listToBag (catMaybes fix_maybes)

	    final_errs  = mapBag (\ err -> err mod src_loc) (unionManyBags (get_errs:chk_errs))
		          `unionBags` errs `unionBags` unionManyBags fix_errs
	    final_warns = mapBag (\ warn -> warn mod src_loc) (unionManyBags chk_warns)
		          `unionBags` warns
	    imp_stuff   = mapBag (\ (n,imp) -> (n,(imp,unitBag src_loc))) imp_flags
        in
	return (final_vals, final_tcs, unquals, final_fixes,
		final_errs, final_warns, imp_stuff)
  where
    as_mod :: Module
    as_mod = case maybe_as of {Nothing -> mod; Just as_this -> as_this}

    mk_occ :: FAST_STRING -> RdrName
    mk_occ str = if qual then Qual as_mod str else Unqual str

    fst_occ :: (FAST_STRING, RnName) -> (RdrName, RnName)
    fst_occ (str, rn) = (mk_occ str, rn)

    pair_occ :: RnName -> Bag (RdrName, RnName)
    pair_occ rn
      = let
	    str      = getLocalName rn
	    qual_bag = unitBag (Qual as_mod str, rn)
	in
	if qual
	then qual_bag
	else qual_bag -- the qualified name is *also* visible
	    `snocBag` (Unqual str, rn)
	    

    pair_as :: RnName -> (Module, RnName)
    pair_as  rn = (as_mod, rn)

-----------------------------
{-
getBuiltins :: ImportNameInfo
	    -> Module
	    -> Maybe (Bool, [RdrNameIE])
	    -> (Bag (FAST_STRING, RnName),
		Bag (FAST_STRING, RnName),
		Maybe (Bool, [RdrNameIE])  -- return IEs that had no effect
	       )

getBuiltins _ modname maybe_spec
-- | modname `notElem` modulesWithBuiltins
  = (emptyBag, emptyBag, maybe_spec)

getBuiltins (((b_val_names,b_tc_names),_,_,_),_,_,_) modname maybe_spec
  = case maybe_spec of 
      Nothing           -> (all_vals, all_tcs, Nothing)

      Just (True, ies)  -> -- hiding does not work for builtin names
			   trace "NOTE: `import Prelude hiding ...' does not hide built-in names" $
			   (all_vals, all_tcs, maybe_spec)

      Just (False, ies) -> let 
			      (vals,tcs,ies_left) = do_builtin ies
			   in
		  	   (vals, tcs, Just (False, ies_left))
  where
    all_vals = do_all_builtin (fmToList b_val_names)
    all_tcs  = do_all_builtin (fmToList b_tc_names)

    do_all_builtin [] = emptyBag
    do_all_builtin (((OrigName mod str),rn):rest)
      = --pprTrace "do_all_builtin:" (ppCat [ppPStr modname, ppPStr mod, ppPStr str]) $
	(if mod == modname then consBag (str, rn) else id) (do_all_builtin rest)

    do_builtin [] = (emptyBag,emptyBag,[]) 
    do_builtin (ie:ies)
      = let
	    (str, orig)
	      = case (ie_name ie) of
		  Unqual s -> (s, OrigName modname s)
		  Qual m s -> pprTrace "do_builtin:surprising qual!" (ppCat [ppPStr m, ppPStr s]) $
			      (s, OrigName modname s)
	in
	case (lookupFM b_tc_names orig) of 	-- NB: we favour the tycon/class FM...
	  Just rn -> case (ie,rn) of
	     (IEThingAbs _, WiredInTyCon tc)
		-> (vals, (str, rn) `consBag` tcs, ies_left)
	     (IEThingAll _, WiredInTyCon tc)
		-> (listToBag (map (\ id -> (getLocalName id, WiredInId id)) 
				   (tyConDataCons tc))
		    `unionBags` vals,
		    (str,rn) `consBag` tcs, ies_left)
	     (IEThingWith _ _, WiredInTyCon tc)	-- No checking of With...
		-> (listToBag (map (\ id -> (nameOf (origName "IEThingWith" id), WiredInId id)) 
				   (tyConDataCons tc))
		    `unionBags` vals,
		    (str,rn) `consBag` tcs, ies_left)
	     _ -> panic "importing builtin names (1)"

	  Nothing ->
	    case (lookupFM b_val_names orig) of
	      Nothing -> (vals, tcs, ie:ies_left)
	      Just rn -> case (ie,rn) of
		 (IEVar _, WiredInId _)        
		    -> ((str, rn) `consBag` vals, tcs, ies_left)
		 _ -> panic "importing builtin names (2)"
      where
        (vals, tcs, ies_left) = do_builtin ies
-}

-------------------------
getOrigIEs :: ParsedIface
	   -> Maybe (Bool, [RdrNameIE]) -- "hiding" or not, blah, blah, blah
	   -> ([IE OrigName],
	       [(IE OrigName, ExportFlag)],
	       Bag (Module -> SrcLoc -> Error))

getOrigIEs (ParsedIface _ _ _ _ _ _ exps _ _ _ _ _ _) Nothing			-- import all
  = (map mkAllIE (eltsFM exps), [], emptyBag)

getOrigIEs (ParsedIface _ _ _ _ _ _ exps _ _ _ _ _ _) (Just (True, ies))	-- import hiding
  = (map mkAllIE (eltsFM exps_left), found_ies, errs)
  where
    (found_ies, errs) = lookupIEs exps ies
    exps_left = delListFromFM exps (map (getLocalName.ie_name.fst) found_ies)

getOrigIEs (ParsedIface _ _ _ _ _ _ exps _ _ _ _ _ _) (Just (False, ies))	-- import these
  = (map fst found_ies, found_ies, errs)
  where
    (found_ies, errs) = lookupIEs exps ies

------------------------------------------------
mkAllIE :: (OrigName, ExportFlag) -> IE OrigName

mkAllIE (orig,ExportAbs)
  = --ASSERT(isLexCon (nameOf orig))
    -- the ASSERT is correct, but it is too easy to
    -- trigger when writing .hi files by hand (e.g.
    -- when hackily breaking a module loop)
    IEThingAbs orig
mkAllIE (orig, ExportAll)
  | isLexCon name_orig || isLexSpecialSym name_orig
  = IEThingAll orig
  | otherwise
  = IEVar orig
  where
    name_orig = nameOf orig

------------
lookupIEs :: ExportsMap
	  -> [RdrNameIE]
	  -> ([(IE OrigName, ExportFlag)], -- IEs we found, orig-ified
	      Bag (Module -> SrcLoc -> Error))

lookupIEs exps ies
  = foldr go ([], emptyBag) ies
  where
    go ie (already, errs)
      = let
	    str = case (ie_name ie) of
		    Unqual s -> s
		    Qual m s -> s
	in
	case (lookupFM exps str) of
	  Nothing ->
	    (already, unknownImpSpecErr ie `consBag` errs)
	  Just (orig, flag) ->
	    ((orig_ie orig ie, flag) : already,
	     adderr_if (seen_ie orig already) (duplicateImpSpecErr ie) errs)

    orig_ie orig (IEVar n)          = IEVar       orig
    orig_ie orig (IEThingAbs n)     = IEThingAbs  orig
    orig_ie orig (IEThingAll n)     = IEThingAll  orig
    orig_ie orig (IEThingWith n ns) = IEThingWith orig (map re_orig ns)
      where
	(OrigName mod _) = orig
	re_orig (Unqual s) = OrigName mod s

    seen_ie orig seen_ies = any (\ (ie,_) -> orig == ie_name ie) seen_ies

--------------------------------------------
doOrigIEs iface_cache info mod src_loc us []
  = return (emptyBag,emptyBag,emptyBag,emptyBag,emptyBag)

doOrigIEs iface_cache info mod src_loc us (ie:ies)
  = let
	(us1, us2) = splitUniqSupply us
    in
    doOrigIE iface_cache info mod src_loc us1 ie 
	>>= \ (vals1, tcs1, imps1, errs1, warns1) ->
    doOrigIEs iface_cache info mod src_loc us2 ies
	>>= \ (vals2, tcs2, imps2, errs2, warns2) ->
    return (vals1    `unionBags` vals2,
	    tcs1     `unionBags` tcs2,
	    imps1    `unionBags` imps2,
	    errs1    `unionBags` errs2,
	    warns1   `unionBags` warns2)

----------------------
doOrigIE :: IfaceCache
	 -> ImportNameInfo
	 -> Module
	 -> SrcLoc
	 -> UniqSupply
	 -> IE OrigName
	 -> IO (Bag RnName,			-- values
		Bag RnName,			-- tycons/classes
		Bag (RnName,ExportFlag),	-- import flags
		Bag Error,
		Bag Warning)

doOrigIE iface_cache info mod src_loc us ie
  = with_decl iface_cache (ie_name ie)
	avoided_fn
	(\ err  -> (emptyBag, emptyBag, emptyBag, unitBag err, emptyBag))
	(\ decl -> case initRn True mod emptyRnEnv us
		    	       (setExtraRn info $
		     	    	pushSrcLocRn src_loc $
		     		getIfaceDeclNames ie decl)
		   of
		   ((vals, tcs, imps), errs, warns) -> (vals, tcs, imps, errs, warns))
  where
    avoided_fn Nothing -- the thing should be in the source
      = (emptyBag, emptyBag, emptyBag, emptyBag, emptyBag)
    avoided_fn (Just (Left  rn@(WiredInId _))) -- a builtin value brought into scope
      = (unitBag rn, emptyBag, emptyBag, emptyBag, emptyBag)
    avoided_fn (Just (Right rn@(WiredInTyCon tc)))
	-- a builtin tc brought into scope; we also must bring its
	-- data constructors into scope
      = --pprTrace "avoided:Right:" (ppr PprDebug rn) $
	(listToBag [WiredInId dc | dc <- tyConDataCons tc], unitBag rn, emptyBag, emptyBag, emptyBag)

-------------------------
checkOrigIE :: IfaceCache
	    -> (IE OrigName, ExportFlag)
	    -> IO (Bag (Module -> SrcLoc -> Error), Bag (Module -> SrcLoc -> Warning))

checkOrigIE iface_cache (IEThingAll n, ExportAbs)
  = with_decl iface_cache n
	(\ _    -> (emptyBag, emptyBag))
	(\ err  -> (unitBag (\ mod locn -> err), emptyBag))
	(\ decl -> case decl of
		TypeSig _ _ _ -> (emptyBag, unitBag (allWhenSynImpSpecWarn n))
		other	      -> (unitBag (allWhenAbsImpSpecErr n), emptyBag))

checkOrigIE iface_cache (IEThingWith n ns, ExportAbs)
  = return (unitBag (withWhenAbsImpSpecErr n), emptyBag)

checkOrigIE iface_cache (IEThingWith n ns, ExportAll)
  = with_decl iface_cache n
	(\ _    -> (emptyBag, emptyBag))
	(\ err  -> (unitBag (\ mod locn -> err), emptyBag))
	(\ decl -> case decl of
	     	NewTypeSig _ con _ _         -> (check_with "constructors" [con] ns, emptyBag)
	     	DataSig    _ cons fields _ _ -> (check_with "constructors (and fields)" (cons++fields) ns, emptyBag)
             	ClassSig   _ ops _ _         -> (check_with "class ops"   ops   ns, emptyBag))
  where
    check_with str has origs
      | sortLt (<) (map getLocalName has) == sortLt (<) (map nameOf origs)
      = emptyBag
      | otherwise
      = unitBag (withImpSpecErr str n has origs)

checkOrigIE iface_cache other
  = return (emptyBag, emptyBag)

-----------------------
with_decl :: IfaceCache
	  -> OrigName
	  -> (Maybe (Either RnName RnName) -> something) -- if avoided..
	  -> (Error        -> something)		 -- if an error...
	  -> (RdrIfaceDecl -> something)		 -- if OK...
	  -> IO something

with_decl iface_cache n do_avoid do_err do_decl
  = cachedDecl iface_cache (isLexCon n_name || isLexSpecialSym n_name) n   >>= \ maybe_decl ->
    case maybe_decl of
      CachingAvoided info -> return (do_avoid info)
      CachingFail    err  -> return (do_err   err)
      CachingHit     decl -> return (do_decl  decl)
  where
    n_name = nameOf n

-------------
getFixityDecl :: IfaceCache
	      -> RnName
	      -> IO (Maybe RenamedFixityDecl, Bag Error)

getFixityDecl iface_cache rn
  = let
	(OrigName mod str) = origName "getFixityDecl" rn

	succeeded infx i = return (Just (infx rn i), emptyBag)
    in
    cachedIface iface_cache True str mod >>= \ maybe_iface ->
    case maybe_iface of
      Failed err ->
	return (Nothing, unitBag err)
      Succeeded (ParsedIface _ _ _ _ _ _ _ _ fixes _ _ _ _) ->
	case lookupFM fixes str of
	  Nothing 	    -> return (Nothing, emptyBag)
	  Just (InfixL _ i) -> succeeded InfixL i
	  Just (InfixR _ i) -> succeeded InfixR i
	  Just (InfixN _ i) -> succeeded InfixN i

ie_name (IEVar n)         = n
ie_name (IEThingAbs n)    = n
ie_name (IEThingAll n)    = n
ie_name (IEThingWith n _) = n

adderr_if True  err errs = err `consBag` errs
adderr_if False err errs = errs
\end{code}

*********************************************************
*							*
\subsection{Actually creating the imported names}
*							*
*********************************************************

\begin{code}
getIfaceDeclNames :: IE OrigName -> RdrIfaceDecl
		  -> RnM_IInfo s (Bag RnName,			-- values
				  Bag RnName,			-- tycons/classes
			 	  Bag (RnName,ExportFlag))	-- import flags

getIfaceDeclNames ie (ValSig val src_loc _)
  = newImportedName False src_loc Nothing Nothing val 	`thenRn` \ val_name ->
    returnRn (unitBag (RnName val_name),
	      emptyBag,
	      unitBag (RnName val_name, ExportAll))

getIfaceDeclNames ie (TypeSig tycon src_loc _)
  = newImportedName True src_loc Nothing Nothing tycon  `thenRn` \ tycon_name ->
    returnRn (emptyBag,
	      unitBag (RnSyn tycon_name),
	      unitBag (RnSyn tycon_name, ExportAll))

getIfaceDeclNames ie (NewTypeSig tycon con src_loc _)
  = newImportedName True src_loc Nothing Nothing tycon  `thenRn` \ tycon_name ->
    newImportedName False src_loc (Just (nameExportFlag tycon_name))
				  (Just (nameImportFlag tycon_name))
			       	  con 			`thenRn` \ con_name ->
    returnRn (if imp_all (imp_flag ie) then
		  unitBag (RnConstr con_name tycon_name)
	      else
		  emptyBag,
	      unitBag (RnData tycon_name [con_name] []),
	      unitBag (RnData tycon_name [con_name] [], imp_flag ie))

getIfaceDeclNames ie (DataSig tycon cons fields src_loc _)
  = newImportedName True src_loc Nothing Nothing tycon `thenRn` \ tycon_name ->
    let
	map_me = mapRn (newImportedName False src_loc
				(Just (nameExportFlag tycon_name))
				(Just (nameImportFlag tycon_name)))
    in
    map_me cons	    `thenRn` \ con_names ->
    map_me fields   `thenRn` \ field_names ->
    let
	rn_tycon   = RnData tycon_name con_names field_names
        rn_constrs = [ RnConstr name tycon_name | name <- con_names ]
	rn_fields  = [ RnField name tycon_name | name <- field_names ]
    in
    returnRn (if imp_all (imp_flag ie) then
		  listToBag rn_constrs `unionBags` listToBag rn_fields
	      else
		  emptyBag,
	      unitBag rn_tycon,
	      unitBag (rn_tycon, imp_flag ie))

getIfaceDeclNames ie (ClassSig cls ops src_loc _)
  = newImportedName True src_loc Nothing Nothing cls `thenRn` \ cls_name ->
    mapRn (newImportedName False src_loc (Just (nameExportFlag cls_name))
				         (Just (nameImportFlag cls_name)))
			         	    ops `thenRn` \ op_names ->
    returnRn (if imp_all (imp_flag ie) then
		  listToBag (map (\ n -> RnClassOp n cls_name) op_names)
	      else
		  emptyBag,
	      unitBag (RnClass cls_name op_names),
	      unitBag (RnClass cls_name op_names, imp_flag ie))


imp_all ExportAll = True
imp_all _         = False

imp_flag (IEThingAbs _)    = ExportAbs
imp_flag (IEThingAll _)    = ExportAll
imp_flag (IEThingWith _ _) = ExportAll
\end{code}

*********************************************************
*							*
\subsection{Creating a new imported name}
*							*
*********************************************************

\begin{code}
newImportedName :: Bool			-- True => tycon or class
		-> SrcLoc
		-> Maybe ExportFlag	-- maybe export flag
		-> Maybe ExportFlag	-- maybe import flag
	        -> RdrName		-- orig name
		-> RnM_IInfo s Name

newImportedName tycon_or_class locn maybe_exp maybe_imp rdr
  = let
	orig = qualToOrigName rdr
    in
    getExtraRn `thenRn` \ ((_,b_keys,rec_exp_fn,rec_occ_fn),done_vals,done_tcs,rec_imp_fn) ->
    case ((if tycon_or_class
	   then lookupFM done_tcs
	   else lookupFM done_vals) orig) of

      Just rn -> returnRn (getName rn)
      Nothing -> 
	rnGetUnique	`thenRn` \ u ->
	let 
	    uniq = case lookupFM b_keys orig of
		     Nothing	  -> u
		     Just (key,_) -> key

	    exp  = case maybe_exp of
	    	     Just xx -> xx
	       	     Nothing -> rec_exp_fn n

	    imp  = case maybe_imp of
	    	     Just xx -> xx
	       	     Nothing -> imp_flag

	    (imp_flag, imp_locs) = rec_imp_fn n

	    n = mkImportedName uniq orig imp locn imp_locs exp (rec_occ_fn n) -- NB: two "n"s
	in
	returnRn n
\end{code}

\begin{code}
globalDupNamesErr rdr rns sty
  = ppAboves (message : map pp_dup rns)
  where
    message   = ppBesides [ppStr "multiple declarations of `", pprNonSym sty rdr, ppStr "'"]

    pp_dup rn = addShortErrLocLine (get_loc rn) (\ sty ->
	        ppCat [pp_descrip rn, pprNonSym sty rn]) sty

    get_loc rn = case getImpLocs rn of
		     []   -> getSrcLoc rn
	 	     locs -> head locs

    pp_descrip (RnName _)      = ppStr "as a value:"
    pp_descrip (RnSyn  _)      = ppStr "as a type synonym:"
    pp_descrip (RnData _ _ _)  = ppStr "as a data type:"
    pp_descrip (RnConstr _ _)  = ppStr "as a data constructor:"
    pp_descrip (RnField _ _)   = ppStr "as a record field:"
    pp_descrip (RnClass _ _)   = ppStr "as a class:"
    pp_descrip (RnClassOp _ _) = ppStr "as a class method:"
    pp_descrip _               = ppNil 

dupImportWarn (ImportDecl m1 _ _ _ locn1 : dup_imps) sty
  = ppAboves (item1 : map dup_item dup_imps)
  where
    item1 = addShortWarnLocLine locn1 (\ sty ->
	    ppCat [ppStr "multiple imports from module", ppPStr m1]) sty

    dup_item (ImportDecl m _ _ _ locn)
          = addShortWarnLocLine locn (\ sty ->
            ppCat [ppStr "here was another import from module", ppPStr m]) sty

qualPreludeImportWarn (ImportDecl m _ _ _ locn)
  = addShortWarnLocLine locn (\ sty ->
    ppCat [ppStr "qualified import of prelude module", ppPStr m])

dupQualImportErr ((q1,ImportDecl _ _ _ _ locn1):dup_quals) sty
  = ppAboves (item1 : map dup_item dup_quals)
  where
    item1 = addShortErrLocLine locn1 (\ sty ->
	    ppCat [ppStr "multiple imports (from different modules) with same qualified name", ppPStr q1]) sty

    dup_item (q,ImportDecl _ _ _ _ locn)
          = addShortErrLocLine locn (\ sty ->
            ppCat [ppStr "here was another import with qualified name", ppPStr q]) sty

unknownImpSpecErr ie imp_mod locn
  = addShortErrLocLine locn (\ sty ->
    ppBesides [ppStr "module ", ppPStr imp_mod, ppStr " does not export `", ppr sty (ie_name ie), ppStr "'"])

duplicateImpSpecErr ie imp_mod locn
  = addShortErrLocLine locn (\ sty ->
    ppBesides [ppStr "`", ppr sty (ie_name ie), ppStr "' already seen in import list"])

allWhenSynImpSpecWarn n imp_mod locn
  = addShortWarnLocLine locn (\ sty ->
    ppBesides [ppStr "type synonym `", ppr sty n, ppStr "' should not be imported with (..)"])

allWhenAbsImpSpecErr n imp_mod locn
  = addShortErrLocLine locn (\ sty ->
    ppBesides [ppStr "module ", ppPStr imp_mod, ppStr " only exports `", ppr sty n, ppStr "' abstractly"])

withWhenAbsImpSpecErr n imp_mod locn
  = addShortErrLocLine locn (\ sty ->
    ppBesides [ppStr "module ", ppPStr imp_mod, ppStr " only exports `", ppr sty n, ppStr "' abstractly"])

withImpSpecErr str n has ns imp_mod locn
  = addErrLoc locn "" (\ sty ->
    ppAboves [ ppBesides [ppStr "inconsistent list of", ppStr str, ppStr "in import list for `", ppr sty n, ppStr "'"],
	       ppCat [ppStr "    expected:", ppInterleave ppComma (map (ppr sty) has)],
	       ppCat [ppStr "    found:   ", ppInterleave ppComma (map (ppr sty) ns)] ])

dupFieldErr con locn (dup:rest)
  = addShortErrLocLine locn (\ sty ->
    ppBesides [ppStr "record field `", ppr sty dup, ppStr "declared multiple times in `", ppr sty con, ppStr "'"])
\end{code}
