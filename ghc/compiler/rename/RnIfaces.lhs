%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnIfaces]{Cacheing and Renaming of Interfaces}

\begin{code}
#include "HsVersions.h"

module RnIfaces (
	cachedIface,
	cachedDecl, CachingResult(..),
	rnIfaces,
	IfaceCache, initIfaceCache
    ) where

IMP_Ubiq()

import PreludeGlaST	( thenPrimIO, newVar, readVar, writeVar, SYN_IE(MutableVar) )
#if __GLASGOW_HASKELL__ >= 200
# define ST_THEN `stThen`
# define TRY_IO  tryIO
IMPORT_1_3(GHCio(stThen,tryIO))
#else
# define ST_THEN `thenPrimIO`
# define TRY_IO	 try
#endif

import HsSyn
import HsPragmas	( noGenPragmas )
import RdrHsSyn
import RnHsSyn

import RnMonad
import RnSource		( rnTyDecl, rnClassDecl, rnInstDecl, rnPolyType )
import RnUtils		( SYN_IE(RnEnv), emptyRnEnv, lookupRnEnv, lookupTcRnEnv, extendGlobalRnEnv )
import ParseIface	( parseIface )
import ParseUtils	( ParsedIface(..), RdrIfaceDecl(..), RdrIfaceInst(..),
			  VersionsMap(..), UsagesMap(..)
			)

import Bag		( emptyBag, unitBag, consBag, snocBag,
			  unionBags, unionManyBags, isEmptyBag, bagToList )
import ErrUtils		( SYN_IE(Error), SYN_IE(Warning) )
import FiniteMap	( emptyFM, lookupFM, addToFM, addToFM_C, plusFM, eltsFM,
			  fmToList, delListFromFM, sizeFM, foldFM, unitFM,
			  plusFM_C, addListToFM{-, keysFM ToDo:rm-}, FiniteMap
			)
import Maybes		( maybeToBool, MaybeErr(..) )
import Name		( origName, moduleOf, nameOf, qualToOrigName, OrigName(..),
			  isLexCon, RdrName(..), Name{-instance NamedThing-} )
--import PprStyle		-- ToDo:rm
--import Outputable	-- ToDo:rm
import PrelInfo		( builtinNameMaps, builtinKeysMap, builtinTcNamesMap, SYN_IE(BuiltinNames) )
import Pretty
import UniqFM		( emptyUFM )
import UniqSupply	( splitUniqSupply )
import Util		( sortLt, removeDups, cmpPString, startsWith,
			  panic, pprPanic, assertPanic{-, pprTrace ToDo:rm-} )
\end{code}

\begin{code}
type ModuleToIfaceContents = FiniteMap Module ParsedIface
type ModuleToIfaceFilePath = FiniteMap Module FilePath

#if __GLASGOW_HASKELL__ >= 200
# define REAL_WORLD RealWorld
#else
# define REAL_WORLD _RealWorld
#endif

data IfaceCache
  = IfaceCache
	Module			 -- the name of the module being compiled
	BuiltinNames		 -- so we can avoid going after things
				 -- the compiler already knows about
        (MutableVar REAL_WORLD
	 (ModuleToIfaceContents, -- interfaces for individual interface files
	  ModuleToIfaceContents, -- merged interfaces based on module name
				 -- used for extracting info about original names
	  ModuleToIfaceFilePath))

initIfaceCache mod hi_files
  = newVar (emptyFM,emptyFM,hi_files) ST_THEN \ iface_var ->
    return (IfaceCache mod builtinNameMaps iface_var)
\end{code}

*********************************************************
*							*
\subsection{Reading interface files}
*							*
*********************************************************

Return cached info about a Module's interface; otherwise,
read the interface (using our @ModuleToIfaceFilePath@ map
to decide where to look).

Note: we have two notions of interface
 * the interface for a particular file name
 * the (combined) interface for a particular module name

The idea is that two source files may declare a module
with the same name with the declarations being merged.

This allows us to have file PreludeList.hs producing
PreludeList.hi but defining part of module Prelude.
When PreludeList is imported its contents will be
added to Prelude. In this way all the original names 
for a particular module will be available the imported
decls are renamed.

ToDo: Check duplicate definitons are the same.
ToDo: Check/Merge duplicate pragmas.


\begin{code}
cachedIface :: IfaceCache
	    -> Bool		-- True  => want merged interface for original name
				-- False => want file interface only
	    -> FAST_STRING	-- item that prompted search (debugging only!)
	    -> Module
	    -> IO (MaybeErr ParsedIface Error)

cachedIface (IfaceCache _ _ iface_var) want_orig_iface item modname
  = readVar iface_var ST_THEN \ (iface_fm, orig_fm, file_fm) ->

    case (lookupFM iface_fm modname) of
      Just iface -> return (want_iface iface orig_fm)
      Nothing    ->
      	case (lookupFM file_fm modname) of
	  Nothing   -> return (Failed (noIfaceErr modname))
	  Just file ->
	    readIface file modname item >>= \ read_iface ->
	    case read_iface of
	      Failed err      -> -- pprTrace "module-file map:\n" (ppAboves [ppCat [ppPStr m, ppStr f] | (m,f) <- fmToList file_fm]) $
				 return (Failed err)
	      Succeeded iface ->
		let
		    iface_fm' = addToFM iface_fm modname iface
		    orig_fm'  = addToFM_C mergeIfaces orig_fm (iface_mod iface) iface
		in
		writeVar iface_var (iface_fm', orig_fm', file_fm) ST_THEN \ _ ->
		return (want_iface iface orig_fm')
  where
    want_iface iface orig_fm 
      | want_orig_iface
      = case lookupFM orig_fm modname of
	  Nothing         -> Failed (noOrigIfaceErr modname)
          Just orig_iface -> Succeeded orig_iface
      | otherwise
      = Succeeded iface

    iface_mod (ParsedIface mod _ _ _ _ _ _ _ _ _ _ _ _) = mod

----------
mergeIfaces (ParsedIface mod1 (_, files1) _ _ _ _ _ _ fixes1 tdefs1 vdefs1 idefs1 prags1)
	    (ParsedIface mod2 (_, files2) _ _ _ _ _ _ fixes2 tdefs2 vdefs2 idefs2 prags2)
  = --pprTrace "mergeIfaces:" (ppCat [ppStr "import", ppCat (map ppPStr (bagToList files2)),
    --				    ppStr "merged with", ppPStr mod1]) $
    ASSERT(mod1 == mod2)
    ParsedIface mod1
	(True, unionBags files2 files1)
	(panic "mergeIface: module version numbers")
	(panic "mergeIface: source version numbers")	-- Version numbers etc must be extracted from
	(panic "mergeIface: usage version numbers")	-- the merged file interfaces named above
	(panic "mergeIface: decl version numbers")
	(panic "mergeIface: exports")
	(panic "mergeIface: instance modules")
 	(plusFM_C (dup_merge {-"fixity"      (ppr PprDebug . fixDeclName)-}) fixes1 fixes2)
	(plusFM_C (dup_merge {-"tycon/class" (ppr PprDebug . idecl_nm)-})    tdefs1 tdefs2)
	(plusFM_C (dup_merge {-"value"       (ppr PprDebug . idecl_nm)-})    vdefs1 vdefs2)
	(unionBags idefs1 idefs2)
	(plusFM_C (dup_merge {-"pragma"      ppStr-})			 prags1 prags2)
  where
    dup_merge {-str ppr_dup-} dup1 dup2
      = --pprTrace "mergeIfaces:"
	--   	 (ppCat [ppPStr mod1, ppPStr mod2, ppStr ": dup", ppStr str, ppStr "decl",
	--		 ppr_dup dup1, ppr_dup dup2]) $
        dup2

    idecl_nm (TypeSig    n _ _)     = n
    idecl_nm (NewTypeSig n _ _ _)   = n
    idecl_nm (DataSig    n _ _ _ _) = n
    idecl_nm (ClassSig   n _ _ _)   = n
    idecl_nm (ValSig     n _ _)	    = n

----------
data CachingResult
  = CachingFail	    Error	  -- tried to find a decl, something went wrong
  | CachingHit	    RdrIfaceDecl  -- got it
  | CachingAvoided  (Maybe (Either RnName RnName))
				  -- didn't look in the interface
				  -- file(s); Nothing => the thing
				  -- *should* be in the source module;
				  -- Just (Left ...) => builtin val name;
				  -- Just (Right ..) => builtin tc name

cachedDecl :: IfaceCache
	   -> Bool	-- True <=> tycon or class name
	   -> OrigName
	   -> IO CachingResult

cachedDecl iface_cache@(IfaceCache this_mod (b_val_names,b_tc_names) _)
	   class_or_tycon name@(OrigName mod str)

  = -- pprTrace "cachedDecl:" (ppr PprDebug name) $
    if mod == this_mod then 	        -- some i/face has made a reference
	return (CachingAvoided Nothing) -- to something from this module
    else
    let
	b_env	    = if class_or_tycon then b_tc_names else b_val_names
    in
    case (lookupFM b_env name) of
      Just rn -> -- in builtins!
	return (CachingAvoided (Just ((if class_or_tycon then Right else Left) rn)))

      Nothing ->
	cachedIface iface_cache True str mod >>= \ maybe_iface ->
	case maybe_iface of
	  Failed err -> --pprTrace "cachedDecl:fail:" (ppr PprDebug orig) $
			return (CachingFail err)
	  Succeeded (ParsedIface _ _ _ _ _ _ exps _ _ tdefs vdefs _ _) -> 
	    case (lookupFM (if class_or_tycon then tdefs else vdefs) str) of
	      Just decl -> return (CachingHit  decl)
	      Nothing   -> return (CachingFail (noDeclInIfaceErr mod str))

----------
cachedDeclByType :: IfaceCache
		 -> RnName{-NB: diff type than cachedDecl -}
		 -> IO CachingResult

cachedDeclByType iface_cache rn
    -- the idea is: check that, e.g., if we're given an
    -- RnClass, then we really get back a ClassDecl from
    -- the cache (not an RnData, or something silly)
  = cachedDecl iface_cache (isRnTyConOrClass rn) (origName "cachedDeclByType" rn)  >>= \ maybe_decl ->
    let
	return_maybe_decl = return maybe_decl
	return_failed msg = return (CachingFail msg)
    in
    case maybe_decl of
      CachingAvoided _	  -> return_maybe_decl
      CachingFail io_msg  -> return_failed (ifaceIoErr io_msg rn)
      CachingHit  if_decl ->
	case rn of
	  WiredInId _       -> return_failed (ifaceLookupWiredErr "value" rn)
	  WiredInTyCon _    -> return_failed (ifaceLookupWiredErr "type constructor" rn)
	  RnUnbound _       -> panic "cachedDeclByType:" -- (ppr PprDebug rn)
	  
	  RnSyn _	    -> return_maybe_decl
	  RnData _ _ _	    -> return_maybe_decl
	  RnImplicitTyCon _ -> if is_tycon_decl if_decl
			       then return_maybe_decl
			       else return_failed (badIfaceLookupErr "type constructor" rn if_decl)
	  
	  RnClass _ _	    -> return_maybe_decl
	  RnImplicitClass _ -> if is_class_decl if_decl
			       then return_maybe_decl
			       else return_failed (badIfaceLookupErr "class" rn if_decl)
	  
	  RnName _	    -> return_maybe_decl
	  RnConstr _ _      -> return_maybe_decl
	  RnField _ _ 	    -> return_maybe_decl
	  RnClassOp _ _	    -> return_maybe_decl
	  RnImplicit _	    -> if is_val_decl if_decl
			       then return_maybe_decl
			       else return_failed (badIfaceLookupErr "value" rn if_decl)
  where
    is_tycon_decl (TypeSig _ _ _)	= True
    is_tycon_decl (NewTypeSig _ _ _ _)	= True
    is_tycon_decl (DataSig _ _ _ _ _)	= True
    is_tycon_decl _			= False

    is_class_decl (ClassSig _ _ _ _)	= True
    is_class_decl _			= False

    is_val_decl (ValSig _ _ _)		= True
    is_val_decl (DataSig _ _ _ _ _)	= True	-- may be a constr or field
    is_val_decl (NewTypeSig _ _ _ _)	= True  -- may be a constr
    is_val_decl (ClassSig _ _ _ _)	= True	-- may be a method
    is_val_decl _			= False
\end{code}

\begin{code}
readIface :: FilePath -> Module -> FAST_STRING -> IO (MaybeErr ParsedIface Error)

readIface file modname item
  = --hPutStr stderr ("  reading "++file++" ("++ _UNPK_ item ++")") >>
    TRY_IO (readFile file)  >>= \ read_result ->
    case read_result of
      Left  err      -> return (Failed (cannaeReadErr file err))
      Right contents -> --hPutStr stderr ".."   >>
			let parsed = parseIface contents in
			--hPutStr stderr "..\n" >>
			return (
			case parsed of
			  Failed _    -> parsed
			  Succeeded p -> Succeeded (init_merge modname p)
			)
  where
    init_merge this (ParsedIface mod _ v sv us vs exps insts fixes tdefs vdefs idefs prags)
      =	ParsedIface mod (False, unitBag this) v sv us vs exps insts fixes tdefs vdefs idefs prags
\end{code}


\begin{code}
rnIfaces :: IfaceCache			-- iface cache (mutvar)
	 -> [Module]			-- directly imported modules
	 -> UniqSupply
	 -> RnEnv			-- defined (in the source) name env
	 -> RnEnv			-- mentioned (in the source) name env 
	 -> RenamedHsModule		-- module to extend with iface decls
	 -> [RnName]			-- imported names required (really the
					-- same info as in mentioned name env)
					-- Also, all the things we may look up
					-- later by key (Unique).
	 -> IO (RenamedHsModule,	-- extended module
		RnEnv,			-- final env (for renaming derivings)
		ImplicitEnv,		-- implicit names used (for usage info)
		(UsagesMap,VersionsMap,[Module]),	-- usage info
		(Bag Error, Bag Warning))

rnIfaces iface_cache imp_mods us
	 def_env@((dqual, dunqual, dtc_qual, dtc_unqual), dstack)
	 occ_env@((qual, unqual, tc_qual, tc_unqual), stack)
	 rn_module@(HsModule modname iface_version exports imports fixities
		      typedecls typesigs classdecls instdecls instsigs
		      defdecls binds sigs src_loc)
	 todo
  = {-
    pprTrace "rnIfaces:going after:" (ppCat (map (ppr PprDebug) todo)) $
    pprTrace "rnIfaces:qual:"      (ppCat [ppBesides[ppPStr m,ppChar '.',ppPStr n] | (n,m) <- keysFM qual]) $
    pprTrace "rnIfaces:unqual:"    (ppCat (map ppPStr (keysFM unqual))) $
    pprTrace "rnIfaces:tc_qual:"   (ppCat [ppBesides[ppPStr m,ppChar '.',ppPStr n] | (n,m) <- keysFM tc_qual]) $
    pprTrace "rnIfaces:tc_unqual:" (ppCat (map ppPStr (keysFM tc_unqual))) $

    pprTrace "rnIfaces:dqual:"     (ppCat [ppBesides[ppPStr m,ppChar '.',ppPStr n] | (n,m) <- keysFM dqual]) $
    pprTrace "rnIfaces:dunqual:"   (ppCat (map ppPStr (keysFM dunqual))) $
    pprTrace "rnIfaces:dtc_qual:"  (ppCat [ppBesides[ppPStr m,ppChar '.',ppPStr n] | (n,m) <- keysFM dtc_qual]) $
    pprTrace "rnIfaces:dtc_unqual:"(ppCat (map ppPStr (keysFM dtc_unqual))) $
    -}

    -- do transitive closure to bring in all needed names/defns and insts:

    decls_and_insts todo def_env occ_env empty_return us 
	>>= \ (((if_typedecls, if_classdecls, if_instdecls, if_sigs),
	        if_implicits,
	        if_errs_warns),
	       if_final_env) ->

    -- finalize what we want to say we learned about the
    -- things we used
    finalIfaceInfo iface_cache modname if_final_env if_instdecls {-all_imports_used imp_mods-} >>=
	\ usage_stuff@(usage_info, version_info, instance_mods) ->

    return (HsModule modname iface_version exports imports fixities
		 (typedecls ++ if_typedecls)
		 typesigs
		 (classdecls ++ if_classdecls)
		 (instdecls  ++ if_instdecls)
		 instsigs defdecls binds
		 (sigs ++ if_sigs)
		 src_loc,
	    if_final_env,
	    if_implicits,
	    usage_stuff,
	    if_errs_warns)
  where
    decls_and_insts todo def_env occ_env to_return us
      =	let
	    (us1,us2) = splitUniqSupply us
	in
	do_decls todo	     		 -- initial batch of names to process
	 	 (def_env, occ_env, us1) -- init stuff down
	 	 to_return		 -- acc results
	   >>= \ (decls_return,
		  decls_def_env,
		  decls_occ_env) ->

	cacheInstModules iface_cache imp_mods >>= \ errs ->

	do_insts decls_def_env decls_occ_env emptyRnEnv emptyFM
		 (add_errs errs decls_return) us2

    --------
    do_insts def_env occ_env prev_env done_insts to_return us
      | size_tc_env occ_env == size_tc_env prev_env
      = return (to_return, occ_env)

      | otherwise
      = rnIfaceInstStuff iface_cache modname us1 occ_env done_insts to_return
	   >>= \ (insts_return,
		  new_insts,
		  insts_occ_env,
		  new_unknowns) ->

	do_decls new_unknowns	  		-- new batch of names to process
	 	 (def_env, insts_occ_env, us2) 	-- init stuff down
	 	 insts_return		 	-- acc results
	   >>= \ (decls_return,
		  decls_def_env,
		  decls_occ_env) ->

	do_insts decls_def_env decls_occ_env occ_env new_insts decls_return us3
      where
	(us1,us') = splitUniqSupply us
	(us2,us3) = splitUniqSupply us'

	size_tc_env ((_, _, qual, unqual), _)
	  = sizeFM qual + sizeFM unqual


    do_decls :: [RnName]	-- Names we're looking for; we keep adding/deleting
			  	-- from this list; we're done when empty (nothing
			  	-- more needs to be looked for)
	     -> Go_Down	 	-- see defn below
	     -> To_Return	-- accumulated result
	     -> IO (To_Return,
		    RnEnv,	-- extended decl env
		    RnEnv)	-- extended occ env

    do_decls to_find@[] down to_return
      = return (to_return, defenv down, occenv down)

    do_decls to_find@(n:ns) down to_return 
      = case (lookup_defd down n) of
	  Just  _ -> -- previous processing must've found the stuff for this name;
		     -- continue with the rest:
		     -- pprTrace "do_decls:done:" (ppr PprDebug n) $
		     do_decls ns down to_return

	  Nothing
	   | moduleOf (origName "do_decls" n) == modname ->
		     -- avoid looking in interface for the module being compiled
		     --pprTrace "do_decls:this module error:" (ppr PprDebug n) $
		     do_decls ns down (add_warn (thisModImplicitWarn modname n) to_return)

	   | otherwise ->
		     -- OK, see what the cache has for us...

	     cachedDeclByType iface_cache n >>= \ maybe_ans ->
	     case maybe_ans of
	       CachingAvoided _ ->
		 --pprTrace "do_decls:caching avoided:" (ppr PprDebug n) $
		 do_decls ns down to_return

	       CachingFail err -> -- add the error, but keep going:
		 --pprTrace "do_decls:cache error:" (ppr PprDebug n) $
		 do_decls ns down (add_err err to_return)

	       CachingHit iface_decl -> -- something needing renaming!
		 let
		    (us1, us2) = splitUniqSupply (uniqsupply down)
		 in
		 case (initRn False{-iface-} modname (occenv down) us1 (
			setExtraRn emptyUFM{-no fixities-} $
			rnIfaceDecl iface_decl)) of {
		  ((if_decl, if_defd, if_implicits), if_errs, if_warns) ->
		    let
			new_unknowns = eltsFM (fst if_implicits) ++ eltsFM (snd if_implicits)
		    in
		    {-
		    pprTrace "do_decls:renamed:" (ppAboves [ppr PprDebug n
			, ppCat [ppStr "new unknowns:", interpp'SP PprDebug new_unknowns]
			, ppCat [ppStr "defd vals:", interpp'SP PprDebug [n | (_,n) <- fst if_defd] ]
			, ppCat [ppStr "defd  tcs:", interpp'SP PprDebug [n | (_,n) <- snd if_defd] ]
			]) $
		    -}
		    do_decls (new_unknowns ++ ns)
			     (add_occs       if_defd if_implicits $
			       new_uniqsupply us2 down)
			     (add_decl	     if_decl		$
			       add_implicits if_implicits	$
			        add_errs     if_errs		$
			         add_warns   if_warns to_return)
	         }

-----------
type Go_Down   = (RnEnv,	-- stuff we already have defns for;
				-- to check quickly if we've already
				-- found something for the name under consideration,
			  	-- due to previous processing.
				-- It starts off just w/ the defns for
				-- the things in this module.
		  RnEnv,	-- occurrence env; this gets added to as
				-- we process new iface decls.  It includes
				-- entries for *all* occurrences, including those
				-- for which we have definitions.
		  UniqSupply	-- the obvious
		 )

lookup_defd (def_env, _, _) n
  = (if isRnTyConOrClass n then lookupTcRnEnv else lookupRnEnv) def_env
	(case (origName "lookup_defd" n) of { OrigName m s -> Qual m s })
	-- this is hack because we are reusing the RnEnv technology

defenv	   (def_env, _, _) = def_env
occenv	   (_, occ_env, _) = occ_env
uniqsupply (_, _,      us) = us

new_uniqsupply us (def_env, occ_env, _) = (def_env, occ_env, us)

add_occs (val_defds, tc_defds) (val_imps, tc_imps) (def_env, occ_env, us)
  = case (extendGlobalRnEnv def_env val_defds tc_defds) of { (new_def_env, def_dups) ->
    --(if isEmptyBag def_dups then \x->x else pprTrace "add_occs:" (ppCat [ppr PprDebug n | (n,_,_) <- bagToList def_dups])) $
--  ASSERT(isEmptyBag def_dups)
    let
	de_orig imps = [ (Qual m n, v) | (OrigName m n, v) <- fmToList imps ]
	-- again, this hackery because we are reusing the RnEnv technology

	val_occs = val_defds ++ de_orig val_imps
	tc_occs  = tc_defds  ++ de_orig tc_imps
    in
    case (extendGlobalRnEnv occ_env val_occs tc_occs)   of { (new_occ_env, occ_dups) ->

--  ASSERT(isEmptyBag occ_dups)
--  False because we may get a dup on the name we just shoved in

    (new_def_env, new_occ_env, us) }}

----------------
type To_Return = (([RenamedTyDecl], [RenamedClassDecl], [RenamedInstDecl], [RenamedSig]),
		  ImplicitEnv,	-- new names used implicitly
		  (Bag Error, Bag Warning)
		 )

empty_return :: To_Return
empty_return = (([],[],[],[]), emptyImplicitEnv, (emptyBag,emptyBag))

add_decl decl ((tydecls, classdecls, instdecls, sigs), implicit, msgs)
  = case decl of
      AddedTy	 t -> ((t:tydecls, classdecls, instdecls, sigs), implicit, msgs)
      AddedClass c -> ((tydecls, c:classdecls, instdecls, sigs), implicit, msgs)
      AddedSig	 s -> ((tydecls, classdecls, instdecls, s:sigs), implicit, msgs)

add_insts is ((tydecls, classdecls, instdecls, sigs), implicit, msgs)
  = ((tydecls, classdecls, is ++ instdecls, sigs), implicit, msgs)

add_implicits (val_imps, tc_imps) (decls, (val_fm, tc_fm), msgs)
  = (decls, (val_fm `plusFM` val_imps, tc_fm `plusFM` tc_imps), msgs)

add_err  err (decls,implicit,(errs,warns)) = (decls,implicit,(errs `snocBag`   err,warns))
add_errs ers (decls,implicit,(errs,warns)) = (decls,implicit,(errs `unionBags` ers,warns))
add_warn wrn (decls,implicit,(errs,warns)) = (decls,implicit,(errs, warns `snocBag` wrn))
add_warns ws (decls,implicit,(errs,warns)) = (decls,implicit,(errs, warns `unionBags` ws))
\end{code}

\begin{code}
data AddedDecl -- purely local
  = AddedTy	RenamedTyDecl
  | AddedClass	RenamedClassDecl
  | AddedSig	RenamedSig

rnIfaceDecl :: RdrIfaceDecl
	    -> RnM_Fixes REAL_WORLD
		   (AddedDecl,	-- the resulting decl to add to the pot
		    ([(RdrName,RnName)], [(RdrName,RnName)]),
				-- new val/tycon-class names that have
				-- *been defined* while processing this decl
		    ImplicitEnv -- new implicit val/tycon-class names that we
				-- stumbled into
		   )

rnIfaceDecl (TypeSig tc _ decl)
  = rnTyDecl    decl	`thenRn` \ rn_decl   ->
    lookupTyCon tc	`thenRn` \ rn_tc     ->
    getImplicitUpRn	`thenRn` \ mentioned ->
    let
	defds = ([], [(tc, rn_tc)])
	implicits = mentioned `sub` defds
    in
    returnRn (AddedTy rn_decl, defds, implicits)

rnIfaceDecl (NewTypeSig tc dc _ decl)
  = rnTyDecl    decl	`thenRn` \ rn_decl   ->
    lookupTyCon tc	`thenRn` \ rn_tc     ->
    lookupValue dc	`thenRn` \ rn_dc     ->
    getImplicitUpRn	`thenRn` \ mentioned ->
    let
	defds = ([(dc, rn_dc)], [(tc, rn_tc)])
	implicits = mentioned `sub` defds
    in
    returnRn (AddedTy rn_decl, defds, implicits)

rnIfaceDecl (DataSig tc dcs fcs _ decl)
  = rnTyDecl    decl		`thenRn` \ rn_decl   ->
    lookupTyCon tc		`thenRn` \ rn_tc     ->
    mapRn lookupValue dcs	`thenRn` \ rn_dcs    ->
    mapRn lookupValue fcs	`thenRn` \ rn_fcs    ->
    getImplicitUpRn		`thenRn` \ mentioned ->
    let
	defds = (zip dcs rn_dcs ++ zip fcs rn_fcs , [(tc, rn_tc)])
	implicits = mentioned `sub` defds
    in
    returnRn (AddedTy rn_decl, defds, implicits)

rnIfaceDecl (ClassSig clas ops _ decl)
  = rnClassDecl decl			`thenRn` \ rn_decl   ->
    lookupClass clas			`thenRn` \ rn_clas   ->
    mapRn (lookupClassOp rn_clas) ops	`thenRn` \ rn_ops    ->
    getImplicitUpRn			`thenRn` \ mentioned ->
    let
	defds = (ops `zip` rn_ops, [(clas, rn_clas)])
	implicits = mentioned `sub` defds
    in
    returnRn (AddedClass rn_decl, defds, implicits)

rnIfaceDecl (ValSig f src_loc ty)
    -- should rename_sig in RnBinds be used here? ToDo
  = lookupValue f			`thenRn` \ rn_f  ->
    -- pprTrace "rnIfaceDecl:ValSig:" (ppr PprDebug ty) $
    rnPolyType nullTyVarNamesEnv ty	`thenRn` \ rn_ty ->
    getImplicitUpRn			`thenRn` \ mentioned ->
    let
	defds = ([(f, rn_f)], [])
	implicits = mentioned `sub` defds
    in
    returnRn (AddedSig (Sig rn_f rn_ty noGenPragmas src_loc), defds, implicits)

----
sub :: ImplicitEnv -> ([(RdrName,RnName)], [(RdrName,RnName)]) -> ImplicitEnv

sub (val_ment, tc_ment) (val_defds, tc_defds)
  = (delListFromFM val_ment (map (qualToOrigName . fst) val_defds),
     delListFromFM tc_ment  (map (qualToOrigName . fst) tc_defds))
\end{code}

% ------------------------------

@cacheInstModules@: cache instance modules specified in imports

\begin{code}
cacheInstModules :: IfaceCache -> [Module] -> IO (Bag Error)

cacheInstModules iface_cache@(IfaceCache _ _ iface_var) imp_mods
  = readVar iface_var		ST_THEN \ (iface_fm, _, _) ->
    let
	imp_ifaces      = [ iface | Just iface <- map (lookupFM iface_fm) imp_mods ]
	(imp_imods, _)  = removeDups cmpPString (bagToList (unionManyBags (map get_ims imp_ifaces)))
        get_ims (ParsedIface _ _ _ _ _ _ _ ims _ _ _ _ _) = ims
    in
    --pprTrace "cacheInstModules:" (ppCat (map ppPStr imp_imods)) $
    accumulate (map (cachedIface iface_cache False SLIT("instance_modules")) imp_imods) >>= \ err_or_ifaces ->

    -- Sanity Check:
    -- Assert that instance modules given by direct imports contains
    -- instance modules extracted from all visited modules

    readVar iface_var		ST_THEN \ (all_iface_fm, _, _) ->
    let
	all_ifaces     = eltsFM all_iface_fm
	(all_imods, _) = removeDups cmpPString (bagToList (unionManyBags (map get_ims (all_ifaces))))
    in
    ASSERT(sortLt (<) imp_imods == sortLt (<) all_imods)

    return (bag_errs err_or_ifaces)
  where
    bag_errs [] = emptyBag
    bag_errs (Failed err :rest) = err `consBag` bag_errs rest
    bag_errs (Succeeded _:rest) = bag_errs rest
\end{code}


@rnIfaceInstStuff@: Deal with instance declarations from interface files.

\begin{code}
type InstanceEnv = FiniteMap (OrigName, OrigName) Int

rnIfaceInstStuff
	:: IfaceCache		-- all about ifaces we've read
	-> Module
	-> UniqSupply
	-> RnEnv		-- current occ env
	-> InstanceEnv  	-- instances for these tycon/class pairs done
	-> To_Return
	-> IO (To_Return,
	       InstanceEnv,	-- extended instance env
	       RnEnv,		-- final occ env
	       [RnName])	-- new unknown names

rnIfaceInstStuff iface_cache@(IfaceCache _ _ iface_var) modname us occ_env done_inst_env to_return
  = -- all the instance decls we might even want to consider
    -- are in the ParsedIfaces that are in our cache

    readVar iface_var	ST_THEN \ (_, orig_iface_fm, _) ->
    let
	all_ifaces	  = eltsFM orig_iface_fm
	all_insts	  = concat (map get_insts all_ifaces)
	interesting_insts = filter want_inst all_insts

	-- Sanity Check:
	-- Assert that there are no more instances for the done instances

	claim_done       = filter is_done_inst all_insts
	claim_done_env   = foldr add_done_inst emptyFM claim_done

	has_val fm (k,i) = case lookupFM fm k of { Nothing -> False; Just v -> i == v }
    in
    {-
      pprTrace "all_insts:\n"         (ppr_insts (bagToList all_insts)) $
      pprTrace "interesting_insts:\n" (ppr_insts interesting_insts) $
    -}
    ASSERT(sizeFM done_inst_env == sizeFM claim_done_env)
    ASSERT(all (has_val claim_done_env) (fmToList done_inst_env))

    case (initRn False{-iface-} modname occ_env us (
	    setExtraRn emptyUFM{-no fixities-}	$
	    mapRn rnIfaceInst interesting_insts `thenRn` \ insts ->
	    getImplicitUpRn			`thenRn` \ implicits ->
	    returnRn (insts, implicits))) of {
      ((if_insts, if_implicits), if_errs, if_warns) ->

	return (add_insts      if_insts		$
		 add_implicits if_implicits	$
		  add_errs     if_errs		$
		   add_warns   if_warns to_return,
		foldr add_done_inst done_inst_env interesting_insts,
		add_imp_occs if_implicits occ_env,
		eltsFM (fst if_implicits) ++ eltsFM (snd if_implicits))
    }
  where
    get_insts (ParsedIface imod _ _ _ _ _ _ _ _ _ _ insts _) = [(imod, inst) | inst <- bagToList insts]

    tycon_class clas tycon = (qualToOrigName clas, qualToOrigName tycon)

    add_done_inst (_, InstSig clas tycon _ _) inst_env
      = addToFM_C (+) inst_env (tycon_class clas tycon) 1

    is_done_inst (_, InstSig clas tycon _ _)
      = maybeToBool (lookupFM done_inst_env (tycon_class clas tycon))

    add_imp_occs (val_imps, tc_imps) occ_env
      = case (extendGlobalRnEnv occ_env (de_orig val_imps) (de_orig tc_imps)) of
     	  (ext_occ_env, occ_dups) -> ASSERT(isEmptyBag occ_dups)
				     ext_occ_env
      where
	de_orig imps = [ (Qual m n, v) | (OrigName m n, v) <- fmToList imps ]
	-- again, this hackery because we are reusing the RnEnv technology

    want_inst i@(imod, InstSig clas tycon _ _)
      = -- it's a "good instance" (one to hang onto) if we have a
	-- chance of referring to *both* the class and tycon later on ...
	--pprTrace "want_inst:" (ppCat [ppr PprDebug clas, ppr PprDebug tycon, ppr PprDebug (mentionable tycon), ppr PprDebug (mentionable clas), ppr PprDebug(is_done_inst i)]) $
	mentionable tycon && mentionable clas && not (is_done_inst i)
      where
	mentionable nm
	  = case lookupTcRnEnv occ_env nm of
	      Just  _ -> True
	      Nothing -> -- maybe it's builtin
		let orig = qualToOrigName nm in
		case (lookupFM builtinTcNamesMap orig) of
		  Just  _ -> True
		  Nothing -> maybeToBool (lookupFM builtinKeysMap orig)
\end{code}

\begin{code}
rnIfaceInst :: (Module, RdrIfaceInst) -> RnM_Fixes REAL_WORLD RenamedInstDecl

rnIfaceInst (imod, InstSig _ _ _ inst_decl) = rnInstDecl (inst_decl imod)
\end{code}

\begin{code}
type BigMaps = (FiniteMap Module Version, -- module-version map
		FiniteMap (FAST_STRING,Module) Version) -- ordinary version map

finalIfaceInfo ::
	   IfaceCache			-- iface cache
	-> Module			-- this module's name
	-> RnEnv
	-> [RenamedInstDecl]
--	-> [RnName]			-- all imported names required
--	-> [Module]			-- directly imported modules
	-> IO (UsagesMap,
	       VersionsMap,		-- info about version numbers
	       [Module])		-- special instance modules

finalIfaceInfo iface_cache@(IfaceCache _ _ iface_var) modname if_final_env@((qual, unqual, tc_qual, tc_unqual), stack) if_instdecls
  =
--  pprTrace "usageIf:qual:"      (ppCat [ppBesides[ppPStr m,ppChar '.',ppPStr n] | (n,m) <- keysFM qual]) $
--  pprTrace "usageIf:unqual:"    (ppCat (map ppPStr (keysFM unqual))) $
--  pprTrace "usageIf:tc_qual:"   (ppCat [ppBesides[ppPStr m,ppChar '.',ppPStr n] | (n,m) <- keysFM tc_qual]) $
--  pprTrace "usageIf:tc_unqual:" (ppCat (map ppPStr (keysFM tc_unqual))) $
    readVar iface_var	ST_THEN \ (_, orig_iface_fm, _) ->
    let
	all_ifaces = eltsFM orig_iface_fm
	-- all the interfaces we have looked at

	big_maps
	  -- combine all the version maps we have seen into maps to
	  -- (a) lookup a module-version number, lookup an entity's
	  -- individual version number
	  = foldr mk_map (emptyFM,emptyFM) all_ifaces

	val_stuff@(val_usages, val_versions)
	  = foldFM (process_item big_maps) (emptyFM, emptyFM){-init-} qual

	(all_usages, all_versions)
	  = foldFM (process_item big_maps) val_stuff{-keep going-} tc_qual
    in
    return (all_usages, all_versions, [])
  where
    mk_map (ParsedIface m _ mv _ _ vers _ _ _ _ _ _ _) (mv_map, ver_map)
      = (addToFM     mv_map  m mv, -- add this module
	 addListToFM ver_map [ ((n,m), v) | (n,v) <- fmToList vers ])

    -----------------------
    process_item :: BigMaps
		 -> (FAST_STRING,Module) -> RnName -- RnEnv (QualNames) components
		 -> (UsagesMap, VersionsMap)	   -- input
		 -> (UsagesMap, VersionsMap)	   -- output

    process_item (big_mv_map, big_version_map) key@(n,m) rn as_before@(usages, versions)
      | irrelevant rn
      = as_before
      | m == modname -- this module => add to "versions"
      =	(usages, addToFM versions n 1{-stub-})
      | otherwise  -- from another module => add to "usages"
      = case (add_to_usages usages key) of
	  Nothing	  -> as_before
	  Just new_usages -> (new_usages, versions)
      where
	add_to_usages usages key@(n,m)
	  = case (lookupFM big_mv_map m) of
	      Nothing -> Nothing
	      Just mv ->
	        case (lookupFM big_version_map key) of
		  Nothing -> Nothing
		  Just kv ->
		    Just $ addToFM usages m (
			case (lookupFM usages m) of
			  Nothing -> -- nothing for this module yet...
			    (mv, unitFM n kv)

			  Just (mversion, mstuff) -> -- the "new" stuff will shadow the old
			    ASSERT(mversion == mv)
			    (mversion, addToFM mstuff n kv)
		    )

    irrelevant (RnConstr  _ _) = True	-- We don't report these in their
    irrelevant (RnField   _ _) = True	-- own right in usages/etc.
    irrelevant (RnClassOp _ _) = True
    irrelevant (RnImplicit  n) = isLexCon (nameOf (origName "irrelevant" n)) -- really a RnConstr
    irrelevant _	       = False

\end{code}


\begin{code}
thisModImplicitWarn mod n sty
  = ppBesides [ppPStr SLIT("An interface has an implicit need of "), ppr sty n, ppPStr SLIT("; assuming this module will provide it.")]

noIfaceErr mod sty
  = ppCat [ppPStr SLIT("Could not find interface for:"), ppPStr mod]

noOrigIfaceErr mod sty
  = ppCat [ppPStr SLIT("Could not find original interface for:"), ppPStr mod]

noDeclInIfaceErr mod str sty
  = ppBesides [ppPStr SLIT("Could not find interface declaration of: "),
	       ppPStr mod, ppStr ".", ppPStr str]

cannaeReadErr file err sty
  = ppBesides [ppPStr SLIT("Failed in reading file: "), ppStr file, ppStr "; error=", ppStr (show err)]

ifaceLookupWiredErr msg n sty
  = ppBesides [ppPStr SLIT("Why am I looking up a wired-in "), ppStr msg, ppChar ':', ppr sty n]

badIfaceLookupErr msg name decl sty
  = ppBesides [ppPStr SLIT("Expected a "), ppStr msg, ppStr " declaration, but got this: ???"]

ifaceIoErr io_msg rn sty
  = ppBesides [io_msg sty, ppStr "; looking for: ", ppr sty rn]
\end{code}
