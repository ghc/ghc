%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnIfaces]{Cacheing and Renaming of Interfaces}

\begin{code}
#include "HsVersions.h"

module RnIfaces (
	findHiFiles,
	cachedIface,
	cachedDecl,
	readIface,
	rnIfaces,
	finalIfaceInfo,
	IfaceCache(..),
	VersionInfo(..)
    ) where

import Ubiq

import LibDirectory
import PreludeGlaST	( thenPrimIO, seqPrimIO, readVar, writeVar, MutableVar(..) )

import HsSyn
import HsPragmas	( noGenPragmas )
import RdrHsSyn
import RnHsSyn

import RnMonad
import RnSource		( rnTyDecl, rnClassDecl, rnInstDecl, rnPolyType )
import RnUtils		( RnEnv(..), lookupRnEnv, lookupTcRnEnv, extendGlobalRnEnv )
import ParseIface	( parseIface )
import ParseUtils	( ParsedIface(..), RdrIfaceDecl(..), RdrIfaceInst(..) )

import Bag		( emptyBag, snocBag, unionBags, unionManyBags, isEmptyBag, bagToList )
import CmdLineOpts	( opt_HiSuffix, opt_SysHiSuffix )
import ErrUtils		( Error(..), Warning(..) )
import FiniteMap	( emptyFM, lookupFM, addToFM, plusFM, eltsFM,
			  fmToList, delListFromFM, keysFM{-ToDo:rm-}
			)
import Maybes		( maybeToBool )
import Name		( moduleNamePair, origName, isRdrLexCon,
			  RdrName(..){-instance NamedThing-}
			)
import PprStyle		-- ToDo:rm
import Outputable	-- ToDo:rm
import PrelInfo		( builtinNameInfo )
import Pretty
import Maybes		( MaybeErr(..) )
import UniqFM		( emptyUFM )
import UniqSupply	( splitUniqSupply )
import Util		( startsWith, panic, pprPanic, assertPanic, pprTrace{-ToDo:rm-} )
\end{code}

\begin{code}
type ModuleToIfaceContents = FiniteMap Module ParsedIface
type ModuleToIfaceFilePath = FiniteMap Module FilePath

type IfaceCache
  = MutableVar _RealWorld (ModuleToIfaceContents,
			   ModuleToIfaceFilePath)
\end{code}

*********************************************************
*							*
\subsection{Looking for interface files}
*							*
*********************************************************

Return a mapping from module-name to
absolute-filename-for-that-interface.
\begin{code}
findHiFiles :: [FilePath] -> [FilePath] -> IO (FiniteMap Module FilePath)

findHiFiles dirs sysdirs
  = do_dirs emptyFM (dirs ++ sysdirs)
  where
    do_dirs env [] = return env
    do_dirs env (dir:dirs)
      = do_dir  env     dir	>>= \ new_env ->
	do_dirs new_env dirs
    -------
    do_dir env dir
      = --trace ("Having a go on..."++dir) $
	getDirectoryContents dir    >>= \ entries ->
	do_entries env entries
      where
	do_entries env [] = return env
	do_entries env (e:es)
	  = do_entry   env     e    >>= \ new_env ->
	    do_entries new_env es
	-------
	do_entry env e
	  = case (acceptable_hi (reverse e)) of
	      Nothing  -> --trace ("Deemed uncool:"++e) $
			  return env
	      Just mod ->
		let
		      pmod = _PK_ mod
		in
		case (lookupFM env pmod) of
		  Nothing -> --trace ("Adding "++mod++" -> "++e) $
			     return (addToFM env pmod (dir ++ '/':e))
			     -- ToDo: use DIR_SEP, not /

		  Just xx -> ( if de_dot xx /= e then trace ("Already mapped!! "++mod++" -> "++xx++"; ignoring:"++e) else id) $
			     return env
    -------
    acceptable_hi rev_e -- looking at pathname *backwards*
      = case (startsWith (reverse opt_HiSuffix) rev_e) of
	  Nothing -> Nothing
	  Just xs -> plausible_modname xs{-reversed-}

    -------
    de_dot ('.' : '/' : xs) = xs
    de_dot xs		    = xs

    -------
    plausible_modname rev_e
      = let
	    cand = reverse (takeWhile is_modname_char rev_e)
	in
	if null cand || not (isUpper (head cand))
	then Nothing
	else Just cand
      where
	is_modname_char c = isAlphanum c || c == '_'
\end{code}

*********************************************************
*							*
\subsection{Reading interface files}
*							*
*********************************************************

Return cached info about a Module's interface; otherwise,
read the interface (using our @ModuleToIfaceFilePath@ map
to decide where to look).

\begin{code}
cachedIface :: IfaceCache
	    -> Module
	    -> IO (MaybeErr ParsedIface Error)

cachedIface iface_cache mod
  = readVar iface_cache `thenPrimIO` \ (iface_fm, file_fm) ->

    case (lookupFM iface_fm mod) of
      Just iface -> return (Succeeded iface)
      Nothing    ->
      	case (lookupFM file_fm mod) of
	  Nothing   -> return (Failed (noIfaceErr mod))
	  Just file ->
	    readIface file mod >>= \ read_iface ->
	    case read_iface of
	      Failed err      -> -- pprTrace "module-file map:\n" (ppAboves [ppCat [ppPStr m, ppStr f] | (m,f) <- fmToList file_fm]) $
				 return (Failed err)
	      Succeeded iface ->
		let
		    iface_fm' = addToFM iface_fm mod iface
		in
		writeVar iface_cache (iface_fm', file_fm) `seqPrimIO`
		return (Succeeded iface)

----------
cachedDecl :: IfaceCache
	   -> Bool	-- True <=> tycon or class name
	   -> RdrName
	   -> IO (MaybeErr RdrIfaceDecl Error)

-- ToDo: this is where the check for Prelude.map being
--       located in PreludeList.map should be done ...

cachedDecl iface_cache class_or_tycon orig 
  = cachedIface iface_cache mod 	>>= \ maybe_iface ->
    case maybe_iface of
      Failed err -> return (Failed err)
      Succeeded (ParsedIface _ _ _ _ exps _ _ tdefs vdefs _ _) -> 
	case (lookupFM (if class_or_tycon then tdefs else vdefs) str) of
	  Just decl -> return (Succeeded decl)
	  Nothing   -> return (Failed (noDeclInIfaceErr mod str))
  where
    (mod, str) = moduleNamePair orig

----------
cachedDeclByType :: IfaceCache
		 -> RnName{-NB: diff type than cachedDecl -}
		 -> IO (MaybeErr RdrIfaceDecl Error)

cachedDeclByType iface_cache rn
    -- the idea is: check that, e.g., if we're given an
    -- RnClass, then we really get back a ClassDecl from
    -- the cache (not an RnData, or something silly)
  = cachedDecl iface_cache (isRnTyConOrClass rn) (origName rn)  >>= \ maybe_decl ->
    let
	return_maybe_decl = return maybe_decl
	return_failed msg = return (Failed msg)
    in
    case maybe_decl of
      Failed _ -> return_maybe_decl
      Succeeded if_decl ->
	case rn of
	  WiredInId _       -> return_failed (ifaceLookupWiredErr "value" rn)
	  WiredInTyCon _    -> return_failed (ifaceLookupWiredErr "type constructor" rn)
	  RnUnbound _       -> pprPanic "cachedDeclByType:" (ppr PprDebug rn)
	  
	  RnSyn _	    -> return_maybe_decl
	  RnData _ _	    -> return_maybe_decl
	  RnImplicitTyCon _ -> if is_tycon_decl if_decl
			       then return_maybe_decl
			       else return_failed (badIfaceLookupErr "type constructor" rn if_decl)
	  
	  RnClass _ _	    -> return_maybe_decl
	  RnImplicitClass _ -> if is_class_decl if_decl
			       then return_maybe_decl
			       else return_failed (badIfaceLookupErr "class" rn if_decl)
	  
	  RnName _	    ->  return_maybe_decl
	  RnConstr _ _	    ->  return_maybe_decl
	  RnClassOp _ _	    ->  return_maybe_decl
	  RnImplicit _	    ->  if is_val_decl if_decl
			        then return_maybe_decl
			        else return_failed (badIfaceLookupErr "value/method" rn if_decl)
  where
    is_tycon_decl (TypeSig _ _ _)	= True
    is_tycon_decl (NewTypeSig _ _ _ _)	= True
    is_tycon_decl (DataSig _ _ _ _)	= True
    is_tycon_decl _			= False

    is_class_decl (ClassSig _ _ _ _)	= True
    is_class_decl _			= False

    is_val_decl (ValSig _ _ _)		= True
    is_val_decl (ClassSig _ _ _ _)	= True	-- if the thing we were after *happens* to
						-- be a class op; we will have fished a ClassSig
						-- out of the interface for it.
    is_val_decl _			= False
\end{code}

\begin{code}
readIface :: FilePath -> Module
	      -> IO (MaybeErr ParsedIface Error)

readIface file mod
  = readFile file   `thenPrimIO` \ read_result ->
    case read_result of
      Left  err      -> return (Failed (cannaeReadErr file err))
      Right contents -> return (parseIface contents)
\end{code}


\begin{code}
rnIfaces :: IfaceCache			-- iface cache (mutvar)
	 -> UniqSupply
	 -> RnEnv			-- defined (in the source) name env
	 -> RnEnv			-- mentioned (in the source) name env 
	 -> RenamedHsModule		-- module to extend with iface decls
	 -> [RnName]			-- imported names required (really the
					-- same info as in mentioned name env)
					-- Also, all the things we may look up
					-- later by key (Unique).
	 -> IO (RenamedHsModule,	-- extended module
		ImplicitEnv,		-- implicit names used (for usage info)
		Bag Error,
		Bag Warning)

rnIfaces iface_cache us
	 def_env@((dqual, dunqual, dtc_qual, dtc_unqual), dstack)
	 occ_env@((qual, unqual, tc_qual, tc_unqual), stack)
	 rn_module@(HsModule modname iface_version exports imports fixities
		      typedecls typesigs classdecls instdecls instsigs
		      defdecls binds sigs src_loc)
	 todo
  = {-pprTrace "rnIfaces:going after:" (ppCat (map (ppr PprDebug) todo)) $

    pprTrace "rnIfaces:qual:"      (ppCat [ppBesides[ppPStr m,ppChar '.',ppPStr n] | (m,n) <- keysFM qual]) $
    pprTrace "rnIfaces:unqual:"    (ppCat (map ppPStr (keysFM unqual))) $
    pprTrace "rnIfaces:tc_qual:"   (ppCat [ppBesides[ppPStr m,ppChar '.',ppPStr n] | (m,n) <- keysFM tc_qual]) $
    pprTrace "rnIfaces:tc_unqual:" (ppCat (map ppPStr (keysFM tc_unqual))) $

    pprTrace "rnIfaces:dqual:"     (ppCat [ppBesides[ppPStr m,ppChar '.',ppPStr n] | (m,n) <- keysFM dqual]) $
    pprTrace "rnIfaces:dunqual:"   (ppCat (map ppPStr (keysFM dunqual))) $
    pprTrace "rnIfaces:dtc_qual:"  (ppCat [ppBesides[ppPStr m,ppChar '.',ppPStr n] | (m,n) <- keysFM dtc_qual]) $
    pprTrace "rnIfaces:dtc_unqual:"(ppCat (map ppPStr (keysFM dtc_unqual))) $
    -}
    let
	(us1,us2) = splitUniqSupply us
    in

    -- do transitive closure to bring in all needed names/defns:

    loop todo	      -- initial batch of names to process
	 (def_env, occ_env, us1) -- init stuff down
	 empty_return -- init acc results
	 >>= \ (((if_typedecls, if_classdecls, if_sigs),
		 if_implicits,
		 (if_errs, if_warns)),
		new_occ_env) ->

    -- go back and handle instance things:

    rnIfaceInstStuff iface_cache modname us2 new_occ_env if_implicits
	 >>= \ (if_instdecls, (ifi_errs, ifi_warns)) ->

    return (
	HsModule modname iface_version exports imports fixities
		 (typedecls ++ if_typedecls)
		 typesigs
		 (classdecls ++ if_classdecls)
		 (instdecls  ++ if_instdecls)
		 instsigs defdecls binds
		 (sigs ++ if_sigs)
		 src_loc,
	if_implicits,
	if_errs  `unionBags` ifi_errs,
	if_warns `unionBags` ifi_warns
    )
  where
    loop :: [RnName]	  -- Names we're looking for; we keep adding/deleting
			  -- from this list; we're done when empty (nothing
			  -- more needs to be looked for)
	 -> Go_Down	  -- see defn below
	 -> To_Return	  -- accumulated result
	 -> IO (To_Return, RnEnv{-final occurrence env; to pass on for doing instances-})

    loop to_find@[] down to_return = return (to_return, occenv down)

    loop to_find@(n:ns) down to_return 
      = case (lookup_defd down (origName n)) of
	  Just  _ -> -- previous processing must've found the stuff for this name;
		     -- continue with the rest:
		     -- pprTrace "loop:done:" (ppr PprDebug n) $
		     loop ns down to_return

	  Nothing -> -- OK, see what the cache has for us...

	    cachedDeclByType iface_cache n >>= \ maybe_ans ->
	    case maybe_ans of
	      Failed err -> -- add the error, but keep going:
			    -- pprTrace "loop:cache error:" (ppr PprDebug n) $
			    loop ns down (add_err err to_return)

	      Succeeded iface_decl -> -- something needing renaming!
		let
		    (us1, us2) = splitUniqSupply (uniqsupply down)
		in
		case (initRn False{-iface-} modname (occenv down) us1 (
			setExtraRn emptyUFM{-ignore fixities-} $
			rnIfaceDecl iface_decl)) of {
		  ((if_decl, if_defd, if_implicits), if_errs, if_warns) ->
		    let
			new_unknowns = eltsFM (fst if_implicits) ++ eltsFM (snd if_implicits)
		    in
--		    pprTrace "loop:renamed:" (ppAboves [ppr PprDebug n
--			, ppCat [ppStr "new unknowns:", interpp'SP PprDebug new_unknowns]
--			, ppCat [ppStr "defd vals:", interpp'SP PprDebug [n | (_,n) <- fst if_defd] ]
--			, ppCat [ppStr "defd  tcs:", interpp'SP PprDebug [n | (_,n) <- snd if_defd] ]
--			]) $
		    loop (new_unknowns ++ ns)
			 (add_occs       if_defd if_implicits $
			  new_uniqsupply us2 down)
			 (add_decl	 if_decl	$
			  add_implicits  if_implicits	$
			  add_errs	 if_errs	$
			  add_warns	 if_warns to_return)
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
  = (if isRdrLexCon n then lookupTcRnEnv else lookupRnEnv) def_env n

occenv	   (_, occ_env, _) = occ_env
uniqsupply (_, _,      us) = us

new_uniqsupply us (def_env, occ_env, _) = (def_env, occ_env, us)

add_occs (val_defds, tc_defds) (val_imps, tc_imps) (def_env, occ_env, us)
  = case (extendGlobalRnEnv def_env val_defds tc_defds) of { (new_def_env, def_dups) ->
    ASSERT(isEmptyBag def_dups)
    let
	val_occs = val_defds ++ fmToList val_imps
	tc_occs  = tc_defds  ++ fmToList tc_imps
    in
    case (extendGlobalRnEnv occ_env val_occs tc_occs)   of { (new_occ_env, occ_dups) ->

--  ASSERT(isEmptyBag occ_dups)
-- False because we may get a dup on the name we just shoved in

    (new_def_env, new_occ_env, us) }}

----------------
type To_Return = (([RenamedTyDecl], [RenamedClassDecl], [RenamedSig]),
		  ImplicitEnv,	-- new names used implicitly
		  (Bag Error, Bag Warning)
		 )

empty_return :: To_Return
empty_return = (([],[],[]), emptyImplicitEnv, (emptyBag,emptyBag))

add_decl decl ((tydecls, classdecls, sigs), implicit, msgs)
  = case decl of
      AddedTy	 t -> ((t:tydecls, classdecls, sigs), implicit, msgs)
      AddedClass c -> ((tydecls, c:classdecls, sigs), implicit, msgs)
      AddedSig	 s -> ((tydecls, classdecls, s:sigs), implicit, msgs)

add_implicits (val_imps, tc_imps) (decls, (val_fm, tc_fm), msgs)
  = (decls, (val_fm `plusFM` val_imps, tc_fm `plusFM`  tc_imps), msgs)
  where
    pairify rn = (origName rn, rn)

add_err  err (decls,implicit,(errs,warns)) = (decls,implicit,(errs `snocBag`   err,warns))
add_errs ers (decls,implicit,(errs,warns)) = (decls,implicit,(errs `unionBags` ers,warns))
add_warns ws (decls,implicit,(errs,warns)) = (decls,implicit,(errs, warns `unionBags` ws))
\end{code}

\begin{code}
data AddedDecl -- purely local
  = AddedTy	RenamedTyDecl
  | AddedClass	RenamedClassDecl
  | AddedSig	RenamedSig

rnIfaceDecl :: RdrIfaceDecl
	    -> RnM_Fixes _RealWorld
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

rnIfaceDecl (DataSig tc dcs _ decl)
  = rnTyDecl    decl		`thenRn` \ rn_decl   ->
    lookupTyCon tc		`thenRn` \ rn_tc     ->
    mapRn lookupValue dcs	`thenRn` \ rn_dcs    ->
    getImplicitUpRn		`thenRn` \ mentioned ->
    let
	defds = (dcs `zip` rn_dcs, [(tc, rn_tc)])
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
  = (delListFromFM val_ment (map fst val_defds),
     delListFromFM tc_ment  (map fst tc_defds))
\end{code}

% ------------------------------

@rnIfaceInstStuff@: Deal with instance declarations from interface files.

\begin{code}
rnIfaceInstStuff
	:: IfaceCache	-- all about ifaces we've read
	-> Module
	-> UniqSupply
	-> RnEnv
	-> ImplicitEnv	-- info about all names we've used
	-> IO ([RenamedInstDecl],
	       (Bag Error, Bag Warning))

rnIfaceInstStuff iface_cache modname us occ_env implicit_env
  = -- nearly all the instance decls we might even want
    -- to consider are in the ParsedIfaces that are in our
    -- cache; any *other* instances to consider are in any
    -- "instance modules" fields that we've encounted.
    -- Get both:

    readVar iface_cache	`thenPrimIO` \ (iface_fm, _) ->
    let
	ifaces_so_far	= eltsFM iface_fm
	all_iface_imods = unionManyBags (map get_ims   ifaces_so_far)
	insts_so_far	= unionManyBags (map get_insts ifaces_so_far)
    in
    -- OK, get all the instance decls out of the "instance module"
    -- modules:

    read_iface_imods iface_fm (bagToList all_iface_imods) emptyBag emptyBag{-accumulators-}
			>>= \ (more_insts, ims_errs) ->
    let
	all_insts = insts_so_far `unionBags` more_insts

	-- an instance decl can only be of interest if *both*
	-- its class and tycon have made their way into our
	-- purview:
	interesting_insts = filter (good_inst implicit_env) (bagToList all_insts)
    in
--    pprTrace "in implicit:\n"	    (ppCat (map (ppr PprDebug) (keysFM (snd implicit_env)))) $
--    pprTrace "insts_so_far:\n"      (ppr_insts (bagToList insts_so_far)) $
--    pprTrace "more_insts:\n"        (ppr_insts (bagToList more_insts)) $
--    pprTrace "interesting_insts:\n" (ppr_insts interesting_insts) $
    -- Do the renaming for real:
    --
    case (initRn False{-iface-} modname occ_env us (
	    setExtraRn emptyUFM{-ignore fixities-} $
	    mapRn rnIfaceInst interesting_insts)) of {
      (if_inst_decls, if_errs, if_warns) ->

	return (if_inst_decls, (ims_errs `unionBags` if_errs, if_warns))
    }
  where
    get_insts (ParsedIface _ _ _ _ _   _ _ _ _ insts _) = insts
    get_ims   (ParsedIface _ _ _ _ _ ims _ _ _     _ _) = ims

    good_inst (_, tc_imp_env) i@(InstSig clas tycon _ _)
      = -- it's a "good instance" (one to hang onto) if we have
	-- some chance of referring to *both* the class and tycon
	-- later on.
	mentionable clas && mentionable tycon
      where
	mentionable nm
	  = case (lookupFM tc_imp_env nm) of
	      Just  _ -> True
	      Nothing -> -- maybe it's builtin
		case nm of
		  Qual _ _ -> False
		  Unqual n ->
		    case (lookupFM b_tc_names n) of
		      Just  _ -> True
		      Nothing -> maybeToBool (lookupFM b_keys n)

    (b_tc_names, b_keys) -- pretty UGLY ...
      = case builtinNameInfo of ((_,builtin_tcs),b_keys,_) -> (builtin_tcs,b_keys)

    ppr_insts insts
      = ppAboves (map ppr_inst insts)
      where
	ppr_inst (InstSig c t _ inst_decl)
	  = ppCat [ppr PprDebug c, ppr PprDebug t, ppr PprDebug inst_decl]

    read_iface_imods :: ModuleToIfaceContents
		     -> [Module]
		     -> Bag RdrIfaceInst -> Bag Error
		     -> IO (Bag RdrIfaceInst, Bag Error)

    read_iface_imods iface_fm []     iacc eacc = return (iacc, eacc)
    read_iface_imods iface_fm (m:ms) iacc eacc
      = case (lookupFM iface_fm m) of
	  Just  _ -> -- module's already in our cache; keep going
		     read_iface_imods iface_fm ms iacc eacc

	  Nothing -> -- bring it in
	    cachedIface iface_cache m	>>= \ read_res ->
	    case read_res of
	      Failed msg -> -- oh well, keep going anyway (saving the error)
		read_iface_imods iface_fm ms iacc (eacc `snocBag` msg)

	      Succeeded iface ->
		read_iface_imods iface_fm ms (iacc `unionBags` get_insts iface) eacc
\end{code}

\begin{code}
rnIfaceInst :: RdrIfaceInst -> RnM_Fixes _RealWorld RenamedInstDecl

rnIfaceInst (InstSig _ _ _ inst_decl) = rnInstDecl inst_decl
\end{code}

\begin{code}
finalIfaceInfo ::
	   IfaceCache			-- iface cache
	-> [RnName]			-- all imported names required
	-> [Module]			-- directly imported modules
	-> IO (VersionInfo,		-- info about version numbers
	       [Module])		-- special instance modules

type VersionInfo = [(Module, Version, [(FAST_STRING, Version)])]

finalIfaceInfo iface_cache imps_reqd imp_mods
  = return ([], [])
\end{code}


\begin{code}
noIfaceErr mod sty
  = ppCat [ppPStr SLIT("Could not find interface for:"), ppPStr mod]

noDeclInIfaceErr mod str sty
  = ppBesides [ppPStr SLIT("Could not find interface declaration of: "),
	       ppPStr mod, ppStr ".", ppPStr str]

cannaeReadErr file err sty
  = ppBesides [ppPStr SLIT("Failed in reading file: "), ppStr file, ppStr "; error=", ppStr (show err)]

ifaceLookupWiredErr msg n sty
  = ppBesides [ppPStr SLIT("Why am I looking up a wired-in "), ppStr msg, ppChar ':', ppr sty n]

badIfaceLookupErr msg name decl sty
  = ppBesides [ppPStr SLIT("Expected a "), ppStr msg, ppPStr SLIT(" declaration, but got this: ???")]
\end{code}
