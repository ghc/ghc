%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcTyDecls]{Typecheck algebraic datatypes and type synonyms}

\begin{code}
#include "HsVersions.h"

module TcTyDecls ( tcTyDecls ) where

import TcMonad		-- typechecking monad machinery
import AbsSyn		-- the stuff being typechecked

import AbsUniType	( applyTyCon, mkDataTyCon, mkSynonymTyCon,
			  getUniDataTyCon, isUnboxedDataType,
			  isTyVarTemplateTy, cmpUniTypeMaybeList,
			  pprMaybeTy
			)
import CE		( lookupCE, CE(..) )
import CmdLineOpts	( GlobalSwitch(..) )
import E		( getE_TCE, getE_CE, plusGVE, nullGVE, GVE(..), E )
import ErrUtils		( addShortErrLocLine )
import Errors		( confusedNameErr, specDataNoSpecErr, specDataUnboxedErr )
import FiniteMap	( FiniteMap, emptyFM, plusFM, singletonFM )
import IdInfo		( SpecEnv, mkSpecEnv, SpecInfo(..) )
import Pretty
import SpecTyFuns	( specialiseConstrTys )
import TCE		-- ( nullTCE, unitTCE, lookupTCE, plusTCE, TCE(..), UniqFM )
import TVE		( mkTVE, TVE(..) )
import TcConDecls	( tcConDecls )
import TcMonoType	( tcMonoType )
import TcPragmas	( tcDataPragmas, tcTypePragmas )
import Util
\end{code}

We consult the @CE@/@TCE@ arguments {\em only} to build knots!

The resulting @TCE@ has info about the type constructors in it; the
@GVE@ has info about their data constructors.

\begin{code}
tcTyDecls :: E
	  -> (Name -> Bool)			-- given Name, is it an abstract synonym?
	  -> (Name -> [RenamedDataTypeSig])	-- given Name, get specialisation pragmas
	  -> [RenamedTyDecl]
	  -> Baby_TcM (TCE, GVE, 
		       FiniteMap TyCon [(Bool, [Maybe UniType])])
						-- specialisations:
						--   True  => imported data types i.e. from interface file
						--   False => local data types i.e. requsted by source pragmas

tcTyDecls e _ _ [] = returnB_Tc (nullTCE, nullGVE, emptyFM)

tcTyDecls e is_abs_syn get_spec_sigs (tyd: tyds)
  = tc_decl   tyd	    `thenB_Tc` \ (tce1, gve1, specs1) ->
    tcTyDecls e is_abs_syn get_spec_sigs tyds
			    `thenB_Tc` \ (tce2, gve2, specs2) ->
    let
	tce3   = tce1 `plusTCE` tce2
	gve3   = gve1 `plusGVE` gve2
	specs3 = specs1 `plusFM` specs2
    in
    returnB_Tc (tce3, gve3, specs3)
  where
    rec_ce  = getE_CE  e
    rec_tce = getE_TCE e

    -- continued...
\end{code}

We don't need to substitute here, because the @TCE@s
(which are at the top level) cannot contain free type variables.

Gather relevant info:
\begin{code}
    tc_decl (TyData context name@(PreludeTyCon uniq full_name arity True{-"data"-})
		    tyvars con_decls derivings pragmas src_loc)
			    -- ToDo: context
      = tc_data_decl uniq name full_name arity tyvars con_decls
		     derivings pragmas src_loc

    tc_decl (TyData context name@(OtherTyCon uniq full_name arity True{-"data"-} _)
		    tyvars con_decls derivings pragmas src_loc)
			    -- ToDo: context
      = tc_data_decl uniq name full_name arity tyvars con_decls
		     derivings pragmas src_loc

    tc_decl (TyData _ bad_name _ _ _ _ src_loc)
      = failB_Tc (confusedNameErr "Bad name on a datatype constructor (a Prelude name?)"
		    bad_name src_loc)

    tc_decl (TySynonym name@(PreludeTyCon uniq full_name arity False{-"type"-})
			tyvars mono_ty pragmas src_loc)
      = tc_syn_decl uniq name full_name arity tyvars mono_ty pragmas src_loc

    tc_decl (TySynonym name@(OtherTyCon uniq full_name arity False{-"type"-} _)
			tyvars mono_ty pragmas src_loc)
      = tc_syn_decl uniq name full_name arity tyvars mono_ty pragmas src_loc

    tc_decl (TySynonym bad_name _ _ _ src_loc)
      = failB_Tc (confusedNameErr "Bad name on a type-synonym constructor (a Prelude name?)"
		    bad_name src_loc)
\end{code}

Real work for @data@ declarations:
\begin{code}
    tc_data_decl uniq name full_name arity tyvars con_decls derivings pragmas src_loc
      = addSrcLocB_Tc src_loc (
	let
	    (tve, new_tyvars, _) = mkTVE tyvars
	    rec_tycon		 = lookupTCE rec_tce name
		-- We know the lookup will succeed, because we are just
		-- about to put it in the outgoing TCE!

	    spec_sigs = get_spec_sigs name
	in
	tcSpecDataSigs rec_tce spec_sigs []	`thenB_Tc` \ user_spec_infos ->

	recoverIgnoreErrorsB_Tc ([], []) (
	    tcDataPragmas rec_tce tve rec_tycon new_tyvars pragmas
	)		`thenB_Tc` \ (pragma_con_decls, pragma_spec_infos) ->
	let
	    (condecls_to_use, ignore_condecl_errors_if_pragma)
	      = if null pragma_con_decls then
	            (con_decls, id)
	        else
		    if null con_decls
		    then (pragma_con_decls, recoverIgnoreErrorsB_Tc nullGVE)
		    else panic "tcTyDecls:data: user and pragma condecls!"

	    (imported_specs, specinfos_to_use)
	      = if null pragma_spec_infos then
		    (False, user_spec_infos)
		else
		    if null user_spec_infos
		    then (True, pragma_spec_infos)
		    else panic "tcTyDecls:data: user and pragma specinfos!"

	    specenv_to_use = mkSpecEnv specinfos_to_use
	in
	ignore_condecl_errors_if_pragma
	(tcConDecls rec_tce tve rec_tycon new_tyvars specenv_to_use condecls_to_use)
							`thenB_Tc` \ gve ->
	let
	    condecls = map snd gve

	    derived_classes = map (lookupCE rec_ce) derivings

	    new_tycon
	      = mkDataTyCon uniq
			    full_name arity new_tyvars condecls
			    derived_classes
			    (null pragma_con_decls)
			    -- if constrs are from pragma we are *abstract*

	    spec_list
	      = [(imported_specs, maybe_tys) | (SpecInfo maybe_tys _ _) <- specinfos_to_use]

	    spec_map
	      = if null spec_list then
		    emptyFM
		else
		    singletonFM rec_tycon spec_list
	in
	returnB_Tc (unitTCE uniq new_tycon, gve, spec_map)
	    -- It's OK to return pragma condecls in gve, even
	    -- though some of those names should be "invisible",
	    -- because the *renamer* is supposed to have dealt with
	    -- naming/scope issues already.
	)
\end{code}

Real work for @type@ (synonym) declarations:
\begin{code}
    tc_syn_decl uniq name full_name arity tyvars mono_ty pragmas src_loc
      = addSrcLocB_Tc src_loc (

	let (tve, new_tyvars, _) = mkTVE tyvars
	in
	tcMonoType rec_ce rec_tce tve mono_ty	`thenB_Tc` \ expansion ->
	let
	    -- abstractness info either comes from the interface pragmas
	    -- (tcTypePragmas) or from a user-pragma in this module
	    -- (is_abs_syn)
	    abstract = tcTypePragmas pragmas
		    || is_abs_syn name

	    new_tycon = mkSynonymTyCon uniq full_name
			    arity new_tyvars expansion (not abstract)
	in
	returnB_Tc (unitTCE uniq new_tycon, nullGVE, emptyFM)
	)
\end{code}

%************************************************************************
%*									*
\subsection{Specialisation Signatures for Data Type declarations}
%*									*
%************************************************************************

@tcSpecDataSigs@ checks data type specialisation signatures for
validity, and returns the list of specialisation requests.

\begin{code}
tcSpecDataSigs :: TCE
	       -> [RenamedDataTypeSig]
	       -> [(RenamedDataTypeSig,SpecInfo)]
	       -> Baby_TcM [SpecInfo]

tcSpecDataSigs tce (s:ss) accum
  = tc_sig s			`thenB_Tc` \ info  ->
    tcSpecDataSigs tce ss ((s,info):accum)
  where
    tc_sig (SpecDataSig n ty src_loc)
      = addSrcLocB_Tc src_loc (
	let 
	    ty_names  = extractMonoTyNames (==) ty
	    (tve,_,_) = mkTVE ty_names
	    fake_CE   = panic "tcSpecDataSigs:CE"
	in
	    -- Typecheck specialising type (includes arity check)
	tcMonoType fake_CE tce tve ty			`thenB_Tc` \ tau_ty ->
	let
	    (_,ty_args,_) = getUniDataTyCon tau_ty
	    is_unboxed_or_tyvar ty = isUnboxedDataType ty || isTyVarTemplateTy ty
	in
	    -- Check at least one unboxed type in specialisation
	checkB_Tc (not (any isUnboxedDataType ty_args))
		  (specDataNoSpecErr n ty_args src_loc) `thenB_Tc_`

	    -- Check all types are unboxed or tyvars
	    -- (specific boxed types are redundant)
	checkB_Tc (not (all is_unboxed_or_tyvar ty_args))
		  (specDataUnboxedErr n ty_args src_loc) `thenB_Tc_`

	let
	    maybe_tys     = specialiseConstrTys ty_args
	in
	returnB_Tc (SpecInfo maybe_tys 0 (panic "SpecData:SpecInfo:SpecId"))
	)

tcSpecDataSigs tce [] accum
  = -- Remove any duplicates from accumulated specinfos
    getSwitchCheckerB_Tc		`thenB_Tc` \ sw_chkr ->
    
    (if sw_chkr SpecialiseTrace && not (null duplicates) then
	 pprTrace "Duplicate SPECIALIZE data pragmas:\n"
	          (ppAboves (map specmsg sep_dups))
     else id)(

    (if sw_chkr SpecialiseTrace && not (null spec_infos) then
	 pprTrace "Specialising "
	          (ppHang (ppCat [ppr PprDebug name, ppStr "at types:"])
			4 (ppAboves (map pp_spec spec_infos)))

    else id) (

    returnB_Tc (spec_infos)
    ))
  where
    spec_infos = map (snd . head) equiv

    equiv      = equivClasses cmp_info accum
    duplicates = filter (not . singleton) equiv

    cmp_info (_, SpecInfo tys1 _ _) (_, SpecInfo tys2 _ _)
      = cmpUniTypeMaybeList tys1 tys2

    singleton [_] = True
    singleton _   = False

    sep_dups = tail (concat (map ((:) Nothing . map Just) duplicates))
    specmsg (Just (SpecDataSig _ ty locn, _))
      = addShortErrLocLine locn ( \ sty -> ppr sty ty ) PprDebug
    specmsg Nothing
      = ppStr "***"

    ((SpecDataSig name _ _, _):_) = accum    
    pp_spec (SpecInfo tys _ _) = ppInterleave ppNil [pprMaybeTy PprDebug ty | ty <- tys]
\end{code}
