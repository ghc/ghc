%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[TcTyDecls]{Typecheck type declarations}

\begin{code}
#include "HsVersions.h"

module TcTyDecls (
	tcTyDecl,
	tcConDecl
    ) where

import Ubiq{-uitous-}

import HsSyn		( TyDecl(..), ConDecl(..), BangType(..), MonoType )
import RnHsSyn		( RenamedTyDecl(..), RenamedConDecl(..) )

import TcMonoType	( tcMonoTypeKind, tcMonoType, tcContext )
import TcEnv		( tcLookupTyCon, tcLookupTyVar, tcLookupClass )
import TcMonad
import TcKind		( TcKind, unifyKind, mkTcArrowKind, mkTcTypeKind )

import Id		( mkDataCon, StrictnessMark(..) )
import Kind		( Kind, mkArrowKind, mkBoxedTypeKind )
import SpecEnv		( SpecEnv(..), nullSpecEnv )
import Name		( getNameFullName, Name(..) )
import Pretty
import TyCon		( TyCon, ConsVisible(..), NewOrData(..), mkSynTyCon, mkDataTyCon )
import Type		( getTypeKind )
import TyVar		( getTyVarKind )
import Util		( panic )

\end{code}

\begin{code}
tcTyDecl :: RenamedTyDecl -> TcM s TyCon
\end{code}

Type synonym decls
~~~~~~~~~~~~~~~~~~

\begin{code}
tcTyDecl (TySynonym tycon_name tyvar_names rhs src_loc)
  = tcAddSrcLoc src_loc $
    tcAddErrCtxt (tySynCtxt tycon_name) $

	-- Look up the pieces
    tcLookupTyCon tycon_name			`thenNF_Tc` \ (tycon_kind,  _, rec_tycon) ->
    mapAndUnzipNF_Tc tcLookupTyVar tyvar_names	`thenNF_Tc` \ (tyvar_kinds, rec_tyvars) ->

	-- Look at the rhs
    tcMonoTypeKind rhs				`thenTc` \ (rhs_kind, rhs_ty) ->

	-- Unify tycon kind with (k1->...->kn->rhs)
    unifyKind tycon_kind
	(foldr mkTcArrowKind rhs_kind tyvar_kinds)
						`thenTc_`
    let
	-- Construct the tycon
	result_kind, final_tycon_kind :: Kind 	-- NB not TcKind!
	result_kind      = getTypeKind rhs_ty
	final_tycon_kind = foldr (mkArrowKind . getTyVarKind) result_kind rec_tyvars

	tycon = mkSynTyCon (getItsUnique tycon_name)
			   (getNameFullName tycon_name)
			   final_tycon_kind
			   (length tyvar_names)
			   rec_tyvars
			   rhs_ty
    in
    returnTc tycon
\end{code}

Algebraic data and newtype decls
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
tcTyDecl (TyData context tycon_name tyvar_names con_decls derivings pragmas src_loc)
  = tcTyDataOrNew DataType context tycon_name tyvar_names con_decls derivings pragmas src_loc

tcTyDecl (TyNew context tycon_name tyvar_names con_decl derivings pragmas src_loc)
  = tcTyDataOrNew NewType  context tycon_name tyvar_names con_decl  derivings pragmas src_loc


tcTyDataOrNew data_or_new context tycon_name tyvar_names con_decls derivings pragmas src_loc
  = tcAddSrcLoc src_loc $
    tcAddErrCtxt (tyDataCtxt tycon_name) $

	-- Lookup the pieces
    tcLookupTyCon tycon_name			`thenNF_Tc` \ (tycon_kind, _, rec_tycon) ->
    mapAndUnzipNF_Tc tcLookupTyVar tyvar_names	`thenNF_Tc` \ (tyvar_kinds, rec_tyvars) ->
    tc_derivs derivings				`thenNF_Tc` \ derived_classes ->

	-- Typecheck the context
    tcContext context				`thenTc` \ ctxt ->

	-- Unify tycon kind with (k1->...->kn->Type)
    unifyKind tycon_kind
	(foldr mkTcArrowKind mkTcTypeKind tyvar_kinds)
						`thenTc_`
	-- Walk the condecls
    mapTc (tcConDecl rec_tycon rec_tyvars ctxt) con_decls
						`thenTc` \ con_ids ->
    let
	-- Construct the tycon
	final_tycon_kind :: Kind 		-- NB not TcKind!
	final_tycon_kind = foldr (mkArrowKind . getTyVarKind) mkBoxedTypeKind rec_tyvars

	tycon = mkDataTyCon (getItsUnique tycon_name)
			    final_tycon_kind
			    (getNameFullName tycon_name)
			    rec_tyvars
			    ctxt
			    con_ids
			    derived_classes
			    ConsVisible		-- For now; if constrs are from pragma we are *abstract*
			    data_or_new
    in
    returnTc tycon
  where
    tc_derivs Nothing   = returnNF_Tc []
    tc_derivs (Just ds) = mapNF_Tc tc_deriv ds

    tc_deriv name
      = tcLookupClass name `thenNF_Tc` \ (_, clas) ->
	returnNF_Tc clas
\end{code}


Constructors
~~~~~~~~~~~~
\begin{code}
tcConDecl :: TyCon -> [TyVar] -> [(Class,Type)] -> RenamedConDecl -> TcM s Id

tcConDecl tycon tyvars ctxt (ConDecl name btys src_loc)
  = tcAddSrcLoc src_loc	$
    let
	(stricts, tys) = sep_bangs btys
    in
    mapTc tcMonoType tys `thenTc` \ arg_tys ->
    let
      data_con = mkDataCon (getItsUnique name)
			   (getNameFullName name)
			   stricts
		      	   tyvars
		      	   [] -- ToDo: ctxt; limited to tyvars in arg_tys
		      	   arg_tys
		      	   tycon
			-- nullSpecEnv
    in
    returnTc data_con

tcConDecl tycon tyvars ctxt (ConOpDecl bty1 op bty2 src_loc)
  = tcAddSrcLoc src_loc	$
    let
	(stricts, tys) = sep_bangs [bty1, bty2]
    in
    mapTc tcMonoType tys `thenTc` \ arg_tys ->
    let
      data_con = mkDataCon (getItsUnique op)
			   (getNameFullName op)
			   stricts
		      	   tyvars
		      	   [] -- ToDo: ctxt
		      	   arg_tys
		      	   tycon
			-- nullSpecEnv
    in
    returnTc data_con

tcConDecl tycon tyvars ctxt (NewConDecl name ty src_loc)
  = tcAddSrcLoc src_loc	$
    tcMonoType ty `thenTc` \ arg_ty ->
    let
      data_con = mkDataCon (getItsUnique name)
			   (getNameFullName name)
			   [NotMarkedStrict]
		      	   tyvars
		      	   [] -- ToDo: ctxt
		      	   [arg_ty]
		      	   tycon
			-- nullSpecEnv
    in
    returnTc data_con

tcConDecl tycon tyvars ctxt (RecConDecl con fields src_loc)
  = panic "tcConDecls:RecConDecl"


sep_bangs btys
  = unzip (map sep_bang btys)
  where 
    sep_bang (Banged ty)   = (MarkedStrict, ty)
    sep_bang (Unbanged ty) = (NotMarkedStrict, ty)
\end{code}



Errors and contexts
~~~~~~~~~~~~~~~~~~~
\begin{code}
tySynCtxt tycon_name sty
  = ppCat [ppStr "In the type declaration for", ppr sty tycon_name]

tyDataCtxt tycon_name sty
  = ppCat [ppStr "In the data declaration for", ppr sty tycon_name]

tyNewCtxt tycon_name sty
  = ppCat [ppStr "In the newtype declaration for", ppr sty tycon_name]
\end{code}
