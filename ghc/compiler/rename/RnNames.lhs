%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnNames]{Extracting imported and top-level names in scope}

\begin{code}
#include "HsVersions.h"

module RnNames (
	getGlobalNames,
	GlobalNameInfo(..)
    ) where

import PreludeGlaST	( returnPrimIO, thenPrimIO, MutableVar(..) )

import Ubiq

import HsSyn
import RdrHsSyn
import RnHsSyn

import RnMonad
import RnIfaces		( IfaceCache(..), cacheInterface, ParsedIface )
import RnUtils		( RnEnv(..), emptyRnEnv, extendGlobalRnEnv, qualNameErr, dupNamesErr )

import Bag		( emptyBag, unitBag, unionBags, unionManyBags, mapBag, listToBag, bagToList )
import ErrUtils		( Error(..), Warning(..), addShortErrLocLine )
import FiniteMap	( fmToList )
import Name		( RdrName(..), Name, isQual, mkTopLevName,
			  mkImportedName, nameExportFlag,
			  getLocalName, getSrcLoc, pprNonOp
			)
import PrelInfo		( BuiltinNames(..), BuiltinKeys(..) )
import PrelMods		( fromPrelude )
import Pretty
import SrcLoc		( SrcLoc )
import UniqSupply	( splitUniqSupply )
import Util		( equivClasses, panic )
\end{code}


\begin{code}
type GlobalNameInfo = (BuiltinNames,
		       BuiltinKeys,
		       Name -> ExportFlag,
		       Name -> [RdrName])

type RnM_Info s r = RnMonad GlobalNameInfo s r

getGlobalNames ::
	   IfaceCache		
	-> GlobalNameInfo	
	-> UniqSupply
	-> RdrNameHsModule
	-> PrimIO (RnEnv,
		   [Module],
		   Bag RenamedFixityDecl,
		   Bag Error,
		   Bag Warning)

getGlobalNames iface_var info us
	       (HsModule mod _ _ imports _ ty_decls _ cls_decls _ _ _ binds _ _)
  = case initRn True mod emptyRnEnv us1 
		(setExtraRn info $
		 getSourceNames ty_decls cls_decls binds)
    of { ((src_vals, src_tcs), src_errs, src_warns) ->

    getImportedNames iface_var info us2 imports	`thenPrimIO`
	\ (imp_vals, imp_tcs, imp_mods, imp_fixes, imp_errs, imp_warns) ->

    let
        unqual_vals = mapBag (\rn -> (Unqual (getLocalName rn), rn)) src_vals
        unqual_tcs  = mapBag (\rn -> (Unqual (getLocalName rn), rn)) src_tcs

	all_vals = bagToList (unqual_vals `unionBags` imp_vals)
	all_tcs  = bagToList (unqual_tcs  `unionBags` imp_tcs)

        (all_env, dups) = extendGlobalRnEnv emptyRnEnv all_vals all_tcs

	dup_errs = map dup_err (equivClasses cmp_rdr (bagToList dups))
	cmp_rdr (rdr1,_,_) (rdr2,_,_) = cmp rdr1 rdr2
	dup_err ((rdr,rn,rn'):rest) = globalDupNamesErr rdr (rn:rn': [rn|(_,_,rn)<-rest])

	all_errs  = src_errs `unionBags` imp_errs `unionBags` listToBag dup_errs
	all_warns = src_warns `unionBags` imp_warns
    in
    returnPrimIO (all_env, bagToList imp_mods, imp_fixes, all_errs, all_warns)
    }
  where
    (us1, us2) = splitUniqSupply us
\end{code}

*********************************************************
*							*
\subsection{Top-level source names}
*							*
*********************************************************

\begin{code}
getSourceNames ::
	   [RdrNameTyDecl]
	-> [RdrNameClassDecl]
	-> RdrNameHsBinds
	-> RnM_Info s (Bag RnName,	-- values
		       Bag RnName)	-- tycons/classes

getSourceNames ty_decls cls_decls binds
  = mapAndUnzipRn getTyDeclNames ty_decls   `thenRn` \ (tycon_s, constrs_s) ->
    mapAndUnzipRn getClassNames cls_decls  `thenRn` \ (cls_s, cls_ops_s) ->
    getTopBindsNames binds			   `thenRn` \ bind_names ->
    returnRn (unionManyBags constrs_s `unionBags`
	      unionManyBags cls_ops_s `unionBags` bind_names,
	      listToBag tycon_s `unionBags` listToBag cls_s)


getTyDeclNames :: RdrNameTyDecl
	       -> RnM_Info s (RnName, Bag RnName)	-- tycon and constrs

getTyDeclNames (TyData _ tycon _ condecls _ _ src_loc)
  = newGlobalName src_loc Nothing tycon	`thenRn` \ tycon_name ->
    mapRn (getConDeclName (Just (nameExportFlag tycon_name)))
			       condecls	`thenRn` \ con_names ->
    returnRn (RnData tycon_name con_names,
	      listToBag (map (\ n -> RnConstr n tycon_name) con_names))

getTyDeclNames (TyNew _ tycon _ condecls _ _ src_loc)
  = newGlobalName src_loc Nothing tycon	`thenRn` \ tycon_name ->
    mapRn (getConDeclName (Just (nameExportFlag tycon_name)))
			       condecls	`thenRn` \ con_names ->
    returnRn (RnData tycon_name con_names,
	      listToBag (map (\ n -> RnConstr n tycon_name) con_names))

getTyDeclNames (TySynonym tycon _ _ src_loc)
  = newGlobalName src_loc Nothing tycon	`thenRn` \ tycon_name ->
    returnRn (RnSyn tycon_name, emptyBag)

getConDeclName exp (ConDecl con _ src_loc)
  = newGlobalName src_loc exp con
getConDeclName exp (ConOpDecl _ op _ src_loc)
  = newGlobalName src_loc exp op
getConDeclName exp (NewConDecl con _ src_loc)
  = newGlobalName src_loc exp con
getConDeclName exp (RecConDecl con fields src_loc)
  = panic "getConDeclName:RecConDecl"
    newGlobalName src_loc exp con


getClassNames :: RdrNameClassDecl
	      -> RnM_Info s (RnName, Bag RnName)	-- class and class ops

getClassNames (ClassDecl _ cname _ sigs _ _ src_loc)
  = newGlobalName src_loc Nothing cname	`thenRn` \ class_name ->
    getClassOpNames (Just (nameExportFlag class_name))
				  sigs	`thenRn` \ op_names ->
    returnRn (RnClass class_name op_names,
	      listToBag (map (\ n -> RnClassOp n class_name) op_names))

getClassOpNames exp []
  = returnRn []
getClassOpNames exp (ClassOpSig op _ _ src_loc : sigs)
  = newGlobalName src_loc exp op `thenRn` \ op_name ->
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
doMBinds (FunMonoBind p_name _ locn) 	        = doName locn p_name
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

doField locn (field, _, True{-pun-}) = doName locn field
doField locn (field, pat, _)	     = doPat locn pat

doName locn rdr
  = newGlobalName locn Nothing rdr `thenRn` \ name ->
    returnRn (unitBag (RnName name))
\end{code}

*********************************************************
*							*
\subsection{Creating a new global name}
*							*
*********************************************************

\begin{code}
newGlobalName :: SrcLoc -> Maybe ExportFlag
	      -> RdrName -> RnM_Info s Name

newGlobalName locn maybe_exp rdr
  = getExtraRn			`thenRn` \ (_,_,exp_fn,occ_fn) ->
    getModuleRn  		`thenRn` \ mod ->
    getSourceRn			`thenRn` \ source -> 
    rnGetUnique 		`thenRn` \ u ->
    let
        src_unqual = getLocalName rdr

	src_orig   = if fromPrelude mod
	             then (Unqual src_unqual)
	             else (Qual mod src_unqual)

	exp = case maybe_exp of
	       Just exp -> exp
	       Nothing  -> exp_fn n

	n = if source then
	        mkTopLevName u src_orig locn exp (occ_fn n)
	    else
		mkImportedName u rdr locn exp (occ_fn n)
    in
    addErrIfRn (source && isQual rdr)
	       (qualNameErr "name in definition" (rdr, locn)) `thenRn_`
    returnRn n    
\end{code}

*********************************************************
*							*
\subsection{Imported names}
*							*
*********************************************************

\begin{code}
getImportedNames ::
	   IfaceCache
	-> GlobalNameInfo			-- builtin and knot name info
	-> UniqSupply
	-> [RdrNameImportDecl]			-- import declarations
	-> PrimIO (Bag (RdrName,RnName),	-- imported values in scope
		   Bag (RdrName,RnName),	-- imported tycons/classes in scope
		   Bag Module,			-- directly imported modules
		   Bag RenamedFixityDecl,	-- fixity info for imported names
		   Bag Error,
		   Bag Warning)

getImportedNames iface_var info us imports 
  = returnPrimIO (builtin_vals, builtin_tcs, emptyBag, emptyBag, emptyBag, emptyBag)
  where
    -- For now jsut add the builtin names ...
    (b_names,_,_,_) = info
    builtin_vals = listToBag [(Unqual s, rn) | (s,rn) <- fmToList b_names, not (isRnTyCon rn)]
    builtin_tcs  = listToBag [(Unqual s, rn) | (s,rn) <- fmToList b_names, isRnTyCon rn]
\end{code}


\begin{code}
globalDupNamesErr rdr rns sty
  = ppHang (ppBesides [pprNonOp sty rdr, ppStr " multiply defined:"])
	 4 (ppAboves (map pp_def rns))
  where
    pp_def rn = addShortErrLocLine (getSrcLoc rn) (\ sty -> ppr sty rn) sty

    -- ToDo: print import src locs for imported names
\end{code}
