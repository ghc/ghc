%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcTyClsDecls]{Typecheck type and class declarations}

\begin{code}
module TcTyClsDecls (
	tcTyAndClassDecls
    ) where

#include "HsVersions.h"

import HsSyn		( HsDecl(..), TyClDecl(..),
			  HsType(..), HsTyVarBndr,
			  ConDecl(..), ConDetails(..), 
			  Sig(..), HsPred(..), HsTupCon(..),
			  tyClDeclName, hsTyVarNames, isClassDecl, isSynDecl, isClassOpSig, getBangType
			)
import RnHsSyn		( RenamedHsDecl, RenamedTyClDecl, listTyCon_name )
import BasicTypes	( RecFlag(..), NewOrData(..) )

import TcMonad
import TcEnv		( ValueEnv, TyThing(..), TyThingDetails(..), tyThingKind,
			  tcExtendTypeEnv, tcExtendKindEnv, tcLookupTy
			)
import TcTyDecls	( tcTyDecl1, kcConDetails, mkNewTyConRep )
import TcClassDcl	( tcClassDecl1 )
import TcMonoType	( kcHsTyVars, kcHsType, kcHsBoxedSigType, kcHsContext, mkTyClTyVars )
import TcType		( TcKind, newKindVar, zonkKindEnv )

import TcUnify		( unifyKind )
import Type		( Kind, mkArrowKind, boxedTypeKind, zipFunTys )
import Variance         ( calcTyConArgVrcs )
import Class		( Class, mkClass, classTyCon )
import TyCon		( TyCon, ArgVrcs, AlgTyConFlavour(..), mkSynTyCon, mkAlgTyCon, mkClassTyCon )
import DataCon		( isNullaryDataCon )
import Var		( varName )
import FiniteMap
import Digraph		( stronglyConnComp, SCC(..) )
import Name		( Name, NamedThing(..), NameEnv, getSrcLoc, isTvOcc, nameOccName,
			  mkNameEnv, lookupNameEnv_NF
			)
import Outputable
import Maybes		( mapMaybe, catMaybes )
import UniqSet		( emptyUniqSet, unitUniqSet, unionUniqSets, 
			  unionManyUniqSets, uniqSetToList ) 
import ErrUtils		( Message )
import Unique		( Unique, Uniquable(..) )
\end{code}


%************************************************************************
%*									*
\subsection{Type checking for type and class declarations}
%*									*
%************************************************************************

The main function
~~~~~~~~~~~~~~~~~
\begin{code}
tcTyAndClassDecls :: ValueEnv		-- Knot tying stuff
		  -> [RenamedHsDecl]
		  -> TcM s TcEnv

tcTyAndClassDecls unf_env decls
  = sortByDependency decls 		`thenTc` \ groups ->
    tcGroups unf_env groups

tcGroups unf_env []
  = tcGetEnv	`thenNF_Tc` \ env ->
    returnTc env

tcGroups unf_env (group:groups)
  = tcGroup unf_env group	`thenTc` \ env ->
    tcSetEnv env			$
    tcGroups unf_env groups
\end{code}

Dealing with a group
~~~~~~~~~~~~~~~~~~~~

Consider a mutually-recursive group, binding 
a type constructor T and a class C.

Step 1: 	getInitialKind
	Construct a KindEnv by binding T and C to a kind variable 

Step 2: 	kcTyClDecl
	In that environment, do a kind check

Step 3: Zonk the kinds

Step 4: 	buildTyConOrClass
	Construct an environment binding T to a TyCon and C to a Class.
	a) Their kinds comes from zonking the relevant kind variable
	b) Their arity (for synonyms) comes direct from the decl
	c) The funcional dependencies come from the decl
	d) The rest comes a knot-tied binding of T and C, returned from Step 4
	e) The variances of the tycons in the group is calculated from 
		the knot-tied stuff

Step 5: 	tcTyClDecl1
	In this environment, walk over the decls, constructing the TyCons and Classes.
	This uses in a strict way items (a)-(c) above, which is why they must
	be constructed in Step 4.
	Feed the results back to Step 4.
	
The knot-tying parameters: @rec_details_list@ is an alist mapping @Name@s to
@TyThing@s.  @rec_vrcs@ is a finite map from @Name@s to @ArgVrcs@s.

\begin{code}
tcGroup :: ValueEnv -> SCC RenamedTyClDecl -> TcM s TcEnv
tcGroup unf_env scc
  = 	-- Step 1
    mapNF_Tc getInitialKind decls 				`thenNF_Tc` \ initial_kinds ->

	-- Step 2
    tcExtendKindEnv initial_kinds (mapTc kcTyClDecl decls)	`thenTc_`

	-- Step 3
    zonkKindEnv initial_kinds			`thenNF_Tc` \ final_kinds ->

	-- Tie the knot
    fixTc ( \ ~(rec_details_list,  _) ->
		-- Step 4 
 	let
	    kind_env    = mkNameEnv final_kinds
	    rec_details = mkNameEnv rec_details_list

	    tyclss, all_tyclss :: [(Name, TyThing)]
	    tyclss      = map (buildTyConOrClass is_rec kind_env rec_vrcs rec_details) decls

		-- Add the tycons that come from the classes
		-- We want them in the environment because 
		-- they are mentioned in interface files
	    all_tyclss  = [ (getName tycon, ATyCon tycon) | (_, AClass clas) <- tyclss,
							    let tycon = classTyCon clas
			  ] ++ tyclss

		-- Calculate variances, and (yes!) feed back into buildTyConOrClass.
            rec_vrcs    = calcTyConArgVrcs [tc | (_, ATyCon tc) <- all_tyclss]
	in
		-- Step 5
	tcExtendTypeEnv all_tyclss		$
	mapTc (tcTyClDecl1 unf_env) decls	`thenTc` \ tycls_details ->
	tcGetEnv				`thenNF_Tc` \ env -> 
	returnTc (tycls_details, env)
    )								`thenTc` \ (_, env) ->
    returnTc env
  where
    is_rec = case scc of
		AcyclicSCC _ -> NonRecursive
		CyclicSCC _  -> Recursive

    decls = case scc of
		AcyclicSCC decl -> [decl]
		CyclicSCC decls -> decls

tcTyClDecl1  :: ValueEnv -> RenamedTyClDecl -> TcM s (Name, TyThingDetails)

tcTyClDecl1 unf_env decl
  = tcAddDeclCtxt decl			$
    if isClassDecl decl then
	tcClassDecl1 unf_env decl
    else
	tcTyDecl1 decl
\end{code}


%************************************************************************
%*									*
\subsection{Step 1: Initial environment}
%*									*
%************************************************************************

\begin{code}
getInitialKind :: RenamedTyClDecl -> NF_TcM s (Name, TcKind)
getInitialKind (TySynonym name tyvars _ _)
 = kcHsTyVars tyvars	`thenNF_Tc` \ arg_kinds ->
   newKindVar		`thenNF_Tc` \ result_kind  ->
   returnNF_Tc (name, mk_kind arg_kinds result_kind)

getInitialKind (TyData _ _ name tyvars _ _ _ _ _)
 = kcHsTyVars tyvars	`thenNF_Tc` \ arg_kinds ->
   returnNF_Tc (name, mk_kind arg_kinds boxedTypeKind)

getInitialKind (ClassDecl _ name tyvars _ _ _ _ _ _ _ _ _)
 = kcHsTyVars tyvars	`thenNF_Tc` \ arg_kinds ->
   returnNF_Tc (name, mk_kind arg_kinds boxedTypeKind)

mk_kind tvs_w_kinds res_kind = foldr (mkArrowKind . snd) res_kind tvs_w_kinds
\end{code}


%************************************************************************
%*									*
\subsection{Step 2: Kind checking}
%*									*
%************************************************************************

We need to kind check all types in the mutually recursive group
before we know the kind of the type variables.  For example:

class C a where
   op :: D b => a -> b -> b

class D c where
   bop :: (Monad c) => ...

Here, the kind of the locally-polymorphic type variable "b"
depends on *all the uses of class D*.  For example, the use of
Monad c in bop's type signature means that D must have kind Type->Type.

\begin{code}
kcTyClDecl :: RenamedTyClDecl -> TcM s ()

kcTyClDecl decl@(TySynonym tycon_name hs_tyvars rhs loc)
  = tcAddDeclCtxt decl			$
    kcTyClDeclBody tycon_name hs_tyvars	$ \ result_kind ->
    kcHsType rhs			`thenTc` \ rhs_kind ->
    unifyKind result_kind rhs_kind

kcTyClDecl decl@(TyData _ context tycon_name hs_tyvars con_decls _ _ _ loc)
  = tcAddDeclCtxt decl			$
    kcTyClDeclBody tycon_name hs_tyvars	$ \ result_kind ->
    kcHsContext context			`thenTc_` 
    mapTc_ kc_con_decl con_decls
  where
    kc_con_decl (ConDecl _ _ ex_tvs ex_ctxt details loc)
      = tcAddSrcLoc loc			$
	kcHsTyVars ex_tvs		`thenNF_Tc` \ kind_env ->
	tcExtendKindEnv kind_env	$
	kcConDetails ex_ctxt details

kcTyClDecl decl@(ClassDecl context class_name
			   hs_tyvars fundeps class_sigs
		      	   _ _ _ _ _ _ loc)
  = tcAddDeclCtxt decl			$
    kcTyClDeclBody class_name hs_tyvars	$ \ result_kind ->
    kcHsContext context			`thenTc_`
    mapTc_ kc_sig (filter isClassOpSig class_sigs)
  where
    kc_sig (ClassOpSig _ _ op_ty loc) = tcAddSrcLoc loc (kcHsBoxedSigType op_ty)

kcTyClDeclBody :: Name -> [HsTyVarBndr Name] 	-- Kind of the tycon/cls and its tyvars
	       -> (Kind -> TcM s a)		-- Thing inside
	       -> TcM s a
-- Extend the env with bindings for the tyvars, taken from
-- the kind of the tycon/class.  Give it to the thing inside, and 
-- check the result kind matches
kcTyClDeclBody tc_name hs_tyvars thing_inside
  = tcLookupTy tc_name		`thenNF_Tc` \ tc ->
    let
	(tyvars_w_kinds, result_kind) = zipFunTys (hsTyVarNames hs_tyvars) (tyThingKind tc)
    in
    tcExtendKindEnv tyvars_w_kinds (thing_inside result_kind)
\end{code}


%************************************************************************
%*									*
\subsection{Step 4: Building the tycon/class}
%*									*
%************************************************************************

\begin{code}
buildTyConOrClass 
	:: RecFlag -> NameEnv Kind
	-> FiniteMap TyCon ArgVrcs -> NameEnv TyThingDetails
	-> RenamedTyClDecl -> (Name, TyThing)
	-- Can't fail; the only reason it's in the monad 
	-- is so it can zonk the kinds

buildTyConOrClass is_rec kenv rec_vrcs rec_details
	          (TySynonym tycon_name tyvar_names rhs src_loc)
  = (tycon_name, ATyCon tycon)
  where
	tycon = mkSynTyCon tycon_name tycon_kind arity tyvars rhs_ty argvrcs
	tycon_kind	    = lookupNameEnv_NF kenv tycon_name
	arity		    = length tyvar_names
	tyvars		    = mkTyClTyVars tycon_kind tyvar_names
	SynTyDetails rhs_ty = lookupNameEnv_NF rec_details tycon_name
        argvrcs		    = lookupWithDefaultFM rec_vrcs bogusVrcs tycon

buildTyConOrClass is_rec kenv rec_vrcs  rec_details
	          (TyData data_or_new context tycon_name tyvar_names _ nconstrs _ _ src_loc)
  = (tycon_name, ATyCon tycon)
  where
	tycon = mkAlgTyCon tycon_name tycon_kind tyvars ctxt argvrcs
			   data_cons nconstrs
			   derived_classes
			   flavour is_rec

	DataTyDetails ctxt data_cons derived_classes = lookupNameEnv_NF rec_details tycon_name

	tycon_kind = lookupNameEnv_NF kenv tycon_name
	tyvars	   = mkTyClTyVars tycon_kind tyvar_names
        argvrcs	   = lookupWithDefaultFM rec_vrcs bogusVrcs tycon

	flavour = case data_or_new of
			NewType -> NewTyCon (mkNewTyConRep tycon)
			DataType | all isNullaryDataCon data_cons -> EnumTyCon
				 | otherwise			  -> DataTyCon

buildTyConOrClass is_rec kenv rec_vrcs  rec_details
                  (ClassDecl context class_name
		             tyvar_names fundeps class_sigs def_methods pragmas 
		             tycon_name datacon_name datacon_wkr_name sc_sel_names src_loc)
  = (class_name, AClass clas)
  where
 	clas = mkClass class_name tyvars fds
		       sc_theta sc_sel_ids op_items
		       tycon

	tycon = mkClassTyCon tycon_name class_kind tyvars
                             argvrcs dict_con
			     clas 		-- Yes!  It's a dictionary 
			     flavour

	ClassDetails sc_theta sc_sel_ids op_items dict_con = lookupNameEnv_NF rec_details class_name

	class_kind = lookupNameEnv_NF kenv class_name
	tyvars	   = mkTyClTyVars class_kind tyvar_names
        argvrcs	   = lookupWithDefaultFM rec_vrcs bogusVrcs tycon
	n_fields   = length sc_sel_ids + length op_items

 	flavour | n_fields == 1 = NewTyCon (mkNewTyConRep tycon)
		| otherwise	= DataTyCon

	-- We can find the functional dependencies right away, 
	-- and it is vital to do so. Why?  Because in the next pass
	-- we check for ambiguity in all the type signatures, and we
	-- need the functional dependcies to be done by then
	fds	   = [(map lookup xs, map lookup ys) | (xs,ys) <- fundeps]
	tyvar_env  = mkNameEnv [(varName tv, tv) | tv <- tyvars]
	lookup     = lookupNameEnv_NF tyvar_env

bogusVrcs = panic "Bogus tycon arg variances"
\end{code}


%************************************************************************
%*									*
\subsection{Dependency analysis}
%*									*
%************************************************************************

Dependency analysis
~~~~~~~~~~~~~~~~~~~
\begin{code}
sortByDependency :: [RenamedHsDecl] -> TcM s [SCC RenamedTyClDecl]
sortByDependency decls
  = let		-- CHECK FOR CLASS CYCLES
	cls_sccs   = stronglyConnComp (mapMaybe mk_cls_edges tycl_decls)
	cls_cycles = [ decls | CyclicSCC decls <- cls_sccs]
    in
    checkTc (null cls_cycles) (classCycleErr cls_cycles)	`thenTc_`

    let		-- CHECK FOR SYNONYM CYCLES
	syn_sccs   = stronglyConnComp (filter is_syn_decl edges)
	syn_cycles = [ decls | CyclicSCC decls <- syn_sccs]

    in
    checkTc (null syn_cycles) (typeCycleErr syn_cycles)		`thenTc_`

    	-- DO THE MAIN DEPENDENCY ANALYSIS
    let
	decl_sccs  = stronglyConnComp edges
    in
    returnTc decl_sccs
  where
    tycl_decls = [d | TyClD d <- decls]
    edges      = map mk_edges tycl_decls
    
    is_syn_decl (d, _, _) = isSynDecl d
\end{code}

Edges in Type/Class decls
~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
----------------------------------------------------
-- mk_cls_edges looks only at the context of class decls
-- Its used when we are figuring out if there's a cycle in the
-- superclass hierarchy

mk_cls_edges :: RenamedTyClDecl -> Maybe (RenamedTyClDecl, Unique, [Unique])

mk_cls_edges decl@(ClassDecl ctxt name _ _ _ _ _ _ _ _ _ _)
  = Just (decl, getUnique name, map getUnique (catMaybes (map get_clas ctxt)))
mk_cls_edges other_decl
  = Nothing

----------------------------------------------------
mk_edges :: RenamedTyClDecl -> (RenamedTyClDecl, Unique, [Unique])

mk_edges decl@(TyData _ ctxt name _ condecls _ derivs _ _)
  = (decl, getUnique name, uniqSetToList (get_ctxt ctxt `unionUniqSets`
					 get_cons condecls `unionUniqSets`
					 get_deriv derivs))

mk_edges decl@(TySynonym name _ rhs _)
  = (decl, getUnique name, uniqSetToList (get_ty rhs))

mk_edges decl@(ClassDecl ctxt name _ _ sigs _ _ _ _ _ _ _)
  = (decl, getUnique name, uniqSetToList (get_ctxt ctxt `unionUniqSets`
				         get_sigs sigs))


----------------------------------------------------
get_ctxt ctxt = unionManyUniqSets (map set_name (catMaybes (map get_clas ctxt)))
get_clas (HsPClass clas _) = Just clas
get_clas _                 = Nothing

----------------------------------------------------
get_deriv Nothing     = emptyUniqSet
get_deriv (Just clss) = unionManyUniqSets (map set_name clss)

----------------------------------------------------
get_cons cons = unionManyUniqSets (map get_con cons)

----------------------------------------------------
get_con (ConDecl _ _ _ ctxt details _) 
  = get_ctxt ctxt `unionUniqSets` get_con_details details

----------------------------------------------------
get_con_details (VanillaCon btys)    = unionManyUniqSets (map get_bty btys)
get_con_details (InfixCon bty1 bty2) = unionUniqSets (get_bty bty1) (get_bty bty2)
get_con_details (RecCon nbtys)       = unionManyUniqSets (map (get_bty.snd) nbtys)

----------------------------------------------------
get_bty bty = get_ty (getBangType bty)

----------------------------------------------------
get_ty (HsTyVar name) | isTvOcc (nameOccName name) = emptyUniqSet 
		      | otherwise		   = set_name name
get_ty (HsAppTy ty1 ty2)	      = unionUniqSets (get_ty ty1) (get_ty ty2)
get_ty (HsFunTy ty1 ty2)	      = unionUniqSets (get_ty ty1) (get_ty ty2)
get_ty (HsListTy ty)		      = set_name listTyCon_name `unionUniqSets` get_ty ty
get_ty (HsTupleTy (HsTupCon n _) tys) = set_name n `unionUniqSets` get_tys tys
get_ty (HsUsgTy _ ty) 		      = get_ty ty
get_ty (HsUsgForAllTy _ ty) 	      = get_ty ty
get_ty (HsForAllTy _ ctxt mty) 	      = get_ctxt ctxt `unionUniqSets` get_ty mty
get_ty (HsPredTy (HsPClass name _))   = set_name name
get_ty (HsPredTy (HsPIParam _ _))     = emptyUniqSet	-- I think

----------------------------------------------------
get_tys tys = unionManyUniqSets (map get_ty tys)

----------------------------------------------------
get_sigs sigs
  = unionManyUniqSets (map get_sig sigs)
  where 
    get_sig (ClassOpSig _ _ ty _) = get_ty ty
    get_sig (FixSig _)		  = emptyUniqSet
    get_sig other = panic "TcTyClsDecls:get_sig"

----------------------------------------------------
set_name name = unitUniqSet (getUnique name)
\end{code}


%************************************************************************
%*									*
\subsection{Error management
%*									*
%************************************************************************

\begin{code}
tcAddDeclCtxt decl thing_inside
  = tcAddSrcLoc loc 	$
    tcAddErrCtxt ctxt 	$
    thing_inside
  where
     (name, loc, thing)
	= case decl of
	    (ClassDecl _ name _ _ _ _ _ _ _ _ _ loc) -> (name, loc, "class")
	    (TySynonym name _ _ loc)	             -> (name, loc, "type synonym")
	    (TyData NewType  _ name _ _ _ _ _ loc)   -> (name, loc, "newtype")
	    (TyData DataType _ name _ _ _ _ _ loc)   -> (name, loc, "data type")

     ctxt = hsep [ptext SLIT("In the"), text thing, 
		  ptext SLIT("declaration for"), quotes (ppr name)]
\end{code}

\begin{code}
typeCycleErr, classCycleErr :: [[RenamedTyClDecl]] -> Message

typeCycleErr syn_cycles
  = vcat (map (pp_cycle "Cycle in type declarations:") syn_cycles)

classCycleErr cls_cycles
  = vcat (map (pp_cycle "Cycle in class declarations:") cls_cycles)

pp_cycle str decls
  = hang (text str)
	 4 (vcat (map pp_decl decls))
  where
    pp_decl decl
      = hsep [quotes (ppr name), ptext SLIT("at"), ppr (getSrcLoc name)]
     where
        name = tyClDeclName decl

\end{code}
