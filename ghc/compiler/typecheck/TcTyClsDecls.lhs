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
			  HsType(..), HsTyVar,
			  ConDecl(..), ConDetails(..), BangType(..),
			  Sig(..), HsPred(..),
			  tyClDeclName, isClassDecl, isSynDecl
			)
import RnHsSyn		( RenamedHsDecl, RenamedTyClDecl, listTyCon_name, tupleTyCon_name )
import BasicTypes	( RecFlag(..), NewOrData(..), Arity )

import TcMonad
import Inst		( InstanceMapper )
import TcClassDcl	( kcClassDecl, tcClassDecl1 )
import TcEnv		( ValueEnv, TcTyThing(..),
			  tcExtendTypeEnv, getAllEnvTyCons
			)
import TcTyDecls	( tcTyDecl, kcTyDecl )
import TcMonoType	( kcHsTyVar )
import TcType		( TcKind, newKindVar, newKindVars, kindToTcKind, zonkTcKindToKind )

import Type		( mkArrowKind, boxedTypeKind )

import Class		( Class )
import Var		( TyVar, tyVarKind )
import FiniteMap
import Bag	
import VarSet
import Digraph		( stronglyConnComp, SCC(..) )
import Name		( Name, NamedThing(..), getSrcLoc, isTvOcc, nameOccName )
import Outputable
import Maybes		( mapMaybe, catMaybes, expectJust )
import UniqSet		( UniqSet, emptyUniqSet,
			  unitUniqSet, unionUniqSets, 
			  unionManyUniqSets, uniqSetToList ) 
import ErrUtils		( Message )
import SrcLoc		( SrcLoc )
import TyCon		( TyCon, ArgVrcs )
import Variance         ( calcTyConArgVrcs )
import Unique		( Unique, Uniquable(..) )
import UniqFM		( listToUFM, lookupUFM )
\end{code}

The main function
~~~~~~~~~~~~~~~~~
\begin{code}
tcTyAndClassDecls :: ValueEnv -> InstanceMapper	-- Knot tying stuff
		  -> [RenamedHsDecl]
		  -> TcM s TcEnv

tcTyAndClassDecls unf_env inst_mapper decls
  = sortByDependency decls 		`thenTc` \ groups ->
    tcGroups unf_env inst_mapper groups

tcGroups unf_env inst_mapper []
  = tcGetEnv	`thenNF_Tc` \ env ->
    returnTc env

tcGroups unf_env inst_mapper (group:groups)
  = tcGroup unf_env inst_mapper group	`thenTc` \ env ->
    tcSetEnv env			$
    tcGroups unf_env inst_mapper groups
\end{code}

Dealing with a group
~~~~~~~~~~~~~~~~~~~~

The knot-tying parameters: @rec_tyclss@ is an alist mapping @Name@s to
@TcTyThing@s.  @rec_vrcs@ is a finite map from @Name@s to @ArgVrcs@s.

\begin{code}
tcGroup :: ValueEnv -> InstanceMapper -> SCC RenamedTyClDecl -> TcM s TcEnv
tcGroup unf_env inst_mapper scc
  = 	-- Do kind checking
    mapNF_Tc getTyBinding1 decls 			`thenNF_Tc` \ ty_env_stuff1 ->
    tcExtendTypeEnv ty_env_stuff1 (mapTc kcDecl decls)	`thenTc_`

	-- Tie the knot
--  traceTc (ppr (map fst ty_env_stuff1))		`thenTc_`
    fixTc ( \ ~(rec_tyclss, rec_vrcs, _) ->
	let
	    rec_env = listToUFM rec_tyclss
	in
	
		-- Do type checking
	mapNF_Tc (getTyBinding2 rec_env) ty_env_stuff1	`thenNF_Tc` \ ty_env_stuff2 ->
	tcExtendTypeEnv ty_env_stuff2				$
	mapTc (tcDecl is_rec_group unf_env inst_mapper rec_vrcs) decls
                                                                `thenTc` \ tyclss ->

	tcGetEnv						`thenTc` \ env -> 
        let
            tycons = getAllEnvTyCons env
            vrcs   = calcTyConArgVrcs tycons
        in

	returnTc (tyclss, vrcs, env)
    )								`thenTc` \ (_, _, env) ->
--  traceTc (text "done" <+> ppr (map fst ty_env_stuff1))	`thenTc_`
    returnTc env
  where
    is_rec_group = case scc of
			AcyclicSCC _ -> NonRecursive
			CyclicSCC _  -> Recursive

    decls = case scc of
		AcyclicSCC decl -> [decl]
		CyclicSCC decls -> decls
\end{code}

Dealing with one decl
~~~~~~~~~~~~~~~~~~~~~
\begin{code}
kcDecl decl
  = tcAddDeclCtxt decl		$
    if isClassDecl decl then
	kcClassDecl decl
    else
	kcTyDecl    decl

tcDecl  :: RecFlag 			-- True => recursive group
	 -> ValueEnv -> InstanceMapper -> FiniteMap Name ArgVrcs
	 -> RenamedTyClDecl -> TcM s (Name, TcTyThing)

tcDecl is_rec_group unf_env inst_mapper vrcs_env decl
  = tcAddDeclCtxt decl		$
--  traceTc (text "Starting" <+> ppr name)	`thenTc_`
    if isClassDecl decl then
	tcClassDecl1 unf_env inst_mapper vrcs_env decl	`thenTc` \ clas ->
--	traceTc (text "Finished" <+> ppr name)		`thenTc_`
	returnTc (getName clas, AClass clas)
    else
	tcTyDecl is_rec_group vrcs_env decl	`thenTc` \ tycon ->
--	traceTc (text "Finished" <+> ppr name)	`thenTc_`
	returnTc (getName tycon, ATyCon tycon)

  where
    name = tyClDeclName decl
		

tcAddDeclCtxt decl thing_inside
  = tcAddSrcLoc loc 	$
    tcAddErrCtxt ctxt 	$
    thing_inside
  where
     (name, loc, thing)
	= case decl of
	    (ClassDecl _ name _ _ _ _ _ _ _ _ _ loc) -> (name, loc, "class")
	    (TySynonym name _ _ loc)	         -> (name, loc, "type synonym")
	    (TyData NewType  _ name _ _ _ _ loc) -> (name, loc, "data type")
	    (TyData DataType _ name _ _ _ _ loc) -> (name, loc, "newtype")

     ctxt = hsep [ptext SLIT("In the"), text thing, 
		  ptext SLIT("declaration for"), quotes (ppr name)]
\end{code}


getTyBinders
~~~~~~~~~~~
Extract *binding* names from type and class decls.  Type variables are
bound in type, data, newtype and class declarations, 
	*and* the polytypes in the class op sigs.
	*and* the existentially quantified contexts in datacon decls

Why do we need to grab all these type variables at once, including
those locally-quantified type variables in class op signatures?

	[Incidentally, this only works because the names are all unique by now.]

Because we can only commit to the final kind of a type variable when
we've completed the mutually recursive group. For example:

class C a where
   op :: D b => a -> b -> b

class D c where
   bop :: (Monad c) => ...

Here, the kind of the locally-polymorphic type variable "b"
depends on *all the uses of class D*.  For example, the use of
Monad c in bop's type signature means that D must have kind Type->Type.


\begin{code}
getTyBinding1 :: RenamedTyClDecl -> NF_TcM s (Name, (TcKind, Maybe Arity, TcTyThing))
getTyBinding1 (TySynonym name tyvars _ _)
 = mapNF_Tc kcHsTyVar tyvars		`thenNF_Tc` \ arg_kinds ->
   newKindVar				`thenNF_Tc` \ result_kind  ->
   returnNF_Tc (name, (foldr mkArrowKind result_kind arg_kinds, 
		       Just (length tyvars), 
		       ATyCon (pprPanic "ATyCon: syn" (ppr name))))

getTyBinding1 (TyData _ _ name tyvars _ _ _ _)
 = mapNF_Tc kcHsTyVar tyvars		`thenNF_Tc` \ arg_kinds ->
   returnNF_Tc (name, (foldr mkArrowKind boxedTypeKind arg_kinds, 
		       Nothing,  
		       ATyCon (error "ATyCon: data")))

getTyBinding1 (ClassDecl _ name tyvars _ _ _ _ _ _ _ _ _)
 = mapNF_Tc kcHsTyVar tyvars		`thenNF_Tc` \ arg_kinds ->
   returnNF_Tc (name, (foldr mkArrowKind boxedTypeKind arg_kinds, 
		       Just (length tyvars), 
		       AClass (error "AClass")))

-- Zonk the kind to its final form, and lookup the 
-- recursive tycon/class
getTyBinding2 rec_env (name, (tc_kind, maybe_arity, thing))
  = zonkTcKindToKind tc_kind		`thenNF_Tc` \ kind ->
    returnNF_Tc (name, (kind, maybe_arity, mk_thing thing (lookupUFM rec_env name)))
  where
    mk_thing (ATyCon _) ~(Just (ATyCon tc))  = ATyCon tc
    mk_thing (AClass _) ~(Just (AClass cls)) = AClass cls
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
    is_cls_decl (d, _, _) = isClassDecl d
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

mk_edges decl@(TyData _ ctxt name _ condecls derivs _ _)
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
get_con_details (NewCon ty _)        = get_ty ty
get_con_details (RecCon nbtys)       = unionManyUniqSets (map (get_bty.snd) nbtys)

----------------------------------------------------
get_bty (Banged ty)   = get_ty ty
get_bty (Unbanged ty) = get_ty ty
get_bty (Unpacked ty) = get_ty ty

----------------------------------------------------
get_ty (MonoTyVar name)
  = if isTvOcc (nameOccName name) then emptyUniqSet else set_name name
get_ty (MonoTyApp ty1 ty2)
  = unionUniqSets (get_ty ty1) (get_ty ty2)
get_ty (MonoFunTy ty1 ty2)	
  = unionUniqSets (get_ty ty1) (get_ty ty2)
get_ty (MonoListTy ty)
  = set_name listTyCon_name `unionUniqSets` get_ty ty
get_ty (MonoTupleTy tys boxed)
  = set_name (tupleTyCon_name boxed (length tys)) `unionUniqSets` get_tys tys
get_ty (MonoUsgTy _ ty)
  = get_ty ty
get_ty (MonoUsgForAllTy _ ty)
  = get_ty ty
get_ty (HsForAllTy _ ctxt mty)
  = get_ctxt ctxt `unionUniqSets` get_ty mty
get_ty (MonoDictTy name _)
  = set_name name
get_ty (MonoIParamTy name _)
  = emptyUniqSet

----------------------------------------------------
get_tys tys
  = unionManyUniqSets (map get_ty tys)

----------------------------------------------------
get_sigs sigs
  = unionManyUniqSets (map get_sig sigs)
  where 
    get_sig (ClassOpSig _ _ _ ty _) = get_ty ty
    get_sig (FixSig _)		    = emptyUniqSet
    get_sig other = panic "TcTyClsDecls:get_sig"

----------------------------------------------------
set_name name = unitUniqSet (getUnique name)
set_to_bag set = listToBag (uniqSetToList set)
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
