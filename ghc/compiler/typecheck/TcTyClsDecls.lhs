%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcTyClsDecls]{Typecheck type and class declarations}

\begin{code}
module TcTyClsDecls (
	tcTyAndClassDecls1
    ) where

#include "HsVersions.h"

import HsSyn		( HsDecl(..), TyDecl(..), ClassDecl(..), 
			  HsType(..), HsTyVar,
			  ConDecl(..), ConDetails(..), BangType(..),
			  Sig(..),
			  hsDeclName
			)
import RnHsSyn		( RenamedHsDecl )
import RnEnv		( listTyCon_name, tupleTyCon_name ) -- ToDo: move these
import BasicTypes	( RecFlag(..), Arity )

import TcMonad
import Inst		( InstanceMapper )
import TcClassDcl	( tcClassDecl1 )
import TcEnv		( TcIdOcc(..), GlobalValueEnv, tcExtendTyConEnv, tcExtendClassEnv )
import TcType		( TcKind, newKindVar, newKindVars, kindToTcKind )
import TcTyDecls	( tcTyDecl )
import TcMonoType	( tcTyVarScope )

import TyCon		( tyConKind, tyConArity, isSynTyCon )
import Class		( Class, classBigSig )
import Var		( tyVarKind )
import Bag	
import Digraph		( stronglyConnComp, SCC(..) )
import Name		( Name, NamedThing(..), getSrcLoc, isTvOcc, nameOccName )
import Outputable
import Maybes		( mapMaybe )
import UniqSet		( UniqSet, emptyUniqSet,
			  unitUniqSet, unionUniqSets, 
			  unionManyUniqSets, uniqSetToList ) 
import SrcLoc		( SrcLoc )
import TyCon		( TyCon )
import Unique		( Unique, Uniquable(..) )
import Util		( panic{-, pprTrace-} )

\end{code}

The main function
~~~~~~~~~~~~~~~~~
\begin{code}
tcTyAndClassDecls1 :: GlobalValueEnv -> InstanceMapper	-- Knot tying stuff
		   -> [RenamedHsDecl]
		   -> TcM s (TcEnv s)

tcTyAndClassDecls1 unf_env inst_mapper decls
  = sortByDependency decls 		`thenTc` \ groups ->
    tcGroups unf_env inst_mapper groups

tcGroups unf_env inst_mapper []
  = tcGetEnv	`thenNF_Tc` \ env ->
    returnTc env

tcGroups unf_env inst_mapper (group:groups)
  = tcGroup unf_env inst_mapper group	`thenTc` \ (group_tycons, group_classes) ->

	-- Extend the environment using the new tycons and classes
    tcExtendTyConEnv [(getName tycon, (kindToTcKind (tyConKind tycon),
				       if isSynTyCon tycon then Just (tyConArity tycon) else Nothing,
				       tycon))
		     | tycon <- group_tycons]	 $

    tcExtendClassEnv [(getName clas, (classKind clas, clas))
		     | clas <- group_classes]	 $


	-- Do the remaining groups
    tcGroups unf_env inst_mapper groups
  where
    classKind clas = map (kindToTcKind . tyVarKind) tyvars
		   where
		     (tyvars, _, _, _, _) = classBigSig clas
\end{code}

Dealing with a group
~~~~~~~~~~~~~~~~~~~~

Notice the uses of @zipLazy@, which makes sure
that the knot-tied TyVars, TyCons and Classes aren't looked at too early.

    
\begin{code}
tcGroup :: GlobalValueEnv -> InstanceMapper -> SCC RenamedHsDecl -> TcM s ([TyCon], [Class])
tcGroup unf_env inst_mapper scc
  = 	-- TIE THE KNOT
    fixTc ( \ ~(rec_tycons, rec_classes) ->

		-- EXTEND TYPE AND CLASS ENVIRONMENTS
      let
        mk_tycon_bind (name, arity) = newKindVar	`thenNF_Tc` \ kind ->
				      returnNF_Tc (name, (kind, arity, find name rec_tycons))

	mk_class_bind (name, arity) = newKindVars arity	 `thenNF_Tc` \ kinds ->
				      returnNF_Tc (name, (kinds, find name rec_classes))

        find name []		 = pprPanic "tcGroup" (ppr name)
	find name (thing:things) | name == getName thing = thing
				 | otherwise		 = find name things

      in
      mapNF_Tc mk_tycon_bind tycon_names_w_arities    `thenNF_Tc` \ tycon_binds ->
      mapNF_Tc mk_class_bind class_names_w_arities    `thenNF_Tc` \ class_binds ->
      tcExtendTyConEnv tycon_binds	  $
      tcExtendClassEnv class_binds	  $

		-- DEAL WITH TYPE VARIABLES
      tcTyVarScope tyvar_names 			( \ tyvars ->

		-- DEAL WITH THE DEFINITIONS THEMSELVES
	foldlTc (tcDecl is_rec_group unf_env inst_mapper) ([], []) decls
      )						`thenTc` \ (tycons, classes) ->

      returnTc (tycons, classes)
    )
  where
    is_rec_group = case scc of
			AcyclicSCC _ -> NonRecursive
			CyclicSCC _  -> Recursive

    decls = case scc of
		AcyclicSCC decl -> [decl]
		CyclicSCC decls -> decls

    (tyvar_names, tycon_names_w_arities, class_names_w_arities) = get_binders decls
\end{code}

Dealing with one decl
~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tcDecl  :: RecFlag 			-- True => recursive group
	-> GlobalValueEnv -> InstanceMapper
	-> ([TyCon], [Class])		-- Accumulating parameter
	-> RenamedHsDecl
	-> TcM s ([TyCon], [Class])

tcDecl is_rec_group unf_env inst_mapper (tycons, classes) (TyD decl)
  = tcTyDecl is_rec_group decl	`thenTc` \ tycon ->
    returnTc (tycon:tycons, classes)

tcDecl is_rec_group unf_env inst_mapper (tycons, classes) (ClD decl)
  = tcClassDecl1 unf_env inst_mapper decl   `thenTc` \ clas ->
    returnTc (tycons, clas:classes)
\end{code}

Dependency analysis
~~~~~~~~~~~~~~~~~~~
\begin{code}
sortByDependency :: [RenamedHsDecl] -> TcM s [SCC RenamedHsDecl]
sortByDependency decls
  = let		-- CHECK FOR CLASS CYCLES
	cls_sccs   = stronglyConnComp (mapMaybe mk_cls_edges decls)
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
    edges = mapMaybe mk_edges decls
    
is_syn_decl (TyD (TySynonym _ _ _ _), _, _) = True
is_syn_decl _		                    = False

is_cls_decl (ClD _, _, _) = True
is_cls_decl other         = False
\end{code}

Edges in Type/Class decls
~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
-- mk_cls_edges looks only at the context of class decls
-- Its used when we are figuring out if there's a cycle in the
-- superclass hierarchy

mk_cls_edges :: RenamedHsDecl -> Maybe (RenamedHsDecl, Unique, [Unique])

mk_cls_edges decl@(ClD (ClassDecl ctxt name _ _ _ _ _ _ _))
  = Just (decl, getUnique name, map (getUnique . fst) ctxt)
mk_cls_edges other_decl
  = Nothing


mk_edges :: RenamedHsDecl -> Maybe (RenamedHsDecl, Unique, [Unique])

mk_edges decl@(TyD (TyData _ ctxt name _ condecls derivs _ _))
  = Just (decl, getUnique name, uniqSetToList (get_ctxt ctxt `unionUniqSets` 
					 get_cons condecls `unionUniqSets` 
					 get_deriv derivs))

mk_edges decl@(TyD (TySynonym name _ rhs _))
  = Just (decl, getUnique name, uniqSetToList (get_ty rhs))

mk_edges decl@(ClD (ClassDecl ctxt name _ sigs _ _ _ _ _))
  = Just (decl, getUnique name, uniqSetToList (get_ctxt ctxt `unionUniqSets`
				         get_sigs sigs))

mk_edges other_decl = Nothing

get_ctxt ctxt = unionManyUniqSets (map (set_name.fst) ctxt)

get_deriv Nothing     = emptyUniqSet
get_deriv (Just clss) = unionManyUniqSets (map set_name clss)

get_cons cons = unionManyUniqSets (map get_con cons)

get_con (ConDecl _ _ ctxt details _) 
  = get_ctxt ctxt `unionUniqSets` get_con_details details

get_con_details (VanillaCon btys)    = unionManyUniqSets (map get_bty btys)
get_con_details (InfixCon bty1 bty2) = unionUniqSets (get_bty bty1) (get_bty bty2)
get_con_details (NewCon ty)          =  get_ty ty
get_con_details (RecCon nbtys)       = unionManyUniqSets (map (get_bty.snd) nbtys)

get_bty (Banged ty)   = get_ty ty
get_bty (Unbanged ty) = get_ty ty

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
get_ty (HsForAllTy _ ctxt mty)
  = get_ctxt ctxt `unionUniqSets` get_ty mty
get_ty other = panic "TcTyClsDecls:get_ty"

get_tys tys
  = unionManyUniqSets (map get_ty tys)

get_sigs sigs
  = unionManyUniqSets (map get_sig sigs)
  where 
    get_sig (ClassOpSig _ _ ty _) = get_ty ty
    get_sig other = panic "TcTyClsDecls:get_sig"

set_name name = unitUniqSet (getUnique name)

set_to_bag set = listToBag (uniqSetToList set)
\end{code}


get_binders
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
get_binders :: [RenamedHsDecl]
	    -> ([HsTyVar Name],		-- TyVars;  no dups
		[(Name, Maybe Arity)],	-- Tycons;  no dups; arities for synonyms
		[(Name, Arity)])	-- Classes; no dups; with their arities

get_binders decls = (bagToList tyvars, bagToList tycons, bagToList classes)
  where
    (tyvars, tycons, classes) = foldr (union3 . get_binders1)
				      (emptyBag,emptyBag,emptyBag)
				      decls

    union3 (a1,a2,a3) (b1,b2,b3)
      = (a1 `unionBags` b1, a2 `unionBags` b2, a3 `unionBags` b3)

get_binders1 (TyD (TySynonym name tyvars _ _))
 = (listToBag tyvars, unitBag (name, Just (length tyvars)), emptyBag)
get_binders1 (TyD (TyData _ _ name tyvars condecls _ _ _))
 = (listToBag tyvars `unionBags` cons_tvs condecls,
    unitBag (name,Nothing), emptyBag)
get_binders1 (ClD (ClassDecl _ name tyvars sigs _ _ _ _ _))
 = (listToBag tyvars `unionBags` sigs_tvs sigs,
    emptyBag, unitBag (name, length tyvars))

cons_tvs condecls = unionManyBags (map con_tvs condecls)
  where
    con_tvs (ConDecl _ tvs _ _ _) = listToBag tvs

sigs_tvs sigs = unionManyBags (map sig_tvs sigs)
  where 
    sig_tvs (ClassOpSig _ _ ty _) = pty_tvs ty
    pty_tvs (HsForAllTy tvs _ _)  = listToBag tvs 	-- tvs doesn't include the class tyvar
    pty_tvs other		  = emptyBag
\end{code}


\begin{code}
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
        name = hsDeclName decl
\end{code}
