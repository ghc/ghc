%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[TcTyClsDecls]{Typecheck type and class declarations}

\begin{code}
#include "HsVersions.h"

module TcTyClsDecls (
	tcTyAndClassDecls1
    ) where

import Ubiq{-uitous-}

import HsSyn		( TyDecl(..),  ConDecl(..), BangType(..),
			  ClassDecl(..), MonoType(..), PolyType(..),
			  Sig(..), MonoBinds, Fake, InPat, HsBinds(..), Bind, HsExpr )
import RnHsSyn		( isRnTyCon, RenamedTyDecl(..), RenamedClassDecl(..),
			  RnName(..){-instance Uniquable-}
			)
import TcHsSyn		( TcHsBinds(..), TcIdOcc(..) )

import TcMonad
import Inst		( InstanceMapper(..) )
import TcClassDcl	( tcClassDecl1 )
import TcEnv		( tcExtendTyConEnv, tcExtendClassEnv,
			  tcExtendGlobalValEnv, 
			  tcTyVarScope, tcGetEnv )
import TcKind		( TcKind, newKindVars )
import TcTyDecls	( tcTyDecl, tcRecordSelectors )

import Bag	
import Class		( Class(..), getClassSelIds )
import Digraph		( findSCCs, SCC(..) )
import Outputable	( getSrcLoc )
import PprStyle
import Pretty
import UniqSet		( UniqSet(..), emptyUniqSet,
			  unitUniqSet, unionUniqSets, 
			  unionManyUniqSets, uniqSetToList ) 
import SrcLoc		( SrcLoc )
import TyCon		( TyCon, tyConDataCons )
import Unique		( Unique )
import Util		( panic, pprTrace )

\end{code}

The main function
~~~~~~~~~~~~~~~~~
\begin{code}
data Decl = TyD RenamedTyDecl | ClD RenamedClassDecl

tcTyAndClassDecls1 :: InstanceMapper
		   -> Bag RenamedTyDecl -> Bag RenamedClassDecl
		   -> TcM s (TcEnv s, TcHsBinds s)

tcTyAndClassDecls1 inst_mapper rnty_decls rncls_decls
  = sortByDependency syn_decls cls_decls decls `thenTc` \ groups ->
    tcGroups inst_mapper groups
  where
    cls_decls = mapBag ClD rncls_decls
    ty_decls  = mapBag TyD rnty_decls
    syn_decls = filterBag is_syn_decl ty_decls
    decls     = ty_decls `unionBags` cls_decls

    is_syn_decl (TyD (TySynonym _ _ _ _)) = True
    is_syn_decl _		          = False

tcGroups inst_mapper []
  = tcGetEnv		`thenNF_Tc` \ env ->
    returnTc (env, EmptyBinds)

tcGroups inst_mapper (group:groups)
  = tcGroup inst_mapper group	`thenTc` \ (new_env, binds1) ->

	-- Extend the environment using the new tycons and classes
    tcSetEnv new_env $

	-- Do the remaining groups
    tcGroups inst_mapper groups	`thenTc` \ (final_env, binds2) ->

    returnTc (final_env, binds1 `ThenBinds` binds2)
\end{code}

Dealing with a group
~~~~~~~~~~~~~~~~~~~~
\begin{code}
tcGroup :: InstanceMapper -> Bag Decl -> TcM s (TcEnv s, TcHsBinds s)
tcGroup inst_mapper decls
  = pprTrace "tcGroup: " (ppCat (map (fst.fmt_decl) (bagToList decls))) $

	-- TIE THE KNOT
    fixTc ( \ ~(tycons,classes,_) ->

		-- EXTEND TYPE AND CLASS ENVIRONMENTS
		-- including their data constructors and class operations
		-- NB: it's important that the tycons and classes come back in just
		-- the same order from this fix as from get_binders, so that these
		-- extend-env things work properly.  A bit UGH-ish.
      tcExtendTyConEnv tycon_names_w_arities tycons		  $
      tcExtendClassEnv class_names classes			  $

		-- DEAL WITH TYPE VARIABLES
      tcTyVarScope tyvar_names 			( \ tyvars ->

		-- DEAL WITH THE DEFINITIONS THEMSELVES
	foldBag combine (tcDecl inst_mapper)
		(returnTc (emptyBag, emptyBag))
		decls
      )						`thenTc` \ (tycon_bag,class_bag) ->
      let
	tycons = bagToList tycon_bag
	classes = bagToList class_bag
      in 

		-- SNAFFLE ENV TO RETURN
      tcGetEnv					`thenNF_Tc` \ final_env ->

      returnTc (tycons, classes, final_env)
    ) `thenTc` \ (tycons, classes, final_env) ->


	-- Create any necessary record selector Ids and their bindings
    mapAndUnzipTc tcRecordSelectors tycons	`thenTc` \ (sel_ids_s, binds) ->
	
	-- Extend the global value environment with 
	--	a) constructors
	--	b) record selectors
	--	c) class op selectors

    tcSetEnv final_env						$
    tcExtendGlobalValEnv (concat (map tyConDataCons tycons))	$
    tcExtendGlobalValEnv (concat sel_ids_s)			$
    tcExtendGlobalValEnv (concat (map getClassSelIds classes))  $
    tcGetEnv			`thenNF_Tc` \ really_final_env ->

    returnTc (really_final_env, foldr ThenBinds EmptyBinds binds)

  where
    (tyvar_rn_names, tycon_names_w_arities, class_names) = get_binders decls

    tyvar_names = map de_rn tyvar_rn_names
    de_rn (RnName n) = n

    combine do_a do_b
      = do_a `thenTc` \ (a1,a2) ->
        do_b `thenTc` \ (b1,b2) ->
	returnTc (a1 `unionBags` b1, a2 `unionBags` b2)
\end{code}

Dealing with one decl
~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tcDecl  :: InstanceMapper
	-> Decl
	-> TcM s (Bag TyCon, Bag Class)

tcDecl inst_mapper (TyD decl)
  = tcTyDecl decl	`thenTc` \ tycon ->
    returnTc (unitBag tycon, emptyBag)

tcDecl inst_mapper (ClD decl)
  = tcClassDecl1 inst_mapper decl   `thenTc` \ clas ->
    returnTc (emptyBag, unitBag clas)
\end{code}

Dependency analysis
~~~~~~~~~~~~~~~~~~~
\begin{code}
sortByDependency :: Bag Decl -> Bag Decl -> Bag Decl -> TcM s [Bag Decl]
sortByDependency syn_decls cls_decls decls
  = let		-- CHECK FOR SYNONYM CYCLES
	syn_sccs   = findSCCs mk_edges syn_decls
	syn_cycles = [map fmt_decl (bagToList decls)
	                | CyclicSCC decls <- syn_sccs]

    in
    checkTc (null syn_cycles) (typeCycleErr syn_cycles)		`thenTc_`

    let		-- CHECK FOR CLASS CYCLES
	cls_sccs   = findSCCs mk_edges cls_decls
	cls_cycles = [map fmt_decl (bagToList decls)
		        | CyclicSCC decls <- cls_sccs]

    in
    checkTc (null cls_cycles) (classCycleErr cls_cycles)	`thenTc_`

		-- DO THE MAIN DEPENDENCY ANALYSIS
    let
	decl_sccs  = findSCCs mk_edges decls
	scc_bags   = map bag_acyclic decl_sccs
    in
    returnTc (scc_bags)
    
  where
   bag_acyclic (AcyclicSCC scc) = unitBag scc
   bag_acyclic (CyclicSCC sccs) = sccs

fmt_decl decl
  = (ppr PprForUser name, getSrcLoc name)
  where
    name = get_name decl
    get_name (TyD (TyData _ name _ _ _ _ _))    = name
    get_name (TyD (TyNew  _ name _ _ _ _ _))    = name
    get_name (TyD (TySynonym name _ _ _))       = name
    get_name (ClD (ClassDecl _ name _ _ _ _ _)) = name
\end{code}

Edges in Type/Class decls
~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
mk_edges (TyD (TyData ctxt name _ condecls _ _ _))
  = (uniqueOf name, set_to_bag (get_ctxt ctxt `unionUniqSets` get_cons condecls))
mk_edges (TyD (TyNew  ctxt name _ condecl _ _ _))
  = (uniqueOf name, set_to_bag (get_ctxt ctxt `unionUniqSets` get_cons condecl))
mk_edges (TyD (TySynonym name _ rhs _))
  = (uniqueOf name, set_to_bag (get_ty rhs))
mk_edges (ClD (ClassDecl ctxt name _ sigs _ _ _))
  = (uniqueOf name, set_to_bag (get_ctxt ctxt `unionUniqSets` get_sigs sigs))

get_ctxt ctxt
  = unionManyUniqSets (map (set_name.fst) ctxt)

get_cons cons
  = unionManyUniqSets (map get_con cons)
  where
    get_con (ConDecl _ btys _)
      = unionManyUniqSets (map get_bty btys)
    get_con (ConOpDecl bty1 _ bty2 _)
      = unionUniqSets (get_bty bty1) (get_bty bty2)
    get_con (NewConDecl _ ty _)
      = get_ty ty
    get_con (RecConDecl _ nbtys _)
      = unionManyUniqSets (map (get_bty.snd) nbtys)

    get_bty (Banged ty)   = get_ty ty
    get_bty (Unbanged ty) = get_ty ty

get_ty (MonoTyVar tv)
  = emptyUniqSet
get_ty (MonoTyApp name tys)
  = (if isRnTyCon name then set_name name else emptyUniqSet)
    `unionUniqSets` get_tys tys
get_ty (MonoFunTy ty1 ty2)	
  = unionUniqSets (get_ty ty1) (get_ty ty2)
get_ty (MonoListTy ty)
  = get_ty ty			-- careful when defining [] (,,) etc as
get_ty (MonoTupleTy tys)	-- [ty] (ty,ty,ty) will not give edges!
  = get_tys tys
get_ty other = panic "TcTyClsDecls:get_ty"

get_pty (HsForAllTy _ ctxt mty)
  = get_ctxt ctxt `unionUniqSets` get_ty mty
get_pty other = panic "TcTyClsDecls:get_pty"

get_tys tys
  = unionManyUniqSets (map get_ty tys)

get_sigs sigs
  = unionManyUniqSets (map get_sig sigs)
  where 
    get_sig (ClassOpSig _ ty _ _) = get_pty ty
    get_sig other = panic "TcTyClsDecls:get_sig"

set_name name = unitUniqSet (uniqueOf name)

set_to_bag set = listToBag (uniqSetToList set)
\end{code}


get_binders
~~~~~~~~~~~
Extract *binding* names from type and class decls.  Type variables are
bound in type, data, newtype and class declarations and the polytypes
in the class op sigs.

Why do we need to grab all these type variables at once, including
those locally-quantified type variables in class op signatures?
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
get_binders :: Bag Decl
	    -> ([RnName],		-- TyVars;  no dups
		[(RnName, Maybe Arity)],-- Tycons;  no dups; arities for synonyms
		[RnName])		-- Classes; no dups

get_binders decls = (bagToList tyvars, bagToList tycons, bagToList classes)
  where
    (tyvars, tycons, classes) = foldBag union3 get_binders1
					(emptyBag,emptyBag,emptyBag)
					decls

    union3 (a1,a2,a3) (b1,b2,b3)
      = (a1 `unionBags` b1, a2 `unionBags` b2, a3 `unionBags` b3)

get_binders1 (TyD (TyData _ name tyvars _ _ _ _))
 = (listToBag tyvars, unitBag (name,Nothing), emptyBag)
get_binders1 (TyD (TyNew _ name tyvars _ _ _ _))
 = (listToBag tyvars, unitBag (name,Nothing), emptyBag)
get_binders1 (TyD (TySynonym name tyvars _ _))
 = (listToBag tyvars, unitBag (name, Just (length tyvars)), emptyBag)
get_binders1 (ClD (ClassDecl _ name tyvar sigs _ _ _))
 = (unitBag tyvar `unionBags` sigs_tvs sigs,
    emptyBag, unitBag name)

sigs_tvs sigs = unionManyBags (map sig_tvs sigs)
  where 
    sig_tvs (ClassOpSig _ ty  _ _) = pty_tvs ty
    pty_tvs (HsForAllTy tvs _ _)   = listToBag tvs 	-- tvs doesn't include the class tyvar
\end{code}


\begin{code}
typeCycleErr syn_cycles sty
  = ppAboves (map (pp_cycle sty "Cycle in type declarations ...") syn_cycles)

classCycleErr cls_cycles sty
  = ppAboves (map (pp_cycle sty "Cycle in class declarations ...") cls_cycles)

pp_cycle sty str things
  = ppHang (ppStr str)
	 4 (ppAboves (map pp_thing things))
  where
    pp_thing (pp_name, loc)
      = ppCat [pp_name, ppr sty loc]
\end{code}
