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
			  Sig(..), MonoBinds, Fake, InPat )
import RnHsSyn		( RenamedTyDecl(..), RenamedClassDecl(..) )

import TcMonad
import Inst		( InstanceMapper(..) )
import TcClassDcl	( tcClassDecl1 )
import TcEnv		( tcExtendTyConEnv, tcExtendClassEnv,
			  tcExtendGlobalValEnv, tcExtendKindEnv,
			  tcTyVarScope, tcGetEnv )
import TcKind		( TcKind, newKindVars )
import TcTyDecls	( tcTyDecl )

import Bag	
import Class		( Class(..), getClassSelIds )
import Digraph		( findSCCs, SCC(..) )
import Name		( Name, isTyConName )
import PprStyle
import Pretty
import UniqSet		( UniqSet(..), emptyUniqSet,
			  singletonUniqSet, unionUniqSets, 
			  unionManyUniqSets, uniqSetToList ) 
import SrcLoc		( SrcLoc )
import TyCon		( TyCon, getTyConDataCons )
import Unique		( Unique )
import Util		( panic, pprTrace )

\end{code}

The main function
~~~~~~~~~~~~~~~~~
\begin{code}
data Decl = TyD RenamedTyDecl | ClD RenamedClassDecl

tcTyAndClassDecls1 :: InstanceMapper
		   -> Bag RenamedTyDecl -> Bag RenamedClassDecl
		   -> TcM s (TcEnv s)

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
    returnTc env

tcGroups inst_mapper (group:groups)
  = tcGroup inst_mapper group	`thenTc` \ new_env ->

	-- Extend the environment using the new tycons and classes
    tcSetEnv new_env $

	-- Do the remaining groups
    tcGroups inst_mapper groups
\end{code}

Dealing with a group
~~~~~~~~~~~~~~~~~~~~
\begin{code}
tcGroup :: InstanceMapper -> Bag Decl -> TcM s (TcEnv s)
tcGroup inst_mapper decls
  = fixTc ( \ ~(tycons,classes,_) ->

      pprTrace "tcGroup: " (ppCat (map (fst.fmt_decl) (bagToList decls))) $

		-- EXTEND TYPE AND CLASS ENVIRONMENTS
		-- including their data constructors and class operations
      tcExtendTyConEnv tycons					  $
      tcExtendClassEnv classes					  $
      tcExtendGlobalValEnv (concat (map getTyConDataCons tycons)) $
      tcExtendGlobalValEnv (concat (map getClassSelIds classes))  $

		-- SNAFFLE ENV TO RETURN
      tcGetEnv					`thenNF_Tc` \ final_env ->

		-- DEAL WITH TYPE VARIABLES
      tcTyVarScope tyvar_names 			( \ tyvars ->

		-- MANUFACTURE NEW KINDS, AND EXTEND KIND ENV
	newKindVars (length tycon_names)	`thenNF_Tc` \ tycon_kinds ->
	newKindVars (length class_names)	`thenNF_Tc` \ class_kinds ->
	tcExtendKindEnv tycon_names tycon_kinds		$
	tcExtendKindEnv class_names class_kinds		$


		-- DEAL WITH THE DEFINITIONS THEMSELVES
	foldBag combine (tcDecl inst_mapper)
		(returnTc (emptyBag, emptyBag))
		decls
      )						`thenTc` \ (tycons,classes) ->

      returnTc (bagToList tycons, bagToList classes, final_env)
    ) `thenTc` \ (_, _, final_env) ->
    returnTc final_env

  where
    (tyvar_names, tycon_names, class_names) = get_binders decls

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

fmt_decl (TyD (TySynonym name _ _ _))       = (ppr PprForUser name, getSrcLoc name)
fmt_decl (ClD (ClassDecl _ name _ _ _ _ _)) = (ppr PprForUser name, getSrcLoc name)
\end{code}

Edges in Type/Class decls
~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
mk_edges (TyD (TyData ctxt name _ condecls _ _ _))
  = (getItsUnique name, set_to_bag (get_ctxt ctxt `unionUniqSets` get_cons condecls))
mk_edges (TyD (TyNew  ctxt name _ condecl _ _ _))
  = (getItsUnique name, set_to_bag (get_ctxt ctxt `unionUniqSets` get_cons condecl))
mk_edges (TyD (TySynonym name _ rhs _))
  = (getItsUnique name, set_to_bag (get_ty rhs))
mk_edges (ClD (ClassDecl ctxt name _ sigs _ _ _))
  = (getItsUnique name, set_to_bag (get_ctxt ctxt `unionUniqSets` get_sigs sigs))

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
  = (if isTyConName name then set_name name else emptyUniqSet)
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

set_name name = singletonUniqSet (getItsUnique name)

set_to_bag set = listToBag (uniqSetToList set)
\end{code}

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
	    -> ([Name],	-- TyVars;  no dups
		[Name],	-- Tycons;  no dups
		[Name])	-- Classes; no dups

get_binders decls = (bagToList tyvars, bagToList tycons, bagToList classes)
  where
    (tyvars, tycons, classes) = foldBag union3 get_binders1
					(emptyBag,emptyBag,emptyBag)
					decls

    union3 (a1,a2,a3) (b1,b2,b3)
      = (a1 `unionBags` b1, a2 `unionBags` b2, a3 `unionBags` b3)

get_binders1 (TyD (TyData _ name tyvars _ _ _ _))
 = (listToBag tyvars, unitBag name, emptyBag)
get_binders1 (TyD (TyNew _ name tyvars _ _ _ _))
 = (listToBag tyvars, unitBag name, emptyBag)
get_binders1 (TyD (TySynonym name tyvars _ _))
 = (listToBag tyvars, unitBag name, emptyBag)
get_binders1 (ClD (ClassDecl _ name tyvar sigs _ _ _))
 = (unitBag tyvar `unionBags` sigs_tvs sigs,
    emptyBag, unitBag name)

-- ToDo: will this duplicate the class tyvar

sigs_tvs sigs = unionManyBags (map sig_tvs sigs)
  where 
    sig_tvs (ClassOpSig _ ty  _ _) = pty_tvs ty
    pty_tvs (HsForAllTy tvs _ _)   = listToBag tvs 
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
