%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[TcTyClsDecls]{Typecheck type and class declarations}

\begin{code}
#include "HsVersions.h"

module TcTyClsDecls (
	tcTyAndClassDecls1
    ) where

IMP_Ubiq(){-uitous-}

import HsSyn		( HsDecl(..), TyDecl(..),  ConDecl(..), BangType(..),
			  ClassDecl(..), HsType(..), HsTyVar, DefaultDecl, InstDecl,
			  IfaceSig, Sig(..), MonoBinds, Fake, InPat, HsBinds(..), Bind, HsExpr,
			  hsDeclName
			)
import RnHsSyn		( RenamedTyDecl(..), RenamedClassDecl(..), SYN_IE(RenamedHsDecl)
			)
import TcHsSyn		( SYN_IE(TcHsBinds), TcIdOcc(..) )

import TcMonad
import Inst		( SYN_IE(InstanceMapper) )
import TcClassDcl	( tcClassDecl1 )
import TcEnv		( tcExtendTyConEnv, tcExtendClassEnv )
import SpecEnv		( SpecEnv )
import TcKind		( TcKind, newKindVars )
import TcTyDecls	( tcTyDecl, mkDataBinds )
import TcMonoType	( tcTyVarScope )

import Bag	
import Class		( SYN_IE(Class), classSelIds )
import Digraph		( findSCCs, SCC(..) )
import Name		( Name, getSrcLoc, isTvOcc, nameOccName )
import PprStyle
import Pretty
import UniqSet		( SYN_IE(UniqSet), emptyUniqSet,
			  unitUniqSet, unionUniqSets, 
			  unionManyUniqSets, uniqSetToList ) 
import SrcLoc		( SrcLoc )
import TyCon		( TyCon )
import Unique		( Unique )
import Util		( panic{-, pprTrace-} )

\end{code}

The main function
~~~~~~~~~~~~~~~~~
\begin{code}
tcTyAndClassDecls1 :: InstanceMapper
		   -> [RenamedHsDecl]
		   -> TcM s (TcEnv s)

tcTyAndClassDecls1 inst_mapper decls
  = sortByDependency decls 		`thenTc` \ groups ->
    tcGroups inst_mapper groups

tcGroups inst_mapper []
  = tcGetEnv	`thenNF_Tc` \ env ->
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
tcGroup :: InstanceMapper -> Bag RenamedHsDecl -> TcM s (TcEnv s)
tcGroup inst_mapper decls
  = -- pprTrace "tcGroup: " (ppCat (map (fst.fmt_decl) (bagToList decls))) $

	-- TIE THE KNOT
    fixTc ( \ ~(tycons,classes,_) ->

		-- EXTEND TYPE AND CLASS ENVIRONMENTS
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
    ) `thenTc` \ (_, _, final_env) ->

    returnTc final_env

  where
    (tyvar_names, tycon_names_w_arities, class_names) = get_binders decls

    combine do_a do_b
      = do_a `thenTc` \ (a1,a2) ->
        do_b `thenTc` \ (b1,b2) ->
	returnTc (a1 `unionBags` b1, a2 `unionBags` b2)
\end{code}

Dealing with one decl
~~~~~~~~~~~~~~~~~~~~~
\begin{code}
tcDecl  :: InstanceMapper
	-> RenamedHsDecl
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
sortByDependency :: [RenamedHsDecl] -> TcM s [Bag RenamedHsDecl]
sortByDependency decls
  = let		-- CHECK FOR SYNONYM CYCLES
	syn_sccs   = findSCCs mk_edges syn_decls
	syn_cycles = [ map fmt_decl (bagToList decls)
	             | CyclicSCC decls <- syn_sccs]

    in
    checkTc (null syn_cycles) (typeCycleErr syn_cycles)		`thenTc_`

    let		-- CHECK FOR CLASS CYCLES
	cls_sccs   = findSCCs mk_edges cls_decls
	cls_cycles = [ map fmt_decl (bagToList decls)
		     | CyclicSCC decls <- cls_sccs]

    in
    checkTc (null cls_cycles) (classCycleErr cls_cycles)	`thenTc_`

		-- DO THE MAIN DEPENDENCY ANALYSIS
    let
	decl_sccs  = findSCCs mk_edges ty_cls_decls
	scc_bags   = map bag_acyclic decl_sccs
    in
    returnTc (scc_bags)

  where
    syn_decls    = listToBag (filter is_syn_decl decls)
    ty_cls_decls = listToBag (filter is_ty_cls_decl decls)
    cls_decls    = listToBag (filter is_cls_decl decls)	

    

bag_acyclic (AcyclicSCC scc) = unitBag scc
bag_acyclic (CyclicSCC sccs) = sccs

is_syn_decl (TyD (TySynonym _ _ _ _)) = True
is_syn_decl _		          = False

is_ty_cls_decl (TyD _) = True
is_ty_cls_decl (ClD _) = True
is_ty_cls_decl other   = False

is_cls_decl (ClD _) = True
is_cls_decl other   = False

fmt_decl decl
  = (ppr PprForUser name, getSrcLoc name)
  where
    name = hsDeclName decl
\end{code}

Edges in Type/Class decls
~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
mk_edges (TyD (TyData ctxt name _ condecls derivs _ _))
  = (uniqueOf name, set_to_bag (get_ctxt ctxt `unionUniqSets` 
		    get_cons condecls `unionUniqSets` 
		    get_deriv derivs))

mk_edges (TyD (TyNew  ctxt name _ condecl derivs _ _))
  = (uniqueOf name, set_to_bag (get_ctxt ctxt `unionUniqSets` 
				get_con condecl  `unionUniqSets` 
				get_deriv derivs))

mk_edges (TyD (TySynonym name _ rhs _))
  = (uniqueOf name, set_to_bag (get_ty rhs))

mk_edges (ClD (ClassDecl ctxt name _ sigs _ _ _))
  = (uniqueOf name, set_to_bag (get_ctxt ctxt `unionUniqSets` get_sigs sigs))

get_ctxt ctxt
  = unionManyUniqSets (map (set_name.fst) ctxt)

get_deriv Nothing     = emptyUniqSet
get_deriv (Just clss) = unionManyUniqSets (map set_name clss)

get_cons cons
  = unionManyUniqSets (map get_con cons)

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
  = (if isTvOcc (nameOccName name) then emptyUniqSet else set_name name)
    `unionUniqSets` get_tys tys
get_ty (MonoFunTy ty1 ty2)	
  = unionUniqSets (get_ty ty1) (get_ty ty2)
get_ty (MonoListTy tc ty)
  = set_name tc `unionUniqSets` get_ty ty
get_ty (MonoTupleTy tc tys)
  = set_name tc `unionUniqSets` get_tys tys
get_ty (HsForAllTy _ ctxt mty)
  = get_ctxt ctxt `unionUniqSets` get_ty mty
get_ty other = panic "TcTyClsDecls:get_ty"

get_tys tys
  = unionManyUniqSets (map get_ty tys)

get_sigs sigs
  = unionManyUniqSets (map get_sig sigs)
  where 
    get_sig (ClassOpSig _ ty _ _) = get_ty ty
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
get_binders :: Bag RenamedHsDecl
	    -> ([HsTyVar Name],		-- TyVars;  no dups
		[(Name, Maybe Arity)],	-- Tycons;  no dups; arities for synonyms
		[Name])			-- Classes; no dups

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
    pty_tvs other		   = emptyBag
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
