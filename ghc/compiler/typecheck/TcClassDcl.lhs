%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcClassDcl]{Typechecking class declarations}

\begin{code}
#include "HsVersions.h"

module TcClassDcl ( tcClassDecl1, tcClassDecls2 ) where

IMP_Ubiq()

import HsSyn		( HsDecl(..), ClassDecl(..), HsBinds(..), MonoBinds(..),
			  Match(..), GRHSsAndBinds(..), GRHS(..), HsExpr(..), 
			  DefaultDecl, TyDecl, InstDecl, IfaceSig, Fixity,
			  HsLit(..), OutPat(..), Sig(..), HsType(..), HsTyVar,
			  SYN_IE(RecFlag), nonRecursive, andMonoBinds,
			  Stmt, DoOrListComp, ArithSeqInfo, InPat, Fake )
import HsTypes		( getTyVarName )
import HsPragmas	( ClassPragmas(..) )
import RnHsSyn		( RenamedClassDecl(..), RenamedClassPragmas(..),
			  RenamedClassOpSig(..), SYN_IE(RenamedMonoBinds),
			  RenamedGenPragmas(..), RenamedContext(..), SYN_IE(RenamedHsDecl)
			)
import TcHsSyn		( TcIdOcc(..), SYN_IE(TcHsBinds), SYN_IE(TcMonoBinds), SYN_IE(TcExpr),
			  mkHsTyApp, mkHsTyLam, mkHsDictApp, mkHsDictLam, tcIdType )

import Inst		( Inst, InstOrigin(..), SYN_IE(LIE), emptyLIE, plusLIE, newDicts, newMethod )
import TcEnv		( tcLookupClass, tcLookupTyVar, tcLookupTyCon, newLocalIds, tcAddImportedIdInfo,
			  tcExtendGlobalTyVars )
import TcInstDcls	( tcMethodBind )
import TcKind		( unifyKind, TcKind )
import TcMonad
import TcMonoType	( tcHsType, tcContext )
import TcSimplify	( tcSimplifyAndCheck )
import TcType		( SYN_IE(TcType), SYN_IE(TcTyVar), tcInstType, tcInstSigTyVars, tcInstSigType )

import Bag		( foldBag, unionManyBags )
import Class		( GenClass, GenClassOp, mkClass, mkClassOp, classBigSig, 
			  classOps, classOpString, classOpLocalType, classDefaultMethodId,
			  classOpTagByOccName, SYN_IE(ClassOp), SYN_IE(Class)
			)
import CmdLineOpts      ( opt_PprUserLength )
import Id		( GenId, mkSuperDictSelId, mkMethodSelId, 
			  mkDefaultMethodId, getIdUnfolding,
			  idType, SYN_IE(Id)
			)
import CoreUnfold	( getUnfoldingTemplate )
import IdInfo
import Name		( Name, isLocallyDefined, moduleString, 
			  nameString, NamedThing(..) )
import Outputable
import PrelVals		( nO_DEFAULT_METHOD_ERROR_ID )
import Pretty
import PprType		( GenClass, GenType, GenTyVar, GenClassOp )
import SpecEnv		( SpecEnv )
import SrcLoc		( mkGeneratedSrcLoc )
import Type		( mkFunTy, mkTyVarTy, mkTyVarTys, mkDictTy,
			  mkForAllTy, mkSigmaTy, splitSigmaTy, SYN_IE(Type)
			)
import TysWiredIn	( stringTy )
import TyVar		( unitTyVarSet, GenTyVar, SYN_IE(TyVar) )
import Unique		( Unique )
import UniqFM		( Uniquable(..) )
import Util


-- import TcPragmas	( tcGenPragmas, tcClassOpPragmas )
tcGenPragmas ty id ps = returnNF_Tc noIdInfo
tcClassOpPragmas ty sel def spec ps = returnNF_Tc (noIdInfo `addSpecInfo` spec, 
						   noIdInfo)
\end{code}



Dictionary handling
~~~~~~~~~~~~~~~~~~~
Every class implicitly declares a new data type, corresponding to dictionaries
of that class. So, for example:

	class (D a) => C a where
	  op1 :: a -> a
	  op2 :: forall b. Ord b => a -> b -> b

would implicitly declare

	data CDict a = CDict (D a)	
			     (a -> a)
			     (forall b. Ord b => a -> b -> b)

(We could use a record decl, but that means changing more of the existing apparatus.
One step at at time!)

For classes with just one superclass+method, we use a newtype decl instead:

	class C a where
	  op :: forallb. a -> b -> b

generates

	newtype CDict a = CDict (forall b. a -> b -> b)

Now DictTy in Type is just a form of type synomym: 
	DictTy c t = TyConTy CDict `AppTy` t

Death to "ExpandingDicts".


\begin{code}
tcClassDecl1 rec_inst_mapper
      	     (ClassDecl context class_name
			tyvar_name class_sigs def_methods pragmas src_loc)
  = tcAddSrcLoc src_loc	$
    tcAddErrCtxt (classDeclCtxt class_name) $

	-- LOOK THINGS UP IN THE ENVIRONMENT
    tcLookupClass class_name			`thenTc` \ (class_kind, rec_class) ->
    tcLookupTyVar (getTyVarName tyvar_name)	`thenNF_Tc` \ (tyvar_kind, rec_tyvar) ->
    let
	(rec_class_inst_env, rec_class_op_inst_fn) = rec_inst_mapper rec_class
    in

	-- FORCE THE CLASS AND ITS TYVAR TO HAVE SAME KIND
    unifyKind class_kind tyvar_kind	`thenTc_`

	-- CHECK THE CONTEXT
    tcClassContext rec_class rec_tyvar context pragmas	
				`thenTc` \ (scs, sc_sel_ids) ->

	-- CHECK THE CLASS SIGNATURES,
    mapTc (tcClassSig rec_class rec_tyvar rec_class_op_inst_fn) class_sigs
				`thenTc` \ sig_stuff ->

	-- MAKE THE CLASS OBJECT ITSELF
    let
	(ops, op_sel_ids, defm_ids) = unzip3 sig_stuff
	clas = mkClass (uniqueOf class_name) (getName class_name) rec_tyvar
		       scs sc_sel_ids ops op_sel_ids defm_ids
		       rec_class_inst_env
    in
    returnTc clas
\end{code}


    let
	clas_ty = mkTyVarTy clas_tyvar
	dict_component_tys = [mkDictTy sc clas_ty | sc <- scs] ++
			     [classOpLocalType op | op <- ops])
 	new_or_data = case dict_component_tys of
			[_]   -> NewType
			other -> DataType

        dict_con_id = mkDataCon class_name
			   [NotMarkedStrict]
			   [{- No labelled fields -}]
		      	   [clas_tyvar]
		      	   [{-No context-}]
			   dict_component_tys
		      	   tycon

	tycon = mkDataTyCon class_name
			    (tyVarKind rec_tyvar `mkArrowKind` mkBoxedTypeKind)
			    [rec_tyvar]
			    [{- Empty context -}]
			    [dict_con_id]
			    [{- No derived classes -}]
			    new_or_data
    in


\begin{code}
tcClassContext :: Class -> TyVar
	       -> RenamedContext 	-- class context
	       -> RenamedClassPragmas	-- pragmas for superclasses  
	       -> TcM s ([Class],	-- the superclasses
			 [Id])  	-- superclass selector Ids

tcClassContext rec_class rec_tyvar context pragmas
  = 	-- Check the context.
	-- The renamer has already checked that the context mentions
	-- only the type variable of the class decl.
    tcContext context			`thenTc` \ theta ->
    let
      super_classes = [ supers | (supers, _) <- theta ]
    in

	-- Make super-class selector ids
    mapTc (mk_super_id rec_class) super_classes	`thenTc` \ sc_sel_ids ->

	-- Done
    returnTc (super_classes, sc_sel_ids)

  where
    rec_tyvar_ty = mkTyVarTy rec_tyvar

    mk_super_id rec_class super_class
        = tcGetUnique			`thenNF_Tc` \ uniq ->
	  let
		ty = mkForAllTy rec_tyvar $
		     mkFunTy (mkDictTy rec_class   rec_tyvar_ty)
			     (mkDictTy super_class rec_tyvar_ty)
	  in
	  returnTc (mkSuperDictSelId uniq rec_class super_class ty)


tcClassSig :: Class	    		-- Knot tying only!
	   -> TyVar		 	-- The class type variable, used for error check only
	   -> (ClassOp -> SpecEnv)	-- Ditto; the spec info for the class ops
	   -> RenamedClassOpSig
	   -> TcM s (ClassOp,		-- class op
		     Id,		-- selector id
		     Id)		-- default-method ids

tcClassSig rec_clas rec_clas_tyvar rec_classop_spec_fn
	   (ClassOpSig op_name dm_name
		       op_ty
		       src_loc)
  = tcAddSrcLoc src_loc $
    fixTc ( \ ~(_, rec_sel_id, rec_defm_id) ->	-- Knot for pragmas

	-- Check the type signature.  NB that the envt *already has*
	-- bindings for the type variables; see comments in TcTyAndClassDcls.

    -- NB: Renamer checks that the class type variable is mentioned in local_ty,
    -- and that it is not constrained by theta
    tcHsType op_ty				`thenTc` \ local_ty ->
    let
	global_ty   = mkSigmaTy [rec_clas_tyvar] 
			        [(rec_clas, mkTyVarTy rec_clas_tyvar)]
			        local_ty
	class_op_nm = getOccName op_name
	class_op    = mkClassOp class_op_nm
				(classOpTagByOccName rec_clas{-yeeps!-} class_op_nm)
				local_ty
    in

	-- Build the selector id and default method id
    let
	sel_id  = mkMethodSelId     op_name rec_clas class_op       global_ty
	defm_id = mkDefaultMethodId dm_name rec_clas class_op False global_ty
			-- ToDo: improve the "False"
    in
    tcAddImportedIdInfo defm_id			`thenNF_Tc` \ final_defm_id ->
    returnTc (class_op, sel_id, final_defm_id)
    )
\end{code}


%************************************************************************
%*									*
\subsection[ClassDcl-pass2]{Class decls pass 2: default methods}
%*									*
%************************************************************************

The purpose of pass 2 is
\begin{enumerate}
\item
to beat on the explicitly-provided default-method decls (if any),
using them to produce a complete set of default-method decls.
(Omitted ones elicit an error message.)
\item
to produce a definition for the selector function for each method
and superclass dictionary.
\end{enumerate}

Pass~2 only applies to locally-defined class declarations.

The function @tcClassDecls2@ just arranges to apply @tcClassDecl2@ to
each local class decl.

\begin{code}
tcClassDecls2 :: [RenamedHsDecl]
	      -> NF_TcM s (LIE s, TcHsBinds s)

tcClassDecls2 decls
  = foldr combine
	  (returnNF_Tc (emptyLIE, EmptyBinds))
	  [tcClassDecl2 cls_decl | ClD cls_decl <- decls]
  where
    combine tc1 tc2 = tc1 `thenNF_Tc` \ (lie1, binds1) ->
		      tc2 `thenNF_Tc` \ (lie2, binds2) ->
		      returnNF_Tc (lie1 `plusLIE` lie2,
				   binds1 `ThenBinds` binds2)
\end{code}

@tcClassDecl2@ is the business end of things.

\begin{code}
tcClassDecl2 :: RenamedClassDecl	-- The class declaration
	     -> NF_TcM s (LIE s, TcHsBinds s)

tcClassDecl2 (ClassDecl context class_name
			tyvar_name class_sigs default_binds pragmas src_loc)

  | not (isLocallyDefined class_name)
  = returnNF_Tc (emptyLIE, EmptyBinds)

  | otherwise	-- It is locally defined
  = recoverNF_Tc (returnNF_Tc (emptyLIE, EmptyBinds)) $
    tcAddSrcLoc src_loc		     		      $

	-- Get the relevant class
    tcLookupClass class_name		`thenTc` \ (_, clas) ->
    let
	(tyvar, scs, sc_sel_ids, ops, op_sel_ids, defm_ids)
	  = classBigSig clas

	-- The selector binds are already in the selector Id's unfoldings
	sel_binds = [ CoreMonoBind (RealId sel_id) (getUnfoldingTemplate (getIdUnfolding sel_id))
		    | sel_id <- sc_sel_ids ++ op_sel_ids, 
		      isLocallyDefined sel_id
		    ]

    	final_sel_binds = MonoBind (andMonoBinds sel_binds) [] nonRecursive 
    in
	-- Generate bindings for the default methods
    tcInstSigTyVars [tyvar]		`thenNF_Tc` \ ([clas_tyvar], _, _) ->
    mapAndUnzipTc (buildDefaultMethodBind clas clas_tyvar default_binds) 
		  (op_sel_ids `zip` [0..])
					`thenTc` \ (const_insts_s, meth_binds) ->

    returnTc (unionManyBags const_insts_s, 
	      final_sel_binds `ThenBinds`
	      MonoBind (andMonoBinds meth_binds) [] nonRecursive)
\end{code}

%************************************************************************
%*									*
\subsection[Default methods]{Default methods}
%*									*
%************************************************************************

The default methods for a class are each passed a dictionary for the
class, so that they get access to the other methods at the same type.
So, given the class decl
\begin{verbatim}
class Foo a where
	op1 :: a -> Bool
	op2 :: Ord b => a -> b -> b -> b

	op1 x = True
	op2 x y z = if (op1 x) && (y < z) then y else z
\end{verbatim}
we get the default methods:
\begin{verbatim}
defm.Foo.op1 :: forall a. Foo a => a -> Bool
defm.Foo.op1 = /\a -> \dfoo -> \x -> True

====================== OLD ==================
\begin{verbatim}
defm.Foo.op2 :: forall a, b. (Foo a, Ord b) => a -> b -> b -> b
defm.Foo.op2 = /\ a b -> \ dfoo dord -> \x y z ->
		  if (op1 a dfoo x) && (< b dord y z) then y else z
\end{verbatim}
Notice that, like all ids, the foralls of defm.Foo.op2 are at the top.
====================== END OF OLD ===================

NEW:
\begin{verbatim}
defm.Foo.op2 :: forall a. Foo a => forall b. Ord b => a -> b -> b -> b
defm.Foo.op2 = /\ a -> \ dfoo -> /\ b -> \ dord -> \x y z ->
		  if (op1 a dfoo x) && (< b dord y z) then y else z
\end{verbatim}


When we come across an instance decl, we may need to use the default
methods:
\begin{verbatim}
instance Foo Int where {}
\end{verbatim}
gives
\begin{verbatim}
const.Foo.Int.op1 :: Int -> Bool
const.Foo.Int.op1 = defm.Foo.op1 Int dfun.Foo.Int

const.Foo.Int.op2 :: forall b. Ord b => Int -> b -> b -> b
const.Foo.Int.op2 = defm.Foo.op2 Int dfun.Foo.Int

dfun.Foo.Int :: Foo Int
dfun.Foo.Int = (const.Foo.Int.op1, const.Foo.Int.op2)
\end{verbatim}
Notice that, as with method selectors above, we assume that dictionary
application is curried, so there's no need to mention the Ord dictionary
in const.Foo.Int.op2 (or the type variable).

\begin{verbatim}
instance Foo a => Foo [a] where {}

dfun.Foo.List :: forall a. Foo a -> Foo [a]
dfun.Foo.List
  = /\ a -> \ dfoo_a ->
    let rec
	op1 = defm.Foo.op1 [a] dfoo_list
	op2 = defm.Foo.op2 [a] dfoo_list
	dfoo_list = (op1, op2)
    in
	dfoo_list
\end{verbatim}

\begin{code}
buildDefaultMethodBind
	:: Class
	-> TcTyVar s
	-> RenamedMonoBinds
	-> (Id, Int)
	-> TcM s (LIE s, TcMonoBinds s)

buildDefaultMethodBind clas clas_tyvar default_binds (sel_id, idx)
  = newDicts origin [(clas,inst_ty)]			`thenNF_Tc` \ (this_dict, [this_dict_id]) ->
    let
	avail_insts = this_dict
	defm_id     = classDefaultMethodId clas idx
    in
    tcExtendGlobalTyVars clas_tyvar_set (
	tcMethodBind noDefmExpr inst_ty default_binds (sel_id, idx)
    )						`thenTc` \ (defm_bind, insts_needed, (_, local_defm_id)) ->

	-- CHECK THE CONTEXT OF THE DEFAULT-METHOD BINDS
    tcSimplifyAndCheck
	clas_tyvar_set
	avail_insts
	insts_needed			`thenTc` \ (const_lie, dict_binds) ->

    let
	defm_binds = AbsBinds
		 	[clas_tyvar]
			[this_dict_id]
			[([clas_tyvar], RealId defm_id, local_defm_id)]
			(dict_binds `AndMonoBinds` defm_bind)
    in
    returnTc (const_lie, defm_binds)

  where
    clas_tyvar_set    = unitTyVarSet clas_tyvar
    inst_ty           = mkTyVarTy clas_tyvar
    origin            = ClassDeclOrigin
    noDefmExpr _      = HsApp (HsVar (getName nO_DEFAULT_METHOD_ERROR_ID)) 
			      (HsLit (HsString (_PK_ error_msg)))

    error_msg = show (sep [text "Class",  ppr (PprForUser opt_PprUserLength) clas,
				  text "Method", ppr (PprForUser opt_PprUserLength) sel_id])
\end{code}



Contexts
~~~~~~~~
\begin{code}
classDeclCtxt class_name sty
  = hsep [ptext SLIT("In the class declaration for"), ppr sty class_name]
\end{code}
