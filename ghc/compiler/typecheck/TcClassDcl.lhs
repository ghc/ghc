%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcClassDcl]{Typechecking class declarations}

\begin{code}
#include "HsVersions.h"

module TcClassDcl ( tcClassDecl1, tcClassDecls2, tcMethodBind ) where

IMP_Ubiq()

import HsSyn		( HsDecl(..), ClassDecl(..), HsBinds(..), MonoBinds(..),
			  Match(..), GRHSsAndBinds(..), GRHS(..), HsExpr(..), 
			  DefaultDecl, TyDecl, InstDecl, IfaceSig, Fixity,
			  HsLit(..), OutPat(..), Sig(..), HsType(..), HsTyVar, InPat(..),
			  SYN_IE(RecFlag), nonRecursive, andMonoBinds, collectMonoBinders,
			  Stmt, DoOrListComp, ArithSeqInfo, Fake )
import HsTypes		( getTyVarName )
import HsPragmas	( ClassPragmas(..) )
import RnHsSyn		( RenamedClassDecl(..), RenamedClassPragmas(..),
			  RenamedClassOpSig(..), SYN_IE(RenamedMonoBinds),
			  RenamedGenPragmas(..), RenamedContext(..), SYN_IE(RenamedHsDecl)
			)
import TcHsSyn		( SYN_IE(TcHsBinds), SYN_IE(TcMonoBinds), SYN_IE(TcExpr),
			  mkHsTyApp, mkHsTyLam, mkHsDictApp, mkHsDictLam, tcIdType )

import Inst		( Inst, InstOrigin(..), SYN_IE(LIE), emptyLIE, plusLIE, newDicts, newMethod )
import TcEnv		( tcLookupClass, tcLookupTyVar, newLocalIds, tcAddImportedIdInfo,
			  tcExtendGlobalTyVars )
import TcBinds		( tcBindWithSigs, TcSigInfo(..) )
import TcKind		( unifyKind, TcKind )
import TcMonad
import TcMonoType	( tcHsType, tcContext )
import TcSimplify	( tcSimplifyAndCheck )
import TcType		( TcIdOcc(..), SYN_IE(TcType), SYN_IE(TcTyVar), tcInstType, tcInstSigTyVars, 
			  tcInstSigType, tcInstSigTcType )
import PragmaInfo	( PragmaInfo(..) )

import Bag		( bagToList, unionManyBags )
import Class		( GenClass, mkClass, classBigSig, 
			  classDefaultMethodId,
			  classOpTagByOccName, SYN_IE(Class)
			)
import CmdLineOpts      ( opt_PprUserLength )
import Id		( GenId, mkSuperDictSelId, mkMethodSelId, 
			  mkDefaultMethodId, getIdUnfolding,
			  idType, SYN_IE(Id)
			)
import CoreUnfold	( getUnfoldingTemplate )
import IdInfo
import Name		( Name, isLocallyDefined, moduleString, getSrcLoc, nameOccName,
			  nameString, NamedThing(..) )
import Outputable
import Pretty
import PprType		( GenClass, GenType, GenTyVar )
import SpecEnv		( SpecEnv )
import SrcLoc		( mkGeneratedSrcLoc )
import Type		( mkFunTy, mkTyVarTy, mkTyVarTys, mkDictTy, splitRhoTy,
			  mkForAllTy, mkSigmaTy, splitSigmaTy, SYN_IE(Type)
			)
import TysWiredIn	( stringTy )
import TyVar		( unitTyVarSet, GenTyVar, SYN_IE(TyVar) )
import Unique		( Unique, Uniquable(..) )
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
tcClassDecl1 rec_env rec_inst_mapper
      	     (ClassDecl context class_name
			tyvar_name class_sigs def_methods pragmas src_loc)
  = tcAddSrcLoc src_loc	$
    tcAddErrCtxt (classDeclCtxt class_name) $

	-- LOOK THINGS UP IN THE ENVIRONMENT
    tcLookupClass class_name			`thenTc` \ (class_kind, rec_class) ->
    tcLookupTyVar (getTyVarName tyvar_name)	`thenNF_Tc` \ (tyvar_kind, rec_tyvar) ->
    let
	rec_class_inst_env = rec_inst_mapper rec_class
    in

	-- FORCE THE CLASS AND ITS TYVAR TO HAVE SAME KIND
    unifyKind class_kind tyvar_kind	`thenTc_`

	-- CHECK THE CONTEXT
    tcClassContext rec_class rec_tyvar context pragmas	
				`thenTc` \ (scs, sc_sel_ids) ->

	-- CHECK THE CLASS SIGNATURES,
    mapTc (tcClassSig rec_env rec_class rec_tyvar) class_sigs
				`thenTc` \ sig_stuff ->

	-- MAKE THE CLASS OBJECT ITSELF
    let
	(op_sel_ids, defm_ids) = unzip sig_stuff
	clas = mkClass (uniqueOf class_name) (getName class_name) rec_tyvar
		       scs sc_sel_ids op_sel_ids defm_ids
		       rec_class_inst_env
    in
    returnTc clas
\end{code}


    let
	clas_ty = mkTyVarTy clas_tyvar
	dict_component_tys = classDictArgTys clas_ty
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


tcClassSig :: TcEnv s			-- Knot tying only!
	   -> Class	    		-- ...ditto...
	   -> TyVar		 	-- The class type variable, used for error check only
	   -> RenamedClassOpSig
	   -> TcM s (Id,		-- selector id
		     Maybe Id)		-- default-method ids

tcClassSig rec_env rec_clas rec_clas_tyvar
	   (ClassOpSig op_name maybe_dm_name
		       op_ty
		       src_loc)
  = tcAddSrcLoc src_loc $

	-- Check the type signature.  NB that the envt *already has*
	-- bindings for the type variables; see comments in TcTyAndClassDcls.

    -- NB: Renamer checks that the class type variable is mentioned in local_ty,
    -- and that it is not constrained by theta
    tcHsType op_ty				`thenTc` \ local_ty ->
    let
	global_ty   = mkSigmaTy [rec_clas_tyvar] 
			        [(rec_clas, mkTyVarTy rec_clas_tyvar)]
			        local_ty
    in

	-- Build the selector id and default method id
    let
	sel_id      = mkMethodSelId op_name rec_clas global_ty
	maybe_dm_id = case maybe_dm_name of
			   Nothing      -> Nothing
			   Just dm_name -> let 
					     dm_id = mkDefaultMethodId dm_name rec_clas global_ty
					   in
					   Just (tcAddImportedIdInfo rec_env dm_id)
    in
    returnTc (sel_id, maybe_dm_id)
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
	      -> NF_TcM s (LIE s, TcMonoBinds s)

tcClassDecls2 decls
  = foldr combine
	  (returnNF_Tc (emptyLIE, EmptyMonoBinds))
	  [tcClassDecl2 cls_decl | ClD cls_decl <- decls]
  where
    combine tc1 tc2 = tc1 `thenNF_Tc` \ (lie1, binds1) ->
		      tc2 `thenNF_Tc` \ (lie2, binds2) ->
		      returnNF_Tc (lie1 `plusLIE` lie2,
				   binds1 `AndMonoBinds` binds2)
\end{code}

@tcClassDecl2@ is the business end of things.

\begin{code}
tcClassDecl2 :: RenamedClassDecl	-- The class declaration
	     -> NF_TcM s (LIE s, TcMonoBinds s)

tcClassDecl2 (ClassDecl context class_name
			tyvar_name class_sigs default_binds pragmas src_loc)

  | not (isLocallyDefined class_name)
  = returnNF_Tc (emptyLIE, EmptyMonoBinds)

  | otherwise	-- It is locally defined
  = recoverNF_Tc (returnNF_Tc (emptyLIE, EmptyMonoBinds)) $ 
    tcAddSrcLoc src_loc		     		          $

	-- Get the relevant class
    tcLookupClass class_name		`thenTc` \ (_, clas) ->
    let
	(tyvar, scs, sc_sel_ids, op_sel_ids, defm_ids) = classBigSig clas

	-- The selector binds are already in the selector Id's unfoldings
	sel_binds = [ CoreMonoBind (RealId sel_id) (getUnfoldingTemplate (getIdUnfolding sel_id))
		    | sel_id <- sc_sel_ids ++ op_sel_ids, 
		      isLocallyDefined sel_id
		    ]

    	final_sel_binds = andMonoBinds sel_binds
    in
	-- Generate bindings for the default methods
    tcDefaultMethodBinds clas default_binds		`thenTc` \ (const_insts, meth_binds) ->

    returnTc (const_insts, 
	      final_sel_binds `AndMonoBinds` meth_binds)
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
tcDefaultMethodBinds
	:: Class
	-> RenamedMonoBinds
	-> TcM s (LIE s, TcMonoBinds s)

tcDefaultMethodBinds clas default_binds
  = 	-- Construct suitable signatures
    tcInstSigTyVars [tyvar]		`thenNF_Tc` \ ([clas_tyvar], [inst_ty], inst_env) ->

	-- Typecheck the default bindings
    let
	clas_tyvar_set = unitTyVarSet clas_tyvar

	tc_dm meth_bind
	  = let
		bndr_name  = case meth_bind of
				FunMonoBind name _ _ _		-> name
				PatMonoBind (VarPatIn name) _ _ -> name
				
		idx    	   = classOpTagByOccName clas (nameOccName bndr_name) - 1
		sel_id 	   = op_sel_ids !! idx
		Just dm_id = defm_ids !! idx
	    in
	    tcMethodBind clas origin inst_ty sel_id meth_bind
						`thenTc` \ (bind, insts, (_, local_dm_id)) ->
	    returnTc (bind, insts, ([clas_tyvar], RealId dm_id, local_dm_id))
    in	   
    tcExtendGlobalTyVars clas_tyvar_set (
	mapAndUnzip3Tc tc_dm (flatten default_binds [])
    )						`thenTc` \ (defm_binds, insts_needed, abs_bind_stuff) ->

	-- Check the context
    newDicts origin [(clas,inst_ty)]		`thenNF_Tc` \ (this_dict, [this_dict_id]) ->
    let
	avail_insts   = this_dict
    in
    tcSimplifyAndCheck
	clas_tyvar_set
	avail_insts
	(unionManyBags insts_needed)		`thenTc` \ (const_lie, dict_binds) ->

    let
	full_binds = AbsBinds
		 	[clas_tyvar]
			[this_dict_id]
			abs_bind_stuff
			(dict_binds `AndMonoBinds` andMonoBinds defm_binds)
    in
    returnTc (const_lie, full_binds)

  where
    (tyvar, scs, sc_sel_ids, op_sel_ids, defm_ids) = classBigSig clas
    origin = ClassDeclOrigin

    flatten EmptyMonoBinds rest	      = rest
    flatten (AndMonoBinds b1 b2) rest = flatten b1 (flatten b2 rest)
    flatten a_bind rest		      = a_bind : rest
\end{code}

@tcMethodBind@ is used to type-check both default-method and
instance-decl method declarations.  We must type-check methods one at a
time, because their signatures may have different contexts and
tyvar sets.

\begin{code}
tcMethodBind 
	:: Class
	-> InstOrigin s
	-> TcType s					-- Instance type
	-> Id						-- The method selector
	-> RenamedMonoBinds				-- Method binding (just one)
	-> TcM s (TcMonoBinds s, LIE s, (LIE s, TcIdOcc s))

tcMethodBind clas origin inst_ty sel_id meth_bind
 = tcAddSrcLoc src_loc	 		        $
   newMethod origin (RealId sel_id) [inst_ty]	`thenNF_Tc` \ meth@(_, TcId local_meth_id) ->
   tcInstSigTcType (idType local_meth_id)	`thenNF_Tc` \ (tyvars', rho_ty') ->
   let
	(theta', tau')  = splitRhoTy rho_ty'
	sig_info        = TySigInfo bndr_name local_meth_id tyvars' theta' tau' src_loc
   in
   tcBindWithSigs [bndr_name] meth_bind [sig_info]
		  nonRecursive (\_ -> NoPragmaInfo)	`thenTc` \ (binds, insts, _) ->

   returnTc (binds, insts, meth)
  where
   (bndr_name, src_loc) = case meth_bind of
				FunMonoBind name _ _ loc	  -> (name, loc)
				PatMonoBind (VarPatIn name) _ loc -> (name, loc)
\end{code}

Contexts
~~~~~~~~
\begin{code}
classDeclCtxt class_name sty
  = hsep [ptext SLIT("In the class declaration for"), ppr sty class_name]
\end{code}
