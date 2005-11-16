%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\begin{code}
module BuildTyCl (
	buildSynTyCon, buildAlgTyCon, buildDataCon,
	buildClass,
	mkAbstractTyConRhs, mkNewTyConRhs, mkDataTyConRhs
    ) where

#include "HsVersions.h"

import IfaceEnv		( newImplicitBinder )
import TcRnMonad

import DataCon		( DataCon, isNullarySrcDataCon, dataConTyVars,
			  mkDataCon, dataConFieldLabels, dataConOrigArgTys )
import Var		( tyVarKind, TyVar, Id )
import VarSet		( isEmptyVarSet, intersectVarSet, elemVarSet )
import TysWiredIn  	( unitTy )
import BasicTypes	( RecFlag, StrictnessMark(..) )
import Name		( Name )
import OccName		( mkDataConWrapperOcc, mkDataConWorkerOcc, mkClassTyConOcc,
			  mkClassDataConOcc, mkSuperDictSelOcc )
import MkId		( mkDataConIds, mkRecordSelId, mkDictSelId )
import Class		( mkClass, Class( classTyCon), FunDep, DefMeth(..) )
import TyCon		( mkSynTyCon, mkAlgTyCon, visibleDataCons, tyConStupidTheta,
			  tyConDataCons, isNewTyCon, mkClassTyCon, TyCon( tyConTyVars ),
			  isRecursiveTyCon,
			  ArgVrcs, AlgTyConRhs(..), newTyConRhs )
import Type		( mkArrowKinds, liftedTypeKind, typeKind, 
			  tyVarsOfType, tyVarsOfTypes, tyVarsOfPred,
			  splitTyConApp_maybe, splitAppTy_maybe, getTyVar_maybe,
			  mkPredTys, mkTyVarTys, ThetaType, Type, 
			  substTyWith, zipTopTvSubst, substTheta )
import Outputable
import List		( nub )

\end{code}
	

\begin{code}
------------------------------------------------------
buildSynTyCon name tvs rhs_ty arg_vrcs
  = mkSynTyCon name kind tvs rhs_ty arg_vrcs
  where
    kind = mkArrowKinds (map tyVarKind tvs) (typeKind rhs_ty)


------------------------------------------------------
buildAlgTyCon :: Name -> [TyVar] 
	      -> ThetaType		-- Stupid theta
	      -> AlgTyConRhs
	      -> ArgVrcs -> RecFlag
	      -> Bool			-- True <=> want generics functions
	      -> TcRnIf m n TyCon

buildAlgTyCon tc_name tvs stupid_theta rhs arg_vrcs is_rec want_generics
  = do	{ let { tycon = mkAlgTyCon tc_name kind tvs arg_vrcs stupid_theta
				   rhs fields is_rec want_generics
	      ; kind    = mkArrowKinds (map tyVarKind tvs) liftedTypeKind
	      ; fields  = mkTyConSelIds tycon rhs
	  }
	; return tycon }

------------------------------------------------------
mkAbstractTyConRhs :: AlgTyConRhs
mkAbstractTyConRhs = AbstractTyCon

mkDataTyConRhs :: [DataCon] -> AlgTyConRhs
mkDataTyConRhs cons
  = DataTyCon { data_cons = cons, is_enum = all isNullarySrcDataCon cons }

mkNewTyConRhs :: TyCon -> DataCon -> AlgTyConRhs
mkNewTyConRhs tycon con 
  = NewTyCon { data_con = con, 
	       nt_rhs = rhs_ty,
	       nt_etad_rhs = eta_reduce tvs rhs_ty,
	       nt_rep = mkNewTyConRep tycon rhs_ty }
  where
    tvs    = dataConTyVars con
    rhs_ty = head (dataConOrigArgTys con)
	-- Newtypes are guaranteed vanilla, so OrigArgTys will do

    eta_reduce [] ty = ([], ty)
    eta_reduce (a:as) ty | null as', 
			   Just (fun, arg) <- splitAppTy_maybe ty',
			   Just tv <- getTyVar_maybe arg,
			   tv == a,
			   not (a `elemVarSet` tyVarsOfType fun)
			 = ([], fun)	-- Successful eta reduction
			 | otherwise
			 = (a:as', ty')
	where
  	  (as', ty') = eta_reduce as ty
				
mkNewTyConRep :: TyCon		-- The original type constructor
	      -> Type		-- The arg type of its constructor
	      -> Type		-- Chosen representation type
-- The "representation type" is guaranteed not to be another newtype
-- at the outermost level; but it might have newtypes in type arguments

-- Find the representation type for this newtype TyCon
-- Remember that the representation type is the *ultimate* representation
-- type, looking through other newtypes.
-- 
-- The non-recursive newtypes are easy, because they look transparent
-- to splitTyConApp_maybe, but recursive ones really are represented as
-- TyConApps (see TypeRep).
-- 
-- The trick is to to deal correctly with recursive newtypes
-- such as	newtype T = MkT T

mkNewTyConRep tc rhs_ty
  | null (tyConDataCons tc) = unitTy
	-- External Core programs can have newtypes with no data constructors
  | otherwise		    = go [tc] rhs_ty
  where
	-- Invariant: tcs have been seen before
    go tcs rep_ty 
	= case splitTyConApp_maybe rep_ty of
	    Just (tc, tys)
		| tc `elem` tcs -> unitTy	-- Recursive loop
		| isNewTyCon tc -> ASSERT( isRecursiveTyCon tc )
					-- Non-recursive ones have been 
					-- dealt with by splitTyConApp_maybe
				   go (tc:tcs) (substTyWith tvs tys rhs_ty)
		where
		  (tvs, rhs_ty) = newTyConRhs tc

	    other -> rep_ty 

------------------------------------------------------
buildDataCon :: Name -> Bool -> Bool
	    -> [StrictnessMark] 
	    -> [Name]			-- Field labels
	    -> [TyVar] 
	    -> ThetaType		-- Does not include the "stupid theta"
	    -> [Type] -> TyCon -> [Type]
	    -> TcRnIf m n DataCon
-- A wrapper for DataCon.mkDataCon that
--   a) makes the worker Id
--   b) makes the wrapper Id if necessary, including
--	allocating its unique (hence monadic)
buildDataCon src_name declared_infix vanilla arg_stricts field_lbls
	     tyvars ctxt arg_tys tycon res_tys
  = do	{ wrap_name <- newImplicitBinder src_name mkDataConWrapperOcc
	; work_name <- newImplicitBinder src_name mkDataConWorkerOcc
	-- This last one takes the name of the data constructor in the source
	-- code, which (for Haskell source anyway) will be in the DataName name
	-- space, and puts it into the VarName name space

	; let
		stupid_ctxt = mkDataConStupidTheta tycon arg_tys res_tys
		data_con = mkDataCon src_name declared_infix vanilla
				     arg_stricts field_lbls
				     tyvars stupid_ctxt ctxt
				     arg_tys tycon res_tys dc_ids
		dc_ids = mkDataConIds wrap_name work_name data_con

	; returnM data_con }


-- The stupid context for a data constructor should be limited to
-- the type variables mentioned in the arg_tys
mkDataConStupidTheta tycon arg_tys res_tys
  | null stupid_theta = []	-- The common case
  | otherwise 	      = filter in_arg_tys stupid_theta
  where
    tc_subst	    = zipTopTvSubst (tyConTyVars tycon) res_tys
    stupid_theta    = substTheta tc_subst (tyConStupidTheta tycon)
	-- Start by instantiating the master copy of the 
	-- stupid theta, taken from the TyCon

    arg_tyvars      = tyVarsOfTypes arg_tys
    in_arg_tys pred = not $ isEmptyVarSet $ 
			tyVarsOfPred pred `intersectVarSet` arg_tyvars

------------------------------------------------------
mkTyConSelIds :: TyCon -> AlgTyConRhs -> [Id]
mkTyConSelIds tycon rhs
  =  [ mkRecordSelId tycon fld 
     | fld <- nub (concatMap dataConFieldLabels (visibleDataCons rhs)) ]
	-- We'll check later that fields with the same name 
	-- from different constructors have the same type.
\end{code}


------------------------------------------------------
\begin{code}
buildClass :: Name -> [TyVar] -> ThetaType
	   -> [FunDep TyVar]		-- Functional dependencies
	   -> [(Name, DefMeth, Type)]	-- Method info
	   -> RecFlag -> ArgVrcs	-- Info for type constructor
	   -> TcRnIf m n Class

buildClass class_name tvs sc_theta fds sig_stuff tc_isrec tc_vrcs
  = do	{ tycon_name <- newImplicitBinder class_name mkClassTyConOcc
	; datacon_name <- newImplicitBinder class_name mkClassDataConOcc
		-- The class name is the 'parent' for this datacon, not its tycon,
		-- because one should import the class to get the binding for 
		-- the datacon
	; sc_sel_names <- mapM (newImplicitBinder class_name . mkSuperDictSelOcc) 
				[1..length sc_theta]
	      -- We number off the superclass selectors, 1, 2, 3 etc so that we 
	      -- can construct names for the selectors.  Thus
	      --      class (C a, C b) => D a b where ...
	      -- gives superclass selectors
	      --      D_sc1, D_sc2
	      -- (We used to call them D_C, but now we can have two different
	      --  superclasses both called C!)

	; fixM (\ clas -> do {	-- Only name generation inside loop

	  let { op_tys		   = [ty | (_,_,ty) <- sig_stuff]
	      ; sc_tys		   = mkPredTys sc_theta
	      ;	dict_component_tys = sc_tys ++ op_tys
	      ; sc_sel_ids	   = [mkDictSelId sc_name clas | sc_name <- sc_sel_names]
	      ; op_items = [ (mkDictSelId op_name clas, dm_info)
			   | (op_name, dm_info, _) <- sig_stuff ] }
	  		-- Build the selector id and default method id

	; dict_con <- buildDataCon datacon_name 
				   False 	-- Not declared infix
				   True		-- Is vanilla; tyvars same as tycon
				   (map (const NotMarkedStrict) dict_component_tys)
				   [{- No labelled fields -}]
				   tvs [{-No context-}] dict_component_tys
				   (classTyCon clas) (mkTyVarTys tvs)

	; let {	clas = mkClass class_name tvs fds
		       sc_theta sc_sel_ids op_items
		       tycon

	      ;	tycon = mkClassTyCon tycon_name clas_kind tvs
                             tc_vrcs rhs clas tc_isrec
		-- A class can be recursive, and in the case of newtypes 
		-- this matters.  For example
		-- 	class C a where { op :: C b => a -> b -> Int }
		-- Because C has only one operation, it is represented by
		-- a newtype, and it should be a *recursive* newtype.
		-- [If we don't make it a recursive newtype, we'll expand the
		-- newtype like a synonym, but that will lead to an infinite type]

	      ; clas_kind = mkArrowKinds (map tyVarKind tvs) liftedTypeKind

 	      ; rhs = case dict_component_tys of
			    [rep_ty] -> mkNewTyConRhs tycon dict_con
			    other    -> mkDataTyConRhs [dict_con]
	      }
	; return clas
	})}
\end{code}


