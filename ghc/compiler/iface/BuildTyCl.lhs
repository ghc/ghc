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

import Subst		( substTyWith )
import Util		( zipLazy )
import FieldLabel	( allFieldLabelTags, mkFieldLabel, fieldLabelName )
import VarSet
import DataCon		( DataCon, dataConTyCon, dataConOrigArgTys, mkDataCon, dataConFieldLabels )
import Var		( tyVarKind, TyVar, Id )
import TysWiredIn  	( unitTy )
import BasicTypes	( RecFlag, StrictnessMark(..) )
import Name		( Name )
import OccName		( mkDataConWrapperOcc, mkDataConWorkerOcc, mkClassTyConOcc,
			  mkClassDataConOcc, mkSuperDictSelOcc )
import MkId		( mkDataConIds, mkRecordSelId, mkDictSelId )
import Class		( mkClass, Class( classTyCon), FunDep, DefMeth(..) )
import TyCon		( mkSynTyCon, mkAlgTyCon, visibleDataCons,
			  tyConDataCons, isNewTyCon, mkClassTyCon, TyCon( tyConTyVars ),
			  ArgVrcs, AlgTyConRhs(..), newTyConRhs, visibleDataCons )
import Type		( mkArrowKinds, liftedTypeKind, tyVarsOfTypes, typeKind,
			  tyVarsOfPred, splitTyConApp_maybe, mkPredTys, ThetaType, Type )
import Outputable
import List		( nubBy )

\end{code}
	

\begin{code}
------------------------------------------------------
buildSynTyCon name tvs rhs_ty arg_vrcs
  = mkSynTyCon name kind tvs rhs_ty arg_vrcs
  where
    kind = mkArrowKinds (map tyVarKind tvs) (typeKind rhs_ty)


------------------------------------------------------
buildAlgTyCon :: Name -> [TyVar] -> ThetaType
	      -> AlgTyConRhs
	      -> ArgVrcs -> RecFlag
	      -> Bool			-- True <=> want generics functions
	      -> TcRnIf m n TyCon

buildAlgTyCon tc_name tvs ctxt rhs arg_vrcs is_rec want_generics
  = do	{ let { tycon = mkAlgTyCon tc_name kind tvs ctxt arg_vrcs
				   rhs sel_ids is_rec want_generics
	      ; kind    = mkArrowKinds (map tyVarKind tvs) liftedTypeKind
	      ; sel_ids = mkRecordSelectors tycon rhs
	  }
	; return tycon }

------------------------------------------------------
mkAbstractTyConRhs :: AlgTyConRhs
mkAbstractTyConRhs = AbstractTyCon

mkDataTyConRhs :: [DataCon] -> AlgTyConRhs
mkDataTyConRhs cons
  = DataTyCon cons (all is_nullary cons)
  where
    is_nullary con = null (dataConOrigArgTys con)
	-- NB (null . dataConOrigArgTys).  It used to say isNullaryDataCon
	-- but that looks at the *representation* arity, and isEnumerationType
	-- refers to the *source* code definition

mkNewTyConRhs :: DataCon -> AlgTyConRhs
mkNewTyConRhs con 
  = NewTyCon con 				-- The constructor
	     (head (dataConOrigArgTys con))	-- The RHS type
	     (mkNewTyConRep (dataConTyCon con))	-- The ultimate rep type
				

------------------------------------------------------
buildDataCon :: Name
	    -> [StrictnessMark] 
	    -> [Name]			-- Field labels
	    -> [TyVar] -> ThetaType
	    -> [TyVar] -> ThetaType
	    -> [Type] -> TyCon
	    -> TcRnIf m n DataCon
-- A wrapper for DataCon.mkDataCon that
--   a) makes the worker Id
--   b) makes the wrapper Id if necessary, including
--	allocating its unique (hence monadic)
buildDataCon src_name arg_stricts field_lbl_names 
	     tyvars ctxt ex_tyvars ex_ctxt 
	     arg_tys tycon
  = newImplicitBinder src_name mkDataConWrapperOcc	`thenM` \ wrap_name ->
    newImplicitBinder src_name mkDataConWorkerOcc 	`thenM` \ work_name -> 
	-- This last one takes the name of the data constructor in the source
	-- code, which (for Haskell source anyway) will be in the SrcDataName name
	-- space, and makes it into a "real data constructor name"
    let
		-- Make the FieldLabels
		-- The zipLazy avoids forcing the arg_tys too early
	final_lbls = [ mkFieldLabel name tycon ty tag 
		     | ((name, tag), ty) <- (field_lbl_names `zip` allFieldLabelTags)
					    `zipLazy` arg_tys
		     ]

	ctxt' = thinContext arg_tys ctxt
	data_con = mkDataCon src_name arg_stricts final_lbls
			     tyvars ctxt'
			     ex_tyvars ex_ctxt
			     arg_tys tycon dc_ids
	dc_ids = mkDataConIds wrap_name work_name data_con
    in
    returnM data_con

-- The context for a data constructor should be limited to
-- the type variables mentioned in the arg_tys
thinContext arg_tys ctxt
  = filter in_arg_tys ctxt
  where
      arg_tyvars = tyVarsOfTypes arg_tys
      in_arg_tys pred = not $ isEmptyVarSet $ 
			tyVarsOfPred pred `intersectVarSet` arg_tyvars

------------------------------------------------------
mkRecordSelectors :: TyCon -> AlgTyConRhs -> [Id]
mkRecordSelectors tycon data_cons
  = 	-- We'll check later that fields with the same name 
	-- from different constructors have the same type.
     [ mkRecordSelId tycon field 
     | field <- nubBy eq_name fields ]
  where
    fields = [ field | con <- visibleDataCons data_cons, 
		       field <- dataConFieldLabels con ]
    eq_name field1 field2 = fieldLabelName field1 == fieldLabelName field2
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
				   (map (const NotMarkedStrict) dict_component_tys)
				   [{- No labelled fields -}]
				   tvs [{-No context-}]
				   [{-No existential tyvars-}] [{-Or context-}]
				   dict_component_tys
				   (classTyCon clas)

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
			    [rep_ty] -> mkNewTyConRhs dict_con
			    other    -> mkDataTyConRhs [dict_con]
	      }
	; return clas
	})}
\end{code}


------------------------------------------------------
\begin{code}
mkNewTyConRep :: TyCon		-- The original type constructor
	      -> Type		-- Chosen representation type
				-- (guaranteed not to be another newtype)

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

mkNewTyConRep tc
  | null (tyConDataCons tc) = unitTy
	-- External Core programs can have newtypes with no data constructors
  | otherwise		    = go [] tc
  where
	-- Invariant: tc is a NewTyCon
	-- 	      tcs have been seen before
    go tcs tc 
	| tc `elem` tcs = unitTy
	| otherwise
	= case splitTyConApp_maybe rep_ty of
	    Nothing -> rep_ty 
	    Just (tc', tys) | not (isNewTyCon tc') -> rep_ty
			    | otherwise	           -> go1 (tc:tcs) tc' tys
	where
	  (_,rep_ty) = newTyConRhs tc
	  
    go1 tcs tc tys = substTyWith (tyConTyVars tc) tys (go tcs tc)
\end{code}
