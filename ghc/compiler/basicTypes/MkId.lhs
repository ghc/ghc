%
% (c) The AQUA Project, Glasgow University, 1998
%
\section[StdIdInfo]{Standard unfoldings}

This module contains definitions for the IdInfo for things that
have a standard form, namely:

	* data constructors
	* record selectors
	* method and superclass selectors
	* primitive operations

\begin{code}
module MkId (
	mkSpecPragmaId,	mkWorkerId,

	mkDictFunId, mkDefaultMethodId,
	mkDictSelId,

	mkDataConId, mkDataConWrapId,
	mkRecordSelId,
	mkPrimOpId, mkCCallOpId,

	-- And some particular Ids; see below for why they are wired in
	wiredInIds,
	unsafeCoerceId, realWorldPrimId,
	eRROR_ID, rEC_SEL_ERROR_ID, pAT_ERROR_ID, rEC_CON_ERROR_ID,
	rEC_UPD_ERROR_ID, iRREFUT_PAT_ERROR_ID, nON_EXHAUSTIVE_GUARDS_ERROR_ID,
	nO_METHOD_BINDING_ERROR_ID, aBSENT_ERROR_ID, pAR_ERROR_ID
    ) where

#include "HsVersions.h"


import TysPrim		( openAlphaTyVars, alphaTyVar, alphaTy, 
			  intPrimTy, realWorldStatePrimTy
			)
import TysWiredIn	( boolTy, charTy, mkListTy )
import PrelNames	( pREL_ERR, pREL_GHC )
import PrelRules	( primOpRule )
import Rules		( addRule )
import Type		( Type, ClassContext, mkDictTy, mkDictTys, mkTyConApp, mkTyVarTys,
			  mkFunTys, mkFunTy, mkSigmaTy, classesToPreds,
			  isUnLiftedType, mkForAllTys, mkTyVarTy, tyVarsOfType, tyVarsOfTypes,
			  splitSigmaTy, splitFunTy_maybe, 
			  splitFunTys, splitForAllTys, unUsgTy,
			  mkUsgTy, UsageAnn(..)
			)
import PprType		( pprParendType )
import Module		( Module )
import CoreUtils	( exprType, mkInlineMe )
import CoreUnfold 	( mkTopUnfolding, mkCompulsoryUnfolding, mkOtherCon )
import Literal		( Literal(..) )
import Subst		( mkTopTyVarSubst, substClasses )
import TyCon		( TyCon, isNewTyCon, tyConTyVars, tyConDataCons, isDataTyCon, 
                          tyConTheta, isProductTyCon, isUnboxedTupleTyCon )
import Class		( Class, classBigSig, classTyCon, classTyVars, classSelIds )
import Var		( Id, TyVar )
import VarSet		( isEmptyVarSet )
import Name		( mkDerivedName, mkWiredInIdName, mkLocalName, 
			  mkWorkerOcc, mkSuperDictSelOcc, mkCCallName,
			  Name, NamedThing(..),
			)
import OccName		( mkSrcVarOcc )
import PrimOp		( PrimOp(DataToTagOp, CCallOp), 
			  primOpSig, mkPrimOpIdName,
			  CCall, pprCCallOp
			)
import Demand		( wwStrict, wwPrim, mkStrictnessInfo )
import DataCon		( DataCon, StrictnessMark(..), 
			  dataConFieldLabels, dataConRepArity, dataConTyCon,
			  dataConArgTys, dataConRepType, dataConRepStrictness, 
                          dataConName, dataConTheta,
			  dataConSig, dataConStrictMarks, dataConId
			)
import Id		( idType, mkId,
			  mkVanillaId, mkTemplateLocals,
			  mkTemplateLocal, setInlinePragma, idCprInfo
			)
import IdInfo		( IdInfo, vanillaIdInfo, mkIdInfo,
			  exactArity, setUnfoldingInfo, setCafInfo, setCprInfo,
			  setArityInfo, setInlinePragInfo, setSpecInfo,
			  mkStrictnessInfo, setStrictnessInfo,
			  IdFlavour(..), InlinePragInfo(..), CafInfo(..), StrictnessInfo(..), CprInfo(..)
			)
import FieldLabel	( FieldLabel, FieldLabelTag, mkFieldLabel, fieldLabelName, 
			  firstFieldLabelTag, allFieldLabelTags, fieldLabelType
			)
import CoreSyn
import Maybes
import BasicTypes	( Arity )
import Unique
import Maybe            ( isJust )
import Outputable
import Util		( assoc )
import List		( nub )
\end{code}		


%************************************************************************
%*									*
\subsection{Wired in Ids}
%*									*
%************************************************************************

\begin{code}
wiredInIds
  = [ 	-- These error-y things are wired in because we don't yet have
	-- a way to express in an interface file that the result type variable
	-- is 'open'; that is can be unified with an unboxed type
	-- 
	-- [The interface file format now carry such information, but there's
	--  no way yet of expressing at the definition site for these error-reporting
	--  functions that they have an 'open' result type. -- sof 1/99]

      aBSENT_ERROR_ID
    , eRROR_ID
    , iRREFUT_PAT_ERROR_ID
    , nON_EXHAUSTIVE_GUARDS_ERROR_ID
    , nO_METHOD_BINDING_ERROR_ID
    , pAR_ERROR_ID
    , pAT_ERROR_ID
    , rEC_CON_ERROR_ID
    , rEC_UPD_ERROR_ID

	-- These two can't be defined in Haskell
    , realWorldPrimId
    , unsafeCoerceId
    , getTagId
    ]
\end{code}

%************************************************************************
%*									*
\subsection{Easy ones}
%*									*
%************************************************************************

\begin{code}
mkSpecPragmaId occ uniq ty loc
  = mkId (mkLocalName uniq occ loc) ty (mkIdInfo SpecPragmaId)
	-- Maybe a SysLocal?  But then we'd lose the location

mkDefaultMethodId dm_name rec_c ty
  = mkVanillaId dm_name ty

mkWorkerId uniq unwrkr ty
  = mkVanillaId (mkDerivedName mkWorkerOcc (getName unwrkr) uniq) ty
\end{code}

%************************************************************************
%*									*
\subsection{Data constructors}
%*									*
%************************************************************************

\begin{code}
mkDataConId :: Name -> DataCon -> Id
	-- Makes the *worker* for the data constructor; that is, the function
	-- that takes the reprsentation arguments and builds the constructor.
mkDataConId work_name data_con
  = mkId work_name (dataConRepType data_con) info
  where
    info = mkIdInfo (DataConId data_con)
	   `setArityInfo`	exactArity arity
	   `setStrictnessInfo`	strict_info
	   `setCprInfo`		cpr_info

    arity = dataConRepArity data_con

    strict_info = mkStrictnessInfo (dataConRepStrictness data_con, False)

    cpr_info | isProductTyCon tycon && 
	       not (isUnboxedTupleTyCon tycon) && 
	       arity > 0 			= ReturnsCPR
	     | otherwise 			= NoCPRInfo
	     where
		tycon = dataConTyCon data_con
		-- Newtypes don't have a worker at all
		-- 
		-- If we are a product with 0 args we must be void(like)
		-- We can't create an unboxed tuple with 0 args for this
		-- and since Void has only one, constant value it should 
		-- just mean returning a pointer to a pre-existing cell. 
		-- So we won't really gain from doing anything fancy
		-- and we treat this case as Top.
\end{code}

The wrapper for a constructor is an ordinary top-level binding that evaluates
any strict args, unboxes any args that are going to be flattened, and calls
the worker.

We're going to build a constructor that looks like:

	data (Data a, C b) =>  T a b = T1 !a !Int b

	T1 = /\ a b -> 
	     \d1::Data a, d2::C b ->
	     \p q r -> case p of { p ->
		       case q of { q ->
		       Con T1 [a,b] [p,q,r]}}

Notice that

* d2 is thrown away --- a context in a data decl is used to make sure
  one *could* construct dictionaries at the site the constructor
  is used, but the dictionary isn't actually used.

* We have to check that we can construct Data dictionaries for
  the types a and Int.  Once we've done that we can throw d1 away too.

* We use (case p of q -> ...) to evaluate p, rather than "seq" because
  all that matters is that the arguments are evaluated.  "seq" is 
  very careful to preserve evaluation order, which we don't need
  to be here.

  You might think that we could simply give constructors some strictness
  info, like PrimOps, and let CoreToStg do the let-to-case transformation.
  But we don't do that because in the case of primops and functions strictness
  is a *property* not a *requirement*.  In the case of constructors we need to
  do something active to evaluate the argument.

  Making an explicit case expression allows the simplifier to eliminate
  it in the (common) case where the constructor arg is already evaluated.

\begin{code}
mkDataConWrapId data_con
  = wrap_id
  where
    wrap_id = mkId (dataConName data_con) wrap_ty info
    work_id = dataConId data_con

    info = mkIdInfo (DataConWrapId data_con)
	   `setUnfoldingInfo`	mkTopUnfolding (mkInlineMe wrap_rhs)
	   `setCprInfo`		cpr_info
		-- The Cpr info can be important inside INLINE rhss, where the
		-- wrapper constructor isn't inlined
	   `setArityInfo`	exactArity arity
		-- It's important to specify the arity, so that partial
		-- applications are treated as values
	   `setCafInfo`	      NoCafRefs
		-- The wrapper Id ends up in STG code as an argument,
		-- sometimes before its definition, so we want to
		-- signal that it has no CAFs

    wrap_ty = mkForAllTys all_tyvars $
	      mkFunTys all_arg_tys
	      result_ty

    cpr_info = idCprInfo work_id

    wrap_rhs | isNewTyCon tycon
	     = ASSERT( null ex_tyvars && null ex_dict_args && length orig_arg_tys == 1 )
		-- No existentials on a newtype, but it can have a context
		-- e.g. 	newtype Eq a => T a = MkT (...)

	       mkLams tyvars $ mkLams dict_args $ Lam id_arg1 $
	       Note (Coerce result_ty (head orig_arg_tys)) (Var id_arg1)

{-	I nuked this because map (:) xs would create a
	new local lambda for the (:) in core-to-stg.  
	There isn't a defn for the worker!

	     | null dict_args && all not_marked_strict strict_marks
	     = Var work_id	-- The common case.  Not only is this efficient,
				-- but it also ensures that the wrapper is replaced
				-- by the worker even when there are no args.
				-- 		f (:) x
				-- becomes 
				--		f $w: x
				-- This is really important in rule matching,
				-- which is a bit sad.  (We could match on the wrappers,
				-- but that makes it less likely that rules will match
				-- when we bring bits of unfoldings together
-}

	     | otherwise
	     = mkLams all_tyvars $ mkLams dict_args $ 
	       mkLams ex_dict_args $ mkLams id_args $
	       foldr mk_case con_app 
		     (zip (ex_dict_args++id_args) strict_marks) i3 []

    con_app i rep_ids = mkApps (Var work_id)
			       (map varToCoreExpr (all_tyvars ++ reverse rep_ids))

    (tyvars, theta, ex_tyvars, ex_theta, orig_arg_tys, tycon) = dataConSig data_con
    all_tyvars   = tyvars ++ ex_tyvars

    dict_tys     = mkDictTys theta
    ex_dict_tys  = mkDictTys ex_theta
    all_arg_tys  = dict_tys ++ ex_dict_tys ++ orig_arg_tys
    result_ty    = mkTyConApp tycon (mkTyVarTys tyvars)

    mkLocals i tys = (zipWith mkTemplateLocal [i..i+n-1] tys, i+n)
		   where
		     n = length tys

    (dict_args, i1)    = mkLocals 1  dict_tys
    (ex_dict_args,i2)  = mkLocals i1 ex_dict_tys
    (id_args,i3)       = mkLocals i2 orig_arg_tys
    arity	       = i3-1
    (id_arg1:_)   = id_args		-- Used for newtype only

    strict_marks  = dataConStrictMarks data_con
    not_marked_strict NotMarkedStrict = True
    not_marked_strict other	      = False


    mk_case 
	   :: (Id, StrictnessMark)	-- arg, strictness
	   -> (Int -> [Id] -> CoreExpr) -- body
	   -> Int			-- next rep arg id
	   -> [Id]			-- rep args so far
	   -> CoreExpr
    mk_case (arg,strict) body i rep_args
  	  = case strict of
		NotMarkedStrict -> body i (arg:rep_args)
		MarkedStrict 
		   | isUnLiftedType (idType arg) -> body i (arg:rep_args)
		   | otherwise ->
			Case (Var arg) arg [(DEFAULT,[], body i (arg:rep_args))]

		MarkedUnboxed con tys ->
		   Case (Var arg) arg [(DataAlt con, con_args,
					body i' (reverse con_args++rep_args))]
		   where n_tys = length tys
			 (con_args,i') = mkLocals i tys
\end{code}


%************************************************************************
%*									*
\subsection{Record selectors}
%*									*
%************************************************************************

We're going to build a record selector unfolding that looks like this:

	data T a b c = T1 { ..., op :: a, ...}
		     | T2 { ..., op :: a, ...}
		     | T3

	sel = /\ a b c -> \ d -> case d of
				    T1 ... x ... -> x
				    T2 ... x ... -> x
				    other	 -> error "..."

Similarly for newtypes

	newtype N a = MkN { unN :: a->a }

	unN :: N a -> a -> a
	unN n = coerce (a->a) n
	
We need to take a little care if the field has a polymorphic type:

	data R = R { f :: forall a. a->a }

Then we want

	f :: forall a. R -> a -> a
	f = /\ a \ r = case r of
			  R f -> f a

(not f :: R -> forall a. a->a, which gives the type inference mechanism 
problems at call sites)

Similarly for newtypes

	newtype N = MkN { unN :: forall a. a->a }

	unN :: forall a. N -> a -> a
	unN = /\a -> \n:N -> coerce (a->a) n

\begin{code}
mkRecordSelId tycon field_label unpack_id
	-- Assumes that all fields with the same field label have the same type
	--
	-- Annoyingly, we have to pass in the unpackCString# Id, because
	-- we can't conjure it up out of thin air
  = sel_id
  where
    sel_id     = mkId (fieldLabelName field_label) selector_ty info

    field_ty   = fieldLabelType field_label
    field_name = fieldLabelName field_label
    data_cons  = tyConDataCons tycon
    tyvars     = tyConTyVars tycon	-- These scope over the types in 
					-- the FieldLabels of constructors of this type
    tycon_theta	= tyConTheta tycon	-- The context on the data decl
					--   eg data (Eq a, Ord b) => T a b = ...
    (field_tyvars,field_tau) = splitForAllTys field_ty

    data_ty   = mkTyConApp tycon tyvar_tys
    tyvar_tys = mkTyVarTys tyvars

	-- Very tiresomely, the selectors are (unnecessarily!) overloaded over
	-- just the dictionaries in the types of the constructors that contain
	-- the relevant field.  Urgh.  
	-- NB: this code relies on the fact that DataCons are quantified over
	-- the identical type variables as their parent TyCon
    dict_tys  = [mkDictTy cls tys | (cls, tys) <- tycon_theta, needed_dict (cls, tys)]
    needed_dict pred = or [ pred `elem` (dataConTheta dc) 
			  | (DataAlt dc, _, _) <- the_alts]

    selector_ty :: Type
    selector_ty  = mkForAllTys tyvars $ mkForAllTys field_tyvars $
		   mkFunTys dict_tys $  mkFunTy data_ty field_tau
      
    info = mkIdInfo (RecordSelId field_label)
	   `setArityInfo`	exactArity 1
	   `setUnfoldingInfo`	unfolding	
	   `setCafInfo`		NoCafRefs
	-- ToDo: consider adding further IdInfo

    unfolding = mkTopUnfolding sel_rhs

	
    (data_id:dict_ids) = mkTemplateLocals (data_ty:dict_tys)
    alts      = map mk_maybe_alt data_cons
    the_alts  = catMaybes alts
    default_alt | all isJust alts = []	-- No default needed
		| otherwise	  = [(DEFAULT, [], error_expr)]

    sel_rhs | isNewTyCon tycon = new_sel_rhs
	    | otherwise	       = data_sel_rhs

    data_sel_rhs = mkLams tyvars $ mkLams field_tyvars $ 
		   mkLams dict_ids $ Lam data_id $
	 	   Case (Var data_id) data_id (the_alts ++ default_alt)

    new_sel_rhs  = mkLams tyvars $ mkLams field_tyvars $ Lam data_id $
		    Note (Coerce (unUsgTy field_tau) (unUsgTy data_ty)) (Var data_id)

    mk_maybe_alt data_con 
	  = case maybe_the_arg_id of
		Nothing		-> Nothing
		Just the_arg_id -> Just (DataAlt data_con, arg_ids, 
					 mkVarApps (Var the_arg_id) field_tyvars)
	  where
	    arg_ids 	     = mkTemplateLocals (dataConArgTys data_con tyvar_tys)
				    -- The first one will shadow data_id, but who cares
	    field_lbls	     = dataConFieldLabels data_con
	    maybe_the_arg_id = assocMaybe (field_lbls `zip` arg_ids) field_label

    error_expr = mkApps (Var rEC_SEL_ERROR_ID) [Type (unUsgTy field_tau), err_string]
       -- preserves invariant that type args are *not* usage-annotated on top.  KSW 1999-04.
    err_string = App (Var unpack_id) (Lit (MachStr (_PK_ full_msg)))
    full_msg   = showSDoc (sep [text "No match in record selector", ppr sel_id]) 
\end{code}


%************************************************************************
%*									*
\subsection{Dictionary selectors}
%*									*
%************************************************************************

Selecting a field for a dictionary.  If there is just one field, then
there's nothing to do.  

ToDo: unify with mkRecordSelId.

\begin{code}
mkDictSelId :: Name -> Class -> Id
mkDictSelId name clas
  = sel_id
  where
    ty	      = exprType rhs
    sel_id    = mkId name ty info
    field_lbl = mkFieldLabel name tycon ty tag
    tag       = assoc "MkId.mkDictSelId" (classSelIds clas `zip` allFieldLabelTags) sel_id

    info      = mkIdInfo (RecordSelId field_lbl)
		`setArityInfo`	    exactArity 1
		`setUnfoldingInfo`  unfolding
		`setCafInfo`	    NoCafRefs
		
	-- We no longer use 'must-inline' on record selectors.  They'll
	-- inline like crazy if they scrutinise a constructor

    unfolding = mkTopUnfolding rhs

    tyvars  = classTyVars clas

    tycon      = classTyCon clas
    [data_con] = tyConDataCons tycon
    tyvar_tys  = mkTyVarTys tyvars
    arg_tys    = dataConArgTys data_con tyvar_tys
    the_arg_id = arg_ids !! (tag - firstFieldLabelTag)

    dict_ty    = mkDictTy clas tyvar_tys
    (dict_id:arg_ids) = mkTemplateLocals (dict_ty : arg_tys)

    rhs | isNewTyCon tycon = mkLams tyvars $ Lam dict_id $
			     Note (Coerce (head arg_tys) dict_ty) (Var dict_id)
	| otherwise	   = mkLams tyvars $ Lam dict_id $
			     Case (Var dict_id) dict_id
			     	  [(DataAlt data_con, arg_ids, Var the_arg_id)]
\end{code}


%************************************************************************
%*									*
\subsection{Primitive operations
%*									*
%************************************************************************

\begin{code}
mkPrimOpId :: PrimOp -> Id
mkPrimOpId prim_op 
  = id
  where
    (tyvars,arg_tys,res_ty, arity, strict_info) = primOpSig prim_op
    ty   = mkForAllTys tyvars (mkFunTys arg_tys res_ty)
    name = mkPrimOpIdName prim_op id
    id   = mkId name ty info
		
    info = mkIdInfo (PrimOpId prim_op)
	   `setSpecInfo`	rules
	   `setArityInfo` 	exactArity arity
	   `setStrictnessInfo`	strict_info

    rules = addRule id emptyCoreRules (primOpRule prim_op)


-- For each ccall we manufacture a separate CCallOpId, giving it
-- a fresh unique, a type that is correct for this particular ccall,
-- and a CCall structure that gives the correct details about calling
-- convention etc.  
--
-- The *name* of this Id is a local name whose OccName gives the full
-- details of the ccall, type and all.  This means that the interface 
-- file reader can reconstruct a suitable Id

mkCCallOpId :: Unique -> CCall -> Type -> Id
mkCCallOpId uniq ccall ty
  = ASSERT( isEmptyVarSet (tyVarsOfType ty) )
	-- A CCallOpId should have no free type variables; 
	-- when doing substitutions won't substitute over it
    mkId name ty info
  where
    occ_str = showSDocIface (braces (pprCCallOp ccall <+> ppr ty))
	-- The "occurrence name" of a ccall is the full info about the
	-- ccall; it is encoded, but may have embedded spaces etc!

    name    = mkCCallName uniq occ_str
    prim_op = CCallOp ccall

    info = mkIdInfo (PrimOpId prim_op)
	   `setArityInfo` 	exactArity arity
	   `setStrictnessInfo`	strict_info

    (_, tau) 	 = splitForAllTys ty
    (arg_tys, _) = splitFunTys tau
    arity	 = length arg_tys
    strict_info  = mkStrictnessInfo (take arity (repeat wwPrim), False)
\end{code}


%************************************************************************
%*									*
\subsection{DictFuns}
%*									*
%************************************************************************

\begin{code}
mkDictFunId :: Name		-- Name to use for the dict fun;
	    -> Class 
	    -> [TyVar]
	    -> [Type]
	    -> ClassContext
	    -> Id

mkDictFunId dfun_name clas inst_tyvars inst_tys inst_decl_theta
  = mkVanillaId dfun_name dfun_ty
  where
    (class_tyvars, sc_theta, _, _) = classBigSig clas
    sc_theta' = substClasses (mkTopTyVarSubst class_tyvars inst_tys) sc_theta

    dfun_theta = classesToPreds inst_decl_theta

{-  1 dec 99: disable the Mark Jones optimisation for the sake
    of compatibility with Hugs.
    See `types/InstEnv' for a discussion related to this.

    dfun_theta = case inst_decl_theta of
		   []    -> []	-- If inst_decl_theta is empty, then we don't
				-- want to have any dict arguments, so that we can
				-- expose the constant methods.

		   other -> nub (inst_decl_theta ++ filter not_const sc_theta')
				-- Otherwise we pass the superclass dictionaries to
				-- the dictionary function; the Mark Jones optimisation.
				--
				-- NOTE the "nub".  I got caught by this one:
				--   class Monad m => MonadT t m where ...
				--   instance Monad m => MonadT (EnvT env) m where ...
				-- Here, the inst_decl_theta has (Monad m); but so
				-- does the sc_theta'!
				--
				-- NOTE the "not_const".  I got caught by this one too:
				--   class Foo a => Baz a b where ...
				--   instance Wob b => Baz T b where..
				-- Now sc_theta' has Foo T
-}
    dfun_ty = mkSigmaTy inst_tyvars dfun_theta (mkDictTy clas inst_tys)

    not_const (clas, tys) = not (isEmptyVarSet (tyVarsOfTypes tys))
\end{code}


%************************************************************************
%*									*
\subsection{Un-definable}
%*									*
%************************************************************************

These two can't be defined in Haskell.

unsafeCoerce# isn't so much a PrimOp as a phantom identifier, that
just gets expanded into a type coercion wherever it occurs.  Hence we
add it as a built-in Id with an unfolding here.

The type variables we use here are "open" type variables: this means
they can unify with both unlifted and lifted types.  Hence we provide
another gun with which to shoot yourself in the foot.

\begin{code}
unsafeCoerceId
  = pcMiscPrelId unsafeCoerceIdKey pREL_GHC SLIT("unsafeCoerce#") ty info
  where
    info = vanillaIdInfo
	   `setUnfoldingInfo` mkCompulsoryUnfolding rhs
	   

    ty  = mkForAllTys [openAlphaTyVar,openBetaTyVar]
		      (mkFunTy openAlphaTy openBetaTy)
    [x] = mkTemplateLocals [openAlphaTy]
    rhs = mkLams [openAlphaTyVar,openBetaTyVar,x] $
	  Note (Coerce openBetaTy openAlphaTy) (Var x)
\end{code}


@getTag#@ is another function which can't be defined in Haskell.  It needs to
evaluate its argument and call the dataToTag# primitive.

\begin{code}
getTagId
  = pcMiscPrelId getTagIdKey pREL_GHC SLIT("getTag#") ty info
  where
    info = vanillaIdInfo
	   `setUnfoldingInfo`	mkCompulsoryUnfolding rhs
	-- We don't provide a defn for this; you must inline it

    ty = mkForAllTys [alphaTyVar] (mkFunTy alphaTy intPrimTy)
    [x,y] = mkTemplateLocals [alphaTy,alphaTy]
    rhs = mkLams [alphaTyVar,x] $
	  Case (Var x) y [ (DEFAULT, [], mkApps (Var dataToTagId) [Type alphaTy, Var y]) ]

dataToTagId = mkPrimOpId DataToTagOp
\end{code}

@realWorld#@ used to be a magic literal, \tr{void#}.  If things get
nasty as-is, change it back to a literal (@Literal@).

\begin{code}
realWorldPrimId	-- :: State# RealWorld
  = pcMiscPrelId realWorldPrimIdKey pREL_GHC SLIT("realWorld#")
		 realWorldStatePrimTy
		 (noCafIdInfo `setUnfoldingInfo` mkOtherCon [])
	-- The mkOtherCon makes it look that realWorld# is evaluated
	-- which in turn makes Simplify.interestingArg return True,
	-- which in turn makes INLINE things applied to realWorld# likely
	-- to be inlined
\end{code}


%************************************************************************
%*									*
\subsection[PrelVals-error-related]{@error@ and friends; @trace@}
%*									*
%************************************************************************

GHC randomly injects these into the code.

@patError@ is just a version of @error@ for pattern-matching
failures.  It knows various ``codes'' which expand to longer
strings---this saves space!

@absentErr@ is a thing we put in for ``absent'' arguments.  They jolly
well shouldn't be yanked on, but if one is, then you will get a
friendly message from @absentErr@ (rather than a totally random
crash).

@parError@ is a special version of @error@ which the compiler does
not know to be a bottoming Id.  It is used in the @_par_@ and @_seq_@
templates, but we don't ever expect to generate code for it.

\begin{code}
eRROR_ID
  = pc_bottoming_Id errorIdKey pREL_ERR SLIT("error") errorTy
pAT_ERROR_ID
  = generic_ERROR_ID patErrorIdKey SLIT("patError")
rEC_SEL_ERROR_ID
  = generic_ERROR_ID recSelErrIdKey SLIT("recSelError")
rEC_CON_ERROR_ID
  = generic_ERROR_ID recConErrorIdKey SLIT("recConError")
rEC_UPD_ERROR_ID
  = generic_ERROR_ID recUpdErrorIdKey SLIT("recUpdError")
iRREFUT_PAT_ERROR_ID
  = generic_ERROR_ID irrefutPatErrorIdKey SLIT("irrefutPatError")
nON_EXHAUSTIVE_GUARDS_ERROR_ID
  = generic_ERROR_ID nonExhaustiveGuardsErrorIdKey SLIT("nonExhaustiveGuardsError")
nO_METHOD_BINDING_ERROR_ID
  = generic_ERROR_ID noMethodBindingErrorIdKey SLIT("noMethodBindingError")

aBSENT_ERROR_ID
  = pc_bottoming_Id absentErrorIdKey pREL_ERR SLIT("absentErr")
	(mkSigmaTy [openAlphaTyVar] [] openAlphaTy)

pAR_ERROR_ID
  = pcMiscPrelId parErrorIdKey pREL_ERR SLIT("parError")
    (mkSigmaTy [openAlphaTyVar] [] openAlphaTy) noCafIdInfo

\end{code}


%************************************************************************
%*									*
\subsection{Utilities}
%*									*
%************************************************************************

\begin{code}
pcMiscPrelId :: Unique{-IdKey-} -> Module -> FAST_STRING -> Type -> IdInfo -> Id
pcMiscPrelId key mod str ty info
  = let
	name = mkWiredInIdName key mod (mkSrcVarOcc str) imp
	imp  = mkId name ty info -- the usual case...
    in
    imp
    -- We lie and say the thing is imported; otherwise, we get into
    -- a mess with dependency analysis; e.g., core2stg may heave in
    -- random calls to GHCbase.unpackPS__.  If GHCbase is the module
    -- being compiled, then it's just a matter of luck if the definition
    -- will be in "the right place" to be in scope.

pc_bottoming_Id key mod name ty
 = pcMiscPrelId key mod name ty bottoming_info
 where
    bottoming_info = noCafIdInfo 
		     `setStrictnessInfo` mkStrictnessInfo ([wwStrict], True)
		     
	-- these "bottom" out, no matter what their arguments

generic_ERROR_ID u n = pc_bottoming_Id u pREL_ERR n errorTy

-- Very useful...
noCafIdInfo = vanillaIdInfo `setCafInfo` NoCafRefs

(openAlphaTyVar:openBetaTyVar:_) = openAlphaTyVars
openAlphaTy  = mkTyVarTy openAlphaTyVar
openBetaTy   = mkTyVarTy openBetaTyVar

errorTy  :: Type
errorTy  = mkUsgTy UsMany $
           mkSigmaTy [openAlphaTyVar] [] (mkFunTys [mkUsgTy UsOnce (mkListTy charTy)] 
                                                   (mkUsgTy UsMany openAlphaTy))
    -- Notice the openAlphaTyVar.  It says that "error" can be applied
    -- to unboxed as well as boxed types.  This is OK because it never
    -- returns, so the return type is irrelevant.
\end{code}

