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

	mkDataConId,
	mkRecordSelId,
	mkNewTySelId,
	mkPrimitiveId,

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
import PrelMods		( pREL_ERR, pREL_GHC )
import PrelRules	( primOpRule )
import Rules		( addRule )
import Type		( Type, ThetaType,
			  mkDictTy, mkTyConApp, mkTyVarTys, mkFunTys, mkFunTy, mkSigmaTy,
			  isUnLiftedType, mkForAllTys, mkTyVarTy, tyVarsOfTypes,
			  splitSigmaTy, splitFunTy_maybe, splitAlgTyConApp,
			  splitFunTys, splitForAllTys, unUsgTy,
			  mkUsgTy, UsageAnn(..)
			)
import Module		( Module )
import CoreUnfold 	( mkTopUnfolding, mkCompulsoryUnfolding )
import Subst		( mkTopTyVarSubst, substTheta )
import TyCon		( TyCon, isNewTyCon, tyConDataCons, isDataTyCon )
import Class		( Class, classBigSig, classTyCon, classTyVars, classSelIds )
import Var		( Id, TyVar )
import VarSet		( isEmptyVarSet )
import Const		( Con(..) )
import Name		( mkDerivedName, mkWiredInIdName, mkLocalName, 
			  mkWorkerOcc, mkSuperDictSelOcc,
			  Name, NamedThing(..),
			)
import OccName		( mkSrcVarOcc )
import PrimOp		( PrimOp(DataToTagOp), primOpSig, mkPrimOpIdName, primOpArity, primOpStrictness )
import Demand		( wwStrict )
import DataCon		( DataCon, StrictnessMark(..), dataConStrictMarks, dataConFieldLabels, 
			  dataConArgTys, dataConSig, dataConRawArgTys
			)
import Id		( idType, mkId,
			  mkVanillaId, mkTemplateLocals,
			  mkTemplateLocal, setInlinePragma
			)
import IdInfo		( vanillaIdInfo, mkIdInfo,
			  exactArity, setUnfoldingInfo, setCafInfo,
			  setArityInfo, setInlinePragInfo, setSpecInfo,
			  mkStrictnessInfo, setStrictnessInfo,
			  IdFlavour(..), InlinePragInfo(..), CafInfo(..), IdInfo
			)
import FieldLabel	( FieldLabel, FieldLabelTag, mkFieldLabel, fieldLabelName, 
			  firstFieldLabelTag, allFieldLabelTags
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
mkDataConId :: DataCon -> Id
mkDataConId data_con
  = mkId (getName data_con)
	 id_ty
	 (dataConInfo data_con)
  where
    (tyvars, theta, ex_tyvars, ex_theta, arg_tys, tycon) = dataConSig data_con
    id_ty = mkSigmaTy (tyvars ++ ex_tyvars) 
	              (theta ++ ex_theta)
	              (mkFunTys arg_tys (mkTyConApp tycon (mkTyVarTys tyvars)))
\end{code}

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
dataConInfo :: DataCon -> IdInfo

dataConInfo data_con
  = mkIdInfo (ConstantId (DataCon data_con))
    `setArityInfo` exactArity (n_dicts + n_ex_dicts + n_id_args)
    `setUnfoldingInfo` unfolding
  where
        unfolding = mkTopUnfolding (Note InlineMe con_rhs)
	-- The dictionary constructors of a class don't get a binding,
	-- but they are always saturated, so they should always be inlined.

	(tyvars, theta, ex_tyvars, ex_theta, orig_arg_tys, tycon) 
	   = dataConSig data_con
	rep_arg_tys = dataConRawArgTys data_con
	all_tyvars   = tyvars ++ ex_tyvars

	dict_tys     = [mkDictTy clas tys | (clas,tys) <- theta]
	ex_dict_tys  = [mkDictTy clas tys | (clas,tys) <- ex_theta]

	n_dicts	     = length dict_tys
	n_ex_dicts   = length ex_dict_tys
	n_id_args    = length orig_arg_tys
 	n_rep_args   = length rep_arg_tys

	result_ty    = mkTyConApp tycon (mkTyVarTys tyvars)

	mkLocals i n tys   = (zipWith mkTemplateLocal [i..i+n-1] tys, i+n)
	(dict_args, i1)    = mkLocals 1  n_dicts    dict_tys
	(ex_dict_args,i2)  = mkLocals i1 n_ex_dicts ex_dict_tys
	(id_args,i3)       = mkLocals i2 n_id_args  orig_arg_tys

	(id_arg1:_) = id_args		-- Used for newtype only
	strict_marks  = dataConStrictMarks data_con

	con_app i rep_ids
                | isNewTyCon tycon 
		= ASSERT( length orig_arg_tys == 1 )
		  Note (Coerce result_ty (head orig_arg_tys)) (Var id_arg1)
 		| otherwise
		= mkConApp data_con 
			(map Type (mkTyVarTys all_tyvars) ++ 
			 map Var (reverse rep_ids))

	con_rhs = mkLams all_tyvars $ mkLams dict_args $ 
		  mkLams ex_dict_args $ mkLams id_args $
		  foldr mk_case con_app 
		     (zip (ex_dict_args++id_args) strict_marks) i3 []

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
		   Case (Var arg) arg [(DataCon con, con_args,
					body i' (reverse con_args++rep_args))]
		   where n_tys = length tys
			 (con_args,i') = mkLocals i (length tys) tys
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

\begin{code}
mkRecordSelId field_label selector_ty
  = ASSERT( null theta && isDataTyCon tycon )
    sel_id
  where
    sel_id = mkId (fieldLabelName field_label) selector_ty info

    info = mkIdInfo (RecordSelId field_label)
	   `setArityInfo`	exactArity 1
	   `setUnfoldingInfo`	unfolding	
	   
	-- ToDo: consider adding further IdInfo

    unfolding = mkTopUnfolding sel_rhs

    (tyvars, theta, tau)  = splitSigmaTy selector_ty
    (data_ty,rhs_ty)      = expectJust "StdIdInfoRec" (splitFunTy_maybe tau)
					-- tau is of form (T a b c -> field-type)
    (tycon, _, data_cons) = splitAlgTyConApp data_ty
    tyvar_tys	          = mkTyVarTys tyvars
	
    [data_id] = mkTemplateLocals [data_ty]
    alts      = map mk_maybe_alt data_cons
    the_alts  = catMaybes alts
    default_alt | all isJust alts = []	-- No default needed
		| otherwise	  = [(DEFAULT, [], error_expr)]

    sel_rhs   = mkLams tyvars $ Lam data_id $
		Case (Var data_id) data_id (the_alts ++ default_alt)

    mk_maybe_alt data_con 
	  = case maybe_the_arg_id of
		Nothing		-> Nothing
		Just the_arg_id -> Just (DataCon data_con, arg_ids, Var the_arg_id)
	  where
	    arg_ids 	     = mkTemplateLocals (dataConArgTys data_con tyvar_tys)
				    -- The first one will shadow data_id, but who cares
	    field_lbls	     = dataConFieldLabels data_con
	    maybe_the_arg_id = assocMaybe (field_lbls `zip` arg_ids) field_label

    error_expr = mkApps (Var rEC_SEL_ERROR_ID) [Type (unUsgTy rhs_ty), mkStringLit full_msg]
       -- preserves invariant that type args are *not* usage-annotated on top.  KSW 1999-04.
    full_msg   = showSDoc (sep [text "No match in record selector", ppr sel_id]) 
\end{code}


%************************************************************************
%*									*
\subsection{Newtype field selectors}
%*									*
%************************************************************************

Possibly overkill to do it this way:

\begin{code}
mkNewTySelId field_label selector_ty = sel_id
  where
    sel_id = mkId (fieldLabelName field_label) selector_ty info
		  

    info = mkIdInfo (RecordSelId field_label)
	   `setArityInfo`	exactArity 1	
	   `setUnfoldingInfo`	unfolding
	   
	-- ToDo: consider adding further IdInfo

    unfolding = mkTopUnfolding sel_rhs

    (tyvars, theta, tau)  = splitSigmaTy selector_ty
    (data_ty,rhs_ty)      = expectJust "StdIdInfoRec" (splitFunTy_maybe tau)
					-- tau is of form (T a b c -> field-type)
    (tycon, _, data_cons) = splitAlgTyConApp data_ty
    tyvar_tys	          = mkTyVarTys tyvars
	
    [data_id] = mkTemplateLocals [data_ty]
    sel_rhs   = mkLams tyvars $ Lam data_id $
		Note (Coerce (unUsgTy rhs_ty) (unUsgTy data_ty)) (Var data_id)
\end{code}


%************************************************************************
%*									*
\subsection{Dictionary selectors}
%*									*
%************************************************************************

Selecting a field for a dictionary.  If there is just one field, then
there's nothing to do.

\begin{code}
mkDictSelId name clas ty
  = sel_id
  where
    sel_id    = mkId name ty info
    field_lbl = mkFieldLabel name ty tag
    tag       = assoc "MkId.mkDictSelId" (classSelIds clas `zip` allFieldLabelTags) sel_id

    info      = mkIdInfo (RecordSelId field_lbl)
		`setUnfoldingInfo`  unfolding
		
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
			     	  [(DataCon data_con, arg_ids, Var the_arg_id)]
\end{code}


%************************************************************************
%*									*
\subsection{Primitive operations
%*									*
%************************************************************************

\begin{code}
mkPrimitiveId :: PrimOp -> Id
mkPrimitiveId prim_op 
  = id
  where
    (tyvars,arg_tys,res_ty) = primOpSig prim_op
    ty   = mkForAllTys tyvars (mkFunTys arg_tys res_ty)
    name = mkPrimOpIdName prim_op id
    id   = mkId name ty info
		
    info = mkIdInfo (ConstantId (PrimOp prim_op))
	   `setUnfoldingInfo`	unfolding

-- Not yet... 
--	   `setSpecInfo`	rules
--	   `setArityInfo` 	exactArity arity
--	   `setStrictnessInfo`	strict_info

    arity 		= primOpArity prim_op
    (dmds, result_bot)	= primOpStrictness prim_op
    strict_info		= mkStrictnessInfo (take arity dmds, result_bot)
	-- primOpStrictness can return an infinite list of demands
	-- (cheap hack) but Ids mustn't have such things.
	-- What a mess.

    rules = addRule id emptyCoreRules (primOpRule prim_op)

    unfolding = mkCompulsoryUnfolding rhs
		-- The mkCompulsoryUnfolding says that this Id absolutely 
		-- must be inlined.  It's only used for primitives, 
		-- because we don't want to make a closure for each of them.

    args = mkTemplateLocals arg_tys
    rhs =  mkLams tyvars $ mkLams args $
	   mkPrimApp prim_op (map Type (mkTyVarTys tyvars) ++ map Var args)
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
	    -> ThetaType
	    -> Id

mkDictFunId dfun_name clas inst_tyvars inst_tys inst_decl_theta
  = mkVanillaId dfun_name dfun_ty
  where
    (class_tyvars, sc_theta, _, _) = classBigSig clas
    sc_theta' = substTheta (mkTopTyVarSubst class_tyvars inst_tys) sc_theta

    dfun_theta = inst_decl_theta

{-  1 dec 99: disable the Mark Jones optimisation for the sake
    of compatibility with Hugs.

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
	  Case (Var x) y [ (DEFAULT, [], 
		   Con (PrimOp DataToTagOp) [Type alphaTy, Var y]) ]
\end{code}

@realWorld#@ used to be a magic literal, \tr{void#}.  If things get
nasty as-is, change it back to a literal (@Literal@).

\begin{code}
realWorldPrimId	-- :: State# RealWorld
  = pcMiscPrelId realWorldPrimIdKey pREL_GHC SLIT("realWorld#")
		 realWorldStatePrimTy
		 noCafIdInfo
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
rEC_SEL_ERROR_ID
  = generic_ERROR_ID recSelErrIdKey SLIT("patError")
pAT_ERROR_ID
  = generic_ERROR_ID patErrorIdKey SLIT("patError")
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

