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
	mkDictFunId, mkDefaultMethodId,
	mkDictSelId, 

	mkDataConIds,
	mkRecordSelId, 
	mkPrimOpId, mkFCallId,

	mkReboxingAlt, wrapNewTypeBody, unwrapNewTypeBody,

	-- And some particular Ids; see below for why they are wired in
	wiredInIds, ghcPrimIds,
	unsafeCoerceId, realWorldPrimId, voidArgId, nullAddrId, seqId,
	lazyId, lazyIdUnfolding, lazyIdKey,

	mkRuntimeErrorApp,
	rEC_CON_ERROR_ID, iRREFUT_PAT_ERROR_ID, rUNTIME_ERROR_ID,
	nON_EXHAUSTIVE_GUARDS_ERROR_ID,	nO_METHOD_BINDING_ERROR_ID,
	pAT_ERROR_ID, eRROR_ID,

        unsafeCoerceName
    ) where

#include "HsVersions.h"


import BasicTypes	( Arity, StrictnessMark(..), isMarkedUnboxed, isMarkedStrict )
import Rules		( mkSpecInfo )
import TysPrim		( openAlphaTyVars, alphaTyVar, alphaTy, 
			  realWorldStatePrimTy, addrPrimTy
			)
import TysWiredIn	( charTy, mkListTy )
import PrelRules	( primOpRules )
import Type		( TyThing(..), mkForAllTy, tyVarsOfTypes )
import Coercion         ( mkSymCoercion, mkUnsafeCoercion )
import TcType		( Type, ThetaType, mkDictTy, mkPredTys, mkPredTy, 
			  mkTyConApp, mkTyVarTys, mkClassPred, 
			  mkFunTys, mkFunTy, mkSigmaTy, tcSplitSigmaTy, 
			  isUnLiftedType, mkForAllTys, mkTyVarTy, tyVarsOfType,
			  tcSplitFunTys, tcSplitForAllTys, dataConsStupidTheta
			)
import CoreUtils	( exprType )
import CoreUnfold 	( mkTopUnfolding, mkCompulsoryUnfolding )
import Literal		( nullAddrLit, mkStringLit )
import TyCon		( TyCon, isNewTyCon, tyConDataCons, FieldLabel,
                          tyConStupidTheta, isProductTyCon, isDataTyCon, isRecursiveTyCon,
                          newTyConCo, tyConArity )
import Class		( Class, classTyCon, classSelIds )
import Var		( Id, TyVar, Var )
import VarSet		( isEmptyVarSet, subVarSet, varSetElems )
import Name		( mkFCallName, mkWiredInName, Name, BuiltInSyntax(..) )
import OccName		( mkOccNameFS, varName )
import PrimOp		( PrimOp, primOpSig, primOpOcc, primOpTag )
import ForeignCall	( ForeignCall )
import DataCon		( DataCon, DataConIds(..), dataConTyCon, dataConUnivTyVars,
			  dataConFieldLabels, dataConRepArity, dataConResTys,
			  dataConRepArgTys, dataConRepType, 
			  dataConSig, dataConStrictMarks, dataConExStricts, 
			  splitProductType, isVanillaDataCon, dataConFieldType,
			  dataConInstOrigArgTys
			)
import Id		( idType, mkGlobalId, mkVanillaGlobal, mkSysLocal, 
			  mkTemplateLocals, mkTemplateLocalsNum, mkExportedLocalId,
			  mkTemplateLocal, idName
			)
import IdInfo		( IdInfo, noCafIdInfo,  setUnfoldingInfo, 
			  setArityInfo, setSpecInfo, setCafInfo,
			  setAllStrictnessInfo, vanillaIdInfo,
			  GlobalIdDetails(..), CafInfo(..)
			)
import NewDemand	( mkStrictSig, DmdResult(..),
			  mkTopDmdType, topDmd, evalDmd, lazyDmd, retCPR,
			  Demand(..), Demands(..) )
import DmdAnal		( dmdAnalTopRhs )
import CoreSyn
import Unique		( mkBuiltinUnique, mkPrimOpIdUnique )
import Maybes
import PrelNames
import Util             ( dropList, isSingleton )
import Outputable
import FastString
import ListSetOps	( assoc )
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
	-- no way yet of expressing at the definition site for these 
	-- error-reporting functions that they have an 'open' 
	-- result type. -- sof 1/99]

    eRROR_ID,	-- This one isn't used anywhere else in the compiler
		-- But we still need it in wiredInIds so that when GHC
		-- compiles a program that mentions 'error' we don't
		-- import its type from the interface file; we just get
		-- the Id defined here.  Which has an 'open-tyvar' type.

    rUNTIME_ERROR_ID,
    iRREFUT_PAT_ERROR_ID,
    nON_EXHAUSTIVE_GUARDS_ERROR_ID,
    nO_METHOD_BINDING_ERROR_ID,
    pAT_ERROR_ID,
    rEC_CON_ERROR_ID,

    lazyId
    ] ++ ghcPrimIds

-- These Ids are exported from GHC.Prim
ghcPrimIds
  = [ 	-- These can't be defined in Haskell, but they have
	-- perfectly reasonable unfoldings in Core
    realWorldPrimId,
    unsafeCoerceId,
    nullAddrId,
    seqId
    ]
\end{code}

%************************************************************************
%*									*
\subsection{Data constructors}
%*									*
%************************************************************************

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
mkDataConIds :: Name -> Name -> DataCon -> DataConIds
mkDataConIds wrap_name wkr_name data_con
  | isNewTyCon tycon
  = NewDC nt_wrap_id

  | any isMarkedStrict all_strict_marks		-- Algebraic, needs wrapper
  = AlgDC (Just alg_wrap_id) wrk_id

  | otherwise					-- Algebraic, no wrapper
  = AlgDC Nothing wrk_id
  where
    (tvs, theta, orig_arg_tys) = dataConSig data_con
    tycon       = dataConTyCon data_con

    dict_tys    = mkPredTys theta
    all_arg_tys = dict_tys ++ orig_arg_tys
    tycon_args  = dataConUnivTyVars data_con
    result_ty_args = (mkTyVarTys tycon_args)
    result_ty   = mkTyConApp tycon result_ty_args

    wrap_ty = mkForAllTys tvs (mkFunTys all_arg_tys result_ty)
	-- We used to include the stupid theta in the wrapper's args
	-- but now we don't.  Instead the type checker just injects these
	-- extra constraints where necessary.

	----------- Worker (algebraic data types only) --------------
	-- The *worker* for the data constructor is the function that
	-- takes the representation arguments and builds the constructor.
    wrk_id = mkGlobalId (DataConWorkId data_con) wkr_name
	 		(dataConRepType data_con) wkr_info

    wkr_arity = dataConRepArity data_con
    wkr_info  = noCafIdInfo
	        `setArityInfo`		wkr_arity
	        `setAllStrictnessInfo`	Just wkr_sig
		`setUnfoldingInfo`      evaldUnfolding	-- Record that it's evaluated,
							-- even if arity = 0

    wkr_sig = mkStrictSig (mkTopDmdType (replicate wkr_arity topDmd) cpr_info)
	-- Notice that we do *not* say the worker is strict
	-- even if the data constructor is declared strict
	--	e.g. 	data T = MkT !(Int,Int)
	-- Why?  Because the *wrapper* is strict (and its unfolding has case
	-- expresssions that do the evals) but the *worker* itself is not.
	-- If we pretend it is strict then when we see
	--	case x of y -> $wMkT y
	-- the simplifier thinks that y is "sure to be evaluated" (because
	--  $wMkT is strict) and drops the case.  No, $wMkT is not strict.
	--
	-- When the simplifer sees a pattern 
	--	case e of MkT x -> ...
	-- it uses the dataConRepStrictness of MkT to mark x as evaluated;
	-- but that's fine... dataConRepStrictness comes from the data con
	-- not from the worker Id.

    cpr_info | isProductTyCon tycon && 
	       isDataTyCon tycon    &&
	       wkr_arity > 0 	    &&
	       wkr_arity <= mAX_CPR_SIZE	= retCPR
	     | otherwise 			= TopRes
	-- RetCPR is only true for products that are real data types;
	-- that is, not unboxed tuples or [non-recursive] newtypes

	----------- Wrappers for newtypes --------------
    nt_wrap_id   = mkGlobalId (DataConWrapId data_con) wrap_name wrap_ty nt_wrap_info
    nt_wrap_info = noCafIdInfo		-- The NoCaf-ness is set by noCafIdInfo
		  `setArityInfo` 1	-- Arity 1
		  `setUnfoldingInfo`     newtype_unf
    newtype_unf  = ASSERT( isVanillaDataCon data_con &&
			   isSingleton orig_arg_tys )
	  	   -- No existentials on a newtype, but it can have a context
	  	   -- e.g. 	newtype Eq a => T a = MkT (...)
	  	   mkCompulsoryUnfolding $ 
	  	   mkLams tvs $ Lam id_arg1 $ 
	  	   wrapNewTypeBody tycon result_ty_args
                       (Var id_arg1)

    id_arg1 = mkTemplateLocal 1 (head orig_arg_tys)

	----------- Wrappers for algebraic data types -------------- 
    alg_wrap_id = mkGlobalId (DataConWrapId data_con) wrap_name wrap_ty alg_wrap_info
    alg_wrap_info = noCafIdInfo		-- The NoCaf-ness is set by noCafIdInfo
		    `setArityInfo` 	   alg_arity
			-- It's important to specify the arity, so that partial
			-- applications are treated as values
		    `setUnfoldingInfo`     alg_unf
		    `setAllStrictnessInfo` Just wrap_sig

    all_strict_marks = dataConExStricts data_con ++ dataConStrictMarks data_con
    wrap_sig = mkStrictSig (mkTopDmdType arg_dmds cpr_info)
    arg_dmds = map mk_dmd all_strict_marks
    mk_dmd str | isMarkedStrict str = evalDmd
	       | otherwise	    = lazyDmd
	-- The Cpr info can be important inside INLINE rhss, where the
	-- wrapper constructor isn't inlined.
	-- And the argument strictness can be important too; we
	-- may not inline a contructor when it is partially applied.
	-- For example:
	--	data W = C !Int !Int !Int
	--	...(let w = C x in ...(w p q)...)...
	-- we want to see that w is strict in its two arguments

    alg_unf = mkTopUnfolding $ Note InlineMe $
	      mkLams tvs $ 
	      mkLams dict_args $ mkLams id_args $
	      foldr mk_case con_app 
		    (zip (dict_args ++ id_args) all_strict_marks)
		    i3 []

    con_app i rep_ids = mkApps (Var wrk_id)
			       (map varToCoreExpr (tvs ++ reverse rep_ids))

    (dict_args,i2) = mkLocals 1  dict_tys
    (id_args,i3)   = mkLocals i2 orig_arg_tys
    alg_arity	   = i3-1

    mk_case 
	   :: (Id, StrictnessMark)	-- Arg, strictness
	   -> (Int -> [Id] -> CoreExpr) -- Body
	   -> Int			-- Next rep arg id
	   -> [Id]			-- Rep args so far, reversed
	   -> CoreExpr
    mk_case (arg,strict) body i rep_args
  	  = case strict of
		NotMarkedStrict -> body i (arg:rep_args)
		MarkedStrict 
		   | isUnLiftedType (idType arg) -> body i (arg:rep_args)
		   | otherwise ->
			Case (Var arg) arg result_ty [(DEFAULT,[], body i (arg:rep_args))]

		MarkedUnboxed
		   -> case splitProductType "do_unbox" (idType arg) of
			   (tycon, tycon_args, con, tys) ->
				   Case (Var arg) arg result_ty  
					[(DataAlt con, 
					  con_args,
					  body i' (reverse con_args ++ rep_args))]
			      where 
				(con_args, i') = mkLocals i tys

mAX_CPR_SIZE :: Arity
mAX_CPR_SIZE = 10
-- We do not treat very big tuples as CPR-ish:
--	a) for a start we get into trouble because there aren't 
--	   "enough" unboxed tuple types (a tiresome restriction, 
--	   but hard to fix), 
--	b) more importantly, big unboxed tuples get returned mainly
--	   on the stack, and are often then allocated in the heap
--	   by the caller.  So doing CPR for them may in fact make
--	   things worse.

mkLocals i tys = (zipWith mkTemplateLocal [i..i+n-1] tys, i+n)
	       where
		 n = length tys
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

Similarly for (recursive) newtypes

	newtype N = MkN { unN :: forall a. a->a }

	unN :: forall b. N -> b -> b
	unN = /\b -> \n:N -> (coerce (forall a. a->a) n)


Note [Naughty record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A "naughty" field is one for which we can't define a record 
selector, because an existential type variable would escape.  For example:
	data T = forall a. MkT { x,y::a }
We obviously can't define	
	x (MkT v _) = v
Nevertheless we *do* put a RecordSelId into the type environment
so that if the user tries to use 'x' as a selector we can bleat
helpfully, rather than saying unhelpfully that 'x' is not in scope.
Hence the sel_naughty flag, to identify record selectors that don't really exist.

In general, a field is naughty if its type mentions a type variable that
isn't in the result type of the constructor.

Note [GADT record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For GADTs, we require that all constructors with a common field 'f' have the same
result type (modulo alpha conversion).  [Checked in TcTyClsDecls.checkValidTyCon]
E.g. 
	data T where
	  T1 { f :: a } :: T [a]
	  T2 { f :: a, y :: b  } :: T [a]
and now the selector takes that type as its argument:
	f :: forall a. T [a] -> a
	f t = case t of
		T1 { f = v } -> v
		T2 { f = v } -> v
Note the forall'd tyvars of the selector are just the free tyvars
of the result type; there may be other tyvars in the constructor's
type (e.g. 'b' in T2).

\begin{code}

-- Steps for handling "naughty" vs "non-naughty" selectors:
--  1. Determine naughtiness by comparing field type vs result type
--  2. Install naughty ones with selector_ty of type _|_ and fill in mzero for info
--  3. If it's not naughty, do the normal plan.

mkRecordSelId :: TyCon -> FieldLabel -> Id
mkRecordSelId tycon field_label
	-- Assumes that all fields with the same field label have the same type
  | is_naughty = naughty_id
  | otherwise  = sel_id
  where
    is_naughty = not (tyVarsOfType field_ty `subVarSet` res_tv_set)
    sel_id_details = RecordSelId tycon field_label is_naughty

    -- Escapist case here for naughty construcotrs
    -- We give it no IdInfo, and a type of forall a.a (never looked at)
    naughty_id = mkGlobalId sel_id_details field_label forall_a_a noCafIdInfo
    forall_a_a = mkForAllTy alphaTyVar (mkTyVarTy alphaTyVar)

    -- Normal case starts here
    sel_id = mkGlobalId sel_id_details field_label selector_ty info
    data_cons  	      = tyConDataCons tycon	
    data_cons_w_field = filter has_field data_cons	-- Can't be empty!
    has_field con     = field_label `elem` dataConFieldLabels con

    con1	= head data_cons_w_field
    res_tys	= dataConResTys con1
    res_tv_set	= tyVarsOfTypes res_tys
    res_tvs	= varSetElems res_tv_set
    data_ty	= mkTyConApp tycon res_tys
    field_ty	= dataConFieldType con1 field_label
    
	-- *Very* tiresomely, the selectors are (unnecessarily!) overloaded over
	-- just the dictionaries in the types of the constructors that contain
	-- the relevant field.  [The Report says that pattern matching on a
	-- constructor gives the same constraints as applying it.]  Urgh.  
	--
	-- However, not all data cons have all constraints (because of
	-- BuildTyCl.mkDataConStupidTheta).  So we need to find all the data cons 
	-- involved in the pattern match and take the union of their constraints.
    stupid_dict_tys = mkPredTys (dataConsStupidTheta data_cons_w_field)
    n_stupid_dicts  = length stupid_dict_tys

    (field_tyvars,field_theta,field_tau) = tcSplitSigmaTy field_ty
    field_dict_tys			 = mkPredTys field_theta
    n_field_dict_tys			 = length field_dict_tys
	-- If the field has a universally quantified type we have to 
	-- be a bit careful.  Suppose we have
	--	data R = R { op :: forall a. Foo a => a -> a }
	-- Then we can't give op the type
	--	op :: R -> forall a. Foo a => a -> a
	-- because the typechecker doesn't understand foralls to the
	-- right of an arrow.  The "right" type to give it is
	--	op :: forall a. Foo a => R -> a -> a
	-- But then we must generate the right unfolding too:
	--	op = /\a -> \dfoo -> \ r ->
	--	     case r of
	--		R op -> op a dfoo
	-- Note that this is exactly the type we'd infer from a user defn
	--	op (R op) = op

    selector_ty :: Type
    selector_ty  = mkForAllTys res_tvs $ mkForAllTys field_tyvars $
		   mkFunTys stupid_dict_tys  $  mkFunTys field_dict_tys $
		   mkFunTy data_ty field_tau
      
    arity = 1 + n_stupid_dicts + n_field_dict_tys

    (strict_sig, rhs_w_str) = dmdAnalTopRhs sel_rhs
	-- Use the demand analyser to work out strictness.
	-- With all this unpackery it's not easy!

    info = noCafIdInfo
	   `setCafInfo`		  caf_info
	   `setArityInfo`	  arity
	   `setUnfoldingInfo`     mkTopUnfolding rhs_w_str
	   `setAllStrictnessInfo` Just strict_sig

	-- Allocate Ids.  We do it a funny way round because field_dict_tys is
	-- almost always empty.  Also note that we use max_dict_tys
 	-- rather than n_dict_tys, because the latter gives an infinite loop:
	-- n_dict tys depends on the_alts, which depens on arg_ids, which depends
	-- on arity, which depends on n_dict tys.  Sigh!  Mega sigh!
    stupid_dict_ids  = mkTemplateLocalsNum 1 stupid_dict_tys
    max_stupid_dicts = length (tyConStupidTheta tycon)
    field_dict_base  = max_stupid_dicts + 1
    field_dict_ids   = mkTemplateLocalsNum field_dict_base field_dict_tys
    dict_id_base     = field_dict_base + n_field_dict_tys
    data_id	     = mkTemplateLocal dict_id_base data_ty
    arg_base	     = dict_id_base + 1

    the_alts :: [CoreAlt]
    the_alts   = map mk_alt data_cons_w_field	-- Already sorted by data-con
    no_default = length data_cons == length data_cons_w_field	-- No default needed

    default_alt | no_default = []
		| otherwise  = [(DEFAULT, [], error_expr)]

	-- The default branch may have CAF refs, because it calls recSelError etc.
    caf_info    | no_default = NoCafRefs
	        | otherwise  = MayHaveCafRefs

    sel_rhs = mkLams res_tvs $ mkLams field_tyvars $ 
	      mkLams stupid_dict_ids $ mkLams field_dict_ids $
	      Lam data_id     $ mk_result sel_body

	-- NB: A newtype always has a vanilla DataCon; no existentials etc
	--     res_tys will simply be the dataConUnivTyVars
    sel_body | isNewTyCon tycon = unwrapNewTypeBody tycon res_tys (Var data_id)
	     | otherwise	= Case (Var data_id) data_id field_tau (default_alt ++ the_alts)

    mk_result poly_result = mkVarApps (mkVarApps poly_result field_tyvars) field_dict_ids
	-- We pull the field lambdas to the top, so we need to 
	-- apply them in the body.  For example:
	--	data T = MkT { foo :: forall a. a->a }
	--
	--	foo :: forall a. T -> a -> a
	--	foo = /\a. \t:T. case t of { MkT f -> f a }

    mk_alt data_con 
      = 	-- In the non-vanilla case, the pattern must bind type variables and
		-- the context stuff; hence the arg_prefix binding below
	  mkReboxingAlt uniqs data_con (arg_prefix ++ arg_ids) (Var the_arg_id)
      where
	(arg_prefix, arg_ids)
	   | isVanillaDataCon data_con	 	-- Instantiate from commmon base
	   = ([], mkTemplateLocalsNum arg_base (dataConInstOrigArgTys data_con res_tys))
	   | otherwise		-- The case pattern binds type variables, which are used
				-- in the types of the arguments of the pattern
	   = (dc_tvs ++ mkTemplateLocalsNum arg_base (mkPredTys dc_theta),
	      mkTemplateLocalsNum arg_base' dc_arg_tys)

	(dc_tvs, dc_theta, dc_arg_tys) = dataConSig data_con
	arg_base' = arg_base + length dc_theta

	unpack_base = arg_base' + length dc_arg_tys
	uniqs = map mkBuiltinUnique [unpack_base..]

    	the_arg_id  = assoc "mkRecordSelId:mk_alt" (field_lbls `zip` arg_ids) field_label
    	field_lbls  = dataConFieldLabels data_con

    error_expr = mkRuntimeErrorApp rEC_SEL_ERROR_ID field_tau full_msg
    full_msg   = showSDoc (sep [text "No match in record selector", ppr sel_id]) 


-- (mkReboxingAlt us con xs rhs) basically constructs the case
-- alternative	(con, xs, rhs)
-- but it does the reboxing necessary to construct the *source* 
-- arguments, xs, from the representation arguments ys.
-- For example:
--	data T = MkT !(Int,Int) Bool
--
-- mkReboxingAlt MkT [x,b] r 
--	= (DataAlt MkT, [y::Int,z::Int,b], let x = (y,z) in r)
--
-- mkDataAlt should really be in DataCon, but it can't because
-- it manipulates CoreSyn.

mkReboxingAlt
  :: [Unique]		-- Uniques for the new Ids
  -> DataCon
  -> [Var]		-- Source-level args, including existential dicts
  -> CoreExpr		-- RHS
  -> CoreAlt

mkReboxingAlt us con args rhs
  | not (any isMarkedUnboxed stricts)
  = (DataAlt con, args, rhs)

  | otherwise
  = let
	(binds, args') = go args stricts us
    in
    (DataAlt con, args', mkLets binds rhs)

  where
    stricts = dataConExStricts con ++ dataConStrictMarks con

    go [] stricts us = ([], [])

	-- Type variable case
    go (arg:args) stricts us 
      | isTyVar arg
      = let (binds, args') = go args stricts us
	in  (binds, arg:args')

	-- Term variable case
    go (arg:args) (str:stricts) us
      | isMarkedUnboxed str
      = let
          ty = idType arg
          
	  (tycon, tycon_args, pack_con, con_arg_tys)
	 	 = splitProductType "mkReboxingAlt" ty

	  unpacked_args  = zipWith (mkSysLocal FSLIT("rb")) us con_arg_tys
	  (binds, args') = go args stricts (dropList con_arg_tys us)
	  con_app | isNewTyCon tycon = ASSERT( isSingleton unpacked_args )
				       wrapNewTypeBody tycon tycon_args (Var (head unpacked_args))
					-- ToDo: is this right?  Jun06
		  | otherwise = mkConApp pack_con (map Type tycon_args ++ map Var unpacked_args)
	in
	(NonRec arg con_app : binds, unpacked_args ++ args')

      | otherwise
      = let (binds, args') = go args stricts us
        in  (binds, arg:args')
\end{code}


%************************************************************************
%*									*
\subsection{Dictionary selectors}
%*									*
%************************************************************************

Selecting a field for a dictionary.  If there is just one field, then
there's nothing to do.  

Dictionary selectors may get nested forall-types.  Thus:

	class Foo a where
	  op :: forall b. Ord b => a -> b -> b

Then the top-level type for op is

	op :: forall a. Foo a => 
	      forall b. Ord b => 
	      a -> b -> b

This is unlike ordinary record selectors, which have all the for-alls
at the outside.  When dealing with classes it's very convenient to
recover the original type signature from the class op selector.

\begin{code}
mkDictSelId :: Name -> Class -> Id
mkDictSelId name clas
  = mkGlobalId (ClassOpId clas) name sel_ty info
  where
    sel_ty = mkForAllTys tyvars (mkFunTy (idType dict_id) (idType the_arg_id))
	-- We can't just say (exprType rhs), because that would give a type
	--	C a -> C a
	-- for a single-op class (after all, the selector is the identity)
	-- But it's type must expose the representation of the dictionary
	-- to gat (say)		C a -> (a -> a)

    info = noCafIdInfo
		`setArityInfo`	    	1
		`setUnfoldingInfo`  	mkTopUnfolding rhs
		`setAllStrictnessInfo`	Just strict_sig

	-- We no longer use 'must-inline' on record selectors.  They'll
	-- inline like crazy if they scrutinise a constructor

	-- The strictness signature is of the form U(AAAVAAAA) -> T
 	-- where the V depends on which item we are selecting
	-- It's worth giving one, so that absence info etc is generated
	-- even if the selector isn't inlined
    strict_sig = mkStrictSig (mkTopDmdType [arg_dmd] TopRes)
    arg_dmd | isNewTyCon tycon = evalDmd
	    | otherwise	       = Eval (Prod [ if the_arg_id == id then evalDmd else Abs
					    | id <- arg_ids ])

    tycon      = classTyCon clas
    [data_con] = tyConDataCons tycon
    tyvars     = dataConUnivTyVars data_con
    arg_tys    = ASSERT( isVanillaDataCon data_con ) dataConRepArgTys data_con
    the_arg_id = assoc "MkId.mkDictSelId" (map idName (classSelIds clas) `zip` arg_ids) name

    pred	      = mkClassPred clas (mkTyVarTys tyvars)
    (dict_id:arg_ids) = mkTemplateLocals (mkPredTy pred : arg_tys)

    rhs = mkLams tyvars (Lam dict_id rhs_body)
    rhs_body | isNewTyCon tycon = unwrapNewTypeBody tycon (map mkTyVarTy tyvars) (Var dict_id)
	     | otherwise	= Case (Var dict_id) dict_id (idType the_arg_id)
			     	       [(DataAlt data_con, arg_ids, Var the_arg_id)]

wrapNewTypeBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
-- The wrapper for the data constructor for a newtype looks like this:
--	newtype T a = MkT (a,Int)
--	MkT :: forall a. (a,Int) -> T a
--	MkT = /\a. \(x:(a,Int)). x `cast` CoT a
-- where CoT is the coercion TyCon assoicated with the newtype
--
-- The call (wrapNewTypeBody T [a] e) returns the
-- body of the wrapper, namely
--	e `cast` CoT [a]
--
-- For non-recursive newtypes, GHC currently treats them like type
-- synonyms, so no cast is necessary.  This function is the only
-- place in the compiler that generates 
--
wrapNewTypeBody tycon args result_expr
--  | isRecursiveTyCon tycon	-- Recursive case; use a coerce
  = Cast result_expr co
--  | otherwise
--  = result_expr
  where
    co = mkTyConApp (newTyConCo tycon) args

unwrapNewTypeBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
unwrapNewTypeBody tycon args result_expr
--  | isRecursiveTyCon tycon	-- Recursive case; use a coerce
  = Cast result_expr sym_co
--  | otherwise
--  = result_expr
  where
    sym_co = mkSymCoercion co
    co     = mkTyConApp (newTyConCo tycon) args

-- Old Definition of mkNewTypeBody
-- Used for both wrapping and unwrapping
--mkNewTypeBody tycon result_ty result_expr
--  | isRecursiveTyCon tycon	-- Recursive case; use a coerce
--  = Note (Coerce result_ty (exprType result_expr)) result_expr
--  | otherwise			-- Normal case
--  = result_expr
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
    (tyvars,arg_tys,res_ty, arity, strict_sig) = primOpSig prim_op
    ty   = mkForAllTys tyvars (mkFunTys arg_tys res_ty)
    name = mkWiredInName gHC_PRIM (primOpOcc prim_op) 
			 (mkPrimOpIdUnique (primOpTag prim_op))
			 Nothing (AnId id) UserSyntax
    id   = mkGlobalId (PrimOpId prim_op) name ty info
		
    info = noCafIdInfo
	   `setSpecInfo`	  mkSpecInfo (primOpRules prim_op name)
	   `setArityInfo` 	  arity
	   `setAllStrictnessInfo` Just strict_sig

-- For each ccall we manufacture a separate CCallOpId, giving it
-- a fresh unique, a type that is correct for this particular ccall,
-- and a CCall structure that gives the correct details about calling
-- convention etc.  
--
-- The *name* of this Id is a local name whose OccName gives the full
-- details of the ccall, type and all.  This means that the interface 
-- file reader can reconstruct a suitable Id

mkFCallId :: Unique -> ForeignCall -> Type -> Id
mkFCallId uniq fcall ty
  = ASSERT( isEmptyVarSet (tyVarsOfType ty) )
	-- A CCallOpId should have no free type variables; 
	-- when doing substitutions won't substitute over it
    mkGlobalId (FCallId fcall) name ty info
  where
    occ_str = showSDoc (braces (ppr fcall <+> ppr ty))
	-- The "occurrence name" of a ccall is the full info about the
	-- ccall; it is encoded, but may have embedded spaces etc!

    name = mkFCallName uniq occ_str

    info = noCafIdInfo
	   `setArityInfo` 		arity
	   `setAllStrictnessInfo`	Just strict_sig

    (_, tau) 	 = tcSplitForAllTys ty
    (arg_tys, _) = tcSplitFunTys tau
    arity	 = length arg_tys
    strict_sig   = mkStrictSig (mkTopDmdType (replicate arity evalDmd) TopRes)
\end{code}


%************************************************************************
%*									*
\subsection{DictFuns and default methods}
%*									*
%************************************************************************

Important notes about dict funs and default methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Dict funs and default methods are *not* ImplicitIds.  Their definition
involves user-written code, so we can't figure out their strictness etc
based on fixed info, as we can for constructors and record selectors (say).

We build them as LocalIds, but with External Names.  This ensures that
they are taken to account by free-variable finding and dependency
analysis (e.g. CoreFVs.exprFreeVars).

Why shouldn't they be bound as GlobalIds?  Because, in particular, if
they are globals, the specialiser floats dict uses above their defns,
which prevents good simplifications happening.  Also the strictness
analyser treats a occurrence of a GlobalId as imported and assumes it
contains strictness in its IdInfo, which isn't true if the thing is
bound in the same module as the occurrence.

It's OK for dfuns to be LocalIds, because we form the instance-env to
pass on to the next module (md_insts) in CoreTidy, afer tidying
and globalising the top-level Ids.

BUT make sure they are *exported* LocalIds (mkExportedLocalId) so 
that they aren't discarded by the occurrence analyser.

\begin{code}
mkDefaultMethodId dm_name ty = mkExportedLocalId dm_name ty

mkDictFunId :: Name		-- Name to use for the dict fun;
	    -> [TyVar]
	    -> ThetaType
	    -> Class 
	    -> [Type]
	    -> Id

mkDictFunId dfun_name inst_tyvars dfun_theta clas inst_tys
  = mkExportedLocalId dfun_name dfun_ty
  where
    dfun_ty = mkSigmaTy inst_tyvars dfun_theta (mkDictTy clas inst_tys)

{-  1 dec 99: disable the Mark Jones optimisation for the sake
    of compatibility with Hugs.
    See `types/InstEnv' for a discussion related to this.

    (class_tyvars, sc_theta, _, _) = classBigSig clas
    not_const (clas, tys) = not (isEmptyVarSet (tyVarsOfTypes tys))
    sc_theta' = substClasses (zipTopTvSubst class_tyvars inst_tys) sc_theta
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
\end{code}


%************************************************************************
%*									*
\subsection{Un-definable}
%*									*
%************************************************************************

These Ids can't be defined in Haskell.  They could be defined in
unfoldings in the wired-in GHC.Prim interface file, but we'd have to
ensure that they were definitely, definitely inlined, because there is
no curried identifier for them.  That's what mkCompulsoryUnfolding
does.  If we had a way to get a compulsory unfolding from an interface
file, we could do that, but we don't right now.

unsafeCoerce# isn't so much a PrimOp as a phantom identifier, that
just gets expanded into a type coercion wherever it occurs.  Hence we
add it as a built-in Id with an unfolding here.

The type variables we use here are "open" type variables: this means
they can unify with both unlifted and lifted types.  Hence we provide
another gun with which to shoot yourself in the foot.

\begin{code}
mkWiredInIdName mod fs uniq id
 = mkWiredInName mod (mkOccNameFS varName fs) uniq Nothing (AnId id) UserSyntax

unsafeCoerceName = mkWiredInIdName gHC_PRIM FSLIT("unsafeCoerce#") unsafeCoerceIdKey  unsafeCoerceId
nullAddrName     = mkWiredInIdName gHC_PRIM FSLIT("nullAddr#")	   nullAddrIdKey      nullAddrId
seqName		 = mkWiredInIdName gHC_PRIM FSLIT("seq")	   seqIdKey	      seqId
realWorldName	 = mkWiredInIdName gHC_PRIM FSLIT("realWorld#")	   realWorldPrimIdKey realWorldPrimId
lazyIdName	 = mkWiredInIdName gHC_BASE FSLIT("lazy")         lazyIdKey	      lazyId

errorName		 = mkWiredInIdName gHC_ERR FSLIT("error")	     errorIdKey eRROR_ID
recSelErrorName		 = mkWiredInIdName gHC_ERR FSLIT("recSelError")     recSelErrorIdKey rEC_SEL_ERROR_ID
runtimeErrorName	 = mkWiredInIdName gHC_ERR FSLIT("runtimeError")    runtimeErrorIdKey rUNTIME_ERROR_ID
irrefutPatErrorName	 = mkWiredInIdName gHC_ERR FSLIT("irrefutPatError") irrefutPatErrorIdKey iRREFUT_PAT_ERROR_ID
recConErrorName		 = mkWiredInIdName gHC_ERR FSLIT("recConError")     recConErrorIdKey rEC_CON_ERROR_ID
patErrorName 		 = mkWiredInIdName gHC_ERR FSLIT("patError") 	     patErrorIdKey pAT_ERROR_ID
noMethodBindingErrorName = mkWiredInIdName gHC_ERR FSLIT("noMethodBindingError")
					   noMethodBindingErrorIdKey nO_METHOD_BINDING_ERROR_ID
nonExhaustiveGuardsErrorName 
  = mkWiredInIdName gHC_ERR FSLIT("nonExhaustiveGuardsError") 
		    nonExhaustiveGuardsErrorIdKey nON_EXHAUSTIVE_GUARDS_ERROR_ID
\end{code}

\begin{code}
-- unsafeCoerce# :: forall a b. a -> b
unsafeCoerceId
  = pcMiscPrelId unsafeCoerceName ty info
  where
    info = noCafIdInfo `setUnfoldingInfo` mkCompulsoryUnfolding rhs
	   

    ty  = mkForAllTys [openAlphaTyVar,openBetaTyVar]
		      (mkFunTy openAlphaTy openBetaTy)
    [x] = mkTemplateLocals [openAlphaTy]
    rhs = mkLams [openAlphaTyVar,openBetaTyVar,x] $
--       Note (Coerce openBetaTy openAlphaTy) (Var x)
         Cast (Var x) (mkUnsafeCoercion openAlphaTy openBetaTy)

-- nullAddr# :: Addr#
-- The reason is is here is because we don't provide 
-- a way to write this literal in Haskell.
nullAddrId 
  = pcMiscPrelId nullAddrName addrPrimTy info
  where
    info = noCafIdInfo `setUnfoldingInfo` 
	   mkCompulsoryUnfolding (Lit nullAddrLit)

seqId
  = pcMiscPrelId seqName ty info
  where
    info = noCafIdInfo `setUnfoldingInfo` mkCompulsoryUnfolding rhs
	   

    ty  = mkForAllTys [alphaTyVar,openBetaTyVar]
		      (mkFunTy alphaTy (mkFunTy openBetaTy openBetaTy))
    [x,y] = mkTemplateLocals [alphaTy, openBetaTy]
    rhs = mkLams [alphaTyVar,openBetaTyVar,x,y] (Case (Var x) x openBetaTy [(DEFAULT, [], Var y)])

-- lazy :: forall a?. a? -> a?	 (i.e. works for unboxed types too)
-- Used to lazify pseq:		pseq a b = a `seq` lazy b
-- 
-- Also, no strictness: by being a built-in Id, all the info about lazyId comes from here,
-- not from GHC.Base.hi.   This is important, because the strictness
-- analyser will spot it as strict!
--
-- Also no unfolding in lazyId: it gets "inlined" by a HACK in the worker/wrapper pass
--	(see WorkWrap.wwExpr)	
-- We could use inline phases to do this, but that would be vulnerable to changes in 
-- phase numbering....we must inline precisely after strictness analysis.
lazyId
  = pcMiscPrelId lazyIdName ty info
  where
    info = noCafIdInfo
    ty  = mkForAllTys [alphaTyVar] (mkFunTy alphaTy alphaTy)

lazyIdUnfolding :: CoreExpr	-- Used to expand 'lazyId' after strictness anal
lazyIdUnfolding = mkLams [openAlphaTyVar,x] (Var x)
		where
		  [x] = mkTemplateLocals [openAlphaTy]
\end{code}

@realWorld#@ used to be a magic literal, \tr{void#}.  If things get
nasty as-is, change it back to a literal (@Literal@).

voidArgId is a Local Id used simply as an argument in functions
where we just want an arg to avoid having a thunk of unlifted type.
E.g.
	x = \ void :: State# RealWorld -> (# p, q #)

This comes up in strictness analysis

\begin{code}
realWorldPrimId	-- :: State# RealWorld
  = pcMiscPrelId realWorldName realWorldStatePrimTy
		 (noCafIdInfo `setUnfoldingInfo` evaldUnfolding)
	-- The evaldUnfolding makes it look that realWorld# is evaluated
	-- which in turn makes Simplify.interestingArg return True,
	-- which in turn makes INLINE things applied to realWorld# likely
	-- to be inlined

voidArgId 	-- :: State# RealWorld
  = mkSysLocal FSLIT("void") voidArgIdKey realWorldStatePrimTy
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
mkRuntimeErrorApp 
	:: Id 		-- Should be of type (forall a. Addr# -> a)
			-- 	where Addr# points to a UTF8 encoded string
	-> Type 	-- The type to instantiate 'a'
	-> String	-- The string to print
	-> CoreExpr

mkRuntimeErrorApp err_id res_ty err_msg 
  = mkApps (Var err_id) [Type res_ty, err_string]
  where
    err_string = Lit (mkStringLit err_msg)

rEC_SEL_ERROR_ID		= mkRuntimeErrorId recSelErrorName
rUNTIME_ERROR_ID	 	= mkRuntimeErrorId runtimeErrorName
iRREFUT_PAT_ERROR_ID		= mkRuntimeErrorId irrefutPatErrorName
rEC_CON_ERROR_ID		= mkRuntimeErrorId recConErrorName
pAT_ERROR_ID			= mkRuntimeErrorId patErrorName
nO_METHOD_BINDING_ERROR_ID      = mkRuntimeErrorId noMethodBindingErrorName
nON_EXHAUSTIVE_GUARDS_ERROR_ID	= mkRuntimeErrorId nonExhaustiveGuardsErrorName

-- The runtime error Ids take a UTF8-encoded string as argument
mkRuntimeErrorId name = pc_bottoming_Id name runtimeErrorTy
runtimeErrorTy 	      = mkSigmaTy [openAlphaTyVar] [] (mkFunTy addrPrimTy openAlphaTy)
\end{code}

\begin{code}
eRROR_ID = pc_bottoming_Id errorName errorTy

errorTy  :: Type
errorTy  = mkSigmaTy [openAlphaTyVar] [] (mkFunTys [mkListTy charTy] openAlphaTy)
    -- Notice the openAlphaTyVar.  It says that "error" can be applied
    -- to unboxed as well as boxed types.  This is OK because it never
    -- returns, so the return type is irrelevant.
\end{code}


%************************************************************************
%*									*
\subsection{Utilities}
%*									*
%************************************************************************

\begin{code}
pcMiscPrelId :: Name -> Type -> IdInfo -> Id
pcMiscPrelId name ty info
  = mkVanillaGlobal name ty info
    -- We lie and say the thing is imported; otherwise, we get into
    -- a mess with dependency analysis; e.g., core2stg may heave in
    -- random calls to GHCbase.unpackPS__.  If GHCbase is the module
    -- being compiled, then it's just a matter of luck if the definition
    -- will be in "the right place" to be in scope.

pc_bottoming_Id name ty
 = pcMiscPrelId name ty bottoming_info
 where
    bottoming_info = vanillaIdInfo `setAllStrictnessInfo` Just strict_sig
	-- Do *not* mark them as NoCafRefs, because they can indeed have
	-- CAF refs.  For example, pAT_ERROR_ID calls GHC.Err.untangle,
	-- which has some CAFs
	-- In due course we may arrange that these error-y things are
	-- regarded by the GC as permanently live, in which case we
	-- can give them NoCaf info.  As it is, any function that calls
	-- any pc_bottoming_Id will itself have CafRefs, which bloats
	-- SRTs.

    strict_sig	   = mkStrictSig (mkTopDmdType [evalDmd] BotRes)
	-- These "bottom" out, no matter what their arguments

(openAlphaTyVar:openBetaTyVar:_) = openAlphaTyVars
openAlphaTy  = mkTyVarTy openAlphaTyVar
openBetaTy   = mkTyVarTy openBetaTyVar
\end{code}

