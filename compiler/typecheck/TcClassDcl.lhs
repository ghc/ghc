%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcClassDcl]{Typechecking class declarations}

\begin{code}
module TcClassDcl ( tcClassSigs, tcClassDecl2, 
		    getGenericInstances, 
		    MethodSpec, tcMethodBind, mkMethodBind, 
		    tcAddDeclCtxt, badMethodErr
		  ) where

#include "HsVersions.h"

import HsSyn
import RnHsSyn		( maybeGenericMatch, extractHsTyVars )
import RnExpr		( rnLExpr )
import RnEnv		( lookupTopBndrRn, lookupImportedName )
import Inst		( instToId, newDicts, newDictsAtLoc, newMethod, getOverlapFlag )
import InstEnv		( mkLocalInstance )
import TcEnv		( tcLookupLocatedClass, 
			  tcExtendTyVarEnv, tcExtendIdEnv,
			  InstInfo(..), pprInstInfoDetails,
			  simpleInstInfoTyCon, simpleInstInfoTy,
			  InstBindings(..), newDFunName
			)
import TcBinds		( TcPragFun, tcMonoBinds, tcPrags, mkPragFun, TcSigInfo(..), 
			  TcSigFun, mkTcSigFun )
import TcHsType		( tcHsKindedType, tcHsSigType )
import TcSimplify	( tcSimplifyCheck )
import TcUnify		( checkSigTyVars, sigCtxt )
import TcMType		( tcSkolSigTyVars )
import TcType		( Type, SkolemInfo(ClsSkol, InstSkol), UserTypeCtxt( GenPatCtxt ),
			  TcType, TcThetaType, TcTyVar, mkTyVarTys,
			  mkClassPred, tcSplitSigmaTy, tcSplitFunTys,
			  tcIsTyVarTy, tcSplitTyConApp_maybe, tcSplitForAllTys, tcSplitPhiTy,
			  getClassPredTys_maybe, mkPhiTy, mkTyVarTy
			)
import TcRnMonad
import Generics		( mkGenericRhs, validGenericInstanceType )
import PrelInfo		( nO_METHOD_BINDING_ERROR_ID )
import Class		( classTyVars, classBigSig, 
			  Class, ClassOpItem, DefMeth (..) )
import TyCon		( TyCon, tyConName, tyConHasGenerics )
import Type		( substTyWith )
import MkId		( mkDefaultMethodId, mkDictFunId )
import Id		( Id, idType, idName, mkUserLocal )
import Name		( Name, NamedThing(..) )
import NameEnv		( NameEnv, lookupNameEnv, mkNameEnv )
import NameSet		( nameSetToList )
import OccName		( reportIfUnused, mkDefaultMethodOcc )
import RdrName		( RdrName, mkDerivedRdrName )
import Outputable
import PrelNames	( genericTyConNames )
import DynFlags
import ErrUtils		( dumpIfSet_dyn )
import Util		( count, lengthIs, isSingleton, lengthExceeds )
import Unique		( Uniquable(..) )
import ListSetOps	( equivClassesByUniq, minusList	)
import SrcLoc		( Located(..), srcSpanStart, unLoc, noLoc )
import Maybes		( seqMaybe, isJust, mapCatMaybes )
import List		( partition )
import BasicTypes	( RecFlag(..), Boxity(..) )
import Bag
import FastString
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


%************************************************************************
%*									*
		Type-checking the class op signatures
%*									*
%************************************************************************

\begin{code}
tcClassSigs :: Name	    		-- Name of the class
	    -> [LSig Name]
	    -> LHsBinds Name
	    -> TcM [TcMethInfo]

type TcMethInfo = (Name, DefMeth, Type)	-- A temporary intermediate, to communicate 
					-- between tcClassSigs and buildClass
tcClassSigs clas sigs def_methods
  = do { dm_env <- checkDefaultBinds clas op_names def_methods
       ; mappM (tcClassSig dm_env) op_sigs }
  where
    op_sigs  = [sig | sig@(L _ (TypeSig _ _))       <- sigs]
    op_names = [n   | sig@(L _ (TypeSig (L _ n) _)) <- op_sigs]


checkDefaultBinds :: Name -> [Name] -> LHsBinds Name -> TcM (NameEnv Bool)
  -- Check default bindings
  -- 	a) must be for a class op for this class
  --	b) must be all generic or all non-generic
  -- and return a mapping from class-op to Bool
  --	where True <=> it's a generic default method
checkDefaultBinds clas ops binds
  = do dm_infos <- mapM (addLocM (checkDefaultBind clas ops)) (bagToList binds)
       return (mkNameEnv dm_infos)

checkDefaultBind clas ops (FunBind {fun_id = L _ op, fun_matches = MatchGroup matches _ })
  = do {  	-- Check that the op is from this class
	checkTc (op `elem` ops) (badMethodErr clas op)

   	-- Check that all the defns ar generic, or none are
    ;	checkTc (all_generic || none_generic) (mixedGenericErr op)

    ;	returnM (op, all_generic)
    }
  where
    n_generic    = count (isJust . maybeGenericMatch) matches
    none_generic = n_generic == 0
    all_generic  = matches `lengthIs` n_generic


tcClassSig :: NameEnv Bool		-- Info about default methods; 
	   -> LSig Name
	   -> TcM TcMethInfo

tcClassSig dm_env (L loc (TypeSig (L _ op_name) op_hs_ty))
  = setSrcSpan loc $ do
    { op_ty <- tcHsKindedType op_hs_ty	-- Class tyvars already in scope
    ; let dm = case lookupNameEnv dm_env op_name of
		Nothing    -> NoDefMeth
		Just False -> DefMeth
		Just True  -> GenDefMeth
    ; returnM (op_name, dm, op_ty) }
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

@tcClassDecls2@ generates bindings for polymorphic default methods
(generic default methods have by now turned into instance declarations)

\begin{code}
tcClassDecl2 :: LTyClDecl Name		-- The class declaration
	     -> TcM (LHsBinds Id, [Id])

tcClassDecl2 (L loc (ClassDecl {tcdLName = class_name, tcdSigs = sigs, 
				tcdMeths = default_binds}))
  = recoverM (returnM (emptyLHsBinds, []))	$ 
    setSrcSpan loc		   			$
    tcLookupLocatedClass class_name			`thenM` \ clas ->

	-- We make a separate binding for each default method.
	-- At one time I used a single AbsBinds for all of them, thus
	-- AbsBind [d] [dm1, dm2, dm3] { dm1 = ...; dm2 = ...; dm3 = ... }
	-- But that desugars into
	--	ds = \d -> (..., ..., ...)
	--	dm1 = \d -> case ds d of (a,b,c) -> a
	-- And since ds is big, it doesn't get inlined, so we don't get good
	-- default methods.  Better to make separate AbsBinds for each
    let
	(tyvars, _, _, op_items) = classBigSig clas
	prag_fn			 = mkPragFun sigs
	sig_fn			 = mkTcSigFun sigs
	tc_dm			 = tcDefMeth clas tyvars default_binds sig_fn prag_fn

	dm_sel_ids		 = [sel_id | (sel_id, DefMeth) <- op_items]
	-- Generate code for polymorphic default methods only
	-- (Generic default methods have turned into instance decls by now.)
	-- This is incompatible with Hugs, which expects a polymorphic 
	-- default method for every class op, regardless of whether or not 
	-- the programmer supplied an explicit default decl for the class.  
	-- (If necessary we can fix that, but we don't have a convenient Id to hand.)
    in
    mapAndUnzipM tc_dm dm_sel_ids	`thenM` \ (defm_binds, dm_ids_s) ->
    returnM (listToBag defm_binds, concat dm_ids_s)
    
tcDefMeth clas tyvars binds_in sig_fn prag_fn sel_id
  = do	{ dm_name <- lookupTopBndrRn (mkDefMethRdrName sel_id)
	; let	rigid_info  = ClsSkol clas
		clas_tyvars = tcSkolSigTyVars rigid_info tyvars
		inst_tys    = mkTyVarTys clas_tyvars
		dm_ty       = idType sel_id	-- Same as dict selector!
	        theta       = [mkClassPred clas inst_tys]
		local_dm_id = mkDefaultMethodId dm_name dm_ty
		origin 	    = SigOrigin rigid_info

	; (_, meth_info) <- mkMethodBind origin clas inst_tys binds_in (sel_id, DefMeth)
	; [this_dict] <- newDicts origin theta
	; (defm_bind, insts_needed) <- getLIE (tcMethodBind clas_tyvars theta [this_dict]
							    sig_fn prag_fn meth_info)
    
	; addErrCtxt (defltMethCtxt clas) $ do
    
        -- Check the context
	{ dict_binds <- tcSimplifyCheck
			        (ptext SLIT("class") <+> ppr clas)
				clas_tyvars
			        [this_dict]
			        insts_needed

	-- Simplification can do unification
	; checkSigTyVars clas_tyvars
    
	-- Inline pragmas 
	-- We'll have an inline pragma on the local binding, made by tcMethodBind
	-- but that's not enough; we want one on the global default method too
	-- Specialisations, on the other hand, belong on the thing inside only, I think
	; let (_,dm_inst_id,_) = meth_info
	      sel_name	       = idName sel_id
	      inline_prags     = filter isInlineLSig (prag_fn sel_name)
	; prags <- tcPrags dm_inst_id inline_prags

	; let full_bind = AbsBinds  clas_tyvars
		    		    [instToId this_dict]
		    		    [(clas_tyvars, local_dm_id, dm_inst_id, prags)]
		    		    (dict_binds `unionBags` defm_bind)
	; returnM (noLoc full_bind, [local_dm_id]) }}

mkDefMethRdrName :: Id -> RdrName
mkDefMethRdrName sel_id = mkDerivedRdrName (idName sel_id) mkDefaultMethodOcc
\end{code}


%************************************************************************
%*									*
\subsection{Typechecking a method}
%*									*
%************************************************************************

@tcMethodBind@ is used to type-check both default-method and
instance-decl method declarations.  We must type-check methods one at a
time, because their signatures may have different contexts and
tyvar sets.

\begin{code}
type MethodSpec = (Id, 			-- Global selector Id
		   Id, 			-- Local Id (class tyvars instantiated)
		   LHsBind Name)	-- Binding for the method

tcMethodBind 
	:: [TcTyVar]		-- Skolemised type variables for the
				--  	enclosing class/instance decl. 
				--  	They'll be signature tyvars, and we
				--  	want to check that they don't get bound
				-- Also they are scoped, so we bring them into scope
				-- Always equal the range of the type envt
	-> TcThetaType		-- Available theta; it's just used for the error message
	-> [Inst]		-- Available from context, used to simplify constraints 
				-- 	from the method body
	-> TcSigFun		-- For scoped tyvars, indexed by sel_name
	-> TcPragFun		-- Pragmas (e.g. inline pragmas), indexed by sel_name
	-> MethodSpec		-- Details of this method
	-> TcM (LHsBinds Id)

tcMethodBind inst_tyvars inst_theta avail_insts sig_fn prag_fn
	     (sel_id, meth_id, meth_bind)
  = recoverM (returnM emptyLHsBinds) $
	-- If anything fails, recover returning no bindings.
	-- This is particularly useful when checking the default-method binding of
	-- a class decl. If we don't recover, we don't add the default method to
	-- the type enviroment, and we get a tcLookup failure on $dmeth later.

    	-- Check the bindings; first adding inst_tyvars to the envt
	-- so that we don't quantify over them in nested places
	
    let sel_name = idName sel_id
	meth_sig_fn meth_name = ASSERT( meth_name == idName meth_id ) sig_fn sel_name
	-- The meth_bind metions the meth_name, but sig_fn is indexed by sel_name
    in
    tcExtendTyVarEnv inst_tyvars (
	tcExtendIdEnv [meth_id] 	$	-- In scope for tcInstSig
	addErrCtxt (methodCtxt sel_id)	$
	getLIE 				$
	tcMonoBinds [meth_bind] meth_sig_fn Recursive
    )					`thenM` \ ((meth_bind, mono_bind_infos), meth_lie) ->

	-- Now do context reduction.   We simplify wrt both the local tyvars
	-- and the ones of the class/instance decl, so that there is
	-- no problem with
	--	class C a where
	--	  op :: Eq a => a -> b -> a
	--
	-- We do this for each method independently to localise error messages

    let
	[(_, Just sig, local_meth_id)] = mono_bind_infos
    in

    addErrCtxtM (sigCtxt sel_id inst_tyvars inst_theta (idType meth_id))	$
    newDictsAtLoc (sig_loc sig) (sig_theta sig)		`thenM` \ meth_dicts ->
    let
	meth_tvs   = sig_tvs sig
	all_tyvars = meth_tvs ++ inst_tyvars
	all_insts  = avail_insts ++ meth_dicts
    in
    tcSimplifyCheck
	 (ptext SLIT("class or instance method") <+> quotes (ppr sel_id))
	 all_tyvars all_insts meth_lie		`thenM` \ lie_binds ->

    checkSigTyVars all_tyvars			`thenM_`

    tcPrags meth_id (prag_fn sel_name)		`thenM` \ prags -> 
    let
	poly_meth_bind = noLoc $ AbsBinds meth_tvs
				  (map instToId meth_dicts)
     				  [(meth_tvs, meth_id, local_meth_id, prags)]
				  (lie_binds `unionBags` meth_bind)
    in
    returnM (unitBag poly_meth_bind)


mkMethodBind :: InstOrigin
	     -> Class -> [TcType]	-- Class and instance types
	     -> LHsBinds Name	-- Method binding (pick the right one from in here)
	     -> ClassOpItem
	     -> TcM (Maybe Inst,		-- Method inst
		     MethodSpec)
-- Find the binding for the specified method, or make
-- up a suitable default method if it isn't there

mkMethodBind origin clas inst_tys meth_binds (sel_id, dm_info)
  = mkMethId origin clas sel_id inst_tys		`thenM` \ (mb_inst, meth_id) ->
    let
	meth_name  = idName meth_id
    in
	-- Figure out what method binding to use
	-- If the user suppplied one, use it, else construct a default one
    getSrcSpanM					`thenM` \ loc -> 
    (case find_bind (idName sel_id) meth_name meth_binds of
	Just user_bind -> returnM user_bind 
	Nothing	       -> 
	   mkDefMethRhs origin clas inst_tys sel_id loc dm_info	`thenM` \ rhs ->
		-- Not infix decl
	   returnM (noLoc $ mkFunBind (noLoc meth_name) [mkSimpleMatch [] rhs])
    )						`thenM` \ meth_bind ->

    returnM (mb_inst, (sel_id, meth_id, meth_bind))

mkMethId :: InstOrigin -> Class 
	 -> Id -> [TcType]	-- Selector, and instance types
	 -> TcM (Maybe Inst, Id)
	     
-- mkMethId instantiates the selector Id at the specified types
mkMethId origin clas sel_id inst_tys
  = let
	(tyvars,rho) = tcSplitForAllTys (idType sel_id)
	rho_ty	     = ASSERT( length tyvars == length inst_tys )
		       substTyWith tyvars inst_tys rho
	(preds,tau)  = tcSplitPhiTy rho_ty
        first_pred   = head preds
    in
	-- The first predicate should be of form (C a b)
	-- where C is the class in question
    ASSERT( not (null preds) && 
	    case getClassPredTys_maybe first_pred of
		{ Just (clas1,tys) -> clas == clas1 ; Nothing -> False }
    )
    if isSingleton preds then
	-- If it's the only one, make a 'method'
	getInstLoc origin			`thenM` \ inst_loc ->
    	newMethod inst_loc sel_id inst_tys 	`thenM` \ meth_inst ->
	returnM (Just meth_inst, instToId meth_inst)
    else
	-- If it's not the only one we need to be careful
	-- For example, given 'op' defined thus:
	--	class Foo a where
	--	  op :: (?x :: String) => a -> a
	-- (mkMethId op T) should return an Inst with type
	--	(?x :: String) => T -> T
	-- That is, the class-op's context is still there.  
	-- BUT: it can't be a Method any more, because it breaks
	-- 	INVARIANT 2 of methods.  (See the data decl for Inst.)
	newUnique			`thenM` \ uniq ->
	getSrcSpanM			`thenM` \ loc ->
	let 
	    real_tau = mkPhiTy (tail preds) tau
	    meth_id  = mkUserLocal (getOccName sel_id) uniq real_tau 
			(srcSpanStart loc) --TODO
	in
	returnM (Nothing, meth_id)

     -- The user didn't supply a method binding, 
     -- so we have to make up a default binding
     -- The RHS of a default method depends on the default-method info
mkDefMethRhs origin clas inst_tys sel_id loc DefMeth
  =  -- An polymorphic default method
    lookupImportedName (mkDefMethRdrName sel_id)	`thenM` \ dm_name ->
	-- Might not be imported, but will be an OrigName
    traceRn (text "mkDefMeth" <+> ppr dm_name)		`thenM_`
    returnM (nlHsVar dm_name)

mkDefMethRhs origin clas inst_tys sel_id loc NoDefMeth
  =  	-- No default method
	-- Warn only if -fwarn-missing-methods
    doptM Opt_WarnMissingMethods 		`thenM` \ warn -> 
    warnTc (isInstDecl origin
	   && warn
	   && reportIfUnused (getOccName sel_id))
   	   (omittedMethodWarn sel_id)		`thenM_`
    returnM error_rhs
  where
    error_rhs  = noLoc $ HsLam (mkMatchGroup [mkSimpleMatch wild_pats simple_rhs])
    simple_rhs = nlHsApp (nlHsVar (getName nO_METHOD_BINDING_ERROR_ID)) 
	    	       (nlHsLit (HsStringPrim (mkFastString error_msg)))
    error_msg = showSDoc (hcat [ppr loc, text "|", ppr sel_id ])

	-- When the type is of form t1 -> t2 -> t3
	-- make a default method like (\ _ _ -> noMethBind "blah")
	-- rather than simply        (noMethBind "blah")
	-- Reason: if t1 or t2 are higher-ranked types we get n
	--	   silly ambiguity messages.
	-- Example:	f :: (forall a. Eq a => a -> a) -> Int
	--		f = error "urk"
	-- Here, tcSub tries to force (error "urk") to have the right type,
	-- thus:	f = \(x::forall a. Eq a => a->a) -> error "urk" (x t)
	-- where 't' is fresh ty var.  This leads directly to "ambiguous t".
	-- 
	-- NB: technically this changes the meaning of the default-default
	--     method slightly, because `seq` can see the lambdas.  Oh well.
    (_,_,tau1)    = tcSplitSigmaTy (idType sel_id)
    (_,_,tau2)    = tcSplitSigmaTy tau1
	-- Need two splits because the  selector can have a type like
	-- 	forall a. Foo a => forall b. Eq b => ...
    (arg_tys, _) = tcSplitFunTys tau2
    wild_pats	 = [nlWildPat | ty <- arg_tys]

mkDefMethRhs origin clas inst_tys sel_id loc GenDefMeth 
  = 	-- A generic default method
    	-- If the method is defined generically, we can only do the job if the
	-- instance declaration is for a single-parameter type class with
	-- a type constructor applied to type arguments in the instance decl
	-- 	(checkTc, so False provokes the error)
    ASSERT( isInstDecl origin )	-- We never get here from a class decl
    do	{ checkTc (isJust maybe_tycon)
	 	  (badGenericInstance sel_id (notSimple inst_tys))
	; checkTc (tyConHasGenerics tycon)
	   	  (badGenericInstance sel_id (notGeneric tycon))

	; dflags <- getDOpts
	; ioToTcRn (dumpIfSet_dyn dflags Opt_D_dump_deriv "Filling in method body" 
		   (vcat [ppr clas <+> ppr inst_tys,
			  nest 2 (ppr sel_id <+> equals <+> ppr rhs)]))

		-- Rename it before returning it
	; (rn_rhs, _) <- rnLExpr rhs
	; returnM rn_rhs }
  where
    rhs = mkGenericRhs sel_id clas_tyvar tycon

	  -- The tycon is only used in the generic case, and in that
	  -- case we require that the instance decl is for a single-parameter
	  -- type class with type variable arguments:
	  --	instance (...) => C (T a b)
    clas_tyvar    = head (classTyVars clas)
    Just tycon	  = maybe_tycon
    maybe_tycon   = case inst_tys of 
			[ty] -> case tcSplitTyConApp_maybe ty of
				  Just (tycon, arg_tys) | all tcIsTyVarTy arg_tys -> Just tycon
				  other						  -> Nothing
			other -> Nothing

isInstDecl (SigOrigin (InstSkol _)) = True
isInstDecl (SigOrigin (ClsSkol _))  = False
\end{code}


\begin{code}
-- The renamer just puts the selector ID as the binder in the method binding
-- but we must use the method name; so we substitute it here.  Crude but simple.
find_bind sel_name meth_name binds
  = foldlBag seqMaybe Nothing (mapBag f binds)
  where 
	f (L loc1 bind@(FunBind { fun_id = L loc2 op_name })) | op_name == sel_name
		 = Just (L loc1 (bind { fun_id = L loc2 meth_name }))
	f _other = Nothing
\end{code}


%************************************************************************
%*									*
\subsection{Extracting generic instance declaration from class declarations}
%*									*
%************************************************************************

@getGenericInstances@ extracts the generic instance declarations from a class
declaration.  For exmaple

	class C a where
	  op :: a -> a
	
	  op{ x+y } (Inl v)   = ...
	  op{ x+y } (Inr v)   = ...
	  op{ x*y } (v :*: w) = ...
	  op{ 1   } Unit      = ...

gives rise to the instance declarations

	instance C (x+y) where
	  op (Inl v)   = ...
	  op (Inr v)   = ...
	
	instance C (x*y) where
	  op (v :*: w) = ...

	instance C 1 where
	  op Unit      = ...


\begin{code}
getGenericInstances :: [LTyClDecl Name] -> TcM [InstInfo] 
getGenericInstances class_decls
  = do	{ gen_inst_infos <- mappM (addLocM get_generics) class_decls
	; let { gen_inst_info = concat gen_inst_infos }

	-- Return right away if there is no generic stuff
	; if null gen_inst_info then returnM []
	  else do 

	-- Otherwise print it out
	{ dflags <- getDOpts
	; ioToTcRn (dumpIfSet_dyn dflags Opt_D_dump_deriv "Generic instances" 
	 	   (vcat (map pprInstInfoDetails gen_inst_info)))	
	; returnM gen_inst_info }}

get_generics decl@(ClassDecl {tcdLName = class_name, tcdMeths = def_methods})
  | null generic_binds
  = returnM [] -- The comon case: no generic default methods

  | otherwise	-- A source class decl with generic default methods
  = recoverM (returnM [])				$
    tcAddDeclCtxt decl					$
    tcLookupLocatedClass class_name			`thenM` \ clas ->

	-- Group by type, and
	-- make an InstInfo out of each group
    let
	groups = groupWith listToBag generic_binds
    in
    mappM (mkGenericInstance clas) groups		`thenM` \ inst_infos ->

	-- Check that there is only one InstInfo for each type constructor
  	-- The main way this can fail is if you write
	--	f {| a+b |} ... = ...
	--	f {| x+y |} ... = ...
	-- Then at this point we'll have an InstInfo for each
    let
	tc_inst_infos :: [(TyCon, InstInfo)]
	tc_inst_infos = [(simpleInstInfoTyCon i, i) | i <- inst_infos]

	bad_groups = [group | group <- equivClassesByUniq get_uniq tc_inst_infos,
			      group `lengthExceeds` 1]
	get_uniq (tc,_) = getUnique tc
    in
    mappM (addErrTc . dupGenericInsts) bad_groups	`thenM_`

	-- Check that there is an InstInfo for each generic type constructor
    let
	missing = genericTyConNames `minusList` [tyConName tc | (tc,_) <- tc_inst_infos]
    in
    checkTc (null missing) (missingGenericInstances missing)	`thenM_`

    returnM inst_infos
  where
    generic_binds :: [(HsType Name, LHsBind Name)]
    generic_binds = getGenericBinds def_methods


---------------------------------
getGenericBinds :: LHsBinds Name -> [(HsType Name, LHsBind Name)]
  -- Takes a group of method bindings, finds the generic ones, and returns
  -- them in finite map indexed by the type parameter in the definition.
getGenericBinds binds = concat (map getGenericBind (bagToList binds))

getGenericBind (L loc bind@(FunBind { fun_matches = MatchGroup matches ty }))
  = groupWith wrap (mapCatMaybes maybeGenericMatch matches)
  where
    wrap ms = L loc (bind { fun_matches = MatchGroup ms ty })
getGenericBind _
  = []

groupWith :: ([a] -> b) -> [(HsType Name, a)] -> [(HsType Name, b)]
groupWith op [] 	 = []
groupWith op ((t,v):prs) = (t, op (v:vs)) : groupWith op rest
    where
      vs            = map snd this
      (this,rest)   = partition same_t prs
      same_t (t',v) = t `eqPatType` t'

eqPatLType :: LHsType Name -> LHsType Name -> Bool
eqPatLType t1 t2 = unLoc t1 `eqPatType` unLoc t2

eqPatType :: HsType Name -> HsType Name -> Bool
-- A very simple equality function, only for 
-- type patterns in generic function definitions.
eqPatType (HsTyVar v1)       (HsTyVar v2)    	= v1==v2
eqPatType (HsAppTy s1 t1)    (HsAppTy s2 t2) 	= s1 `eqPatLType` s2 && t2 `eqPatLType` t2
eqPatType (HsOpTy s1 op1 t1) (HsOpTy s2 op2 t2) = s1 `eqPatLType` s2 && t2 `eqPatLType` t2 && unLoc op1 == unLoc op2
eqPatType (HsNumTy n1)	     (HsNumTy n2)	= n1 == n2
eqPatType (HsParTy t1)	     t2			= unLoc t1 `eqPatType` t2
eqPatType t1		     (HsParTy t2)	= t1 `eqPatType` unLoc t2
eqPatType _ _ = False

---------------------------------
mkGenericInstance :: Class
		  -> (HsType Name, LHsBinds Name)
		  -> TcM InstInfo

mkGenericInstance clas (hs_ty, binds)
  -- Make a generic instance declaration
  -- For example:	instance (C a, C b) => C (a+b) where { binds }

  = 	-- Extract the universally quantified type variables
	-- and wrap them as forall'd tyvars, so that kind inference
	-- works in the standard way
    let
	sig_tvs = map (noLoc.UserTyVar) (nameSetToList (extractHsTyVars (noLoc hs_ty)))
	hs_forall_ty = noLoc $ mkExplicitHsForAllTy sig_tvs (noLoc []) (noLoc hs_ty)
    in
	-- Type-check the instance type, and check its form
    tcHsSigType GenPatCtxt hs_forall_ty		`thenM` \ forall_inst_ty ->
    let
	(tyvars, inst_ty) = tcSplitForAllTys forall_inst_ty
    in
    checkTc (validGenericInstanceType inst_ty)
	    (badGenericInstanceType binds)	`thenM_`

	-- Make the dictionary function.
    getSrcSpanM						`thenM` \ span -> 
    getOverlapFlag					`thenM` \ overlap_flag -> 
    newDFunName clas [inst_ty] (srcSpanStart span)	`thenM` \ dfun_name ->
    let
	inst_theta = [mkClassPred clas [mkTyVarTy tv] | tv <- tyvars]
	dfun_id    = mkDictFunId dfun_name tyvars inst_theta clas [inst_ty]
	ispec	   = mkLocalInstance dfun_id overlap_flag
    in
    returnM (InstInfo { iSpec = ispec, iBinds = VanillaInst binds [] })
\end{code}


%************************************************************************
%*									*
		Error messages
%*									*
%************************************************************************

\begin{code}
tcAddDeclCtxt decl thing_inside
  = addErrCtxt ctxt thing_inside
  where
     thing = case decl of
	   	ClassDecl {}		  -> "class"
		TySynonym {}		  -> "type synonym"
		TyData {tcdND = NewType}  -> "newtype"
		TyData {tcdND = DataType} -> "data type"

     ctxt = hsep [ptext SLIT("In the"), text thing, 
		  ptext SLIT("declaration for"), quotes (ppr (tcdName decl))]

defltMethCtxt clas
  = ptext SLIT("When checking the default methods for class") <+> quotes (ppr clas)

methodCtxt sel_id
  = ptext SLIT("In the definition for method") <+> quotes (ppr sel_id)

badMethodErr clas op
  = hsep [ptext SLIT("Class"), quotes (ppr clas), 
	  ptext SLIT("does not have a method"), quotes (ppr op)]

omittedMethodWarn sel_id
  = ptext SLIT("No explicit method nor default method for") <+> quotes (ppr sel_id)

badGenericInstance sel_id because
  = sep [ptext SLIT("Can't derive generic code for") <+> quotes (ppr sel_id),
	 because]

notSimple inst_tys
  = vcat [ptext SLIT("because the instance type(s)"), 
	  nest 2 (ppr inst_tys),
	  ptext SLIT("is not a simple type of form (T a b c)")]

notGeneric tycon
  = vcat [ptext SLIT("because the instance type constructor") <+> quotes (ppr tycon) <+> 
	  ptext SLIT("was not compiled with -fgenerics")]

badGenericInstanceType binds
  = vcat [ptext SLIT("Illegal type pattern in the generic bindings"),
	  nest 4 (ppr binds)]

missingGenericInstances missing
  = ptext SLIT("Missing type patterns for") <+> pprQuotedList missing
	  
dupGenericInsts tc_inst_infos
  = vcat [ptext SLIT("More than one type pattern for a single generic type constructor:"),
	  nest 4 (vcat (map ppr_inst_ty tc_inst_infos)),
	  ptext SLIT("All the type patterns for a generic type constructor must be identical")
    ]
  where 
    ppr_inst_ty (_,inst) = ppr (simpleInstInfoTy inst)

mixedGenericErr op
  = ptext SLIT("Can't mix generic and non-generic equations for class method") <+> quotes (ppr op)
\end{code}
