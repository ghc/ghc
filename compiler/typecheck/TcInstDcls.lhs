%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcInstDecls]{Typechecking instance declarations}

\begin{code}
module TcInstDcls ( tcInstDecls1, tcInstDecls2 ) where

#include "HsVersions.h"

import HsSyn
import TcBinds		( mkPragFun, tcPrags, badBootDeclErr )
import TcTyClsDecls     ( tcIdxTyInstDecl )
import TcClassDcl	( tcMethodBind, mkMethodBind, badMethodErr, badATErr,
			  omittedATWarn, tcClassDecl2, getGenericInstances )
import TcRnMonad       
import TcMType		( tcSkolSigType, checkValidInstance,
			  checkValidInstHead )
import TcType		( TcType, mkClassPred, tcSplitSigmaTy,
			  tcSplitDFunHead,  SkolemInfo(InstSkol),
			  tcSplitTyConApp, 
			  tcSplitDFunTy, mkFunTy ) 
import Inst		( newDictBndr, newDictBndrs, instToId, showLIE, 
			  getOverlapFlag, tcExtendLocalInstEnv )
import InstEnv		( mkLocalInstance, instanceDFunId )
import FamInst		( tcExtendLocalFamInstEnv )
import FamInstEnv	( extractFamInsts )
import TcDeriv		( tcDeriving )
import TcEnv		( InstInfo(..), InstBindings(..), 
			  newDFunName, tcExtendIdEnv, tcExtendGlobalEnv
			)
import TcHsType		( kcHsSigType, tcHsKindedType )
import TcUnify		( checkSigTyVars )
import TcSimplify	( tcSimplifySuperClasses )
import Type		( zipOpenTvSubst, substTheta, mkTyConApp, mkTyVarTy,
                          TyThing(ATyCon), isTyVarTy, tcEqType,
                          substTys, emptyTvSubst, extendTvSubst )
import Coercion         ( mkSymCoercion )
import TyCon            ( TyCon, tyConName, newTyConCo_maybe, tyConTyVars,
			  isTyConAssoc, tyConFamInst_maybe,
			  assocTyConArgPoss_maybe )
import DataCon		( classDataCon, dataConInstArgTys )
import Class		( Class, classTyCon, classBigSig, classATs )
import Var		( TyVar, Id, idName, idType, tyVarName )
import MkId		( mkDictFunId )
import Name		( Name, getSrcLoc, nameOccName )
import NameSet		( addListToNameSet, emptyNameSet, minusNameSet,
			  nameSetToList ) 
import Maybe		( fromJust, catMaybes )
import Monad		( when )
import List		( find )
import DynFlags		( DynFlag(Opt_WarnMissingMethods) )
import SrcLoc		( srcLocSpan, unLoc, noLoc, Located(..), srcSpanStart,
			  getLoc)
import ListSetOps	( minusList )
import Util		( snocView, dropList )
import Outputable
import Bag
import BasicTypes	( Activation( AlwaysActive ), InlineSpec(..) )
import HscTypes		( implicitTyThings )
import FastString
\end{code}

Typechecking instance declarations is done in two passes. The first
pass, made by @tcInstDecls1@, collects information to be used in the
second pass.

This pre-processed info includes the as-yet-unprocessed bindings
inside the instance declaration.  These are type-checked in the second
pass, when the class-instance envs and GVE contain all the info from
all the instance and value decls.  Indeed that's the reason we need
two passes over the instance decls.

Here is the overall algorithm.
Assume that we have an instance declaration

    instance c => k (t tvs) where b

\begin{enumerate}
\item
$LIE_c$ is the LIE for the context of class $c$
\item
$betas_bar$ is the free variables in the class method type, excluding the
   class variable
\item
$LIE_cop$ is the LIE constraining a particular class method
\item
$tau_cop$ is the tau type of a class method
\item
$LIE_i$ is the LIE for the context of instance $i$
\item
$X$ is the instance constructor tycon
\item
$gammas_bar$ is the set of type variables of the instance
\item
$LIE_iop$ is the LIE for a particular class method instance
\item
$tau_iop$ is the tau type for this instance of a class method
\item
$alpha$ is the class variable
\item
$LIE_cop' = LIE_cop [X gammas_bar / alpha, fresh betas_bar]$
\item
$tau_cop' = tau_cop [X gammas_bar / alpha, fresh betas_bar]$
\end{enumerate}

ToDo: Update the list above with names actually in the code.

\begin{enumerate}
\item
First, make the LIEs for the class and instance contexts, which means
instantiate $thetaC [X inst_tyvars / alpha ]$, yielding LIElistC' and LIEC',
and make LIElistI and LIEI.
\item
Then process each method in turn.
\item
order the instance methods according to the ordering of the class methods
\item
express LIEC' in terms of LIEI, yielding $dbinds_super$ or an error
\item
Create final dictionary function from bindings generated already
\begin{pseudocode}
df = lambda inst_tyvars
       lambda LIEI
	 let Bop1
	     Bop2
	     ...
	     Bopn
	 and dbinds_super
	      in <op1,op2,...,opn,sd1,...,sdm>
\end{pseudocode}
Here, Bop1 \ldots Bopn bind the methods op1 \ldots opn,
and $dbinds_super$ bind the superclass dictionaries sd1 \ldots sdm.
\end{enumerate}


%************************************************************************
%*									*
\subsection{Extracting instance decls}
%*									*
%************************************************************************

Gather up the instance declarations from their various sources

\begin{code}
tcInstDecls1	-- Deal with both source-code and imported instance decls
   :: [LTyClDecl Name]		-- For deriving stuff
   -> [LInstDecl Name]		-- Source code instance decls
   -> TcM (TcGblEnv,		-- The full inst env
	   [InstInfo],		-- Source-code instance decls to process; 
				-- contains all dfuns for this module
	   HsValBinds Name)	-- Supporting bindings for derived instances

tcInstDecls1 tycl_decls inst_decls
  = checkNoErrs $
    do {        -- Stop if addInstInfos etc discovers any errors
		-- (they recover, so that we get more than one error each
		-- round) 

		-- (1) Do class instance declarations and instances of indexed
		--     types 
       ; let { idxty_decls = filter (isIdxTyDecl . unLoc) tycl_decls }
       ; local_info_tycons <- mappM tcLocalInstDecl1  inst_decls
       ; idx_tycons        <- mappM tcIdxTyInstDeclTL idxty_decls

       ; let { (local_infos,
	        at_tycons)     = unzip local_info_tycons
	     ; local_info      = concat local_infos
	     ; at_idx_tycon    = concat at_tycons ++ catMaybes idx_tycons
	     ; clas_decls      = filter (isClassDecl.unLoc) tycl_decls 
	     ; implicit_things = concatMap implicitTyThings at_idx_tycon
	     }

	        -- (2) Add the tycons of indexed types and their implicit
	        --     tythings to the global environment
       ; tcExtendGlobalEnv (at_idx_tycon ++ implicit_things) $ do {

	        -- (3) Instances from generic class declarations
       ; generic_inst_info <- getGenericInstances clas_decls

	        -- Next, construct the instance environment so far, consisting
	        -- of 
		--   a) local instance decls
		--   b) generic instances
		--   c) local family instance decls
       ; addInsts local_info         $ do {
       ; addInsts generic_inst_info  $ do {
       ; addFamInsts at_idx_tycon    $ do {

	        -- (4) Compute instances from "deriving" clauses; 
		-- This stuff computes a context for the derived instance
		-- decl, so it needs to know about all the instances possible
       ; (deriv_inst_info, deriv_binds) <- tcDeriving tycl_decls
       ; addInsts deriv_inst_info   $ do {

       ; gbl_env <- getGblEnv
       ; returnM (gbl_env, 
		  generic_inst_info ++ deriv_inst_info ++ local_info,
		  deriv_binds) 
    }}}}}}
  where
    -- Make sure that toplevel type instance are not for associated types.
    -- !!!TODO: Need to perform this check for the TyThing of type functions,
    --		too.
    tcIdxTyInstDeclTL ldecl@(L loc decl) =
      do { tything <- tcIdxTyInstDecl ldecl
	 ; setSrcSpan loc $
	     when (isAssocFamily tything) $
	       addErr $ assocInClassErr (tcdName decl)
	 ; return tything
	 }
    isAssocFamily (Just (ATyCon tycon)) =
      case tyConFamInst_maybe tycon of
        Nothing       -> panic "isAssocFamily: no family?!?"
	Just (fam, _) -> isTyConAssoc fam
    isAssocFamily (Just _	      ) = panic "isAssocFamily: no tycon?!?"
    isAssocFamily Nothing               = False

assocInClassErr name = 
  ptext SLIT("Associated type") <+> quotes (ppr name) <+> 
  ptext SLIT("must be inside a class instance")

addInsts :: [InstInfo] -> TcM a -> TcM a
addInsts infos thing_inside
  = tcExtendLocalInstEnv (map iSpec infos) thing_inside

addFamInsts :: [TyThing] -> TcM a -> TcM a
addFamInsts tycons thing_inside
  = tcExtendLocalFamInstEnv (extractFamInsts tycons) thing_inside
\end{code} 

\begin{code}
tcLocalInstDecl1 :: LInstDecl Name 
		 -> TcM ([InstInfo], [TyThing])	-- [] if there was an error
	-- A source-file instance declaration
	-- Type-check all the stuff before the "where"
	--
	-- We check for respectable instance type, and context
tcLocalInstDecl1 decl@(L loc (InstDecl poly_ty binds uprags ats))
  =	-- Prime error recovery, set source location
    recoverM (returnM ([], []))		$
    setSrcSpan loc			$
    addErrCtxt (instDeclCtxt1 poly_ty)	$

    do	{ is_boot <- tcIsHsBoot
	; checkTc (not is_boot || (isEmptyLHsBinds binds && null uprags))
		  badBootDeclErr

	-- Typecheck the instance type itself.  We can't use 
	-- tcHsSigType, because it's not a valid user type.
	; kinded_ty <- kcHsSigType poly_ty
	; poly_ty'  <- tcHsKindedType kinded_ty
	; let (tyvars, theta, tau) = tcSplitSigmaTy poly_ty'
	
	-- Next, process any associated types.
	; idx_tycons <- mappM tcIdxTyInstDecl ats

	-- Now, check the validity of the instance.
	; (clas, inst_tys) <- checkValidInstHead tau
	; checkValidInstance tyvars theta clas inst_tys
	; checkValidAndMissingATs clas (tyvars, inst_tys) 
				  (zip ats idx_tycons)

	-- Finally, construct the Core representation of the instance.
	-- (This no longer includes the associated types.)
	; dfun_name <- newDFunName clas inst_tys (srcSpanStart loc)
	; overlap_flag <- getOverlapFlag
	; let dfun           = mkDictFunId dfun_name tyvars theta clas inst_tys
	      ispec          = mkLocalInstance dfun overlap_flag

	; return ([InstInfo { iSpec  = ispec, 
			      iBinds = VanillaInst binds uprags }],
		  catMaybes idx_tycons)
        }
  where
    -- We pass in the source form and the type checked form of the ATs.  We
    -- really need the source form only to be able to produce more informative
    -- error messages.
    checkValidAndMissingATs :: Class
			    -> ([TyVar], [TcType])     -- instance types
			    -> [(LTyClDecl Name,       -- source form of AT
			         Maybe TyThing)]       -- Core form of AT
			    -> TcM ()
    checkValidAndMissingATs clas inst_tys ats
      = do { -- Issue a warning for each class AT that is not defined in this
	     -- instance.
	   ; let classDefATs = listToNameSet . map tyConName . classATs $ clas
                 definedATs  = listToNameSet . map (tcdName.unLoc.fst)  $ ats
		 omitted     = classDefATs `minusNameSet` definedATs
	   ; warn <- doptM Opt_WarnMissingMethods
	   ; mapM_ (warnTc warn . omittedATWarn) (nameSetToList omitted)
	   
	     -- Ensure that all AT indexes that correspond to class parameters
	     -- coincide with the types in the instance head.  All remaining
	     -- AT arguments must be variables.  Also raise an error for any
	     -- type instances that are not associated with this class.
	   ; mapM_ (checkIndexes clas inst_tys) ats
	   }

    checkIndexes _    _        (hsAT, Nothing)             = 
      return ()	   -- skip, we already had an error here
    checkIndexes clas inst_tys (hsAT, Just (ATyCon tycon)) = 
-- !!!TODO: check that this does the Right Thing for indexed synonyms, too!
      checkIndexes' clas inst_tys hsAT 
		    (tyConTyVars tycon, 
		     snd . fromJust . tyConFamInst_maybe $ tycon)
    checkIndexes _ _ _ = panic "checkIndexes"

    checkIndexes' clas (instTvs, instTys) hsAT (atTvs, atTys)
      = let atName = tcdName . unLoc $ hsAT
	in
	setSrcSpan (getLoc hsAT)       $
	addErrCtxt (atInstCtxt atName) $
	case find ((atName ==) . tyConName) (classATs clas) of
	  Nothing     -> addErrTc $ badATErr clas atName  -- not in this class
	  Just atDecl -> 
	    case assocTyConArgPoss_maybe atDecl of
	      Nothing   -> panic "checkIndexes': AT has no args poss?!?"
	      Just poss -> 

	        -- The following is tricky!  We need to deal with three
	        -- complications: (1) The AT possibly only uses a subset of
	        -- the class parameters as indexes and those it uses may be in
	        -- a different order; (2) the AT may have extra arguments,
	        -- which must be type variables; and (3) variables in AT and
	        -- instance head will be different `Name's even if their
	        -- source lexemes are identical.
		--
		-- Re (1), `poss' contains a permutation vector to extract the
		-- class parameters in the right order.
		--
		-- Re (2), we wrap the (permuted) class parameters in a Maybe
		-- type and use Nothing for any extra AT arguments.  (First
		-- equation of `checkIndex' below.)
		--
		-- Re (3), we replace any type variable in the AT parameters
		-- that has the same source lexeme as some variable in the
		-- instance types with the instance type variable sharing its
		-- source lexeme.
		--
	        let relevantInstTys = map (instTys !!) poss
		    instArgs	    = map Just relevantInstTys ++ 
				      repeat Nothing  -- extra arguments
		    renaming        = substSameTyVar atTvs instTvs
		in
	        zipWithM_ checkIndex (substTys renaming atTys) instArgs

    checkIndex ty Nothing 
      | isTyVarTy ty         = return ()
      | otherwise            = addErrTc $ mustBeVarArgErr ty
    checkIndex ty (Just instTy) 
      | ty `tcEqType` instTy = return ()
      | otherwise            = addErrTc $ wrongATArgErr ty instTy

    listToNameSet = addListToNameSet emptyNameSet 

    substSameTyVar []       _            = emptyTvSubst
    substSameTyVar (tv:tvs) replacingTvs = 
      let replacement = case find (tv `sameLexeme`) replacingTvs of
			  Nothing  -> mkTyVarTy tv
			  Just rtv -> mkTyVarTy rtv
          --
          tv1 `sameLexeme` tv2 = 
	    nameOccName (tyVarName tv1) == nameOccName (tyVarName tv2)
      in
      extendTvSubst (substSameTyVar tvs replacingTvs) tv replacement
\end{code}


%************************************************************************
%*									*
\subsection{Type-checking instance declarations, pass 2}
%*									*
%************************************************************************

\begin{code}
tcInstDecls2 :: [LTyClDecl Name] -> [InstInfo] 
	     -> TcM (LHsBinds Id, TcLclEnv)
-- (a) From each class declaration, 
--	generate any default-method bindings
-- (b) From each instance decl
--	generate the dfun binding

tcInstDecls2 tycl_decls inst_decls
  = do	{	-- (a) Default methods from class decls
	  (dm_binds_s, dm_ids_s) <- mapAndUnzipM tcClassDecl2 $
			            filter (isClassDecl.unLoc) tycl_decls
	; tcExtendIdEnv (concat dm_ids_s) 	$ do 
    
	 	-- (b) instance declarations
	; inst_binds_s <- mappM tcInstDecl2 inst_decls

		-- Done
	; let binds = unionManyBags dm_binds_s `unionBags` 
		      unionManyBags inst_binds_s
	; tcl_env <- getLclEnv		-- Default method Ids in here
	; returnM (binds, tcl_env) }
\end{code}

======= New documentation starts here (Sept 92)	 ==============

The main purpose of @tcInstDecl2@ is to return a @HsBinds@ which defines
the dictionary function for this instance declaration.	For example
\begin{verbatim}
	instance Foo a => Foo [a] where
		op1 x = ...
		op2 y = ...
\end{verbatim}
might generate something like
\begin{verbatim}
	dfun.Foo.List dFoo_a = let op1 x = ...
				   op2 y = ...
			       in
				   Dict [op1, op2]
\end{verbatim}

HOWEVER, if the instance decl has no context, then it returns a
bigger @HsBinds@ with declarations for each method.  For example
\begin{verbatim}
	instance Foo [a] where
		op1 x = ...
		op2 y = ...
\end{verbatim}
might produce
\begin{verbatim}
	dfun.Foo.List a = Dict [Foo.op1.List a, Foo.op2.List a]
	const.Foo.op1.List a x = ...
	const.Foo.op2.List a y = ...
\end{verbatim}
This group may be mutually recursive, because (for example) there may
be no method supplied for op2 in which case we'll get
\begin{verbatim}
	const.Foo.op2.List a = default.Foo.op2 (dfun.Foo.List a)
\end{verbatim}
that is, the default method applied to the dictionary at this type.

What we actually produce in either case is:

	AbsBinds [a] [dfun_theta_dicts]
		 [(dfun.Foo.List, d)] ++ (maybe) [(const.Foo.op1.List, op1), ...]
		 { d = (sd1,sd2, ..., op1, op2, ...)
		   op1 = ...
		   op2 = ...
	 	 }

The "maybe" says that we only ask AbsBinds to make global constant methods
if the dfun_theta is empty.

		
For an instance declaration, say,

	instance (C1 a, C2 b) => C (T a b) where
		...

where the {\em immediate} superclasses of C are D1, D2, we build a dictionary
function whose type is

	(C1 a, C2 b, D1 (T a b), D2 (T a b)) => C (T a b)

Notice that we pass it the superclass dictionaries at the instance type; this
is the ``Mark Jones optimisation''.  The stuff before the "=>" here
is the @dfun_theta@ below.

First comes the easy case of a non-local instance decl.


\begin{code}
tcInstDecl2 :: InstInfo -> TcM (LHsBinds Id)
-- Returns a binding for the dfun

------------------------
-- Derived newtype instances
--
-- In the case of a newtype, things are rather easy
-- 	class Show a => Foo a b where ...
-- 	newtype T a = MkT (Tree [a]) deriving( Foo Int )
-- The newtype gives an FC axiom looking like
--	axiom CoT a ::  T a :=: Tree [a]
--   (see Note [Newtype coercions] in TyCon for this unusual form of axiom)
--
-- So all need is to generate a binding looking like: 
-- 	dfunFooT :: forall a. (Foo Int (Tree [a], Show (T a)) => Foo Int (T a)
--	dfunFooT = /\a. \(ds:Show (T a)) (df:Foo (Tree [a])).
--		  case df `cast` (Foo Int (sym (CoT a))) of
--		     Foo _ op1 .. opn -> Foo ds op1 .. opn
--
-- If there are no superclasses, matters are simpler, because we don't need the case
-- see Note [Newtype deriving superclasses] in TcDeriv.lhs

tcInstDecl2 (InstInfo { iSpec = ispec, iBinds = NewTypeDerived mb_preds })
  = do	{ let dfun_id      = instanceDFunId ispec 
	      rigid_info   = InstSkol dfun_id
	      origin	   = SigOrigin rigid_info
	      inst_ty      = idType dfun_id
	; (tvs, theta, inst_head_ty) <- tcSkolSigType rigid_info inst_ty
		-- inst_head_ty is a PredType

	; inst_loc <- getInstLoc origin
	; (rep_dict_id : sc_dict_ids, wrap_fn)
		<- make_wrapper inst_loc tvs theta mb_preds
		-- Here, we are relying on the order of dictionary 
		-- arguments built by NewTypeDerived in TcDeriv; 
		-- namely, that the rep_dict_id comes first
	   
        ; let (cls, cls_inst_tys) = tcSplitDFunHead inst_head_ty
	      the_coercion     = make_coercion cls cls_inst_tys
              coerced_rep_dict = mkHsCoerce the_coercion (HsVar rep_dict_id)

	; body <- make_body cls cls_inst_tys inst_head_ty sc_dict_ids coerced_rep_dict
              
        ; return (unitBag (noLoc $ VarBind dfun_id $ noLoc $ mkHsCoerce wrap_fn body)) }
  where

      -----------------------
      -- 	make_wrapper
      -- We distinguish two cases:
      -- (a) there is no tyvar abstraction in the dfun, so all dicts are constant,
      --     and the new dict can just be a constant
      --	(mb_preds = Just preds)
      -- (b) there are tyvars, so we must make a dict *fun*
      --	(mb_preds = Nothing)
      -- See the defn of NewTypeDerived for the meaning of mb_preds
    make_wrapper inst_loc tvs theta (Just preds)	-- Case (a)
      = ASSERT( null tvs && null theta )
	do { dicts <- newDictBndrs inst_loc preds
	   ; extendLIEs dicts
	   ; return (map instToId dicts, idCoercion) }
    make_wrapper inst_loc tvs theta Nothing 	-- Case (b)
      = do { dicts <- newDictBndrs inst_loc theta
	   ; let dict_ids = map instToId dicts
	   ; return (dict_ids, mkCoTyLams tvs <.> mkCoLams dict_ids) }

      -----------------------
      -- 	make_coercion
      -- The inst_head looks like (C s1 .. sm (T a1 .. ak))
      -- But we want the coercion (C s1 .. sm (sym (CoT a1 .. ak)))
      --	with kind (C s1 .. sm (T a1 .. ak)  :=:  C s1 .. sm <rep_ty>)
      --	where rep_ty is the (eta-reduced) type rep of T
      -- So we just replace T with CoT, and insert a 'sym'
      -- NB: we know that k will be >= arity of CoT, because the latter fully eta-reduced

    make_coercion cls cls_inst_tys
	| Just (all_tys_but_last, last_ty) <- snocView cls_inst_tys
	, (tycon, tc_args) <- tcSplitTyConApp last_ty	-- Should not fail
	, Just co_con <- newTyConCo_maybe tycon
	, let co = mkSymCoercion (mkTyConApp co_con tc_args)
        = ExprCoFn (mkTyConApp cls_tycon (all_tys_but_last ++ [co]))
        | otherwise	-- The newtype is transparent; no need for a cast
        = idCoercion
	where
          cls_tycon = classTyCon cls

      -----------------------
      -- 	make_body
      -- Two cases; see Note [Newtype deriving superclasses] in TcDeriv.lhs
      -- (a) no superclasses; then we can just use the coerced dict
      -- (b) one or more superclasses; then new need to do the unpack/repack
	
    make_body cls cls_inst_tys inst_head_ty sc_dict_ids coerced_rep_dict
	| null sc_dict_ids		-- Case (a)
	= return coerced_rep_dict
	| otherwise 			-- Case (b)
 	= do { op_ids		 <- newSysLocalIds FSLIT("op") op_tys
	     ; dummy_sc_dict_ids <- newSysLocalIds FSLIT("sc") (map idType sc_dict_ids)
	     ; let the_pat = ConPatOut { pat_con = noLoc cls_data_con, pat_tvs = [],
				    	 pat_dicts = dummy_sc_dict_ids,
				    	 pat_binds = emptyLHsBinds,
				    	 pat_args = PrefixCon (map nlVarPat op_ids),
				    	 pat_ty = inst_head_ty} 
	           the_match = mkSimpleMatch [noLoc the_pat] the_rhs
	           the_rhs = mkHsConApp cls_data_con cls_inst_tys $
			     map HsVar (sc_dict_ids ++ op_ids)

	     ; return (HsCase (noLoc coerced_rep_dict) $
		       MatchGroup [the_match] (mkFunTy inst_head_ty inst_head_ty)) }
	where
          cls_data_con = classDataCon cls
          cls_arg_tys  = dataConInstArgTys cls_data_con cls_inst_tys 
          op_tys       = dropList sc_dict_ids cls_arg_tys

------------------------
-- Ordinary instances

tcInstDecl2 (InstInfo { iSpec = ispec, iBinds = VanillaInst monobinds uprags })
  = let 
	dfun_id    = instanceDFunId ispec
	rigid_info = InstSkol dfun_id
	inst_ty    = idType dfun_id
    in
	 -- Prime error recovery
    recoverM (returnM emptyLHsBinds)		$
    setSrcSpan (srcLocSpan (getSrcLoc dfun_id))	$
    addErrCtxt (instDeclCtxt2 (idType dfun_id))	$

	-- Instantiate the instance decl with skolem constants 
    tcSkolSigType rigid_info inst_ty	`thenM` \ (inst_tyvars', dfun_theta', inst_head') ->
		-- These inst_tyvars' scope over the 'where' part
		-- Those tyvars are inside the dfun_id's type, which is a bit
		-- bizarre, but OK so long as you realise it!
    let
	(clas, inst_tys') = tcSplitDFunHead inst_head'
        (class_tyvars, sc_theta, _, op_items) = classBigSig clas

        -- Instantiate the super-class context with inst_tys
	sc_theta' = substTheta (zipOpenTvSubst class_tyvars inst_tys') sc_theta
	origin	  = SigOrigin rigid_info
    in
	 -- Create dictionary Ids from the specified instance contexts.
    getInstLoc InstScOrigin				`thenM` \ sc_loc -> 
    newDictBndrs sc_loc sc_theta'			`thenM` \ sc_dicts ->
    getInstLoc origin					`thenM` \ inst_loc -> 
    newDictBndrs inst_loc dfun_theta'			`thenM` \ dfun_arg_dicts ->
    newDictBndr inst_loc (mkClassPred clas inst_tys') 	`thenM` \ this_dict ->
		-- Default-method Ids may be mentioned in synthesised RHSs,
		-- but they'll already be in the environment.

	-- Typecheck the methods
    let		-- These insts are in scope; quite a few, eh?
	avail_insts = [this_dict] ++ dfun_arg_dicts ++ sc_dicts
    in
    tcMethods origin clas inst_tyvars' 
	      dfun_theta' inst_tys' avail_insts 
	      op_items monobinds uprags		`thenM` \ (meth_ids, meth_binds) ->

	-- Figure out bindings for the superclass context
	-- Don't include this_dict in the 'givens', else
	-- sc_dicts get bound by just selecting  from this_dict!!
    addErrCtxt superClassCtxt
	(tcSimplifySuperClasses inst_tyvars'
			 dfun_arg_dicts
			 sc_dicts)	`thenM` \ sc_binds ->

	-- It's possible that the superclass stuff might unified one
	-- of the inst_tyavars' with something in the envt
    checkSigTyVars inst_tyvars' 	`thenM_`

	-- Deal with 'SPECIALISE instance' pragmas 
    tcPrags dfun_id (filter isSpecInstLSig uprags)	`thenM` \ prags -> 
    
	-- Create the result bindings
    let
        dict_constr   = classDataCon clas
	scs_and_meths = map instToId sc_dicts ++ meth_ids
	this_dict_id  = instToId this_dict
	inline_prag | null dfun_arg_dicts = []
		    | otherwise	= [InlinePrag (Inline AlwaysActive True)]
		-- Always inline the dfun; this is an experimental decision
		-- because it makes a big performance difference sometimes.
		-- Often it means we can do the method selection, and then
		-- inline the method as well.  Marcin's idea; see comments below.
		--
		-- BUT: don't inline it if it's a constant dictionary;
		-- we'll get all the benefit without inlining, and we get
		-- a **lot** of code duplication if we inline it
		--
		--	See Note [Inline dfuns] below

	dict_rhs
	  = mkHsConApp dict_constr inst_tys' (map HsVar scs_and_meths)
		-- We don't produce a binding for the dict_constr; instead we
		-- rely on the simplifier to unfold this saturated application
		-- We do this rather than generate an HsCon directly, because
		-- it means that the special cases (e.g. dictionary with only one
		-- member) are dealt with by the common MkId.mkDataConWrapId code rather
		-- than needing to be repeated here.

	dict_bind  = noLoc (VarBind this_dict_id dict_rhs)
	all_binds  = dict_bind `consBag` (sc_binds `unionBags` meth_binds)

	main_bind = noLoc $ AbsBinds
		 	    inst_tyvars'
		 	    (map instToId dfun_arg_dicts)
		 	    [(inst_tyvars', dfun_id, this_dict_id, 
					    inline_prag ++ prags)] 
		 	    all_binds
    in
    showLIE (text "instance") 		`thenM_`
    returnM (unitBag main_bind)


tcMethods origin clas inst_tyvars' dfun_theta' inst_tys' 
	  avail_insts op_items monobinds uprags
  = 	-- Check that all the method bindings come from this class
    let
	sel_names = [idName sel_id | (sel_id, _) <- op_items]
	bad_bndrs = collectHsBindBinders monobinds `minusList` sel_names
    in
    mappM (addErrTc . badMethodErr clas) bad_bndrs	`thenM_`

	-- Make the method bindings
    let
	mk_method_bind = mkMethodBind origin clas inst_tys' monobinds
    in
    mapAndUnzipM mk_method_bind op_items 	`thenM` \ (meth_insts, meth_infos) ->

	-- And type check them
	-- It's really worth making meth_insts available to the tcMethodBind
	-- Consider	instance Monad (ST s) where
	--		  {-# INLINE (>>) #-}
	--		  (>>) = ...(>>=)...
	-- If we don't include meth_insts, we end up with bindings like this:
	--	rec { dict = MkD then bind ...
	--	      then = inline_me (... (GHC.Base.>>= dict) ...)
	--	      bind = ... }
	-- The trouble is that (a) 'then' and 'dict' are mutually recursive, 
	-- and (b) the inline_me prevents us inlining the >>= selector, which
	-- would unravel the loop.  Result: (>>) ends up as a loop breaker, and
	-- is not inlined across modules. Rather ironic since this does not
	-- happen without the INLINE pragma!  
	--
	-- Solution: make meth_insts available, so that 'then' refers directly
	-- 	     to the local 'bind' rather than going via the dictionary.
	--
	-- BUT WATCH OUT!  If the method type mentions the class variable, then
	-- this optimisation is not right.  Consider
	--	class C a where
	--	  op :: Eq a => a
	--
	--	instance C Int where
	--	  op = op
	-- The occurrence of 'op' on the rhs gives rise to a constraint
	--	op at Int
	-- The trouble is that the 'meth_inst' for op, which is 'available', also
	-- looks like 'op at Int'.  But they are not the same.
    let
	prag_fn	       = mkPragFun uprags
	all_insts      = avail_insts ++ catMaybes meth_insts
	sig_fn n       = Just []	-- No scoped type variables, but every method has
					-- a type signature, in effect, so that we check
					-- the method has the right type
	tc_method_bind = tcMethodBind inst_tyvars' dfun_theta' all_insts sig_fn prag_fn
	meth_ids       = [meth_id | (_,meth_id,_) <- meth_infos]
    in

    mapM tc_method_bind meth_infos		`thenM` \ meth_binds_s ->
   
    returnM (meth_ids, unionManyBags meth_binds_s)
\end{code}


		------------------------------
	[Inline dfuns] Inlining dfuns unconditionally
		------------------------------

The code above unconditionally inlines dict funs.  Here's why.
Consider this program:

    test :: Int -> Int -> Bool
    test x y = (x,y) == (y,x) || test y x
    -- Recursive to avoid making it inline.

This needs the (Eq (Int,Int)) instance.  If we inline that dfun
the code we end up with is good:

    Test.$wtest =
	\r -> case ==# [ww ww1] of wild {
		PrelBase.False -> Test.$wtest ww1 ww;
		PrelBase.True ->
		  case ==# [ww1 ww] of wild1 {
		    PrelBase.False -> Test.$wtest ww1 ww;
		    PrelBase.True -> PrelBase.True [];
		  };
	    };
    Test.test = \r [w w1]
	    case w of w2 {
	      PrelBase.I# ww ->
		  case w1 of w3 { PrelBase.I# ww1 -> Test.$wtest ww ww1; };
	    };

If we don't inline the dfun, the code is not nearly as good:

    (==) = case PrelTup.$fEq(,) PrelBase.$fEqInt PrelBase.$fEqInt of tpl {
	      PrelBase.:DEq tpl1 tpl2 -> tpl2;
	    };
    
    Test.$wtest =
	\r [ww ww1]
	    let { y = PrelBase.I#! [ww1]; } in
	    let { x = PrelBase.I#! [ww]; } in
	    let { sat_slx = PrelTup.(,)! [y x]; } in
	    let { sat_sly = PrelTup.(,)! [x y];
	    } in
	      case == sat_sly sat_slx of wild {
		PrelBase.False -> Test.$wtest ww1 ww;
		PrelBase.True -> PrelBase.True [];
	      };
    
    Test.test =
	\r [w w1]
	    case w of w2 {
	      PrelBase.I# ww ->
		  case w1 of w3 { PrelBase.I# ww1 -> Test.$wtest ww ww1; };
	    };

Why doesn't GHC inline $fEq?  Because it looks big:

    PrelTup.zdfEqZ1T{-rcX-}
	= \ @ a{-reT-} :: * @ b{-reS-} :: *
            zddEq{-rf6-} _Ks :: {PrelBase.Eq{-23-} a{-reT-}}
            zddEq1{-rf7-} _Ks :: {PrelBase.Eq{-23-} b{-reS-}} ->
            let {
              zeze{-rf0-} _Kl :: (b{-reS-} -> b{-reS-} -> PrelBase.Bool{-3c-})
              zeze{-rf0-} = PrelBase.zeze{-01L-}@ b{-reS-} zddEq1{-rf7-} } in
            let {
              zeze1{-rf3-} _Kl :: (a{-reT-} -> a{-reT-} -> PrelBase.Bool{-3c-})
              zeze1{-rf3-} = PrelBase.zeze{-01L-} @ a{-reT-} zddEq{-rf6-} } in
            let {
              zeze2{-reN-} :: ((a{-reT-}, b{-reS-}) -> (a{-reT-}, b{-reS-})-> PrelBase.Bool{-3c-})
              zeze2{-reN-} = \ ds{-rf5-} _Ks :: (a{-reT-}, b{-reS-})
		               ds1{-rf4-} _Ks :: (a{-reT-}, b{-reS-}) ->
                  	     case ds{-rf5-}
                  	     of wild{-reW-} _Kd { (a1{-rf2-} _Ks, a2{-reZ-} _Ks) ->
                  	     case ds1{-rf4-}
                  	     of wild1{-reX-} _Kd { (b1{-rf1-} _Ks, b2{-reY-} _Ks) ->
                  	     PrelBase.zaza{-r4e-}
                  	       (zeze1{-rf3-} a1{-rf2-} b1{-rf1-})
                  	       (zeze{-rf0-} a2{-reZ-} b2{-reY-})
                  	     }
                  	     } } in     
            let {
              a1{-reR-} :: ((a{-reT-}, b{-reS-})-> (a{-reT-}, b{-reS-})-> PrelBase.Bool{-3c-})
              a1{-reR-} = \ a2{-reV-} _Ks :: (a{-reT-}, b{-reS-})
			    b1{-reU-} _Ks :: (a{-reT-}, b{-reS-}) ->
                    	  PrelBase.not{-r6I-} (zeze2{-reN-} a2{-reV-} b1{-reU-})
            } in
              PrelBase.zdwZCDEq{-r8J-} @ (a{-reT-}, b{-reS-}) a1{-reR-} zeze2{-reN-})

and it's not as bad as it seems, because it's further dramatically
simplified: only zeze2 is extracted and its body is simplified.


%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
instDeclCtxt1 hs_inst_ty 
  = inst_decl_ctxt (case unLoc hs_inst_ty of
			HsForAllTy _ _ _ (L _ (HsPredTy pred)) -> ppr pred
			HsPredTy pred	                 -> ppr pred
			other			         -> ppr hs_inst_ty)	-- Don't expect this
instDeclCtxt2 dfun_ty
  = inst_decl_ctxt (ppr (mkClassPred cls tys))
  where
    (_,_,cls,tys) = tcSplitDFunTy dfun_ty

inst_decl_ctxt doc = ptext SLIT("In the instance declaration for") <+> quotes doc

superClassCtxt = ptext SLIT("When checking the super-classes of an instance declaration")

atInstCtxt name = ptext SLIT("In the associated type instance for") <+> 
		  quotes (ppr name)

mustBeVarArgErr ty = 
  sep [ ptext SLIT("Arguments that do not correspond to a class parameter") <+>
        ptext SLIT("must be variables")
      , ptext SLIT("Instead of a variable, found") <+> ppr ty
      ]

wrongATArgErr ty instTy =
  sep [ ptext SLIT("Type indexes must match class instance head")
      , ptext SLIT("Found") <+> ppr ty <+> ptext SLIT("but expected") <+>
         ppr instTy
      ]
\end{code}
