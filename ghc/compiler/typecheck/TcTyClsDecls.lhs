%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcTyClsDecls]{Typecheck type and class declarations}

\begin{code}
module TcTyClsDecls (
	tcTyAndClassDecls
    ) where

#include "HsVersions.h"

import HsSyn		( TyClDecl(..),  HsConDetails(..), HsTyVarBndr(..),
			  ConDecl(..),   Sig(..), BangType(..), HsBang(..),
			  tyClDeclTyVars, getBangType, getBangStrictness
			)
import RnHsSyn		( RenamedTyClDecl, RenamedConDecl )
import BasicTypes	( RecFlag(..), NewOrData(..), StrictnessMark(..) )
import HscTypes		( implicitTyThings )
import BuildTyCl	( buildClass, buildAlgTyCon, buildSynTyCon, buildDataCon )
import TcRnMonad
import TcEnv		( TcTyThing(..), TyThing(..), 
			  tcLookup, tcLookupGlobal, tcExtendGlobalEnv,
			  tcExtendRecEnv, tcLookupTyVar )
import TcTyDecls	( calcTyConArgVrcs, calcRecFlags, calcCycleErrs )
import TcClassDcl	( tcClassSigs, tcAddDeclCtxt )
import TcHsType		( kcHsTyVars, kcHsLiftedSigType, kcHsSigType, kcCheckHsType, 
			  kcHsContext, tcTyVarBndrs, tcHsKindedType, tcHsKindedContext )
import TcMType		( newKindVar, checkValidTheta, checkValidType, checkFreeness, 
			  UserTypeCtxt(..), SourceTyCtxt(..) ) 
import TcUnify		( unifyKind )
import TcType		( TcKind, ThetaType, TcType,
			  mkArrowKind, liftedTypeKind, 
			  tcSplitSigmaTy, tcEqType )
import Type		( splitTyConApp_maybe )
import PprType		( pprThetaArrow, pprParendType )
import FieldLabel	( fieldLabelName, fieldLabelType )
import Generics		( validGenericMethodType, canDoGenerics )
import Class		( Class, className, classTyCon, DefMeth(..), classBigSig )
import TyCon		( TyCon, ArgVrcs, DataConDetails(..), 
			  tyConDataCons, mkForeignTyCon, isProductTyCon, isRecursiveTyCon,
			  tyConTheta, getSynTyConDefn, tyConDataCons, isSynTyCon, tyConName )
import DataCon		( DataCon, dataConWrapId, dataConName, dataConSig, dataConFieldLabels )
import Var		( TyVar, idType, idName )
import Name		( Name, getSrcLoc )
import Outputable
import Util		( zipLazy, isSingleton, notNull )
import ListSetOps	( equivClasses )
import CmdLineOpts	( DynFlag( Opt_GlasgowExts, Opt_Generics, Opt_UnboxStrictFields ) )
\end{code}


%************************************************************************
%*									*
\subsection{Type checking for type and class declarations}
%*									*
%************************************************************************

Dealing with a group
~~~~~~~~~~~~~~~~~~~~
Consider a mutually-recursive group, binding 
a type constructor T and a class C.

Step 1: 	getInitialKind
	Construct a KindEnv by binding T and C to a kind variable 

Step 2: 	kcTyClDecl
	In that environment, do a kind check

Step 3: Zonk the kinds

Step 4: 	buildTyConOrClass
	Construct an environment binding T to a TyCon and C to a Class.
	a) Their kinds comes from zonking the relevant kind variable
	b) Their arity (for synonyms) comes direct from the decl
	c) The funcional dependencies come from the decl
	d) The rest comes a knot-tied binding of T and C, returned from Step 4
	e) The variances of the tycons in the group is calculated from 
		the knot-tied stuff

Step 5: 	tcTyClDecl1
	In this environment, walk over the decls, constructing the TyCons and Classes.
	This uses in a strict way items (a)-(c) above, which is why they must
	be constructed in Step 4. Feed the results back to Step 4.
	For this step, pass the is-recursive flag as the wimp-out flag
	to tcTyClDecl1.
	

Step 6:		Extend environment
	We extend the type environment with bindings not only for the TyCons and Classes,
	but also for their "implicit Ids" like data constructors and class selectors

Step 7:		checkValidTyCl
	For a recursive group only, check all the decls again, just
	to check all the side conditions on validity.  We could not
	do this before because we were in a mutually recursive knot.


The knot-tying parameters: @rec_details_list@ is an alist mapping @Name@s to
@TyThing@s.  @rec_vrcs@ is a finite map from @Name@s to @ArgVrcs@s.

\begin{code}
tcTyAndClassDecls :: [RenamedTyClDecl]
   	           -> TcM TcGblEnv 	-- Input env extended by types and classes 
					-- and their implicit Ids,DataCons
tcTyAndClassDecls decls
  = do	{ 	-- First check for cyclic type synonysm or classes
		-- See notes with checkCycleErrs
	  checkCycleErrs decls

	; tyclss <- fixM (\ rec_tyclss ->
	  do	{ lcl_things <- mappM getInitialKind decls
			-- Extend the local env with kinds, and
			-- the global env with the knot-tied results
		; let { gbl_things = mkGlobalThings decls rec_tyclss }
		; tcExtendRecEnv gbl_things lcl_things $ do	

		-- The local type environment is populated with 
		--		{"T" -> ARecTyCon k, ...}
		-- and the global type envt with
		-- 		{"T" -> ATyCon T, ...}
		-- where k is T's (unzonked) kind
		--	 T is the loop-tied TyCon itself
		-- We must populate the environment with the loop-tied T's right
		-- away, because the kind checker may "fault in" some type 
		-- constructors that recursively mention T

		-- Kind-check the declarations, returning kind-annotated decls
		{ kc_decls <- mappM kcTyClDecl decls

		-- Calculate variances and rec-flag
		; let {	calc_vrcs = calcTyConArgVrcs rec_tyclss
		      ; calc_rec  = calcRecFlags     rec_tyclss }
		    
		; mappM (tcTyClDecl calc_vrcs calc_rec) kc_decls
	    }})
	-- Finished with knot-tying now
	-- Extend the environment with the finished things
	; tcExtendGlobalEnv tyclss $ do

	-- Perform the validity check
	{ traceTc (text "ready for validity check")
	; mappM_ checkValidTyCl decls
 	; traceTc (text "done")
   
	-- Add the implicit things;
	-- we want them in the environment because 
	-- they may be mentioned in interface files
	; let {	implicit_things = concatMap implicitTyThings tyclss }
	; traceTc ((text "Adding" <+> ppr tyclss) $$ (text "and" <+> ppr implicit_things))
  	; tcExtendGlobalEnv implicit_things getGblEnv
    }}

mkGlobalThings :: [RenamedTyClDecl] 	-- The decls
	       -> [TyThing]		-- Knot-tied, in 1-1 correspondence with the decls
	       -> [(Name,TyThing)]
-- Driven by the Decls, and treating the TyThings lazily
-- make a TypeEnv for the new things
mkGlobalThings decls things
  = map mk_thing (decls `zipLazy` things)
  where
    mk_thing (ClassDecl {tcdName = name}, ~(AClass cl)) = (name,         AClass cl)
    mk_thing (decl, 			  ~(ATyCon tc)) = (tcdName decl, ATyCon tc)
\end{code}


%************************************************************************
%*									*
		Kind checking
%*									*
%************************************************************************

We need to kind check all types in the mutually recursive group
before we know the kind of the type variables.  For example:

class C a where
   op :: D b => a -> b -> b

class D c where
   bop :: (Monad c) => ...

Here, the kind of the locally-polymorphic type variable "b"
depends on *all the uses of class D*.  For example, the use of
Monad c in bop's type signature means that D must have kind Type->Type.

\begin{code}
------------------------------------------------------------------------
getInitialKind :: TyClDecl Name -> TcM (Name, TcTyThing)

-- Note the lazy pattern match on the ATyCon etc
-- Exactly the same reason as the zipLay above

getInitialKind (TyData {tcdName = name})
 = newKindVar				`thenM` \ kind  ->
   returnM (name, ARecTyCon kind)

getInitialKind (TySynonym {tcdName = name})
 = newKindVar				`thenM` \ kind  ->
   returnM (name, ARecTyCon kind)

getInitialKind (ClassDecl {tcdName = name})
 = newKindVar				`thenM` \ kind  ->
   returnM (name, ARecClass kind)


------------------------------------------------------------------------
kcTyClDecl :: RenamedTyClDecl -> TcM RenamedTyClDecl

kcTyClDecl decl@(TySynonym {tcdSynRhs = rhs})
  = do 	{ res_kind <- newKindVar
	; kcTyClDeclBody decl res_kind		$ \ tvs' ->
	  do { rhs' <- kcCheckHsType rhs res_kind
	     ; return (decl {tcdTyVars = tvs', tcdSynRhs = rhs'}) } }

kcTyClDecl decl@(TyData {tcdND = new_or_data, tcdCtxt = ctxt, tcdCons = cons})
  = kcTyClDeclBody decl liftedTypeKind	$ \ tvs' ->
    do	{ ctxt' <- kcHsContext ctxt	
	; cons' <- mappM kc_con_decl cons
	; return (decl {tcdTyVars = tvs', tcdCtxt = ctxt', tcdCons = cons'}) }
  where
    kc_con_decl (ConDecl name ex_tvs ex_ctxt details loc)
      = kcHsTyVars ex_tvs		$ \ ex_tvs' ->
	do { ex_ctxt' <- kcHsContext ex_ctxt
	   ; details' <- kc_con_details details 
	   ; return (ConDecl name ex_tvs' ex_ctxt' details' loc)}

    kc_con_details (PrefixCon btys) 
	= do { btys' <- mappM kc_arg_ty btys ; return (PrefixCon btys') }
    kc_con_details (InfixCon bty1 bty2) 
	= do { bty1' <- kc_arg_ty bty1; bty2' <- kc_arg_ty bty2; return (InfixCon bty1' bty2') }
    kc_con_details (RecCon fields) 
	= do { fields' <- mappM kc_field fields; return (RecCon fields') }

    kc_field (fld, bty) = do { bty' <- kc_arg_ty bty ; return (fld, bty') }

    kc_arg_ty (BangType str ty) = do { ty' <- kc_arg_ty_body ty; return (BangType str ty') }
    kc_arg_ty_body = case new_or_data of
		   	 DataType -> kcHsSigType
			 NewType  -> kcHsLiftedSigType
	    -- Can't allow an unlifted type for newtypes, because we're effectively
	    -- going to remove the constructor while coercing it to a lifted type.

kcTyClDecl decl@(ClassDecl {tcdCtxt = ctxt,  tcdSigs = sigs})
  = kcTyClDeclBody decl liftedTypeKind	$ \ tvs' ->
    do	{ ctxt' <- kcHsContext ctxt	
	; sigs' <- mappM kc_sig sigs
	; return (decl {tcdTyVars = tvs', tcdCtxt = ctxt', tcdSigs = sigs'}) }
  where
    kc_sig (Sig nm op_ty loc) = do { op_ty' <- kcHsLiftedSigType op_ty
				   ; return (Sig nm op_ty' loc) }
    kc_sig other_sig	      = return other_sig

kcTyClDecl decl@(ForeignType {}) 
  = return decl

kcTyClDeclBody :: RenamedTyClDecl -> TcKind
	       -> ([HsTyVarBndr Name] -> TcM a)
	       -> TcM a
  -- Extend the env with bindings for the tyvars, taken from
  -- the kind of the tycon/class.  Give it to the thing inside, and 
  -- check the result kind matches
kcTyClDeclBody decl res_kind thing_inside
  = tcAddDeclCtxt decl		$
    kcHsTyVars (tyClDeclTyVars decl)	$ \ kinded_tvs ->
    do 	{ tc_ty_thing <- tcLookup (tcdName decl)
	; let { tc_kind = case tc_ty_thing of
			    ARecClass k -> k
			    ARecTyCon k -> k
	  }
	; unifyKind tc_kind (foldr (mkArrowKind . kindedTyVarKind) 
				   res_kind kinded_tvs)
	; thing_inside kinded_tvs }

kindedTyVarKind (KindedTyVar _ k) = k
\end{code}


%************************************************************************
%*									*
\subsection{Type checking}
%*									*
%************************************************************************

\begin{code}
tcTyClDecl :: (Name -> ArgVrcs) -> (Name -> RecFlag) 
	   -> RenamedTyClDecl -> TcM TyThing

tcTyClDecl calc_vrcs calc_isrec decl
  = tcAddDeclCtxt decl (tcTyClDecl1 calc_vrcs calc_isrec decl)

tcTyClDecl1 calc_vrcs calc_isrec 
	  (TySynonym {tcdName = tc_name, tcdTyVars = tvs, tcdSynRhs = rhs_ty})
  =   tcTyVarBndrs tvs		$ \ tvs' -> do 
    { rhs_ty' <- tcHsKindedType rhs_ty
    ; return (ATyCon (buildSynTyCon tc_name tvs' rhs_ty' arg_vrcs)) }
  where
    arg_vrcs = calc_vrcs tc_name

tcTyClDecl1 calc_vrcs calc_isrec 
	  (TyData {tcdND = new_or_data, tcdCtxt = ctxt, tcdTyVars = tvs,
		   tcdName = tc_name, tcdCons = cons})
  = tcTyVarBndrs tvs		$ \ tvs' -> do 
  { ctxt' 	 <- tcHsKindedContext ctxt
  ; want_generic <- doptM Opt_Generics
  ; tycon <- fixM (\ tycon -> do 
	{ cons' <- mappM (tcConDecl new_or_data tycon tvs' ctxt') cons
	; buildAlgTyCon new_or_data tc_name tvs' ctxt' 
  			(DataCons cons') arg_vrcs is_rec
			(want_generic && canDoGenerics cons')
	})
  ; return (ATyCon tycon)
  }
  where
    arg_vrcs = calc_vrcs tc_name
    is_rec   = calc_isrec tc_name

tcTyClDecl1 calc_vrcs calc_isrec 
	  (ClassDecl {tcdName = class_name, tcdTyVars = tvs, 
		      tcdCtxt = ctxt, tcdMeths = meths,
		      tcdFDs = fundeps, tcdSigs = sigs} )
  = tcTyVarBndrs tvs		$ \ tvs' -> do 
  { ctxt' <- tcHsKindedContext ctxt
  ; fds' <- mappM tc_fundep fundeps
  ; sig_stuff <- tcClassSigs class_name sigs meths
  ; clas <- fixM (\ clas ->
		let 	-- This little knot is just so we can get
			-- hold of the name of the class TyCon, which we
			-- need to look up its recursiveness and variance
		    tycon_name = tyConName (classTyCon clas)
		    tc_isrec = calc_isrec tycon_name
		    tc_vrcs  = calc_vrcs  tycon_name
		in
		buildClass class_name tvs' ctxt' fds' 
			   sig_stuff tc_isrec tc_vrcs)
  ; return (AClass clas) }
  where
    tc_fundep (tvs1, tvs2) = do { tvs1' <- mappM tcLookupTyVar tvs1 ;
				; tvs2' <- mappM tcLookupTyVar tvs2 ;
				; return (tvs1', tvs2') }


tcTyClDecl1 calc_vrcs calc_isrec 
	  (ForeignType {tcdName = tc_name, tcdExtName = tc_ext_name})
  = returnM (ATyCon (mkForeignTyCon tc_name tc_ext_name liftedTypeKind 0 []))

-----------------------------------
tcConDecl :: NewOrData -> TyCon -> [TyVar] -> ThetaType 
	  -> RenamedConDecl -> TcM DataCon

tcConDecl new_or_data tycon tyvars ctxt 
	   (ConDecl name ex_tvs ex_ctxt details src_loc)
  = addSrcLoc src_loc		$
    tcTyVarBndrs ex_tvs		$ \ ex_tvs' -> do 
    { ex_ctxt' <- tcHsKindedContext ex_ctxt
    ; unbox_strict <- doptM Opt_UnboxStrictFields
    ; let 
	tc_datacon field_lbls btys
	  = do { arg_tys <- mappM (tcHsKindedType . getBangType) btys
    	       ; buildDataCon name 
    		    (argStrictness unbox_strict tycon btys arg_tys)
    		    field_lbls
    		    tyvars ctxt ex_tvs' ex_ctxt'
    		    arg_tys tycon }
    ; case details of
	PrefixCon btys     -> tc_datacon [] btys
	InfixCon bty1 bty2 -> tc_datacon [] [bty1,bty2]
	RecCon fields      -> do { checkTc (null ex_tvs') (exRecConErr name)
				 ; let { (field_names, btys) = unzip fields }
				 ; tc_datacon field_names btys } }

argStrictness :: Bool		-- True <=> -funbox-strict_fields
	      -> TyCon -> [BangType Name] 
	      -> [TcType] -> [StrictnessMark]
argStrictness unbox_strict tycon btys arg_tys
 = zipWith (chooseBoxingStrategy unbox_strict tycon) 
	   arg_tys 
	   (map getBangStrictness btys ++ repeat HsNoBang)

-- We attempt to unbox/unpack a strict field when either:
--   (i)  The field is marked '!!', or
--   (ii) The field is marked '!', and the -funbox-strict-fields flag is on.

chooseBoxingStrategy :: Bool -> TyCon -> TcType -> HsBang -> StrictnessMark
chooseBoxingStrategy unbox_strict_fields tycon arg_ty bang
  = case bang of
	HsNoBang				    -> NotMarkedStrict
	HsStrict | unbox_strict_fields && can_unbox -> MarkedUnboxed
	HsUnbox  | can_unbox			    -> MarkedUnboxed
	other					    -> MarkedStrict
  where
    can_unbox = case splitTyConApp_maybe arg_ty of
		   Nothing 	       -> False
		   Just (arg_tycon, _) -> not (isRecursiveTyCon tycon) &&
					  isProductTyCon arg_tycon
\end{code}

%************************************************************************
%*									*
\subsection{Dependency analysis}
%*									*
%************************************************************************

Validity checking is done once the mutually-recursive knot has been
tied, so we can look at things freely.

\begin{code}
checkCycleErrs :: [TyClDecl Name] -> TcM ()
checkCycleErrs tyclss
  | null syn_cycles && null cls_cycles
  = return ()
  | otherwise
  = do	{ mappM_ recSynErr syn_cycles
	; mappM_ recClsErr cls_cycles
	; failM	}	-- Give up now, because later checkValidTyCl
			-- will loop if the synonym is recursive
  where
    (syn_cycles, cls_cycles) = calcCycleErrs tyclss

checkValidTyCl :: RenamedTyClDecl -> TcM ()
-- We do the validity check over declarations, rather than TyThings
-- only so that we can add a nice context with tcAddDeclCtxt
checkValidTyCl decl
  = tcAddDeclCtxt decl $
    do	{ thing <- tcLookupGlobal (tcdName decl)
	; traceTc (text "Validity of" <+> ppr thing)	
	; case thing of
	    ATyCon tc -> checkValidTyCon tc
	    AClass cl -> checkValidClass cl 
	; traceTc (text "Done validity of" <+> ppr thing)	
	}

-------------------------
checkValidTyCon :: TyCon -> TcM ()
checkValidTyCon tc
  | isSynTyCon tc 
  = checkValidType syn_ctxt syn_rhs
  | otherwise
  = 	-- Check the context on the data decl
    checkValidTheta (DataTyCtxt name) (tyConTheta tc)	`thenM_` 
	
	-- Check arg types of data constructors
    mappM_ checkValidDataCon data_cons			`thenM_`

	-- Check that fields with the same name share a type
    mappM_ check_fields groups

  where
    syn_ctxt	 = TySynCtxt name
    name         = tyConName tc
    (_, syn_rhs) = getSynTyConDefn tc
    data_cons    = tyConDataCons tc

    fields = [field | con <- data_cons, field <- dataConFieldLabels con]
    groups = equivClasses cmp_name fields
    cmp_name field1 field2 = fieldLabelName field1 `compare` fieldLabelName field2

    check_fields fields@(first_field_label : other_fields)
	-- These fields all have the same name, but are from
	-- different constructors in the data type
	= 	-- Check that all the fields in the group have the same type
		-- NB: this check assumes that all the constructors of a given
		-- data type use the same type variables
	  checkTc (all (tcEqType field_ty) other_tys) (fieldTypeMisMatch field_name)
	where
	    field_ty   = fieldLabelType first_field_label
	    field_name = fieldLabelName first_field_label
	    other_tys  = map fieldLabelType other_fields

-------------------------------
checkValidDataCon :: DataCon -> TcM ()
checkValidDataCon con
  = addErrCtxt (dataConCtxt con) (
      checkValidType ctxt (idType (dataConWrapId con))	`thenM_`
		-- This checks the argument types and
		-- ambiguity of the existential context (if any)
      checkFreeness ex_tvs ex_theta)
  where
    ctxt = ConArgCtxt (dataConName con) 
    (_, _, ex_tvs, ex_theta, _, _) = dataConSig con


-------------------------------
checkValidClass :: Class -> TcM ()
checkValidClass cls
  = do	{ 	-- CHECK ARITY 1 FOR HASKELL 1.4
	  gla_exts <- doptM Opt_GlasgowExts

    	-- Check that the class is unary, unless GlaExs
	; checkTc (notNull tyvars) (nullaryClassErr cls)
	; checkTc (gla_exts || unary) (classArityErr cls)

   	-- Check the super-classes
	; checkValidTheta (ClassSCCtxt (className cls)) theta

	-- Check the class operations
	; mappM_ check_op op_stuff

  	-- Check that if the class has generic methods, then the
	-- class has only one parameter.  We can't do generic
	-- multi-parameter type classes!
	; checkTc (unary || no_generics) (genericMultiParamErr cls)
	}
  where
    (tyvars, theta, _, op_stuff) = classBigSig cls
    unary 	= isSingleton tyvars
    no_generics = null [() | (_, GenDefMeth) <- op_stuff]

    check_op (sel_id, dm) 
	= addErrCtxt (classOpCtxt sel_id) (
	  checkValidTheta SigmaCtxt (tail theta) 	`thenM_`
		-- The 'tail' removes the initial (C a) from the
		-- class itself, leaving just the method type

	  checkValidType (FunSigCtxt op_name) tau	`thenM_`

		-- Check that for a generic method, the type of 
		-- the method is sufficiently simple
	  checkTc (dm /= GenDefMeth || validGenericMethodType op_ty)
		  (badGenericMethodType op_name op_ty)
	)
	where
	  op_name = idName sel_id
	  op_ty   = idType sel_id
	  (_,theta,tau) = tcSplitSigmaTy op_ty



---------------------------------------------------------------------
fieldTypeMisMatch field_name
  = sep [ptext SLIT("Different constructors give different types for field"), quotes (ppr field_name)]

dataConCtxt con = sep [ptext SLIT("When checking the data constructor:"),
		       nest 2 (ex_part <+> pprThetaArrow ex_theta <+> ppr con <+> arg_part)]
  where
    (_, _, ex_tvs, ex_theta, arg_tys, _) = dataConSig con
    ex_part | null ex_tvs = empty
	    | otherwise   = ptext SLIT("forall") <+> hsep (map ppr ex_tvs) <> dot
	-- The 'ex_theta' part could be non-empty, if the user (bogusly) wrote
	--	data T a = Eq a => T a a
	-- So we make sure to print it

    fields = dataConFieldLabels con
    arg_part | null fields = sep (map pprParendType arg_tys)
	     | otherwise   = braces (sep (punctuate comma 
			     [ ppr n <+> dcolon <+> ppr ty 
			     | (n,ty) <- fields `zip` arg_tys]))

classOpCtxt sel_id = sep [ptext SLIT("When checking the class method:"),
			  nest 2 (ppr sel_id <+> dcolon <+> ppr (idType sel_id))]

nullaryClassErr cls
  = ptext SLIT("No parameters for class")  <+> quotes (ppr cls)

classArityErr cls
  = vcat [ptext SLIT("Too many parameters for class") <+> quotes (ppr cls),
	  parens (ptext SLIT("Use -fglasgow-exts to allow multi-parameter classes"))]

genericMultiParamErr clas
  = ptext SLIT("The multi-parameter class") <+> quotes (ppr clas) <+> 
    ptext SLIT("cannot have generic methods")

badGenericMethodType op op_ty
  = hang (ptext SLIT("Generic method type is too complex"))
       4 (vcat [ppr op <+> dcolon <+> ppr op_ty,
		ptext SLIT("You can only use type variables, arrows, and tuples")])

recSynErr tcs
  = addSrcLoc (getSrcLoc (head tcs)) $
    addErr (sep [ptext SLIT("Cycle in type synonym declarations:"),
		 nest 2 (vcat (map ppr_thing tcs))])

recClsErr clss
  = addSrcLoc (getSrcLoc (head clss)) $
    addErr (sep [ptext SLIT("Cycle in class declarations (via superclasses):"),
		 nest 2 (vcat (map ppr_thing clss))])

ppr_thing :: Name -> SDoc
ppr_thing n = ppr n <+> parens (ppr (getSrcLoc n))


exRecConErr name
  = ptext SLIT("Can't combine named fields with locally-quantified type variables")
    $$
    (ptext SLIT("In the declaration of data constructor") <+> ppr name)
\end{code}
