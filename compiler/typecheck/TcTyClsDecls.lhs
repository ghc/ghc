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
			  ConDecl(..),   Sig(..), NewOrData(..), ResType(..),
			  tyClDeclTyVars, isSynDecl, isClassDecl, hsConArgs,
			  LTyClDecl, tcdName, hsTyVarName, LHsTyVarBndr
			)
import HsTypes          ( HsBang(..), getBangStrictness )
import BasicTypes	( RecFlag(..), StrictnessMark(..) )
import HscTypes		( implicitTyThings, ModDetails )
import BuildTyCl	( buildClass, buildAlgTyCon, buildSynTyCon, buildDataCon,
			  mkDataTyConRhs, mkNewTyConRhs )
import TcRnMonad
import TcEnv		( TyThing(..), 
			  tcLookupLocated, tcLookupLocatedGlobal, 
			  tcExtendGlobalEnv, tcExtendKindEnv, tcExtendKindEnvTvs,
			  tcExtendRecEnv, tcLookupTyVar, InstInfo )
import TcTyDecls	( calcRecFlags, calcClassCycles, calcSynCycles )
import TcClassDcl	( tcClassSigs, tcAddDeclCtxt )
import TcHsType		( kcHsTyVars, kcHsLiftedSigType, kcHsType, 
			  kcHsContext, tcTyVarBndrs, tcHsKindedType, tcHsKindedContext,
			  kcHsSigType, tcHsBangType, tcLHsConResTy, tcDataKindSig )
import TcMType		( newKindVar, checkValidTheta, checkValidType, 
			  -- checkFreeness, 
			  UserTypeCtxt(..), SourceTyCtxt(..) ) 
import TcType		( TcKind, TcType, Type, tyVarsOfType, mkPhiTy,
			  mkArrowKind, liftedTypeKind, mkTyVarTys, 
			  tcSplitSigmaTy, tcEqTypes, tcGetTyVar_maybe )
import Type		( PredType(..), splitTyConApp_maybe, mkTyVarTy
			  -- pprParendType, pprThetaArrow
			)
import Generics		( validGenericMethodType, canDoGenerics )
import Class		( Class, className, classTyCon, DefMeth(..), classBigSig, classTyVars )
import TyCon		( TyCon, AlgTyConRhs( AbstractTyCon ),
			  tyConDataCons, mkForeignTyCon, isProductTyCon, isRecursiveTyCon,
			  tyConStupidTheta, synTyConRhs, isSynTyCon, tyConName,
                          isNewTyCon )
import DataCon		( DataCon, dataConUserType, dataConName, 
			  dataConFieldLabels, dataConTyCon, dataConAllTyVars,
			  dataConFieldType, dataConResTys )
import Var		( TyVar, idType, idName )
import VarSet		( elemVarSet, mkVarSet )
import Name		( Name, getSrcLoc )
import Outputable
import Maybe		( isJust )
import Maybes		( expectJust )
import Unify		( tcMatchTys, tcMatchTyX )
import Util		( zipLazy, isSingleton, notNull, sortLe )
import List		( partition )
import SrcLoc		( Located(..), unLoc, getLoc, srcLocSpan )
import ListSetOps	( equivClasses, minusList )
import List		( delete )
import Digraph		( SCC(..) )
import DynFlags		( DynFlag( Opt_GlasgowExts, Opt_Generics, 
					Opt_UnboxStrictFields ) )
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

Identification of recursive TyCons
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The knot-tying parameters: @rec_details_list@ is an alist mapping @Name@s to
@TyThing@s.

Identifying a TyCon as recursive serves two purposes

1.  Avoid infinite types.  Non-recursive newtypes are treated as
"transparent", like type synonyms, after the type checker.  If we did
this for all newtypes, we'd get infinite types.  So we figure out for
each newtype whether it is "recursive", and add a coercion if so.  In
effect, we are trying to "cut the loops" by identifying a loop-breaker.

2.  Avoid infinite unboxing.  This is nothing to do with newtypes.
Suppose we have
        data T = MkT Int T
        f (MkT x t) = f t
Well, this function diverges, but we don't want the strictness analyser
to diverge.  But the strictness analyser will diverge because it looks
deeper and deeper into the structure of T.   (I believe there are
examples where the function does something sane, and the strictness
analyser still diverges, but I can't see one now.)

Now, concerning (1), the FC2 branch currently adds a coercion for ALL
newtypes.  I did this as an experiment, to try to expose cases in which
the coercions got in the way of optimisations.  If it turns out that we
can indeed always use a coercion, then we don't risk recursive types,
and don't need to figure out what the loop breakers are.

For newtype *families* though, we will always have a coercion, so they
are always loop breakers!  So you can easily adjust the current
algorithm by simply treating all newtype families as loop breakers (and
indeed type families).  I think.

\begin{code}
tcTyAndClassDecls :: ModDetails -> [LTyClDecl Name]
   	           -> TcM TcGblEnv 	-- Input env extended by types and classes 
					-- and their implicit Ids,DataCons
tcTyAndClassDecls boot_details decls
  = do	{ 	-- First check for cyclic type synonysm or classes
		-- See notes with checkCycleErrs
	  checkCycleErrs decls
	; mod <- getModule
	; traceTc (text "tcTyAndCl" <+> ppr mod)
	; (syn_tycons, alg_tyclss) <- fixM (\ ~(rec_syn_tycons, rec_alg_tyclss) ->
	  do	{ let {	-- Calculate variances and rec-flag
		      ; (syn_decls, alg_decls) = partition (isSynDecl . unLoc)
						   decls }
			-- Extend the global env with the knot-tied results
			-- for data types and classes
			-- 
			-- We must populate the environment with the loop-tied T's right
			-- away, because the kind checker may "fault in" some type 
			-- constructors that recursively mention T
		; let { gbl_things = mkGlobalThings alg_decls rec_alg_tyclss }
		; tcExtendRecEnv gbl_things $ do

			-- Kind-check the declarations
		{ (kc_syn_decls, kc_alg_decls) <- kcTyClDecls syn_decls alg_decls

		; let {	calc_rec  = calcRecFlags boot_details rec_alg_tyclss
		      ; tc_decl   = addLocM (tcTyClDecl calc_rec) }
			-- Type-check the type synonyms, and extend the envt
		; syn_tycons <- tcSynDecls kc_syn_decls
		; tcExtendGlobalEnv syn_tycons $ do

			-- Type-check the data types and classes
		{ alg_tyclss <- mappM tc_decl kc_alg_decls
		; return (syn_tycons, alg_tyclss)
	    }}})
	-- Finished with knot-tying now
	-- Extend the environment with the finished things
	; tcExtendGlobalEnv (syn_tycons ++ alg_tyclss) $ do

	-- Perform the validity check
	{ traceTc (text "ready for validity check")
	; mappM_ (addLocM checkValidTyCl) decls
 	; traceTc (text "done")
   
	-- Add the implicit things;
	-- we want them in the environment because 
	-- they may be mentioned in interface files
	; let {	implicit_things = concatMap implicitTyThings alg_tyclss }
	; traceTc ((text "Adding" <+> ppr alg_tyclss) $$ (text "and" <+> ppr implicit_things))
  	; tcExtendGlobalEnv implicit_things getGblEnv
    }}

mkGlobalThings :: [LTyClDecl Name] 	-- The decls
	       -> [TyThing]		-- Knot-tied, in 1-1 correspondence with the decls
	       -> [(Name,TyThing)]
-- Driven by the Decls, and treating the TyThings lazily
-- make a TypeEnv for the new things
mkGlobalThings decls things
  = map mk_thing (decls `zipLazy` things)
  where
    mk_thing (L _ (ClassDecl {tcdLName = L _ name}), ~(AClass cl))
	 = (name, AClass cl)
    mk_thing (L _ decl, ~(ATyCon tc))
         = (tcdName decl, ATyCon tc)
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

However type synonyms work differently.  They can have kinds which don't
just involve (->) and *:
	type R = Int#		-- Kind #
	type S a = Array# a	-- Kind * -> #
	type T a b = (# a,b #)	-- Kind * -> * -> (# a,b #)
So we must infer their kinds from their right-hand sides *first* and then
use them, whereas for the mutually recursive data types D we bring into
scope kind bindings D -> k, where k is a kind variable, and do inference.

\begin{code}
kcTyClDecls syn_decls alg_decls
  = do	{ 	-- First extend the kind env with each data 
		-- type and class, mapping them to a type variable
	  alg_kinds <- mappM getInitialKind alg_decls
	; tcExtendKindEnv alg_kinds $ do

		-- Now kind-check the type synonyms, in dependency order
		-- We do these differently to data type and classes,
		-- because a type synonym can be an unboxed type
		--	type Foo = Int#
		-- and a kind variable can't unify with UnboxedTypeKind
		-- So we infer their kinds in dependency order
	{ (kc_syn_decls, syn_kinds) <- kcSynDecls (calcSynCycles syn_decls)
	; tcExtendKindEnv syn_kinds $  do

		-- Now kind-check the data type and class declarations, 
		-- returning kind-annotated decls
	{ kc_alg_decls <- mappM (wrapLocM kcTyClDecl) alg_decls

	; return (kc_syn_decls, kc_alg_decls) }}}

------------------------------------------------------------------------
getInitialKind :: LTyClDecl Name -> TcM (Name, TcKind)
-- Only for data type and class declarations
-- Get as much info as possible from the data or class decl,
-- so as to maximise usefulness of error messages
getInitialKind (L _ decl)
  = do 	{ arg_kinds <- mapM (mk_arg_kind . unLoc) (tyClDeclTyVars decl)
	; res_kind  <- mk_res_kind decl
	; return (tcdName decl, mkArrowKinds arg_kinds res_kind) }
  where
    mk_arg_kind (UserTyVar _)        = newKindVar
    mk_arg_kind (KindedTyVar _ kind) = return kind

    mk_res_kind (TyData { tcdKindSig = Just kind }) = return kind
	-- On GADT-style declarations we allow a kind signature
	--	data T :: *->* where { ... }
    mk_res_kind other = return liftedTypeKind


----------------
kcSynDecls :: [SCC (LTyClDecl Name)] 
	   -> TcM ([LTyClDecl Name], 	-- Kind-annotated decls
		   [(Name,TcKind)])	-- Kind bindings
kcSynDecls []
  = return ([], [])
kcSynDecls (group : groups)
  = do	{ (decl,  nk)  <- kcSynDecl group
	; (decls, nks) <- tcExtendKindEnv [nk] (kcSynDecls groups)
	; return (decl:decls, nk:nks) }
			
----------------
kcSynDecl :: SCC (LTyClDecl Name) 
	   -> TcM (LTyClDecl Name, 	-- Kind-annotated decls
		   (Name,TcKind))	-- Kind bindings
kcSynDecl (AcyclicSCC ldecl@(L loc decl))
  = tcAddDeclCtxt decl	$
    kcHsTyVars (tcdTyVars decl) (\ k_tvs ->
    do { traceTc (text "kcd1" <+> ppr (unLoc (tcdLName decl)) <+> brackets (ppr (tcdTyVars decl)) 
			<+> brackets (ppr k_tvs))
       ; (k_rhs, rhs_kind) <- kcHsType (tcdSynRhs decl)
       ; traceTc (text "kcd2" <+> ppr (unLoc (tcdLName decl)))
       ; let tc_kind = foldr (mkArrowKind . kindedTyVarKind) rhs_kind k_tvs
       ; return (L loc (decl { tcdTyVars = k_tvs, tcdSynRhs = k_rhs }),
		 (unLoc (tcdLName decl), tc_kind)) })

kcSynDecl (CyclicSCC decls)
  = do { recSynErr decls; failM }	-- Fail here to avoid error cascade
					-- of out-of-scope tycons

kindedTyVarKind (L _ (KindedTyVar _ k)) = k

------------------------------------------------------------------------
kcTyClDecl :: TyClDecl Name -> TcM (TyClDecl Name)
	-- Not used for type synonyms (see kcSynDecl)

kcTyClDecl decl@(TyData {tcdND = new_or_data, tcdCtxt = ctxt, tcdCons = cons})
  = kcTyClDeclBody decl	$ \ tvs' ->
    do	{ ctxt' <- kcHsContext ctxt	
	; cons' <- mappM (wrapLocM kc_con_decl) cons
	; return (decl {tcdTyVars = tvs', tcdCtxt = ctxt', tcdCons = cons'}) }
  where
    kc_con_decl (ConDecl name expl ex_tvs ex_ctxt details res) = do
      kcHsTyVars ex_tvs $ \ex_tvs' -> do
        ex_ctxt' <- kcHsContext ex_ctxt
        details' <- kc_con_details details 
        res'     <- case res of
          ResTyH98 -> return ResTyH98
          ResTyGADT ty -> do { ty' <- kcHsSigType ty; return (ResTyGADT ty') }
        return (ConDecl name expl ex_tvs' ex_ctxt' details' res')

    kc_con_details (PrefixCon btys) 
	= do { btys' <- mappM kc_larg_ty btys ; return (PrefixCon btys') }
    kc_con_details (InfixCon bty1 bty2) 
	= do { bty1' <- kc_larg_ty bty1; bty2' <- kc_larg_ty bty2; return (InfixCon bty1' bty2') }
    kc_con_details (RecCon fields) 
	= do { fields' <- mappM kc_field fields; return (RecCon fields') }

    kc_field (fld, bty) = do { bty' <- kc_larg_ty bty ; return (fld, bty') }

    kc_larg_ty bty = case new_or_data of
			DataType -> kcHsSigType bty
			NewType  -> kcHsLiftedSigType bty
	-- Can't allow an unlifted type for newtypes, because we're effectively
	-- going to remove the constructor while coercing it to a lifted type.
	-- And newtypes can't be bang'd

-- !!!TODO -=chak
kcTyClDecl decl@(ClassDecl {tcdCtxt = ctxt,  tcdSigs = sigs})
  = kcTyClDeclBody decl	$ \ tvs' ->
    do	{ is_boot <- tcIsHsBoot
	; ctxt' <- kcHsContext ctxt	
	; sigs' <- mappM (wrapLocM kc_sig) sigs
	; return (decl {tcdTyVars = tvs', tcdCtxt = ctxt', tcdSigs = sigs'}) }
  where
    kc_sig (TypeSig nm op_ty) = do { op_ty' <- kcHsLiftedSigType op_ty
				   ; return (TypeSig nm op_ty') }
    kc_sig other_sig	      = return other_sig

kcTyClDecl decl@(ForeignType {})
  = return decl

kcTyClDeclBody :: TyClDecl Name
	       -> ([LHsTyVarBndr Name] -> TcM a)
	       -> TcM a
-- getInitialKind has made a suitably-shaped kind for the type or class
-- Unpack it, and attribute those kinds to the type variables
-- Extend the env with bindings for the tyvars, taken from
-- the kind of the tycon/class.  Give it to the thing inside, and 
 -- check the result kind matches
kcTyClDeclBody decl thing_inside
  = tcAddDeclCtxt decl		$
    do 	{ tc_ty_thing <- tcLookupLocated (tcdLName decl)
	; let tc_kind	 = case tc_ty_thing of { AThing k -> k }
	      (kinds, _) = splitKindFunTys tc_kind
	      hs_tvs 	 = tcdTyVars decl
	      kinded_tvs = ASSERT( length kinds >= length hs_tvs )
			   [ L loc (KindedTyVar (hsTyVarName tv) k)
			   | (L loc tv, k) <- zip hs_tvs kinds]
	; tcExtendKindEnvTvs kinded_tvs (thing_inside kinded_tvs) }
\end{code}


%************************************************************************
%*									*
\subsection{Type checking}
%*									*
%************************************************************************

\begin{code}
tcSynDecls :: [LTyClDecl Name] -> TcM [TyThing]
tcSynDecls [] = return []
tcSynDecls (decl : decls) 
  = do { syn_tc <- addLocM tcSynDecl decl
       ; syn_tcs <- tcExtendGlobalEnv [syn_tc] (tcSynDecls decls)
       ; return (syn_tc : syn_tcs) }

tcSynDecl
  (TySynonym {tcdLName = L _ tc_name, tcdTyVars = tvs, tcdSynRhs = rhs_ty})
  = tcTyVarBndrs tvs		$ \ tvs' -> do 
    { traceTc (text "tcd1" <+> ppr tc_name) 
    ; rhs_ty' <- tcHsKindedType rhs_ty
    ; return (ATyCon (buildSynTyCon tc_name tvs' rhs_ty')) }

--------------------
tcTyClDecl :: (Name -> RecFlag) -> TyClDecl Name -> TcM TyThing

tcTyClDecl calc_isrec decl
  = tcAddDeclCtxt decl (tcTyClDecl1 calc_isrec decl)

tcTyClDecl1 calc_isrec 
  (TyData {tcdND = new_or_data, tcdCtxt = ctxt, tcdTyVars = tvs,
	   tcdLName = L _ tc_name, tcdKindSig = mb_ksig, tcdCons = cons})
  = tcTyVarBndrs tvs	$ \ tvs' -> do 
  { extra_tvs <- tcDataKindSig mb_ksig
  ; let final_tvs = tvs' ++ extra_tvs
  ; stupid_theta <- tcHsKindedContext ctxt
  ; want_generic <- doptM Opt_Generics
  ; unbox_strict <- doptM Opt_UnboxStrictFields
  ; gla_exts     <- doptM Opt_GlasgowExts
  ; is_boot	 <- tcIsHsBoot	-- Are we compiling an hs-boot file?

	-- Check that we don't use GADT syntax in H98 world
  ; checkTc (gla_exts || h98_syntax) (badGadtDecl tc_name)

	-- Check that the stupid theta is empty for a GADT-style declaration
  ; checkTc (null stupid_theta || h98_syntax) (badStupidTheta tc_name)

	-- Check that there's at least one condecl,
	-- or else we're reading an hs-boot file, or -fglasgow-exts
  ; checkTc (not (null cons) || gla_exts || is_boot)
	    (emptyConDeclsErr tc_name)
    
	-- Check that a newtype has exactly one constructor
  ; checkTc (new_or_data == DataType || isSingleton cons) 
	    (newtypeConError tc_name (length cons))

  ; tycon <- fixM (\ tycon -> do 
	{ data_cons <- mappM (addLocM (tcConDecl unbox_strict new_or_data 
						 tycon final_tvs)) 
			     cons
	; tc_rhs <-
	    if null cons && is_boot 	-- In a hs-boot file, empty cons means
	    then return AbstractTyCon	-- "don't know"; hence Abstract
	    else case new_or_data of
		   DataType -> return (mkDataTyConRhs data_cons)
		   NewType  -> 
                       ASSERT( isSingleton data_cons )
                       mkNewTyConRhs tc_name tycon (head data_cons)
	; buildAlgTyCon tc_name final_tvs stupid_theta tc_rhs is_rec
			(want_generic && canDoGenerics data_cons) h98_syntax
	})
  ; return (ATyCon tycon)
  }
  where
    is_rec   = calc_isrec tc_name
    h98_syntax = case cons of 	-- All constructors have same shape
			L _ (ConDecl { con_res = ResTyGADT _ }) : _ -> False
			other -> True

tcTyClDecl1 calc_isrec 
  (ClassDecl {tcdLName = L _ class_name, tcdTyVars = tvs, 
	      tcdCtxt = ctxt, tcdMeths = meths,
	      tcdFDs = fundeps, tcdSigs = sigs, tcdATs = ats} )
  = tcTyVarBndrs tvs		$ \ tvs' -> do 
  { ctxt' <- tcHsKindedContext ctxt
  ; fds' <- mappM (addLocM tc_fundep) fundeps
  -- !!!TODO: process `ats`; what do we want to store in the `Class'? -=chak
  ; sig_stuff <- tcClassSigs class_name sigs meths
  ; clas <- fixM (\ clas ->
		let 	-- This little knot is just so we can get
			-- hold of the name of the class TyCon, which we
			-- need to look up its recursiveness and variance
		    tycon_name = tyConName (classTyCon clas)
		    tc_isrec = calc_isrec tycon_name
		in
		buildClass class_name tvs' ctxt' fds' 
			   sig_stuff tc_isrec)
  ; return (AClass clas) }
  where
    tc_fundep (tvs1, tvs2) = do { tvs1' <- mappM tcLookupTyVar tvs1 ;
				; tvs2' <- mappM tcLookupTyVar tvs2 ;
				; return (tvs1', tvs2') }


tcTyClDecl1 calc_isrec 
  (ForeignType {tcdLName = L _ tc_name, tcdExtName = tc_ext_name})
  = returnM (ATyCon (mkForeignTyCon tc_name tc_ext_name liftedTypeKind 0))

-----------------------------------
tcConDecl :: Bool 		-- True <=> -funbox-strict_fields
	  -> NewOrData -> TyCon -> [TyVar]
	  -> ConDecl Name -> TcM DataCon

tcConDecl unbox_strict NewType tycon tc_tvs	-- Newtypes
	  (ConDecl name _ ex_tvs ex_ctxt details ResTyH98)
  = do	{ let tc_datacon field_lbls arg_ty
		= do { arg_ty' <- tcHsKindedType arg_ty	-- No bang on newtype
		     ; buildDataCon (unLoc name) False {- Prefix -} 
				    [NotMarkedStrict]
		    		    (map unLoc field_lbls)
			   	    tc_tvs []  -- No existentials
				    [] []      -- No equalities, predicates
				    [arg_ty']
				    tycon }

		-- Check that a newtype has no existential stuff
	; checkTc (null ex_tvs && null (unLoc ex_ctxt)) (newtypeExError name)

	; case details of
	    PrefixCon [arg_ty] -> tc_datacon [] arg_ty
	    RecCon [(field_lbl, arg_ty)] -> tc_datacon [field_lbl] arg_ty
	    other -> failWithTc (newtypeFieldErr name (length (hsConArgs details)))
			-- Check that the constructor has exactly one field
	}

tcConDecl unbox_strict DataType tycon tc_tvs	-- Data types
	  (ConDecl name _ tvs ctxt details res_ty)
  = tcTyVarBndrs tvs		$ \ tvs' -> do 
    { ctxt' <- tcHsKindedContext ctxt
    ; (univ_tvs, ex_tvs, eq_preds, data_tc) <- tcResultType tycon tc_tvs tvs' res_ty
    ; let 
	tc_datacon is_infix field_lbls btys
	  = do { let bangs = map getBangStrictness btys
	       ; arg_tys <- mappM tcHsBangType btys
    	       ; buildDataCon (unLoc name) is_infix
    		    (argStrictness unbox_strict tycon bangs arg_tys)
    		    (map unLoc field_lbls)
    		    univ_tvs ex_tvs eq_preds ctxt' arg_tys
		    data_tc }
		-- NB:	we put data_tc, the type constructor gotten from the constructor 
		--	type signature into the data constructor; that way 
		--	checkValidDataCon can complain if it's wrong.

    ; case details of
	PrefixCon btys     -> tc_datacon False [] btys
	InfixCon bty1 bty2 -> tc_datacon True  [] [bty1,bty2]
	RecCon fields      -> tc_datacon False field_names btys
			   where
			      (field_names, btys) = unzip fields
                              
    }

tcResultType :: TyCon
	     -> [TyVar] 	-- data T a b c = ...
	     -> [TyVar] 	-- where MkT :: forall a b c. ...
	     -> ResType Name
	     -> TcM ([TyVar],	 	-- Universal
		     [TyVar],		-- Existential
		     [(TyVar,Type)],	-- Equality predicates
		     TyCon)		-- TyCon given in the ResTy
	-- We don't check that the TyCon given in the ResTy is
	-- the same as the parent tycon, becuase we are in the middle
	-- of a recursive knot; so it's postponed until checkValidDataCon

tcResultType decl_tycon tc_tvs dc_tvs ResTyH98
  = return (tc_tvs, dc_tvs, [], decl_tycon)
	-- In H98 syntax the dc_tvs are the existential ones
	--	data T a b c = forall d e. MkT ...
	-- The {a,b,c} are tc_tvs, and {d,e} are dc_tvs

tcResultType _ tc_tvs dc_tvs (ResTyGADT res_ty)
	-- E.g.  data T a b c where
	--	   MkT :: forall x y z. T (x,y) z z
	-- Then we generate
	--	([a,z,c], [x,y], [a:=:(x,y), c:=:z], T)

  = do	{ (dc_tycon, res_tys) <- tcLHsConResTy res_ty
		-- NB: tc_tvs and dc_tvs are distinct
	; let univ_tvs = choose_univs [] tc_tvs res_tys
		-- Each univ_tv is either a dc_tv or a tc_tv
	      ex_tvs = dc_tvs `minusList` univ_tvs
	      eq_spec = [ (tv, ty) | (tv,ty) <- univ_tvs `zip` res_tys, 
				      tv `elem` tc_tvs]
	; return (univ_tvs, ex_tvs, eq_spec, dc_tycon) }
  where
  	-- choose_univs uses the res_ty itself if it's a type variable
	-- and hasn't already been used; otherwise it uses one of the tc_tvs
    choose_univs used tc_tvs []
	= ASSERT( null tc_tvs ) []
    choose_univs used (tc_tv:tc_tvs) (res_ty:res_tys) 
	| Just tv <- tcGetTyVar_maybe res_ty, not (tv `elem` used)
	= tv    : choose_univs (tv:used) tc_tvs res_tys
	| otherwise
	= tc_tv : choose_univs used tc_tvs res_tys

-------------------
argStrictness :: Bool		-- True <=> -funbox-strict_fields
	      -> TyCon -> [HsBang]
	      -> [TcType] -> [StrictnessMark]
argStrictness unbox_strict tycon bangs arg_tys
 = ASSERT( length bangs == length arg_tys )
   zipWith (chooseBoxingStrategy unbox_strict tycon) arg_tys bangs

-- We attempt to unbox/unpack a strict field when either:
--   (i)  The field is marked '!!', or
--   (ii) The field is marked '!', and the -funbox-strict-fields flag is on.
--
-- We have turned off unboxing of newtypes because coercions make unboxing 
-- and reboxing more complicated
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
		   Just (arg_tycon, _) -> not (isNewTyCon arg_tycon) && not (isRecursiveTyCon tycon) &&
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
checkCycleErrs :: [LTyClDecl Name] -> TcM ()
checkCycleErrs tyclss
  | null cls_cycles
  = return ()
  | otherwise
  = do	{ mappM_ recClsErr cls_cycles
	; failM	}	-- Give up now, because later checkValidTyCl
			-- will loop if the synonym is recursive
  where
    cls_cycles = calcClassCycles tyclss

checkValidTyCl :: TyClDecl Name -> TcM ()
-- We do the validity check over declarations, rather than TyThings
-- only so that we can add a nice context with tcAddDeclCtxt
checkValidTyCl decl
  = tcAddDeclCtxt decl $
    do	{ thing <- tcLookupLocatedGlobal (tcdLName decl)
	; traceTc (text "Validity of" <+> ppr thing)	
	; case thing of
	    ATyCon tc -> checkValidTyCon tc
	    AClass cl -> checkValidClass cl 
	; traceTc (text "Done validity of" <+> ppr thing)	
	}

-------------------------
-- For data types declared with record syntax, we require
-- that each constructor that has a field 'f' 
--	(a) has the same result type
--	(b) has the same type for 'f'
-- module alpha conversion of the quantified type variables
-- of the constructor.

checkValidTyCon :: TyCon -> TcM ()
checkValidTyCon tc 
  | isSynTyCon tc 
  = checkValidType syn_ctxt syn_rhs
  | otherwise
  = 	-- Check the context on the data decl
    checkValidTheta (DataTyCtxt name) (tyConStupidTheta tc)	`thenM_` 
	
	-- Check arg types of data constructors
    mappM_ (checkValidDataCon tc) data_cons			`thenM_`

	-- Check that fields with the same name share a type
    mappM_ check_fields groups

  where
    syn_ctxt  = TySynCtxt name
    name      = tyConName tc
    syn_rhs   = synTyConRhs tc
    data_cons = tyConDataCons tc

    groups = equivClasses cmp_fld (concatMap get_fields data_cons)
    cmp_fld (f1,_) (f2,_) = f1 `compare` f2
    get_fields con = dataConFieldLabels con `zip` repeat con
	-- dataConFieldLabels may return the empty list, which is fine

    -- See Note [GADT record selectors] in MkId.lhs
    -- We must check (a) that the named field has the same 
    --                   type in each constructor
    --               (b) that those constructors have the same result type
    --
    -- However, the constructors may have differently named type variable
    -- and (worse) we don't know how the correspond to each other.  E.g.
    --     C1 :: forall a b. { f :: a, g :: b } -> T a b
    --     C2 :: forall d c. { f :: c, g :: c } -> T c d
    -- 
    -- So what we do is to ust Unify.tcMatchTys to compare the first candidate's
    -- result type against other candidates' types BOTH WAYS ROUND.
    -- If they magically agrees, take the substitution and
    -- apply them to the latter ones, and see if they match perfectly.
    check_fields fields@((label, con1) : other_fields)
	-- These fields all have the same name, but are from
	-- different constructors in the data type
	= recoverM (return ()) $ mapM_ checkOne other_fields
                -- Check that all the fields in the group have the same type
		-- NB: this check assumes that all the constructors of a given
		-- data type use the same type variables
        where
        tvs1 = mkVarSet (dataConAllTyVars con1)
        res1 = dataConResTys con1
        fty1 = dataConFieldType con1 label

        checkOne (_, con2)    -- Do it bothways to ensure they are structurally identical
	    = do { checkFieldCompat label con1 con2 tvs1 res1 res2 fty1 fty2
		 ; checkFieldCompat label con2 con1 tvs2 res2 res1 fty2 fty1 }
	    where        
                tvs2 = mkVarSet (dataConAllTyVars con2)
		res2 = dataConResTys con2 
                fty2 = dataConFieldType con2 label

checkFieldCompat fld con1 con2 tvs1 res1 res2 fty1 fty2
  = do	{ checkTc (isJust mb_subst1) (resultTypeMisMatch fld con1 con2)
	; checkTc (isJust mb_subst2) (fieldTypeMisMatch fld con1 con2) }
  where
    mb_subst1 = tcMatchTys tvs1 res1 res2
    mb_subst2 = tcMatchTyX tvs1 (expectJust "checkFieldCompat" mb_subst1) fty1 fty2

-------------------------------
checkValidDataCon :: TyCon -> DataCon -> TcM ()
checkValidDataCon tc con
  = setSrcSpan (srcLocSpan (getSrcLoc con))	$
    addErrCtxt (dataConCtxt con)		$ 
    do	{ checkTc (dataConTyCon con == tc) (badDataConTyCon con)
	; checkValidType ctxt (dataConUserType con) }
  where
    ctxt = ConArgCtxt (dataConName con) 

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
	; mappM_ (check_op gla_exts) op_stuff

  	-- Check that if the class has generic methods, then the
	-- class has only one parameter.  We can't do generic
	-- multi-parameter type classes!
	; checkTc (unary || no_generics) (genericMultiParamErr cls)

    	-- Check that the class has no associated types, unless GlaExs
	; checkTc (gla_exts || no_ats) (badATDecl cls)
	}
  where
    (tyvars, theta, _, op_stuff) = classBigSig cls
    unary 	= isSingleton tyvars
    no_generics = null [() | (_, GenDefMeth) <- op_stuff]
    no_ats      = True -- !!!TODO: determine whether the class has ATs -=chak

    check_op gla_exts (sel_id, dm) 
      = addErrCtxt (classOpCtxt sel_id tau) $ do
	{ checkValidTheta SigmaCtxt (tail theta)
		-- The 'tail' removes the initial (C a) from the
		-- class itself, leaving just the method type

	; checkValidType (FunSigCtxt op_name) tau

		-- Check that the type mentions at least one of
		-- the class type variables
	; checkTc (any (`elemVarSet` tyVarsOfType tau) tyvars)
	          (noClassTyVarErr cls sel_id)

		-- Check that for a generic method, the type of 
		-- the method is sufficiently simple
	; checkTc (dm /= GenDefMeth || validGenericMethodType tau)
		  (badGenericMethodType op_name op_ty)
	}
	where
	  op_name = idName sel_id
	  op_ty   = idType sel_id
	  (_,theta1,tau1) = tcSplitSigmaTy op_ty
	  (_,theta2,tau2)  = tcSplitSigmaTy tau1
	  (theta,tau) | gla_exts  = (theta1 ++ theta2, tau2)
		      | otherwise = (theta1, 	       mkPhiTy (tail theta1) tau1)
		-- Ugh!  The function might have a type like
		-- 	op :: forall a. C a => forall b. (Eq b, Eq a) => tau2
		-- With -fglasgow-exts, we want to allow this, even though the inner 
		-- forall has an (Eq a) constraint.  Whereas in general, each constraint 
		-- in the context of a for-all must mention at least one quantified
		-- type variable.  What a mess!


---------------------------------------------------------------------
resultTypeMisMatch field_name con1 con2
  = vcat [sep [ptext SLIT("Constructors") <+> ppr con1 <+> ptext SLIT("and") <+> ppr con2, 
		ptext SLIT("have a common field") <+> quotes (ppr field_name) <> comma],
	  nest 2 $ ptext SLIT("but have different result types")]
fieldTypeMisMatch field_name con1 con2
  = sep [ptext SLIT("Constructors") <+> ppr con1 <+> ptext SLIT("and") <+> ppr con2, 
	 ptext SLIT("give different types for field"), quotes (ppr field_name)]

dataConCtxt con = ptext SLIT("In the definition of data constructor") <+> quotes (ppr con)

classOpCtxt sel_id tau = sep [ptext SLIT("When checking the class method:"),
			      nest 2 (ppr sel_id <+> dcolon <+> ppr tau)]

nullaryClassErr cls
  = ptext SLIT("No parameters for class")  <+> quotes (ppr cls)

classArityErr cls
  = vcat [ptext SLIT("Too many parameters for class") <+> quotes (ppr cls),
	  parens (ptext SLIT("Use -fglasgow-exts to allow multi-parameter classes"))]

noClassTyVarErr clas op
  = sep [ptext SLIT("The class method") <+> quotes (ppr op),
	 ptext SLIT("mentions none of the type variables of the class") <+> 
		ppr clas <+> hsep (map ppr (classTyVars clas))]

genericMultiParamErr clas
  = ptext SLIT("The multi-parameter class") <+> quotes (ppr clas) <+> 
    ptext SLIT("cannot have generic methods")

badGenericMethodType op op_ty
  = hang (ptext SLIT("Generic method type is too complex"))
       4 (vcat [ppr op <+> dcolon <+> ppr op_ty,
		ptext SLIT("You can only use type variables, arrows, lists, and tuples")])

recSynErr syn_decls
  = setSrcSpan (getLoc (head sorted_decls)) $
    addErr (sep [ptext SLIT("Cycle in type synonym declarations:"),
		 nest 2 (vcat (map ppr_decl sorted_decls))])
  where
    sorted_decls = sortLocated syn_decls
    ppr_decl (L loc decl) = ppr loc <> colon <+> ppr decl

recClsErr cls_decls
  = setSrcSpan (getLoc (head sorted_decls)) $
    addErr (sep [ptext SLIT("Cycle in class declarations (via superclasses):"),
		 nest 2 (vcat (map ppr_decl sorted_decls))])
  where
    sorted_decls = sortLocated cls_decls
    ppr_decl (L loc decl) = ppr loc <> colon <+> ppr (decl { tcdSigs = [] })

sortLocated :: [Located a] -> [Located a]
sortLocated things = sortLe le things
  where
    le (L l1 _) (L l2 _) = l1 <= l2

badDataConTyCon data_con
  = hang (ptext SLIT("Data constructor") <+> quotes (ppr data_con) <+>
		ptext SLIT("returns type") <+> quotes (ppr (dataConTyCon data_con)))
       2 (ptext SLIT("instead of its parent type"))

badGadtDecl tc_name
  = vcat [ ptext SLIT("Illegal generalised algebraic data declaration for") <+> quotes (ppr tc_name)
	 , nest 2 (parens $ ptext SLIT("Use -fglasgow-exts to allow GADTs")) ]

badStupidTheta tc_name
  = ptext SLIT("A data type declared in GADT style cannot have a context:") <+> quotes (ppr tc_name)

newtypeConError tycon n
  = sep [ptext SLIT("A newtype must have exactly one constructor,"),
	 nest 2 $ ptext SLIT("but") <+> quotes (ppr tycon) <+> ptext SLIT("has") <+> speakN n ]

newtypeExError con
  = sep [ptext SLIT("A newtype constructor cannot have an existential context,"),
	 nest 2 $ ptext SLIT("but") <+> quotes (ppr con) <+> ptext SLIT("does")]

newtypeFieldErr con_name n_flds
  = sep [ptext SLIT("The constructor of a newtype must have exactly one field"), 
	 nest 2 $ ptext SLIT("but") <+> quotes (ppr con_name) <+> ptext SLIT("has") <+> speakN n_flds]

badATDecl cl_name
  = vcat [ ptext SLIT("Illegal associated type declaration in") <+> quotes (ppr cl_name)
	 , nest 2 (parens $ ptext SLIT("Use -fglasgow-exts to allow ATs")) ]

emptyConDeclsErr tycon
  = sep [quotes (ppr tycon) <+> ptext SLIT("has no constructors"),
	 nest 2 $ ptext SLIT("(-fglasgow-exts permits this)")]
\end{code}
