%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Typechecking class declarations

\begin{code}
module TcClassDcl ( tcClassSigs, tcClassDecl2, 
		    findMethodBind, instantiateMethod, tcInstanceMethodBody,
		    mkGenericDefMethBind, getGenericInstances, 
		    tcAddDeclCtxt, badMethodErr, badATErr, omittedATWarn
		  ) where

#include "HsVersions.h"

import HsSyn
import RnHsSyn
import RnExpr
import Inst
import InstEnv
import TcPat( addInlinePrags )
import TcEnv
import TcBinds
import TcUnify
import TcHsType
import TcMType
import TcType
import TcRnMonad
import BuildTyCl( TcMethInfo )
import Generics
import Class
import TyCon
import MkId
import Id
import Name
import Var
import VarSet
import NameEnv
import NameSet
import Outputable
import PrelNames
import DynFlags
import ErrUtils
import Util
import ListSetOps
import SrcLoc
import Maybes
import BasicTypes
import Bag
import FastString

import Control.Monad
import Data.List
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

tcClassSigs clas sigs def_methods
  = do { dm_env <- mapM (addLocM (checkDefaultBind clas op_names)) 
                        (bagToList def_methods)
       ; mapM (tcClassSig (mkNameEnv dm_env)) op_sigs }
  where
    op_sigs  = [sig | sig@(L _ (TypeSig _ _))       <- sigs]
    op_names = [n   |     (L _ (TypeSig (L _ n) _)) <- op_sigs]

checkDefaultBind :: Name -> [Name] -> HsBindLR Name Name -> TcM (Name, DefMethSpec)
  -- Check default bindings
  -- 	a) must be for a class op for this class
  --	b) must be all generic or all non-generic
checkDefaultBind clas ops (FunBind {fun_id = L _ op, fun_matches = MatchGroup matches _ })
  = do {  	-- Check that the op is from this class
 	 checkTc (op `elem` ops) (badMethodErr clas op)

   	-- Check that all the defns ar generic, or none are
       ; case (none_generic, all_generic) of
           (True, _) -> return (op, VanillaDM)
           (_, True) -> return (op, GenericDM)
           _         -> failWith (mixedGenericErr op)
    }
  where
    n_generic    = count (isJust . maybeGenericMatch) matches
    none_generic = n_generic == 0
    all_generic  = matches `lengthIs` n_generic

checkDefaultBind _ _ b = pprPanic "checkDefaultBind" (ppr b)


tcClassSig :: NameEnv DefMethSpec	-- Info about default methods; 
	   -> LSig Name
	   -> TcM TcMethInfo

tcClassSig dm_env (L loc (TypeSig (L _ op_name) op_hs_ty))
  = setSrcSpan loc $ do
    { op_ty <- tcHsKindedType op_hs_ty	-- Class tyvars already in scope
    ; let dm = lookupNameEnv dm_env op_name `orElse` NoDM
    ; return (op_name, dm, op_ty) }
tcClassSig _ s = pprPanic "tcClassSig" (ppr s)
\end{code}


%************************************************************************
%*									*
		Class Declarations
%*									*
%************************************************************************

\begin{code}
tcClassDecl2 :: LTyClDecl Name		-- The class declaration
	     -> TcM (LHsBinds Id)

tcClassDecl2 (L loc (ClassDecl {tcdLName = class_name, tcdSigs = sigs, 
				tcdMeths = default_binds}))
  = recoverM (return emptyLHsBinds)	$
    setSrcSpan loc		   	$
    do  { clas <- tcLookupLocatedClass class_name

	-- We make a separate binding for each default method.
	-- At one time I used a single AbsBinds for all of them, thus
	-- AbsBind [d] [dm1, dm2, dm3] { dm1 = ...; dm2 = ...; dm3 = ... }
	-- But that desugars into
	--	ds = \d -> (..., ..., ...)
	--	dm1 = \d -> case ds d of (a,b,c) -> a
	-- And since ds is big, it doesn't get inlined, so we don't get good
	-- default methods.  Better to make separate AbsBinds for each
	; let
	      (tyvars, _, _, op_items) = classBigSig clas
	      rigid_info  = ClsSkol clas
	      prag_fn	  = mkPragFun sigs default_binds
	      sig_fn	  = mkSigFun sigs
	      clas_tyvars = tcSkolSigTyVars rigid_info tyvars
	      pred  	  = mkClassPred clas (mkTyVarTys clas_tyvars)
	; this_dict <- newEvVar pred

	; let tc_dm = tcDefMeth clas clas_tyvars
				this_dict default_binds
	      			sig_fn prag_fn

	; dm_binds <- tcExtendTyVarEnv clas_tyvars $
                      mapM tc_dm op_items

	; return (listToBag (catMaybes dm_binds)) }

tcClassDecl2 d = pprPanic "tcClassDecl2" (ppr d)
    
tcDefMeth :: Class -> [TyVar] -> EvVar -> LHsBinds Name
          -> SigFun -> PragFun -> ClassOpItem
          -> TcM (Maybe (LHsBind Id))
-- Generate code for polymorphic default methods only (hence DefMeth)
-- (Generic default methods have turned into instance decls by now.)
-- This is incompatible with Hugs, which expects a polymorphic 
-- default method for every class op, regardless of whether or not 
-- the programmer supplied an explicit default decl for the class.  
-- (If necessary we can fix that, but we don't have a convenient Id to hand.)
tcDefMeth clas tyvars this_dict binds_in sig_fn prag_fn (sel_id, dm_info)
  = case dm_info of
      NoDefMeth       -> return Nothing
      GenDefMeth      -> return Nothing
      DefMeth dm_name -> do
    	{ let sel_name = idName sel_id
	; local_dm_name <- newLocalName sel_name
 	  -- Base the local_dm_name on the selector name, because
 	  -- type errors from tcInstanceMethodBody come from here

		-- See Note [Silly default-method bind]
		-- (possibly out of date)

	; let meth_bind = findMethodBind sel_name binds_in
			  `orElse` pprPanic "tcDefMeth" (ppr sel_id)
		-- dm_info = DefMeth dm_name only if there is a binding in binds_in

	      dm_sig_fn  _  = sig_fn sel_name
	      dm_id         = mkDefaultMethodId sel_id dm_name
	      local_dm_type = instantiateMethod clas sel_id (mkTyVarTys tyvars)
	      local_dm_id   = mkLocalId local_dm_name local_dm_type
              prags         = prag_fn sel_name

        ; dm_id_w_inline <- addInlinePrags dm_id prags
        ; spec_prags     <- tcSpecPrags dm_id prags

        ; warnTc (not (null spec_prags))
                 (ptext (sLit "Ignoring SPECIALISE pragmas on default method") 
                  <+> quotes (ppr sel_name))

        ; liftM Just $
          tcInstanceMethodBody (ClsSkol clas)
                               tyvars 
                               [this_dict]
                               Nothing
                               dm_id_w_inline local_dm_id
                               dm_sig_fn IsDefaultMethod meth_bind }

---------------
tcInstanceMethodBody :: SkolemInfo -> [TcTyVar] -> [EvVar]
		     -> Maybe EvBind
                     -> Id -> Id
          	     -> SigFun -> TcSpecPrags -> LHsBind Name 
          	     -> TcM (LHsBind Id)
tcInstanceMethodBody skol_info tyvars dfun_ev_vars
		     this_dict meth_id local_meth_id
		     meth_sig_fn specs 
                     (L loc bind)
  = do	{       -- Typecheck the binding, first extending the envt
		-- so that when tcInstSig looks up the local_meth_id to find
		-- its signature, we'll find it in the environment
	  let full_given = case this_dict of
                             Nothing -> dfun_ev_vars
			     Just (EvBind dict _) -> dict : dfun_ev_vars
              lm_bind = L loc (bind { fun_id = L loc (idName local_meth_id) })
			     -- Substitue the local_meth_name for the binder
			     -- NB: the binding is always a FunBind

	; (ev_binds, (tc_bind, _)) 
               <- checkConstraints skol_info emptyVarSet tyvars full_given $
		  tcExtendIdEnv [local_meth_id] $
	          tcPolyBinds TopLevel meth_sig_fn no_prag_fn 
		  	     NonRecursive NonRecursive
		  	     [lm_bind]

        -- Add the binding for this_dict, if we have one
        ; ev_binds' <- case this_dict of
                         Nothing                -> return ev_binds
                         Just (EvBind self rhs) -> extendTcEvBinds ev_binds self rhs

	; let full_bind = AbsBinds { abs_tvs = tyvars, abs_ev_vars = dfun_ev_vars
                                   , abs_exports = [(tyvars, meth_id, local_meth_id, specs)]
				   , abs_ev_binds = ev_binds'
                                   , abs_binds = tc_bind }

        ; return (L loc full_bind) } 
  where
    no_prag_fn  _ = []		-- No pragmas for local_meth_id; 
    		    		-- they are all for meth_id
\end{code}

\begin{code}
instantiateMethod :: Class -> Id -> [TcType] -> TcType
-- Take a class operation, say  
--	op :: forall ab. C a => forall c. Ix c => (b,c) -> a
-- Instantiate it at [ty1,ty2]
-- Return the "local method type": 
--	forall c. Ix x => (ty2,c) -> ty1
instantiateMethod clas sel_id inst_tys
  = ASSERT( ok_first_pred ) local_meth_ty
  where
    (sel_tyvars,sel_rho) = tcSplitForAllTys (idType sel_id)
    rho_ty = ASSERT( length sel_tyvars == length inst_tys )
    	     substTyWith sel_tyvars inst_tys sel_rho

    (first_pred, local_meth_ty) = tcSplitPredFunTy_maybe rho_ty
    		`orElse` pprPanic "tcInstanceMethod" (ppr sel_id)

    ok_first_pred = case getClassPredTys_maybe first_pred of
		      Just (clas1, _tys) -> clas == clas1
                      Nothing -> False
	      -- The first predicate should be of form (C a b)
	      -- where C is the class in question


---------------------------
findMethodBind	:: Name  	        -- Selector name
          	-> LHsBinds Name 	-- A group of bindings
		-> Maybe (LHsBind Name)	-- The binding
findMethodBind sel_name binds
  = foldlBag mplus Nothing (mapBag f binds)
  where 
    f bind@(L _ (FunBind { fun_id = L _ op_name }))
             | op_name == sel_name
    	     = Just bind
    f _other = Nothing
\end{code}

Note [Polymorphic methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    class Foo a where
	op :: forall b. Ord b => a -> b -> b -> b
    instance Foo c => Foo [c] where
        op = e

When typechecking the binding 'op = e', we'll have a meth_id for op
whose type is
      op :: forall c. Foo c => forall b. Ord b => [c] -> b -> b -> b

So tcPolyBinds must be capable of dealing with nested polytypes; 
and so it is. See TcBinds.tcMonoBinds (with type-sig case).

Note [Silly default-method bind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we pass the default method binding to the type checker, it must
look like    op2 = e
not  	     $dmop2 = e
otherwise the "$dm" stuff comes out error messages.  But we want the
"$dm" to come out in the interface file.  So we typecheck the former,
and wrap it in a let, thus
	  $dmop2 = let op2 = e in op2
This makes the error messages right.


%************************************************************************
%*									*
	Extracting generic instance declaration from class declarations
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
mkGenericDefMethBind :: Class -> [Type] -> Id -> TcM (LHsBind Name)
mkGenericDefMethBind clas inst_tys sel_id
  = 	-- A generic default method
    	-- If the method is defined generically, we can only do the job if the
	-- instance declaration is for a single-parameter type class with
	-- a type constructor applied to type arguments in the instance decl
	-- 	(checkTc, so False provokes the error)
    do	{ checkTc (isJust maybe_tycon)
	 	  (badGenericInstance sel_id (notSimple inst_tys))
	; checkTc (tyConHasGenerics tycon)
	   	  (badGenericInstance sel_id (notGeneric tycon))

	; dflags <- getDOpts
	; liftIO (dumpIfSet_dyn dflags Opt_D_dump_deriv "Filling in method body"
		   (vcat [ppr clas <+> ppr inst_tys,
			  nest 2 (ppr sel_id <+> equals <+> ppr rhs)]))

		-- Rename it before returning it
	; (rn_rhs, _) <- rnLExpr rhs
        ; return (noLoc $ mkFunBind (noLoc (idName sel_id))
                                    [mkSimpleMatch [] rn_rhs]) }
  where
    rhs = mkGenericRhs sel_id clas_tyvar tycon

	  -- The tycon is only used in the generic case, and in that
	  -- case we require that the instance decl is for a single-parameter
	  -- type class with type variable arguments:
	  --	instance (...) => C (T a b)
    clas_tyvar  = ASSERT (not (null (classTyVars clas))) head (classTyVars clas)
    Just tycon	= maybe_tycon
    maybe_tycon = case inst_tys of 
			[ty] -> case tcSplitTyConApp_maybe ty of
				  Just (tycon, arg_tys) | all tcIsTyVarTy arg_tys -> Just tycon
				  _    						  -> Nothing
			_ -> Nothing


---------------------------
getGenericInstances :: [LTyClDecl Name] -> TcM [InstInfo Name] 
getGenericInstances class_decls
  = do	{ gen_inst_infos <- mapM (addLocM get_generics) class_decls
	; let { gen_inst_info = concat gen_inst_infos }

	-- Return right away if there is no generic stuff
	; if null gen_inst_info then return []
	  else do 

	-- Otherwise print it out
	{ dflags <- getDOpts
	; liftIO (dumpIfSet_dyn dflags Opt_D_dump_deriv "Generic instances"
	         (vcat (map pprInstInfoDetails gen_inst_info)))	
	; return gen_inst_info }}

get_generics :: TyClDecl Name -> TcM [InstInfo Name]
get_generics decl@(ClassDecl {tcdLName = class_name, tcdMeths = def_methods})
  | null generic_binds
  = return [] -- The comon case: no generic default methods

  | otherwise	-- A source class decl with generic default methods
  = recoverM (return [])                                $
    tcAddDeclCtxt decl                                  $ do
    clas <- tcLookupLocatedClass class_name

	-- Group by type, and
	-- make an InstInfo out of each group
    let
	groups = groupWith listToBag generic_binds

    inst_infos <- mapM (mkGenericInstance clas) groups

	-- Check that there is only one InstInfo for each type constructor
  	-- The main way this can fail is if you write
	--	f {| a+b |} ... = ...
	--	f {| x+y |} ... = ...
	-- Then at this point we'll have an InstInfo for each
	--
	-- The class should be unary, which is why simpleInstInfoTyCon should be ok
    let
	tc_inst_infos :: [(TyCon, InstInfo Name)]
	tc_inst_infos = [(simpleInstInfoTyCon i, i) | i <- inst_infos]

	bad_groups = [group | group <- equivClassesByUniq get_uniq tc_inst_infos,
			      group `lengthExceeds` 1]
	get_uniq (tc,_) = getUnique tc

    mapM_ (addErrTc . dupGenericInsts) bad_groups

	-- Check that there is an InstInfo for each generic type constructor
    let
	missing = genericTyConNames `minusList` [tyConName tc | (tc,_) <- tc_inst_infos]

    checkTc (null missing) (missingGenericInstances missing)

    return inst_infos
  where
    generic_binds :: [(HsType Name, LHsBind Name)]
    generic_binds = getGenericBinds def_methods
get_generics decl = pprPanic "get_generics" (ppr decl)


---------------------------------
getGenericBinds :: LHsBinds Name -> [(HsType Name, LHsBind Name)]
  -- Takes a group of method bindings, finds the generic ones, and returns
  -- them in finite map indexed by the type parameter in the definition.
getGenericBinds binds = concat (map getGenericBind (bagToList binds))

getGenericBind :: LHsBindLR Name Name -> [(HsType Name, LHsBindLR Name Name)]
getGenericBind (L loc bind@(FunBind { fun_matches = MatchGroup matches ty }))
  = groupWith wrap (mapCatMaybes maybeGenericMatch matches)
  where
    wrap ms = L loc (bind { fun_matches = MatchGroup ms ty })
getGenericBind _
  = []

groupWith :: ([a] -> b) -> [(HsType Name, a)] -> [(HsType Name, b)]
groupWith _  [] 	 = []
groupWith op ((t,v):prs) = (t, op (v:vs)) : groupWith op rest
    where
      vs              = map snd this
      (this,rest)     = partition same_t prs
      same_t (t', _v) = t `eqPatType` t'

eqPatLType :: LHsType Name -> LHsType Name -> Bool
eqPatLType t1 t2 = unLoc t1 `eqPatType` unLoc t2

eqPatType :: HsType Name -> HsType Name -> Bool
-- A very simple equality function, only for 
-- type patterns in generic function definitions.
eqPatType (HsTyVar v1)       (HsTyVar v2)    	= v1==v2
eqPatType (HsAppTy s1 t1)    (HsAppTy s2 t2) 	= s1 `eqPatLType` s2 && t1 `eqPatLType` t2
eqPatType (HsOpTy s1 op1 t1) (HsOpTy s2 op2 t2) = s1 `eqPatLType` s2 && t1 `eqPatLType` t2 && unLoc op1 == unLoc op2
eqPatType (HsNumTy n1)	     (HsNumTy n2)	= n1 == n2
eqPatType (HsParTy t1)	     t2			= unLoc t1 `eqPatType` t2
eqPatType t1		     (HsParTy t2)	= t1 `eqPatType` unLoc t2
eqPatType _ _ = False

---------------------------------
mkGenericInstance :: Class
		  -> (HsType Name, LHsBinds Name)
		  -> TcM (InstInfo Name)

mkGenericInstance clas (hs_ty, binds) = do
  -- Make a generic instance declaration
  -- For example:	instance (C a, C b) => C (a+b) where { binds }

	-- Extract the universally quantified type variables
	-- and wrap them as forall'd tyvars, so that kind inference
	-- works in the standard way
    let
	sig_tvs = userHsTyVarBndrs $ map noLoc $ nameSetToList $
                  extractHsTyVars (noLoc hs_ty)
	hs_forall_ty = noLoc $ mkExplicitHsForAllTy sig_tvs (noLoc []) (noLoc hs_ty)

	-- Type-check the instance type, and check its form
    forall_inst_ty <- tcHsSigType GenPatCtxt hs_forall_ty
    let
	(tyvars, inst_ty) = tcSplitForAllTys forall_inst_ty

    checkTc (validGenericInstanceType inst_ty)
            (badGenericInstanceType binds)

	-- Make the dictionary function.
    span <- getSrcSpanM
    overlap_flag <- getOverlapFlag
    dfun_name <- newDFunName clas [inst_ty] span
    let
	inst_theta = [mkClassPred clas [mkTyVarTy tv] | tv <- tyvars]
	dfun_id    = mkDictFunId dfun_name tyvars inst_theta clas [inst_ty]
	ispec	   = mkLocalInstance dfun_id overlap_flag

    return (InstInfo { iSpec = ispec, iBinds = VanillaInst binds [] False })
\end{code}


%************************************************************************
%*									*
		Error messages
%*									*
%************************************************************************

\begin{code}
tcAddDeclCtxt :: TyClDecl Name -> TcM a -> TcM a
tcAddDeclCtxt decl thing_inside
  = addErrCtxt ctxt thing_inside
  where
     thing | isClassDecl decl  = "class"
	   | isTypeDecl decl   = "type synonym" ++ maybeInst
	   | isDataDecl decl   = if tcdND decl == NewType 
				 then "newtype" ++ maybeInst
				 else "data type" ++ maybeInst
	   | isFamilyDecl decl = "family"
	   | otherwise         = panic "tcAddDeclCtxt/thing"

     maybeInst | isFamInstDecl decl = " instance"
	       | otherwise          = ""

     ctxt = hsep [ptext (sLit "In the"), text thing, 
		  ptext (sLit "declaration for"), quotes (ppr (tcdName decl))]

badMethodErr :: Outputable a => a -> Name -> SDoc
badMethodErr clas op
  = hsep [ptext (sLit "Class"), quotes (ppr clas), 
	  ptext (sLit "does not have a method"), quotes (ppr op)]

badATErr :: Class -> Name -> SDoc
badATErr clas at
  = hsep [ptext (sLit "Class"), quotes (ppr clas), 
	  ptext (sLit "does not have an associated type"), quotes (ppr at)]

omittedATWarn :: Name -> SDoc
omittedATWarn at
  = ptext (sLit "No explicit AT declaration for") <+> quotes (ppr at)

badGenericInstance :: Var -> SDoc -> SDoc
badGenericInstance sel_id because
  = sep [ptext (sLit "Can't derive generic code for") <+> quotes (ppr sel_id),
	 because]

notSimple :: [Type] -> SDoc
notSimple inst_tys
  = vcat [ptext (sLit "because the instance type(s)"), 
	  nest 2 (ppr inst_tys),
	  ptext (sLit "is not a simple type of form (T a1 ... an)")]

notGeneric :: TyCon -> SDoc
notGeneric tycon
  = vcat [ptext (sLit "because the instance type constructor") <+> quotes (ppr tycon) <+> 
	  ptext (sLit "was not compiled with -XGenerics")]

badGenericInstanceType :: LHsBinds Name -> SDoc
badGenericInstanceType binds
  = vcat [ptext (sLit "Illegal type pattern in the generic bindings"),
	  nest 2 (ppr binds)]

missingGenericInstances :: [Name] -> SDoc
missingGenericInstances missing
  = ptext (sLit "Missing type patterns for") <+> pprQuotedList missing
	  
dupGenericInsts :: [(TyCon, InstInfo a)] -> SDoc
dupGenericInsts tc_inst_infos
  = vcat [ptext (sLit "More than one type pattern for a single generic type constructor:"),
	  nest 2 (vcat (map ppr_inst_ty tc_inst_infos)),
	  ptext (sLit "All the type patterns for a generic type constructor must be identical")
    ]
  where 
    ppr_inst_ty (_,inst) = ppr (simpleInstInfoTy inst)

mixedGenericErr :: Name -> SDoc
mixedGenericErr op
  = ptext (sLit "Can't mix generic and non-generic equations for class method") <+> quotes (ppr op)
\end{code}
