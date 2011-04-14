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
import Inst
import InstEnv
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
	    -> TcM [TcMethInfo]    -- One for each method

tcClassSigs clas sigs def_methods
  = do { -- Check that all def_methods are in the class
       ; op_info <- mapM (addLocM tc_sig) [sig | sig@(L _ (TypeSig _ _)) <- sigs]
       ; let op_names = [ n | (n,_,_) <- op_info ]

       ; sequence_ [ failWithTc (badMethodErr clas n)
                   | n <- dm_bind_names, not (n `elem` op_names) ]
		   -- Value binding for non class-method (ie no TypeSig)

       ; sequence_ [ failWithTc (badGenericMethod clas n)
                   | n <- genop_names, not (n `elem` dm_bind_names) ]
		   -- Generic signature without value binding

       ; return op_info }
  where
    dm_bind_names :: [Name]	-- These ones have a value binding in the class decl
    dm_bind_names = [op | L _ (FunBind {fun_id = L _ op}) <- bagToList def_methods]

    genop_names :: [Name]   -- These ones have a generic signature
    genop_names = [n | L _ (GenericSig (L _ n) _) <- sigs]

    tc_sig (TypeSig (L _ op_name) op_hs_ty)
      = do { op_ty <- tcHsKindedType op_hs_ty	-- Class tyvars already in scope
           ; let dm | op_name `elem` genop_names   = GenericDM
                    | op_name `elem` dm_bind_names = VanillaDM
                    | otherwise                    = NoDM
           ; return (op_name, dm, op_ty) }
    tc_sig sig = pprPanic "tc_cls_sig" (ppr sig)
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
              prag_fn     = mkPragFun sigs default_binds
	      sig_fn	  = mkSigFun sigs
              clas_tyvars = tcSuperSkolTyVars tyvars
	      pred  	  = mkClassPred clas (mkTyVarTys clas_tyvars)
	; this_dict <- newEvVar pred

	; traceTc "TIM2" (ppr sigs)
	; let tc_dm = tcDefMeth clas clas_tyvars
				this_dict default_binds sigs
	      			sig_fn prag_fn

	; dm_binds <- tcExtendTyVarEnv clas_tyvars $
                      mapM tc_dm op_items

	; return (unionManyBags dm_binds) }

tcClassDecl2 d = pprPanic "tcClassDecl2" (ppr d)
    
tcDefMeth :: Class -> [TyVar] -> EvVar -> LHsBinds Name -> [LSig Name]
          -> SigFun -> PragFun -> ClassOpItem
          -> TcM (LHsBinds TcId)
-- Generate code for polymorphic default methods only (hence DefMeth)
-- (Generic default methods have turned into instance decls by now.)
-- This is incompatible with Hugs, which expects a polymorphic 
-- default method for every class op, regardless of whether or not 
-- the programmer supplied an explicit default decl for the class.  
-- (If necessary we can fix that, but we don't have a convenient Id to hand.)
tcDefMeth _ tyvars _ binds_in sigs sig_fn prag_fn (sel_id, dm_info)
  | NoDefMeth <- dm_info = return emptyBag
  | otherwise
  = do	{ (dm_id, tvs, sig_loc) <- tc_dm_id dm_info 
	; let L loc meth_bind = findMethodBind sel_name binds_in
		                `orElse` pprPanic "tcDefMeth" (ppr sel_id)
              dm_bind = L loc (meth_bind { fun_id = L loc (idName dm_id) })
                             -- Substitute the meth_name for the binder
			     -- NB: the binding is always a FunBind

	      dm_sig_fn  _  = Just (clas_tv_names ++ tvs, sig_loc)
              dm_prag_fn _  = prag_fn sel_name

	; (binds,_) <- tcExtendIdEnv [dm_id] $
	               tcPolyBinds TopLevel dm_sig_fn dm_prag_fn 
		  	     NonRecursive NonRecursive
		  	     [dm_bind]
        ; return binds }
  where
    sel_name      = idName sel_id
    clas_tv_names = map getName tyvars

    -- Find the 'generic op :: ty' signature among the sigs
    -- If dm_info is GenDefMeth, the corresponding signature
    -- should jolly well exist!  Hence the panic
    genop_lhs_ty = case [lty | L _ (GenericSig (L _ n) lty) <- sigs
                             , n == sel_name ] of
                      [lty] -> lty
                      _     -> pprPanic "tcDefMeth" (ppr sel_name $$ ppr sigs)

    tc_dm_id :: DefMeth -> TcM (Id, [Name], SrcSpan)
    -- Make a default-method Id of the appropriate type
    -- That may entail getting the generic-default signature
    -- from the type signatures.
    -- Also return the in-scope tyvars for the default method, and their binding site
    tc_dm_id NoDefMeth         = panic "tc_dm_id"
    tc_dm_id (DefMeth dm_name) 
      | Just (tvs, loc) <- sig_fn sel_name
      = return (mkDefaultMethodId sel_id dm_name, tvs, loc)
      | otherwise
      = pprPanic "No sig for" (ppr sel_name)
    tc_dm_id (GenDefMeth dm_name)
      = setSrcSpan loc $
        do { tau <- tcHsKindedType genop_lhs_ty
	   ; checkValidType (FunSigCtxt sel_name) tau	
           ; return ( mkExportedLocalId dm_name (mkForAllTys tyvars tau)
                    , hsExplicitTvs genop_lhs_ty, loc ) }
      where
        loc = getLoc genop_lhs_ty

---------------
tcInstanceMethodBody :: SkolemInfo -> [TcTyVar] -> [EvVar]
                     -> Id -> Id
          	     -> SigFun -> TcSpecPrags -> LHsBind Name 
          	     -> TcM (LHsBind Id)
tcInstanceMethodBody skol_info tyvars dfun_ev_vars
                     meth_id local_meth_id
		     meth_sig_fn specs 
                     (L loc bind)
  = do	{       -- Typecheck the binding, first extending the envt
		-- so that when tcInstSig looks up the local_meth_id to find
		-- its signature, we'll find it in the environment
          let lm_bind = L loc (bind { fun_id = L loc (idName local_meth_id) })
                             -- Substitute the local_meth_name for the binder
			     -- NB: the binding is always a FunBind
        ; traceTc "TIM" (ppr local_meth_id $$ ppr (meth_sig_fn (idName local_meth_id))) 
	; (ev_binds, (tc_bind, _)) 
               <- checkConstraints skol_info tyvars dfun_ev_vars $
		  tcExtendIdEnv [local_meth_id] $
	          tcPolyBinds TopLevel meth_sig_fn no_prag_fn 
		  	     NonRecursive NonRecursive
		  	     [lm_bind]

        ; let full_bind = AbsBinds { abs_tvs = tyvars, abs_ev_vars = dfun_ev_vars
                                   , abs_exports = [(tyvars, meth_id, local_meth_id, specs)]
                                   , abs_ev_binds = ev_binds
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
mkGenericDefMethBind :: Class -> [Type] -> Id -> Name -> TcM (LHsBind Name)
mkGenericDefMethBind clas inst_tys sel_id dm_name
  = 	-- A generic default method
    	-- If the method is defined generically, we only have to call the
        -- dm_name.
    do	{ dflags <- getDOpts
	; liftIO (dumpIfSet_dyn dflags Opt_D_dump_deriv "Filling in method body"
		   (vcat [ppr clas <+> ppr inst_tys,
			  nest 2 (ppr sel_id <+> equals <+> ppr rhs)]))

        ; return (noLoc $ mkFunBind (noLoc (idName sel_id))
                                    [mkSimpleMatch [] rhs]) }
  where
    rhs = nlHsVar dm_name

---------------------------
getGenericInstances :: [LTyClDecl Name] -> TcM [InstInfo Name] 
getGenericInstances class_decls
  = do	{ gen_inst_infos <- mapM (addLocM get_generics) class_decls
	; let { gen_inst_info = concat gen_inst_infos }

	-- Return right away if there is no generic stuff
	; if null gen_inst_info then return []
	  else do 

	-- Otherwise print it out
        { dumpDerivingInfo $ hang (ptext (sLit "Generic instances"))
                                2 (vcat (map pprInstInfoDetails gen_inst_info))
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
        ispec      = mkLocalInstance dfun_id overlap_flag

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

badGenericMethod :: Outputable a => a -> Name -> SDoc
badGenericMethod clas op
  = hsep [ptext (sLit "Class"), quotes (ppr clas), 
	  ptext (sLit "has a generic-default signature without a binding"), quotes (ppr op)]

badATErr :: Class -> Name -> SDoc
badATErr clas at
  = hsep [ptext (sLit "Class"), quotes (ppr clas), 
	  ptext (sLit "does not have an associated type"), quotes (ppr at)]

omittedATWarn :: Name -> SDoc
omittedATWarn at
  = ptext (sLit "No explicit AT declaration for") <+> quotes (ppr at)

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
\end{code}
