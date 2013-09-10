%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Typechecking class declarations

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcClassDcl ( tcClassSigs, tcClassDecl2, 
		    findMethodBind, instantiateMethod, tcInstanceMethodBody,
                    HsSigFun, mkHsSigFun, lookupHsSig, emptyHsSigs,
		    tcMkDeclCtxt, tcAddDeclCtxt, badMethodErr
		  ) where

#include "HsVersions.h"

import HsSyn
import TcEnv
import TcPat( addInlinePrags )
import TcEvidence( idHsWrapper )
import TcBinds
import TcUnify
import TcHsType
import TcMType
import Type     ( getClassPredTys_maybe )
import TcType
import TcRnMonad
import BuildTyCl( TcMethInfo )
import Class
import Id
import Name
import NameEnv
import NameSet
import Var
import Outputable
import SrcLoc
import Maybes
import BasicTypes
import Bag
import FastString
import Util

import Control.Monad
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
tcClassSigs :: Name	             -- Name of the class
	    -> [LSig Name]
	    -> LHsBinds Name
	    -> TcM ([TcMethInfo],    -- Exactly one for each method
                    NameEnv Type)    -- Types of the generic-default methods
tcClassSigs clas sigs def_methods
  = do { traceTc "tcClassSigs 1" (ppr clas)

       ; gen_dm_prs <- concat <$> mapM (addLocM tc_gen_sig) gen_sigs
       ; let gen_dm_env = mkNameEnv gen_dm_prs

       ; op_info <- concat <$> mapM (addLocM (tc_sig gen_dm_env)) vanilla_sigs

       ; let op_names = mkNameSet [ n | (n,_,_) <- op_info ]
       ; sequence_ [ failWithTc (badMethodErr clas n)
                   | n <- dm_bind_names, not (n `elemNameSet` op_names) ]
		   -- Value binding for non class-method (ie no TypeSig)

       ; sequence_ [ failWithTc (badGenericMethod clas n)
                   | (n,_) <- gen_dm_prs, not (n `elem` dm_bind_names) ]
		   -- Generic signature without value binding

       ; traceTc "tcClassSigs 2" (ppr clas)
       ; return (op_info, gen_dm_env) }
  where
    vanilla_sigs = [L loc (nm,ty) | L loc (TypeSig    nm ty) <- sigs]
    gen_sigs     = [L loc (nm,ty) | L loc (GenericSig nm ty) <- sigs]
    dm_bind_names :: [Name]	-- These ones have a value binding in the class decl
    dm_bind_names = [op | L _ (FunBind {fun_id = L _ op}) <- bagToList def_methods]

    tc_sig genop_env (op_names, op_hs_ty)
      = do { traceTc "ClsSig 1" (ppr op_names)
           ; op_ty <- tcClassSigType op_hs_ty	-- Class tyvars already in scope
           ; traceTc "ClsSig 2" (ppr op_names)
           ; return [ (op_name, f op_name, op_ty) | L _ op_name <- op_names ] }
           where
             f nm | nm `elemNameEnv` genop_env = GenericDM
                  | nm `elem` dm_bind_names    = VanillaDM
                  | otherwise                  = NoDM

    tc_gen_sig (op_names, gen_hs_ty)
      = do { gen_op_ty <- tcClassSigType gen_hs_ty
           ; return [ (op_name, gen_op_ty) | L _ op_name <- op_names ] }
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
	      sig_fn	  = mkHsSigFun sigs
              clas_tyvars = snd (tcSuperSkolTyVars tyvars)
	      pred  	  = mkClassPred clas (mkTyVarTys clas_tyvars)
	; this_dict <- newEvVar pred

	; traceTc "TIM2" (ppr sigs)
	; let tc_dm = tcDefMeth clas clas_tyvars
				this_dict default_binds 
	      			sig_fn prag_fn

	; dm_binds <- tcExtendTyVarEnv clas_tyvars $
                      mapM tc_dm op_items

	; return (unionManyBags dm_binds) }

tcClassDecl2 d = pprPanic "tcClassDecl2" (ppr d)
    
tcDefMeth :: Class -> [TyVar] -> EvVar -> LHsBinds Name
          -> HsSigFun -> PragFun -> ClassOpItem
          -> TcM (LHsBinds TcId)
-- Generate code for polymorphic default methods only (hence DefMeth)
-- (Generic default methods have turned into instance decls by now.)
-- This is incompatible with Hugs, which expects a polymorphic 
-- default method for every class op, regardless of whether or not 
-- the programmer supplied an explicit default decl for the class.  
-- (If necessary we can fix that, but we don't have a convenient Id to hand.)
tcDefMeth clas tyvars this_dict binds_in hs_sig_fn prag_fn (sel_id, dm_info)
  = case dm_info of
      NoDefMeth          -> do { mapM_ (addLocM (badDmPrag sel_id)) prags
                               ; return emptyBag }
      DefMeth dm_name    -> tc_dm dm_name 
      GenDefMeth dm_name -> tc_dm dm_name 
  where
    sel_name           = idName sel_id
    prags              = prag_fn sel_name
    (dm_bind,bndr_loc) = findMethodBind sel_name binds_in
	                 `orElse` pprPanic "tcDefMeth" (ppr sel_id)

    -- Eg.   class C a where
    --          op :: forall b. Eq b => a -> [b] -> a
    --		gen_op :: a -> a
    -- 		generic gen_op :: D a => a -> a
    -- The "local_dm_ty" is precisely the type in the above
    -- type signatures, ie with no "forall a. C a =>" prefix

    tc_dm dm_name 
      = do { dm_id <- tcLookupId dm_name
	   ; local_dm_name <- setSrcSpan bndr_loc (newLocalName sel_name)
 	     -- Base the local_dm_name on the selector name, because
 	     -- type errors from tcInstanceMethodBody come from here

           ; dm_id_w_inline <- addInlinePrags dm_id prags
           ; spec_prags     <- tcSpecPrags dm_id prags

           ; let local_dm_ty = instantiateMethod clas dm_id (mkTyVarTys tyvars)
                 hs_ty       = lookupHsSig hs_sig_fn sel_name 
                               `orElse` pprPanic "tc_dm" (ppr sel_name)

           ; local_dm_sig <- instTcTySig hs_ty local_dm_ty local_dm_name
           ; warnTc (not (null spec_prags))
                    (ptext (sLit "Ignoring SPECIALISE pragmas on default method") 
                     <+> quotes (ppr sel_name))

           ; tc_bind <- tcInstanceMethodBody (ClsSkol clas) tyvars [this_dict]
                                             dm_id_w_inline local_dm_sig
                                             IsDefaultMethod dm_bind

           ; return (unitBag tc_bind) }

---------------
tcInstanceMethodBody :: SkolemInfo -> [TcTyVar] -> [EvVar]
                     -> Id -> TcSigInfo
          	     -> TcSpecPrags -> LHsBind Name 
          	     -> TcM (LHsBind Id)
tcInstanceMethodBody skol_info tyvars dfun_ev_vars
                     meth_id local_meth_sig
		     specs (L loc bind)
  = do	{ let local_meth_id = sig_id local_meth_sig
              lm_bind = L loc (bind { fun_id = L loc (idName local_meth_id) })
                             -- Substitute the local_meth_name for the binder
			     -- NB: the binding is always a FunBind
	; (ev_binds, (tc_bind, _, _)) 
               <- checkConstraints skol_info tyvars dfun_ev_vars $
	          tcPolyCheck NonRecursive no_prag_fn local_meth_sig [lm_bind]

        ; let export = ABE { abe_wrap = idHsWrapper, abe_poly = meth_id
                           , abe_mono = local_meth_id, abe_prags = specs }
              full_bind = AbsBinds { abs_tvs = tyvars, abs_ev_vars = dfun_ev_vars
                                   , abs_exports = [export]
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
type HsSigFun = NameEnv (LHsType Name)

emptyHsSigs :: HsSigFun
emptyHsSigs = emptyNameEnv

mkHsSigFun :: [LSig Name] -> HsSigFun
mkHsSigFun sigs = mkNameEnv [(n, hs_ty) 
                            | L _ (TypeSig ns hs_ty) <- sigs
                            , L _ n <- ns ]

lookupHsSig :: HsSigFun -> Name -> Maybe (LHsType Name)
lookupHsSig = lookupNameEnv

---------------------------
findMethodBind	:: Name  	        -- Selector name
          	-> LHsBinds Name 	-- A group of bindings
		-> Maybe (LHsBind Name, SrcSpan)
          	-- Returns the binding, and the binding 
                -- site of the method binder
findMethodBind sel_name binds
  = foldlBag mplus Nothing (mapBag f binds)
  where 
    f bind@(L _ (FunBind { fun_id = L bndr_loc op_name }))
             | op_name == sel_name
    	     = Just (bind, bndr_loc)
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
		Error messages
%*									*
%************************************************************************

\begin{code}
tcMkDeclCtxt :: TyClDecl Name -> SDoc
tcMkDeclCtxt decl = hsep [ptext (sLit "In the"), pprTyClDeclFlavour decl, 
                      ptext (sLit "declaration for"), quotes (ppr (tcdName decl))]

tcAddDeclCtxt :: TyClDecl Name -> TcM a -> TcM a
tcAddDeclCtxt decl thing_inside
  = addErrCtxt (tcMkDeclCtxt decl) thing_inside

badMethodErr :: Outputable a => a -> Name -> SDoc
badMethodErr clas op
  = hsep [ptext (sLit "Class"), quotes (ppr clas), 
	  ptext (sLit "does not have a method"), quotes (ppr op)]

badGenericMethod :: Outputable a => a -> Name -> SDoc
badGenericMethod clas op
  = hsep [ptext (sLit "Class"), quotes (ppr clas), 
	  ptext (sLit "has a generic-default signature without a binding"), quotes (ppr op)]

{-
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
-}
badDmPrag :: Id -> Sig Name -> TcM ()
badDmPrag sel_id prag
  = addErrTc (ptext (sLit "The") <+> hsSigDoc prag <+> ptext (sLit "for default method") 
              <+> quotes (ppr sel_id) 
              <+> ptext (sLit "lacks an accompanying binding"))
\end{code}
