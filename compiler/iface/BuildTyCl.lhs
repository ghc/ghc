%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module BuildTyCl (
        buildSynTyCon,
        buildAlgTyCon, 
        buildDataCon,
        buildPatSyn, mkPatSynMatcherId, mkPatSynWrapperId,
        TcMethInfo, buildClass,
        distinctAbstractTyConRhs, totallyAbstractTyConRhs,
        mkNewTyConRhs, mkDataTyConRhs, 
        newImplicitBinder
    ) where

#include "HsVersions.h"

import IfaceEnv
import FamInstEnv( FamInstEnvs )
import DataCon
import PatSyn
import Var
import VarSet
import BasicTypes
import Name
import MkId
import Class
import TyCon
import Type
import TypeRep
import TcType
import Id
import Coercion

import DynFlags
import TcRnMonad
import UniqSupply
import Util
import Outputable
\end{code}
	

\begin{code}
------------------------------------------------------
buildSynTyCon :: Name -> [TyVar] -> [Role] 
              -> SynTyConRhs
              -> Kind                   -- ^ Kind of the RHS
              -> TyConParent
              -> TcRnIf m n TyCon
buildSynTyCon tc_name tvs roles rhs rhs_kind parent 
  = return (mkSynTyCon tc_name kind tvs roles rhs parent)
  where kind = mkPiKinds tvs rhs_kind


------------------------------------------------------
distinctAbstractTyConRhs, totallyAbstractTyConRhs :: AlgTyConRhs
distinctAbstractTyConRhs = AbstractTyCon True
totallyAbstractTyConRhs  = AbstractTyCon False

mkDataTyConRhs :: [DataCon] -> AlgTyConRhs
mkDataTyConRhs cons
  = DataTyCon {
        data_cons = cons,
        is_enum = not (null cons) && all is_enum_con cons
		  -- See Note [Enumeration types] in TyCon
    }
  where
    is_enum_con con
       | (_tvs, theta, arg_tys, _res) <- dataConSig con
       = null theta && null arg_tys


mkNewTyConRhs :: Name -> TyCon -> DataCon -> TcRnIf m n AlgTyConRhs
-- ^ Monadic because it makes a Name for the coercion TyCon
--   We pass the Name of the parent TyCon, as well as the TyCon itself,
--   because the latter is part of a knot, whereas the former is not.
mkNewTyConRhs tycon_name tycon con 
  = do	{ co_tycon_name <- newImplicitBinder tycon_name mkNewTyCoOcc
	; let co_tycon = mkNewTypeCo co_tycon_name tycon etad_tvs etad_roles etad_rhs
	; traceIf (text "mkNewTyConRhs" <+> ppr co_tycon)
	; return (NewTyCon { data_con    = con, 
		       	     nt_rhs      = rhs_ty,
		       	     nt_etad_rhs = (etad_tvs, etad_rhs),
 		       	     nt_co 	 = co_tycon } ) }
                             -- Coreview looks through newtypes with a Nothing
                             -- for nt_co, or uses explicit coercions otherwise
  where
    tvs    = tyConTyVars tycon
    roles  = tyConRoles tycon
    inst_con_ty = applyTys (dataConUserType con) (mkTyVarTys tvs)
    rhs_ty = ASSERT( isFunTy inst_con_ty ) funArgTy inst_con_ty
	-- Instantiate the data con with the 
	-- type variables from the tycon
	-- NB: a newtype DataCon has a type that must look like
	--        forall tvs.  <arg-ty> -> T tvs
	-- Note that we *can't* use dataConInstOrigArgTys here because
	-- the newtype arising from   class Foo a => Bar a where {}
  	-- has a single argument (Foo a) that is a *type class*, so
	-- dataConInstOrigArgTys returns [].

    etad_tvs   :: [TyVar]  -- Matched lazily, so that mkNewTypeCo can
    etad_roles :: [Role]   -- return a TyCon without pulling on rhs_ty
    etad_rhs   :: Type     -- See Note [Tricky iface loop] in LoadIface
    (etad_tvs, etad_roles, etad_rhs) = eta_reduce (reverse tvs) (reverse roles) rhs_ty
 
    eta_reduce :: [TyVar]	-- Reversed
               -> [Role]        -- also reversed
	       -> Type		-- Rhs type
	       -> ([TyVar], [Role], Type)  -- Eta-reduced version
                                           -- (tyvars in normal order)
    eta_reduce (a:as) (_:rs) ty | Just (fun, arg) <- splitAppTy_maybe ty,
			          Just tv <- getTyVar_maybe arg,
			          tv == a,
			          not (a `elemVarSet` tyVarsOfType fun)
			        = eta_reduce as rs fun
    eta_reduce tvs rs ty = (reverse tvs, reverse rs, ty)
				

------------------------------------------------------
buildDataCon :: FamInstEnvs 
            -> Name -> Bool
	    -> [HsBang] 
	    -> [Name]			-- Field labels
	    -> [TyVar] -> [TyVar]	-- Univ and ext 
            -> [(TyVar,Type)]           -- Equality spec
	    -> ThetaType		-- Does not include the "stupid theta"
					-- or the GADT equalities
	    -> [Type] -> Type		-- Argument and result types
	    -> TyCon			-- Rep tycon
	    -> TcRnIf m n DataCon
-- A wrapper for DataCon.mkDataCon that
--   a) makes the worker Id
--   b) makes the wrapper Id if necessary, including
--	allocating its unique (hence monadic)
buildDataCon fam_envs src_name declared_infix arg_stricts field_lbls
	     univ_tvs ex_tvs eq_spec ctxt arg_tys res_ty rep_tycon
  = do	{ wrap_name <- newImplicitBinder src_name mkDataConWrapperOcc
	; work_name <- newImplicitBinder src_name mkDataConWorkerOcc
	-- This last one takes the name of the data constructor in the source
	-- code, which (for Haskell source anyway) will be in the DataName name
	-- space, and puts it into the VarName name space

        ; us <- newUniqueSupply
        ; dflags <- getDynFlags
	; let
		stupid_ctxt = mkDataConStupidTheta rep_tycon arg_tys univ_tvs
		data_con = mkDataCon src_name declared_infix
				     arg_stricts field_lbls
				     univ_tvs ex_tvs eq_spec ctxt
				     arg_tys res_ty rep_tycon
				     stupid_ctxt dc_wrk dc_rep
                dc_wrk = mkDataConWorkId work_name data_con
                dc_rep = initUs_ us (mkDataConRep dflags fam_envs wrap_name data_con)

	; return data_con }


-- The stupid context for a data constructor should be limited to
-- the type variables mentioned in the arg_tys
-- ToDo: Or functionally dependent on?  
--	 This whole stupid theta thing is, well, stupid.
mkDataConStupidTheta :: TyCon -> [Type] -> [TyVar] -> [PredType]
mkDataConStupidTheta tycon arg_tys univ_tvs
  | null stupid_theta = []	-- The common case
  | otherwise 	      = filter in_arg_tys stupid_theta
  where
    tc_subst	 = zipTopTvSubst (tyConTyVars tycon) (mkTyVarTys univ_tvs)
    stupid_theta = substTheta tc_subst (tyConStupidTheta tycon)
	-- Start by instantiating the master copy of the 
	-- stupid theta, taken from the TyCon

    arg_tyvars      = tyVarsOfTypes arg_tys
    in_arg_tys pred = not $ isEmptyVarSet $ 
		      tyVarsOfType pred `intersectVarSet` arg_tyvars


------------------------------------------------------
buildPatSyn :: Name -> Bool -> Bool
            -> [Var]
            -> [TyVar] -> [TyVar]     -- Univ and ext
            -> ThetaType -> ThetaType -- Prov and req
            -> Type                  -- Result type
            -> TyVar
            -> TcRnIf m n PatSyn
buildPatSyn src_name declared_infix has_wrapper args univ_tvs ex_tvs prov_theta req_theta pat_ty tv
  = do	{ (matcher, _, _) <- mkPatSynMatcherId src_name args
                                              univ_tvs ex_tvs
                                              prov_theta req_theta
                                              pat_ty tv
        ; wrapper <- case has_wrapper of
            False -> return Nothing
            True -> fmap Just $
                    mkPatSynWrapperId src_name args
                                      (univ_tvs ++ ex_tvs) (prov_theta ++ req_theta)
                                      pat_ty
        ; return $ mkPatSyn src_name declared_infix
                            args
                            univ_tvs ex_tvs
                            prov_theta req_theta
                            pat_ty
                            matcher
                            wrapper }

mkPatSynMatcherId :: Name
                  -> [Var]
                  -> [TyVar]
                  -> [TyVar]
                  -> ThetaType -> ThetaType
                  -> Type
                  -> TyVar
                  -> TcRnIf n m (Id, Type, Type)
mkPatSynMatcherId name args univ_tvs ex_tvs prov_theta req_theta pat_ty res_tv
  = do { matcher_name <- newImplicitBinder name mkMatcherOcc

       ; let res_ty = TyVarTy res_tv
             cont_ty = mkSigmaTy ex_tvs prov_theta $
                       mkFunTys (map varType args) res_ty

       ; let matcher_tau = mkFunTys [pat_ty, cont_ty, res_ty] res_ty
             matcher_sigma = mkSigmaTy (res_tv:univ_tvs) req_theta matcher_tau
             matcher_id = mkVanillaGlobal matcher_name matcher_sigma
       ; return (matcher_id, res_ty, cont_ty) }

mkPatSynWrapperId :: Name
                  -> [Var]
                  -> [TyVar]
                  -> ThetaType
                  -> Type
                  -> TcRnIf n m Id
mkPatSynWrapperId name args qtvs theta pat_ty
  = do { wrapper_name <- newImplicitBinder name mkDataConWrapperOcc

       ; let wrapper_tau = mkFunTys (map varType args) pat_ty
             wrapper_sigma = mkSigmaTy qtvs theta wrapper_tau

       ; let wrapper_id = mkVanillaGlobal wrapper_name wrapper_sigma
       ; return wrapper_id }

\end{code}


------------------------------------------------------
\begin{code}
type TcMethInfo = (Name, DefMethSpec, Type)  
        -- A temporary intermediate, to communicate between 
        -- tcClassSigs and buildClass.

buildClass :: Name -> [TyVar] -> [Role] -> ThetaType
	   -> [FunDep TyVar]		   -- Functional dependencies
	   -> [ClassATItem]		   -- Associated types
	   -> [TcMethInfo]                 -- Method info
	   -> ClassMinimalDef              -- Minimal complete definition
	   -> RecFlag			   -- Info for type constructor
	   -> TcRnIf m n Class

buildClass tycon_name tvs roles sc_theta fds at_items sig_stuff mindef tc_isrec
  = fixM  $ \ rec_clas -> 	-- Only name generation inside loop
    do	{ traceIf (text "buildClass")

	; datacon_name <- newImplicitBinder tycon_name mkClassDataConOcc
		-- The class name is the 'parent' for this datacon, not its tycon,
		-- because one should import the class to get the binding for 
		-- the datacon


	; op_items <- mapM (mk_op_item rec_clas) sig_stuff
	  		-- Build the selector id and default method id

	      -- Make selectors for the superclasses 
	; sc_sel_names <- mapM  (newImplicitBinder tycon_name . mkSuperDictSelOcc) 
				[1..length sc_theta]
        ; let sc_sel_ids = [ mkDictSelId sc_name rec_clas 
                           | sc_name <- sc_sel_names]
	      -- We number off the Dict superclass selectors, 1, 2, 3 etc so that we 
	      -- can construct names for the selectors. Thus
	      --      class (C a, C b) => D a b where ...
	      -- gives superclass selectors
	      --      D_sc1, D_sc2
	      -- (We used to call them D_C, but now we can have two different
	      --  superclasses both called C!)
	
	; let use_newtype = isSingleton arg_tys
		-- Use a newtype if the data constructor 
		--   (a) has exactly one value field
		--       i.e. exactly one operation or superclass taken together
                --   (b) that value is of lifted type (which they always are, because
                --       we box equality superclasses)
		-- See note [Class newtypes and equality predicates]

		-- We treat the dictionary superclasses as ordinary arguments.  
                -- That means that in the case of
		--     class C a => D a
		-- we don't get a newtype with no arguments!
	      args      = sc_sel_names ++ op_names
	      op_tys	= [ty | (_,_,ty) <- sig_stuff]
	      op_names  = [op | (op,_,_) <- sig_stuff]
	      arg_tys   = sc_theta ++ op_tys
              rec_tycon = classTyCon rec_clas
               
	; dict_con <- buildDataCon (panic "buildClass: FamInstEnvs")
                                   datacon_name
				   False 	-- Not declared infix
				   (map (const HsNoBang) args)
				   [{- No fields -}]
				   tvs [{- no existentials -}]
                                   [{- No GADT equalities -}] 
                                   [{- No theta -}]
                                   arg_tys
				   (mkTyConApp rec_tycon (mkTyVarTys tvs))
				   rec_tycon

	; rhs <- if use_newtype
		 then mkNewTyConRhs tycon_name rec_tycon dict_con
		 else return (mkDataTyConRhs [dict_con])

	; let {	clas_kind = mkPiKinds tvs constraintKind

 	      ; tycon = mkClassTyCon tycon_name clas_kind tvs roles
 	                             rhs rec_clas tc_isrec
		-- A class can be recursive, and in the case of newtypes 
		-- this matters.  For example
		-- 	class C a where { op :: C b => a -> b -> Int }
		-- Because C has only one operation, it is represented by
		-- a newtype, and it should be a *recursive* newtype.
		-- [If we don't make it a recursive newtype, we'll expand the
		-- newtype like a synonym, but that will lead to an infinite
		-- type]

	      ; result = mkClass tvs fds 
			         sc_theta sc_sel_ids at_items
				 op_items mindef tycon
	      }
	; traceIf (text "buildClass" <+> ppr tycon) 
	; return result }
  where
    mk_op_item :: Class -> TcMethInfo -> TcRnIf n m ClassOpItem
    mk_op_item rec_clas (op_name, dm_spec, _) 
      = do { dm_info <- case dm_spec of
                          NoDM      -> return NoDefMeth
                          GenericDM -> do { dm_name <- newImplicitBinder op_name mkGenDefMethodOcc
			  	          ; return (GenDefMeth dm_name) }
                          VanillaDM -> do { dm_name <- newImplicitBinder op_name mkDefaultMethodOcc
			  	          ; return (DefMeth dm_name) }
           ; return (mkDictSelId op_name rec_clas, dm_info) }
\end{code}

Note [Class newtypes and equality predicates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
	class (a ~ F b) => C a b where
	  op :: a -> b

We cannot represent this by a newtype, even though it's not
existential, because there are two value fields (the equality
predicate and op. See Trac #2238

Moreover, 
	  class (a ~ F b) => C a b where {}
Here we can't use a newtype either, even though there is only
one field, because equality predicates are unboxed, and classes
are boxed.
