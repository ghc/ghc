%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcInstUtil]{Utilities for typechecking instance declarations}

The bits common to TcInstDcls and TcDeriv.

\begin{code}
#include "HsVersions.h"

module TcInstUtil (
	InstInfo(..),
	mkInstanceRelatedIds,
	buildInstanceEnvs
    ) where

IMP_Ubiq()

import HsSyn		( MonoBinds, Fake, InPat, Sig )
import RnHsSyn		( SYN_IE(RenamedMonoBinds), RenamedSig(..), 
			  RenamedInstancePragmas(..) )

import TcMonad		hiding ( rnMtoTcM )
import Inst		( SYN_IE(InstanceMapper) )

import Bag		( bagToList )
import Class		( GenClass, GenClassOp, SYN_IE(ClassInstEnv),
			  classBigSig, classOps, classOpLocalType,
			  SYN_IE(ClassOp)
			)
import CoreSyn		( GenCoreExpr(..), mkValLam, mkTyApp )
import Id		( GenId, mkDictFunId, mkConstMethodId, mkSysLocal )
import MatchEnv		( nullMEnv, insertMEnv )
import Maybes		( MaybeErr(..), mkLookupFunDef )
import Name		( getSrcLoc, Name{--O only-} )
import PprType		( GenClass, GenType, GenTyVar )
import Pretty
import SpecEnv		( SpecEnv, nullSpecEnv, addOneToSpecEnv )
import SrcLoc		( SrcLoc )
import Type		( mkSigmaTy, mkForAllTys, mkDictTy, mkTyVarTys,
			  splitForAllTy, instantiateTy, matchTy, SYN_IE(ThetaType) )
import TyVar		( GenTyVar )
import Unique		( Unique )
import Util		( equivClasses, zipWithEqual, panic )

import IdInfo		( noIdInfo )
--import TcPragmas	( tcDictFunPragmas, tcGenPragmas )
\end{code}

    instance c => k (t tvs) where b

\begin{code}
data InstInfo
  = InstInfo
      Class	        -- Class, k
      [TyVar]		-- Type variables, tvs
      Type		-- The type at which the class is being instantiated
      ThetaType		-- inst_decl_theta: the original context, c, from the
			--   instance declaration.  It constrains (some of)
			--   the TyVars above
      ThetaType		-- dfun_theta: the inst_decl_theta, plus one
			--   element for each superclass; the "Mark
			--   Jones optimisation"
      Id		-- The dfun id
      [Id]		-- Constant methods (either all or none)
      RenamedMonoBinds	-- Bindings, b
      Bool		-- True <=> local instance decl
      Module		-- Name of module where this instance defined
      SrcLoc		-- Source location assoc'd with this instance's defn
      [RenamedSig]	-- User pragmas recorded for generating specialised instances
\end{code}

%************************************************************************
%*									*
\subsection{Creating instance related Ids}
%*									*
%************************************************************************

\begin{code}
mkInstanceRelatedIds :: Bool
		     -> SrcLoc
		     -> Module
                     -> RenamedInstancePragmas
		     -> Class 
		     -> [TyVar]
		     -> Type
		     -> ThetaType
		     -> [RenamedSig]
		     -> TcM s (Id, ThetaType, [Id])

mkInstanceRelatedIds from_here src_loc inst_mod inst_pragmas
		     clas inst_tyvars inst_ty inst_decl_theta uprags
  = 	-- MAKE THE DFUN ID
    let
	dfun_theta = case inst_decl_theta of
			[]    -> []	-- If inst_decl_theta is empty, then we don't
					-- want to have any dict arguments, so that we can
					-- expose the constant methods.

			other -> inst_decl_theta ++ super_class_theta
					-- Otherwise we pass the superclass dictionaries to
					-- the dictionary function; the Mark Jones optimisation.

	dfun_ty = mkSigmaTy inst_tyvars dfun_theta (mkDictTy clas inst_ty)
    in
    tcGetUnique 			`thenNF_Tc` \ dfun_uniq ->
    fixTc ( \ rec_dfun_id ->

{- LATER
	tcDictFunPragmas dfun_ty rec_dfun_id inst_pragmas
					`thenNF_Tc` \ dfun_pragma_info ->
	let
	    dfun_specenv = mkInstSpecEnv clas inst_ty inst_tyvars dfun_theta
	    dfun_id_info = dfun_pragma_info `addInfo` dfun_specenv
	in
-}
	let dfun_id_info = noIdInfo in	-- For now

	returnTc (mkDictFunId dfun_uniq clas inst_ty dfun_ty from_here src_loc inst_mod dfun_id_info)
    ) `thenTc` \ dfun_id ->

	-- MAKE THE CONSTANT-METHOD IDS
	-- if there are no type variables involved
    (if (null inst_decl_theta)
     then
	mapTc mk_const_meth_id class_ops
     else
	returnTc []
    )					`thenTc` \ const_meth_ids ->

    returnTc (dfun_id, dfun_theta, const_meth_ids)
  where
    (class_tyvar, super_classes, _, class_ops, _, _) = classBigSig clas
    tenv = [(class_tyvar, inst_ty)]
  
    super_class_theta = super_classes `zip` repeat inst_ty

    mk_const_meth_id op
	= tcGetUnique		`thenNF_Tc` \ uniq ->
	  fixTc (\ rec_const_meth_id ->

{- LATER
		-- Figure out the IdInfo from the pragmas
	     (case assocMaybe opname_prag_pairs (getName op) of
		Nothing   -> returnTc inline_info
		Just prag -> tcGenPragmas (Just meth_ty) rec_const_meth_id prag
	     )			`thenNF_Tc` \ id_info ->
-}
	     let id_info = noIdInfo 	-- For now
	     in
	     returnTc (mkConstMethodId uniq clas op inst_ty meth_ty
				       from_here src_loc inst_mod id_info)
	  )
	where
	  op_ty       = classOpLocalType op
	  meth_ty     = mkForAllTys inst_tyvars (instantiateTy tenv op_ty)
{- LATER
	  inline_me   = isIn "mkInstanceRelatedIds" op ops_to_inline
	  inline_info = if inline_me
			then noIdInfo `addInfo_UF` (iWantToBeINLINEd UnfoldAlways)
			else noIdInfo

    opname_prag_pairs = case inst_pragmas of
			   ConstantInstancePragma _ name_prag_pairs -> name_prag_pairs
			   other_inst_pragmas			    -> []

    ops_to_inline = [op | (InlineSig op _) <- uprags]
-}
\end{code}


%************************************************************************
%*									*
\subsection{Converting instance info into suitable InstEnvs}
%*									*
%************************************************************************

\begin{code}
buildInstanceEnvs :: Bag InstInfo
		  -> TcM s InstanceMapper

buildInstanceEnvs info
  = let
    	icmp :: InstInfo -> InstInfo -> TAG_
    	(InstInfo c1 _ _ _ _ _ _ _ _ _ _ _) `icmp` (InstInfo c2 _ _ _ _ _ _ _ _ _ _ _)
	  = c1 `cmp` c2

	info_by_class = equivClasses icmp (bagToList info)
    in
    mapTc buildInstanceEnv info_by_class    `thenTc` \ inst_env_entries ->
    let
	class_lookup_fn = mkLookupFunDef (==) inst_env_entries 
					 (nullMEnv, \ o -> nullSpecEnv)
    in
    returnTc class_lookup_fn
\end{code}

\begin{code}
buildInstanceEnv :: [InstInfo]		-- Non-empty, and all for same class
		 -> TcM s (Class, (ClassInstEnv, (ClassOp -> SpecEnv)))

buildInstanceEnv inst_infos@((InstInfo clas _ _ _ _ _ _ _ _ _ _ _) : _)
  = foldlTc addClassInstance
	    (nullMEnv, [(op, nullSpecEnv) | op <- classOps clas])
	    inst_infos
					`thenTc` \ (class_inst_env, op_inst_envs) ->
    returnTc (clas, (class_inst_env,
		     mkLookupFunDef (==) op_inst_envs
				    (panic "buildInstanceEnv")))
\end{code}

@addClassInstance@ adds the appropriate stuff to the @ClassInstEnv@
based on information from a single instance declaration.  It complains
about any overlap with an existing instance.

\begin{code}
addClassInstance
    :: (ClassInstEnv, [(ClassOp,SpecEnv)])
    -> InstInfo
    -> TcM s (ClassInstEnv, [(ClassOp,SpecEnv)])

addClassInstance
    (class_inst_env, op_spec_envs)
    (InstInfo clas inst_tyvars inst_ty _ _ 
	      dfun_id const_meth_ids _ _ _ src_loc _)
  = 

-- We only add specialised/overlapped instances
-- if we are specialising the overloading
-- ToDo ... This causes getConstMethodId errors!
--
--    if not (is_plain_instance inst_ty) && not opt_SpecialiseOverloaded
--    then
--	-- Drop this specialised/overlapped instance
--	returnTc (class_inst_env, op_spec_envs)
--    else	

	-- Add the instance to the class's instance environment
    case insertMEnv matchTy class_inst_env inst_ty dfun_id of {
	Failed (ty', dfun_id')    -> dupInstFailure clas (inst_ty, src_loc) 
							 (ty', getSrcLoc dfun_id');
	Succeeded class_inst_env' -> 

	-- If there are any constant methods, then add them to 
	-- the SpecEnv of each class op (ie selector)
	--
	-- Example.  class    Foo a     where { op :: Baz b => a -> b; ... }
	--	     instance Foo (p,q) where { op (x,y) = ...       ; ... }
	--
	-- The class decl means that 
	--	op :: forall a. Foo a => forall b. Baz b => a -> b
	--
	-- The constant method from the instance decl will be:
	--	op_Pair :: forall p q b. Baz b => (p,q) -> b
	--
	-- What we put in op's SpecEnv is
	--	(p,q) |-->  (\d::Foo (p,q) -> op_Pair p q)
	--
	-- Here, [p,q] are the inst_tyvars, and d is a dict whose only
	-- purpose is to cancel with the dict to which op is applied.
	-- 
	-- NOTE THAT this correctly deals with the case where there are
	-- constant methods even though there are type variables in the
	-- instance declaration.

    tcGetUnique				`thenNF_Tc` \ uniq ->
    let 
      dict = mkSysLocal SLIT("dict_tpl") uniq (mkDictTy clas inst_ty) src_loc
		-- Slightly disgusting, but it's only a placeholder for
		-- a dictionary to be chucked away.

      op_spec_envs' | null const_meth_ids = op_spec_envs
		    | otherwise		  = zipWithEqual "add_const_meth" add_const_meth op_spec_envs const_meth_ids

      add_const_meth (op,spec_env) meth_id
        = (op, case addOneToSpecEnv spec_env [inst_ty] rhs of
		 Failed (tys', rhs') -> panic "TcInstDecls:add_const_meth"
	         Succeeded spec_env' -> spec_env' )
        where
	  rhs = mkValLam [dict] (mkTyApp (Var meth_id) (mkTyVarTys inst_tyvars))
    in
    returnTc (class_inst_env', op_spec_envs')
    }
\end{code}

\begin{code}
dupInstFailure clas info1@(ty1, locn1) info2@(ty2, locn2)
	-- Overlapping/duplicate instances for given class; msg could be more glamourous
  = tcAddErrCtxt ctxt $
    failTc (\sty -> ppStr "Duplicate or overlapping instance declarations")
  where
    ctxt sty = ppHang (ppSep [ppBesides[ppStr "Class `", ppr sty clas, ppStr "'"],
			      ppBesides[ppStr "type `", ppr sty ty1, ppStr "'"]])
		    4 (ppSep [ppBesides [ppStr "at ", ppr sty locn1],
		    	      ppBesides [ppStr "and ", ppr sty locn2]])
\end{code}
