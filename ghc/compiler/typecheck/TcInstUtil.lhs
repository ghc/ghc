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

import TcMonad
import Inst		( SYN_IE(InstanceMapper) )

import Bag		( bagToList, Bag )
import Class		( GenClass, SYN_IE(ClassInstEnv),
			  classBigSig, SYN_IE(Class)
			)
import CoreSyn		( GenCoreExpr(..), mkValLam, mkTyApp )
import Id		( GenId, mkDictFunId, mkSysLocal, SYN_IE(Id) )
import MatchEnv		( nullMEnv, insertMEnv )
import Maybes		( MaybeErr(..), mkLookupFunDef )
import Name		( getSrcLoc, Name{--O only-} )
import PprType		( GenClass, GenType, GenTyVar )
import Pretty
import SpecEnv		( SpecEnv, nullSpecEnv, addOneToSpecEnv )
import SrcLoc		( SrcLoc )
import Type		( mkSigmaTy, mkForAllTys, mkDictTy, mkTyVarTys,
			  instantiateTy, matchTy, SYN_IE(ThetaType),
			  SYN_IE(Type) )
import TyVar		( GenTyVar, SYN_IE(TyVar) )
import Unique		( Unique )
import Util		( equivClasses, zipWithEqual, panic{-, pprTrace-}, Ord3(..) )

import Outputable
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
      RenamedMonoBinds	-- Bindings, b
      SrcLoc		-- Source location assoc'd with this instance's defn
      [RenamedSig]	-- User pragmas recorded for generating specialised instances
\end{code}

%************************************************************************
%*									*
\subsection{Creating instance related Ids}
%*									*
%************************************************************************

\begin{code}
mkInstanceRelatedIds :: Name		-- Name to use for the dict fun;
		     -> Class 
		     -> [TyVar]
		     -> Type
		     -> ThetaType
		     -> (Id, ThetaType)

mkInstanceRelatedIds dfun_name clas inst_tyvars inst_ty inst_decl_theta
  = (dfun_id, dfun_theta)
  where
    (_, super_classes, _, _, _) = classBigSig clas
    super_class_theta = super_classes `zip` repeat inst_ty

    dfun_theta = case inst_decl_theta of
			[]    -> []	-- If inst_decl_theta is empty, then we don't
					-- want to have any dict arguments, so that we can
					-- expose the constant methods.

			other -> inst_decl_theta ++ super_class_theta
					-- Otherwise we pass the superclass dictionaries to
					-- the dictionary function; the Mark Jones optimisation.

    dfun_ty = mkSigmaTy inst_tyvars dfun_theta (mkDictTy clas inst_ty)

    dfun_id = mkDictFunId dfun_name dfun_ty clas inst_ty
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
    	(InstInfo c1 _ _ _ _ _ _ _ _) `icmp` (InstInfo c2 _ _ _ _ _ _ _ _)
	  = c1 `cmp` c2

	info_by_class = equivClasses icmp (bagToList info)
    in
    mapTc buildInstanceEnv info_by_class    `thenTc` \ inst_env_entries ->
    let
	class_lookup_fn = mkLookupFunDef (==) inst_env_entries nullMEnv
    in
    returnTc class_lookup_fn
\end{code}

\begin{code}
buildInstanceEnv :: [InstInfo]		-- Non-empty, and all for same class
		 -> TcM s (Class, ClassInstEnv)

buildInstanceEnv inst_infos@((InstInfo clas _ _ _ _ _ _ _ _) : _)
  = foldlTc addClassInstance
	    nullMEnv
	    inst_infos				`thenTc` \ class_inst_env ->
    returnTc (clas, class_inst_env)
\end{code}

@addClassInstance@ adds the appropriate stuff to the @ClassInstEnv@
based on information from a single instance declaration.  It complains
about any overlap with an existing instance.

\begin{code}
addClassInstance
    :: ClassInstEnv
    -> InstInfo
    -> TcM s ClassInstEnv

addClassInstance class_inst_env
    (InstInfo clas inst_tyvars inst_ty _ _ 
	      dfun_id _ src_loc _)
  = 	-- Add the instance to the class's instance environment
    case insertMEnv matchTy class_inst_env inst_ty dfun_id of
	Failed (ty', dfun_id')    -> recoverTc (returnTc class_inst_env) $
				     dupInstFailure clas (inst_ty, src_loc) 
							 (ty', getSrcLoc dfun_id');
	Succeeded class_inst_env' -> returnTc class_inst_env'

{- 		OLD STUFF FOR CONSTANT METHODS 

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
		END OF OLD STUFF -}

\end{code}

\begin{code}
dupInstFailure clas info1@(ty1, locn1) info2@(ty2, locn2)
	-- Overlapping/duplicate instances for given class; msg could be more glamourous
  = tcAddErrCtxt ctxt $
    failTc (\sty -> ptext SLIT("Duplicate or overlapping instance declarations"))
  where
    ctxt sty = sep [hsep [ptext SLIT("for"), 
			  pprQuote sty $ \ sty -> ppr sty clas <+> ppr sty ty1],
		    nest 4 (sep [ptext SLIT("at")  <+> ppr sty locn1,
		    	         ptext SLIT("and") <+> ppr sty locn2])]
\end{code}
