%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcMonadFns]{Auxilliary functions for typechecker monad}

\begin{code}
#include "HsVersions.h"

module TcMonadFns (
	newDict, newDicts, newMethod, newOverloadedLit,

	copyTyVars,
	newOpenTyVarTy, newPolyTyVarTy,
	newPolyTyVarTys,

--UNUSED:	newLocalWithOpenTyVarTy, newLocalWithPolyTyVarTy,
	newLocalWithGivenTy,
	newSpecPragmaId, newSpecId,
	newClassOpLocals,
	newLocalsWithOpenTyVarTys, newLocalsWithPolyTyVarTys,

	mkIdsWithOpenTyVarTys, mkIdsWithPolyTyVarTys,
	mkIdsWithGivenTys,

	applyTcSubstAndCollectTyVars,
	applyTcSubstAndExpectTyVars,

	-- and to make the interface self-sufficient...
	Bag, Class, Binds, MonoBinds, TypecheckedPat, Id, Inst, SpecInfo,
	OverloadedLit, InstOrigin, TcResult, Name, SrcLoc, Subst, Maybe,
	Error(..), TyVar, UniType, UnifyErrContext, UniqueSupply,
	PprStyle, Pretty(..), PrettyRep
    ) where

import TcMonad		-- the underlying monadery
import AbsSyn

import AbsUniType
import Id		( mkId, mkUserLocal, mkSpecPragmaId, mkSpecId,
			  selectIdInfoForSpecId, Id, DictVar(..) )
import IdInfo
import Inst		( mkDict, mkMethod, mkLitInst,
			  Inst(..), -- .. for pragmas
			  OverloadedLit, InstOrigin
			)
import Maybes		( Maybe(..) )
import E		( LVE(..) )
import Errors		( Error(..), UnifyErrInfo )
import Unique		( Unique, UniqueSupply )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[TcMonadFns-newNameThings]{Making new things from the name supply}
%*									*
%************************************************************************

@newPolyTyVars@ takes list of ``old'' template type vars, and manufactures 
a list of freshly-uniqued type vars.

\begin{code}
copyTyVars :: [TyVarTemplate]		-- Old type vars
	   -> NF_TcM
		([(TyVarTemplate,TauType)],--Old-to-new assoc list
		 [TyVar],		-- New type vars
		 [TauType])		-- New type vars wrapped in a UniTyVar

copyTyVars old_tyvars
  = getTyVarUniquesTc (length old_tyvars) `thenNF_Tc` \ new_uniqs ->
    returnNF_Tc (instantiateTyVarTemplates old_tyvars new_uniqs)

newOpenTyVarTys :: Int -> NF_TcM [UniType]
newOpenTyVarTys n
  = getTyVarUniquesTc n	`thenLazilyNF_Tc` \ new_uniqs ->
    returnNF_Tc [mkTyVarTy (mkOpenSysTyVar u) | u <- new_uniqs]

newPolyTyVarTys :: Int -> NF_TcM [UniType]
newPolyTyVarTys n
  = getTyVarUniquesTc n	`thenLazilyNF_Tc` \ new_uniqs ->
    returnNF_Tc [mkTyVarTy (mkPolySysTyVar u) | u <- new_uniqs]

newOpenTyVarTy, newPolyTyVarTy :: NF_TcM UniType
newOpenTyVarTy
  = getTyVarUniqueTc `thenLazilyNF_Tc` \ new_uniq ->
    returnNF_Tc (mkTyVarTy (mkOpenSysTyVar new_uniq))

newPolyTyVarTy
  = getTyVarUniqueTc `thenLazilyNF_Tc` \ new_uniq ->
    returnNF_Tc (mkTyVarTy (mkPolySysTyVar new_uniq))
\end{code}

The functions @newDicts@, @newMethod@, and @newOverloadedLit@ build
new @Inst@s.
  
\begin{code}
newDicts :: InstOrigin -> ThetaType -> NF_TcM [Inst]
newDicts orig theta
 = getUniquesTc (length theta)		`thenNF_Tc` \ new_uniqs ->
   returnNF_Tc (zipWith mk_dict_var new_uniqs theta) 
 where
   mk_dict_var u (clas, ty) = mkDict u clas ty orig

newDict :: InstOrigin -> Class -> UniType -> NF_TcM Inst
newDict orig clas ty
 = getUniqueTc 		`thenNF_Tc` \ new_uniq ->
   returnNF_Tc (mkDict new_uniq clas ty orig)

newMethod :: InstOrigin -> Id -> [UniType] -> NF_TcM Inst
newMethod orig id tys
 = getUniqueTc 			`thenNF_Tc` \ new_uniq ->
   returnNF_Tc (mkMethod new_uniq id tys orig)

newOverloadedLit :: InstOrigin -> OverloadedLit -> UniType -> NF_TcM Inst
newOverloadedLit orig lit ty
 = getUniqueTc 			`thenNF_Tc` \ new_uniq ->
   returnNF_Tc (mkLitInst new_uniq lit ty orig)
\end{code}

Make a fresh batch of locals, derived from name, each typed with a fresh
type variable, and return an LVE of them. 
\begin{itemize}

\item	@mkIdsWithTyVarTys@ uses the supplied names directly (including their
	uniques), and generates a @TopId@ or @Local@ depending on whether
	the name is a @FullName@ or not.

\item	@mkIdsWithGivenTys@ does as above, but the types are supplied.
\end{itemize}

\begin{code}
mkIdsWithPolyTyVarTys, mkIdsWithOpenTyVarTys :: [Name] -> NF_TcM LVE
mkIdsWithPolyTyVarTys names
  = let
	no_of_names = length names
    in
    newPolyTyVarTys no_of_names  `thenNF_Tc` \ tys ->
    returnNF_Tc (mkIdsWithGivenTys names tys (nOfThem no_of_names noIdInfo))

mkIdsWithOpenTyVarTys names
  = let
	no_of_names = length names
    in
    newOpenTyVarTys no_of_names  `thenNF_Tc` \ tys ->
    returnNF_Tc (mkIdsWithGivenTys names tys (nOfThem no_of_names noIdInfo))

mkIdsWithGivenTys :: [Name] -> [UniType] -> [IdInfo] -> LVE
    -- not monadic any more (WDP 94/05)
    -- Not done w/ zips/etc for "efficiency" (?)
mkIdsWithGivenTys [] [] _ = []
mkIdsWithGivenTys (name:names) (ty:tys) (id_info:id_infos)
  = (name, mkId name ty id_info) : mkIdsWithGivenTys names tys id_infos

newLocalsWithOpenTyVarTys, newLocalsWithPolyTyVarTys  :: [Name] -> NF_TcM [Id]
newLocalsWithOpenTyVarTys = new_locals_given_tyvar_fun newOpenTyVarTys
newLocalsWithPolyTyVarTys = new_locals_given_tyvar_fun newPolyTyVarTys

new_locals_given_tyvar_fun new_tyvar_fun names
  = new_tyvar_fun no_of_names		`thenNF_Tc` \ tys ->
    getUniquesTc no_of_names		`thenNF_Tc` \ uniqs ->
    let  ids = zipWith3 mk_local names uniqs tys  in
    returnNF_Tc ids
  where
    no_of_names = length names
    mk_local name uniq ty = mkUserLocal (getOccurrenceName name) uniq ty 
					(getSrcLoc name)
\end{code}

@newLocal*@ creates a new unique local variable with the given
string and type. @newLocals@ is similar, but works on lists of strings
and types.

\begin{code}
{- UNUSED:
newLocalWithOpenTyVarTy, newLocalWithPolyTyVarTy  :: Name -> NF_TcM Id

newLocalWithOpenTyVarTy name
  = newOpenTyVarTy 	`thenNF_Tc` \ ty ->
    newLocalWithGivenTy name ty

newLocalWithPolyTyVarTy name
  = newPolyTyVarTy 	`thenNF_Tc` \ ty ->
    newLocalWithGivenTy name ty
-}

newLocalWithGivenTy :: Name -> UniType -> NF_TcM Id
newLocalWithGivenTy name ty 
  = getUniqueTc 	`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkUserLocal (getOccurrenceName name) uniq ty (getSrcLoc name))

newSpecPragmaId :: Name -> UniType -> Maybe SpecInfo -> NF_TcM Id
newSpecPragmaId name ty specinfo
  = getUniqueTc 	`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkSpecPragmaId (getOccurrenceName name) uniq ty specinfo (getSrcLoc name))

newSpecId :: Id -> [Maybe UniType] -> UniType -> NF_TcM Id
newSpecId unspec spec_tys ty
  = getUniqueTc 	`thenNF_Tc` \ uniq ->
    returnNF_Tc (mkSpecId uniq unspec spec_tys ty (selectIdInfoForSpecId unspec))
\end{code}

ToDo: This @newClassOpLocals@ is used only to make new ClassOps.  Pretty yukky.

\begin{code}
newClassOpLocals :: [(TyVarTemplate, TauType)]
					-- The class type variable mapped to 
					-- the instance type (an InstTyEnv)
		 -> [ClassOp] 		-- The class ops
		 -> NF_TcM [Id]		-- Suitable Ids for the polymorphic
					-- methods
newClassOpLocals inst_env ops
  = getSrcLocTc				`thenNF_Tc` \ src_loc ->
    getUniquesTc (length ops)		`thenNF_Tc` \ uniqs ->
    returnNF_Tc (zipWith (new_local src_loc) ops uniqs)
  where
    new_local src_loc op uniq
      = mkUserLocal (getClassOpString op)
		    uniq
		    (instantiateTy inst_env (getClassOpLocalType op))
		    src_loc
\end{code}

%************************************************************************
%*									*
Back-substitution functions.  These just apply the current
substitution to their argument(s).
%*									*
%************************************************************************

@applyTcSubstAndCollectTyVars@ applies a substitution to a list of type
variables, takes the free type vars of the resulting types, and
returns all of them as list without duplications.

\begin{code}
applyTcSubstAndCollectTyVars :: [TyVar] -> NF_TcM [TyVar]
applyTcSubstAndCollectTyVars tyvars
  = applyTcSubstToTyVars tyvars	`thenNF_Tc` \ tys ->
    returnNF_Tc (extractTyVarsFromTys tys)

applyTcSubstAndExpectTyVars :: [TyVar] -> NF_TcM [TyVar]
applyTcSubstAndExpectTyVars tyvars
  = applyTcSubstToTyVars tyvars	`thenNF_Tc` \ tys ->
    returnNF_Tc (map (getTyVar "applyTcSubstAndExpectTyVars") tys)
\end{code}
