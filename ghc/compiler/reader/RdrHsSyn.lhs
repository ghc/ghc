%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RdrHsSyn]{Specialisations of the @HsSyn@ syntax for the reader}

(Well, really, for specialisations involving @RdrName@s, even if
they are used somewhat later on in the compiler...)

\begin{code}
module RdrHsSyn (
	RdrNameArithSeqInfo,
	RdrNameBangType,
	RdrNameClassOpSig,
	RdrNameConDecl,
	RdrNameContext,
	RdrNameSpecDataSig,
	RdrNameDefaultDecl,
	RdrNameForeignDecl,
	RdrNameGRHS,
	RdrNameGRHSs,
	RdrNameHsBinds,
	RdrNameHsDecl,
	RdrNameHsExpr,
	RdrNameHsModule,
	RdrNameIE,
	RdrNameImportDecl,
	RdrNameInstDecl,
	RdrNameMatch,
	RdrNameMonoBinds,
	RdrNamePat,
	RdrNameHsType,
	RdrNameSig,
	RdrNameStmt,
	RdrNameTyClDecl,
	RdrNameRuleBndr,
	RdrNameRuleDecl,

	RdrNameClassOpPragmas,
	RdrNameClassPragmas,
	RdrNameDataPragmas,
	RdrNameGenPragmas,
	RdrNameInstancePragmas,
	extractHsTyRdrNames, 
	extractPatsTyVars, extractRuleBndrsTyVars,

	mkOpApp, mkClassDecl, mkClassOpSig
    ) where

#include "HsVersions.h"

import HsSyn
import OccName		( mkClassTyConOcc, mkClassDataConOcc, 
			  mkSuperDictSelOcc, mkDefaultMethodOcc
			)
import RdrName		( RdrName, isRdrTyVar, mkRdrUnqual, rdrNameOcc )
import Util		( thenCmp )
import HsPragmas	( GenPragmas, ClassPragmas, DataPragmas, ClassOpPragmas, InstancePragmas )
import List		( nub )
import Outputable
\end{code}

 
%************************************************************************
%*									*
\subsection{Type synonyms}
%*									*
%************************************************************************

\begin{code}
type RdrNameArithSeqInfo	= ArithSeqInfo		RdrName RdrNamePat
type RdrNameBangType		= BangType		RdrName
type RdrNameClassOpSig		= Sig			RdrName
type RdrNameConDecl		= ConDecl		RdrName
type RdrNameContext		= Context 		RdrName
type RdrNameHsDecl		= HsDecl		RdrName RdrNamePat
type RdrNameSpecDataSig		= SpecDataSig		RdrName
type RdrNameDefaultDecl		= DefaultDecl		RdrName
type RdrNameForeignDecl		= ForeignDecl		RdrName
type RdrNameGRHS		= GRHS			RdrName RdrNamePat
type RdrNameGRHSs		= GRHSs			RdrName RdrNamePat
type RdrNameHsBinds		= HsBinds		RdrName RdrNamePat
type RdrNameHsExpr		= HsExpr		RdrName RdrNamePat
type RdrNameHsModule		= HsModule		RdrName RdrNamePat
type RdrNameIE			= IE			RdrName
type RdrNameImportDecl 		= ImportDecl		RdrName
type RdrNameInstDecl		= InstDecl		RdrName RdrNamePat
type RdrNameMatch		= Match			RdrName RdrNamePat
type RdrNameMonoBinds		= MonoBinds		RdrName RdrNamePat
type RdrNamePat			= InPat			RdrName
type RdrNameHsType		= HsType		RdrName
type RdrNameSig			= Sig			RdrName
type RdrNameStmt		= Stmt			RdrName RdrNamePat
type RdrNameTyClDecl		= TyClDecl		RdrName RdrNamePat
type RdrNameRuleBndr		= RuleBndr		RdrName
type RdrNameRuleDecl		= RuleDecl		RdrName RdrNamePat

type RdrNameClassOpPragmas	= ClassOpPragmas	RdrName
type RdrNameClassPragmas	= ClassPragmas		RdrName
type RdrNameDataPragmas		= DataPragmas		RdrName
type RdrNameGenPragmas		= GenPragmas		RdrName
type RdrNameInstancePragmas	= InstancePragmas	RdrName
\end{code}


%************************************************************************
%*									*
\subsection{A few functions over HsSyn at RdrName}
%*									*
%************************************************************************

@extractHsTyRdrNames@ finds the free variables of a HsType
It's used when making the for-alls explicit.

\begin{code}
extractHsTyRdrNames :: HsType RdrName -> [RdrName]
extractHsTyRdrNames ty = nub (extract_ty ty [])

extractRuleBndrsTyVars :: [RuleBndr RdrName] -> [RdrName]
extractRuleBndrsTyVars bndrs = filter isRdrTyVar (nub (foldr go [] bndrs))
			     where
			       go (RuleBndr _)       acc = acc
			       go (RuleBndrSig _ ty) acc = extract_ty ty acc

extractHsCtxtRdrNames :: Context RdrName -> [RdrName]
extractHsCtxtRdrNames ty = nub (extract_ctxt ty [])

extract_ctxt ctxt acc = foldr extract_ass acc ctxt
		      where
			extract_ass (cls, tys) acc = foldr extract_ty (cls : acc) tys

extract_ty (MonoTyApp ty1 ty2)	acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (MonoListTy ty)	acc = extract_ty ty acc
extract_ty (MonoTupleTy tys _)  acc = foldr extract_ty acc tys
extract_ty (MonoFunTy ty1 ty2)	acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (MonoDictTy cls tys)	acc = foldr extract_ty (cls : acc) tys
extract_ty (MonoUsgTy usg ty)	acc = extract_ty ty acc
extract_ty (MonoTyVar tv)       acc = tv : acc
extract_ty (HsForAllTy (Just tvs) ctxt ty) 
				acc = acc ++
				      (filter (`notElem` locals) $
				       extract_ctxt ctxt (extract_ty ty []))
				    where
				      locals = map getTyVarName tvs


extractPatsTyVars :: [RdrNamePat] -> [RdrName]
extractPatsTyVars pats = nub (foldr extract_pat [] pats)

extract_pat (SigPatIn pat ty)	   acc = extract_ty ty acc
extract_pat WildPatIn	      	   acc = acc
extract_pat (VarPatIn var)         acc = acc
extract_pat (LitPatIn _)	   acc = acc
extract_pat (LazyPatIn pat)        acc = extract_pat pat acc
extract_pat (AsPatIn a pat)        acc = extract_pat pat acc
extract_pat (NPlusKPatIn n _)      acc = acc
extract_pat (ConPatIn c pats)      acc = foldr extract_pat acc pats
extract_pat (ConOpPatIn p1 c f p2) acc = extract_pat p1 (extract_pat p2 acc)
extract_pat (NegPatIn  pat)        acc = extract_pat pat acc
extract_pat (ParPatIn  pat)        acc = extract_pat pat acc
extract_pat (ListPatIn pats)       acc = foldr extract_pat acc pats
extract_pat (TuplePatIn pats _)    acc = foldr extract_pat acc pats
extract_pat (RecPatIn c fields)    acc = foldr (\ (f,pat,_) acc -> extract_pat pat acc) acc fields
\end{code}


A useful function for building @OpApps@.  The operator is always a variable,
and we don't know the fixity yet.

\begin{code}
mkOpApp e1 op e2 = OpApp e1 (HsVar op) (error "mkOpApp:fixity") e2
\end{code}

mkClassDecl builds a RdrClassDecl, filling in the names for tycon and datacon
by deriving them from the name of the class.  We fill in the names for the
tycon and datacon corresponding to the class, by deriving them from the
name of the class itself.  This saves recording the names in the interface
file (which would be equally good).

Similarly for mkClassOpSig and default-method names.

\begin{code}
mkClassDecl cxt cname tyvars sigs mbinds prags loc
  = ClassDecl cxt cname tyvars sigs mbinds prags tname dname sc_sel_names loc
  where
    cls_occ = rdrNameOcc cname
    dname   = mkRdrUnqual (mkClassDataConOcc cls_occ)
    tname   = mkRdrUnqual (mkClassTyConOcc   cls_occ)
    sc_sel_names = [mkRdrUnqual (mkSuperDictSelOcc n cls_occ) | n <- [1..length cxt]]
	-- We number off the superclass selectors, 1, 2, 3 etc so that we can construct
	-- names for the selectors.  Thus
	--	class (C a, C b) => D a b where ...
	-- gives superclass selectors
	--	D_sc1, D_sc2
	-- (We used to call them D_C, but now we can have two different
	--  superclasses both called C!)

mkClassOpSig has_default_method op ty loc
  | not has_default_method = ClassOpSig op Nothing	ty loc
  | otherwise		   = ClassOpSig op (Just dm_rn) ty loc
  where
    dm_rn = mkRdrUnqual (mkDefaultMethodOcc (rdrNameOcc op))
\end{code}
