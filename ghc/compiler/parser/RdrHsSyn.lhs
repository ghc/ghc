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
	RdrNameConDetails,
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
	RdrNameHsTyVar,
	RdrNameSig,
	RdrNameStmt,
	RdrNameTyClDecl,
	RdrNameRuleDecl,
	RdrNameRuleBndr,
	RdrNameHsRecordBinds,

	RdrBinding(..),
	RdrMatch(..),
	SigConverter,

	RdrNameClassOpPragmas,
	RdrNameClassPragmas,
	RdrNameDataPragmas,
	RdrNameGenPragmas,
	RdrNameInstancePragmas,
	extractHsTyRdrNames, 
	extractHsTyRdrTyVars, extractHsTysRdrTyVars,
	extractPatsTyVars, 
	extractRuleBndrsTyVars,
 
	mkOpApp, mkClassDecl, mkClassOpSig,

	cvBinds,
	cvMonoBindsAndSigs,
	cvTopDecls,
	cvValSig, cvClassOpSig, cvInstDeclSig
    ) where

#include "HsVersions.h"

import HsSyn
import Name		( mkClassTyConOcc, mkClassDataConOcc )
import OccName		( mkClassTyConOcc, mkClassDataConOcc, 
                          mkSuperDictSelOcc, mkDefaultMethodOcc
                      	)
import RdrName		( RdrName, isRdrTyVar, mkRdrUnqual, rdrNameOcc )
import Util		( thenCmp )
import HsPragmas	
import List		( nub )
import BasicTypes	( RecFlag(..) )
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
type RdrNameConDetails		= ConDetails		RdrName
type RdrNameContext		= HsContext 		RdrName
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
type RdrNameHsTyVar		= HsTyVar		RdrName
type RdrNameSig			= Sig			RdrName
type RdrNameStmt		= Stmt			RdrName RdrNamePat
type RdrNameTyClDecl		= TyClDecl		RdrName RdrNamePat
type RdrNameRuleBndr            = RuleBndr              RdrName
type RdrNameRuleDecl            = RuleDecl              RdrName RdrNamePat

type RdrNameHsRecordBinds	= HsRecordBinds		RdrName RdrNamePat

type RdrNameClassOpPragmas	= ClassOpPragmas	RdrName
type RdrNameClassPragmas	= ClassPragmas		RdrName
type RdrNameDataPragmas		= DataPragmas		RdrName
type RdrNameGenPragmas		= GenPragmas		RdrName
type RdrNameInstancePragmas	= InstancePragmas	RdrName
\end{code}


%************************************************************************
%*									*
\subsection{A few functions over HsSyn at RdrName}
%*                                                                    *
%************************************************************************

@extractHsTyRdrNames@ finds the free variables of a HsType
It's used when making the for-alls explicit.

\begin{code}
extractHsTyRdrNames :: HsType RdrName -> [RdrName]
extractHsTyRdrNames ty = nub (extract_ty ty [])

extractHsTyRdrTyVars	 :: RdrNameHsType -> [RdrName]
extractHsTyRdrTyVars ty =  filter isRdrTyVar (extractHsTyRdrNames ty)

extractHsTysRdrTyVars	  :: [RdrNameHsType] -> [RdrName]
extractHsTysRdrTyVars tys =  filter isRdrTyVar (nub (extract_tys tys []))

extractRuleBndrsTyVars :: [RuleBndr RdrName] -> [RdrName]
extractRuleBndrsTyVars bndrs = filter isRdrTyVar (nub (foldr go [] bndrs))
                           where
                             go (RuleBndr _)       acc = acc
                             go (RuleBndrSig _ ty) acc = extract_ty ty acc

extractHsCtxtRdrNames :: HsContext RdrName -> [RdrName]
extractHsCtxtRdrNames ty = nub (extract_ctxt ty [])

extract_ctxt ctxt acc = foldr extract_pred acc ctxt

extract_pred (HsPClass cls tys) acc	= foldr extract_ty (cls : acc) tys
extract_pred (HsPIParam n ty) acc	= extract_ty ty acc

extract_tys tys acc = foldr extract_ty acc tys

extract_ty (MonoTyApp ty1 ty2)          acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (MonoListTy ty)              acc = extract_ty ty acc
extract_ty (MonoTupleTy tys _)          acc = foldr extract_ty acc tys
extract_ty (MonoFunTy ty1 ty2)          acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (MonoDictTy cls tys)         acc = foldr extract_ty (cls : acc) tys
extract_ty (MonoUsgTy usg ty)           acc = extract_ty ty acc
extract_ty (MonoUsgForAllTy uv ty)      acc = extract_ty ty acc
extract_ty (MonoTyVar tv)               acc = tv : acc
extract_ty (HsForAllTy Nothing ctxt ty) acc = extract_ctxt ctxt (extract_ty ty acc)
extract_ty (HsForAllTy (Just tvs) ctxt ty) 
                                acc = acc ++
                                      (filter (`notElem` locals) $
				       extract_ctxt ctxt (extract_ty ty []))
				    where
				      locals = map getTyVarName tvs


extractPatsTyVars :: [RdrNamePat] -> [RdrName]
extractPatsTyVars pats = filter isRdrTyVar (nub (foldr extract_pat [] pats))

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

mkClassDecl builds a RdrClassDecl, filling in the names for tycon and datacon
by deriving them from the name of the class.  We fill in the names for the
tycon and datacon corresponding to the class, by deriving them from the
name of the class itself.  This saves recording the names in the interface
file (which would be equally good).

Similarly for mkClassOpSig and default-method names.
  
\begin{code}
mkClassDecl cxt cname tyvars fds sigs mbinds prags loc
  = ClassDecl cxt cname tyvars fds sigs mbinds prags tname dname sc_sel_names loc
  where
    cls_occ = rdrNameOcc cname
    dname   = mkRdrUnqual (mkClassDataConOcc cls_occ)
    tname   = mkRdrUnqual (mkClassTyConOcc   cls_occ)
    sc_sel_names = [ mkRdrUnqual (mkSuperDictSelOcc n cls_occ) 
		   | n <- [1..length cxt]]
      -- We number off the superclass selectors, 1, 2, 3 etc so that we 
      -- can construct names for the selectors.  Thus
      --      class (C a, C b) => D a b where ...
      -- gives superclass selectors
      --      D_sc1, D_sc2
      -- (We used to call them D_C, but now we can have two different
      --  superclasses both called C!)

mkClassOpSig has_default_method op ty loc
  = ClassOpSig op dm_rn has_default_method ty loc
  where
    dm_rn = mkRdrUnqual (mkDefaultMethodOcc (rdrNameOcc op))
\end{code}

A useful function for building @OpApps@.  The operator is always a variable,
and we don't know the fixity yet.

\begin{code}
mkOpApp e1 op e2 = OpApp e1 (HsVar op) (error "mkOpApp:fixity") e2
\end{code}

%************************************************************************
%*									*
\subsection[rdrBinding]{Bindings straight out of the parser}
%*									*
%************************************************************************

\begin{code}
data RdrBinding
  =   -- On input we use the Empty/And form rather than a list
    RdrNullBind
  | RdrAndBindings    RdrBinding RdrBinding

      -- Value bindings havn't been united with their
      -- signatures yet
  | RdrValBinding     RdrNameMonoBinds

      -- Signatures are mysterious; we can't
      -- tell if its a Sig or a ClassOpSig,
      -- so we just save the pieces:
  | RdrSig            RdrNameSig

      -- The remainder all fit into the main HsDecl form
  | RdrHsDecl         RdrNameHsDecl
  
type SigConverter = RdrNameSig -> RdrNameSig
\end{code}

\begin{code}
data RdrMatch
  = RdrMatch
	     [RdrNamePat]
	     (Maybe RdrNameHsType)
	     RdrNameGRHSs
\end{code}

%************************************************************************
%*									*
\subsection[cvDecls]{Convert various top-level declarations}
%*									*
%************************************************************************

We make a point not to throw any user-pragma ``sigs'' at
these conversion functions:

\begin{code}
cvValSig, cvClassOpSig, cvInstDeclSig :: SigConverter

cvValSig      sig = sig

cvInstDeclSig sig = sig

cvClassOpSig (Sig var poly_ty src_loc) = ClassOpSig var (panic "cvClassOpSig:dm_name")
							(panic "cvClassOpSig:dm_present")
							poly_ty src_loc
cvClassOpSig sig 		       = sig
\end{code}


%************************************************************************
%*									*
\subsection[cvBinds-etc]{Converting to @HsBinds@, @MonoBinds@, etc.}
%*									*
%************************************************************************

Function definitions are restructured here. Each is assumed to be recursive
initially, and non recursive definitions are discovered by the dependency
analyser.

\begin{code}
cvBinds :: SigConverter -> RdrBinding -> RdrNameHsBinds
	-- The mysterious SigConverter converts Sigs to ClassOpSigs
	-- in class declarations.  Mostly it's just an identity function

cvBinds sig_cvtr binding
  = case (cvMonoBindsAndSigs sig_cvtr binding) of { (mbs, sigs) ->
    MonoBind mbs sigs Recursive
    }
\end{code}

\begin{code}
cvMonoBindsAndSigs :: SigConverter
		   -> RdrBinding
		   -> (RdrNameMonoBinds, [RdrNameSig])

cvMonoBindsAndSigs sig_cvtr fb
  = mangle_bind (EmptyMonoBinds, []) fb
  where
    mangle_bind acc RdrNullBind
      = acc

    mangle_bind acc (RdrAndBindings fb1 fb2)
      = mangle_bind (mangle_bind acc fb1) fb2

    mangle_bind (b_acc, s_acc) (RdrSig sig)
      = (b_acc, sig_cvtr sig : s_acc)

    mangle_bind (b_acc, s_acc) (RdrValBinding binding)
      = (b_acc `AndMonoBinds` binding, s_acc)
\end{code}


%************************************************************************
%*									*
\subsection[PrefixToHS-utils]{Utilities for conversion}
%*									*
%************************************************************************

Separate declarations into all the various kinds:

\begin{code}
cvTopDecls :: RdrBinding -> [RdrNameHsDecl]
cvTopDecls bind
  = let
	(top_decls, mono_binds, sigs) = go ([], EmptyMonoBinds, []) bind 
    in
    (ValD (MonoBind mono_binds sigs Recursive) : top_decls)
  where
    go acc		  RdrNullBind		 = acc
    go acc                (RdrAndBindings b1 b2) = go (go acc b1) b2
    go (topds, mbs, sigs) (RdrHsDecl d)		 = (d : topds, mbs, sigs)
    go (topds, mbs, sigs) (RdrSig (FixSig d))    = (FixD d  : topds, mbs, sigs)
    go (topds, mbs, sigs) (RdrSig sig)		 = (topds, mbs, sig:sigs)
    go (topds, mbs, sigs) (RdrValBinding bind)   = (topds, mbs `AndMonoBinds` bind, sigs)
\end{code}
