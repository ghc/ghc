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
	RdrNameDefaultDecl,
	RdrNameForeignDecl,
	RdrNameCoreDecl,
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
	RdrNameDeprecation,
	RdrNameHsRecordBinds,
	RdrNameFixitySig,

	RdrBinding(..),
	RdrMatch(..),
	SigConverter,

	extractHsTyRdrNames,  extractHsTyRdrTyVars, 
	extractHsCtxtRdrTyVars, extractGenericPatTyVars,
 
	mkHsOpApp, mkClassDecl, mkClassOpSigDM, 
	mkHsNegApp, mkNPlusKPat, mkHsIntegral, mkHsFractional,
	mkHsDo, mkHsSplice,

	cvBinds,
	cvMonoBindsAndSigs,
	cvTopDecls,
	cvValSig, cvClassOpSig, cvInstDeclSig,
        mkTyData
    ) where

#include "HsVersions.h"

import HsSyn		-- Lots of it
import OccName		( mkDefaultMethodOcc, mkVarOcc )
import RdrName		( RdrName, isRdrTyVar, mkRdrUnqual, rdrNameOcc, isRdrTyVar )
import List		( nub )
import BasicTypes	( RecFlag(..), FixitySig )
import Class            ( DefMeth (..) )
\end{code}

 
%************************************************************************
%*									*
\subsection{Type synonyms}
%*									*
%************************************************************************

\begin{code}
type RdrNameArithSeqInfo	= ArithSeqInfo		RdrName
type RdrNameBangType		= BangType		RdrName
type RdrNameClassOpSig		= Sig			RdrName
type RdrNameConDecl		= ConDecl		RdrName
type RdrNameConDetails		= HsConDetails		RdrName RdrNameBangType
type RdrNameContext		= HsContext 		RdrName
type RdrNameHsDecl		= HsDecl		RdrName
type RdrNameDefaultDecl		= DefaultDecl		RdrName
type RdrNameForeignDecl		= ForeignDecl		RdrName
type RdrNameCoreDecl		= CoreDecl		RdrName
type RdrNameGRHS		= GRHS			RdrName
type RdrNameGRHSs		= GRHSs			RdrName
type RdrNameHsBinds		= HsBinds		RdrName
type RdrNameHsExpr		= HsExpr		RdrName
type RdrNameHsModule		= HsModule		RdrName
type RdrNameIE			= IE			RdrName
type RdrNameImportDecl 		= ImportDecl		RdrName
type RdrNameInstDecl		= InstDecl		RdrName
type RdrNameMatch		= Match			RdrName
type RdrNameMonoBinds		= MonoBinds		RdrName
type RdrNamePat			= InPat			RdrName
type RdrNameHsType		= HsType		RdrName
type RdrNameHsTyVar		= HsTyVarBndr		RdrName
type RdrNameSig			= Sig			RdrName
type RdrNameStmt		= Stmt			RdrName
type RdrNameTyClDecl		= TyClDecl		RdrName

type RdrNameRuleBndr            = RuleBndr              RdrName
type RdrNameRuleDecl            = RuleDecl              RdrName
type RdrNameDeprecation         = DeprecDecl            RdrName
type RdrNameFixitySig		= FixitySig		RdrName

type RdrNameHsRecordBinds	= HsRecordBinds		RdrName
\end{code}


%************************************************************************
%*									*
\subsection{A few functions over HsSyn at RdrName}
%*                                                                    *
%************************************************************************

@extractHsTyRdrNames@ finds the free variables of a HsType
It's used when making the for-alls explicit.

\begin{code}
extractHsTyRdrNames :: RdrNameHsType -> [RdrName]
extractHsTyRdrNames ty = nub (extract_ty ty [])

extractHsTyRdrTyVars :: RdrNameHsType -> [RdrName]
extractHsTyRdrTyVars ty = nub (filter isRdrTyVar (extract_ty ty []))

extractHsCtxtRdrNames :: HsContext RdrName -> [RdrName]
extractHsCtxtRdrNames ty = nub (extract_ctxt ty [])
extractHsCtxtRdrTyVars :: HsContext RdrName -> [RdrName]
extractHsCtxtRdrTyVars ty = filter isRdrTyVar (extractHsCtxtRdrNames ty)

extract_ctxt ctxt acc = foldr extract_pred acc ctxt

extract_pred (HsClassP cls tys) acc	= foldr extract_ty (cls : acc) tys
extract_pred (HsIParam n ty) acc	= extract_ty ty acc

extract_tys tys = foldr extract_ty [] tys

extract_ty (HsAppTy ty1 ty2)          acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (HsListTy ty)              acc = extract_ty ty acc
extract_ty (HsPArrTy ty)              acc = extract_ty ty acc
extract_ty (HsTupleTy _ tys)          acc = foldr extract_ty acc tys
extract_ty (HsFunTy ty1 ty2)          acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (HsPredTy p)		      acc = extract_pred p acc
extract_ty (HsTyVar tv)               acc = tv : acc
extract_ty (HsForAllTy Nothing cx ty) acc = extract_ctxt cx (extract_ty ty acc)
extract_ty (HsOpTy ty1 nam ty2)       acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (HsParTy ty)               acc = extract_ty ty acc
-- Generics
extract_ty (HsNumTy num)              acc = acc
extract_ty (HsKindSig ty k)	      acc = extract_ty ty acc
extract_ty (HsForAllTy (Just tvs) ctxt ty) 
                                acc = acc ++
                                      (filter (`notElem` locals) $
				       extract_ctxt ctxt (extract_ty ty []))
				    where
				      locals = hsTyVarNames tvs

extractGenericPatTyVars :: RdrNameMonoBinds -> [RdrName]
-- Get the type variables out of the type patterns in a bunch of
-- possibly-generic bindings in a class declaration
extractGenericPatTyVars binds
  = filter isRdrTyVar (nub (get binds []))
  where
    get (AndMonoBinds b1 b2)   acc = get b1 (get b2 acc)
    get (FunMonoBind _ _ ms _) acc = foldr get_m acc ms
    get other		       acc = acc

    get_m (Match (TypePat ty : _) _ _) acc = extract_ty ty acc
    get_m other			       acc = acc
\end{code}


%************************************************************************
%*									*
\subsection{Construction functions for Rdr stuff}
%*                                                                    *
%************************************************************************

mkClassDecl builds a RdrClassDecl, filling in the names for tycon and datacon
by deriving them from the name of the class.  We fill in the names for the
tycon and datacon corresponding to the class, by deriving them from the
name of the class itself.  This saves recording the names in the interface
file (which would be equally good).

Similarly for mkConDecl, mkClassOpSig and default-method names.

	*** See "THE NAMING STORY" in HsDecls ****
  
\begin{code}
mkClassDecl (cxt, cname, tyvars) fds sigs mbinds loc
  = ClassDecl { tcdCtxt = cxt, tcdName = cname, tcdTyVars = tyvars,
		tcdFDs = fds,  tcdSigs = sigs,  tcdMeths = mbinds,
		tcdLoc = loc }

mkTyData new_or_data (context, tname, tyvars) data_cons maybe src
  = TyData { tcdND = new_or_data, tcdCtxt = context, tcdName = tname,
	     tcdTyVars = tyvars,  tcdCons = data_cons, 
	     tcdDerivs = maybe,   tcdLoc = src, tcdGeneric = Nothing }

mkClassOpSigDM op ty loc
  = ClassOpSig op (DefMeth dm_rn) ty loc
  where
    dm_rn = mkRdrUnqual (mkDefaultMethodOcc (rdrNameOcc op))
\end{code}

\begin{code}
mkHsNegApp :: RdrNameHsExpr -> RdrNameHsExpr
-- If the type checker sees (negate 3#) it will barf, because negate
-- can't take an unboxed arg.  But that is exactly what it will see when
-- we write "-3#".  So we have to do the negation right now!

mkHsNegApp (HsLit (HsIntPrim i))    = HsLit (HsIntPrim (-i))    
mkHsNegApp (HsLit (HsFloatPrim i))  = HsLit (HsFloatPrim (-i))  
mkHsNegApp (HsLit (HsDoublePrim i)) = HsLit (HsDoublePrim (-i)) 
mkHsNegApp expr	    		    = NegApp expr     placeHolderName
\end{code}

A useful function for building @OpApps@.  The operator is always a
variable, and we don't know the fixity yet.

\begin{code}
mkHsOpApp e1 op e2 = OpApp e1 (HsVar op) (error "mkOpApp:fixity") e2
\end{code}

These are the bits of syntax that contain rebindable names
See RnEnv.lookupSyntaxName

\begin{code}
mkHsIntegral   i      = HsIntegral   i  placeHolderName
mkHsFractional f      = HsFractional f  placeHolderName
mkNPlusKPat n k       = NPlusKPatIn n k placeHolderName
mkHsDo ctxt stmts loc = HsDo ctxt stmts [] placeHolderType loc
\end{code}

\begin{code}
mkHsSplice e = HsSplice unqualSplice e

unqualSplice = mkRdrUnqual (mkVarOcc FSLIT("splice"))
		-- A name (uniquified later) to
		-- identify the splice
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

cvClassOpSig (Sig var poly_ty src_loc) = mkClassOpSigDM var poly_ty src_loc
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
