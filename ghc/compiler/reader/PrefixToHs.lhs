%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrefixToHS]{Support routines for converting ``prefix form'' to Haskell abstract syntax}

Support routines for reading prefix-form from the Lex/Yacc parser.

\begin{code}
module PrefixToHs (
	cvBinds,
	cvMonoBindsAndSigs,
	cvTopDecls,
	cvValSig, cvClassOpSig, cvInstDeclSig
    ) where

#include "HsVersions.h"

import PrefixSyn	-- and various syntaxen.
import HsSyn
import RdrHsSyn

import BasicTypes	( RecFlag(..) )
import SrcLoc		( mkSrcLoc )
import Util		( mapAndUnzip )
import Panic		( panic, assertPanic )
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

cvClassOpSig (Sig var poly_ty src_loc) = ClassOpSig var Nothing poly_ty src_loc
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
cvBinds :: SrcFile -> SigConverter -> RdrBinding -> RdrNameHsBinds
	-- The mysterious SigConverter converts Sigs to ClassOpSigs
	-- in class declarations.  Mostly it's just an identity function

cvBinds sf sig_cvtr binding
  = case (cvMonoBindsAndSigs sf sig_cvtr binding) of { (mbs, sigs) ->
    MonoBind mbs sigs Recursive
    }
\end{code}

\begin{code}
cvMonoBindsAndSigs :: SrcFile
		   -> SigConverter
		   -> RdrBinding
		   -> (RdrNameMonoBinds, [RdrNameSig])

cvMonoBindsAndSigs sf sig_cvtr fb
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
cvTopDecls :: SrcFile -> RdrBinding -> [RdrNameHsDecl]
cvTopDecls srcfile bind
  = let
	(top_decls, mono_binds, sigs) = go ([], EmptyMonoBinds, []) bind 
    in
    (ValD (MonoBind mono_binds sigs Recursive) : top_decls)
  where
    go acc		  RdrNullBind		 = acc
    go acc                (RdrAndBindings b1 b2) = go (go acc b1) b2
    go (topds, mbs, sigs) (RdrTyClDecl d)	 = (TyClD d : topds, mbs, sigs)
    go (topds, mbs, sigs) (RdrInstDecl d)	 = (InstD d : topds, mbs, sigs) 
    go (topds, mbs, sigs) (RdrDefaultDecl d)     = (DefD d  : topds, mbs, sigs)
    go (topds, mbs, sigs) (RdrForeignDecl d)     = (ForD d  : topds, mbs, sigs)
    go (topds, mbs, sigs) (RdrSig (FixSig d))    = (FixD d  : topds, mbs, sigs)
    go (topds, mbs, sigs) (RdrSig sig)		 = (topds, mbs, sig:sigs)
    go (topds, mbs, sigs) (RdrValBinding bind)   = (topds, mbs `AndMonoBinds` bind, sigs)
\end{code}
