%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrefixToHS]{Support routines for converting ``prefix form'' to Haskell abstract syntax}

Support routines for reading prefix-form from the Lex/Yacc parser.

\begin{code}
#include "HsVersions.h"

module PrefixToHs (
	cvValSig,
	cvClassOpSig,
	cvInstDeclSig,

	cvBinds,
	cvMonoBindsAndSigs,
	cvMatches,
	cvOtherDecls
    ) where

IMP_Ubiq(){-uitous-}

import PrefixSyn	-- and various syntaxen.
import HsSyn
import RdrHsSyn
import HsPragmas	( noGenPragmas, noClassOpPragmas )

import SrcLoc		( mkSrcLoc )
import Util		( mapAndUnzip, panic, assertPanic )
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

cvValSig (RdrTySig vars poly_ty src_loc)
  = [ Sig v poly_ty src_loc | v <- vars ]

cvClassOpSig (RdrTySig vars poly_ty src_loc)
  = [ ClassOpSig v poly_ty noClassOpPragmas src_loc | v <- vars ]

cvInstDeclSig (RdrSpecValSig        sigs) = sigs
cvInstDeclSig (RdrInlineValSig      sig)  = [ sig ]
cvInstDeclSig (RdrDeforestSig	    sig)  = [ sig ]
cvInstDeclSig (RdrMagicUnfoldingSig sig)  = [ sig ]
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
cvBinds sf sig_cvtr binding
  = case (cvMonoBindsAndSigs sf sig_cvtr binding) of { (mbs, sigs) ->
    if (null sigs)
    then SingleBind (RecBind mbs)
    else BindWith   (RecBind mbs) sigs
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
    -- If the function being bound has at least one argument, then the
    -- guarded right hand sides of each pattern binding are knitted
    -- into a series of patterns, each matched with its corresponding
    -- guarded right hand side (which may contain several
    -- alternatives). This series is then paired with the name of the
    -- function. Otherwise there is only one pattern, which is paired
    -- with a guarded right hand side.

    mangle_bind acc (RdrAndBindings fb1 fb2)
      = mangle_bind (mangle_bind acc fb1) fb2

    mangle_bind (b_acc, s_acc) sig@(RdrTySig _ _ _)
      = (b_acc, s_acc ++ sig_cvtr sig)

    mangle_bind (b_acc, s_acc) (RdrSpecValSig	     sig) = (b_acc, sig ++ s_acc)
    mangle_bind (b_acc, s_acc) (RdrInlineValSig      sig) = (b_acc, sig : s_acc)
    mangle_bind (b_acc, s_acc) (RdrDeforestSig       sig) = (b_acc, sig : s_acc)
    mangle_bind (b_acc, s_acc) (RdrMagicUnfoldingSig sig) = (b_acc, sig : s_acc)

    mangle_bind (b_acc, s_acc)
		(RdrPatternBinding lousy_srcline [patbinding])
      -- WDP: the parser has trouble getting a good line-number on RdrPatternBindings.
      = case (cvPatMonoBind sf patbinding) of { (pat, grhss, binds) ->
	let
	    src_loc = mkSrcLoc sf good_srcline
	in
	(b_acc `AndMonoBinds`
	 PatMonoBind pat (GRHSsAndBindsIn grhss binds) src_loc, s_acc)
	}
      where
	good_srcline = case patbinding of
			 RdrMatch_NoGuard ln _ _ _ _ -> ln
			 RdrMatch_Guards  ln _ _ _ _ -> ln


    mangle_bind _ (RdrPatternBinding _ _)
      = panic "mangleBinding: more than one pattern on a RdrPatternBinding"

    mangle_bind (b_acc, s_acc) (RdrFunctionBinding srcline patbindings)
	    -- must be a function binding...
      = case (cvFunMonoBind sf patbindings) of { (var, inf, matches) ->
	(b_acc `AndMonoBinds`
	 FunMonoBind var inf matches (mkSrcLoc sf srcline), s_acc)
	}

    mangle_bind (b_acc, s_acc) other = (b_acc, s_acc)
\end{code}

\begin{code}
cvPatMonoBind :: SrcFile -> RdrMatch -> (RdrNamePat, [RdrNameGRHS], RdrNameHsBinds)

cvPatMonoBind sf (RdrMatch_NoGuard srcline srcfun pat expr binding)
  = (pat, [OtherwiseGRHS expr (mkSrcLoc sf srcline)], cvBinds sf cvValSig binding)

cvPatMonoBind sf (RdrMatch_Guards srcline srcfun pat guardedexprs binding)
  = (pat, map (cvGRHS sf srcline) guardedexprs, cvBinds sf cvValSig binding)

cvFunMonoBind :: SrcFile -> [RdrMatch] -> (RdrName {-VarName-}, Bool {-InfixDefn-}, [RdrNameMatch])

cvFunMonoBind sf matches
  = (head srcfuns, head infixdefs, cvMatches sf False matches)
  where
    (srcfuns, infixdefs) = mapAndUnzip get_mdef matches
    -- ToDo: Check for consistent srcfun and infixdef

    get_mdef (RdrMatch_NoGuard _ sfun pat _ _) = get_pdef pat
    get_mdef (RdrMatch_Guards  _ sfun pat _ _) = get_pdef pat

    get_pdef (ConPatIn fn _)       = (fn, False)
    get_pdef (ConOpPatIn _ op _ _) = (op, True)
    get_pdef (ParPatIn pat)	   = get_pdef pat


cvMatches :: SrcFile -> Bool -> [RdrMatch] -> [RdrNameMatch]
cvMatch	  :: SrcFile -> Bool -> RdrMatch   -> RdrNameMatch

cvMatches sf is_case matches = map (cvMatch sf is_case) matches

cvMatch sf is_case rdr_match
  = foldr PatMatch
	  (GRHSMatch (GRHSsAndBindsIn guarded_exprs (cvBinds sf cvValSig binding)))

	  -- For a FunMonoBinds, the first flattened "pattern" is
	  -- just the function name, and we don't want to keep it.
	  -- For a case expr, it's (presumably) a constructor name -- and
	  -- we most certainly want to keep it!	 Hence the monkey busines...

	  (if is_case then -- just one pattern: leave it untouched...
	      [pat]
	   else		   -- function pattern; extract arg patterns...
	      case pat of ConPatIn fn pats      -> pats
			  ConOpPatIn p1 op _ p2 -> [p1,p2]
			  ParPatIn pat	        -> panic "PrefixToHs.cvMatch:ParPatIn"
	  )
  where
    (pat, binding, guarded_exprs)
      = case rdr_match of
	  RdrMatch_NoGuard ln b c expr    d -> (c,d, [OtherwiseGRHS expr (mkSrcLoc sf ln)])
	  RdrMatch_Guards  ln b c gd_exps d -> (c,d, map (cvGRHS sf ln) gd_exps)

cvGRHS :: SrcFile -> SrcLine -> (RdrNameHsExpr, RdrNameHsExpr) -> RdrNameGRHS
cvGRHS sf sl (g, e) = GRHS g e (mkSrcLoc sf sl)
\end{code}

%************************************************************************
%*									*
\subsection[PrefixToHS-utils]{Utilities for conversion}
%*									*
%************************************************************************

Separate declarations into all the various kinds:

\begin{code}
cvOtherDecls :: RdrBinding -> [RdrNameHsDecl]
cvOtherDecls b 
  = go [] b
  where
    go acc (RdrAndBindings b1 b2) = go (go acc b1) b2
    go acc (RdrTyDecl d)	  = TyD d   : acc
    go acc (RdrClassDecl d)	  = ClD d   : acc
    go acc (RdrInstDecl d)	  = InstD d : acc 
    go acc (RdrDefaultDecl d)     = DefD d  : acc
    go acc other		  = acc
\end{code}
