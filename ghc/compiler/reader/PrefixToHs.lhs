%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrefixToHS]{Support routines for converting ``prefix form'' to Haskell abstract syntax}

Support routines for reading prefix-form from the Lex/Yacc parser.

\begin{code}
#include "HsVersions.h"

module PrefixToHs (
	cvBinds,
	cvClassOpSig,
	cvInstDeclSig,
	cvMatches,
	cvMonoBinds,
	cvSepdBinds,
	cvValSig,
	sepDeclsForInterface,
	sepDeclsForTopBinds,
	sepDeclsIntoSigsAndBinds
    ) where

import Ubiq{-uitous-}

import PrefixSyn	-- and various syntaxen.
import HsSyn
import RdrHsSyn
import HsPragmas	( noGenPragmas, noClassOpPragmas )

import ProtoName	( ProtoName(..) )
import SrcLoc		( mkSrcLoc2 )
import Util		( panic, assertPanic )
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

cvValSig (RdrTySig vars poly_ty pragmas src_loc)
  = [ Sig v poly_ty (cvt_pragmas pragmas) src_loc | v <- vars ]
  where
    cvt_pragmas RdrNoPragma	   = noGenPragmas
    cvt_pragmas (RdrGenPragmas ps) = ps

cvClassOpSig (RdrTySig vars poly_ty pragmas src_loc)
  = [ ClassOpSig v poly_ty (cvt_pragmas pragmas) src_loc | v <- vars ]
  where
    cvt_pragmas RdrNoPragma	       = noClassOpPragmas
    cvt_pragmas (RdrClassOpPragmas ps) = ps

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
cvBinds :: SrcFile -> SigConverter -> RdrBinding -> ProtoNameHsBinds
cvBinds sf sig_cvtr raw_binding
  = cvSepdBinds sf sig_cvtr (sepDeclsForBinds raw_binding)

cvSepdBinds :: SrcFile -> SigConverter -> [RdrBinding] -> ProtoNameHsBinds
cvSepdBinds sf sig_cvtr bindings
  = case (mkMonoBindsAndSigs sf sig_cvtr bindings) of { (mbs, sigs) ->
    if (null sigs)
    then SingleBind (RecBind mbs)
    else BindWith   (RecBind mbs) sigs
    }

cvMonoBinds :: SrcFile -> [RdrBinding] -> ProtoNameMonoBinds
cvMonoBinds sf bindings
  = case (mkMonoBindsAndSigs sf bottom bindings) of { (mbs,sigs) ->
    if (null sigs)
    then mbs
    else panic "cvMonoBinds: some sigs present"
    }
  where
    bottom = panic "cvMonoBinds: sig converter!"
\end{code}

\begin{code}
mkMonoBindsAndSigs :: SrcFile
		   -> SigConverter
		   -> [RdrBinding]
		   -> (ProtoNameMonoBinds, [ProtoNameSig])

mkMonoBindsAndSigs sf sig_cvtr fbs
  = foldl mangle_bind (EmptyMonoBinds, []) fbs
  where
    -- If the function being bound has at least one argument, then the
    -- guarded right hand sides of each pattern binding are knitted
    -- into a series of patterns, each matched with its corresponding
    -- guarded right hand side (which may contain several
    -- alternatives). This series is then paired with the name of the
    -- function. Otherwise there is only one pattern, which is paired
    -- with a guarded right hand side.

    mangle_bind (b_acc, s_acc) sig@(RdrTySig _ _ _ _)
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
	    src_loc = mkSrcLoc2 sf good_srcline
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
      = case (cvFunMonoBind sf patbindings) of { (var, matches) ->
	(b_acc `AndMonoBinds`
	 FunMonoBind var matches (mkSrcLoc2 sf srcline), s_acc)
	}
\end{code}

\begin{code}
cvPatMonoBind :: SrcFile -> RdrMatch -> (ProtoNamePat, [ProtoNameGRHS], ProtoNameHsBinds)

cvPatMonoBind sf (RdrMatch_NoGuard srcline srcfun pat expr binding)
  = (pat, [OtherwiseGRHS expr (mkSrcLoc2 sf srcline)], cvBinds sf cvValSig binding)

cvPatMonoBind sf (RdrMatch_Guards srcline srcfun pat guardedexprs binding)
  = (pat, map (cvGRHS sf srcline) guardedexprs, cvBinds sf cvValSig binding)

cvFunMonoBind :: SrcFile -> [RdrMatch] -> (ProtoName {-VarName-}, [ProtoNameMatch])

cvFunMonoBind sf matches
  = (srcfun {- cheating ... -}, cvMatches sf False matches)
  where
    srcfun = case (head matches) of
	       RdrMatch_NoGuard _ sfun _ _ _ -> sfun
	       RdrMatch_Guards  _ sfun _ _ _ -> sfun

cvMatches :: SrcFile -> Bool -> [RdrMatch] -> [ProtoNameMatch]
cvMatch	  :: SrcFile -> Bool -> RdrMatch   -> ProtoNameMatch

cvMatches sf is_case matches = map (cvMatch sf is_case) matches

cvMatch sf is_case rdr_match
  = foldr PatMatch
	  (GRHSMatch (GRHSsAndBindsIn guarded_exprs (cvBinds sf cvValSig binding)))

	  -- For a FunMonoBinds, the first flattened "pattern" is
	  -- just the function name, and we don't want to keep it.
	  -- For a case expr, it's (presumably) a constructor name -- and
	  -- we most certainly want to keep it!	 Hence the monkey busines...

	  (if is_case then -- just one pattern: leave it untouched...
	      [pat']
	   else
	      case pat' of
		ConPatIn _ pats -> pats
	  )
  where
    (pat, binding, guarded_exprs)
      = case rdr_match of
	  RdrMatch_NoGuard ln b c expr    d -> (c,d, [OtherwiseGRHS expr (mkSrcLoc2 sf ln)])
	  RdrMatch_Guards  ln b c gd_exps d -> (c,d, map (cvGRHS sf ln) gd_exps)

    ---------------------
    pat' = doctor_pat pat

    -- a ConOpPatIn in the corner may be handled by converting it to
    -- ConPatIn...

    doctor_pat (ConOpPatIn p1 op p2) = ConPatIn op [p1, p2]
    doctor_pat other_pat	     = other_pat

cvGRHS :: SrcFile -> SrcLine -> (ProtoNameHsExpr, ProtoNameHsExpr) -> ProtoNameGRHS

cvGRHS sf sl (g, e) = GRHS g e (mkSrcLoc2 sf sl)
\end{code}

%************************************************************************
%*									*
\subsection[PrefixToHS-utils]{Utilities for conversion}
%*									*
%************************************************************************

Separate declarations into all the various kinds:
\begin{display}
tys		RdrTyDecl
ty "sigs" 	RdrSpecDataSig
classes		RdrClassDecl
insts		RdrInstDecl
inst "sigs" 	RdrSpecInstSig
defaults	RdrDefaultDecl
binds		RdrFunctionBinding RdrPatternBinding RdrTySig
		RdrSpecValSig RdrInlineValSig RdrDeforestSig
		RdrMagicUnfoldingSig
iimps		RdrIfaceImportDecl (interfaces only)
\end{display}

This function isn't called directly; some other function calls it,
then checks that what it got is appropriate for that situation.
(Those functions follow...)

\begin{code}
sepDecls (RdrTyDecl a)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
 = (a:tys,tysigs,classes,insts,instsigs,defaults,binds,iimps,ifixs)

sepDecls a@(RdrFunctionBinding _ _)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
 = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps,ifixs)

sepDecls a@(RdrPatternBinding _ _)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
 = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps,ifixs)

-- RdrAndBindings catered for below...

sepDecls (RdrClassDecl a)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,tysigs,a:classes,insts,instsigs,defaults,binds,iimps,ifixs)

sepDecls (RdrInstDecl a)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,tysigs,classes,a:insts,instsigs,defaults,binds,iimps,ifixs)

sepDecls (RdrDefaultDecl a)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,tysigs,classes,insts,instsigs,a:defaults,binds,iimps,ifixs)

sepDecls a@(RdrTySig _ _ _ _)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps,ifixs)

sepDecls (RdrIfaceImportDecl a)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,tysigs,classes,insts,instsigs,defaults,binds,a:iimps,ifixs)

sepDecls (RdrIfaceFixities a)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,tysigs,classes,insts,instsigs,defaults,binds,iimps,a++ifixs)

sepDecls a@(RdrSpecValSig _)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps,ifixs)

sepDecls a@(RdrInlineValSig _)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps,ifixs)

sepDecls a@(RdrDeforestSig _)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps,ifixs)

sepDecls a@(RdrMagicUnfoldingSig _)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps,ifixs)

sepDecls (RdrSpecInstSig a)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,tysigs,classes,insts,a:instsigs,defaults,binds,iimps,ifixs)

sepDecls (RdrSpecDataSig a)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,a:tysigs,classes,insts,instsigs,defaults,binds,iimps,ifixs)

sepDecls RdrNullBind
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = (tys,tysigs,classes,insts,instsigs,defaults,binds,iimps,ifixs)

sepDecls (RdrAndBindings bs1 bs2)
	 tys tysigs classes insts instsigs defaults binds iimps ifixs
  = case (sepDecls bs2 tys tysigs classes insts instsigs defaults binds iimps ifixs) of {
      (tys,tysigs,classes,insts,instsigs,defaults,binds,iimps,ifixs) ->
	  sepDecls bs1 tys tysigs classes insts instsigs defaults binds iimps ifixs
    }
\end{code}

\begin{code}
sepDeclsForTopBinds binding
  = case (sepDecls binding [] [] [] [] [] [] [] [] [])
	of { (tys,tysigs,classes,insts,instsigs,defaults,binds,iimps,ifixs) ->
    ASSERT ((null iimps)
	 && (null ifixs))
    (tys,tysigs,classes,insts,instsigs,defaults,binds)
    }

sepDeclsForBinds binding
  = case (sepDecls binding [] [] [] [] [] [] [] [] [])
	of { (tys,tysigs,classes,insts,instsigs,defaults,binds,iimps,ifixs) ->
    ASSERT ((null tys)
	 && (null tysigs)
	 && (null classes)
	 && (null insts)
	 && (null instsigs)
	 && (null defaults)
	 && (null iimps)
	 && (null ifixs))
    binds
    }

sepDeclsIntoSigsAndBinds binding
  = case (sepDeclsForBinds binding) of { sigs_and_binds ->
    foldr sep_stuff ([],[]) sigs_and_binds
    }
  where
    sep_stuff s@(RdrTySig _ _ _ _)       (sigs,defs) = (s:sigs,defs)
    sep_stuff s@(RdrSpecValSig _)        (sigs,defs) = (s:sigs,defs)
    sep_stuff s@(RdrInlineValSig _)      (sigs,defs) = (s:sigs,defs)
    sep_stuff s@(RdrDeforestSig  _)      (sigs,defs) = (s:sigs,defs)
    sep_stuff s@(RdrMagicUnfoldingSig _) (sigs,defs) = (s:sigs,defs)
    sep_stuff d@(RdrFunctionBinding _ _) (sigs,defs) = (sigs,d:defs)
    sep_stuff d@(RdrPatternBinding  _ _) (sigs,defs) = (sigs,d:defs)


sepDeclsForInterface binding
  = case (sepDecls binding [] [] [] [] [] [] [] [] [])
	of { (tys,tysigs,classes,insts,instsigs,defaults,sigs,iimps,ifixs) ->
    ASSERT ((null defaults)
	 && (null tysigs)
	 && (null instsigs))
    ASSERT (not (not_all_sigs sigs))
    (tys,classes,insts,sigs,iimps,ifixs)
    }
  where
    not_all_sigs sigs = not (all is_a_sig sigs)

    is_a_sig (RdrTySig _ _ _ _) = True
    is_a_sig anything_else      = False
\end{code}
