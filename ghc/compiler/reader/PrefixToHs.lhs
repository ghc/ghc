%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[PrefixToHS]{Support routines for converting ``prefix form'' to Haskell abstract syntax}

Support routines for reading prefix-form from the Lex/Yacc parser.

\begin{code}
#include "HsVersions.h"

module PrefixToHs (
	cvBinds,
	cvClassOpSig,
	cvInstDeclSig,
	cvInstDecls,
	cvMatches,
	cvMonoBinds,
	cvSepdBinds,
	cvValSig,
	sepDeclsForInterface,
	sepDeclsForTopBinds,
	sepDeclsIntoSigsAndBinds
    ) where

IMPORT_Trace		-- ToDo: rm
import Pretty

import AbsSyn
import HsCore		-- ****** NEED TO SEE CONSTRUCTORS ******
import HsPragmas	-- ****** NEED TO SEE CONSTRUCTORS ******
import Outputable
import PrefixSyn
import ProtoName	-- ProtoName(..), etc.
import SrcLoc		( mkSrcLoc2 )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[cvDecls]{Convert various top-level declarations}
%*									*
%************************************************************************

\begin{code}
cvInstDecls :: Bool -> FAST_STRING -> FAST_STRING
	    -> [FAST_STRING -> FAST_STRING -> Bool -> ProtoNameInstDecl] -- incomplete InstDecls
	    -> [ProtoNameInstDecl]

cvInstDecls from_here orig_modname informant_modname decls
  = [ decl_almost orig_modname informant_modname from_here
    | decl_almost <- decls ]
\end{code}

We make a point not to throw any user-pragma ``sigs'' at
these conversion functions:
\begin{code}
cvValSig, cvClassOpSig, cvInstDeclSig :: SigConverter

cvValSig (RdrTySig vars poly_ty pragmas src_loc)
  = [ Sig v poly_ty (cvt_pragmas pragmas) src_loc | v <- vars ]
  where
    cvt_pragmas RdrNoPragma	   = NoGenPragmas
    cvt_pragmas (RdrGenPragmas ps) = ps

cvClassOpSig (RdrTySig vars poly_ty pragmas src_loc)
  = [ ClassOpSig v poly_ty (cvt_pragmas pragmas) src_loc | v <- vars ]
  where
    cvt_pragmas RdrNoPragma	       = NoClassOpPragmas
    cvt_pragmas (RdrClassOpPragmas ps) = ps

cvInstDeclSig (RdrSpecValSig        sigs) = sigs
cvInstDeclSig (RdrInlineValSig      sig)  = [ sig ]
cvInstDeclSig (RdrDeforestSig	    sig)  = [ sig ]
cvInstDeclSig (RdrMagicUnfoldingSig sig)  = [ sig ]
\end{code}

%************************************************************************
%*									*
\subsection[cvBinds-etc]{Converting to @Binds@, @MonoBinds@, etc.}
%*									*
%************************************************************************

Function definitions are restructured here. Each is assumed to be recursive
initially, and non recursive definitions are discovered by the dependency
analyser.

\begin{code}
cvBinds :: SrcFile -> SigConverter -> RdrBinding -> ProtoNameBinds
cvBinds sf sig_cvtr raw_binding
  = cvSepdBinds sf sig_cvtr (sepDeclsForBinds raw_binding)

cvSepdBinds :: SrcFile -> SigConverter -> [RdrBinding] -> ProtoNameBinds
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
		(RdrPatternBinding lousy_srcline [patbinding@(RdrMatch good_srcline _ _ _ _)])
      -- WDP: the parser has trouble getting a good line-number on RdrPatternBindings.
      = case (cvPatMonoBind sf patbinding) of { (pat, grhss, binds) ->
	let
	    src_loc = mkSrcLoc2 sf good_srcline
	in
	(b_acc `AndMonoBinds`
	 PatMonoBind pat (GRHSsAndBindsIn grhss binds) src_loc, s_acc)
	}

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
cvPatMonoBind :: SrcFile -> RdrMatch -> (ProtoNamePat, [ProtoNameGRHS], ProtoNameBinds)

cvPatMonoBind sf (RdrMatch srcline srcfun pat guardedexprs binding)
  = (pat, cvGRHSs srcfun sf srcline guardedexprs, cvBinds sf cvValSig binding)

cvFunMonoBind :: SrcFile -> [RdrMatch] -> (ProtoName {-VarName-}, [ProtoNameMatch])

cvFunMonoBind sf matches@((RdrMatch srcline srcfun pat guardedexprs binding):_)
  = ( Unk srcfun, -- cheating ...
      cvMatches sf False matches )

cvMatches :: SrcFile -> Bool -> [RdrMatch] -> [ProtoNameMatch]
cvMatch	  :: SrcFile -> Bool -> RdrMatch   -> ProtoNameMatch

cvMatches sf is_case matches = map (cvMatch sf is_case) matches

cvMatch sf is_case (RdrMatch srcline srcfun pat guardedexprs binding)
  = foldr PatMatch
	  (GRHSMatch (GRHSsAndBindsIn (cvGRHSs srcfun sf srcline guardedexprs)
				      (cvBinds sf cvValSig binding)))

	  -- For a FunMonoBinds, the first flattened "pattern" is
	  -- just the function name, and we don't want to keep it.
	  -- For a case expr, it's (presumably) a constructor name -- and
	  -- we most certainly want to keep it!	 Hence the monkey busines...

--	  (trace ("cvMatch:"++(ppShow 80 (ppr PprDebug pat))) (
	  (if is_case then -- just one pattern: leave it untouched...
	      [pat']
	   else
	      case pat' of
		ConPatIn _ pats -> pats
	  )
--	  ))
  where
    pat' = doctor_pat pat

    -- a ConOpPatIn in the corner may be handled by converting it to
    -- ConPatIn...

    doctor_pat (ConOpPatIn p1 op p2) = ConPatIn op [p1, p2]
    doctor_pat other_pat	     = other_pat

cvGRHSs :: FAST_STRING -> SrcFile -> SrcLine -> [(ProtoNameExpr, ProtoNameExpr)] -> [ProtoNameGRHS]

cvGRHSs sfun sf sl guarded_exprs = map (cvGRHS sfun sf sl) guarded_exprs

cvGRHS :: FAST_STRING -> SrcFile -> SrcLine -> (ProtoNameExpr, ProtoNameExpr) -> ProtoNameGRHS

cvGRHS sfun sf sl (Var v@(Unk str), e)
	| str == SLIT("__o") -- "__otherwise" ToDo: de-urgh-ify
  = OtherwiseGRHS e (mkSrcLoc2 sf sl)

cvGRHS sfun sf sl (g, e)
  = GRHS g e (mkSrcLoc2 sf sl)
\end{code}

%************************************************************************
%*									*
\subsection[PrefixToHS-utils]{Utilities for conversion}
%*									*
%************************************************************************

Separate declarations into all the various kinds:
\begin{display}
tys		RdrTyData RdrTySynonym
type "sigs" 	RdrAbstractTypeSig RdrSpecDataSig
classes		RdrClassDecl
instances	RdrInstDecl
instance "sigs" RdrSpecInstSig
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
sepDecls (RdrTyData a)
	 tys tysigs classes insts instsigs defaults binds iimps
 = (a:tys,tysigs,classes,insts,instsigs,defaults,binds,iimps)

sepDecls (RdrTySynonym a)
	 tys tysigs classes insts instsigs defaults binds iimps
 = (a:tys,tysigs,classes,insts,instsigs,defaults,binds,iimps)

sepDecls a@(RdrFunctionBinding _ _)
	 tys tysigs classes insts instsigs defaults binds iimps
 = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps)

sepDecls a@(RdrPatternBinding _ _)
	 tys tysigs classes insts instsigs defaults binds iimps
 = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps)

-- RdrAndBindings catered for below...

sepDecls (RdrClassDecl a)
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,tysigs,a:classes,insts,instsigs,defaults,binds,iimps)

sepDecls (RdrInstDecl a)
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,tysigs,classes,a:insts,instsigs,defaults,binds,iimps)

sepDecls (RdrDefaultDecl a)
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,tysigs,classes,insts,instsigs,a:defaults,binds,iimps)

sepDecls a@(RdrTySig _ _ _ _)
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps)

sepDecls (RdrIfaceImportDecl a)
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,tysigs,classes,insts,instsigs,defaults,binds,a:iimps)

sepDecls a@(RdrSpecValSig _)
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps)

sepDecls a@(RdrInlineValSig _)
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps)

sepDecls a@(RdrDeforestSig _)
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps)

sepDecls a@(RdrMagicUnfoldingSig _)
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,tysigs,classes,insts,instsigs,defaults,a:binds,iimps)

sepDecls (RdrSpecInstSig a)
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,tysigs,classes,insts,a:instsigs,defaults,binds,iimps)

sepDecls (RdrAbstractTypeSig a)
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,a:tysigs,classes,insts,instsigs,defaults,binds,iimps)

sepDecls (RdrSpecDataSig a)
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,a:tysigs,classes,insts,instsigs,defaults,binds,iimps)

sepDecls RdrNullBind
	 tys tysigs classes insts instsigs defaults binds iimps
  = (tys,tysigs,classes,insts,instsigs,defaults,binds,iimps)

sepDecls (RdrAndBindings bs1 bs2)
	 tys tysigs classes insts instsigs defaults binds iimps
  = case (sepDecls bs2 tys tysigs classes insts instsigs defaults binds iimps) of {
      (tys,tysigs,classes,insts,instsigs,defaults,binds,iimps) ->
	  sepDecls bs1 tys tysigs classes insts instsigs defaults binds iimps
    }
\end{code}

\begin{code}
sepDeclsForTopBinds binding
  = case (sepDecls binding [] [] [] [] [] [] [] [])
	of { (tys,tysigs,classes,insts,instsigs,defaults,binds,iimps) ->
    ASSERT (null iimps)
    (tys,tysigs,classes,insts,instsigs,defaults,binds)
    }

sepDeclsForBinds binding
  = case (sepDecls binding [] [] [] [] [] [] [] [])
	of { (tys,tysigs,classes,insts,instsigs,defaults,binds,iimps) ->
    ASSERT ((null tys)
	 && (null tysigs)
	 && (null classes)
	 && (null insts)
	 && (null instsigs)
	 && (null defaults)
	 && (null iimps))
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
  = case (sepDecls binding [] [] [] [] [] [] [] [])
	of { (tys,tysigs,classes,insts,instsigs,defaults,sigs,iimps) ->
    ASSERT ((null defaults)
	 && (null tysigs)
	 && (null instsigs))
    ASSERT (not (not_all_sigs sigs))
    (tys,classes,insts,sigs,iimps)
    }
  where
    not_all_sigs sigs = not (all is_a_sig sigs)

    is_a_sig (RdrTySig _ _ _ _) = True
    is_a_sig anything_else      = False
\end{code}
