%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[HsBinds]{Abstract syntax: top-level bindings and signatures}

Datatype for: @Binds@, @Bind@, @Sig@, @MonoBinds@.

\begin{code}
#include "HsVersions.h"

module HsBinds where

import AbsUniType	( pprUniType, TyVar, UniType
			  IF_ATTACK_PRAGMAS(COMMA cmpTyVar)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import HsExpr		( Expr )
import HsMatches	( pprMatches, pprGRHSsAndBinds, Match, GRHSsAndBinds )
import HsPat		( ProtoNamePat(..), RenamedPat(..),
			  TypecheckedPat, InPat
			  IF_ATTACK_PRAGMAS(COMMA typeOfPat)
			)
import HsPragmas	( GenPragmas, ClassOpPragmas )
import HsTypes		( PolyType )
import Id		( Id, DictVar(..) )
import IdInfo		( UnfoldingGuidance )
import Inst		( Inst )
import Name		( Name )
import Outputable
import Pretty
import ProtoName	( ProtoName(..) ) -- .. for pragmas only
import SrcLoc		( SrcLoc )
import Unique		( Unique )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[AbsSyn-Binds]{Bindings: @Binds@}
%*									*
%************************************************************************

The following syntax may produce new syntax which is not part of the input,
and which is instead a translation of the input to the typechecker.
Syntax translations are marked TRANSLATION in comments. New empty
productions are useful in development but may not appear in the final
grammar.

Collections of bindings, created by dependency analysis and translation:

\begin{code}
data Binds bdee pat		-- binders and bindees
  = EmptyBinds

  | ThenBinds	(Binds bdee pat)
		(Binds bdee pat)

  | SingleBind	(Bind  bdee pat)

  | BindWith		-- Bind with a type signature.
			-- These appear only on typechecker input
			-- (PolyType [in Sigs] can't appear on output)
		(Bind bdee pat)		-- really ProtoNameBind, but...
					-- (see "really" comment below)
		[Sig bdee]

  | AbsBinds			-- Binds abstraction; TRANSLATION
		[TyVar]
		[DictVar]
		[(Id, Id)]		-- (old, new) pairs
		[(Inst, Expr bdee pat)]	-- local dictionaries
		(Bind bdee pat)		-- "the business end"

	-- Creates bindings for *new* (polymorphic, overloaded) locals
	-- in terms of *old* (monomorphic, non-overloaded) ones.
	--
	-- See section 9 of static semantics paper for more details.
	-- (You can get a PhD for explaining the True Meaning
	--  of this last construct.)
\end{code}

The corresponding unparameterised synonyms:

\begin{code}
type ProtoNameBinds	= Binds ProtoName ProtoNamePat
type RenamedBinds       = Binds Name 	  RenamedPat
type TypecheckedBinds	= Binds Id        TypecheckedPat
\end{code}

\begin{code}
nullBinds :: Binds bdee pat -> Bool
nullBinds EmptyBinds		= True
nullBinds (ThenBinds b1 b2)	= (nullBinds b1) && (nullBinds b2)
nullBinds (SingleBind b)	= nullBind b
nullBinds (BindWith b _)	= nullBind b
nullBinds (AbsBinds _ _ _ ds b)	= (null ds) && (nullBind b)
\end{code}

ToDo: make this recursiveness checking also require that
there be something there, i.e., not null ?
\begin{code}
{- UNUSED:
bindsAreRecursive :: TypecheckedBinds -> Bool

bindsAreRecursive EmptyBinds		= False
bindsAreRecursive (ThenBinds b1 b2)
  = (bindsAreRecursive b1) || (bindsAreRecursive b2)
bindsAreRecursive (SingleBind b)	= bindIsRecursive b
bindsAreRecursive (BindWith b _)	= bindIsRecursive b
bindsAreRecursive (AbsBinds _ _ _ ds b)
  = (bindsAreRecursive d) || (bindIsRecursive b)
-}
\end{code}

\begin{code}
instance (NamedThing bdee, Outputable bdee,
            NamedThing pat, Outputable pat) =>
		Outputable (Binds bdee pat) where

    ppr sty EmptyBinds = ppNil
    ppr sty (ThenBinds binds1 binds2)
     = ppAbove (ppr sty binds1) (ppr sty binds2)
    ppr sty (SingleBind bind) = ppr sty bind
    ppr sty (BindWith bind sigs)
     = ppAbove (if null sigs then ppNil else ppr sty sigs) (ppr sty bind)
    ppr sty (AbsBinds tyvars dictvars local_pairs dict_binds val_binds)
     = ppAbove (ppSep [ppPStr SLIT("AbsBinds"),
		      ppBesides[ppLbrack, interpp'SP sty tyvars, ppRbrack],
		      ppBesides[ppLbrack, interpp'SP sty dictvars, ppRbrack],
		      ppBesides[ppLbrack, interpp'SP sty local_pairs, ppRbrack]])
	    (ppNest 4 (ppAbove (ppAboves (map (ppr sty) dict_binds)) (ppr sty val_binds)))
\end{code}

%************************************************************************
%*									*
\subsection[AbsSyn-Sig]{@Sig@: type signatures and value-modifying user pragmas}
%*									*
%************************************************************************

It is convenient to lump ``value-modifying'' user-pragmas (e.g.,
``specialise this function to these four types...'') in with type
signatures.  Then all the machinery to move them into place, etc.,
serves for both.

\begin{code}
data Sig name
  = Sig		name		-- a bog-std type signature
		(PolyType name)
		(GenPragmas name) -- only interface ones have pragmas
		SrcLoc

  | ClassOpSig	name		-- class-op sigs have different pragmas
		(PolyType name)
		(ClassOpPragmas name)	-- only interface ones have pragmas
		SrcLoc

  | SpecSig 	name		-- specialise a function or datatype ...
		(PolyType name) -- ... to these types
		(Maybe name)	-- ... maybe using this as the code for it
		SrcLoc

  | InlineSig	name		  -- INLINE f [howto]
		UnfoldingGuidance -- "howto": how gung-ho we are about inlining
		SrcLoc

  -- ToDo: strictly speaking, could omit based on -DOMIT_DEFORESTER
  | DeforestSig name            -- Deforest using this function definition
              	SrcLoc
 
  | MagicUnfoldingSig
		name		-- Associate the "name"d function with
		FAST_STRING	-- the compiler-builtin unfolding (known
		SrcLoc		-- by the String name)
		      
type ProtoNameSig  = Sig ProtoName
type RenamedSig    = Sig Name

type ProtoNameClassOpSig  = Sig ProtoName
type RenamedClassOpSig    = Sig Name
\end{code}

\begin{code}
instance (Outputable name) => Outputable (Sig name) where
    ppr sty (Sig var ty pragmas _)
      = ppHang (ppCat [ppr sty var, ppPStr SLIT("::")])
	     4 (ppAbove (ppr sty ty)
			(ifnotPprForUser sty (ppr sty pragmas)))

    ppr sty (ClassOpSig var ty pragmas _)
      = ppHang (ppCat [ppr sty var, ppPStr SLIT("::")])
	     4 (ppAbove (ppr sty ty)
			(ifnotPprForUser sty (ppr sty pragmas)))

    ppr sty (DeforestSig var _)
      = ppHang (ppCat [ppStr "{-# DEFOREST", ppr sty var])
                   4 (ppStr "#-}")

    ppr sty (SpecSig var ty using _)
      = ppHang (ppCat [ppPStr SLIT("{-# SPECIALIZE"), ppr sty var, ppPStr SLIT("::")])
	     4 (ppCat [ppr sty ty, pp_using using, ppPStr SLIT("#-}")])
      where
	pp_using Nothing   = ppNil
	pp_using (Just me) = ppCat [ppChar '=', ppr sty me]

    ppr sty (InlineSig var _ _)
      = ppHang (ppCat [ppPStr SLIT("{-# INLINE"), ppr sty var])
	     4 (ppCat [ppPStr SLIT("<enthusiasm not done yet>"), ppPStr SLIT("#-}")])

    ppr sty (MagicUnfoldingSig var str _)
      = ppCat [ppPStr SLIT("{-# MAGIC_UNFOLDING"), ppr sty var, ppPStr str, ppPStr SLIT("#-}")]
\end{code}

%************************************************************************
%*									*
\subsection[AbsSyn-Bind]{Binding: @Bind@}
%*									*
%************************************************************************

\begin{code}
data Bind bdee pat		-- binders and bindees
  = EmptyBind	-- because it's convenient when parsing signatures
  | NonRecBind	(MonoBinds bdee pat)
  | RecBind	(MonoBinds bdee pat)
\end{code}

The corresponding unparameterised synonyms:

\begin{code}
type ProtoNameBind		= Bind ProtoName ProtoNamePat
type RenamedBind        = Bind Name RenamedPat
type TypecheckedBind	= Bind Id	   TypecheckedPat
\end{code}

\begin{code}
nullBind :: Bind bdee pat -> Bool
nullBind EmptyBind		= True
nullBind (NonRecBind bs)	= nullMonoBinds bs
nullBind (RecBind bs)		= nullMonoBinds bs
\end{code}

\begin{code}
bindIsRecursive :: TypecheckedBind -> Bool
bindIsRecursive EmptyBind	= False
bindIsRecursive (NonRecBind _)	= False
bindIsRecursive (RecBind _)	= True
\end{code}

\begin{code}
instance (NamedThing bdee, Outputable bdee,
             NamedThing pat, Outputable pat) =>
		Outputable (Bind bdee pat) where
    ppr sty EmptyBind = ppNil
    ppr sty (NonRecBind binds)
     = ppAbove (ifnotPprForUser sty (ppStr "{- nonrec -}"))
	       (ppr sty binds)
    ppr sty (RecBind binds)
     = ppAbove (ifnotPprForUser sty (ppStr "{- rec -}"))
	       (ppr sty binds)
\end{code}

%************************************************************************
%*									*
\subsection[AbsSyn-MonoBinds]{Bindings: @MonoBinds@}
%*									*
%************************************************************************

Global bindings (where clauses)

\begin{code}
data MonoBinds bdee pat		-- binders and bindees
  = EmptyMonoBinds			-- TRANSLATION
  | AndMonoBinds    (MonoBinds bdee pat)
		    (MonoBinds bdee pat)
  | PatMonoBind     pat
		    (GRHSsAndBinds bdee pat)
		    SrcLoc
  | VarMonoBind	    Id			-- TRANSLATION
		    (Expr bdee pat)
  | FunMonoBind     bdee
		    [Match bdee pat]	-- must have at least one Match
		    SrcLoc
\end{code}

The corresponding unparameterised synonyms:
\begin{code}
type ProtoNameMonoBinds	    = MonoBinds ProtoName ProtoNamePat
type RenamedMonoBinds	    = MonoBinds Name	  RenamedPat
type TypecheckedMonoBinds   = MonoBinds Id        TypecheckedPat
\end{code}

\begin{code}
nullMonoBinds :: MonoBinds bdee pat -> Bool
nullMonoBinds EmptyMonoBinds		= True
nullMonoBinds (AndMonoBinds bs1 bs2)	= (nullMonoBinds bs1) && (nullMonoBinds bs2)
nullMonoBinds other_monobind		= False
\end{code}

\begin{code}
instance (NamedThing bdee, Outputable bdee,
             NamedThing pat, Outputable pat) =>
		Outputable (MonoBinds bdee pat) where
    ppr sty EmptyMonoBinds = ppNil
    ppr sty (AndMonoBinds binds1 binds2)
     = ppAbove (ppr sty binds1) (ppr sty binds2)

    ppr sty (PatMonoBind pat grhss_n_binds locn)
     = ppAboves [
	    ifPprShowAll sty (ppr sty locn),
	    (if (hasType pat) then
		ppHang (ppCat [ppr sty pat, ppStr "::"]) 4 (pprUniType sty (getType pat))
	    else
		ppNil
	    ),
	    (ppHang (ppr sty pat) 4 (pprGRHSsAndBinds sty False grhss_n_binds)) ]

    ppr sty (FunMonoBind fun matches locn)
     = ppAboves [
	    ifPprShowAll sty (ppr sty locn),
	    if (hasType fun) then
		ppHang (ppCat [pprNonOp sty fun, ppStr "::"]) 4 
		       (pprUniType sty (getType fun))
	    else
		ppNil,
    	   pprMatches sty (False, pprNonOp sty fun) matches
       ]

    ppr sty (VarMonoBind name expr)
     = ppHang (ppCat [pprNonOp sty name, ppEquals]) 4 (ppr sty expr)
\end{code}
