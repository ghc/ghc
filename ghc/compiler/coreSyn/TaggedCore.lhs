%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TaggedCore]{``Tagged binder'' core syntax (including \tr{Simplifiable*})}

This module defines a particular parameterisation of the @CoreSyntax@
data type.  For ``binders,'' we use a pair: an @Id@ (the actual
binder) and a ``tag''---any old thing we want to pin on.
Bindees are @Ids@, as usual.

By far the prevalent use is with a ``tag'' of a @BinderInfo@, as used
in the simplifier.  So we have a full swatch of synonyms for
\tr{Simplifiable} this and that.

\begin{code}
#include "HsVersions.h"

module TaggedCore (
	TaggedBinder(..), TaggedCoreBinding(..), TaggedCoreExpr(..),
	TaggedCoreAtom(..), TaggedCoreCaseAlternatives(..),
	TaggedCoreCaseDefault(..),
#ifdef DPH
	TaggedCoreParQuals(..),
	TaggedCoreParCommunicate(..),
	CoreParCommunicate(..),
	CoreParQuals(..),
#endif
	unTagBinders, unTagBindersAlts,

	CoreArg(..), applyToArgs, decomposeArgs, collectArgs,

	SimplifiableBinder(..), SimplifiableCoreBinding(..),
	SimplifiableCoreExpr(..), SimplifiableCoreAtom(..),
	SimplifiableCoreCaseAlternatives(..),
	SimplifiableCoreCaseDefault(..),
#ifdef DPH
	SimplifiableCoreParQuals(..),
	SimplifiableCoreParCommunicate(..),
#endif

	CoreBinding(..), CoreExpr(..),	CoreAtom(..), -- re-exported
	CoreCaseAlternatives(..), CoreCaseDefault(..),

	-- and to make the interface self-sufficient ...
	Outputable(..), NamedThing(..),
	ExportFlag, Pretty(..), PprStyle, PrettyRep,

	BasicLit, BinderInfo, GlobalSwitch, Id, PrimOp, CostCentre,
	SrcLoc, TyCon, TyVar, UniType, Unique
    ) where

import CoreFuns		( unTagBinders, unTagBindersAlts, digForLambdas )
import CoreSyn		-- mostly re-exporting this stuff
import BinderInfo	( BinderInfo )
import Outputable
import Util
\end{code}

\begin{code}
type TaggedBinder tag = (Id, tag)

type TaggedCoreProgram tag = [CoreBinding (TaggedBinder tag) Id]
type TaggedCoreBinding tag =  CoreBinding (TaggedBinder tag) Id
type TaggedCoreExpr    tag =  CoreExpr    (TaggedBinder tag) Id
type TaggedCoreAtom    tag =  CoreAtom			     Id

#ifdef DPH
type TaggedCoreParQuals tag = CoreParQuals (TaggedBinder tag) Id
type TaggedCoreParCommunicate tag
  = CoreParCommunicate (TaggedBinder tag) Id
#endif {- Data Parallel Haskell -}

type TaggedCoreCaseAlternatives tag = CoreCaseAlternatives (TaggedBinder tag) Id
type TaggedCoreCaseDefault tag = CoreCaseDefault (TaggedBinder tag) Id
\end{code}

\begin{code}
type SimplifiableBinder = (Id, BinderInfo)

type SimplifiableCoreProgram = [CoreBinding SimplifiableBinder Id]
type SimplifiableCoreBinding =  CoreBinding SimplifiableBinder Id
type SimplifiableCoreExpr    =  CoreExpr    SimplifiableBinder Id
type SimplifiableCoreAtom    =  CoreAtom		       Id

#ifdef DPH
type SimplifiableCoreParQuals = CoreParQuals SimplifiableBinder Id
type SimplifiableCoreParCommunicate
  = CoreParCommunicate SimplifiableBinder Id
#endif {- Data Parallel Haskell -}

type SimplifiableCoreCaseAlternatives = CoreCaseAlternatives SimplifiableBinder Id
type SimplifiableCoreCaseDefault      = CoreCaseDefault SimplifiableBinder Id
\end{code}
