%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[IdInfo]{@IdInfos@: Non-essential information about @Ids@}

(And a pretty good illustration of quite a few things wrong with
Haskell. [WDP 94/11])

\begin{code}
module IdInfo (
	IdInfo,		-- Abstract

	noIdInfo,
	ppIdInfo,

	-- Arity
	ArityInfo(..),
	exactArity, atLeastArity, unknownArity,
	arityInfo, setArityInfo, ppArityInfo,

	-- Demand
	DemandInfo,
	noDemandInfo, mkDemandInfo, demandInfo, ppDemandInfo, setDemandInfo, willBeDemanded,
	Demand(..), 	 				-- Non-abstract

	-- Strictness
	StrictnessInfo(..),				-- Non-abstract
	workerExists,
	mkStrictnessInfo, mkBottomStrictnessInfo, noStrictnessInfo, bottomIsGuaranteed,
	strictnessInfo, ppStrictnessInfo, setStrictnessInfo, 

	-- Unfolding
	unfoldingInfo, setUnfoldingInfo, 

	-- Inline prags
	InlinePragInfo(..),
	inlinePragInfo, setInlinePragInfo,

	-- Specialisation
	IdSpecEnv, specInfo, setSpecInfo,

	-- Update
	UpdateInfo, UpdateSpec,
	mkUpdateInfo, updateInfo, updateInfoMaybe, ppUpdateInfo, setUpdateInfo,

	-- Arg usage 
	ArgUsageInfo, ArgUsage(..), ArgUsageType,
	mkArgUsageInfo, argUsageInfo, setArgUsageInfo, getArgUsage,

	-- FB type
	FBTypeInfo, FBType(..), FBConsum(..), FBProd(..),
	fbTypeInfo, ppFBTypeInfo, setFBTypeInfo, mkFBTypeInfo, getFBType
    ) where

#include "HsVersions.h"


import {-# SOURCE #-} CoreUnfold ( Unfolding, noUnfolding )
import {-# SOURCE #-} CoreSyn	 ( CoreExpr )

-- for mkdependHS, CoreSyn.hi-boot refers to it:
import BinderInfo ( BinderInfo )

import SpecEnv	        ( SpecEnv, emptySpecEnv )
import BasicTypes	( NewOrData )

import Demand
import Outputable	

import Char 		( ord )
\end{code}

An @IdInfo@ gives {\em optional} information about an @Id@.  If
present it never lies, but it may not be present, in which case there
is always a conservative assumption which can be made.

Two @Id@s may have different info even though they have the same
@Unique@ (and are hence the same @Id@); for example, one might lack
the properties attached to the other.

The @IdInfo@ gives information about the value, or definition, of the
@Id@.  It does {\em not} contain information about the @Id@'s usage
(except for @DemandInfo@? ToDo).

\begin{code}
data IdInfo
  = IdInfo {
	arityInfo :: ArityInfo,			-- Its arity

	demandInfo :: DemandInfo,		-- Whether or not it is definitely demanded

	specInfo :: IdSpecEnv,			-- Specialisations of this function which exist

	strictnessInfo :: StrictnessInfo,	-- Strictness properties

	unfoldingInfo :: Unfolding,		-- Its unfolding; for locally-defined
						-- things, this can *only* be NoUnfolding

	updateInfo :: UpdateInfo,		-- Which args should be updated

	argUsageInfo :: ArgUsageInfo, 		-- how this Id uses its arguments

	fbTypeInfo :: FBTypeInfo,		-- the Foldr/Build W/W property of this function.

	inlinePragInfo :: InlinePragInfo	-- Inline pragmas
    }
\end{code}

Setters

\begin{code}
setFBTypeInfo 	  fb info = info { fbTypeInfo = fb }
setArgUsageInfo   au info = info { argUsageInfo = au }
setUpdateInfo	  ud info = info { updateInfo = ud }
setDemandInfo	  dd info = info { demandInfo = dd }
setStrictnessInfo st info = info { strictnessInfo = st }
setSpecInfo 	  sp info = info { specInfo = sp }
setArityInfo	  ar info = info { arityInfo = ar  }
setInlinePragInfo pr info = info { inlinePragInfo = pr }
setUnfoldingInfo  uf info = info { unfoldingInfo = uf }
\end{code}


\begin{code}
noIdInfo = IdInfo {
		arityInfo	= UnknownArity,
		demandInfo	= UnknownDemand,
		specInfo	= emptySpecEnv,
		strictnessInfo	= NoStrictnessInfo,
		unfoldingInfo	= noUnfolding,
		updateInfo	= NoUpdateInfo,
		argUsageInfo	= NoArgUsageInfo,
		fbTypeInfo	= NoFBTypeInfo, 
		inlinePragInfo  = NoPragmaInfo
	   }
\end{code}

\begin{code}
ppIdInfo :: Bool	-- True <=> print specialisations, please
	 -> IdInfo
	 -> SDoc

ppIdInfo specs_please (IdInfo {arityInfo, updateInfo, strictnessInfo, demandInfo})
  = hsep [
	    ppArityInfo arityInfo,
	    ppUpdateInfo updateInfo,
	    ppStrictnessInfo strictnessInfo,
	    ppDemandInfo demandInfo
	]
\end{code}

%************************************************************************
%*									*
\subsection[arity-IdInfo]{Arity info about an @Id@}
%*									*
%************************************************************************

\begin{code}
data ArityInfo
  = UnknownArity	-- No idea
  | ArityExactly Int	-- Arity is exactly this
  | ArityAtLeast Int	-- Arity is this or greater

exactArity   = ArityExactly
atLeastArity = ArityAtLeast
unknownArity = UnknownArity

ppArityInfo UnknownArity	 = empty
ppArityInfo (ArityExactly arity) = hsep [ptext SLIT("_A_"), int arity]
ppArityInfo (ArityAtLeast arity) = hsep [ptext SLIT("_A>_"), int arity]
\end{code}

%************************************************************************
%*									*
\subsection{Inline-pragma information}
%*									*
%************************************************************************

\begin{code}
data InlinePragInfo
  = NoPragmaInfo

  | IWantToBeINLINEd

  | IMustNotBeINLINEd	-- Used by the simplifier to prevent looping
			-- on recursive definitions

  | IMustBeINLINEd	-- Absolutely must inline; used for PrimOps only
\end{code}


%************************************************************************
%*									*
\subsection[specialisation-IdInfo]{Specialisation info about an @Id@}
%*									*
%************************************************************************

A @IdSpecEnv@ holds details of an @Id@'s specialisations. 

\begin{code}
type IdSpecEnv = SpecEnv CoreExpr
\end{code}

For example, if \tr{f}'s @SpecEnv@ contains the mapping:
\begin{verbatim}
	[List a, b]  ===>  (\d -> f' a b)
\end{verbatim}
then when we find an application of f to matching types, we simply replace
it by the matching RHS:
\begin{verbatim}
	f (List Int) Bool ===>  (\d -> f' Int Bool)
\end{verbatim}
All the stuff about how many dictionaries to discard, and what types
to apply the specialised function to, are handled by the fact that the
SpecEnv contains a template for the result of the specialisation.

There is one more exciting case, which is dealt with in exactly the same
way.  If the specialised value is unboxed then it is lifted at its
definition site and unlifted at its uses.  For example:

	pi :: forall a. Num a => a

might have a specialisation

	[Int#] ===>  (case pi' of Lift pi# -> pi#)

where pi' :: Lift Int# is the specialised version of pi.



%************************************************************************
%*									*
\subsection[strictness-IdInfo]{Strictness info about an @Id@}
%*									*
%************************************************************************

We specify the strictness of a function by giving information about
each of the ``wrapper's'' arguments (see the description about
worker/wrapper-style transformations in the PJ/Launchbury paper on
unboxed types).

The list of @Demands@ specifies: (a)~the strictness properties
of a function's arguments; (b)~the {\em existence} of a ``worker''
version of the function; and (c)~the type signature of that worker (if
it exists); i.e. its calling convention.

\begin{code}
data StrictnessInfo
  = NoStrictnessInfo

  | BottomGuaranteed	-- This Id guarantees never to return;
			-- it is bottom regardless of its arguments.
			-- Useful for "error" and other disguised
			-- variants thereof.

  | StrictnessInfo [Demand] 
		   Bool		-- True <=> there is a worker. There might not be, even for a
				-- strict function, because:
				-- 	(a) the function might be small enough to inline, 
				--	    so no need for w/w split
				-- 	(b) the strictness info might be "SSS" or something, so no w/w split.

				-- Worker's Id, if applicable, and a list of the constructors
				-- mentioned by the wrapper.  This is necessary so that the
				-- renamer can slurp them in.  Without this info, the renamer doesn't
				-- know which data types to slurp in concretely.  Remember, for
				-- strict things we don't put the unfolding in the interface file, to save space.
				-- This constructor list allows the renamer to behave much as if the
				-- unfolding *was* in the interface file.
\end{code}

\begin{code}
mkStrictnessInfo :: [Demand] -> Bool -> StrictnessInfo

mkStrictnessInfo xs has_wrkr
  | all is_lazy xs	 = NoStrictnessInfo		-- Uninteresting
  | otherwise		 = StrictnessInfo xs has_wrkr
  where
    is_lazy (WwLazy False) = True	-- NB "Absent" args do *not* count!
    is_lazy _	           = False	-- (as they imply a worker)

noStrictnessInfo       = NoStrictnessInfo
mkBottomStrictnessInfo = BottomGuaranteed

bottomIsGuaranteed BottomGuaranteed = True
bottomIsGuaranteed other    	    = False

ppStrictnessInfo NoStrictnessInfo = empty
ppStrictnessInfo BottomGuaranteed = ptext SLIT("_bot_")

ppStrictnessInfo (StrictnessInfo wrapper_args wrkr_maybe)
  = hsep [ptext SLIT("_S_"), text (showList wrapper_args "")]
\end{code}


\begin{code}
workerExists :: StrictnessInfo -> Bool
workerExists (StrictnessInfo _ worker_exists) = worker_exists
workerExists other			      = False
\end{code}


%************************************************************************
%*									*
\subsection[demand-IdInfo]{Demand info about an @Id@}
%*									*
%************************************************************************

Whether a value is certain to be demanded or not.  (This is the
information that is computed by the ``front-end'' of the strictness
analyser.)

This information is only used within a module, it is not exported
(obviously).

\begin{code}
data DemandInfo
  = UnknownDemand
  | DemandedAsPer Demand
\end{code}

\begin{code}
noDemandInfo = UnknownDemand

mkDemandInfo :: Demand -> DemandInfo
mkDemandInfo demand = DemandedAsPer demand

willBeDemanded :: DemandInfo -> Bool
willBeDemanded (DemandedAsPer demand) = isStrict demand
willBeDemanded _		      = False

ppDemandInfo UnknownDemand	      = text "{-# L #-}"
ppDemandInfo (DemandedAsPer info) = hsep [text "{-#", text (showList [info] ""), text "#-}"]
\end{code}


%************************************************************************
%*									*
\subsection[update-IdInfo]{Update-analysis info about an @Id@}
%*									*
%************************************************************************

\begin{code}
data UpdateInfo
  = NoUpdateInfo
  | SomeUpdateInfo UpdateSpec
  deriving (Eq, Ord)
      -- we need Eq/Ord to cross-chk update infos in interfaces

-- the form in which we pass update-analysis info between modules:
type UpdateSpec = [Int]
\end{code}

\begin{code}
mkUpdateInfo = SomeUpdateInfo

updateInfoMaybe NoUpdateInfo	    = Nothing
updateInfoMaybe (SomeUpdateInfo []) = Nothing
updateInfoMaybe (SomeUpdateInfo	 u) = Just u
\end{code}

Text instance so that the update annotations can be read in.

\begin{code}
ppUpdateInfo NoUpdateInfo	       = empty
ppUpdateInfo (SomeUpdateInfo [])   = empty
ppUpdateInfo (SomeUpdateInfo spec) = (<>) (ptext SLIT("_U_ ")) (hcat (map int spec))
\end{code}

%************************************************************************
%*									*
\subsection[argUsage-IdInfo]{Argument Usage info about an @Id@}
%*									*
%************************************************************************

\begin{code}
data ArgUsageInfo
  = NoArgUsageInfo
  | SomeArgUsageInfo ArgUsageType

data ArgUsage = ArgUsage Int	-- number of arguments (is linear!)
	      | UnknownArgUsage

type ArgUsageType  = [ArgUsage]		-- c_1 -> ... -> BLOB
\end{code}

\begin{code}
mkArgUsageInfo [] = NoArgUsageInfo
mkArgUsageInfo au = SomeArgUsageInfo au

getArgUsage :: ArgUsageInfo -> ArgUsageType
getArgUsage NoArgUsageInfo	  = []
getArgUsage (SomeArgUsageInfo u)  = u
\end{code}

\begin{code}
{- UNUSED:
ppArgUsageInfo NoArgUsageInfo	  = empty
ppArgUsageInfo (SomeArgUsageInfo aut) = (<>) (ptext SLIT("_L_ ")) (ppArgUsageType aut)
-}

ppArgUsage (ArgUsage n)      = int n
ppArgUsage (UnknownArgUsage) = char '-'

ppArgUsageType aut = hcat
	[ char '"' ,
	  hcat (punctuate comma (map ppArgUsage aut)),
	  char '"' ]
\end{code}


%************************************************************************
%*									*
\subsection[FBType-IdInfo]{Type of an expression through Foldr/build's eyes}
%*									*
%************************************************************************

\begin{code}
data FBTypeInfo
  = NoFBTypeInfo
  | SomeFBTypeInfo FBType

data FBType = FBType [FBConsum] FBProd deriving (Eq)

data FBConsum = FBGoodConsum | FBBadConsum deriving(Eq)
data FBProd = FBGoodProd | FBBadProd deriving(Eq)
\end{code}

\begin{code}
mkFBTypeInfo = SomeFBTypeInfo

getFBType :: FBTypeInfo -> Maybe FBType
getFBType NoFBTypeInfo	      = Nothing
getFBType (SomeFBTypeInfo u)  = Just u
\end{code}

\begin{code}
ppFBTypeInfo NoFBTypeInfo = empty
ppFBTypeInfo (SomeFBTypeInfo (FBType cons prod))
      = (<>) (ptext SLIT("_F_ ")) (ppFBType cons prod)

ppFBType cons prod = hcat
	([ char '"' ] ++ map ppCons cons ++ [ char '-', ppProd prod, char '"' ])
  where
	ppCons FBGoodConsum = char 'G'
	ppCons FBBadConsum  = char 'B'
	ppProd FBGoodProd   = char 'G'
	ppProd FBBadProd    = char 'B'
\end{code}
