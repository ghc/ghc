%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[IdInfo]{@IdInfos@: Non-essential information about @Ids@}

(And a pretty good illustration of quite a few things wrong with
Haskell. [WDP 94/11])

\begin{code}
#include "HsVersions.h"

module IdInfo (
	IdInfo,		-- Abstract

	noIdInfo,
	ppIdInfo,
	applySubstToIdInfo, apply_to_IdInfo,    -- not for general use, please

	ArityInfo(..),
	exactArity, atLeastArity, unknownArity,
	arityInfo, addArityInfo, ppArityInfo,

	DemandInfo,
	noDemandInfo, mkDemandInfo, demandInfo, ppDemandInfo, addDemandInfo, willBeDemanded,

	StrictnessInfo(..),				-- Non-abstract
	Demand(..),	    				-- Non-abstract
	wwLazy, wwStrict, wwUnpack, wwPrim, wwEnum,

	getWorkerId_maybe,
	workerExists,
	mkStrictnessInfo, mkBottomStrictnessInfo, noStrictnessInfo, bottomIsGuaranteed,
	strictnessInfo, ppStrictnessInfo, addStrictnessInfo, 

	unfoldInfo, addUnfoldInfo, 

	specInfo, addSpecInfo,

	UpdateInfo, SYN_IE(UpdateSpec),
	mkUpdateInfo, updateInfo, updateInfoMaybe, ppUpdateInfo, addUpdateInfo,

	DeforestInfo(..),
	deforestInfo, ppDeforestInfo, addDeforestInfo,

	ArgUsageInfo, ArgUsage(..), SYN_IE(ArgUsageType),
	mkArgUsageInfo, argUsageInfo, addArgUsageInfo, getArgUsage,

	FBTypeInfo, FBType(..), FBConsum(..), FBProd(..),
	fbTypeInfo, ppFBTypeInfo, addFBTypeInfo, mkFBTypeInfo, getFBType
    ) where

IMP_Ubiq()
IMPORT_1_3(Char(toLower))

IMPORT_DELOOPER(IdLoop)	-- IdInfo is a dependency-loop ranch, and
			-- we break those loops by using IdLoop and
			-- *not* importing much of anything else,
			-- except from the very general "utils".

import Type		( eqSimpleTy, splitFunTyExpandingDicts )
import CmdLineOpts	( opt_OmitInterfacePragmas )

import Demand
import Maybes		( firstJust )
import Outputable	( ifPprInterface, Outputable(..){-instances-} )
import PprStyle		( PprStyle(..) )
import Pretty
import Unique		( pprUnique )
import Util		( mapAccumL, panic, assertPanic, pprPanic )

#ifdef REALLY_HASKELL_1_3
ord = fromEnum :: Char -> Int
#endif

applySubstToTy = panic "IdInfo.applySubstToTy"
showTypeCategory = panic "IdInfo.showTypeCategory"
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
  = IdInfo
	ArityInfo		-- Its arity

	DemandInfo		-- Whether or not it is definitely
				-- demanded

	SpecEnv
				-- Specialisations of this function which exist

	(StrictnessInfo Id)
				-- Strictness properties, notably
				-- how to conjure up "worker" functions

	Unfolding
				-- Its unfolding; for locally-defined
				-- things, this can *only* be NoUnfolding

	UpdateInfo		-- Which args should be updated

	DeforestInfo            -- Whether its definition should be
				-- unfolded during deforestation

	ArgUsageInfo		-- how this Id uses its arguments

	FBTypeInfo		-- the Foldr/Build W/W property of this function.
\end{code}

\begin{code}
noIdInfo = IdInfo UnknownArity UnknownDemand nullSpecEnv NoStrictnessInfo noUnfolding
		  NoUpdateInfo Don'tDeforest NoArgUsageInfo NoFBTypeInfo 
\end{code}

Simply turgid.  But BE CAREFUL: don't @apply_to_Id@ if that @Id@
will in turn @apply_to_IdInfo@ of the self-same @IdInfo@.  (A very
nasty loop, friends...)
\begin{code}
apply_to_IdInfo ty_fn idinfo@(IdInfo arity demand spec strictness unfold
			      update deforest arg_usage fb_ww)
  | isNullSpecEnv spec
  = idinfo
  | otherwise
  = panic "IdInfo:apply_to_IdInfo"
{- LATER:
    let
	new_spec = apply_spec spec

	-- NOT a good idea:
	--   apply_strict strictness 	`thenLft` \ new_strict ->
	--   apply_wrap wrap 		`thenLft` \ new_wrap ->
    in
    IdInfo arity demand new_spec strictness unfold
	   update deforest arg_usage fb_ww
  where
    apply_spec (SpecEnv is)
      = SpecEnv (map do_one is)
      where
    	do_one (SpecInfo ty_maybes ds spec_id)
	  = --apply_to_Id ty_fn spec_id	`thenLft` \ new_spec_id ->
	    SpecInfo (map apply_to_maybe ty_maybes) ds spec_id
	  where
	    apply_to_maybe Nothing   = Nothing
	    apply_to_maybe (Just ty) = Just (ty_fn ty)
-}

{- NOT a good idea;
    apply_strict info@NoStrictnessInfo = returnLft info
    apply_strict BottomGuaranteed = ???
    apply_strict (StrictnessInfo wrap_arg_info id_maybe)
      = (case id_maybe of
	   Nothing -> returnLft Nothing
	   Just xx -> applySubstToId subst xx `thenLft` \ new_xx ->
		      returnLft (Just new_xx)
	) `thenLft` \ new_id_maybe ->
	returnLft (StrictnessInfo wrap_arg_info new_id_maybe)
-}
\end{code}

Variant of the same thing for the typechecker.
\begin{code}
applySubstToIdInfo s0 (IdInfo arity demand spec strictness unfold
			      update deforest arg_usage fb_ww)
  = panic "IdInfo:applySubstToIdInfo"
{- LATER:
    case (apply_spec s0 spec) of { (s1, new_spec) ->
    (s1, IdInfo arity demand new_spec strictness unfold update deforest arg_usage fb_ww) }
  where
    apply_spec s0 (SpecEnv is)
      = case (mapAccumL do_one s0 is) of { (s1, new_is) ->
	(s1, SpecEnv new_is) }
      where
    	do_one s0 (SpecInfo ty_maybes ds spec_id)
	  = case (mapAccumL apply_to_maybe s0 ty_maybes) of { (s1, new_maybes) ->
	    (s1, SpecInfo new_maybes ds spec_id) }
	  where
	    apply_to_maybe s0 Nothing   = (s0, Nothing)
	    apply_to_maybe s0 (Just ty)
	      = case (applySubstToTy s0 ty) of { (s1, new_ty) ->
		(s1, Just new_ty) }
-}
\end{code}

\begin{code}
ppIdInfo :: PprStyle
	 -> Bool	-- True <=> print specialisations, please
	 -> IdInfo
	 -> Pretty

ppIdInfo sty specs_please
    	 (IdInfo arity demand specenv strictness unfold update deforest arg_usage fbtype)
  = ppCat [
		    -- order is important!:
		    ppArityInfo sty arity,
		    ppUpdateInfo sty update,
		    ppDeforestInfo sty deforest,

		    ppStrictnessInfo sty strictness,

		    if specs_please
		    then ppNil -- ToDo -- sty (not (isDataCon for_this_id))
					 -- better_id_fn inline_env (mEnvToList specenv)
		    else ppNil,

		    -- DemandInfo needn't be printed since it has no effect on interfaces
		    ppDemandInfo sty demand,
		    ppFBTypeInfo sty fbtype
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
\end{code}

\begin{code}
exactArity   = ArityExactly
atLeastArity = ArityAtLeast
unknownArity = UnknownArity

arityInfo (IdInfo arity _ _ _ _ _ _ _ _) = arity

addArityInfo id_info			UnknownArity = id_info
addArityInfo (IdInfo _ a c d e f g h i) arity	     = IdInfo arity a c d e f g h i

ppArityInfo sty UnknownArity	     = ppNil
ppArityInfo sty (ArityExactly arity) = ppCat [ppPStr SLIT("_A_"), ppInt arity]
ppArityInfo sty (ArityAtLeast arity) = ppCat [ppPStr SLIT("_A>_"), ppInt arity]
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
\end{code}

\begin{code}
demandInfo (IdInfo _ demand _ _ _ _ _ _ _) = demand

addDemandInfo (IdInfo a _ c d e f g h i) demand = IdInfo a demand c d e f g h i

ppDemandInfo PprInterface _	      = ppNil
ppDemandInfo sty UnknownDemand	      = ppStr "{-# L #-}"
ppDemandInfo sty (DemandedAsPer info) = ppCat [ppStr "{-#", ppStr (showList [info] ""), ppStr "#-}"]
\end{code}

%************************************************************************
%*									*
\subsection[specialisation-IdInfo]{Specialisation info about an @Id@}
%*									*
%************************************************************************

See SpecEnv.lhs

\begin{code}
specInfo (IdInfo _ _ spec _ _ _ _ _ _) = spec

addSpecInfo id_info spec | isNullSpecEnv spec = id_info
addSpecInfo (IdInfo a b _ d e f g h i) spec   = IdInfo a b spec d e f g h i
\end{code}

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
data StrictnessInfo bdee
  = NoStrictnessInfo

  | BottomGuaranteed	-- This Id guarantees never to return;
			-- it is bottom regardless of its arguments.
			-- Useful for "error" and other disguised
			-- variants thereof.

  | StrictnessInfo	[Demand]	-- The main stuff; see below.
			(Maybe bdee)	-- Worker's Id, if applicable.
					-- (It may not be applicable because the strictness info
					-- might say just "SSS" or something; so there's no w/w split.)
\end{code}

\begin{code}
mkStrictnessInfo :: [Demand] -> Maybe bdee -> StrictnessInfo bdee

mkStrictnessInfo xs wrkr 
  | all is_lazy xs	 = NoStrictnessInfo		-- Uninteresting
  | otherwise		 = StrictnessInfo xs wrkr
  where
    is_lazy (WwLazy False) = True	-- NB "Absent" args do *not* count!
    is_lazy _	           = False	-- (as they imply a worker)

noStrictnessInfo       = NoStrictnessInfo
mkBottomStrictnessInfo = BottomGuaranteed

bottomIsGuaranteed BottomGuaranteed = True
bottomIsGuaranteed other    	    = False

strictnessInfo (IdInfo _ _ _ strict _ _ _ _ _) = strict

addStrictnessInfo id_info 		     NoStrictnessInfo = id_info
addStrictnessInfo (IdInfo a b d _ e f g h i) strict	      = IdInfo a b d strict e f g h i

ppStrictnessInfo sty NoStrictnessInfo = ppNil
ppStrictnessInfo sty BottomGuaranteed = ppPStr SLIT("_S_ _!_")

ppStrictnessInfo sty (StrictnessInfo wrapper_args wrkr_maybe)
  = ppCat [ppPStr SLIT("_S_"), ppStr (showList wrapper_args ""), pp_wrkr]
  where
    pp_wrkr = case wrkr_maybe of
		 Nothing   -> ppNil
		 Just wrkr -> ppr sty wrkr
\end{code}


\begin{code}
workerExists :: StrictnessInfo bdee -> Bool
workerExists (StrictnessInfo _ (Just worker_id)) = True
workerExists other				 = False

getWorkerId_maybe :: StrictnessInfo bdee -> Maybe bdee
getWorkerId_maybe (StrictnessInfo _ maybe_worker_id) = maybe_worker_id
getWorkerId_maybe other				     = Nothing
\end{code}


%************************************************************************
%*									*
\subsection[unfolding-IdInfo]{Unfolding info about an @Id@}
%*									*
%************************************************************************

\begin{code}
unfoldInfo (IdInfo _ _ _ _ unfolding _ _ _ _) = unfolding

addUnfoldInfo (IdInfo a b d e _ f g h i) uf = IdInfo a b d e uf f g h i
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
#ifdef REALLY_HASKELL_1_3
instance Read UpdateInfo where
#else
instance Text UpdateInfo where
#endif
    readsPrec p s | null s    = panic "IdInfo: empty update pragma?!"
		  | otherwise = [(SomeUpdateInfo (map ok_digit s),"")]
      where
	ok_digit c | c >= '0' && c <= '2' = ord c - ord '0'
		   | otherwise = panic "IdInfo: not a digit while reading update pragma"

updateInfo (IdInfo _ _ _ _ _ update _ _ _) = update

addUpdateInfo id_info			 NoUpdateInfo = id_info
addUpdateInfo (IdInfo a b d e f _ g h i) upd_info     = IdInfo a b d e f upd_info g h i

ppUpdateInfo sty NoUpdateInfo	       = ppNil
ppUpdateInfo sty (SomeUpdateInfo [])   = ppNil
ppUpdateInfo sty (SomeUpdateInfo spec) = ppBeside (ppPStr SLIT("_U_ ")) (ppBesides (map ppInt spec))
\end{code}

%************************************************************************
%*                                                                    *
\subsection[deforest-IdInfo]{Deforestation info about an @Id@}
%*                                                                    *
%************************************************************************

The deforest info says whether this Id is to be unfolded during
deforestation.  Therefore, when the deforest pragma is true, we must
also have the unfolding information available for this Id.

\begin{code}
data DeforestInfo
  = Don'tDeforest                     -- just a bool, might extend this
  | DoDeforest                                -- later.
  -- deriving (Eq, Ord)
\end{code}

\begin{code}
deforestInfo (IdInfo _ _ _ _ _ _ deforest _ _) = deforest

addDeforestInfo id_info 		   Don'tDeforest = id_info
addDeforestInfo (IdInfo a b d e f g _ h i) deforest	 = IdInfo a b d e f g deforest h i

ppDeforestInfo sty Don'tDeforest = ppNil
ppDeforestInfo sty DoDeforest    = ppPStr SLIT("_DEFOREST_")
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
  -- ??? deriving (Eq, Ord)

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
argUsageInfo (IdInfo _ _ _ _ _  _ _ au _) = au

addArgUsageInfo id_info			   NoArgUsageInfo = id_info
addArgUsageInfo (IdInfo a b d e f g h _ i) au_info	  = IdInfo a b d e f g h au_info i

ppArgUsageInfo sty NoArgUsageInfo	  = ppNil
ppArgUsageInfo sty (SomeArgUsageInfo aut) = ppBeside (ppPStr SLIT("_L_ ")) (ppArgUsageType aut)

ppArgUsage (ArgUsage n)      = ppInt n
ppArgUsage (UnknownArgUsage) = ppChar '-'

ppArgUsageType aut = ppBesides
	[ ppChar '"' ,
	  ppIntersperse ppComma (map ppArgUsage aut),
	  ppChar '"' ]
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
fbTypeInfo (IdInfo _ _ _ _ _ _ _ _ fb) = fb

addFBTypeInfo id_info NoFBTypeInfo = id_info
addFBTypeInfo (IdInfo a b d e f g h i _) fb_info = IdInfo a b d e f g h i fb_info

ppFBTypeInfo sty NoFBTypeInfo = ppNil
ppFBTypeInfo sty (SomeFBTypeInfo (FBType cons prod))
      = ppBeside (ppPStr SLIT("_F_ ")) (ppFBType cons prod)

ppFBType cons prod = ppBesides
	([ ppChar '"' ] ++ map ppCons cons ++ [ ppChar '-', ppProd prod, ppChar '"' ])
  where
	ppCons FBGoodConsum = ppChar 'G'
	ppCons FBBadConsum  = ppChar 'B'
	ppProd FBGoodProd   = ppChar 'G'
	ppProd FBBadProd    = ppChar 'B'
\end{code}
