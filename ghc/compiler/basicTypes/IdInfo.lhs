%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[IdInfo]{@IdInfos@: Non-essential information about @Ids@}

(And a pretty good illustration of quite a few things wrong with
Haskell. [WDP 94/11])

\begin{code}
#include "HsVersions.h"

module IdInfo (
	IdInfo,		-- abstract
	noIdInfo,
	boringIdInfo,
	ppIdInfo,
	applySubstToIdInfo, apply_to_IdInfo,    -- not for general use, please

	OptIdInfo(..),	-- class; for convenience only
			-- all the *Infos herein are instances of it

	-- component "id infos"; also abstract:
	SrcLoc,
	getSrcLocIdInfo,

	ArityInfo,
	mkArityInfo, unknownArity, arityMaybe,

	DemandInfo,
	mkDemandInfo,
	willBeDemanded,

	MatchEnv,		-- the SpecEnv
	StrictnessInfo(..), 	-- non-abstract
	Demand(..),	    	-- non-abstract

	wwLazy, wwStrict, wwUnpack, wwPrim, wwEnum,
	indicatesWorker, nonAbsentArgs,
	mkStrictnessInfo, mkBottomStrictnessInfo,
	getWrapperArgTypeCategories,
	getWorkerId,
	workerExists,
	bottomIsGuaranteed,

	mkUnfolding,
	noInfo_UF, getInfo_UF, addInfo_UF, -- to avoid instance virus

	UpdateInfo,
	mkUpdateInfo,
	UpdateSpec(..),
	updateInfoMaybe,

	DeforestInfo(..),

	ArgUsageInfo,
	ArgUsage(..),
	ArgUsageType(..),
	mkArgUsageInfo,
	getArgUsage,

	FBTypeInfo,
	FBType(..),
	FBConsum(..),
	FBProd(..),
	mkFBTypeInfo,
	getFBType

    ) where

import Ubiq

import IdLoop		-- IdInfo is a dependency-loop ranch, and
			-- we break those loops by using IdLoop and
			-- *not* importing much of anything else,
			-- except from the very general "utils".

import CmdLineOpts	( opt_OmitInterfacePragmas )
import Maybes		( firstJust )
import MatchEnv		( nullMEnv, isEmptyMEnv, mEnvToList )
import Outputable	( ifPprInterface, Outputable(..){-instances-} )
import PprStyle		( PprStyle(..) )
import Pretty
import SrcLoc		( mkUnknownSrcLoc )
import Type		( eqSimpleTy )
import Util		( mapAccumL, panic, assertPanic, pprPanic )

applySubstToTy = panic "IdInfo.applySubstToTy"
splitTypeWithDictsAsArgs = panic "IdInfo.splitTypeWithDictsAsArgs"
showTypeCategory = panic "IdInfo.showTypeCategory"
mkFormSummary = panic "IdInfo.mkFormSummary"
occurAnalyseGlobalExpr = panic "IdInfo.occurAnalyseGlobalExpr"
isWrapperFor = panic "IdInfo.isWrapperFor"
pprCoreUnfolding = panic "IdInfo.pprCoreUnfolding"
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

	(MatchEnv [Type] CoreExpr)
				-- Specialisations of this function which exist
				-- This corresponds to a SpecEnv which we do
				-- not import directly to avoid loop

	StrictnessInfo		-- Strictness properties, notably
				-- how to conjure up "worker" functions

	UnfoldingDetails	-- Its unfolding; for locally-defined
				-- things, this can *only* be NoUnfoldingDetails

	UpdateInfo		-- Which args should be updated

	DeforestInfo            -- Whether its definition should be
				-- unfolded during deforestation

	ArgUsageInfo		-- how this Id uses its arguments

	FBTypeInfo		-- the Foldr/Build W/W property of this function.

	SrcLoc			-- Source location of definition

	-- ToDo: SrcLoc is in FullNames too (could rm?)  but it
	-- is needed here too for things like ConstMethodIds and the
	-- like, which don't have full-names of their own Mind you,
	-- perhaps the Name for a constant method could give the
	-- class/type involved?
\end{code}

\begin{code}
noIdInfo = IdInfo noInfo noInfo noInfo noInfo noInfo_UF
		  noInfo noInfo noInfo noInfo mkUnknownSrcLoc

-- "boring" means: nothing to put in interface
boringIdInfo (IdInfo UnknownArity
		     UnknownDemand
		     specenv
		     strictness
		     unfolding
		     NoUpdateInfo
		     Don'tDeforest
		     _ {- arg_usage: currently no interface effect -}
		     _ {- no f/b w/w -}
		     _ {- src_loc: no effect on interfaces-}
	      )
	      |  null (mEnvToList specenv)
	      && boring_strictness strictness
	      && boring_unfolding unfolding
  = True
  where
    boring_strictness NoStrictnessInfo = True
    boring_strictness BottomGuaranteed = False
    boring_strictness (StrictnessInfo wrap_args _) = all_present_WwLazies wrap_args

    boring_unfolding NoUnfoldingDetails = True
    boring_unfolding _	    	    	= False

boringIdInfo _ = False

pp_NONE = ppPStr SLIT("_N_")
\end{code}

Simply turgid.  But BE CAREFUL: don't @apply_to_Id@ if that @Id@
will in turn @apply_to_IdInfo@ of the self-same @IdInfo@.  (A very
nasty loop, friends...)
\begin{code}
apply_to_IdInfo ty_fn idinfo@(IdInfo arity demand spec strictness unfold
			      update deforest arg_usage fb_ww srcloc)
  | isEmptyMEnv spec
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
	   update deforest arg_usage fb_ww srcloc
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
			      update deforest arg_usage fb_ww srcloc)
  = panic "IdInfo:applySubstToIdInfo"
{- LATER:
    case (apply_spec s0 spec) of { (s1, new_spec) ->
    (s1, IdInfo arity demand new_spec strictness unfold update deforest arg_usage fb_ww srcloc) }
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
	 -> Id		-- The Id for which we're printing this IdInfo
	 -> Bool	-- True <=> print specialisations, please
	 -> (Id -> Id)	-- to look up "better Ids" w/ better IdInfos;
	 -> IdEnv UnfoldingDetails
			-- inlining info for top-level fns in this module
	 -> IdInfo	-- see MkIface notes
	 -> Pretty

ppIdInfo sty for_this_id specs_please better_id_fn inline_env
    i@(IdInfo arity demand specenv strictness unfold update deforest arg_usage fbtype srcloc)
  | boringIdInfo i
  = ppPStr SLIT("_NI_")

  | otherwise
  = let
	stuff = ppCat [
		    -- order is important!:
		    ppInfo sty better_id_fn arity,
		    ppInfo sty better_id_fn update,
		    ppInfo sty better_id_fn deforest,

		    pp_strictness sty (Just for_this_id)
						  better_id_fn inline_env strictness,

		    if bottomIsGuaranteed strictness
		    then pp_NONE
		    else pp_unfolding sty for_this_id inline_env unfold,

		    if specs_please
		    then ppSpecs sty (not (isDataCon for_this_id))
				 better_id_fn inline_env (mEnvToList specenv)
		    else pp_NONE,

		    -- DemandInfo needn't be printed since it has no effect on interfaces
		    ppInfo sty better_id_fn demand,
		    ppInfo sty better_id_fn fbtype
		]
    in
    case sty of
      PprInterface -> if opt_OmitInterfacePragmas
		      then ppNil
		      else stuff
      _		   -> stuff
\end{code}

%************************************************************************
%*									*
\subsection[OptIdInfo-class]{The @OptIdInfo@ class (keeps things tidier)}
%*									*
%************************************************************************

\begin{code}
class OptIdInfo a where
    noInfo	:: a
    getInfo	:: IdInfo -> a
    addInfo	:: IdInfo -> a -> IdInfo
		-- By default, "addInfo" will not overwrite
		-- "info" with "non-info"; look at any instance
		-- to see an example.
    ppInfo	:: PprStyle -> (Id -> Id) -> a -> Pretty
\end{code}

%************************************************************************
%*									*
\subsection[srcloc-IdInfo]{Source-location info in an @IdInfo@}
%*									*
%************************************************************************

Not used much, but...
\begin{code}
getSrcLocIdInfo  (IdInfo _ _ _ _ _ _ _ _ _ src_loc) = src_loc
\end{code}

%************************************************************************
%*									*
\subsection[arity-IdInfo]{Arity info about an @Id@}
%*									*
%************************************************************************

\begin{code}
data ArityInfo
  = UnknownArity	-- no idea
  | ArityExactly Int	-- arity is exactly this
\end{code}

\begin{code}
mkArityInfo  = ArityExactly
unknownArity = UnknownArity

arityMaybe :: ArityInfo -> Maybe Int

arityMaybe UnknownArity	    = Nothing
arityMaybe (ArityExactly i) = Just i
\end{code}

\begin{code}
instance OptIdInfo ArityInfo where
    noInfo = UnknownArity

    getInfo (IdInfo arity _ _ _ _ _ _ _ _ _) = arity

    addInfo id_info UnknownArity = id_info
    addInfo (IdInfo _ a c d e f g h i j) arity = IdInfo arity a c d e f g h i j

    ppInfo sty _ UnknownArity	      = ifPprInterface sty pp_NONE
    ppInfo sty _ (ArityExactly arity) = ppCat [ppPStr SLIT("_A_"), ppInt arity]
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
mkDemandInfo :: Demand -> DemandInfo
mkDemandInfo demand = DemandedAsPer demand

willBeDemanded :: DemandInfo -> Bool
willBeDemanded (DemandedAsPer demand) = isStrict demand
willBeDemanded _		      = False
\end{code}

\begin{code}
instance OptIdInfo DemandInfo where
    noInfo = UnknownDemand

    getInfo (IdInfo _ demand _ _ _ _ _ _ _ _) = demand

{-	DELETED!  If this line is in, there is no way to
	nuke a DemandInfo, and we have to be able to do that
	when floating let-bindings around
    addInfo id_info UnknownDemand = id_info
-}
    addInfo (IdInfo a _ c d e f g h i j) demand = IdInfo a demand c d e f g h i j

    ppInfo PprInterface _ _	      = ppNil
    ppInfo sty _ UnknownDemand	      = ppStr "{-# L #-}"
    ppInfo sty _ (DemandedAsPer info)
      = ppCat [ppStr "{-#", ppStr (showList [info] ""), ppStr "#-}"]
\end{code}

%************************************************************************
%*									*
\subsection[specialisation-IdInfo]{Specialisation info about an @Id@}
%*									*
%************************************************************************

See SpecEnv.lhs

\begin{code}
instance OptIdInfo (MatchEnv [Type] CoreExpr) where
    noInfo = nullMEnv

    getInfo (IdInfo _ _ spec _ _ _ _ _ _ _) = spec

    addInfo id_info spec | null (mEnvToList spec) = id_info
    addInfo (IdInfo a b _ d e f g h i j) spec = IdInfo a b spec d e f g h i j

    ppInfo sty better_id_fn spec
      = ppSpecs sty True better_id_fn nullIdEnv (mEnvToList spec)

ppSpecs sty print_spec_id_info better_id_fn inline_env spec_env
  = if null spec_env then ppNil else panic "IdInfo:ppSpecs"
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
data StrictnessInfo
  = NoStrictnessInfo

  | BottomGuaranteed	-- This Id guarantees never to return;
			-- it is bottom regardless of its arguments.
			-- Useful for "error" and other disguised
			-- variants thereof.

  | StrictnessInfo	[Demand]	-- the main stuff; see below.
			(Maybe Id)	-- worker's Id, if applicable.
\end{code}

This type is also actually used in the strictness analyser:
\begin{code}
data Demand
  = WwLazy		-- Argument is lazy as far as we know
	MaybeAbsent	-- (does not imply worker's existence [etc]).
			-- If MaybeAbsent == True, then it is
			-- *definitely* lazy.  (NB: Absence implies
			-- a worker...)

  | WwStrict		-- Argument is strict but that's all we know
			-- (does not imply worker's existence or any
			-- calling-convention magic)

  | WwUnpack		-- Argument is strict & a single-constructor
	[Demand]	-- type; its constituent parts (whose StrictInfos
			-- are in the list) should be passed
			-- as arguments to the worker.

  | WwPrim		-- Argument is of primitive type, therefore
			-- strict; doesn't imply existence of a worker;
			-- argument should be passed as is to worker.

  | WwEnum		-- Argument is strict & an enumeration type;
			-- an Int# representing the tag (start counting
			-- at zero) should be passed to the worker.
  deriving (Eq, Ord)
      -- we need Eq/Ord to cross-chk update infos in interfaces

type MaybeAbsent = Bool -- True <=> not even used

-- versions that don't worry about Absence:
wwLazy	    = WwLazy 	  False
wwStrict    = WwStrict
wwUnpack xs = WwUnpack xs
wwPrim	    = WwPrim
wwEnum	    = WwEnum
\end{code}

\begin{code}
mkStrictnessInfo :: [Demand] -> Maybe Id -> StrictnessInfo

mkStrictnessInfo [] _    = NoStrictnessInfo
mkStrictnessInfo xs wrkr = StrictnessInfo xs wrkr

mkBottomStrictnessInfo = BottomGuaranteed

bottomIsGuaranteed BottomGuaranteed = True
bottomIsGuaranteed other    	    = False

getWrapperArgTypeCategories
	:: Type		-- wrapper's type
	-> StrictnessInfo	-- strictness info about its args
	-> Maybe String

getWrapperArgTypeCategories _ NoStrictnessInfo	    = Nothing
getWrapperArgTypeCategories _ BottomGuaranteed
  = trace "getWrapperArgTypeCategories:BottomGuaranteed!" Nothing  -- wrong
getWrapperArgTypeCategories _ (StrictnessInfo [] _) = Nothing

getWrapperArgTypeCategories ty (StrictnessInfo arg_info _)
  = Just (mkWrapperArgTypeCategories ty arg_info)

workerExists :: StrictnessInfo -> Bool
workerExists (StrictnessInfo _ (Just worker_id)) = True
workerExists other				 = False

getWorkerId :: StrictnessInfo -> Id

getWorkerId (StrictnessInfo _ (Just worker_id)) = worker_id
#ifdef DEBUG
getWorkerId junk = pprPanic "getWorkerId: " (ppInfo PprDebug (\x->x) junk)
#endif
\end{code}

\begin{code}
isStrict :: Demand -> Bool

isStrict WwStrict	= True
isStrict (WwUnpack _)	= True
isStrict WwPrim		= True
isStrict WwEnum		= True
isStrict _		= False

nonAbsentArgs :: [Demand] -> Int

nonAbsentArgs cmpts
  = foldr tick_non 0 cmpts
  where
    tick_non (WwLazy True) acc = acc
    tick_non other	   acc = acc + 1

all_present_WwLazies :: [Demand] -> Bool
all_present_WwLazies infos
  = and (map is_L infos)
  where
    is_L (WwLazy False) = True	-- False <=> "Absent" args do *not* count!
    is_L _	        = False	-- (as they imply a worker)
\end{code}

WDP 95/04: It is no longer enough to look at a list of @Demands@ for
an ``Unpack'' or an ``Absent'' and declare a worker.  We also have to
check that @mAX_WORKER_ARGS@ hasn't been exceeded.  Therefore,
@indicatesWorker@ mirrors the process used in @mk_ww_arg_processing@
in \tr{WwLib.lhs}.  A worker is ``indicated'' when we hit an Unpack
or an Absent {\em that we accept}.
\begin{code}
indicatesWorker :: [Demand] -> Bool

indicatesWorker dems
  = fake_mk_ww (_trace "mAX_WORKER_ARGS" 6 - nonAbsentArgs dems) dems
  where
    fake_mk_ww _ [] = False
    fake_mk_ww _ (WwLazy True : _) = True -- we accepted an Absent
    fake_mk_ww extra_args (WwUnpack cmpnts : dems)
      | extra_args_now > 0 = True -- we accepted an Unpack
      where
	extra_args_now = extra_args + 1 - nonAbsentArgs cmpnts

    fake_mk_ww extra_args (_ : dems)
      = fake_mk_ww extra_args dems
\end{code}

\begin{code}
mkWrapperArgTypeCategories
	:: Type		-- wrapper's type
	-> [Demand]	-- info about its arguments
	-> String	-- a string saying lots about the args

mkWrapperArgTypeCategories wrapper_ty wrap_info
  = case (splitTypeWithDictsAsArgs wrapper_ty) of { (_,arg_tys,_) ->
    map do_one (wrap_info `zip` (map showTypeCategory arg_tys))
    }
  where
    -- ToDo: this needs FIXING UP (it was a hack anyway...)
    do_one (WwPrim, _) = 'P'
    do_one (WwEnum, _) = 'E'
    do_one (WwStrict, arg_ty_char) = arg_ty_char
    do_one (WwUnpack _, arg_ty_char)
      = if arg_ty_char `elem` "CIJFDTS"
	then toLower arg_ty_char
	else if arg_ty_char == '+' then 't'
	else trace ("mkWrapp..:funny char:"++[arg_ty_char]) '-'
    do_one (other_wrap_info, _) = '-'
\end{code}

Whether a worker exists depends on whether the worker has an
absent argument, a @WwUnpack@ argument, (or @WwEnum@ ToDo???) arguments.

If a @WwUnpack@ argument is for an {\em abstract} type (or one that
will be abstract outside this module), which might happen for an
imported function, then we can't (or don't want to...) unpack the arg
as the worker requires.  Hence we have to give up altogether, and call
the wrapper only; so under these circumstances we return \tr{False}.

\begin{code}
instance Text Demand where
    readList str = read_em [{-acc-}] str
      where
	read_em acc []		= [(reverse acc, "")]
	-- lower case indicates absence...
	read_em acc ('L' : xs)	= read_em (WwLazy   False : acc) xs
	read_em acc ('A' : xs)	= read_em (WwLazy   True  : acc) xs
	read_em acc ('S' : xs)	= read_em (WwStrict : acc) xs
	read_em acc ('P' : xs)	= read_em (WwPrim : acc) xs
	read_em acc ('E' : xs)	= read_em (WwEnum : acc) xs

	read_em acc (')' : xs)	= [(reverse acc, xs)]
	read_em acc ( 'U'  : '(' : xs)
	  = case (read_em [] xs) of
	      [(stuff, rest)] -> read_em (WwUnpack stuff : acc) rest
	      _ -> panic ("Text.Demand:"++str++"::"++xs)

	read_em acc other = panic ("IdInfo.readem:"++other)

    showList wrap_args rest = (concat (map show1 wrap_args)) ++ rest
      where
	show1 (WwLazy False) = "L"
	show1 (WwLazy True)  = "A"
	show1 WwStrict	     = "S"
	show1 WwPrim	     = "P"
	show1 WwEnum	     = "E"
	show1 (WwUnpack args)= "U(" ++ (concat (map show1 args)) ++ ")"

instance Outputable Demand where
    ppr sty si = ppStr (showList [si] "")

instance OptIdInfo StrictnessInfo where
    noInfo = NoStrictnessInfo

    getInfo (IdInfo _ _ _ strict _ _ _ _ _ _) = strict

    addInfo id_info NoStrictnessInfo = id_info
    addInfo (IdInfo a b d _ e f g h i j) strict = IdInfo a b d strict e f g h i j

    ppInfo sty better_id_fn strictness_info
      = pp_strictness sty Nothing better_id_fn nullIdEnv strictness_info
\end{code}

We'll omit the worker info if the thing has an explicit unfolding
already.
\begin{code}
pp_strictness sty _ _ _ NoStrictnessInfo = ifPprInterface sty pp_NONE

pp_strictness sty _ _ _ BottomGuaranteed = ppPStr SLIT("_S_ _!_")

pp_strictness sty for_this_id_maybe better_id_fn inline_env
    info@(StrictnessInfo wrapper_args wrkr_maybe)
  = let
	(have_wrkr, wrkr_id) = case wrkr_maybe of
				 Nothing -> (False, panic "ppInfo(Strictness)")
				 Just xx -> (True,  xx)

	wrkr_to_print   = better_id_fn wrkr_id
	wrkr_info	= getIdInfo   wrkr_to_print

	-- if we aren't going to be able to *read* the strictness info
	-- in TcPragmas, we need not even print it.
	wrapper_args_to_use
	  = if not (indicatesWorker wrapper_args) then
		wrapper_args -- no worker/wrappering in any case
	    else
		case for_this_id_maybe of
		  Nothing -> wrapper_args
		  Just id -> if externallyVisibleId id
			     && (unfoldingUnfriendlyId id || not have_wrkr) then
				-- pprTrace "IdInfo: unworker-ising:" (ppCat [ppr PprDebug have_wrkr, ppr PprDebug id]) $
				map un_workerise wrapper_args
			     else
				wrapper_args

	id_is_worker
	  = case for_this_id_maybe of
	      Nothing -> False
	      Just id -> isWorkerId id

	am_printing_iface = case sty of { PprInterface -> True ; _ -> False }

	pp_basic_info
	  = ppBesides [ppStr "_S_ \"",
		ppStr (showList wrapper_args_to_use ""), ppStr "\""]

	pp_with_worker
	  = ppBesides [ ppSP, ppChar '{',
			ppIdInfo sty wrkr_to_print True{-wrkr specs, yes!-} better_id_fn inline_env wrkr_info,
			ppChar '}' ]
    in
    if all_present_WwLazies wrapper_args_to_use then -- too boring
	ifPprInterface sty pp_NONE

    else if id_is_worker && am_printing_iface then
    	pp_NONE -- we don't put worker strictness in interfaces
		-- (it can be deduced)

    else if not (indicatesWorker wrapper_args_to_use)
	 || not have_wrkr
	 || boringIdInfo wrkr_info then
	ppBeside pp_basic_info ppNil
    else
	ppBeside pp_basic_info pp_with_worker
  where
    un_workerise (WwLazy   _) = WwLazy False -- avoid absence
    un_workerise (WwUnpack _) = WwStrict
    un_workerise other	      = other
\end{code}

%************************************************************************
%*									*
\subsection[unfolding-IdInfo]{Unfolding info about an @Id@}
%*									*
%************************************************************************

\begin{code}
mkUnfolding guide expr
  = GenForm False (mkFormSummary NoStrictnessInfo expr)
	(BSCC("OccurExpr") occurAnalyseGlobalExpr expr ESCC)
	guide
\end{code}

\begin{code}
noInfo_UF = NoUnfoldingDetails

getInfo_UF (IdInfo _ _ _ _ unfolding _ _ _ _ _)
  = case unfolding of
      GenForm _ _ _ BadUnfolding -> NoUnfoldingDetails
      unfolding_as_was 		     -> unfolding_as_was

-- getInfo_UF ensures that any BadUnfoldings are never returned
-- We had to delay the test required in TcPragmas until now due
-- to strictness constraints in TcPragmas

addInfo_UF id_info@(IdInfo a b c d e f g h i j) NoUnfoldingDetails = id_info
addInfo_UF   (IdInfo a b d e _ f g h i j) uf = IdInfo a b d e uf f g h i j
\end{code}

\begin{code}
pp_unfolding sty for_this_id inline_env uf_details
  = case (lookupIdEnv inline_env for_this_id) of
      Nothing -> pp uf_details
      Just dt -> pp dt
  where
    pp NoUnfoldingDetails = pp_NONE

    pp (MagicForm tag _)
      = ppCat [ppPStr SLIT("_MF_"), ppPStr tag]

    pp (GenForm _ _ _ BadUnfolding) = pp_NONE

    pp (GenForm _ _ template guide)
      = let
	    untagged = unTagBinders template
	in
    	if untagged `isWrapperFor` for_this_id
	then -- pprTrace "IdInfo:isWrapperFor:" (ppAbove (ppr PprDebug for_this_id) (ppr PprDebug untagged))
	     pp_NONE
	else ppCat [ppPStr SLIT("_F_"), ppr sty guide, pprCoreUnfolding untagged]

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
instance Text UpdateInfo where
    readsPrec p s | null s    = panic "IdInfo: empty update pragma?!"
		  | otherwise = [(SomeUpdateInfo (map ok_digit s),"")]
      where
	ok_digit c | c >= '0' && c <= '2' = ord c - ord '0'
		   | otherwise = panic "IdInfo: not a digit while reading update pragma"

instance OptIdInfo UpdateInfo where
    noInfo = NoUpdateInfo

    getInfo (IdInfo _ _ _ _ _ update _ _ _ _) = update

    addInfo id_info NoUpdateInfo = id_info
    addInfo (IdInfo a b d e f _ g h i j) upd_info = IdInfo a b d e f upd_info g h i j

    ppInfo sty better_id_fn NoUpdateInfo	= ifPprInterface sty pp_NONE
    ppInfo sty better_id_fn (SomeUpdateInfo [])	= ifPprInterface sty pp_NONE
    ppInfo sty better_id_fn (SomeUpdateInfo spec)
      = ppBeside (ppPStr SLIT("_U_ ")) (ppBesides (map ppInt spec))
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
instance OptIdInfo DeforestInfo where
    noInfo = Don'tDeforest

    getInfo (IdInfo _ _ _ _ _ _ deforest _ _ _) = deforest

    addInfo id_info Don'tDeforest = id_info
    addInfo (IdInfo a b d e f g _ h i j) deforest =
    	IdInfo a b d e f g deforest h i j

    ppInfo sty better_id_fn Don'tDeforest
      = ifPprInterface sty pp_NONE
    ppInfo sty better_id_fn DoDeforest
      = ppPStr SLIT("_DEFOREST_")
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
mkArgUsageInfo = SomeArgUsageInfo

getArgUsage :: ArgUsageInfo -> ArgUsageType
getArgUsage NoArgUsageInfo	    = []
getArgUsage (SomeArgUsageInfo u)  = u
\end{code}

\begin{code}
instance OptIdInfo ArgUsageInfo where
    noInfo = NoArgUsageInfo

    getInfo (IdInfo _ _ _ _ _  _ _ au _ _) = au

    addInfo id_info NoArgUsageInfo = id_info
    addInfo (IdInfo a b d e f g h _ i j) au_info = IdInfo a b d e f g h au_info i j

    ppInfo sty better_id_fn NoArgUsageInfo		= ifPprInterface sty pp_NONE
    ppInfo sty better_id_fn (SomeArgUsageInfo [])	= ifPprInterface sty pp_NONE
    ppInfo sty better_id_fn (SomeArgUsageInfo aut)
      = ppBeside (ppPStr SLIT("_L_ ")) (ppArgUsageType aut)


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
  -- ??? deriving (Eq, Ord)

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
instance OptIdInfo FBTypeInfo where
    noInfo = NoFBTypeInfo

    getInfo (IdInfo _ _ _ _ _ _ _ _ fb _) = fb

    addInfo id_info NoFBTypeInfo = id_info
    addInfo (IdInfo a b d e f g h i _ j) fb_info = IdInfo a b d e f g h i fb_info j

    ppInfo PprInterface _ NoFBTypeInfo = ppNil
    ppInfo sty 		_ NoFBTypeInfo = ifPprInterface sty pp_NONE
    ppInfo sty 		_ (SomeFBTypeInfo (FBType cons prod))
      = ppBeside (ppPStr SLIT("_F_ ")) (ppFBType cons prod)

--ppFBType (FBType n)      = ppBesides [ppInt n]
--ppFBType (UnknownFBType) = ppBesides [ppStr "-"]
--

ppFBType cons prod = ppBesides
	([ ppChar '"' ] ++ map ppCons cons ++ [ ppChar '-', ppProd prod, ppChar '"' ])
  where
	ppCons FBGoodConsum = ppChar 'G'
	ppCons FBBadConsum  = ppChar 'B'
	ppProd FBGoodProd   = ppChar 'G'
	ppProd FBBadProd    = ppChar 'B'
\end{code}
