%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Util]{Highly random utility functions}

\begin{code}
#if defined(COMPILING_GHC)
# include "HsVersions.h"
# define IF_NOT_GHC(a) {--}
#else
# define panic error
# define TAG_ _CMP_TAG
# define LT_ _LT
# define EQ_ _EQ
# define GT_ _GT
# define GT__ _
# define tagCmp_ _tagCmp
# define FAST_STRING String
# define ASSERT(x) {-nothing-}
# define IF_NOT_GHC(a) a
# define COMMA ,
#endif

#ifndef __GLASGOW_HASKELL__
# undef TAG_
# undef LT_
# undef EQ_
# undef GT_
# undef tagCmp_
#endif

module Util (
	-- Haskell-version support
#ifndef __GLASGOW_HASKELL__
	tagCmp_,
	TAG_(..),
#endif
	-- general list processing
	IF_NOT_GHC(forall COMMA exists COMMA)
	zipEqual, nOfThem, lengthExceeds, isSingleton,
#if defined(COMPILING_GHC)
	isIn, isn'tIn,
#endif

	-- association lists
	assoc,
#ifdef USE_SEMANTIQUE_STRANAL
	clookup, clookrepl, elemIndex, (\\\),
#endif

	-- duplicate handling
	hasNoDups, equivClasses, runs, removeDups,

	-- sorting
	IF_NOT_GHC(quicksort COMMA stableSortLt COMMA mergesort COMMA)
	sortLt,
	IF_NOT_GHC(mergeSort COMMA) naturalMergeSortLe,	-- from Carsten
	IF_NOT_GHC(naturalMergeSort COMMA mergeSortLe COMMA)

	-- transitive closures
	transitiveClosure,

	-- accumulating
	mapAccumL, mapAccumR, mapAccumB,

	-- comparisons
	IF_NOT_GHC(cmpString COMMA)
#ifdef USE_FAST_STRINGS
	cmpPString,
#else
	substr,
#endif
	-- pairs
	IF_NOT_GHC(cfst COMMA applyToPair COMMA applyToFst COMMA)
	IF_NOT_GHC(applyToSnd COMMA foldPair COMMA)
	unzipWith

	-- error handling
#if defined(COMPILING_GHC)
	, panic, pprPanic, pprTrace
# ifdef DEBUG
	, assertPanic
# endif
#endif {- COMPILING_GHC -}

	-- and to make the interface self-sufficient...
#if __HASKELL1__ < 3
# if defined(COMPILING_GHC)
	, Maybe(..){-.. for pragmas...-}, PrettyRep, Pretty(..)
# else
	, Maybe
# endif
#endif

#ifdef USE_ATTACK_PRAGMAS
	-- as more-or-less of a *HACK*, Util exports
	-- many types abstractly, so that pragmas will be
	-- able to see them (given that most modules
	-- import Util).
	,
	AbstractC,
	ArgUsage,
	ArgUsageInfo,
	ArithSeqInfo,
	ArityInfo,
	Bag,
	BasicLit,
	Bind,
	BinderInfo,
	Binds,
	CAddrMode,
	CExprMacro,
	CLabel,
	CSeq,
	CStmtMacro,
	CcKind,
	Class,
	ClassDecl,
	ClassOp,
	ClassOpPragmas,
	ClassPragmas,
	ClosureInfo,
	ConDecl,
	CoreArg,
	CoreAtom,
	CoreBinding,
	CoreCaseAlternatives,
	CoreCaseDefault,
	CoreExpr,
	CostCentre,
	DataPragmas,
	DataTypeSig,
	DefaultDecl,
	DeforestInfo,
	Delay,
	Demand,
	DemandInfo,
	DuplicationDanger,
	EnclosingCcDetails,
	EndOfBlockInfo,
	ExportFlag,
	Expr,
	FBConsum,
	FBProd,
	FBType,
	FBTypeInfo,
	FiniteMap,
	FixityDecl,
	FormSummary,
	FullName,
	FunOrArg,
	GRHS,
	GRHSsAndBinds,
	GenPragmas,
	GlobalSwitch,
	HeapOffset,
	IE,
	Id,
	IdDetails,
	IdEnv(..), -- UGH
	IdInfo,
	IdVal,
	IfaceImportDecl,
	ImpStrictness,
	ImpUnfolding,
	ImportedInterface,
	InPat,
	InsideSCC,
	Inst,
	InstDecl,
	InstOrigin,
	InstTemplate,
	InstTy,
	InstancePragmas,
	Interface,
	IsDupdCC, IsCafCC,
	LambdaFormInfo,
	Literal,
	MagicId,
	MagicUnfoldingFun,
	Match,
	Module,
	MonoBinds,
	MonoType,
	Name,
	NamedThing(..), -- SIGH
	OptIdInfo(..), -- SIGH
	OrdList,
	Outputable(..), -- SIGH
	OverloadedLit,
	PolyType,
	PprStyle,
	PrimKind,
	PrimOp,
	ProtoName,
	Provenance,
	Qual,
	RegRelative,
	Renaming,
	ReturnInfo,
	SMRep,
	SMSpecRepKind,
	SMUpdateKind,
	Sequel,
	ShortName,
	Sig,
	SimplCount,
	SimplEnv,
	SimplifierSwitch,
	SpecEnv,
	SpecInfo,
	SpecialisedInstanceSig,
	SplitUniqSupply,
	SrcLoc,
	StableLoc,
	StandardFormInfo,
	StgAtom,
	StgBinderInfo,
	StgBinding,
	StgCaseAlternatives,
	StgCaseDefault,
	StgExpr,
	StgRhs,
	StrictnessInfo,
	StubFlag,
	SwitchResult,
	TickType,
	TyCon,
	TyDecl,
	TyVar,
	TyVarEnv(..),
	TyVarTemplate,
	TypePragmas,
	TypecheckedPat,
	UfCostCentre,
	UfId,
	UnfoldEnv,
	UnfoldItem,
	UnfoldConApp,
	UnfoldingCoreAlts,
	UnfoldingCoreAtom,
	UnfoldingCoreBinding,
	UnfoldingCoreDefault,
	UnfoldingCoreExpr,
	UnfoldingDetails,
	UnfoldingGuidance,
	UnfoldingPrimOp,
	UniType,
	UniqFM,
	Unique,
	UniqueSupply,
	UpdateFlag,
	UpdateInfo,
	VolatileLoc,

#if ! OMIT_NATIVE_CODEGEN
	Reg,
	CodeSegment,
	RegLoc,
	StixReg,
	StixTree,
#endif

	getIdUniType, typeOfBasicLit, typeOfPat,
	getIdKind, kindOfBasicLit,
	kindFromType,

	eqId, cmpId,
	eqName, cmpName,
	cmpProtoName, eqProtoName,
	cmpByLocalName, eqByLocalName,
	eqUnique, cmpUnique,
	showUnique,

	switchIsOn,

	ppNil, ppStr, ppInt, ppInteger, ppDouble,
#if __GLASGOW_HASKELL__ >= 23
	ppRational, --- ???
#endif
	cNil, cStr, cAppend, cCh, cShow,
#if __GLASGOW_HASKELL__ >= 23
	cPStr,
#endif

--	mkBlackHoleCLabel,

	emptyBag, snocBag,
	emptyFM,
--OLD:	emptySet,
	nullSpecEnv,
	
	mkUnknownSrcLoc,
	
	pprCoreBinding, pprCoreExpr, pprTyCon, pprUniType,

	tagOf_PrimOp,
	pprPrimOp

#endif {-USE_ATTACK_PRAGMAS-}
    ) where

#if defined(COMPILING_GHC)
IMPORT_Trace
import Pretty
#endif
#if __HASKELL1__ < 3
import Maybes		( Maybe(..) )
#endif

#if defined(COMPILING_GHC)
import Id
import IdInfo
import Outputable

# ifdef USE_ATTACK_PRAGMAS

import AbsCSyn
import AbsSyn
import AbsUniType
import Bag
import BasicLit
import BinderInfo
import CLabelInfo
import CgBindery
import CgMonad
import CharSeq
import ClosureInfo
import CmdLineOpts
import CoreSyn
import FiniteMap
import HsCore
import HsPragmas
import Inst
import InstEnv
import Name
import NameTypes
import OrdList
import PlainCore
import PrimOps
import ProtoName
import CostCentre
import SMRep
import SimplEnv
import SimplMonad
import SplitUniq
import SrcLoc
import StgSyn
import TyVarEnv
import UniqFM
import Unique

#  if ! OMIT_NATIVE_CODEGEN
import AsmRegAlloc	( Reg )
import MachDesc
import Stix
#  endif

# endif {-USE_ATTACK_PRAGMAS-}

#endif
\end{code}

%************************************************************************
%*									*
\subsection[Utils-version-support]{Functions to help pre-1.2 versions of (non-Glasgow) Haskell}
%*									*
%************************************************************************

This is our own idea:
\begin{code}
#ifndef __GLASGOW_HASKELL__
data TAG_ = LT_ | EQ_ | GT_

tagCmp_ :: Ord a => a -> a -> TAG_
tagCmp_ a b = if a == b then EQ_ else if a < b then LT_ else GT_
#endif
\end{code}

%************************************************************************
%*									*
\subsection[Utils-lists]{General list processing}
%*									*
%************************************************************************

Quantifiers are not standard in Haskell. The following fill in the gap.

\begin{code}
forall :: (a -> Bool) -> [a] -> Bool
forall pred []     = True
forall pred (x:xs) = pred x && forall pred xs

exists :: (a -> Bool) -> [a] -> Bool
exists pred []     = False
exists pred (x:xs) = pred x || exists pred xs
\end{code}

A paranoid @zip@ that checks the lists are of equal length.
Alastair Reid thinks this should only happen if DEBUGging on;
hey, why not?

\begin{code}
zipEqual :: [a] -> [b] -> [(a,b)]

#ifndef DEBUG
zipEqual a b = zip a b
#else
zipEqual []     []     = []
zipEqual (a:as) (b:bs) = (a,b) : zipEqual as bs
zipEqual as     bs     = panic "zipEqual: unequal lists"
#endif
\end{code}

\begin{code}
nOfThem :: Int -> a -> [a]
nOfThem n thing = take n (repeat thing)

lengthExceeds :: [a] -> Int -> Bool

[]	`lengthExceeds` n =  0 > n
(x:xs)	`lengthExceeds` n = (1 > n) || (xs `lengthExceeds` (n - 1))

isSingleton :: [a] -> Bool

isSingleton [x] = True
isSingleton  _  = False
\end{code}

Debugging/specialising versions of \tr{elem} and \tr{notElem}
\begin{code}
#if defined(COMPILING_GHC)
isIn, isn'tIn :: (Eq a) => String -> a -> [a] -> Bool

# ifndef DEBUG
isIn    msg x ys = elem__    x ys
isn'tIn msg x ys = notElem__ x ys

--these are here to be SPECIALIZEd (automagically)
elem__ _ []	= False
elem__ x (y:ys)	= x==y || elem__ x ys

notElem__ x []	   =  True
notElem__ x (y:ys) =  x /= y && notElem__ x ys

# else {- DEBUG -}
isIn msg x ys
  = elem ILIT(0) x ys
  where
    elem i _ []	    = False
    elem i x (y:ys)
      | i _GE_ ILIT(100) = panic ("Over-long elem in: " ++ msg)
      | otherwise	 = x == y || elem (i _ADD_ ILIT(1)) x ys

isn'tIn msg x ys
  = notElem ILIT(0) x ys
  where
    notElem i x [] =  True
    notElem i x (y:ys)
      | i _GE_ ILIT(100) = panic ("Over-long notElem in: " ++ msg)
      | otherwise	 =  x /= y && notElem (i _ADD_ ILIT(1)) x ys

# endif {- DEBUG -}

# ifdef USE_ATTACK_PRAGMAS
{-# SPECIALIZE isIn :: String -> BasicLit -> [BasicLit] -> Bool #-}
{-# SPECIALIZE isIn :: String -> Class -> [Class] -> Bool #-}
{-# SPECIALIZE isIn :: String -> Id -> [Id] -> Bool #-}
{-# SPECIALIZE isIn :: String -> Int -> [Int] -> Bool #-}
{-# SPECIALIZE isIn :: String -> MagicId -> [MagicId] -> Bool #-}
{-# SPECIALIZE isIn :: String -> Name -> [Name] -> Bool #-}
{-# SPECIALIZE isIn :: String -> TyCon -> [TyCon] -> Bool #-}
{-# SPECIALIZE isIn :: String -> TyVar -> [TyVar] -> Bool #-}
{-# SPECIALIZE isIn :: String -> TyVarTemplate -> [TyVarTemplate] -> Bool #-}
{-# SPECIALIZE isIn :: String -> Unique -> [Unique] -> Bool #-}
{-# SPECIALIZE isIn :: String -> _PackedString -> [_PackedString] -> Bool #-}
{-# SPECIALIZE isn'tIn :: String -> (Id, Id) -> [(Id, Id)] -> Bool #-}
{-# SPECIALIZE isn'tIn :: String -> Int -> [Int] -> Bool #-}
{-# SPECIALIZE isn'tIn :: String -> Id -> [Id] -> Bool #-}
{-# SPECIALIZE isn'tIn :: String -> MagicId -> [MagicId] -> Bool #-}
{-# SPECIALIZE isn'tIn :: String -> TyCon -> [TyCon] -> Bool #-}
{-# SPECIALIZE isn'tIn :: String -> TyVar -> [TyVar] -> Bool #-}
{-# SPECIALIZE isn'tIn :: String -> TyVarTemplate -> [TyVarTemplate] -> Bool #-}
# endif

#endif {- COMPILING_GHC -}
\end{code}

%************************************************************************
%*									*
\subsection[Utils-assoc]{Association lists}
%*									*
%************************************************************************

See also @assocMaybe@ and @mkLookupFun@ in module @Maybes@.

\begin{code}
assoc :: (Eq a) => String -> [(a, b)] -> a -> b

assoc crash_msg lst key
  = if (null res)
    then panic ("Failed in assoc: " ++ crash_msg)
    else head res
  where res = [ val | (key', val) <- lst, key == key']

#if defined(COMPILING_GHC)
# ifdef USE_ATTACK_PRAGMAS
{-# SPECIALIZE assoc :: String -> [(Id,            a)] -> Id		-> a #-}
{-# SPECIALIZE assoc :: String -> [(Class,         a)] -> Class		-> a #-}
{-# SPECIALIZE assoc :: String -> [(Name,          a)] -> Name		-> a #-}
{-# SPECIALIZE assoc :: String -> [(PrimKind,      a)] -> PrimKind	-> a #-}
{-# SPECIALIZE assoc :: String -> [(String,        a)] -> String        -> a #-}
{-# SPECIALIZE assoc :: String -> [(TyCon,         a)] -> TyCon		-> a #-}
{-# SPECIALIZE assoc :: String -> [(TyVar,         a)] -> TyVar		-> a #-}
{-# SPECIALIZE assoc :: String -> [(TyVarTemplate, a)] -> TyVarTemplate -> a #-}
{-# SPECIALIZE assoc :: String -> [(UniType,       a)] -> UniType	-> a #-}
{-# SPECIALIZE assoc :: String -> [(_PackedString, a)] -> _PackedString -> a #-}
# endif
#endif
\end{code}

Given a list of associations one wants to look for the most recent
association for a given key. A couple of functions follow that cover
the simple lookup, the lookup with a default value when the key not
found, and two corresponding functions operating on unzipped lists
of associations.

\begin{code}
#ifdef USE_SEMANTIQUE_STRANAL

clookup :: (Eq a) => [a] -> [b] -> a -> b
clookup = clookupElse (panic "clookup")
  where
   -- clookupElse :: (Eq a) => b -> [a] -> [b] -> a -> b
   clookupElse d [] [] a = d
   clookupElse d (x:xs) (y:ys) a
                | a==x = y
                | True = clookupElse d xs ys a
#endif
\end{code}

The following routine given a curried environment replaces the entry
labelled with a given name with a new value given. The new value is
given in the form of a function that allows to transform the old entry.

Assumption is that the list of labels contains the given one and that
the two lists of the curried environment are of equal lengths.

\begin{code}
#ifdef USE_SEMANTIQUE_STRANAL
clookrepl :: Eq a => [a] -> [b] -> a -> (b -> b) -> [b]
clookrepl (a:as) (b:bs) x f
   = if x == a then  (f b:bs)  else  (b:clookrepl as bs x f)
#endif
\end{code}

The following returns the index of an element in a list.

\begin{code}
#ifdef USE_SEMANTIQUE_STRANAL

elemIndex :: Eq a => [a] -> a -> Int
elemIndex as x = indx as x 0
   where
     indx :: Eq a => [a] -> a -> Int -> Int
     indx (a:as) x n = if a==x then n else indx as x ((n+1)::Int)
# if defined(COMPILING_GHC)
     indx [] x n     = pprPanic "element not in list in elemIndex" ppNil
# else
     indx [] x n     = error "element not in list in elemIndex"
# endif
#endif
\end{code}

%************************************************************************
%*									*
\subsection[Utils-dups]{Duplicate-handling}
%*									*
%************************************************************************

List difference (non-associative). In the result of @xs \\\ ys@, the
first occurrence of each element of ys in turn (if any) has been
removed from xs.  Thus, @(xs ++ ys) \\\ xs == ys@.  This function is
a copy of @\\@ from report 1.1 and is added to overshade the buggy
version from the 1.0 version of Haskell.

This routine can be removed after the compiler bootstraps itself and
a proper @\\@ is can be applied.

\begin{code}
#ifdef USE_SEMANTIQUE_STRANAL
(\\\) :: (Eq a) => [a] -> [a] -> [a]
(\\\) =  foldl del
   where
    []     `del` _ = []
    (x:xs) `del` y
	| x == y    = xs
	| otherwise = x : xs `del` y
#endif
\end{code}

\begin{code}
hasNoDups :: (Eq a) => [a] -> Bool
hasNoDups xs = f [] xs
  where
    f seen_so_far []     = True
    f seen_so_far (x:xs) = if x `is_elem` seen_so_far then
				False
			   else
				f (x:seen_so_far) xs

#if defined(COMPILING_GHC)
    is_elem = isIn "hasNoDups"
#else
    is_elem = elem
#endif
#if defined(COMPILING_GHC)
# ifdef USE_ATTACK_PRAGMAS
{-# SPECIALIZE hasNoDups :: [TyVar] -> Bool #-}
# endif
#endif
\end{code}

\begin{code}
equivClasses :: (a -> a -> TAG_) 	-- Comparison
	     -> [a] 
	     -> [[a]]

equivClasses cmp stuff@[]     = []
equivClasses cmp stuff@[item] = [stuff]
equivClasses cmp items
  = runs eq (sortLt lt items)
  where
    eq a b = case cmp a b of { EQ_ -> True; _ -> False }
    lt a b = case cmp a b of { LT_ -> True; _ -> False }
\end{code}

The first cases in @equivClasses@ above are just to cut to the point
more quickly...

@runs@ groups a list into a list of lists, each sublist being a run of
identical elements of the input list. It is passed a predicate @p@ which
tells when two elements are equal.

\begin{code}
runs :: (a -> a -> Bool) 	-- Equality 
     -> [a] 
     -> [[a]]

runs p []     = []
runs p (x:xs) = case (span (p x) xs) of
		  (first, rest) -> (x:first) : (runs p rest)
\end{code}

\begin{code}
removeDups :: (a -> a -> TAG_) 	-- Comparison function
	   -> [a]
	   -> ([a], 	-- List with no duplicates
	       [[a]])	-- List of duplicate groups.  One representative from
			-- each group appears in the first result

removeDups cmp []  = ([], [])
removeDups cmp [x] = ([x],[])
removeDups cmp xs
  = case (mapAccumR collect_dups [] (equivClasses cmp xs)) of { (dups, xs') ->
    (xs', dups) }
  where
    collect_dups dups_so_far [x]         = (dups_so_far,      x)
    collect_dups dups_so_far dups@(x:xs) = (dups:dups_so_far, x)
\end{code}

%************************************************************************
%*									*
\subsection[Utils-sorting]{Sorting}
%*									*
%************************************************************************

%************************************************************************
%*									*
\subsubsection[Utils-quicksorting]{Quicksorts}
%*									*
%************************************************************************

\begin{code}
-- tail-recursive, etc., "quicker sort" [as per Meira thesis]
quicksort :: (a -> a -> Bool)		-- Less-than predicate
	  -> [a]			-- Input list
	  -> [a]			-- Result list in increasing order

quicksort lt []      = []
quicksort lt [x]     = [x]
quicksort lt (x:xs)  = split x [] [] xs
  where
    split x lo hi []		     = quicksort lt lo ++ (x : quicksort lt hi)
    split x lo hi (y:ys) | y `lt` x  = split x (y:lo) hi ys
			 | True      = split x lo (y:hi) ys
\end{code}

Quicksort variant from Lennart's Haskell-library contribution.  This
is a {\em stable} sort.

\begin{code}
stableSortLt = sortLt	-- synonym; when we want to highlight stable-ness

sortLt :: (a -> a -> Bool) 		-- Less-than predicate
       -> [a] 				-- Input list
       -> [a]				-- Result list

sortLt lt l = qsort lt   l []

-- qsort is stable and does not concatenate.
qsort :: (a -> a -> Bool)	-- Less-than predicate
      -> [a]			-- xs, Input list
      -> [a]			-- r,  Concatenate this list to the sorted input list
      -> [a]			-- Result = sort xs ++ r

qsort lt []     r = r
qsort lt [x]    r = x:r
qsort lt (x:xs) r = qpart lt x xs [] [] r

-- qpart partitions and sorts the sublists
-- rlt contains things less than x, 
-- rge contains the ones greater than or equal to x.
-- Both have equal elements reversed with respect to the original list.

qpart lt x [] rlt rge r =
    -- rlt and rge are in reverse order and must be sorted with an
    -- anti-stable sorting
    rqsort lt rlt (x : rqsort lt rge r)

qpart lt x (y:ys) rlt rge r =
    if lt y x then
	-- y < x
	qpart lt x ys (y:rlt) rge r
    else	
	-- y >= x
	qpart lt x ys rlt (y:rge) r

-- rqsort is as qsort but anti-stable, i.e. reverses equal elements
rqsort lt []     r = r
rqsort lt [x]    r = x:r
rqsort lt (x:xs) r = rqpart lt x xs [] [] r

rqpart lt x [] rle rgt r =
    qsort lt rle (x : qsort lt rgt r)

rqpart lt x (y:ys) rle rgt r =
    if lt x y then
	-- y > x
	rqpart lt x ys rle (y:rgt) r
    else
	-- y <= x
	rqpart lt x ys (y:rle) rgt r
\end{code}

%************************************************************************
%*									*
\subsubsection[Utils-dull-mergesort]{A rather dull mergesort}
%*									*
%************************************************************************

\begin{code}
mergesort :: (a -> a -> TAG_) -> [a] -> [a]

mergesort cmp xs = merge_lists (split_into_runs [] xs)
  where
    a `le` b = case cmp a b of { LT_ -> True;  EQ_ -> True; GT__ -> False }
    a `ge` b = case cmp a b of { LT_ -> False; EQ_ -> True; GT__ -> True  }

    split_into_runs []        []	    	= []
    split_into_runs run       []	    	= [run]
    split_into_runs []        (x:xs)		= split_into_runs [x] xs
    split_into_runs [r]       (x:xs) | x `ge` r = split_into_runs [r,x] xs
    split_into_runs rl@(r:rs) (x:xs) | x `le` r = split_into_runs (x:rl) xs
				     | True     = rl : (split_into_runs [x] xs)

    merge_lists []	 = []
    merge_lists (x:xs)   = merge x (merge_lists xs)

    merge [] ys = ys
    merge xs [] = xs
    merge xl@(x:xs) yl@(y:ys)
      = case cmp x y of
	  EQ_  -> x : y : (merge xs ys)
	  LT_  -> x : (merge xs yl)
	  GT__ -> y : (merge xl ys)
\end{code}

%************************************************************************
%*									*
\subsubsection[Utils-Carsten-mergesort]{A mergesort from Carsten}
%*									*
%************************************************************************

\begin{display}
Date: Mon, 3 May 93 20:45:23 +0200
From: Carsten Kehler Holst <kehler@cs.chalmers.se>
To: partain@dcs.gla.ac.uk
Subject: natural merge sort beats quick sort [ and it is prettier ]

Here a piece of Haskell code that I'm rather fond of. See it as an
attempt to get rid of the ridiculous quick-sort routine. group is
quite useful by itself I think it was John's idea originally though I
believe the lazy version is due to me [surprisingly complicated].
gamma [used to be called] is called gamma because I got inspired by
the Gamma calculus. It is not very close to the calculus but does
behave less sequentially than both foldr and foldl. One could imagine a
version of gamma that took a unit element as well thereby avoiding the
problem with empty lists.

I've tried this code against

   1) insertion sort - as provided by haskell
   2) the normal implementation of quick sort
   3) a deforested version of quick sort due to Jan Sparud
   4) a super-optimized-quick-sort of Lennart's

If the list is partially sorted both merge sort and in particular
natural merge sort wins. If the list is random [ average length of
rising subsequences = approx 2 ] mergesort still wins and natural
merge sort is marginally beaten by Lennart's soqs. The space
consumption of merge sort is a bit worse than Lennart's quick sort
approx a factor of 2. And a lot worse if Sparud's bug-fix [see his
fpca article ] isn't used because of group.

have fun 
Carsten
\end{display}

\begin{code}
group :: (a -> a -> Bool) -> [a] -> [[a]]

group p [] = [[]]
group p (x:xs) = 
   let ((h1:t1):tt1) = group p xs
       (t,tt) = if null xs then ([],[]) else
                if x `p` h1 then (h1:t1,tt1) else 
                   ([], (h1:t1):tt1)
   in ((x:t):tt)

generalMerge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
generalMerge p xs [] = xs
generalMerge p [] ys = ys
generalMerge p (x:xs) (y:ys) | x `p` y   = x : generalMerge p xs (y:ys)
                             | otherwise = y : generalMerge p (x:xs) ys

-- gamma is now called balancedFold

balancedFold :: (a -> a -> a) -> [a] -> a
balancedFold f [] = error "can't reduce an empty list using balancedFold"
balancedFold f [x] = x
balancedFold f l  = balancedFold f (balancedFold' f l)

balancedFold' :: (a -> a -> a) -> [a] -> [a]
balancedFold' f (x:y:xs) = f x y : balancedFold' f xs
balancedFold' f xs = xs

generalMergeSort p [] = []
generalMergeSort p xs = (balancedFold (generalMerge p) . map (: [])) xs

generalNaturalMergeSort p [] = []
generalNaturalMergeSort p xs = (balancedFold (generalMerge p) . group p) xs

mergeSort, naturalMergeSort :: Ord a => [a] -> [a]

mergeSort = generalMergeSort (<=)
naturalMergeSort = generalNaturalMergeSort (<=)

mergeSortLe le = generalMergeSort le
naturalMergeSortLe le = generalNaturalMergeSort le
\end{code}

%************************************************************************
%*									*
\subsection[Utils-transitive-closure]{Transitive closure}
%*									*
%************************************************************************

This algorithm for transitive closure is straightforward, albeit quadratic.

\begin{code}
transitiveClosure :: (a -> [a])		-- Successor function
		  -> (a -> a -> Bool)	-- Equality predicate
		  -> [a] 
		  -> [a]		-- The transitive closure

transitiveClosure succ eq xs
 = do [] xs
 where
   do done [] 			   = done
   do done (x:xs) | x `is_in` done = do done xs
   		  | otherwise      = do (x:done) (succ x ++ xs)

   x `is_in` []                 = False
   x `is_in` (y:ys) | eq x y    = True
  		    | otherwise = x `is_in` ys
\end{code}

%************************************************************************
%*									*
\subsection[Utils-accum]{Accumulating}
%*									*
%************************************************************************

@mapAccumL@ behaves like a combination
of  @map@ and @foldl@;
it applies a function to each element of a list, passing an accumulating
parameter from left to right, and returning a final value of this
accumulator together with the new list.

\begin{code}
mapAccumL :: (acc -> x -> (acc, y)) 	-- Function of elt of input list
					-- and accumulator, returning new
					-- accumulator and elt of result list
	    -> acc 		-- Initial accumulator
	    -> [x] 		-- Input list
	    -> (acc, [y])		-- Final accumulator and result list

mapAccumL f b []     = (b, [])
mapAccumL f b (x:xs) = (b'', x':xs') where
					  (b', x') = f b x
					  (b'', xs') = mapAccumL f b' xs
\end{code}

@mapAccumR@ does the same, but working from right to left instead.  Its type is
the same as @mapAccumL@, though.

\begin{code}
mapAccumR :: (acc -> x -> (acc, y)) 	-- Function of elt of input list
					-- and accumulator, returning new
					-- accumulator and elt of result list
	    -> acc 		-- Initial accumulator
	    -> [x] 		-- Input list
	    -> (acc, [y])		-- Final accumulator and result list

mapAccumR f b []     = (b, [])
mapAccumR f b (x:xs) = (b'', x':xs') where
					  (b'', x') = f b' x
					  (b', xs') = mapAccumR f b xs
\end{code}

Here is the bi-directional version, that works from both left and right.

\begin{code}
mapAccumB :: (accl -> accr -> x -> (accl, accr,y))
      				-- Function of elt of input list
      				-- and accumulator, returning new
      				-- accumulator and elt of result list
          -> accl 			-- Initial accumulator from left
          -> accr 			-- Initial accumulator from right
          -> [x] 			-- Input list
          -> (accl, accr, [y])	-- Final accumulators and result list

mapAccumB f a b []     = (a,b,[])
mapAccumB f a b (x:xs) = (a'',b'',y:ys)
   where
	(a',b'',y)  = f a b' x
	(a'',b',ys) = mapAccumB f a' b xs
\end{code}

%************************************************************************
%*									*
\subsection[Utils-comparison]{Comparisons}
%*									*
%************************************************************************

See also @tagCmp_@ near the versions-compatibility section.

\begin{code}
cmpString :: String -> String -> TAG_

cmpString []     []	= EQ_
cmpString (x:xs) (y:ys) = if	  x == y then cmpString xs ys
			  else if x  < y then LT_
			  else		      GT_
cmpString []     ys	= LT_
cmpString xs     []	= GT_

cmpString _ _ = case (panic "cmpString") of { s -> -- BUG avoidance: never get here
		cmpString s "" -- will never get here
		}
\end{code}

\begin{code}
#ifdef USE_FAST_STRINGS
cmpPString :: FAST_STRING -> FAST_STRING -> TAG_

cmpPString x y
  = case (_tagCmp x y) of { _LT -> LT_ ; _EQ -> EQ_ ; _GT -> GT_ }
#endif
\end{code}

\begin{code}
#ifndef USE_FAST_STRINGS
substr :: FAST_STRING -> Int -> Int -> FAST_STRING

substr str beg end
  = ASSERT (beg >= 0 && beg <= end)
    take (end - beg + 1) (drop beg str)
#endif
\end{code}

%************************************************************************
%*									*
\subsection[Utils-pairs]{Pairs}
%*									*
%************************************************************************

The following are curried versions of @fst@ and @snd@.

\begin{code}
cfst :: a -> b -> a	-- stranal-sem only (Note)
cfst x y = x
\end{code}

The following provide us higher order functions that, when applied
to a function, operate on pairs.

\begin{code}
applyToPair :: ((a -> c),(b -> d)) -> (a,b) -> (c,d)
applyToPair (f,g) (x,y) = (f x, g y)

applyToFst :: (a -> c) -> (a,b)-> (c,b)
applyToFst f (x,y) = (f x,y)

applyToSnd :: (b -> d) -> (a,b) -> (a,d)
applyToSnd f (x,y) = (x,f y)

foldPair :: (a->a->a,b->b->b) -> (a,b) -> [(a,b)] -> (a,b)
foldPair fg ab [] = ab
foldPair fg@(f,g) ab ((a,b):abs) = (f a u,g b v)
                       where (u,v) = foldPair fg ab abs
\end{code}

\begin{code}
unzipWith :: (a -> b -> c) -> [(a, b)] -> [c]
unzipWith f pairs = map ( \ (a, b) -> f a b ) pairs
\end{code}

%************************************************************************
%*									*
\subsection[Utils-errors]{Error handling}
%*									*
%************************************************************************

\begin{code}
#if defined(COMPILING_GHC)
panic x = error ("panic! (the `impossible' happened):\n\t"
	      ++ x ++ "\n\n"
	      ++ "Please report it as a compiler bug "
	      ++ "to glasgow-haskell-bugs@dcs.glasgow.ac.uk.\n\n" )

pprPanic heading pretty_msg = panic (heading++(ppShow 80 pretty_msg))

pprTrace heading pretty_msg = trace (heading++(ppShow 80 pretty_msg))

# ifdef DEBUG
assertPanic :: String -> Int -> a
assertPanic file line = panic ("ASSERT failed! file "++file++", line "++show line)
# endif
#endif {- COMPILING_GHC -}
\end{code}
