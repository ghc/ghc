%
% (c) The AQUA Project, Glasgow University, 1993-1995
%
\section[SimplMonad]{The simplifier Monad}

\begin{code}
#include "HsVersions.h"

module SimplMonad (
	SmplM(..),
	initSmpl, returnSmpl, thenSmpl, thenSmpl_,
	mapSmpl, mapAndUnzipSmpl,
	
	-- Counting
	SimplCount{-abstract-}, TickType(..), tick, tickN,
	simplCount, detailedSimplCount,
	zeroSimplCount, showSimplCount, combineSimplCounts,

	-- Cloning
	cloneId, cloneIds, cloneTyVarSmpl, newIds, newId,

	-- and to make the interface self-sufficient...
	BinderInfo, CoreExpr, Id, PrimOp, TyVar, UniType,
	SplitUniqSupply

	IF_ATTACK_PRAGMAS(COMMA splitUniqSupply)
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)

import TaggedCore
import PlainCore

import AbsUniType	( cloneTyVar )
import CmdLineOpts
import Id		( mkIdWithNewUniq, mkSysLocal )
import IdInfo
import SimplEnv
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import SplitUniq
import Unique
import Util

infixr 9  `thenSmpl`, `thenSmpl_`
\end{code}

%************************************************************************
%*									*
\subsection[Monad]{Monad plumbing}
%*									*
%************************************************************************

For the simplifier monad, we want to {\em thread} a unique supply and a counter.
(Command-line switches move around through the explicitly-passed SimplEnv.)

\begin{code}
type SmplM result
  = SplitUniqSupply
  -> SimplCount    -- things being threaded
  -> (result, SimplCount)
\end{code}

\begin{code}
initSmpl :: SplitUniqSupply -- no init count; set to 0
	  -> SmplM a
	  -> (a, SimplCount)

initSmpl us m = m us zeroSimplCount

#ifdef __GLASGOW_HASKELL__
{-# INLINE thenSmpl #-}
{-# INLINE thenSmpl_ #-}
{-# INLINE returnSmpl #-}
#endif

returnSmpl :: a -> SmplM a
returnSmpl e us sc = (e, sc)

thenSmpl  :: SmplM a -> (a -> SmplM b) -> SmplM b
thenSmpl_ :: SmplM a -> SmplM b -> SmplM b

thenSmpl m k us sc0
  = case splitUniqSupply us of { (s1, s2) ->
    case (m s1 sc0)	    of { (m_result, sc1) ->
    k m_result s2 sc1 }}

thenSmpl_ m k us sc0
  = case splitUniqSupply us of { (s1, s2) ->
    case (m s1 sc0) 	    of { (_, sc1) ->
    k s2 sc1 }}

mapSmpl	    	:: (a -> SmplM b) -> [a] -> SmplM [b]
mapAndUnzipSmpl :: (a -> SmplM (b, c)) -> [a] -> SmplM ([b],[c])

mapSmpl f [] = returnSmpl []
mapSmpl f (x:xs)
  = f x		    `thenSmpl` \ x'  ->
    mapSmpl f xs    `thenSmpl` \ xs' ->
    returnSmpl (x':xs')

mapAndUnzipSmpl f [] = returnSmpl ([],[])
mapAndUnzipSmpl f (x:xs)
  = f x			    `thenSmpl` \ (r1,  r2)  ->
    mapAndUnzipSmpl f xs    `thenSmpl` \ (rs1, rs2) ->
    returnSmpl (r1:rs1, r2:rs2)
\end{code}


%************************************************************************
%*									*
\subsection[SimplCount]{Counting up what we've done}
%*									*
%************************************************************************

The assoc list isn't particularly costly, because we only use
the number of ticks in ``real life.''

The right thing to do, if you want that to go fast, is thread
a mutable array through @SimplM@.

\begin{code}
data SimplCount
  = SimplCount	FAST_INT	    -- number of ticks
		[(TickType, Int)]   -- assoc list of all diff kinds of ticks

data TickType
  = UnfoldingDone    {-UNUSED: | Unused -}
  | FoldrBuild	     | MagicUnfold	| ConReused
  | CaseFloatFromLet | CaseOfCase	{-UNUSED: | CaseFloatFromApp -}
  | LetFloatFromLet  | LetFloatFromCase {-UNUSED: | LetFloatFromApp -}
  | KnownBranch	     | Let2Case		{-UNUSED: | UnboxingLet2Case -}
  | CaseMerge	     {-UNUSED: | CaseToLet-}	| CaseElim
  | CaseIdentity
  | AtomicRhs	-- Rhs of a let-expression was an atom
  | EtaExpansion     {-UNUSED: | ArityExpand-}
  {-UNUSED: | ConstantFolding-}  | CaseOfError	{-UNUSED: | InlineRemoved -}
  | FoldrConsNil
  | Foldr_Nil
  | FoldrFoldr
  | Foldr_List
  | FoldrCons
  | FoldrInline
  | TyBetaReduction
  | BetaReduction
  deriving (Eq, Ord, Ix)

instance Text TickType where
    showsPrec p UnfoldingDone	= showString "UnfoldingDone    "
--UNUSED:    showsPrec p Unused		= showString "Unused           "
    showsPrec p FoldrBuild	= showString "FoldrBuild       "
    showsPrec p MagicUnfold	= showString "MagicUnfold      "
    showsPrec p ConReused	= showString "ConReused        "
    showsPrec p CaseFloatFromLet= showString "CaseFloatFromLet "
    showsPrec p CaseOfCase	= showString "CaseOfCase       "
--UNUSED:    showsPrec p CaseFloatFromApp= showString "CaseFloatFromApp "
    showsPrec p LetFloatFromLet	= showString "LetFloatFromLet  "
    showsPrec p LetFloatFromCase= showString "LetFloatFromCase "
--UNUSED:    showsPrec p LetFloatFromApp	= showString "LetFloatFromApp  "
    showsPrec p KnownBranch	= showString "KnownBranch      "
    showsPrec p Let2Case	= showString "Let2Case         "
--UNUSED:    showsPrec p UnboxingLet2Case= showString "UnboxingLet2Case "
    showsPrec p CaseMerge	= showString "CaseMerge        "
--UNUSED:    showsPrec p CaseToLet	= showString "CaseToLet        "
    showsPrec p CaseElim	= showString "CaseElim         "
    showsPrec p CaseIdentity	= showString "CaseIdentity     "
    showsPrec p AtomicRhs	= showString "AtomicRhs        "
    showsPrec p EtaExpansion	= showString "EtaExpansion     "
--UNUSED:    showsPrec p ArityExpand	= showString "ArityExpand      "
--UNUSED:    showsPrec p ConstantFolding	= showString "ConstantFolding  "
    showsPrec p CaseOfError	= showString "CaseOfError      "
--UNUSED:    showsPrec p InlineRemoved	= showString "InlineRemoved    "
    showsPrec p FoldrConsNil	= showString "FoldrConsNil     "
    showsPrec p Foldr_Nil	= showString "Foldr_Nil        "
    showsPrec p FoldrFoldr	= showString "FoldrFoldr       "
    showsPrec p Foldr_List	= showString "Foldr_List       "
    showsPrec p FoldrCons	= showString "FoldrCons        "
    showsPrec p FoldrInline	= showString "FoldrInline      "
    showsPrec p TyBetaReduction	= showString "TyBetaReduction  "
    showsPrec p BetaReduction	= showString "BetaReduction    "

showSimplCount :: SimplCount -> String

showSimplCount (SimplCount _ stuff)
  = shw stuff
  where
    shw []	    = ""
    shw ((t,n):tns) | n /= 0	= show t ++ ('\t' : show n) ++ ('\n' : shw tns)
		    | otherwise = shw tns

zeroSimplCount :: SimplCount
zeroSimplCount
  = SimplCount ILIT(0)
	[(UnfoldingDone, 0),
--UNUSED:	 (Unused, 0),
	 (FoldrBuild, 0),
	 (MagicUnfold, 0),
	 (ConReused, 0),
	 (CaseFloatFromLet, 0),
	 (CaseOfCase, 0),
--UNUSED:	 (CaseFloatFromApp, 0),
	 (LetFloatFromLet, 0),
	 (LetFloatFromCase, 0),
--UNUSED:	 (LetFloatFromApp, 0),
	 (KnownBranch, 0),
	 (Let2Case, 0),
--UNUSED:	 (UnboxingLet2Case, 0),
	 (CaseMerge, 0),
--UNUSED:	 (CaseToLet, 0),
	 (CaseElim, 0),
	 (CaseIdentity, 0),
	 (AtomicRhs, 0),
	 (EtaExpansion, 0),
--UNUSED:	 (ArityExpand,0),
--UNUSED:	 (ConstantFolding, 0),
	 (CaseOfError, 0),
--UNUSED:	 (InlineRemoved,0),
	 (FoldrConsNil,0),
	 (Foldr_Nil,0),
	 (FoldrFoldr,0),
	 (Foldr_List,0),
	 (FoldrCons,0),
	 (FoldrInline,0),
	 (TyBetaReduction,0),
	 (BetaReduction,0) ]
--
--= array (con2tag_TickType UnfoldingDone, con2tag_TickType FoldrInline) 
--        [ i := 0 | i <- indices zeroSimplCount ]
\end{code}

Counting-related monad functions:
\begin{code}
tick :: TickType -> SmplM ()

tick tick_type us (SimplCount n stuff)
  = ((), SimplCount (n _ADD_ ILIT(1))
#ifdef OMIT_SIMPL_COUNTS
		    stuff -- don't change anything
#else
		    (inc_tick stuff)
#endif
    )
  where
    inc_tick [] = panic "couldn't inc_tick!"
    inc_tick (x@(ttype, cnt) : xs)
      = if ttype == tick_type then
	    let
		incd = cnt + 1
	    in
	    (ttype, incd) : xs
        else
	    x : inc_tick xs

tickN :: TickType -> Int -> SmplM ()

tickN tick_type IBOX(increment) us (SimplCount n stuff)
  = ((), SimplCount (n _ADD_ increment)
#ifdef OMIT_SIMPL_COUNTS
		    stuff -- don't change anything
#else
		    (inc_tick stuff)
#endif
    )
  where
    inc_tick [] = panic "couldn't inc_tick!"
    inc_tick (x@(ttype, cnt) : xs)
      = if ttype == tick_type then
	    let
		incd = cnt + IBOX(increment)
	    in
	    (ttype, incd) : xs
        else
	    x : inc_tick xs

simplCount :: SmplM Int
simplCount us sc@(SimplCount n _) = (IBOX(n), sc)

detailedSimplCount :: SmplM SimplCount
detailedSimplCount us sc = (sc, sc)

combineSimplCounts :: SimplCount -> SimplCount -> SimplCount

#ifdef OMIT_SIMPL_COUNTS
combineSimplCounts (SimplCount n1 stuff1) (SimplCount n2 stuff2)
  = SimplCount (n1 _ADD_ n2)
	       stuff1 -- just pick one
#else
combineSimplCounts (SimplCount n1 stuff1) (SimplCount n2 stuff2)
  = SimplCount (n1 _ADD_ n2)
	       (zipWith (\ (t1,c1) (t2,c2) -> (t1,c1+c2)) stuff1 stuff2)
#endif
\end{code}

%************************************************************************
%*									*
\subsection{Monad primitives}
%*									*
%************************************************************************

\begin{code}
newId :: UniType -> SmplM Id
newId ty us sc
  = (mkSysLocal SLIT("s") uniq ty mkUnknownSrcLoc, sc)
  where
    uniq = getSUnique us

newIds :: [UniType] -> SmplM [Id]
newIds tys us sc
  = (zipWith mk_id tys uniqs, sc)
  where
    uniqs  = getSUniques (length tys) us
    mk_id ty uniq = mkSysLocal SLIT("s") uniq ty mkUnknownSrcLoc

cloneTyVarSmpl :: TyVar -> SmplM TyVar

cloneTyVarSmpl tyvar us sc
  = (new_tyvar, sc)
  where
   uniq = getSUnique us
   new_tyvar = cloneTyVar tyvar uniq

cloneId :: SimplEnv -> InBinder -> SmplM OutId
cloneId env (id,_) us sc
  = (mkIdWithNewUniq id_with_new_ty uniq, sc)
  where
    id_with_new_ty = simplTyInId env id
    uniq = getSUnique us

cloneIds :: SimplEnv -> [InBinder] -> SmplM [OutId]
cloneIds env binders = mapSmpl (cloneId env) binders
\end{code}
