%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\begin{code}
#include "HsVersions.h"
#include "../../includes/platform.h"
#include "../../includes/GhcConstants.h"

module AsmRegAlloc (
	FutureLive(..), RegLiveness(..), RegUsage(..), Reg(..),
	MachineRegisters(..), MachineCode(..),

	mkReg, runRegAllocate, runHairyRegAllocate,
	extractMappedRegNos,

	-- And, for self-sufficiency
	CLabel, OrdList, PrimKind, UniqSet(..), UniqFM,
	FiniteMap, Unique
    ) where

IMPORT_Trace

import CLabelInfo	( CLabel )
import FiniteMap
import MachDesc
import Maybes		( maybeToBool, Maybe(..) )
import OrdList		-- ( mkUnitList, mkSeqList, mkParList, OrdList )
import Outputable
import Pretty
import PrimKind		( PrimKind(..) )
import UniqSet
import Unique
import Util

#if ! OMIT_NATIVE_CODEGEN

# if alpha_TARGET_ARCH
import AlphaCode	-- ( AlphaInstr, AlphaRegs ) -- for specializing

{-# SPECIALIZE
    runRegAllocate :: AlphaRegs -> [Int] -> (OrdList AlphaInstr) -> [AlphaInstr]
  #-}
# endif

# if i386_TARGET_ARCH
import I386Code		-- ( I386Instr, I386Regs ) -- for specializing

{-# SPECIALIZE
    runRegAllocate :: I386Regs -> [Int] -> (OrdList I386Instr) -> [I386Instr]
  #-}
# endif

# if sparc_TARGET_ARCH
import SparcCode	-- ( SparcInstr, SparcRegs ) -- for specializing

{-# SPECIALIZE
    runRegAllocate :: SparcRegs -> [Int] -> (OrdList SparcInstr) -> [SparcInstr]
  #-}
# endif

#endif

\end{code}

%************************************************************************
%*									*
\subsection[Reg]{Real registers}
%*									*
%************************************************************************

Static Registers correspond to actual machine registers.  These should
be avoided until the last possible moment.

Dynamic registers are allocated on the fly, usually to represent a single
value in the abstract assembly code (i.e. dynamic registers are usually
single assignment).  Ultimately, they are mapped to available machine
registers before spitting out the code.

\begin{code}

data Reg = FixedReg  FAST_INT		-- A pre-allocated machine register

	 | MappedReg FAST_INT		-- A dynamically allocated machine register

	 | MemoryReg Int PrimKind	-- A machine "register" actually held in a memory
					-- allocated table of registers which didn't fit
					-- in real registers.

	 | UnmappedReg Unique PrimKind	-- One of an infinite supply of registers,
					-- always mapped to one of the earlier two
					-- before we're done.
	 -- No thanks: deriving (Eq)

mkReg :: Unique -> PrimKind -> Reg
mkReg = UnmappedReg

instance Text Reg where
    showsPrec _ (FixedReg i)	= showString "%"  . shows IBOX(i)
    showsPrec _ (MappedReg i)	= showString "%"  . shows IBOX(i)
    showsPrec _ (MemoryReg i _) = showString "%M"  . shows i
    showsPrec _ (UnmappedReg i _) = showString "%U" . shows i

#ifdef DEBUG
instance Outputable Reg where
    ppr sty r = ppStr (show r)
#endif

cmpReg (FixedReg i) (FixedReg i') = cmp_ihash i i'
cmpReg (MappedReg i) (MappedReg i') = cmp_ihash i i'
cmpReg (MemoryReg i _) (MemoryReg i' _) = cmp_i i i'
cmpReg (UnmappedReg u _) (UnmappedReg u' _) = cmpUnique u u'
cmpReg r1 r2 =
    let tag1 = tagReg r1
	tag2 = tagReg r2
    in
	if tag1 _LT_ tag2 then LT_ else GT_
    where
	tagReg (FixedReg _)	 = (ILIT(1) :: FAST_INT)
	tagReg (MappedReg _)	 = ILIT(2)
	tagReg (MemoryReg _ _)	 = ILIT(3)
	tagReg (UnmappedReg _ _) = ILIT(4)

cmp_i :: Int -> Int -> TAG_
cmp_i a1 a2 = if a1 == a2 then EQ_ else if a1 < a2 then LT_ else GT_

cmp_ihash :: FAST_INT -> FAST_INT -> TAG_
cmp_ihash a1 a2 = if a1 _EQ_ a2 then EQ_ else if a1 _LT_ a2 then LT_ else GT_

instance Eq Reg where
    a == b = case cmpReg a b of { EQ_ -> True;  _ -> False }
    a /= b = case cmpReg a b of { EQ_ -> False; _ -> True  }

instance Ord Reg where
    a <= b = case cmpReg a b of { LT_ -> True;	EQ_ -> True;  GT__ -> False }
    a <	 b = case cmpReg a b of { LT_ -> True;	EQ_ -> False; GT__ -> False }
    a >= b = case cmpReg a b of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case cmpReg a b of { LT_ -> False; EQ_ -> False; GT__ -> True  }
#ifdef __GLASGOW_HASKELL__
    _tagCmp a b = case cmpReg a b of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
#endif

instance NamedThing Reg where
    -- the *only* method that should be defined is "getTheUnique"!
    -- (so we can use UniqFMs/UniqSets on Regs
    getTheUnique (UnmappedReg u _) = u
    getTheUnique (FixedReg i)	   = mkPseudoUnique1 IBOX(i)
    getTheUnique (MappedReg i)	   = mkPseudoUnique2 IBOX(i)
    getTheUnique (MemoryReg i _)   = mkPseudoUnique3 i
\end{code}

This is the generic register allocator.

%************************************************************************
%*									*
\subsection[RegPlace]{Map Stix registers to {\em real} registers}
%*									*
%************************************************************************

An important point:  The @regUsage@ function for a particular assembly language
must not refer to fixed registers, such as Hp, SpA, etc.  The source and destination
lists should only refer to dynamically allocated registers or static registers
from the free list.  As far as we are concerned, the fixed registers simply don't
exist (for allocation purposes, anyway).

\begin{code}

class MachineRegisters a where
    mkMRegs	    :: [Int] -> a
    possibleMRegs   :: PrimKind -> a -> [Int]
    useMReg	    :: a -> FAST_INT -> a
    useMRegs	    :: a -> [Int] -> a
    freeMReg	    :: a -> FAST_INT -> a
    freeMRegs	    :: a -> [Int] -> a

type RegAssignment = FiniteMap Reg Reg
type RegConflicts = FiniteMap Int (UniqSet Reg)

data FutureLive
  = FL	(UniqSet Reg)
	(FiniteMap CLabel (UniqSet Reg))
fstFL (FL a b) = a

data RegHistory a
  = RH	a
	Int
	RegAssignment

data RegFuture
  = RF	(UniqSet Reg)	-- in use
	FutureLive	-- future
	RegConflicts

data RegInfo a
  = RI	(UniqSet Reg)	-- in use
	(UniqSet Reg)	-- sources
	(UniqSet Reg)	-- destinations
	[Reg]		-- last used
	RegConflicts

data RegUsage
  = RU	(UniqSet Reg)
	(UniqSet Reg)

data RegLiveness
  = RL	(UniqSet Reg)
	FutureLive

class MachineCode a where
-- OLD:
--    flatten	    :: OrdList a -> [a]
      regUsage	    :: a -> RegUsage
      regLiveness   :: a -> RegLiveness -> RegLiveness
      patchRegs	    :: a -> (Reg -> Reg) -> a
      spillReg	    :: Reg -> Reg -> OrdList a
      loadReg	    :: Reg -> Reg -> OrdList a

\end{code}

First we try something extremely simple.
If that fails, we have to do things the hard way.

\begin{code}

runRegAllocate
    :: (MachineRegisters a, MachineCode b)
    => a
    -> [Int]
    -> (OrdList b)
    -> [b]

runRegAllocate regs reserve_regs instrs =
    case simpleAlloc of 
	Just x  -> x
	Nothing -> hairyAlloc
  where
    flatInstrs	= flattenOrdList instrs
    simpleAlloc = simpleRegAlloc regs [] emptyFM flatInstrs
    hairyAlloc	= hairyRegAlloc regs reserve_regs flatInstrs

runHairyRegAllocate		-- use only hairy for i386!
    :: (MachineRegisters a, MachineCode b)
    => a
    -> [Int]
    -> (OrdList b)
    -> [b]

runHairyRegAllocate regs reserve_regs instrs
  = hairyRegAlloc regs reserve_regs flatInstrs
  where
    flatInstrs	= flattenOrdList instrs
\end{code}

Here is the simple register allocator.	Just dole out registers until
we run out, or until one gets clobbered before its last use.  Don't
do anything fancy with branches.  Just pretend that you've got a block
of straight-line code and hope for the best.  Experience indicates that
this approach will suffice for about 96 percent of the code blocks that
we generate.

\begin{code}

simpleRegAlloc
    :: (MachineRegisters a, MachineCode b)
    => a		-- registers to select from
    -> [Reg]		-- live static registers
    -> RegAssignment	-- mapping of dynamics to statics
    -> [b]		-- code
    -> Maybe [b]

simpleRegAlloc _ _ _ [] = Just []
simpleRegAlloc free live env (instr:instrs) =
    if null deadSrcs && maybeToBool newAlloc && maybeToBool instrs2 then
	Just (instr3 : instrs3)
    else
	Nothing
  where
    instr3 = patchRegs instr (lookup env2)

    (srcs, dsts) = case regUsage instr of { RU s d -> (uniqSetToList s, uniqSetToList d) }

    lookup env x = case lookupFM env x of {Just y -> y; Nothing -> x}

    deadSrcs = [r | r@(UnmappedReg _ _) <- srcs, lookup env r `not_elem` live]
    newDsts  = [r | r@(UnmappedReg _ _) <- dsts, r `not_elem` keysFM env]

    newAlloc = foldr allocateNewReg (Just (free, [])) newDsts
    (free2, new) = case newAlloc of Just x -> x

    env2 = env `addListToFM` new

    live2 = map snd new ++ [x | x <- live, x `not_elem` dsts]

    instrs2 = simpleRegAlloc free2 live2 env2 instrs
    instrs3 = case instrs2 of Just x -> x

    allocateNewReg
	:: MachineRegisters a
	=> Reg
	-> Maybe (a, [(Reg, Reg)])
	-> Maybe (a, [(Reg, Reg)])

    allocateNewReg _ Nothing = Nothing

    allocateNewReg d@(UnmappedReg _ pk) (Just (free, prs)) =
	if null choices then Nothing
	else Just (free2, prs2)
      where
	choices = possibleMRegs pk free
	reg = head choices
	free2 = free `useMReg` (case reg of {IBOX(reg2) -> reg2} )
	prs2 = ((d,  MappedReg (case reg of {IBOX(reg2) -> reg2})) : prs)

\end{code}

Here is the ``clever'' bit. First go backward (i.e. left), looking for
the last use of dynamic registers. Then go forward (i.e. right), filling
registers with static placements.

\begin{code}

hairyRegAlloc
    :: (MachineRegisters a, MachineCode b)
    => a
    -> [Int]
    -> [b]
    -> [b]

hairyRegAlloc regs reserve_regs instrs =
    case mapAccumB (doRegAlloc reserve_regs)
	    (RH regs' 1 emptyFM) noFuture instrs
    of (RH _ loc' _, _, instrs') ->
	if loc' == 1 then instrs' else
	case mapAccumB do_RegAlloc_Nil
		(RH regs'' loc' emptyFM) noFuture (flattenOrdList (patchMem instrs'))
	of ((RH _ loc'' _),_,instrs'') ->
	    if loc'' == loc' then instrs'' else panic "runRegAllocate"
  where
    regs' = regs `useMRegs` reserve_regs
    regs'' = mkMRegs reserve_regs `asTypeOf` regs

do_RegAlloc_Nil = doRegAlloc [] -- out here to avoid CAF (sigh)
do_RegAlloc_Nil
    :: (MachineRegisters a, MachineCode b)
    => RegHistory a
    -> RegFuture
    -> b
    -> (RegHistory a, RegFuture, b)

noFuture :: RegFuture
noFuture = RF emptyUniqSet (FL emptyUniqSet emptyFM) emptyFM
\end{code}

Here we patch instructions that reference ``registers'' which are really in
memory somewhere (the mapping is under the control of the machine-specific
code generator).  We place the appropriate load sequences before any instructions
that use memory registers as sources, and we place the appropriate spill sequences
after any instructions that use memory registers as destinations.  The offending
instructions are rewritten with new dynamic registers, so we have to run register
allocation again after all of this is said and done.

\begin{code}

patchMem
    :: MachineCode a
    => [a]
    -> OrdList a

patchMem cs = foldr (mkSeqList . patchMem') mkEmptyList cs

patchMem'
    :: MachineCode a
    => a
    -> OrdList a

patchMem' instr =
    if null memSrcs && null memDsts then mkUnitList instr
    else mkSeqList
	    (foldr mkParList mkEmptyList loadSrcs)
	    (mkSeqList instr'
		(foldr mkParList mkEmptyList spillDsts))

    where
	(RU srcs dsts) = regUsage instr

	memToDyn (MemoryReg i pk) = UnmappedReg (mkBuiltinUnique i) pk
	memToDyn other		  = other

	memSrcs = [ r | r@(MemoryReg _ _) <- uniqSetToList srcs]
	memDsts = [ r | r@(MemoryReg _ _) <- uniqSetToList dsts]

	loadSrcs = map load memSrcs
	spillDsts = map spill memDsts

	load mem = loadReg mem (memToDyn mem)
	spill mem = spillReg (memToDyn mem) mem

	instr' = mkUnitList (patchRegs instr memToDyn)

\end{code}

\begin{code}

doRegAlloc
    :: (MachineRegisters a, MachineCode b)
    => [Int]
    -> RegHistory a
    -> RegFuture
    -> b
    -> (RegHistory a, RegFuture, b)

doRegAlloc reserved_regs free_env in_use instr = (free_env', in_use', instr')
  where
      (free_env', instr') = doRegAlloc' reserved_regs free_env info instr
      (in_use', info) = getUsage in_use instr

\end{code}

\begin{code}

getUsage
    :: MachineCode a
    => RegFuture
    -> a
    -> (RegFuture, RegInfo a)

getUsage (RF next_in_use future reg_conflicts) instr =
    (RF in_use' future' reg_conflicts',
     RI in_use' srcs dsts last_used reg_conflicts')
	 where (RU srcs dsts) = regUsage instr
	       (RL in_use future') = regLiveness instr (RL next_in_use future)
	       live_through = in_use `minusUniqSet` dsts
	       last_used = [ r | r <- uniqSetToList srcs,
			     not (r `elementOfUniqSet` (fstFL future) || r `elementOfUniqSet` in_use)]
	       in_use' = srcs `unionUniqSets` live_through
	       reg_conflicts' = case new_conflicts of
		    [] -> reg_conflicts
		    _ -> addListToFM reg_conflicts new_conflicts
	       new_conflicts = if isEmptyUniqSet live_dynamics then []
			       else [ (r, merge_conflicts r)
					| r <- extractMappedRegNos (uniqSetToList dsts) ]
	       merge_conflicts reg = case lookupFM reg_conflicts reg of
			    Nothing -> live_dynamics
			    Just conflicts -> conflicts `unionUniqSets` live_dynamics
	       live_dynamics = mkUniqSet
			    [ r | r@(UnmappedReg _ _) <- uniqSetToList live_through ]

doRegAlloc'
    :: (MachineRegisters a, MachineCode b)
    => [Int]
    -> RegHistory a
    -> RegInfo b
    -> b
    -> (RegHistory a, b)

doRegAlloc' reserved (RH frs loc env) (RI in_use srcs dsts lastu conflicts) instr =

    (RH frs'' loc' env'', patchRegs instr dynToStatic)

    where

      -- free up new registers
      free :: [Int]
      free = extractMappedRegNos (map dynToStatic lastu)

      -- (1) free registers that are used last as source operands in this instruction
      frs_not_in_use = frs `useMRegs` (extractMappedRegNos (uniqSetToList in_use))
      frs' = (frs_not_in_use `freeMRegs` free) `useMRegs` reserved

      -- (2) allocate new registers for the destination operands
      -- allocate registers for new dynamics

      new_dynamix = [ r | r@(UnmappedReg _ _) <- uniqSetToList dsts, r `not_elem` keysFM env ]

      (frs'', loc', new) = foldr allocateNewRegs (frs', loc, []) new_dynamix

      env' = addListToFM env new

      env'' = delListFromFM env' lastu

      dynToStatic :: Reg -> Reg
      dynToStatic dyn@(UnmappedReg _ _) =
	case lookupFM env' dyn of
	    Just r -> r
	    Nothing -> trace "Lost register; possibly a floating point type error in a _ccall_?" dyn
      dynToStatic other = other

      allocateNewRegs
	:: MachineRegisters a
	=> Reg -> (a, Int, [(Reg, Reg)]) -> (a, Int, [(Reg, Reg)])

      allocateNewRegs d@(UnmappedReg _ pk) (fs, mem, lst) = (fs', mem', (d, f) : lst)
	where (fs', f, mem') = case acceptable fs of
		[] -> (fs, MemoryReg mem pk, mem + 1)
		(IBOX(x2):_) -> (fs `useMReg` x2, MappedReg x2, mem)

	      acceptable regs = filter no_conflict (possibleMRegs pk regs)
	      no_conflict reg = case lookupFM conflicts reg of
		    Nothing -> True
		    Just conflicts -> not (d `elementOfUniqSet` conflicts)
\end{code}

\begin{code}
extractMappedRegNos :: [Reg] -> [Int]

extractMappedRegNos regs
  = foldr ex [] regs
  where
    ex (MappedReg i) acc = IBOX(i) : acc  -- we'll take it
    ex _	     acc = acc		  -- leave it out
\end{code}

We keep a local copy of the Prelude function \tr{notElem},
so that it can be specialised.  (Hack me gently.  [WDP 94/11])
\begin{code}
not_elem x []	    =  True
not_elem x (y:ys)   =  x /= y && not_elem x ys
\end{code}
