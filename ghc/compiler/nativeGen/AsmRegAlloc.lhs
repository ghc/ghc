%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[AsmRegAlloc]{Register allocator}

\begin{code}
module AsmRegAlloc ( runRegAllocate, runHairyRegAllocate ) where	

#include "HsVersions.h"

import MachCode		( InstrList )
import MachMisc		( Instr )
import MachRegs
import RegAllocInfo

import FiniteMap	( emptyFM, addListToFM, delListFromFM, lookupFM, keysFM )
import Maybes		( maybeToBool )
import OrdList		( mkEmptyList, mkUnitList, mkSeqList, mkParList,
			  flattenOrdList, OrdList
			)
import Unique		( mkBuiltinUnique )
import Util		( mapAccumB )
import Outputable
\end{code}

This is the generic register allocator.

First we try something extremely simple.  If that fails, we have to do
things the hard way.

\begin{code}
runRegAllocate
    :: MRegsState
    -> ([Instr] -> [[RegNo]])
    -> InstrList
    -> [Instr]

runRegAllocate regs find_reserve_regs instrs
  = case simpleAlloc of
	Just simple -> simple
	Nothing     -> tryHairy reserves
  where
    tryHairy [] 
       = error "nativeGen: register allocator: too difficult!  Try -fvia-C.\n"
    tryHairy (resv:resvs)
       = case hairyAlloc resv of
            Just success -> success
            Nothing      -> fooble resvs (tryHairy resvs)

    fooble [] x = x
    fooble (resvs:_) x = trace ("nativeGen: spilling with " 
                                ++ show (length resvs - 2) ++ 
                                " int temporaries") x

    reserves         = find_reserve_regs flatInstrs
    flatInstrs       = flattenOrdList instrs
    simpleAlloc      = simpleRegAlloc regs [] emptyFM   flatInstrs
    hairyAlloc resvd = hairyRegAlloc  regs resvd flatInstrs


runHairyRegAllocate
    :: MRegsState
    -> [RegNo]
    -> InstrList
    -> Maybe [Instr]

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
    :: MRegsState	-- registers to select from
    -> [Reg]		-- live static registers
    -> RegAssignment	-- mapping of dynamics to statics
    -> [Instr]		-- code
    -> Maybe [Instr]

simpleRegAlloc _ _ _ [] = Just []

simpleRegAlloc free live env (instr:instrs)
 | null deadSrcs        && 
   maybeToBool newAlloc && 
   maybeToBool instrs2 
 = Just (instr3 : instrs3)
 | otherwise
 = Nothing
  where
    instr3 = patchRegs instr (lookup env2)

    (srcs, dsts) = case regUsage instr of 
                      (RU s d) -> (regSetToList s, regSetToList d)

    lookup env x = case lookupFM env x of Just y -> y; Nothing -> x

    deadSrcs = [r | r@(UnmappedReg _ _) <- srcs, lookup env r `not_elem` live]
    newDsts  = [r | r@(UnmappedReg _ _) <- dsts, r `not_elem` keysFM env]

    newAlloc = foldr allocateNewReg (Just (free, [])) newDsts
    (free2, new) = case newAlloc of Just x -> x

    env2 = env `addListToFM` new

    live2 = map snd new ++ [x | x <- live, x `not_elem` dsts]

    instrs2 = simpleRegAlloc free2 live2 env2 instrs
    instrs3 = case instrs2 of Just x -> x

    allocateNewReg
	:: Reg
	-> Maybe (MRegsState, [(Reg, Reg)])
	-> Maybe (MRegsState, [(Reg, Reg)])

    allocateNewReg _ Nothing = Nothing

    allocateNewReg d@(UnmappedReg _ pk) (Just (free, prs))
      | null choices = Nothing
      | otherwise    = Just (free2, prs2)
      where
	choices = possibleMRegs pk free
	reg     = head choices
	free2   = free `useMReg` (case reg of {IBOX(reg2) -> reg2} )
	prs2    = ((d,  MappedReg (case reg of {IBOX(reg2) -> reg2})) : prs)
\end{code}

Here is the ``clever'' bit. First go backward (i.e. left), looking for
the last use of dynamic registers. Then go forward (i.e. right), filling
registers with static placements.

hairyRegAlloc takes reserve_regs as the regs to use as spill
temporaries.  First it tries to allocate using all regs except
reserve_regs.  If that fails, it inserts spill code and tries again to
allocate regs, but this time with the spill temporaries available.
Even this might not work if there are insufficient spill temporaries:
in the worst case on x86, we'd need 3 of them, for insns like
addl (reg1,reg2,4) reg3, since this insn uses all 3 regs as input.

\begin{code}
hairyRegAlloc
    :: MRegsState
    -> [RegNo]
    -> [Instr]
    -> Maybe [Instr]

hairyRegAlloc regs reserve_regs instrs =
  case mapAccumB (doRegAlloc reserve_regs) 
                 (RH regs' 1 emptyFM) noFuture instrs of 
     (RH _ mloc1 _, _, instrs')
        -- succeeded w/out using reserves
        | mloc1 == 1 -> Just instrs'
        -- failed, and no reserves avail, so pointless to attempt spilling 
        | null reserve_regs -> Nothing
        -- failed, but we have reserves, so attempt to do spilling
        | otherwise  
        -> let instrs_patched' = patchMem instrs'
               instrs_patched  = flattenOrdList instrs_patched'
           in
               case mapAccumB (doRegAlloc []) (RH regs'' mloc1 emptyFM) 
                    noFuture instrs_patched of
                  ((RH _ mloc2 _),_,instrs'') 
                     -- successfully allocated the patched code
        	     | mloc2 == mloc1 -> Just instrs''
                     -- no; we have to give up
                     | otherwise      -> Nothing 
                       -- instrs''
	               -- pprPanic "runRegAllocate" (ppr mloc2 <+> ppr mloc1)
  where
    regs'  = regs `useMRegs` reserve_regs
    regs'' = mkMRegsState reserve_regs

    noFuture :: RegFuture
    noFuture = RF emptyRegSet (FL emptyRegSet emptyFM) emptyFM
\end{code}

Here we patch instructions that reference ``registers'' which are really in
memory somewhere (the mapping is under the control of the machine-specific
code generator).  We place the appropriate load sequences before any instructions
that use memory registers as sources, and we place the appropriate spill sequences
after any instructions that use memory registers as destinations.  The offending
instructions are rewritten with new dynamic registers, so we have to run register
allocation again after all of this is said and done.

\begin{code}
patchMem :: [Instr] -> InstrList

patchMem cs = foldr (mkSeqList . patchMem') mkEmptyList cs

patchMem' :: Instr -> InstrList

patchMem' instr
 | null memSrcs && null memDsts = mkUnitList instr
 | otherwise =
    mkSeqList
      (foldr mkParList mkEmptyList loadSrcs)
      (mkSeqList instr'
	         (foldr mkParList mkEmptyList spillDsts))

    where
	(RU srcs dsts) = regUsage instr

	memToDyn (MemoryReg i pk) = UnmappedReg (mkBuiltinUnique i) pk
	memToDyn other		  = other

	memSrcs = [ r | r@(MemoryReg _ _) <- regSetToList srcs]
	memDsts = [ r | r@(MemoryReg _ _) <- regSetToList dsts]

	loadSrcs = map load memSrcs
	spillDsts = map spill memDsts

	load mem = loadReg mem (memToDyn mem)
	spill mem = spillReg (memToDyn mem) mem

	instr' = mkUnitList (patchRegs instr memToDyn)
\end{code}

\begin{code}
doRegAlloc
    :: [RegNo]
    -> RegHistory MRegsState
    -> RegFuture
    -> Instr
    -> (RegHistory MRegsState, RegFuture, Instr)

doRegAlloc reserved_regs free_env in_use instr = (free_env', in_use', instr')
  where
      (free_env', instr') = doRegAlloc' reserved_regs free_env info instr
      (in_use', info) = getUsage in_use instr
\end{code}

\begin{code}
getUsage
    :: RegFuture
    -> Instr
    -> (RegFuture, RegInfo Instr)

getUsage (RF next_in_use future reg_conflicts) instr
  = (RF in_use' future' reg_conflicts',
     RI in_use' srcs dsts last_used reg_conflicts')
	 where (RU srcs dsts) = regUsage instr
	       (RL in_use future') = regLiveness instr (RL next_in_use future)
	       live_through = in_use `minusRegSet` dsts
	       last_used = [ r | r <- regSetToList srcs,
			     not (r `elementOfRegSet` (fstFL future) 
                                  || r `elementOfRegSet` in_use)]

	       in_use' = srcs `unionRegSets` live_through

	       reg_conflicts' = 
	        case new_conflicts of
		  [] -> reg_conflicts
		  _  -> addListToFM reg_conflicts new_conflicts

	       new_conflicts
	        | isEmptyRegSet live_dynamics = []
		| otherwise =
		  [ (r, merge_conflicts r)
		  | r <- extractMappedRegNos (regSetToList dsts) ]

	       merge_conflicts reg = 
	        case lookupFM reg_conflicts reg of
		  Nothing        -> live_dynamics
		  Just conflicts -> conflicts `unionRegSets` live_dynamics

	       live_dynamics 
                  = mkRegSet [ r | r@(UnmappedReg _ _) 
                                      <- regSetToList live_through ]

doRegAlloc'
    :: [RegNo]
    -> RegHistory MRegsState
    -> RegInfo Instr
    -> Instr
    -> (RegHistory MRegsState, Instr)

doRegAlloc' reserved (RH frs loc env) 
                     (RI in_use srcs dsts lastu conflicts) instr =

    (RH frs'' loc' env'', patchRegs instr dynToStatic)

    where

      -- free up new registers
      free :: [RegNo]
      free = extractMappedRegNos (map dynToStatic lastu)

      -- (1) free registers that are used last as 
      --     source operands in this instruction
      frs_not_in_use = frs `useMRegs` 
                       (extractMappedRegNos (regSetToList in_use))
      frs' = (frs_not_in_use `freeMRegs` free) `useMRegs` reserved

      -- (2) allocate new registers for the destination operands
      -- allocate registers for new dynamics

      new_dynamix = [ r | r@(UnmappedReg _ _) <- regSetToList dsts, 
                          r `not_elem` keysFM env ]

      (frs'', loc', new) = foldr allocateNewRegs (frs', loc, []) new_dynamix

      env' = addListToFM env new

      env'' = delListFromFM env' lastu

      dynToStatic :: Reg -> Reg
      dynToStatic dyn@(UnmappedReg _ _) =
	case lookupFM env' dyn of
	    Just r -> r
	    Nothing -> trace ("Lost register; possibly a floating point"
                              ++" type error in a _ccall_?") dyn
      dynToStatic other = other

      allocateNewRegs :: Reg 
                      -> (MRegsState, Int, [(Reg, Reg)]) 
		      -> (MRegsState, Int, [(Reg, Reg)])

      allocateNewRegs d@(UnmappedReg _ pk) (fs, mem, lst) 
         = (fs', mem', (d, f) : lst)
	where 
	 (fs', f, mem') = 
	   case acceptable fs of
	    []           -> (fs, MemoryReg mem pk, mem + 1)
	    (IBOX(x2):_) -> (fs `useMReg` x2, MappedReg x2, mem)

         acceptable regs = filter no_conflict (possibleMRegs pk regs)

	 no_conflict reg = 
	   case lookupFM conflicts reg of
	     Nothing        -> True
	     Just conflicts -> not (d `elementOfRegSet` conflicts)
\end{code}

We keep a local copy of the Prelude function \tr{notElem},
so that it can be specialised.  (Hack me gently.  [WDP 94/11])
\begin{code}
not_elem x []	    =  True
not_elem x (y:ys)   =  x /= y && not_elem x ys
\end{code}
