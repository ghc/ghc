%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[AssembleInfo]{Machine-specific info used for assembly}

The (machine-independent) assembler itself is in @AsmAssemble@.

\begin{code}
#include "nativeGen/NCG.h"

module AssembleInfo ( assembleInstr ) where

#include "HsVersions.h"

import MachMisc
import CLabel		( CLabel )
import Outputable
import Foreign		( Word8, Ptr )
\end{code}

%************************************************************************
%*									*
\subsection{@assembleInstr@; generate bytes for an insn}
%*									*
%************************************************************************

@assembleInstr@ returns the bytes (Word8's) for a given instruction.
It takes the address where the instruction is to be placed, and an
environment mapping C labels to addresses.  The latter two are needed
for calculating address offsets in call, jump, etc, instructions.
The mapping can be a Nothing, indicating that the caller doesn't care
what the resulting offsets are.  If that's so, @assembleInstr@ is 
being called as the first pass of 2-pass assembly.  For the final
pass, the correct mapping (and base address) must of course be
supplied.

\begin{code}

assembleInstr :: Ptr Word8 -> Maybe (CLabel -> Ptr Word8) -> Instr -> [Word8]

#if alpha_TARGET_ARCH
assembleInstr base_addr label_map instr
   = panic "assembleInstr(Alpha)"
#endif {- alpha_TARGET_ARCH -}
assembleInstr base_addr label_map instr
   = panic "assembleInstr(Alpha)"
#if sparc_TARGET_ARCH
#endif {- sparc_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if i386_TARGET_ARCH
assembleInstr base_addr label_map instr = case instr of
    _ -> []

#if 0
    MOV    sz src dst	-> usageRW src dst
    MOVZxL sz src dst	-> usageRW src dst
    MOVSxL sz src dst	-> usageRW src dst
    LEA    sz src dst	-> usageRW src dst
    ADD    sz src dst	-> usageRM src dst
    SUB    sz src dst	-> usageRM src dst
    IMUL   sz src dst	-> usageRM src dst
    IQUOT  sz src dst	-> usageRM src dst
    IREM   sz src dst	-> usageRM src dst
    AND    sz src dst	-> usageRM src dst
    OR     sz src dst	-> usageRM src dst
    XOR    sz src dst	-> usageRM src dst
    NOT    sz op	-> usageM op
    NEGI   sz op	-> usageM op
    SHL    sz imm dst	-> usageM dst
    SAR    sz imm dst	-> usageM dst
    SHR    sz imm dst	-> usageM dst
    BT     sz imm src	-> mkRU (use_R src) []

    PUSH   sz op	-> mkRU (use_R op) []
    POP    sz op	-> mkRU [] (def_W op)
    TEST   sz src dst	-> mkRU (use_R src ++ use_R dst) []
    CMP    sz src dst	-> mkRU (use_R src ++ use_R dst) []
    SETCC  cond op	-> mkRU [] (def_W op)
    JXX    cond lbl	-> mkRU [] []
    JMP    dsts op	-> mkRU (use_R op) []
    CALL   imm		-> mkRU [] callClobberedRegs
    CLTD		-> mkRU [eax] [edx]
    NOP			-> mkRU [] []

    GMOV   src dst	-> mkRU [src] [dst]
    GLD    sz src dst	-> mkRU (use_EA src) [dst]
    GST    sz src dst	-> mkRU (src : use_EA dst) []

    GLDZ   dst		-> mkRU [] [dst]
    GLD1   dst		-> mkRU [] [dst]

    GFTOD  src dst	-> mkRU [src] [dst]
    GFTOI  src dst	-> mkRU [src] [dst]

    GDTOF  src dst	-> mkRU [src] [dst]
    GDTOI  src dst	-> mkRU [src] [dst]

    GITOF  src dst	-> mkRU [src] [dst]
    GITOD  src dst	-> mkRU [src] [dst]

    GADD   sz s1 s2 dst	-> mkRU [s1,s2] [dst]
    GSUB   sz s1 s2 dst	-> mkRU [s1,s2] [dst]
    GMUL   sz s1 s2 dst	-> mkRU [s1,s2] [dst]
    GDIV   sz s1 s2 dst	-> mkRU [s1,s2] [dst]

    GCMP   sz src1 src2	-> mkRU [src1,src2] []
    GABS   sz src dst	-> mkRU [src] [dst]
    GNEG   sz src dst	-> mkRU [src] [dst]
    GSQRT  sz src dst	-> mkRU [src] [dst]
    GSIN   sz src dst	-> mkRU [src] [dst]
    GCOS   sz src dst	-> mkRU [src] [dst]
    GTAN   sz src dst	-> mkRU [src] [dst]

    COMMENT _		-> noUsage
    SEGMENT _ 		-> noUsage
    LABEL   _		-> noUsage
    ASCII   _ _		-> noUsage
    DATA    _ _		-> noUsage
    DELTA   _           -> noUsage
    _			-> pprPanic "regUsage(x86)" empty
#endif

#endif {- i386_TARGET_ARCH -}
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
\end{code}
