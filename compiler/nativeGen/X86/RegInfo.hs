
module X86.RegInfo (
	RegUsage(..),
	noUsage,
	regUsage,
	patchRegs,
	jumpDests,
	isJumpish,
	patchJump,
	isRegRegMove,

        JumpDest, 
	canShortcut, 
	shortcutJump, 

	mkSpillInstr,
	mkLoadInstr,
	mkRegRegMoveInstr,
	mkBranchInstr,

	spillSlotSize,
	maxSpillSlots,
	spillSlotToOffset		
)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

import X86.Instr
import X86.Regs
import RegsBase

import BlockId
import Outputable
import Constants	( rESERVED_C_STACK_BYTES )
import FastBool


-- -----------------------------------------------------------------------------
-- RegUsage type

-- @regUsage@ returns the sets of src and destination registers used
-- by a particular instruction.  Machine registers that are
-- pre-allocated to stgRegs are filtered out, because they are
-- uninteresting from a register allocation standpoint.  (We wouldn't
-- want them to end up on the free list!)  As far as we are concerned,
-- the fixed registers simply don't exist (for allocation purposes,
-- anyway).

-- regUsage doesn't need to do any trickery for jumps and such.  Just
-- state precisely the regs read and written by that insn.  The
-- consequences of control flow transfers, as far as register
-- allocation goes, are taken care of by the register allocator.

data RegUsage = RU [Reg] [Reg]

noUsage :: RegUsage
noUsage  = RU [] []


regUsage :: Instr -> RegUsage
regUsage instr = case instr of
    MOV    _ src dst	-> usageRW src dst
    MOVZxL _ src dst	-> usageRW src dst
    MOVSxL _ src dst	-> usageRW src dst
    LEA    _ src dst	-> usageRW src dst
    ADD    _ src dst	-> usageRM src dst
    ADC    _ src dst	-> usageRM src dst
    SUB    _ src dst	-> usageRM src dst
    IMUL   _ src dst	-> usageRM src dst
    IMUL2  _ src       -> mkRU (eax:use_R src) [eax,edx]
    MUL    _ src dst	-> usageRM src dst
    DIV    _ op	-> mkRU (eax:edx:use_R op) [eax,edx]
    IDIV   _ op	-> mkRU (eax:edx:use_R op) [eax,edx]
    AND    _ src dst	-> usageRM src dst
    OR     _ src dst	-> usageRM src dst

    XOR    _ (OpReg src) (OpReg dst)
        | src == dst    -> mkRU [] [dst]

    XOR    _ src dst	-> usageRM src dst
    NOT    _ op		-> usageM op
    NEGI   _ op		-> usageM op
    SHL    _ imm dst	-> usageRM imm dst
    SAR    _ imm dst	-> usageRM imm dst
    SHR    _ imm dst	-> usageRM imm dst
    BT     _ _   src	-> mkRUR (use_R src)

    PUSH   _ op		-> mkRUR (use_R op)
    POP    _ op		-> mkRU [] (def_W op)
    TEST   _ src dst	-> mkRUR (use_R src ++ use_R dst)
    CMP    _ src dst	-> mkRUR (use_R src ++ use_R dst)
    SETCC  _ op		-> mkRU [] (def_W op)
    JXX    _ _		-> mkRU [] []
    JXX_GBL _ _		-> mkRU [] []
    JMP     op		-> mkRUR (use_R op)
    JMP_TBL op _        -> mkRUR (use_R op)
    CALL (Left _)  params   -> mkRU params callClobberedRegs
    CALL (Right reg) params -> mkRU (reg:params) callClobberedRegs
    CLTD   _		-> mkRU [eax] [edx]
    NOP			-> mkRU [] []

#if i386_TARGET_ARCH
    GMOV   src dst	-> mkRU [src] [dst]
    GLD    _ src dst	-> mkRU (use_EA src) [dst]
    GST    _ src dst	-> mkRUR (src : use_EA dst)

    GLDZ   dst		-> mkRU [] [dst]
    GLD1   dst		-> mkRU [] [dst]

    GFTOI  src dst	-> mkRU [src] [dst]
    GDTOI  src dst	-> mkRU [src] [dst]

    GITOF  src dst	-> mkRU [src] [dst]
    GITOD  src dst	-> mkRU [src] [dst]

    GADD   _ s1 s2 dst	-> mkRU [s1,s2] [dst]
    GSUB   _ s1 s2 dst	-> mkRU [s1,s2] [dst]
    GMUL   _ s1 s2 dst	-> mkRU [s1,s2] [dst]
    GDIV   _ s1 s2 dst	-> mkRU [s1,s2] [dst]

    GCMP   _ src1 src2   -> mkRUR [src1,src2]
    GABS   _ src dst     -> mkRU [src] [dst]
    GNEG   _ src dst     -> mkRU [src] [dst]
    GSQRT  _ src dst     -> mkRU [src] [dst]
    GSIN   _ _ _ src dst -> mkRU [src] [dst]
    GCOS   _ _ _ src dst -> mkRU [src] [dst]
    GTAN   _ _ _ src dst -> mkRU [src] [dst]
#endif

#if x86_64_TARGET_ARCH
    CVTSS2SD   src dst	-> mkRU [src] [dst]
    CVTSD2SS   src dst	-> mkRU [src] [dst]
    CVTTSS2SIQ src dst	-> mkRU (use_R src) [dst]
    CVTTSD2SIQ src dst	-> mkRU (use_R src) [dst]
    CVTSI2SS   src dst	-> mkRU (use_R src) [dst]
    CVTSI2SD   src dst	-> mkRU (use_R src) [dst]
    FDIV _     src dst	-> usageRM src dst
#endif    

    FETCHGOT reg        -> mkRU [] [reg]
    FETCHPC  reg        -> mkRU [] [reg]

    COMMENT _		-> noUsage
    DELTA   _           -> noUsage
    SPILL   reg _	-> mkRU [reg] []
    RELOAD  _  reg	-> mkRU []    [reg]

    _other		-> panic "regUsage: unrecognised instr"

 where
    -- 2 operand form; first operand Read; second Written
    usageRW :: Operand -> Operand -> RegUsage
    usageRW op (OpReg reg)	= mkRU (use_R op) [reg]
    usageRW op (OpAddr ea)	= mkRUR (use_R op ++ use_EA ea)
    usageRW _ _			= panic "X86.RegInfo.usageRW: no match"

    -- 2 operand form; first operand Read; second Modified
    usageRM :: Operand -> Operand -> RegUsage
    usageRM op (OpReg reg)	= mkRU (use_R op ++ [reg]) [reg]
    usageRM op (OpAddr ea)	= mkRUR (use_R op ++ use_EA ea)
    usageRM _ _			= panic "X86.RegInfo.usageRM: no match"

    -- 1 operand form; operand Modified
    usageM :: Operand -> RegUsage
    usageM (OpReg reg)		= mkRU [reg] [reg]
    usageM (OpAddr ea)		= mkRUR (use_EA ea)
    usageM _			= panic "X86.RegInfo.usageM: no match"

    -- Registers defd when an operand is written.
    def_W (OpReg reg)		= [reg]
    def_W (OpAddr _ )		= []
    def_W _			= panic "X86.RegInfo.def_W: no match"

    -- Registers used when an operand is read.
    use_R (OpReg reg)  = [reg]
    use_R (OpImm _)    = []
    use_R (OpAddr ea)  = use_EA ea

    -- Registers used to compute an effective address.
    use_EA (ImmAddr _ _) = []
    use_EA (AddrBaseIndex base index _) = 
	use_base base $! use_index index
	where use_base (EABaseReg r) x = r : x
	      use_base _ x             = x
	      use_index EAIndexNone   = []
	      use_index (EAIndex i _) = [i]

    mkRUR src = src' `seq` RU src' []
	where src' = filter interesting src

    mkRU src dst = src' `seq` dst' `seq` RU src' dst'
	where src' = filter interesting src
	      dst' = filter interesting dst

interesting :: Reg -> Bool
interesting (VirtualRegI  _)  = True
interesting (VirtualRegHi _)  = True
interesting (VirtualRegF  _)  = True
interesting (VirtualRegD  _)  = True
interesting (RealReg i)       = isFastTrue (freeReg i)




-- -----------------------------------------------------------------------------
-- 'patchRegs' function

-- 'patchRegs' takes an instruction and applies the given mapping to
-- all the register references.

patchRegs :: Instr -> (Reg -> Reg) -> Instr
patchRegs instr env = case instr of
    MOV  sz src dst	-> patch2 (MOV  sz) src dst
    MOVZxL sz src dst	-> patch2 (MOVZxL sz) src dst
    MOVSxL sz src dst	-> patch2 (MOVSxL sz) src dst
    LEA  sz src dst	-> patch2 (LEA  sz) src dst
    ADD  sz src dst	-> patch2 (ADD  sz) src dst
    ADC  sz src dst	-> patch2 (ADC  sz) src dst
    SUB  sz src dst	-> patch2 (SUB  sz) src dst
    IMUL sz src dst 	-> patch2 (IMUL sz) src dst
    IMUL2 sz src        -> patch1 (IMUL2 sz) src
    MUL sz src dst 	-> patch2 (MUL sz) src dst
    IDIV sz op		-> patch1 (IDIV sz) op
    DIV sz op		-> patch1 (DIV sz) op
    AND  sz src dst	-> patch2 (AND  sz) src dst
    OR   sz src dst	-> patch2 (OR   sz) src dst
    XOR  sz src dst	-> patch2 (XOR  sz) src dst
    NOT  sz op 		-> patch1 (NOT  sz) op
    NEGI sz op		-> patch1 (NEGI sz) op
    SHL  sz imm dst 	-> patch1 (SHL sz imm) dst
    SAR  sz imm dst 	-> patch1 (SAR sz imm) dst
    SHR  sz imm dst 	-> patch1 (SHR sz imm) dst
    BT   sz imm src     -> patch1 (BT  sz imm) src
    TEST sz src dst	-> patch2 (TEST sz) src dst
    CMP  sz src dst	-> patch2 (CMP  sz) src dst
    PUSH sz op		-> patch1 (PUSH sz) op
    POP  sz op		-> patch1 (POP  sz) op
    SETCC cond op	-> patch1 (SETCC cond) op
    JMP op		-> patch1 JMP op
    JMP_TBL op ids      -> patch1 JMP_TBL op $ ids

#if i386_TARGET_ARCH
    GMOV src dst	-> GMOV (env src) (env dst)
    GLD  sz src dst	-> GLD sz (lookupAddr src) (env dst)
    GST  sz src dst	-> GST sz (env src) (lookupAddr dst)

    GLDZ dst		-> GLDZ (env dst)
    GLD1 dst		-> GLD1 (env dst)

    GFTOI src dst	-> GFTOI (env src) (env dst)
    GDTOI src dst	-> GDTOI (env src) (env dst)

    GITOF src dst	-> GITOF (env src) (env dst)
    GITOD src dst	-> GITOD (env src) (env dst)

    GADD sz s1 s2 dst	-> GADD sz (env s1) (env s2) (env dst)
    GSUB sz s1 s2 dst	-> GSUB sz (env s1) (env s2) (env dst)
    GMUL sz s1 s2 dst	-> GMUL sz (env s1) (env s2) (env dst)
    GDIV sz s1 s2 dst	-> GDIV sz (env s1) (env s2) (env dst)

    GCMP sz src1 src2	-> GCMP sz (env src1) (env src2)
    GABS sz src dst	-> GABS sz (env src) (env dst)
    GNEG sz src dst	-> GNEG sz (env src) (env dst)
    GSQRT sz src dst	-> GSQRT sz (env src) (env dst)
    GSIN sz l1 l2 src dst	-> GSIN sz l1 l2 (env src) (env dst)
    GCOS sz l1 l2 src dst	-> GCOS sz l1 l2 (env src) (env dst)
    GTAN sz l1 l2 src dst	-> GTAN sz l1 l2 (env src) (env dst)
#endif

#if x86_64_TARGET_ARCH
    CVTSS2SD src dst	-> CVTSS2SD (env src) (env dst)
    CVTSD2SS src dst	-> CVTSD2SS (env src) (env dst)
    CVTTSS2SIQ src dst	-> CVTTSS2SIQ (patchOp src) (env dst)
    CVTTSD2SIQ src dst	-> CVTTSD2SIQ (patchOp src) (env dst)
    CVTSI2SS src dst	-> CVTSI2SS (patchOp src) (env dst)
    CVTSI2SD src dst	-> CVTSI2SD (patchOp src) (env dst)
    FDIV sz src dst	-> FDIV sz (patchOp src) (patchOp dst)
#endif    

    CALL (Left _)  _	-> instr
    CALL (Right reg) p	-> CALL (Right (env reg)) p
    
    FETCHGOT reg        -> FETCHGOT (env reg)
    FETCHPC  reg        -> FETCHPC  (env reg)
   
    NOP			-> instr
    COMMENT _		-> instr
    DELTA _ 		-> instr
    SPILL  reg slot	-> SPILL (env reg) slot
    RELOAD slot reg	-> RELOAD slot (env reg)

    JXX _ _		-> instr
    JXX_GBL _ _		-> instr
    CLTD _		-> instr

    _other		-> panic "patchRegs: unrecognised instr"

  where
    patch1 insn op      = insn $! patchOp op
    patch2 insn src dst = (insn $! patchOp src) $! patchOp dst

    patchOp (OpReg  reg) = OpReg $! env reg
    patchOp (OpImm  imm) = OpImm imm
    patchOp (OpAddr ea)  = OpAddr $! lookupAddr ea

    lookupAddr (ImmAddr imm off) = ImmAddr imm off
    lookupAddr (AddrBaseIndex base index disp)
      = ((AddrBaseIndex $! lookupBase base) $! lookupIndex index) disp
      where
	lookupBase EABaseNone       = EABaseNone
	lookupBase EABaseRip        = EABaseRip
	lookupBase (EABaseReg r)    = EABaseReg (env r)
				 
	lookupIndex EAIndexNone     = EAIndexNone
	lookupIndex (EAIndex r i)   = EAIndex (env r) i


-- -----------------------------------------------------------------------------
-- Determine the possible destinations from the current instruction.

-- (we always assume that the next instruction is also a valid destination;
-- if this isn't the case then the jump should be at the end of the basic
-- block).

jumpDests :: Instr -> [BlockId] -> [BlockId]
jumpDests insn acc
  = case insn of
	JXX _ id	-> id : acc
	JMP_TBL _ ids	-> ids ++ acc
	_		-> acc


isJumpish :: Instr -> Bool
isJumpish instr
 = case instr of
	JMP{}		-> True
	JXX{}		-> True
	JXX_GBL{}	-> True
	JMP_TBL{}	-> True
	CALL{}		-> True
	_		-> False

-- | Change the destination of this jump instruction
--	Used in joinToTargets in the linear allocator, when emitting fixup code
--	for join points.
patchJump :: Instr -> BlockId -> BlockId -> Instr
patchJump insn old new
  = case insn of
	JXX cc id | id == old -> JXX cc new
	JMP_TBL _ _     -> error "Cannot patch JMP_TBL"
	_other		-> insn


-- -----------------------------------------------------------------------------
-- Detecting reg->reg moves

-- The register allocator attempts to eliminate reg->reg moves whenever it can,
-- by assigning the src and dest temporaries to the same real register.

isRegRegMove :: Instr -> Maybe (Reg,Reg)
isRegRegMove (MOV _ (OpReg r1) (OpReg r2)) = Just (r1,r2)
isRegRegMove _  = Nothing



data JumpDest = DestBlockId BlockId | DestImm Imm


canShortcut :: Instr -> Maybe JumpDest
canShortcut (JXX ALWAYS id) 	= Just (DestBlockId id)
canShortcut (JMP (OpImm imm)) 	= Just (DestImm imm)
canShortcut _ 			= Nothing


shortcutJump :: (BlockId -> Maybe JumpDest) -> Instr -> Instr
shortcutJump fn insn@(JXX cc id) = 
  case fn id of
    Nothing                -> insn
    Just (DestBlockId id') -> shortcutJump fn (JXX cc id')
    Just (DestImm imm)     -> shortcutJump fn (JXX_GBL cc imm)

shortcutJump _ other = other



-- -----------------------------------------------------------------------------
-- Generating spill instructions

mkSpillInstr
   :: Reg		-- register to spill
   -> Int		-- current stack delta
   -> Int		-- spill slot to use
   -> Instr

#if   i386_TARGET_ARCH
mkSpillInstr reg delta slot
  = let	off     = spillSlotToOffset slot
    in
    let off_w = (off-delta) `div` 4
    in case regClass reg of
	   RcInteger -> MOV II32 (OpReg reg) (OpAddr (spRel off_w))
	   _         -> GST FF80 reg (spRel off_w) {- RcFloat/RcDouble -}

#elif x86_64_TARGET_ARCH
mkSpillInstr reg delta slot
  = let	off     = spillSlotToOffset slot
    in
    let off_w = (off-delta) `div` 8
    in case regClass reg of
	   RcInteger -> MOV II64 (OpReg reg) (OpAddr (spRel off_w))
	   RcDouble  -> MOV FF64 (OpReg reg) (OpAddr (spRel off_w))
		-- ToDo: will it work to always spill as a double?
		-- does that cause a stall if the data was a float?
#else
mkSpillInstr _ _ _
    =   panic "X86.RegInfo.mkSpillInstr: not defined for this architecture."
#endif


mkLoadInstr
   :: Reg		-- register to load
   -> Int		-- current stack delta
   -> Int		-- spill slot to use
   -> Instr
#if   i386_TARGET_ARCH
mkLoadInstr reg delta slot
  = let off     = spillSlotToOffset slot
    in
	let off_w = (off-delta) `div` 4
        in case regClass reg of {
              RcInteger -> MOV II32 (OpAddr (spRel off_w)) (OpReg reg);
              _         -> GLD FF80 (spRel off_w) reg} {- RcFloat/RcDouble -}
#elif x86_64_TARGET_ARCH
mkLoadInstr reg delta slot
  = let off     = spillSlotToOffset slot
    in
	let off_w = (off-delta) `div` 8
        in case regClass reg of
              RcInteger -> MOV II64 (OpAddr (spRel off_w)) (OpReg reg)
              _         -> MOV FF64 (OpAddr (spRel off_w)) (OpReg reg)
#else
mkLoadInstr _ _ _
  =    panic "X86.RegInfo.mkLoadInstr: not defined for this architecture."
#endif



mkRegRegMoveInstr
    :: Reg
    -> Reg
    -> Instr
mkRegRegMoveInstr src dst
    = case regClass src of
        RcInteger -> MOV wordSize (OpReg src) (OpReg dst)
#if   i386_TARGET_ARCH
        RcDouble  -> GMOV src dst
#else
        RcDouble  -> MOV FF64 (OpReg src) (OpReg dst)
	RcFloat   -> panic "X86.RegInfo.mkRegRegMoveInstr: no match"
#endif


mkBranchInstr
    :: BlockId
    -> [Instr]

mkBranchInstr id = [JXX ALWAYS id]


spillSlotSize :: Int
spillSlotSize = IF_ARCH_i386(12, 8)

maxSpillSlots :: Int
maxSpillSlots = ((rESERVED_C_STACK_BYTES - 64) `div` spillSlotSize) - 1

-- convert a spill slot number to a *byte* offset, with no sign:
-- decide on a per arch basis whether you are spilling above or below
-- the C stack pointer.
spillSlotToOffset :: Int -> Int
spillSlotToOffset slot
   | slot >= 0 && slot < maxSpillSlots
   = 64 + spillSlotSize * slot
   | otherwise
   = pprPanic "spillSlotToOffset:" 
              (   text "invalid spill location: " <> int slot
	      $$  text "maxSpillSlots:          " <> int maxSpillSlots)
