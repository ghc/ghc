module DLX
  (
      Instr(..)
     ,BranchFunc(..)
     ,ImmOpcode(..)
     ,RegOpcode(..)
     ,JmpOpcode(..)
     ,DLXReg(..)
     ,SrcReg
     ,DstReg
     ,DLX_Trans
     ,DLXCell
     ,DLX_Word
     ,DLX_Instr
     ,DLX_InstrMem
     ,dlx2trans
     ,VDLXTrans
     ,VReg
     ,VTrans
     ,module DLX_Cell
     ,module DLX_Reg
     ,module DLX_Op
  ) where


import Ix
import Hawk
import Word
import Trans
import DLX_Cell
import DLX_Reg
import DLX_Op

type DLX_Word = Word32
type VDLXTrans = VTrans DLXReg DLX_Word
type VReg a = Virtual a Int
type VTrans r w = Trans DLX_Op (VReg r) 

fillIn x= fillInCells x 


data Instr reg i
           = ImmIns     ImmOpcode reg reg i |
             RegReg     RegOpcode AluOp reg reg reg |
             Jmp        JmpOpcode Int |
             Nop
             deriving (Eq,Show, Read)


data BranchFunc = Never | Always | IfEqZero | IfNeqZero
            deriving (Eq,Show, Read)

data ImmOpcode = LoadStoreImm LoadStoreOp |
           ALUImm AluOp |
           BEQZ | BNEZ |
           JR |
           JALR 
           deriving (Eq,Show, Read)

data RegOpcode = MOVI2S | MOVS2I |
           ALU 
           deriving (Eq,Show, Read)

data JmpOpcode = J |
           JAL |
           TRAP |
           RFE
           deriving (Eq,Show, Read)



instance Register DLXReg where
     readOnly R0 = True
     readOnly Dummy = True
     readOnly _  = False
     pc = PC
     specpc = SpecPC
--     specpc = PC
--  bug fix?  Thu Nov 19 18:12:24 PST 1998
     ispc x = PC == x
     isspecpc x = SpecPC == x

type DLXCell a = DLX_Cell DLXReg a
type DLX_Trans a = Trans DLX_Op (DLXCell a)
type DLX_Instr a = Instr DLXReg a

type DLX_InstrMem a = InstrMemoryState DLX_Word (DLX_Instr a)


type SrcReg = DLXReg     -- Source register
type DstReg = DLXReg     -- Destination register



regNothing R0 = Reg R0 (Val 0)
regNothing reg = Reg reg NotKnown

dlx2trans :: Word2 i a => Instr DLXReg i -> DLX_Trans a

dlx2trans (ImmIns (LoadStoreImm loadOp@(Load _ _ )) dest src offset)
  = Trans [regNothing dest] (MemOp loadOp) 
          [regNothing src,Imm (toWord offset)] []
 --         [regNothing src,Imm (toWord offset),regNothing Dummy] []

{-
dlx2trans (ImmIns (LoadStoreImm storeOp@(Store _ )) writeAddr writeReg offset)
  = Trans [regNothing Dummy] (MemOp storeOp) [regNothing writeAddr,
			      Imm (toWord offset),
			      regNothing writeReg] []

dlx2trans (ImmIns (ALUImm SetHi) dest _ imm)
  = Trans [destCell] (ExecOp SetHi) [Imm (toWord imm)] []
    where
      destCell = regNothing dest

dlx2trans (ImmIns (ALUImm aluFunc) dest src imm)
  = Trans [destCell] (ExecOp aluFunc) [srcCell,Imm (toWord imm)] []
    where
      destCell = regNothing dest
      srcCell = regNothing src

dlx2trans (ImmIns BEQZ _ src pcOffset)
  = Trans [pcNothing'] (CondExecOp (Add Signed) Input1) [regNothing src,
							    pcNothing',
							    Imm (toWord pcOffset)]
          []

dlx2trans (ImmIns BNEZ _ src pcOffset)
  = Trans [pcNothing'] (CondExecOp Input1 (Add Signed)) [regNothing src,
							    pcNothing',
							    Imm (toWord pcOffset)]
    []

dlx2trans (ImmIns JR _ src _ )
  = Trans [pcNothing'] (ExecOp Input1) [regNothing src] []


dlx2trans (RegReg ALU aluFunc dest src1 src2)
  = Trans [regNothing dest] (ExecOp aluFunc) [regNothing src1, regNothing src2] []

dlx2trans (RegReg unknownOp _ _ _ _ )
  = error ("Can't translate " ++ show unknownOp)

dlx2trans (Jmp J offset)
  = Trans [pcNothing'] (ExecOp (Add Signed)) [pcNothing', Imm (toWord offset)] []

dlx2trans (ImmIns JALR _ src _ )
  = Trans [pcNothing',regNothing R31]
	  (ParExecOp Input1 Input2)
	  [regNothing src, pcNothing'] []

dlx2trans (Jmp JAL offset)
  = Trans [pcNothing',regNothing R31] 
	  (ParExecOp (Add Signed) Input2)
	  [Imm (toWord offset),pcNothing']
          []


dlx2trans (Jmp TRAP offset )
  = Trans [pcNothing',regNothing IAR]
	  (ParExecOp Input1 Input2)
	  [Imm (toWord offset),pcNothing']
          []


dlx2trans (Jmp RFE _ )
  = Trans [pcNothing'] (ExecOp Input1) [regNothing IAR] []

dlx2trans Nop
--  = Trans [Reg Dummy (Val 0)] (NoOp "dlx2trans") [] []
    = Trans [] (NoOp "dlx2trans") [] []

-}

pcNothing' = Reg PC NotKnown


instance Show a => Probe (DLXCell a)
instance Probe DLXReg

instance Probe DLX_Op where
  outp (ExecOp (Add _ ))   = "+"
  outp (ExecOp (Sub _ ))   = "-"
  outp (ExecOp (Div _ ))   = "/"
  outp (ExecOp (Mult _ ))  = "*"
  outp (ExecOp op)         = show op
  outp (MemOp (Load _ _))  = "Load"
  outp (MemOp (Store _))   = "Store"
  outp (ParExecOp op1 op2) = "PAR("++outp op1++","++outp op2 ++ ")"
  outp x                   = show x


instance Show a => Probe (DLX_Trans a) where
   outp (Trans [] op [] i) = outp op ++  outInfo i
   outp (Trans [x] (CondExecOp op1 op2) [c,y,z] i)
       = outp x ++ " <- " ++ "(if0 " ++ outp c ++ " ("
            ++ outp op1 ++ "," ++ outp op2 ++ ")) "
            ++ outp y  ++ " " ++ outp z
            ++ outInfo i
   outp (Trans dummy (MemOp (Store x)) [c,y,z] i)
       = outp (MemOp (Store x)) ++" "++ outp c ++"("++ outp y ++") <- "
         ++  outp z ++ outInfo i
   outp (Trans [o] op [x,y] i)
       = outp o ++ " <- " ++ outp x ++ " " ++ outp op ++ " " ++ outp y
                          ++ outInfo i
   outp (Trans [] op l i) = outp op ++" "++ outList l ++ outInfo i
   outp (Trans [o] op l i)
       = outp o ++ " <- " ++ outp op ++" "++ outList l ++ outInfo i
   outp (Trans l1 op l2 i)
       = outList l1 ++" <- "++ outp op ++" "++ outList l2 ++ outInfo i

outInfo [] = ""
outInfo l = "  {" ++ foldr1 (\x y -> x ++ "," ++ y) (map outp l) ++ "}"

outList [] = ""
outList [x] = outp x
outList l = "[" ++ foldr1 (\x y -> x ++ "," ++ y) (map outp l) ++ "]"










