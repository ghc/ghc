module DLX_Op (DLX_Op(..)) where

import Arithmetic
import Memory
import Instruction
import Probe

data DLX_Op = ExecOp AluOp |
	  CondExecOp AluOp AluOp |	-- If first source == zero, then the
					--  first AluOp is performed on rest
					--  of source arguments, otherwise the
					--  second AluOp is performed on them.
	  ParExecOp AluOp AluOp |	-- The first destination cell is the
					--  result of the first AluOp; the
					--  second destination cell is the
					--  result of the second AluOp.
	  MemOp LoadStoreOp |
	  NoOp String			-- Null operation. The string argument
					--  can be used as a comment indicating
					--  which phase generated a stall.
	  deriving (Eq,Show)

-- Begin Signature ---------------------------------------------------------
-- End Signature ---------------------------------------------------------

instance Instruction DLX_Op where
   isNoOp t = case t of
		NoOp _ -> True
		_      -> False   

   isAddOp t = case t of
		ExecOp (Add _) -> True
              	_ -> False 

   isSubOp t = case t of
             	ExecOp (Sub _) -> True   
             	_ -> False

   isMultOp t = case t of
             	ExecOp (Mult _) -> True
            	_ -> False
                     
   isDivOp t = case t of
           	ExecOp (Div _) -> True
            	_ -> False

   isJumpOp t = case t of
             	CondExecOp _ _ -> True
             	_ -> False           

   isMemOp t = case t of
            	MemOp _ -> True
             	_ -> False
 
   isLoadOp t = case t of
		MemOp (Load _ _) -> True
		_ -> False

   isStoreOp t = case t of
		MemOp (Store _) -> True
		_ -> False

   noOp = NoOp ""
   isAluOp t = case t of
                ExecOp _ -> True
                _ -> False

   isCmpOp t = case t of
                ExecOp (S _) -> True
                _ -> False

   isBoolOp t = case t of
                ExecOp Xor -> True
                ExecOp Or -> True
                ExecOp And -> True
                ExecOp Not -> True
                _ -> False

   isMoveOp t = case t of
		ExecOp Input1 -> True
		_ -> False
 
   aluOp (ExecOp f) = f

   isCond = isJumpOp

   isPar (ParExecOp _ _) = True
   isPar _ = False

   fstOp (ParExecOp f _) = f
   fstOp (CondExecOp f _) = f

   sndOp (ParExecOp f g) = g
   sndOp (CondExecOp f g) = g

   memOp (MemOp f) = f



