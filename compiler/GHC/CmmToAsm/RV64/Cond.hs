module GHC.CmmToAsm.RV64.Cond  where

import GHC.Prelude

-- FIXME: These conditions originate from the Aarch64 backend.  I'm not even sure
-- we use all of them there. For RISCV we need to synthesize some of them, as
-- RISCV has a much more reduced (ha!) set of condtionals.

-- TODO: This appears to go a bit overboard? Maybe we should stick with what LLVM
-- settled on for fcmp?
-- false: always yields false, regardless of operands.
-- oeq: yields true if both operands are not a QNAN and op1 is equal to op2.
-- ogt: yields true if both operands are not a QNAN and op1 is greater than op2.
-- oge: yields true if both operands are not a QNAN and op1 is greater than or equal to op2.
-- olt: yields true if both operands are not a QNAN and op1 is less than op2.
-- ole: yields true if both operands are not a QNAN and op1 is less than or equal to op2.
-- one: yields true if both operands are not a QNAN and op1 is not equal to op2.
-- ord: yields true if both operands are not a QNAN.
-- ueq: yields true if either operand is a QNAN or op1 is equal to op2.
-- ugt: yields true if either operand is a QNAN or op1 is greater than op2.
-- uge: yields true if either operand is a QNAN or op1 is greater than or equal to op2.
-- ult: yields true if either operand is a QNAN or op1 is less than op2.
-- ule: yields true if either operand is a QNAN or op1 is less than or equal to op2.
-- une: yields true if either operand is a QNAN or op1 is not equal to op2.
-- uno: yields true if either operand is a QNAN.
-- true: always yields true, regardless of operands.
--
-- LLVMs icmp knows about:
-- eq: yields true if the operands are equal, false otherwise. No sign interpretation is necessary or performed.
-- ne: yields true if the operands are unequal, false otherwise. No sign interpretation is necessary or performed.
-- ugt: interprets the operands as unsigned values and yields true if op1 is greater than op2.
-- uge: interprets the operands as unsigned values and yields true if op1 is greater than or equal to op2.
-- ult: interprets the operands as unsigned values and yields true if op1 is less than op2.
-- ule: interprets the operands as unsigned values and yields true if op1 is less than or equal to op2.
-- sgt: interprets the operands as signed values and yields true if op1 is greater than op2.
-- sge: interprets the operands as signed values and yields true if op1 is greater than or equal to op2.
-- slt: interprets the operands as signed values and yields true if op1 is less than op2.
-- sle: interprets the operands as signed values and yields true if op1 is less than or equal to op2.

data Cond
    = ALWAYS -- b.al
    | EQ     -- b.eq
    | NE     -- b.ne
    -- signed
    | SLT    -- b.lt
    | SLE    -- b.le
    | SGE    -- b.ge
    | SGT    -- b.gt
    -- unsigned
    | ULT    -- b.lo
    | ULE    -- b.ls
    | UGE    -- b.hs
    | UGT    -- b.hi
    -- ordered
    | OLT    -- b.mi
    | OLE    -- b.ls
    | OGE    -- b.ge
    | OGT    -- b.gt
    -- unordered
    | UOLT   -- b.lt
    | UOLE   -- b.le
    | UOGE   -- b.pl
    | UOGT   -- b.hi
    -- others
    | NEVER  -- b.nv
    | VS     -- oVerflow set
    | VC     -- oVerflow clear
    deriving (Eq, Show)
