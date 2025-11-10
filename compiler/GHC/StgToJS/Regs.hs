{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module GHC.StgToJS.Regs
  ( StgReg (..)
  , Special(..)
  , sp
  , stack
  , r1, r2, r3, r4
  , pattern R1, pattern R2, pattern R3, pattern R4
  , regsFromR1
  , regsFromR2
  , regsFromTo
  , jsRegsFrom
  , jsRegsFromR1
  , jsRegsFromR2
  , StgRet (..)
  , regNumber
  , jsReg
  , highReg
  , highReg_expr
  , maxReg
  , maxLowReg
  , minReg
  , minHighReg
  , lowRegs
  , lowRegsCount
  , lowRegsIdents
  , retRegs
  , register
  , foreignRegister
  )
where

import GHC.Prelude

import GHC.JS.JStg.Syntax
import GHC.JS.Ident
import GHC.JS.Make

import GHC.StgToJS.Symbols

import GHC.Data.FastString
import GHC.Utils.Panic.Plain

import Data.Array
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.Semigroup ((<>))

-- | General purpose "registers"
newtype StgReg
  = StgReg Int
  deriving (Eq,Ord,Ix)

pattern R1, R2, R3, R4 :: StgReg
pattern R1 = StgReg 0
pattern R2 = StgReg 1
pattern R3 = StgReg 2
pattern R4 = StgReg 3

-- | Stack registers
data Special
  = Stack
  | Sp
  deriving (Show, Eq)

-- | Return registers
--
-- Extra results from foreign calls can be stored here (while first result is
-- directly returned)
data StgRet = Ret1 | Ret2 | Ret3 | Ret4 | Ret5 | Ret6 | Ret7 | Ret8 | Ret9 | Ret10
  deriving (Eq, Ord, Show, Enum, Bounded, Ix)

instance ToJExpr Special where
  toJExpr Stack  = hdStack
  toJExpr Sp     = hdStackPtr

instance ToJExpr StgReg where
  toJExpr r = register r

instance ToJExpr StgRet where
  toJExpr r = rets ! r

---------------------------------------------------
-- helpers
---------------------------------------------------

sp :: JStgExpr
sp = toJExpr Sp

stack :: JStgExpr
stack = toJExpr Stack

r1, r2, r3, r4 :: JStgExpr
r1 = toJExpr R1
r2 = toJExpr R2
r3 = toJExpr R3
r4 = toJExpr R4

-- | 1-indexed register number (R1 has index 1)
regNumber :: StgReg -> Int
regNumber (StgReg r) = r+1

-- | StgReg from 1-indexed number
regFromNumber :: Int -> StgReg
regFromNumber r = assert (r >= 1) $ StgReg (r-1)

regsFromTo :: StgReg -> StgReg -> [StgReg]
regsFromTo (StgReg x) (StgReg y) = map StgReg [x .. y]

-- | Register expression from its 1-indexed index
jsReg :: Int -> JStgExpr
jsReg r = toJExpr (regFromNumber r)

minReg :: StgReg
minReg = R1

maxReg :: StgReg
maxReg = regFromNumber maxBound

lowRegsCount :: Int
lowRegsCount = 31

maxLowReg :: StgReg
maxLowReg = regFromNumber lowRegsCount

-- | First register stored in h$regs array instead of having its own top-level
-- variable
minHighReg :: StgReg
minHighReg = case maxLowReg of
  StgReg r -> StgReg (r+1)

-- | List of registers, starting from R1
regsFromR1 :: [StgReg]
regsFromR1 = regsFromTo R1 maxReg ++ repeat (panic "StgToJS: code requires too many registers")

-- | List of registers, starting from R2
regsFromR2 :: [StgReg]
regsFromR2 = tail regsFromR1

-- | List of registers, starting from R1 as JStgExpr
jsRegsFromR1 :: [JStgExpr]
jsRegsFromR1 = fmap toJExpr regsFromR1

-- | List of registers, starting from R2 as JExpr
jsRegsFromR2 :: [JStgExpr]
jsRegsFromR2 = tail jsRegsFromR1

-- | List of registers, starting from given reg as JExpr
jsRegsFrom :: StgReg -> [JStgExpr]
jsRegsFrom (StgReg n) = drop n jsRegsFromR1

-- | High register
highReg :: Int -> JStgExpr
highReg r = assert (r >= regNumber minHighReg) $ IdxExpr hdRegs (toJExpr (r - regNumber minHighReg))

-- | High register indexing with a JS expression
highReg_expr :: JStgExpr -> JStgExpr
highReg_expr r = IdxExpr hdRegs (r - toJExpr (regNumber minHighReg))


---------------------------------------------------
-- caches
---------------------------------------------------

lowRegs :: [StgReg]
lowRegs = regsFromTo minReg maxLowReg

lowRegsIdents :: [Ident]
lowRegsIdents = map reg_to_ident lowRegs
  where
    -- low regs are named h$r1, h$r2, etc.
    reg_to_ident r = name (mkFastString (unpackFS hdStr ++ "r" ++ show (regNumber r)))

retRegs :: [Ident]
retRegs = [name . mkFastStringByteString
           $ hdB <> BSC.pack (map toLower $ show n) | n <- enumFrom Ret1]

-- cache JExpr representing StgRet
rets :: Array StgRet JStgExpr
rets = listArray (minBound, maxBound) (map retN (enumFrom Ret1))
  where
    retN = global . mkFastString . (unpackFS hdStr ++) . map toLower . show

-- | Given a register, return the JS syntax object representing that register
foreignRegister :: StgRet -> JStgExpr
foreignRegister i = rets ! i

-- | Given a register, return the JS syntax object representing that register
register :: StgReg -> JStgExpr
register i
  | i <= maxCachedReg = register_cache ! i -- Expressions of common registers are cached.
  | otherwise         = make_high_reg i    -- Expression of higher registers are made on the fly

maxCachedReg :: StgReg
maxCachedReg = regFromNumber 128

-- cache JExpr representing StgReg
register_cache :: Array StgReg JStgExpr
register_cache = listArray (minReg, maxCachedReg) (map (global . identFS) lowRegsIdents ++ map make_high_reg (regsFromTo minHighReg maxCachedReg))

-- | Make h$regs[XXX] expression for the register
make_high_reg :: StgReg -> JStgExpr
make_high_reg r = highReg (regNumber r)
