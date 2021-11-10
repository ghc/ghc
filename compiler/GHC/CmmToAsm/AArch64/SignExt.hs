module GHC.CmmToAsm.AArch64.SignExt
    ( elideRedundantExtensions
    ) where

import GHC.Prelude

import GHC.Platform.Reg
import GHC.CmmToAsm.Instr hiding (regUsageOfInstr)
import GHC.CmmToAsm.AArch64.Instr
import GHC.Cmm.Type
import GHC.Platform

import Data.Maybe (fromMaybe)
import GHC.Data.OrdList
import qualified Data.Map.Strict as M

-- | An environment mapping registers to the current 'RegExtState' of their contents.
-- For instance, an entry of @(ZeroExt, W8)@ means that the register contains
-- an 8-bit value in zero-extended form; that is, the top 48 bits are zero.
-- If a register is not present in the environment then it is in an unknown
-- state and must be extended appropriately on usage.
type RegExtensionEnv = M.Map Reg (ExtType, Width)

data ExtType = SignExt | ZeroExt
  deriving (Eq, Ord)

elideRedundantExtensions :: Platform -> OrdList Instr -> OrdList Instr
elideRedundantExtensions platform = toOL . go mempty . fromOL
  where
    go :: RegExtensionEnv -> [Instr] -> [Instr]
    go _   [] = []
    go env (instr:rest)
      | Just (reg, extType, tgtWidth) <- isRegExtensionInstr instr 
      , Just (s0, w0) <- M.lookup reg env
      , extType == s0
      , tgtWidth == w0
        -- the extension is redundant
      = go env' rest

      | otherwise
      = instr : go env' rest
      where
        env' = instrExtSig platform instr env

isRegExtensionInstr :: Instr -> Maybe (Reg, ExtType, Width)
isRegExtensionInstr instr =
    case instr of
      SXTB a b -> simple a b SignExt W8
      UXTB a b -> simple a b ZeroExt W8
      SXTH a b -> simple a b SignExt W16
      UXTH a b -> simple a b ZeroExt W16
      _        -> Nothing
  where
    simple :: Operand -> Operand -> ExtType -> Width -> Maybe (Reg, ExtType, Width)
    simple a b extType w
      | Just (ra, _) <- isReg a
      , Just (rb, _) <- isReg b
      , ra == rb
      = Just (ra, extType, w)
      | otherwise
      = Nothing

-- | Update the 'RegExtensionEnv' after execution of the given instruction.
instrExtSig :: Platform -> Instr -> RegExtensionEnv -> RegExtensionEnv
instrExtSig platform instr =
    case instr of
      -- extensions
      SBFM a b c d          -> resS a
      UBFM a b c d          -> resZ a
      SBFX a b c d          -> resS a
      UBFX a b c d          -> resZ a
      SXTB a b              -> resS a
      UXTB a b              -> resZ a
      SXTH a b              -> resS a
      UXTH a b              -> resZ a

      -- arithmetic in general doesn't preserve extension state
      ADD a b c             -> clobber a
      MSUB a b c d          -> clobber a
      MUL a b c             -> clobber a
      SUB a b c             -> clobber a
      NEG a b               -> clobber a

      -- division can't affect high bits
      SDIV a b c            -> resS a
      UDIV a b c            -> resZ a

      -- bitwise operations don't affect high bits and therefore preserve extension state
      AND{}                 -> id
      EOR{}                 -> id
      ORR{}                 -> id

      -- loads always zero-extend
      LDR w dst _           -> resZ dst

      -- moves are tricky
      MOV dst src           -> moveTo src dst

      -- otherwise conservatively clobber the destination registers
      _                     -> clobberAll
  where
    moveTo :: Operand -> Operand -> RegExtensionEnv -> RegExtensionEnv
    moveTo src dst env
      | Just (rb, w) <- isReg dst
      = let state = fromMaybe (ZeroExt, w) $ do
              (ra, _) <- isReg src
              M.lookup ra env
        in M.insert rb state env
    moveTo _src _dst env = env

    resS, resZ :: Operand -> RegExtensionEnv -> RegExtensionEnv
    resS = res SignExt
    resZ = res ZeroExt

    -- produces a result
    res :: ExtType -> Operand -> RegExtensionEnv -> RegExtensionEnv
    res extType x env | Just (r, w) <- isReg x =
        M.insert r (extType, w) env
    res _ _ env = env

    clobberAll :: RegExtensionEnv -> RegExtensionEnv
    clobberAll env =
        foldl' (flip M.delete) env $ writes (regUsageOfInstr platform instr)

    clobber :: Operand -> RegExtensionEnv -> RegExtensionEnv
    clobber o env
      | Just (r, _) <- isReg o
      = M.delete r env
      | otherwise
      = env

isReg :: Operand -> Maybe (Reg, Width)
isReg (OpReg w r) = Just (r, w)
isReg (OpRegExt w r _ _) = Just (r, w)
isReg (OpRegShift w r _ _) = Just (r, w)
isReg _ = Nothing

