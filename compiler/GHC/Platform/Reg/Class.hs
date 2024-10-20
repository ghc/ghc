{-# LANGUAGE LambdaCase #-}
module GHC.Platform.Reg.Class
  ( RegClass (..), RegArch(..), registerArch )
where

import GHC.Prelude

import GHC.Types.Unique
import GHC.Builtin.Uniques ( mkRegClassUnique )
import GHC.Platform.ArchOS
import GHC.Utils.Outputable ( Outputable(ppr), text )


-- | The class of a register.
--      Used in the register allocator.
--      We treat all registers in a class as being interchangeable.
--
newtype RegClass = RegClass Int
  deriving ( Eq, Ord, Show )

instance Uniquable RegClass where
  getUnique ( RegClass i ) = mkRegClassUnique i

-- | This instance is just used for the graph colouring register allocator.
-- Prefer using either 'GHC.Platform.Reg.Class.Separate.pprRegClass'
-- or 'GHC.Platform.Reg.Class.Unified.pprRegClass', which is more informative.
instance Outputable RegClass where
  ppr (RegClass i) = ppr i

-- | The register architecture of a given machine.
data RegArch
  -- | Floating-point and vector registers are unified (e.g. X86, AArch64).
  = Unified
  -- | Floating-point and vector registers are separate (e.g. RISC-V).
  | Separate
  -- | No vector registers.
  | NoVectors
  deriving ( Eq, Ord, Show )

instance Outputable RegArch where
  ppr regArch = text (show regArch)

-- | What is the register architecture of the given architecture?
registerArch :: Arch -> RegArch
registerArch arch =
  case arch of
    ArchX86       -> Unified
    ArchX86_64    -> Unified
    ArchPPC       -> Unified
    ArchPPC_64 {} -> Unified
    ArchAArch64   -> Unified
    ArchRISCV64   -> Separate
    _             -> NoVectors
