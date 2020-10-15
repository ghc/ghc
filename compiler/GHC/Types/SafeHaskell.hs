-- | This stuff here is related to supporting the Safe Haskell extension,
-- primarily about storing under what trust type a module has been compiled.
module GHC.Types.SafeHaskell
   ( IsSafeImport
   , SafeHaskellMode(..)
   , IfaceTrustInfo
   , getSafeMode
   , setSafeMode
   , noIfaceTrustInfo
   )
where

import GHC.Prelude

import GHC.Utils.Binary
import GHC.Utils.Outputable

import Data.Word


-- | Is an import a safe import?
type IsSafeImport = Bool

-- | The various Safe Haskell modes
data SafeHaskellMode
   = Sf_None          -- ^ inferred unsafe
   | Sf_Unsafe        -- ^ declared and checked
   | Sf_Trustworthy   -- ^ declared and checked
   | Sf_Safe          -- ^ declared and checked
   | Sf_SafeInferred  -- ^ inferred as safe
   | Sf_Ignore        -- ^ @-fno-safe-haskell@ state
   deriving (Eq)

instance Show SafeHaskellMode where
    show Sf_None         = "None"
    show Sf_Unsafe       = "Unsafe"
    show Sf_Trustworthy  = "Trustworthy"
    show Sf_Safe         = "Safe"
    show Sf_SafeInferred = "Safe-Inferred"
    show Sf_Ignore       = "Ignore"

instance Outputable SafeHaskellMode where
    ppr = text . show

-- | Safe Haskell information for 'ModIface'
-- Simply a wrapper around SafeHaskellMode to sepperate iface and flags
newtype IfaceTrustInfo = TrustInfo SafeHaskellMode

getSafeMode :: IfaceTrustInfo -> SafeHaskellMode
getSafeMode (TrustInfo x) = x

setSafeMode :: SafeHaskellMode -> IfaceTrustInfo
setSafeMode = TrustInfo

noIfaceTrustInfo :: IfaceTrustInfo
noIfaceTrustInfo = setSafeMode Sf_None

trustInfoToNum :: IfaceTrustInfo -> Word8
trustInfoToNum it
  = case getSafeMode it of
            Sf_None         -> 0
            Sf_Unsafe       -> 1
            Sf_Trustworthy  -> 2
            Sf_Safe         -> 3
            Sf_SafeInferred -> 4
            Sf_Ignore       -> 0

numToTrustInfo :: Word8 -> IfaceTrustInfo
numToTrustInfo 0 = setSafeMode Sf_None
numToTrustInfo 1 = setSafeMode Sf_Unsafe
numToTrustInfo 2 = setSafeMode Sf_Trustworthy
numToTrustInfo 3 = setSafeMode Sf_Safe
numToTrustInfo 4 = setSafeMode Sf_SafeInferred
numToTrustInfo n = error $ "numToTrustInfo: bad input number! (" ++ show n ++ ")"

instance Outputable IfaceTrustInfo where
    ppr (TrustInfo Sf_None)          = text "none"
    ppr (TrustInfo Sf_Ignore)        = text "none"
    ppr (TrustInfo Sf_Unsafe)        = text "unsafe"
    ppr (TrustInfo Sf_Trustworthy)   = text "trustworthy"
    ppr (TrustInfo Sf_Safe)          = text "safe"
    ppr (TrustInfo Sf_SafeInferred)  = text "safe-inferred"

instance Binary IfaceTrustInfo where
    put_ bh iftrust = putByte bh $ trustInfoToNum iftrust
    get bh = getByte bh >>= (return . numToTrustInfo)
