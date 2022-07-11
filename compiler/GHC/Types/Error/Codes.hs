-- Defines types for diagnostic codes.
-- Separate from GHC.Types.Error to avoid a (worse)
-- module loop with GHC.Driver.Errors.Ppr

module GHC.Types.Error.Codes (

     -- * Diagnostic codes
     DiagnosticCode
   , mkDiagnosticCode
   , GhcDiagnosticCode
   , prefixGhcDiagnosticCode
   , numDigitsInGhcDiagnosticCode
   , ghcDiagnosticCodeNumber
  ) where

import GHC.Prelude
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Text.Printf

----------------------------------------------------------------
--                                                            --
-- Diagnostic Codes                                           --
--                                                            --
----------------------------------------------------------------

{- Note [Diagnostic codes]
~~~~~~~~~~~~~~~~~~~~~~~~~~
"RAE": Write note.
Talk about difference between DiagnosticCode and GhcDiagnosticCode.
Talk about aspirations to remove Maybe.
-}

-- | A diagnostic code (called an "error code" in its specification
-- at "RAE": TODO) has a prefix and a suffix. Briefly, the prefix is
-- an alphanumeric string assigned by the Haskell Foundation (in order
-- to keep codes from different tools distinct). The suffix is a string
-- of digits uniquely identifying a diagnostic.
--
-- To make a 'DiagnosticCode' from a 'GhcDiagnosticCode', see 'prefixGhcDiagnosticCode'.
--
-- See also Note [Diagnostic codes]
data DiagnosticCode = MkDiagnosticCode SDoc SDoc

mkDiagnosticCode :: SDoc   -- ^ prefix of diagnostic code; must be assiged by Haskell Foundation
                 -> SDoc   -- ^ suffix of diagnostic code; must be a string of digits
                 -> DiagnosticCode
mkDiagnosticCode = MkDiagnosticCode

instance Outputable DiagnosticCode where
  ppr (MkDiagnosticCode prefix suffix) = brackets $ prefix <> char '-' <> suffix

-- | Convert the GHC-specific 'GhcDiagnosticCode' to a tool-agnostic
-- 'DiagnosticCode' by adding the @GHC-@ prefix.
prefixGhcDiagnosticCode :: GhcDiagnosticCode -> DiagnosticCode
prefixGhcDiagnosticCode (MkGhcDiagnosticCode n)
  = MkDiagnosticCode (text ghcDiagnosticCodePrefix) (text ppr_n)
  where
    ppr_n = printf format_string n
    format_string = "%0" ++ show numDigitsInGhcDiagnosticCode ++ "d"

-- | The code used within GHC to label a diagnostic. See Note [Diagnostic codes].
newtype GhcDiagnosticCode = MkGhcDiagnosticCode Int
  deriving (Eq, Ord)

-- | Make it easy to write code without using the constructor
instance Num GhcDiagnosticCode where
  fromInteger = MkGhcDiagnosticCode . fromInteger

  (+) = panic "adding GhcDiagnosticCodes"
  (-) = panic "subtracting GhcDiagnosticCodes"
  (*) = panic "multiplying GhcDiagnosticCodes"
  abs = panic "abs GhcDiagnosticCode"
  negate = panic "negate GhcDiagnosticCode"
  signum = panic "signum GhcDiagnosticCode"

-- | Extract the diagnostic code number from a 'GhcDiagnosticCode'
ghcDiagnosticCodeNumber :: GhcDiagnosticCode -> Int
ghcDiagnosticCodeNumber (MkGhcDiagnosticCode n) = n

-- | The Haskell-Foundation-assigned prefix for GHC's diagnostic codes.
ghcDiagnosticCodePrefix :: String
ghcDiagnosticCodePrefix = "GHC"

-- | The minimum number of digits of a diagnostic code. Codes are prefixed
-- with 0s to print this many digits.
numDigitsInGhcDiagnosticCode :: Int
numDigitsInGhcDiagnosticCode = 5

-- This instance outputs the full diagnostic code, including its "GHC-"
-- prefix, and wraps it in brackets for visual distinction.
instance Outputable GhcDiagnosticCode where
  ppr = ppr . prefixGhcDiagnosticCode
