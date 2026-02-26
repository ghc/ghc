module GHC.Toolchain.NormaliseTriple
  ( normaliseTriple
  , normaliseLlvmTarget
  ) where

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program
import Data.Text (strip, pack, unpack)
import Data.List (isPrefixOf)

-- | Normalise the triple by calling `config.sub` on the given triple.
normaliseTriple :: String -> M String
normaliseTriple triple = do
  let norm = unpack . strip . pack
  normalised_triple <- norm <$> readProgramStdout shProgram ["config.sub", triple]
  logInfo $ unwords ["Normalised triple:", triple, "~>", normalised_triple]
  return normalised_triple

-- | Normalise the LLVM target triple for platform conventions.
--
-- Apple's LLVM toolchain uses @arm64@ as the canonical architecture name
-- for AArch64 on Apple platforms, while GNU config.sub normalises to
-- @aarch64@. This mismatch causes problems with toolchain wrappers (e.g.
-- nix cc-wrapper) that do string comparison on the @--target@ flag.
--
-- See also: the @llvm-targets@ file uses @arm64-apple-darwin@.
normaliseLlvmTarget :: String -> String
normaliseLlvmTarget triple
  | "aarch64-apple-" `isPrefixOf` triple
  = "arm64-" ++ drop (length "aarch64-") triple
  | otherwise
  = triple
