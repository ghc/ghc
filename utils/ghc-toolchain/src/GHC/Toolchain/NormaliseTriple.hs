module GHC.Toolchain.NormaliseTriple where

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program
import Data.Text (strip, pack, unpack)

-- | Normalise the triple by calling `config.sub` on the given triple.
normaliseTriple :: String -> M String
normaliseTriple triple = do
  let norm = unpack . strip . pack
  normalised_triple <- norm <$> readProgramStdout shProgram ["config.sub", triple]
  logInfo $ unwords ["Normalised triple:", triple, "~>", normalised_triple]
  return normalised_triple
