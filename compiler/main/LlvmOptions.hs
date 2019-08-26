module LlvmOptions where

import GhcPrelude

data LlvmOptions = LlvmOptions
  { llvmOptions_fillUndefWithGarbage :: Bool
  }

class HasLlvmOptions a where
  getLlvmOptions :: a -> LlvmOptions
