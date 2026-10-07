-- TODO Everthing in this module should be moved to
-- Language.Haskell.Syntax.Decls

module Language.Haskell.Syntax.Specificity (
        Specificity(..),
        ) where

import Prelude

import Control.DeepSeq (NFData(..))
import Data.Data

-- | Whether an 'Invisible' argument may appear in source Haskell.
data Specificity = InferredSpec
                   -- ^ the argument may not appear in source Haskell, it is
                   -- only inferred.
                 | SpecifiedSpec
                   -- ^ the argument may appear in source Haskell, but isn't
                   -- required.
  deriving (Eq, Ord, Data)

instance NFData Specificity where
  rnf SpecifiedSpec = ()
  rnf InferredSpec = ()
