{-# LANGUAGE LinearTypes #-}

import GHC.Types

f = id :: a %(m n :: Multiplicity) -> a
