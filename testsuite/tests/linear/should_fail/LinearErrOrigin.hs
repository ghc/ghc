{-# LANGUAGE LinearTypes #-}
module LinearErrOrigin where

import GHC.Types (Multiplicity)

-- The error message should mention "arising from multiplicity of x".

foo :: (a %(p :: Multiplicity) -> b) -> a %(q :: Multiplicity) -> b
foo f x = f x
