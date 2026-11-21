{-# LANGUAGE NoExperimentalDependentTypes #-}
module OrdinaryEitherMapping (example) where

import qualified PreludeFmapEitherProof as Proof

-- | Show the checked equations with the corresponding runtime results.
example :: String
example = show ( Proof.example
               , fmap not (Left 'x' :: Either Char Bool)
               , fmap not (Right True :: Either Bool Bool) )
