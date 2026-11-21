{-# LANGUAGE ExplicitForAll, NoExperimentalDependentTypes #-}

module ZipLengthSource (zipLength) where

import PackedVecLibrary (Nat(..))

-- | Count the pairs produced by zipping two lists.
zipLength :: forall a b. [a] -> [b] -> Nat
zipLength [] _ = Z
zipLength (_ : _) [] = Z
zipLength (_ : xs) (_ : ys) = S (zipLength xs ys)
