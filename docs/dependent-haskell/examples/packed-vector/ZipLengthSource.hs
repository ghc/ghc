{-# LANGUAGE ExplicitForAll, NoExperimentalDependentTypes #-}

module ZipLengthSource (countWithHeader, zipLength) where

import PackedVecLibrary (Nat(..))

-- | Count the pairs produced by zipping two lists.
zipLength :: forall a b. [a] -> [b] -> Nat
zipLength [] _ = Z
zipLength (_ : _) [] = Z
zipLength (_ : xs) (_ : ys) = S (zipLength xs ys)

-- | Count the records if the header list has at least one entry.
countWithHeader :: forall a b. [a] -> [b] -> Nat
countWithHeader [] _ = Z
countWithHeader (_ : _) [] = Z
countWithHeader headers@(_ : _) (_ : rows) = S (countWithHeader headers rows)
