{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}

-- | Construct and open an imported dependent pair in ordinary Haskell.
module ListVectorClient (fromList, contents) where

import GHC.Sigma (Sigma(Pair))
import PackedVecLibrary (natToInt, sourceLength, sourceVector, toList)
import StoredVector (ListVector)

-- | The same list supplies the length and the vector.
fromList :: [a] -> ListVector a
fromList xs = Pair (sourceLength xs) (sourceVector xs)

-- | Read either field independently.
contents :: ListVector a -> (Int, [a])
contents (Pair n xs) = (natToInt n, toList xs)
