{-# LANGUAGE NoExperimentalDependentTypes #-}

-- | Use imported retained contracts from an ordinary source module.
module OrdinaryVecClient (ordinaryReplicate, ordinaryRoundTrip, ordinaryZipExact) where

import PackedVecLibrary (Nat, fromListExact, genericVector, replicateNative, toList, zipVec)

{- Note [Ordinary callers of retained APIs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The imported signatures retain their runtime indices. Application checking
uses those signatures to infer the erased length required by toList.
The exported functions return ordinary lists. The native implementation still
needs the restricted compiler options documented in Showcase.hs.
-}

-- | Build an indexed vector at a runtime length and return its elements.
ordinaryReplicate :: Nat -> a -> [a]
ordinaryReplicate n x = toList (replicateNative n x)

-- | Preserve a list through a vector indexed by the actual input list.
ordinaryRoundTrip :: [a] -> [a]
ordinaryRoundTrip xs = toList (genericVector xs)

-- | Zip two lists when both lengths match the caller's requested size.
ordinaryZipExact :: Nat -> [a] -> [b] -> Maybe [(a, b)]
ordinaryZipExact n xs ys = do
    left <- fromListExact n xs
    right <- fromListExact n ys
    pure (toList (zipVec left right))
