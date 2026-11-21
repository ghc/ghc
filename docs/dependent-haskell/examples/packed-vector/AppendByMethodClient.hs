{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}
module AppendByMethodClient (appendTwoAndOneByMethod) where

import qualified GHC.TypeNats as N
import GHC.TypeNats.Experimental (FromNatural)
import RetainedSumOperator (appendByMethod)
import PackedVecLibrary (Nat(..), Vec(..), replicateNative, toList)

type Length n = FromNatural Nat n

two :: Vec Char (Length 2)
two = replicateNative 2 'a'

one :: Vec Char (Length 1)
one = replicateNative 1 'b'

joined :: Vec Char (Length (2 N.+ 1))
joined = appendByMethod 2 1 two one

headVec :: Vec a ('S n) -> a
headVec (VCons x _) = x

-- | Observe an append of lengths two and one, its head, and an empty append.
appendTwoAndOneByMethod :: (String, Char, String)
appendTwoAndOneByMethod = (toList joined, headVec joined, toList empty)
  where
    empty :: Vec Char (Length 0)
    empty = appendByMethod 0 0 VNil VNil
