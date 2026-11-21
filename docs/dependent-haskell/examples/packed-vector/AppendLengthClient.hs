{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}
module AppendLengthClient (appendTwoAndOne) where

import qualified GHC.TypeNats as N
import GHC.TypeNats.Experimental (FromNatural)
import PackedVecLibrary (Nat(..), Vec(..), appendNative, replicateNative, toList)

type Length n = FromNatural Nat n

two :: Vec Char (Length 2)
two = replicateNative 2 'a'

one :: Vec Char (Length 1)
one = replicateNative 1 'b'

joined :: Vec Char (Length (2 N.+ 1))
joined = appendNative 2 1 two one

headVec :: Vec a ('S n) -> a
headVec (VCons x _) = x

appendTwoAndOne :: (String, Char, String)
appendTwoAndOne = (toList joined, headVec joined, toList empty)
  where
    empty :: Vec Char (Length 0)
    empty = appendNative 0 0 VNil VNil
