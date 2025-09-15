{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE TypeOperators            #-}

-- from https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5899#note_407871

module T18851c where

-- base
import           Data.Kind (Type)

data Nat
type Plus1 :: Nat -> Nat
type family Plus1 n = r | r -> n

data V (n :: Nat) = V

data VSucc n where
  VSucc :: V n -> VSucc (Plus1 n)

foo :: VSucc n -> VSucc n -> VSucc n
foo (VSucc _) (VSucc _) = VSucc V
