{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ImpredicativeTypes, RankNTypes, TypeApplications, TypeFamilies #-}
module WitnessMismatch where
import Data.Kind (Type)
type family Key (a :: Type) :: Type where
    Key a = Bool
type P = forall a. a -> a
data Pair x y = Pair x y
type Bad = exists (a :: Type). Pair (Key a) a
wrong :: Pair Int P
wrong = Pair @Int @P 3 id
bad :: Bad
bad = wrong
