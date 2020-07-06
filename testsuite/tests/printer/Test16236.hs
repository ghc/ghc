{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies
             , TypeApplications, TypeInType #-}

module DumpParsedAst where
import Data.Kind

data Peano = Zero | Succ Peano

type family Length (as :: [k]) :: Peano where
  Length (a : as) = Succ (Length as)
  Length '[]      = Zero

-- vis kind app
data T f (a :: k) = MkT (f a)

type family F1 (a :: k) (f :: k -> Type) :: Type where
  F1 @Peano a f = T @Peano f a

data family DF3 (a :: k)
data instance DF3 @(K.Type -> K.Type) b = DF3Char
