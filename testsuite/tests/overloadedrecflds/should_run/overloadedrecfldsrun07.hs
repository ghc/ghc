{-# LANGUAGE DataKinds
           , PolyKinds
           , FlexibleContexts
           , FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , OverloadedLabels
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
           , TypeOperators
           , UndecidableInstances
  #-}

import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits
import Data.Kind
import Data.Proxy

data Label (x :: Symbol) = Label

instance KnownSymbol x => Show (Label x) where
  show _ = "#" ++ symbolVal (Proxy @x)

data Labelled x a = Label x := a
  deriving Show

data Rec :: forall k. [(k, Type)] -> Type where
  Nil  :: Rec '[]
  (:>) :: Labelled x a -> Rec xs -> Rec ('(x, a) ': xs)
instance Show (Rec '[]) where
  show Nil = "Nil"
deriving instance (KnownSymbol x, Show a, Show (Rec xs)) => Show (Rec ('(x, a) ': xs))
infixr 5 :>

instance {-# OVERLAPS #-} a ~ b => HasField foo (Rec ('(foo, a) ': xs)) b where
  hasField ((l := v) :> xs) = (\ v' -> (l := v') :> xs, v)

instance HasField foo (Rec xs) b => HasField foo (Rec ('(bar, a) ': xs)) b where
  hasField (x :> vs) = (\ v -> x :> setField @foo vs v, getField @foo vs)

instance y ~ x => IsLabel y (Label x) where
  fromLabel = Label

instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

x :: Rec '[ '("foo", Int), '("bar", Bool)]
x = #foo := 42 :> #bar := True :> Nil

y = #bar := 'x' :> undefined

main = do print (#foo x)
          print (#bar x)
          print (#bar y)
          print (setField @"foo" x 11)
