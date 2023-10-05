{-# LANGUAGE DataKinds
           , PolyKinds
           , FlexibleContexts
           , FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , OverloadedLabels
           , ScopedTypeVariables
           , TypeApplications
           , TypeOperators
           , UndecidableInstances
  #-}

import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits
import Data.Kind

data Label (x :: Symbol) = Label
data Labelled x a = Label x := a

data Rec :: forall k. [(k, Type)] -> Type where
  Nil  :: Rec '[]
  (:>) :: Labelled x a -> Rec xs -> Rec ('(x, a) ': xs)
infixr 5 :>

instance {-# OVERLAPS #-} a ~ b => HasField foo (Rec ('(foo, a) ': xs)) b where
  getField ((_ := v) :> _) = v

instance HasField foo (Rec xs) b => HasField foo (Rec ('(bar, a) ': xs)) b where
  getField (_ :> vs) = getField @foo vs

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
