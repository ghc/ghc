{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, FlexibleContexts #-}

module T11990b where

import GHC.TypeLits
import Data.Proxy

type family PartialTF t :: Symbol where
  PartialTF Int  = "Int"
  PartialTF Bool = "Bool"
  PartialTF a    = TypeError (Text "Unexpected type @ PartialTF: "
                                                         :<>: ShowType a)

type family NestedPartialTF (tsym :: Symbol) :: Symbol where
  NestedPartialTF "Int" = "int"
  NestedPartialTF "Bool" = "bool"
  NestedPartialTF a =
    TypeError (Text "Unexpected type @ NestedPartialTF: " :<>: ShowType a)

testPartialTF :: forall a.(KnownSymbol (PartialTF a)) => a -> String
testPartialTF t = symbolVal (Proxy :: Proxy (PartialTF a))

testNesPartialTF ::
  forall a.(KnownSymbol (NestedPartialTF (PartialTF a))) => a -> String
testNesPartialTF t = symbolVal (Proxy :: Proxy (NestedPartialTF (PartialTF a)))

t2 = testNesPartialTF 'a'
