{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, FlexibleContexts #-}

module T11990a where

import GHC.TypeLits
import Data.Proxy

type family PartialTF t :: Symbol where
  PartialTF Int  = "Int"
  PartialTF Bool = "Bool"
  PartialTF a    = TypeError (Text "Unexpected type @ PartialTF: "
                                                         :<>: ShowType a)

testPartialTF :: forall a.(KnownSymbol (PartialTF a)) => a -> String
testPartialTF t = symbolVal (Proxy :: Proxy (PartialTF a))

t1 = testPartialTF 'a'

{- Above code rightly fails with the following error:
    • Unexpected type: Char
    • In the expression: testPartialTF 'a'
      In an equation for ‘t1’: t1 = testPartialTF 'a'
-}
