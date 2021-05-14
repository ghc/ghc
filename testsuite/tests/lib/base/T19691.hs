{-# Language DerivingStrategies       #-}
{-# Language TypeApplications         #-}
{-# Language GADTs                    #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeFamilies             #-}

import Data.Kind
import Type.Reflection

type Dict :: Constraint -> Type
data Dict cls where
 Dict :: cls => Dict cls

deriving stock
  instance Show (Dict cls)

type WitnessList :: forall k. k -> Type
data WitnessList as where
  WitnessList :: Typeable a => WitnessList (f a)

deriving stock
  instance Show (WitnessList as)

toDict :: TypeRep a -> Dict (Typeable a)
toDict TypeRep = Dict

witness :: TypeRep as -> Maybe (WitnessList as)
witness (App _ TypeRep) = Just WitnessList

main :: IO ()
main = do
  print (toDict  (TypeRep @[Int]))
  print (witness (TypeRep @[Int]))
