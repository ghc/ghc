{-# LANGUAGE TypeFamilies, RecordWildCards #-}
module AssocWildCards where

class FooClass a where
   data FooType a

instance FooClass Int where
   data FooType Int = FooInt { fooIntVal :: Int }

fooInt :: Int -> FooType Int
fooInt fooIntVal = FooInt{..}

fromFooInt :: FooType Int -> Int
fromFooInt (FooInt{..}) = fooIntVal
