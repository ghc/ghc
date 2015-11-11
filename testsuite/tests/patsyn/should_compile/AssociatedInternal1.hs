{-# LANGUAGE PatternSynonyms #-}
module AssociatedInternal1 (A(NewA,MkA, NoA)) where

newtype A = NewA (Maybe Int)

pattern MkA n = NewA (Just n)

pattern NoA = NewA Nothing
