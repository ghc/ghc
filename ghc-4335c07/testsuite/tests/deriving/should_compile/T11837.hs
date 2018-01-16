{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
module T11837 where

class Category (cat :: k -> k -> *) where
  catId   :: cat a a
  catComp :: cat b c -> cat a b -> cat a c

newtype T (c :: k -> k -> *) a b = MkT (c a b) deriving Category
