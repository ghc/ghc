{-# LANGUAGE TypeFamilies #-}

module T12127a where

class C a where
  data T a

instance C Int where
  data T Int = MkT { x, y :: Int }
