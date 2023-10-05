{-# LANGUAGE TypeFamilies #-}
module T9896 where

class Test a where
  type TestT a :: *

instance Test Bool where
  newtype TestT Bool = Int
