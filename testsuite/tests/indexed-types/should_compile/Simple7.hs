{-# LANGUAGE TypeFamilies #-}

module ShouldCompile where

class C1 a where
  data S1 a :: *

-- instance of data families can be data or newtypes
instance C1 Char where
  newtype S1 Char = S1Char ()
