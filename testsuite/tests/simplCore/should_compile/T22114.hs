{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}

module T22114 where

import Data.Kind (Type)

value :: [Int] -> () -> Maybe Bool
value = valu
  where valu [0] = valuN
        valu _   = \_ -> Nothing

type family T :: Type where
  T = () -> Maybe Bool

valuN :: T
valuN = valuN
