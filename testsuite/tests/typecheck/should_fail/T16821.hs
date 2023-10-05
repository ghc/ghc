{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedNewtypes #-}
module T16821 where

import Data.Kind

type family Id (x :: Type) :: Type where
  Id x = x

newtype T :: Id Type where
  MkT :: Int -> T

f :: T -> T
f (MkT x) = MkT (x + 1)
