{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module T17384 where

import Data.Kind

data T :: Type -> Type where
  MkT :: T (Maybe Bool)
