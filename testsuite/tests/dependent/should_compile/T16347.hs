{-# LANGUAGE GADTs      #-}
{-# LANGUAGE TypeInType #-}
module T16347 where

import Data.Kind

data T f :: f Type -> Type where
  MkT :: T f a
