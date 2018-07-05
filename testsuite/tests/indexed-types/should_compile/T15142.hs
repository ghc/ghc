{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}


module T15142 where

import Data.Kind

class ListTuple (tuple :: Type) (as :: [(k, Type)]) where
  type ListToTuple as :: Type

class C (a :: Type) (b :: k) where
  type T b
