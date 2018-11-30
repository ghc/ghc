{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T14230a where

import Data.Kind

class C a where
  data CD k (a :: k) :: k -> *

instance C (Maybe a) where
  data CD k (a :: k -> *) :: (k -> *) -> *
