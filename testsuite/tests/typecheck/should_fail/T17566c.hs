{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T17566c where

import Data.Kind

class C2 f z where
  type T2 (f :: k -> Type)
  data D2 (x :: (f :: k -> Type) a)
