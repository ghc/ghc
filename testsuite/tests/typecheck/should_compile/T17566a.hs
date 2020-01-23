{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T17566a where

import Data.Kind

class C1 (f :: k -> Type) z where
  type T1 (x :: f a) :: f a
  data D1 (x :: f a)

class C2 f z where
  type T2 (x :: f a) :: f a
  data D2 (x :: f a)
