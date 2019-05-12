{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module T16356_Fail2 where

class C (a :: j) where
  type T (a :: j) (b :: k)
  type T @k @k a b = k
