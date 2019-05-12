{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wunused-type-patterns #-}
module T16356_Compile2 where

class C (a :: j) where
  type T1 (a :: j) (b :: k)
  type T1 @j @_ a _ = Int

  type T2 (a :: j) (b :: k)
  type forall j (a :: j). T2 a _ = Int

  type T3 (a :: j) (b :: k)
  type forall j (a :: j). T3 @j @_ a _ = Int
