{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
module T12174 where

data V a
data T = forall (a :: S). MkT (V a)
data S = forall (a :: T). MkS (V a)
