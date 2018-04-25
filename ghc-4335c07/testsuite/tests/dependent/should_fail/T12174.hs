{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
module T12174 where

data V a
data T = forall (a :: S). MkT (V a)
data S = forall (a :: T). MkS (V a)
