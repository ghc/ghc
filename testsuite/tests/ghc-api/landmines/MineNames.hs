{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-

Exercising avoidance of known landmines.

We need one each of

  PostTc id Kind
  PostTc id Type

  PostRn id Fixity
  PostRn id NameSet


-}
module MineNames where

foo :: Int
foo = 1
