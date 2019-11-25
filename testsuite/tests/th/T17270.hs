{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
module T17270 where

import Data.Type.Equality

f :: a :~: Int -> b :~: Bool -> a :~: b -> void
f Refl Refl x = case x of {}

$([d| g :: a :~: Int -> b :~: Bool -> a :~: b -> void
      g Refl Refl x = case x of {}
    |])
