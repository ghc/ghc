{-# LANGUAGE TypeFamilies #-}
module T3418 where

newtype (a ~ b) => S a b = S { unS :: a }
