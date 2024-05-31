{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}
module T24911 where

$([d| infixl 4 data ###
      (###) :: a -> a -> a
      x ### y = x

      infixl 4 type ###
      type (###) :: a -> a -> a
      type x ### y = x
    |])
