{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module T10598_fail6 where

newtype F x = F ([x], Maybe x) deriving Functor
