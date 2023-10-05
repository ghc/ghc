{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module T14875 where

$([d| f :: Bool -> Bool
      f x = case x of
              (True :: Bool) -> True
              (False :: Bool) -> False

      g :: Bool -> Bool
      g x = (case x of
               True -> True
               False -> False) :: Bool
    |])
