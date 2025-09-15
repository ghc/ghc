{-# LANGUAGE TemplateHaskell #-}
module T14032e where

$([d| infix 5 :*:
      data a :*: b = a :*: b
    |])
