{-# LANGUAGE TemplateHaskell #-}
module T15815A where

mkFoo tyQ = [d|
    foo :: a ~ $(tyQ) => a
    foo = undefined
  |]
