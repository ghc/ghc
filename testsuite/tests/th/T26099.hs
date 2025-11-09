{-# LANGUAGE TemplateHaskell #-}
module M where

type T = Int

a = $(3 :: T)
