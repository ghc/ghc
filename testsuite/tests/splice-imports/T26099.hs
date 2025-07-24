{-# LANGUAGE TemplateHaskell #-}
module M where

type T = Int

--a :: forall z . z
--a = $(undefined :: z)

b = $(undefined :: T)
