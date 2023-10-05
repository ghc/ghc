{-# LANGUAGE TypeFamilies, DataKinds #-}

module ExpandTFs where

-- from https://mail.haskell.org/pipermail/ghc-devs/2020-November/019366.html,
-- where it is requested to expand (Foo Int) in the error message

type family Foo a where Foo Int = String
type family Bar a :: Maybe (Foo Int) where Bar a = '()
