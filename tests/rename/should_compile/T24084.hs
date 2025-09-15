{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module T24084 where

import T24084_B (Foo, Bar)

data X

instance Foo X where
  type Bar X = X
