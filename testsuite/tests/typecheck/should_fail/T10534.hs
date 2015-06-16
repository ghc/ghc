{-# LANGUAGE TypeFamilies #-}

module T10534 where

import T10534a

newtype instance DF a = MkDF ()

unsafeCoerce :: a -> b
unsafeCoerce = silly
