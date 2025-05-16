{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
module T25056a
  ( T
  , T_(unT)
  , data T
  ) where

type T = T_ ()

data T_ a = PrivateT { unT_ :: a }

pattern T :: a -> T_ a
pattern T { unT } <- PrivateT { unT_ = unT }
