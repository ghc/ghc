{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module T8165_fail2 where

class C a where
  type T a

newtype Loop = MkLoop Loop
  deriving C
