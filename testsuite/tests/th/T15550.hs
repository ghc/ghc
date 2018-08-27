{-# LANGUAGE TemplateHaskell #-}
module T15550 where

$([d| myId :: a -> a
      myId x = x
      {-# NOINLINE [1] myId #-}
      {-# RULES "myId" forall x. myId x = x #-}
    |])
