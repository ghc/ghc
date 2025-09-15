{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-unused-do-bind #-}
module T8470 where

data User
type family MethodResult ev
type instance MethodResult User = ()
type EventResult a = MethodResult a

foo = do undefined :: IO (EventResult User)
         return ()
