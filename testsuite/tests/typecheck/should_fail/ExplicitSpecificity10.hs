{-# LANGUAGE TypeApplications, RankNTypes #-}

module ExplicitSpecificity10 where

newtype T = MkT { unT :: forall {a}. a -> a }

test :: T -> Bool -> Bool
test t = unT t @Bool
