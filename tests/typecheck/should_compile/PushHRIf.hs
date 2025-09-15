{-# LANGUAGE RankNTypes #-}

module PushHRIf where

foo = (if True then id else id) :: forall a. a -> a

bar = (foo 'x', foo True)
