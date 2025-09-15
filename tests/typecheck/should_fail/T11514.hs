{-# LANGUAGE RankNTypes #-}

module T11514 where

foo :: forall a. (Show a => a -> a) -> ()
foo = undefined
