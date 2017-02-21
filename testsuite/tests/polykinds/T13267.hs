{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module T13267 where

type C1 a = (Show (a -> Bool))

instance C1 Int where

type C2 a = (Show Bool, Show Int)

instance C2 Int where
