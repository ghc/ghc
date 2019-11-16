{-# LANGUAGE ExplicitForAll, PartialTypeSignatures #-}
module Bug where

g,h:: forall a. a -> _
g x = h x

h x = g x

