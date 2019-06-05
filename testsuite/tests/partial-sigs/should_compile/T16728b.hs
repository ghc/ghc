{-# LANGUAGE ExplicitForAll, PartialTypeSignatures #-}
module Bug where

g,h:: forall a. a -> _

g x = x       -- Instantiates the wildcard to 'a'

h x = True    -- Instantiates the wildcard to Bool

