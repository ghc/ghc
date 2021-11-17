{-# LANGUAGE FunctionalDependencies #-}

module T20661 where

-- Use an empty context to signify that this is not
-- an abstract class, but a class with no methods.
class () => C a b | a -> b
