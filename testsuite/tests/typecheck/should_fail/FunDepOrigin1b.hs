{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module FunDepOrigin1b where

class C a b | a -> b where
  op :: a -> b -> b

-- foo :: (C Bool (Maybe a), C Bool [b]) => x -> (Maybe a, [b])
foo _ = (op True Nothing, op False [])

-- See Note [Suppressing confusing errors] in GHC.Tc.Errors
