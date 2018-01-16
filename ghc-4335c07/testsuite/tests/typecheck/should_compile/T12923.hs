{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module T12923 where

-- Test that ExtendedDefaultRules defaults multiparameter typeclasses with only
-- one parameter of kind Type.
class Works a (b :: Bool) where
   works :: a -> A b

data A (b :: Bool) = A deriving Show

instance Works Integer 'True where works _ = A

main :: IO ()
main = print (works 5 :: A 'True)
