{-# LANGUAGE RankNTypes, FlexibleInstances #-}

-- This one made ghc-4.08 crash 
-- rename/RnEnv.lhs:239: Non-exhaustive patterns in function get_tycon_key
-- The type in the Monad instance is utterly bogus, of course

module ShouldCompile ( Set ) where


data Set a = Set [a]
       deriving (Eq, Ord, Read, Show)

instance Functor Set where
        f `fmap` (Set xs) = Set $ f `fmap` xs

instance Monad (forall a. Eq a => Set a) where
        return x = Set [x]

instance Eq (forall a. [a]) where 
