{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wredundant-constraints -Wall -Werror #-}
import GHC.TypeLits (TypeError, ErrorMessage(..))

class C a where f :: a -> a
instance {-# OVERLAPPING #-} C Int where f _ = 42
instance {-# OVERLAPPABLE #-} TypeError ( 'Text "Only Int is supported" ) => C a where f = undefined

main :: IO ()
main = print $ f (42::Int)
