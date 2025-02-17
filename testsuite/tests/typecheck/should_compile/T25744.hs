{-# LANGUAGE QuantifiedConstraints #-}

module T25744 where

foo :: ( forall x. Eq x => Eq ( f x ) ) => f Int -> Bool
foo _ = False

bar :: forall g. ( forall y. Eq y => Eq ( g y ) ) => g Int -> Bool
bar = withUnrelated foo

withUnrelated :: ( Eq ( f v ) => r ) -> r
withUnrelated f = error "not important"
