{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeFamilies #-}
module LocalDerivedAssociated (Signal(..), Flipped, Sample(..)) where

-- | Show still uses the regular deriving pass.
data Signal = Stop | Go
    deriving Show

-- | Check the structural equations before using them in Flipped.
flipSignal :: Signal -> Signal
flipSignal Stop = Go
flipSignal Go = Stop

-- | Use the checked equations on a type argument.
type Flipped (s :: Signal) = flipSignal s

-- | Select the result type for an ordinary input.
class Sample a where
    type Reading a
    sample :: a -> Reading a

instance Sample () where
    type Reading () = LocalDerivedAssociated.identity Signal
    sample () = flipSignal Stop

-- | Supply a type to the associated equation without using the instance.
identity :: a -> a
identity x = x
