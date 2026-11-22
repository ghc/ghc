{-# LANGUAGE DeriveFunctor, NoExperimentalDependentTypes #-}
module These (These(..)) where

-- | A value from either side or a value from both sides.
data These e a = This e | That a | These e a
    deriving (Functor, Show)
