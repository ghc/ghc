module WarnNoncanonical where

newtype M a = M a

instance Functor M where
  fmap = undefined

instance Applicative M where
  liftA2 = undefined
  pure = undefined
  (*>) = (>>)

instance Monad M where
  (>>=) = undefined
  (>>) = undefined
