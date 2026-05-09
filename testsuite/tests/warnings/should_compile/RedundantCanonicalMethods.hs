module RedundantCanonicalMethods where

data U = U
data P a = P deriving (Functor)

instance Semigroup U where
  _ <> _ = U

instance Monoid U where
  mappend = (<>)
  mempty = U

instance Applicative P where
  pure _ = P
  _ <*> _ = P

instance Monad P where
  return = pure
  (>>) = (*>)
  _ >>= _ = P
