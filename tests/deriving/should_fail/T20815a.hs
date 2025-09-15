module T20815a where

class Functor f => Alt f where
  (<!>) :: f a -> f a -> f a

  some :: Applicative f => f a -> f [a]
  some v = some_v
    where many_v = some_v <!> pure []
          some_v = (:) <$> v <*> many_v

instance Alt [] where
  (<!>) = (++)

-- Tests that an `Applicative f` constraint can be inferred
newtype T f a = T (f a) deriving (Functor, Alt)

data NotApplicative a = NotApplicative a deriving Functor

instance Alt NotApplicative where
  (<!>) = const

t :: T NotApplicative Int
t = T (NotApplicative 1) <!> T (NotApplicative 2)
