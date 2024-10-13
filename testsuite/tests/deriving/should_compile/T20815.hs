module T20815 where

class Functor f => Alt f where
  (<!>) :: f a -> f a -> f a

  some :: Applicative f => f a -> f [a]
  some v = some_v
    where many_v = some_v <!> pure []
          some_v = (:) <$> v <*> many_v

instance Alt [] where
  (<!>) = (++)

newtype L a = L [a] deriving (Functor, Alt)
