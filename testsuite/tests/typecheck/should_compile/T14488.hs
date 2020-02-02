{-# LANGUAGE RankNTypes #-}

module T14488 where

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

data T a = MkT { _tfield :: Eq a => a }

tfield :: Eq a => Lens' (T a) a
tfield f t = (\x -> MkT x) <$> f (_tfield t)
