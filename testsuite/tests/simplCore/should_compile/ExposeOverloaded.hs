{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -O -fno-worker-wrapper -funfolding-creation-threshold=50 #-}
{-# OPTIONS_GHC -dno-typeable-binds -fexpose-overloaded-unfoldings #-}

module ExposeOverloaded where

-- Will get an unfolding because of the Functor
foo :: Functor f => Maybe (Maybe (Maybe (Maybe (Maybe (Maybe (f a))))))
    -> (a -> b)
    -> Maybe (Maybe (Maybe (Maybe (f b))))
foo (Just (Just (Just (Just (Just (Just x)))))) f = Just $ Just $ Just $ Just $ fmap f x
foo _ _ = Nothing

type family C a where
  C Int = Eq Int
  C Bool = Ord Bool

-- The Enum constraint should cause bars unfolding to be exposed.
bar :: (C a, Enum a) => a -> a -> Bool
bar a b = fromEnum (succ a) > fromEnum (pred . pred . pred . pred . pred $ b)

-- But even if there is just *a* constraint that's not obviously a class the
-- unfolding should be expose.
fam :: (C a) => a -> Char -> Int -> (Bool, a)
fam c a b = (fromEnum (succ a) > fromEnum (pred . pred . pred . pred . pred $ b), c)

-- While the constraint itself is useless to the specialiser, this still gets exposed
-- by -fexpose-overloaded-unfoldings since we check for the presence of a constraint
-- and not it's usefulness.
{-# NOINLINE eq_constraint #-}
eq_constraint :: (a~b) => a -> b -> (a,b)
eq_constraint a b = (a,b)

newtype F t a = F {unF :: (Functor t => t a) }

-- Will get NO unfolding currently since the class dictionary is hidden under the newtype.
-- We might fix this eventually. But since the specializer doesn't handle this well
-- this isn't important yet.
baz :: Maybe (Maybe (Maybe (Maybe (Maybe (Maybe (F t a))))))
    -> (a -> b)
    -> Maybe (Maybe (Maybe (Maybe (F t b))))
baz (Just (Just (Just (Just (Just (Just (x))))))) f = Just $ Just $ Just $ Just $ F $ fmap f (unF x)
baz _ _ = Nothing