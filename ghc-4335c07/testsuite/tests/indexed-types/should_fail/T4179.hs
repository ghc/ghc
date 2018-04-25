{-# LANGUAGE TypeFamilies  #-}

module T4179 where

class DoC a where
     type A2 a
     type A3 a
     op :: a -> A2 a -> A3 a

data Con x = InCon (x (Con x))
type FCon x = x (Con x)

-- should have been changed to this, which works
-- foldDoC :: Functor f => (f a -> a) -> A2 (FCon f) -> Con f -> a
-- foldDoC f i (InCon t) = f (fmap (foldDoC f i) t)
-- this original version causes GHC to hang 
foldDoC :: Functor f => (f a -> a) -> Con f -> a 
foldDoC f (InCon t) = f (fmap (foldDoC f) t)

doCon :: (DoC (FCon x)) => Con x -> A2 (FCon x) -> A3 (FCon x) 
doCon (InCon x) = op x

-- Note that if this is commented out then there's no hang: 
-- presumably because GHC doesn't have to perform type deduction for foldDoC.
fCon :: (Functor x, DoC (FCon x)) => Con x -> A2 (FCon x) -> A3 (FCon x) 
fCon = foldDoC op
