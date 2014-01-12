-- Check that "->" is an instance of Eval

module ShouldSucceed where

instance Show (a->b) where
        show _ = error "attempt to show function"

instance (Eq b) => Eq (a -> b) where
	(==) f g = error "attempt to compare functions"

	-- Since Eval is a superclass of Num this fails 
	-- unless -> is an instance of Eval
instance (Num b) => Num (a -> b) where
    f + g                     =  \a -> f a + g a
    f - g                     =  \a -> f a - g a
    f * g                     =  \a -> f a * g a
    negate f                  =  \a -> negate (f a)
    abs f                     =  \a -> abs (f a)
    signum f                  =  \a -> signum (f a)
    fromInteger n             =  \a -> fromInteger n
