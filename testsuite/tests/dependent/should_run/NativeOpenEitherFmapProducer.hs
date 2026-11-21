{-# LANGUAGE CPP, NoExperimentalDependentTypes #-}
module NativeOpenEitherFmapProducer (Result(..), Translate(..)) where

data Result e a = Failed e | Succeeded a

class Translate t where
    translate :: (a -> b) -> t a -> t b
    count :: t a -> Int

instance {-# OVERLAPPABLE #-} Translate (Result e) where
#ifdef STRICT
    translate _ (Failed e) = seq e (Failed e)
#else
    translate _ (Failed e) = Failed e
#endif
    translate f (Succeeded a) = Succeeded (f a)
    count _ = 1
