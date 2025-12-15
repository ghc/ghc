{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE ViewPatterns #-}

module T26331a where

pair :: forall a. Bool -> a -> forall b. b -> (a,b)
pair = error "urk"

f :: Int -> ((Int,Bool),(Int,Char))
f (pair True -> x) = (x True, x 'c')  -- (x :: forall b. b -> (Int,b))
