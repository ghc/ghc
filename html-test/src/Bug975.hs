{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ExplicitForAll #-}
module Bug973 where

showRead
  :: forall a b. (Show a, Read b)
  => a -- ^ this gets turned into a string...
  -> b -- ^ ...from which this is read
showRead = read . show

-- | Same as 'showRead', but with type variable order flipped
showRead'
  :: forall b a. (Show a, Read b)
  => a -- ^ this gets turned into a string...
  -> b -- ^ ...from which this is read
showRead' = read . show
