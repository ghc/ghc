{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE GADTs #-}

module GadtConstructorArgs (Boo(..)) where

data Boo where
  Fot :: { x :: Int  -- ^ an 'x'
         , y :: Int  -- ^ a 'y'
         } -> Boo

  -- | Record GADT with docs
  Fob :: { w :: Int  -- ^ a 'w'
         , z :: Int  -- ^ a 'z'
         } -> Boo    -- ^ a 'Boo'
