{-# LANGUAGE AllowAmbiguousTypes  #-}

module Main where

import GHC.InfoProv
import Unsafe.Coerce

-- Boilerplate to help us access the literal dictionaries

data Dict c where
    Dict :: forall c. c => Dict c

data Box where
    Box :: forall a. a -> Box

mkBox :: forall a. a => Box
mkBox = unsafeCoerce (Dict @a)

-- Interesting bit

data A = A
data B a = B a

-- Becomes a `StgRhsCon`, which used to not get IPE estimate based on Name
instance Show A where
  show = undefined

-- Becomes a `StgRhsClosure`, which does get IPE estimate based on Name
instance Show a => Show (B a) where
  show = undefined

main :: IO ()
main = do
    -- Should both result in InfoProvs with correct source locations
    (\(Box !d) -> print =<< whereFrom d) $ mkBox @(Show A)
    (\(Box !d) -> print =<< whereFrom d) $ mkBox @(Show (B A))
