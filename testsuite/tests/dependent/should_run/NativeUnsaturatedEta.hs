{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# LANGUAGE NoExperimentalDependentTypes #-}
module NativeUnsaturatedEta where

import Data.Coerce (coerce)

data family Eta tag a
newtype instance Eta Bool a = Eta (Maybe a)
  deriving (Functor, Applicative, Monad)

-- The higher-kinded argument to Box has a representational role.
data Box f = Box (f Int)

wrap :: Box Maybe -> Box (Eta Bool)
wrap = coerce

throughMonad :: Eta Bool Int
throughMonad = pure 20 >>= \x -> pure (x + 22)

answer :: Maybe Int
answer = case wrap (Box (Just 42)) of
  Box (Eta x) -> x
