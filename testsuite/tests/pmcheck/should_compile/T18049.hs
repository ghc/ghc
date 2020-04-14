{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
module Bug where

import Data.Kind

data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True

f :: SBool b
  -> (b ~ True  => SBool b -> r)
  -> (b ~ False => SBool b -> r)
  -> r
f x t f =
  case x of
    SFalse -> f x
    STrue  -> t x

g :: forall b. SBool b -> ()
g x = f x
  (\x' ->
    case x' of
      -- SFalse -> ()
      STrue  -> ())
  (\_ -> ())
