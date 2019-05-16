{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Bug where

data family Sing (a :: k)

data instance Sing (z :: Bool) where
  SFalse :: Sing False
  STrue  :: Sing True

pattern STooGoodToBeTrue :: forall (z :: Bool). ()
                         => z ~ True
                         => Sing z
pattern STooGoodToBeTrue = STrue
{-# COMPLETE SFalse, STooGoodToBeTrue #-}

wibble :: Sing (z :: Bool) -> Bool
wibble STrue = True

wobble :: Sing (z :: Bool) -> Bool
wobble STooGoodToBeTrue = True

