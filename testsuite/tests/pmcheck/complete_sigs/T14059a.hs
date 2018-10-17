{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module T14059a where

data SBool (z :: Bool) where
  SFalse :: SBool False
  STrue  :: SBool True

pattern STooGoodToBeTrue :: forall (z :: Bool). ()
                         => z ~ True
                         => SBool z
pattern STooGoodToBeTrue = STrue
{-# COMPLETE SFalse, STooGoodToBeTrue #-}

wibble :: SBool z -> Bool
wibble STrue = True

wobble :: SBool z -> Bool
wobble STooGoodToBeTrue = True
