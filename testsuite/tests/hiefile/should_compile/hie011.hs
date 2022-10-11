{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwrite-ide-info #-}
module M where

import Language.Haskell.TH.Syntax

newtype T = T { getT :: Int }

instance Lift T where
  lift = undefined
  liftTyped v = [||T $$(liftTyped (getT v))||]


top_level :: ()
top_level = $$([|| () ||])
