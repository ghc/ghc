{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeAbstractions #-}
module Foo where

import Language.Haskell.TH hiding (Type)
import Data.Kind (Type)

y :: forall b. Show b => b -> String
y = $$(undefined :: forall (m :: Type -> Type). Code m (forall a. Show a => a -> String))
