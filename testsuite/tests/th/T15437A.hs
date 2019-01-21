{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module T15437A where

import Language.Haskell.TH.Syntax (Q, TExp)
import Type.Reflection

get :: forall a. Int
get = 1

foo :: forall alpha. Typeable alpha => Q (TExp Int)
foo = [|| get @alpha ||]
