{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module T15437A where

import Language.Haskell.TH.Syntax (Q, TExp)

get :: forall a. Int
get = 1

foo :: forall a. Q (TExp Int)
foo = [|| get @a ||]
