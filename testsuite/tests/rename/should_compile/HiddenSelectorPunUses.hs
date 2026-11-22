{-# LANGUAGE NoFieldSelectors, ScopedTypeVariables #-}
module HiddenSelectorPunUses where

import SelectorOwner (Row)
import qualified SelectorOwner as Qualified (Row(label), First(duplicate))

data NoSelector = NoSelector { label :: Bool }

hidden (value :: label) = (value :: label)
qualifiedOnly (value :: duplicate) = (value :: duplicate)
