{-# LANGUAGE Strict #-}
module StrictMatch (strictOrder) where

import Nested (HiddenProduct, Product(..))

strictOrder :: Bool -> HiddenProduct Int -> Bool
strictOrder True (Product _ _) = True
strictOrder False ~_ = False
