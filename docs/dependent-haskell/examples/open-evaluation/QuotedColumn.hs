{-# LANGUAGE DataKinds, GADTs, LinearTypes, RequiredTypeArguments, TemplateHaskell #-}
module QuotedColumn (readQuoted) where

import Data.Kind (Type)
import GHC.Exts (Multiplicity(Many))

$([d|
    data Column (a :: Type) (b :: Type) where
        Column :: forall a. !a %Many -> forall b -> Column a b
    |])

-- | Consume the column once and return its unrestricted field twice.
duplicate :: Column a b %1 -> (a, a)
duplicate (Column value (type b)) = (value, value)

-- | Construct the quoted declaration in its written argument order.
readQuoted :: Int -> (Int, Int)
readQuoted n = duplicate (Column n (type Bool))
