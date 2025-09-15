{-# LANGUAGE LinearTypes #-}
module LinearPartialSig where

-- We should suggest that _ :: Multiplicity
f :: a %_ -> a
f x = x
