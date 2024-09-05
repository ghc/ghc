{-# LANGUAGE LinearTypes, UnicodeSyntax #-}

module TooManyMultiplicitiesU where

f :: a %1 ⊸ b
f = undefined

-- UnicodeSyntax doesn't affect the type signature in the error message. So the
-- error message has `%1 %1 ->`, which is a little awkward.
