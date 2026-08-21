{-# OPTIONS_GHC -Wimplicit-field-strictness #-}
{-# LANGUAGE StrictData #-}
module T16836c where

-- unannotated fields warn under StrictData too
data T a = MkT a !Bool ~Char
