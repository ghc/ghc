module Test where

import Data.Bits
import GHC.Num.Integer
import GHC.Num.Natural

integer_to_int :: Integer -> Maybe Int
integer_to_int = toIntegralSized

natural_to_word :: Natural -> Maybe Word
natural_to_word = toIntegralSized
