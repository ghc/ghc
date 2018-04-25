-- #hide
module Data.Time.Calendar.Private where

import Data.Fixed

data PadOption = Pad Int Char | NoPad

showPadded :: PadOption -> String -> String
showPadded NoPad s = s
showPadded (Pad i c) s = replicate (i - length s) c ++ s

showPaddedNum :: (Num t,Ord t,Show t) => PadOption -> t -> String
showPaddedNum NoPad i = show i
showPaddedNum pad i | i < 0 = '-':(showPaddedNum pad (negate i))
showPaddedNum pad i = showPadded pad $ show i

show2Fixed :: Pico -> String
show2Fixed x | x < 10 = '0':(showFixed True x)
show2Fixed x = showFixed True x

show2 :: (Num t,Ord t,Show t) => t -> String
show2 = showPaddedNum $ Pad 2 '0'

show3 :: (Num t,Ord t,Show t) => t -> String
show3 = showPaddedNum $ Pad 3 '0'

show4 :: (Num t,Ord t,Show t) => t -> String
show4 = showPaddedNum $ Pad 4 '0'

mod100 :: (Integral i) => i -> i
mod100 x = mod x 100

div100 :: (Integral i) => i -> i
div100 x = div x 100

clip :: (Ord t) => t -> t -> t -> t
clip a _ x | x < a = a
clip _ b x | x > b = b
clip _ _ x = x

clipValid :: (Ord t) => t -> t -> t -> Maybe t
clipValid a _ x | x < a = Nothing
clipValid _ b x | x > b = Nothing
clipValid _ _ x = Just x
